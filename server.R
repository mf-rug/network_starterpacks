options(reticulate.output_handler = function(x) cat(x, "\n"))

envs <- reticulate::virtualenv_list()
if (!'venv_shiny_app' %in% envs) {
    reticulate::virtualenv_create(envname = 'venv_shiny_app',
                                  python = '/usr/bin/python3')
    reticulate::virtualenv_install('venv_shiny_app',
                                   packages = c('Bio', 'openai', 'atproto', 'python-dotenv'))
}
# https://github.com/ranikay/shiny-reticulate-app
# Set environment BEFORE this
reticulate::use_virtualenv('venv_shiny_app', required = TRUE)


shinyServer(function(input, output, session) {
  shinyjs::runjs("$('#add_prompt').attr('maxlength', 250)")
  # Reactive values
  sp_df <- reactiveVal(NULL)
  users_df <- reactiveVal(NULL)
  follows <- reactiveVal(NULL)
  followers <- reactiveVal(NULL)
  mutuals <- reactiveVal(NULL)
  logs <- reactiveVal(NULL)
  fler_table_all_button <- reactiveVal(NULL)
  fl_table_all_button <- reactiveVal(NULL)
  sp_table_all_button <- reactiveVal(NULL)
  req_df <- reactiveVal(NULL)
  
  # Function to load and merge part files
  load_and_merge_parts <- function(user, recover=FALSE) {
    flwer_data <- read.delim(paste0('./flwer_data_',input$user, '.tsv'), header=TRUE, sep='\t', stringsAsFactors = F)
    # part files or single file depending on new run or recover
    pattern = paste0("^part_", ".*", user, "\\.tsv$")
    if (recover) {
      pattern <- paste0("^part_all_", user, "\\.tsv$")
      part_files <- list.files(path = "./",
                               pattern = pattern,
                               full.names = TRUE)
  
      # Standardize column names and handle empty data frames
      standard_columns <- c("Type", "Handle", "Display.Name", "Starter.Pack.Name", "URL")
      df <- read.delim(part_files, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      merged_table <- df[, standard_columns, drop = FALSE]  # Reorder to standard column order
    } else {
      part_files <- list.files(path = "./",
                               pattern = pattern,
                               full.names = TRUE)
      # Standardize column names and handle empty data frames
      standard_columns <- c("Type", "Handle", "Display.Name", "Starter.Pack.Name", "URL")
      part_tables <- lapply(part_files, function(part_file) {
        df <- read.delim(part_file, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
        
        # Ensure the data frame has the correct columns
        if (nrow(df) == 0) {
          df <- as.data.frame(matrix(ncol = length(standard_columns), nrow = 0))
          colnames(df) <- standard_columns
        } else {
          missing_cols <- setdiff(standard_columns, colnames(df))
          for (col in missing_cols) {
            df[[col]] <- NA  # Add missing columns with NA values
          }
          df <- df[, standard_columns, drop = FALSE]  # Reorder to standard column order
        }
        return(df)
      })
      # Remove any completely empty tables
      part_tables <- part_tables[sapply(part_tables, nrow) > 0]
      if (length(part_tables) == 0) {
        browser()
        print('log to empty error')
        logs("All part tables are empty.")
        stop("All part tables are empty.")
      }
      
      # Merge all part tables
      merged_table <- do.call(rbind, part_tables)
      if (!input$follows) {
        print('follows not considered')
        merged_table <- merged_table[merged_table$Type != 'Follow',]
      }
      if (!input$followers) {
        print('followers not considered')
        merged_table <- merged_table[merged_table$Type != 'Follower',]
      }
    }
    if (!input$follows) {
      merged_table <- merged_table[merged_table$Type != 'Follow',]
    }
    if (!input$followers) {
      merged_table <- merged_table[merged_table$Type != 'Follower',]
    }
    
    merge_path = paste0("./part_all_", user, '.tsv')
    file.remove(part_files)
    write.table(merged_table, merge_path, sep='\t', quote = FALSE, row.names = FALSE)
    
    sp_df(merged_table)  # Update the reactive value with the merged table
  }
  
  
  observeEvent(input$run_sp, {
    logs(NULL)  # Reset logs
    old_run = paste0("./part_all_", input$user, '.tsv')
    if (file.exists(old_run)) {
      file.remove(old_run)
    }
    tryCatch({
      withProgress(message = "Logging in", value = 0.2, {
        tryCatch({
          sys <- import("sys")
          sys$argv <- c('./login.py', input$username, input$app_password)
          source_python("./login.py")
          
          result <- py$main()
          if (is.null(result)) {
            stop('Login Failed')
          }
          logs(paste0("Login successful.\n"))
        }, error = function(e) {
          logs(paste0("Error during login process: ", e$message, "\n"))
          updateTabsetPanel(session, "main_tabs", selected = "Logs")
          stop("Error during login process: ", e$message, "\n")
        })
      })
      
      # Step 1: Run the first Python script (./get_follows.py)
      withProgress(message = "Fetching followers and follows...", value = 0.2, {
        sys <- import("sys")
        sys$argv <- c('./get_follows.py', input$user, input$follows, input$followers)
        cat('Running this command:\n', paste('python3 get_follows.py',  input$user, input$follows, input$followers), '\n')
        source_python("./get_follows.py")
        result <- py$main()
        if (result$status == "error") {
          logs(paste0('Error in getting follow(er)s:\n', str_extract(result$message, "(?<=message=')[^']*"), '\n'))
          updateTabsetPanel(session, "main_tabs", selected = "Logs")
          stop(paste0("Error: Stopping because of a python error in get_follows.py:\n", str_extract(result$message, "(?<=message=')[^']*")))
        } else {
          logs("Successfully got follows and followers.")
        }
      })

      # Step 2: Read flwer_data.tsv
      # browser()
      flwer_data_path <- paste0("./flwer_data_", input$user, ".tsv")
      if (!file.exists(flwer_data_path)) {
        stop(paste(flwer_data_path, " not found."))
      }
      flwer_data <- read.delim(flwer_data_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      follows(flwer_data[flwer_data$Type == 'Follow',])
      followers(flwer_data[flwer_data$Type == 'Follower',])
      mutuals(flwer_data[flwer_data$Type == 'Mutual',])
      
      # Step 3: Loop over users in batches and call ./process_follows.py
      if (input$sps_flls || input$sps_fllwers) {
        if (!input$sps_flls) {
          flwer_data_adj <- flwer_data[flwer_data$Type != "Follow",]
        } else if (!input$sps_fllwers) {
          flwer_data_adj <- flwer_data[flwer_data$Type != "Follower",]
        } else {
          flwer_data_adj <- flwer_data
        }
        flwer_data_path_adj = paste0("./flwer_data_adj_", input$user, ".tsv")
        write_tsv(flwer_data_adj, flwer_data_path_adj, col_names = TRUE)
        total_users <- nrow(flwer_data_adj)
        total_batches <- ceiling(total_users / 25)  # Calculate total number of batches
        withProgress(message = "Processing SPs", value = 0, {
          for (batch in seq_len(total_batches)) {
            # Calculate line range for the current batch
            start_line <- (batch - 1) * 25 + 1
            end_line <- min(batch * 25, total_users)
            # browser()
            # Call ./process_25_follows.py
            incProgress(1 / total_batches, detail = paste("User batch", batch, '/', total_batches, ': users', start_line, '-', end_line))
            sys <- import("sys")
            sys_args <- c('./process_25_follows.py', 
                          flwer_data_path_adj,
                          input$user,
                          start_line, 
                          end_line)
            sys$argv <- sys_args
            source_python("./process_25_follows.py")
            cat('Running this command:\npython3', paste(sys_args, collapse = " "),'\n')
            result <- py$main()
            if (result$status == "error") {
              logs(paste0('Error in processing:\n', result$message, '\n'))
              updateTabsetPanel(session, "main_tabs", selected = "Logs")
              stop("Error: Stopping because of a python error in process_25_follows.py")
            } else {
              logs("Successfully got follows and followers.")
            }
          }
        })
        load_and_merge_parts(input$user)
      }
      
    }, error = function(e) {
      logs(paste(e$message, '\n'))
      updateTabsetPanel(session, "main_tabs", selected = "Logs")
    })
  })

  
  observeEvent(input$restore_sp, {
    sp_df(NULL)
    logs(NULL)
    tryCatch({
      if (file.exists(paste0("./part_all_", input$user, ".tsv"))) {
        load_and_merge_parts(input$user, recover = TRUE)
      } else {
        stop('No previous runs found. Run a new search.')
      }
      logs('Successfully restored table')
    }, error = function(e) {
      if (str_detect(e$message, 'cannot open the connection')) {
        e$message <- 'No previous runs found. Run a new search.'
        if (input$user == '') {
          e$message <- 'No user name supplied.'
        }
      }
      updateTabsetPanel(session, "main_tabs", selected = "Logs")
      output$logs <- renderText({
        paste("Error in restore:", e$message)
      })
    })
  })
  
  
  observeEvent(input$run_users, {
    # if (file.exists(paste0('./', input$user, '_flwer_data_detailed.tsv'))) {
    #   flwer_data <- read_tsv(
    #     paste0('./', input$user, '_flwer_data_detailed.tsv'),
    #     col_names = TRUE,
    #     show_col_types = FALSE
    #   )
    #   users_df(flwer_data)
    # }
    job_id = paste0(sample(c(letters, LETTERS, 0:9)), collapse = '')
    flwer_data_sp = NULL
    if (!is.null(sp_df()) && nrow(sp_df()) > 0) {
      write_tsv(req_df(), paste0("./request_", job_id, '.tsv'))
      withProgress(message = "Getting users in SPs.", value = 0.2, {
        tryCatch({
          sys <- import("sys")
          sys$argv <- c('./get_users_from_sp.py', job_id)
          source_python('./get_users_from_sp.py')
          result <- py$main()
        }, error = function(e) {
          print(paste0('error in ./get_users_from_sp.py:', e$message))
        })
      })
      withProgress(message = "Getting SP user's data.", value = 0.5, {
        tryCatch({
          json_file_path <- paste0("requested_users_", job_id, '.json')
          sys <- import("sys")
          sys_args <- c(
            './analyse_users.py',
            json_file_path,
            ifelse(input$user == '', 'None', input$user),
            ifelse(input$username == '', 'None', input$username),
            input$get_posts,
            input$aisum,
            ifelse(input$api_key == '' || is.null(input$api_key), 'None', input$api_key),
            ifelse(input$add_prompt == '' || is.null(input$add_prompt), 'None', input$add_prompt),
            input$post_cutoff,
            input$sp_cutoff,
            input$fllwer_cutoff,
            input$days_cutoff,
            input$already_fl
          )
          sys$argv <- sys_args
          cat('Running this command:\npython3', paste(sys_args, collapse = " "),'\n')
          source_python('./analyse_users.py')
          result <- py$main()
          flwer_data_sp <- read.delim(result$output_file, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
        }, error = function(e) {
          print(paste0('error in ./analyse_users.py:', e$message))
        })
      })
    }
    withProgress(message = "Getting selected follow(er)s.", value = 0.7, {
      tryCatch({
        df <- req_df()
        json_data <- as.list(setNames(df[df$type != 'sp', 'type'], df[df$type != 'sp', 'name']))
        json_file_path <- paste0("flers_", job_id, '.json')
        write_json(json_data, json_file_path, pretty = TRUE, auto_unbox = TRUE)
        
        incProgress(0.1, message = "Getting follow(er)'s data")
        sys <- import("sys")
        sys_args <- c(
          './analyse_users.py',
          json_file_path,
          ifelse(input$user == '', 'None', input$user),
          ifelse(input$username == '', 'None', input$username),
          input$get_posts,
          TRUE, #input$aisum,
          ifelse(input$api_key == '' || is.null(input$api_key), 'None', input$api_key),
          ifelse(input$add_prompt == '' || is.null(input$add_prompt), 'None', input$add_prompt),
          0, #, input$post_cutoff,
          NA, #, input$sp_cutoff,
          0, #, input$fllwer_cutoff,
          0, #, input$days_cutoff,
          FALSE #, input$already_fl
        )
        sys$argv <- sys_args
        cat('Running this command:\npython3', paste(sys_args, collapse = " "),'\n')
        source_python('./analyse_users.py')
        result <- py$main()
        print(paste0('Success:', result))
        flwer_data_fl <- read.delim(result$output_file, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      }, error = function(e) {
        print(paste0('error in fl(er) analysis and ./analyse_users.py:', e$message))
      })
    })
    users_df(rbind(flwer_data_fl, flwer_data_sp))
  })
  
  
  output$user_table <- renderUI({
    df <- users_df()
    if (is.data.frame(df)) {
      if (nrow(df) > 0) {
        HTML(
          paste0(
            '<details class="user_details"><summary>',
            '<span style="display: flex; justify-content: space-between;">',
            
            '<span style="font-size:19px;text-decoration: underline;"><strong>Got a total of ', 
            nrow(df) , 
            ' unique user', 
            ifelse(nrow(df) > 1, 's', ''), 
            ' after starter packs analysis</strong></span>',
             '<span><i class="fa fa-chevron-down"></i></span>',
            '</span></summary>',
            DT::dataTableOutput('user_table_out'),
            '</details>'
          ))
      } else {
        DT::dataTableOutput('user_table_out')
      }
    } else {
      DT::dataTableOutput('user_table_out')
    }
  })
  output$user_table_out <- DT::renderDataTable({
    df <- users_df()
    if (is.data.frame(df) && nrow(df) > 0) {
      df$avatar <- paste0('<img src="', df$avatar, '" style="width: 50px; height: 50px; border-radius: 50%; overflow: hidden; object-fit: cover;">')
      df[is.na(df$Description), "Description"] <- ''
      df1 <- df[!is.na(as.numeric(df$Count)), ]
      df2 <- df[is.na(as.numeric(df$Count)), ]
      if (nrow(df1) > 0) {
        df1$Handle <-
          paste0(
              '<div class=scrollable>',
              '<table style="width: 100%; border-collapse: collapse;">',
              # Top row: Avatar in the left cell, Name and Handle in the right cell
              '<tr>',
              '<td style="width: 20%; text-align: center; vertical-align: middle;">',
              df1$avatar,
              '</td>',
              '<td style="width: 80%; text-align: left; vertical-align: middle; line-height:1.15;">',
              '<div>',
              '<strong>', df1$Name, '</strong><br>',
              '<a href="https://bsky.app/profile/', df1$Handle, 
              '" target="_blank">', df1$Handle, '</a><br>',
              
              '<span style="font-size:13px; color: grey;"><i>Listed in ', df1$Count, ' starter pack',
              ifelse(df1$Count > 1, 's', ''), '</i></span>',
              '</div>',
              '</td>',
              '</tr>',
              
              '<tr>',
              '<td colspan="2" style="padding: 0px; font-size: 13px;">',
              '<p>', df1$Description, '</p>',
              '</td>',
              '</tr>',
              '</table>',
              '</div>'
          )
      }
      if (nrow(df2) > 0) {
        df2$Handle <-
          paste0(
            '<div class=scrollable>',
              '<table style="width: 100%; border-collapse: collapse;">',
                # Top row: Avatar in the left cell, Name and Handle in the right cell
                '<tr>',
                  '<td style="width: 20%; text-align: center; vertical-align: middle;">',
                    df2$avatar,
                  '</td>',
                  '<td style="width: 80%; text-align: left; vertical-align: middle; line-height:1.15;">',
                    '<div>',
                      '<strong>', df2$Name, '</strong><br>',
                      '<a href="https://bsky.app/profile/', df2$Handle,
                      '" target="_blank">', df2$Handle, '</a><br>',
                      '<span style="font-size:13px; color: grey;"><i>Is a selected ', df2$Count, '</i></span>',
                    '</div>',
                  '</td>',
                '</tr>',
                
                '<tr>',
                '<td colspan="2" style="padding: 0px; font-size: 13px;">',
                '<p>', df2$Description, '</p>',
                '</td>',
                '</tr>',
              '</table>',
            '</div>'
          )
       }
      df <- rbind(df1,df2)
      colnames(df) <- c('user', 'name', 'avatar', 'SPs', 'posts', 'reposts', 'fllwrs', 'fllws', 'Description', 'lastPosts', 'AIcateg', 'AIsig', 'AIsummary')
      df$lastPosts <- str_replace_all(df$lastPosts, '\\|\\|\\-\\-\\|\\|', '<br>• ') %>% 
                      str_replace(., '^(?=.)', '•') %>% 
                      str_replace_all(., '\\|\\|\\|\\|', '<br>') %>% 
                      str_extract(., "•[^•]*(?:•[^•]*)?(?:•[^•]*)?(?:•[^•]*)?(?:•[^•]*)?") %>% 
                      str_replace('^', '<div class=scrollable><p style="font-size:11px; ">') %>% 
                      str_replace('$', '</p></div>')
      df$AIsig <- df$AIsig %>% 
                      str_replace('^', '<div class=scrollable><p style="font-size:11px; ">') %>% 
                      str_replace('$', '</p></div>')
      df$AIcateg <- df$AIcateg %>% 
                       str_replace('^', '<p style="font-size:13px; ">') %>% 
                       str_replace('$', '</p></div>')
      df$AIsummary <- df$AIsummary %>% 
                    str_replace('^', '<div class=scrollable><p style="font-size:11px; ">') %>% 
                    str_replace('$', '</p>')
      # browser()
      df$reposts <- sapply(df$reposts, function(x) if (!is.na(x) && !str_detect(x, "NA")) { paste(x, '%') } else {x})
      df$reposts <- paste0('<p style="text-align:right;margin:0;padding:0;">', df$reposts, '</p>')
      if (isolate(input$aisum)) {
        df <- df[order(df$SPs, decreasing = TRUE), c('user', 'posts', 'reposts', 'fllwrs', 'fllws', 'lastPosts', 'AIcateg', 'AIsig', 'AIsummary')]
      } else {
        if (isolate(input$get_posts)) {
          df <- df[order(df$SPs, decreasing = TRUE), c('user', 'posts', 'reposts', 'fllwrs', 'fllws', 'lastPosts')]
        } else {
          df <- df[order(df$SPs, decreasing = TRUE), c('user', 'posts', 'fllwrs', 'fllws')]
        }
      }
      datatable(df,
        rownames = FALSE,
        escape = FALSE,
        options = list(pageLength = 20,
                       scrollX = '400px'
        ))
    }
  })
  
  # Render the final table in the Shiny app
  output$sp_table_out <- renderDT({
    if (input$sps_flls || input$sps_fllwers) {
      req(sp_df())  # Ensure the table is loaded
      df <- sp_df()
      if (nrow(df) > 0) {
        unique_values <- unique(df$Handle)
        colors <- colorRampPalette(c("#9B2252", "#0FAB74", "#AB4513", "#98228B", "#8B8B00"))(length(unique_values))
        colors <- sample(colors)
        df$cols <- apply(df, 1, function(x) { colors[which(x[2] == unique_values)]})
        df$Handle <- paste0('<font style="color:', df$col, '">', df$Display.Name, '</font> (<a href="https://bsky.app/profile/', df$Handle, '" target="_blank">', df$Handle, '</a>)')
    
        df$URL <- paste0('<a href="', df$URL, '" target="_blank">', df$Starter.Pack.Name, '</a>')
        colnames(df) <- c('Type', 'Connection', 'display name', 'starter pack name', 'Starter Pack')
        # Reorder rows by custom type order: mutuals -> follow -> follower
        df$Type <- factor(df$Type, levels = c("user", "Mutual", "Follow", "Follower"))
        df <- df[order(df$Type), ]
        
        # Select and display the desired columns
        df <- df[, c('Starter Pack', 'Connection', 'Type')]
  
        datatable(df[,], 
                  rownames = FALSE,
                  escape = FALSE,
                  options = list(pageLength = 10)) %>%
          formatStyle(0, target = 'row', lineHeight='75%') %>% 
          formatStyle(
            columns = "Connection",  # Apply formatting to the 'Connection' column
            target = "cell",  # Apply formatting to the cell
            backgroundColor = styleEqual(
              levels = unique_values,
              values = colors
            )
          )
      }
    }
  })
  
  output$sp_table <- renderUI({
    if (input$sps_flls || input$sps_fllwers) {
      df <- sp_df()
      if (is.data.frame(df)) {
        if(nrow(df) > 0) {
          HTML(
            paste0(
              '<details><summary>',
              '<span style="display: flex; justify-content: space-between;">',
              '<span style="font-size:19px;text-decoration: underline;"><strong>Found ', nrow(df) ,' starter pack', ifelse(nrow(df) > 1, 's', ''), ' for <a href="https://bsky.app/profile/',
              isolate(input$user),
              '" target="_blank">',
              isolate(input$user),
              '</a>',
              ifelse(isolate(input$follows) || isolate(input$followers), paste0(' + ', nrow(mutuals()), ' mutuals'), ''),
              ifelse(isolate(input$followers), paste0(' + ', nrow(followers()), ' followers'), ''),
              ifelse(isolate(input$follows), paste0(' + ', nrow(follows()), ' follows'), ''),
              '</span>',
              '<span><i class="fa fa-chevron-down"></i></span>',
              '</strong>',
              '</span></summary>',
              DTOutput('sp_table_out'),
              '<i>select SPs for analysis by clicking on the rows, or</i>', 
              actionBttn('all_sps', HTML('<i>select all</i>'), style = 'minimal', color = 'primary', size = 's'),
              '</details>'
            ))
      } else {
        HTML(paste0(
          '<span style="font-size:19px;text-decoration: underline;"><strong>Found 0 mutuals for <a href="https://bsky.app/profile/',
          isolate(input$user),
          '" target="_blank">',
          isolate(input$user),
          '</a></span>'
        ))
      }
      } 
    }
  })  
  
  observeEvent(input$all_sps, {
    df <- sp_df()
    if (is.data.frame(df) && nrow(df) > 0) {
      if (is.null(sp_table_all_button()) || sp_table_all_button() == '<i>select all</i>') {
        updateActionButton(session = getDefaultReactiveDomain(), 'all_sps', label = '<i>unselect all</i>')
        selectRows(sp_table_out_proxy, selected = 1:nrow(df))
        sp_table_all_button('<i>unselect all</i>')
      } else {
        sp_table_all_button('<i>select all</i>')
        updateActionButton(session = getDefaultReactiveDomain(), 'all_sps', label = '<i>select all</i>')
        selectRows(sp_table_out_proxy, selected = NULL)
      }
    }
  })
  
  sp_table_out_proxy <- dataTableProxy("sp_table_out")
  
  
  
  
  
  
  
  
  ############################# Follows table
  output$fl_table <- renderUI({
    df <- follows()
    if (is.data.frame(df)) {
      HTML(
        paste0(
          '<details><summary>',
          '<span style="display: flex; justify-content: space-between;">',
          '<span style="font-size:19px;text-decoration: underline;"><strong>Found ', nrow(df) ,' follow', ifelse(nrow(df) > 1, 's', ''), ' for <a href="https://bsky.app/profile/',
          isolate(input$user),
          '" target="_blank">',
          isolate(input$user),
          '</a></span>',
          '<span><i class="fa fa-chevron-down"></i></span>',
          '</strong>',
          '</span></summary>',
          DTOutput('fl_table_out'),
          '<i>select follows for analysis by clicking on the rows, or</i>', 
          actionBttn('all_fls', HTML('<i>select all</i>'), style = 'minimal', color = 'primary', size = 's'),
          '</details>'
        ))
    }
  })
  
  
  observeEvent(input$all_fls, {
    df <- follows()
    if (is.data.frame(df) && nrow(df) > 0) {
      if (is.null(fl_table_all_button()) || fl_table_all_button() == '<i>select all</i>') {
        updateActionButton(session = getDefaultReactiveDomain(), 'all_fls', label = '<i>unselect all</i>')
        selectRows(fl_table_out_proxy, selected = 1:nrow(df))
        fl_table_all_button('<i>unselect all</i>')
      } else {
        fl_table_all_button('<i>select all</i>')
        updateActionButton(session = getDefaultReactiveDomain(), 'all_fls', label = '<i>select all</i>')
        selectRows(fl_table_out_proxy, selected = NULL)
      }
    }
  })
  
  fl_table_out_proxy <- dataTableProxy("fl_table_out")
  
  output$fl_table_out <- renderDT({
    df <- follows()
    if (input$follows && is.data.frame(df) && nrow(df) > 0) {
      df[!is.na(df$Display.Name) & df$Display.Name == '', 'Display.Name'] <- '<i>n.a.</i>'
      df$avatar <- paste0('<img src="', df$avatar, '" style="width: 35px; height: 35px; border-radius: 50%; overflow: hidden; object-fit: cover;">')
      df$user <- paste0(
        '<table style="width: 100%;">
        <th style="padding:0;">
          <td style="margin: 0; padding: 0;line-height: 1; width: 35px;">',
        df$avatar,
        '</td>
        </th>
        <th>
          <td style="margin: 0; padding: 0; line-height: normal;">',
        df$Display.Name,
        ' (<a href="https://bsky.app/profile/',
        df$Handle,
        '" target="_blank">',
        df$Handle,
        '</a>)',
        '</td>
        </th>
      </table>'
      )
      df <- df[c('user', "Description", "Created")]
      colnames(df)[1] <- 'User'
      df$Created <- format(ymd_hms(gsub("Z", "", df$Created)), "%d %b %Y") %>% 
        str_replace('^', '<span style="font-size:13px; ">') %>% 
        str_replace('$', '</span>')
      df$Description <- df$Description %>% 
        str_replace('^', '<div class=scrollable><p style="font-size:11px; margin: 0;">') %>% 
        str_replace('$', '</p></div>')
      datatable(df,
                escape = FALSE,
                rownames = FALSE,
                options = list(scrollX = FALSE)) %>% 
        formatStyle(0, target = 'row', lineHeight='75%')
    }
  })
  
  #############################   #############################   
  
  
  ############################# Mutuals table
  output$mut_table <- renderUI({
    df <- mutuals()
    if (is.data.frame(df)) {
      if (nrow(df) > 0) {
        HTML(
          paste0(
            '<details><summary>',
            '<span style="display: flex; justify-content: space-between;">',
            '<span style="font-size:19px;text-decoration: underline;"><strong>Found ', nrow(df) ,' mutual', ifelse(nrow(df) > 1, 's', ''), ' for <a href="https://bsky.app/profile/',
            isolate(input$user),
            '" target="_blank">',
            isolate(input$user),
            '</a></span>',
            '<span><i class="fa fa-chevron-down"></i></span>',
            '</strong>',
            '</span></summary>',
            DTOutput('mut_table_out'),
            '<i>select mutuals for analysis by clicking on the rows, or</i>', 
            actionBttn('all_muts', HTML('<i>select all</i>'), style = 'minimal', color = 'primary', size = 's'),
            '</details>'
          ))
      } else {
        HTML(paste0(
          '<span style="font-size:19px;text-decoration: underline;"><strong>Found 0 mutuals for <a href="https://bsky.app/profile/',
          isolate(input$user),
          '" target="_blank">',
          isolate(input$user),
          '</a></span>'
        ))
      }
    }
  })
  
  
  observeEvent(input$all_muts, {
    df <- mutuals()
    if (is.data.frame(df) && nrow(df) > 0) {
      if (is.null(mut_table_all_button()) || mut_table_all_button() == '<i>select all</i>') {
        updateActionButton(session = getDefaultReactiveDomain(), 'all_muts', label = '<i>unselect all</i>')
        selectRows(mut_table_out_proxy, selected = 1:nrow(df))
        mut_table_all_button('<i>unselect all</i>')
      } else {
        mut_table_all_button('<i>select all</i>')
        updateActionButton(session = getDefaultReactiveDomain(), 'all_muts', label = '<i>select all</i>')
        selectRows(mut_table_out_proxy, selected = NULL)
      }
    }
  })
  
  mut_table_out_proxy <- dataTableProxy("mut_table_out")
  
  output$mut_table_out <- renderDT({
    df <- mutuals()
    if (is.data.frame(df) && nrow(df) > 0) {
      df[!is.na(df$Display.Name) & df$Display.Name == '', 'Display.Name'] <- '<i>n.a.</i>'
      df$avatar <- paste0('<img src="', df$avatar, '" style="width: 50px; height: 50px; border-radius: 50%; overflow: hidden; object-fit: cover;">')
      df$user <- paste0(
        '<table style="width: 100%;">
        <th style="padding:0;">
          <td style="margin: 0; padding: 0;line-height: 1; width: 35px;">',
        df$avatar,
        '</td>
        </th>
        <th>
          <td style="margin: 0; padding: 0; line-height: normal;">',
        df$Display.Name,
        ' (<a href="https://bsky.app/profile/',
        df$Handle,
        '" target="_blank">',
        df$Handle,
        '</a>)',
        '</td>
        </th>
      </table>'
      )
      df <- df[c('user', "Description", "Created")]
      colnames(df)[1] <- 'User'
      df$Created <- format(ymd_hms(gsub("Z", "", df$Created)), "%d %b %Y") %>% 
        str_replace('^', '<span style="font-size:13px; ">') %>% 
        str_replace('$', '</span>')
      df$Description <- df$Description %>% 
        str_replace('^', '<div class=scrollable><p style="font-size:11px; margin: 0;">') %>% 
        str_replace('$', '</p></div>')
      datatable(df,
                escape = FALSE,
                rownames = FALSE) %>% 
        formatStyle(0, target = 'row', lineHeight='75%')
    }
  })
  
  #############################   ############################# mut
  
  
  
  output$fler_table <- renderUI({
    df <- followers()
    if (is.data.frame(df)) {
      if (nrow(df) > 0) {
        HTML(
          paste0(
            '<details><summary>',
            '<span style="display: flex; justify-content: space-between;">',
            '<span style="font-size:19px;text-decoration: underline;"><strong>Found ', nrow(df) ,' follower', ifelse(nrow(df) > 1, 's', ''), ' for <a href="https://bsky.app/profile/',
            isolate(input$user),
            '" target="_blank">',
            isolate(input$user),
            '</a></span>',
            '<span><i class="fa fa-chevron-down"></i></span>',
            '</strong></summary>',
            '</span>',
            DTOutput('fler_table_out'),
            '<i>select followers for analysis by clicking on the rows, or</i>', 
            actionBttn('all_flers', HTML('<i>select all</i>'), style = 'minimal', color = 'primary', size = 's'),
            '</details>'
          ))
    } else {
      HTML(paste0(
        '<span style="font-size:19px;text-decoration: underline;"><strong>Found 0 mutuals for <a href="https://bsky.app/profile/',
        isolate(input$user),
        '" target="_blank">',
        isolate(input$user),
        '</a></span>'
      ))
    }
    }
  })
  
  fler_table_out_proxy <- dataTableProxy("fler_table_out")
  
  observeEvent(input$all_flers, {
    df <- followers()
    if (is.data.frame(df) && nrow(df) > 0) {
      if (is.null(fler_table_all_button()) || fler_table_all_button() == '<i>select all</i>') {
        updateActionButton(session = getDefaultReactiveDomain(), 'all_flers', label = '<i>unselect all</i>')
        selectRows(fler_table_out_proxy, selected = 1:nrow(df))
        fler_table_all_button('<i>unselect all</i>')
      } else {
        fler_table_all_button('<i>select all</i>')
        updateActionButton(session = getDefaultReactiveDomain(), 'all_flers', label = '<i>select all</i>')
        selectRows(fler_table_out_proxy, selected = NULL)
      }
    }
  })
  
  output$fler_table_out <- renderDT({
    df <- followers()
    if (input$followers && is.data.frame(df) && nrow(df) > 0) {
      df[!is.na(df$Display.Name) & df$Display.Name == '', 'Display.Name'] <- '<i>n.a.</i>'
      df$avatar <- paste0('<img src="', df$avatar, '" style="width: 40px; height: 40px; border-radius: 50%; overflow: hidden; object-fit: cover;">')
      df$user <- paste0(
        '<table style="width: 100%;">
        <th style="padding:0;">
          <td style="margin: 0; padding: 0;line-height: 1; width: 35px;">',
        df$avatar,
        '</td>
        </th>
        <th>
          <td style="margin: 0; padding: 0; line-height: normal;">',
        df$Display.Name,
        ' (<a href="https://bsky.app/profile/',
        df$Handle,
        '" target="_blank">',
        df$Handle,
        '</a>)',
        '</td>
        </th>
      </table>'
      )
      df <- df[c('user', "Description", "Created")]
      colnames(df)[1] <- 'User'
      df$Created <- format(ymd_hms(gsub("Z", "", df$Created)), "%d %b %Y") %>% 
        str_replace('^', '<span style="font-size:13px; ">') %>% 
        str_replace('$', '</span>')
      df$Description <- df$Description %>% 
        str_replace('^', '<div class=scrollable><p style="font-size:11px; margin: 0;">') %>% 
        str_replace('$', '</p></div>')
      datatable(df,
                escape = FALSE,
                rownames = FALSE) %>% 
        formatStyle(0, target = 'row', lineHeight='75%')
    }
  })

  output$user_table_header <- renderUI({
    df <- users_df()
    if (is.data.frame(df)) {
      HTML(paste0('<span style="font-size:19px;text-decoration: underline;"><strong>Found ', nrow(df) ,' unique user', ifelse(nrow(df) > 1, 's', ''), ' in the starter packs</strong></span>'))
    }
  })
  
  output$selected_for_anal <- renderUI({
    if (is.null(input$fler_table_out_rows_selected) && is.null(input$fl_table_out_rows_selected) && is.null(input$sp_table_out_rows_selected)) {
      disable('run_users')
      HTML('<i><span style="color: grey; font-size: 13px;">Select users / starter packs for analysis.</span></i>')
    } else {
      enable('run_users')
      request = list(follow = follows()[input$fl_table_out_rows_selected, 'Handle'],
                     follower = followers()[input$fler_table_out_rows_selected, 'Handle'],
                     sp = sp_df()[input$sp_table_out_rows_selected, 'Starter.Pack.Name'])
      req_df <- do.call(rbind, lapply(names(request), function(name) {
        if (length(unlist(request[[name]] > 0))) {
          data.frame(
            type = name,
            name = unlist(request[[name]]), 
            stringsAsFactors = TRUE
          )
        }
      }))
      
      if (!is.null(sp_df()) && nrow(sp_df()) > 0) {
        req_df[req_df$type == 'sp', 'URL'] <- sp_df()[input$sp_table_out_rows_selected, 'URL']
      }
      
      req_df(req_df)
      
      HTML(
        paste0(
          '<i><span style="color: grey; font-size: 13px;">',
          length(c(input$fl_table_out_rows_selected,input$fler_table_out_rows_selected)),
          ' users and ',
          length(input$sp_table_out_rows_selected),
          ' starter packs selected analysis.</span></i>'
        )
      )
    }
  })
  observe({
    if (input$aisum) {
      if (input$username == ''){
        runjs('$("#username").tooltip({title: "Provide your handle to get personalized AI ranking.", placement: "right"}).tooltip("show");')
      }
    } else {
      runjs('$("#username").tooltip("dispose");')
    }})
  
  observe({
    if (input$get_posts) {
      shinyjs::show('post_hidden')
    } else {
      shinyjs::hide('post_hidden')
      updatePrettySwitch(session, 'aisum', value = FALSE)
    }
  })
  
  observe({
    if (input$aisum) {
      shinyjs::show('ai_hidden')
    } else {
      shinyjs::hide('ai_hidden')
    }
  })
  
  # necessary to trigger despite log on inactive tab
  observe({
    if (!is.null(logs()) && startsWith(logs(), "Error")) {
      updateTabsetPanel(session, "main_tabs", selected = "Logs")
      shinyjs::hide('user_anal_hidden')
    } else {
      updateTabsetPanel(session, "main_tabs", selected = "Results")
      if (is.data.frame(isolate(sp_df())) || is.data.frame(isolate(follows())) || is.data.frame(isolate(followers()))) {
        shinyjs::show('user_anal_hidden')
      }
    }
  })
  
  output$logs <- renderText({
    req(logs())
    out <- ''
    if (!is.null(logs()) && startsWith(logs(), "Error")) {
      out <- 'Last log output from file:\n'
  
      # Path to the log file
      log_file <- "./atproto_script.log"
      
      # Check if the file exists
      if (!file.exists(log_file)) {
        return("Log file not found.")
      }
      
      # Read the entire file
      logs <- readLines(log_file)
      
      # Combine logs into a single text block
      logs_combined <- paste(logs, collapse = "\n")
      
      # Split into paragraphs based on three empty lines
      paragraphs <- unlist(strsplit(logs_combined, "\n\n\n"))
      
      # Get the last paragraph, trim whitespace
      last_paragraph <- paragraphs[length(paragraphs)]
      last_paragraph <- trimws(last_paragraph)
      
      # Return the last paragraph
      if (nchar(last_paragraph) > 0) {
        out <- paste0(out, last_paragraph)
      } else {
        out <- paste0(out, "No logs available.")
      }
    }
    return(paste0(logs(), '\n', out))
  })
})
