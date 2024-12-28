options(reticulate.output_handler = function(x) cat(x, "\n"))

envs <- reticulate::virtualenv_list()
if (!'venv_shiny_app' %in% envs) {
    reticulate::virtualenv_create(envname = 'venv_shiny_app',
                                  python = '/usr/bin/python3')
    reticulate::virtualenv_install('venv_shiny_app',
                                   packages = c('Bio'))
}
# https://github.com/ranikay/shiny-reticulate-app
# Set environment BEFORE this
reticulate::use_virtualenv('venv_shiny_app', required = TRUE)


shinyServer(function(input, output, session) {

  # Reactive values
  final_table <- reactiveVal(NULL)
  follows <- reactiveVal(NULL)
  followers <- reactiveVal(NULL)
  mutuals <- reactiveVal(NULL)
  logs <- reactiveVal(NULL)
  
  # Function to load and merge part files
  load_and_merge_parts <- function(user, recover=FALSE) {
    user_list <- read.delim(paste0('./user_list_',input$user, '.tsv'), header=TRUE, sep='\t', stringsAsFactors = F)
    follows(nrow(user_list[user_list$Type == 'Follow',]))
    followers(nrow(user_list[user_list$Type == 'Follower',]))
    mutuals(nrow(user_list[user_list$Type == 'Mutual',]))
    
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
    browser()
    file.remove(part_files)
    write.table(merged_table, merge_path, sep='\t', quote = FALSE, row.names = FALSE)
    final_table(merged_table)  # Update the reactive value with the merged table

  }
  
  
  observeEvent(input$run_button, {
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
        source_python("./get_follows.py")
        result <- py$main()
        if (result$status == "error") {
          logs(paste0('Error in getting follow(er)s:\n', str_extract(result$message, "(?<=message=')[^']*"), '\n'))
          updateTabsetPanel(session, "main_tabs", selected = "Logs")
          stop("Stopping because of a python error in get_follows.py")
        } else {
          logs("Successfully got follows and followers.")
        }
      })

      # Step 2: Read user_list.tsv
      user_list_path <- paste0("./user_list_", input$user, ".tsv")
      if (!file.exists(user_list_path)) {
        stop("user_list.tsv not found.")
      }
      user_data <- read.delim(user_list_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

      # Step 3: Loop over users in batches and call ./process_follows.py
      total_users <- nrow(user_data)
      total_batches <- ceiling(total_users / 25)  # Calculate total number of batches
      
      withProgress(message = "Processing", value = 0, {
        for (batch in seq_len(total_batches)) {
          # Calculate line range for the current batch
          start_line <- (batch - 1) * 25 + 1
          end_line <- min(batch * 25, total_users)
          
          # Call ./process_25_follows.py
          incProgress(1 / total_batches, detail = paste("Batch", batch, '/', total_batches, ': Users', start_line, '-', end_line))
          sys <- import("sys")
          sys$argv <- c('./process_25_follows.py', 
                        user_list_path,
                        input$user,
                        start_line, 
                        end_line)
          source_python("./process_25_follows.py")
          result <- py$main()
          if (result$status == "error") {
            logs(paste0('Error in processing:\n', result$message, '\n'))
            updateTabsetPanel(session, "main_tabs", selected = "Logs")
            stop("Stopping because of a python error in process_25_follows.py")
          } else {
            logs("Successfully got follows and followers.")
          }
        }
      })
      
      
      load_and_merge_parts(input$user)
      
    }, error = function(e) {
      logs(paste0('Error in processing follow(er)s\n:', e$message, '\n'))
      browser()
      updateTabsetPanel(session, "main_tabs", selected = "Logs")
      stop("Stopping because of a python error in process_25_follows.py")
    })
  })
  
  observeEvent(input$anal_button, {
    logs(NULL)
    tryCatch({
      load_and_merge_parts(input$user, recover = TRUE)
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
  
  # Render the final table in the Shiny app
  output$table_output <- renderDT({
    req(final_table())  # Ensure the table is loaded
    df <- final_table()
    if (nrow(df) > 0) {
      unique_values <- unique(df$Handle)
      colors <- colorRampPalette(c("#9B2252", "#0FAB74", "#AB4513", "#98228B", "#8B8B00"))(length(unique_values))
      colors <- sample(colors)
      df$cols <- apply(df, 1, function(x) { colors[which(x[2] == unique_values)]})
      df$Handle <- paste0('<font style="color:', df$col, '">', df$Display.Name, '</font> (<a href="https://bsky.app/profile/', df$Handle, '" target="_blank">', df$Handle, '</a>)')
  
      df$URL <- paste0('<a href="', df$URL, '" target="_blank">', df$Starter.Pack.Name, '</a>')
      colnames(df) <- c('Type', 'Connection', 'display name', 'starter pack name', 'Starter Pack')
      # Reorder rows by custom type order: mutuals -> follow -> follower
      df$Type <- factor(df$Type, levels = c("Mutual", "Follow", "Follower"))
      df <- df[order(df$Type), ]
      
      # Select and display the desired columns
      df <- df[, c('Starter Pack', 'Connection', 'Type')]

      datatable(df[,], 
                rownames = FALSE,
                escape = FALSE,
                options = list(pageLength = 20)) %>%
        formatStyle(
          columns = "Connection",  # Apply formatting to the 'Connection' column
          target = "cell",  # Apply formatting to the cell
          backgroundColor = styleEqual(
            levels = unique_values,
            values = colors
          )
        )
    }
  })
  
  output$table_header <- renderUI({
    df <- final_table()
    if (is.data.frame(df)) {
      HTML(
        paste0(
          '<span style="font-size:19px;">Found ', nrow(df) ,' starter packs for <a href="https://bsky.app/profile/',
          input$user,
          '" target="_blank">',
          input$user,
          '</a>',
          ifelse(isolate(input$follows) || isolate(input$followers), paste0(' + ', mutuals(), ' mutuals'), ''),
          ifelse(isolate(input$follows), paste0(' + ', followers(), ' followers'), ''),
          ifelse(isolate(input$followers), paste0(' + ', follows(), ' follows'), ''),
          '</span>'
        )
      )
    }
  })
  
  # necessary to trigger despite log on inactive tab
  observe({
    if (!is.null(logs()) && startsWith(logs(), "Error")) {
      updateTabsetPanel(session, "main_tabs", selected = "Logs")
    } else {
      updateTabsetPanel(session, "main_tabs", selected = "Results")
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
