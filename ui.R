library(shiny)
library(DT)
library(shinyWidgets)
library(bslib)
library(stringr)
library(readr)
library(shinycssloaders)
library(reticulate)
library(shinyjs)
library(lubridate)

shinyUI(page_navbar(
  useShinyjs(),  # Set up shinyjs
  tags$head(
    tags$link(
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css", 
      rel = "stylesheet"
    )
  ),
  tags$style(
    HTML("
      #fler_table_out table > thead > tr > th,
      #fler_table_out table > tbody > tr > th,
      #fler_table_out table > tfoot > tr > th,
      #fler_table_out table > thead > tr > td,
      #fler_table_out table > tbody > tr > td,
      #fler_table_out table > tfoot > tr > td 
      
      #fl_table_out table > thead > tr > th,
      #fl_table_out table > tbody > tr > th,
      #fl_table_out table > tfoot > tr > th,
      #fl_table_out table > thead > tr > td,
      #fl_table_out table > tbody > tr > td,
      #fl_table_out table > tfoot > tr > td 
      
      #sp_table_out table > thead > tr > th,
      #sp_table_out table > tbody > tr > th,
      #sp_table_out table > tfoot > tr > th,
      #sp_table_out table > thead > tr > td,
      #sp_table_out table > tbody > tr > td,
      #sp_table_out table > tfoot > tr > td 

      #user_table_out table > thead > tr > th,
      #user_table_out table > tbody > tr > th,
      #user_table_out table > tfoot > tr > th,
      #user_table_out table > thead > tr > td,
      #user_table_out table > tbody > tr > td,
      #user_table_out table > tfoot > tr > td {
      
      padding: 0.2rem; 
      margin: 0; 
      vertical-align: middle; 
    }
    .scrollablex {
        max-width: 25vw;
        margin: 0;
        padding: 0;
        white-space: nowrap; /* Prevent text from wrapping */
        overflow-x: scroll;
        overflow-y: hidden; /* Disable vertical scrolling */
        table-layout: fixed;
    }
    .scrollable {
        width: 100%;
        max-height: 175px;
        margin: 0;
        padding: 0;
        overflow: scroll;
        table-layout: fixed;
    }
    .user {
        padding: -3px, -3, -3px;
        overflow-x: scroll;
        table-layout: fixed;
    }
    details > summary {
        list-style-type: none;
    }
    
    details[open] > summary {
        list-style-type: none;
    }
  
    details {
        border: 1px solid transparent;
        box-shadow: 2px 2px 8px rgba(50, 180, 200, 0.3);
        border-image: repeating-linear-gradient(to right,
  #c4e17f 7%, #c4e17f 14%,
  #c9ff56 21%, #f2fa71 27%,
  #fad071 33%, #fad071 40%,
  #f0766b 47%, #f0766b 54%,
  #db9dbe 60%, #db9dbe 66%,
  #c49cdf 71%, #c49cdf 78%,
  #6599e2 84%, #6599e2 90%,
  #61c2e4 95%, #61c2e4 100%);
        border-image-slice: 1;
        background: linear-gradient(to right, rgba(0, 100, 50, 0.01), rgba(0, 200, 150, 0.05)); 
        border-radius: 0.5rem;
        padding: 0.5rem;
    }
    details[open] > summary {
        margin-bottom: 0.5rem;
    }
    
    
    
    .user_details > summary {
        list-style-type: none;
    }
    
    .user_details[open] > summary {
        list-style-type: none;
    }

    .user_details {
        border: 1.5px solid transparent;
        box-shadow: 2px 2px 8px rgba(50, 180, 200, 0.3);
        border-image: repeating-linear-gradient(to left,
  #c4e17f 7%, #c4e17f 14%,
  #c9ff56 21%, #f2fa71 27%,
  #fad071 33%, #fad071 40%,
  #f0766b 47%, #f0766b 54%,
  #db9dbe 60%, #db9dbe 66%,
  #c49cdf 71%, #c49cdf 78%,
  #6599e2 84%, #6599e2 90%,
  #61c2e4 95%, #61c2e4 100%);
        border-image-slice: 1;
        background: linear-gradient(to right, rgba(50, 0, 80, 0.01), rgba(100, 0, 150, 0.05)); 
        border-radius: 0.5rem;
        padding: 0.5rem;
    }
    .user_details[open] > summary {
        margin-bottom: 0.5rem;
    }
  ")
  ),
  title = strong('Max\' Network FindR'),
  nav_spacer(), # push nav items to the right
  nav_item(HTML('<span style="white-space: nowrap; font-size: 11px;">made by <a href ="https://bsky.app/profile/maxfus.bsky.social" target="_blank">Max Fürst</a></span>'), ),
  nav_item(),
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  ),
  sidebarLayout(
    sidebarPanel(
      HTML('<strong>Login</strong>'),
      HTML('<hr style="margin: 0px 0 5px; border-color: #656565; "/>'),
      textInput("username", "Username:", "", placeholder = 'yourLogin.bsky.social'),  # Pre-filled with 'fraaije'
      passwordInput("app_password", HTML('App Password <a href="https://bsky.app/settings/app-passwords" target="_blank">(get one)</a>:'), placeholder = 'your app password'),
      HTML('<strong>Settings</strong>'),
      HTML('<hr style="margin: 0px 0 5px; border-color: #656565; "/>'),
      textInput("user", "User to center network on", value = 'mfdebugapi.bsky.social'),
      HTML('<p style="margin: -10px 0 4px;  "/>Also check this user\'s</p>'),
      # HTML('<br style="margin: 0px 0 7px;  "/>'),
      fluidRow(
        column(5, prettySwitch('follows', 'follows', value = TRUE, status = 'info')),
        column(7, prettySwitch('sps_flls', 'their starter pcks.', value = FALSE, status = 'info')),
      ),
      fluidRow(
        column(5, prettySwitch('followers', 'followers', value = TRUE, status = 'info')),
        column(7, prettySwitch('sps_fllwers', 'their starter pcks.', value = FALSE, status = 'info')),
      ),
      HTML('<strong>Start user/SP search</strong>'),
      HTML('<hr style="margin: 0px 0 10px; border-color: #656565; "/>'),
      fluidRow(
        column(4, 
               actionButton("run_sp", "Run")
        ),
        column(8, align='right',
               actionBttn("restore_sp", HTML('<span style="color:grey; font-size:14px;">Restore previous</span>'), style = 'minimal', color='royal',size ='lg'),
        ),
      ),
      shinyjs::hidden(div(id='user_anal_hidden',
                          br(),
                          HTML('<strong>User analysis on selected users/SPs</strong>'),
                          HTML('<hr style="margin: 5px 0 10px; border-color: #656565; "/>'),
                          prettySwitch('get_posts', "Get recent posts of each user", status = 'info', TRUE),
                          div(id='post_hidden',
                              prettySwitch('aisum', "AI categorize, summarize, & rank", status = 'info')
                              ),
                          div(id='ai_hidden',
                              textInput("api_key", HTML("Provide an OpenAI <a href='https://platform.openai.com/api-keys'>API key</a>")),
                              textAreaInput(
                                "add_prompt",
                                HTML(
                                  '<p style="line-height:0.94;margin:0px 0 -1px;">Additional intructions to AI prompt<br><span style="color:grey; font-size:13px;">(e.g.: <i>"focus ranking on similar scientific interests"</i>) </span></p>'
                                  ),
                                height = '100px'          
                                ),
                              HTML('<p style="margin:5px 0 10px;"><strong>Filter for SP users: </strong></p>'),
                              prettySwitch('already_fl', 'Exclude accounts user already follows', TRUE, 'info'),
                              sliderInput('post_cutoff', HTML('&nbspExclude if post count under'), 1, 200, 20),
                              sliderInput('sp_cutoff', HTML('&nbspExclude if SP appearances under'), 1, 50, 1),
                              sliderInput('fllwer_cutoff', HTML('&nbspExclude if followers under'), 1, 250, 10),
                              sliderInput('days_cutoff', HTML('&nbspExclude if days since registration under'), 1, 365, 14),
                              ),
                          HTML('<hr style="margin: -6px 0 10px; border-color: #656565; "/>'),
                          fluidRow(
                            column(4, actionButton('run_users', 'Run', disabled = TRUE)),
                            column(8, align='right',
                                   actionBttn("restore_users", 
                                              HTML('<span style="color:grey; font-size:14px;">Restore previous</span>'), 
                                              style = 'minimal', color='royal',size ='lg'),
                                   )
                            ),
                          uiOutput('selected_for_anal'), 
                          )
                      ),
      width = 3
    ),
    
    ##################################
    # Main Panel
    mainPanel(
      tabsetPanel(id = "main_tabs",
        tabPanel(title = 'Results', value = 'Results', 
                 br(),
                 uiOutput('mut_table'), 
                 uiOutput('fl_table'),
                 uiOutput('fler_table'), 
                 uiOutput('sp_table'),
                 uiOutput('user_table', width='70vw'),
                 # htmlOutput('user_table_header'), 
                 # DT::dataTableOutput("user_table_out", width='70vw'),
                 br(),
        ),
        tabPanel(title = 'Logs', value = 'Logs', br(),
                 verbatimTextOutput('logs', placeholder = TRUE)
        )
      )
    )
  )
))
