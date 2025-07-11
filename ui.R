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
library(jsonlite)
library(dplyr)

shinyUI(page_navbar(
  title = strong('Max\' Network FindR'),
  header = tagList(
    useShinyjs(),  
    tags$head(
      tags$link(
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css", 
        rel = "stylesheet"
      )
    ),
    includeCSS("www/theme.css"),
    includeScript("www/custom.js")
  ),
  nav_spacer(), # push nav items to the right
  nav_item(HTML('<span style="white-space: nowrap; font-size: 11px;">made by <a href ="https://bsky.app/profile/maxfus.bsky.social" target="_blank">Max Fürst</a></span>'), ),
  nav_item(),
  nav_item(
    input_dark_mode(id = "dark_mode")
  ),
  nav_panel('Main', 
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
                                passwordInput("api_key", HTML("Provide an OpenAI <a href='https://platform.openai.com/api-keys'>API key</a>")),
                                tags$div(class = 'small-textarea',
                                         textAreaInput(
                                    "add_prompt",
                                    HTML(
                                      '<p style="line-height:0.94;margin:0px 0 -1px;">Additional intructions to AI prompt<br><span style="color:grey; font-size:13px;">(e.g.: <i>"focus ranking on similar scientific interests"</i>) </span></p>'
                                      ),
                                    value = 'Rate how interesting this user is to follow on a scale of 1-10. Favor users with more total and engaging posts and disfavor users that mostly repost instead of original posts.\n',
                                    height = '100px'          
                                         )
                                ),
                                HTML('<p style="margin:5px 0 10px;"><strong>Filter users: </strong></p>'),
                                prettySwitch('already_fl', 'Exclude accounts user already follows', TRUE, 'info'),
                                prettySwitch('filter_only_sp', 'Apply filters only to SP users', TRUE, 'info'),
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
                            )), br(),
        shinyjs::hidden(div(id='user_filter_hidden',
                            HTML('<strong>Filter users in analysis table</strong>'),
                            HTML('<hr style="margin: 5px 0 10px; border-color: #656565; "/>'),
                            prettySwitch('already_fl_us', 'Hide accounts already following', TRUE, 'info'),
                            div(
                              style = "display: flex; align-items: center; width: 100%; padding-bottom: 10px;",
                              HTML("Filter description"),  # Keep this normal-sized
                              div(
                                style = "margin-left: auto; display: flex; align-items: center; gap: 5px;",
                                div(
                                  class = "custom-switch",
                                  prettySwitch('case', HTML('<small>Case&nbsp&nbsp</small>'), status = 'info')
                                ),
                                div(
                                  class = "custom-switch",
                                  prettySwitch('regex', HTML('<small>RegEx</small>'), status = 'info')
                                )
                              )
                            ),
                            textAreaInput('descr_filter', NULL),
                            div(class = "slider-container",
                                span("min # posts", class = "slider-label"),  # Label next to slider
                                div(class = "slider-wrapper", sliderInput('min_posts', NULL, 0, 1000, 0))
                            ),
                            div(class = "slider-container",
                                span(HTML("min # posts<br>last month"), class = "slider-label"),  # Label next to slider
                                div(class = "slider-wrapper", sliderInput('min_posts_lm', NULL, 0, 100, 0))
                            ),
                            div(class = "slider-container",
                                span("min # follows", class = "slider-label"),  # Label next to slider
                                div(class = "slider-wrapper", sliderInput('min_fl', NULL, 0, 1000, 0))
                            ),
                            div(class = "slider-container",
                                span("min # followers", class = "slider-label"),  # Label next to slider
                                div(class = "slider-wrapper", sliderInput('min_fler', NULL, 0, 1000, 0))
                            ),
                            div(class = "slider-container",
                                span("max % reposts", class = "slider-label"),  # Label next to slider
                                div(class = "slider-wrapper", sliderInput('max_repost', NULL, 0, 100, 100))
                            )
        )),
                            
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
  )
))
