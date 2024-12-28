library(shiny)
library(DT)
library(shinyWidgets)
library(bslib)
library(stringr)
library(shinycssloaders)
library(reticulate)


shinyUI(page_navbar(
  title = strong('Starter Packs in Network Finder'),
  nav_spacer(), # push nav items to the right
  nav_item(HTML('<span style="white-space: nowrap; font-size: 11px;">made by <a href ="https://bsky.app/profile/maxfus.bsky.social" target="_blank">Max Fürst</a></span>'), ),
  nav_item(),
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  ),
  sidebarLayout(
    sidebarPanel(
      HTML('<strong>Login</strong>'),
      HTML('<hr style="margin: 5px 0 5px; border-color: #656565; "/>'),
      textInput("username", "Username:", "", placeholder = 'yourLogin.bsky.social'),  # Pre-filled with 'fraaije'
      passwordInput("app_password", HTML('App Password <a href="https://bsky.app/settings/app-passwords" target="_blank">(get one)</a>:'), placeholder = 'your app password'),
      HTML('<strong>Settings</strong>'),
      HTML('<hr style="margin: 5px 0 5px; border-color: #656565; "/>'),
      textInput("user", "User to check for starter packs:"),
      "Also check this users's",
      HTML('<br style="margin: 0px 0 7px;  "/>'),
      fluidRow(
        prettySwitch('follows', 'Follows', value = TRUE, status = 'info'),
        prettySwitch('followers', 'Followers', value = TRUE, status = 'info'),
      ),
      HTML('<strong>Start starter pack search</strong>'),
      HTML('<hr style="margin: 5px 0 15px; border-color: #656565; "/>'),
      fluidRow(
        column(4, 
          actionButton("run_button", "Run")
        ),
        # HTML('<span style="color:lightgrey;">&nbsp&nbsp&nbsp|&nbsp</span>'), 
        column(8, align='right',
               actionBttn("anal_button", HTML('<span style="color:grey; font-size:14px;">Restore previous</span>'), style = 'minimal', color='royal',size ='lg'),
        ),
      ),
      htmlOutput("response"),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(id = "main_tabs",
        tabPanel(title = 'Results', value = 'Results', br(),
                 htmlOutput('table_header'), 
                 HTML('<hr style="margin: 0px 0 5px; border-color: #656565; "/>'),
                 DTOutput("table_output") %>% withSpinner(color = "#0dc5c1"),br()
        ),
        tabPanel(title = 'Logs', value = 'Logs', br(),
                 verbatimTextOutput('logs', placeholder = TRUE)
        )
      )
    )
  )
))
