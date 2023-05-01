# Set language to English
Sys.setlocale(category = "LC_CTYPE", locale = "en_US.UTF-8")

# Specify required packages
required_packages <- c("dplyr", "ggplot2", "tidyr", "renv", "shiny", "shinyjs",
                       "Rcpp")

# Install missing packages
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load required packages
#lapply(required_packages, library, character.only = TRUE)

#renv::restore()


# Shiny App
if (interactive()) {
  library("shiny")
  library("shinyjs")
  library("RSQLite")
  
  shinyApp(
    # Define the UI
    ui <- fluidPage(
      useShinyjs(),
      h1(id='h1', "Welcome to the Automatic R grader management tool!"),
      
      # Dropdown to select the username
      selectInput(inputId = "username",
                  label = "Select your username:",
                  choices = c(dbGetQuery(con, "SELECT DISTINCT username FROM users")$username),
                  selected = "-- Please select your username"),
      
      # Button to continue
      actionButton(inputId = "continueBtn",
                   label = "Continue",
                   disabled = TRUE
      ),
      
      # Hyperlink to register
      p(HTML("<p id='text-register'>Are you new? 
         <a href='#' id='register-link'>Register here</a></p>")),
      
      # Tabset with 3 tabs, will show after clicking 'Continue'
      conditionalPanel(
        condition = "input.continueBtn > 0",
        tabsetPanel(id = "tabset",
                    
                    # Tab 1
                    tabPanel(title = "Tab 1", "This is the content of Tab 1"),
                    # Tab 2
                    tabPanel(title = "Tab 2", "This is the content of Tab 2"),
                    # Tab 3
                    tabPanel(title = "Tab 3", "This is the content of Tab 3")
                    
        )
      )
    ),
    
    # Define the server
    server <- function(input, output, session) {
      
      # Connect to the SQLite database
      con <- dbConnect(RSQLite::SQLite(), "sqliteRAutoGrader.db")
      
      # Define a reactive expression to get the user_id for the selected username
      user_id <- reactive({
        if (input$username == "-- Please select your username") {
          1
        } else {
          dbGetQuery(con, paste0("SELECT user_id FROM users WHERE username = '", input$username, "'"))$user_id
        }
      })
      
      # Enable continue button if username is selected
      observeEvent(input$username, {
        # Get the user_id for the selected username
        user_id_val <- user_id()
        
        # Debugging statement
        print(user_id_val)
        
        # Enable the continue button only if the user_id is not 0
        if (user_id_val != 0) {
          shinyjs::enable("continueBtn")
        } else {
          shinyjs::disable("continueBtn")
        }
      })
      
      # When the continue button is clicked, show the tabset
      observeEvent(input$continueBtn, {
        hide("h1")
        hide("text-register")
        hide("register-link")
        hide("username")
        hide("continueBtn")
        show("tabset")
      })
    }
  )
}

# Run the app
shinyApp(ui, server)


