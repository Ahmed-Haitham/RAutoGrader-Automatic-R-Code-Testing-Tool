# Set language to English
Sys.setlocale(category = "LC_CTYPE", locale = "en_US.UTF-8")

# Specify required packages
required_packages <- c("dplyr", "ggplot2", "tidyr", "renv", "shiny", "shinyjs",
                       "Rcpp", "RSQLite", "R6", "DBI")

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


# R6 classes

# Define the database connection
con <- dbConnect(RSQLite::SQLite(), "sqliteRAutoGrader.db")

# Define the R6 classes for each table
# users - without user_id because is auto-incremented
users <- R6Class("users",
                 public = list(
                   username = NULL,
                   
                   initialize = function(username) {
                     
                     self$username <- username
                     
                     invisible(self)
                   },
                   
                   insert = function() {
                     dbExecute(con, "INSERT INTO users (username) VALUES (?)", self$username)
                   }
                 )
)

# courses - without course id as it is auto-incremented
courses <- R6Class("courses",
                   public = list(
                     
                     course_name = NULL,
                     
                     initialize = function(course_name) {
                       
                       self$course_name <- course_name
                       
                       invisible(self)
                     },
                     
                     insert = function() {
                       dbExecute(con, "INSERT INTO courses (course_name) VALUES (?)", self$course_name)
                     }
                   )
)

# groups - without group_id? how to retrieve?
groups <- R6Class("groups",
                  public = list(
                    group_id = NULL,
                    course_id = NULL,
                    group_name = NULL,
                    time = NULL,
                    days_week = NULL,
                    
                    initialize = function(group_id, course_id, group_name, time, days_week) {
                      self$group_id <- group_id
                      self$course_id <- course_id
                      self$group_name <- group_name
                      self$time <- time
                      self$days_week <- days_week
                      
                      invisible(self)
                    },
                    
                    insert = function() {
                      dbExecute(con, "INSERT INTO groups (group_id, course_id, group_name, time, days_week) VALUES (?, ?, ?, ?, ?)",
                                self$group_id, self$course_id, self$group_name, self$time, self$days_week)
                    }
                  )
)

# tasks - without group_id? how to retrieve?
tasks <- R6Class("tasks",
                 public = list(
                   task_id = NULL,
                   group_id = NULL,
                   task_number = NULL,
                   grading_scale = NULL,
                   submission_date = NULL,
                   task_mean_grade = NULL,
                   
                   initialize = function(task_id, group_id, task_number, grading_scale, submission_date, task_mean_grade) {
                     self$task_id <- task_id
                     self$group_id <- group_id
                     self$task_number <- task_number
                     self$grading_scale <- grading_scale
                     self$submission_date <- submission_date
                     self$task_mean_grade <- task_mean_grade
                     
                     invisible(self)
                   },
                   
                   insert = function() {
                     dbExecute(con, "INSERT INTO tasks (task_id, group_id, task_number, grading_scale, submission_date, task_mean_grade)
                     VALUES (?, ?, ?, ?, ?, ?)", self$task_id, self$group_id, self$task_number, self$grading_scale,
                               self$submission_date, self$task_mean_grade)
                   }
                 )
)

# teaching - without group_id? how to retrieve?
teaching <- R6Class("teaching",
                    public = list(
                      teach_id = NULL,
                      user_id = NULL,
                      course_id = NULL,
                      group_id = NULL,
                      
                      initialize = function(teach_id, user_id, course_id, group_id) {
                        self$teach_id <- teach_id
                        self$user_id <- user_id
                        self$course_id <- course_id
                        self$group_id <- group_id
                        
                        invisible(self)
                      },
                      
                      insert = function() {
                        dbExecute(con, "INSERT INTO teaching (teach_id, user_id, course_id, group_id) VALUES (?, ?, ?, ?)",
                                  self$teach_id, self$user_id, self$course_id, self$group_id)
                      }
                    )
)
                                  
# questions - without group_id? how to retrieve?
questions <- R6Class("questions",
                     public = list(
                       question_id = NULL,
                       task_id = NULL,
                       question_name = NULL,
                       question_answer = NULL,
                       
                       initialize = function(question_id, task_id, question_name, question_answer) {
                         self$question_id <- question_id
                         self$task_id <- task_id
                         self$question_name <- question_name
                         self$question_answer <- question_answer
                         
                         invisible(self)
                       },
                       
                       insert = function() {
                         dbExecute(con, "INSERT INTO questions (question_id, task_id, question_name, question_answer) VALUES (?, ?, ?, ?)",
                                   self$question_id, self$task_id, self$question_name, self$question_answer)
                       }
                     )
)

# TEST R6 CLASSES - not tested yet 
maria <- users$new("Maria Kubara")

# Create the courses
r_intro <- courses$new("R intro")
webscraping <- courses$new("Webscraping")
econometrics <- courses$new("Econometrics")

# Create the groups
r_intro_gr1 <- groups$new("gr 1", "9-10.45", c("Mon", "Thu"))
r_intro_gr2 <- groups$new("gr 2", "11-12.30", c("Mon", "Thu"))
r_intro_gr3 <- groups$new("gr 3", "16.45-18.15", c("Wed", "Fri"))
webscraping_gr1 <- groups$new("gr 1", "11-12.30", c("Mon", "Thu"))
webscraping_gr2 <- groups$new("gr 2", "16.45-18.15", c("Wed", "Fri"))
econometrics_gr1 <- groups$new("gr 1", "9-10.45", c("Mon", "Thu"))

# Create the teaching relationship - table
maria_teaching_r_intro_gr1 <- teaching$new(teach_id = 1, user_id = maria$user_id, course_id = r_intro$course_id, group_id = r_intro_gr1$group_id)
maria_teaching_r_intro_gr3 <- teaching$new(teach_id = 2, user_id = maria$user_id, course_id = r_intro$course_id, group_id = r_intro_gr3$group_id)
maria_teaching_econometrics_gr1 <- teaching$new(teach_id = 3, user_id = maria$user_id, course_id = econometrics$course_id, group_id = econometrics_gr1$group_id)

# Check if the data was inserted correctly into the database???
dbGetQuery(con, "SELECT * FROM users")
dbGetQuery(con, "SELECT * FROM courses")
dbGetQuery(con, "SELECT * FROM groups")
dbGetQuery(con, "SELECT * FROM tasks")
dbGetQuery(con, "SELECT * FROM teaching")
dbGetQuery(con, "SELECT * FROM questions")

