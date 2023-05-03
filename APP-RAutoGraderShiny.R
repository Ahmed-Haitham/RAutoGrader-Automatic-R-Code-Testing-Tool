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
# users - ok SQL working
users <- R6Class("users",
                 public = list(
                   user_id = NULL,
                   username = NULL,
                   
                   initialize = function(username) {
                     self$username <- username
                     
                     # retrieve the last user_id from the users table
                     result <- dbGetQuery(con, "SELECT user_id FROM users ORDER BY user_id DESC LIMIT 1")
                     
                     if (nrow(result) == 0) {
                       # if there are no existing users, then user_id = 1
                       self$user_id <- 1
                     } else {
                       # set the user_id to the last user_id plus one
                       self$user_id <- result + 1
                     }
                     
                     invisible(self)
                   },
                   
                   insert = function() {
                     query <- paste0("INSERT INTO users (user_id, username) VALUES (", 
                                     self$user_id, ", '", self$username, "')")
                     dbExecute(con, query)
                   }
                 )
)

# courses - Ok SQL working
courses <- R6Class("courses",
                   public = list(
                     course_id = NULL,
                     course_name = NULL,
                     group_name = NULL,
                     group_hour = NULL,  # format xx:xx-xx:xx length 11 max
                     day_week = NULL, # format mon, tue, wed, thu, fri, sat length 3 max
                     
                     initialize = function(course_name, group_name, group_hour, day_week) {
                       self$course_name <- course_name
                       self$group_name <- group_name
                       self$group_hour <- group_hour
                       self$day_week <- day_week
                       
                       courseid <- dbGetQuery(con, "SELECT course_id FROM courses ORDER BY course_id DESC LIMIT 1")
                       if (nrow(courseid) == 0) {
                         self$course_id <- 1
                       } else {
                         self$course_id <- courseid + 1
                       }
                       
                       
                       invisible(self)
                     },
                     
                     insert = function() {
                       query <- paste0("INSERT INTO courses (course_id, course_name, 
                                       group_name, group_hour, day_week) VALUES (", 
                                       self$course_id, ", '", self$course_name, "', '", 
                                       self$group_name, "', '", self$group_hour, "', '", 
                                       self$day_week, "')")
                       
                       dbExecute(con, query)
                     }
                   )
)

# create variable scale, extract day from day_week? group schedule + 2 days = 100, + 3 days = 80?
# --- pending code here ---

# tests - Ok SQL working
tests <- R6Class("tests",
                  public = list(
                    test_id = NULL,
                    test_topic = NULL,
                    
                    initialize = function(test_topic) {
                      self$test_topic <- test_topic
                      testid <- dbGetQuery(con, "SELECT test_id FROM tests ORDER BY test_id DESC LIMIT 1")
                      if (nrow(testid) == 0) {
                        self$test_id <- 1
                      } else {
                        self$test_id <- testid + 1
                      }
                      
                      invisible(self)
                    },
                    
                    insert = function() {
                      query <- paste0("INSERT INTO tests (test_id, test_topic) VALUES (", 
                                      self$test_id, ", '", self$test_topic, "')")
                      dbExecute(con, query)
                    }
                  )
)

# tests deadlines, scale on 100 points or 80 points
# -- variables here --

# questions - Ok SQL connection working
questions <- R6Class("questions",
                     public = list(
                       question_id = NULL,
                       test_id = NULL,
                       question_number = NULL,
                       question_description = NULL,
                       question_answer = NULL,
                       answers_filepath = "None",
                       
                       initialize = function(question_number, question_description,
                                             question_answer, answers_filepath = "None") {
                         
                         self$question_number <- question_number
                         self$question_description <- question_description
                         self$question_answer <- question_answer
                         self$answers_filepath <- answers_filepath
                         
                         questid <- dbGetQuery(con, "SELECT question_id FROM questions ORDER BY question_id DESC LIMIT 1")
                         if (nrow(questid) == 0) {
                           self$question_id <- 1
                         } else {
                           self$question_id <- questid + 1
                         }
                         
                         result <- dbGetQuery(con, "SELECT test_id FROM tests ORDER BY test_id DESC LIMIT 1")
                         if (nrow(result) == 0) {
                           self$test_id <- 1
                         } else {
                           self$test_id <- result
                         }
                         
                         
                         invisible(self)
                       },
                       
                       insert = function() {
                         query <- paste0("INSERT INTO questions (question_id, test_id, question_number, 
                                       question_description, question_answer, answers_filepath) VALUES (", 
                                         self$question_id, ", ", self$test_id, ", '", self$question_number, "', '",
                                         self$question_description, "', '", self$question_answer, "', '", 
                                         self$answers_filepath, "')")
                         
                         dbExecute(con, query)
                       }
                     )
)

# teaching - in progress
teaching <- R6Class("teaching",
                    public = list(
                      
                      user_id = NULL,
                      course_id = NULL,
                      test_id = NULL,
                      question_id = NULL,
                      
                      initialize = function(user_id, course_id, test_id, question_id) {
                        #course_id_query <- sprintf("SELECT course_id FROM courses WHERE course_name='%s'", course_name)
                        #course_id <- dbGetQuery(con, course_id_query)$course_id[1]
                        
                        self$user_id <- user_id
                        self$course_id <- course_id
                        self$test_id <- test_id
                        self$question_id <- question_id
                        
                        invisible(self)
                      },
                      
                      insert = function() {
                        dbExecute(con, "INSERT INTO teaching (teach_id, user_id, course_id, 
                                  test_id, question_id) VALUES (?, ?, ?, ?, ?)",
                                  self$user_id, self$course_id, self$test_id, self$question_id)
                      }
                    )
)
                                  

# TEST R6 CLASSES - not tested yet 
# tested - ok
maria <- users$new("Maria Kubara")
maria$insert()
dbGetQuery(con, "SELECT * FROM users")

# Create the groups
# tested - ok 
r_intro_gr1 <- courses$new("R intro", "gr 1", "9-10.45", "Mon")
r_intro_gr1$insert()
dbGetQuery(con, "SELECT * FROM courses")

# create tests
# tested - ok
loops <- tests$new("Data types")
loops$insert()
dbGetQuery(con, "SELECT * FROM tests")

# Create questions
# tested - ok
q1 <- questions$new(1, "choose the correct answer:", "vector")
q1$insert()
dbGetQuery(con, "SELECT * FROM questions")

# Create the teaching relationship - table
# testing 
maria_teaching_r_intro_gr1 <- teaching$new()
maria_teaching_r_intro_gr1$insert()
dbGetQuery(con, "SELECT * FROM teaching")


