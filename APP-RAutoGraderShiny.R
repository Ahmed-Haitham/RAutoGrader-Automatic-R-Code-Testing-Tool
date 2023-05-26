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
lapply(required_packages, library, character.only = TRUE)

#renv::restore()

# Connect to the SQLite database
con <- dbConnect(RSQLite::SQLite(), "sqliteRAutoGrader.db")

# -------------------------------------------------------------------------------------------#
# ----------------------- R6 classes --------------------------------------------------------#

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

# groups - Ok SQL working
groups <- R6Class("groups",
                  public = list(
                    group_id = NULL,
                    group_name = NULL,
                    
                    initialize = function(group_name) {
                      self$group_name <- group_name
                      groupid <- dbGetQuery(con, "SELECT group_id FROM groups ORDER BY group_id DESC LIMIT 1")
                      if (nrow(groupid) == 0) {
                        self$group_id <- 1
                      } else {
                        self$group_id <- groupid + 1
                      }
                      
                      invisible(self)
                    },
                    
                    insert = function() {
                      query <- paste0("INSERT INTO groups (group_id, group_name) VALUES (", 
                                      self$group_id, ", '", self$group_name, "')")
                      dbExecute(con, query)
                    }
                  )
)

# days_of_week - Ok SQL working
days_of_week <- R6Class("days_of_weeks",
                        public = list(
                          id_days = NULL,
                          days = NULL,
                          
                          initialize = function(days) {
                            self$days <- days
                            dayid <- dbGetQuery(con, "SELECT id_days FROM days_of_week ORDER BY id_days DESC LIMIT 1")
                            if (nrow(dayid) == 0) {
                              self$id_days <- 1
                            } else {
                              self$id_days <- dayid + 1
                            }
                            
                            invisible(self)
                          },
                          
                          insert = function() {
                            query <- paste0("INSERT INTO days_of_week (id_days, days) VALUES (", 
                                            self$id_days, ", '", self$days, "')")
                            dbExecute(con, query)
                          }
                        )
)

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

# teaching - ok, SQL connection working
teaching <- R6Class("teaching",
                    public = list(
                      teach_id = NULL,
                      user_id = NULL,
                      course_id = NULL,
                      test_id = NULL,
                      question_id = NULL,
                      group_id = NULL,
                      id_days = NULL,
                      schedule_id = NULL,
                      
                      initialize = function(user_id, course_id, test_id, question_id, group_id, id_days, 
                                            schedule_id) {
                        teach <- dbGetQuery(con, "SELECT teach_id FROM teaching ORDER BY teach_id DESC LIMIT 1")
                        if (nrow(teach) == 0) {
                          self$teach_id <- 1
                        } else {
                          self$teach_id <- teach + 1
                        }
                        
                        self$user_id <- user_id
                        self$course_id <- course_id
                        self$test_id <- test_id
                        self$question_id <- question_id
                        self$group_id <- group_id
                        self$id_days <- id_days
                        self$schedule_id <- schedule_id
                        
                        invisible(self)
                      },
                      
                      insert = function() {
                        query <- paste0("INSERT INTO teaching (teach_id, user_id, course_id, 
                                       test_id, question_id, group_id, id_days, schedule_id) VALUES (", 
                                        self$teach_id, ", ", self$user_id, ", '", self$course_id, "', '",
                                        self$test_id, "', '", self$question_id, "', '", self$group_id, "', '",
                                        self$id_days, "', '", self$schedule_id, "')")
                        
                        dbExecute(con, query)
                      }
                    )
)

# courses - Ok SQL working
courses <- R6Class("courses",
                   public = list(
                     course_id = NULL,
                     course_name = NULL,
                     initialize = function(course_name) {
                       self$course_name <- course_name
                       
                       courseid <- dbGetQuery(con, "SELECT course_id FROM courses ORDER BY course_id DESC LIMIT 1")
                       if (nrow(courseid) == 0) {
                         self$course_id <- 1
                       } else {
                         self$course_id <- courseid + 1
                       }
                       
                       
                       invisible(self)
                     },
                     
                     insert = function() {
                       query <- paste0("INSERT INTO courses (course_id, course_name) VALUES (", 
                                       self$course_id, ", '", self$course_name, "')")
                       
                       dbExecute(con, query)
                     }
                   )
)
# schedules - Ok SQL working
schedules <- R6Class("schedules",
                     public = list(
                       schedule_id = NULL,
                       start_time = "00:00",
                       end_time = "17:30",
                       initialize = function(start_time, end_time) {
                         self$start_time <- start_time
                         self$end_time <- end_time
                         
                         scheduleid <- dbGetQuery(con, "SELECT schedule_id FROM schedules ORDER BY schedule_id DESC LIMIT 1")
                         if (nrow(scheduleid) == 0) {
                           self$schedule_id <- 1
                         } else {
                           self$schedule_id <- scheduleid + 1
                         }
                         
                         invisible(self)
                       },
                       
                       insert = function() {
                         query <- paste0("INSERT INTO schedules (schedule_id, start_time, end_time) VALUES (",
                                         self$schedule_id, ", '", self$start_time, "', '", self$end_time, "')")
                         
                         dbExecute(con, query)
                       }
                     )
)

# questions - Ok SQL connection working
questions <- R6Class("questions",
                     public = list(
                       question_id = NULL,
                       test_id = NULL,
                       question_number = NULL,
                       question_description = NULL,
                       question_answer = NULL,
                       answers_filepath = "None",
                       
                       initialize = function(test_id, question_number, question_description,
                                             question_answer, answers_filepath = "None") {
                         self$test_id <- test_id
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

# ------------------------ TEST R6 CLASSES - all ok -------------------------------------------#
# tested - ok
test1 <- users$new("DQN")
test1$insert()
dbGetQuery(con, "SELECT * FROM users")

# Create the groups
# tested - ok 
test2 <- groups$new("gr 11")
test2$insert()
dbGetQuery(con, "SELECT * FROM groups")

# create new course
# tested - ok
test3 <- courses$new("Adv. Econometrics")
test3$insert()
dbGetQuery(con, "SELECT * FROM courses")

# create tests
# tested - ok
test4 <- tests$new("Shiny I")
test4$insert()
dbGetQuery(con, "SELECT * FROM tests")

# Create questions
# tested - ok
test5 <- questions$new(3,1, "choose the correct answer:", "vector")
test5$insert()
dbGetQuery(con, "SELECT * FROM questions")

# create new days
# tested - ok
test6 <- days_of_week$new("testing")
test6$insert()
dbGetQuery(con, "SELECT * FROM days_of_week")

# create new schedule
# tested - ok
test7 <- schedules$new("20:15", "21:45")
test7$insert()
dbGetQuery(con, "SELECT * FROM schedules")

# Create the teaching relationship - table
# tested - ok 
test6 <- teaching$new(5,3,3,5,12,8,8)
test6$insert()
dbGetQuery(con, "SELECT * FROM teaching")

#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------

# Shiny App
# Shiny App
if (interactive()) {
  library("shiny")
  library("shinyjs")
  library("RSQLite")
  con <- dbConnect(RSQLite::SQLite(), "sqliteRAutoGrader.db")
  
  shinyApp(
    
    ui <- fluidPage(
      useShinyjs(),
      h1(id = 'h1', "Welcome to the Automatic R grader management tool!"),
      
      # Dropdown to select the username
      selectInput(
        inputId = "username",
        label = "Select your username:",
        choices = c(dbGetQuery(con, "SELECT DISTINCT username FROM users")$username),
        selected = "-- Please select your username"
      ),
      
      # Button to continue
      actionButton(
        inputId = "continueBtn",
        label = "Continue",
        disabled = TRUE
      ),
      
      # Hyperlink to register
      p(HTML("<p id='text-register'>Are you new? 
         <a href='#' id='register-link'>Register here</a></p>")),
      
      # Tabset with 3 tabs, will show after clicking 'Continue'
      uiOutput("tabset_ui")
    ),
    
    # Define the server
    server <- function(input, output, session) {
      
      # Reactive expression to get the user_id from the selected username
      user_id <- reactive({
        if (input$username == "-- Please select your username") {
          0
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
        print(input$username)
      })
      
      # Get the test IDs for the selected course
      test_ids <- reactive({
        req(input$courses)
        
        course_id <- dbGetQuery(con, paste0("
          SELECT DISTINCT course_id FROM courses WHERE course_name = '",
                                            input$courses, "'"))$course_id
        
        teach_ids <- dbGetQuery(con, paste0("
          SELECT DISTINCT teach_id FROM teaching
          WHERE user_id = (SELECT user_id FROM users
                            WHERE username = '", input$username, "'
          ) AND course_id = '", course_id, "'"))$teach_id
        
        dbGetQuery(con, paste0("
          SELECT DISTINCT test_id FROM teaching
          WHERE teach_id IN (", paste(teach_ids, collapse = ","), ")"))$test_id
      })
      
      # When the continue button is clicked, show the tabset
      observeEvent(input$continueBtn, {
        hide("h1")
        hide("text-register")
        hide("register-link")
        hide("username")
        hide("continueBtn")
        output$tabset_ui <- renderUI({
          sidebarLayout(
            sidebarPanel(
              h3(id = "welcome-msg", "Welcome, ", input$username),
              selectInput(
                inputId = "courses",
                label = "Select one of your courses:",
                choices = dbGetQuery(con, paste0("
                      SELECT DISTINCT course_name FROM courses 
                      JOIN teaching ON teaching.course_id = courses.course_id
                      JOIN users ON users.user_id = teaching.user_id
                      WHERE users.username = '", input$username, "'"
                ))$course_name
              ),
              checkboxGroupInput(
                inputId = 'groups',
                label = 'Select the group(s) for analysis:',
                choices = NULL
              ),
              selectInput(
                inputId = "tests",
                label = "Select the test:",
                choices = NULL
              )
            ),
            mainPanel(
              tabsetPanel(
                id = "tabset",
                # Tab 1
                tabPanel(title = "Tab 1", "This is the content of Tab 1"),
                # Tab 2
                tabPanel(title = "Tab 2", "This is the content of Tab 2"),
                # Tab 3
                tabPanel(title = "Tab 3", "This is the content of Tab 3")
              )
            )
          )
        })
      })
      
      # Update the checkbox options whenever the courses selection changes
      observeEvent(input$courses, {
        # Reset the selected value of the tests dropdown if the previous selection is not available in the updated choices
        if (!is.null(input$tests) && !(input$tests %in% dbGetQuery(con, paste0("
                      SELECT DISTINCT test_topic FROM tests WHERE test_id IN (",
                                                                               paste(test_ids(), collapse = ","), ")"))$test_topic)) {
          updateSelectInput(session, "tests", selected = NULL)
        }
        
        # Get the user_id for the selected username
        user_id_val <- user_id()
        
        # Get the group names for the selected course
        group_names <- dbGetQuery(con, paste0("
          SELECT DISTINCT groups.group_name FROM courses JOIN teaching 
          ON courses.course_id = teaching.course_id JOIN groups ON 
          teaching.group_id = groups.group_id WHERE teaching.user_id = '", user_id_val,
                                              "' AND teaching.course_id IN (SELECT DISTINCT courses.course_id FROM courses 
          JOIN teaching ON courses.course_id = teaching.course_id WHERE teaching.user_id = '",
                                              user_id_val, "' AND courses.course_name = '", input$courses, "')"))$group_name
        
        # Update the checkbox options
        updateCheckboxGroupInput(session, "groups", choices = group_names)
      })
      
      # Update the test choices whenever the courses or groups selection changes
      observeEvent({input$courses; input$groups}, {
        # Get the user_id for the selected username
        user_id_val <- user_id()
        
        # Get the test topics for the selected course and groups
        test_topics <- dbGetQuery(con, paste0("
          SELECT DISTINCT tests.test_topic FROM tests JOIN teaching 
          ON tests.test_id = teaching.test_id JOIN groups ON 
          teaching.group_id = groups.group_id WHERE teaching.user_id = '", user_id_val,
                                              "' AND teaching.course_id IN (SELECT DISTINCT courses.course_id FROM courses 
          JOIN teaching ON courses.course_id = teaching.course_id WHERE teaching.user_id = '",
                                              user_id_val, "' AND courses.course_name = '", input$courses, "')
          AND groups.group_name IN ('", paste(input$groups, collapse = "','"), "')"))$test_topic
        
        # Update the choices for the tests dropdown
        updateSelectInput(session, "tests", choices = test_topics)
      })
      
    }
  )
}

# Run the app
shinyApp(ui, server)

