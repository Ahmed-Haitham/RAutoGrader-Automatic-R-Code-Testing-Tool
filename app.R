# Set language to English
Sys.setlocale(category = "LC_CTYPE", locale = "en_US.UTF-8")

# Specify required packages
required_packages <- c("dplyr", "ggplot2", "tidyr", "renv", "shiny", "shinyjs",
                       "Rcpp", "RSQLite", "R6", "DBI","openxlsx","tidyverse")

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
                   teach_id = NULL,
                   
                   initialize = function(test_topic, teach_id) {
                     
                     testid <- dbGetQuery(con, "SELECT test_id FROM tests ORDER BY test_id DESC LIMIT 1")
                     if (nrow(testid) == 0) {
                       self$test_id <- 1
                     } else {
                       self$test_id <- testid + 1
                     }
                     
                     self$test_topic <- test_topic
                     self$teach_id <- teach_id
                     
                     invisible(self)
                   },
                   
                   insert = function() {
                     query <- paste0("INSERT INTO tests (test_id, test_topic, teach_id) VALUES (", 
                                     self$test_id, ", '", self$test_topic, "', '", self$teach_id, "')")
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
                      group_id = NULL,
                      id_days = NULL,
                      schedule_id = NULL,
                      
                      initialize = function(user_id, course_id, group_id, id_days, 
                                            schedule_id) {
                        teach <- dbGetQuery(con, "SELECT teach_id FROM teaching ORDER BY teach_id DESC LIMIT 1")
                        if (nrow(teach) == 0) {
                          self$teach_id <- 1
                        } else {
                          self$teach_id <- teach + 1
                        }
                        
                        self$user_id <- user_id
                        self$course_id <- course_id
                        self$group_id <- group_id
                        self$id_days <- id_days
                        self$schedule_id <- schedule_id
                        
                        invisible(self)
                      },
                      
                      insert = function() {
                        query <- paste0("INSERT INTO teaching (teach_id, user_id, course_id, 
                                       group_id, id_days, schedule_id) VALUES (", 
                                        self$teach_id, ", ", self$user_id, ", '", self$course_id, "', '",
                                        self$group_id, "', '", self$id_days, "', '", self$schedule_id, "')")
                        
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
                         
                         scheduleid <- dbGetQuery(con, "SELECT schedule_id 
                                                  FROM schedules ORDER BY schedule_id DESC LIMIT 1")
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
                         
                         questid <- dbGetQuery(con, "SELECT question_id FROM questions 
                                               ORDER BY question_id DESC LIMIT 1")
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


#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------

# Shiny App
if (interactive()) {
  library("shiny")
  library("shinyjs")
  library("RSQLite")
  library(tidyverse)
  

  

  con <- dbConnect(RSQLite::SQLite(), "sqliteRAutoGrader.db")
  
  shinyApp(
    
    ui <- fluidPage(
      useShinyjs(),
      h4(id = 'h1home', style = 'text-align: center;', '- Automatic R Grader tool 2023 -'),
      h1(id = 'h1',style = "text-align: center;", "Welcome to the Automatic R grader management tool!"),
      hr(),
      
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
          SELECT DISTINCT test_id FROM tests
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
                tabPanel(title = "CREATE",
                         br(),
                         tabsetPanel(
                           tabPanel(
                             title = 'NEW COURSE',
                             h3(id = "h3newcourse", 'Create a new course'),
                             hr(),
                             p(id = "h5ncdescription", 'Are you teaching a new course? In this section you will be able 
                                to add a new course to your courses list.', br(),
                               'Please only use this option if the course you want add is NOT 
                               available in your current courses list.'),
                             textInput(inputId = 'newcourse', label = 'Course name: '),
                             selectInput(inputId = 'grouplist', label = 'Choose group: ', choices = 
                                           dbGetQuery(con, paste0("SELECT * FROM groups"))$group_name),
                             selectInput(inputId = 'dayclass', label = 'Teaching day: ', choices = 
                                           dbGetQuery(con, paste0("SELECT * FROM days_of_week"))$days
                             ),
                             
                             fluidRow(
                               column(
                                 width = 6,
                                 selectInput(inputId = 'starttime', label = 'Starts at: ', choices = 
                                               dbGetQuery(con, paste0("SELECT * FROM schedules"))$start_time
                                 )
                               ),
                               column(
                                 width = 6,
                                 selectInput(inputId = 'endtime', label = 'Until: ', choices = 
                                               dbGetQuery(con, paste0("SELECT * FROM schedules"))$end_time)
                               )
                             ),
                             # Button                                         
                             actionButton(inputId = 'newcourseteach', label = 'Save')
                             
                           ),
                           
                           tabPanel(
                             title = "NEW GROUP",
                             h3(id = "h3newgroup", 'Create a new group'),
                             hr(),
                             p(id = "h5ncdescription", 'In this section you will be able to
                                 add a new group to an existing course from your courses list.', br(),
                               'Please only use this option if the course already exists and you just 
                               want to assign a new group to it.'),
                             hr(),
                             selectInput(inputId = 'slctcourseBtn', label = 'Select the course: ',
                                         choices = dbGetQuery(con, paste0("
                                   SELECT DISTINCT course_name FROM courses JOIN teaching 
                                   ON teaching.course_id = courses.course_id JOIN users 
                                   ON users.user_id = teaching.user_id WHERE users.username = '", 
                                                                          input$username, "'"))$course_name),
                             fluidRow(
                               column(
                                 width = 6,
                                 selectInput(inputId = 'newgroupsBtn', label = 'Select the group to add: ',
                                             choices = dbGetQuery(con, paste0("SELECT group_name 
                                                                          FROM groups WHERE group_id>0"))$group_name)
                               ),
                               column(
                                 width = 6,
                                 selectInput(inputId = 'newdayBtn', label = 'Select the day: ', choices = 
                                               dbGetQuery(con, paste0("SELECT days FROM days_of_week 
                                                                      WHERE id_days>0"))$days)
                                 )
                             ),
                             fluidRow(
                               column(
                                 width = 6,
                                 selectInput(inputId = 'newstartBtn', label = 'Select the start time of the course: ', 
                                             choices = dbGetQuery(con, paste0("SELECT start_time FROM schedules 
                                                                      WHERE schedule_id>0"))$start_time)
                                 ),
                               column(
                                 width = 6,
                                 selectInput(inputId = 'newendBtn', label = 'Select the end time of the course: ', 
                                             choices = dbGetQuery(con, paste0("SELECT end_time FROM schedules 
                                                                      WHERE schedule_id>0"))$end_time)
                               )
                             ),
                             
                             #button
                             actionButton(inputId = 'newgroupBtn', label = 'Save')
                            
                           ),
                           
                           tabPanel(
                             title = "NEW TEST",
                             h3(id = "h3newtest", 'Create a new test'),
                             hr(),
                             p(id = "h5ncdescription", 'In this section you will be able to
                                 add a new test to and assigned it to the course and groups of your choice.', br(),
                               'You can assign one test to one course but the test can be assigned to many groups 
                               if applicable.'),
                             
                             # Creation form
                             fluidRow(
                               column(
                                 width = 6,
                                 selectInput(
                                   inputId = "courses2",
                                   label = "Select the course to assign the test: ",
                                   choices = dbGetQuery(con, paste0("
                                   SELECT DISTINCT course_name FROM courses JOIN teaching 
                                   ON teaching.course_id = courses.course_id JOIN users 
                                   ON users.user_id = teaching.user_id WHERE users.username = '", 
                                                                    input$username, "'"))$course_name
                                 )
                               ),
                               column(
                                 width = 6,
                                 checkboxGroupInput(
                                   inputId = 'groups2',
                                   label = 'Select the group(s) that had the test:',
                                   choices = NULL
                                 )
                               )
                             ),
                             p("There are 2 options to create your new test. Through an 
                               R file attachment or through manual input.",
                               tags$ul(
                                 tags$li(tags$b("Attachment: "), "you can attach an .R file with the 
                                 questions and answers. Please check the project description file to 
                                         make sure your attachment has all the required variables."),
                                 tags$li(tags$b("Manual input: "), "with this option, you will have to 
                                 provide the questions' description and the correct answer")
                               )
                             ),
                             br(),
                             selectInput(inputId = 'attachManualBtn', label = 'Please select your preferred option: ',
                                         choices = c('Attachment', 'Manual input')),
                             hr(),
                             textInput(inputId = "testtopic", label = "Test topic/name", value = ""),
                             fluidRow(
                               column(
                                 width = 6,
                                 numericInput(inputId = "questnumber", 
                                              label = "Question number: ", 
                                              value = 1, min = 1, max = 99)
                               ),
                               column(
                                 width = 6,
                                 numericInput(inputId = "questpoints", 
                                              label = "Points if correct ", 
                                              value = 0, min = 0, max = 500)
                               )
                             ),
                             fluidRow(
                               column(
                                 width = 6,
                                 textInput(inputId = "questanswer", 
                                           label = "Question answer: ", value = "")
                               ),
                               column(
                                 width = 6,
                                 textInput(inputId = "question", label = "Question description: ", value = "")
                               )
                             ),
                             
                             # Buttons
                             actionButton(inputId = "submitBtn", label = "Submit"),
                             actionButton(inputId = "addQuestionBtn", label = "Add another question"),
                             
                             #Attachment
                             fileInput(inputId = "attachField", label = "Choose the file to attach: ", accept = ".R"),
                             actionButton(inputId = "attachBtn", label = "Attach")
                           )
                         )
                ),
                
                # Tab 2
                tabPanel(title = "Tab 2",
                         hr(),
                         "Here you can see how many students took the test, ",
                         "and You can Export all students results into excel file.",
                         hr(),
                         # Add the download handler
                         downloadButton("download_excel", "Download Excel"),
                        
                         # Output table for test results
                         tableOutput("test_results_table")
                         
                         ),
                # Tab 3
                tabPanel(title = "Tab 3", "This is the content of Tab 3")
              )
            )
          )
        })
      })
      
      #Buttons
      # button CREATE - New course - aka 'Save' id= 'newcourseteach'
      observeEvent(input$newcourseteach, {
        # create new course using class courses$new(input$newcourse) and then $insert()
      })
      
      
      # Update the checkbox options (sidebar panel) whenever the courses selection changes
      observeEvent(input$courses, {
        # Reset the selected value of the tests dropdown if the previous selection is not available in the 
        #updated choices
        if (!is.null(input$tests) && !(input$tests %in% dbGetQuery(con, paste0("
                      SELECT DISTINCT test_topic 
                      FROM tests WHERE test_id IN (", paste(test_ids(), collapse = ","), ")"))$test_topic)) {
          updateSelectInput(session, "tests", selected = NULL)
        }
        
        # Get the user_id for the selected username
        user_id_val <- user_id()
        
        # Get the group names for the selected course
        group_names <- dbGetQuery(con, paste0("SELECT group_info FROM group_info_view WHERE user_id = '", 
                                              user_id_val, "'AND course_name = '", input$courses, "'"))$group_info
        
        # Update the checkbox options
        updateCheckboxGroupInput(session, "groups", choices = group_names)
      })
      
      # Update the drop-down options for groups (CREATE tab) whenever the courses selection changes
      observeEvent(input$courses2, {
        # Get the user_id for the selected username
        user_id_val2 <- user_id()
        
        # Get the group names for the selected course
        group_names2 <- dbGetQuery(con, paste0("SELECT group_info FROM group_info_view WHERE user_id = '", 
                                               user_id_val2, "'AND course_name = '", input$courses2, "'"))$group_info
        # Update the dropdown options
        updateCheckboxGroupInput(session, "groups2", choices = group_names2)
      })
      #
      
      
      # Update the test choices whenever the courses or groups selection changes
      observeEvent({input$courses; input$groups}, {
        # Get the user_id for the selected username
        user_id_val <- user_id()
        
        # Get the test topics for the selected course and groups
        test_topics <- dbGetQuery(con, paste0("SELECT tests.test_topic FROM tests 
        JOIN teaching ON tests.teach_id = teaching.teach_id JOIN group_info_view 
        ON teaching.teach_id = group_info_view.teach_id WHERE teaching.user_id = '", user_id_val, "'
      AND group_info_view.course_name = '", input$courses, "' 
      AND group_info_view.group_info = '", input$groups, "'"))$test_topic
        
        # Update the choices for the tests dropdown
        updateSelectInput(session, "tests", choices = test_topics)
      })
      
      
      
      ############ Tab 2 
      
      output$test_results_table <- renderTable({
        # Run the SQL query to fetch the test results
        query <- "
      SELECT  t.test_id,test_topic as 'Test Topic',st.studentCount as 'Submitted Students' FROM tests t ,
        (select  test_id, count( distinct student_id) as studentCount from submissions group by test_id ) as st
         where 
         st.test_id =  t.test_id
         and 
         t.test_id IN (
                       SELECT DISTINCT q.test_id
                      FROM questions q
                      )  and  active = 1 and  t.test_id !=0;
    "
        results <- dbGetQuery(con, query)
        
        
        
        
        # Return the results as a data frame
        results
      }  ) 
      
      
      ### Export :
      
      # Download handler for exporting data
      output$download_excel <- downloadHandler(
        filename = function() {
          # Set the filename for the exported Excel file
          paste0("test_results_", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          
          ## Select test and it's final grades
          query <- "Select t.test_id,t.test_topic,sum(q.score_points) finalGrade
                    from tests t,questions q
                    where t.test_id = q.test_id
                    and t.test_id != 0
                    group by test_topic"
          results_tests <- dbGetQuery(con, query)
          
        
          
          
          query <- "select test_id,student_id,sum(evaluation) score from submissions
                    group by test_id,student_id"
          results_students <- dbGetQuery(con, query)
          
          
          merged_df <- merge(results_tests, results_students, by = "test_id", all = TRUE)
          merged_df <- transform(merged_df, score_ratio = (score / finalGrade) * 100)
          
        
          merged_df <- merged_df %>%
            select(-test_id, -finalGrade, -score)
          
          # Pivot the data frame
          pivoted_df <- pivot_wider(
            data = merged_df,
            names_from = test_topic,
            values_from = score_ratio,
            values_fill = 0
          )
          
        
          
          # Write the data to an Excel file
          write.xlsx(pivoted_df, file, rowNames= FALSE)
        }
      )
      
      
      ###### End Tab2
      
      
      
    }
  )
}
