# Set language to English
Sys.setlocale(category = "LC_CTYPE", locale = "en_US.UTF-8")

# Specify required packages
required_packages <- c("dplyr", "ggplot2", "tidyr", "renv", "shiny", "shinyjs",
                       "Rcpp", "RSQLite", "R6", "DBI", "shinyWidgets")

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


library(shiny)
library(shinyjs)
library(DBI)
library(RSQLite)
library(shinyWidgets)
library(myPackage)
library(Rcpp)
Sys.setenv("PKG_LIBS" = "-lsqlite3")
sourceCpp("mycppCode.cpp")




loginUI <- fluidPage(
  useShinyjs(),
  fluidRow(
    column(width = 4),
    column(
      width = 4,
      id = "login_panel",
      wellPanel(
        textInput(inputId = "student_id", label = "Student ID"),
        passwordInput(inputId = "password", label = "Password"),
        br(),
        actionButton(inputId = "login_btn", label = "Log in")
      )
    ),
    column(width = 4)
  ),
  fluidRow(
    column(width = 12,
           id = "welcome_panel",
           style = "display:none;",
           h1(id = "welcome_msg"),
           uiOutput("test_links")
    )
  ),
  fluidRow(
    column(width = 12,
           id = "questions_panel",
           style = "display:none;",
           uiOutput("questions_output"),
           actionButton(inputId = "submit_btn", label = "Submit Answers")
    )
  )
)


loginServer <- function(input, output, session) {
  # Connect to the SQLite database
  con <- dbConnect(RSQLite::SQLite(), "sqliteRAutoGrader.db")
  
  # Clean up the database connection when the app is closed
  onSessionEnded(function() {
    dbDisconnect(con)
  }, session)
  
  observeEvent(input$login_btn, {
    # Get the entered student ID and password
    student_id <- input$student_id
    password <- input$password
    
    # Query the database to check user credentials
    query <- paste0("SELECT COUNT(*) FROM students WHERE Student_Id = '", student_id, "' AND Password = '", password, "'")
    result <- dbGetQuery(con, query)
    valid_credentials <- result[[1]] > 0
    
    # If valid credentials, show the welcome page
    if (valid_credentials) {
      #CPP code
      username <- getUserName(student_id)
     
      
     
      
      # Hide the login panel
      shinyjs::hide("login_panel")
      
      # Show the welcome panel and set the welcome message
      shinyjs::show("welcome_panel")
      shinyjs::html("welcome_msg", paste0("Welcome, ", username, "!"))
      
      # Query the database to fetch the tests

      tests  <- fetch_tests(con)
      
      # Render the test links dynamically
      output$test_links <- renderUI({
        test_links <- lapply(1:nrow(tests), function(i) {
          test_id <- tests$test_id[i]
          test_topic <- tests$test_topic[i]

          actionLink(
            inputId = paste0("test_", test_id),
            label = test_topic,
            style = "margin-right: 10px;",
            onclick = sprintf("Shiny.setInputValue('selected_test', '%s')", test_id)  # Set selected_test input value to test_id
          )
        })
        do.call(tagList, test_links)
      })
      
     
      
      # Handle test link clicks
      observeEvent(input$selected_test, {
        # Get the test ID from the input value
        test_id <- as.numeric(input$selected_test)
        
        shinyjs::hide("login_panel")
        # Query the database to fetch questions for the selected test
        questions_query <- paste0("SELECT question_id, question_description,type  FROM questions WHERE test_id = ", test_id)
        questions <- dbGetQuery(con, questions_query)
        
       ### prepare the UI:
          # Show the questions and input fields dynamically
          output$questions_output <- renderUI({
            inputs <- lapply(1:nrow(questions), function(i) {
              question_id <- questions$question_id[i]
              question_description <- questions$question_description[i]
              question_type <- questions$type[i]
              
              if (question_type == "MCQ") {
                choices_query <- paste0("SELECT Answer_sympol,Answer_description FROM choices WHERE question_id = ", question_id)
                choices <- dbGetQuery(con, choices_query)
                
                
                
                # Create a list of radio buttons for MCQ questions
                mcq_inputs <- lapply(1:nrow(choices), function(j) {
                  choice_sympol <- as.character(choices$Answer_sympol[j])
                  choice_description <- choices$Answer_description[j]
                  
                  radioButtons(
                    inputId = paste0("answer_", question_id),
                    label = choice_description,
                    choices = choice_sympol,
                    selected = NULL
                  )
                  
                })
                
                tagList(
                  h4(paste0("Q", question_id, ") ", question_description)),
                  mcq_inputs
                )
              } else {
              
              tagList(
                h4(paste0("Q", question_id, ") ", question_description)),
                textInput(inputId = paste0("answer_", question_id), label = "Answer")
              )
              }
              
            })
            do.call(tagList, inputs)
          })
        
        # Replace the entire user interface with the new UI
          
          #Hide current page 
          shinyjs::hide("welcome_panel")
          
          # Show the welcome panel and set the welcome message
          shinyjs::show("questions_panel")
      })
      
      
      # Handle submit button click
      observeEvent(input$submit_btn, {
        
        # Get the selected test ID
        test_id <- as.numeric(input$selected_test)
        
        #get dataset path 
        dataset_path_query <- paste0("SELECT datasetPath FROM tests WHERE test_id = ", test_id)
        dataset_path <- dbGetQuery(con, dataset_path_query)
        
        # Query the database to fetch questions for the selected test
        questions_query <- paste0("SELECT question_id, question_description,question_answer,score_points,type FROM questions WHERE test_id = ", test_id)
        questions <- dbGetQuery(con, questions_query)
        
      
        ##Saving the result
        for (i in 1:nrow(questions)) {
          question_id <- questions$question_id[i]
          user_answer <- input[[paste0("answer_", question_id)]]
          # question write answer 
          correct_answer <-questions$question_answer[i]
          
          print(user_answer)
          
          ## Here we do the evualation later i will move it to function or a package or maybe cpp 
          
          if(questions$type[i] == 'TXT') {
            
            #question dataset
            dataset <- read.csv(as.character(dataset_path))
            

            result <- tryCatch(
              {
                eval(parse(text = correct_answer))
              },
              error = function(err) {
                 -1
              }
            )
            #Serilaize correct answer
            serialized_correct_answer <- serialize(result, NULL)
            
            
            result <- tryCatch(
              {
              eval(parse(text = user_answer))
              },
              error = function(err) {
                -1
              }
            )
            #Serilaize user answer
            serialized_user_answer <- serialize(result, NULL)
            
            if( identical(serialized_user_answer, serialized_correct_answer)) {
              
              evaluation_score = questions$score_points[i]
            } else 
            {
              evaluation_score = 0;
            }
            
          } else {
            
            ## normal TXT answer
            if( correct_answer == user_answer  ) {
              
              evaluation_score = questions$score_points[i]
            } else 
            {
              evaluation_score = 0;
            }
            
          }
            
            
 
          ## end of evualtation
          query <- paste0("INSERT INTO submissions (test_id, question_id, student_id, answer,evaluation) VALUES (",
                          test_id, ", ", question_id, ", '", student_id, "', '", user_answer, "', ",evaluation_score," )")
          
          # Execute the SQL query to insert the answer
          dbExecute(con, query)
        }
        
        
        # Show a thank you message
        showModal(modalDialog(
          title = "Thank You!",
          paste0("Thanks for submitting your answers, ", username, "!"),
          easyClose = TRUE
        ))
        
        ## go back to tests
        #Hide current page 
        shinyjs::hide("questions_panel") #questions_panel
        
        # Show the welcome panel and set the welcome message
        shinyjs::show("welcome_panel") # welcome_panel
      })
      
      
    }
    # Show an error message or perform other actions for invalid credentials
    else {
      showModal(modalDialog(
        title = "Login Failed",
        "Invalid student ID or password. Please try again.",
        easyClose = TRUE
      ))
    }
    
  })
}



# Run the shiny app
shinyApp(ui = loginUI, server = loginServer)

