#' A function that inserts a question
#'
#' The function below insert a question to the test.
#'
#' @author Ahmed Abdelmaksoud
#' @return a dataframe.
#' @export  


insert = function() {
  
  # Connect to the SQLite database
  con <- dbConnect(RSQLite::SQLite(), "sqliteRAutoGrader.db")
  
  query <- paste0("INSERT INTO questions (question_id, test_id, question_number, 
                                       question_description, question_answer, score_points, type) VALUES (", 
                  self$question_id, ", ", self$test_id, ", '", self$question_number, "', '",
                  self$question_description, "', '", self$question_answer, "', '", 
                  self$score_points, "', '", self$type,"')")
  
  dbExecute(con, query)
}