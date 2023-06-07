#' Fetch tests from the database
#'
#' This function executes a query to fetch the tests from the database.
#'
#' @param con A database connection object.
#'
#' @return A data frame containing the fetched tests.
#' @export
#' @import RSQLite
fetch_tests <- function(con) {
  tests_query <- "SELECT test_id, test_topic FROM tests WHERE test_id IN (
                     SELECT DISTINCT q.test_id
                     FROM questions q
                 ) AND active = 1 AND test_id != 0"
  tests <- dbGetQuery(con, tests_query)
  return(tests)
}