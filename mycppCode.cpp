#include <Rcpp.h>
#include <sqlite3.h>
#include <string>

// [[Rcpp::depends(Rcpp)]]

// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
std::string getUserName(std::string student_id) {
  sqlite3* db;
  int rc = sqlite3_open("sqliteRAutoGrader.db", &db);

  std::string username;
  if (rc == SQLITE_OK) {
    std::string query = "SELECT firstName FROM students WHERE Student_Id = '" + student_id + "'";
    sqlite3_stmt* stmt;
    rc = sqlite3_prepare_v2(db, query.c_str(), -1, &stmt, NULL);
    if (rc == SQLITE_OK) {
      if (sqlite3_step(stmt) == SQLITE_ROW) {
        const unsigned char* result = sqlite3_column_text(stmt, 0);
        if (result != NULL) {
          username = std::string(reinterpret_cast<const char*>(result));
        }
      }
      sqlite3_finalize(stmt);
    }
    sqlite3_close(db);
  }

  return username;
}
