

delete_data_by_id <- function(database_url, path, data_id) {
  # 構建特定 ID 的 Firebase Realtime Database 路徑
  full_path <- paste0(database_url, path, "/", data_id, ".json")
  
  # 發送 DELETE 請求刪除資料
  delete_response <- httr::DELETE(url = full_path)
  
  if (httr::http_error(delete_response)) {
    warning("Failed to delete data.")
  } else {
    message("Data with ID '", data_id, "' deleted successfully.")
  }
}


delete_data_by_id(Sys.getenv("db_url"), "/main/記帳囉", "-Njf9T62hRbSedhA2oLn")