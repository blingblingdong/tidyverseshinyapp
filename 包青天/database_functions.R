
fileConversion <- function(x){
  if (isS4(x)) {
    output = classConversion(x)
  } else {
    output = x
  }
}

upload <- function(x, projectURL, directory = "main", token = "none"){
  output = fileConversion(x)
  if (token == "none") {
    Response = httr::POST(paste0(projectURL,"/",directory,".json"), body = jsonlite::toJSON(output, auto_unbox = TRUE))
  } else {
    Response = httr::POST(paste0(projectURL,"/",directory,".json?auth=",token), body = jsonlite::toJSON(output, auto_unbox = TRUE))
  }
  return(paste0(directory,"/",httr::content(Response)$name))
}


download <- function(projectURL, fileName, secretKey = "none", token = "none", isClass = FALSE) {
  
  if (secretKey == "none" && token == "none") {
    urlPath = paste0(projectURL,"/",fileName,".json")
  } else if (token != "none") {
    urlPath = paste0(projectURL,"/",fileName,".json?auth=",token)
  } else {
    urlPath = paste0(projectURL,"/",fileName,".json?auth=",secretKey)
  }
  
  data = httr::GET(urlPath)
  
  if (is.null(jsonlite::fromJSON(httr::content(data,"text")))) warning("No data found at database location.")
  if (isClass) {
    retrievedData = httr::content(data,"text")
    tempPath = tempfile()
    writeBin(jsonlite::base64_dec(jsonlite::fromJSON(retrievedData)), tempPath)
    return(readRDS(tempPath))
  } else {
    return(jsonlite::fromJSON(httr::content(data,"text")))
  }
}



upload_row <- function(x, projectURL, fileName) {
  upload(x = x, projectURL = projectURL, directory = paste0("main/", fileName))
}

download_df <- function(projectURL, fileName) {
  rbindlist(
    download(projectURL = projectURL, fileName = paste0("main/", fileName))
  )
}