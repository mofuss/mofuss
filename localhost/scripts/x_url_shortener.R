library(httr)
library(jsonlite)

ShortURL <- function(link, linkPreview = FALSE) {
  
  api <- if(linkPreview) {"http://v.gd/create.php?format=json"} else {"http://is.gd/create.php?format=json"}
  query <- list(url = link)
  request <- httr::GET(api, query = query)
  content <- httr::content(request, as = "text", encoding = "utf-8")
  result <- jsonlite::fromJSON(content)
  
  return(result)
  
}

# 
ShortURL("https://drive.google.com/drive/folders/1H6OqxALkgcuTzlLcCL32sqmEoB5LJGe5?usp=sharing")

#
ShortURL("https://drive.google.com/drive/folders/19Wklp0OcKLCxFZdMTlK2UOogpkQQng2w?usp=share_link")
