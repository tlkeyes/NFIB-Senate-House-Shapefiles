### LIBRARIES
library(rvest)
library(dplyr)


# CREATE FUNCTIONS FOR USE ON BOTH SENATE AND HOUSE URL----

getFileNames <- function(url){
  # READ HTML
  page <- read_html(url)
  
  # EXTRACT TABLE FROM HTML
  raw_table <- page %>%
    html_nodes("table") %>%
    '[['(1) %>%
    html_table() %>%
    select(Name) %>%
    filter(grepl(".zip", Name))
  
  # PUT DATA INTO A LIST
  output <- raw_table %>%
    pull()
  
  return(output)
}

### DOWNLOAD ZIP FILES----
downloadZips <- function(url, file_destination, file_list){
  for(i in file_list){
    file_dest <- paste0(file_destination, i)
    file_url <- paste0(url, "/", i)
    download.file(url = file_url,
                  destfile = file_dest)
  }
}

### EXTRACT FILES FROM ZIP ARCHIVE----
extractFiles <- function(file_destination, extract_destination, file_list){
  for(i in file_list){
    shapefile_name <- paste0(gsub('.{4}$', '', i))
    
    pattern <- '\\.shp$|\\.shx$|\\.dbf$'
    export_files <- export_files <- grep(pattern, 
                                         unzip(zipfile = paste0(file_destination, i), list=TRUE)$Name, 
                                         ignore.case=TRUE, 
                                         value=TRUE)
    
    unzip(zipfile = paste0(file_destination, i),
          exdir = extract_destination,
          files = export_files)
  }
}
