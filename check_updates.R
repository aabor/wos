#' Create files data frame
#'
#' Function creates data frame consisting of file names and their last access
#' time from source and destination folders
#'
#' @param sourcePath string path to source code files
#' @param destinationPath string path to copy source code files
#'
#' @return data.frame
#' @export
#'
#' @examples
#' df<-get_updating_files_df(sourcePath, destinationPath)
#' df[8:15,]
get_updating_files_df<-function(sourcePath, destinationPath){
  rx <-"\\.r$|\\.R$|\\.md$|\\.yml$|Dockerfile"
  oldwd<-getwd()
  setwd(sourcePath)
  new_files<-dir(pattern = rx)
  dfNF<-tibble(file_name=new_files) %>% 
    mutate(last_update_new=file.mtime(new_files))
  setwd(destinationPath)
  old_files<-dir(pattern = rx)
  dfOF<-tibble(file_name=old_files) %>% 
    mutate(last_update_old=file.mtime(old_files))
  setwd(oldwd)
  dfNF %>% 
    left_join(dfOF, by="file_name")
}
#' Check shiny application for updates
#'
#' Function checks last change date of the all files relative to the project in
#' the source folder and controls if files in the destination folder have to be
#' updated
#'
#' @param sourcePath string path to source code files
#' @param destinationPath string path to copy source code files
#'
#' @return string with file names to update formated as it will be shown in
#'   modal dialog to the user
#' @export
#'
#' @examples
#' sourcePath<-g$sourcePath
#' destinationPath<-g$destinationPath
#' check_for_updates_shiny_app(sourcePath, destinationPath)
check_for_updates_shiny_app<-function(sourcePath, destinationPath, checkAndUpdate=F){
  dfUF<-get_updating_files_df(sourcePath, destinationPath)
  new_files_string<-dfUF %>% 
    filter(is.na(last_update_old)) %>% 
    pull(file_name) %>% 
    glue_collapse(sep=", ", last = " and ") %>% 
    as.character
  updating_files_string<-dfUF %>% 
    filter(last_update_new>last_update_old) %>% 
    pull(file_name) %>% 
    glue_collapse(sep=", ", last = " and ") %>% 
    as.character
  found_files_string<-""
  if(checkAndUpdate){
    if(length(new_files_string)>0){
      found_files_string<-paste("New files", new_files_string, "added.\r\n")
    }
    if(length(updating_files_string)>0){
      found_files_string<-paste(found_files_string, "Files", updating_files_string, "updated.")
    }
    
  }else{
    if(length(new_files_string)>0){
      found_files_string<-paste("New files", new_files_string, "will be added.\r\n")
    }
    if(length(updating_files_string)>0){
      found_files_string<-paste(found_files_string, "Files", updating_files_string, "will be updated.")
    }
  }
  found_files_string
}
#' Update Shiny application
#'
#' @param sourcePath string path to source code files
#' @param destinationPath string path to copy source code files
#'
#' @return string with copied file names  formated as it will be shown in modal
#'   dialog to the user
#' @export
#'
#' @examples
#' update_shiny_app(sourcePath, destinationPath)
update_shiny_app<-function(sourcePath, destinationPath){
  found_files_string<-check_for_updates_shiny_app(sourcePath, destinationPath, T)
  dfUF<-get_updating_files_df(sourcePath, destinationPath)
  new_files<-dfUF %>% 
    filter(is.na(last_update_old)) %>% 
    pull(file_name)
  updating_files<-dfUF %>% 
    filter(last_update_new>last_update_old) %>% 
    pull(file_name)
  found_files<-c(new_files, updating_files)
  if(length(found_files)>0){
    from<-file.path(sourcePath, found_files)
    to<-file.path(destinationPath, found_files)
    res<-sum(file.copy(from, to, overwrite = T, copy.date = F))
  }
  if(res!=length(found_files)){
    found_files_string<-"Error coping files..."
  }
  found_files_string
}

