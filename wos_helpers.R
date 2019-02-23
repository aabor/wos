#' Add tags to string
#'
#' Function adds string before and after the tagged_string
#'
#' @param tagged_string string
#' @param opening_tag string
#' @param ending_tag string
#'
#' @return string
#' @export
#'
#' @examples
#' #highlight words in HTML text string using tags
#' text<-"Empirically, firm-bank and executive- loan officer relationships are correlated, which suggests that the presence of personal lending relationships may confound previous studies of institutional lending relationships."
#' suggested_terms_vector<-c("relationships", "personal", "lending")
#' rx<-capture(group_or(suggested_terms_vector, group.all = T))
#' str_view_all(text, pattern=rx)
#' gsubf(rx, text, add_tags_color)
add_tags_color<-function(tagged_string){
  str_c("<mark>", 
        tagged_string, 
        "</mark>")
}  

#' Applying a function to a backreference within gsub
#'
#' R does not have the option of applying a function directly to a match via
#' gsub. You'll actually have to extract the match, transform the value, then
#' replace the value. This is relativaly easy with the regmatches function.
#'
#' @param pattern regex pattern
#' @param x text string
#' @param f function to apply to matched strings
#' @param ... additional parameters to pass in f
#'
#' @return
#' @export
#'
#' @examples
#' text<-c("aLexander", "konstantin")
#' gsubf("^al", text, toupper)
gsubf <- function(pattern, x, f, ...) {
  m <- gregexpr(pattern, x)
  regmatches(x, m) <- lapply(regmatches(x, m), f, ...)
  x   
}

#' Extract first letters
#'
#' Extracts first letters of words in text.
#' @param str string 
#'
#' @return string with first letters separated by space
#' @export
#'
#' @examples
#' str<-c("Alexander A", "Alex B")
#' str<-"Ruth V"
#' (res<-extractFirstLetters(str))
#' class(res)
extractFirstLetters<-function(str){
#  print(str)
  str %>% 
    str_split(" ") %>% 
    map(str_sub,1,1) %>% 
    map(glue_collapse, " ") %>% 
    unlist
}

#' Create an acronym
#'
#' Create an acronym from string, removing stop words like 'The' 'of' and so on
#'
#' @param str string
#'
#' @return string
#' @export
#'
#' @examples
#' str<-"The Quarterly Journal of Economics"
#' getAcro(str)
getAcro <- function(str) {
  #remove stop words  
  text_source <- VectorSource(str %>% tolower())
  corpus <- VCorpus(text_source)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  #convert VCorpus to string
  df <- data.frame(text=unlist(sapply(corpus, `[`, "content")), 
                   stringsAsFactors=F)
  str<-df$text
  #create an acronym
  str %>% str_squish() %>% 
    words() %>% 
    str_sub(1,1) %>% 
    glue_collapse(sep = '') %>% 
    str_to_upper()
}
#' Applying a function to a backreference within gsub
#' 
#' Helper function to use in general expressions
#'
#' @param pattern general expression pattern
#' @param x variable to apply function on  
#' @param f function to apply
#'
#' @return x variable transformed by function f
#' @export
#'
#' @examples
#' pattern <- capture(or(START, SPC, '&', '-') %R% WRD)
#' gsubf(pattern, "string To Lower", tolower)
gsubf <- function(pattern, x, f) {
  na_control <- x
  if (length(na_control) > 1) {
    na_control <- na_control[1]
  }
  if (!is.na(na_control)) {
    m <- gregexpr(pattern, x)
    regmatches(x, m) <- lapply(regmatches(x, m), f)
  }
  x
}

#' Remove empty strings from vector
#'
#' @param v vector with strings
#'
#' @return the same vector but without empty strings
#' @export
#'
#' @examples
#' v<-c("string 1", "", "string 3")
#' removeEmptyStringsFromCharacterVector(v)
removeEmptyStringsFromCharacterVector <- function(v) {
  v[v != ""]
}
#' All data.frame columns to character
#' 
#' Convert all columns in data frame in characted type vectors 
#'
#' @param df data.frame
#'
#' @return data.frame 
#' @export
#'
#' @examples
#' df<-data.frame(A=c(1,2,3), B=c("","B", NA))
#' str(df)
#' df<-allVariablesToCharacters(df)
#' str(df)
allVariablesToCharacters <- function(df) {
  df %>% mutate_all(funs(as.character), colnames(df))
}

#' Sort columns in data frame
#'
#' @param tbl data.frame or tibble
#' @param n_index_columns numeric index of the right column which remains in
#'   place
#' @param decreasing boolean indicate if sort is in decreaseing order default =
#'   True
#'
#' @return
#' @export
#'
#' @examples
#' df<-data.frame(A=rnorm(3), C=rnorm(3), B=rnorm(3))
#' df
#' sortVariables(df, 1, F)
sortVariables <- function(df, n_index_columns = 1, decreasing = T) {
  cols <- colnames(df)
  col_indexes <- cols[1:n_index_columns]
  col_values<-cols[(n_index_columns+1):length(cols)]
  cols <- c(col_indexes, sort(col_values, decreasing = decreasing))
  df[, cols]
}
#'Create a callback function to update progress.
#'
#'Each time this is called:
#'
#'- If `value` is NULL, it will move the progress bar 1/progress_job of the
#'remaining distance. If non-NULL, it will set the progress to that value.
#'
#'- It also accepts optional detail text.
#'
#'@param progress progress object create on server side
#'@param value integer or NULL
#'@param detail string message to show on the screen
#'
#'  Function uses two global variables "progress_job" and "progress_nstep" which
#'  indicate total number of steps and current position in the queue
#'  respectively.
#'
#'  It is possible to show message about small job steps without incresing
#'  progress bar value. Just leave value NULL.
#'
#'@return
#'@export
#'
#' @examples
updateProgress <- function(progress, value = NULL, detail = NULL, progress_job = 1) {
  if (is.null(value)) {
    value <- progress$getValue()
    value <- value + (progress$getMax() - value) / progress_job
  }
  msg<-paste("Step ", progress_nstep+1, ": ", detail, sep="")
  progress$set(value = value, detail = msg)
  print(msg)
}
#' Echo
#'
#' @param msg string echo message
#' @param log_con file log connection
#' @param new_step boolean increase progress count by one?
#' @param progress shiny app progress bar, default = NULL
#'
#' @return
#' @export
#'
#' @examples
#' give_echo("message")
give_echo<-function(msg, log_con=NULL, new_step=FALSE, progress=NULL){
  cat(msg)
  if(new_step){
    progress_nstep<<-progress_nstep + 1
  }
  if (!is.null(progress)) updateProgress(progress, detail = msg, progress_job)
  if(!is.null(log_con)) cat(msg, file = log_con)
}

#' Correct file path
#' 
#' E.g. change windows path to linux like
#' use manually
#'
#' @param file_name 
#'
#' @return
#' @export
#'
#' @examples
#' file_path<-"C:/WoS/pdf/snrKovaleva2017453484.pdf"
#' file_path<-"C:/WoS/pdf/elrChiu201610171025.pdf"
#' correct_file_path(file_path)
correct_file_path<-function(file_path){
  if(is.na(file_path))return(NA)
  file_name<-str_split(file_path, pattern = "/") %>% unlist %>% last
  file_name
}
# dfWoS<-dfWoS %>%
#   mutate(file=pmap_chr(list(file), correct_file_path))
# # 
# df<-dfWoS %>%
#   filter(!is.na(file)) %>%
#   select(key, file)
# df %>% tail
# files<-df %>% pull(file)
# files %>% tail
# file.exists(files)
# getwd()
# df %>%
#   filter(!is.na(file)) %>%
#   select(key, doi, file)
