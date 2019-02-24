#' Create bib key 
#' 
#' Creates unique bibliography key to use in main bibliography data.frame
#'
#' @param publisher3 string abbreviated publisher name
#' @param author string authors as are
#' @param year integer
#' @param pages string
#'
#' @return string bibliography key
#' @export
#'
#' @examples
#' df1<-dfWoS[1,]
#' GetBibKey(df1$publisher3, df1$author, df1$year, df1$pages)
GetBibKey <- function(publisher3, author, year, pages) {
  key<-NA
  pattern <- "[^\\d]"
  tryCatch({
    if (is.null(pages)) {
      pages <- '000-000'
    } else {
      if (is.na(pages)) {
        pages <- '000-000'
      }
    }
    pages <- str_replace_all(pages, pattern = pattern, '')
    if (str_length(pages) > 8) {
      pages <- str_sub(pages, 1, 8)
    }
    (author <- str_split(author, "(?: And | and )") %>% unlist)
    author <- author[1]
    author <- str_split(author, ", ") %>% unlist
    author <- author[1]
    #replace all non alpha numeric, delete spaces as well
    if(is.na(year)){
      year<-year(Sys.Date())
    }
    key <- str_replace(author, pattern = "[^[:alnum:]]", "") %>% 
      paste(year, pages, sep = '') %>%
      #      iconv("UTF-8", "ASCII", sub = "") %>% 
      str_replace_all(pattern = "[^[:alnum:]]", "")
    key<-str_c(publisher3, key)    
  }, error = function(e) {
    cat(paste(author, year, pages, '\r\n'))
    cat(paste(e, '\r\n'))
  })
  key
}
#'Create unique keys
#'
#'Create unique keys for each bibliography item. Key format: publisher3 + author
#'family name + year + pages 
#'
#'@param  dfNewBib data.frame with new loaded bibliography records
#'
#'@return
#'@export
#'
#' @examples
#' dfNewBib<-dfLoadedBibs
#' nrow(dfNewBib)
#' createBibKeys(dfNewBib) %>% head
createBibKeys <- function(dfNewBib,  log_con=NULL, progress=NULL) {
  'Creating bib keys...' %>% 
    give_echo(log_con, T, progress)
  df_right<-d$libstat$jcitescores %>% 
    mutate(journal=title) %>% 
    select(journal, jacro, jscore)
  
  df <- left_join(dfNewBib, df_right, by = "journal") %>% 
    mutate(key =pmap_chr(list(publisher3, author, year, pages), GetBibKey)) %>% 
    distinct(key, .keep_all = T) %>% 
    arrange(journal, desc(year))
  paste('created', nrow(df), 'bib keys...') %>% 
    give_echo(log_con, F, progress)
  paste(nrow(dfNewBib)-nrow(df), 'duplicates deleted...') %>% 
    give_echo(log_con, F, progress)
  df
}
#' Correct author names
#'
#' @param df data.frame with bibliography records. All names will be represented
#'   in format "Family_Name, Given_Name1 Given_Name2"
#'
#' @return data.frame
#' @export
#'
#' @examples
#' df<-dfLoadedBibs
#' df$author
#' res<-correctAuthorName(df)
#' res$author
correctAuthorName <- function(df, log_con=NULL, progress=NULL) {
  'Swapping first and last names...\r\n' %>% 
    give_echo(log_con, T, progress)
  df1 <- filter(df, !str_detect(author, pattern = ','))
  df2 <- filter(df, str_detect(author, pattern = ','))
  df1<-df1 %>% 
    mutate(author=pmap_chr(list(author, publisher3), function(author, publisher3){
      ret<-""
      paste("correcting names:", publisher3, author, "\r\n") %>% 
        give_echo(log_con, F, progress)
      auths<-str_split(author, pattern = ' and ') %>% unlist
      if(publisher3=="wly"){
        auths<-map(auths, str_replace, " ", ", ") %>% unlist
        ret=glue_collapse(auths, sep = ' and ')
      }else{
        l1 <- str_split(auths, pattern = " ")
        l2<-map(l1, function(auth) {
          (auth <- str_trim(auth))
          (len <- length(auth))
          if (len > 1) {
            (lname <- auth[len])
            (fname<-paste(auth[-len], collapse=' '))
            (auth <- paste(lname, fname, sep = ', '))
          }
        })
        ret<-glue_collapse(l2, sep=' and ')
      }
      ret
    }))
  df <- bind_rows(df1, df2) %>% 
    mutate(author=map_chr(author, str_to_title)) %>% 
    mutate(author=map_chr(author, str_replace_all, pattern = " And ", replacement = " and "))
  print('Author`s names corrected.')
  df
}
#' Converts bibtex bibliography record to data.frame
#'
#' @param bib string with bibtex record
#'
#' @return data.frame
#' @export
#'
#' @examples
#' bib<-bibs[1]
bib_to_df <- function(bib,  log_con=NULL, progress=NULL) {
  print(bib)
  tryCatch({
    pattern<-",[ ]*([\\w]+)[ ]*="
    fields <- str_match_all(bib, pattern)[[1]][, 2]
    (fields <- tolower(fields))
    field_values <- str_replace_all(bib, pattern = pattern, '\t') %>%
      str_split('\t')
    (field_values <- field_values[[1]])
    field_values <- field_values[-1]
    pattern <- '[{}()"\']+'
    field_values <- str_replace_all(field_values, pattern = pattern, '')
    length(field_values) == length(fields)
    df <- bind_cols(fn = fields, fv = field_values)
    (filter <- which(df$fn == 'keywords'))
    df_keywords<- df[filter, 'fv']
    keywords <- pull(df_keywords, fv)
    (keywords<-paste(keywords, collapse = ' and '))
    tbl_keywords <- tibble(fn = 'keywords', fv = keywords)
    df <- bind_rows(filter(df, fn != 'keywords'), tbl_keywords)
    (fns <- pull(df, fn))
    df$fn<-NULL
    df <- as_tibble(t(df))
    names(df)<-fns
    df <- allVariablesToCharacters(df)
    (cols<-names(df))
    if (!('author' %in% cols)) {
      #print('deleting item without author...')
      #print(df)
      return(NA)
    }
  }, error = function(e) {
    cat(paste("Bib",bib, "caused error:", '\r\n'))
    cat(paste(e, '\r\n'))
  })
  fixed_names<-c("doi", "journal", "author", "year", "volume", "number", "month", "pages", "title", "keywords", "abstract")
  map(fixed_names, function(n){
    if(!n %in% names(df)){
      df[[n]]<<-NA
    }
  })
  if(is.na(df$title)){
    paste("No title:", glue_collapse(df$title, df$author, df$journal, sep=" "), "\r\n") %>% 
    give_echo(log_con, F, progress)
    return(NA)
  }else{
    df %<>% 
      mutate(journal=str_replace(journal, pattern="JCMS: ", replacement = ""))
    paste(df$title, "\r\n") %>% 
      give_echo(log_con, F, progress)
    df
  }
}
#'Bib to APA style
#'
#'Convert one row data frame bibliography to APA style record in HTML format
#'
#'@param tbl one row tibble
#'
#'@return string
#'@export
#'
#' @examples
#' tbl<-dfWoS[1,]
#' tbl
#' getAPAstyleBib(tbl)
getAPAstyleBib<-function(tbl)
{
  authors<-str_split(tbl["author"], " and ") %>% 
    unlist %>% 
    lapply(function(author){
      author_splitted<-str_split(author, ', ') %>% unlist
      paste(author_splitted[1], 
            lapply(author_splitted[-1], str_sub,1,1) 
            %>% paste('.', sep='') 
            %>% glue_collapse(' '))
    }) %>% glue_collapse(', ')
  
  paste(authors, ' (', tbl$year, '). ', 
        tbl$title, '. ',
        tbl$journal, ', ', tbl$volume, ', ', tbl$pages, '. ',
        ifelse(is.na(tbl$doi), '', tbl$doi), '.<br/>', 
        'Keywords: ', ifelse(is.na(tbl$keywords), 'None', tbl$keywords), '.<br/>', 
        'Abstract: ', ifelse(is.na(tbl$abstract), 'None', tbl$abstract), '.',
        sep='')
}
#' Select and order columns in data.frame
#'
#' @param df data.frame with bibliography records
#'
#' @return data.frame with ordered columns
#' @export
#'
#' @examples
#' sort_columns_in_wos_dataframe(dfWoS)
sort_columns_in_wos_dataframe<-function(df){
  df %>% 
    select(key, jscore, quartile, top10, ascore, nrecords, doi, publisher3,  
           file, jacro, journal, author, url, year, volume, number, month, pages, 
           title, keywords, abstract, updated)
}
