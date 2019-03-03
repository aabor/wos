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
#' dfNewBib<-dfLoadedBibs %>% filter(journal=="The Quarterly Journal of Economics")
#' nrow(dfNewBib)
#' createBibKeys(dfNewBib) %>% head
createBibKeys <- function(dfNewBib, progress=NULL) {
  'Creating bib keys...' %>% 
    echo("createBibKeys", T, progress)
  df<-dfNewBib %>% 
    mutate(key=pmap_chr(list(publisher3, author, year, pages), GetBibKey)) %>% 
    distinct(key, .keep_all = T) %>% 
    arrange(journal, desc(year))
  paste('created', nrow(df), 'bib keys...') %>% 
    echo("createBibKeys", F, progress)
  paste(nrow(dfNewBib)-nrow(df), 'duplicates deleted...') %>% 
    echo("createBibKeys", F, progress)
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
correctAuthorName <- function(df, progress=NULL) {
  'Swapping first and last names...' %>% 
    echo("correctAuthorName", T, progress)
  df1 <- filter(df, !str_detect(author, pattern = ','))
  df2 <- filter(df, str_detect(author, pattern = ','))
  df1<-df1 %>% 
    mutate(author=pmap_chr(list(author, publisher3), function(author, publisher3){
      ret<-""
      paste("correcting names:", publisher3, author, "\r\n") %>% 
        echo("correctAuthorName", F, progress)
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
  'Author`s names corrected.' %>% 
    echo("correctAuthorName", F, progress)
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
#' idx=26
#' (bib<-bibs[idx])
#' bib_to_df(idx, bib, NULL)
bib_to_df <- function(bib) {
  tryCatch({
    bib_type<-""
    pattern<-",\\s*[\\w]+\\s*=\\s*\\{[[:alnum:][:punct:]-]+\\}"
    #str_view_all(bib, pattern)
    fields<-""
    fields_values<-""
    if(str_count(bib, pattern)>3){
      bib_type<-"curly_braces"
      pattern_fields<-',\\s*([\\w]+)\\s*=\\s*\\"?\\{'
      #str_view_all(bib, pattern_fields)
      pattern_values <- "=\\s*\\{(?:[^{}]*|(?R))*\\}" # matching balanced curly brackets
      fields <- str_match_all(bib, pattern_fields)[[1]][, 2]
      fields <- tolower(fields)
      fields_values<-regmatches(bib, gregexpr(pattern=pattern_values, bib, perl = TRUE)) %>% 
        unlist %>% 
        str_remove_all(pattern="[{}]") %>% 
        str_squish()
    }
    pattern<-',[ \\w]+\\s*=\\s*\\"[[:alnum:][:punct:]-]+\\"'
    if(str_count(bib, pattern)>3){
      bib_type<-"double_quotes"
      pattern_fields<-',\\s*([\\w]+)\\s*=\\s*\\"'
      pattern_values <- '=\\s*\\"(?:[^"]*|(?R))*\\"' # matching balanced curly brackets
      fields <- str_match_all(bib, pattern_fields)[[1]][, 2]
      fields <- tolower(fields)
      fields_values<-regmatches(bib, gregexpr(pattern=pattern_values, bib, perl = TRUE)) %>% 
        unlist %>% 
        str_squish() %>% 
        str_remove_all(pattern='^=') %>% 
        str_squish() %>% 
        str_remove_all(pattern='^"|"$') %>% 
        str_squish()
    }
    tibble(fs=fields, fvs=fields_values)
    # names(fields_values) <-fields
    # df<-bind_rows(fields_values)
    # df %<>% 
    #   mutate(keywords=str_replace_all(keywords, ", ", " and "))
    # if (!('author' %in% names(df))) {
    #   "No author in " %c% bib %>% 
    #   echo("bib_to_df", F, progress, level = "error")
    #   return(NA)
    # }
  }, error = function(e) {
    print(e)
    "Bib: " %c% bib %c% " caused error: " %c% e %>% 
      echo("bib_to_df", F, progress, level = "error")
  })
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
