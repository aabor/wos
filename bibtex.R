#' Convert bibliography data.frame to HTML
#'
#' Creates bibtex text file and converts it in HTML representaion to render HTML
#' text in the browser window
#'
#' @param df data.frame with bibliography records
#'
#' @return
#' @export
#'
#' @examples
#' df<-dfWoSExport
#' convertDataFrameToBibTexHTML(df)
convertDataFrameToBibTexHTML<-function(df){
  convertDataFrameToBibTex(df) %>% 
    mutate(bib_item=str_replace_all(bib_item, ",\n", ",<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")) %>% 
    pull(bib_item) %>%     
    HTML
}
#' Convert data.frame to bibtex format
#'
#' @param df data.frame with bibliography records 
#'
#' @return
#' @export
#'
#' @examples
#' df<-dfWoS[1:3,]
#' res<-convertDataFrameToBibTex(df)
#' res%>% class
#' str(res)
#' res$bib_item
convertDataFrameToBibTex<-function(df){
  print('Start converting data frame...')
  df %<>% 
    select(key, doi, publisher, file, journal, author, url, year, volume, month, 
           pages, title, keywords, abstract)
  (cols <- names(df))
  (sep_cols <- paste('sep', cols, sep = '_'))
  sep_cols <- c('sep_first', sep_cols)
  (first_col <- first(sep_cols))
  (last_col <- last(sep_cols))
  n <- nrow(df)
  df_sep <- map(1:length(sep_cols), function(i) {
    cname<-sep_cols[i]
    if (cname == first_col) {
      df <- data.frame(cname = rep('@Article{', n))
    } else {
      if (cname == 'sep_key') {
        df <- data.frame(cname = rep(paste(',\n    ', cols[i], ' = {', sep = ''), n))
      } else {
        if (cname == last_col) {
          df <- data.frame(cname = rep('},\n}', n))
        } else {
          df <- data.frame(cname = rep(paste('},\n    ', cols[i], ' = {', sep = ''), n))
        }
      }
    }
    names(df) <- cname
    df
  }) %>% bind_cols
  df_bib <- bind_cols(df, df_sep)
  (id <- c(seq_along(cols), 0:length(cols) + 0.5))
  cols_ext <- c(cols, sep_cols)
  cols_ordered <- cols_ext[order(id)]
  df_bib <- df_bib[, cols_ordered]
  bibs <- unite(df_bib, bib_item, 1:length(cols_ordered), sep = '') %>% as.data.frame
  bibs
}
#' Export to JabRef (".bibtex")
#'
#' Convert and save bibliography data frame to JabRef ("\\.bibtex|\\.bib") file
#' @param df data frame with bibliography records
#' @param bibFile path to ".bib" file
#'
#' @return
#' @export
#'
#' @examples
#' df<-dfWoS[1,]
#' saveDataFrameToBibTeX("WoS.bib")
saveDataFrameToBibTeX <- function(df, bibFile) {
  print('Saving bib records...')
  bibs<-convertDataFrameToBibTex(df)
  write.table(bibs, file = bibFile, append = F, row.names = F, col.names = F, quote = F, sep = '')
  print('bibs saved.')
}
#' Export library to RMarkdown document
#'
#' Export bibliography records in plain text with RMarkdown tags. Paste and copy
#' manually exported records to RMardown research paper template. Template must
#' contain references to MS Word document styles, bibliography data base in
#' bibtex format, bibliography styles and (optionaly) author personal info.
#'
#' RMarkdown will load full references in bibtex format from "bibliography.bib"
#' file referenced in RMardown paper template.
#'
#'
#' @param df data frame with bibliography records
#' @param txtDF path to RMardown text file
#'
#' @return function does not return any value
#' @export
#'
#' @examples
#' path<-file.path(papers_path,g$paths$)
#' req(file.exists(path))
#' file_path<-file.path(path,"bibliography.rmd")
#' exportLibraryToRMarkdown(dfWoSExport, file_path)
exportLibraryToRMarkdown <- function(dfWoSExport, file_path) {
  if (file.exists(file_path)) file.remove(file_path)
  convertDataFrameToRMarkdown(dfWoSExport) %>% 
    write.table(file = file_path, append = F, row.names = F, col.names = F, quote = F, sep = '')
  print(paste('RMarkdown references created and saved to file', file_path))
}
#' Convert data.frame to RMarkdown references
#'
#' @param dfWoSExport data.frame with bibliography references
#'
#' @return vector of strings each containing one RMarkdown reference 
#' @export
#'
#' @examples
#' convertDataFrameToRMarkdown(dfWoSExport)
convertDataFrameToRMarkdown<-function(dfWoSExport){
  dfWoSExport %>% 
    rowwise() %>% 
    do({
      data.frame(text=get_rmarkdown_reference(.), stringsAsFactors = F)
    }) %>% 
    pull(text) 
}
#' Creates RMarkdown reference
#'
#' @param row data.frame consisting of one bibliography record
#'
#' @return string
#' @export
#'
#' @examples
#' get_rmarkdown_reference(dfWoSExport[1,])
get_rmarkdown_reference<-function(row){
  keywords<-str_replace_all(row$keywords, " and ", "; ")
  paste('[@', row$key, ']\r\n', 
        row$author, '\r\n',
        "Title: ", row$title, '\r\n',
        "Key words: ", keywords, '\r\n',
        sep='')
}
#' Convert data.frame to RMarkdown in HTML format
#'
#' @param dfWoSExport data.frame with bibliography references
#'
#' @return vector of strings each containing one RMarkdown reference formated as
#'   HTML document
#' @export
#'
#' @examples
#' convertDataFrameToRMarkdownHTML(dfWoSExport)
convertDataFrameToRMarkdownHTML<-function(dfWoSExport){
  convertDataFrameToRMarkdown(dfWoSExport) %>% 
    lapply(str_replace_all,"\r\n", "<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;") %>% 
    glue_collapse(sep="<br/>") %>% 
    HTML
}
#' Creates RMarkdown representation bibliography data.frame
#'
#' Function creates bibliography references in RMarkdown format. For each
#' reference the text chunks with highlighted searching terms are added from
#' full text documents. 
#' 
#' Text is converted to HTML format
#'
#' @param dfWoSExport data.frame with bibliography records
#' @param suggested_terms string with terms to find in full text separated by space 
#' @param progress shiny app progress indicator
#'
#' @return
#' @export
#'
#' @examples
convertDataFrameToRMarkdownHTMLWithTextChunks<-function(dfWoSExport, suggested_terms, progress=NULL){
  chunks<-get_text_chunks_from_selected_records(dfWoSExport, suggested_terms, progress)
  if(nrow(chunks)==0)return()
  chunks %>% 
    unite(item, reference, text_chunks) %>% 
    pull(item) %>% 
    lapply(str_replace_all,"\r\n", "<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;") %>% 
    glue_collapse(sep="<br/><br/>") %>% 
    HTML
}
