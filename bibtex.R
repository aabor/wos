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
    mutate(bib_item=str_replace_all(bib_item, "^\n|,\n", ",<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")) %>% 
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
#' df<-dfWoSExport
#' convertDataFrameToBibTex(df)
#' res%>% class
#' str(res)
#' res$bib_item
convertDataFrameToBibTex<-function(df){
  'Start converting data frame...' %>% 
    echo("convertDataFrameToBibTex", T)
  df %>% 
    select(key, doi, publisher3, file, journal, author, year, volume, month, 
           pages, title, keywords, abstract) %>% 
    gather(key="fs", value="fvs", doi:abstract) %>% 
    mutate(kv=fs %c% " = {" %c% fvs %c% "}") %>% 
    group_by(key) %>% 
    summarise(kvs=glue_collapse(kv, sep=",\n")) %>% 
    mutate(bib_item='\n@Article{' %c% key %c% ", " %c% kvs %c% "}") %>% 
    select(bib_item)
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
  'Saving bib records...' %>% 
    echo("saveDataFrameToBibTeX", F)
  bibs<-convertDataFrameToBibTex(df)
  write.table(bibs, file = bibFile, append = F, row.names = F, col.names = F, quote = F, sep = '')
  'bibs saved.' %>% 
    echo("saveDataFrameToBibTeX", F)
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
  paste('RMarkdown references created and saved to file', file_path) %>% 
    echo("exportLibraryToRMarkdown", T)
}
#' Convert data.frame to RMarkdown references
#'
#' @param dfWoSExport data.frame with bibliography references
#'
#' @return vector of strings each containing one RMarkdown reference 
#' @export
#'
#' @examples
#' convertDataFrameToRMarkdown(dfWoSExport[1:2,])
convertDataFrameToRMarkdown<-function(dfWoSExport){
  dfWoSExport %>% 
    mutate(text=pmap_chr(list(key, author, title, keywords), get_rmarkdown_reference)) %>%
    select(text) %>% 
    summarise(refs=glue_collapse(text, sep ="")) %>% 
    pull(refs)
}
#' Creates RMarkdown reference
#'
#' @param row data.frame consisting of one bibliography record
#'
#' @return string
#' @export
#'
#' @examples
#' row<-dfWoSExport[1,]
#' ch<-get_rmarkdown_reference(row$key, row$author, row$title, row$keywords)
#' ch %>% class
get_rmarkdown_reference<-function(key, author, title, keywords){
  keywords<-str_replace_all(keywords, " and ", "; ")
  '\r\n[@' %c%  key %c% ']\r\n' %c% author %c% '\r\n' %c% "Title: " %c%
    title %c% '\r\n' %c% "Key words: " %c% keywords %c% '\r\n'
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
    map(str_replace_all,"\r\n", "<br/>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;") %>% 
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
#' progress<-NULL
#' suggested_terms<-suggested_terms<-get_terms(dfWoSExport)
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
