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
    select(key, jscore, quartile, top10, ascore, nrecords, doi, publisher3, publisher, 
           file, jacro, journal, author, url, year, volume, number, month, pages, 
           title, keywords, abstract, updated)
}
