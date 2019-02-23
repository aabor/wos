#' Totals for library descriptive statistics
#'
#' @param df data.frame
#' @param start_year numeric
#'
#' @return data.frame
#' @export
#'
#' @examples
#' libraryDescriptivesTotals(df, 2012)
libraryDescriptivesTotals <- function(df, start_year = 2012) {
  df <- select(df, year, IsAbstract, IsFullText)
  df <- na.omit(df)
  totAll <- paste(sum(df$IsAbstract), sum(df$IsFullText), nrow(df), sep = '/')
  df_sh <- df %>%
    filter(year > start_year)
  totShort <- paste(sum(df_sh$IsAbstract), sum(df_sh$IsFullText), nrow(df_sh), sep = '/')
  data.frame(total = totShort, totalAll = totAll)
}
#' Generate journals descriptive statistics table
#'
#' @param dfWoSaug data.frame with bibliography records, augmented with columns
#'   which indicate existance of full text and abstract
#' @param start_year integer frontier year after which statistics start
#'
#' @return
#' @export
#'
#' @examples
#' libraryDescriptivesJournals(dfWoSaug)
libraryDescriptivesJournals <- function(dfWoSaug, start_year = 2012) {
  res_short <- dfWoSaug %>%
    filter(year > start_year) %>%
    group_by(journal) %>%
    summarise(abstract = sum(IsAbstract), 
              full_text = sum(IsFullText), 
              n = n(), 
              total = paste(abstract, full_text, n, sep = '/'),
              updated=max(updated)) %>% 
    select(journal, total, updated)
  res_all <- dfWoSaug %>%
    group_by(journal) %>%
    summarise(abstract = sum(IsAbstract), 
              full_text = sum(IsFullText), 
              n = n(), 
              totalAll = paste(abstract, full_text, n, sep = '/'))
  left_join(res_short, res_all, by = 'journal') %>% 
    select(journal, totalAll, updated) %>% 
    mutate(updated=format(updated, "%Y-%m-%d", tz=g$tz))
}
libraryDescriptivesYears <- function(df_journals, start_year = 2012) {
  res <- df_journals %>%
    filter(year > start_year) %>%
    group_by(year) %>%
    summarise(abstract = sum(IsAbstract), full_text = sum(IsFullText), n = n(), value = paste(abstract, full_text, n, sep = '/')) %>%
    arrange(desc(year))
  res <- res[, c('year', 'value')]
  res <- t(res)
  cols <- res[1,] %>% as.vector
  res <- res %>% as.data.frame
  cols <- paste('Y', cols, sep = '')
  colnames(res) <- cols
  res <- res[-1,]
  rownames(res) <- c('total')
  res <- allVariablesToCharacters(res)
  res
}
#'Library descriptive statistics
#'
#'
#'Computes general descriptive statistics for each journal, adds author scores
#'and verbal author scores
#'@param dfWoSaug data frame augmented with IsAbstract and IsFullText columns
#'@param start_year numeric the year from which to start computations
#'
#'@return
#'@export
#'
#' @examples
#' articlesDescriptives(dfWoSaug, start_year = 2012) %>% head
articlesDescriptives <- function(dfWoSaug, start_year = 2012) {
  jScores<-dfWoSaug %>% 
    select(journal, publisher3, jscore) %>% 
    distinct(journal, .keep_all = T)
  topResearcherJn <- dfWoSaug %>%
    filter(ascore > 1) %>%
    select(journal, ascore) %>%
    group_by(journal) %>%
    summarize(topR = sum(ascore))
  verbResearcherJn <- dfWoSaug %>%
    select(journal, nrecords) %>%
    filter(nrecords > 1) %>%
    group_by(journal) %>%
    summarize(verbR = sum(nrecords))
  journal_general_stat<-jScores %>% 
    left_join(topResearcherJn, by="journal") %>% 
    left_join(verbResearcherJn, by="journal")
  
  ret<-dfWoSaug %>%
    filter(year > start_year) %>%
    group_by(journal, year) %>%
    summarise(abstract = sum(IsAbstract), 
              full_text = sum(IsFullText), 
              n = n(), 
              value = paste(abstract, full_text, n, sep = '/')) %>% 
    select(journal, year, value) %>% 
    mutate(year=paste('Y', year, sep='')) %>% 
    left_join(journal_general_stat, by = 'journal') %>% 
    spread(year, value) %>% 
    sortVariables(5, decreasing = T) %>% 
    mutate_at(c("topR", "verbR"),funs(replace(., is.na(.), 0))) %>% 
    ungroup
  ret
}
#' Generate journal URL tag
#'
#' Use this tag in UI, for example in Data Table cells
#' 
#' @param jtitle string journal title
#'
#' @return html tag with journal title and weblink
#' @export
#'
#' @examples
#' df<-df_whole_library_stat[1,]
#' df_all<-df_whole_library_stat %>% filter(publisher3=="oup")
#' df<-df_all[1,]
#' (journal<-df$journal)
#' journal<-"Journal of Development Economics"
#' generate_journal_url_tag(journal)
generate_journal_url_tag <- function(journal) {
  #print(journal)
  url_tag<-journal
  url_path<-""
  j=wosdoc$journals %>% 
    filter(title==journal | mydbtitle==journal) %>% 
    select(title, publisher3, url, url_tocken)
  #print(j)
  if(j$publisher3 == "elr"){
    jtitle<-j$title
    jtitle<-str_replace_all(jtitle, pattern = "[[:punct:]]", replacement = "")
    jname<-str_to_lower(jtitle) %>% str_split(" ") %>% unlist %>% glue_collapse(sep="-")
    url_path<-file.path(j$url, jname)
  }
  if(j$publisher3 %in% c("wly", "emd", "snr", "tyr")) {
    url_path<-file.path(j$url, j$url_tocken)
  }
  if(j$publisher3=="oup"){
    url_path<-file.path(j$url, j$url_tocken, "issue")
  }
  if(j$publisher3=="aea"){
    url_path<-file.path(j$url, j$url_tocken, "issues")
  }
  
  if(j$publisher3=="cup"){
    jname<-str_to_lower(j$title) %>% str_split(" ") %>% unlist %>% glue_collapse(sep="-")
    url_path<-file.path(j$url, jname, "all-issues")
  }
  if(j$publisher3 %in% c("now", "ares")){
    url_path<-j$url
  }
  if(url_path!=""){
    url_tag<-a(journal, href=url_path, target="_blank") %>% as.character()
  }
  url_tag
}
#' Journal descriptives
#'
#' @param dfj dataframe with bibliogarphy records for only one journal
#' @param journal_name 
#'
#' @return
#' @export
#'
#' @examples
#' journals<-dfWoSaug$journal %>% unique()
#' journals[str_detect(journals, pattern="Finance")]
#' which(journals=="The Journal of Finance")
#' journal_name<-journals[14]
#' dfj <- filter(dfWoSaug, journal == journal_name)
#' dfj
#' dfj$number %>% unique
#' dfj$year %>% unique
#' journalStatistics(dfj, journal_name)
journalStatistics <- function(dfj, journal_name) {
  dfNumbers<-dfj %>% 
    filter(!is.na(number)) %>% 
    arrange(year, desc(number)) %>% 
    group_by(year, number) %>% 
    summarise(records=n()) %>% 
    ungroup() %>% 
    select(year, number) %>% 
    mutate(year=glue("Y{year}")) %>% 
    group_by(year) %>% 
    summarise(numbers=glue_collapse(number, sep=", ", last=" and ")) %>% 
    ungroup()
  
  res <- dfj %>%
    filter(year > 2012) %>%
    group_by(volume, year) %>%
    summarise(abstract = sum(IsAbstract), 
              full_text = sum(IsFullText), 
              n = n(), 
              value = paste(abstract, full_text, n, sep = '/')) %>% 
    ungroup() %>%
    arrange(year, desc(volume)) %>% 
    mutate(year=glue("Y{year}"))
  res <- res[, c('year', 'volume', 'value')]
  res <- mutate(res, vol_str = paste(volume, '(', value, ')', sep = ''))
  res$volume <- NULL
  res$value <- NULL
  
  years <- res %>% group_by(year) %>%
    summarise(n = n()) %>%
    collect %>% .[["year"]]
  journalYearStatistics <- function(df, selected_year) {
    filter(res, year == selected_year) %>%
      collect %>% .[['vol_str']] %>%
      paste(collapse = '; ')
  }
  year_stats <- lapply(1:length(years), function(i)
    journalYearStatistics(res, years[i])
  ) %>% unlist
  res <- bind_cols(data.frame(year = years), data.frame(year_stats)) %>%
    arrange(desc(year)) %>% as.tibble() %>% 
    map(as.character) %>% 
    bind_cols()
  colnames(res) <- c('year', journal_name)
  res %<>% left_join(dfNumbers, by ="year") %>% 
    mutate(numbers=ifelse(is.na(numbers), "", numbers))
  res<-res[,c(1,3,2)]
  res
}

#' Separate statistics for each journal in database
#'
#' @param dfWoSaug data.frame
#'
#' @return list of data.frames. Each data frame contains statistics for only one
#'   journal
#' @export
#'
#' @examples
#' dfWoSaug<-augment_wos_dataframe(dfWoS)
#' separateJournalsStatistics(dfWoSaug)
separateJournalsStatistics <- function(dfWoSaug, progress=NULL) {
  journals<-dfWoSaug$journal %>% unique()
  l<-dfWoSaug %>%
    nest(-journal) %>%
    mutate(jstat=pmap(list(data, journal), journalStatistics)) %>%
    pull(jstat) 
  names(l)<-journals
  l
}

