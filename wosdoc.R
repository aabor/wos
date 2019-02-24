#' Create wos document
#'
#' @return
#' @export
#'
#' @examples
create_wosdoc<-function(){
  d<-list()
  d$select<-list()
  d$paths<-list()
  d$filters<-list()
  d$filters$year<-list()
  d$filters$jscore<-list()
  d$filters$ascore<-list()
  d$filters$nrecord<-list()
  d$libstat<-list()
  d$authors<-list()
  d$colnames<-list()
  d$paths$papers<-dir(g$paths$papers)
  d$select$paper<-""
  d$select$fileFormat<-".rds"
  d$select$exportFolder<-""
  d$journals<-g$specs$journals %>% 
    left_join(g$specs$publishers, by="publisher3") %>% 
    as_tibble()
  d$libstat$jcitescores<-createCiteScoreDF(g$paths$jscore)
  d$authors$df<-createResearcherDF()
  d
}
#' Update wos document
#'
#' @param d 
#' @param WoSDT 
#'
#' @return
#' @export
#'
#' @examples
update_wosdoc<-function(d, dfWoS){
  d$filters$years<-dfWoS$year %>% unique() 
  d$filters$year$min<-min(d$filters$years)
  d$filters$year$max<-max(d$filters$years)
  
  d$filters$jscores<-dfWoS$jscore %>% unique()
  d$filters$jscore$min<-min(d$filters$jscores, na.rm = T)
  d$filters$jscore$max<-max(d$filters$jscores, na.rm = T)
  
  d$filters$ascores<-dfWoS$ascore %>% unique()
  d$filters$ascore$min<-min(d$filters$ascores, na.rm = T)
  d$filters$ascore$max<-max(d$filters$ascores, na.rm = T)
  
  d$filters$nrecords<-dfWoS$nrecords %>% unique()
  d$filters$nrecord$min<-min(d$filters$nrecords, na.rm = T)
  d$filters$nrecord$max<-max(d$filters$nrecords, na.rm = T)
  
  d$select$journals<-c("All", g$specs$journals$title)
  d$select$publishers<-c("All", g$specs$publishers$publisher3)
  d$select$authors$all<-dfWoS$author %>% 
    unique() %>% 
    map(str_split, " and ") %>% 
    map(unlist) %>% 
    unlist %>% 
    unique()
  d$colnames$cjcs<-names(d$libstat$jcitescores)
  d$colnames$cjcs_citescores<-cjcs[str_detect(cjcs, "^sc")][1:3]
  d$colnames$ssci_dt<-c(g$colnames$cjcs_main, d$colnames$cjcs_citescores, "url_tocken")
  d$colnames$ssci_dt_shown<-c("Journal", "J", "Pub", "Q", "Perc", d$colnames$cjcs_citescores, "Tocken")
  
  #library statistics
  compute_research_papers_statistics(dfWoS)
  
  d
}
#' Create cite scores data frame
#'
#' CiteScore metrics downloaded from Scopus for recent years
#' https://www.scopus.com/sources

#'
#' @param xlsxSelectedJournals string path to Excel book with manually selected journals
#' @param jscore_path string path to folder with cite score metrics in .txt format for each year
#' @param log_con log file connection
#' @param progress shiny progress bar object
#'
#' @return data.frame with structure  "title"      "publisher"  "quartile"
#'   "percentile" "issn"       "sc2016"     "sc2015" "sc2014"     "sc2013"
#'   "sc2012"     "sc2011"
#' @export
#'
#' @examples
#' progress<-NULL
#' log_con<-NULL
#' sc<-createCiteScoreDF(g$paths$jscore_path) %>% head
createCiteScoreDF <- function(jscore_path, log_con=NULL, progress=NULL) {
  myjournals<-d$journals
  files<-dir(g$paths$jscore, pattern = "\\.txt$", full.names = T, recursive = F)
  if(length(files)==0)return(NULL)
  files<-sort(files, decreasing = T)
  sheets<-tools::file_path_sans_ext(basename(files))
  col_metrics<-tibble(sheet=sheets) %>% 
    filter(str_detect(sheets, 'All')) %>% 
    mutate(sheet=str_replace(sheet, pattern = ' All', '')) %>% 
    arrange(desc(sheet)) %>% 
    pull(sheet) %>% 
    paste('sc', ., sep = '') 
  arrange_quo = parse_quosure(col_metrics[1]) # refernce column by its string name "col_name"
  last_col_quo = parse_quosure(col_metrics[length(col_metrics)]) # refernce column by its string name "col_name"
  'CiteScores metrics found. Extracting metrics...\r\n' %>% 
    give_echo(log_con, F, progress)
  AllCiteScores<-map(files,function(file) {
    file_name<-basename(file)
    year<-str_sub(file_name, 0,4) %>% as.integer()
    year_processed<-paste("sc", year, sep="")
    cat(paste("reading", file_name, "\r\n"))
    if(year>=2017)
    {
      df<-read_cite_score_2017(file)%>% 
        mutate(year=year_processed)
    }else{
      df<-read_tsv(file, skip = 1, progress = T, trim_ws = T, col_types = cols()) %>% 
        select(Title, CiteScore, Quartile,`Top 10% (CiteScore Percentile)`) %>% 
        distinct(Title, .keep_all = T) %>% 
        mutate(year=year_processed)
      names(df)<-c("title", "citescore", "quartile", "top10", "year")
    }
    paste(nrow(df), "journals parsed\r\n") %>% 
      give_echo(log_con, F, progress)
    paste("file", file_name, "loaded...", "parsed", nrow(df), "journals\r\n") %>% 
      give_echo(log_con, T, progress)
    df
  }) %>% 
    reduce(bind_rows)
  'cite score statistics loaded...\r\n' %>% 
    give_echo(log_con, T, progress)
  
  dfMyJournalsFull<-left_join(myjournals, AllCiteScores, by = 'title') %>% 
    mutate(title=mydbtitle) %>% 
    select(-mydbtitle) %>% 
    mutate(year=ifelse(is.na(year), col_metrics[1], year)) %>% 
    as_tibble()
  dfCiteScores<-dfMyJournalsFull %>% 
    select(title, year, citescore, publisher3, publisher, url_tocken) %>% 
    spread(year, citescore, fill=0.0)
  
  dfQuartiles<-dfMyJournalsFull %>% 
    select(title, year, quartile) %>%     
    spread(year, quartile, fill="Q4") %>% 
    select(title, !!arrange_quo) %>% 
    mutate(quartile=!!arrange_quo) %>% 
    select(-!!arrange_quo) %>% 
    mutate(quartile=str_replace(quartile, "Quartile ", "Q")) %>% 
    as_tibble()
  
  dfTop10<-dfMyJournalsFull %>% 
    select(title, year, top10) %>% 
    spread(year, top10, fill="") %>% 
    select(title, !!arrange_quo) %>% 
    mutate(top10=!!arrange_quo) %>% 
    select(-!!arrange_quo)
  df<-reduce(list(dfCiteScores, dfQuartiles, dfTop10), left_join, by="title") %>% 
    mutate(jscore=!!arrange_quo) %>% 
    mutate(jacro=pmap_chr(list(title), getAcro)) %>% 
    select(title, jacro, jscore, publisher3, publisher, quartile, top10, !!arrange_quo:!!last_col_quo, url_tocken) %>% 
    mutate(jscore=as.numeric(jscore)) %>% 
    arrange(desc(jscore))
  df %>% 
    left_join(g$specs$publishers %>% select(-publisher), by="publisher3")->df
  'cite score database created...' %>% 
    give_echo(log_con, T, progress)
  df %<>%  
    mutate(url_title=pmap_chr(list(title), generate_journal_url_tag))
  df
}
#' Load cite score statistics in format of 2017 year
#'
#' @param file path to file cite score statistics in tab delimited text format
#'
#' @return data frame with journals records
#' @export
#'
#' @examples
#' (file<-files[1])
#' read_cite_score_2017(file)
read_cite_score_2017<-function(file){
  df<-read_tsv(file, progress = T, trim_ws = T, col_types = cols(), skip = 1) %>% 
    select(Title, `CiteScore`) %>% 
    distinct(Title, .keep_all = T)
  names(df)<-c("title", "citescore")
  df %<>% 
    mutate(citescore=as.numeric(citescore))
  quantiles<-quantile(df$citescore, c(.25, .5, .75, .9), na.rm=T)
  df %>% 
    mutate(quartile=ifelse(citescore<=quantiles[1], "Q4", 
                           ifelse(citescore<=quantiles[2], "Q3", 
                                  ifelse(citescore<=quantiles[3], "Q2", "Q1")))) %>% 
    mutate(quartile=ifelse(is.na(quartile), "Q4", quartile)) %>% 
    mutate(top10=ifelse(citescore>quantiles[4], "Top 10%", "")) %>% 
    mutate(top10=ifelse(is.na(top10), "", top10)) %>% 
    arrange(-citescore)
}
#' Import top researchers
#'
#' @param researchers_path path to the folder with Excel files with Highly Cited
#'   Researcher lists collected by Clarivate Analytics, downloaded from
#'   https://clarivate.com/hcr/researchers-list/archived-lists/
#'   
#'
#' @return
#' @export
#'
#' @examples
#' auths<-createResearcherDF() 
createResearcherDF <- function(progress=NULL) {
  old_path <- getwd()
  setwd(g$paths$researchers)
  files <- dir(pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)
  (years <- str_sub(files, 3, 6))
  not_letters <- "[^[[:alnum:]][[:space:]]-]"
  df <- map(years, function(year) {
    tryCatch({
      rm()
      gc()
      print(year)
      #year<-2018
      rx<-"^\\./" %c% year
      f <- files[which(str_detect(files, pattern = rx))]
      book <- loadWorkbook(f)
      sh <- readWorksheet(book, sheet = 1)
      sh<-sh[,1:4]
      names(sh) <- c('fm_name', 'l_name', 'category', 'affiliation')
      sh <- allVariablesToCharacters(sh) %>% as_tibble()
      economic_authors<-sh %>% subset(str_detect(category, "(?:Economics|Business|Social)")) %>% 
        mutate(fm_name = str_replace_all(fm_name, pattern = not_letters, ''),
               l_name = str_replace_all(l_name, pattern = '[[:blank:]]', ''),
               year = year) %>% 
        rowwise() %>% 
        mutate(fm_abb=extractFirstLetters(fm_name)) %>% 
        mutate(key= str_c(l_name, fm_abb, sep = ' ') %>% str_trim(),
               key=str_replace_all(key, " ", "_")) %>% 
        select(key, year, l_name, fm_name, fm_abb, affiliation)%>% 
        distinct(key, .keep_all = T)
      msg<-paste(nrow(economic_authors), 'distinct authors found in year', year)
      print(msg)
      if (!is.null(progress)) {
        updateProgress(progress, detail = msg)
      }
      economic_authors
    }, error = function(e) {
      cat(paste('File ', f, 'caused a mistake:', e, '\r\n'))
    })
  }) %>% 
    bind_rows %>% 
    arrange(desc(year), l_name) 
  progress_nstep<<-progress_nstep + 1
  msg<-paste(nrow(df), 'total number of authors for all years, distinct authors', length(df$key %>% unique()))
  if (!is.null(progress)) {
    updateProgress(progress, detail = msg)
  }
  setwd(old_path)
  df
}
#' Create Bibliography data frame
#'
#' Create Bibliography data frame to show in the main panel window must contain
#' abbreviated journal names, dates in appropriate format, shorted titles and
#' author names
#' @param df data.frame
#'
#' @return data.frame
#' @export
#'
#' @examples
#' df<-dfWoS
#' res<-getWoSDT(df)
#' res$jacro %>% unique()
getWoSDT <- function() {
  WoSDT<-addCiteScores(dfWoS)
  df %>%
    mutate(title7 = paste(str_sub(title, 1, 35), "...", sep = ''),
           authors = str_replace_all(author, " and ", "; ")) %>%
    mutate(
      authors = paste(str_sub(authors, 1, 12), "...", sep = ''),
      updated = format(updated, "%Y-%m-%d %H:%M:%S", tz = g$tz)
    ) %>%
    mutate(pdf = ifelse(is.na(file), NA, '<img src="pdf.ico" height="25"></img>')) %>%
    #    dplyr::select(pdf, authors, jscore, ascore, nrecords, title7, year, journal, publisher3, updated) %>%
    arrange(desc(updated), desc(pdf), desc(ascore), desc(year))
}
#' Add cite scores
#'
#' Add cite scores to the bibliography data base
#' @param dfWoS data.frame with bibliography records
#'
#' @return
#' @export data.frame augmented with cite score columns. Cite scores must be
#'   loaded separately and saved in config file
#'
#' @examples
#' addCiteScores(dfWoS) %>% head
#' addCiteScores(dfWoS)
#' dfWoS %>% 
#'   filter(is.na(ascore))
#' addCiteScores(dfLoadedBibs)
#' dfCiteScores$journal
#' dfLoadedBibs$journal
#' d$libstat$jcitescores<-createCiteScoreDF(jscore_path, NULL, progress)
#' df<-dfWoS
addCiteScores<-function(df,  log_con=NULL, progress=NULL){
  "Adding cite scores..." %>% 
    give_echo(log_con, T, progress)
  dfCiteScores<-d$libstat$jcitescores %>% 
    mutate(journal=title) %>%
    select(journal, jacro, jscore, quartile, top10, publisher3, publisher)
  dfWoS_short<-df %>% 
    select(-jacro, -jscore, -publisher3, -publisher)
  ret<-dfWoS_short %>% left_join(dfCiteScores, by="journal") %>%
    mutate(year=as.numeric(year),
           jscore=as.numeric(jscore),
           ascore=as.numeric(ascore),
           nrecords=as.numeric(nrecords)) %>% 
    sort_columns_in_wos_dataframe
  progress_nstep<<-progress_nstep + 1
  'done...' %>% 
    give_echo(log_con, F, progress)
  dfWoS
}
#' Add researchers
#'
#' Add researchers to the bibliography data frame. Researchers statistcis must
#' be loaded separately and saved in config file
#'
#' @param dfWoS data.frame with bibliography records
#'
#' @return data.frame augmented with researcher statistics columns
#' @export
#'
#' @examples
#' res<-addResearchers(dfWoS)
#' res %>% subset(nrecords>4)
#' res %>% subset(ascore>2)
#' res %>% subset(nrecords>2) %>% subset(ascore>2) 
#' res %>% filter(is.na(ascore))
#' log_con<-NULL
#' progress<-NULL
addResearchers <- function(dfWoS, log_con=NULL, progress=NULL) {
  dfResearchers<-d$authors$df
  (years <- unique(dfResearchers$year))
  frontier_year <- min(setdiff(years, min(years)))
  frontier_year<-as.integer(frontier_year)
  'extracting key-year-author from main data.frame...\r\n' %>% 
    give_echo(log_con, T, progress)
  df <- dfWoS %>%
    select(key, author, year) %>% 
    mutate(year = ifelse(year < frontier_year, frontier_year - 1, year) %>% as.character) %>%
    mutate(author = str_to_title(author)) %>%
    separate(col = author, into = paste('a', 1:5, sep = '_'), sep = ' And ') %>%
    gather(nauthor, author, a_1:a_5, na.rm = T) %>%
    separate(col = author, into = c('lname', 'fname'), sep = ', ') %>%
    mutate(fm_abb = str_split(fname, pattern = "(?:-| )") %>% 
             lapply(FUN = function(x) { paste(str_sub(x, 1, 1), 
                                              collapse = ' ') }) 
           %>% unlist) %>%
    unite(author, lname, fm_abb, sep = ' ') %>%
    select(key, year, author)
  'extracted key-year-author from main data.frame...\r\n' %>% 
    give_echo(log_con, T, progress)
  df_r <- dfResearchers %>%
    mutate(year = ifelse(year<frontier_year, frontier_year - 1, year) %>% as.character) %>%
    mutate(fm_abb = str_replace(fm_abb, pattern = '-', ' ')) %>%
    unite(author, l_name, fm_abb, sep = ' ') %>%
    mutate(author = str_to_title(author)) %>%
    select(year, author)
  'created top researchers data.frame...\r\n' %>% 
    give_echo(log_con, T, progress)
  df_verb <- df %>%
    group_by(author, year) %>%
    summarize(verb = n()) %>%
    ungroup() %>%
    filter(verb > 5)
  df_verb_WoS <- df %>% 
    group_by(key) %>%
    summarize(nrecords = n()) %>%
    select(key, nrecords) %>%
    mutate(nrecords=as.integer(nrecords))
  'created verbose researchers data.frame...\r\n' %>% 
    give_echo(log_con, T, progress)
  dfWoS <- dfWoS %>%
    select(-nrecords) %>% 
    left_join(df_verb_WoS, by = 'key') %>%
    mutate(nrecords = as.integer(ifelse(is.na(nrecords), 1, nrecords)) ) %>% 
    mutate(year=as.numeric(year),
           jscore=as.numeric(jscore),
           ascore=as.numeric(ascore),
           nrecords=as.numeric(nrecords)) %>% 
    sort_columns_in_wos_dataframe()
  'added verbose researchers to the library...\r\n' %>% 
    give_echo(log_con, T, progress)
  
  df_sel <- semi_join(df, df_r, by = c('author', 'year')) %>%
    group_by(key) %>%
    summarize(ascore = (n()+1) %>% as.integer) %>%
    select(key, ascore) 
  dfWoS <- dfWoS %>%
    select(-ascore) %>% 
    left_join(df_sel, by = 'key') %>%
    mutate(ascore = ifelse(is.na(ascore), 1, ascore) %>% as.integer) %>%
    sort_columns_in_wos_dataframe
  'added top researhers to the library...\r\n' %>% 
    give_echo(log_con, T, progress)
  #side effects: updating verbose and top researchers databases in config file
  d$authors$verbR<<-semi_join(df, df_verb, by = c('author', 'year')) %>% 
    select(year, author) %>%
    distinct(year, author) %>%
    mutate(year = str_c('Y', year)) %>%
    group_by(year) %>%
    mutate(idx = 1:n()) %>%
    ungroup() %>%
    spread(year, author)
  msg<-'created verbose researcher`s table...\r\n' %>% 
    give_echo(log_con, T, progress)
  d$authors$topR<<- semi_join(df, df_r, by = c('author', 'year')) %>% 
    mutate(author=str_to_title(author)) %>% 
    select(year, author) %>%
    distinct(year, author) %>%
    mutate(year = str_c('Y', year)) %>%
    group_by(year) %>%
    mutate(idx = 1:n()) %>%
    ungroup() %>%
    spread(year, author)
  msg<-'created top researcher`s table...\r\n' %>% 
    give_echo(log_con, F, progress)
  
  dfWoS
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
#' journal<-"Journal of Development Economics"
#' generate_journal_url_tag(journal)
generate_journal_url_tag <- function(journal) {
  #print(journal)
  url_tag<-journal
  url_path<-""
  j=d$journals %>% 
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