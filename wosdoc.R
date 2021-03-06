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
  d$filters$year$frontierYear<-2016
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
  myjournals<-g$specs$journals %>% 
    left_join(g$specs$publishers, by="publisher3") %>% 
    as_tibble()
  d$journals<-myjournals
  d$libstat$jcitescores<-createCiteScoreDF(myjournals, g$paths$jscore)
  d$authors$df<-createResearcherDF()
  echo("New document created", "create_wosdoc")
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
  
  d$citeScores<-getCiteScoresDF(dfWoS)
  d$filters$jscores<-d$citeScores$jscore %>% unique()
  d$filters$jscore$min<-min(d$filters$jscores, na.rm = T)
  d$filters$jscore$max<-max(d$filters$jscores, na.rm = T)
  
  d<-addResearcherScoresDF(d, dfWoS)
  d$filters$ascores<-d$authors$authorScores$ascore %>% unique()
  d$filters$ascore$min<-min(d$filters$ascores, na.rm = T)
  d$filters$ascore$max<-max(d$filters$ascores, na.rm = T)
  
  d$filters$nrecords<-d$authors$authorScores$nrecords %>% unique()
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
  nsm<-d$colnames$cjcs
  d$colnames$cjcs_citescores<-nsm[str_detect(nsm, "^sc")][1:3]
  d$colnames$ssci_dt<-c(g$colnames$cjcs_main, d$colnames$cjcs_citescores, "url_tocken")
  d$colnames$ssci_dt_shown<-c("Journal", "J", "Pub", "Q", "Perc", d$colnames$cjcs_citescores, "Tocken")
  
  #library statistics
  d<-compute_research_papers_statistics(d, dfWoS)
  echo("Document updated", "update_wosdoc")
  d
}
#' Create cite scores data frame
#'
#' CiteScore metrics downloaded from Scopus for recent years
#' https://www.scopus.com/sources

#'
#' @param xlsxSelectedJournals string path to Excel book with manually selected journals
#' @param jscore_path string path to folder with cite score metrics in .txt format for each year
#' @param progress shiny progress bar object
#'
#' @return data.frame with structure  "title"      "publisher"  "quartile"
#'   "percentile" "issn"       "sc2016"     "sc2015" "sc2014"     "sc2013"
#'   "sc2012"     "sc2011"
#' @export
#'
#' @examples
#' progress<-NULL
#' sc<-createCiteScoreDF(g$paths$jscore_path) %>% head
createCiteScoreDF <- function(myjournals, jscore_path, progress=NULL) {
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
  echo('CiteScores metrics found. Extracting metrics...\r\n', "createCiteScoreDF", F, progress)
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
      echo("createCiteScoreDF", F, progress)
    paste("file", file_name, "loaded...", "parsed", nrow(df), "journals\r\n") %>% 
      echo("createCiteScoreDF", T, progress)
    df
  }) %>% 
    reduce(bind_rows)
  'cite score statistics loaded...\r\n' %>% 
    echo("createCiteScoreDF", T, progress)
  
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
    arrange(desc(jscore)) %>% 
    left_join(g$specs$publishers %>% select(-publisher), by="publisher3") %>% 
    mutate(url_title=pmap_chr(list(title, publisher3, url, url_tocken), generate_journal_url_tag))

  'cite score database created...' %>% 
    echo("createCiteScoreDF", T, progress)
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
      paste(nrow(economic_authors), 'distinct authors found in year', year) %>% 
        echo("createResearcherDF")
      economic_authors
    }, error = function(e) {
      paste('File ', f, 'caused a mistake:', e, '\r\n') %>% 
        echo("createResearcherDF", F, NULL, level = "error")
    })
  }) %>% 
    bind_rows %>% 
    arrange(desc(year), l_name) 
  paste(nrow(df), 'total number of authors for all years, distinct authors', length(df$key %>% unique())) %>% 
    echo("createResearcherDF", F, progress)
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
#' res %>% filter(is.na(key))
getWoSDT <- function(dfWoS) {
  if(is.null(dfWoS))return(NULL)
  dfWoS %>% 
    left_join(d$libstat$jcitescores %>% select(-publisher3, -url), by=c('journal'="title")) %>% 
    left_join(d$authors$authorScores, by="key") %>% 
    mutate(updated = format(updated, "%Y-%m-%d %H:%M:%S", tz = g$tz),
           authors = str_replace_all(author, " and ", "; ")) %>%
    mutate(
      title7 = paste(str_sub(title, 1, 35), "...", sep = ''),
      authors = paste(str_sub(authors, 1, 12), "...", sep = '')) %>%
    mutate(pdf = ifelse(is.na(file), NA, '<img src="pdf.ico" height="25"></img>')) %>%
    select(pdf, key, authors, jscore, ascore, nrecords, title7, year, journal, publisher3, jacro, updated, file, author, doi, volume, month, keywords, abstract, title, pages) %>%
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
getCiteScoresDF<-function(dfWoS,  progress=NULL){
  echo("Adding cite scores...","getCiteScoresDF", T, progress)
  dfCiteScores<-d$libstat$jcitescores %>% 
    mutate(journal=title) %>%
    select(journal, jacro, jscore, quartile, top10)
  dfWoS %>% 
    select(key, journal) %>% 
    left_join(dfCiteScores, by="journal") %>% 
    select(-journal)
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
#' progress<-NULL
addResearcherScoresDF <- function(d, dfWoS, progress=NULL) {
  dfResearchers<-d$authors$df
  (years <- unique(dfResearchers$year))
  frontier_year <- min(setdiff(years, min(years)))
  frontier_year<-as.integer(frontier_year)
  echo('extracting key-year-author from main data.frame...', "addResearcherScoresDF", T, progress)
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
  echo('extracted key-year-author from main data.frame...', "addResearcherScoresDF", T, progress)
  df_r <- dfResearchers %>%
    mutate(year = ifelse(year<frontier_year, frontier_year - 1, year) %>% as.character) %>%
    mutate(fm_abb = str_replace(fm_abb, pattern = '-', ' ')) %>%
    unite(author, l_name, fm_abb, sep = ' ') %>%
    mutate(author = str_to_title(author)) %>%
    select(year, author)
  echo('created top researchers data.frame...', "addResearcherScoresDF", T, progress)
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
  'created verbose researchers data.frame...' %>% 
    echo("addResearcherScoresDF", T, progress)
  df_sel <- semi_join(df, df_r, by = c('author', 'year')) %>%
    group_by(key) %>%
    summarize(ascore = (n()+1) %>% as.integer) %>%
    select(key, ascore) 
  d$authors$authorScores<- dfWoS %>%
    select(key) %>% 
    left_join(df_verb_WoS, by = 'key') %>%
    mutate(nrecords = as.integer(ifelse(is.na(nrecords), 1, nrecords)) ) %>% 
    left_join(df_sel, by = 'key') %>%
    mutate(ascore = ifelse(is.na(ascore), 1, ascore) %>% as.integer) 
  'added top researhers to the library...' %>% 
    echo("addResearcherScoresDF", T, progress)
  #side effects: updating verbose and top researchers databases in config file
  d$authors$verbR<-semi_join(df, df_verb, by = c('author', 'year')) %>% 
    select(year, author) %>%
    distinct(year, author) %>%
    mutate(year = str_c('Y', year))
  if(nrow(d$authors$verbR)>0){
    d$authors$verbR%>%
      group_by(year) %>%
      mutate(idx = 1:n()) %>%
      ungroup() %>%
      spread(year, author)
  }
  msg<-'created verbose researcher`s table...' %>% 
    echo("addResearcherScoresDF", T, progress)
  d$authors$topR<-semi_join(df, df_r, by = c('author', 'year')) %>% 
    mutate(author=str_to_title(author)) %>% 
    select(year, author) %>%
    distinct(year, author) %>%
    mutate(year = str_c('Y', year)) %>%
    group_by(year)
  if(nrow(d$authors$topR)>0){
    d$authors$topR%<>%
      mutate(idx = 1:n()) %>%
      ungroup() %>%
      spread(year, author)
  }
  msg<-'created top researcher`s table...' %>% 
    echo("addResearcherScoresDF", T, progress)
  d
}
#' Get journal url path
#'
#' @param publisher3 
#' @param url 
#' @param title 
#' @param url_tocken 
#'
#' @return
#' @export
#'
#' @examples
#' d$journals %>% filter(mydbtitle=="Journal of Development Economics")
#' (df<-d$journals[45,])
#' (publisher3<-df$publisher3)
#' (url<-df$url)
#' (title<-df$title)
#' (url_tocken<-df$url_tocken)
#' get_url_path(publisher3, url, title, url_tocken)
get_url_path<-function(publisher3, url, title, url_tocken){
  ret<-switch(publisher3,
                   elr=elr_url(url, title),
                   oup=file.path(url, url_tocken, "issue"),
                   aea=file.path(url, url_tocken, "issues"),
                   now=url,
                   ares=url,
                   cup={
                     name<-str_to_lower(title) %>% str_split(" ") %>% unlist %>% glue_collapse(sep="-")
                     file.path(url, name, "all-issues")
                   },
                   file.path(url, url_tocken))
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
#' (df<-d$journals[53,])
#' (title<-df$title)
#' (publisher3<-df$publisher3)
#' (url<-df$url)
#' (url_tocken<-df$url_tocken)
#' generate_journal_url_tag(title, publisher3, url, url_tocken)
generate_journal_url_tag <- function(title, publisher3, url, url_tocken) {
  #print(journal)
  url_tag<-title
  url_path<-get_url_path(publisher3, url, title, url_tocken)
  if(url_path!=""){
    url_tag<-a(title, href=url_path, target="_blank") %>% as.character()
  }
  url_tag
}
generate_last_volume_url_tag<-function(last_volume_url){
  a(last_volume_url, href=last_volume_url, target="_blank") %>% as.character()
}
#' Create url for Elsevier journals
#'
#' @param url 
#' @param title 
#'
#' @return
#' @export
#'
#' @examples
elr_url<-function(url, title){
  title<-str_replace_all(title, pattern = "[[:punct:]]", replacement = "")
  name<-str_to_lower(title) %>% str_split(" ") %>% unlist %>% glue_collapse(sep="-")
  file.path(url, name)
}
  