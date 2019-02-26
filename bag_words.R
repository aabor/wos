#' Create keywords data frame
#'
#' Create data frame with columns c(key, year, jscore, ascore, nrecords,
#' keyword) from research papers bibliography records where each keyword is
#' placed in separate row. Bibliography keys may be duplicated. Maximum 10
#' keywords chunks in one record allowed
#'
#' @param df data.frame, DT with bibliography records
#'
#' @return data.frame
#' @export
#'
#' @examples
#' dfKeywords<-createKeywordsDF(WoSDT)
createKeywordsDF <- function(df) {
  'Transforming data frame, creating separate keywords...' %>% 
    echo("createKeywordsDF", F)
  dfKeywords <- filter(df, !is.na(keywords)) %>%
    select(key, year, jscore, ascore, nrecords, keywords) %>%
    separate(keywords, into = paste('kw', 1:10, sep = '_'), sep = ' and ') %>%
    gather(kws, keyword, kw_1:kw_10, na.rm = T) %>%
    select(key, year, jscore, ascore, nrecords, keyword)
  dfKeywords
}
#' Compute key words scores
#'
#' Key words score depends on journal score, author score and number of
#' occurences. We multiply these values for generalized statistics
#'
#' @param dfKeywords data.frame with key words
#' @param byyear boolean set option if calculate for each year separately
#'
#' @return
#' @export
#'
#' @examples
#' dfKeywords <- createKeywordsDF(WoSDT)
#' (dfKeywordModifiedScore<-computeKeywordScores(dfKeywords))
#' (dfKeywordModifiedScoreByYear<-computeKeywordScores(dfKeywords, byyear=T))
computeKeywordScores <- function(dfKeywords, byyear = F) {
  'Computing word counts and scores' %>% 
    echo("computeKeywordScores", F)
  df_ws <- dfKeywords %>%
    mutate(word_score = jscore * ascore * nrecords)
  if (byyear) {
    'Computing by year' %>% 
      echo("computeKeywordScores", F)
    df_ws <- df_ws %>%
      select(year, keyword, word_score) %>%
      group_by(year, keyword) %>%
      mutate(score = sum(word_score)) %>%
      summarise(n = n(), mscore = mean(score)) %>%
      ungroup() %>%
      filter(!is.na(mscore)) %>%
      arrange(desc(year), desc(mscore))
  } else {
    'Computing for the whole library' %>% 
      echo("computeKeywordScores", F)
    df_ws <- df_ws %>%
      select(keyword, word_score) %>%
      group_by(keyword) %>%
      mutate(score = sum(word_score)) %>%
      summarise(n = n(), mscore = mean(score)) %>%
      filter(!is.na(mscore)) %>%
      arrange(desc(mscore))
  }
  df_ws
}

#' Key words clouds
#'
#' Create and save on disc wordcouds in .png graphical format.
#'
#' @param dfKeywords data.frame with keywords frequencies
#' @param df_ws data.frame with keywords ranged by modified score word_score =
#'   product(jscore, ascore, nrecords)
#'
#' @return void
#' @export
#'
#' @examples
#' createKeywordScoresWordclouds(dfKeywordModifiedScore) 
createKeywordScoresWordclouds <- function(dfKeywordModifiedScore) {
  pal<-brewer.pal(11,"BrBG")
  png("img/wordcloud_n.png", width = 1280, height = 800)
  wordcloud(dfKeywordModifiedScore$keyword, dfKeywordModifiedScore$n, scale = c(8, .2), min.freq = 3,
            max.words = 50, random.order = FALSE, rot.per = .15, colors=pal)
  dev.off()
  png("img/wordcloud_mscore.png", width = 1280, height = 800)
  wordcloud(dfKeywordModifiedScore$keyword, dfKeywordModifiedScore$mscore, scale = c(8, .2), min.freq = 3,
            max.words = 50, random.order = FALSE, rot.per = .15, colors=pal)
  dev.off()
}
#' Wordcloud key words frequencies
#'
#' @param df data.frame with key words modified scores
#'
#' @return
#' @export
#'
#' @examples
#' wordcloudKeywordsFrequencies(dfKeywordModifiedScore)
wordcloudKeywordsFrequencies <- function(dfKeywordModifiedScore) {
  if(is.null(dfKeywordModifiedScore))return(NULL)
  pal<-brewer.pal(11,"BrBG")
  wordcloud(dfKeywordModifiedScore$keyword, dfKeywordModifiedScore$n, scale = c(8, .2), 
            min.freq = 3, max.words = 50, 
            random.order = FALSE, rot.per = .15, 
            colors=pal)
}
#' Wordcloud key words modified score
#'
#' @param df data.frame with key words modified score
#'
#' @return
#' @export
#'
#' @examples
#' wordcloudKeywordsMscore(dfKeywordModifiedScore)
wordcloudKeywordsMscore <- function(dfKeywordModifiedScore) {
  if(is.null(dfKeywordModifiedScore))return(NULL)
  pal<-brewer.pal(11,"BrBG")
  wordcloud(dfKeywordModifiedScore$keyword, dfKeywordModifiedScore$mscore, scale = c(8, .2), 
            min.freq = 3, max.words = 50, 
            random.order = FALSE, rot.per = .15, 
            colors=pal)
}

#' Keyword frequencies by year table
#'
#' @param dfKeywordFrequencies data.frame
#' @param stop_year integer
#'
#' @return
#' @export
#'
#' @examples
#' createKeywordFrequenciesByYear(dfKeywordModifiedScoreByYear, 2015)
createKeywordFrequenciesByYear <- function(dfKeywordModifiedScoreByYear, stop_year) {
  if(is.null(dfKeywordModifiedScoreByYear))return(NULL)
  dfKeywordModifiedScoreByYear %>% 
    select(keyword, year, n) %>% 
    filter(year > stop_year) %>%
    mutate(year = str_c('Y', year, sep = '')) %>%
    spread(year, n) %>%
    arrange(desc(Y2017))
}
#' Keyword modified score by year table
#'
#' @param dfKeywordModifiedScore data.frame
#' @param stop_year integer
#'
#' @return
#' @export
#'
#' @examples
#' createKeywordModifiedScoresByYear(dfKeywordModifiedScoreByYear, 2015)
createKeywordModifiedScoresByYear <- function(dfKeywordModifiedScoreByYear, stop_year) {
  if(is.null(dfKeywordModifiedScoreByYear))return(NULL)
  dfKeywordModifiedScoreByYear %>% 
    select(keyword, year, mscore) %>% 
    filter(year > stop_year) %>%
    mutate(year = str_c('Y', year, sep = '')) %>%
    spread(year, mscore) %>%
    arrange(desc(Y2017))
}
cleanText <- function(text) {
  text %>%
    bracketX() %>%
    replace_number() %>%
    replace_abbreviation() %>%
    replace_contraction() %>%
    replace_symbol() %>%
    str_trim()
}
cleanCorpus <- function(corpus) {
  sw_lower <- stopwords("en")
  add_stop_words<-c('evidence', 'market', 'financial', 'risk', 'model','models', 'markets', 'stock', 'pricing', 'price', 'two', 'one', 'theory', 'analysis', 'impact', 'approach', 'optimal', 'portfolio', 'asset', 'investment', 'performance')
  my_stop_words <- c(add_stop_words, add_stop_words, sw_lower, sw_lower)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, my_stop_words)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus
}



