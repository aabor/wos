#' Words bibliography subset
#'
#' Subset bibliography data frame using words critiria. Algorithm uses
#' "stringdist" package function "amatch" to approximately match words.
#'
#' @param WoSDT data.frame with bibliography records
#' @param searchVector vector of strings in which search will be performed.
#'   Usually this vector represents data.frame column coerced to vector which
#'   contains bibliography fields in which to search.
#' @param searchString string that contains words to search.
#'
#' @return
#' @export
#'
#' @examples
#' searchVector<-WoSDT$title
#' searchWords<-"value risk"
#' filterBibliographyByWords(WoSDT, searchVector, searchWords)
filterBibliographyByWords<-function(WoSDT, searchVector, searchString){
  searchVector %<>% str_to_lower
  searchString %<>% str_to_lower
  searchWords<-words(searchString)
  filter<-searchVector %>% 
    lapply(words) %>% 
    lapply(function(vtitle) {
      amatch(searchWords, vtitle, maxDist = 2)
    }) %>% 
    lapply(function(x){
      ifelse(sum(!is.na(x))==length(x), T, F)
    }) %>% unlist
  WoSDT %>% 
    subset(filter)
}

# feed<-feed.extract("http://rss.sciencedirect.com/publication/science/10575219", encoding = integer())
# read_xml(feed %>% unlist)
# class(feed)
# txt<-feed %>% unlist 
# df<-data.frame(title=txt, stringsAsFactors = F) %>% rownames_to_column(var="tag") %>% 
#   subset(title!="updated") %>% 
#   mutate(tag=str_replace_all(tag, pattern = "items.", replacement = "")) %>% 
#   subset(!is.na(str_match(tag, "title")))
# 
# df$title
#ReadCrossRef(df$title[1], list(issn = "10575219", "from-pub-date" = 2018), limit = 3)
# df %>% head
# str_view(txt, pattern = " ")
# class(txt)
# names(txt)
# length(txt)
# txt[1:10]
# read_html(feed %>% unlist)
# feed[[2]]
# ?feed.extract
# parse.rdf(rss)
# parse.rdf(feed)

# dois<-dfWoS[1:2,]$doi
# url1<-"https://doi.org/"
# urls<-paste(url1, dois, sep="")
# GetBibEntryWithDOI(doi=c("10.1111/jofi.12188"),
#                    temp.file="~/wos_data/new_bib.bib",
#                    delete.file = F)
# 
# GetBibEntryWithDOI(c("10.1016/j.iheduc.2003.11.004", "10.3998/3336451.0004.203"), 
#                    temp.file = file.path(getwd(), "bibs.bib"), delete.file = F)
# 
