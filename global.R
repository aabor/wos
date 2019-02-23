source("wos_imports.R")
source("wos_helpers.R")
source("functions.R")
source("bib_converter.R")
source("bibtex.R")
source("researchers.R")
source("lib_stat.R")
source("addBibs.R")
source("bag_words.R")
source("wos_text_mine.R") 
source("a_text_search.R")
source("selected_records_text_mining.R")
source("check_updates.R")
# home directory is mounted as current user documents
# set variables
app_path<-getwd()
progress_nstep<-0
progress_job<-0
# config vars
#g$tz<-"Europe/Moscow"
#write_json(g, jsonConfig, pretty=T)
g<-read_json("config.json", simplifyVector = T)

rdaWosDoc<-file.path(g$paths$home, "wosdoc.rda")
#save(wosdoc, file=rdaWosDoc)
load(file=rdaWosDoc, verbose = T)
names(wosdoc)
wosdoc$paths$papers<-dir(g$paths$papers)
wosdoc$journals<-dfFromExcel(g$files$xlsxSelectedJournals, 'selected_journals')
wosdoc$publishers<-dfFromExcel(g$files$xlsxSelectedJournals, 'publishers')
wosdoc$journals %<>% 
  select(-publisher) %>% 
  left_join(wosdoc$publishers, by="publisher3")


wos_dt_col_names<-c("pdf","authors", "jscore", "ascore", "nrecords", "title7", "year", "jacro", "publisher3", "updated")
wos_dt_shown_col_names<-c("pdf","Authors", "Jsc", "Asc", "Nrd", "Title", "Year", "J", "Pub", "Updated")
cjcs<-names(wosdoc$libstat$jcitescores)
cjcs_main<-c("title","jacro", "publisher3", "quartile", "top10")
cjcs_citescores<-cjcs[str_detect(cjcs, "^sc")][1:3]
ssci_dt_col_names<-c(cjcs_main, cjcs_citescores, "url_tocken")
ssci_dt_shown_col_names<-c("Journal", "J", "Pub", "Q", "Perc", cjcs_citescores, "Tocken")
top_researchers_dt_col_names<-c("year", "l_name", "fm_name", "affiliation")
top_researchers_dt_shown_col_names<-c("Year", "Last Name", "Family Name", "Affiliation")

#dfWoS<-dfWoS %>% mutate(keywords=str_replace_all(keywords, pattern=' And ', replacement = " and "))
#dfWoS$keywords
#write_rds(dfWoS, g$files$rdsWoS)
#file.exists("/home/rstudio/Documents/Library/wos/pdf/elrDoidge2017464487.pdf")
dfWoS<-read_rds(g$files$rdsWoS)
#dfWoS$file %>% file.exists() %>% sum
# dfWoS %<>% mutate(year=as.numeric(year),
#                   jscore=as.numeric(jscore),
#                   ascore=as.numeric(ascore),
#                   nrecords=as.numeric(nrecords))
#dfWoS %<>% filter(!is.na(journal))


WoSDT<-getWoSDT(dfWoS)
#str(WoSDT)
#g$exportFolder<-"China Finance"
wosdoc$paths$export<-"China Finance"
lastResearchPaperFolder<-file.path(g$paths$papers, wosdoc$paths$export)
file_path<-file.path(lastResearchPaperFolder,paste("bibliography.rds", sep=''))
if(file.exists(lastResearchPaperFolder) & file.exists(file_path)){
  dfWoSExport<-read_rds(file_path)
}else{
  dfWoSExport<-WoSDT[0,]
}
# set initial values to filters
years<-WoSDT$year %>% unique() 
g$year$min<-min(years)
g$year$max<-max(years)

jscores<-WoSDT$jscore %>% unique()
g$jscore$min<-min(jscores, na.rm = T)
g$jscore$max<-max(jscores, na.rm = T)

ascores<-WoSDT$ascore %>% unique()
g$ascore$min<-min(ascores, na.rm = T)
g$ascore$max<-max(ascores, na.rm = T)

nrecords<-WoSDT$nrecords %>% unique()
g$nrecords$min<-min(nrecords, na.rm = T)
g$nrecords$max<-max(nrecords, na.rm = T)

g$journals<-c("All", WoSDT$journal %>% unique())
g$publishers<-c("All", WoSDT$publisher %>% unique())
authors<-WoSDT$author %>% unique()
wosdoc$authors$all<-lapply(authors, function(author){str_split(author, " and ")}) %>% unlist %>% unique()

