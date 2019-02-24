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
source("wosdoc.R")
g<-NULL
d<-NULL
source("user_interface.R")
# home directory is mounted as current user documents
# set variables
app_path<-getwd()
progress_nstep<-0
progress_job<-0
# config vars
#g$tz<-"Europe/Moscow"

#write_json(g, "config.json", pretty=T, simplifyVector = T, auto_unbox=T)
g<-read_json("config.json", simplifyVector = T)

document_path<-file.path(g$paths$home, "wosdoc.rds")
if(file.exists(document_path)){
  d<-read_rds(document_path)
}else{
  d<-create_wosdoc()
}

wos_dt_col_names<-c("pdf","authors", "jscore", "ascore", "nrecords", "title7", "year", "jacro", "publisher3", "updated")
wos_dt_shown_col_names<-c("pdf","Authors", "Jsc", "Asc", "Nrd", "Title", "Year", "J", "Pub", "Updated")
cjcs<-names(d$libstat$jcitescores)
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
d<-update_wosdoc(d, dfWoS)
#dfWoS$file %>% file.exists() %>% sum
# dfWoS %<>% mutate(year=as.numeric(year),
#                   jscore=as.numeric(jscore),
#                   ascore=as.numeric(ascore),
#                   nrecords=as.numeric(nrecords))
#dfWoS %<>% filter(!is.na(journal))


WoSDT<-getWoSDT()
#write_rds(d, document_path)

#str(WoSDT)
#g$exportFolder<-"China Finance"
lastResearchPaperFolder<-file.path(g$paths$papers, d$select$exportFolder)
file_path<-file.path(lastResearchPaperFolder,g$files$rdsbib)
if(file.exists(lastResearchPaperFolder) && file.exists(file_path)){
  dfWoSExport<-read_rds(file_path)
}else{
  dfWoSExport<-WoSDT[0,]
}

