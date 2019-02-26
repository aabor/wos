source("wos_imports.R")
source("wos_helpers.R")
source("functions.R")
source("bib_converter.R")
source("bibtex.R")
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
# home directory is mounted as current user documents
# set variables
app_path<-getwd()
progress_nstep<-0
progress_job<-0
# config vars
#g$tz<-"Europe/Moscow"

#write_json(g, "config.json", pretty=T, simplifyVector = T, auto_unbox=T)
g<-read_json("config.json", simplifyVector = T)
configure_log(g, logger_name = "wos")
# dfWoS %>% 
#   group_by(year) %>% 
#   summarise(n=n()) %>% 
#   arrange(desc(year))

document_path<-file.path(g$paths$home, "wosdoc.rds")
if(file.exists(document_path)){
  d<-read_rds(document_path)
}else{
  d<-create_wosdoc()
  #write_rds(d, document_path)
}
#write_rds(dfWoS, g$files$rdsWoS)
dfWoS<-read_rds(g$files$rdsWoS)
#dfWoS %>% filter(is.na(key))
# dfWoS$jscore<-NULL
# dfWoS$jacro<-NULL
IsBibs<-F
if(IsBibs){
  dfWoS<-import_bibtex_and_pdf(dfWoS)
  d<-update_wosdoc(d, dfWoS)
}
#write_rds(d, document_path)

WoSDT<-getWoSDT(dfWoS)

lastResearchPaperFolder<-file.path(g$paths$papers, d$select$exportFolder)
file_path<-file.path(lastResearchPaperFolder,g$files$rdsbib)
if(file.exists(lastResearchPaperFolder) && file.exists(file_path)){
  dfWoSExport<-read_rds(file_path)
}else{
  dfWoSExport<-WoSDT[0,]
}

