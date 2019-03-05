rv <- reactiveValues(WoSExportDT = dfWoSExport,
                     WoSDT=getWoSDT(dfWoS),
                     ResearchersDT=d$authors$df, 
                     TopResearchersByYearDT=d$authors$topR, 
                     VerboseResearchersByYearDT=d$authors$verbR, 
                     JournalsDT=d$libstat$jcitescores,
                     WholeLibraryStatDT=d$libstat$general,
                     DetailedLibraryStatDT=d$libstat$detailed,
                     UpdatePlanDT=d$libstat$updatePlan,
                     UpdateHistoryDT=d$libstat$updateHistory,
                     PublisherStatDT=d$publishers,
                     keywordsmodifiedScores=d$keywords$modifiedScores,
                     keywordsmodifiedScoresByYear=d$keywords$modifiedScoresByYear,
                     keywordsfrequenciesByYear=d$keywords$frequenciesByYear,
                     coherence_mat=d$top_topic_model$coherence,
                     top_topic_model=d$top_topic_model)

#write_rds(g, rdsConfig)

shinyServer(function(input,output,clientData, session){
  # Functions in the server
  updateExportFolders<-function(){
    d$paths$papers<<-dir(g$paths$papers)
    updateSelectInput(session, "exportFolders", choices=d$paths$papers, selected=d$select$exportFolder)
  }
  observe({
    d$select$fileFormat<<-input$exportFileFormats
    d$filters$year$frontierYear<<-input$frontierYear
  })
  observeEvent(input$exportFolders,{
    reloadBibs()
  })
  
  # open pdf for currently selected record
  shinyjs::disable("openPDFMain")
  observeEvent(input$tableBibs_row_last_clicked, {
    disableDownloadButton<-T
    s = input$tableBibs_row_last_clicked
    if (length(s)) {
      f=rv$WoSDT[s,]$file
      if(!is.na(f)){
        if(f!=""){
          f<-file.path(g$paths$pdf, f)
          if(file.exists(f)){
            disableDownloadButton<-F
          }
        }
      }
    }
    if(disableDownloadButton){
      shinyjs::disable("openPDFMain")
    }
    else{
      shinyjs::enable("openPDFMain")
    }
  })
  shinyjs::disable("openPDF")
  observeEvent(input$tableBibsExport_row_last_clicked, {
    disableDownloadButton<-T
    s = input$tableBibsExport_row_last_clicked
    if (length(s)) {
      f=rv$WoSExportDT[s,]$file
      if(!is.na(f)){
        if(f!=""){
          f<-file.path(g$paths$pdf, f)
          if(file.exists(f)){
            disableDownloadButton<-F
          }
        }
      }
    }
    if(disableDownloadButton){
      shinyjs::disable("openPDF")
    }
    else{
      shinyjs::enable("openPDF")
    }
  })
  generate_file_name_main <- function() {
    f<-""
    s = input$tableBibs_row_last_clicked
    if (length(s)) {
      f=rv$WoSDT[s,]$file
      if(!is.na(f)){
        if(f!=""){
          f<-file.path(g$paths$pdf, f)
        }
      }
    }
    f
  }
  generate_file_name <- function() {
    f<-""
    s = input$tableBibsExport_row_last_clicked
    if (length(s)) {
      f=rv$WoSExportDT[s,]$file
      if(!is.na(f)){
        if(f!=""){
          f<-file.path(g$paths$pdf, f)
        }
      }
    }
    f
  }
  output$openPDFMain <- downloadHandler(
    filename = function() {
      basename(generate_file_name_main())
    },
    content = function(file) {
      myfile <- generate_file_name_main()
      file.copy(myfile, file)
    },
    contentType = "application/pdf"
  )
  output$openPDF <- downloadHandler(
    filename = function() {
      basename(generate_file_name())
    },
    content = function(file) {
      myfile <- generate_file_name()
      file.copy(myfile, file)
    },
    contentType = "application/pdf"
  )
  # Load bibliography records from selected folder
  #load when server starts
#  getCurrentResearchPaperBibliographyPath(input$exportFolders)
  #load any time by user request
  reloadBibs<-function(){
    path<-file.path(g$paths$papers,
                    input$exportFolders)
    file_path<-file.path(path,paste("bibliography.rds", sep=''))
    if(file.exists(file_path)){
      rv$WoSExportDT<-read_rds(file_path)
      dfWoSExport<<-rv$WoSExportDT
    }
    else{
      rv$WoSExportDT<-WoSDT[0,]
      dfWoSExport<<-WoSDT[0,]
    }
  }
  observeEvent(input$loadBibs,{
    reloadBibs()
  })
  #Export bibliography to research paper folder
  observeEvent(input$exportBibs,{
    path<-file.path(g$paths$papers,
                    input$exportFolders)
    req(file.exists(path))
    file_path<-file.path(path,paste("bibliography", input$exportFileFormats, sep=''))
    if(input$exportFileFormats==".csv"){
      write_csv(rv$WoSExportDT, file_path)
    }
    if(input$exportFileFormats==".rds"){
      write_rds(rv$WoSExportDT, file_path)
    }
    if(input$exportFileFormats==".bib"){
      saveDataFrameToBibTeX(rv$WoSExportDT, file_path)
    }
    if(input$exportFileFormats==".rmd"){
      exportLibraryToRMarkdown(rv$WoSExportDT, file_path)
    }
    showModal(modalDialog(
      title = "Important message",
      paste("Bibliography records saved to file", file_path),
      easyClose=T,
      footer = tagList(
        modalButton("OK"))
    ))
  })
  # filter to main database
  observeEvent(input$resetFilter,{
    updateTextAreaInput(session, "searchKeyWords", value = "")
    updateTextAreaInput(session, "searchTitle", value = "")
    updateTextAreaInput(session, "searchAbstract", value = "")
    updateTextAreaInput(session, "searchFullText", value = "")
    updateSelectInput(session, "Journal", selected = "All")
    updateSelectInput(session, "Publisher", selected = "All")
    updateSliderInput(session, "sliderNrecords", 
                      min = d$filters$nrecord$min, max = d$filters$nrecord$max,
                      value = c(1, d$filters$nrecord$max))
    updateSliderInput(session, "sliderAscore", 
                      min = d$filters$ascore$min, max = d$filters$ascore$max,
                      value = c(0, d$filters$ascore$max))
    updateSliderInput(session, "sliderYear", 
                min = d$filters$year$min, max = d$filters$year$max,
                value = c(d$filters$year$min,d$filters$year$max))
    updateSliderInput(session, "sliderJscore", 
                min = d$filters$jscore$min, max = d$filters$jscore$max,
                value = c(d$filters$jscore$min,d$filters$jscore$max))
    updateCheckboxInput(session, "checkKeyWords", value = F)
    updateCheckboxInput(session, "checkAbstract", value = F)
    updateCheckboxInput(session, "checkFullText", value = F)
    updateTextInput(session, "Authors", value = "")
  })
  observeEvent(input$applyFilter,{
    # Create a Progress object
    progress_nstep<<-0
    progress_job<<-1
    progress <- shiny::Progress$new()
    progress$set(message = "Filtering data base", value = 0)
    on.exit(progress$close())

    # main task
    dt<-dfWoS %>% getWoSDT()
    if(input$checkKeyWords){
      dt<-dt %>% subset(!is.na(keywords))
    }
    cat(paste(nrow(dt), "keywords\r\n"))
    if(input$checkAbstract){
      dt<-dt %>% subset(!is.na(abstract))
    }
    cat(paste(nrow(dt), "abstract\r\n"))
    if(input$checkFullText){
      dt<-dt %>% subset(!is.na(file))
    }
    cat(paste(nrow(dt), "fulltext\r\n"))
    dt<-dt %>% subset(input$sliderYear[1]<=year & year<=input$sliderYear[2]) %>% 
      subset(input$sliderJscore[1]<=jscore & jscore<=input$sliderJscore[2]) %>% 
      subset(input$sliderAscore[1]<=ascore & ascore<=input$sliderAscore[2]) %>% 
      subset(input$sliderNrecords[1]<=nrecords & nrecords<=input$sliderNrecords[2])
    cat(paste(input$sliderYear[1], input$sliderYear[2], "\r\n"))
    cat(paste(nrow(dt), "sliders\r\n"))
    
    if(input$Journal!="All"){
      dt<-dt %>% subset(journal==input$Journal)
      #dt<-dt %>% subset(journal==)
    }
    if(input$Publisher!="All"){
      dt<-dt %>% subset(publisher3==input$Publisher)
      cat(paste("Searching for"), nrow(df), "\r\n")
    }
    cat("Filtering by words")
    if(input$searchKeyWords!=""){
      dt<-filterBibliographyByWords(dt, dt$keywords, input$searchKeyWords)
    }
    if(input$searchTitle!=""){
      dt<-filterBibliographyByWords(dt, dt$title, input$searchTitle)
    }
    if(input$searchAbstract!=""){
      dt<-filterBibliographyByWords(dt, dt$abstract, input$searchAbstract)
    }
    if(input$searchFullText!=""){
      #dt<-filterBibliographyByWords(dt, dt$keywords, input$searchFullText)
    }
    progress_nstep<<-progress_nstep + 1
    msg<-'filtering finished...'
    if (!is.null(progress)) {
      updateProgress(progress, detail = msg)
    }
    rv$WoSDT<-dt
  })
  observeEvent(input$deleteSelectedFromMainDB, {
    s = input$tableBibs_rows_selected
    if (length(s)) {
      dfSelected<-rv$WoSDT[s,]
      showModal(modalDialog(
        title = "Important message",
        paste(nrow(dfSelected), "records with keys", dfSelected$key %>% glue_collapse(sep=" "), "will be deleted. Continue?"),
        easyClose=T,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("okDeleteFromMain", "OK"))
      ))
    }
  })
  observeEvent(input$okDeleteFromMain,{
    removeModal()
    s = input$tableBibs_rows_selected
    if (length(s)) {
      dfSelected<-rv$WoSDT[s,]
      dfWoS<<-dfWoS %>%
        filter(!(key %in% dfSelected$key))
      rv$WoSDT<-getWoSDT(dfWoS)
      showModal(modalDialog(
        title = "Important message",
        paste("Deleted", nrow(dfSelected), "records with keys", dfSelected$key %>% glue_collapse(sep=" ")),
        easyClose=T,
        footer = tagList(
          modalButton("OK"))
      ))
    }
  })
  observeEvent(input$checkShowDuplicates,{
    dt<-dfWoS %>% getWoSDT()
    if(input$checkShowDuplicates==T){
      dup_keys<-dt[duplicated(dt[, c('key')]),]$key
      dt<-dt%>% 
        subset(key %in% dup_keys)
    }
    rv$WoSDT<-dt
  })
  observeEvent(input$deleteDuplicates,{
    dfWoS<<- dfWoS %>% 
      distinct(key, .keep_all = T)
    updateCheckboxInput(session, "checkShowDuplicates", value = F)
    rv$WoSDT<-getWoSDT(dfWoS)
  })
  observeEvent(input$checkReferences,{
    progress_nstep<<-0
    progress_job<<-1
    progress <- shiny::Progress$new()
    progress$set(message = "Checking file references", value = 0)
    on.exit(progress$close())
    dfWoS<<-check_pdf_file_references(dfWoS, progress)
    rv$WoSDT<-getWoSDT(dfWoS)
  })
  # main data base
  output$tableBibs = DT::renderDataTable({
    #rv$WoSDT<-getWoSDT(dfWoS)
    datatable(rv$WoSDT[,g$colnames$wos_dt], rownames = F, 
              colnames = g$colnames$wos_dt_shown,
              escape = F,
              options = list(
                lengthMenu = list(c(7, 15, 50), c('5', '15', "50")),
                pageLength = 7
              ),
              selection = 'multi')
  }, server=T)
  DTproxy <- DT::dataTableProxy("tableBibs", session = session)
  observeEvent(input$resetSelectionInMainDB, {
    DT::selectRows(DTproxy, NULL)
    shinyjs::disable("openPDFMain")
  })
  
  # show full bibliography of the selected record in textbox
  output$cur_bib = renderUI({
    s = input$tableBibs_row_last_clicked
    if (length(s)) {
      getAPAstyleBib(rv$WoSDT[s,]) %>% HTML
    }
  })
  output$cur_bibtex = renderUI({
    s = input$tableBibs_row_last_clicked
    if (length(s)) {
      convertDataFrameToBibTexHTML(rv$WoSDT[s,])
    }
  })
  output$cur_bibtex_multy = renderUI({
    if (nrow(rv$WoSExportDT)>0) {
      convertDataFrameToBibTexHTML(rv$WoSExportDT)
    }
  })
  output$cur_bibtex_rmarkdown = renderUI({
    if (nrow(rv$WoSExportDT)>0) {
      convertDataFrameToRMarkdownHTML(rv$WoSExportDT)
    }
  })
  observeEvent(input$suggestTerms,{
    updateTextAreaInput(session, "termsToFindInFullTexts", value = get_terms(rv$WoSExportDT))
  })
  text_chunks<-eventReactive(input$get_text_chunks,{
    if(input$termsToFindInFullTexts!=""){
      progress_nstep<<-0
      progress_job<<-1
      progress <- shiny::Progress$new()
      progress$set(message = "Checking file references", value = 0)
      on.exit(progress$close())
      convertDataFrameToRMarkdownHTMLWithTextChunks(rv$WoSExportDT, input$termsToFindInFullTexts, progress)
    }
  })
  output$text_chunks_with_terms=renderUI({
    text_chunks()
  })
  # add selected records to special table for further export
  observeEvent(input$addSelected, {
    s = input$tableBibs_rows_selected
    if (length(s)) {
      rv$WoSExportDT<-bind_rows(rv$WoSDT[s,], rv$WoSExportDT) %>% 
        distinct(key, .keep_all = T)
      dfWoSExport<<-rv$WoSExportDT
    }
  })
  # remove selected records to special table for further export
  observeEvent(input$removeSelected, {
    s = input$tableBibsExport_row_last_clicked
    if (length(s)) {
      rv$WoSExportDT<-rv$WoSExportDT[-s,]
      dfWoSExport<<-rv$WoSExportDT
    }
  })
  #change export folder
  observe({
    d$select$exportFolder<<-input$exportFolders
  })
  # add new export folder
  observeEvent(input$addExportFolder,{
    dir.create(file.path(g$paths$papers, input$newExportFolder))
    d$select$exportFolder<<-input$newExportFolder
    d$paths$papers<<-c(input$newExportFolder, d$paths$papers)
    updateSelectInput(session, "exportFolders", choices=d$paths$papers, selected=input$newExportFolder)
    updateTextInput(session, "newExportFolder", value = "")
  })
  #update export folder on the start of each session
  updateExportFolders()
  # delete existing export folder
  observeEvent(input$deleteExportFolder,{
    showModal(modalDialog(
      title = "Important message",
      paste("All contnent of the folder ", input$exportFolders, " will be deleted! Continue?"),
      easyClose=T,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("okDelete", "OK"))
    ))
  })
  observeEvent(input$okDelete,{
      removeModal()
      folder_path<<-file.path(g$paths$papers, input$exportFolders)
      cat(paste("Deleting... ", folder_path, '\n'))
      res<-unlink(folder_path, recursive = T)
      cat(paste("Deleting folder returned ", res, '\n'))
      g$export$folder<<-""
      updateExportFolders()
  })
  # table with records to export
  output$tableBibsExport = DT::renderDataTable({
    datatable(rv$WoSExportDT[, g$colnames$wos_dt], rownames = F, 
              colnames = g$colnames$wos_dt_shown,
              escape = F,
              options = list(
                lengthMenu = list(c(7, 15, 50), c('7', '15', "50")),
                pageLength = 7
              ),
              selection = 'single')
  })
  # summary table for records to export
  output$tableBibsExportSummary = DT::renderDataTable({
    #dfWoSExport
    datatable(rv$WoSExportDT %>% get_export_records_summary, rownames = F, 
              colnames = g$colnames$export_records_summary,
              escape = F,
              options = list(
                lengthMenu = list(c(3, 5, 15), c('3', '5', "15")),
                pageLength = 5
              ),
              selection = 'single')
  })
  #table with top researchers from Clarivate Analytics database
  output$tableResearchers = DT::renderDataTable({
    datatable(rv$ResearchersDT[, g$colnames$top_researchers_dt], rownames = F,
              colnames = g$colnames$top_researchers_dt_shown,
              escape = F,
              options = list(
                lengthMenu = list(c(7, 15, 50), c('7', '15', "50")),
                pageLength = 7
              ),
              selection = 'single')
  })
  #table with top researchers by year
  output$tableTopResearchersByYear = DT::renderDataTable({
    datatable(rv$TopResearchersByYearDT, rownames = F,
#              colnames = top_researchers_dt_shown_col_names,
              escape = F,
              options = list(
                lengthMenu = list(c(7, 15, 50), c('7', '15', "50")),
                pageLength = 7
              ),
              selection = 'single')
  })
  #table with verbose researchers by year
  output$tableVerboseResearchersByYear = DT::renderDataTable({
    datatable(rv$VerboseResearchersByYearDT, rownames = F,
              #colnames = top_researchers_dt_shown_col_names,
              escape = F,
              options = list(
                lengthMenu = list(c(7, 15, 50), c('7', '15', "50")),
                pageLength = 7
              ),
              selection = 'single')
  })
  #table with journal SSCI powered by Scopus
  output$tableJournals = DT::renderDataTable({
    datatable(rv$JournalsDT[, d$colnames$ssci_dt], rownames = F,
              colnames = d$colnames$ssci_dt_shown,
              escape = F,
              options = list(
                lengthMenu = list(c(7, 15, 60), c('5', '15', "60")),
                pageLength = 60
              ),
              selection = 'single')
  })
  # text box with full bibliography of the selected record
  output$cur_bibExport = renderUI({
    s = input$tableBibsExport_row_last_clicked
    if (length(s)) {
      getAPAstyleBib(rv$WoSExportDT[s,]) %>% HTML
    }
  })
  output$cur_bibtexExport = renderUI({
    s = input$tableBibsExport_row_last_clicked
    if (length(s)) {
      convertDataFrameToBibTexHTML(rv$WoSExportDT[s,])
    }
  })
  ## Library import
  #
  journal_cur_selection<-reactive({
    journal<-rv$WholeLibraryStatDT[input$tableWholeLibraryStat_row_last_clicked,"journal_name"]
  })
  observeEvent(journal_cur_selection(), {
      updateSelectInput(session, "separateJournal", 
                        selected = journal_cur_selection())
  })
  journal_cur_selection_update_plan<-reactive({
    journal<-rv$UpdatePlanDT[input$tableJournalUpdatePlan_row_last_clicked,"journal"]
  })
  observeEvent(journal_cur_selection_update_plan(), {
    updateSelectInput(session, "separateJournal", 
                      selected = journal_cur_selection_update_plan())
  })
  observeEvent(input$tablePublisherStat_row_last_clicked, {
    s = input$tablePublisherStat_row_last_clicked
    if (length(s)) {
      j=rv$PublisherStatDT[s,]
      rv$WholeLibraryStatDT<-d$libstat$general %>%
        filter(publisher3==j$publisher3)
    }
  })
  
  # import research papers from downloads folder
  observeEvent(input$importResearchPapers,{
    # Create a Progress object
    progress_nstep<<-0
    progress_job<<-16
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    # main task
    progress$set(message = "Importing research papers", value = 0)
    cat("Start importing research papers on server side\n")
    nrow_old<-nrow(dfWoS)
    dfWoS<<-import_bibtex_and_pdf(dfWoS, progress, input$deleteSourcePDFs)
    d<<-update_wosdoc(d, dfWoS)
    msg<-"import folder is empty"
    rv$WholeLibraryStatDT<-d$libstat$general
    rv$DetailedLibraryStatDT<-d$libstat$detailed
    rv$PublisherStatDT<-d$libstat$publishers
    rv$UpdatePlanDT<-d$libstat$updatePlan
    rv$UpdateHistoryDT<-d$libstat$updateHistory
    rv$WoSDT<-getWoSDT(dfWoS)
    msg<-paste("Import finished", "Total steps", progress_nstep)
    showModal(modalDialog(
      title = "Note",
      msg,
      easyClose=T,
      footer = tagList(
        modalButton("OK"))
    ))
  })    
  observeEvent(input$getBibEntryWithDOI, {
    # Create a Progress object
    progress_nstep<<-0
    progress_job<<-16
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    # main task
    progress$set(message = "Loading bib entries using DOI System API", value = 0)
    msg<-load_bib_entries_for_downloaded_pdfs(progress)
    showModal(modalDialog(
      title = "Note",
      msg,
      easyClose=T,
      footer = tagList(
        modalButton("OK"))
    ))
  })
  #table with statistics for separate journal
  output$tableSeparateJournalStat = DT::renderDataTable({
    journal_name<-input$separateJournal
    #print(journal_name)
    datatable(rv$DetailedLibraryStatDT[[journal_name]], rownames = F,
              escape = F,
              options = list(
                lengthMenu = list(c(5, 15, 60), c('5', '15', "30")),
                pageLength = 5
              ),
              selection = 'single')
  })
  #table with whole library statistics
  output$tableWholeLibraryStat = DT::renderDataTable({
    whole_lib_stat_dt_col_names<-setdiff(names(rv$WholeLibraryStatDT), "journal_name")
    datatable(rv$WholeLibraryStatDT[, whole_lib_stat_dt_col_names], rownames = F,
              escape = F,
              options = list(
                lengthMenu = list(c(7, 15, 60), c('5', '15', "60")),
                pageLength = 60
              ),
              selection = 'single')
  })
  output$tableJournalUpdatePlan=DT::renderDataTable({
    datatable(rv$UpdatePlanDT[, g$colnames$journal_update_plan], rownames = F,
              escape=F, selection="single",
              colnames = g$colnames$journal_update_plan_shown)
  })
  output$tableJournalUpdatesHistory=DT::renderDataTable({
    datatable(rv$UpdateHistoryDT[, g$colnames$journal_updates_history], rownames = F,
              escape=F,
              colnames = g$colnames$journal_updates_history_shown)
  })
  #table with publishers statistics
  output$tablePublisherStat = DT::renderDataTable({
    datatable(rv$PublisherStatDT, rownames = F,
              escape = F,
              options = list(
                lengthMenu = list(c(3, 7, 15), c('3', '7', "15")),
                pageLength = 7
              ),
              selection = 'single')
  })
  
  # compute and show key words statistics
  observeEvent(input$computeKeyWordsFrequencies,{
    dfKeywords<-createKeywordsDF(rv$WoSDT)
    keywords<-list()

    keywords$modifiedScores<-computeKeywordScores(dfKeywords)
    dfKeywordModifiedScoreByYear<-computeKeywordScores(dfKeywords, byyear=T)
    
    keywords$modifiedScoresByYear<-createKeywordModifiedScoresByYear(dfKeywordModifiedScoreByYear, 2013)
    keywords$frequenciesByYear<-createKeywordFrequenciesByYear(dfKeywordModifiedScoreByYear, 2013)
  
    d$keywords<<-keywords
    rv$keywordsmodifiedScores=keywords$modifiedScores
    rv$keywordsmodifiedScoresByYear=keywords$modifiedScoresByYear
    rv$keywordsfrequenciesByYear=keywords$frequenciesByYear
    
    showModal(modalDialog(
      title = "Note",
      "Key word frequencies computed...",
      easyClose=T,
      footer = tagList(
        modalButton("OK"))
    ))
  })
  # show key word modified score by year in table
  output$tableKeyWordMscore = DT::renderDataTable({
    datatable(rv$keywordsmodifiedScoresByYear, rownames = F,
              escape = F,
              options = list(
                lengthMenu = list(c(7, 15, 60), c('5', '15', "60")),
                pageLength = 7
              ),
              selection = 'single')
  })
  # show key word frequencies by year in table
  output$tableKeyWordFrequencies = DT::renderDataTable({
    datatable(rv$keywordsfrequenciesByYear, rownames = F,
              escape = F,
              options = list(
                lengthMenu = list(c(7, 15, 60), c('5', '15', "60")),
                pageLength = 7
              ),
              selection = 'single')
  })
  # plot wordcloud of key words orderd by modified score
  output$plotKeyWordMscore<-renderPlot(height = 800, {
    wordcloudKeywordsMscore(rv$keywordsmodifiedScores)
  })
  # plot wordcloud of key words orderd by frequencies
  output$plotKeyWordFrequencies<-renderPlot(height = 800, {
    wordcloudKeywordsFrequencies(rv$keywordsmodifiedScores)
  })
  observeEvent(input$startTopicAnalysis,{
    # Create a Progress object
    progress_nstep<<-0
    progress_job<<-10
    progress <- shiny::Progress$new()
    progress$set(message = "Topic analysis", value = 0)
    on.exit(progress$close())
    # main task
    
    topicAnalysis(rv$WoSDT, d$filters$year$frontierYear, progress)
    
    progress_nstep<<-progress_nstep + 1
    msg<-'topic analysis finished...'
    if (!is.null(progress)) {
      updateProgress(progress, detail = msg)
    }
    showModal(modalDialog(
      title = "Note",
      "Topic analysis finished",
      easyClose=T,
      footer = tagList(
        modalButton("OK"))
    ))
  })
  #show topic analysis results  
  observeEvent(input$showTopicAnalysisResults,{
    rv$top_topic_model=d$top_topic_model
    rv$coherence_mat=d$top_topic_model$coherence
  })
  #d$top_topic_model$topModel$summary
  #table top topic model summary
  output$tableTopicModelSummary = DT::renderDataTable({
    datatable(rv$top_topic_model$topModel$summary, rownames = F,
              colnames = c("Topic", "Cluster", "Coherence,%", "Documents", "Top Terms"),
#              colnames = c("Topic", "Cluster", "Coherence,%", "Documents", "Top Terms", "Top Terms Prime"),
              escape = F,
              options = list(
                lengthMenu = list(c(7, 15, 30), c('5', '15', "30")),
                pageLength = 7
              ),
              selection = 'single')
  })
  output$plotTopicModelDendrogram=renderPlot({
    if(is.null(rv$top_topic_model$topModel$hclust))return(NULL)
    plot(rv$top_topic_model$topModel$hclust)
    rect.hclust(rv$top_topic_model$topModel$hclust, k = length(unique(rv$top_topic_model$topModel$hclust$clustering)))
    })
  output$plotTopicModelCoherence=renderPlot({
    plot(rv$coherence_mat, type = "o")
    })
  observeEvent(input$checkForUpdates,{
    found_files<-check_for_updates_shiny_app(input$sourcePath, input$destinationPath)
    if(found_files==""){
      showModal(modalDialog(
        title = "Important message",
        paste("No files to update. You are using latest version."),
        easyClose=T,
        footer = tagList(
          modalButton("OK")) ))
    }else{
      showModal(modalDialog(
        title = "Important message",
        paste(found_files, "Continue?"),
        easyClose=T,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("okUpdateShinyApp", "OK"))
      ))
    }
  })
  observeEvent(input$okUpdateShinyApp,{
    removeModal()
    if (T) {
      updated_files<-update_shiny_app(input$sourcePath, input$destinationPath)
      showModal(modalDialog(
        title = "Important message",
        paste(updated_files),
        easyClose=T,
        footer = tagList(
          modalButton("OK"))
      ))
    }
  })
  onStop(function() {
    #save main bibliography data.frame
    saveWoSLibrary(dfWoS)
    #save last bibliography records file
    lastResearchPaperFolder<-file.path(g$paths$papers, d$select$exportFolder)
    if(file.exists(lastResearchPaperFolder)){
      file_path<-file.path(lastResearchPaperFolder,g$files$rdsbib)
      #print(rv$WoSExportDT)
      write_rds(dfWoSExport, file_path)
      write_rds(d, document_path)
      # file_path2<-file.path(lastResearchPaperFolder,paste("bibliography.csv", sep=''))
      # write_csv(dfWoSExport, file_path2)
    }
    cat("Session stopped and saved\n")
    stopApp()
#    write_csv(data.frame(A=rnorm(10), B=rnorm(10)), csvConfig)
  })
})