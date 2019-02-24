#' Import bib references and full text
#'
#' Main import function, organizes complete pipe from reading bibliographic
#' information, importing full texts and computing library statistics
#'
#'
#' @param df data.frame with bibliography records that will be updtated.
#' @param deleteSourcePDFs boolean if deletion of source pdf files is allowed
#'
#' @return function saves data.frame in .bib and .rds formats with backup,
#'   computes library statistics. All messages and data frames are returned in
#'   list
#' @export
#'
#' @examples
#' progress<-NULL
#' deleteSourcePDFs<-F
#' import_bibtex_and_pdf(dfWoS)
import_bibtex_and_pdf<-function(dfWoS, progress=NULL, deleteSourcePDFs=F){
  #start log
  log_path <- file.path(g$paths$db,'reindex_dataframe.txt')
  if(file.exists(log_path))file.remove(log_path)
  log_con <- file(log_path, open = "a")
  paste(format(Sys.time(),"%Y-%m-%d %H:%M:%S", tz=g$tz), "log started\r\n") %>%   
    give_echo(log_con, T, progress)
  #main 
  dfWoS %<>% 
    mutate(doi=str_to_lower(doi))
  "loading bibliogrpaphy files from disk...\r\n" %>% 
    give_echo(log_con, T, progress)
  nrecordsAtStart<-nrow(dfWoS)
  rx <- "\\{(?:[^{}]*|(?R))*\\}" # matching balanced curly brackets
  files <- dir(g$paths$new_pdf, pattern = "\\.bib|\\.bibtex|\\.txt$", full.names = TRUE, recursive = TRUE)
  if(length(files)>0){
    bibs <- map(files, function(f) {
      paste("extracting bib references", basename(f), "...") %>% 
        give_echo(log_con, F, progress)
      bib<-readLines(f, encoding = "UTF-8") %>% 
        paste(collapse = " ") %>% 
        str_replace_all(pattern = '\n|\r|\t', ' ') %>% 
        str_replace_all('&Amp;', '&') %>% 
        regmatches(., gregexpr(pattern=rx, ., perl = TRUE)) %>% 
        map(str_sub, 2, -2) %>% 
        map(str_trim) %>% 
        map(str_squish) %>% 
        map(str_replace, pattern = "[ ,]+", replacement = ",")
      paste("done\r\n") %>% 
        give_echo(log_con, F, progress)
      bib
    }) %>% unlist
    paste(length(bibs), "bibliography items loaded\r\n") %>% 
      give_echo(log_con, F, progress)
    
    if(deleteSourcePDFs){
      rem_res<-sum(file.remove(files))
      paste(rem_res, "bibliography files removed from disk\r\n") %>% 
        give_echo(log_con, F, progress)
    }
    paste('converting', length(bibs), '.bibtex records to data.frame...') %>% 
      give_echo(log_con, T, progress)
    
    non_digit <- "[^\\d]"
    lLoadedBibs<-map(bibs, bib_to_df, log_con, progress) 
    dfLoadedBibs<-lLoadedBibs[!is.na(lLoadedBibs)] %>% 
      reduce(bind_rows)%>% 
      mutate(year=str_replace_all(year, pattern=non_digit, '')) %>% 
      select(doi, journal, author, year, volume, number, month, pages, title, keywords, abstract) %>% 
      subset(!is.na(author)) %>% 
      subset(!is.na(doi)) %>% 
      mutate(doi=str_replace(doi, pattern="https://doi.org/", replacement = "")) %>% 
      mutate(doi=str_trim(doi),
             author=str_trim(author),
             year=str_trim(year),
             volume=str_trim(volume),
             number=str_trim(number),
             month=str_trim(month),
             pages=str_trim(pages),
             journal=str_trim(journal),
             title=str_trim(title),
             keywords=str_trim(keywords),
             abstract=str_trim(abstract)) %>% 
      mutate(journal=  str_replace_all(journal, "[^[[:alpha:]]|[[:punct:]]|[[:space:]]]", '')) %>% 
      mutate(pages=str_replace_all(pages, pattern = "[^\\d]+", '-'),
             journal=str_replace_all(journal, pattern = '&', 'and'),
             abstract=str_replace_all(abstract, "^(?:Abstract |ABSTRACT|Abstract)", '')) %>% 
      mutate(key=NA, jacro=NA, jscore=NA, quartile=NA, top10=NA, ascore=NA, nrecords=NA, 
             publisher3=NA, publisher=NA, updated=NA,
             file=NA, url=NA) %>% 
      addCiteScores(log_con, progress) %>% 
      correctAuthorName(log_con, progress) %>% 
      createBibKeys(log_con, progress) %>% 
      allVariablesToCharacters() %>% 
      mutate(year=as.numeric(year),
             number=as.numeric(number),
             jscore=as.numeric(jscore),
             ascore=as.numeric(ascore),
             nrecords=as.numeric(nrecords),
             updated=Sys.time()) %>% 
      arrange(journal, desc(year), desc(volume))
    paste("converted successfully", nrow(dfLoadedBibs), "bibliography records\r\n") %>% 
      give_echo(log_con, F, progress)
    nbefore<-nrow(dfWoS)
    records_to_update<-intersect(dfLoadedBibs$key, dfWoS$key)
    nrecords_to_update<-length(records_to_update)
    dfWoS %>% names
    #add new records to old data.frame
    # dfWoS %<>% 
    #   mutate(number=as.numeric(number))
    dfWoS<-dfWoS %>% 
      filter(!(key %in% records_to_update)) %>% 
      bind_rows(dfLoadedBibs) %>% 
      distinct(key, .keep_all = T) %>% 
      filter(!is.na(journal)) %>% 
      mutate(doi=str_to_lower(doi)) %>% 
      sort_columns_in_wos_dataframe
    nafter<-nrow(dfWoS)
    paste(nbefore - nafter, "records with duplicate keys have been deleted", nrecords_to_update, "will be updated\r\n") %>% 
      give_echo(log_con, F, progress)
    "saving data.frame in .rds format\r\n" %>% 
      give_echo(log_con, F, progress)
    saveWoSLibrary(dfWoS)
  }

  files<-dir(g$paths$new_pdf, pattern = "\\.zip$", full.names = TRUE, recursive = TRUE)
  nfiles<-length(files)
  paste("extracting", nfiles, ".zip archives with full texts...") %>% 
    give_echo(log_con, F, progress)
  if(nfiles>0){
    map(files, unzip, exdir=g$paths$new_pdf)
    file.remove(files)
  }
  paste(nfiles, "archives extracted.", nfiles,".zip files deleted...\r\n",
             "Reading .pdf files\r\n") %>% 
    give_echo(log_con, F, progress)
  
  files <- dir(g$paths$new_pdf, pattern = "\\.pdf$", full.names = TRUE, recursive = TRUE)
  paste(length(files), "files will be processed\r\n") %>% 
    give_echo(log_con, F, progress)
  df_filename_doi<-get_filename_doi(files, log_con, progress)
  "adding full text file paths to loaded bibliography records\r\n" %>% 
    give_echo(log_con, F, progress)
  dfNewPDFs<-dfWoS %>% 
    select(-file) %>% 
    right_join(df_filename_doi, by="doi") %>% 
    mutate(file_from=ifelse(is.na(file), NA, file.path(g$paths$new_pdf, basename(file))),
           file_to=ifelse(is.na(file), NA,file.path(g$paths$pdf, paste(key,".pdf", sep="")))) %>% 
    mutate(file=ifelse(is.na(file), NA,paste(key,".pdf", sep="")))
  res<-0
  if(nrow(dfNewPDFs)>0){
      if(deleteSourcePDFs){
        "deleting source pdf files...\r\n" %>% 
          give_echo(log_con, T, progress)
        dfNewPDFs %>% 
          rowwise() %>% 
          do({
            res<-data.frame(file=basename(.$file_from), 
                            res=file.rename(from = .$file_from, to = .$file_to))
            paste("file", .$file_from, "deleted\r\n") %>% 
              give_echo(log_con, F, progress)
            res
          })
        paste(nrow(dfNewPDFs), "files deleted\r\n") %>% 
          give_echo(log_con, F, progress)
      }else{
        "coping pdf files...\r\n" %>% 
          give_echo(log_con, T, progress)
        dfNewPDFs %>% 
          rowwise() %>% 
          do({
            res<-data.frame(file=basename(.$file_from), 
                       res=file.copy(from = .$file_from, to = .$file_to, overwrite = T))
            paste("file", .$file_from, "copied to", .$file_to, "\r\n") %>% 
              give_echo(log_con, F, progress)
            res
          })
        paste(nrow(dfNewPDFs), "files copied\r\n")
      }
  }
  titles_to_delete<-c("Editorial","Guest Editorial", 
                      "Special Issue Editors Introduction",
                      "Editors Note","Comment","Preface")
  dfWoS %<>% 
    filter(!(title %in% titles_to_delete)) %>% 
    distinct(key, .keep_all = T) %>% 
    check_pdf_file_references(log_con, progress) %>% 
    addResearchers(log_con, progress)
  nrecordsAtEnd<-nrow(dfWoS)
  nNewRecords<-nrecordsAtEnd-nrecordsAtStart
  report_msg<-paste("In old data base", nrecordsAtStart, "records, in updated data base", nrecordsAtEnd,
        "records \r\nIn total", nNewRecords, "records added \r\n")
  give_echo(report_msg, log_con, F, progress)
  l<-list()
  "saving data.frame in .bibtex format\r\n" %>% 
    give_echo(log_con, T, progress)
  saveDataFrameToBibTeX(dfWoS, g$files$bibWoS)
  "saving data.frame in .rds format\r\n" %>% 
    give_echo(log_con, F, progress)
  saveWoSLibrary(dfWoS)
  "pdf files imported, data frame saved\r\n" %>% 
    give_echo(log_con, F, progress)
  dfWoS<<-dfWoS
  l$report_msg<-paste(report_msg, "\r\nFull log report has been saved to", log_path)
  
  #library statistics
  compute_research_papers_statistics(dfWoS, log_con, progress)
  close(log_con)
  l
}
#' Add additional columns to data frame that indicate the presence of full text and abstract
#'
#' @param dfWoS 
#'
#' @return
#' @export
#'
#' @examples
#' augment_wos_dataframe(dfWoS)
augment_wos_dataframe<-function(dfWoS){
  dfWoS %>% 
    mutate(IsFullText = ifelse(is.na(file) | file == '', F, T),
           IsAbstract = ifelse(is.na(abstract) | abstract == '', F, T))
}
#' Compute research papers statistics
#'
#' @param dfWoS data frame with bibliography records
#' @param log_con log txt file
#' @param progress 
#'
#' @return
#' @export
#'
#' @examples
#' progress<-NULL
#' compute_research_papers_statistics(dfWoS)
compute_research_papers_statistics<-function(dfWoS, log_con=NULL, progress=NULL)
{
  dfWoSaug<-augment_wos_dataframe(dfWoS)
  'computing whole library statistics...\r\n' %>% 
    give_echo(log_con, T, progress)
  start_year<-year(Sys.Date())-3
  df_whole_library_stat <- articlesDescriptives(dfWoSaug, start_year)
  df_whole_library_stat <- left_join(df_whole_library_stat, 
                                     libraryDescriptivesJournals(dfWoSaug, start_year), 
                                     by = 'journal') %>% 
    filter(!is.na(journal))
  df_whole_library_stat %<>% mutate_all(funs(replace(., is.na(.), '0/0'))) %>% 
    mutate(topR=as.numeric(topR),
           verbR=as.numeric(verbR))
  df_whole_library_stat %<>%
    mutate(journal_name=journal) %>% 
    rowwise() %>% 
    mutate(journal=generate_journal_url_tag(journal_name))
  d$libstat$general<<-df_whole_library_stat
  d$libstat$publishers<<-dfWoS %>% 
    group_by(publisher3) %>% 
    summarise(publishers=glue_collapse(unique(publisher), sep="; "),
              njournals=length(unique(journal)),
              avgjscore=round(mean(unique(jscore)), 2),
              nrecords=n(),
              start=min(year),
              end=max(year)) %>% 
    arrange(desc(njournals), desc(avgjscore))
  'whole library statistics was created and exported to configuration...\r\n' %>% 
    give_echo(log_con, F, progress)
  'Creating separate statistics for each journal...\r\n' %>% 
    give_echo(log_con, T, progress)
  d$libstat$detailed<<-separateJournalsStatistics(dfWoSaug, progress)
  'detailed library statistics was created and exported to configuration...\r\n' %>% 
    give_echo(log_con, T, progress)
  'Library statistics created...\r\n' %>% 
    give_echo(log_con, F, progress)
  paste(format(Sys.time(),"%Y-%m-%d %H:%M:%S", tz=g$tz), "log closed\r\n") %>% 
    give_echo(log_con, F, progress)
}
#' Get doi data.frame
#'
#' Read pdf files and create data.frame where each row contains file name and
#' corresponding doi reference if present
#'
#' @param files list of .pdf files
#'
#' @return data.frame
#' @export
#'
#' @examples
#' files<-dir(g$paths$new_pdf, pattern = "\\.pdf$", full.names = TRUE, recursive = TRUE)
#' filename_doi(files) %>% head
#' dois<-get_filename_doi(files)$doi
get_filename_doi<-function(files, log_con=NULL, progress=NULL){
  nfiles<-length(files)
  if(nfiles==0)return({
    data.frame(file="",doi="", stringsAsFactors = F)[0,]
    })
  df<-data.frame(file=files, 
                 doi=map(files,extract_doi_from_metadata, log_con, progress) %>% 
                   unlist, 
                 stringsAsFactors = F) %>% 
    subset(!is.na(doi))  
  paste("in", nrow(df), "PDF files the doi was found in metadata\r\n") %>% 
    give_echo(log_con, F, progress)
  notfound<-setdiff(files, df$file)
  if(length(notfound)>0){
    paste("warning: cannot find doi for", length(notfound),"files:", 
               notfound%>%
                 basename() %>% 
                 glue_collapse(sep = ", ", last=" and "), "\r\n") %>% 
      give_echo(log_con, F, progress)
  }
  dfDup<-df %>% 
    filter(duplicated(.[["doi"]]))
  del_res<-sum(file.remove(dfDup$file))
  paste("deleted", 
        del_res, 
        "pdf documents duplicates", 
        glue_collapse(basename(dfDup$file), 
                 sep = ", ", 
                 last=" and "), 
        "\r\n") %>% 
    give_echo(log_con, F, progress)
  df<-df %>% 
    filter(!duplicated(.[["doi"]])) %>% 
    mutate(doi=str_to_lower(doi))
  df
}
#' Load bib entries using DOI System API
#'
#' Function uses GetBibEntryWithDOI(doi, temp.file = tempfile(fileext = ".bib"),
#' delete.file = TRUE) function from "RefManageR" package to download available
#' bibliography information.
#' 
#' The bibliographic information returned by the search of the http://dx.doi.org.
#'
#' DOIs are extracted from pdf files downloaded previously from journals. Files
#' must be in downloads folder
#'
#' @param progress
#'
#' @return
#' @export
#'
#' @examples
#' progress<-NULL
load_bib_entries_for_downloaded_pdfs<-function(progress){
  log_path <- file.path(g$paths$db,'download_bib_entries_log.txt')
  if(file.exists(log_path))file.remove(log_path)
  log_con <- file(log_path, open = "a")
  paste(format(Sys.time(),"%Y-%m-%d %H:%M:%S", tz=g$tz), "log started\r\n") %>%   
    give_echo(log_con, T, progress)
  
  files<-dir(g$paths$new_pdf, pattern = "\\.pdf$", full.names = TRUE, recursive = TRUE)
  dois<-get_filename_doi(files, log_con, progress)
  #dois1<-dois[20:21,]
  "searching bib entries in http://dx.doi.org\r\n" %>% 
    give_echo(log_con, T, progress)
  
  dfResult<-dois1 %>% 
    rowwise() %>% 
    do({
      bib_path<-paste(tools::file_path_sans_ext(.$file), ".bib", sep="")
      bibFound<-F
      tryCatch({
        paste("searching bib entry for", basename(.$file), "\r\n") %>% 
          give_echo(log_con, F, progress)
        GetBibEntryWithDOI(.$doi, 
                           temp.file = , 
                           delete.file = F)
        bibFound<-T
      }, error = function(e) {    
        paste("could not get bib entry\r\n", 
              "error message:", e, "\r\n") %>% 
          give_echo(log_con, F, progress)
      })
      Sys.sleep(10)
      data.frame(doi=.$doi, file=basename(.$file), bib=ifelse(bibFound, basename(bib_path), NA))
    })
  bibs_loaded<-nrow(dfResult) - dfResult %>% pull(bib) %>% is.na %>% sum()
  results_report_path<-file.path(g$paths$db, "bib_entries_loaded.csv")
  msg<-paste(nrow(dois), 
             "total dois found", 
             bibs_loaded, 
             "bib entries downloaded, report saved to", 
             results_report_path, "\r\n") 
  msg %>% 
    give_echo(log_con, F, progress)
  write_csv(dfResult, results_report_path)
  close(log_con)
  msg
  }
#' Get DOI from PDF
#'
#' Extracts doi from metadata in PDF file. Depends on 'pdftools' library, which
#' allows multiple tasks on .pdf files. In particular, functions reads metadata
#' hided deeply in PDF document, these data cannot be red by simple PDF reader.
#'
#' @param pdf_file_name path to .pdf file
#'
#' @return string with doi if found, NA in other case
#' @export
#'
#' @examples
#' log_con<-NULL
#' progress<-NULL
#' files <- dir(g$paths$new_pdf, pattern = "\\.pdf$", full.names = TRUE, recursive = TRUE)
#' basename(files)
#' (pdf_file_name<-files[78])
extract_doi_from_metadata<-function(pdf_file_name, log_con=NULL, progress=NULL){
  paste("extracting doi from pdf file:", basename(pdf_file_name), "...") %>% 
    give_echo(log_con, F, progress)
  doi<-NULL
  info<-NULL
  tryCatch({
    info<-pdf_info(pdf_file_name)
  }, error = function(e) {    
    paste("could not read metadata\r\n", 
              "error message:", e, "\r\n") %>% 
      give_echo(log_con, F, progress)
  })
  if(!is.null(info)){
    tryCatch({
      #if "doi" tag present extract it and return
      if("doi" %in% names(info$keys)){
        doi<-info$keys[["doi"]]
      }
      if("WPS-ARTICLEDOI" %in% names(info$keys)){
        doi<-info$keys[["WPS-ARTICLEDOI"]]
      }
      if(!is.null(doi)){
        if(doi==""){
          doi<-NULL
        }else{
          paste(doi, "in key\r\n") %>% 
            give_echo(log_con, F, progress)
          return(doi)
        }
      }
    }, error = function(e) {    
      paste("Error reading metadata\r\n", 
            "error message:", e, "\r\n") %>% 
        give_echo(log_con, F, progress)
    })
    # try to get doi from deep metadata tags (not visible from Acrobat Reader)
    tryCatch({
      html<-read_html(info$metadata)
      dois<-html_nodes(html, "doi") %>% html_text()
      if(length(dois)>0){
        doi<-dois[1]
        paste(doi, "in meta data tags\r\n") %>% 
          give_echo(log_con, F, progress)
        return(doi)
      }
    }, error = function(e) {    
      paste("Error parsing html metadata code\r\n", 
            "error message:", e, "\r\n") %>%     
        give_echo(log_con, F, progress)
    })
  }
  # try to det doi from text on the first page of pdf document
  first_page<-""
  tryCatch({
    first_page<-pdf_text(pdf_file_name)
    first_page<-first_page[1]
  }, error = function(e) {    
    paste("Error reading full text\r\n", 
          "error message:", e, "\r\n") %>% 
      give_echo(log_con, F, progress)
    return(NA)
  })
  paste("searching doi in full text...") %>% 
    give_echo(log_con, F, progress)
  doi<-first_page %>%
    get_doi_from_first_page
  if(is.na(doi)){
    paste("searching doi on the first page image...") %>% 
      give_echo(log_con, F, progress)
    pdf_render_page(pdf_file_name, page=1, dpi=600) %>% 
      png::writePNG(g$files$pdf_first_page)
    doi<-ocr(g$files$pdf_first_page) %>% 
      get_doi_from_first_page
  }
  if(!is.na(doi)){
    doi %<>%  str_replace("doi ", "") %>% 
      str_trim()
    msg<-paste(doi, "\r\n")
    cat(msg)
    cat(msg, file =  log_con)
    return(doi)
  }else{
    "not found\r\n" %>% 
      give_echo(log_con, F, progress)
    return(NA)
  }
}
#' Extract doi from plain text
#' 
#' Function uses regular regressions to search doi in plain text
#'
#' @param first_page string with text to search
#'
#' @return string with doi or NA
#' @export
#'
#' @examples
get_doi_from_first_page<-function(first_page){
  doi_pattern<-regex("doi ([[:alnum:]]|[[:punct:]])+")
  first_page %>%
    tolower() %>%
    str_replace_all("[[:space:]]", " ") %>%
    str_replace_all("doi:", "doi ") %>%
    str_replace_all("doi.org/", " doi ") %>% 
    str_replace_all("dx.doi.org/", " doi ") %>% 
    str_replace_all("\\. ", " ") %>%
    str_squish() %>%
    str_extract(doi_pattern)
}

#' Check file references
#'
#' Function checks if pdf file from bibliography reference exists, if not file
#' field in main data.frame will be set to NA
#'
#' @param dfWoS data.frame with bibliography references
#' @param progress shiny progress bar object
#'
#' @return data.frame
#' @export
#'
#' @examples
#' progress<-NULL
#' log_con<-NULL
#' check_pdf_file_references(dfWoS)
#' 
check_pdf_file_references<-function(dfWoS, log_con=NULL, progress=NULL){
  "correcting file names..." %>% 
    give_echo(log_con, F, progress)
  dfWoS %<>% 
    mutate(file_name=paste(key,".pdf", sep=""),
           file_path=file.path(g$paths$pdf, file_name)) %>% 
    mutate(file=ifelse(file.exists(file_path), file_name, NA)) %>% 
    select(-file_name, -file_path) %>% 
    mutate(year=ifelse(is.na(year), get_pdf_creation_year(file), year))
  "file names corrected,\r\ncounting full text records..." %>% 
    give_echo(log_con, T, progress)
  stat<-file.path(g$paths$pdf, dfWoS$file) %>% file.exists %>% sum
  paste(stat, "full text found.\r\n") %>% 
    give_echo(log_con, T, progress)
  dfWoS
}
#' Get pdf creation date
#'
#' @param file_name 
#'
#' @return
#' @export
#'
#' @examples
#' get_pdf_creation_year("wlyWNA419464.pdf")
get_pdf_creation_year<-function(file_name){
  ycreated<-NULL
  f<-file.path(g$paths$pdf, file_name)
  if(file.exists(file_name)){
    tryCatch({
      info<-pdf_info(file_name)
      ycreated<-info[["created"]] %>% as.Date() %>% year
    })    
  }else{
    ycreated<-Sys.Date() %>% year
  }
  ycreated
}
