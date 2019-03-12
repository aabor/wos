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
#' dfWoS<-import_bibtex_and_pdf(dfWoS)
import_bibtex_and_pdf<-function(dfWoS, progress=NULL, deleteSourcePDFs=F){
  "Started" %>%   
    echo("import_bibtex_and_pdf", T, progress)
  dfWoS %<>% 
    mutate(doi=str_to_lower(doi))
  "loading bibliogrpaphy files from disk..." %>% 
    echo("import_bibtex_and_pdf", T, progress)
  nrecordsAtStart<-nrow(dfWoS)
  rx <- "\\{(?:[^{}]*|(?R))*\\}" # matching balanced curly brackets
  files <- dir(g$paths$new_pdf, pattern = "\\.bib|\\.bibtex|\\.txt$", full.names = TRUE, recursive = TRUE)
  if(length(files)==0){
    "No new bib records found, aborting operation..." %>%
      echo("import_bibtex_and_pdf", F, progress)
    return(dfWoS)
  }
  bibs <- map(files, function(f) {
    paste("extracting bib references", basename(f), "...") %>% 
      echo("import_bibtex_and_pdf", F, progress)
    bib<-readLines(f, encoding = "UTF-8") %>% 
      paste(collapse = " ") %>% 
      str_replace_all(pattern = '\n|\r|\t', ' ') %>% 
      str_replace_all('&Amp;', '&') %>% 
      regmatches(., gregexpr(pattern=rx, ., perl = TRUE)) %>% 
      map(str_sub, 2, -2) %>% 
      map(str_trim) %>% 
      map(str_squish) %>% 
      map(str_replace, pattern = "[ ,]+", replacement = ",")
    paste("done") %>% 
      echo("import_bibtex_and_pdf", F, progress)
    bib
  }) %>% unlist
  paste(length(bibs), "bibliography items loaded\r\n") %>% 
    echo("import_bibtex_and_pdf", F, progress)
  
  files_to<-file.path(g$paths$bibarchive, basename(files))
  if(deleteSourcePDFs){
    res<-sum(file.rename(files, files_to))
    res %c% " bibliography files moved to bib archive\r\n" %>% 
      echo("import_bibtex_and_pdf", F, progress)
  }else{
    res<-sum(file.copy(files, files_to, overwrite = T))
    res %c% " bibliography files copied to bib archive\r\n" %>% 
      echo("import_bibtex_and_pdf", F, progress)
  }
  paste('converting', length(bibs), '.bibtex records to data.frame...') %>% 
    echo("import_bibtex_and_pdf", F, progress)
  counter <- 0
  dfLoadedBibs<-tibble(idx=1:length(bibs), bibs_raw=!!bibs) %>% 
    mutate(bibdf=pmap(list(bibs_raw), function(bib){
      tryCatch({
        counter<-counter+1
        tibble(fs=str_match_all(bib, ',\\s*([\\w]+)\\s*=\\s*\\"?\\{?')[[1]][, 2] %>% 
                 tolower, 
              fvs=regmatches(bib, gregexpr(pattern='=\\s*\\"?\\{?(?:[^"{}]*|(?R))*\\}?\\"?', bib, perl = TRUE)) %>% 
                unlist %>% 
                str_remove_all(pattern='=\\s?\\"?\\{?|\\}?\\"?') %>% 
                str_squish)
        }, error=function(e){
                  echo(e)
                  echo(bib)
                  echo("bib position: " %c% counter)
                  tibble(fs=character(), fvs=character())
                })
    })) %>% 
    select(-bibs_raw) %>% 
    unnest() %>% 
    spread(key=fs, value=fvs) %>% 
    filter(!is.na(author) & !is.na(doi)) %>% 
    bind_rows(tibble(doi=character(), journal=character(), author=character(), 
                     year=integer(), volume=integer(), number=integer(), 
                     month=character(), pages=character(), title=character(), 
                     keywords=character(), abstract=character(), file=character())) %>% 
    mutate(year=str_replace_all(year, pattern="[^\\d]", ''),
           doi=str_replace(doi, pattern="https://doi.org/", replacement = "")) %>% 
    map_dfc(str_trim) %>% 
    mutate(journal=str_replace_all(journal, "[^[[:alpha:]]|[[:punct:]]|[[:space:]]]", '')) %>% 
    mutate(journal=str_replace(journal, pattern = "JCMS: ", replacement = "")) %>% 
    mutate(pages=str_replace_all(pages, pattern = "[^\\d]+", '-'),
           journal=str_replace_all(journal, pattern = '&', 'and'),
           abstract=str_replace_all(abstract, "^(?i)abstract", '')) %>% 
    left_join(d$journals %>% 
                mutate(journal=mydbtitle) %>% 
                select(journal, publisher3), by="journal") %>% 
    correctAuthorName(progress) %>% 
    createBibKeys(progress) %>% 
    mutate(year=as.numeric(year),
           number=as.numeric(number),
           updated=Sys.time()) %>% 
    arrange(journal, desc(year), desc(volume)) %>% 
    select(key, doi, publisher3, journal, author, year, volume, number, month, pages, 
         title, keywords, abstract, file, updated)
  
  "converted successfully " %c% nrow(dfLoadedBibs) %c% " bibliography records\r\n" %>% 
    echo("import_bibtex_and_pdf", F, progress)
  nbefore<-nrow(dfWoS)
  records_to_update<-intersect(dfLoadedBibs$key, dfWoS$key)
  nrecords_to_update<-length(records_to_update)
  dfWoS<-dfWoS %>% 
    filter(!(key %in% records_to_update)) %>% 
    filter(!is.na(key)) %>% 
    bind_rows(dfLoadedBibs) %>% 
    distinct(key, .keep_all = T) %>% 
    filter(!is.na(journal)) %>% 
    mutate(doi=str_to_lower(doi))
  nafter<-nrow(dfWoS)
  paste(nbefore - nafter, "records with duplicate keys have been deleted", nrecords_to_update, "will be updated\r\n") %>% 
    echo("import_bibtex_and_pdf", F, progress)
  
  files<-dir(g$paths$new_pdf, pattern = "\\.zip$", full.names = TRUE, recursive = TRUE)
  nfiles<-length(files)
  paste("extracting", nfiles, ".zip archives with full texts...") %>% 
    echo("import_bibtex_and_pdf", F, progress)
  if(nfiles>0){
    map(files, unzip, exdir=g$paths$new_pdf)
    file.remove(files)
  }
  paste(nfiles, "archives extracted.", nfiles,".zip files deleted...\r\n",
             "Reading .pdf files\r\n") %>% 
    echo("import_bibtex_and_pdf", F, progress)
  
  files <- dir(g$paths$new_pdf, pattern = "\\.pdf$", full.names = TRUE, recursive = TRUE)
  paste(length(files), ".pdf files found for proccessing") %>% 
    echo("import_bibtex_and_pdf", F, progress)
  df_filename_doi<-get_filename_doi(files, progress)
  "adding full text file paths to loaded bibliography records\r\n" %>% 
    echo("import_bibtex_and_pdf", F, progress)
  dfNewPDFs<-dfWoS %>% 
    select(-file) %>% 
    right_join(df_filename_doi, by="doi") %>% 
    mutate(file_from=ifelse(is.na(file), NA, file.path(g$paths$new_pdf, basename(file))),
           file_to=ifelse(is.na(file), NA,file.path(g$paths$pdf, paste(key,".pdf", sep="")))) %>% 
    mutate(file=ifelse(is.na(file), NA,paste(key,".pdf", sep="")))
  if(nrow(dfNewPDFs)>0){
      if(deleteSourcePDFs){
        "deleting source pdf files..." %>% 
          echo("import_bibtex_and_pdf", F, progress)
        dfNewPDFs %>% 
          mutate(res=pmap(list(file_from, file_to), function(ff, ft){
            res<-file.rename(from = ff, to = ft)
            paste("file", ff, "deleted", ft) %>% 
              echo("import_bibtex_and_pdf", F, progress)
            res
          }))
        paste(nrow(dfNewPDFs), "files deleted\r\n") %>% 
          echo("import_bibtex_and_pdf", F, progress)
      }else{
        "coping pdf files...\r\n" %>% 
          echo("import_bibtex_and_pdf", T, progress)
        dfNewPDFs %>% 
          mutate(res=pmap(list(file_from, file_to), function(ff, ft){
            res<-file.copy(from = ff, to = ft, overwrite = T)
            paste("file", ff, "copied to", ft, "\r\n") %>% 
              echo("import_bibtex_and_pdf", F, progress)
            res
          }))
        paste(nrow(dfNewPDFs), "files copied") %>% 
          echo("import_bibtex_and_pdf", F, progress)
      }
  }
  titles_to_delete<-c("Editorial","Guest Editorial", 
                      "Special Issue Editors Introduction",
                      "Editors Note","Comment","Preface")
  dfWoS<-dfWoS %>% 
    filter(!(title %in% titles_to_delete)) %>% 
    distinct(key, .keep_all = T) %>% 
    check_pdf_file_references(progress)
  nrecordsAtEnd<-nrow(dfWoS)
  nNewRecords<-nrecordsAtEnd-nrecordsAtStart
  paste("In old data base", nrecordsAtStart, "records, in updated data base", nrecordsAtEnd,
        "records \r\nIn total", nNewRecords, "records added \r\n") %>% 
    echo("import_bibtex_and_pdf", F, progress)
  "saving data.frame in .bibtex format\r\n" %>% 
    echo("import_bibtex_and_pdf", T, progress)
  dfWoS %<>% filter(!is.na(key))
  
  saveDataFrameToBibTeX(dfWoS, g$files$bibWoS)
  "saving data.frame in .rds format\r\n" %>% 
    echo("import_bibtex_and_pdf", F, progress)
  saveWoSLibrary(dfWoS)
  "pdf files imported, data frame saved\r\n" %>% 
    echo("import_bibtex_and_pdf", F, progress)
  dfWoS
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
get_filename_doi<-function(files, progress=NULL){
  nfiles<-length(files)
  if(nfiles==0)return({
    tibble(file=character(),doi=character())
    })
  df<-tibble(file=files, 
                 doi=map(files,extract_doi_from_metadata, progress) %>% 
                   unlist) %>% 
    subset(!is.na(doi))  
  "found doi in " %c% nrow(df) %c% "PDF files from total " %c% nfiles %c% "files" %>% 
    echo("get_filename_doi", F, progress)
  notfound<-setdiff(files, df$file)
  if(length(notfound)>0){
    paste("warning: cannot find doi for", length(notfound),"files:", 
               notfound%>%
                 basename() %>% 
                 glue_collapse(sep = ", ", last=" and "), "\r\n") %>% 
      echo("get_filename_doi", F, progress)
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
    echo("get_filename_doi", F, progress)
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
  "Started" %>% 
    echo("load_bib_entries_for_downloaded_pdfs", T, progress)
  files<-dir(g$paths$new_pdf, pattern = "\\.pdf$", full.names = TRUE, recursive = TRUE)
  dois<-get_filename_doi(files, progress)
  #dois1<-dois[20:21,]
  "searching bib entries in http://dx.doi.org\r\n" %>% 
    echo("load_bib_entries_for_downloaded_pdfs", T, progress)
  
  dfResult<-dois1 %>% 
    rowwise() %>% 
    do({
      bib_path<-paste(tools::file_path_sans_ext(.$file), ".bib", sep="")
      bibFound<-F
      tryCatch({
        paste("searching bib entry for", basename(.$file), "\r\n") %>% 
          echo("load_bib_entries_for_downloaded_pdfs", F, progress)
        GetBibEntryWithDOI(.$doi, 
                           temp.file = , 
                           delete.file = F)
        bibFound<-T
      }, error = function(e) {    
        paste("could not get bib entry\r\n", 
              "error message:", e, "\r\n") %>% 
          echo("load_bib_entries_for_downloaded_pdfs", F, progress, level="error")
      })
      Sys.sleep(10)
      data.frame(doi=.$doi, file=basename(.$file), bib=ifelse(bibFound, basename(bib_path), NA))
    })
  bibs_loaded<-nrow(dfResult) - dfResult %>% pull(bib) %>% is.na %>% sum()
  results_report_path<-file.path(g$paths$db, "bib_entries_loaded.csv")
  paste(nrow(dois), 
             "total dois found", 
             bibs_loaded, 
             "bib entries downloaded, report saved to", 
             results_report_path, "\r\n") %>% 
    echo("load_bib_entries_for_downloaded_pdfs", F, progress)
  write_csv(dfResult, results_report_path)
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
#' progress<-NULL
#' files <- dir(g$paths$new_pdf, pattern = "\\.pdf$", full.names = TRUE, recursive = TRUE)
#' basename(files)
#' (pdf_file_name<-files[78])
extract_doi_from_metadata<-function(pdf_file_name, progress=NULL){
  "looking: " %c% basename(pdf_file_name) %>% 
    echo("extract_doi_from_metadata", T, progress)
  doi<-NULL
  info<-NULL
  tryCatch({
    info<-pdf_info(pdf_file_name)
  }, error = function(e) {    
    paste("could not read metadata\r\n", 
              "error message:", e, "\r\n") %>% 
      echo("extract_doi_from_metadata", F, progress, level="error")
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
          doi %c% " in key" %>% 
            echo("extract_doi_from_metadata", F, progress)
          return(doi)
        }
      }
    }, error = function(e) {    
      paste("Error reading metadata\r\n", 
            "error message:", e, "\r\n") %>% 
        echo("extract_doi_from_metadata", F, progress, level="error")
    })
    # try to get doi from deep metadata tags (not visible from Acrobat Reader)
    tryCatch({
      html<-read_html(info$metadata)
      dois<-html_nodes(html, "doi") %>% html_text()
      if(length(dois)>0){
        doi<-dois[1]
        doi %c% " in meta data tags" %>% 
          echo("extract_doi_from_metadata", F, progress)
        return(doi)
      }
    }, error = function(e) {    
      paste("Error parsing html metadata code\r\n", 
            "error message:", e, "\r\n") %>%     
        echo("extract_doi_from_metadata", F, progress)
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
      echo("extract_doi_from_metadata", F, progress, level = "error")
    return(NA)
  })
  paste("searching doi in full text...") %>% 
    echo("extract_doi_from_metadata", F, progress)
  doi<-first_page %>%
    get_doi_from_first_page
  if(!is.na(doi)){
    doi %<>%  str_replace("doi ", "") %>% 
      str_trim()
    doi %c% " in the first page text" %>% 
      echo("extract_doi_from_metadata", F, progress)
    return(doi)
  }else{
    paste("searching doi on the first page image...") %>% 
      echo("extract_doi_from_metadata", F, progress)
    pdf_render_page(pdf_file_name, page=1, dpi=600) %>% 
      png::writePNG(g$files$pdf_first_page)
    doi<-ocr(g$files$pdf_first_page) %>% 
      get_doi_from_first_page
    if(!is.na(doi)){
      doi %<>%  str_replace("doi ", "") %>% 
        str_trim()
      paste(doi, " in the optically recognized first page text") %>% 
        echo("extract_doi_from_metadata", F, progress)
      return(doi)
    }
  }
  "doi not found" %>% 
      echo("extract_doi_from_metadata", F, progress)
    return(NA)
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
#' check_pdf_file_references(dfWoS)
check_pdf_file_references<-function(dfWoS, progress=NULL){
  "correcting file names..." %>% 
    echo("check_pdf_file_references", F, progress)
  dfWoS %<>% 
    mutate(file_name=paste(key,".pdf", sep=""),
           file_path=file.path(g$paths$pdf, file_name)) %>% 
    mutate(file=ifelse(file.exists(file_path), file_name, NA)) %>% 
    select(-file_name, -file_path) %>% 
    mutate(year=ifelse(is.na(year), get_pdf_creation_year(file), year))
  "file names corrected,\r\ncounting full text records..." %>% 
    echo("check_pdf_file_references", T, progress)
  stat<-file.path(g$paths$pdf, dfWoS$file) %>% file.exists %>% sum
  paste(stat, "full text found.\r\n") %>% 
    echo("check_pdf_file_references", T, progress)
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
