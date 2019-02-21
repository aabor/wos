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
getWoSDT <- function(df) {
  
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
#' Write data.frame to Excel
#'
#' @param xlsxBookFileName string path to Excel book
#' @param xlsxSheetName string sheet name in Excel book
#' @param df data.frame
#' @param progress shiny progress bar
#'
#' @return
#' @export
#'
#' @examples
dfToExcel <- function(xlsxBookFileName, xlsxSheetName, df, progress=NULL) {
  df[nrow(df) + 1,] <- NA
  df[nrow(df), 1] <- paste('Last updated', format(Sys.time(), "%a %b %d %X %Y"))
  print(paste(msg, 'sheet named', xlsxSheetName))
  book <- loadWorkbook(xlsxBookFileName)
  setStyleAction(book, XLC$"STYLE_ACTION.NONE")
  tryCatch({
    clearSheet(book, xlsxSheetName)
  }, error = function(e) {
    #        cat(paste(e, '\n'))
    cat(paste('Creating new sheet', '\n'))
  })
  createSheet(book, xlsxSheetName)
  writeWorksheet(book, sheet = xlsxSheetName, df%>%as.data.frame)
  saveWorkbook(book, xlsxBookFileName)
  print(paste('Data frame saved to sheet', xlsxSheetName, 'in', xlsxBookFileName))
}
#' Load data.frame from Excel
#'
#' @param xlsxBookFileName string path to Excel book
#' @param xlsxSheetName string sheet name in Excel book
#' @param df data.frame
#' @param progress shiny progress bar
#'
#' @return
#' @export
#'
#' @examples
#' xlsxBookFileName<-g$files$xlsxSelectedJournals
#' xlsxSheetName<-"selected_journals"
dfFromExcel <- function(xlsxBookFileName, xlsxSheetName, progress=NULL) {
  book <- loadWorkbook(xlsxBookFileName)
  progress_nstep<<-progress_nstep + 1
  msg<-'Excel workbook loaded...'
  if (!is.null(progress)) {
    updateProgress(progress, detail = msg)
  }
  df <- readWorksheet(book, sheet = xlsxSheetName)
  df<-df[!str_detect(df[, 1], pattern = 'Last updated'),]
  progress_nstep<<-progress_nstep + 1
  msg<-'Excel worksheet has been red...'
  if (!is.null(progress)) {
    updateProgress(progress, detail = msg)
  }
  print(paste('Data frame loaded from sheet', xlsxSheetName, 'in', xlsxBookFileName))
  df
}
#' Save library with backed up file
#'
#' @param dfWoS data.frame with bibliography records
#'
#' @return
#' @export
#'
#' @examples
#' saveWoSLibrary(dfWoS)
saveWoSLibrary<-function(dfWoS){
  file.copy(g$files$rdsWoS, g$files$rdsWoS_old)
  cat(paste("library backed up in file", g$files$rdsWoS_old, '\n'))
  write_rds(dfWoS, g$files$rdsWoS)
  cat(paste("library saved in file", g$files$rdsWoS, '\n'))
  #write_csv(dfWoS, g$files$csvWoS_old)
}

