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
  df %>% as_tibble()
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
  "library backed up in file:" %c% g$files$rdsWoS_old %>% 
    echo("saveWoSLibrary", F)
  write_rds(dfWoS, g$files$rdsWoS)
  "library saved in file" %c% g$files$rdsWoS %>% 
    echo("saveWoSLibrary", F)
  #write_csv(dfWoS, g$files$csvWoS_old)
}
