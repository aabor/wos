require(RSelenium)
ePrefs <- makeFirefoxProfile(
  list(
    browser.download.dir = "/home/seluser/Downloads",
    "browser.download.folderList" = 2L,
    "browser.download.manager.showWhenStarting" = FALSE,
    "browser.helperApps.neverAsk.saveToDisk" = "text/bib, multipart/x-zip,application/zip,application/x-zip-compressed,application/x-compressed,application/msword,application/csv,text/csv,image/png ,image/jpeg, application/pdf, text/html,text/plain,  application/excel, application/vnd.ms-excel, application/x-excel, application/x-msexcel, application/octet-stream"))
remDr <- remoteDriver(remoteServerAddr = "selenium", 
                      extraCapabilities = ePrefs, 
                      port = 4444L)

remDr <- remoteDriver(remoteServerAddr = "selenium", 
                      port = 4444L, 
                      extraCapabilities = eCaps)
remDr$open()
remDr$navigate("http://www.colorado.edu/conflict/peace/download/")
firstzip <- remDr$findElement("xpath", "//a[contains(@href, 'zip')]")
firstzip$clickElement()
remDr$screenshot(T)
list.files("/home/rstudio/Downloads/")
remDr$quit()
