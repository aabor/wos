source("user_interface.R")
dbHeader <- dashboardHeader(titleWidth = 140)
dbHeader$children[[2]]$children<-shiny::span(shiny::tagList(shiny::tags$img(src='wos-logo.png',
                                                       height='45',
                                                       width='125')))
shinyUI(dashboardPage(title="WoS bibliography manager",
                      dbHeader,
                      dashboardSidebar(wosMenu, width = 125, collapsed = F),
                      dashboardBody(useShinyjs(),
                                    shiny::tags$head(shiny::tags$link(rel = "shortcut icon",
                                                        href = "lighthouse-stroke.png")),
                                    shinyDashboardThemes(theme = "grey_dark"),
                                    tabItems(
                                      tabBibs,
                                      tabResearchers,
                                      tabJournals,
                                      tabLibraryImport,
                                      tabKeywordsAnalysis,
                                      tabTopicAnalysis,
                                      tabAbout
                                      ))))
