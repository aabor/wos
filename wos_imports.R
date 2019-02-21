#Main R packages
# tidyverse, includes the following packages:
# ggplot2 is a system for declaratively creating graphics, based on The Grammar of Graphics. You provide the data, tell ggplot2 how to map variables to aesthetics, what graphical primitives to use, and it takes care of the details. Learn more ...
# dplyr provides a grammar of data manipulation, providing a consistent set of verbs that solve the most common data manipulation challenges. Learn more ...
# tidyr provides a set of functions that help you get to tidy data. Tidy data is data with a consistent form: in brief, every variable goes in a column, and every column is a variable. Learn more ...
# readr provides a fast and friendly way to read rectangular data (like csv, tsv, and fwf). It is designed to flexibly parse many types of data found in the wild, while still cleanly failing when data unexpectedly changes. Learn more ...
# purrr enhances R’s functional programming (FP) toolkit by providing a complete and consistent set of tools for working with functions and vectors. Once you master the basic concepts, purrr allows you to replace many for loops with code that is easier to write and more expressive. Learn more ...
# tibble is a modern re-imagining of the data frame, keeping what time has proven to be effective, and throwing out what it has not. Tibbles are data.frames that are lazy and surly: they do less and complain more forcing you to confront problems earlier, typically leading to cleaner, more expressive code. Learn more ...
# stringr provides a cohesive set of functions designed to make working with strings as easy as possible. It is built on top of stringi, which uses the ICU C library to provide fast, correct implementations of common string manipulations. Learn more ...
# forcats provides a suite of useful tools that solve common problems with factors. R uses factors to handle categorical variables, variables that have a fixed and known set of possible values.
library(tidyverse)
# Unit Testing for R
library(testthat)
# Reading, writing and manipulating Microsoft Excel files from within R, based on Java.
library(XLConnect)
# Reading, writing and manipulating Microsoft Excel files from within R, not depended on Java.
# can read large Excel files
library(readxl)
# for dates and date-times
library(lubridate)
# magrittr provides the pipe, %>% used throughout the tidyverse. It also provide a number of more specialised piping operators (like %$% and %<>%) that can be useful in other places.
library(magrittr)
#provides an alternative to paste() that makes it easier to combine data and strings.
library(glue)
# it turns models into tidy data which you can then wrangle and visualise using the tools you already know.
library(broom)
# eXtensible Time Series. 
# Provide for uniform handling of R's different time-based data classes by extending zoo, 
# maximizing native format information preservation and allowing for user level customization 
# and extension, while simplifying cross-class interoperability.
library(xts)
# Deployment Interface for R Markdown Documents and Shiny Applications. 
library(rsconnect)
# Fast and friendly way to read rectangular data (like 'csv', 'tsv', and 'fwf'). 
# It is designed to flexibly parse many types of data
# read_csv(), write_csv()
library(readr)
# wrappers around the fantastic 'stringi' package
library(stringr)
# Low-level API for programming with R
library(rlang)
#JSON Parser and Generator for R
library(jsonlite)
# Parsing and Generating XML Within R
library(XML)
# save lapply operations
library(purrr)

#######################################################################################
##Graphics
# Functions to improve user experience of shiny apps by Dean Attali
library(shinyjs)
# Create dashboards with 'Shiny'. A dashboard has three parts: a header, a sidebar, and a body. 
library(shinydashboard)
# Dashboard themes
library(dashboardthemes)
# provides an interface between R and the Google. Charts API. Google Charts
# offer interactive charts which can be embedded into web pages. The best known
# of these charts is probably the Motion Chart
library(googleVis)
# A Wrapper of the JavaScript Library 'DataTables'. 
# Data objects in R can be rendered as HTML tables using the JavaScript library 'DataTables' 
# (typically via R Markdown or Shiny).
library(DT)
# Plotly's R graphing library makes interactive, publication-quality graphs online. 
# line plots, scatter plots, area charts, bar charts, error bars, box plots, histograms, heatmaps, subplots, multiple-axes, and 3D (WebGL based) charts
library(plotly)
# An R interface to the 'dygraphs' JavaScript charting library. 
# Charting time-series data in R, including highly configurable series, 
# axis-display and interactive features like zoom/pan and series/point highlighting.
library(dygraphs)

#######################################################################################
## Text functions
# Bibliography reference manager
library(RefManageR)
# Retrieve data from RSS/Atom feeds
library(feedeR)
# Working with text
library(textshape)
# transcript analysis, Text Mining/ Natural Language Processing: frequency counts of sentence types, words, sentences, turns of talk, syllables and other assorted analysis tasks
library(qdap)
# Text Mining
library(tm)
# Text Extraction, Rendering and Converting of PDF Documents. Utilities based on 'libpoppler' for extracting text, fonts, attachments and metadata from a PDF file. Also supports high quality rendering of PDF documents info PNG, JPEG, TIFF format, or into raw bitmap vectors
library(pdftools)
# Natural Language Processing
library(wordcloud)
# an R interface to WordNet , a large lexical database of English. 
library(wordnet)
# An R interface to Weka (Version 3.9.2). Weka is a collection of machine learning algorithms for data mining tasks written in Java, containing tools for data pre-processing, classification, regression, clustering, association rules, and visualization
library(RWeka)
# an R interface to KEA (Version 5.0). KEA (for Keyphrase Extraction Algorithm) allows for extracting keyphrases from text documents. It can be either used for free indexing or for indexing with a controlled vocabulary.
library(RKEA)
# Porter's word stemming algorithm. 
library(SnowballC)
# helps split text into tokens, supporting shingled n-grams, skip n-grams, words, word stems, sentences, paragraphs, characters, lines, and regular expressions. 
library(tokenizers)
# Automatic Text Classification via Supervised Learning
library(RTextTools)
# wrapper for several topic models that take similarly-formatted input and give similarly-formatted output
library(textmineR)
#approximate string matching version of R's native 'match' function
library(stringdist)
#collection of regular expression tools associated with the 'qdap' package that
#may be useful outside of the context of discourse analysis. Tools include
#removal/extraction/replacement of abbreviations, dates, dollar amounts, email
#addresses, hash tags, numbers, percentages, citations, person tags, phone
#numbers, times, and zip codes.
library(qdapRegex)
# OCR - Optical character recognition
library(tesseract)
# web scrapping
# Wrappers around the 'xml2' and 'httr' packages to make it easy to download, then manipulate, HTML and XML.
library(rvest)
# Tools for Descriptive Statistics first descriptive tasks in data analysis,
# consisting of calculating descriptive statistics, drawing graphical summaries and reporting the results
# %overlaps% determines if two date ranges overlap
library(DescTools)
