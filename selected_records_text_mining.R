#' Suggest term for text mining
#'
#' Functions extracts meaningful words from title and key word from all selected
#' papers and return vectors of unique words
#'
#' @param dfWoSExport data.frame with bibliography records
#'
#' @return vector of strings
#' @export
#'
#' @examples
#' suggested_terms<-get_terms(dfWoSExport)
get_terms<-function(dfWoSExport){
  c(dfWoSExport$keywords %>% words, dfWoSExport$title %>% words) %>% 
    unique %>% 
    tolower %>% 
    VectorSource %>% 
    VCorpus %>% 
    cleanCorpus %>% 
    TermDocumentMatrix %>% 
    as.matrix %>% 
    rowSums %>% 
    names %>% 
    glue_collapse(sep = " ") %>% 
    HTML
}
#' Get terms from one text file
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
get_terms_df<-function(x){
  tibble(term=x,
         detected=str_detect(pages_short_nopunct, pattern=x)) %>% 
    rowid_to_column("wp") %>% 
    filter(detected) %>% 
    select(term, wp)
}

#' Get text chunks from one record only
#' 
#' @param key 
#' @param file 
#' @param author 
#' @param title 
#' @param keywords 
#'
#' @return
#' @export
#'
#' @examples
#' (df<-dfProccessing[1,])
#' key<-df$key
#' file<-df$file
#' author<-df$author
#' title<-df$title
#' keywords<-df$keywords
#' get_text_chunks_from_record(key, file, author, title, keywords)
get_text_chunks_from_record<-function(key, file, author, title, keywords){
  pdf_file<-file.path(g$paths$pdf, basename(file))
  "searching terms in full text: " %c% basename(file) %>% 
    echo("get_text_chunks_from_record", F, NULL)
  text_chunks<-NULL
  print(pdf_file)
  main_text<-pdf_text(pdf_file)
  pages<-ifelse(length(main_text)>2,
                main_text[2:(length(main_text)-1)],
                main_text)
  pages_short<-cleanText(pages) %>% 
    glue_collapse(sep=" ") %>% 
    as.character() %>% 
    str_split(pattern = " ") %>% 
    unlist
  pages_short_nopunct<-pages_short %>% 
    str_replace_all(pattern="[[:punct:]]", "")
  #get_terms_df(suggested_terms_vector[1])
  res<-map_df(suggested_terms_vector, get_terms_df)
  #print(res)
  res<-res %>% 
    group_by(term, wp) %>% 
    summarise(wcount=n()) %>% 
    ungroup %>% 
    top_n(n=5, wcount)
  res_short<-res %>% 
    pull(wp)
  worders<-data.frame(position=res_short, distance=c(NA, diff(res_short)))
  chunks<-worders %>% 
    filter(!is.na(distance)) %>% 
    mutate(new_group=ifelse(distance<10, 0, 1),
           group=cumsum(new_group)) %>% 
    group_by(group) %>% 
    summarise(start=min(position),
              end=max(position),
              n=n()) %>% 
    arrange(desc(n))
  if(nrow(chunks)>3){
    chunks<-chunks[1:3,]
  }
  text_chunks<-chunks %>% 
    mutate(text=pmap(list(start, end), function(start, end){
      start<-ifelse(start>10, start-9, start)
      end<-ifelse(end<(length(pages_short)-10), end+8, end)
      ret<-tibble(k=key, 
                  text=pages_short[start:end] %>% 
                    glue_collapse(sep=" ") %>% 
                    as.character() %>% 
                    str_replace_all("_", " "))
      ret
    })) %>% select(text) %>% 
    unnest
  tibble(key=!!key, 
         reference=get_rmarkdown_reference(key, author, title, keywords), 
         text_chunks=text_chunks %>% 
           pull(text) %>% 
           glue_collapse(sep ="\r\n") 
         %>% as.character()) %>% 
    mutate(reference=highlight_terms(reference, suggested_terms_vector),
           text_chunks=highlight_terms(text_chunks, suggested_terms_vector))
}
#' Get text chunks from selected records
#'
#' @param dfWoSExport
#' @param progress
#'
#' @return data.frame with columns c("key", "reference", "text") key states for each
#'   document, reference is RMarkdown reference, text is text found. For each document not more than three text
#'   chunks are selected if found.
#' @export
#'
#' @examples
#' progress<-NULL
#' pdf_file<-"~/wos_data/pdf/wlyAdamNA549.pdf"
#' suggested_terms<-get_terms(dfWoSExport)
#' get_text_chunks_from_selected_records(dfWoSExport, suggested_terms)
get_text_chunks_from_selected_records<-function(dfWoSExport, suggested_terms, progress=NULL){
  suggested_terms_vector<-words(suggested_terms)
  "Proccessing terms" %c% glue_collapse(suggested_terms_vector, sep = "; ") %>% 
    echo("get_text_chunks_from_selected_records", T, progress)
  dfProccessing<-dfWoSExport %>% 
    mutate(pdf_file=file.path(g$paths$pdf, basename(file))) %>% 
    subset(!is.na(file)) %>% 
    subset(file.exists(pdf_file)) %>% 
    select(key, file, author, title, keywords)
  #dfProccessing<-dfProccessing %>% tail
  if(nrow(dfProccessing)==0){
    "Warning no .pdfs for selected bibliography records, imposible to get text chunks" %>% 
      echo("get_text_chunks_from_selected_records", F, progress, level = "warn")
  }
  dfProccessing %>%
    mutate(data=pmap(list(key, file, author, title, keywords),
                     get_text_chunks_from_record)) %>% 
    select(data) %>% 
    unnest
}
#' Hightligt words in text
#'
#' Function inserts HTML tags in text to highligt words from passed variable
#'
#' @param text string with text, usually long
#' @param terms_to_highlight vector of strings representing words to highlight
#'
#' @return string
#' @export
#'
#' @examples
#' text<-"breakup cost, such as reputation loss ) . Empirically, firm-bank and executive- loan officer relationships are correlated, which suggests that the presence of personal lending relationships may confound previous studies of institutional lending relationships. These results contribute three new findings about lending relationships. First, I find causal evidence that endogenous personal relationships between corporate executives and lenders are a key"
#' highlight_terms(text, suggested_terms_vector)
highlight_terms<-function(text, suggested_terms_vector){
  vignore_case<-c(suggested_terms_vector, str_to_title(suggested_terms_vector))
  rx<-"(" %c% glue_collapse(vignore_case, sep="|") %c% ")"
  gsubf(rx, text, add_tags_color)
}
#' Prepaire table with export stats to show in shiny app
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
#' df<-dfWoSExport
get_export_records_summary<-function(df){
  Js_aug<-d$journals %>% select(mydbtitle, url, url_tocken) %>% 
    left_join(d$libstat$general %>% 
                select(journal_name, updated), by=c("mydbtitle"="journal_name")) %>% 
    rename(journal=mydbtitle)
    
  df%>%   
    select(-url, -updated) %>% 
    left_join(Js_aug, by="journal") %>% 
    mutate(journal=pmap_chr(list(journal, publisher3, url, url_tocken), generate_journal_url_tag)) %>% 
    select(journal, publisher3, jacro, jscore, ascore, updated) %>% 
    group_by(journal, publisher3, jacro, jscore, updated) %>% 
    summarise(nrecords=n(),
              mnascore=mean(ascore)) %>% 
    select(journal, publisher3, jacro, nrecords, jscore, mnascore, updated) %>% 
    arrange(desc(nrecords))
}