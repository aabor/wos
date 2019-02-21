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
#' get_text_chunks_from_selected_records(dfWoSExport, suggested_terms)
get_text_chunks_from_selected_records<-function(dfWoSExport, suggested_terms, progress=NULL){
  suggested_terms_vector<-words(suggested_terms)
  df<-dfWoSExport %>% 
    mutate(pdf_file=file.path(g$paths$pdf, basename(.$file))) %>% 
    subset(file.exists(pdf_file))
  if(nrow(df)==0)print("Warning no .pdfs for selected bibliography records, imposible to get text chunks")
  df %>% 
    subset(!is.na(file)) %>% 
    rowwise() %>% 
    do({
      pdf_file<-file.path(g$paths$pdf, basename(.$file))
      paste("searching terms in full text", basename(.$file), "\r\n") %>% 
        give_echo(NULL, T, progress)
      text_chunks<-NULL
      main_text<-pdf_text(pdf_file)
      pages<-ifelse(length(main_text)>2,
                    main_text[2:(length(main_text)-1)],
                    main_text)
      pages_short<-cleanText(pages) %>% glue_collapse(sep=" ") %>% as.character() %>% str_split(pattern = " ") %>% unlist
      pages_short_nopunct<-pages_short %>% 
        str_replace_all(pattern="[[:punct:]]", "")
      res<-lapply(suggested_terms_vector, function(x){
        df<-data.frame(detected=str_detect(pages_short_nopunct, pattern=x)) %>% 
          mutate(wp=1:n()) %>% 
          filter(detected) %>% 
          select(wp)
        df
      })
      names(res)<-suggested_terms_vector
      wcounts<-res %>% lapply(nrow) %>% 
        unlist %>% 
        as.data.frame %>% 
        rownames_to_column()
      names(wcounts)<-c("term", "wcount")
      terms<-wcounts %>% 
        arrange(desc(wcount)) %>% 
        filter(wcount>5) %>% 
        pull(term)
      res_short<-terms %>% lapply(function(x)res[[x]]) %>% unlist %>% sort
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
      key<-.$key
      text_chunks<-chunks %>% 
        rowwise() %>% 
        do({
          start<-ifelse(.$start>10, .$start-9, .$start)
          end<-ifelse(.$end<(length(pages_short)-10), .$end+8, .$end)
          dfRet<-data.frame(key=key, stringsAsFactors = F)
          dfRet$text<-pages_short[start:end] %>% 
            glue_collapse(sep=" ") %>% 
            as.character() %>% 
            str_replace_all("_", " ")
          dfRet
        })
      reference<-get_rmarkdown_reference(.)
      ret<-tibble(key=.$key, 
                 reference=get_rmarkdown_reference(.), 
                 text_chunks=text_chunks$text %>% 
                   glue_collapse(sep ="\r\n") %>% as.character()) %>% 
        mutate(reference=highlight_terms(reference, suggested_terms_vector),
          text_chunks=highlight_terms(text_chunks, suggested_terms_vector))
    })
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
