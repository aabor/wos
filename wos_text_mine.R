#' Topic analysis
#' 
#' Perfoms LDA topic modeling on the R package "textmineR" basics.
#' 
#' Methodology description: https://github.com/TommyJones/textmineR
#' 
#' Package on CRAN repository: https://cran.r-project.org/web/packages/textmineR/index.html
#'
#' @param df data.frame with bibliography records
#' @param progress progress bar object
#'
#' @return
#' @export
#'
#' @examples
#' frontier_year=2017
#' df<-dfWoS
#' progress=NULL
#' topicAnalysis(df)
topicAnalysis<-function(df, frontier_year=2016, progress = NULL){
  # Create a document term matrix
  "creating document term matrix" %>% 
    echo("topicAnalysis", T, progress)
  df<-df %>% 
    filter(!is.na(year)) %>% 
    filter(year>frontier_year) %>% 
    select(title, key)
  dtm <- CreateDtm(df$title, 
                   doc_names = 1:nrow(df), 
                   ngram_window = c(1, 2))
  
  dim(dtm)
  paste("document terms matrix of dimensions", glue_collapse(dim(dtm), sep=":"), "created...") %>% 
    echo("topicAnalysis", T, progress)

  # explore basic frequencies & curate vocabulary
  tf <- TermDocFreq(dtm = dtm)
  
  paste("basic frequencies & curate vocabulary matrix with", nrow(tf), "terms (in rows) created...") %>% 
    echo("topicAnalysis", T, progress)
  
  # Eliminate words appearing less than 2 times or in more than half of the
  # documents
  vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]
  
  
  dtm <- dtm[ , vocabulary]
  
  dim(dtm)
  
  # fit some LDA models and select the best number of topics
  "fitting LDA models" %>% 
    echo("topicAnalysis", T, progress)
  k_list <- seq(5, 50, by = 5)
  
  model_dir <-file.path(g$paths$topic_models, 
                        glue("models_{digest::digest(vocabulary, algo = 'sha1')}"))
  
  if (!dir.exists(model_dir)) dir.create(model_dir)
  
  # model_list<-map(k_list, .f=function(k){
  #   gc()
  #   model_name<-paste0(k, "_topics.rda")
  #   cat(glue("{model_name}\r\n"))
  #   filename = file.path(model_dir, model_name)
  #   if (!file.exists(filename)) {
  #     m <- FitLdaModel(dtm = dtm, k = k, iterations = 500)
  #     m$k <- k
  #     m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
  #     save(m, file = filename)
  #   } else {
  #     load(filename)
  #   }
  #   m
  # })
  model_list <- TmParallelApply(
    X = k_list,
    cpus=parallel::detectCores()-2,
    FUN = function(k){
      gc()
      model_name<-paste0(k, "_topics.rda")
      filename = file.path(model_dir, model_name)
      
      if (!file.exists(filename)) {
        m <- FitLdaModel(dtm = dtm, k = k, iterations = 500)
        m$k <- k
        m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
        save(m, file = filename)
      } else {
        load(filename)
      }
      m
    }, 
    export=c("dtm", "model_dir")) # export only needed for Windows machines
  
  d$top_topic_model$model_list<-model_list
  write_rds(d, document_path)
  
  progress_nstep<<-progress_nstep + 1
  msg<-paste(length(model_list), "models fitted")
  if (!is.null(progress)) {
    updateProgress(progress, detail = msg)
  }
  
  
  coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                              coherence = sapply(model_list, function(x) mean(x$coherence)), 
                              stringsAsFactors = FALSE)
  
  d$top_topic_model$coherence_mat<-coherence_mat

  plot(coherence_mat, type = "o")
  
  # select k based on maximum average coherence
  model <- model_list[ which.max(coherence_mat$coherence) ][[ 1 ]]
  
  
  names(model) # phi is P(words | topics), theta is P(topics | documents)

  paste("coherence matrix calculated... computing summary statistics") %>% 
    echo("topicAnalysis", T, progress)

  # Calculate some summary statistics etc. Which is the real value-add of textmineR
  
  # Get the R-squared of this model
  model$r2 <- CalcTopicModelR2(dtm = dtm, phi = model$phi, theta = model$theta)
  
  model$r2
  
  # top 5 terms of the model according to phi & phi-prime
  model$top_terms <- GetTopTerms(phi = model$phi, M = 5)
  
  # phi-prime, P(topic | words) for classifying new documents
  # model$phi_prime <- textmineR::CalcPhiPrime(phi = model$phi, theta = model$theta, p_docs = rowSums(dtm))
  # model$top_terms_prime <- textmineR::GetTopTerms(phi = model$phi_prime, M = 5)
  
  # give a hard in/out assignment of topics in documents
  model$assignments <- model$theta
  
  model$assignments[ model$assignments < 0.05 ] <- 0
  
  model$assignments <- model$assignments / rowSums(model$assignments)
  
  model$assignments[ is.na(model$assignments) ] <- 0

  "models summary statistics calculated..." %>% 
    echo("topicAnalysis", T, progress)

  # Get some topic labels using n-grams from the DTM
  model$labels <- LabelTopics(assignments = model$assignments, 
                              dtm = dtm,
                              M = 2)
  "topic labels using n-grams from the DTM calculated..." %>% 
    echo("topicAnalysis", T, progress)

  # Probabilistic coherence: measures statistical support for a topic
  model$coherence <- CalcProbCoherence(phi = model$phi, dtm = dtm, M = 5)
  "probabilistic coherence: measures statistical support for a topic calculated..." %>% 
    echo("topicAnalysis", T, progress)

  # Number of documents in which each topic appears
  model$num_docs <- colSums(model$assignments > 0)
  
  "summary statistics calculated... clastering topics" %>% 
    echo("topicAnalysis", T, progress)

  # cluster topics together in a dendrogram
  model$topic_linguistic_dist <- CalcHellingerDist(model$phi)
  
  model$hclust <- hclust(as.dist(model$topic_linguistic_dist), "ward.D")
  
  model$hclust$clustering <- cutree(model$hclust, k = 10)
  
  model$hclust$labels <- paste(model$hclust$labels, model$labels[ , 1])

  model$hclust$labels %<>% 
    str_replace("t_", "t") %>% 
    str_replace_all("_", " ")
  
  
  plot(model$hclust)
  rect.hclust(model$hclust, k = length(unique(model$hclust$clustering)))

  "topics clustered saving results..." %>% 
    echo("topicAnalysis", T, progress)

  # make a summary table
  model$summary <- data.frame(topic     = rownames(model$phi),
                              cluster   = model$hclust$clustering,
                              model$labels,
                              coherence = model$coherence,
                              num_docs  = model$num_docs,
                              top_terms = apply(model$top_terms, 2, function(x){
                                paste(x, collapse = ", ")
                              }),
                              # top_terms_prime = apply(model$top_terms_prime, 2, function(x){
                              #   paste(x, collapse = ", ")
                              # }),
                              stringsAsFactors = FALSE)
  model$summary %<>% 
    select(topic, cluster, coherence, num_docs, top_terms) %>% 
#    select(topic, cluster, coherence, num_docs, top_terms, top_terms_prime) %>% 
    arrange(model$hclust$clustering) %>% 
    mutate(coherence = round(coherence*100, digits = 2),
           top_terms=str_replace_all(top_terms, ", ", "; "))#,
           #top_terms_prime=str_replace_all(top_terms_prime, ", ", "; "),
           #top_terms_prime=str_replace_all(top_terms_prime, "_", " "))
  #model$summary %>% head
  
  #View(model$summary)

  d$top_topic_model$topModel<-model
  write_rds(d, document_path)
  
  file_name_rds<-file.path(g$paths$db,"top_research_topics.rds")
  write_rds(model$summary, file_name_rds)
  write_csv(model$summary, file.path(g$paths$db,"top_research_topics.csv"))
  paste("summary table created and saved to file", file_name_rds) %>% 
    echo("topicAnalysis", T, progress)
}
