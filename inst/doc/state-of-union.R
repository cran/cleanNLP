## ----setup, include=FALSE-----------------------------------------------------
# CRAN will not have spaCy installed, so create static vignette
knitr::opts_chunk$set(eval = FALSE)

## ----echo = FALSE, message=FALSE----------------------------------------------
#  library(magrittr)
#  library(dplyr)
#  library(ggplot2)
#  library(cleanNLP)
#  library(sotu)

## -----------------------------------------------------------------------------
#  cnlp_init_spacy()

## -----------------------------------------------------------------------------
#  input <- sotu_meta
#  input$text <- sotu_text

## -----------------------------------------------------------------------------
#  anno <- cnlp_annotate(input)

## -----------------------------------------------------------------------------
#  anno$token %>%
#    group_by(doc_id, sid) %>%
#    summarize(sent_len = n()) %$%
#    quantile(sent_len, seq(0,1,0.1))

## -----------------------------------------------------------------------------
#  anno$token %>%
#    filter(upos == "NOUN") %>%
#    group_by(lemma) %>%
#    summarize(count = n()) %>%
#    top_n(n = 42, count) %>%
#    arrange(desc(count)) %>%
#    use_series(lemma)

## ----fig.height=6, fig.width=7------------------------------------------------
#  anno$token %>%
#    group_by(doc_id) %>%
#    summarize(n = n()) %>%
#    left_join(anno$document, by="doc_id") %>%
#    ggplot(aes(year, n)) +
#      geom_line(color = grey(0.8)) +
#      geom_point(aes(color = sotu_type)) +
#      geom_smooth(method="loess", formula = y ~ x) +
#      theme_minimal()

## -----------------------------------------------------------------------------
#  anno$entity %>%
#    filter(entity_type == "LOC") %>%
#    group_by(entity) %>%
#    summarize(count = n()) %>%
#    top_n(n = 44, count) %>%
#    arrange(desc(count)) %>%
#    use_series(entity)

## -----------------------------------------------------------------------------
#  anno$token %>%
#    left_join(
#      anno$token,
#      c("doc_id"="doc_id", "sid"="sid", "tid"="tid_source"),
#      suffix=c("", "_source")
#    ) %>%
#    left_join(anno$document, by="doc_id") %>%
#    filter(year == 2001) %>%
#    filter(relation == "dobj") %>%
#    select(doc_id = doc_id, start = token, word = token_source) %>%
#    left_join(word_frequency, by="word") %>%
#    filter(frequency < 0.001) %>%
#    select(doc_id, start, word) %$%
#    sprintf("%s => %s", start, word)

## -----------------------------------------------------------------------------
#  anno$token %>%
#    left_join(
#      anno$token, c("doc_id"="doc_id", "sid"="sid", "tid"="tid_source"),
#      suffix=c("", "_source")
#    ) %>%
#    left_join(anno$document, by="doc_id") %>%
#    filter(year == 2002) %>%
#    filter(relation == "dobj") %>%
#    select(doc_id = doc_id, start = token, word = token_source) %>%
#    left_join(word_frequency, by="word") %>%
#    filter(frequency < 0.001) %>%
#    select(doc_id, start, word) %$%
#    sprintf("%s => %s", start, word)

## -----------------------------------------------------------------------------
#  pca <- anno$token %>%
#    filter(xpos %in% c("NN", "NNS")) %>%
#    cnlp_utils_tfidf(min_df = 0.05, max_df = 0.95, tf_weight = "dnorm") %>%
#    cnlp_utils_pca()
#  pca <- bind_cols(anno$document, pca)
#  pca

## ----fig.height=6, fig.width=7------------------------------------------------
#  ggplot(pca, aes(PC1, PC2)) +
#    geom_point(aes(color = cut(year, 10, dig.lab = 4)), alpha = 0.35, size = 4) +
#    ggrepel::geom_text_repel(data = filter(pca, !duplicated(president)),
#                    aes(label = president), color = grey(0.4), cex = 3) +
#    labs(color = "Years") +
#    scale_color_viridis_d(end = 0.9, option = "C") +
#    theme(axis.title.x = element_text(size = 14),
#          axis.title.y = element_text(size = 14),
#          axis.text.x = element_blank(),
#          axis.text.y = element_blank()) +
#    theme_void()

## ----message=FALSE, warning=FALSE---------------------------------------------
#  library(topicmodels)
#  mat <- anno$token %>%
#    filter(xpos %in% c("NN", "NNS")) %>%
#    cnlp_utils_tf(min_df = 0.05, max_df = 0.95)
#  
#  tm <- LDA(mat, k = 16)

## ----fig.height=9, fig.width=7------------------------------------------------
#  terms <- posterior(tm)$terms
#  topics <- posterior(tm)$topics
#  topic_df <- tibble(topic = as.integer(col(topics)),
#                     doc_id = anno$document$doc_id[as.integer(row(topics))],
#                     val = as.numeric(topics)) %>%
#                left_join(anno$document, by="doc_id")
#  top_terms <- apply(terms, 1,
#                 function(v) {
#                   paste(colnames(mat)[order(v, decreasing = TRUE)[1:5]], collapse = ", ")
#                   })
#  top_terms <- as.character(top_terms)
#  
#  index <- rank(-1 * tapply(topic_df$year * topic_df$val, topic_df$topic, which.max))
#  topic_df$topic_new <- index[topic_df$topic]
#  top_terms_df <- tibble(top_terms, topic = 1:length(top_terms))
#  top_terms_df$topic_new <- index[top_terms_df$topic]
#  
#  ggplot(topic_df, aes(year, topic_new)) +
#    geom_point(aes(size = val, color = factor(topic_new))) +
#    geom_text(data = top_terms_df, x = mean(topic_df$year),
#              size = 5, aes(y = topic_new + 0.4, label = top_terms, color = factor(topic_new)),
#              show.legend = FALSE) +
#      scale_color_viridis_d(end = 0.7, option = "C") +
#    theme(axis.text.y=element_blank(),
#          axis.title.y=element_blank(),
#          legend.position="bottom",
#          axis.title.x = element_text(size = 16),
#          axis.text.x = element_text(size = 14)) +
#    labs(size = "Posterior probability") +
#    theme_minimal() +
#    scale_y_continuous(breaks=FALSE) +
#    ylab("") +
#    xlab("Year") +
#    guides(colour = FALSE)

## -----------------------------------------------------------------------------
#  df <- anno$token %>%
#    left_join(anno$document, by="doc_id") %>%
#    filter(year > 2000) %>%
#    mutate(new_id = paste(doc_id, sid, sep = "-")) %>%
#    filter(xpos %in% c("NN", "NNS"))
#  mat <- cnlp_utils_tf(df, doc_var = "new_id")
#  dim(mat)

## -----------------------------------------------------------------------------
#  meta <- tibble(new_id = rownames(mat)) %>%
#    left_join(df[!duplicated(df$new_id),], by="new_id") %>%
#    mutate(y = as.numeric(president == "Barack Obama")) %>%
#    mutate(train = year %in% seq(2001, 2016, by = 2))

## ----message=FALSE------------------------------------------------------------
#  library(glmnet)
#  model <- cv.glmnet(mat[meta$train,], meta$y[meta$train], family = "binomial")

## ----fig.height=6, fig.width=7------------------------------------------------
#  meta$pred <- predict(model, newx = mat, type = "response", s = model$lambda.1se)
#  ggplot(meta, aes(factor(year),pred)) +
#    geom_boxplot(aes(fill = relevel(factor(president), "George W. Bush"))) +
#    labs(fill = "President") + xlab("year") + ylab("predicted probability") +
#    scale_fill_viridis_d(alpha = 0.6, end = 0.75, option = "C") +
#    coord_flip() +
#    theme(axis.title.x = element_text(size = 12),
#          axis.text.x = element_text(size = 10),
#          axis.title.y = element_text(size = 12),
#          axis.text.y = element_text(size = 10)) +
#    theme_minimal() +
#    ylab("Predicted probability") +
#    xlab("Year")

## -----------------------------------------------------------------------------
#  beta <- coef(model, s = model[["lambda"]][11])[-1]
#  sprintf("%s (%d)", colnames(mat), sign(beta))[beta != 0]

