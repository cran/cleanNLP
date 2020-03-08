## ----setup, include=FALSE-----------------------------------------------------
# CRAN will not have spaCy installed, so create static vignette
knitr::opts_chunk$set(eval = FALSE)

## ---- echo = FALSE, message=FALSE---------------------------------------------
#  library(magrittr)
#  library(dplyr)
#  library(ggplot2)
#  library(cleanNLP)
#  library(jsonlite)
#  library(stringi)
#  library(xml2)

## -----------------------------------------------------------------------------
#  grab_wiki <- function(lang, page) {
#    url <- sprintf(
#      "https://%s.wikipedia.org/w/api.php?action=parse&format=json&page=%s",
#      lang,
#      page)
#    page_json <- jsonlite::fromJSON(url)$parse$text$"*"
#    page_xml <- xml2::read_xml(page_json, asText=TRUE)
#    page_text <- xml_text(xml_find_all(page_xml, "//div/p"))
#  
#    page_text <- stri_replace_all(page_text, "", regex="\\[[0-9]+\\]")
#    page_text <- stri_replace_all(page_text, " ", regex="\n")
#    page_text <- stri_replace_all(page_text, " ", regex="[ ]+")
#    page_text <- page_text[stri_length(page_text) > 10]
#  
#    return(page_text)
#  }
#  
#  penguin <- grab_wiki("en", "penguin")
#  penguin[1:10] # just show the first 10 paragraphs

## -----------------------------------------------------------------------------
#  cnlp_init_udpipe()
#  anno <- cnlp_annotate(penguin, verbose=FALSE)
#  anno$token

## -----------------------------------------------------------------------------
#  token <- anno$token
#  token$new_token <- token$token_with_ws
#  change_these <- which(token$xpos %in% c("NNP", "NNPS"))
#  token$new_token[change_these] <- stri_trans_toupper(token$new_token[change_these])

## -----------------------------------------------------------------------------
#  paragraphs <- tapply(token$new_token, token$doc_id, paste, collapse="")[1:10]
#  paragraphs <- stri_wrap(paragraphs, simplify=FALSE, exdent = 1)
#  cat(unlist(lapply(paragraphs, function(v) c(v, ""))), sep="\n")

