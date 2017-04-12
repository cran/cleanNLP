## ----setup, include=FALSE------------------------------------------------
# CRAN will not have spaCy installed, so create static vignette
knitr::opts_chunk$set(eval = FALSE)

## ------------------------------------------------------------------------
#  text <- c("The regular early morning yell of horror was the sound of",
#            "Arthur Dent waking up and suddenly remembering where he",
#            "was. It wasn't just that the cave was cold, it wasn't just",
#            "that it was damp and smelly. It was the fact that the cave",
#            "was in the middle of Islington and there wasn't a bus due",
#            "for two million years.")
#  text <- paste(text, collapse = " ")

## ------------------------------------------------------------------------
#  library(cleanNLP)
#  init_spaCy()
#  obj <- annotate(text, as_strings = TRUE)

## ------------------------------------------------------------------------
#  names(obj)

## ------------------------------------------------------------------------
#  get_document(obj)

## ------------------------------------------------------------------------
#  get_token(obj)

## ---- eval = FALSE-------------------------------------------------------
#  download_core_nlp()

## ------------------------------------------------------------------------
#  init_coreNLP(speed = 2L, lib_location = lib_loc)

## ---- echo = FALSE-------------------------------------------------------
#  lib_loc <- "~/local/core_nlp_files/stanford-corenlp-full-2016-10-31"
#  init_coreNLP(speed = 2L, lib_location = lib_loc)

## ------------------------------------------------------------------------
#  obj <- annotate(text, as_strings = TRUE, backend = "coreNLP")
#  obj

## ------------------------------------------------------------------------
#  od <- tempfile()
#  write_annotation(obj, od)
#  dir(od)

## ------------------------------------------------------------------------
#  anno <- read_annotation(od)

