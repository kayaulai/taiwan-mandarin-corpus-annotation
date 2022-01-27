library(spacyr)
library(tidyverse)

spacy_install(lang_models="zh_core_web_sm")
spacy_initialize(model="zh_core_web_sm")
data<-read_csv("~/Desktop/modifieddata.csv")
unlist(data$Utterance)
parsefile <- str_replace_all(data$Utterance,"[[:print:]]","")
parsedfile<-spacy_tokenize(data$Utterance)
parsedfile


