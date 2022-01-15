library(tidyverse)
library(dplyr)
library(data.table)
library(readr)
library(reshape)

data<-read_csv("NCCU-TM026-CN-MM (Sample data)  - Transcribing.csv")

x = c()
for (i in seq(1,length(data$Utterance))){
  if (is.na(data$Sequence[i])){
    x=c(x,x[i-1])
  }
  else{
    x=c(x,data$Sequence[i])
  }
}

result= data %>%
  mutate(Utterance=ifelse(str_detect(Utterance, "\\.{3}\\([:digit:]\\.[:digit:]\\)"),
                  gsub('^.{3}', '', Utterance),Utterance)) %>%
  mutate(Utterance=ifelse(str_detect(Utterance, "\\.{3}"),
                          gsub('\\.{3}', '(...)', Utterance),Utterance)) %>%
  mutate(Utterance=ifelse(str_detect(Utterance, "^\\.{2}"),
                          gsub('^\\.{2}', '(.)', Utterance),Utterance)) %>%
  mutate(Utterance=ifelse(str_detect(Utterance, "\\[{3}"),
                          gsub('\\[{3}', '[3', Utterance),Utterance))%>%
  mutate(Utterance=ifelse(str_detect(Utterance, "\\[{2}"),
                          gsub('\\[{2}', '[2', Utterance),Utterance))%>%
  mutate(Utterance=ifelse(str_detect(Utterance, "\\]{3}"),
                          gsub('\\]{3}', '3]', Utterance),Utterance))%>%
  mutate(Utterance=ifelse(str_detect(Utterance, "\\]{2}"),
                          gsub('\\]{2}', '2]', Utterance),Utterance)) %>%
  mutate(Utterance=ifelse(str_detect(Utterance, "X"),
                          gsub('X', '#', Utterance),Utterance)) %>%
  rename(c(Sequence = "TurnSeq")) %>%
  mutate(TurnSeq=x) %>%
  mutate(IUSeq=seq(1,length(data$Utterance)))

write.csv(result,"modifieddata.csv")
