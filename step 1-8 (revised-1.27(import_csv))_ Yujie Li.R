library(tidyverse)
library(dplyr)
library(data.table)
library(readr)
library(reshape)

data<-read_csv("NCCU-TM026-CN-MM (Sample data)  - Transcribing1.csv",locale = locale(encoding = "GB18030"))

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
  mutate(Speaker=ifelse(str_detect(Speaker, "M1:"),
                          gsub('M1:', 'M1', Speaker),Speaker)) %>%
  mutate(Speaker=ifelse(str_detect(Speaker, "M2:"),
                          gsub('M2:', 'M2', Speaker),Speaker)) %>%
  rename(c(TurnSeq = "Sequence")) %>%
  mutate(TurnSeq=x) %>%
  mutate(IUSeq=seq(1,length(data$Utterance)))

write.csv(result,"modifieddata1.csv",fileEncoding = "UTF-8")
