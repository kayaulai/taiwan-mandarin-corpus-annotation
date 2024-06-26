library(tidyverse)
library(dplyr)
library(readr)

setwd("C:/Users/User/Documents/GitHub/taiwan-mandarin-corpus-annotation/4_final_tokenised")
data <- read.csv("NCCU-TM039-CN-FM_Lu&Yuting.csv")


x = data$Turn
speaker = data$Speaker
for (i in 1:length(data$Utterance)){
  print(i)
  if (is.na(data$Turn[i])){
    x[i]=x[i-1]
    speaker[i] = speaker[i-1]
  }
}

data = data %>% mutate(Utterance = as.character(Utterance))

result= data %>%
  mutate(Utterance=case_when(str_detect(Utterance, "\\.{3}\\([:digit:]\\.[:digit:]\\)") ~
                               gsub('^.{3}', '', Utterance), T ~ Utterance)) %>%
  mutate(Utterance=ifelse(str_detect(Utterance, "\\.{3}"),
                          gsub('\\.{3}', '(...)', Utterance),Utterance)) %>%
  mutate(Utterance=ifelse(str_detect(Utterance, "^\\.{2}"),
                          gsub('^\\.{2}', '(.)', Utterance),Utterance)) %>%
  mutate(Utterance=ifelse(str_detect(Utterance, " \\.{2} "),
                        gsub(' \\.{2} ', ' (.) ', Utterance),Utterance)) %>%
  mutate(Utterance=ifelse(str_detect(Utterance, "--"),
                          gsub('--', '—', Utterance),Utterance)) %>%
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
  rename(c(TurnSeq = "Turn")) %>%
  mutate(TurnSeq=x) %>%
  mutate(Speaker = speaker) %>%
  mutate(IUSeq=seq(1,length(data$Utterance)))

write.csv(result,"../5_dft_converted/NCCU-TM039-CN-FM_Lu&Yuting.csv",fileEncoding = "UTF-8")
