library(tidyverse)
library(dplyr)
library(readr)

setwd("C:/Users/kayau/Documents/GitHub/taiwan-mandarin-corpus-annotation")
data1<-read_csv("C:/Users/kayau/Documents/GitHub/taiwan-mandarin-corpus-annotation/5_manual_tokenised/NCCU-TM026-CN-MM_Jack Sun Manual Split File.csv")[1:100,]
data4<-read_csv("C:/Users/kayau/Documents/GitHub/taiwan-mandarin-corpus-annotation/5_manual_tokenised/manual - NCCU-TM026-CN-MM_Sunny Zhong.csv")[101:202,]
data4 = data4[,-1]
data3<-read_csv("C:/Users/kayau/Documents/GitHub/taiwan-mandarin-corpus-annotation/5_manual_tokenised/manual - NCCU-TM026-CN-MM_Yujie Li.csv",locale = locale(encoding = "GB18030"))[201:300,]
data2<-read_csv("C:/Users/kayau/Documents/GitHub/taiwan-mandarin-corpus-annotation/5_manual_tokenised/NCCU_TM026-CN-MM_Shujie Zhang.csv")[301:401,]
data5<-read_csv("C:/Users/kayau/Documents/GitHub/taiwan-mandarin-corpus-annotation/5_manual_tokenised/manual - NCCU-TM026-CN-MM_Sabrina.csv")
data5<-data5[401:nrow(data5),]
colnames(data2) = colnames(data1)
colnames(data3) = colnames(data1)
colnames(data4) = colnames(data1)
colnames(data5) = colnames(data1)
data = rbind(data1,data4,data3,data2,data5)

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

write.csv(result,"6_dft_converted/NCCU-TM026-CN-MM_dft.csv",fileEncoding = "UTF-8")
