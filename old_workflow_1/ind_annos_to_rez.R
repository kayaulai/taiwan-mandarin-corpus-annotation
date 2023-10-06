library(tidyverse)
filename = "NCCU-TM016-CN-Ryan.csv"
data = read_csv(paste0("C:/Users/kayau/Documents/GitHub/taiwan-mandarin-corpus-annotation/old_5_manual_split/", filename))
data = data  %>% mutate(unitSeq = 1:nrow(data))

data_owpl = data.frame(TurnSeq = integer(0), unitSeq = integer(0), Speaker = character(0), Utterance = character(0))
splits = strsplit(as.character(data$Utterance)," ")
for(i in 1:nrow(data)){
  data_owpl = add_row(data_owpl,
                      TurnSeq = pull(data[i,],"Turn"),
                      unitSeq = pull(data[i,],"unitSeq"),
                      Speaker = pull(data[i,],"Speaker"),
                      Utterance = splits[[i]])
}
filename = substr(filename, 1, (nchar(filename)-3))
data_owpl = cbind(filename,data_owpl)
write_csv(data_owpl, paste0("C:/Users/kayau/Documents/GitHub/taiwan-mandarin-corpus-annotation/8_manual_split/", filename, ".csv"))
