filename = "NCCU-TM049.csv"
setwd("C:/Users/User/Documents/GitHub/taiwan-mandarin-corpus-annotation")
data = read_csv(paste0("7_dft_converted/", filename))[,]

data_owpl = data.frame(TurnSeq = integer(0), IUSeq = integer(0), Speaker = character(0), Utterance = character(0))
splits = strsplit(as.character(data$Utterance)," ")
for(i in 1:nrow(data)){
  data_owpl = add_row(data_owpl,
                      TurnSeq = pull(data[i,],"TurnSeq"),
                      IUSeq = pull(data[i,],"IUSeq"),
                      Speaker = pull(data[i,],"Speaker"),
                      Utterance = splits[[i]])
}
filename = substr(filename, 1, (nchar(filename)-3))
data_owpl = cbind(filename,data_owpl)
write_csv(data_owpl, paste0("8_rez_input/", "NCCU-TM049.csv"))
