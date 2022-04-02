filename = "NCCU-TM026-CN-MM_dft.csv"
data = read_csv(paste0("6_dft_converted/", filename))[,-c(1,2,6)]

data_owpl = data.frame(TurnSeq = integer(0), IUSeq = integer(0), Speaker = character(0), Utterance = character(0))
splits = strsplit(as.character(data$Utterance)," ")
for(i in 1:nrow(data)){
  data_owpl = add_row(data_owpl,
                      TurnSeq = pull(data[i,],"TurnSeq"),
                      IUSeq = pull(data[i,4],"IUSeq"),
                      Speaker = pull(data[i,],"Speaker"),
                      Utterance = splits[[i]])
}
filename = substr(filename, 1, (nchar(filename)-3))
data_owpl = cbind(filename,data_owpl)
write_csv(data_owpl, paste0("7_rez_input/", substr(filename, 1, (nchar(filename)-5)), ".csv"))
