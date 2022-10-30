library(rezonateR)

currPath = "C:/Users/kayau/Documents/GitHub/taiwan-mandarin-corpus-annotation/"
fileName = "NCCU-TM025-CN-FM"
annotator1 = "Ryan"
annotator2 = "Yujie"

rez1 = importRez(currPath %+% "8_manual_split/" %+% fileName %+% "_" %+% annotator1 %+% ".rez", concatFields = "Utterance")
rez2 = importRez(currPath %+% "8_manual_split/" %+% fileName %+% "_" %+% annotator2 %+% ".rez", concatFields = "Utterance")
