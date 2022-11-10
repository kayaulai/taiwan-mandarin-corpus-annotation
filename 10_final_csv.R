library(rezonateR)

docname = "NCCU-TM025-CN-FM.rez"
finalRez = importRez("9_final_split/" %+% docname, concatFields = "Utterance")
finalRez$tokenDF = finalRez$tokenDF %>% rez_mutate(Utterance = case_when(str_detect(Utterance, "\\[{0,1}\\.\\.\\]{0,1}") ~ "(.)", T ~ Utterance))
finalRez$entryDF = reload(finalRez$entryDF, finalRez)
finalRez$unitDF = reload(finalRez$unitDF, finalRez)

finalCsv = finalRez$unitDF %>% select(-unitStart, -unitEnd, -pID)

rez_write_csv(finalCsv, "10_final_csv/" %+% substring(docname, 1, nchar(docname) - 4) %+% ".csv")
