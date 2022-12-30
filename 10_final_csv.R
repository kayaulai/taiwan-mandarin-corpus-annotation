library(rezonateR)

setwd("C:/Users/kayau/Documents/GitHub/taiwan-mandarin-corpus-annotation")
docname = "NCCU-TM049.rez"
finalRez = importRez("old_9_rez_file/" %+% docname, concatFields = "Utterance")
finalRez = finalRez %>% addUnitSeq("token")
finalRez$tokenDF = finalRez$tokenDF %>% rez_mutate(Utterance = case_when(str_detect(Utterance, "\\[{0,1}\\.\\.\\]{0,1}") & !str_detect(Utterance, "\\.\\.\\.") ~ str_replace(Utterance, "\\.\\.", "(.)"), T ~ Utterance))

removeExtraNumbers = function(transcript){
  numberedCloses = which(str_detect(transcript, "[0-9]\\]"))
  for(close in numberedCloses){
    numberLoc = str_locate(transcript[close], "[0-9]\\]")[1,1]
    currNum = substring(transcript[close], numberLoc, numberLoc)
    if(any(str_detect(transcript[1:close], paste0("\\[", currNum)))){
      transcript[close] = str_replace(transcript[close], "[0-9]\\]", "\\]")
    }
  }
  transcript
}

finalRez$tokenDF = finalRez$tokenDF %>% rez_group_by(unit) %>% rez_mutate(Utterance = removeExtraNumbers(Utterance)) %>% rez_ungroup()

finalRez$tokenDF = finalRez$tokenDF %>% rez_mutate(Utterance = case_when(str_detect(Utterance, "\\[{0,1}\\.\\.\\]{0,1}") & !str_detect(Utterance, "\\.\\.\\.") ~ "(.)", T ~ Utterance))

finalRez$tokenDF = finalRez$tokenDF %>% rez_mutate(Utterance = str_replace(Utterance, "--", "—"))


finalRez$entryDF = reload(finalRez$entryDF, finalRez)
finalRez$unitDF = reload(finalRez$unitDF, finalRez, fields = "Utterance")

finalRez = finalRez %>% addFieldForeign("token", "", "unit", "",
                                        targetForeignKeyName = "unit",
                                        targetFieldName = "turnSeq",
                                        sourceFieldName = "TurnSeq")
finalRez = finalRez %>% addFieldForeign("token", "", "unit", "",
                                        targetForeignKeyName = "unit",
                                        targetFieldName = "participant",
                                        sourceFieldName = "Speaker")


finalCsv_token = finalRez$tokenDF %>% rez_rename(text = Utterance) %>% rez_select(turnSeq, unitSeq, participant, text)
finalCsv_unit = finalRez$unitDF %>% select(-Text) %>% rez_rename(Text = Utterance) %>% rez_select(unitSeq, TurnSeq, Speaker, Text)


rez_write_csv(finalCsv_token, "10_final_csv_owpl/" %+% substring(docname, 1, nchar(docname) - 4) %+% ".csv", quote = "needed")
#write_tsv(finalCsv_unit, "10_final_csv_owpl/" %+% substring(docname, 1, nchar(docname) - 4) %+% ".txt")

write_tsv(finalCsv_unit, "10_final_csv_unit/" %+% substring(docname, 1, nchar(docname) - 4) %+% ".txt")
