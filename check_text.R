library(tidyverse)
library(compare)

anno1 = read_csv("C:/Users/kayau/Documents/GitHub/taiwan-mandarin-corpus-annotation/old_5_manual_split/NCCU-TM049_Shujie Zhang.csv")
anno2 = read_csv("C:/Users/kayau/Documents/GitHub/taiwan-mandarin-corpus-annotation/old_5_manual_split/NCCU-TM049_Danni.csv")

str_strip_last = function(strings, regex){
  locs = str_locate_all(strings, punct_regex)
  lapply(1:length(strings), function(i){
    curr_locs = locs[[i]]
    if(nrow(curr_locs) > 0) substring(strings[i], 1, curr_locs[nrow(curr_locs), 1] - 1)
    else strings[i]
    
  })
}

stripPunct = function(data, punct = c("\\?", "\\.", "--", ",")){
 punct_regex = punct_regex = paste0(" (", paste0(punct, collapse = "|"), ")")
 data %>% mutate(Utterance = str_strip_last(Utterance, punct_regex))
}

anno1p = stripPunct(anno1)
anno2p = stripPunct(anno2)

comparisonResult = compare(anno1p, anno2p)

anno1spl = anno1p %>% group_split(Speaker)
anno2spl = anno2p %>% group_split(Speaker)

wrong_df = data.frame(Turn = integer(0), Speaker = character(0), Utterance = character(0))
wrong_df_2 = data.frame(Turn = integer(0), Speaker = character(0), Utterance = character(0))
for(x in 1:length(anno1spl)){
  comparison = compare(anno1spl[[x]]$Utterance, anno2spl[[x]]$Utterance)$detailedResult
  wrong_df = wrong_df %>% rbind(anno1spl[[x]] %>% filter(!comparison))
  wrong_df_2 = wrong_df_2 %>% rbind(anno2spl[[x]] %>% filter(!comparison))
}

wrong_df = wrong_df %>% left_join(wrong_df_2, by = "Turn", suffix = c("_1", "_2")) %>% arrange(Turn)

write_csv(anno1p, "NCCU-TM049_Shujie_nopunct.csv")
write_csv(anno2p, "NCCU-TM049_Danni_nopunct.csv")
