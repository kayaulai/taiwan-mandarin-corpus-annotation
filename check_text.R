library(tidyverse)
library(compare)
library(levenR)
library(stringi)
library(segsimflex)


anno1 = read_csv("C:/Users/kayau/Documents/GitHub/taiwan-mandarin-corpus-annotation/old_5_manual_split/NCCU-TM016_Sunny.csv")
anno2 = read_csv("C:/Users/kayau/Documents/GitHub/taiwan-mandarin-corpus-annotation/old_5_manual_split/NCCU-TM016-CN-Ryan.csv")

anno1 = rezTrans("C:\\Users\\kayau\\Documents\\GitHub\\taiwan-mandarin-corpus-annotation\\8_manual_split\\NCCU-TM025-CN-FM_Ryan.rez")
anno2 = rezTrans("C:\\Users\\kayau\\Documents\\GitHub\\taiwan-mandarin-corpus-annotation\\8_manual_split\\NCCU-TM025-CN-FM_Yujie.rez")

str_strip_last = function(strings, regex){
  contains = str_ends(strings, regex)
  locs = str_locate_all(strings, regex)
  sapply(1:length(strings), function(i){
    curr_locs = locs[[i]]
    if(nrow(curr_locs) > 0 & contains[i]) substring(strings[i], 1, curr_locs[nrow(curr_locs), 1] - 1)
    else strings[i]
  })
}

str_extract_last = function(strings, regex){
  contains = str_ends(strings, regex)
  locs = str_locate_all(strings, regex)
  sapply(1:length(strings), function(i){
    curr_locs = locs[[i]]
    if(nrow(curr_locs) > 0 & contains[i]) substring(strings[i], curr_locs[nrow(curr_locs), 1], curr_locs[nrow(curr_locs), 2])
    else NA
  })
}

str_extract_first = function(strings, regex){
  contains = str_starts(strings, regex)
  locs = str_locate(strings, regex)
  sapply(1:length(strings), function(i){
    curr_locs = locs[i,]
    if(contains[i]) substring(strings[i], curr_locs[1], curr_locs[2]) else NA
  })
}


str_replace_last = function(strings, regex, replacement){
  contains = str_ends(strings, regex)
  locs = str_locate_all(strings, regex)
  sapply(1:length(strings), function(i){
    curr_locs = locs[[i]]
    if(nrow(curr_locs) > 0 & contains[i]) paste0(substring(strings[i], 1, curr_locs[nrow(curr_locs), 1] - 1), replacement)
    else NA
  })
}



stripPunct = function(data, punct = c("\\?", "\\.", "--", ",")){
 punct_regex = paste0(" (", paste0(punct, collapse = "|"), ")")
 data %>% mutate(Utterance = str_strip_last(Utterance, punct_regex))
}

anno1p = stripPunct(anno1)
anno2p = stripPunct(anno2)

write_csv(anno1p %>% mutate(Utterance = unlist(Utterance)), "anno1.csv")
write_csv(anno2p%>% mutate(Utterance = unlist(Utterance)), "anno2.csv")


write_file(anno1p$Utterance %>% paste0(collapse = " "), "anno1col.csv")
write_file(anno2p$Utterance %>% paste0(collapse = " "), "anno2col.csv")


write_csv(anno1p %>% group_by(Speaker) %>% summarise(Utt = paste0(Utterance, collapse = " ")) %>% select(Utt), "anno1col.csv")
write_csv(anno2p %>% group_by(Speaker) %>% summarise(Utt = paste0(Utterance, collapse = " ")) %>% select(Utt), "anno2col.csv")

anno1p %>% group_by(Speaker) %>% summarise(Utterance = str_extract_all(Utterance, " ") %>% sapply(function(x) paste0(x, collapse= "")) %>% paste0(collapse = "")) %>% pull(Utterance) %>% nchar + anno1p %>% group_by(Speaker) %>% count %>% pull(n)
anno2p %>% group_by(Speaker) %>% summarise(Utterance = str_extract_all(Utterance, " ") %>% sapply(function(x) paste0(x, collapse= "")) %>% paste0(collapse = "")) %>% pull(Utterance) %>% nchar + anno1p %>% group_by(Speaker) %>% count %>% pull(n)

#Correct bdlist lengths
anno1p %>% group_by(Speaker) %>% summarise(Utterance %>% strsplit(" ") %>% sapply(length) %>% sum)
anno2p %>% group_by(Speaker) %>% summarise(Utterance %>% strsplit(" ") %>% sapply(length) %>% sum)

anno1p %>% group_by(Speaker) %>% summarise(Utt = paste0(Utterance, collapse = " ")) %>% select(Utt) == anno2p %>% group_by(Speaker) %>% summarise(Utt = paste0(Utterance, collapse = " ")) %>% select(Utt)
#If no segmentation differences
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

#If there are segmentation differences (usually)
allUtts = c(anno1$Utterance, anno2$Utterance) %>% unique

#Use characters to represent unique utterances
#1F00:22FF, 0400:04FF, 0F00:0FFF, 1F00:1FFF
# unicodeChinese1 = paste0("<U+", strtoi("4e00", 16L):strtoi("62f9", 16L) %>% as.hexmode, ">")
# unicodeKorean1 = paste0("<U+", strtoi("AC00", 16L):strtoi("D7AF", 16L) %>% as.hexmode, ">")
# unicodeGreek = paste0("<U+", strtoi("1F00", 16L):strtoi("22FF", 16L) %>% as.hexmode, ">")
# unicodeCyrllic = paste0("<U+", strtoi("0400", 16L):strtoi("04FF", 16L) %>% as.hexmode, ">")
# unicodeTibetan = paste0("<U+", strtoi("0F00", 16L):strtoi("0FFF", 16L) %>% as.hexmode, ">")
# codeToChar = function(string) stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", string))
# charSet = codeToChar(unicodeGreek)

charSet = (read_lines("C:/Users/kayau/Documents/GitHub/taiwan-mandarin-corpus-annotation/chars") %>% strsplit(""))[[1]]

anno1 = anno1 %>% mutate(UtteranceInt = factor(Utterance, levels = allUtts) %>% as.integer)
anno2 = anno2 %>% mutate(UtteranceInt = factor(Utterance, levels = allUtts) %>% as.integer)

charRep1 = charSet[anno1$UtteranceInt] %>% paste0(collapse="")
charRep2 = charSet[anno2$UtteranceInt]%>% paste0(collapse="")
alignment = levenAll(charRep1, charRep2, align = T)

anno1spl = anno1p %>% group_split(Speaker)
anno2spl = anno2p %>% group_split(Speaker)

rezTrans

M_nccu = matrix(
  c(1, .5, .25, .25, 0, 0,
    .5, 1, .5, .5, 0, .25,
    .25, .5, 1, .25, 0, 0,
    .25, .5, .25, 1, 0, .25,
    0,0,0,0,1, .25,
    0, .25, 0, .25, .25, 1),
  nrow = 6)
bounds_nccu = c(",", ".", "?", "+")
transCost = (1-M_nccu[,6])*.5
sim_Score(nccu_t016[[1]], nccu_t016[[2]], transCost = transCost, m = M_nccu, boundaries = bounds_nccu)
sim_Score(nccu_t025[[1]], nccu_t025[[2]], transCost = transCost, m = M_nccu, boundaries = bounds_nccu)
sim_Score(nccu_t049[[1]], nccu_t049[[2]], transCost = transCost, m = M_nccu, boundaries = bounds_nccu)
