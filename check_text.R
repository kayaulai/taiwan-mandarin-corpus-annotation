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
  c(1, .5, .25, .25, 1, 0,
    .5, 1, .5, .5, 1, .25,
    .25, .5, 1, .25, 1, 0,
    .25, .5, .25, 1, 1, .25,
    1,1,1,1,1, 1,
    0, .25, 0, .25, 1, 1),
  nrow = 6)
bounds_nccu = c(",", ".", "?", "+")
transCost = (1-M_nccu[,6])*.5
t16_m = sim_Score(z[[1]], nccu_t016[[2]], transCost = transCost, m = M_nccu, boundaries = bounds_nccu)
t25_m = sim_Score(nccu_t025[[1]], nccu_t025[[2]], transCost = transCost, m = M_nccu, boundaries = bounds_nccu)
t49_m = sim_Score(nccu_t049[[1]], nccu_t049[[2]], transCost = transCost, m = M_nccu, boundaries = bounds_nccu)
t16_i = sim_Score(nccu_t016[[1]], nccu_t016[[2]])
t25_i = sim_Score(nccu_t025[[1]], nccu_t025[[2]])
t49_i = sim_Score(nccu_t049[[1]], nccu_t049[[2]])

t16 = c(t16_i, t16_m)
t25 = c(t25_i, t25_m)
t49 = c(t49_i, t49_m)

scores = rbind(t16, t25,  t49)
colnames(scores) = c("SI", "SBI", "SM", "SBM")
scores = scores %>% data.frame %>% rownames_to_column(var = "text") %>%
  pivot_longer(cols = c("SI", "SBI", "SM", "SBM"))
ggplot(scores, aes(x = text, y = value, col = name, group = name, shape = name)) + geom_point() + geom_line()


t16_m_detailed = sim_Score(nccu_t016[[1]], nccu_t016[[2]], transCost = transCost, m = M_nccu, boundaries = bounds_nccu, record = T)
t25_m_detailed = sim_Score(nccu_t025[[1]], nccu_t025[[2]], transCost = transCost, m = M_nccu, boundaries = bounds_nccu, record = T)
t49_m_detailed = sim_Score(nccu_t049[[1]], nccu_t049[[2]], transCost = transCost, m = M_nccu, boundaries = bounds_nccu, record = T)

operations = rbind(t16_m_detailed, t25_m_detailed, t49_m_detailed)
punct = c("\\?", "\\.", "--", ",")
punct_regex = paste0(" (", paste0(punct, collapse = "|"), ")")

getPuncts = function(text){
  result = text$Utterance %>% str_extract_last(punct_regex)
  result = substr(result[!is.na(result)], 2, nchar(result[!is.na(result)]))
  case_when(result == "--" ~ "+", T ~ result)
}


getOpDist = function(report){
  ops = report$record
  puncts = c(",", ".", "?", "+")
  result = data.frame()

  for(punct in puncts){
    ops_subst_1 = ops %>% filter(e1 == punct, e2 != ";", e2 != " ", type == "Substitution")
    ops_subst_pt_1 = ops %>% filter(e1 == punct, e2 != ";", e2 != " ", type == "Substitution", postTranspose)
    ops_unclass_1 = ops %>% filter(e1 == punct, e2 == ";", type == "Substitution")
    ops_del_1 = ops %>% filter(e1 == punct, e2 == " ", type == "Substitution")
    ops_trans_1 = ops %>% filter(e1 == punct, type == "Transposition")

    subst_1 = nrow(ops_subst_1)
    del_1 = nrow(ops_del_1)
    unclass_1 = nrow(ops_unclass_1)
    trans_pure_1 = nrow(ops_trans_1) - nrow(ops_subst_pt_1)


    ops_subst_2 = ops %>% filter(e2 == punct, e1 != ";", e1 != " ", type == "Substitution")
    ops_subst_pt_2 = ops %>% filter(e2 == punct, e1 != ";", e1 != " ", type == "Substitution", postTranspose)
    ops_unclass_2 = ops %>% filter(e2 == punct, e1 == ";", type == "Substitution")
    ops_del_2 = ops %>% filter(e2 == punct, e1 == " ", type == "Substitution")
    ops_trans_2 = ops %>% filter(e2 == punct, type == "Transposition")

    subst_2 = nrow(ops_subst_2)
    del_2 = nrow(ops_del_2)
    unclass_2 = nrow(ops_unclass_2)
    trans_pure_2 = nrow(ops_trans_2) - nrow(ops_subst_pt_2)

    correct = sum(report$fullMatches == punct)

    result = rbind(result,
                   rbind(ops_subst_1 %>% dplyr::rename(op = e2, punct = e1), ops_subst_2  %>% dplyr::rename(op = e1, punct = e2)) %>%
                      group_by(op, punct) %>% count() %>%
                      ungroup %>%
                      dplyr::add_row(op = "tr", n = trans_pure_1 + trans_pure_2, punct = punct) %>%
                      add_row(op = "del", n = del_1 + del_2, punct = punct) %>%
                      add_row(op = "uc", n = unclass_1 + unclass_2, punct = punct) %>%
                      add_row(op = "/", n = correct * 2, punct = punct)
    )
  }

  result

}


reports = list("t16"= t16_m_detailed, "t25"=  t25_m_detailed, "t49" = t49_m_detailed)

ops_all = lapply(names(reports), function(textID){
  getOpDist(reports[[textID]]) %>% mutate(text = textID)
}) %>% bind_rows

library(ggpattern)

ops_summ = ops_all %>%
  filter(op != "uc") %>%
  group_by(op, punct) %>% summarise(n = sum(n)) %>% group_by(punct) %>% mutate(perc = n / sum(n)) %>%
  ungroup %>%
  mutate(op = case_when(op == "/" ~ "Full match",
                        op == "del" ~ "Deletion",
                        op == "tr" ~ "Transposition",
                        op == "," ~ "Substitution by ,",
                        op == "." ~ "Substitution by .",
                        op == "?" ~ "Substitution by ?",
                        op == "+" ~ "Substitution by --",
                        T ~ op) %>%
           factor(levels = c("Full match", "Substitution by ,", "Substitution by .", "Substitution by ?", "Substitution by --", "Deletion", "Transposition"))) %>%
  mutate(punct = case_when(punct == "+" ~ "--", T  ~ punct)%>% factor(levels = c("/", ",", ".", "?", "--"))) %>%
  mutate(pattern = case_when(op == "Full match" ~ "stripe",
                             op == "Substitution by ," ~ "crosshatch",
                             op == "Substitution by ." ~ "point",
                             op == "Substitution by ?" ~ "circle",
                             op == "Substitution by --" ~ "stripe",
                             op == "Deletion" ~ "crosshatch",
                             op == "Transposition" ~ "point"))
ggplot(ops_summ, aes(fill = op, x = punct, y =perc, pattern = pattern)) +  xlab ("Percentage") + ylab("Endnote") + geom_bar_pattern(stat="identity")


ops_summ_2 = ops_all %>%
  filter(op != "uc") %>%
  group_by(op, punct) %>% summarise(n = sum(n)) %>% group_by(punct) %>% mutate(perc = n / sum(n)) %>%
  ungroup %>%
  mutate(op = case_when(op == "+" ~ "--",
                        op == "/" ~ "match",
                        T ~ op) %>%
           factor(levels = c("match", ",", ".", "?", "--", "del", "tr"))) %>%
  mutate(punct = case_when(punct == "+" ~ "--", T  ~ punct)%>% factor(levels = c("/", ",", ".", "?", "--")))
ggplot(ops_summ_2, aes(fill = op, x = punct, y =perc)) +  xlab ("Percentage") + ylab("Endnote") + geom_bar(stat="identity", position = "dodge")


ggplot(ops_summ_2 %>%
         mutate(punct = case_when(punct == "," ~ "Continuing (,)",
                                  punct == "." ~ "Falling (.)",
                                  punct == "?" ~ "Appeal (?)",
                                  punct == "--" ~ "Truncation (--)")), aes(x = op, y =perc)) +  xlab ("Endnote") + ylab("Percentage") + geom_bar(stat="identity") + facet_wrap(vars(punct), ncol = 2)
