library(spacyr)
library(tidyverse)
library("stringr")
library(dplyr)
spacy_install(lang_models="zh_core_web_sm")
spacy_initialize(model="zh_core_web_sm")

file_name <- "NCCU-TM025-CN-FM.csv"  # Only change the file path to the input file in <1_auto_split>
data <- read_csv(paste0("1_auto_split/", file_name))

unlist(data$Utterance)
parsedfile <- str_replace_all(data$Utterance,"[[:print:]]","")
parsedfile<-spacy_tokenize(data$Utterance)
## change ??()??
changeB<-function(p){
  for (i in seq(1,length(p))){ #Loop on lines
    temp=""
    temp2=""
    x=0
    for (g in seq(1,length(p[[i]]))){ #Loop on tokens
      print(p[[i]][g])
      if (substring(p[[i]][g],1,1)=="("){ #Token starts with a (
        for (j in seq(1,length(p[[i]]))){
          for (k in seq(1,nchar(p[[i]][j]))){
            e=substring(p[[i]][j],k,k)
            temp=paste0(temp,e)
            if (e==")"){
              x=1
              temp2=substring(p[[i]][j],k+1,nchar(p[[i]][j]))
              z=j+1
              temp3=p[[i]][z:length(p[[i]])]
              break
            }
          } 
          if (x==1){
            break
            x=0
          }
        }
        p[[i]]=c(temp,temp2,temp3)
      }
  }
  }
  return(p)
}
parsedfile<-changeB(parsedfile)
parsedfile
conjunct<-function(p){
  for (i in seq(1,length(p))){
    new_p = ""
    a=""
    for (j in seq(1,length(p[[i]]))){
      a = c(a,p[[i]][j])
      new_p = paste(a,collapse = " ")
    }
    p[[i]] = new_p
    
    #Non-Mandarin language tags and tokens
    p[[i]]=str_replace_all(p[[i]],"< L","<L")
    p[[i]]=str_replace_all(p[[i]],"(\\d) >","\\1>")
    p[[i]]=str_replace_all(p[[i]],"L (\\d)","L\\1")
    p[[i]]=str_replace_all(p[[i]],"([^ <])L(\\d)","\\1 L\\2")
    p[[i]]=str_replace_all(p[[i]],"L(\\d)([^ >])","L\\1 \\2")
    p[[i]]=str_replace_all(p[[i]],"([A-Za-z]) \\d ([A-Za-z])","\\1\\d \\2") #Putting tone and syllable together
    
    #Laughter tags
    p[[i]]=str_replace_all(p[[i]],"< @","<@")
    p[[i]]=str_replace_all(p[[i]],"@ >","@>")
    p[[i]]=str_replace_all(p[[i]],"@ @","@@")
    
    #Brackets: Removing spaces between brackets and content
    p[[i]]=str_replace_all(p[[i]],"\\( ","(")
    p[[i]]=str_replace_all(p[[i]]," \\)",")")
    p[[i]]=str_replace_all(p[[i]],"\\[ ","[") # change [ a ] to [a]
    p[[i]]=str_replace_all(p[[i]]," \\]" ,"]")
    
    #Brackets; Adding spaces between brackets and non-content
    p[[i]]=str_replace_all(p[[i]],"([^ ])<" ,"\\1 <")#change <to<
    p[[i]]=str_replace_all(p[[i]],">([^ ])","> \\1") #first digit after >
    p[[i]]=str_replace_all(p[[i]],"([^ ])\\(","\\1 \\(") #change )to) 
    p[[i]]=str_replace_all(p[[i]],"\\)([^ ])","\\) \\1") #change )to) 
    
    #Pause-specific
    p[[i]]=str_replace_all(p[[i]],"(\\d) \\.","\\1.")
    p[[i]]=str_replace_all(p[[i]],"\\. (\\d)"  ,".\\1")
    p[[i]]=str_replace_all(p[[i]],"\\. \\.","\\.\\.")
    p[[i]]=str_replace_all(p[[i]],"\\.([^ .])","\\. \\1")
    
  }
  return(p)
}
test<-conjunct(parsedfile)
test

change_column=unlist(test)
data=mutate(data, Utterance = change_column)
write_csv(data,paste0("2_auto_tokenised/", file_name))
