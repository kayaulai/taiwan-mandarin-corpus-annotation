library(spacyr)
library(tidyverse)
library("stringr")
library(dplyr)
spacy_install(lang_models="zh_core_web_sm")
spacy_initialize(model="zh_core_web_sm")

file_name <- "NCCU-TM017 - Sheet1.csv"  # Only change the file path to the input file in <1_auto_split>
data <- read_csv(paste0("1_auto_split/", file_name))

unlist(data$Utterance)
parsedfile <- str_replace_all(data$Utterance,"[[:print:]]","")
parsedfile<-spacy_tokenize(data$Utterance)
## change ??()??
changeB<-function(p){
  for (i in seq(1,length(p))){
    temp=""
    temp2=""
    x=0
    for (g in seq(1,length(p[[i]]))){
    if (substring(p[[i]][g],1,1)=="("){
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
  c1="< L"
  c7="L (\\d)"
  c2="(\\d) >"
  c8= "([^ <])L(\\d)"
  c16= "L(\\d)([^ >])"
  c3="\\( "
  c4= " \\)"
  c5="< @"
  c6="@ >"
  c9 ="([^ ])<" # ^ means except, [^ ] means except blank
  c10 =">([^ ])"
  c11="\\)([^ ])"
  c12="\\[ "
  c13=" \\]" # change [ a ] to [a]
  c14 = "(\\d) \\."
  c15 = "\\. (\\d)"  
  for (i in seq(1,length(p))){
    new_p = ""
    a=""
    for (j in seq(1,length(p[[i]]))){
      a = c(a,p[[i]][j])
      new_p = paste(a,collapse = " ")
    }
    p[[i]] = new_p
    p[[i]]=str_replace_all(p[[i]],c1,"<L")
    p[[i]]=str_replace_all(p[[i]],c2,"\\1>")
    p[[i]]=str_replace_all(p[[i]],c7,"L\\1")
    p[[i]]=str_replace_all(p[[i]],c8,"\\1 L\\2")
    p[[i]]=str_replace_all(p[[i]],c16,"L\\1 \\2")
    p[[i]]=str_replace_all(p[[i]],c3,"(")
    p[[i]]=str_replace_all(p[[i]],c4,")")
    p[[i]]=str_replace_all(p[[i]],c5,"<@")
    p[[i]]=str_replace_all(p[[i]],c6,"@>")
    p[[i]]=str_replace_all(p[[i]],c9,"\\1 <")#change <to<
    p[[i]]=str_replace_all(p[[i]],c10,"> \\1") #first digit after >
    p[[i]]=str_replace_all(p[[i]],c11,"\\) \\1") #change )to) 
    p[[i]]=str_replace_all(p[[i]],c12,"[")
    p[[i]]=str_replace_all(p[[i]],c13,"]")
    p[[i]]=str_replace_all(p[[i]],c14,"\\1.")
    p[[i]]=str_replace_all(p[[i]],c15,".\\1")
  }
  return(p)
}
test<-conjunct(parsedfile)
test

change_column=unlist(test)
data=mutate(data, Utterance = change_column)
write_csv(data,paste0("2_auto_tokenised/", file_name))
