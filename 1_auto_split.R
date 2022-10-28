# Only change line 5 and line something at the end 
library(stringr)
library(tidyverse)
file_name <- "NCCU-TM004-CN-FF.csv"  # Only change the file path to the input file in <0_raw>
TM <- read_csv(paste0("0_raw/", file_name))  


# ... separation
dot3_sep<-function(d){
  u <- data.frame()
  s <- data.frame()
  t <- data.frame()
  d <- mutate(d, Utterance = (str_replace_all(d$Utterance, "…", '...')))
  for (i in 1:length(d$Utterance)) {  
    y<-str_locate_all(d$Utterance[i], "\\[{0,1}\\.{2,3}\\]{0,1}")[[1]]
    # if(!(is.na(y))){
    z<-c(1)  
    for(j in y[,1] ){
      z<-c(z,j)
    }
    z<-c(z,nchar(d$Utterance[i])+1)
    if(length(z)==1){
      u=rbind(u,d$Utterance[i])
      s=rbind(s,d$Speaker[i])
      t=rbind(t,d$Turn[i])
    }else{
      for(k in 1:(length(z)-1)){
        if (!((substring(d$Utterance[i],z[k],z[k+1]-1))=='')){
          u=rbind(u,substring(d$Utterance[i],z[k],z[k+1]-1))
          s=rbind(s,d$Speaker[i])
          t=rbind(t,d$Turn[i])
        }
      }
    }
  }
  a=data.frame()
  a=data.frame(Turn = t, Speaker = s, Utterance = u)
  colnames(a)= c("Turn","Speaker","Utterance")
  return(a)
}
x=dot3_sep(TM)

# change "[[[">"[3"
a= x %>%
  mutate(Utterance=ifelse(str_detect(Utterance, "\\[{3}"),
                          gsub('\\[{3}', '[3', Utterance),Utterance))%>%
  mutate(Utterance=ifelse(str_detect(Utterance, "\\[{2}"),
                          gsub('\\[{2}', '[2', Utterance),Utterance))%>%
  mutate(Utterance=ifelse(str_detect(Utterance, "\\]{3}"),
                          gsub('\\]{3}', '3]', Utterance),Utterance))%>%
  mutate(Utterance=ifelse(str_detect(Utterance, "\\]{2}"),
                          gsub('\\]{2}', '2]', Utterance),Utterance))


# bracket overlap
brkt_overlap<-function(d,n){ # (data, max number of bracket)
  re_list1=c("\\[\\D","\\[2","\\[3","\\[4","\\[5","\\[6","\\[7","\\[8","\\[9")
  re_list2=c("\\D\\]","2\\]","3\\]","4\\]","5\\]","6\\]","7\\]","8\\]","9\\]")
  
  for (m in (1:n)){
    i=0
    while(i<length((d[,3]))){
      i=i+1
      if(str_detect(d[i,3],re_list1[m])){
        for(j in i:length((d[,3]))){
          if(str_detect(d[j,3],re_list2[m])){
            for(k in (j+1):length((d[,3]))){  
              if(str_detect(d[k,3],re_list1[m])){
                for(l in (k):length((d[,3]))){
                  if(str_detect(d[l,3],re_list2[m])){
                    if ((j+1)<=(k-1)){
                      d=rbind(d[1:j, ],
                              d[k:l, ],
                              d[(j+1):(k-1), ],
                              d[(l+1):nrow(d), ])
                      i=i+(l-k)+1
                    }else{
                      d=rbind(d[1:j, ],d[k:l, ],
                              d[(l+1):nrow(d), ])
                      i=i+1+j-i
                    }
                    # 1:j + k:l + (j+1:k) + (l+1:n)
                    
                    break
                  }
                }
                break
              }
            }
            break
          }
        }
      }
    } 
  }
  return(d)
}
o=brkt_overlap(a,3)


result= o %>%
  mutate(Utterance=ifelse(str_detect(Utterance, "\\[3"),
                          gsub('\\[3', '[[[', Utterance),Utterance))%>%
  mutate(Utterance=ifelse(str_detect(Utterance, "\\[2"),
                          gsub('\\[2', '[[', Utterance),Utterance))%>%
  mutate(Utterance=ifelse(str_detect(Utterance, "3\\]"),
                          gsub('3\\]', ']]]', Utterance),Utterance))%>%
  mutate(Utterance=ifelse(str_detect(Utterance, "2\\]"),
                          gsub('2\\]', ']]', Utterance),Utterance))

write_csv(result,paste0("1_auto_split/", file_name))

