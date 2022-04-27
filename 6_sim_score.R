library(data.table)
library(readr)
library(reshape)
library(tidyverse)
library(dplyr)

"{ }{,}{,}{ }{.}{ }{ }{,}{.}{ }{-}{,}{ }{,}"
"{ }{ }{,}{-}{!}{,}{ }{ }{.}{,}{ }{ }{-}{,}"

# Replace --
reDS <- function(d){
  result=mutate(d, Utterance = (str_replace_all(d$Utterance, "--", '+')))
  return(result)
}
# Replace NA speakers
reNA <- function(d){
  for (i in seq(1,length(d$Utterance))){  # for each row
    if (is.na(d$Speaker[i])){
      d$Speaker[i]=d$Speaker[i-1]
    }
  }
  return(d)
}  # input modified data from csv
# Seperate speakers
sepSpeaker <- function(d){
  num_speaker=length(levels(as.factor(d$Speaker)))  
  # Create empty datadrame
  sep=data.frame(array(dim = c(max(summary(as.factor(d$Speaker))),1,2)))
  colnames(sep)=levels(as.factor(d$Speaker))
  # Seperate
  for (s in seq(1,num_speaker)){  # for each speaker
    for (i in seq(1,length(d$Utterance))){  # for each row
      if (d$Speaker[i]==levels(as.factor(d$Speaker))[s]){
        sep[i,s]=d$Utterance[i]
      }
    }
  }
  return(sep)
}  # input reNA
# Generate boundry lists
# return one list of boundary 
genBd <- function(d,sep){
  num_speaker=length(levels(as.factor(d$Speaker)))
  bd=data.frame(array(c('',''),dim = c(1,1,num_speaker)))
  colnames(bd)=levels(as.factor(d$Speaker))
  # fill in boundry lists
  for (s in seq(1,num_speaker)){  # for each speaker
    for (i in seq(1,length(sep[,s]))){  # for each row
      if (!(is.na(sep[i,s]))){
        for (j in seq(2,nchar(sep[i,s])-2)){  # for each element
          if (substring(sep[i,s],j,j)==" "){
            bd[1,s]=paste0(bd[1,s]," ")
          }
        }
        bd[1,s]=paste0(bd[1,s],paste0(substring(sep[i,s],
                                                nchar(sep[i,s]),
                                                nchar(sep[i,s]))))
      }
    }
  }
  return(bd)
}  # input reNA, sepSpeaker
# With record
# Calculate cost of two boundary lists
# sub=1 cost, trans=0.5 cost/dist 
calCost <- function(l1,l2){
  record=data.frame()
  subCost=1
  transCost=0.5
  if (length(l1)!=length(l2)){
    return ("different speakers try again")
  }  # check speaker num
  for (s in seq(1,length(l1))){
    if (nchar(l1[s])!=nchar(l2[s])){
      return ("different length of elements")
    }
  }  # check element length
  
  cost=0  
  t1=""
  t2=""  # temp for cost between two fixed boundries
  for (s in seq(1,length(l1))){  # for each speacker
    for (i in seq(1,nchar(l1[s]))){  # for index of element in each list
      e1 =substring(l1[s],i,i)
      e2 =substring(l2[s],i,i)
      if (e1==e2 & e1!=" " & e2!=" "){
        if (t1!="" & t2!="" ){
          t11=data.frame(n=seq(nchar(t1)),e=c(sepChar(t1)[[1]]))
          t22=data.frame(n=seq(nchar(t2)),e=c(sepChar(t2)[[1]]))
          paresult=parSim(t11,t22)
          sd=sTOd(paresult[2])
          cost=cost+as.numeric(paresult[1])
          if (paresult[2] !=""){
            df=data.frame(speaker=s,
                          type=sd$type,
                          oldPosition=as.numeric(sd$oldPosition)+i-length(t1),
                          newPosition=as.numeric(sd$newPosition)+i-length(t1), 
                          oldChar=sd$oldChar, 
                          newChar=sd$newChar)
            record = rbind(record,df)
          }
          t1=""
          t2=""}
      }else{
        if (e1!=e2 & e1!=" " & e2!=" " ){
          cost=cost+subCost
          if (paresult[2] !=""){
            df=data.frame(speaker=s,
                          type="s",
                          oldPosition=i,
                          newPosition=i, 
                          oldChar=e1, 
                          newChar=e2)
            record=rbind(record,df)
          }
          if (t1!="" & t2!= ""){
            t11=data.frame(n=seq(nchar(t1)),e=c(sepChar(t1)[[1]]))
            t22=data.frame(n=seq(nchar(t2)),e=c(sepChar(t2)[[1]]))
            paresult=parSim(t11,t22)
            sd=sTOd(paresult[2])
            cost=cost+as.numeric(paresult[1])
            if (paresult[2] !=""){
              df=data.frame(speaker=s,
                            type=sd$type,
                            oldPosition=as.numeric(sd$oldPosition)+i-length(t1),
                            newPosition=as.numeric(sd$newPosition)+i-length(t1), 
                            oldChar=sd$oldChar, 
                            newChar=sd$newChar)
              record = rbind(record,df)
            }
            t1=""
            t2=""
          }
        }else{
          t1=paste0(t1,e1)
          t2=paste0(t2,e2)
        }
      }
    }
    if (t1!="" & t2!= ""){
      t11=data.frame(n=seq(nchar(t1)),e=c(sepChar(t1)[[1]]))
      t22=data.frame(n=seq(nchar(t2)),e=c(sepChar(t2)[[1]]))
      paresult=parSim(t11,t22)
      sd=sTOd(paresult[2])
      cost=cost+as.numeric(paresult[1])
      if (paresult[2] !=""){
        df=data.frame(speaker=s,
                      type=sd$type,
                      oldPosition=as.numeric(sd$oldPosition)+i-length(t1),
                      newPosition=as.numeric(sd$newPosition)+i-length(t1), 
                      oldChar=sd$oldChar, 
                      newChar=sd$newChar)
        record = rbind(record,df)
      }
      t1=""
      t2=""
    }
  }
  return(c(cost,record))
}  # input 2 genBd
# Calculate cost between each sub string Without record
parSim <- function(t1,t2){
  subCost=1
  transCost=0.5
  
  e1 =t1$e[1]
  e2 =t2$e[1]
  
  s1=t1[2:nrow(t1),]
  s2=t2[2:nrow(t2),]
  
  t=rbind(t2[2,],t2[1,],t2[3:nrow(t1),])
  
  sdf=paste0("s","#",t1$n[1],"#",e1,"#",t2$n[1],"#",e2,"&")
  tdf=paste0("t","#",t1$n[1],"#",e1,"#",t1$n[2],"#",t1$e[2],"#",t2$n[1],"#",e2,"#",t2$n[2],"#",t2$e[2],"&")

  
  if (nrow(t1)<=1){
    if (e1==e2){
      return (c(0,''))
    }else{
      return (c(subCost,sdf))
    }
  }else{
    if (e1==e2){
      return(c(parSim(s1,s2)[1],
               paste0(parSim(s1,s2)[2])))
    }else{
      if(e1!=" " & e2!=" "){
        return(c(subCost+as.numeric(parSim(s1,s2)[1]),
                 paste0(parSim(s1,s2)[2],sdf))
        )
      }else{
        if ((t2$e[2]!=" " & t2$e[1]==" ")){
          if (subCost+as.numeric(parSim(s1,s2)[1])
              <transCost+as.numeric(parSim(t1,t)[1])){
            return(c(subCost+as.numeric(parSim(s1,s2)[1]),
                     paste0(parSim(s1,s2)[2],sdf))
            )
          }else{
            return(c(transCost+as.numeric(parSim(t1,t)[1]),
                     paste0(parSim(t1,t)[2],tdf))
            )
          }
        }else{if((t2$e[2]==" " & t2$e[1]!=" ")){
          if (subCost+as.numeric(parSim(s1,s2)[1])
              <transCost+as.numeric(parSim(t1,t)[1])){
            return(c(subCost+as.numeric(parSim(s1,s2)[1]),
                     paste0(parSim(s1,s2)[2],sdf))
            )
          }else{
            return(c(transCost+as.numeric(parSim(t1,t)[1]),
                     paste0(parSim(t1,t)[2],tdf))
            )
          }
        }else{
          return(c(subCost+as.numeric(parSim(s1,s2)[1]),
                   paste0(parSim(s1,s2)[2],sdf))
          )
        }
        }
      }
    }
  }
}
# Without record
# Calculate cost of two boundary lists
calCost1<-function(l1,l2){
  record=data.frame()
  subCost=1
  transCost=0.5
  if (length(l1)!=length(l2)){
    return ("different speakers try again")
  }  # check speaker num
  for (s in seq(1,length(l1))){
    if (nchar(l1[s])!=nchar(l2[s])){
      return ("different length of elements")
    }
  }  # check element length
  
  cost=0  
  t1=""
  t2=""  # temp for cost between two fixed boundries
  for (s in seq(1,length(l1))){  # for each speacker
    for (i in seq(1,nchar(l1[s]))){  # for index of element in each list
      e1 =substring(l1[s],i,i)
      e2 =substring(l2[s],i,i)
      if (e1==e2 & e1!=" " & e2!=" "){
        cost=cost+parSim1(t1,t2)
        # record
        t1=""
        t2=""
      }else{
        if (e1!=e2 & e1!=" " & e2!=" " ){
          cost=cost+subCost
          record=rbind(record,data.frame(old= e1, new = e2))
          cost=cost+parSim1(t1,t2)
          # record
          t1=""
          t2=""
        }else{
          t1=paste0(t1,e1)
          t2=paste0(t2,e2)
        }
      }
    }
    cost=cost+parSim1(t1,t2)
    # record
    t1=""
    t2=""
  }
  return(cost)
}  # input 2 genBd
# Calulate cost between each sub string Without record
parSim1<- function(t1,t2){
  subCost=1
  transCost=0.5
  e1 =substring(t1,1,1)
  e2 =substring(t2,1,1)
  s1=substring(t1,2,nchar(t1))
  s2=substring(t2,2,nchar(t2))
  t=paste0(substring(t2,2,2),substring(t2,1,1),substring(t2,3,nchar(t2)))
  if (nchar(t1)<=1){
    if (e1==e2){
      return (0)
    }else{
      return (subCost)
    }
  }else{
    if (e1==e2){
      return(parSim1(s1,s2))
    }else{
      if(e1!=" " & e2!=" "){
        return(subCost+parSim1)
      }else{
        if ((substring(t2,2,2)!=" " & substring(t2,1,1)==" ")
            | (substring(t2,2,2)==" " & substring(t2,1,1)!=" ")){
          return(min(subCost+parSim1(s1,s2),transCost+parSim1(t1,t)))
        }else{
          return(subCost+parSim1(s1,s2))
        }
      }
    }
  }
}
# Transform the parsim String to dataframe.
sTOd <- function(s){
  slist=strsplit(s,"&")
  result= data.frame()
  for (i in slist[[1]]){
    slist2=strsplit(i,"#")
    a=slist2[[1]][1]
    if (a=="s"){
      result=rbind(result, data.frame(type="s",
                              oldPosition=slist2[[1]][2], 
                              newPosition=slist2[[1]][4], 
                              oldChar=slist2[[1]][3], 
                              newChar=slist2[[1]][5]))
    }else{
      if (substring(i,3,3)==" "){
        result=rbind(result, data.frame(type="t",
                                 oldPosition=slist2[[1]][4], 
                                 newPosition=slist2[[1]][2], 
                                 oldChar=slist2[[1]][5], 
                                 newChar=slist2[[1]][5]))
      }else{
        result=rbind(result, data.frame(type="t",
                                        oldPosition=slist2[[1]][2], 
                                        newPosition=slist2[[1]][4], 
                                        oldChar=slist2[[1]][3], 
                                        newChar=slist2[[1]][3]))
      }

    }
  }
  return(result)
}
# Separate strings
sepChar <- function(t1){
  return (strsplit(t1,''))
}
# Calculate bound number
bdNum <- function(bd){  # input boundry list
  n=0
  l=0
  for (s in length(bd)){  # for each speacker
    for (i in seq(1,nchar(bd[s]))){  # for index of element in each list
      e =substring(bd[1,s],i,i)
      l=l+1
    }
    n=n+l
    l=0
  }
  return(n)
}  # input genBd
# calculate sim score
simScore <- function(n, cost){
  return((n-cost)/(n))
}  # input bdNum, calCost
# check difference between two bdlists
checkDiff <- function(l1,l2){
  d1=''
  d2=''
  for (s in seq(1,length(l1))){  # for each speacker
    for (i in seq(1,nchar(l1[s]))){  # for index of element in each list
      e1 =substring(l1[s],i,i)
      e2 =substring(l2[s],i,i)
      if (e1!=e2){
        d1=paste0(d1,'|',e1,i)
        d2=paste0(d2,'|',e2,i)
      }
    }
  }
  d1=paste0(d1,'|')
  d2=paste0(d2,'|')
  return(data.frame(d1,d2))
}
# final function (record = TRUE produce the step of change but it will be super slow)
sim_Score<-function(d1,d2, record = FALSE){
  d1=reDS(d1)
  d1=reNA(d1)
  se1=sepSpeaker(d1)
  bdlist1=genBd(d1,se1)
  
  d2=reDS(d2)
  d2=reNA(d2)
  se2=sepSpeaker(d2)
  bdlist2=genBd(d2,se2)
  
  if (record == TRUE){
    cost=calCost(bdlist1,bdlist2)
    bdNumber=bdNum(bdlist1)
    sim=simScore(bdNumber,as.numeric(cost[1]))
    return(c(cost,sim))
  }else{
    cost=calCost1(bdlist1,bdlist2)
    bdNumber=bdNum(bdlist1)
    sim=simScore(bdNumber,cost)
    return(sim)
  }
}

# main (example)
data1=read_csv("modifieddata1.csv")
data2=read_csv("modifieddata2.csv")
sim_Score(data1,data2)


