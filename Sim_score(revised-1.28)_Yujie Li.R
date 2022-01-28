library(data.table)
library(readr)
library(reshape)

l1="{ }{,}{,}{ }{.}{ }{,}{.}{ }{-}{,}{ }{,}"
l2="{ }{ }{,}{-}{!}{,}{ }{.}{,}{ }{ }{-}{,}"



# Replace NA speakers
reNA<-function(d){
  for (i in seq(1,length(d$Utterance))){  # for each row
    if (is.na(d$Speaker[i])){
      d$Speaker[i]=d$Speaker[i-1]
    }
  }
  return(d)
}  # input data from csv

# Seperate speakers
sepSpeaker<-function(d){
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
genBd<-function(d,sep){
  num_speaker=length(levels(as.factor(d$Speaker)))
  bd=data.frame(array(c('',''),dim = c(1,1,num_speaker)))
  colnames(bd)=levels(as.factor(d$Speaker))
  # fill in boundry lists
  for (s in seq(1,num_speaker)){  # for each speaker
    for (i in seq(1,length(sep[,s]))){  # for each row
      if (!(is.na(sep[i,s]))){
        for (j in seq(2,nchar(sep[i,s])-1)){  # for each element
          bd[1,s]=paste0(bd[1,s],"{ }")
        }
        bd[1,s]=paste0(bd[1,s],paste0("{",substring(sep[i,s],
                                                               nchar(sep[i,s]),
                                                               nchar(sep[i,s])),"}"))
      }
    }
  }
  return(bd)
}  # input reNA, sepSpeaker

# Calculate cost of two boundry lists
calCost<-function(l1,l2){
  if (length(l1)!=length(l2)){
    return ("different speakers try again")
  }  # check speaker num
  for (s in seq(1,length(l1))){
    if (nchar(l1[s])!=nchar(l2[s])){
      return ("different length of elements")
    }
  }  # check element length
  cost=0  # add/del=1 cost, sub/trans=0.5 cost
  t1=c()
  t2=c()  # temp for cost between two fixed boundries
  for (s in seq(1,length(l1))){  # for each speacker
    for (i in seq(1,nchar(l1[s]))){  # for index of element in each list
      e1 =substring(l1[1,s],i,i)
      e2 =substring(l2[1,s],i,i)
      if (e1==e2 & e1!="{"& e1!="}"& e2!="{"& e1!="}"){
        cost=cost+parSim(t1,t2)
        t1=c()
        t2=c()
      }
      if (e1!=e2){
        if (e1!=" " & e2!=" "){
          cost=cost+0.5
          cost=cost+parSim(t1,t2)
          t1=c()
          t2=c()
        }
        if (e1==" "){
          t2=c(t2,e2)
        }
        if (e2==" "){
          t1=c(t1,e1)
        }
      }
    }
  }
  return(cost)
}  # input 2 genBd
parSim<- function(t1,t2){
  return(max(length(t1),length(t2)))
}

# Calculate bound number
bdNum<- function(bd){  # input boundry list
  n=0
  l=0
  for (s in length(bd)){  # for each speacker
    for (i in seq(1,nchar(bd[s]))){  # for index of element in each list
      e =substring(bd[1,s],i,i)
      if (e!="}" & e!="{"){
        l=l+1
      }
    }
    n=n+l
    l=0
  }
  return(n)
}  # input genBd

# calculate sim score
simScore<- function(n, cost){
  return((n-cost)/(n))
}  # input bdNum, calCost


# main (example)
data1=read_csv("modifieddata.csv")
data2=read_csv("modifieddata1.csv")
data1=reNA(data1)
data2=reNA(data2)
se1=sepSpeaker(data1)
se2=sepSpeaker(data2)
bdlist1=genBd(data1,se1)
bdlist2=genBd(data2,se2)
cost=calCost(bdlist1,bdlist2)
bdNumber=bdNum(bdlist1)
sim=simScore(bdNumber,cost)

