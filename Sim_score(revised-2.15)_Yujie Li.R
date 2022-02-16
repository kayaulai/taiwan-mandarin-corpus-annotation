library(data.table)
library(readr)
library(reshape)

"{ }{,}{,}{ }{.}{ }{ }{,}{.}{ }{-}{,}{ }{,}"
"{ }{ }{,}{-}{!}{,}{ }{ }{.}{,}{ }{ }{-}{,}"
"{,}{?}{ }"
"{ }{,}{?}"

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
# return one list of boundry 
genBd<-function(d,sep){
  num_speaker=length(levels(as.factor(d$Speaker)))
  bd=data.frame(array(c('',''),dim = c(1,1,num_speaker)))
  colnames(bd)=levels(as.factor(d$Speaker))
  # fill in boundry lists
  for (s in seq(1,num_speaker)){  # for each speaker
    for (i in seq(1,length(sep[,s]))){  # for each row
      if (!(is.na(sep[i,s]))){
        for (j in seq(2,nchar(sep[i,s])-1)){  # for each element
          bd[1,s]=paste0(bd[1,s]," ")
        }
        bd[1,s]=paste0(bd[1,s],paste0(substring(sep[i,s],
                                                nchar(sep[i,s]),
                                                nchar(sep[i,s]))))
      }
    }
  }
  return(bd)
}  # input reNA, sepSpeaker

# Calculate cost of two boundry lists
# sub=1 cost, trans=0.5 cost/dist
calCost<-function(l1,l2){
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
        cost=cost+parSim(t1,t2)
        t1=""
        t2=""
      }else{
        if (e1!=e2 & e1!=" " & e2!=" " ){
          cost=cost+subCost
          cost=cost+parSim(t1,t2)
          t1=""
          t2=""
        }else{
          t1=paste0(t1,e1)
          t2=paste0(t2,e2)
        }
      }
    }
    cost=cost+parSim(t1,t2)
    t1=""
    t2=""
  }
  return(cost)
}  # input 2 genBd

# Calulate cost between each sub string
parSim<- function(t1,t2){
  e1 =substring(t1,1,1)
  e2 =substring(t2,1,1)
  s1=substring(t1,2,nchar(t1))
  s2=substring(t2,2,nchar(t2))
  t=paste0(substring(t2,2,2),substring(t2,1,1),substring(t2,3,nchar(t2)))
  if (nchar(t1)==1){
    if (e1==e2){
      return (0)
    }else{
      return (subCost)
    }
  }else{
    if (e1==e2){
      return(parSim(s1,s2))
    }else{
      if(e1!=" " & e2!=" "){
        return(subCost+parSim(s1,s2))
      }else{
        if ((substring(t2,2,2)!=" " & substring(t2,1,1)==" ")
            | (substring(t2,2,2)==" " & substring(t2,1,1)!=" ")){
          return(min(subCost+parSim(s1,s2),transCost+parSim(t1,t)))
        }else{
          return(subCost+parSim(s1,s2))
        }
      }
    }
  }
}

# Calculate bound number
bdNum<- function(bd){  # input boundry list
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


l1=",? "
l11=" "
l2=" ,?"
l22=" "
a=cbind(l1,l11)
b=cbind(l2,l22)
calCost(a,b)



