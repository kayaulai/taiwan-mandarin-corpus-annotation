library(data.table)
library(readr)
library(reshape)

l1="{ }{,}{,}{ }{.}{ }{ }{,}{.}{ }{-}{,}{ }{,}"
l2="{ }{ }{,}{-}{!}{,}{ }{ }{.}{,}{ }{ }{-}{,}"

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

# Delete same empty
delEmpty<-function(l1,l2){
  if (length(l1)!=length(l2)){
    return ("different speakers try again")
  }  # check speaker num
  for (s in seq(1,length(l1))){
    if (nchar(l1[s])!=nchar(l2[s])){
      return ("different length of elements")
    }
  }  # check element length
  
  r1=data.frame(array(c('',''),dim = c(1,1,num_speaker)))
  colnames(r1)=levels(as.factor(d$Speaker))
  r2=data.frame(array(c('',''),dim = c(1,1,num_speaker)))
  colnames(r2)=levels(as.factor(d$Speaker))
  
  for (s in seq(1,length(l1))){  # for each speaker
    for (i in seq(1,nchar(l1[s]))){  # for index of element in each list
      e1 =substring(l1[1,s],i,i)
      e2 =substring(l2[1,s],i,i)
      if (!(e1==e2 & e1 ==" ")){
        r1[1,s]=paste0(r1[1,s],e1)
        r2[1,s]=paste0(r2[1,s],e2)
      }
    }
  }
return(c(r1,r2))
        
}  # input 2 genBd

# Calculate cost of two boundry lists
# add/del/sub=1 cost£¬ trans=0.5 cost
calCost<-function(l1,l2){
  if (length(l1)!=length(l2)){
    return ("different speakers try again")
  }  # check speaker num
  for (s in seq(1,length(l1))){
    if (nchar(l1[s])!=nchar(l2[s])){
      return ("different length of elements")
    }
  }  # check element length
  
  addCost=1
  delCost=1
  subCost=1
  transCost=0.5
  
  cost=0
  for (s in seq(1,length(l1))){  # for each speaker
    # initialize confusion matrix
    dpArray=array(dim=c(nchar(l1[s])+1,nchar(l1[s])+1))  
    dpArray[1,1]=0
    temp=0
    
    # initialize first row
    for (i in seq(2,nchar(l1[s])+1)){  # for index of element in each list
      e1 =substring(l1[1,s],i,i)
      if (e1!=" "){
        temp=temp+1
      }
      dpArray[1,i]=temp
    }
    # initialize first column
    temp=0
    for (i in seq(2,nchar(l2[s])+1)){  # for index of element in each list
      e2 =substring(l2[1,s],i,i)
      if (e2!=" "){
        temp=temp+1
      }
      dpArray[i,1]=temp
    }
    
    # dp algorithm
    for (i in seq(2,nchar(l1[s])+1)){ 
      for (j in seq(2,nchar(l2[s])+1)){  
        e1 =substring(l1[1,s],i,i)
        e2 =substring(l2[1,s],j,j)
        if (e1 == e2){
          Cost = 0
        }else{
          Cost = subCost
        }
        dpArray[i,j]=min(dpArray[i-1, j] + delCost,     # deletion
                         dpArray[i, j-1] + addCost,     # insertion
                         dpArray[i-1, j-1] + Cost)      # substitution
        if (i > 1 & j > 1 & 
            e1 == substring(l2[1,s],j-1,j-1) & 
            substring(l1[1,s],i-1,i-1) == e2){
          dpArray[i, j]=min(dpArray[i, j],
                            dpArray[i-2, j-2] + transCost)  # transposition
        }
      }
    }
    cost=cost+dpArray[nchar(l1[s])+1, nchar(l2[s])+1]
  }
  return (cost)
}  # input 2 delEmpty

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
emplist1=cbind(delEmpty(bdlist1,bdlist2)[1],delEmpty(bdlist1,bdlist2)[2])
emplist2=cbind(delEmpty(bdlist1,bdlist2)[3],delEmpty(bdlist1,bdlist2)[4])

cost=calCost(emplist1,emplist2)
bdNumber=bdNum(bdlist1)
sim=simScore(bdNumber,cost)




checkdiff<-function(l1,l2){
  for (s in seq(1,num_speaker)){  
    for (i in seq(1,nchar(l1[s]))){ 
      if (substring(l1[1,s],i,i) != substring(l2[1,s],i,i)){
        print(c(substring(l1[1,s],i,i), substring(l2[1,s],i,i)))
      }
    }
  }
}



