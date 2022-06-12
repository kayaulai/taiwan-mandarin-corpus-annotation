library(data.table)
library(readr)
library(reshape)
library(tidyverse)
library(dplyr)
## only need to change file name in line 406 407

source("6_sim_score.R")# input Yujie's 6_sim_score.R
puncNum <- function(bd){ # Used to get the number of each kind's punctuation in data.bd is the number of punctuation
  commaNum = 0
  periodNum = 0
  questionNum = 0
  dashNum = 0
  n=0  # number of boundries in a list
  l=0  # temperate variable for counting
  for (s in length(bd)){  # for each speaker why length (bd) mens speaker here?
    for (i in seq(1,nchar(bd[s]))){  # for index of element in each list
      e =substring(bd[1,s],i,i)
      if (substring(bd[1,s],i,i) == ","){
        commaNum = commaNum + 1
      }
      if (substring(bd[1,s],i,i) == "."){
        periodNum = periodNum + 1
      }
      if (substring(bd[1,s],i,i) == "?"){
        questionNum = questionNum + 1
      }
      if (substring(bd[1,s],i,i) == "+"){
        dashNum = dashNum + 1
      }
      l=l+1
    }
    n=n+l
    l=0
  }
  Num = c(commaNum,periodNum,questionNum,dashNum)
  #print(typeof(Num))
  return(Num)
}
gePlace <- function(num,number){ # generate a place to insert punctuation
  # creating a list with sample function
  lst = list(sample(1:num, size = number, replace = T))
  return (lst)
}
insertPunc <- function(place,puncnum,number,num,bd){ #insert punctuation based on gePlace
  puncList = rep(" ",num)
  for (i in seq(1,num)){#use lengths to get the length of each data in ist
    for (j in seq(1,length(bd))){
      if (i %in% data1Place[[1]]){
        prob = floor(runif(1,min=1,max=number))
        if (1<=prob && prob<puncnum[1]){ #if the number is in range of comma
            data[[j]][] = ","
        }
        if (puncnum[1]<=prob && prob<(puncnum[2]+puncnum[1])){ #if the number is in range of period
          puncList[i] = "."
        }
        if ((puncnum[2]+puncnum[1])<=prob && prob<(puncnum[2]+puncnum[3]+puncnum[1])){ #if the number is in range of ?
          puncList[i] = "?"
        }
        if ((puncnum[2]+puncnum[3]+puncnum[1])<=prob && prob<(puncnum[2]+puncnum[3]+puncnum[4]+puncnum[1])){ #if the number is in range of ?
          puncList[i] = "+"
        }
      }
    }
  }
  puncList = paste(puncList, collapse = "")
  return (puncList)
}
speaker_num <- function(bd,dataPlace,number,puncnum,num){ #generate random puncList
  allP=c(",",".","?","+")
  puncList = rep(" ",num)
  for (i in seq(1,length(bd))){
    for (j in seq(1,nchar(bd[[i]]))){ 
        prob = floor(runif(1,min=1,max=num))
        if (1<=prob && prob<puncnum[1]){#if the number is in range of comma
          substring(bd[[i]],j,j) = ","
        }
        if (puncnum[1]<=prob && prob<(puncnum[2]+puncnum[1])){ #if the number is in range of period
          substring(bd[[i]],j,j) = "."
        }
        if ((puncnum[2]+puncnum[1])<=prob && prob<(puncnum[2]+puncnum[3]+puncnum[1])){ #if the number is in range of ?
          substring(bd[[i]],j,j) = "."
        }
        if ((puncnum[2]+puncnum[3]+puncnum[1])<=prob && prob<(puncnum[2]+puncnum[3]+puncnum[4]+puncnum[1])){ #if the number is in range of +
          substring(bd[[i]],j,j) = "."
        }
        }
  }
  return (bd)
}

data1=read_csv('NCCU-TM049_Shujie Zhang.csv') # sample data for geeting the numbert of boundary
data2=read_csv('NCCU-TM049_Shujie Zhang_Test.csv')
asim=sim_Score(data1,data2)#check similarity score of two data input

# for inter-annotated agreement
bd1=createBD(data1) # create boundary list for a data file
data1Num = bdNum(bd1)  # calculate the number of boundries for a file 

bd2=createBD(data2) # create boundary list for a data file
data2Num = bdNum(bd2)  # calculate the number of boundries for a file 

data1Puncnum = puncNum(bd1)#number of punctuation in each kind
data1Number = data1Puncnum[1]+data1Puncnum[2]+data1Puncnum[3]+data1Puncnum[4] #get total number of punctuation
data1Place = gePlace(data1Num, data1Number)#num is total number of boundry, number is number of punctuation

data2Puncnum = puncNum(bd2)
data2Number = data2Puncnum[1]+data2Puncnum[2]+data2Puncnum[3]+data2Puncnum[4] #get total number of punctuation
data2Place = gePlace(data2Num, data2Number)#num is total number of boundry, number is number of punctuation

#main function
tcost = numeric(1000) 
for (i in seq(1,1000)){
  dataPunc1 = speaker_num(bd1,data1Place,data1Number,data1Puncnum,data1Num)
  dataPunc2 = speaker_num(bd2,data2Place,data2Number,data2Puncnum,data2Num)
  cost = calCost1(dataPunc1,dataPunc2)#If only need to use calculate cost without using simularity score, use calCost1
  tcost[i] = cost
  print(i)
  }

checkDiff(dataPunc1,dataPunc2) #check the difference between two data input
tcost = sum(tcost)
avgcol=tcost/1000 #calculate average cost

avgsim=(data1Num-avgcol)/data1Num #calculate average simularity score

IAA=(asim-avgsim)/(1-avgsim)#calculate IAA
