TM002<- read_csv("~/Desktop/NCCU-TM002-CN-FF - Sheet1.csv")
TM002
x<-data.frame()
i = "hi ... there"
for (i in TM002$Utterance) {  
  y<-str_locate_all(i, "\\.{2,3}")[[1]]
  # if(!(is.na(y))){
  z<-c()  
  for(j in y[,1] ){
    z<-c(z,j)
    
    
  }
  z<-c(z,nchar(i)+1)
  if(length(z)==1){
    x=rbind(x,i)
    
  }else{
    for(k in 1:(length(z)-1)){
      x=rbind(x,substring(i,z[k],z[k+1]-1))
      print(substring(i,z[k],z[k+1]-1))
      
    }
  }
}
library(stringr)
for(i in 1:length(x)){
  
  if(str_detect(x[[i]],"\\[")){
    for(j in i:length(x)){
      if(str_detect(x[[j]],'\\]')==TRUE){
        for(k in j+1:length(x)){
          if(str_detect(x[[k]],'\\[')==TRUE){
            for(z in k:length(x)){
              if(str_detect(x[[z]],"\\]")==TRUE){
                d_frame<-data_frame()
                for(l in k:z){
                  rbind(d_frame,x[l])
                  print(x[l])
                }
                }
    }
      }
    }
  }
 }
  }
}
