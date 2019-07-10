rankhospital<-function(state,outcome2,num = "best"){
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  library(tidyverse)
  if(outcome2 %in% c("heart attack","heart failure","pneumonia"))
  {
    if(state %in% outcome$State)
    {
      hospitals<-outcome[outcome$State==state,]
      
      if(outcome2=="heart attack")
      {
        hospitals2<-hospitals %>% select(c(2,11))
        hospitals3<-hospitals2[!is.na(as.numeric(as.character(hospitals2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))),]
        hn<-hospitals3[with(hospitals3,order(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),Hospital.Name)),]    
        hn$rank=1:length(hn$Hospital.Name)
        if(num=="best")
        {
          hn2<-hn[hn$rank==1,]
          hn3<-hn2$Hospital.Name
          print(hn3)        }
        else if(num=="worst")
        {
          hn2<-tail(hn,1)
          hn3<-hn2$Hospital.Name
          print(hn3)        }
        else if(num>length(hn$rank))
        {
          print(NA)
        }
        else
        {
          hn2<-hn[hn$rank==num,]
          hn3<-hn2$Hospital.Name
          print(hn3)          }
      }
      else if(outcome2=="heart failure")
      {
        hospitals2<-na.omit(hospitals %>% select(c(2,17)))
        hospitals3<-hospitals2[!is.na(as.numeric(as.character(hospitals2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))),]
        hn<-hospitals3[with(hospitals3,order(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),Hospital.Name)),]
        hn$rank=1:length(hn$Hospital.Name)
        if(num=="best")
        {
          hn2<-hn[hn$rank==1,]
          hn3<-hn2$Hospital.Name
          print(hn3)        }
        else if(num=="worst")
        {
          hn2<-tail(hn,1)
          hn3<-hn2$Hospital.Name
          print(hn3)        }
        else if(num>length(hn$rank))
        {
          print(NA)
        }
        else
        {
          hn2<-hn[hn$rank==num,]
          hn3<-hn2$Hospital.Name
          print(hn3)          }
      }
      else if(outcome2=="pneumonia")
      {
        hospitals2<-na.omit(hospitals %>% select(c(2,23)))
        hospitals3<-hospitals2[!is.na(as.numeric(as.character(hospitals2$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))),]
        hn<-hospitals3[with(hospitals3,order(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),Hospital.Name)),]
        
        hn$rank=1:length(hn$Hospital.Name)
        if(num=="best")
        {
          hn2<-hn[hn$rank==1,]
          hn3<-hn2$Hospital.Name
          print(hn3)
        }
        else if(num=="worst")
        {
          hn2<-tail(hn,1)
          hn3<-hn2$Hospital.Name
          print(hn3)        }
        else if(num>length(hn$rank))
        {
          print(NA)
        }
        else
        {
          hn2<-hn[hn$rank==num,]
          hn3<-hn2$Hospital.Name
          print(hn3)          }
      }
    }
    else
    {
      stop("Invalid State")
    }
    
  }
  
  else
  {
    stop("Invalid Outcome")
  }
}
