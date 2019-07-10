best<-function(state,outcome2){
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
      
        hn<-min(as.numeric(hospitals3$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
        hn2<-hospitals3[hospitals3$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==hn,]
        hn3<-sort(hn2$Hospital.Name)
        print(hn3[1])
        }
        else if(outcome2=="heart failure")
        {
          hospitals2<-hospitals %>% select(c(2,17))
          hospitals3<-hospitals2[!is.na(as.numeric(as.character(hospitals2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))),]
          hn<-min(as.numeric(hospitals3$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
          hn2<-hospitals3[hospitals3$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure==hn,]
          hn3<-sort(hn2$Hospital.Name)
          print(hn3[1])
          }
        else if(outcome2=="pneumonia")
        {
          hospitals2<-hospitals %>% select(c(2,23))
          hospitals3<-hospitals2[!is.na(as.numeric(as.character(hospitals2$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))),]
          hn<-min(as.numeric(hospitals3$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
          hn2<-hospitals3[hospitals3$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia==hn,]
          hn3<-sort(hn2$Hospital.Name)
          print(hn3[1])
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
  