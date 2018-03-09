
setwd("/Users/Merrikhpour/Desktop/R Programming Course/week4") 
#install.packages("plyr")
library(plyr)
data2<- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors=FALSE)
df<- data.frame()
# heart attack is column 11, heart failure is column 17, and pneumonia is column 23. 
column_index <- c(11,17,23)
df<- data2[ ,c(2,7,column_index)]
names(df)<- c("Hospital Name", "State", "Heart Attack Death", "Heart Failure Death", "Pneumonia Death"  )
attach(df)


rankhospital<- function(state, outcome, num= "best") {
        
        nthanswer<- character()
        
       
        ## check that state and outcome is valid
        
        if ((sum(state==levels((as.factor(df$State))))==0) && (sum(outcome==c("heart attack", "heart failure", "pneumonia"))==0) ) { paste("Error in best (",state, outcome, ") : invalid state and outcome", sep = " ")}
        else if (sum(state==levels((as.factor(df$State))))==0) { paste("Error in best (",state, outcome, ") : invalid state", sep = " ")} 
        else if (sum(outcome==c("heart attack", "heart failure", "pneumonia"))==0) { paste("Error in best (",state, outcome, ") : invalid outcome", sep = " ")}
        else if ((sum(state==levels((as.factor(df$State))))!=0) && (sum(outcome==c("heart attack", "heart failure", "pneumonia"))!=0) ){ 
                
                if (outcome=="heart attack") {

                        arrangeddf <- arrange(df,df$State,df$`Heart Attack Death`, df$`Hospital Name`)
                        arrangeddfhearattackcompletecases <- arrangeddf[complete.cases(arrangeddf$`Heart Attack Death`), ]
                        splitdf<- split(arrangeddfhearattackcompletecases, arrangeddfhearattackcompletecases$State)
                        orderedhospitals<- sapply(splitdf, function(x) x[[1]])
                        if (num=="best") {  
                                nthhospitals <- lapply(orderedhospitals, function(L) L[1])
                                nthhospitals
                                }
                        else if (num=="worst") { 
                                
                                nthhospitals <- lapply(orderedhospitals, tail, n=1)
                                nthhospitals
                        }
                        else {
                                
                                nthhospitals <- lapply(orderedhospitals, function(L) L[num]) 
                                }
                        
                        nthhospitals
                }
                
                       if (outcome=="heart failure") {
                
                               arrangeddf <- arrange(df,df$State,df$`Heart Failure Death`, df$`Hospital Name`)
                               arrangeddfheartfailurecompletecases <- arrangeddf[complete.cases(arrangeddf$`Heart Failure Death`), ]
                               splitdf<- split(arrangeddfheartfailurecompletecases, arrangeddfheartfailurecompletecases$State)
                               orderedhospitals<- sapply(splitdf, function(x) x[[1]])
                               if (num=="best") {  
                                       nthhospitals <- lapply(orderedhospitals, function(L) L[1])
                                       nthhospitals
                               }
                               else if (num=="worst") { 
                                       
                                       nthhospitals <- lapply(orderedhospitals, tail, n=1)
                                       nthhospitals
                               }
                               else {
                                       
                                       nthhospitals <- lapply(orderedhospitals, function(L) L[num]) 
                               }
                               
                               nthhospitals
                       }
                
                
                    if (outcome=="pneumonia") {
                
                            arrangeddf <- arrange(df,df$State,df$`Pneumonia Death`, df$`Hospital Name`)
                            arrangeddfpneumoniacompletecases <- arrangeddf[complete.cases(arrangeddf$`Pneumonia Death`), ]
                            splitdf<- split(arrangeddfpneumoniacompletecases, arrangeddfpneumoniacompletecases$State)
                            orderedhospitals<- sapply(splitdf, function(x) x[[1]])
                            if (num=="best") {  
                                    nthhospitals <- lapply(orderedhospitals, function(L) L[1])
                                    nthhospitals
                            }
                            else if (num=="worst") { 
                                    
                                    nthhospitals <- lapply(orderedhospitals, tail, n=1)
                                    nthhospitals
                            }
                            else {
                                    
                                    nthhospitals <- lapply(orderedhospitals, function(L) L[num]) 
                            }
                            
                            nthhospitals
                    }
                
    
                assign("nthhospitalsGlobal", nthhospitals, envir = .GlobalEnv)
                nthhospitalsunlisted<- unlist(nthhospitalsGlobal)
                nthanswer<- nthhospitalsunlisted[state]
                assign("nthanswerGlobal", nthanswer, envir = .GlobalEnv)
                nthanswerGlobal
                
        
        }
}