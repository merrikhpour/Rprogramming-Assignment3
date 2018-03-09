

setwd("/Users/Merrikhpour/Desktop/R Programming Course/week4") 
#install.packages("plyr")
library(plyr)
datatest1<- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors=FALSE)
df<- data.frame()
# heart attack is column 11, heart failure is column 17, and pneumonia is column 23. 

#datatest1<- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors=FALSE)

column_index <- c(11,17,23)
df<- datatest1[ ,c(2,7,column_index)]
names(df)<- c("Hospital Name", "State", "Heart Attack Death", "Heart Failure Death", "Pneumonia Death"  )
## read outcome data
attach(df)
best <- function (state= character(), outcome= character()) {
       # attach(datatest1)
        #attach(df)
        
        #print(state)
        #print(outcome)
        bestanswer<- character()
        besthospitalname<- character()
        
        
        
        #datatest1<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        #is.na(datatest1)<- datatest1=="Not Available"
        
        ## check that state and outcome is valid
        
        ##if ((sum(state==levels(df$State))==0) && (sum(outcome==c("heart attack", "heart failure", "pneumonia"))==0) ) { paste("Error in best (",state, outcome, ") : invalid state and outcome", sep = " ")}
        ##else if (sum(state==levels(df$State))==0) { paste("Error in best (",state, outcome, ") : invalid state", sep = " ")} 
        ##else if (sum(outcome==c("heart attack", "heart failure", "pneumonia"))==0) { paste("Error in best (",state, outcome, ") : invalid outcome", sep = " ")}
        ##else if ((sum(state==levels(df$State))!=0) && (sum(outcome==c("heart attack", "heart failure", "pneumonia"))!=0) ){ 
                
                if (outcome=="heart attack") {
                        ##print(df$State)
                        arrangeddf <- arrange(df,df$State,df$`Heart Attack Death`, df$`Hospital Name`)
                        splitdf<- split(arrangeddf, arrangeddf$State)
                        orderedhospitals<- sapply(splitdf, function(x) x[[1]])
                        besthospitals<- lapply(orderedhospitals, function(L) L[[1]])
                        besthospitals
                        #print(state)
                        #print(outcome)
                        # bestanswer<- besthospitals$state
                        #print(bestanswer)
                        #bestanswer
                }
                
                if (outcome=="heart failure") {
                        
                        arrangeddf <- arrange(df,df$State,df$`Heart Failure Death`, df$`Hospital Name`)
                        splitdf<- split(arrangeddf, arrangeddf$State)
                        orderedhospitals<- sapply(splitdf, function(x) x[[1]])
                        besthospitals<- lapply(orderedhospitals, function(L) L[[1]])
                        bestanswer<- besthospitals$state
                        bestanswer
                }
                
                if (outcome=="pneumonia") {
                        
                        arrangeddf <- arrange(df,df$State,df$`Pneumonia Death`, df$`Hospital Name`)
                        splitdf<- split(arrangeddf, arrangeddf$State)
                        orderedhospitals<- sapply(splitdf, function(x) x[[1]])
                        besthospitals<- lapply(orderedhospitals, function(L) L[[1]])
                        #bestanswer<- besthospitals$state
                        besthospitals
                }
               # return(bestanswer)
        #}
        
        ### besthospitalsunlisted<- unlist(besthospitals)
       ### bestanswer<- besthospitals-unlisted[state]
        #### assign("bestanswerGlobal", bestanswer, envir = .GlobalEnv)
        #### bestanswerGlobal
        
        assign("besthospitalsGlobal", besthospitals, envir = .GlobalEnv)
        besthospitalsunlisted<- unlist(besthospitalsGlobal)
        bestanswer<- besthospitalsunlisted[state]
        assign("bestanswerGlobal", bestanswer, envir = .GlobalEnv)
        bestanswerGlobal
        #return(besthospitalname)
        #print(class(besthospitals))
        
        #besthospitals
        ## Return hospital name in that state with lowest 30-day death
        ## rate   
        #besthospitals
        #assign("besthospitalname", bestanswer, envir = .GlobalEnv)
        #return(besthospitalname)
}