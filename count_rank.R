# ==============================================================================
# Script Name: bestranking.R
# Author     : Dominic Mutiso Muia
# Date       : 28-07-2024
# Description: This script performs data manipulation and analysis using data
#              from the Hospital Compare website run by the U.S Department of
#              Health and Human Services. It includes user-defined functions
#              to identify the best hospitals for specific outcomes in each
#              state, and rank hospitals by outcome.
# ==============================================================================

#Clear workspace and set the working directory
rm(list=ls())
getwd()
setwd("C:/Users/Dominic/Desktop/coursera/rprog_data_ProgAssignment3-data")
library(dplyr)


# ==============================================================================
# Plot the 30-day mortality rates for heart attack
# ==============================================================================

##1.
outcome_<-read.csv("outcome-of-care-measures.csv", colClasses = "character")

names(outcome_)

outcome_[,11]<-as.numeric(outcome_[,11])

hist(outcome_[,11])


# ==============================================================================
# Finding the best hospital in a state
# ==============================================================================

best<-function(State="AK",Outcome="heart attack"){
        
        #Read outcome data
        outcome_<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        #Check if State is valid
        if (!State %in% unique(outcome_$State) )  {
                stop("Invalid state")
        } 
        
        #Check if Outcome is valid
        if (!Outcome %in% c("heart attack","heart failure","pneumonia")) {
                stop("Invalid outcome")
        }
        
        #Data Manipulation
        outcome1<-outcome_ %>% select("Hospital.Name","State",starts_with("Hospital.30.Day")) %>% 
                rename_with(~gsub("^Hospital.30(\\.+\\w+)(\\.+\\w+)\\.\\.(Mortality)\\.\\.Rates\\.from\\.","",.x)) %>%
                rename_with(~tolower(gsub("\\."," ",.x))) %>%
                mutate(
                        outcome = suppressWarnings(as.numeric(!!sym(Outcome)))
                      ) %>% 
                arrange(state, outcome, `hospital name`) %>% 
                group_by(state) %>% 
                slice(1) %>%
                filter(!is.na(`hospital name`) & !is.na(outcome) & state==State)
        
        return(outcome1[,1])
}

best()

best("NY","pneumonia")

best("AK","pneumonia")

best("SC","heart attack")


# ==============================================================================
# Ranking Hospitals by Outcome in a state
# ==============================================================================

rm(list=ls())

rankhospital<-function(State="AK",Outcome="heart attack",num="best"){
        
        #Read outcome data
        outcome_<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        #Check if State is valid
        if (!State %in% unique(outcome_$State) )  {
                stop("Invalid State")
        } 
        
        #Check if Outcome is valid
        if (!Outcome %in% c("heart attack","heart failure","pneumonia")) {
                stop("Invalid outcome")
        }
        
        #Data Manipulation
        outcome1<-outcome_ %>% select("Hospital.Name","State",starts_with("Hospital.30.Day")) %>% 
                rename_with(~gsub("^Hospital.30(\\.+\\w+)(\\.+\\w+)\\.\\.(Mortality)\\.\\.Rates\\.from\\.","",.x)) %>%
                rename_with(~tolower(gsub("\\."," ",.x))) %>% 
                mutate(
                        outcome = suppressWarnings(as.numeric(!!sym(Outcome)))
                ) %>% 
                filter(!is.na(`hospital name`) & !is.na(outcome) & state == State)%>%
                arrange(state, outcome, `hospital name`) %>% 
                group_by(state) %>% 
                mutate(rank = row_number(),
                       no_hspt = last(rank),#total number of hospitals- retain last value of rank"
                       rank_c = case_when(rank == 1                  ~"best",
                                          rank == no_hspt & rank>1   ~"worst",
                                          TRUE                       ~as.character(rank)
                       )
                ) 
        #If you filter outcome1 first based on 'num', it might return 0 records incase num value
        #provided is not in the dataset. Thus the subsequent check criteria may fail.
        
        #Check for digits only- \\d+
        if (regexpr("\\d+",num)>= 1){
                if (as.numeric(num) > max(outcome1$no_hspt)) {return(NA)}
                else {  outcome2<-outcome1 %>% filter(rank_c == num)
                return(outcome2[1,1])
                }
        }
        
        #Non-numeric--"Best" and "Worst"
        else {
                outcome2<-outcome1 %>% filter(rank_c == num)
                return(outcome2[1,1])
        }
        
}

rankhospital("NC", "heart attack","worst")

rankhospital("WA","heart attack", "7")

rankhospital("TX","pneumonia", "10")

rankhospital("NY","heart attack", "7")

# ==============================================================================
# Ranking Hospitals in all states
# ==============================================================================

rm(list=ls())

rankall<-function(Outcome = "heart attack",num = "best"){
        
        #Read outcome data
        outcome_<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        
        #Check if Outcome is valid
        if (!Outcome %in% c("heart attack","heart failure","pneumonia")) {
                stop("Invalid outcome")
        }
        
        #Data Manipulation
        outcome1<-outcome_ %>% select("Hospital.Name","State",starts_with("Hospital.30.Day")) %>% 
                rename_with(~gsub("^Hospital.30(\\.+\\w+)(\\.+\\w+)\\.\\.(Mortality)\\.\\.Rates\\.from\\.","",.x)) %>%
                rename_with(~tolower(gsub("\\."," ",.x))) %>% 
                mutate(
                        outcome = suppressWarnings(as.numeric(!!sym(Outcome)))
                ) %>% 
                filter(!is.na(`hospital name`) & !is.na(outcome) )%>%
                arrange(state, outcome, `hospital name`) %>% 
                group_by(state) %>% 
                mutate(rank = row_number(),
                       no_hspt = last(rank),#total number of hospitals- retain last value of rank"
                       rank_c = case_when(rank == 1                 ~"best",
                                          rank == no_hspt & rank>1  ~"worst",
                                          TRUE                      ~as.character(rank)
                       )
                ) 
        #If you filter outcome1 first based on 'num', it might return 0 records in case num value
        #provided is not in the dataset. Thus the subsequent check criteria may fail.
        
        #Check for digits only- \\d+
        if (regexpr("\\d+",num)>= 1){
                if (as.numeric(num) > max(outcome1$no_hspt)) {return(NA)}
                else {  outcome2<-outcome1 %>% filter(rank_c == num)
                full<-expand.grid(state = unique(outcome1$state),`hospitalname_` = NA_character_)
                full1<-left_join(full,outcome2, by = "state") %>% 
                        mutate(hospitalname = ifelse(!is.na(`hospital name`),`hospital name`, hospitalname_ ))  %>% 
                        select (`hospitalname`,state)     
                return(full1) 
                }
        }
        
        #Non-numeric--"Best" and "Worst"
        else {
                outcome3<-outcome1 %>% filter(rank_c == num)
                full<-expand.grid(state = unique(outcome1$state),`hospitalname_` = NA_character_)
                full1<-left_join(full,outcome3, by = "state") %>% 
                        mutate(hospitalname = ifelse(!is.na(`hospital name`),`hospital name`, hospitalname_ ))  %>% 
                        select (`hospitalname`,state)     
                return(full1) 
        }
        
}


r<-rankall("heart attack","4")
as.character(subset(r,state == "HI")$hospitalname)

r<-rankall("pneumonia","worst")
as.character(subset(r,state == "NJ")$hospitalname)

r<-rankall("heart failure",10)
as.character(subset(r,state == "NV")$hospitalname)


# ==============================================================================
# End of script
# ==============================================================================
