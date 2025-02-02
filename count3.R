##4
rm(list=ls())

##4
rm(list=ls())

rankall<-function(Outcome="heart attack",num="best"){
        
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
                mutate(rank=row_number(),
                       no_hspt=last(rank),#total number of hospitals- retain last value of rank"
                       rank_c=case_when(rank==1                ~"best",
                                        rank==no_hspt & rank>1 ~"worst",
                                        TRUE                   ~as.character(rank)
                       )
                ) 
        #If you filter outcome1 first based on 'num', it might return 0 records in case num value
        #provided is not in the dataset. Thus the subsequent check criteria may fail.
        
        #Check for digits only- \\d+
        if (regexpr("\\d+",num)>= 1){
                if (as.numeric(num) > max(outcome1$no_hspt)) {return(NA)}
                else {  outcome2<-outcome1 %>% filter(rank_c==num)
                        full<-expand.grid(state=unique(outcome1$state),`hospitalname_`=NA_character_)
                        full1<-left_join(full,outcome2,by="state") %>% 
                                mutate(hospitalname=ifelse(!is.na(`hospital name`),`hospital name`, hospitalname_ ))  %>% 
                               select (`hospitalname`,state)     
                return(full1) 
                }
        }
        
        #Non-numeric--"Best" and "Worst"
        else {
                outcome3<-outcome1 %>% filter(rank_c==num)
                full<-expand.grid(state=unique(outcome1$state),`hospitalname_`=NA_character_)
                full1<-left_join(full,outcome3,by="state") %>% 
                        mutate(hospitalname=ifelse(!is.na(`hospital name`),`hospital name`, hospitalname_ ))  %>% 
                        select (`hospitalname`,state)     
                return(full1) 
        }
        
}


rankall("heart attack","20")

tail(rankall("pneumonia","worst"),3)

tail(rankall("heart failure"),10)

