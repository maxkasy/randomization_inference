library(tidyverse)
library(magrittr)

ReadData=function(filepath, key=NULL, k=4, ncovs=1){ #key to consistently create strata
  Data=read_csv(filepath)
  varnames=names(Data)

  Data %<>% drop_na %>% #drop rows with missing entries
    filter(intake_interview_date > as.Date("2019-01-01")) # drop rows with 2018 dates
 
    
  treatDummies=paste("treatment",1:k, sep="") #names of treatment variables
  strataVars=paste("covar",1:ncovs, sep="") #names of control variables
  
  Data %<>% mutate(Strata=(interaction(select(.,strataVars)))) #create strata
  #recoding the strata levels in a reproducible way across datasets 
  oldlevels=levels(Data$Strata) 
  key=data_frame(Strata=oldlevels, strata=factor(1:length(oldlevels)))
  Data %<>% left_join(., key, by = "Strata") %>%
    select(-Strata)
  
  #create factor variable from treatment
  Data %<>% mutate(treatment=factor(as.matrix(Data[treatDummies])%*%(1:k), levels=1:k))
  
  
  tibble(Y=Data$outcome, 
         D=Data$treatment,
         X=Data$strata,
         date = Data$intake_interview_date,
         ID=Data$X1,
         observed=as.Date(NA))
  
}


merge_observed_date = function(start_date="2019-04-07", end_date="2019-12-26") {
  nx=16
  key=tibble(Strata=factor(1:nx),
             strata=factor(1:nx))
  k=4
  
  # Read full data from last available file
  datapath = paste("../reproducible_analysis/jordan_current_data/",
                   end_date, "_priordata.csv", sep="")
  full_data=ReadData(datapath,key)

  # Iterate over all prior dates to get units with outcomes available at each date
  for (date in as.list(seq.Date(from=as.Date(start_date), to=as.Date(end_date),"days"))) {
    datapath = paste("../reproducible_analysis/jordan_current_data/",
                     date, "_priordata.csv", sep="")
    
    if (file.exists(datapath)) {
      current_observations=read_csv(datapath) %>% 
        mutate(ID=X1,
               observed_current=date) %>% 
        select(ID, observed_current)
      
      # replace ovserved variable with current date for those that have not been observed yet
      full_data %<>% left_join(current_observations) 
      
      full_data[is.na(full_data$observed),] %<>%
        mutate(observed=observed_current) 
      
      full_data %<>% 
        select(-observed_current)
    }
    
  }
  
  write_csv(full_data, "merged_full_data.csv")
}
