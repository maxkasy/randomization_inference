library(tidyverse)
library(magrittr)

# Applying arbitrary estimator to all replicates ----  
full_data =  read_csv("merged_full_data.csv") %>% 
    mutate(D_simulated=D)
replicates = 1:16

# function that applies estimator to each of the replicate datasets corresponding to indices
list_of_estimates = function(estimator, replicates.=replicates) {
    map(replicates.,
        function(index) 
            paste("simulated_D/simulated_D_", index, ".csv", sep = "") %>% 
            read_csv() %>% 
            estimator()
    )    
}

# function to calculate p-values for all rows and chosen column variable
p_values = function(estimator, column_name) {
    estimates = full_data %>% 
        estimator() %>% 
        mutate(p_value = NA)
    replicate_estimates = estimator %>% 
        list_of_estimates()
    
    estimate_column = estimates[[column_name]]
    estimate_less_than_replicate = map(replicate_estimates, column_name) %>% 
        map(function(replicate_column) replicate_column < estimate_column)
    
    estimates$p_value = do.call(cbind, estimate_less_than_replicate) %>% 
        rowMeans()

    return(estimates)
}




# Weighted means ----

treatment_names = c("cash", "information", "psychological", "control")
strata_codes = read_csv("strata_codes.csv") 
strata_names = paste(ifelse(strata_codes$nationality =="syrian", "Syr", "Jor"),
                     ifelse(strata_codes$gender =="male", "M", "F"),
                     ifelse(strata_codes$above_secondary_edu ==1, ">= HS", "< HS"),
                     ifelse(strata_codes$ever_employed ==1, "ever emp", "never emp"),
                     sep = ",")
strata_codes = strata_codes %>%
    mutate(stratum=factor(stratum, labels=strata_names))


# define estimators that take replicate datasets as input
# Starting with weighted means (as in check-up dashboard)
# These are IPW estimates
weighted_means = function(data = full_data) {
    success_rates = data %>% 
        mutate(treatment=factor(D_simulated, labels=treatment_names),
               stratum=factor(X, labels=strata_names)) %>% 
        left_join(strata_codes, by="stratum") %>% 
        # select(-c(D,X, D_simulated)) %>% 
        group_by(stratum, nationality, gender, above_secondary_edu, ever_employed, treatment) %>% 
        summarise(avg=mean(Y), count=n()) %>% 
        ungroup()
    
    pop_numbers = success_rates %>% 
        group_by(stratum) %>% 
        summarise(pop_count=sum(count))
    
    success_rates= success_rates %>% 
        left_join(pop_numbers, by="stratum")
    
    # now do various groupings
    
    success_by_gender=success_rates %>% 
        group_by(gender, treatment) %>% 
        summarise(success_rate=weighted.mean(avg, pop_count)) %>% 
        ungroup()
    
    success_by_nationality=success_rates %>% 
        group_by(nationality, treatment) %>% 
        summarise(success_rate=weighted.mean(avg, pop_count)) %>% 
        ungroup() 
    
    success_by_education=success_rates %>% 
        group_by(above_secondary_edu, treatment) %>% 
        summarise(success_rate=weighted.mean(avg, pop_count)) %>% 
        ungroup()
    
    success_by_experience=success_rates %>% 
        group_by(ever_employed, treatment) %>% 
        summarise(success_rate=weighted.mean(avg, pop_count))  %>% 
        ungroup()
    
    list(gender = success_by_gender, 
         nationality = success_by_nationality, 
         education = success_by_education, 
         experience = success_by_experience) %>% 
        bind_rows(.id = "grouping_variable")
}

# Calculate p values, for weighted means
p_values_means = p_values(weighted_means, "success_rate")

# note that this does not give confidence sets & doesn't make much sense as is
# need to pick hypotheses to test & calculate corresponding p-values



