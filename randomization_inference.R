library(furrr)

source("ReadData.R")
source("MCMC_HierarchicalThompson.R")

# parameters of the algorithm
alpha = 0.2
nx = 16
k = 4

# number of MCMC draws
RR = 10000 #50000
# number of randomization inference replicates
replicates = 128

# merge in dates of first observation, and store merged_full_data.csv
# merge_observed_date("2019-04-07", "2020-01-03")

full_data =  read_csv("merged_full_data.csv") %>%
    mutate(D_simulated = factor(NA, levels = 1:k))


start_date = "2019-02-10"
end_date = "2020-01-03"


simulate_treatment = function(replication) {
    for (current_date in as.list(seq.Date(from = as.Date(start_date),
                                          to = as.Date(end_date), "days"))) {
        # print(current_date)
        
        prior_data = full_data %>%
            filter(observed < current_date)
        
        if (nrow(prior_data) > 0) {
            Pstar = DtchoiceMCMCProbabilities(prior_data$Y,
                                              prior_data$D_simulated,
                                              prior_data$X,
                                              #outcomes, treatments, and covariates thus far
                                              k, nx, #number of treatments and number of strata
                                              RR = RR)
            Pactual = (1 - alpha) * Pstar + alpha / k
        } else {
            Pactual = matrix(1 / k, nx, k)
        }
        
        current_X = full_data[full_data$date == current_date, "X"]
        current_D_simulated = map_int(as.integer(current_X[[1]]),
                    function(x) sample(1:k, size = 1, prob = Pactual[x, ])) %>% 
                factor(levels = 1:k)

        full_data[full_data$date == current_date, ] %<>%
            mutate(D_simulated = current_D_simulated)
        
    }
    write_csv(full_data,
              paste("simulated_D/simulated_D_", replication, ".csv", sep = ""))
}


# parallelize simulations
plan(multiprocess)

# each replication generates one simulated treatment assignment and saves it in a separate file
future_map(1:replicates,
    simulate_treatment)

