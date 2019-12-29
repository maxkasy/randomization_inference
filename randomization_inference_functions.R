# function taking full data and index of available outcomes to get assignment probabilities
Pt_simulated=function(full_data, outcome_available_units, current_units){
    Pstar=DtchoiceMCMCProbabilities(full_data$Y,full_data$D,full_data$X, #outcomes, treatments, and covariates thus far
                                    full_data$k,full_data$nx, #number of treatments and number of strata
                                    RR=RR)
    Pactual=(1-alpha) * Pstar + alpha * (1/full_data$k)
}