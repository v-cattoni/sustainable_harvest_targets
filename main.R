#### INSTALLING PACKAGES ####
# install.packages(c("bridgesampling", "coda", "rstan", "lbfgsb3c", "loo"))
# library(bridgesampling)
# library(coda)
# library(rstan)
# library(lbfgsb3c)
# library(loo)

#### READING IN FILES ####
load("./data_files/RAMCore[asmt][v4.495].rdata") #ram database
cols <- read.csv("./data_files/non_det_cols.csv", header <- FALSE)[, 1] #non-deterministic data sets
   
#### CONSTANTS ####
num_rows <- 
  length(Bio[,1]) #number of years
num_cols <- 
  length(cols) #number of non-deterministic data sets

#### INITIALISING ####
bio_mat <-
  matrix(NA, num_rows, num_cols) #biomass (year, species)
catch_mat <-
  matrix(NA, num_rows, num_cols) #catch(MT) (year, species)

bh_data <- c()
rk_data <- c()
pt_data <- c()
hs_data <- c()

bh_ml <- c()
rk_ml <- c()
pt_ml <- c()
hs_ml <- c()

#### SORTING DATA ####
#Creates a matrix of Biomass data and a matrix of Catch data with only overlapping entries.
for (i in cols) {
  for (j in 1:num_rows) {
    if (FALSE %in% is.na(Bio[, i][j]) &
        FALSE %in% is.na(MCatch[, i][j])) {
      bio_mat[j, match(i, cols)] <- c(Bio[, i][j])
      catch_mat[j, match(i, cols)] <-
        c(MCatch[, i][j])
    }
  }
}

#### MAIN ####
for (i in 1:num_cols){
  print(i)
  bios <- bio_mat[, i][!is.na(bio_mat[, i])] 
  catchs <- catch_mat[, i][!is.na(catch_mat[, i])]
  
  #data
  Ei <- bios[1:(length(bios) - 1)] - catchs[1:(length(bios) - 1)] #Escapement = biomass - catch in year i
  Ri <- bios[2:length(bios)] #Recruitment = biomass in year t+1
  
  warmups <- 100
  
  total_iterations <- 200
  
  max_treedepth <-  30
  
  n_chains <-  4
  
  adapt_delta <- 0.999
  
  data <- list(n = length(Ei),
               Ri = Ri,
               Ei = Ei,
               max_r = max(Ri)
  )
  
  #### BEVERTON-HOLT ####
  bh_fit <- stan(
   file = "./models/bh_model.stan",
    data = data,
    chains = n_chains,
    warmup = warmups,
    thin = 3,
    iter = total_iterations,
    control = list(max_treedepth = max_treedepth,
                   adapt_delta = adapt_delta)
  )
  bh_data <- append(bh_data, bh_fit)
  bh_ml <- append(bh_ml, exp(bridge_sampler(bh_fit)$logml))

  
  #### RICKER ####
  rk_fit <- stan(
    file = "./models/rk_model.stan",
    data = data,
    chains = n_chains,
    warmup = warmups,
    thin = 3,
    iter = total_iterations,
    control = list(max_treedepth = max_treedepth,
                   adapt_delta = adapt_delta)
  )
  rk_data <- append(rk_data, rk_fit)
  rk_ml <- append(rk_ml, exp(bridge_sampler(rk_fit)$logml))

  #### PELLA-TOMLINSON ####
  pt_fit <- stan(
    file = "./models/pt_model.stan",
    data = data,
    chains = n_chains,
    warmup = warmups,
    thin = 3,
    iter = total_iterations,
    control = list(max_treedepth = max_treedepth,
                   adapt_delta = adapt_delta)
  )
  pt_data <- append(pt_data, pt_fit)
  pt_ml <- append(pt_ml, exp(bridge_sampler(pt_fit)$logml))
  
  #### HOCKEY-STICK ####
  hs_fit <- stan(
    file = "./models/hs_model.stan",
    data = data,
    chains = n_chains,
    warmup = warmups,
    thin = 3,
    iter = total_iterations,
    control = list(max_treedepth = max_treedepth,
                   adapt_delta = adapt_delta)
  )
  hs_data <- append(hs_data, hs_fit)
  hs_ml <- append(hs_ml, exp(bridge_sampler(hs_fit)$logml))
}

save(bh_data, file = "./mcmc_data/bh_data.RData")
save(rk_data, file = "./mcmc_data/rk_data.RData")
save(pt_data, file = "./mcmc_data/pt_data.RData")
save(hs_data, file = "./mcmc_data/hs_data.RData")

save(bh_ml, file = "./mcmc_data/bh_ml.RData")
save(rk_ml, file = "./mcmc_data/rk_ml.RData")
save(pt_ml, file = "./mcmc_data/pt_ml.RData")
save(hs_ml, file = "./mcmc_data/hs_ml.RData")


