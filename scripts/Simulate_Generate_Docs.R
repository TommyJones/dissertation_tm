######################################################################
# This script generates several simulated corpora varying certain
# input parameters. 
######################################################################

rm(list=ls())

source("scripts/SimulateFunction.R")

######################################################################
# Define my parameters here
######################################################################

# Default parameters
K <- 50
D <- 2000
V <- 5000
lambda <- 500

# lists of parameters to vary
k_list <- c(20, 50, 100, 200)
names(k_list) <- paste("k", k_list, sep="_")

# d_list <- c(1000, 2000, 5000, 10000)
# names(d_list) <- paste("d", d_list, sep="_")

v_list <- c(2000, 5000, 10000, 25000)
names(v_list) <- paste("v", v_list, sep="_")

lambda_list <- c(50, 100, 250, 500, 1000)
names(lambda_list) <- paste("l", lambda_list, sep="_")

######################################################################
# Vary K
######################################################################
k_par_list <- lapply(k_list, function(k){
    SimulatePar(D = D, V = V, K = k, cpus = 4)
})

k_dtm_list <- lapply(k_par_list, function(par){
    SampleDocs(phi = par$phi, theta = par$theta, lambda = lambda, cpus = 4)
})

save(k_par_list, k_dtm_list, file="output/Simulated_Vary_K.RData")

rm(k_par_list, k_dtm_list)
gc()

######################################################################
# Vary lambda
######################################################################
lambda_par <- SimulatePar(D = D, V = V, K = K, cpus = 4) 

lambda_dtm_list <- lapply(lambda_list, function(l){
    SampleDocs(phi = lambda_par$phi, theta = lambda_par$theta, lambda = l, cpus = 4)
})

save(lambda_par, lambda_dtm_list, file="output/Simulated_Vary_lambda.RData")

rm(lambda_par, lambda_dtm_list)
gc()

######################################################################
# Vary V
######################################################################
v_par_list <- lapply(v_list, function(v){
    SimulatePar(D = D, V = v, K = K, cpus = 4)
})

v_dtm_list <- lapply(v_par_list, function(par){
    SampleDocs(phi = par$phi, theta = par$theta, lambda = lambda, cpus = 4)
})

save(v_par_list, v_dtm_list, file="output/Simulated_Vary_V.RData")

rm(v_par_list, v_dtm_list)
gc()

######################################################################
# Vary D
######################################################################
d_theta_list <- lapply(1:10, function(j){
    AddDocs(K = K, D = 1000, cpus=4)
})

for(j in 1:length(d_theta_list)){
    rownames(d_theta_list[[ j ]]) <- paste( rownames(d_theta_list[[ j ]]), j, sep="_" )
}

d_phi <- SimulatePar(D = 2, V = V, K = K, cpus = 1)$phi

d_dtm_list <- lapply(d_theta_list, function(theta){
    SampleDocs(phi = d_phi, theta = theta, lambda = lambda, cpus = 4)
})

save(d_theta_list, d_phi, d_dtm_list, file="output/Simulated_Vary_D.RData")

rm(d_theta_list, d_dtm_list, d_phi)
gc()




