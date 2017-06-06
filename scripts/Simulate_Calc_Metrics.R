#####################################################
# This script runs through our various simulated
# corpora and calculates R2, LL, and McFadden's R2
#####################################################

rm(list=ls())

library(idaTopicModels)

####################################################
# Metric Function
####################################################
MetricFun <- function(phi, theta, dtm, parallel, cpus){
    K <- nrow(phi)
    D <- nrow(dtm)
    V <- ncol(dtm)
    len <- Matrix::rowSums(dtm)
    len <- mean(len)
    
    theta <- theta / rowSums(theta)
    phi <- phi / rowSums(phi)
    
    # these are for the "no model" ll in McFadden's
    phi_raw <- Matrix::colSums(dtm) + 1 / ncol(dtm)
    phi_raw <- phi_raw / sum(phi_raw)
    phi_raw <- rbind(phi_raw, phi_raw) 
    rownames(phi_raw) <- c("t_1", "t_2")
    colnames(phi_raw) <- colnames(dtm)
    
    theta_raw <- rep(0.5, nrow(dtm))
    theta_raw <- cbind(theta_raw, theta_raw)
    rownames(theta_raw) <- rownames(dtm)
    colnames(theta_raw) <- rownames(phi_raw)
    
    ll_model <- CalcLikelihood(dtm=dtm, phi=phi, theta=theta, parallel=parallel, cpus=cpus)
    ll_raw <- CalcLikelihood(dtm=dtm, phi=phi_raw, theta=theta_raw, parallel=parallel, cpus=cpus)
    r2 <- CalcTopicModelR2(dtm=dtm, phi=phi, theta=theta, parallel=parallel, cpus=cpus)
    r2_mac <- 1 - ll_model/ll_raw
    
    result <- data.frame(K=K, D=D, V=V, len=len, ll_model=ll_model, ll_raw=ll_raw, r2=r2, r2_mac=r2_mac, stringsAsFactors=FALSE)
    return(result)
}



####################################################
# Various K
####################################################
load("output/Simulated_Vary_K.RData")

for(j in 1:length(k_par_list)){
    k_par_list[[ j ]]$dtm <- k_dtm_list[[ j ]]
}

sfInit(parallel=T, cpus=4)
sfExport("MetricFun")
sfLibrary(idaTopicModels)

k_metrics <- sfLapply(k_par_list, function(x){
    MetricFun(phi = x$phi, theta = x$theta, dtm = x$dtm, parallel = FALSE, cpus = NULL)
})

sfStop()

k_metrics <- do.call(rbind, k_metrics)

k_metrics <- k_metrics[ order(k_metrics$K) , ]

rm(k_dtm_list, k_par_list)
gc()

####################################################
# Various lambda
####################################################
load("output/Simulated_Vary_lambda.RData")

sfInit(parallel = T, cpus = 4)
sfExport(list=c("MetricFun", "lambda_par"))
sfLibrary(idaTopicModels)

lambda_metrics <- sfLapply(lambda_dtm_list, function(x){
    MetricFun(phi = lambda_par$phi, theta = lambda_par$theta, dtm = x, parallel = F, cpus = NULL)
})

sfStop()

lambda_metrics <- do.call(rbind, lambda_metrics)

lambda_metrics <- lambda_metrics[ order(lambda_metrics$K) , ]

rm(lambda_dtm_list, lambda_par)
gc()

#####################################################
# Various V
#####################################################
load("output/Simulated_Vary_V.RData")

for(j in 1:length(v_par_list)){
    v_par_list[[ j ]]$dtm <- v_dtm_list[[ j ]]
}

sfInit(parallel=T, cpus=4)
sfExport("MetricFun")
sfLibrary(idaTopicModels)

v_metrics <- sfLapply(v_par_list, function(x){
    MetricFun(phi = x$phi, theta = x$theta, dtm = x$dtm, parallel = FALSE, cpus = NULL)
})

sfStop()

v_metrics <- do.call(rbind, v_metrics)

v_metrics <- v_metrics[ order(v_metrics$K) , ]

rm(v_dtm_list, v_par_list)
gc()

#####################################################
# Various D
#####################################################
load("output/Simulated_Vary_D.RData")

d_stacked <- list(d_1000=list(dtm=d_dtm_list[[ 1 ]], theta=d_theta_list[[ 1 ]]),
                  d_2000=list(dtm=do.call(rBind, d_dtm_list[ 1:2 ]), theta=do.call(rbind, d_theta_list[ 1:2 ]) ),
                  d_5000=list(dtm=do.call(rBind, d_dtm_list[ 1:5 ]), theta=do.call(rbind, d_theta_list[ 1:5 ])),
                  d_10000=list(dtm=do.call(rBind, d_dtm_list), theta=do.call(rbind, d_theta_list))
                  )

d_metrics <- lapply(d_stacked, function(d){
    MetricFun(phi = d_phi, theta = d$theta, dtm = d$dtm, parallel = T, cpus = 4)
})

d_metrics <- do.call(rbind, d_metrics)

d_metrics <- d_metrics[ order(d_metrics$K) , ]

rm(d_stacked, d_phi, d_theta_list, d_dtm_list)
gc()

#####################################################
# Combine and Save
#####################################################
simulated_metrics <- rbind(d_metrics, k_metrics, lambda_metrics, v_metrics)

save(d_metrics, k_metrics, lambda_metrics, v_metrics, simulated_metrics, file="output/SimulatedMetrics.RData")



