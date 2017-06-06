##########################################################
# Functions to generate a corpus with varying properties
###########################################################

#### Function to simulate phi and theta parameters
SimulatePar <- function(D, V, K, cpus=4){
    library("LearnBayes")
    library("snowfall")
    
    alpha <- rep(5/K, K)
    
    Zipf <- function(V) 1/(1:V * log(1.78 * V) ) # approximate empirical zipf distribution http://mathworld.wolfram.com/ZipfsLaw.html
    
    zipf.law <- Zipf(V=V)
    
    zipf.law <- zipf.law / sum(zipf.law) # normalize to sum to one (rounding error makes this slightly larger than one)
    
    beta <- zipf.law
    
    # phi
    phi <- sapply(1:K, function(j){
        result <- sample(x = beta, size = V, replace = F)
        names(result) <- paste("w", 1:V, sep="_")
        return(result)
    })
    
    phi <- t(phi)
    
    rownames(phi) <- paste("t", 1:K, sep="_")
    
    # theta
    sfInit(parallel = T, cpus = cpus)
    sfExport(list=c("alpha", "K"))
    sfLibrary(LearnBayes)
    
    theta <- sfSapply(1:D, function(j){
        result <- as.numeric(rdirichlet(n = 1, par = alpha))
        names(result) <- paste("t", 1:K, sep="_")
        return(result)
    })
    
    sfStop()
    
    theta <- t(theta)
    rownames(theta) <- paste("d", 1:D, sep="_")
    
    return(list(theta=theta, phi=phi))
}

# Function to sample from phi and theta to construct a dtm
SampleDocs <- function(phi, theta, lambda, cpus=4){
    library("Matrix")
    library("snowfall")
    
    D <- nrow(theta)
    K <- ncol(theta)
    V <- ncol(phi)
    
    sfInit(parallel=T, cpus=cpus)
    sfExport(list=c("phi", "D", "V", "K", "lambda"))
    sfLibrary(Matrix)
    
    dtm <- sfApply(theta, 1, function(d){
        n_d <- rpois(n = 1, lambda = lambda)
        
        result <-rep(0, V)
        
        for( j in 1:n_d){
            topic <- as.numeric(rmultinom(n = 1, size = 1, prob = d))
            word <- as.numeric(rmultinom(n = 1, size = 1, prob = phi[ topic == 1 , ]))
            result <- result + word
        }
        
        result <- Matrix(result, nrow = 1, sparse = T)
        colnames(result) <- colnames(phi)
        
        return(result)
    })
    
    sfStop()
            
    # if you have a lot of documents, combine them in batches
    if( length(dtm) > 500 ){
        partitions <- length(dtm) / 500 # do in batches of about 500
        
        partitions <- round(partitions)
        
        breaks <- round(length(dtm)/partitions)
        
        indeces <- seq(from = 1, to = length(dtm), by = breaks)
        
        sfInit(parallel=T, cpus=min(cpus, partitions))
        sfExport("dtm")
        sfLibrary(Matrix)
        
        dtm <- sfLapply(indeces, function(j){
            
            do.call(rBind, dtm[ j:min(j + breaks - 1, length(dtm)) ])
            
        })
        
        sfStop()
    }
    
    dtm <- do.call(rBind, dtm)
    
    rownames(dtm) <- rownames(theta)
    
    return(dtm)
}

# function generates a theta for a given phi
AddDocs <- function(K, D, cpus=4){
    library("snowfall")
    library("LearnBayes")
    
    alpha <- rep(5 / K, K)
    
    sfInit(parallel = T, cpus = cpus)
    sfExport(list=c("alpha", "K"))
    sfLibrary(LearnBayes)
    
    theta <- sfSapply(1:D, function(j){
        result <- as.numeric(rdirichlet(n = 1, par = alpha))
        names(result) <- paste("t", 1:K, sep="_")
        return(result)
    })
    
    sfStop()
    
    theta <- t(theta)
    rownames(theta) <- paste("d", 1:D, sep="_")
    
    return(theta)
}