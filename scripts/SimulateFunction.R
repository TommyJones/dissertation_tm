##########################################################
# Functions to generate a corpus with varying properties
###########################################################

### Function to simulate phi and theta parameters ------------------------------
SimulatePar <- function(D, V, K, alpha = NULL, beta = NULL, cpus=4){

  
    if (is.null(alpha)) {
      
      alpha <- rep(5/K, K)
      
    }
    
  if (is.null(beta)) {
    Zipf <- function(V) 1/(1:V * log(1.78 * V) ) # approximate empirical zipf distribution http://mathworld.wolfram.com/ZipfsLaw.html
    
    zipf.law <- Zipf(V=V)
    
    zipf.law <- zipf.law / sum(zipf.law) # normalize to sum to one (rounding error makes this slightly larger than one)
    
    beta <- zipf.law / min(zipf.law) # ensures minimum entry is one. # must fix to control variability
    
    
  }
    
    # phi
    
    # Use dirichlet, not multinomial. Variability of words within documents is
    # controlled by scale of beta. If smallest entries of beta are tiny, then
    # you have a "small sample" and zipf's law will not hold
    phi <- gtools::rdirichlet(n = K, alpha = beta)
    
    
    rownames(phi) <- paste("t", 1:K, sep="_")
    
    if (is.null(names(beta))){
      colnames(phi) <- paste("w", 1:V, sep = "_")
    } else {
      colnames(phi) <- names(beta)
    }
    
    # theta
    
    theta <- gtools::rdirichlet(n = D, alpha = alpha)
    
    # theta <- t(theta)
    rownames(theta) <- paste("d", 1:D, sep="_")
    colnames(theta) <- rownames(phi)
    
    return(list(theta=theta, phi=phi))
}

### Function to sample from phi and theta to construct a dtm -------------------
SampleDocs <- function(phi, theta, lambda = NULL, cpus = 4){

    D <- nrow(theta)
    K <- ncol(theta)
    V <- ncol(phi)
    
    # vector of document lengths
    if ( ! is.null(lambda) & length(lambda) == 1) {
      
      doc_lengths <- rpois(n = D, lambda = lambda) # document length / number of samples
      
    } else if (! is.null(lambda) & length(lambda) > 1) {
      doc_lengths <- lambda
    } else {
      
      warning("ill-defined lambda specified, choosing lambda = 300")
      
      doc_lengths <- rpois(n = 1, lambda = 300) # document length / number of samples
      
    }
    
    # make theta into an iterator
    step <- ceiling(nrow(theta) / cpus)
    
    batches <- seq(1, nrow(theta), by = step)
    
    theta <- lapply(batches, function(x){
      theta[ x:min(x + step - 1, nrow(theta)) , ]
    })
    
    theta <- lapply(theta, function(x){
      as.list(as.data.frame(t(x)))
    })
    
    # iterate over theta to perform sampling of words
    dtm <- parallel::mclapply(theta, function(batch){
      
      result <- lapply(batch, function(d){
        
        n_d <- sample(doc_lengths, 1)
        
        # take n_d topic samples
        topics <- rmultinom(n = n_d, size = 1, prob = d)
        
        # reduce and format to optimize for time/memory
        topics <- rowSums(topics)
        
        topics <- rbind(topic_index = seq_along(topics), 
                        times_sampled = topics)
        
        topics <- topics[ , topics[ "times_sampled" , ] > 0 ]
        
        # sample words from each topic
      
        if (is.null(dim(topics))) # corrects for event only one topic is sampled
          x <- matrix(x, nrow = 2)
        
        words <- apply(topics, 2, function(x){
          
          result <- rmultinom(n = x[ "times_sampled" ], size = 1, prob = phi[ x[ "topic_index" ] , ])
          
          if (! is.null(dim(result)))
            result <- rowSums(result)
          
          result
        })
        
        # turn into a single vector
        words <- Matrix::Matrix(rowSums(words), nrow = 1, sparse = T)
        
        # garbage collection to reduce memory footprint
        gc()
        
        return(words)
      })
      
      result <- textmineR::RecursiveRbind(result)
    }, mc.cores = cpus)
    
    dtm <- RecursiveRbind(dtm)
    
    
    # theta <- as.list(as.data.frame(t(theta)))
    # 
    # # iterate over theta in parallel, performing sampling of words
    # dtm <- parallel::mclapply(theta, function(d){
    #   
    #   n_d <- rpois(n = 1, lambda = lambda) # document length / number of samples
    #   
    #   # take n_d topic samples
    #   topics <- rmultinom(n = n_d, size = 1, prob = d)
    #   
    #   # reduce and format to optimize for time/memory
    #   topics <- rowSums(topics)
    #   
    #   topics <- rbind(topic_index = seq_along(topics), 
    #                   times_sampled = topics)
    #   
    #   topics <- topics[ , topics[ "times_sampled" , ] > 0 ]
    #   
    #   # sample words from each topic
    #   words <- apply(topics, 2, function(x){
    #     
    #     result <- rmultinom(n = x[ "times_sampled" ], size = 1, prob = phi[ x[ "topic_index" ] , ])
    #     
    #     if (! is.null(dim(result)))
    #       result <- rowSums(result)
    #     
    #     result
    #   })
    #   
    #   # turn into a single vector
    #   words <- Matrix::Matrix(rowSums(words), nrow = 1, sparse = T)
    # 
    #   # garbage collection to reduce memory footprint
    #   gc()
    #   
    #   return(words)
    #   
    # }, mc.cores = cpus)
    # 
    # dtm <- textmineR::RecursiveRbind(dtm)
    
    rownames(dtm) <- names(theta)
    colnames(dtm) <- colnames(phi)
    
    return(dtm)
}

# function generates a theta for a given phi
# AddDocs <- function(K, D, cpus=4){
#     library("snowfall")
#     library("gtools")
#     
#     alpha <- rep(5 / K, K)
#     
#     sfInit(parallel = T, cpus = cpus)
#     sfExport(list=c("alpha", "K"))
#     sfLibrary(gtools)
#     
#     theta <- sfSapply(1:D, function(j){
#         result <- as.numeric(gtools::rdirichlet(n = 1, alpha = alpha))
#         names(result) <- paste("t", 1:K, sep="_")
#         return(result)
#     })
#     
#     sfStop()
#     
#     theta <- t(theta)
#     rownames(theta) <- paste("d", 1:D, sep="_")
#     
#     return(theta)
# }