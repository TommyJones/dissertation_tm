
rm(list = ls())

library(textmineR)

load("data_derived/cmpl_text_sample.RData")

dtm <- CreateDtm(cmpl_text_sample, ngram_window = c(1,2), stopword_vec = c())

tf <- TermDocFreq(dtm)
tf <- tf[ order(tf$term_freq, decreasing = TRUE) , ]
tf$rank <- 1:nrow(tf)
# sw <- c(tm::stopwords("english"), tm::stopwords("smart"))
sw <- tm::stopwords("english")
sw <- CreateDtm(doc_vec = sw, stopword_vec = c())
sw <- colnames(sw)
sw <- sort(sw)

sw_freq <- tf$term[ tf$doc_freq >= nrow(dtm) / 4 ]

# plot
plot(log(tf$rank), log(tf$term_freq), type = "l")

points(log(tf$rank[ ! tf$term %in% sw ]), log(tf$term_freq[ ! tf$term %in% sw ]), 
       pch = 19, col = rgb(0,0,1, 0.15))

points(log(tf$rank[ tf$term %in% sw ]), log(tf$term_freq[ tf$term %in% sw ]), 
       pch = 19, col = rgb(1,0,0, 0.15))

# plot
plot(tf$rank, tf$term_freq, type = "l")
points(tf$rank[ ! tf$term %in% sw ], tf$term_freq[ ! tf$term %in% sw ], 
       pch = 19, col = rgb(0,0,1, 0.3))
points(tf$rank[ tf$term %in% sw ], tf$term_freq[ tf$term %in% sw ], 
       pch = 19, col = rgb(1,0,0, 0.3))

# plot
plot(log(tf$rank), log(tf$term_freq), type = "l")

points(log(tf$rank[ tf$doc_freq >= nrow(dtm) / 4 ]), 
       log(tf$term_freq[ tf$doc_freq >= nrow(dtm) / 4 ]), 
       pch = 19, col = rgb(1,0,0, 0.15))

points(log(tf$rank[ tf$doc_freq < nrow(dtm) / 4 ]), 
       log(tf$term_freq[ tf$doc_freq < nrow(dtm) / 4 ]), 
       pch = 19, col = rgb(0,0,1, 0.15))


# plot
plot(tf$rank, tf$term_freq, type = "l")

points(tf$rank[ tf$doc_freq < nrow(dtm) / 4 ], 
       tf$term_freq[ tf$doc_freq < nrow(dtm) / 4 ], 
       pch = 19, col = rgb(0,0,1, 0.3))

points(tf$rank[ tf$doc_freq >= nrow(dtm) / 4 ], 
       tf$term_freq[ tf$doc_freq >= nrow(dtm) / 4 ], 
       pch = 19, col = rgb(1,0,0, 0.3))


GetSingleDocTf <- function(doc_row){
  
  tf_mat <- TermDocFreq(dtm = rbind(doc_row, doc_row))
  
  tf_mat <- data.frame(term = tf_mat$term,
                       term_freq = tf_mat$term_freq,
                       stringsAsFactors = F)
  
  tf_mat <- tf_mat[ order(tf_mat$term_freq, decreasing = T) , ]
  
  tf_mat$rank <- seq_along(tf_mat[[ 1 ]])
  
  tf_mat
}


candidates <- rowSums(dtm) > 1000
derp <- dtm[ candidates , ]
skee <- lapply(rownames(derp), function(x){
result <- GetSingleDocTf(doc_row = derp[ x , ])
result <- result[ result$term_freq > 0 , ]
})
names(skee) <- rownames(derp)

plot(log(skee[[ 1 ]]$rank), log(skee[[ 1 ]]$term_freq), type = "l")

points(log(skee[[ 1 ]]$rank[ ! skee[[ 1 ]]$term %in% sw_freq ]), 
       log(skee[[ 1 ]]$term_freq[ ! skee[[ 1 ]]$term %in% sw_freq ]), 
       pch = 19, col = rgb(0,0,1,0.2))

points(log(skee[[ 1 ]]$rank[ skee[[ 1 ]]$term %in% sw_freq ]), 
       log(skee[[ 1 ]]$term_freq[ skee[[ 1 ]]$term %in% sw_freq ]), 
       pch = 19, col = rgb(1,0,0,0.2))

plot(log(skee[[ 1 ]]$rank), log(skee[[ 1 ]]$term_freq) / max(log(skee[[ 1 ]]$term_freq)), type = "l")

for(j in 2:length(skee)){
  points(log(skee[[ j ]]$rank), log(skee[[ j ]]$term_freq) / max(log(skee[[ j ]]$term_freq)), type = "l", col = sample(colors(), 1))
}
lines(log(tf$rank), log(tf$term_freq) / max(log(tf$term_freq), na.rm = T), lty = 2, lwd = 2)

# play with chi-squared
chi2 <- function(a, b, c, d){
  # (x[ 1 , 1] + x[ 2, 2 ])^2 * sum(x) / ((x[ 1 ,1 ] + x[ 1 , 2 ]) * (x[ 2, 1 ] + x[ 2, 2 ]) * (x[ 1, 2 ] + x[ 2, 2 ]) * (x[ 1, 1 ] + x[ 2, 1 ]))
  
  (a + b) ^ 2 * (a + b + c + d) / ((a + b) * (c + d) * (b + d) * (a + c)) 
}

x <- as.data.frame(cbind(a = nih_dtm[ , "the" ], 
                         b = rowSums(nih_dtm), 
                         c = rep(sum(nih_dtm[ , "the" ]), nrow(nih_dtm)), 
                         d = rep(sum(rowSums(nih_dtm)), nrow(nih_dtm))))

my_chi2 <- chi2(a = x$a, b = x$b, c = x$c, d = x$d)

words <- colSums(nih_dtm)

words <- names(words)[ words > nrow(nih_dtm) / 4 ]

names(words) <- words

word_sums <- colSums(nih_dtm)

total <- sum(colSums(nih_dtm))

words_chi2 <- lapply(words, function(x){
  as.data.frame(cbind(a = nih_dtm[ , x ], 
                      b = rowSums(nih_dtm), 
                      c = rep(word_sums[ x ], nrow(nih_dtm)), 
                      d = rep(total, nrow(nih_dtm))))
})

words_chi2 <- lapply(words_chi2, function(x){
  chi2(a = x$a, b = x$b, c = x$c, d = x$d)
})

words_chi2 <- lapply(words_chi2, function(x){
  data.frame(chi2 = x, pchi2 = pchisq(x, df = 1))
})

CalcTStatistic <- function(n1, N1, n2, N2, scale = NULL, cohend = FALSE){
  # This function calculates t-statistics and cohen's d-statistics
  # The scale option will allow you to cap the maximum number of observations
  # to consider in the (much likely larger) control group (N2)
  
  p1 <- n1 / N1
  p2 <- n2 / N2
  
  s2_1 <- p1 * (1 - p1)
  s2_2 <- p2 * (1 - p2)
  
  if (! is.null(scale) & cohend) 
    warning("scale is not NULL and cohend is TRUE. 
            This will create a scaled version of Cohen's d statistic 
            (which won't change your results much). 
            Did you mean to do this?")
  
  if (! is.null(scale)){
    if (! is.numeric(scale))
      stop("If scale is non-NULL, it must be numeric and preferrably an integer.")
    
    scale_f <- N1 / N2
    N2[ N2 > scale ] <- scale
    N1 <- scale_f * scale
  }
  
  if (cohend) {
    t <- (p1 - p2) / sqrt(((N1 - 1) * s2_1 + (N2 - 1) * s2_2) / (N1 + N2))
  } else {
    t <- (p1 - p2) / sqrt(s2_1 / N1 + s2_2 / N2)
  }
  t
}

words_dstat <- lapply(words, function(x){
  as.data.frame(cbind(a = nih_dtm[ , x ], 
                      b = rowSums(nih_dtm), 
                      c = rep(word_sums[ x ], nrow(nih_dtm)), 
                      d = rep(total, nrow(nih_dtm))))
})

words_dstat <- lapply(words_dstat, function(x){
  CalcTStatistic(n1 = x[[ 1 ]], N1 = x[[ 2 ]],
                 n2 = x[[ 3 ]], N2 = x[[ 4 ]],
                 cohend = TRUE)
})

dstat_maxes <- sapply(words_dstat, function(x) max(x, na.rm = TRUE))



