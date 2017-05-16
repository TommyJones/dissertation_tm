################################################################################
# This script does some pre-formatting for our raw text so it can be
# more-easily used downstream
################################################################################

rm(list = ls())


### load files -----------------------------------------------------------------
load("data_raw/cmpl_raw.RData")
load("data_raw/r_stats_twitter.RData")
nih <- read.csv("data_raw/RePORTER_PRJABS_C_FY2014.csv", colClasses = "character")

### extract text and dentifiers and convert to utf-8 ---------------------------

# note that cmpl contains 39 duplicated values based on id and text
cmpl_text <- cmpl$cdescr

cmpl_text <- stringr::str_conv(cmpl_text, "UTF-8")

names(cmpl_text) <- cmpl$cmplid

cmpl_text <- cmpl_text[ ! duplicated(names(cmpl_text)) ]

# r_stats_text contains duplicated text, but not duplicated id, dups handeled
# in below section, not here
names(r_stats_text) <- sapply(r_stats, function(x) x$id)

r_stats_text <- stringr::str_conv(r_stats_text, "UTF-8")

# nih data appears to contain no duplicates
nih_text <- nih$ABSTRACT_TEXT

nih_text <- stringr::str_conv(nih_text, "UTF-8")

names(nih_text) <- nih$APPLICATION_ID

### Sample or de-duplicate to have smaller sets to work with -------------------
set.seed(04952380)

cmpl_text_sample <- sample(seq_along(cmpl_text), 10000)

nih_text_sample <- sample(nih_text, 10000)

r_stats_text_unique <- r_stats_text[ ! duplicated(r_stats_text) ]


### save results ---------------------------------------------------------------

save(cmpl, cmpl_text, file = "data_derived/cmpl_full.RData")

save(cmpl_text, file = "data_derived/cmpl_text.RData")

save(cmpl_text_sample, file = "data_derived/cmpl_text_sample.RData")

save(nih_text, file = "data_derived/nih_text.RData")

save(nih_text_sample, file = "data_derived/nih_text_sample.RData")

save(r_stats, r_stats_text, file = "data_derived/r_stats_full.RData")

save(r_stats_text_unique, file = "data_derived/r_stats_text_unique.RData")
