# function updated from Rogier Kievit 
# 28-10-22 Nicholas Judd nickkjudd@gmail.com
# https://dplyr.tidyverse.org/reference/across.html

fn_summary_SE <- function(tblData, MeasureVar, arrGroupVars, dblCI = .95, blNa = TRUE) {
  
  sum_statistics <- tblData %>%
    group_by(across({{ arrGroupVars }})) %>%
    dplyr::summarise(across({{MeasureVar}},
                            list(N = ~length(.x),
                                 mean = ~ mean(.x, na.rm = blNa), #blNA might not work
                                 median = ~ median(.x, na.rm = blNa),
                                 sd = ~ sd(.x, na.rm = blNa),
                                 sem = ~ sd(.x, na.rm = blNa)/sqrt(length(.x)), # sd/sqrt(n)
                                 sem_ci = ~ (sd(.x, na.rm = blNa)/sqrt(length(.x))) * qt(dblCI / 2 + .5, length(.x) - 1)), 
                     .names = "{.col}_{.fn}"))
  return(sum_statistics)
}

# old function doesn't work with multiple groups

# fn_summary_SE_old <- function(tblData, quoMeasureVar, arrGroupVars, dblCI = .95, blNa = TRUE) {
#   sum_statistics <- tblData %>%
#     group_by(across({{ arrGroupVars }})) %>%
#    dplyr::summarise(
#       N = n(),
#       score_mean := mean(!!quoMeasureVar, na.rm = blNa),
#       score_median := median(!!quoMeasureVar, na.rm = blNa),
#       sd := sd(!!quoMeasureVar, na.rm = blNa),
#       sem = sd / sqrt(N),
#       sem_ci = sem * qt(dblCI / 2 + .5, N - 1)
#     )
# 
#   return(sum_statistics)
# }

# older summary function with explanations (forces pylr installation)

# summarySE <- function(data = NULL, measurevar, groupvars = NULL, na.rm = FALSE,
#                       conf.interval = .95, .drop = TRUE) {
#   
#   # New version of length which can handle NA's: if na.rm==T, don't count them
#   length2 <- function(x, na.rm = FALSE) {
#     if (na.rm) {
#       sum(!is.na(x))
#     } else {
#       length(x)
#     }
#   }
#   
#   # This does the summary. For each group's data frame, return a vector with
#   # N, mean, median, and sd
#   
#   datac <- plyr::ddply(data, groupvars, .drop=.drop,
#                        .fun = function(xx, col) {
#                          c(N      = length2(xx[[col]], na.rm=na.rm),
#                            mean   = mean(xx[[col]], na.rm=na.rm),
#                            median = median(xx[[col]], na.rm=na.rm),
#                            sd      = sd(xx[[col]], na.rm=na.rm)
#                          )
#                        },
#                        measurevar
#   )
#   
#   
#   # playspace 
#   ct <- plyr::ddply(rep_data, c("group", "time"), .drop=T,
#                     .fun = function(xx, col) {
#                       c(N      = length2(xx[[col]], na.rm=F),
#                         mean   = mean(xx[[col]], na.rm=F),
#                         median = median(xx[[col]], na.rm=F),
#                         sd      = sd(xx[[col]], na.rm=F)
#                       )
#                     },
#                     "score"
#   )
#   
#   ct1 <- rep_data %>% 
#     group_by(group, time) %>% # so this needs to be changed from a char
#     summarise(N = n(), 
#               mean = mean(score),
#               median = median(score),
#               sd = sd(score),
#               sem = sd / sqrt(N),
#               ci = sem * qt(.95 / 2 + .5, N - 1))
#   
#   
#   
#   
#   # Rename the "mean" and "median" columns    
#   datac <- plyr::rename(datac, c("mean" = paste0(measurevar, "_mean", sep = "")))
#   datac <- plyr::rename(datac, c("median" = paste(measurevar, "_median", sep = "")))
#   
#   datac$sem <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
#   
#   # Confidence interval multiplier for standard error
#   # Calculate t-statistic for confidence interval:
#   # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
#   ciMult <- qt(conf.interval / 2 + .5, datac$N - 1)
#   datac$ci <- datac$sem * ciMult
#   
#   return(datac)
# }



