################################################################################
# Libraries
################################################################################
library(tidyverse)
library(dplyr)
library(ppsr)
library(DT)
library(futile.logger)
library(tryCatchLog)

################################################################################
# Error logger
################################################################################
# This file could be saved and dropped into a slack channel by a bot daily to
# make certain data collected from that day was uploaded to the database without issue
test <- file("/Users/aosnacz/blue_crow_project/error_log_folder/error_file.log", open = "wt")
sink(test, append = TRUE, type = "message")

################################################################################
# Functions and Variables
################################################################################
root_path_processing <- "/Users/aosnacz/blue_crow_project/"
root_path_app<- "/Users/aosnacz/blue_crow_project/blue_crow_dash/"

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

################################################################################
# Process/read in all the provided data
################################################################################
# Ideally we're pulling the data from an active API or SQL database and not from csvs
individual_athlete_data_raw <- read.csv(paste0(root_path_processing, "data.csv"), stringsAsFactors = FALSE)

# remove any duplicate data
individual_athlete_data_raw <- distinct(individual_athlete_data_raw)

# I prefer dates to be in YYYY-MM-DD format
individual_athlete_data_raw$date <- as.Date(individual_athlete_data_raw$date, "%m/%d/%Y")

# get mean, standard deviation, and threshold values for outliers of specific metrics by day
tryCatch({
  
individual_athlete_data_raw_testing <- dplyr::group_by(individual_athlete_data_raw, athletes, date, metric) %>%
  dplyr::mutate(mean_outlier_detect = mean(value, na.rm = T),
         sd_outlier_detect = sd(value, na.rm = T),
         Tmin = mean_outlier_detect-(3*sd_outlier_detect),
         Tmax = mean_outlier_detect+(3*sd_outlier_detect),
         ) %>% ungroup()
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

# remove outlier, anything that is greater than 3 sds is considered an outlier
tryCatch({
  
individual_athlete_data_raw_testing <- individual_athlete_data_raw_testing %>% dplyr::mutate(is_outlier = ifelse(value < Tmin | value > Tmax, paste0("yes"), paste0("no")))
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

individual_athlete_data_raw_testing <- dplyr::filter(individual_athlete_data_raw_testing, is_outlier != "yes" | is.na(is_outlier))

# now that we've caught any outliers we can find the daily means for metrics
tryCatch({
  
individual_athlete_data_raw <- dplyr::group_by(individual_athlete_data_raw_testing, athletes, assessment, metric, date) %>% mutate(value = mean(value, na.rm = T)) %>% ungroup()
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

# now remove dupes
individual_athlete_data_raw <- distinct(individual_athlete_data_raw)

# TRY CATCH IS AROUND EACH FUNCTION SO WE KNOW IF SOMETHING GOES WRONG, WHAT EXACTLY IT IS
# Also for norms I only select baseline assessments so we don't overfit to athletes with more data than others
# we build normative values off the baseline assessments of athletes
tryCatch({
  
norms <- dplyr::group_by(dplyr::filter(dplyr::group_by(individual_athlete_data_raw, athletes, assessment, metric), date==min(date)), assessment, metric) %>%
  summarise(sample_size = n(), mean_value = mean(value, na.rm = T), sd_value = sd(value, na.rm = T)) %>% ungroup()
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

# now we save the data as a csv. ideally we don't have to do that and rather an update gets pushed to our API or to a SQL DB
write.csv(round_df(norms, 2), paste0(root_path_app, 'norms.csv'), row.names=FALSE)

# we build position normative values off the baseline assessments of athletes
tryCatch({
  
norms_pos <- group_by(filter(group_by(individual_athlete_data_raw, athletes,position, assessment, metric), date==min(date)), position, assessment, metric) %>%
  summarise(sample_size = n(), mean_value = mean(value, na.rm = T), sd_value = sd(value, na.rm = T)) %>% ungroup()
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

# now we save the data as a csv. ideally we don't have to do that and rather an update gets pushed to our API or to a SQL DB
write.csv(round_df(norms_pos, 2), paste0(root_path_app, 'norms_pos.csv'), row.names=FALSE)

# create age groups (honestly these are just going to be arbitrary)
tryCatch({
  
individual_athlete_data_raw <- individual_athlete_data_raw %>% dplyr::mutate(age_group = ifelse(age >= 17 & age < 21, paste("17-21 year old"), 
                                                        ifelse(age >= 21 & age < 25, paste("21-24 year old"),
                                                               ifelse(age >= 25 & age < 30, paste("25-29 year old"), paste("30+ year old")))))
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

# we build age group normative values off the baseline assessments of athletes
tryCatch({
  
norms_age<- group_by(filter(group_by(individual_athlete_data_raw, athletes, age_group,assessment, metric), date==min(date)), age_group, assessment, metric) %>%
  summarise(sample_size = n(), mean_value = mean(value, na.rm = T), sd_value = sd(value, na.rm = T)) %>% ungroup()
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

# now we save the data as a csv. ideally we don't have to do that and rather an update gets pushed to our API or to a SQL DB
write.csv(round_df(norms_age, 2), paste0(root_path_app, 'norms_age.csv'), row.names=FALSE)

# we build by handedness normative values off the baseline assessments of athletes
# this can be important for finding trends in asymmetries between laterality groups
tryCatch({
  
norms_hand <- group_by(filter(group_by(individual_athlete_data_raw, athletes,laterality, assessment, metric), date==min(date)), laterality, assessment, metric) %>% 
  summarise(sample_size = n(), mean_value = mean(value, na.rm = T), sd_value = sd(value, na.rm = T)) %>% ungroup()
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

# now we save the data as a csv. ideally we don't have to do that and rather an update gets pushed to our API or to a SQL DB
write.csv(round_df(norms_hand, 2), paste0(root_path_app, 'norms_hand.csv'), row.names=FALSE)

# Now merge normative values to the DF as we're gonna want these for percentiles and other fun stuff
# before that we need to rename our columns so we know what they are
tryCatch({
  
norms <- dplyr::rename(norms, mean_value_all_data = mean_value,
                       sd_value_all_data = sd_value,
                       sample_size_all_data = sample_size)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

tryCatch({
  
norms_age <- dplyr::rename(norms_age, mean_value_age_data = mean_value,
                           sd_value_age_data = sd_value,
                           sample_size_age_data = sample_size)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

tryCatch({
  
norms_hand <- dplyr::rename(norms_hand, mean_value_hand_data = mean_value,
                            sd_value_hand_data = sd_value,
                            sample_size_hand_data = sample_size)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

tryCatch({
  
norms_pos <- dplyr::rename(norms_pos, mean_value_pos_data = mean_value,
                           sd_value_pos_data = sd_value,
                           sample_size_pos_data = sample_size)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

# Now lets get this data into our main DF! 
# IMPORTANT!!! NEVER OVERWRITE THE RAW, MAKE A NEW DF SO YOU CAN DEBUG
# Each df should have the same number of observations, but 3 more variables
tryCatch({
  
individual_athlete_data_merged <- base::merge(individual_athlete_data_raw, norms, by = c("assessment", "metric"), all = T)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

tryCatch({
  
individual_athlete_data_merged2 <- base::merge(individual_athlete_data_merged, norms_age, by = c("age_group", "assessment", "metric"), all = T)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

tryCatch({
  
individual_athlete_data_merged3 <- base::merge(individual_athlete_data_merged2, norms_pos, by = c("position", "assessment", "metric"), all = T)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

tryCatch({
  
individual_athlete_data_final_merge <- base::merge(individual_athlete_data_merged3, norms_hand, by = c("laterality", "assessment", "metric"), all = T)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

# next step is getting those percentiles, smallest worthwhile change, and magnitude of change
# Grab z-scores quick so we can reference SDs from mean
tryCatch({
  
individual_athlete_data_zscores <- individual_athlete_data_final_merge %>%
  dplyr::mutate(zscore_all = (value - mean_value_all_data)/sd_value_all_data,
         zscore_age = (value - mean_value_age_data)/sd_value_age_data,
         zscore_hand = (value - mean_value_hand_data)/sd_value_hand_data,
         zscore_pos = (value - mean_value_pos_data)/sd_value_pos_data
         )
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

# Get percentiles for athletes
tryCatch({
  
individual_athlete_data_percentiles <- individual_athlete_data_zscores %>%
  dplyr::mutate(percentile_all = pnorm(zscore_all)*100,
         percentile_age = pnorm(zscore_age)*100,
         percentile_hand = pnorm(zscore_hand)*100,
         percentile_pos = pnorm(zscore_pos)*100
  )
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

# Let's get our last test value into a column, along with the change in value
# sort dates so if we call lag it will pull the date before
tryCatch({
  
individual_athlete_data_percentiles <- individual_athlete_data_percentiles %>%
                        arrange(as.Date(date, "%Y-%m-%d"))
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

# get the last value
tryCatch({
  
individual_athlete_data_percentiles <- dplyr::group_by(distinct(individual_athlete_data_percentiles), athletes, assessment, metric, laterality) %>% 
  dplyr::mutate(last_value = dplyr::lag(value))%>% ungroup()
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

# Find % change, abs change
tryCatch({
  
individual_athlete_data_percentiles <- dplyr::group_by(distinct(individual_athlete_data_percentiles), athletes, assessment, metric, laterality) %>%
  dplyr::mutate(value_change = value - last_value,
                pct_change = (value_change/ last_value)*100)%>% ungroup()
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

# Now we find the smallest worthwhile change (.2 = small, but not trivial effect size)
tryCatch({
  
individual_athlete_data_percentiles <- individual_athlete_data_percentiles %>%
  dplyr::mutate(swc = .2*sd(last_value, na.rm = T),
                was_significant = ifelse(value_change >= swc, paste0("Sig"), paste0("Not Sig")))
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
# And finally let's get the magnitude of said change
tryCatch({
  
individual_athlete_data_percentiles <- individual_athlete_data_percentiles %>% dplyr::mutate(
  effect_size = abs((as.numeric(value_change) - as.numeric(lag(value_change))) / as.numeric(sd_value_all_data))
  )
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

tryCatch({
  
individual_athlete_data_percentiles <- individual_athlete_data_percentiles %>% dplyr::mutate(
                                effect_size_interpretation = ifelse(effect_size >= 4, paste0("XXL"),
                                                         ifelse(effect_size < 4 & effect_size >= 2, paste0("XL"),
                                                                ifelse(effect_size < 2 & effect_size >= 1.2, paste0("Large"),
                                                                       ifelse(effect_size < 1.2 & effect_size >= .6, paste0("Moderate"),
                                                                              ifelse(effect_size < .6 & effect_size >= .2, paste0("Small"),
                                                                                     ifelse(effect_size < .2 & effect_size > 0, paste0("Trivial"),
                                                                                            paste0("No Change"))))))))
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

individual_athlete_data_final <- individual_athlete_data_percentiles

################################################################################
# Save data
################################################################################
# now we save the data as a csv. ideally we don't have to do that and rather an update gets pushed to our API or to a SQL DB
individual_athlete_data_final_round <- round_df(individual_athlete_data_final, 2)
write.csv(individual_athlete_data_final_round, paste0(root_path_app, 'individual_athlete_data_final.csv'), row.names=FALSE)

################################################################################
# End error logger
################################################################################

# end of error log
sink(type = "message")