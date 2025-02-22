# title: Identifying Valid TOPX data
# author: "Jessica Arend"
# last updated: "2025-02-18"

# clear workspace
rm(list=ls())

# set working directories
pacman::p_load(dplyr)

#### CHANGE THIS WHEN RUNNING
# load in data
dat_all <- read.csv("~/Documents/umn_work/topx_analyses/topx_mri_clean/aggregate_data/aggregate_topx_20250125.csv")
View(dat_all)

# invalid data ----------
# create csv of invalid IDs, visits/dates, and trial types
dat_ex <- dat_all %>% filter(trial_type == "AX" & error > .90 | trial_type == "AY" & error > .90 | trial_type == "BX" & error > .90 | trial_type == "BY" & error > .50 | n > 136)

# save as invalid
# write.csv(dat_ex, file=paste0("~/Documents/umn_work/topx_analyses/topx_mri_behav/data/aggregated_topx_invalid_details_",gsub("-", "", Sys.Date()), ".csv"), row.names = FALSE)

# create csv of just invalid IDs and visits/dates
dat_ex_abbrev <- dat_ex %>% 
  select(subj, visit, date)
dat_ex_abbrev <- dat_ex_abbrev[!duplicated(dat_ex_abbrev), ]

# save as invalid
write.csv(dat_ex_abbrev,file=paste0("~/Documents/umn_work/topx_analyses/topx_mri_behav/data/aggregated_topx_invalid_",gsub("-", "", Sys.Date()), ".csv"), row.names = FALSE)

# number of participants with invalid data
n_invalid <- length(unique(dat_ex$subj))

# valid data ----------
valid_dat <- dat_all[!(dat_all$subj %in% dat_ex$subj),]

# save as valid
# write.csv(valid_dat,file=paste0("~/Documents/UMN Work & General/TOPX analyses/mturk aggregated data/aggregated_data_valid_",gsub("-", "", Sys.Date()), ".csv"), row.names = FALSE)