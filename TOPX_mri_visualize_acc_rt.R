# Title: TOPX Visualizing Accuracy and RT (for EAB)
# Author: Jessica Arend
# Last updated: 20231204

# clear workspace
rm(list=ls())

# set working directories
pacman::p_load(dplyr, stringr, ggplot2, tidyr)

# load in data
dat <- read.csv("~/Documents/umn_work/topx_analyses/topx_clean/aggregate_data/aggregate_topx_20250315.csv")
View(dat)

# valid and invalid data ----------
# used this to ID subjects to manually delete from excel aggregate sheet
dat_ex <- dat %>% filter(trial_type == "AX" & error > .90 | trial_type == "AY" & error > .90 | trial_type == "BX" & error > .90 | trial_type == "BY" & error > .50 | n > 136)
View(dat_ex)

n_invalid <- length(unique(dat_ex$subj))

# save as invalid
#write.csv(dat_ex,file=paste0("~/Documents/UMN Work & General/TOPX analyses/mri aggregated data/aggregated_data_invalid_",gsub("-", "", Sys.Date()), ".csv"), row.names = FALSE)

valid_dat <- dat[!(dat$subj %in% dat_ex$subj),]

n_valid <- length(unique(valid_dat$subj))

# save as valid
#write.csv(valid_dat,file=paste0("~/Documents/UMN Work & General/TOPX analyses/mri aggregated data/aggregated_data_valid_",gsub("-", "", Sys.Date()), ".csv"), row.names = FALSE)

# re-read in dat with subjects removed
#dat <- read.csv("~/Documents/UMN Work & General/TOPX analyses/mri aggregated data/aggregated_data_valid_20240202.csv")
#View(dat)

# create subsets by visit -----------
# recode visits
dat[dat == "BL1.5"] <- "BL1"
dat[dat == "BL2.5"] <- "BL2"
dat[dat == "6M.5"] <- "6M"
dat[dat == "7M.5"] <- "7M"
dat[dat == "12M.5"] <- "12M"

## Baseline 1
dat_BL1 <- dat %>% 
  filter(visit == "BL1") 

## Baseline 2
dat_BL2 <- dat %>% 
  filter(visit == "BL2") 

## 6-Month
dat_6M <- dat %>% 
  filter(visit == "6M") 

# 7-Month
dat_7M <- dat %>% 
  filter(visit == "7M") 

# 12-Month
dat_12M <- dat %>% 
  filter(visit == "12M") 

# merge BL1 and BL2 ---------------
dat_bl1_rev <- subset(dat_BL1, (subj %in% dat_BL2$subj))
dat_bl2_rev <- subset(dat_BL2, (subj %in% dat_BL1$subj))
dat_all <- rbind(dat_bl1_rev, dat_bl2_rev)

# number of observations at visit ------------
nBL1_all <- (sum(dat_BL1$visit == "BL1"))/4
nBL1_ctl <- (sum(dat_BL1$visit == "BL1" & 
                  dat_BL1$subj_type == "Control"))/4
nBL1_pt <- (sum(dat_BL1$visit == "BL1" & 
                 dat_BL1$subj_type == "Patient"))/4

nBL2_all <- (sum(dat_BL2$visit == "BL2"))/4
nBL2_ctl <- (sum(dat_BL2$visit == "BL2" & 
                   dat_BL2$subj_type == "Control"))/4
nBL2_pt <- (sum(dat_BL2$visit == "BL2" & 
                  dat_BL2$subj_type == "Patient"))/4

n6M_all <- (sum(dat_6M$visit == "6M"))/4
n6M_ctl <- (sum(dat_6M$visit == "6M" & 
                  dat_6M$subj_type == "Control"))/4
n6M_pt <- (sum(dat_6M$visit == "6M" & 
                 dat_6M$subj_type == "Patient"))/4

n7M_all <- (sum(dat_7M$visit == "7M"))/4
n7M_ctl <- (sum(dat_7M$visit == "7M" &
                  dat_7M$subj_type == "Control"))/4
n7M_pt <- (sum(dat_7M$visit == "7M" &
                 dat_7M$subj_type == "Patient"))/4

n12M_all <- (sum(dat_12M$visit == "12M"))/4
n12M_ctl <- (sum(dat_12M$visit == "12M" &
                  dat_12M$subj_type == "Control"))/4
n12M_pt <- (sum(dat_12M$visit == "12M" &
                 dat_12M$subj_type == "Patient"))/4

# Error Rates ---------
## BL1 Error----------------
### BL1 Error ANOVA -----------
anova1 <- aov(error ~ trial_type * subj_type + 
                Error(subj), data = dat_BL1)
summary(anova1)
anova1a <- aov(error ~ trial_type, data = dat_BL1)
summary(anova1a)
TukeyHSD(anova1a)
anova1b <- aov(error ~ subj_type, data = dat_BL1)
summary(anova1b)
TukeyHSD(anova1b)

### BL1 Error Boxplot ------
pdf("~/Documents/UMN Work & General/TOPX analyses/misc/graphs/TOPX_BL1_Error_20231204.pdf", width = 4.5, height = 4)
ggplot(dat_BL1, aes(x=trial_type, y=error, fill = subj_type)) + 
  geom_boxplot(outlier.colour="black", outlier.size = .75) +
  labs(title="TOPX Error Rates at BL1 Visit", 
       x="Trial Type", y="Mean Error Rate") +
  scale_fill_manual(name = "Subject Type",
                    labels = c("Control (n=61)", 
                               "Early Psychosis (n=61)"),
                               values=c("dodgerblue", "chartreuse4")) +
  coord_cartesian(ylim = c(0,1.01)) +
  theme(legend.position="bottom")
dev.off()

### BL1 Error Lineplot --------
# summary data based on dx
dxTable_BL1err <- dat_BL1 %>% 
  group_by(subj_type, trial_type) %>% 
  summarise_at(vars(error), list(mean = mean, sd = sd))
View(dxTable_BL1err)

### summary data into lineplot -------
pdf("~/Documents/UMN Work & General/TOPX analyses/misc/graphs/TOPX_BL1_Error_line_20240110.pdf", width = 4.5, height = 4)
ggplot(dxTable_BL1err, aes(x = trial_type, y = mean, color = subj_type, group = subj_type)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.2)) +
  labs(title="TOPX Error at BL1", 
       x="Trial Type", y="Error Rate (mean)") +
  scale_color_manual(name = "Subject Type",
                     values = c("dodgerblue2", "green3"),
                    labels = c("HC (N=61)", 
                               "EP (N=61)"))
dev.off()


## BL2 Error ------------
### BL2 Error ANOVA ------------
anova2 <- aov(error ~ trial_type * subj_type + Error(subj), data = dat_BL2)
summary(anova2)
anova2a <- aov(error ~ trial_type, data = dat_BL2)
summary(anova2a)
TukeyHSD(anova2a)
anova2b <- aov(error ~ subj_type, data = dat_BL2)
summary(anova2b)

### BL2 Error Boxplot ---------
pdf("~/Documents/UMN Work & General/TOPX analyses/misc/graphs/TOPX_BL2_Error_20231204.pdf", width = 4.5, height = 4)
ggplot(dat_BL2, aes(x=trial_type, y=error, fill = subj_type)) + 
  geom_boxplot(outlier.colour="black", outlier.size = .75) +
  labs(title="TOPX Error Rates at BL2 Visit", 
       x="Trial Type", y="Mean Error Rate") +
  scale_fill_manual(name = "Subject Type",
                    labels = c("Control (n=56)", 
                               "Early Psychosis (n=51)"),
                    values=c("dodgerblue", "#2D817D")) +
  coord_cartesian(ylim = c(0,1.01)) +
  theme(legend.position="bottom")
dev.off()

### BL2 Error Lineplot --------
# summary data based on dx
dxTable_BL2err <- dat_BL2 %>% 
  group_by(subj_type, trial_type) %>% 
  summarise_at(vars(error), list(mean = mean, sd = sd))
View(dxTable_BL2err)

### summary data into lineplot ---------
ggplot(dxTable_BL2err, aes(x = trial_type, y = mean, color = subj_type, group = subj_type)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.2))

## Comb BL Error --------------
### Comb BL Error ANOVA -----------
anova11 <- aov(error ~ trial_type * subj_type + visit +
                Error(subj), data = dat_all)
summary(anova11)

anova11a <- aov(error ~ trial_type, data = dat_all)
summary(anova11a)
TukeyHSD(anova11a)

anova11b <- aov(error ~ subj_type, data = dat_all)
summary(anova11b)
TukeyHSD(anova11b)

## 6M Error -------------------
# 6M Error ANOVA - not enough patients yet
anova3 <- aov(error ~ trial_type * subj_type + Error(subj), data = dat_6M)
summary(anova3)
anova3a <- aov(error ~ trial_type, data = dat_6M)
summary(anova3a)
TukeyHSD(anova3a)
anova3b <- aov(error ~ subj_type, data = dat_6M)
summary(anova3b)

# visualize 6M Error using ggplot
pdf("~/Documents/UMN Work & General/TOPX analyses/misc/graphs/TOPX_6M_Error_20231204.pdf", width = 4.5, height = 4)
ggplot(dat_6M, aes(x=trial_type, y=error, fill = subj_type)) + 
  geom_boxplot(outlier.colour="black", outlier.size = .75) +
  labs(title="TOPX Error Rates at 6-Month Visit", 
       x="Trial Type", y="Mean Error Rate") +
  scale_fill_manual(name = "Subject Type",
                    labels = c("HC (n=40)", 
                               "EP (n=28)"),
                    values=c("dodgerblue", "chartreuse4")) +
  coord_cartesian(ylim = c(0,1.01)) +
  theme(legend.position="bottom")
dev.off()

## 7M Error ----------------
# 7M Error ANOVA
anova4 <- aov(error ~ trial_type * subj_type + Error(subj), data = dat_7M)
summary(anova4)
anova4a <- aov(error ~ trial_type, data = dat_7M)
summary(anova4a)
TukeyHSD(anova4a)
anova4b <- aov(error ~ subj_type, data = dat_7M)
summary(anova4b)

# visualize 7M Error using ggplot
pdf("~/Documents/UMN Work & General/TOPX analyses/misc/graphs/TOPX_7M_Error_20231204.pdf", width = 4.5, height = 4)
ggplot(dat_7M, aes(x=trial_type, y=error, fill = subj_type)) + 
  geom_boxplot(outlier.colour="black", outlier.size = .75) +
  labs(title="TOPX Error Rates at 7-Month Visit", 
       x="Trial Type", y="Mean Error Rate") +
  scale_fill_manual(name = "Subject Type",
                    labels = c("HC (n=19)", 
                               "EP (n=6)"),
                    values=c("dodgerblue", "chartreuse4")) +
  coord_cartesian(ylim = c(0,1.01)) +
  theme(legend.position="bottom")
dev.off()

# Reaction Times --------
## BL1 RT ---------------
### BL1 RT ANOVA ------------
anova5 <- aov(rt_mdn ~ trial_type * subj_type + Error(subj), data = dat_BL1)
summary(anova5)

anova5a <- aov(rt_mdn ~ trial_type, data = dat_BL1)
summary(anova5a)
TukeyHSD(anova5a)

anova5b <- aov(rt_mdn ~ subj_type, data = dat_BL1)
summary(anova5b)
TukeyHSD(anova5b)

### BL1 RT Boxplot ------
pdf("~/Documents/UMN Work & General/TOPX analyses/misc/graphs/TOPX_BL1_RT_20231204.pdf", width = 4.5, height = 4)
ggplot(dat_BL1, aes(x=trial_type, y=rt_mdn, fill = subj_type)) + 
  geom_boxplot(outlier.colour="black", outlier.size = .75) +
  labs(title="TOPX Reaction Times at BL1 Visit", 
       x="Trial Type", y="Median Reaction Time (ms)") +
  scale_fill_manual(name = "Subject Type",
                    labels = c("Control (n=61)", 
                               "Early Psychosis (n=61)"),
                    values=c("dodgerblue", "green2")) +
  coord_cartesian(ylim = c(195, 875)) +
  theme(legend.position="bottom")
dev.off()

### BL1 RT Lineplot --------
# summary data based on dx
dxTable_BL1rt <- dat_BL1 %>% 
  group_by(subj_type, trial_type) %>% 
  summarise_at(vars(rt_mdn), list(mean = mean, sd = sd))
View(dxTable_BL1rt)

### summary data into lineplot -------
pdf("~/Documents/UMN Work & General/TOPX analyses/misc/graphs/TOPX_BL1_RT_line_20240110.pdf", width = 4.5, height = 4)
ggplot(dxTable_BL1rt, aes(x = trial_type, y = mean, color = subj_type, group = subj_type)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.2))  +
  labs(title="TOPX Reaction Times at BL1", 
       x="Trial Type", y="RT (ms, median)") +
  scale_color_manual(name = "Subject Type",
                     values = c("dodgerblue2", "green3"),
                     labels = c("HC (N=61)", 
                                "EP (N=61)"))
dev.off()

## BL2 RT ---------------
### BL2 RT ANOVA ---------
anova6 <- aov(rt_mdn ~ trial_type * subj_type + Error(subj), data = dat_BL2)
summary(anova6)

anova6a <- aov(rt_mdn ~ trial_type, data = dat_BL2)
summary(anova6a)
TukeyHSD(anova6a)

anova6b <- aov(rt_mdn ~ subj_type, data = dat_BL2)
summary(anova6b)
TukeyHSD(anova6b)

### BL2 RT Boxplot -------------
pdf("~/Documents/UMN Work & General/TOPX analyses/misc/graphs/TOPX_BL2_RT_20231204.pdf", width = 4.5, height = 4)
ggplot(dat_BL2, aes(x=trial_type, y=rt_mdn, fill = subj_type)) + 
  geom_boxplot(outlier.colour="black", outlier.size = .75) +
  labs(title="TOPX Reaction Times at BL2 Visit", 
       x="Trial Type", y="Median Reaction Time (ms)") +
  scale_fill_manual(name = "Subject Type",
                    labels = c("Control (n=56)", 
                               "Early Psychosis (n=51)"),
                    values=c("dodgerblue", "#2D817D")) +
  coord_cartesian(ylim = c(195, 875)) +
  theme(legend.position="bottom")
dev.off()

### BL2 RT Lineplot --------
# summary data based on dx
dxTable_BL2rt <- dat_BL2 %>% 
  group_by(subj_type, trial_type) %>% 
  summarise_at(vars(rt_mdn), list(mean = mean, sd = sd))
View(dxTable_BL2rt)

# summary data into lineplot
ggplot(dxTable_BL2rt, aes(x = trial_type, y = mean, color = subj_type, group = subj_type)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, position = position_dodge(0.2))

## Comb BL RT ---------------
### Comb BL RT ANOVA ------------
anova12 <- aov(rt_mdn ~ trial_type * subj_type + Error(subj), data = dat_all)
summary(anova12)
anova12a <- aov(rt_mdn ~ trial_type, data = dat_all)
summary(anova12a)
TukeyHSD(anova12a)
anova12b <- aov(rt_mdn ~ subj_type, data = dat_all)
summary(anova12b)

## 6M RT ------------------
# 6M RT ANOVA
anova7 <- aov(rt_mdn ~ trial_type * subj_type + Error(subj), data = dat_6M)
summary(anova7)
anova7a <- aov(rt_mdn ~ trial_type, data = dat_6M)
summary(anova7a)
TukeyHSD(anova7a)

# visualize 6M RT using ggplot
pdf("~/Documents/UMN Work & General/TOPX analyses/misc/graphs/TOPX_6M_RT_202301204.pdf", width = 4.5, height = 4)
ggplot(dat_6M, aes(x=trial_type, y=rt_mdn, fill = subj_type)) + 
  geom_boxplot(outlier.colour="black", outlier.size = .75) +
  labs(title="TOPX Reaction Times at 6-Month Visit", 
       x="Trial Type", y="Median Reaction Time (ms)") +
  scale_fill_manual(name = "Subject Type",
                    labels = c("HC (n=40)", 
                               "EP (n=28)"),
                    values=c("dodgerblue", "chartreuse4")) +
  coord_cartesian(ylim = c(195, 875)) +
  theme(legend.position="bottom")
dev.off()

## 7M RT -----------------
# 7M RT ANOVA
anova8 <- aov(rt_mdn ~ trial_type * subj_type + Error(subj), data = dat_7M)
summary(anova8)
anova8a <- aov(rt_mdn ~ trial_type, data = dat_7M)
summary(anova8a)
TukeyHSD(anova8a)

# visualize 7M RT using ggplot
pdf("~/Documents/UMN Work & General/TOPX analyses/misc/graphs/TOPX_7M_RT_20231204.pdf", width = 4.5, height = 4)
ggplot(dat_7M, aes(x=trial_type, y=rt_mdn, 
                   fill = subj_type)) + 
  geom_boxplot(outlier.colour="black", outlier.size = .75) +
  labs(title="TOPX Reaction Times at 7-Month Visit", 
       x="Trial Type", y="Median Reaction Time (ms)") +
  scale_fill_manual(name = "Subject Type",
                    labels = c("Control (n=19)", 
                               "Early Psychosis (n=6)"),
                    values=c("dodgerblue", "chartreuse4")) +
  coord_cartesian(ylim = c(195, 875)) +
  theme(legend.position="bottom")
dev.off()

# RT Differences ----------------------------------

# pivot data to wide format
dat_wide <- dat %>%
  pivot_wider(id_cols = c(subj, task, visit, date, 
                          subj_type, d_context, d_expect),
              names_from = trial_type,
              values_from = c(n, numcorrect, rt_mean, rt_mdn, 
                              accuracy, accuracy_adj, error),
              names_vary = "slowest",
              names_glue = "{trial_type}_{.value}")

# calculate difference variables
dat_wide$AYAXdiff <- dat_wide$AY_rt_mdn - dat_wide$AX_rt_mdn
dat_wide$BXAXdiff <- dat_wide$BX_rt_mdn - dat_wide$AX_rt_mdn

dat_diff <- cbind(dat_wide$subj, dat_wide$visit, dat_wide$AYAXdiff, dat_wide$BXAXdiff)
colnames(dat_diff) <- c("subj", "visit", "AYAX_diff", "BXAX_diff")

#dat_diff_long <- as.data.frame(dat_diff) %>% 
#  pivot_longer(
#    cols = AYAX_diff:BXAX_diff,
#    names_to = c("diff", ".value"),
#    names_pattern = "^([[:alpha:]]{2})_(.*)")

# # pivot data back to long format
# dat_long <- dat_wide %>% 
#   pivot_longer(
#     cols = AX_n:BY_error,
#     names_to = c("trial_type", ".value"),
#     names_pattern = "^([[:alpha:]]{2})_(.*)")

# create subsets of wide data by visit
dat_BL1_w <- dat_wide %>% 
  filter(visit == "BL1") 

## Baseline 2
dat_BL2_w <- dat_wide %>% 
  filter(visit == "BL2") 

## AYAX------
# BL1 RT diff ANOVA - AYAX
anova9 <- aov(AYAXdiff ~ subj_type, data = dat_BL1_w)
summary(anova9)

# calculate group averages
AYAX_mean <- aggregate(x= dat_BL1_w$AYAXdiff,
                       by = list(dat_BL1_w$subj_type),
                       FUN = mean)
View(AYAX_mean)
AYAX_mdn <- aggregate(x= dat_BL1_w$AYAXdiff,
                       by = list(dat_BL1_w$subj_type),  
                       FUN = median)
View(AYAX_mdn)

## BXAX-----------
# BL1 RT diff ANOVA - BXAX
anova10 <- aov(BXAXdiff ~ subj_type, data = dat_BL1_w)
summary(anova10)

# calculate group averages
BXAX_mean <- aggregate(x= dat_BL1_w$BXAXdiff,
                       by = list(dat_BL1_w$subj_type),
                       FUN = mean)
View(BXAX_mean)
BXAX_mdn <- aggregate(x= dat_BL1_w$BXAXdiff,
                      by = list(dat_BL1_w$subj_type),  
                      FUN = median)
View(BXAX_mdn)

# T-tests of d-prime ---------------------
## BL1 d-prime -------------
## create dataframe with only 1 set of dprimes per subj
dat_BL1_AX <- dat %>% filter(trial_type == "AX" & visit == "BL1")

# create a numeric value for subject type
dat_BL1_AX <- dat_BL1_AX %>%
  mutate(subj_type_n = ifelse(subj_type=="Control", 0, 1))

t.test(dat_BL1_AX$subj_type_n, dat_BL1_AX$d_context, paired=FALSE, alternative = "two.sided")

t.test(dat_BL1_AX$subj_type_n, dat_BL1_AX$d_expect, paired=FALSE, alternative = "two.sided")

# look at table to confirm t-test significant differences
dTable <- dat_BL1_AX %>% group_by(subj_type) %>% summarise_at(vars(d_context, d_expect), list(mean))
View(dTable)

## BL2 d-prime -------------
## create dataframe with only 1 set of dprimes per subj
dat_BL2_AX <- dat %>% filter(trial_type == "AX" & visit == "BL2")

# create a numeric value for subject type
dat_BL2_AX <- dat_BL2_AX %>%
  mutate(subj_type_n = ifelse(subj_type=="Control", 0, 1))

t.test(dat_BL2_AX$subj_type_n, dat_BL2_AX$d_context, paired=FALSE, alternative = "two.sided")

t.test(dat_BL2_AX$subj_type_n, dat_BL2_AX$d_expect, paired=FALSE, alternative = "two.sided")

# look at table to confirm t-test significant differences
dTable <- dat_BL2_AX %>% group_by(subj_type) %>% summarise_at(vars(d_context, d_expect), list(mean))
View(dTable)

## Comb BL d-prime -------------
## create dataframe with only 1 set of dprimes per subj
dat_all_AX <- dat_all %>% filter(trial_type == "AX")

# create a numeric value for subject type
dat_all_AX <- dat_all_AX %>%
  mutate(subj_type_n = ifelse(subj_type=="Control", 0, 1))

t.test(dat_all_AX$subj_type_n, dat_all_AX$d_context, paired=TRUE, alternative = "two.sided")

t.test(dat_all_AX$subj_type_n, dat_all_AX$d_expect, paired=TRUE, alternative = "two.sided")

# look at table to confirm t-test significant differences
dTable <- dat_all_AX %>% group_by(subj_type) %>% summarise_at(vars(d_context, d_expect), list(mean))
View(dTable)

# demographics --------------------
dat_dx <- read.csv("misc/STEP_dx_DATA_20231206.csv")
dat_demo <- read.csv("misc/STEP_demo_DATA_20231206.csv")
# need to manually change first variable to "subj" in csv's (will script this later)

dat_dx <- as.data.frame(dat_dx)
dat_demo <- as.data.frame(dat_demo)

# combine data into 1 demographics dataframe
demo <- inner_join(dat_demo, dat_dx %>% 
                     select(c(subj, primary_dx, 
                              primary_dx_other)))
demo <- inner_join(demo, dat_demo %>% select (c(subj, race, multiracial, hispanic, gender_cis, gender_trans, gender_other, age)))
View(demo)

# recode gender
demo$gender <- if_else(is.na(demo$gender_cis), demo$gender_trans, demo$gender_cis)

# recode affective/nonaffective psychosis
demo$dx_affect <- as.character(demo$primary_dx)
demo$dx_affect <- as.numeric(recode(demo$dx_affect, "0" = "0", "1" = "1", "2" = "1", "3" = "1", "4" = "1", "5" = "2", "6" = "2", "7" = "1"))
table(demo$dx_affect)

# match demo to data subjects
demo <- subset(demo, (subj %in% dat$subj))

# create demographic dataframes by ppt type
demo_pt <- demo %>% filter(primary_dx != 0)
demo_ctl <- demo %>% filter(primary_dx == 0)

# frequencies
table(demo$race)/121
table(demo_pt$race)/61 
table(demo_ctl$race)/57

table(demo$gender)/121
table(demo_pt$gender)/61
table(demo_ctl$gender)/57

mean(demo$age)
mean(demo_pt$age)
mean(demo_ctl$age)
table(demo_pt$primary_dx)
table(demo_pt$primary_dx)/61 #__% Sz, __% SzAff, __% NOS, __% BP, __% MDD, __% other // __% Sz/SzAff, __% Bipolar, __% MDD, __% Psychosis NOS
table(demo_pt$dx_affect)
table(demo_pt$hispanic)/61 
table(demo_ctl$hispanic)/57
