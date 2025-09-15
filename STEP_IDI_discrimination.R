# STEP Discrimination Analyses
# Author: Jessica Arend
# Last Updated: 2024.10.27

# clear workspace
rm(list=ls())

# read in packages
pacman::p_load(dplyr, data.table, stringr, tibble, stats, car, ggplot2,  lme4, emmeans)
  # tidyr,  lme4, emmeans, lmerTest, Matrix, pbkrtest, psych,  performance, parameters)

# set working directory and read in data
# setwd("~/Documents/UMN Work & General/TOPX analyses/")

## function to clean data -------
clean_data <- function(data) {
  ex_strings <- c('TEST', "Test", "test", "NDAR", "archive")
  dat_probs <- data[grep(paste(ex_strings, collapse='|'), data$subj, ignore.case = TRUE), ]
  data <- data[!data$subj %in% dat_probs$subj, ]
  data$subj <- gsub('PROOFPOINT|-|[*]','', data$subj)
  data$subj <- trimws(data$subj)
  return(as.data.frame(data))
}

# # THIS FUNCTION DOESN'T WORK (yet) 
# # Define the function
# keep_2nd <- function(data, date_col, repeat_col, instance_value) {
#   data <- as.data.table(data)
#   tmp <- data[order(date_col), 
#               if (any(repeat_col == instance_value, na.rm = TRUE))
#                 first(.I[repeat_col == instance_value])
#               else last(.I), keyby = "subj"]
#   data <- data[tmp$V1]
#   return(data)
# }
# 
# # Example usage
# dat_idi_bl1_rev <- keep_2nd(dat_idi_bl1, "idi_date", "idi_repeat_instance", instance_value = "2")

# read in data ----------

### demographics -------
# data exports > select instrument: "demographics" and event: "baseline"
dat_demo <- read.csv("../data/nontask_data/demographics/step_demographics_20241027.csv")
# rename and reformat subject column and clean data
names(dat_demo)[1] <- "subj"
dat_demo <- clean_data(dat_demo)
# remove blank rows
dat_demo <- filter(dat_demo, demo_date != "")
# remove unnecessary rows
dat_demo <- subset(dat_demo, select = -c(redcap_event_name, redcap_repeat_instrument, redcap_repeat_instance, redcap_survey_identifier, demographics_complete))

#### recode gender --------
# recode gender 3 ways - 1=man, 2=woman, 3=nonbinary/other
dat_demo <- dat_demo %>%
  add_column(gender = if_else(is.na(dat_demo$gender_cis), dat_demo$gender_trans, dat_demo$gender_cis), .after = "gender_other")

# manually code some nonbinary ppts
tnb_ppts <- c("SP1030", "SP2014", "SP2084", "SP2112", "SP2116", "SP2120", 
              "SP2128", "SP2144", "SP2149","SP2160", "SP2195", "SP2197", "SP2207")
dat_demo <- dat_demo %>% 
  mutate(gender = ifelse(subj %in% tnb_ppts, "3", gender))

# recode to 5-way split: 1=cis man, 2=cis woman, 3=trans man, 4=trans woman, 5 = enby
dat_demo <- dat_demo %>%
  mutate(
    gender_5split = case_when(
      cis_or_trans==1 & gender_cis==1 ~ 1, # cis man
      cis_or_trans==1 & gender_cis==2 ~ 2, # cis woman
      cis_or_trans==2 & gender_trans==1 ~ 3, #trans man
      cis_or_trans==2 & gender_trans==2 ~ 4, # trans woman
      cis_or_trans==2 & gender_trans==3 ~ 5), #nonbinary
    .after = "gender")
dat_demo <- dat_demo %>% 
  mutate(gender_5split = ifelse(subj %in% tnb_ppts, "5", gender_5split))

# recode cis or trans: 1 = cis man, 2 = cis woman, 3 = trans/enby
dat_demo <- dat_demo %>% 
  mutate(
    gender_3split = case_when(
      gender_5split == 1 ~ 1,
      gender_5split == 2 ~ 2, 
      gender_5split == 3 ~ 3,
      gender_5split == 4 ~ 3,
      gender_5split == 5 ~ 3),
    .after = "gender_5split")

# recode cis or trans: 0 = cis, 1 = trans/enby
dat_demo <- dat_demo %>% 
  mutate(
    gender_cis_trans = case_when(
      gender_5split == 1 ~ 0,
      gender_5split == 2 ~ 0, 
      gender_5split == 3 ~ 1,
      gender_5split == 4 ~ 1,
      gender_5split == 5 ~ 1),
    .after = "gender_5split")

#### recode race -------
# now: 0=white, 1=black/african am, 2=indigenous am/alaska native, 3=asian/asian am, 4=native hawaiian/pacific islander, 5=multiracial, 6 = multiracial (latino/a)
# before: 0=white, 1=black/african am, 2=indigenous am/alaska native, 3=asian/asian am, 4=native hawaiian/pacific islander, 5=multiracial, 6 = other)
dat_demo <- dat_demo %>% 
  mutate(
    race_v2 = case_when(
      race==0 ~ 0,
      race==1 ~ 1,race==2 ~ 2, race==3 ~ 3, race==4 ~ 4, race==5 ~ 5, race==6~5),
    .after = "race")

# now: 0 = white and 1 = BIPOC/non-white
# before: 0=white, 1=black/african am, 2=indigenous am/alaska native, 3=asian/asian am, 4=native hawaiian/pacific islander, 5=multiracial)
dat_demo <- dat_demo %>% 
  mutate(
    race_2split = case_when(
      race==0 ~ 0,
      race==1 ~ 1,race==2 ~ 1, race==3 ~ 1, race==4 ~ 1, race==5 ~ 1, race==6~1),
    .after = "race")

#### recode sexuality ---------
# now: 1 = straight and 2 = queer
# before: 1 = Heterosexual or straight, 2 = Gay or lesbian, 3 = Bisexual, 4 = Prefer not to say, 5 =Other
dat_demo <- dat_demo %>% 
  mutate(
    sexuality_2split = case_when(
      sexual_orientation == 1 ~ 0,
      sexual_orientation == 2 ~ 1,
      sexual_orientation == 3 ~ 1,
      sexual_orientation == 4 ~ 1,
      sexual_orientation == 5 ~ 1),
    .after = "sexual_orientation")

# recode sexual orientation: 1 = heterosexual, 2 = gay/lesbian, 3 = bi/pan, 4 = queer/questioning/ace
dat_demo <- dat_demo %>% 
  mutate(
    sexuality_4split = case_when(
      sexual_orientation == 1 ~ 1,
      sexual_orientation == 2 ~ 2,
      sexual_orientation == 3 ~ 3,
      sexual_orientation == 4 ~ 4,
      sexual_orientation == 5 ~ 4),
    .after = "sexuality_2split")

pan_ppts <- c("SP1101", "SP2084", "SP2087", "SP2093", "SP2096", "SP2112", "SP2149", 
              "SP2155", "SP2158", "SP2167", "SP2183", "SP2196", "SP2197", "SP2248", "SP2266")
dat_demo <- dat_demo %>% 
  mutate(sexuality_4split = ifelse(subj %in% pan_ppts, "3", sexuality_4split))

# ace_ppts <- c("SP1069", "SP2014", "SP2059", "SP2101")
# dat_demo <- dat_demo %>% 
#   mutate(sexuality_5split = ifelse(subj %in% ace_ppts, "4", sexuality_5split))

queer_ppts <- c("SP1016", "SP1073", "SP2031", "SP2057", "SP2080", "SP2083", "SP2097", "SP2176", "SP2211", #queer/questioning
                "SP1069", "SP2014", "SP2059", "SP2101") # ace
dat_demo <- dat_demo %>% 
  mutate(sexuality_4split = ifelse(subj %in% queer_ppts, "4", sexuality_4split))

#### recode education -------------
# recode "highest caregiver education" variable, taking highest of mother/father
dat_demo$hi_caregiver_education <- pmax(dat_demo$father_education, dat_demo$mother_education, na.rm = TRUE)

dat_demo$hi_caregiver_education <- as.numeric(dat_demo$hi_caregiver_education)


### diagnosis  -------
# pull data from REDCap > "dx" report
dat_dx <- read.csv("../data/nontask_data/diagnosis/step_diagnosis_20241027.csv")
# rename and reformat subject column and clean data
names(dat_dx)[1] <- "subj"
dat_dx <- clean_data(dat_dx)
# remove unnecessary rows
dat_dx<- subset(dat_dx, select = -c(redcap_event_name, redcap_repeat_instrument, redcap_repeat_instance, diagnostic_information_complete))
# add variable for subject type 
dat_dx$group <- "group"
dat_dx <- dat_dx %>%
  mutate(
    group = case_when(
      str_sub(subj, 1, 3) == 'SP1' ~ 'Control',
      str_sub(subj, 1, 3) == 'SP2' ~ 'Patient',
      TRUE ~ group))

# if a control and dx = NA, change dx = 0
dat_dx$primary_dx <- ifelse(dat_dx$group == "Control" & is.na(dat_dx$primary_dx), 0, dat_dx$primary_dx)

# diagnoses: 0=control, 1=Sz, 2=SzAff, 3=Szform, 4=NOS, 5=BP, 6=MDD, 7=other


# recode affective/nonaffective psychosis
# dx_affect: 0 = control, 1 = non-aff (Sz/Szaff), 2 = aff (BP/MDD), 3 = NOS
dat_dx <- dat_dx %>% 
  mutate(
    dx_affect = case_when(
      primary_dx == 0 ~ 0,
      primary_dx == 1 ~ 1,
      primary_dx == 2 ~ 1,
      primary_dx == 3 ~ 1,
      primary_dx == 4 ~ 3,
      primary_dx == 5 ~ 2,
      primary_dx == 6 ~ 2,
      primary_dx == 7 ~ 3),
    .after = "primary_dx")

# recode primary dx - 5 split
# before: diagnoses: 0=control, 1=Sz, 2=SzAff, 3=Szform, 4=NOS, 5=BP, 6=MDD, 7=other
# now: 0 = control, 1 = Sz/Szform, 2 = SzAff, 3 = BP, 4 = MDD, 5 = NOS/other
dat_dx <- dat_dx %>% 
  mutate(
    dx_5split= case_when(
      primary_dx == 0 ~ 0,
      primary_dx == 1 ~ 1,
      primary_dx == 2 ~ 1,
      primary_dx == 3 ~ 2,
      primary_dx == 4 ~ 5,
      primary_dx == 5 ~ 3,
      primary_dx == 6 ~ 4,
      primary_dx == 7 ~ 5),
    .after = "primary_dx")


### IDI discrimination -------
# pull data from REDCap > "IDI" report 
dat_idi <- read.csv("../data/nontask_data/other_data/step_idi_bl-6m_20241027.csv")
# rename and reformat subject column and clean data
names(dat_idi)[1] <- "subj"
dat_idi <- clean_data(dat_idi)
# remove or rename unnecessary rows
dat_idi <- subset(dat_idi, select = -c(redcap_repeat_instrument, idi_intersectional_discrimination_index_complete))
dat_idi <- rename(dat_idi,
                      idi_repeat_instance="redcap_repeat_instance")


# subset by visit type
dat_idi_bl1 <- filter(dat_idi, redcap_event_name == "baseline_arm_1" & idi_date != "")
dat_idi_6m <- filter(dat_idi, redcap_event_name == "6mo_followup_arm_1" & idi_date != "")

# if a given subj has two IDIs, keep 2nd instance
dat_idi_bl1 <- as.data.table(dat_idi_bl1)
tmp <- dat_idi_bl1[order(idi_date), 
                    if (any(idi_repeat_instance == "2", na.rm = TRUE))
                      first(.I[idi_repeat_instance == "2"]) 
                    else last(.I), keyby = "subj"]
dat_idi_bl1 <- dat_idi_bl1[tmp$V1]

dat_idi_6m <- as.data.table(dat_idi_6m)
tmp <- dat_idi_6m[order(idi_date), 
                   if (any(idi_repeat_instance == "2", na.rm = TRUE))
                     first(.I[idi_repeat_instance == "2"]) 
                   else last(.I), keyby = "subj"]
dat_idi_6m <- dat_idi_6m[tmp$V1]

# key variables: 
# idi_anticipated: sum([idi01_dr_might],[idi02_job_might],[idi03_housing_might],[idi04_employer_might],[idi05_bank_might],[idi06_police_might],[idi07_attack_might],[idi08_harass_might],[idi09_rel_might])

#idi_daytoday: 	sum([idi10_joking],[idi11_unfriendly],[idi12_names],[idi13_afraid],[idi14_stare],[idi15_likeothers],[idi16_belong],[idi17_questions],[idi18_smart])

#idi_major: sum([idi19_dr],[idi20_job],[idi21_housing],[idi22_police],[idi23_school],[idi24_bank],[idi25_move],[idi26_relation],[idi27_harass],[idi28_threatattack],[idi29_physattack],[idi30_sexual],[idi31_property])

#idi_total: sum([idi_anticipated],[idi_daytoday],[idi_major])


### BPRS  -------------------
dat_bprs <- read.csv("../data/nontask_data/clinical/step_bprs_bl-6m_20241027.csv")

# calculate wilson-sponheim subfactors
dat_bprs <- dat_bprs %>% 
  mutate(bprs_pos_ws = bprs08_gran + bprs09_susp + bprs10_hall + bprs11_unus + bprs12_bizb) %>% 
  mutate(bprs_neg_ws = bprs16_blun + bprs17_emot + bprs18_motr) %>% 
  mutate(bprs_disorg_ws = bprs01_somc + bprs06_host + bprs13_self + bprs14_diso + bprs15_conc + bprs20_unco) %>% 
  mutate(bprs_mania_ws = bprs07_elat + bprs08_gran + bprs21_exci + bprs23_mohy + bprs24_mann) %>% 
  mutate(bprs_dep_anx_ws = bprs02_anxi + bprs03_depr + bprs04_suic + bprs05_guil)

dat_bprs$bprs_pos_ws2 <- dat_bprs$bprs_pos_ws/5
dat_bprs$bprs_neg_ws2 <- dat_bprs$bprs_neg_ws/3
dat_bprs$bprs_disorg_ws2 <- dat_bprs$bprs_disorg_ws/6
dat_bprs$bprs_mania_ws2 <- dat_bprs$bprs_mania_ws/5
dat_bprs$bprs_dep_anx_ws2 <- dat_bprs$bprs_dep_anx_ws/4

dat_bprs <- dat_bprs %>%
  select(record_id, bprs_date, redcap_event_name, redcap_repeat_instance, bprs_total, bprs_positive, bprs_negative, bprs_disorganized, bprs_mania, bprs_depression, bprs_pos_ws, bprs_neg_ws, bprs_disorg_ws, bprs_mania_ws, bprs_dep_anx_ws, bprs_pos_ws2, bprs_neg_ws2, bprs_disorg_ws2, bprs_mania_ws2, bprs_dep_anx_ws2) 

# rename variables
colnames(dat_bprs) <- c("subj", "bprs_date", "redcap_event_name", "bprs_repeat_instance", "bprs_total", 
                        "bprs_pos", "bprs_neg", "bprs_disorg", "bprs_mania", "bprs_depr", 
                        "bprs_pos_ws", "bprs_neg_ws", "bprs_disorg_ws", "bprs_mania_ws", "bprs_dep_anx_ws",
                        "bprs_pos_ws2", "bprs_neg_ws2", "bprs_disorg_ws2", "bprs_mania_ws2", "bprs_dep_anx_ws2")

# clean and remove duplicate rows
dat_bprs <- clean_data(dat_bprs)
dat_bprs <- dat_bprs[!is.na(dat_bprs$bprs_repeat_instance), ]

# subset by visit type
dat_bprs_bl1 <- filter(dat_bprs, redcap_event_name == "baseline_arm_1" & bprs_date != "")
dat_bprs_6m <- filter(dat_bprs, redcap_event_name == "6mo_followup_arm_1" & bprs_date != "")

# if a given subj has two BPRS, keep 2nd instance
dat_bprs_bl1 <- as.data.table(dat_bprs_bl1)
tmp <- dat_bprs_bl1[order(bprs_date), 
                    if (any(bprs_repeat_instance == "2")) 
                      first(.I[bprs_repeat_instance == "2"]) 
                    else last(.I), keyby = "subj"]
dat_bprs_bl1 <- dat_bprs_bl1[tmp$V1]

dat_bprs_6m <- as.data.table(dat_bprs_6m)
tmp <- dat_bprs_6m[order(bprs_date), 
                    if (any(bprs_repeat_instance == "2")) 
                      first(.I[bprs_repeat_instance == "2"]) 
                    else last(.I), keyby = "subj"]
dat_bprs_6m <- dat_bprs_6m[tmp$V1]

### SPQ  -------------------
dat_spq <- read.csv("../data/nontask_data/clinical/step_spq_bl_20241027.csv")

# rename variables
colnames(dat_spq) <- c("subj", "redcap_event_name", "spq_repeat_instrument", "spq_repeat_instance", "date", paste0("spq_", 1:32), "spq_notes", "spq_complete")

dat_spq <- dat_spq %>% 
  mutate(spq_ir = spq_1 + spq_2 + spq_3, # ideas of reference
         spq_s = spq_4 + spq_5 + spq_6, # suspiciousness
         spq_cf = spq_7 + spq_8 + spq_9, # no close friends
         spq_ca = spq_10 + spq_11 + spq_12, # constricted affect
         spq_eb = spq_13 + spq_14 + spq_15 + spq_16, # eccentric behavior
         spq_sa = spq_17 + spq_18 + spq_19 + spq_20, # social anxiety
         spq_mt = spq_21 + spq_22 + spq_23 + spq_24, # magical thinking
         spq_os = spq_25 + spq_26 + spq_27 + spq_28, # odd speech
         spq_up = spq_29 + spq_30 + spq_31 + spq_32, #unusual perception
         spq_cp = spq_ir + spq_s + spq_mt + spq_up, # cognitive perceptual
         spq_do = spq_eb + spq_os, # disorganized
         spq_ip = spq_cf + spq_ca + spq_sa, # interpersonal
         spq_total = spq_cp + spq_do + spq_ip)

dat_spq <- dat_spq %>%
  select(subj, date, redcap_event_name, spq_repeat_instance, spq_total, spq_cp, spq_do, spq_ip)

colnames(dat_spq) <- c("subj",  "spq_date","redcap_event_name", "spq_repeat_instance", "spq_total", "spq_cp", "spq_do", "spq_ip")

# clean and remove rows with only NA values
dat_spq <- clean_data(dat_spq)

# subset by visit type
dat_spq_bl1 <- filter(dat_spq, redcap_event_name == "baseline_arm_1" & spq_date != "")
# no 6m SPQ data

# if a given subj has two spq, keep 2nd instance
dat_spq_bl1 <- as.data.table(dat_spq_bl1)
tmp <- dat_spq_bl1[order(spq_date), 
                    if (any(spq_repeat_instance == "2")) 
                      first(.I[spq_repeat_instance == "2"]) 
                    else last(.I), keyby = "subj"]
dat_spq_bl1 <- dat_spq_bl1[tmp$V1]

### GFS/GFR  ------------------
# load in files
dat_gf <- read.csv("../data/nontask_data/functioning/step_gf_bl-6m_20241027.csv")

# create dataframe with only target variables
dat_gf <- dat_gf %>% 
  select(record_id, gbl_func_date, redcap_event_name, redcap_repeat_instance, gbl_func_soc_curr, gbl_func_role_curr)

# rename variables
colnames(dat_gf) <- c("subj", "gf_date", "redcap_event_name", "gf_repeat_instance", "gfs", "gfr")

# clean data and remove rows with only NA values
dat_gf <- clean_data(dat_gf)

# subset by visit type
dat_gf_bl1 <- filter(dat_gf, redcap_event_name == "baseline_arm_1" & gf_date != "")
dat_gf_6m <- filter(dat_gf, redcap_event_name == "6mo_followup_arm_1" & gf_date != "")

# if a given subj has two gf, keep 2nd instance
dat_gf_bl1 <- as.data.table(dat_gf_bl1)
tmp <- dat_gf_bl1[order(gf_date), 
                   if (any(gf_repeat_instance == "2")) 
                     first(.I[gf_repeat_instance == "2"]) 
                   else last(.I), keyby = "subj"]
dat_gf_bl1 <- dat_gf_bl1[tmp$V1]

# commented out because there are currently not any repeat instances = 2
# dat_gf_6m <- as.data.table(dat_gf_6m)
# tmp <- dat_gf_6m[order(gf_date), 
#                   if (any(gf_repeat_instance == "2")) 
#                     first(.I[gf_repeat_instance == "2"]) 
#                   else last(.I), keyby = "subj"]
# dat_gf_6m <- dat_gf_6m[tmp$V1]

### WHODAS --------------
# load in files
dat_whodas <- read.csv("../data/nontask_data/functioning/step_whodas_bl-6m_20241027.csv")

# rename variables
names(dat_whodas)[1] <- "subj"

# clean data and remove rows with only NA values
dat_whodas <- clean_data(dat_whodas)

# subset by visit type
dat_whodas_bl1 <- filter(dat_whodas, redcap_event_name == "baseline_arm_1" & whodas_date != "")
dat_whodas_6m <- filter(dat_whodas, redcap_event_name == "6mo_followup_arm_1" & whodas_date != "")

# commented out because currently no repeat instances of whodas
# # if a given subj has two whodas, keep 2nd instance
# dat_whodas_bl1 <- as.data.table(dat_whodas_bl1)
# tmp <- dat_whodas_bl1[order(whodas_date), 
#                   if (any(whodas_repeat_instance == "2")) 
#                     first(.I[whodas_repeat_instance == "2"]) 
#                   else last(.I), keyby = "subj"]
# dat_whodas_bl1 <- dat_whodas_bl1[tmp$V1]
# 
# dat_whodas_6m <- as.data.table(dat_whodas_6m)
# tmp <- dat_whodas_6m[order(whodas_date),
#                   if (any(whodas_repeat_instance == "2"))
#                     first(.I[whodas_repeat_instance == "2"])
#                   else last(.I), keyby = "subj"]
# dat_whodas_6m <- dat_whodas_6m[tmp$V1]

# combine data ------------
# combine demo and dx
demo <- inner_join(dat_demo, dat_dx %>% 
                     select(c(subj, primary_dx, 
                              primary_dx_other, dx_affect, dx_5split,
                              duration_of_psychosis, group)))
#setdiff(dat_demo$subj, dat_dx$subj)
#setdiff(dat_dx$subj, dat_demo$subj) #who we dropped - doesn't matter bc those people don't have IDI anyways

# combine bl1 demo and IDI, keeping all, then add other data
dat_bl1 <- inner_join(dat_idi_bl1, demo)
dat_bl1 <- left_join(dat_bl1, dat_bprs_bl1)
dat_bl1 <- left_join(dat_bl1, dat_spq_bl1)
dat_bl1 <- left_join(dat_bl1, dat_gf_bl1)
dat_bl1 <- left_join(dat_bl1, dat_whodas_bl1)

# combine bl1 demo and IDI, keeping all, then add other data
dat_6m <- inner_join(dat_idi_6m, demo)
dat_6m <- left_join(dat_6m, dat_bprs_6m)
dat_6m <- left_join(dat_6m, dat_gf_6m)
# no SPQ
dat_6m <- left_join(dat_6m, dat_whodas_6m)

# BL1 data --------------
### sample descriptives --------
dat_bl1_pt <- subset(dat_bl1, group == "Patient") 
dat_bl1_ctl <- subset(dat_bl1, group == "Control")

nPt <- nrow(dat_bl1_pt)
nCtl <- nrow(dat_bl1_ctl)

#### gender ---------
# recode to 5-way split: 1=cis man, 2=cis woman, 3=trans man, 4=trans woman, 5 = enby
table(dat_bl1_pt$gender_5split)
round((table(dat_bl1_pt$gender_5split)/nPt*100),2) 
sum(is.na(dat_bl1_pt$gender_5split)) # prefer not to say
round(((sum(is.na(dat_bl1_pt$gender_5split)))/nPt*100),2) 
table(dat_bl1_ctl$gender_5split)
round((table(dat_bl1_ctl$gender_5split)/nCtl*100),2) 
sum(is.na(dat_bl1_ctl$gender_5split))
round(((sum(is.na(dat_bl1_ctl$gender_5split)))/nPt*100),2)

chisq.test(dat_bl1$gender_5split, dat_bl1$group)

round((table(dat_bl1_pt$gender_cis_trans)/nPt*100),2) # % cis or trans
round((table(dat_bl1_ctl$gender_cis_trans)/nCtl*100),2)

#### sexuality --------
round((table(dat_bl1_pt$sexuality_2split)/nPt*100),2) # % straight or queer
round((table(dat_bl1_ctl$sexuality_2split)/nCtl*100),2) 

table(dat_bl1_pt$sexuality_4split) # 1 = heterosexual, 2 = gay/lesbian, 3 = bi/pan, 4 = queer
round((table(dat_bl1_pt$sexuality_4split)/nPt*100),2)
table(dat_bl1_ctl$sexuality_4split)
round((table(dat_bl1_ctl$sexuality_4split)/nCtl*100),2) 

chisq.test(dat_bl1$sexuality_4split, dat_bl1$group)


#### age ---------
mean(dat_bl1_pt$age)
sd(dat_bl1_pt$age)

mean(dat_bl1_ctl$age)
sd(dat_bl1_ctl$age)

leveneTest(dat_bl1$age, dat_bl1$group, data = dat_bl1)
t.test(age ~ group, data = dat_bl1, var.equal = TRUE)

#### race & ethnicity ---------
# 0=white, 1=black/african am, 2=indigenous am/alaska native, 3=asian/asian am, 4=native hawaiian/pacific islander, 5=multiracial, 6 = ?
table(dat_bl1_pt$race)
round((table(dat_bl1_pt$race)/nPt*100),2) # in percentages
sum(is.na(dat_bl1_pt$race))

table(dat_bl1_ctl$race)
round((table(dat_bl1_ctl$race)/nCtl*100),2) # in percentages
sum(is.na(dat_bl1_ctl$race))

chisq.test(dat_bl1$race, dat_bl1$group)


# hispanic, 0 = no, 1 = yes
table(dat_bl1_pt$hispanic)
round((table(dat_bl1_pt$hispanic)/nPt*100),2) # in percentages
sum(is.na(dat_bl1_pt$hispanic))
table(dat_bl1_ctl$hispanic)
round((table(dat_bl1_ctl$hispanic)/nCtl*100),2) # in percentages
sum(is.na(dat_bl1_ctl$hispanic))


#### education ---------
# self years of education
mean(dat_bl1_pt$subj_education, na.rm = TRUE)
sd(dat_bl1_pt$subj_education, na.rm = TRUE)
sum(is.na(dat_bl1_pt$subj_education))

mean(dat_bl1_ctl$subj_education, na.rm = TRUE)
sd(dat_bl1_ctl$subj_education, na.rm = TRUE)
sum(is.na(dat_bl1_ctl$subj_education))

# highest caregiver education
mean(dat_bl1_pt$hi_caregiver_education, na.rm = TRUE)
sd(dat_bl1_pt$hi_caregiver_education, na.rm = TRUE)
sum(is.na(dat_bl1_pt$hi_caregiver_education))

mean(dat_bl1_ctl$hi_caregiver_education, na.rm = TRUE)
sd(dat_bl1_ctl$hi_caregiver_education, na.rm = TRUE)
sum(is.na(dat_bl1_ctl$hi_caregiver_education))

leveneTest(dat_bl1$hi_caregiver_education, dat_bl1$group, data = dat_bl1)
t.test(hi_caregiver_education ~ group, data = dat_bl1, var.equal = TRUE)

#### employment ---------
# 1= Full-time (40 hours/week or more), 2=Half-time (approximately 20 hours/week), 3=Quarter time (approximately 10 hours/week), 4=Temporarily laid off/furloughed, sick/medical leave or parental leave, 5= Unemployed, looking for work, 6=Unemployed, not looking for work (e.g., supported by family), 7=On disability benefits (permanently or temporarily), 8=Student, 9=Volunteer, 10=Homemaker,11=Retired, 12=Other
table(dat_bl1_pt$subj_employ_status)
round((table(dat_bl1_pt$subj_employ_status)/nPt*100),2)
table(dat_bl1_ctl$subj_employ_status)
round((table(dat_bl1_ctl$subj_employ_status)/nCtl*100),2) 

dat_bl1$subj_employ_status <- as.numeric(dat_bl1$subj_employ_status)
chisq.test(dat_bl1$subj_employ_status, dat_bl1$group)


#### dx --------
# diagnoses: 0=control, 1=Sz, 2=SzAff, 3=Szform, 4=NOS, 5=BP, 6=MDD, 7=other
table(dat_bl1_pt$primary_dx)
round((table(dat_bl1_pt$primary_dx)/nPt*100),2) # in percentages

# 0 = control, 1 = Sz/Szform, 2 = SzAff, 3 = BP, 4 = MDD, 5 = NOS/other
round((table(dat_bl1_pt$dx_5split)/nPt*100),2) # in percentages

#  0 = control, 1 = non-aff (Sz/Szaff/Szfpr,), 2 = aff (BP/MDD), 3 = NOS/other
round((table(dat_bl1_pt$dx_affect)/nPt*100),2) # in percentages
sum(is.na(dat_bl1_pt$primary_dx))


### IDI descriptives --------
#### by group -------
dat_bl1 %>% 
  group_by(group) %>% 
  summarise(
    n = n(),
    ant_m = mean(idi_anticipated, na.rm = TRUE),
    ant_sd = sd(idi_anticipated, na.rm = TRUE),
    d2d_m = mean(idi_daytoday, na.rm = TRUE),
    d2d_sd = sd(idi_daytoday, na.rm = TRUE),
    maj_m = mean(idi_major, na.rm = TRUE),
    maj_sd = sd(idi_major, na.rm = TRUE),
    tot_m = mean(idi_total, na.rm = TRUE),
    tot_sd = sd(idi_total, na.rm = TRUE))

#### by race --------
dat_bl1 %>% 
  group_by(race) %>% 
  summarise(
    n = n(),
    ant_m = mean(idi_anticipated, na.rm = TRUE),
    ant_sd = sd(idi_anticipated, na.rm = TRUE),
    d2d_m = mean(idi_daytoday, na.rm = TRUE),
    d2d_sd = sd(idi_daytoday, na.rm = TRUE),
    maj_m = mean(idi_major, na.rm = TRUE),
    maj_sd = sd(idi_major, na.rm = TRUE),
    tot_m = mean(idi_total, na.rm = TRUE),
    tot_sd = sd(idi_total, na.rm = TRUE))

dat_bl1 %>% 
  group_by(race, group) %>% 
  summarise(
    n = n(),
    ant_m = mean(idi_anticipated, na.rm = TRUE),
    ant_sd = sd(idi_anticipated, na.rm = TRUE),
    d2d_m = mean(idi_daytoday, na.rm = TRUE),
    d2d_sd = sd(idi_daytoday, na.rm = TRUE),
    maj_m = mean(idi_major, na.rm = TRUE),
    maj_sd = sd(idi_major, na.rm = TRUE),
    tot_m = mean(idi_total, na.rm = TRUE),
    tot_sd = sd(idi_total, na.rm = TRUE))

dat_bl1 %>% 
  group_by(race_2split, group) %>% 
  summarise(
    n = n(),
    ant_m = mean(idi_anticipated, na.rm = TRUE),
    ant_sd = sd(idi_anticipated, na.rm = TRUE),
    d2d_m = mean(idi_daytoday, na.rm = TRUE),
    d2d_sd = sd(idi_daytoday, na.rm = TRUE),
    maj_m = mean(idi_major, na.rm = TRUE),
    maj_sd = sd(idi_major, na.rm = TRUE),
    tot_m = mean(idi_total, na.rm = TRUE),
    tot_sd = sd(idi_total, na.rm = TRUE))

#### by gender --------
dat_bl1 %>% 
  group_by(gender) %>% #1: cis/trans man, 2: cis/trans woman, 3: enby/other
  summarise(
    n = n(),
    ant_m = mean(idi_anticipated, na.rm = TRUE),
    ant_sd = sd(idi_anticipated, na.rm = TRUE),
    d2d_m = mean(idi_daytoday, na.rm = TRUE),
    d2d_sd = sd(idi_daytoday, na.rm = TRUE),
    maj_m = mean(idi_major, na.rm = TRUE),
    maj_sd = sd(idi_major, na.rm = TRUE),
    tot_m = mean(idi_total, na.rm = TRUE),
    tot_sd = sd(idi_total, na.rm = TRUE))

dat_bl1 %>% 
  group_by(gender_5split) %>% #1:cisM, 2:cisW, 3:transM, 4:transW, 5:enby/oth
  summarise(
    n = n(),
    ant_m = mean(idi_anticipated, na.rm = TRUE),
    ant_sd = sd(idi_anticipated, na.rm = TRUE),
    d2d_m = mean(idi_daytoday, na.rm = TRUE),
    d2d_sd = sd(idi_daytoday, na.rm = TRUE),
    maj_m = mean(idi_major, na.rm = TRUE),
    maj_sd = sd(idi_major, na.rm = TRUE),
    tot_m = mean(idi_total, na.rm = TRUE),
    tot_sd = sd(idi_total, na.rm = TRUE))

dat_bl1 %>% 
  group_by(gender, group) %>% #1: cis/trans man, 2: cis/trans woman, 3: enby/other
  summarise(
    n = n(),
    ant_m = mean(idi_anticipated, na.rm = TRUE),
    ant_sd = sd(idi_anticipated, na.rm = TRUE),
    d2d_m = mean(idi_daytoday, na.rm = TRUE),
    d2d_sd = sd(idi_daytoday, na.rm = TRUE),
    maj_m = mean(idi_major, na.rm = TRUE),
    maj_sd = sd(idi_major, na.rm = TRUE),
    tot_m = mean(idi_total, na.rm = TRUE),
    tot_sd = sd(idi_total, na.rm = TRUE))

dat_bl1 %>% 
  group_by(gender_5split, group) %>% #1:cisM, 2:cisW, 3:transM, 4:transW, 5:enby/oth
  summarise(
    n = n(),
    ant_m = mean(idi_anticipated, na.rm = TRUE),
    ant_sd = sd(idi_anticipated, na.rm = TRUE),
    d2d_m = mean(idi_daytoday, na.rm = TRUE),
    d2d_sd = sd(idi_daytoday, na.rm = TRUE),
    maj_m = mean(idi_major, na.rm = TRUE),
    maj_sd = sd(idi_major, na.rm = TRUE),
    tot_m = mean(idi_total, na.rm = TRUE),
    tot_sd = sd(idi_total, na.rm = TRUE))

dat_bl1 %>% 
  group_by(group, gender_5split) %>% #1:cisM, 2:cisW, 3:transM, 4:transW, 5:enby/oth
  summarise(
    n = n(),
    ant_m = mean(idi_anticipated, na.rm = TRUE),
    ant_sd = sd(idi_anticipated, na.rm = TRUE),
    d2d_m = mean(idi_daytoday, na.rm = TRUE),
    d2d_sd = sd(idi_daytoday, na.rm = TRUE),
    maj_m = mean(idi_major, na.rm = TRUE),
    maj_sd = sd(idi_major, na.rm = TRUE),
    tot_m = mean(idi_total, na.rm = TRUE),
    tot_sd = sd(idi_total, na.rm = TRUE))

dat_bl1 %>% 
  group_by(gender_3split, group) %>% #1:cisM, 2:cisW, 3:transM, 4:transW, 5:enby/oth
  summarise(
    n = n(),
    ant_m = mean(idi_anticipated, na.rm = TRUE),
    ant_sd = sd(idi_anticipated, na.rm = TRUE),
    d2d_m = mean(idi_daytoday, na.rm = TRUE),
    d2d_sd = sd(idi_daytoday, na.rm = TRUE),
    maj_m = mean(idi_major, na.rm = TRUE),
    maj_sd = sd(idi_major, na.rm = TRUE),
    tot_m = mean(idi_total, na.rm = TRUE),
    tot_sd = sd(idi_total, na.rm = TRUE))



dat_bl1 %>% 
  group_by(gender_cis_trans, group) %>% #0 = cis, 1 = trans/enby
  summarise(
    n = n(),
    ant_m = mean(idi_anticipated, na.rm = TRUE),
    ant_sd = sd(idi_anticipated, na.rm = TRUE),
    d2d_m = mean(idi_daytoday, na.rm = TRUE),
    d2d_sd = sd(idi_daytoday, na.rm = TRUE),
    maj_m = mean(idi_major, na.rm = TRUE),
    maj_sd = sd(idi_major, na.rm = TRUE),
    tot_m = mean(idi_total, na.rm = TRUE),
    tot_sd = sd(idi_total, na.rm = TRUE))

# key variables: 
#idi_anticipated: sum([idi01_dr_might],[idi02_job_might],[idi03_housing_might],[idi04_employer_might],[idi05_bank_might],[idi06_police_might],[idi07_attack_might],[idi08_harass_might],[idi09_rel_might])

#idi_daytoday: 	sum([idi10_joking],[idi11_unfriendly],[idi12_names],[idi13_afraid],[idi14_stare],[idi15_likeothers],[idi16_belong],[idi17_questions],[idi18_smart])

#idi_major: sum([idi19_dr],[idi20_job],[idi21_housing],[idi22_police],[idi23_school],[idi24_bank],[idi25_move],[idi26_relation],[idi27_harass],[idi28_threatattack],[idi29_physattack],[idi30_sexual],[idi31_property])

#idi_total: sum([idi_anticipated],[idi_daytoday],[idi_major])

### IDI analyses ------------------
#### group*gender ---------
dat_bl1$gender_3split <- as.factor(dat_bl1$gender_3split)

##### IDI-A: group*gender_3split ----------
mod_ggA <- lm(idi_anticipated ~ group * gender_3split, data = dat_bl1)
summary(mod_ggA)
anova(mod_ggA)

print(emmeans(mod_ggA, list(pairwise ~ group), adjust = "mvt"))
print(emmeans(mod_ggA, list(pairwise ~ gender_3split), adjust = "mvt"))
print(emmeans(mod_ggA, list(pairwise ~ group:gender_3split), adjust = "mvt"))
print(emmeans(mod_ggA, list(pairwise ~ gender_3split:group), adjust = "mvt"))

# plot
plot_ggA <- ggplot(dat_bl1, aes(x= gender, y=idi_anticipated, group = group)) + 
  stat_summary(fun="mean", geom="line", aes(color = group), linewidth = 1.5, position = position_dodge(width = 0.05)) + 
  stat_summary(fun="mean", geom="point", aes(color = group), size = 3, position = position_dodge(width = 0.05)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(color = group), 
               width = 0.2, size = 1, position = position_dodge(width = 0.05)) + 
  labs(y = "IDI-Anticipated", x = "", color = "") +
  coord_cartesian(ylim = c(1, 23)) +
  scale_color_manual(values=c("#2C737F", "#AA8829"),
                     labels=c("Ctl", "EP")) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(face="bold", size = 12),
        axis.title = element_text(face="bold", size = 16),
        legend.text = element_text(face="bold", size = 14),
        legend.title = element_text(face = "bold", size = 16),
        plot.title = element_text(face = "bold", size = 22))
(plot1 <- plot_ggA + scale_x_discrete(labels=c("1" = "Cis Man",
                                               "2" = "Cis Woman",
                                               "3" = "Trans/Enby")))
ggsave(paste0("graphs/", format(Sys.Date(), "%Y%m%d"), "_IDIa_gender.png"),
       plot1, width = 5, height = 4)

##### IDI-D: group*gender_3split ----------
mod_ggD <- lm(idi_daytoday ~ group * gender_3split, data = dat_bl1)
summary(mod_ggD)
anova(mod_ggD)

print(emmeans(mod_ggD, list(pairwise ~ group), adjust = "mvt"))
print(emmeans(mod_ggD, list(pairwise ~ gender_3split), adjust = "mvt"))
print(emmeans(mod_ggD, list(pairwise ~ group:gender_3split), adjust = "mvt"))
print(emmeans(mod_ggD, list(pairwise ~ gender_3split:group), adjust = "mvt"))

# plot
plot_ggD <- ggplot(dat_bl1, aes(x= gender, y=idi_daytoday, group = group)) + 
  stat_summary(fun="mean", geom="line", aes(color = group), linewidth = 1.5, position = position_dodge(width = 0.05)) + 
  stat_summary(fun="mean", geom="point", aes(color = group), size = 3, position = position_dodge(width = 0.05)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(color = group), 
               width = 0.2, size = 1, position = position_dodge(width = 0.05)) + 
  labs(y = "IDI-Day-to-Day", x = "", color = "") +
  coord_cartesian(ylim = c(1, 23)) +
  scale_color_manual(values=c("#2C737F", "#AA8829"),
                     labels=c("Ctl", "EP")) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(face="bold", size = 12),
        axis.title = element_text(face="bold", size = 16),
        legend.text = element_text(face="bold", size = 14),
        legend.title = element_text(face = "bold", size = 16),
        plot.title = element_text(face = "bold", size = 22))
(plot2 <- plot_ggD + scale_x_discrete(labels=c("1" = "Cis Man",
                                               "2" = "Cis Woman",
                                     "3" = "Trans/Enby")))
ggsave(paste0("graphs/", format(Sys.Date(), "%Y%m%d"), "_IDId_gender.png"),
       plot2, width = 5, height = 4)

##### IDI-M: group*gender_3split ----------
mod_ggM <- lm(idi_major ~ group * gender_3split, data = dat_bl1)
summary(mod_ggM)
anova(mod_ggM)

print(emmeans(mod_ggM, list(pairwise ~ group), adjust = "mvt"))
print(emmeans(mod_ggM, list(pairwise ~ gender_3split), adjust = "mvt"))
print(emmeans(mod_ggM, list(pairwise ~ group:gender_3split), adjust = "mvt"))
print(emmeans(mod_ggM, list(pairwise ~ gender_3split:group), adjust = "mvt"))

# plot
plot_ggM <- ggplot(dat_bl1, aes(x= gender, y=idi_major, group = group)) + 
  stat_summary(fun="mean", geom="line", aes(color = group), linewidth = 1.5, position = position_dodge(width = 0.05)) + 
  stat_summary(fun="mean", geom="point", aes(color = group), size = 3, position = position_dodge(width = 0.05)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(color = group), 
               width = 0.2, size = 1, position = position_dodge(width = 0.05)) + 
  labs(y = "IDI-Major", x = "", color = "") +
  coord_cartesian(ylim = c(0, 23)) +
  scale_color_manual(values=c("#2C737F", "#AA8829"),
                     labels=c("Ctl", "EP")) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(face="bold", size = 12),
        axis.title = element_text(face="bold", size = 16),
        legend.text = element_text(face="bold", size = 14),
        legend.title = element_text(face = "bold", size = 16),
        plot.title = element_text(face = "bold", size = 22))
(plot3 <- plot_ggM + scale_x_discrete(labels=c("1" = "Cis Man", "2" = "Cis Woman",
                                     "3" = "Trans/Enby")))
ggsave(paste0("graphs/", format(Sys.Date(), "%Y%m%d"), "_IDIm_gender.png"),
       plot3, width = 5, height = 4)

#mod1 <- lmer(idi_anticipated ~ group + gender_3split + (1|subj), data = dat_bl1) - not working

# scatterplot of pts positive symptoms + anticipated discrimination
# ggplot(dat_bl1_pt, aes(x = bprs_pos_ws, y = idi_anticipated)) +
#   geom_point()

# # Obtain estimated marginal means and confidence intervals, then plot 
# emm_results <- emmeans(mod1, ~ group + gender_3split)
# emm_df <- as.data.frame(emm_results)
# ggplot(emm_df, aes(x = gender_3split, y = emmean, color = group, group = group)) +
#   geom_line() +
#   geom_point() +
#   geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
#   labs(
#     x = "Gender (3-Split)",
#     y = "Estimated Marginal Mean of idi_anticipated",
#     title = "Estimated Marginal Means of idi_anticipated by Group and Gender") 

# predicted_values <- emmeans(mod1, ~ group * gender_5split)
# predicted_df <- as.data.frame(predicted_values)
# ggplot(predicted_df, aes(x = gender_5split, y = emmean, color = group, group = group)) +
#   geom_line(size = 1) +
#   geom_point(size = 3) +
#   geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
#   theme_minimal() +
#   labs(title = "Interaction Effect of Group and Gender on IDI",
#        x = "Gender (5-Split)",
#        y = "Predicted IDI (Anticipated)") +
#   scale_color_manual(values = c("blue", "red"))  # Customize colors as needed

#### group*race (5 split) ---------
dat_bl1$race_v2 <- as.factor(dat_bl1$race_v2)

##### IDI-A: group*race ----------
mod_grA <- lm(idi_anticipated ~ group * race_v2, data = dat_bl1)
summary(mod_grA)
anova(mod_grA)

dat_bl1_minus_r2 <- filter(dat_bl1, dat_bl1$race_v2!="2")
mod_grA2 <- lm(idi_anticipated ~ group * race_v2, data = dat_bl1_minus_r2)
summary(mod_grA2)
anova(mod_grA2)

print(emmeans(mod_grA2, list(pairwise ~ group), adjust = "mvt"))
#0=white, 1=Black, 3=Asian, 5=multiracial (no ctl 2, no ctl/pt 4, 6 recoded to 5)
print(emmeans(mod_grA2, list(pairwise ~ race_v2), adjust = "mvt"))
print(emmeans(mod_grA2, list(pairwise ~ group:race_v2), adjust = "mvt"))
print(emmeans(mod_grA2, list(pairwise ~ race_v2:group), adjust = "mvt"))

# plot
plot_grA <- ggplot(dat_bl1, aes(x= race_v2, y=idi_anticipated, group = group)) + 
  stat_summary(fun="mean", geom="line", aes(color = group), linewidth = 1.5) + 
  stat_summary(fun="mean", geom="point", aes(color = group), size = 3) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(color = group), 
               width = 0.2, size = 1, position = position_dodge(width = 0.05)) + 
  labs(y = "IDI-Anticipated", x = "Race", color = "") +
  scale_color_manual(values=c("#2C737F", "#AA8829"),
                     labels=c("Ctl", "EP")) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(face="bold", size = 10),
        axis.title = element_text(face="bold", size = 16),
        legend.text = element_text(face="bold", size = 14),
        legend.title = element_text(face = "bold", size = 16),
        plot.title = element_text(face = "bold", size = 22))
plot_grA + scale_x_discrete(labels=c("0" = "Wh", "1" = "B/AA",
                                     "2" = "IA/AN", "3" = "A/AA", "4" = "NH", 
                                     "5" = "M/O"))
# 0=white, 1=black/african am, 2=indigenous am/alaska native, 3=asian/asian am, 4=native hawaiian/pacific islander, 5=multiracial, 6 = ?

##### IDI-D: group*race ----------
mod_grD <- lm(idi_daytoday ~ group * race_v2, data = dat_bl1)
summary(mod_grD)
anova(mod_grD)

mod_grD2 <- lm(idi_daytoday ~ group * race_v2, data = dat_bl1_minus_r2)
summary(mod_grD2)
anova(mod_grD2)

print(emmeans(mod_grD2, list(pairwise ~ group), adjust = "mvt"))
print(emmeans(mod_grD2, list(pairwise ~ race_v2), adjust = "mvt"))
print(emmeans(mod_grD2, list(pairwise ~ group:race_v2), adjust = "mvt"))
print(emmeans(mod_grD2, list(pairwise ~ race_v2:group), adjust = "mvt"))

# plot
plot_grD <- ggplot(dat_bl1, aes(x= race_v2, y=idi_daytoday, group = group)) + 
  stat_summary(fun="mean", geom="line", aes(color = group), linewidth = 1.5) + 
  stat_summary(fun="mean", geom="point", aes(color = group), size = 3) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(color = group), 
               width = 0.2, size = 1, position = position_dodge(width = 0.05)) + 
  labs(y = "IDI-Day-to-Day", x = "Race", color = "") +
  scale_color_manual(values=c("#2C737F", "#AA8829"),
                     labels=c("Ctl", "EP")) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(face="bold", size = 10),
        axis.title = element_text(face="bold", size = 16),
        legend.text = element_text(face="bold", size = 14),
        legend.title = element_text(face = "bold", size = 16),
        plot.title = element_text(face = "bold", size = 22))
plot_grD + scale_x_discrete(labels=c("0" = "Wh", "1" = "B/AA",
                                     "2" = "IA/AN", "3" = "A/AA", "4" = "NH", 
                                     "5" = "M/O"))
# 0=white, 1=black/african am, 2=indigenous am/alaska native, 3=asian/asian am, 4=native hawaiian/pacific islander, 5=multiracial, 6 = ?

##### IDI-M: group*race ----------
mod_grM <- lm(idi_major ~ group * race_v2, data = dat_bl1)
summary(mod_grM)
anova(mod_grM)

mod_grM2 <- lm(idi_major ~ group * race_v2, data = dat_bl1_minus_r2)
summary(mod_grM2)
anova(mod_grM2)

print(emmeans(mod_grM2, list(pairwise ~ group), adjust = "mvt"))
print(emmeans(mod_grM2, list(pairwise ~ race_v2), adjust = "mvt"))
print(emmeans(mod_grM2, list(pairwise ~ group:race_v2), adjust = "mvt"))
print(emmeans(mod_grM2, list(pairwise ~ race_v2:group), adjust = "mvt"))

# plot
plot_grM <- ggplot(dat_bl1, aes(x= race_v2, y=idi_major, group = group)) + 
  stat_summary(fun="mean", geom="line", aes(color = group), linewidth = 1.5, position = position_dodge(width = 0.05)) + 
  stat_summary(fun="mean", geom="point", aes(color = group), size = 3) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(color = group), 
               width = 0.2, size = 1, position = position_dodge(width = 0.05)) + 
  labs(y = "IDI-Major", x = "Race", color = "") +
  scale_color_manual(values=c("#2C737F", "#AA8829"),
                     labels=c("Ctl", "EP")) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(face="bold", size = 10),
        axis.title = element_text(face="bold", size = 16),
        legend.text = element_text(face="bold", size = 14),
        legend.title = element_text(face = "bold", size = 16),
        plot.title = element_text(face = "bold", size = 22))
plot_grM + scale_x_discrete(labels=c("0" = "Wh", "1" = "B/AA",
                                     "2" = "IA/AN", "3" = "A/AA", "4" = "NH", 
                                     "5" = "M/O"))
# 0=white, 1=black/african am, 2=indigenous am/alaska native, 3=asian/asian am, 4=native hawaiian/pacific islander, 5=multiracial, 6 = ?

#### group*race (2 split) ---------
dat_bl1$race_2split <- as.factor(dat_bl1$race_2split)

##### IDI-A: group*race ----------
mod_grA <- lm(idi_anticipated ~ group * race_2split, data = dat_bl1)
summary(mod_grA)
anova(mod_grA)

print(emmeans(mod_grA, list(pairwise ~ group), adjust = "mvt"))
#0=white, 1=Black, 3=Asian, 5=multiracial (no ctl 2, no ctl/pt 4, 6 recoded to 5)
print(emmeans(mod_grA, list(pairwise ~ race_2split), adjust = "mvt"))
print(emmeans(mod_grA, list(pairwise ~ group:race_2split), adjust = "mvt"))
print(emmeans(mod_grA, list(pairwise ~ race_2split:group), adjust = "mvt"))

dat_bl1$race_2split <- factor(dat_bl1$race_2split, levels = c(0,1))


# plot
(plot_grA <- ggplot(dat_bl1, aes(x= race_2split, y=idi_anticipated, group = group)) + 
  stat_summary(fun="mean", geom="line", aes(color = group), linewidth = 1.5, position = position_dodge(width = 0.05)) + 
  stat_summary(fun="mean", geom="point", aes(color = group), size = 3, position = position_dodge(width = 0.05)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(color = group), 
               width = 0.2, size = 1, position = position_dodge(width = 0.05)) + 
  labs(y = "IDI-Anticipated", x = "", color = "") +
  coord_cartesian(ylim = c(1, 23)) +
  scale_color_manual(values=c("#2C737F", "#AA8829"),
                     labels=c("Ctl", "EP")) + 
  scale_x_discrete(labels=c("0" = "White", "1" = "BIPOC")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line.x = element_line(colour = "black"), 
        axis.line.y = element_line(colour = "black"), 
        axis.text = element_text(face="bold", size = 12),
        axis.title = element_text(face="bold", size = 16),
        legend.text = element_text(face="bold", size = 14),
        legend.title = element_text(face = "bold", size = 16),
        plot.title = element_text(face = "bold", size = 22)))
ggsave(paste0("graphs/", format(Sys.Date(), "%Y%m%d"), "_IDIa_race.png"),
       plot_grA, width = 5, height = 4)
# 0=white, 1=black/african am, 2=indigenous am/alaska native, 3=asian/asian am, 4=native hawaiian/pacific islander, 5=multiracial, 6 = ?

##### IDI-D: group*race ----------
mod_grD <- lm(idi_daytoday ~ group * race_2split, data = dat_bl1)
summary(mod_grD)
anova(mod_grD)

print(emmeans(mod_grD, list(pairwise ~ group), adjust = "mvt"))
print(emmeans(mod_grD, list(pairwise ~ race_2split), adjust = "mvt"))
print(emmeans(mod_grD, list(pairwise ~ group:race_2split), adjust = "mvt"))
print(emmeans(mod_grD, list(pairwise ~ race_2split:group), adjust = "mvt"))

# plot
(plot_grD <- ggplot(dat_bl1, aes(x= race_2split, y=idi_daytoday, group = group)) + 
  stat_summary(fun="mean", geom="line", aes(color = group), linewidth = 1.5, 
               position = position_dodge(width = 0.05)) + 
  stat_summary(fun="mean", geom="point", aes(color = group), size = 3, position = position_dodge(width = 0.05)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(color = group), 
               width = 0.2, size = 1, position = position_dodge(width = 0.05)) + 
  labs(y = "IDI-Day-to-Day", x = "", color = "") +
  coord_cartesian(ylim = c(1, 23)) +
  scale_x_discrete(labels=c("0" = "White", "1" = "BIPOC")) +
  scale_color_manual(values=c("#2C737F", "#AA8829"),
                     labels=c("Ctl", "EP")) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(face="bold", size = 12),
        axis.title = element_text(face="bold", size = 16),
        legend.text = element_text(face="bold", size = 14),
        legend.title = element_text(face = "bold", size = 16),
        plot.title = element_text(face = "bold", size = 22)))
ggsave(paste0("graphs/", format(Sys.Date(), "%Y%m%d"), "_IDId_race.png"),
       plot_grD, width = 5, height = 4)

# 0=white, 1=black/african am, 2=indigenous am/alaska native, 3=asian/asian am, 4=native hawaiian/pacific islander, 5=multiracial, 6 = ?

##### IDI-M: group*race ----------
mod_grM <- lm(idi_major ~ group * race_2split, data = dat_bl1)
summary(mod_grM)
anova(mod_grM)

print(emmeans(mod_grM, list(pairwise ~ group), adjust = "mvt"))
print(emmeans(mod_grM, list(pairwise ~ race_2split), adjust = "mvt"))
print(emmeans(mod_grM, list(pairwise ~ group:race_2split), adjust = "mvt"))
print(emmeans(mod_grM, list(pairwise ~ race_2split:group), adjust = "mvt"))

# plot
(plot_grM <- ggplot(dat_bl1, aes(x= race_2split, y=idi_major, group = group)) + 
  stat_summary(fun="mean", geom="line", aes(color = group), linewidth = 1.5,
               position = position_dodge(width = 0.05)) + 
  stat_summary(fun="mean", geom="point", aes(color = group), size = 3,
               position = position_dodge(width = 0.05)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(color = group), 
               width = 0.2, size = 1, position = position_dodge(width = 0.05)) + 
  labs(y = "IDI-Major", x = "", color = "") +
  coord_cartesian(ylim = c(0, 23)) +
  scale_x_discrete(labels=c("0" = "White", "1" = "BIPOC")) +
  scale_color_manual(values=c("#2C737F", "#AA8829"),
                     labels=c("Ctl", "EP")) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(face="bold", size = 12),
        axis.title = element_text(face="bold", size = 16),
        legend.text = element_text(face="bold", size = 14),
        legend.title = element_text(face = "bold", size = 16),
        plot.title = element_text(face = "bold", size = 22)))
ggsave(paste0("graphs/", format(Sys.Date(), "%Y%m%d"), "_IDIm_race.png"),
       plot_grM, width = 5, height = 4)
# 0=white, 1=black/african am, 2=indigenous am/alaska native, 3=asian/asian am, 4=native hawaiian/pacific islander, 5=multiracial, 6 = ?


