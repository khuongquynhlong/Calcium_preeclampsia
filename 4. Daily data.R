library(tableone)
library(lubridate)
library(tidyverse)
library(haven)
# Define not in operator
`%!in%` = Negate(`%in%`)


# Original data
load("Data/TrialData.RData")
dat <- dat_all

# Baseline data
bs_dat <- readRDS("Data/Cleaning/baseline_charac.RDS")

# Outcome data
outcome_dat <- readRDS("Data/Cleaning/outcome_preeclampsia.RDS")

# Visit long data
ppv_dpv_dat <- readRDS("Data/Cleaning/visit_long.RDS")



#---------- Create daily and monthly data
#===============================================================================
# 225 people had no visit
dat_nomis <- ppv_dpv_dat |> filter(!is.na(visit_date))


ids_nomiss <- dat_nomis$StudySubjectID |> unique()
outcome_dat |> filter(StudySubjectID %!in% ids_nomiss) |> pull(end_status) |> table()


# ppv_dpv_dat |> filter(StudySubjectID %in% ltfuid) |> View()
#---------- Work for participants with at least 1 visit 1st
#-------------------------------------------------------------------------------
dat_nomis <- dat_nomis |> left_join(bs_dat |> select(StudySubjectID, adm_date, scr_date), 
                                    by = "StudySubjectID") |>
  arrange(StudySubjectID, visit_date)

dat_nomis <- dat_nomis |> group_by(StudySubjectID) |>
  mutate(visit_date_pre = lag(visit_date, 1),
         visit = 1:n()) |>
  ungroup() |>
  mutate(visit_date_pre = if_else(is.na(visit_date_pre), adm_date, visit_date_pre),
         day_since_previsit = as.numeric(visit_date - visit_date_pre))

dat_nomis <- dat_nomis |> select(StudySubjectID, adm_date, scr_date, visit_date, 
                                 visit_date_pre, day_since_previsit, names(dat_nomis))

# Replicate data to be daily
#-------------------------------------------------------------------------------
ids_num <- dat_nomis$StudySubjectID |> unique()


expand_visit_list <- list()

for (i in 1:length(ids_num)) {
  curr_id <- ids_num[i]
  dat_id <- dat_nomis[dat_nomis$StudySubjectID == curr_id, ]
  visit_id <- dat_id$visit
  
  temp_dat_visit <- list()
  
  # Loop through each visit
  for(j in 1:length(visit_id)){ 
    curr_visit <- visit_id[j]
    dat_id_visit <- dat_id[dat_id$visit == j, ]
    
    freq <- dat_id_visit$day_since_previsit
    
    # Replicate
    dat_id_visit_day <- rbind(dat_id_visit[rep(1, freq), ])
    rownames(dat_id_visit_day) <- NULL
    
    temp_dat_visit[[j]] <- dat_id_visit_day
  }
  # Append all visits
  expand_visit_list[[i]] <- do.call(rbind, temp_dat_visit)
}

# Append all participants
dat_nomis_daily <- do.call(rbind, expand_visit_list)


dat_nomis_daily <- dat_nomis_daily |>
  group_by(StudySubjectID) |>
  mutate(day_since_start = 1:n(),
         visit_date_sim = adm_date + day_since_start) |>
  ungroup() |> 
  select(StudySubjectID, adm_date, visit_date, visit_date_sim, day_since_start,
         day_since_previsit, shlth_prob, nantihyp_med, SBP, DBP, dpv, dosage)


dat_nomis_daily <- dat_nomis_daily |>
  mutate(adhere_per = dosage/day_since_previsit,
         adhere_per = ifelse(adhere_per > 1, 1, adhere_per))



#---------- Work for participants with no visit
#-------------------------------------------------------------------------------

dat_frame_all <- outcome_dat |>
  select(StudySubjectID, adm_date, end_date) |>
  mutate(
    day_since_previsit = as.numeric(end_date - adm_date)
  )



ids_num_all <- dat_frame_all$StudySubjectID |> unique()


expand_visit_list_all <- list()

for (i in 1:length(ids_num_all)) {
  curr_id <- ids_num_all[i]
  dat_id <- dat_frame_all[dat_frame_all$StudySubjectID == curr_id, ]
    
  freq <- dat_id$day_since_previsit
    
  # Replicate
  dat_id_visit_day <- rbind(dat_id[rep(1, freq), ])
  rownames(dat_id_visit_day) <- NULL

  # Append all visits
  expand_visit_list_all[[i]] <- dat_id_visit_day
}

# Append all participants
dat_all_daily <- do.call(rbind, expand_visit_list_all)


dat_all_daily <- dat_all_daily |> group_by(StudySubjectID) |>
  mutate(
    day_since_start = 1:n(),
    visit_date_sim = adm_date + day_since_start
  )


dat_all_daily <- dat_all_daily |> 
  select(StudySubjectID, adm_date, end_date, day_since_start, visit_date_sim)



#---------- Merge no visit to at least 1 visit to form sample frame
#-------------------------------------------------------------------------------

dat_daily_frame <- dat_all_daily |> left_join(dat_nomis_daily, 
                                              by = c("StudySubjectID", "adm_date",
                                                     "day_since_start", "visit_date_sim"))

# 1020564 obs (correct)
dim(dat_daily_frame) 

# write_dta(dat_daily_frame, "Temp/dat_daily_frame.dta")
# write_dta(dat_nomis_daily, "Temp/dat_nomis_daily.dta")

dat_daily_frame$StudySubjectID |> unique() |> length()

saveRDS(dat_daily_frame, "Data/Cleaning/daily_frame.RDS")


