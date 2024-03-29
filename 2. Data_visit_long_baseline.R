library(tableone)
library(lubridate)
library(tidyverse)
library(haven)
# Define not in operator
`%!in%` = Negate(`%in%`)


load("Data/TrialData.RData")

dat <- dat_all

# In the paper, from 2563 women assessed for eligibility, we have 1216 excluded, 1347 eligible, 
# However, 1355 were randomized, 678 to treatment and 677 allocated to placebo

table(dat$trt_grp, useNA = "always")

# Summary follow-up
table(dat$eos02a, dat$trt_grp, useNA = "always")


# Pregnant
table(dat$subject_preg, useNA = "always")
table(dat$eos02a, dat$subject_preg, useNA = "always")



#---------- Baseline characteristics (V)
#===============================================================================
# Age	                      adm_age	                  adm02
# Weight	                  adm_weight	              adm16
# Height	                  adm_height	              adm17
# Previous eclampsia	      adm_eclampsia	            adm08
# Previous HELLP syndrome	  adm_hellp	                adm09
# Parity	                  adm_parity	              adm03
# Previous livebirth	      adm_alive	                adm13
# Gestational age at birth	adm_gestageatb	          adm11
# Induction or CS	          adm_typebirth	            adm10
# ICU admission (mother)	  adm_ICUadm	              adm12
# Serious health problems	  adm_hlth_prob	            adm14a
# SBP	                      adm_sysbp	                adm15a
# DBP	                      adm_diasbp	              adm15b
# Study center	            adm_center	              scrcen
#----- adm.date

dat <- dat |>
  mutate(
    adm_date         = mdy(adm01),
    scr_date         = mdy(scr01),
    min_date         = min(adm_date),
    adm_age          = adm02,
    adm_weight       = adm16,
    adm_height       = adm17,
    adm_eclampsia    = adm08,
    adm_hellp        = adm09,
    adm_parity       = adm03,
    adm_alive        = adm13,
    adm_gestageatb   = adm11,
    adm_typebirth    = adm10,
    adm_ICUadm       = adm12,
    adm_hlth_prob    = adm14a,
    adm_sysbp        = adm15a,
    adm_diasbp       = adm15b,
    adm_center       = scrcen,
    adm_country      = COUNTRY)

bs_var <- c("adm_age", "adm_weight", "adm_height", "adm_eclampsia", "adm_hellp", 
            "adm_parity", "adm_alive", "adm_gestageatb", "adm_typebirth", "adm_ICUadm", 
            "adm_hlth_prob", "adm_sysbp", "adm_diasbp", "adm_center", "adm_country")


bs_dat <- dat |> select(StudySubjectID, adm_date, scr_date, min_date, all_of(bs_var), trt_grp)

glimpse(bs_dat)

bs_dat <- bs_dat |>  mutate(
  adm_weight = ifelse(adm_weight == 999, NA, adm_weight),
  adm_height = ifelse(adm_height == 999, NA, adm_height),
  adm_bmi = adm_weight/(adm_height/100)^2,
  adm_eclampsia = parse_number(as.character(adm_eclampsia)),
  adm_eclampsia = ifelse(adm_eclampsia == 9, NA, adm_eclampsia),
  adm_eclampsia = adm_eclampsia - 1,
  adm_hellp = parse_number(as.character(adm_hellp)),
  adm_hellp = ifelse(adm_hellp == 9, NA, adm_hellp),
  adm_hellp = adm_hellp - 1,
  adm_parity = ifelse(adm_parity == 9, NA, adm_parity),
  adm_alive = parse_number(as.character(adm_alive)),
  adm_alive = adm_alive - 1,
  adm_gestageatb = ifelse(adm_gestageatb == 99, NA, adm_gestageatb),
  adm_typebirth = parse_number(as.character(adm_typebirth)),
  adm_typebirth = ifelse(adm_typebirth == 9, NA, adm_typebirth),
  adm_ICUadm = parse_number(as.character(adm_ICUadm)),
  adm_ICUadm = adm_ICUadm - 1,
  adm_hlth_prob = parse_number(as.character(adm_hlth_prob)),
  adm_hlth_prob = adm_hlth_prob - 1,
  adm_sysbp = ifelse(adm_sysbp == 999, NA, adm_sysbp),
  adm_diasbp = ifelse(adm_diasbp == 999, NA, adm_diasbp),
)



bs_var_new <- c("adm_age", "adm_weight", "adm_height", "adm_eclampsia", "adm_hellp", 
            "adm_parity", "adm_alive", "adm_gestageatb", "adm_typebirth", "adm_ICUadm", 
            "adm_hlth_prob", "adm_sysbp", "adm_diasbp", "adm_country", "adm_bmi")


bs_var_cate <- c("adm_eclampsia", "adm_hellp", "adm_parity", "adm_alive", 
                 "adm_typebirth", "adm_ICUadm", "adm_hlth_prob")


bs_dat_des <- bs_dat |>
  mutate_at(bs_var_cate, function(x){as.factor(x)})



tab_m <- CreateTableOne(vars = c(bs_var_new), 
                        strata = "trt_grp", 
                        data = bs_dat_des, test = FALSE)
print(tab_m, smd = TRUE)



# saveRDS(bs_dat, "Data/Cleaning/baseline_charac.RDS")


#---------- Time-varying and events (PPV)
#===============================================================================

#----- Date of visit  ppv02_# 
ppv_date <- names(dat)[grep('ppv02_', names(dat))]
# dat <- dat |> mutate_at(ppv_date, function(x){mdy(as.character(x))})
dat |> select(all_of(ppv_date)) |> glimpse()

ppv_date_dat <- dat |> select(StudySubjectID, all_of(ppv_date)) |>
  pivot_longer(cols = -StudySubjectID,
               names_to = 'visit',
               names_prefix = 'ppv02_',
               values_to = 'visit_date') |>
  mutate(visit = as.numeric(visit),
         visit_date = mdy(visit_date))


#----- Pregnancy  ppv04_# 
ppv_preg <- names(dat)[grep('ppv04_', names(dat))]
dat$ppv04_1 <- parse_number(as.character(dat$ppv04_1))

dat |> select(all_of(ppv_preg)) |> glimpse()

ppv_preg_dat <- dat |> select(StudySubjectID, all_of(ppv_preg)) |>
  pivot_longer(cols = -StudySubjectID,
               names_to = 'visit',
               names_prefix = 'ppv04_',
               values_to = 'preg') |>
  mutate(visit = as.numeric(visit),
         preg = preg - 1)


#----- Any serious health problems  ppv03a_# 
ppv_shlth_prob <- names(dat)[grep('ppv03a_', names(dat))]
dat$ppv03a_1 <- parse_number(as.character(dat$ppv03a_1))

dat |> select(all_of(ppv_shlth_prob)) |> glimpse()

ppv_shlth_prob_dat <- dat |> select(StudySubjectID, all_of(ppv_shlth_prob)) |>
  pivot_longer(cols = -StudySubjectID,
               names_to = 'visit',
               names_prefix = 'ppv03a_',
               values_to = 'shlth_prob') |>
  mutate(visit = as.numeric(visit),
         shlth_prob = shlth_prob - 1)


#----- Number of antihypertensive medicines  ppv07_# 
ppv_nantihyp_med <- names(dat)[grep('ppv07_', names(dat))]
dat |> select(all_of(ppv_nantihyp_med)) |> glimpse()

ppv_nantihyp_med_dat <- dat |> select(StudySubjectID, all_of(ppv_nantihyp_med)) |>
  pivot_longer(cols = -StudySubjectID,
               names_to = 'visit',
               names_prefix = 'ppv07_',
               values_to = 'nantihyp_med') |>
  mutate(visit = as.numeric(visit),
         nantihyp_med = ifelse(nantihyp_med == 9, NA, nantihyp_med))


#----- SBP ppv08a_# 
ppv_SBP <- names(dat)[grep('ppv08a_', names(dat))]
dat |> select(all_of(ppv_SBP)) |> glimpse()

ppv_SBP_dat <- dat |> select(StudySubjectID, all_of(ppv_SBP)) |>
  pivot_longer(cols = -StudySubjectID,
               names_to = 'visit',
               names_prefix = 'ppv08a_',
               values_to = 'SBP') |>
  mutate(visit = as.numeric(visit),
         SBP = ifelse(SBP == 999, NA, SBP))

#----- DBP ppv08a_# 
ppv_DBP <- names(dat)[grep('ppv08b_', names(dat))]
dat |> select(all_of(ppv_DBP)) |> glimpse()

ppv_DBP_dat <- dat |> select(StudySubjectID, all_of(ppv_DBP)) |>
  pivot_longer(cols = -StudySubjectID,
               names_to = 'visit',
               names_prefix = 'ppv08b_',
               values_to = 'DBP') |>
  mutate(visit = as.numeric(visit),
         DBP = ifelse(DBP == 999, NA, DBP))


#----- Number of tablets returned only on visits with a date ppv10c_# 
ppv_dosage_info <- names(dat)[grep('ppv10c_', names(dat))]
dat |> select(all_of(ppv_dosage_info)) |> glimpse()

ppv_dosage_info_dat <- dat |> select(StudySubjectID, all_of(ppv_dosage_info)) |>
  pivot_longer(cols = -StudySubjectID,
               names_to = 'visit',
               names_prefix = 'ppv10c_',
               values_to = 'dosage_bottle') |>
  mutate(visit = as.numeric(visit),
         dosage_bottle = 84 - dosage_bottle)


#----- Number of tablets estimate only on visits with a date ppv10d_# 
ppv_dosage_est <- names(dat)[grep('ppv10d_', names(dat))]
dat |> select(all_of(ppv_dosage_est)) |> glimpse()

ppv_dosage_est_dat <- dat |> select(StudySubjectID, all_of(ppv_dosage_est)) |>
  pivot_longer(cols = -StudySubjectID,
               names_to = 'visit',
               names_prefix = 'ppv10d_',
               values_to = 'dosage_est') |>
  mutate(visit = as.numeric(visit),
         dosage_est = ifelse(dosage_est == 99, NA, dosage_est))


#----- Does she continue in the study? ppv11_# 
ppv_cont <- names(dat)[grep('ppv11_', names(dat))]
dat$ppv11_1 <- parse_number(as.character(dat$ppv11_1))
dat |> select(all_of(ppv_cont)) |> glimpse()

ppv_cont_dat <- dat |> select(StudySubjectID, all_of(ppv_cont)) |>
  pivot_longer(cols = -StudySubjectID,
               names_to = 'visit',
               names_prefix = 'ppv11_',
               values_to = 'cont_visit') |>
  mutate(visit = as.numeric(visit),
         cont_visit = cont_visit - 1)



#----- Date form completed ppvdat_# 
ppv_com_date <- names(dat)[grep('ppvdat_', names(dat))]
dat |> select(all_of(ppv_com_date)) |> glimpse()


ppv_com_date_dat <- dat |> select(StudySubjectID, all_of(ppv_com_date)) |>
  pivot_longer(cols = -StudySubjectID,
               names_to = 'visit',
               names_prefix = 'ppvdat_',
               values_to = 'complete_date') |>
  mutate(visit = as.numeric(visit),
         complete_date = mdy(complete_date))


#----- Merge all PPV data
#-------------------------------------------------------------------------------
ppv_dat <- ppv_date_dat |> 
  left_join(ppv_preg_dat, by = c("StudySubjectID", "visit"))|> 
  left_join(ppv_shlth_prob_dat, by = c("StudySubjectID", "visit"))|> 
  left_join(ppv_nantihyp_med_dat, by = c("StudySubjectID", "visit"))|> 
  left_join(ppv_SBP_dat, by = c("StudySubjectID", "visit"))|> 
  left_join(ppv_DBP_dat, by = c("StudySubjectID", "visit"))|> 
  left_join(ppv_dosage_info_dat, by = c("StudySubjectID", "visit"))|> 
  left_join(ppv_dosage_est_dat, by = c("StudySubjectID", "visit"))|> 
  left_join(ppv_cont_dat, by = c("StudySubjectID", "visit"))|> 
  left_join(ppv_com_date_dat, by = c("StudySubjectID", "visit"))

glimpse(ppv_dat)


ppv_dat$dipstick = 9
ppv_dat$visit_type = 0
ppv_dat$ppv = 1
ppv_dat$dpv = 0

#---------- Time-varying and events (PPV)
#===============================================================================
#-----  Type of DPV form: dpv01_# 
dpv_type <- names(dat)[grep('dpv01_', names(dat))]
dat$dpv01_1 <- parse_number(as.character(dat$dpv01_1))

dat |> select(all_of(dpv_type)) |> glimpse()

dpv_type_dat <- dat |> select(StudySubjectID, all_of(dpv_type)) |>
  pivot_longer(cols = -StudySubjectID,
               names_to = 'visit',
               names_prefix = 'dpv01_',
               values_to = 'visit_type') |>
  mutate(visit = as.numeric(visit))


#----- Date of visit  dpv02_# 
dpv_date <- names(dat)[grep('dpv02_', names(dat))]
dat |> select(all_of(dpv_date)) |> glimpse()

dpv_date_dat <- dat |> select(StudySubjectID, all_of(dpv_date)) |>
  pivot_longer(cols = -StudySubjectID,
               names_to = 'visit',
               names_prefix = 'dpv02_',
               values_to = 'visit_date') |>
  mutate(visit = as.numeric(visit),
         visit_date = mdy(visit_date))


#----- Pregnancy
dpv_date_dat$preg <- ifelse(!is.na(dpv_date_dat$visit_date), 1, NA)


#----- Any serious health problems  dpv05a_# 
dpv_shlth_prob <- names(dat)[grep('dpv05a_', names(dat))]
dat$dpv05a_1 <- parse_number(as.character(dat$dpv05a_1))

dat |> select(all_of(dpv_shlth_prob)) |> glimpse()

dpv_shlth_prob_dat <- dat |> select(StudySubjectID, all_of(dpv_shlth_prob)) |>
  pivot_longer(cols = -StudySubjectID,
               names_to = 'visit',
               names_prefix = 'dpv05a_',
               values_to = 'shlth_prob') |>
  mutate(visit = as.numeric(visit),
         shlth_prob = shlth_prob - 1)


#----- Number of antihypertensive medicines dpv06_# 
dpv_nantihyp_med <- names(dat)[grep('dpv06_', names(dat))]
dat |> select(all_of(dpv_nantihyp_med)) |> glimpse()

dpv_nantihyp_med_dat <- dat |> select(StudySubjectID, all_of(dpv_nantihyp_med)) |>
  pivot_longer(cols = -StudySubjectID,
               names_to = 'visit',
               names_prefix = 'dpv06_',
               values_to = 'nantihyp_med') |>
  mutate(visit = as.numeric(visit),
         nantihyp_med = ifelse(nantihyp_med == 9, NA, nantihyp_med))


#----- SBP dpv08a_# 
dpv_SBP <- names(dat)[grep('dpv08a_', names(dat))]
dat |> select(all_of(dpv_SBP)) |> glimpse()

dpv_SBP_dat <- dat |> select(StudySubjectID, all_of(dpv_SBP)) |>
  pivot_longer(cols = -StudySubjectID,
               names_to = 'visit',
               names_prefix = 'dpv08a_',
               values_to = 'SBP') |>
  mutate(visit = as.numeric(visit),
         SBP = ifelse(SBP == 999, NA, SBP))

#----- DBP dpv08b_# 
dpv_DBP <- names(dat)[grep('dpv08b_', names(dat))]
dat |> select(all_of(dpv_DBP)) |> glimpse()

dpv_DBP_dat <- dat |> select(StudySubjectID, all_of(dpv_DBP)) |>
  pivot_longer(cols = -StudySubjectID,
               names_to = 'visit',
               names_prefix = 'dpv08b_',
               values_to = 'DBP') |>
  mutate(visit = as.numeric(visit),
         DBP = ifelse(DBP == 999, NA, DBP))


#----- Number of tablets returned only on visits with a date dpv16c_# 
dpv_dosage_info <- names(dat)[grep('dpv16c_', names(dat))]
dat |> select(all_of(dpv_dosage_info)) |> glimpse()

dpv_dosage_info_dat <- dat |> select(StudySubjectID, all_of(dpv_dosage_info)) |>
  pivot_longer(cols = -StudySubjectID,
               names_to = 'visit',
               names_prefix = 'dpv16c_',
               values_to = 'dosage_bottle') |>
  mutate(visit = as.numeric(visit),
         dosage_bottle = 84 - dosage_bottle)


#----- Number of tablets estimate only on visits with a date dpv16d_# 
dpv_dosage_est <- names(dat)[grep('dpv16d_', names(dat))]
dat |> select(all_of(ppv_dosage_est)) |> glimpse()

dpv_dosage_est_dat <- dat |> select(StudySubjectID, all_of(dpv_dosage_est)) |>
  pivot_longer(cols = -StudySubjectID,
               names_to = 'visit',
               names_prefix = 'dpv16d_',
               values_to = 'dosage_est') |>
  mutate(visit = as.numeric(visit),
         dosage_est = ifelse(dosage_est == 99, NA, dosage_est))


#----- Does she continue in the study? dpv17_# 
dpv_cont <- names(dat)[grep('dpv17_', names(dat))]
dat$dpv17_1 <- parse_number(as.character(dat$dpv17_1))
dat |> select(all_of(dpv_cont)) |> glimpse()

dpv_cont_dat <- dat |> select(StudySubjectID, all_of(dpv_cont)) |>
  pivot_longer(cols = -StudySubjectID,
               names_to = 'visit',
               names_prefix = 'dpv17_',
               values_to = 'cont_visit') |>
  mutate(visit = as.numeric(visit),
         cont_visit = cont_visit - 1)



#----- Date form completed ppvdat_# 
dpv_com_date <- names(dat)[grep('dpvdat_', names(dat))]
dat |> select(all_of(dpv_com_date)) |> glimpse()


dpv_com_date_dat <- dat |> select(StudySubjectID, all_of(dpv_com_date)) |>
  pivot_longer(cols = -StudySubjectID,
               names_to = 'visit',
               names_prefix = 'dpvdat_',
               values_to = 'complete_date') |>
  mutate(visit = as.numeric(visit),
         complete_date = mdy(complete_date))


#----- Proteinurea on dipstick dpv09_# 
dpv_dipstick <- names(dat)[grep('dpv09_', names(dat))]
dat$dpv09_1 <- parse_number(as.character(dat$dpv09_1))

dat |> select(all_of(dpv_dipstick)) |> glimpse()

dpv_dipstick_dat <- dat |> select(StudySubjectID, all_of(dpv_dipstick)) |>
  pivot_longer(cols = -StudySubjectID,
               names_to = 'visit',
               names_prefix = 'dpv09_',
               values_to = 'dipstick') |>
  mutate(visit = as.numeric(visit))




dpv_dat <- dpv_date_dat |> 
  left_join(dpv_type_dat, by = c("StudySubjectID", "visit"))|> 
  left_join(dpv_shlth_prob_dat, by = c("StudySubjectID", "visit"))|> 
  left_join(dpv_nantihyp_med_dat, by = c("StudySubjectID", "visit"))|> 
  left_join(dpv_SBP_dat, by = c("StudySubjectID", "visit"))|> 
  left_join(dpv_DBP_dat, by = c("StudySubjectID", "visit"))|> 
  left_join(dpv_dosage_info_dat, by = c("StudySubjectID", "visit"))|> 
  left_join(dpv_dosage_est_dat, by = c("StudySubjectID", "visit"))|> 
  left_join(dpv_cont_dat, by = c("StudySubjectID", "visit"))|> 
  left_join(dpv_com_date_dat, by = c("StudySubjectID", "visit"))|> 
  left_join(dpv_dipstick_dat, by = c("StudySubjectID", "visit"))


dpv_dat$ppv = 0
dpv_dat$dpv = 1


glimpse(dpv_dat)
glimpse(ppv_dat)


ppv_dpv_dat <- rbind(ppv_dat, dpv_dat) |>
  arrange(StudySubjectID, dpv, visit)


ppv_dpv_dat <- ppv_dpv_dat |> mutate(
  dosage = ifelse(!is.na(dosage_bottle), dosage_bottle,
                  ifelse(!is.na(dosage_est), dosage_est, NA)),
  dosage = abs(dosage),
  # dosage = ifelse(dosage > 100, 84, dosage)
)


saveRDS(ppv_dpv_dat, "Data/Cleaning/visit_long.RDS")


