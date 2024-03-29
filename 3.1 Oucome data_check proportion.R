library(tableone)
library(lubridate)
library(tidyverse)
library(haven)
# Define not in operator
`%!in%` = Negate(`%in%`)


load("Data/TrialData.RData")


dat <- dat_all



dat <- dat |>
  mutate(
    adm_date         = mdy(adm01),
    scr_date         = mdy(scr01),
    min_date         = min(adm_date))


    
#---------- Endpoints
#===============================================================================
#----- Pre-eclampsia
#-------------------------------------------------------------------------------
table(analysis_all$del22, analysis_all$trt_grp, exclude = F)
# Compare with publish paper, this participant had pre-eclampsia
del_mis_ID <- analysis_all$StudySubjectID[is.na(analysis_all$del22)]

dat$del22[dat$StudySubjectID == del_mis_ID] <- "2 - Yes"
table(dat$del22, dat$trt_grp, exclude = F) # OK

dat <- dat  |>
  mutate(
    del22 = parse_number(as.character(del22)),
    del22 = del22 - 1,
    indi = if_else(is.na(mdy(del25a)), NA, 1),
    del25a = if_else(del22 == 1 & is.na(indi), del06a, del25a),
    del06a = mdy(del06a),
    del25a = mdy(del25a),
    eos01 = mdy(eos01)) |>
  rename(
    ga_week = del05a,
    ga_day = del05b,
    del_date = del06a,
    preg_out = del10,
    pre_eclampsia = del22,
    pre_eclampsia_date = del25a,
    end_date = eos01,
    end_status = eos02a,
  ) |> select(-c(indi))


dat <- dat |> mutate(pre_eclampsia = ifelse(is.na(pre_eclampsia), 0, pre_eclampsia))


table(dat$pre_eclampsia, dat$trt_grp, exclude = F)


# Pregnancy
dat <- dat |> mutate(pregnancy = ifelse(subject_preg == "Pregnant", 1, 0),
                     preg_date = del_date - (ga_week*7 + ga_day),
                     day_to_pregnancy = preg_date - adm_date)

table(dat$pregnancy, dat$trt_grp, exclude = F)

# dat |> select(StudySubjectID, adm01, trt_grp, ga_week, ga_day, del_date,
#               preg_out, subject_preg, pregnancy, preg_date, day_to_pregnancy, timeinstudyuntilpreg, end_date) |>
#   View()



#----- Early pregnancy loss/ abortion (before 20 weeks)
#-------------------------------------------------------------------------------
ids_conc_excl$id.miss.less20_c |> length()
ids_conc_excl$id.miss.less20_p |> length()
ids_conc_excl$id.term.less20_c |> length()
ids_conc_excl$id.term.less20_p |> length()

ids_pregloss <- c(ids_conc_excl$id.miss.less20_c, ids_conc_excl$id.miss.less20_p,
                  ids_conc_excl$id.term.less20_c, ids_conc_excl$id.term.less20_p)

dat <- dat |>
  mutate(
    early_loss = ifelse(StudySubjectID %in% ids_pregloss, 1, 0),
    early_loss_date = dplyr::if_else(early_loss == 1, del_date, NA),
    early_loss_date = dplyr::if_else(early_loss == 1 & is.na(early_loss_date), end_date, early_loss_date),
  )


table(dat$early_loss, dat$trt_grp, exclude = F)


# Livebirth
dat <- dat |> mutate(live_birth = ifelse(preg_out == "1 - Live birth", 1, 0),
                     live_birth = ifelse(is.na(live_birth), 0, live_birth))

table(dat$live_birth, dat$trt_grp, exclude = F)



# Early-onset pre-eclampsia  (<32 weeks’ gestation)

dat |> select(ga_week, ga_day, pre_eclampsia, pre_eclampsia_date) |>
  filter(pre_eclampsia == 1) |> View()


dat <- dat |> mutate(
  early_pre_eclampsia = ifelse(pre_eclampsia == 1 & ga_week < 32, 1, 0)
)


table(dat$early_pre_eclampsia, dat$trt_grp, exclude = F)

# Eclampsia

dat <- dat |> mutate(
  eclampsia = ifelse(del23 == "2 - Yes", 1, 0),
  eclampsia = ifelse(is.na(eclampsia), 0, eclampsia)
)

table(dat$eclampsia, dat$trt_grp, exclude = F)


# Placental abruption
dat <- dat |> mutate(
  place_ab = ifelse(del16 == "2 - Yes", 1, 0),
  place_ab = ifelse(is.na(place_ab), 0, place_ab)
)

table(dat$place_ab, dat$trt_grp, exclude = F)


# HELLP syndrome
dat <- dat |> mutate(
  HELLP = ifelse(del35 == "2 - Yes", 1, 0),
  HELLP = ifelse(is.na(HELLP), 0, HELLP)
)

table(dat$HELLP, dat$trt_grp, exclude = F)



# Severe gestational hypertension
dat <- dat  |>
  rename(
    highest_SBP = del01a,
    highest_DBP = del01b,
    highest_urea = del02,
  ) |>
  mutate(
    highest_SBP = ifelse(highest_SBP == 999, NA, highest_SBP),
    highest_DBP = ifelse(highest_DBP == 999, NA, highest_DBP),
    highest_urea = parse_number(as.character(highest_urea)),
    highest_urea = ifelse(highest_urea == 9, NA, highest_urea)
  )

dat |> select(highest_SBP, highest_DBP, highest_urea, end_status, pre_eclampsia) |> filter(pre_eclampsia ==1) |> View()

dat <- dat |> mutate(
  severe_hypertension = ifelse((highest_SBP >=160 | highest_DBP >= 110) & pregnancy==1, 1, 0),
  severe_hypertension = ifelse(is.na(severe_hypertension), 0, severe_hypertension)
)

table(dat$severe_hypertension, dat$trt_grp, exclude = F)

# Severe pre-eclampsia
dat <- dat |> mutate(
  severe_pre_eclampsia = ifelse(pre_eclampsia == 1 & severe_hypertension ==1, 1, 0)
)

table(dat$severe_pre_eclampsia, dat$trt_grp, exclude = F)


# Severe pre-eclamptic complications index
#----------
dat <- dat |> mutate(
  sev_preecl_index = ifelse(severe_pre_eclampsia == 1| early_pre_eclampsia == 1| eclampsia == 1|
                              place_ab== 1| HELLP== 1| severe_hypertension== 1, 1, 0)
)

table(dat$sev_preecl_index, dat$trt_grp, exclude = F)





# Admission to ICU or any special care unit
dat <- dat |> mutate(
  ICU24 = ifelse(del17 == "2 - Yes", 1, 0),
  ICU24 = ifelse(is.na(ICU24), 0, ICU24)
)

table(dat$ICU24, dat$trt_grp, exclude = F)

# Eclampsia
table(dat$eclampsia, dat$trt_grp, exclude = F)

# Placental abruption
table(dat$place_ab, dat$trt_grp, exclude = F)

# HELLP syndrome
table(dat$HELLP, dat$trt_grp, exclude = F)

# Renal failure
dat <- dat  |>
  mutate(
    del28 = ifelse(del28 == 9999, NA, del28),
    renal_fail = ifelse(del28 > 120, 1, 0),
    renal_fail = ifelse(is.na(renal_fail), 0, renal_fail)
  )

table(dat$renal_fail, dat$trt_grp, exclude = F)


# Maternal death
dat <- dat |> mutate(
  mat_death = ifelse(del37a == "3 - Died" | tel02a == "3 - Died", 1, 0),
  mat_death = ifelse(is.na(mat_death), 0, mat_death)
)

table(dat$mat_death, dat$trt_grp, exclude = F)

# Severe maternal morbidity and mortality index
dat <- dat |> mutate(
  sev_morb_morta_index = ifelse(ICU24 == 1| eclampsia == 1| mat_death ==1|
                              place_ab== 1| HELLP== 1| renal_fail== 1, 1, 0),
  sev_morb_morta_index = ifelse(is.na(sev_morb_morta_index), 0, sev_morb_morta_index)
)

table(dat$sev_morb_morta_index, dat$trt_grp, exclude = F)



out_new <- c("pre_eclampsia", "pregnancy", "early_loss", "live_birth", 
             "sev_preecl_index", "severe_pre_eclampsia", "early_pre_eclampsia", 
             "eclampsia", "place_ab", "HELLP", "severe_hypertension", "ICU24", 
            "sev_morb_morta_index", "mat_death", "renal_fail")


df_des <- dat |> select("trt_grp", all_of(out_new)) |>
  mutate_at(out_new, function(x){factor(x, levels = c(0, 1), labels = c("No", "Yes"))})

tab_m <- CreateTableOne(vars = c(out_new), 
                        strata = "trt_grp", 
                        data = df_des, test = FALSE)
print(tab_m, smd = TRUE)



dat <- dat |> mutate(trt_grp01 = ifelse(trt_grp == "Calcium", 1, 0))



for (i in out_new) {
  a <- paste0("m_", i)
  eval(call("<-", as.name(a), 
            glm(get(i) ~ trt_grp, family = binomial(link = "log"), data = dat)
  ))
}



coef_tab <- rbind(summary(m_pre_eclampsia)$coefficients, 
                  summary(m_pregnancy)$coefficients,
                  summary(m_early_loss)$coefficients,
                  summary(m_live_birth)$coefficients,
                  summary(m_sev_preecl_index)$coefficients,
                  summary(m_severe_pre_eclampsia)$coefficients,
                  summary(m_early_pre_eclampsia)$coefficients,
                  summary(m_eclampsia)$coefficients,
                  summary(m_place_ab)$coefficients,
                  summary(m_HELLP)$coefficients,
                  summary(m_severe_hypertension)$coefficients,
                  summary(m_ICU24)$coefficients,
                  summary(m_sev_morb_morta_index)$coefficients,
                  summary(m_mat_death)$coefficients,
                  summary(m_renal_fail)$coefficients) |> 
  as_tibble(rownames = "Parameter") |>
  filter(Parameter != "(Intercept)") |>
  mutate(RR = round(exp(Estimate), 2),
         lb = round(exp(Estimate - 1.96*`Std. Error`), 2),
         ub = round(exp(Estimate + 1.96*`Std. Error`), 2),
         CI95 = paste0(RR, " (",lb, " - ", ub, ")"))


coef_tab$Parameter <- out_new

