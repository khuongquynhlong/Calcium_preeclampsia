library(tableone)
library(lubridate)
library(tidyverse)
library(haven)
library(writexl)

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
dat_daily_frame <- readRDS("Data/Cleaning/daily_frame.RDS")


#===============================================================================
names(outcome_dat)
names(dat_daily_frame)

# merge daily data with outcome data
daily_outcome <- dat_daily_frame |> 
  left_join(outcome_dat, by = c("StudySubjectID", "adm_date", "end_date"))


glimpse(daily_outcome)


# daily_outcome |> filter(pre_eclampsia == 1) |> View()


daily_outcome <- daily_outcome |>
  mutate(
    m_preeclamp = if_else(pre_eclampsia == 0, 0, 
                          if_else(visit_date_sim == pre_eclampsia_date, 1, 
                                  ifelse(visit_date_sim < pre_eclampsia_date, 0, 999))),
    m_preg = if_else(preg_check == 0, 0,
                     if_else(visit_date_sim >= preg_date, 1, 0)),
    m_preg_loss = if_else(early_loss == 0, 0, 
                          if_else(visit_date_sim == early_loss_date, 1, 
                                  ifelse(visit_date_sim < early_loss_date, 0, 999))),
    m_ltfu = if_else(ltfu == 0, 0, 
                          if_else(visit_date_sim == ltfu_date, 1, 
                                  ifelse(visit_date_sim < ltfu_date, 0, 999))),
    m_efu_nopreg = if_else(efu_nopreg == 0, 0, 
                     if_else(visit_date_sim == efu_nopreg_date, 1, 
                             ifelse(visit_date_sim < efu_nopreg_date, 0, 999)))
    )


# Remove nuisance dates
daily_outcome <- daily_outcome |> filter(m_preeclamp < 999 & m_preg_loss < 999 & m_ltfu < 999 & m_efu_nopreg < 999)
daily_outcome$last_id <- as.numeric(!duplicated(daily_outcome$StudySubjectID, fromLast = T))

# Create month index
daily_outcome <- daily_outcome |> mutate(month_since_start = ceiling(day_since_start/28))


monthly_outcome <- daily_outcome |> group_by(StudySubjectID, month_since_start) |>
  summarise(shlth_prob = mean(shlth_prob, na.rm = T),
            nantihyp_med = round(mean(nantihyp_med, na.rm = T)),
            SBP = mean(SBP, na.rm = T),
            DBP = mean(DBP, na.rm = T),
            dosage = round(mean(dosage, na.rm = T)),
            adhere_per = mean(adhere_per, na.rm = T),
            m_preeclamp = mean(m_preeclamp, na.rm = T),
            m_preg = mean(m_preg, na.rm = T),
            m_preg_loss = mean(m_preg_loss, na.rm = T),
            m_ltfu = mean(m_ltfu, na.rm = T),
            m_efu_nopreg = mean(m_efu_nopreg, na.rm = T),
            check_preecl = mean(pre_eclampsia, na.rm = T),
            check_preg = mean(preg_check, na.rm = T),
            check_loss = mean(early_loss, na.rm = T),
            check_ltfu = mean(ltfu, na.rm = T),
            check_efu = mean(efu_nopreg, na.rm = T)) |> 
  ungroup() |>
  mutate(
    m_preeclamp = if_else(m_preeclamp > 0, 1, 0),
    m_preg = if_else(m_preg > 0, 1, 0),
    m_preg_loss = if_else(m_preg_loss > 0, 1, 0),
    m_ltfu = if_else(m_ltfu > 0, 1, 0),
    m_efu_nopreg = if_else(m_efu_nopreg > 0, 1, 0)) |>
  mutate_at(c("shlth_prob", "nantihyp_med", "SBP", "DBP", "dosage", "adhere_per",
              "m_preeclamp", "m_preg", "m_preg_loss", "m_ltfu", "m_efu_nopreg"),
            function(x){ifelse(is.nan(x), NA, x)}) |>
  group_by(StudySubjectID) |>
  mutate(month_since_preg = cumsum(m_preg)) |> 
  ungroup()



# Check and remove observations with more than 10*4 weeks of pregnancy
monthly_outcome <- monthly_outcome |>
  mutate(
    flag_preecl = if_else(month_since_preg > 10, 1, 0),
    m_preeclamp = ifelse(month_since_preg != 10, m_preeclamp,
                         ifelse(check_preecl == 1, 1, 0)),
    flag_efu = if_else(month_since_start > 60 & check_efu == 1, 1, 0),
    m_efu_nopreg = ifelse(month_since_start != 60, m_efu_nopreg, 
                          ifelse(check_efu == 1, 1, 0)))

# Censored 36 month if no pregnancy or ltfu
# monthly_outcome <- monthly_outcome |>
#   mutate(
#     flag_preecl = if_else(month_since_preg > 10, 1, 0),
#     m_preeclamp = ifelse(month_since_preg != 10, m_preeclamp,
#                          ifelse(check_preecl == 1, 1, 0)),
#     flag_efu = if_else(month_since_start > 60 & check_efu == 1, 1, 0),
#     m_efu_nopreg = ifelse(month_since_start != 60, m_efu_nopreg, 
#                           ifelse(check_efu == 1, 1, 0)))



monthly_outcome <- monthly_outcome |> filter(flag_preecl == 0 & flag_efu == 0)
monthly_outcome$last_id <- as.numeric(!duplicated(monthly_outcome$StudySubjectID, fromLast = T))


monthly_outcome <- monthly_outcome |> 
  select(-c(flag_preecl, flag_efu))

glimpse(monthly_outcome)


# Fix loss to follow-up

monthly_outcome <- monthly_outcome |>
  mutate(
    m_ltfu = if_else(check_ltfu == 1 & last_id == 1 & m_ltfu == 0, 1, m_ltfu)
  )


# Merge with baseline data
monthly_outcome_bs <- monthly_outcome |> left_join(bs_dat, by = "StudySubjectID")

glimpse(monthly_outcome_bs)

monthly_outcome_bs$StudySubjectID |> unique() |> length()
monthly_outcome_bs[monthly_outcome_bs$trt_grp == "Calcium",]$StudySubjectID |> unique() |> length()
monthly_outcome_bs[monthly_outcome_bs$trt_grp == "Placebo",]$StudySubjectID |> unique() |> length()



monthly_outcome_bs |> filter(month_since_start > 60) |> nrow()


# Check
monthly_outcome_bs_last = monthly_outcome_bs |> filter(last_id == 1)
table(monthly_outcome_bs_last$m_preeclamp, exclude = F)
table(monthly_outcome_bs_last$m_preg, exclude = F)
table(monthly_outcome_bs_last$m_preg_loss, exclude = F)
table(monthly_outcome_bs_last$m_ltfu, exclude = F)
table(monthly_outcome_bs_last$m_efu_nopreg, exclude = F)


# writexl::write_xlsx(monthly_outcome, "Data/Cleaning/monthly_outcome.xlsx")
# write_dta(monthly_outcome, "Data/Cleaning/test.dta")



# Create time-varying treatment
#===============================================================================
# m_preg_indi <- monthly_outcome_bs |> filter(month_since_preg == 1) |> 
#   select(StudySubjectID, month_since_start) |>
#   rename(m_preg_indi = month_since_start)
# 
# 
# monthly_outcome_bs <- monthly_outcome_bs |> 
#   left_join(m_preg_indi, by = "StudySubjectID") |>
#   mutate(
#     month_since_preg2 = month_since_start - m_preg_indi,
#     trt_varying3 = trt_grp,
#     trt_varying3 = if_else(month_since_preg >5, "UsualCare", trt_varying)
#   )


monthly_outcome_bs <- monthly_outcome_bs |> 
  mutate(
    trt_varying2 = trt_grp,
    trt_varying3 = trt_grp,
    trt_varying2 = if_else(month_since_preg >5, "Calcium", trt_varying2),
    trt_varying3 = if_else(month_since_preg >5, "UsualCare", trt_varying3)
  )

# monthly_outcome_bs |> filter(check_preg == 1) |>
#   select(StudySubjectID, month_since_start, month_since_preg, m_preg, check_preg,
#          trt_grp, trt_varying2, trt_varying3) |> View()



saveRDS(monthly_outcome_bs, "Data/Cleaned data/monthly_data_preecl.RDS")





























