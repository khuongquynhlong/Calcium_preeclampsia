library(tableone)
library(lubridate)
library(tidyverse)
library(haven)
# Define not in operator
`%!in%` = Negate(`%in%`)

# Baseline data
bs_dat <- readRDS("Data/Cleaning/baseline_charac.RDS")


load("Data/TrialData.RData")



dat <- dat_all


#---------- Endpoints
#===============================================================================


#----- Pre-eclampsia
#-------------------------------------------------------------------------------
table(analysis_all$del22, analysis_all$trt_grp, exclude = F)
# Compare with publish paper, this participant had pre-eclampsia
del_mis_ID <- analysis_all$StudySubjectID[is.na(analysis_all$del22)]

dat$del22[dat$StudySubjectID == del_mis_ID] <- "2 - Yes"
table(dat$del22, dat$trt_grp, exclude = F) # OK


# dat |> select(StudySubjectID, del06a, del10, del11, del12, del13, del15) |> View()

outcome_dat <- dat |> select(StudySubjectID, trt_grp, del05a, del05b, del06a, del10,
                             del22, del25a, eos01, eos02a) |>
  mutate(
    del22 = parse_number(as.character(del22)),
    del22 = del22 - 1,
    indi = ifelse(is.na(mdy(del25a)), NA, 1),
    del25a = ifelse(del22 == 1 & is.na(indi), del06a, del25a),
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


# outcome_dat |> filter(pre_eclampsia == 1) |> View()

table(outcome_dat$pre_eclampsia, outcome_dat$trt_grp, exclude = F) # OK




#----- Double check pregnancy
#-------------------------------------------------------------------------------
table(outcome_dat$preg_out, exclude = F)  # 643 had pregnancy  ==> missing 8, must check


ids_noconc$id_noconc_c |> length()
ids_noconc$id_noconc_p |> length()

ids_lf$id_lf_conc_c  |> length()
ids_lf$id_lf_conc_p  |> length()
ids_lf$id_lf_withd_c |> length()
ids_lf$id_lf_withd_p |> length()

# ID without pregnancy
ids_nopreg <- c(ids_noconc$id_noconc_c, ids_noconc$id_noconc_p,
                ids_lf$id_lf_conc_c, ids_lf$id_lf_conc_p,
                ids_lf$id_lf_withd_c, ids_lf$id_lf_withd_p)

outcome_dat <- outcome_dat |>
  mutate(
    preg_check = ifelse(StudySubjectID %in% ids_nopreg, 0, 1)
  )

table(outcome_dat$preg_check, outcome_dat$trt_grp, exclude = F) # OK
outcome_dat |> filter(preg_check == 0 & !is.na(preg_out)) |> nrow() # OK

# 8 cases with pregnancy but had no information on GA or pregnancy outcome
ids_preg_noinfor <- outcome_dat |> filter(preg_check == 1 & is.na(preg_out)) |> pull(StudySubjectID)


#----- Create date of conception
outcome_dat <- outcome_dat |>
  mutate(
    preg_date = del_date - (ga_week*7 + ga_day)
  )


# outcome_dat |> filter(preg_check == 1) |> View()



#----- Early pregnancy loss/ abortion (before 20 weeks)
#-------------------------------------------------------------------------------
ids_conc_excl$id.miss.less20_c |> length()
ids_conc_excl$id.miss.less20_p |> length()
ids_conc_excl$id.term.less20_c |> length()
ids_conc_excl$id.term.less20_p |> length()

ids_pregloss <- c(ids_conc_excl$id.miss.less20_c, ids_conc_excl$id.miss.less20_p,
                  ids_conc_excl$id.term.less20_c, ids_conc_excl$id.term.less20_p)

outcome_dat <- outcome_dat |>
  mutate(
    early_loss = ifelse(StudySubjectID %in% ids_pregloss, 1, 0),
    early_loss_date = dplyr::if_else(early_loss == 1, del_date, NA),
    early_loss_date = dplyr::if_else(early_loss == 1 & is.na(early_loss_date), end_date, early_loss_date),
  )


table(outcome_dat$early_loss, outcome_dat$trt_grp, exclude = F)

outcome_dat |> filter(preg_check == 0 & early_loss == 1) |> nrow() # OK


#----- Withdrawal/loss to follow-up before 20 weeks
#-------------------------------------------------------------------------------
ids_lf$id_lf_conc_c |> length()
ids_lf$id_lf_conc_p |> length()
ids_lf$id_lf_withd_c |> length()
ids_lf$id_lf_withd_p |> length()
ids_conc_excl$id.lf.less20_c |> length()
ids_conc_excl$id.lf.less20_p |> length()
ids_conc_excl$id.discp_p |> length()
ids_conc_excl$id.lf.more20_c |> length()


table(outcome_dat$end_status, outcome_dat$trt_grp, exclude = F)


ids_loss_early <- c(ids_lf$id_lf_conc_c, ids_lf$id_lf_conc_p,
                    ids_lf$id_lf_withd_c, ids_lf$id_lf_withd_p,
                    ids_conc_excl$id.lf.less20_c, ids_conc_excl$id.lf.less20_p,
                    ids_conc_excl$id.discp_p, ids_conc_excl$id.lf.more20_c
                    )

outcome_dat <- outcome_dat |>
  mutate(
    ltfu = ifelse(StudySubjectID %in% ids_loss_early, 1, 0),
    ltfu_date = dplyr::if_else(ltfu == 1, end_date, NA),
  )

# outcome_dat |> filter(ltfu == 1) |> View()|> View()

table(outcome_dat[outcome_dat$ltfu==1,]$end_status, 
      outcome_dat[outcome_dat$ltfu==1,]$trt_grp, exclude = F) # Ok



#----- End of follow-up without pregnancy
#-------------------------------------------------------------------------------
ids_end_nopreg <- c(ids_noconc$id_noconc_c, ids_noconc$id_noconc_p)


outcome_dat <- outcome_dat |>
  mutate(
    efu_nopreg = ifelse(StudySubjectID %in% ids_end_nopreg, 1, 0),
    efu_nopreg_date = dplyr::if_else(efu_nopreg == 1, end_date, NA),
  )


table(outcome_dat[outcome_dat$efu_nopreg==1,]$end_status, 
      outcome_dat[outcome_dat$efu_nopreg==1,]$trt_grp, exclude = F) # Ok



# Check again

outcome_dat$pre_eclampsia[is.na(outcome_dat$pre_eclampsia)] <- 0

table(outcome_dat$pre_eclampsia, exclude = F)
table(outcome_dat$preg_check, exclude = F)
table(outcome_dat$early_loss, exclude = F)
table(outcome_dat$ltfu, exclude = F)
table(outcome_dat$efu_nopreg, exclude = F)


# outcome_dat |> filter(pre_eclampsia == 1 | preg_check == 1 | early_loss == 1 |
#                         ltfu == 1 | efu_nopreg == 1) |> View()





# Refine all data
#-------------------------------------------------------------------------------
# Case 4460
outcome_dat <- outcome_dat |>
  mutate(preg_date = if_else(StudySubjectID == "4460", early_loss_date - (ga_week*7 - ga_day), preg_date))


outcome_dat <- outcome_dat |> 
  left_join(bs_dat |> select(StudySubjectID, adm_date, scr_date), by = "StudySubjectID")


outcome_dat <- outcome_dat |> select(StudySubjectID, adm_date, scr_date, names(outcome_dat))


outcome_dat <- outcome_dat |>
  mutate(preg_date = if_else(preg_check == 1 & is.na(preg_date), scr_date + as.numeric((end_date - scr_date)/2), preg_date))


# Add 09/03/2024
outcome_dat <- outcome_dat |>
  mutate(ltfu_date = if_else(ltfu == 1, scr_date + as.numeric((end_date - scr_date)/2), NA))


# ltfuid <- outcome_dat |> filter(end_status == "5 - Lost to follow-up") |> pull(StudySubjectID)

# outcome_dat |> filter(StudySubjectID %in% c("5302", "5627", "4512", "5285", "5328", "5445", "5322", "5466")) |> View()

# outcome_dat |> filter(pre_eclampsia == 1) |> View()
# outcome_dat |> filter(preg_check == 1) |> View()
# outcome_dat |> filter(early_loss == 1) |> View()
# outcome_dat |> filter(ltfu == 1) |> View()
# outcome_dat |> filter(efu_nopreg == 1) |> View()


# Correct
table(outcome_dat$pre_eclampsia, exclude = F)
table(outcome_dat$preg_check, exclude = F)
table(outcome_dat$early_loss, exclude = F)
table(outcome_dat$ltfu, exclude = F)
table(outcome_dat$efu_nopreg, exclude = F)




saveRDS(outcome_dat, "Data/Cleaning/outcome_preeclampsia.RDS")






