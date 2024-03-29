library(foreign)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

dat <- read.spss("/Users/ernestou/Library/CloudStorage/Box-Box/CAP Study/23_Dec_2017_10_32_03_complete_CRFs_unblinded_labeled_2.sav",
                 to.data.frame=TRUE)
attributes(dat)

# In the paper, from 2563 women assessed for eligibility, we have 1216 excluded, 1347 eligible, 
# However, 1355 were randomized, 678 to treatment and 677 allocated to placebo

table(dat$trt_grp, useNA = "always")

# Placebo Calcium  <NA> 
#  677     678    1208 

table(dat$adm13, useNA = "always" )

# Subset to those who were randomized
############################################################################ 

# reproducing diagram in paper
# 1) we divide the data into those in calcium and placebo
dat_c <- dat %>% filter(trt_grp == "Calcium") 
dat_p <- dat %>% filter(trt_grp == "Placebo") 

# 2 a) follow-up in calcium group 
table(dat_c$eos02a)
# 2 a) follow-up in placebo group 
table(dat_p$eos02a)

# 149 completed study without pregnancy in calcium arm (matches paper)
id_noconc_c <- dat_c$StudySubjectID[dat_c$eos02a == "2 - Completed study without pregnancy"]
# 167 completed study without pregnancy in placebo arm (matches paper)
id_noconc_p <- dat_p$StudySubjectID[dat_p$eos02a == "2 - Completed study without pregnancy"]

# 41 withdrew before conception in calcium arm (matches paper)
id_lf_withd_c <- dat_c$StudySubjectID[dat_c$eos02a == "4 - Discontinued while not pregnant"]

# 27 withdrew before conception in placebo arm (matches paper)
id_lf_withd_p <- dat_p$StudySubjectID[dat_p$eos02a == "4 - Discontinued while not pregnant"]

# to obtain 157 (calcium) and 163 (placebo) lost to follow-up before conception 
# we cross tabulate eso02a option 5 and subject_preg 
table(dat_c$eos02a, dat_c$subject_preg, useNA = "always")
table(dat_p$eos02a, dat_p$subject_preg, useNA = "always")

id_lf_conc_c <- dat_c$StudySubjectID[dat_c$eos02a == "5 - Lost to follow-up" 
                                      & dat_c$subject_preg == "Not pregnant"]

id_lf_conc_p <- dat_p$StudySubjectID[dat_p$eos02a == "5 - Lost to follow-up" 
                                      &  dat_p$subject_preg == "Not pregnant"]


# Number of women who got conceived during study (matches paper) 
table(dat_c$subject_preg, useNA = "always")
# Not pregnant     #Pregnant 
# 347               331        
table(dat_p$subject_preg)
# Not pregnant     #Pregnant 
# 357               320 

# 3) subset to those who conceived
dat_c_c <- dat_c %>% filter(subject_preg == 'Pregnant')
dat_p_c <- dat_p %>% filter(subject_preg == 'Pregnant')

# 3 a) Reproduce exclusion criteria among those who conceived in calcium arm
############################################################################ 

##### Lost to follow up before and after 20 weeks
##################################################

# lost to follow up before 20 weeks in the calcium arm
lf_c <- dat_c_c %>% filter(eos02a == "5 - Lost to follow-up" )

# first obs in lf_c has last visit at 20 weeks, then there are no more visits
# we can use their id to obtain the right individuals from the flow chart
table(lf_c$dpv01_1)
table(lf_c$dpv01_2, useNA = 'always')

##### Lost to follow before 20 weeks (1 participant)

## Store id of participant who conceived and was lost to follow up before 20 weeks  
id.lf.less20_c <- lf_c$StudySubjectID[is.na(lf_c$dpv01_2)]

## Store id of participants who conceived and were lost to follow up after 20 weeks 
id.lf.more20_c <-  lf_c$StudySubjectID[!is.na(lf_c$dpv01_2)]

##### Pregnancy loss and abortion before 20 weeks
##################################################

# using del10 we can obtain pregnancy loss before 20 weeks (miscarriage)
# however there are 31 miscarriages but on the paper it says 27 
# 4 of them need to be after 20 weeks 
table(dat_c_c$del10, useNA = "always")

# Subset data
mis_c <- dat_c_c %>% filter(del10 == "2 - Miscarriage")

# If we use miscarriage and best gestational age at time of miscarriage 
# we obtain the resulting 27 pregnancy losses before 20 weeks by leaving out 
# 4 observations that are 20 weeks or afterwards 
table(mis_c$del05a)

id.miss.less20_c <- mis_c$StudySubjectID[mis_c$del05a < 20]

# Now we try to find the 5 observations who terminated pregnancy 
# (assuming this is the same as requested abortion) before 20 weeks
term_c <- dat_c_c %>% filter(del10 == "5 - Termination of pregnancy")
table(term_c$del05a)

# we see all 5 were before 20 weeks, hence we use their subject ids
id.term.less20_c <- term_c$StudySubjectID

# Now we look at reason for intervention
table(term_c$del08b, useNA = 'always')
table(term_c$del08c1, useNA = 'always')

##############################
## Discontinued while pregnant
##############################

# 0 Discontinued while pregnant in calcium arm (matches paper)
id.discp_c <- dat_c$StudySubjectID[dat_c$eos02a == "3 - Discontinued while pregnant"]

## Subset Data
##############################

### Subset data to those who were followed up for more than 20 weeks
ids.excluded.20_c <- c(id.lf.less20_c, id.lf.more20_c, id.miss.less20_c, id.term.less20_c)

### Finally we subset data to those who don't have loss to follow up 
analysis_c <- dat_c_c %>% filter(! StudySubjectID %in% ids.excluded.20_c)

# 3 b) Reproduce exclusion criteria among those who conceived in placebo arm
############################################################################ 

##### Lost to follow up before and after 20 weeks
##################################################

# lost to follow up before 20 weeks in the placebo arm
lf_p <- dat_p_c %>% filter(eos02a == "5 - Lost to follow-up" )

# All obs in lf_p had last visit at 8 weeks, then there are no more visits
# we can use their id to obtain the 4 individuals from the flow chart
id.lf.less20_p <- lf_p$StudySubjectID

##### Pregnancy loss and abortion before 20 weeks
##################################################

# We have 32 miscarriages in the placebo arm 
table(dat_p_c$del10, useNA = "always")
mis_p <- dat_p_c %>% filter(del10 == "2 - Miscarriage")

# If we use miscarriage and best gestational age at time of miscarriage 
# we obtain the resulting 27 pregnancy losses before 20 weeks by leaving out 
# 5 observations that are 20 weeks or afterwards 
table(mis_p$del05a, mis_p$del05b)
id.miss.less20_p <- mis_p$StudySubjectID[mis_p$del05a < 20]

# Now we try to find the 5 observations who requested abortion before 20 weeks
term_p <- dat_p_c %>% filter(del10 == "5 - Termination of pregnancy")

# we see 6 were before or at 20 weeks, one more than the paper 
table(term_p$del05a, term_p$del05b)

### Woman request is not sufficiently informative because there are 4, and we need 5 observations
table(term_p$del08b, useNA = 'always')

# Other variables related to ending pregnancy but do not help retrieving the 5 from the paper
# table(term_p$del08a)
# table(term_p$del08b, useNA = 'always')
# table(term_p$del08c2, useNA = 'always')

# Because there is no other information, 
# I assume that 19 weeks and 5 days was rounded to more than 20 weeks 
# and remove the observation in the hopes to get the same results later on 
# I argue that this makes most sense because all of the 
# cutoffs are time dependent and all are at 20 weeks. 
id.term.less20_p <- term_p$StudySubjectID[term_p$del05a <= 14]

##############################
## Discontinued while pregnant
##############################

# Using eos02a we can obtain discontinued while pregnant (matches numbers)
# lost to follow-up matches numbers in this case, and agrees with the next 0 in 
# the flow chart 

id.discp_p <- unique(dat_p_c$StudySubjectID[which(dat_p_c$eos02a == "3 - Discontinued while pregnant")])

##############################
## Subset Data
##############################

### Subset data to those who were followed up for more than 20 weeks
ids.less.20_p <- c(id.lf.less20_p, id.miss.less20_p, id.term.less20_p, id.discp_p)

follow.up.more.20_p <- dat_p_c %>% filter(! StudySubjectID %in% ids.less.20_p)

### No one had lost to follow up after 20 weeks in placebo arm 
analysis_p <- follow.up.more.20_p 

##############################
## Store Information
##############################

# dat_c = those randomized to calcium
# dat_p = those randomized to placebo

# follow.up.more.20_c = those who conceived in calcium arm 
## and pregnancy was not terminated & followed at least 20 weeks after gestation

# follow.up.more.20_p = those who conceived in placebo arm
## and pregnancy was not terminated & followed at least 20 weeks after gestation

# analysis_c = those who conceived in calcium arm, 
#              and were not lost to follow up after 20 weeks
# analysis_p = those who conceived in placebo arm, 
#              and were not lost to follow up after 20 weeks

# id_lf_conc_c = study id of participants who were lost to follow up 
#                 before conception in calcium arm
# id_lf_conc_p = study id of participants who were lost to follow up 
#                 before conception in placebo arm 

# id_lf_withd_c = study id of participants who withdrew before 
#                  conception in calcium arm 
# id_lf_withd_p = study id of participants who withdrew before 
#                  conception in placebo arm

# id.discp_p = discontinued while pregnant in the placebo arm 

# id_noconc_c = study id of participants who did not conceive during the study 
# id_noconc_p = study id of participants who did not conceive during the study 

#### Ids of participants who conceived and were excluded from analysis
# id.lf.less20_c = conceived and was lost to follow up before 20 weeks in calcium arm  
# id.lf.more20_c = conceived and were lost to follow up after 20 weeks in calcium arm  
# id.term.less20_c = id pregnancy loss before 20 weeks in calcium arm 
# id.discp_c = discontinued while pregnant in calcium arm 
# id.term.less20_c = abortion in calcium arm 

# id.lf.less20_p = conceived and was lost to follow up before 20 weeks  in the placebo arm 
# id.lf.more20_p = not included because there are no IDs here
# id.term.less20_p = id pregnancy loss before 20 weeks in the placebo arm
# id.discp_p = discontinued while pregnant in the placebo arm
# id.term.less20_p = abortion in placebo arm 


# pregnancy loss
id.miss.less20_c <- mis_c$StudySubjectID[mis_c$del05a < 20]

# abortion 
id.term.less20_c

# lost to follow up before conception or withdrew before conception 
ids_lf = list(id_lf_conc_c = id_lf_conc_c, id_lf_conc_p = id_lf_conc_p,
              id_lf_withd_c = id_lf_withd_c, id_lf_withd_p = id_lf_withd_p)

# participants who did not conceive during the study            
ids_noconc = list(id_noconc_c = id_noconc_c, id_noconc_p = id_noconc_p)

# participants who conceived but were lost to follow up, had pregnancy loss, aborted, or discontinued while pregnant
ids_conc_excl <- list(id.lf.less20_c = id.lf.less20_c, 
                      id.lf.more20_c = id.lf.more20_c,
                      id.discp_c = id.discp_c, 
                      id.miss.less20_c = id.miss.less20_c,
                      id.term.less20_c = id.term.less20_c,
                      id.lf.less20_p = id.lf.less20_p, 
                      id.term.less20_p = id.term.less20_p,
                      id.miss.less20_p = id.miss.less20_p,
                      id.discp_p = id.discp_p)

analysis_all <- rbind(analysis_c, analysis_p)
dat_all <- rbind(dat_c, dat_p)

#table(analysis_c$adm13, useNA = "always" )
#table(analysis_p$adm13, useNA = "always" )
#prop.table(table(analysis_c$adm13, useNA = "always" ))
#prop.table(table(analysis_p$adm13, useNA = "always" ))

save(dat_all,
     analysis_all,
     ids_lf, 
     ids_noconc,
     ids_conc_excl,
     file = "Data/TrialData.RData")


