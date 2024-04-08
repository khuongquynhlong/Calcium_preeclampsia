library(tidyverse)
library(mice)


# Original data
load("Data/Imputed data/imp_pre.RData")

impdf <- complete(imp, "long")

# PP
load("Data/Result data/allImpute_preecl_PP.RData")

# NC
nc_df <- readRDS("Data/Result data/allImpute_preecl_NC.RDS")

# ITT
load("Data/Result data/allImpute_ITT.RData")


#-------------------------------------------------------------------------------
#----------  Set up theme
#===============================================================================
mytheme <- function(...) {
  theme_classic() +
    theme(
      plot.title = element_text(size = 12,color = "grey10",  face = "bold", hjust = 0.5),
      axis.line = element_line(linetype = "solid"),
      axis.text = element_text(color = "gray10", size = 8),
      axis.title = element_text(color = "gray10", size = 10),
      plot.background = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      legend.title = element_text(size = 10, face = "bold"),
      legend.direction = "horizontal",
      legend.position = "top",
      legend.background = element_rect(fill = NA, color = NA),
      legend.text = element_text(size = 10),
      legend.key.width = unit(1, "line"),
      strip.text = element_text(size = 10, face = "bold"),
      strip.background = element_rect(fill = NA, color = NA)
    )
}




#----- Compare simulated values to the actual values
#===============================================================================
# Create month of getting pregnancy only, not cumulative pregnancy
# Create indicator for imputation sets
id_imp <- cbind(id_sim = 0:99999, imp_id = rep(1:10, each = 10000)) |>
  as.data.frame()

nc_df2 <- nc_df |> mutate(id_sim = cumsum(last),
                          id_sim = ifelse(last==1, id_sim - 1, id_sim)) |>
  left_join(id_imp, by = "id_sim")


nc_df2 <- nc_df2 |> group_by(imp_id, id) |>
  mutate(Zp_cum = cumsum(Zp)) |>
  ungroup() |>
  mutate(Zp = if_else(Zp_cum > 1, 0, Zp))

# summary
out_pp_sum <- nc_df2 |> 
  group_by(mm) |>
  summarise(T1_sim = mean(T1p),
            T2_sim = mean(T2p),
            T3_sim = mean(T3p),
            X_sim = mean(Xp),
            Z_sim = mean(Zp),
            C_sim = mean(Cp),
            S_sim = mean(Sp),
            D_sim = mean(Dp),
            Y_sim = mean(Yp))|>
  ungroup()


impdf$X <- ifelse(impdf$adhere_per > 0.8, 1, 0)


impdf2 <- impdf |> group_by(.imp, ID) |>
  mutate(m_preg_cum = cumsum(m_preg)) |>
  ungroup() |>
  mutate(m_preg = if_else(m_preg_cum > 1, 0, m_preg))


ori_pp_sum <- impdf2 |> 
  group_by(month_since_start) |>
  summarise(T1_ori = mean(shlth_prob),
            T2_ori = mean(SBP),
            T3_ori = mean(DBP),
            X_ori = mean(X),
            Z_ori = mean(m_preg),
            C_ori = mean(m_ltfu),
            S_ori = mean(m_efu_nopreg),
            D_ori = mean(m_preg_loss),
            Y_ori = mean(m_preeclamp)) |>
  ungroup() |>
  rename(mm = month_since_start)


NC_df_viz <- out_pp_sum |> left_join(ori_pp_sum, by = "mm")


T1_f <- NC_df_viz |>
  ggplot(aes(x = mm)) +
  geom_line(aes(y = T1_ori)) +
  geom_line(aes(y = T1_sim), color = "red") +
  mytheme() +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Month on study",
       y = "%",
       title = "Any health problem")

T2_f <- NC_df_viz |>
  ggplot(aes(x = mm)) +
  geom_line(aes(y = T2_ori)) +
  geom_line(aes(y = T2_sim), color = "red") +
  mytheme() +
  scale_y_continuous(limits = c(110, 130)) +
  labs(x = "Month on study",
       y = "mmHg",
       title = "Systolic blood pressure")

T3_f <- NC_df_viz |>
  ggplot(aes(x = mm)) +
  geom_line(aes(y = T3_ori)) +
  geom_line(aes(y = T3_sim), color = "red") +
  mytheme() +
  scale_y_continuous(limits = c(70, 85)) +
  labs(x = "Month on study",
       y = "mmHg",
       title = "Diastolic blood pressure")

Z_f <- NC_df_viz |>
  ggplot(aes(x = mm)) +
  geom_line(aes(y = Z_ori)) +
  geom_line(aes(y = Z_sim), color = "red") +
  mytheme() +
  scale_y_continuous(limits = c(0, 0.1))+
  labs(x = "Month on study",
       y = "%",
       title = "Pregnancy")

C_f <- NC_df_viz |>
  ggplot(aes(x = mm)) +
  geom_line(aes(y = C_ori)) +
  geom_line(aes(y = C_sim), color = "red") +
  mytheme() +
  scale_y_continuous(limits = c(0, 0.1))+
  labs(x = "Month on study",
       y = "%",
       title = "Loss to follow-up")

D_f <- NC_df_viz |>
  ggplot(aes(x = mm)) +
  geom_line(aes(y = D_ori)) +
  geom_line(aes(y = D_sim), color = "red") +
  mytheme() +
  scale_y_continuous(limits = c(0, 0.05))+
  labs(x = "Month on study",
       y = "%",
       title = "Early pregnancy loss")


S_f <- NC_df_viz |>
  ggplot(aes(x = mm)) +
  geom_line(aes(y = S_ori)) +
  geom_line(aes(y = S_sim), color = "red") +
  mytheme() +
  scale_y_continuous(limits = c(0, 1))+
  labs(x = "Month on study",
       y = "%",
       title = "End of study without pregnant")


Y_f <- NC_df_viz |>
  ggplot(aes(x = mm)) +
  geom_line(aes(y = Y_ori)) +
  geom_line(aes(y = Y_sim), color = "red") +
  mytheme() +
  scale_y_continuous(limits = c(0, 0.1))+
  labs(x = "Month on study",
       y = "%",
       title = "Pre-eclampsia")



png("Results/Validation/Validation_line.png", units="in", width = 14, height = 9, res = 300)
cowplot::plot_grid(T1_f, T2_f, T3_f, Z_f, C_f, S_f, D_f, Y_f, ncol = 3, labels = "AUTO")
dev.off()






#---------- Compare some events
df_raw <- readRDS("Data/Cleaned data/monthly_data_preecl.RDS") 


# Simulated
(mTreat_all <- mean(nc_df[nc_df$last == 1 & nc_df$Rp==1, ]$Zp))
(mContr_all <- mean(nc_df[nc_df$last == 1 & nc_df$Rp==0, ]$Zp))

(mTreat_all <- mean(nc_df[nc_df$last == 1 & nc_df$Rp==1, ]$Cp))
(mContr_all <- mean(nc_df[nc_df$last == 1 & nc_df$Rp==0, ]$Cp))

(mTreat_all <- mean(nc_df[nc_df$last == 1 & nc_df$Rp==1, ]$Dp))
(mContr_all <- mean(nc_df[nc_df$last == 1 & nc_df$Rp==0, ]$Dp))

(mTreat_all <- mean(nc_df[nc_df$last == 1 & nc_df$Rp==1, ]$Sp))
(mContr_all <- mean(nc_df[nc_df$last == 1 & nc_df$Rp==0, ]$Sp))



table(df_raw$m_efu_nopreg, df_raw$trt_grp)


149/678*100
167/677*100


#----- CI bootstrap
#===============================================================================
#---------- NC
boot_NC <- readRDS("Data/Result data/allImpute_preecl_boot_NC100.RDS") |>
  mutate_all(as.numeric)

(mTreat_all <- mean(nc_df[nc_df$last == 1 & nc_df$Rp==1, ]$Yp))
quantile(boot_NC$mTreat_all, probs = c(0.025, 0.975))*100

(mContr_all <- mean(nc_df[nc_df$last == 1 & nc_df$Rp==0, ]$Yp))
quantile(boot_NC$mContr_all, probs = c(0.025, 0.975))*100

(mTreat_preg <- mean(nc_df[nc_df$last == 1 & nc_df$Zp==1 & nc_df$Rp==1, ]$Yp))
quantile(boot_NC$mTreat_preg, probs = c(0.025, 0.975))*100

(mContr_preg <- mean(nc_df[nc_df$last == 1 & nc_df$Zp==1 & nc_df$Rp==0, ]$Yp))
quantile(boot_NC$mContr_preg, probs = c(0.025, 0.975))*100

# RD
mTreat_all - mContr_all
quantile(boot_NC$RD_all, probs = c(0.025, 0.975))*100

mTreat_preg - mContr_preg
quantile(boot_NC$RD_preg, probs = c(0.025, 0.975))*100

# RR
mTreat_all / mContr_all
quantile(boot_NC$RR_all, probs = c(0.025, 0.975))

mTreat_preg / mContr_preg
quantile(boot_NC$RR_preg, probs = c(0.025, 0.975))


par(mfrow = c(3, 3))
hist(boot_NC$mTreat_all)
hist(boot_NC$mTreat_preg)
hist(boot_NC$mContr_all)
hist(boot_NC$mContr_preg)

hist(boot_NC$RD_all)
hist(boot_NC$RD_preg)
hist(boot_NC$RR_all)
hist(boot_NC$RR_preg)
par(mfrow = c(1, 1))







#---------- ITT

boot_ITT <- readRDS("Data/Result data/allImpute_preecl_boot_ITT100.RDS")


(mTreat_all <- mean(treated_ITT[treated_ITT$last == 1,]$Yp))
quantile(boot_ITT$mTreat_all, probs = c(0.025, 0.975))*100

(mContr_all <- mean(placebo_ITT[placebo_ITT$last == 1,]$Yp))
quantile(boot_ITT$mContr_all, probs = c(0.025, 0.975))*100

(mTreat_preg <- mean(treated_ITT[treated_ITT$last == 1 & treated_ITT$Zp==1, ]$Yp))
quantile(boot_ITT$mTreat_preg, probs = c(0.025, 0.975))*100

(mContr_preg <- mean(placebo_ITT[placebo_ITT$last == 1 & placebo_ITT$Zp==1, ]$Yp))
quantile(boot_ITT$mContr_preg, probs = c(0.025, 0.975))*100

# RD
mTreat_all - mContr_all
quantile(boot_ITT$RD_all, probs = c(0.025, 0.975))*100

mTreat_preg - mContr_preg
quantile(boot_ITT$RD_preg, probs = c(0.025, 0.975))*100


# RR
mTreat_all / mContr_all
quantile(boot_ITT$RR_all, probs = c(0.025, 0.975))

mTreat_preg / mContr_preg
quantile(boot_ITT$RR_preg, probs = c(0.025, 0.975))


par(mfrow = c(3, 3))
hist(boot_ITT$mTreat_all)
hist(boot_ITT$mTreat_preg)
hist(boot_ITT$mContr_all)
hist(boot_ITT$mContr_preg)

hist(boot_ITT$RD_all)
hist(boot_ITT$RD_preg)
hist(boot_ITT$RR_all)
hist(boot_ITT$RR_preg)
par(mfrow = c(1, 1))




#---------- PP
boot_pp <- readRDS("Data/Result data/allImpute_preecl_boot_PP100.RDS")


(mTreat_all <- mean(treated_pp[treated_pp$last == 1,]$Yp))*100
quantile(boot_pp$mTreat_all, probs = c(0.025, 0.975))*100

(mContr_all <-mean(placebo_pp[placebo_pp$last == 1,]$Yp))*100
quantile(boot_pp$mContr_all, probs = c(0.025, 0.975))*100

(mTreat_preg <- mean(treated_pp[treated_pp$last == 1 & treated_pp$Zp==1, ]$Yp))*100
quantile(boot_pp$mTreat_preg, probs = c(0.025, 0.975))*100

(mContr_preg <- mean(placebo_pp[placebo_pp$last == 1 & placebo_pp$Zp==1, ]$Yp))*100
quantile(boot_pp$mContr_preg, probs = c(0.025, 0.975))*100

# RD
mTreat_all - mContr_all
quantile(boot_pp$RD_all, probs = c(0.025, 0.975))*100

mTreat_preg - mContr_preg
quantile(boot_pp$RD_preg, probs = c(0.025, 0.975))*100


# RR
mTreat_all / mContr_all
quantile(boot_pp$RR_all, probs = c(0.025, 0.975))

mTreat_preg / mContr_preg
quantile(boot_pp$RR_preg, probs = c(0.025, 0.975))


par(mfrow = c(3, 3))
hist(boot_pp$mTreat_all)
hist(boot_pp$mTreat_preg)
hist(boot_pp$mContr_all)
hist(boot_pp$mContr_preg)

hist(boot_pp$RD_all)
hist(boot_pp$RD_preg)
hist(boot_pp$RR_all)
hist(boot_pp$RR_preg)
par(mfrow = c(1, 1))





#----- Sensitivity analysis
#===============================================================================
rm(list = ls())

#---------- Adherence 4/7 days
load("Data/Result data/allImpute_preecl_PP_47cut.RData")

(mTreat_all <- mean(treated_pp[treated_pp$last == 1,]$Yp))*100
(mContr_all <- mean(placebo_pp[placebo_pp$last == 1,]$Yp))*100

(mTreat_preg <- mean(treated_pp[treated_pp$last == 1 & treated_pp$Zp==1, ]$Yp))*100
(mContr_preg <- mean(placebo_pp[placebo_pp$last == 1 & placebo_pp$Zp==1, ]$Yp))*100

# RD
(mTreat_all - mContr_all)*100
(mTreat_preg - mContr_preg)*100


# RR
mTreat_all / mContr_all
mTreat_preg / mContr_preg



#---------- Adherence 5/7 days
rm(list = ls())
load("Data/Result data/allImpute_preecl_PP_57cut.RData")

(mTreat_all <- mean(treated_pp[treated_pp$last == 1,]$Yp))*100
(mContr_all <- mean(placebo_pp[placebo_pp$last == 1,]$Yp))*100

(mTreat_preg <- mean(treated_pp[treated_pp$last == 1 & treated_pp$Zp==1, ]$Yp))*100
(mContr_preg <- mean(placebo_pp[placebo_pp$last == 1 & placebo_pp$Zp==1, ]$Yp))*100

# RD
(mTreat_all - mContr_all)*100
(mTreat_preg - mContr_preg)*100


# RR
mTreat_all / mContr_all
mTreat_preg / mContr_preg

#---------- Adherence 6/7 days
rm(list = ls())
load("Data/Result data/allImpute_preecl_PP_67cut.RData")

(mTreat_all <- mean(treated_pp[treated_pp$last == 1,]$Yp))*100
(mContr_all <- mean(placebo_pp[placebo_pp$last == 1,]$Yp))*100

(mTreat_preg <- mean(treated_pp[treated_pp$last == 1 & treated_pp$Zp==1, ]$Yp))*100
(mContr_preg <- mean(placebo_pp[placebo_pp$last == 1 & placebo_pp$Zp==1, ]$Yp))*100

# RD
(mTreat_all - mContr_all)*100
(mTreat_preg - mContr_preg)*100


# RR
mTreat_all / mContr_all
mTreat_preg / mContr_preg



#----- Sensitivity analysis
#===============================================================================
rm(list = ls())

#---------- NC
nc_df <- readRDS("Data/Result data/allImpute_preecl_NC_cens12.RDS")

(mTreat_all <- mean(nc_df[nc_df$last == 1 & nc_df$Rp==1, ]$Yp))*100
(mContr_all <- mean(nc_df[nc_df$last == 1 & nc_df$Rp==0, ]$Yp))*100
(mTreat_preg <- mean(nc_df[nc_df$last == 1 & nc_df$Zp==1 & nc_df$Rp==1, ]$Yp))*100
(mContr_preg <- mean(nc_df[nc_df$last == 1 & nc_df$Zp==1 & nc_df$Rp==0, ]$Yp))*100

# RD
(mTreat_all - mContr_all)*100
(mTreat_preg - mContr_preg)*100

# RR
mTreat_all / mContr_all
mTreat_preg / mContr_preg


#---------- NC
nc_df <- readRDS("Data/Result data/allImpute_preecl_NC_cens18.RDS")

(mTreat_all <- mean(nc_df[nc_df$last == 1 & nc_df$Rp==1, ]$Yp))*100
(mContr_all <- mean(nc_df[nc_df$last == 1 & nc_df$Rp==0, ]$Yp))*100
(mTreat_preg <- mean(nc_df[nc_df$last == 1 & nc_df$Zp==1 & nc_df$Rp==1, ]$Yp))*100
(mContr_preg <- mean(nc_df[nc_df$last == 1 & nc_df$Zp==1 & nc_df$Rp==0, ]$Yp))*100

# RD
(mTreat_all - mContr_all)*100
(mTreat_preg - mContr_preg)*100

# RR
mTreat_all / mContr_all
mTreat_preg / mContr_preg







#---------- ITT
load("Data/Result data/allImpute_ITT_cens12.RData")

(mTreat_all <- mean(treated_ITT[treated_ITT$last == 1,]$Yp))*100
(mContr_all <- mean(placebo_ITT[placebo_ITT$last == 1,]$Yp))*100

(mTreat_preg <- mean(treated_ITT[treated_ITT$last == 1 & treated_ITT$Zp==1, ]$Yp))*100
(mContr_preg <- mean(placebo_ITT[placebo_ITT$last == 1 & placebo_ITT$Zp==1, ]$Yp))*100

# RD
(mTreat_all - mContr_all)*100
(mTreat_preg - mContr_preg)*100

# RR
mTreat_all / mContr_all
mTreat_preg / mContr_preg

#---------- ITT
load("Data/Result data/allImpute_ITT_cens18.RData")

(mTreat_all <- mean(treated_ITT[treated_ITT$last == 1,]$Yp))*100
(mContr_all <- mean(placebo_ITT[placebo_ITT$last == 1,]$Yp))*100

(mTreat_preg <- mean(treated_ITT[treated_ITT$last == 1 & treated_ITT$Zp==1, ]$Yp))*100
(mContr_preg <- mean(placebo_ITT[placebo_ITT$last == 1 & placebo_ITT$Zp==1, ]$Yp))*100

# RD
(mTreat_all - mContr_all)*100
(mTreat_preg - mContr_preg)*100

# RR
mTreat_all / mContr_all
mTreat_preg / mContr_preg


#---------- PP
load("Data/Result data/allImpute_preecl_PP_cens12.RData")

(mTreat_all <- mean(treated_pp[treated_pp$last == 1,]$Yp))*100
(mContr_all <- mean(placebo_pp[placebo_pp$last == 1,]$Yp))*100

(mTreat_preg <- mean(treated_pp[treated_pp$last == 1 & treated_pp$Zp==1, ]$Yp))*100
(mContr_preg <- mean(placebo_pp[placebo_pp$last == 1 & placebo_pp$Zp==1, ]$Yp))*100

# RD
(mTreat_all - mContr_all)*100
(mTreat_preg - mContr_preg)*100


# RR
mTreat_all / mContr_all
mTreat_preg / mContr_preg


#---------- PP
load("Data/Result data/allImpute_preecl_PP_cens18.RData")

(mTreat_all <- mean(treated_pp[treated_pp$last == 1,]$Yp))*100
(mContr_all <- mean(placebo_pp[placebo_pp$last == 1,]$Yp))*100

(mTreat_preg <- mean(treated_pp[treated_pp$last == 1 & treated_pp$Zp==1, ]$Yp))*100
(mContr_preg <- mean(placebo_pp[placebo_pp$last == 1 & placebo_pp$Zp==1, ]$Yp))*100

# RD
(mTreat_all - mContr_all)*100
(mTreat_preg - mContr_preg)*100


# RR
mTreat_all / mContr_all
mTreat_preg / mContr_preg




















