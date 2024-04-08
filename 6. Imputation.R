library(tidyverse)
library(mice)
library(VIM)
library(splines)
library(mgcv)

# Define not in operator
`%!in%` = Negate(`%in%`)

#-----

df_raw <- readRDS("Data/Cleaned data/monthly_data_preecl.RDS") |>
  mutate(ID = StudySubjectID)

df_raw |> filter(month_since_start >= 40) |> pull(StudySubjectID) |> unique() |> length()
df_raw |> filter(month_since_start < 40) |> pull(StudySubjectID) |> unique() |> length()

sapply(df_raw, function(x){sum(is.na(x))})

glimpse(df_raw)



#----- Create lag variables for adherence
df_raw <- df_raw |> group_by(StudySubjectID) |>
  mutate(adhere_per_l1 = lag(adhere_per, 1),
         adhere_per_l1 = if_else(is.na(adhere_per_l1), adhere_per, adhere_per_l1),
         adhere_per_l2 = lag(adhere_per_l1, 1),
         adhere_per_l2 = if_else(is.na(adhere_per_l2), adhere_per_l1, adhere_per_l2),) |>
  ungroup() |>
  mutate(adm_center = as.character(adm_center),
         adm_country = case_when(adm_country == "Argentina" ~ "1",
                                 adm_country == "Zimbabwe" ~ "2",
                                 adm_country == "South Africa" ~ "3"),
         adm_country = as.factor(adm_country),
         adm_center = case_when(adm_center == "0101 - CEMIC Buenos Aires" ~ "1",
                                adm_center == "1629 - Harare Hospital (Harare)" ~ "2",
                                adm_center == "1825 - Hospital Maternidad Sardá" ~ "3",
                                adm_center == "1827 - Hospital Maternidad Tucumán" ~ "4",
                                adm_center == "1829 - Hospital Italiano de Buenos Aires" ~ "5",
                                adm_center == "1958 - Chris Hari Baragwanath Hospital (Johannesburg)" ~ "6",
                                adm_center == "2065 - Mowbray Maternity Hospital (Cape Town)" ~ "7",
                                adm_center == "2067 - Cecilia Makiwane Hospital (East London)" ~ "8",
                                adm_center == "2068 - Frere Hospital (East London)" ~ "9",
                                adm_center == "2069 - Tygerberg Hospital (Stellenbosch)" ~ "10",
                                adm_center == "1829 - Hospital Italiano de Buenos Aires (San Justo)" ~ "11"))



#----- Define predictor for imputed model
# non_impute_var <- c("StudySubjectID", "dosage", "adm_date", "scr_date", "min_date", 
#                     "adm_weight", "adm_gestageatb", "trt_varying2")

impute_var <- c("ID", "month_since_start","trt_grp", "trt_varying2", "trt_varying3",
                "shlth_prob", "SBP", "DBP",
                "adhere_per", "adhere_per_l1", "adhere_per_l2",
                "m_preeclamp", "m_preg", "m_preg_loss", "m_ltfu", "m_efu_nopreg",
                "month_since_preg", "last_id", 
                "adm_country", "adm_age", "adm_parity", "adm_eclampsia", 
                "adm_hellp", "adm_alive", "adm_ICUadm", "adm_hlth_prob", "adm_sysbp",
                "adm_diasbp", "adm_bmi")

df_unimp <- df_raw |> select(all_of(impute_var))

glimpse(df_unimp)

#----- Cheking missing value patterns
#-------------------------------------------------------------------------------
# mis_var <- c("adhere_per", "shlth_prob", "nantihyp_med", "SBP", "DBP",
#             "adm_age", "adm_height", "adm_eclampsia", "adm_hellp", "adm_parity", 
#             "adm_alive", "adm_typebirth", "adm_ICUadm", 
#             "adm_hlth_prob", "adm_sysbp", "adm_diasbp", "adm_center", "adm_bmi", "trt_grp")

sapply(df_unimp, function(x){sum(is.na(x))})

mis_var <- c("shlth_prob", "SBP", "DBP", "adhere_per",
             "adm_age", "adm_parity", "adm_eclampsia", 
             "adm_hellp", "adm_alive", "adm_ICUadm", "adm_hlth_prob", "adm_sysbp",
             "adm_diasbp", "adm_bmi")

png("Results/Imputation/missing_pattern.png", units="in", width = 13, height = 8, res = 300)
aggr(df_unimp |> select(all_of(mis_var)),
     col=c('skyblue','red'), numbers=TRUE, 
     sortVars=TRUE, labels = mis_var, cex.axis=0.7, gap=0, 
     ylab=c("Histogram of missing data","Pattern"))
dev.off()


#----- Doing imputation
#-------------------------------------------------------------------------------
ini <- mice(df_unimp, maxit = 0)
non_pred_var <- c("trt_varying2", "ID")

#---------- Ignore some unimpute variables
pred <- ini$predictorMatrix
pred[, non_pred_var] <- 0

# Impute
imp <- mice(df_unimp, pred = pred, m = 10, seed = 9999, maxit = 10)

png("Results/Imputation/Imputed_values.png", units="in", width = 12, height = 10, res = 300)
stripplot(imp, adhere_per + shlth_prob + SBP + DBP + adm_parity + adm_eclampsia +
            adm_hellp + adm_sysbp + adm_diasbp + adm_bmi ~ .imp,  layout = c(4, 3), alpha = 0.2,
          cex = 0.5, jitter = T)
dev.off()

# save(imp, file = "Data/Imputed data/imp_pre.RData")


imp$imp$adhere_per
sapply(imp$imp$adhere_per, function(x){mean(x>0.8)})

# Adherence monthly
#-------------------------------------------------------------------------------
#----------  Set up theme
#===============================================================================
mytheme <- function(...) {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 12,color = "grey10",  face = "bold", hjust = 0.5),
      axis.line = element_line(linetype = "solid"),
      axis.text = element_text(color = "gray10", size = 10),
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



# Monthly adherence by month since enrollment
#-------------------------------------------------------------------------------
# Create smooth for 95%CI
enroll_original_df <- df_unimp |> filter(!is.na(adhere_per)) |>
  group_by(month_since_start, trt_grp) |>
  summarise(adhere_per_m = mean(adhere_per, na.rm = T),
            adhere_se = sd(adhere_per, na.rm = T)/sqrt(n()),
            adhere_ub = adhere_per_m + 1.96*adhere_se,
            adhere_lb = adhere_per_m - 1.96*adhere_se) 

fit_adhere_ub <- gam(adhere_ub ~ s(month_since_start) + trt_grp,
             method = "REML", data=enroll_original_df)
fit_adhere_lb <- gam(adhere_lb ~ s(month_since_start) + trt_grp,
                     method = "REML", data=enroll_original_df)

enroll_original_df$adhere_ub_smooth <- predict(fit_adhere_ub)
enroll_original_df$adhere_lb_smooth <- predict(fit_adhere_lb)


# Plot
enroll_original_CI <- enroll_original_df |>
  ggplot(aes(x = month_since_start, y = adhere_per_m, color = trt_grp)) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = F) +
  geom_ribbon(aes(ymin=adhere_lb_smooth, ymax=adhere_ub_smooth), alpha=0.05, linetype = 2) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  mytheme() +
  scale_color_brewer(palette = "Set1") +
  labs(
    color = NULL,
    x = "Month since enrollment",
    y = "% of adherence",
    title = "Original data"
    )
enroll_original_CI



enroll_original <- df_unimp |> 
  group_by(month_since_start, trt_grp) |>
  summarise(adhere_per = mean(adhere_per, na.rm = T)) |>
  ggplot(aes(x = month_since_start, y = adhere_per, color = trt_grp)) +
  geom_point(alpha = 0.7) +
  geom_smooth(fill = "gray80", se = F) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  mytheme() +
  scale_color_brewer(palette = "Set1") +
  labs(
    color = NULL,
    x = "Month since enrollment",
    y = "% of adherence",
    title = "Original data"
  )
enroll_original


enroll_original_70 <- df_unimp |> 
  group_by(month_since_start, trt_grp) |>
  summarise(adhere_per = mean(adhere_per >= 0.8, na.rm = T)) |>
  ggplot(aes(x = month_since_start, y = adhere_per, color = trt_grp)) +
  geom_point(alpha = 0.7) +
  geom_smooth(fill = "gray80", se = F) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  mytheme() +
  scale_color_brewer(palette = "Set1") +
  labs(
    color = NULL,
    x = "Month since enrollment",
    y = "Adherence > 80%",
    title = "Original data"
  )



for (i in 1:10) {
  c <- paste0("enroll_", i)
  d <- paste0("enroll_70_", i)
  eval(call("<-", as.name(c), 
            complete(imp, i) |>
              group_by(month_since_start, trt_grp) |>
              summarise(adhere_per = mean(adhere_per, na.rm = T)) |>
              ggplot(aes(x = month_since_start, y = adhere_per, color = trt_grp)) +
              geom_point(alpha = 0.7) +
              geom_smooth(fill = "gray80", se = F) +
              scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
              mytheme() +
              scale_color_brewer(palette = "Set1") +
              labs(
                color = NULL,
                x = "Month since enrollment",
                y = "% of adherence",
                title = paste0("Imputed # ", i)
              )
  ))
  
  eval(call("<-", as.name(d), 
            complete(imp, i) |>
              group_by(month_since_start, trt_grp) |>
              summarise(adhere_per = mean(adhere_per >= 0.8, na.rm = T)) |>
              ggplot(aes(x = month_since_start, y = adhere_per, color = trt_grp)) +
              geom_point(alpha = 0.7) +
              geom_smooth(fill = "gray80", se = F) +
              scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
              mytheme() +
              scale_color_brewer(palette = "Set1") +
              labs(
                color = NULL,
                x = "Month since enrollment",
                y = "Adherence > 80%",
                title = paste0("Imputed # ", i)
              )
  ))
}


png("Results/Imputation/Imputed_adherence_enroll.png", units="in", width = 10, height = 12, res = 300)
cowplot::plot_grid(enroll_original, enroll_1, enroll_2, enroll_3, enroll_4, enroll_5,
                   enroll_6, enroll_7, enroll_8, enroll_9, enroll_10,
                   ncol = 3, labels = "AUTO")
dev.off()

png("Results/Imputation/Imputed_adherence_enroll_80.png", units="in",width = 10, height = 12, res = 300)
cowplot::plot_grid(enroll_original_70, enroll_70_1, enroll_70_2, enroll_70_3, 
                   enroll_70_4, enroll_70_5, enroll_70_6, enroll_70_7, 
                   enroll_70_8, enroll_70_9, enroll_70_10,
                   ncol = 3, labels = "AUTO")
dev.off()

png("Results/Imputation/Imputed_adherence_original.png", units="in",width = 12, height = 5, res = 300)
cowplot::plot_grid(enroll_original, enroll_original_70, ncol = 2, labels = "AUTO")
dev.off()


png("Results/Imputation/Imputed_adherence_original.png", units="in", width = 9, height = 6, res = 300)
enroll_original_CI
dev.off()


# Percentage of missing across months_since_enrollment
#-------------------------------------------------------------------------------
missing_by_month <- df_unimp |> 
  group_by(month_since_start, trt_grp) |>
  summarise(month_adh_miss = sum(is.na(adhere_per))/n()) |>
  ggplot(aes(x = month_since_start, y = month_adh_miss, color = trt_grp)) +
  geom_point() +
  geom_smooth(se = F) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 60)) +
  mytheme() +
  theme(
    plot.title = element_text(size = 12,color = "grey10",  face = "bold", hjust = 0.5),
    axis.line = element_line(linetype = "solid"),
    axis.text = element_text(color = "gray10", size = 12),
    axis.title = element_text(color = "gray10", size = 12),
  ) + 
  labs(
    color = NULL,
    x = "Month since enrollment",
    y = "% of missing of adherence",
    title = "% of missing of adherence by month"
  )


png("Results/Imputation/missing_by_month.png", units="in", width = 8, height = 5, res = 300)
missing_by_month
dev.off()



missing_by_month_cov <- df_unimp |> 
  group_by(month_since_start) |>
  summarise(shlth_prob_miss = sum(is.na(shlth_prob))/n(),
            systolic_bp_miss = sum(is.na(SBP))/n(),
            diastolic_bp_miss = sum(is.na(DBP))/n()) |>
  gather(-month_since_start, key = "var", value = "val") |>
  mutate(var = case_when(var == "shlth_prob_miss" ~ "Health problem",
                         var == "systolic_bp_miss" ~ "SBP",
                         var == "diastolic_bp_miss" ~ "DBP")) |>
  ggplot(aes(x = month_since_start, y = val, color = var)) +
  geom_jitter() +
  geom_smooth(se = F) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(limit = c(1, 60)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  mytheme() +
  theme(
    plot.title = element_text(size = 12,color = "grey10",  face = "bold", hjust = 0.5),
    axis.line = element_line(linetype = "solid"),
    axis.text = element_text(color = "gray10", size = 12),
    axis.title = element_text(color = "gray10", size = 12),
  ) + 
  labs(
    color = NULL,
    x = "Month since enrollment",
    y = "% of missing",
    title = "% of missing of time-varying by month"
  )

png("Results/Imputation/missing_by_month_covariates.png", units="in", width = 8, height = 5, res = 300)
missing_by_month_cov
dev.off()





table(df_unimp$m_preeclamp)
table(df_unimp$m_preeclamp)







# Monthly adherence by month since enrollment
#-------------------------------------------------------------------------------
# Add censoring for pregnancy
df_unimp <- df_unimp |> group_by(ID) |>
  mutate(preg_cens_indi12 = if_else(m_preg == 0 & month_since_start > 12, 1, 0),
         preg_cens_indi_cum12 = cumsum(preg_cens_indi12),
         preg_cens_indi18 = if_else(m_preg == 0 & month_since_start > 18, 1, 0),
         preg_cens_indi_cum18 = cumsum(preg_cens_indi18)) |>
  ungroup()


enroll_original_cut12 <- df_unimp |> 
  filter(preg_cens_indi_cum12 == 0) |> 
  group_by(month_since_start, trt_grp) |>
  summarise(adhere_per = mean(adhere_per, na.rm = T)) |>
  ggplot(aes(x = month_since_start, y = adhere_per, color = trt_grp)) +
  geom_point(alpha = 0.7) +
  geom_smooth(fill = "gray80", se = F) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  mytheme() +
  scale_color_brewer(palette = "Set1") +
  labs(
    color = NULL,
    x = "Month since enrollment",
    y = "% Adherence",
    title = "Censoring participants with no pregnancy after 12 months of enrollment"
  )
enroll_original_cut12




enroll_original_cut18 <- df_unimp |> 
  filter(preg_cens_indi_cum18 == 0) |> 
  group_by(month_since_start, trt_grp) |>
  summarise(adhere_per = mean(adhere_per, na.rm = T)) |>
  ggplot(aes(x = month_since_start, y = adhere_per, color = trt_grp)) +
  geom_point(alpha = 0.7) +
  geom_smooth(fill = "gray80", se = F) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  mytheme() +
  scale_color_brewer(palette = "Set1") +
  labs(
    color = NULL,
    x = "Month since enrollment",
    y = "% Adherence",
    title = "Censoring participants with no pregnancy after 18 months of enrollment"
  )
enroll_original_cut18



png("Results/Imputation/adherence_enroll_cut1218.png", units="in", width = 12, height = 5, res = 300)
cowplot::plot_grid(enroll_original_cut12, enroll_original_cut18,
                   ncol = 2, labels = "AUTO")
dev.off()











