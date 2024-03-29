library(tidyverse)
library(mice)



# Original data
load("Data/Imputed data/imp_pre.RData")

impdf <- complete(imp, "long")

# PP
load("Data/Imputed data/allImpute_preecl_PP.RData")


# NC
nc_df <- readRDS("Data/Imputed data/allImpute_preecl_NC.RDS")

# ITT
load("Data/Imputed data/allImpute_ITT.RData")

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


#===============================================================================

# PP
mean(treated_pp[treated_pp$last == 1 & treated_pp$Zp==1, ] $Yp)
mean(placebo_pp[placebo_pp$last == 1 & placebo_pp$Zp==1, ] $Yp)

mean(treated_pp[treated_pp$last == 1,]$Yp)
mean(placebo_pp[placebo_pp$last == 1,]$Yp)

# NC
mean(nc_df[nc_df$last == 1 & nc_df$Zp==1 & nc_df$Rp==1, ] $Yp)
mean(nc_df[nc_df$last == 1 & nc_df$Zp==1 & nc_df$Rp==0, ] $Yp)

mean(nc_df[nc_df$last == 1 & nc_df$Rp==1, ] $Yp)
mean(nc_df[nc_df$last == 1 & nc_df$Rp==0, ] $Yp)


# ITT
mean(treated_ITT[treated_ITT$last == 1 & treated_ITT$Zp==1, ] $Yp)
mean(placebo_ITT[placebo_ITT$last == 1 & placebo_ITT$Zp==1, ] $Yp)

mean(treated_ITT[treated_ITT$last == 1,] $Yp)
mean(placebo_ITT[placebo_ITT$last == 1,] $Yp)



out_pp_sum <- nc_df |> 
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

ori_pp_sum <- impdf |> 
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
  geom_line(aes(y = T1_sim), color = "green2") +
  mytheme() +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Month on study",
       y = "%",
       title = "Any health problem")

T2_f <- NC_df_viz |>
  ggplot(aes(x = mm)) +
  geom_line(aes(y = T2_ori)) +
  geom_line(aes(y = T2_sim), color = "green2") +
  mytheme() +
  scale_y_continuous(limits = c(110, 130)) +
  labs(x = "Month on study",
       y = "mmHg",
       title = "Systolic blood pressure")

T3_f <- NC_df_viz |>
  ggplot(aes(x = mm)) +
  geom_line(aes(y = T3_ori)) +
  geom_line(aes(y = T3_sim), color = "green2") +
  mytheme() +
  scale_y_continuous(limits = c(70, 85)) +
  labs(x = "Month on study",
       y = "mmHg",
       title = "Diastolic blood pressure")

Z_f <- NC_df_viz |>
  ggplot(aes(x = mm)) +
  geom_line(aes(y = Z_ori)) +
  geom_line(aes(y = Z_sim), color = "green2") +
  mytheme() +
  scale_y_continuous(limits = c(0, 1))+
  labs(x = "Month on study",
       y = "%",
       title = "Pregnancy")

C_f <- NC_df_viz |>
  ggplot(aes(x = mm)) +
  geom_line(aes(y = C_ori)) +
  geom_line(aes(y = C_sim), color = "green2") +
  mytheme() +
  scale_y_continuous(limits = c(0, 0.1))+
  labs(x = "Month on study",
       y = "%",
       title = "Loss to follow-up")

D_f <- NC_df_viz |>
  ggplot(aes(x = mm)) +
  geom_line(aes(y = D_ori)) +
  geom_line(aes(y = D_sim), color = "green2") +
  mytheme() +
  scale_y_continuous(limits = c(0, 0.05))+
  labs(x = "Month on study",
       y = "%",
       title = "Early pregnancy loss")


S_f <- NC_df_viz |>
  ggplot(aes(x = mm)) +
  geom_line(aes(y = S_ori)) +
  geom_line(aes(y = S_sim), color = "green2") +
  mytheme() +
  scale_y_continuous(limits = c(0, 1))+
  labs(x = "Month on study",
       y = "%",
       title = "End of study without pregnant")


Y_f <- NC_df_viz |>
  ggplot(aes(x = mm)) +
  geom_line(aes(y = Y_ori)) +
  geom_line(aes(y = Y_sim), color = "green2") +
  mytheme() +
  scale_y_continuous(limits = c(0, 0.1))+
  labs(x = "Month on study",
       y = "%",
       title = "Pre-eclampsia")



png("Results/Validation/Validation_line.png", units="in", width = 14, height = 9, res = 300)
cowplot::plot_grid(T1_f, T2_f, T3_f, Z_f, C_f, S_f, D_f, Y_f, ncol = 3, labels = "AUTO")
dev.off()










