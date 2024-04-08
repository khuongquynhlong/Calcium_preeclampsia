library(plyr)
library(data.table)
library(dplyr)
library(mice)
# library(VIM)
library(mgcv)
library(splines)
# library(DescTools)
library(doParallel)
# library(microbenchmark)


#---------- Set up parallel
#===============================================================================
cores <- detectCores()
cores
registerDoParallel(cores=cores)



#----- Load data imputed
#===============================================================================
load("imp_pre.RData")


#----- Function for extract complete set and prepare data for g-computation
#===============================================================================
df_prep <- function(data, nimp, preg_cens = NULL) {
  df_imp <- complete(data, nimp)
  
  # Baseline characteristics
  SS <- data.frame(model.matrix(~factor(df_imp$adm_country))[,-1])
  names(SS) <- c(paste("V", 1:2, sep = ""))
  SS$V3 <- df_imp$adm_age
  SS$V4 <- df_imp$adm_parity
  SS$V5 <- df_imp$adm_eclampsia
  SS$V6 <- df_imp$adm_hellp
  SS$V7 <- df_imp$adm_bmi
  SS$V8 <- df_imp$adm_alive
  SS$V9 <- df_imp$adm_ICUadm
  SS$V10 <- df_imp$adm_hlth_prob
  SS$V11 <- df_imp$adm_sysbp
  SS$V12 <- df_imp$adm_diasbp
  
  # Randomization
  SS$R <- ifelse(df_imp$trt_grp == "Calcium", 1, 0)
  
  # Time-varying treatment
  SS$Tr = df_imp$trt_varying3
  
  # Compliance
  SS$X <- df_imp$adhere_per
  
  # time-dependent variables
  SS$T1 <- round(df_imp$shlth_prob)
  SS$T2 <- df_imp$SBP
  SS$T3 <- df_imp$DBP
  
  SS$j <- df_imp$month_since_start
  SS$id <- df_imp$ID
  
  # outcomes
  SS$Z <- df_imp$m_preg
  SS$D <- df_imp$m_preg_loss
  SS$C <- df_imp$m_ltfu
  SS$S <- df_imp$m_efu_nopreg
  SS$Y <- df_imp$m_preeclamp
  
  
  # New dataset
  SS <- SS %>% group_by(id) |>
    mutate(Xl = lag(X, n = 1, default = mean(X)),
           Xl1 = lag(Xl, n = 1, default = mean(Xl)),
           T1l = lag(T1, n = 1, default = round(mean(T1))),
           T1l1 = lag(T1l, n = 1, default = round(mean(T1l))),
           T2l = lag(T2, n = 1, default = round(mean(T2))),
           T2l1 = lag(T2l, n = 1, default = round(mean(T2l))),
           T3l = lag(T3, n = 1, default = round(mean(T3))),
           T3l1 = lag(T3l, n = 1, default = round(mean(T3l))),
           Zl = lag(Z, n = 1, default = 0),
           Zl1 = lag(Zl, n = 1, default = 0),
           Cl = lag(C, n = 1, default =0)) |>
    ungroup() |>
    as.data.frame()
  
  # Add censoring for pregnancy
  SS <- SS |> group_by(id) |>
    mutate(preg_cens_indi = if_else(Z == 0 & j > preg_cens, 1, 0),
           preg_cens_indi_cum = cumsum(preg_cens_indi)) |>
    ungroup()
  
  if(!is.null(preg_cens)){
    SS <- SS |> filter(preg_cens_indi_cum == 0) |>
      mutate(S = if_else(Z == 0 & j == preg_cens, 1, S))
  }
  SS <- SS |> select(-c(preg_cens_indi, preg_cens_indi_cum)) |>
    as.data.frame()
  
  return(SS)
}



#----- Function to bootstrap the g Computation algorithm
#===============================================================================
g_boot <- function(data, nimp, threshold, montecarlo, randomization = NULL, 
                   adherence = NULL, interaction = NULL, censoring = NULL, preg_cens = NULL,
                   length = 60, seed) { 
  df <- df_prep(data = data, nimp = nimp, preg_cens = preg_cens)
  
  # Define adherence (999 = "natural course")
  if (threshold != 999) {
    df$X <- ifelse(df$X > threshold, 1, 0)
    df$Xl <- ifelse(df$Xl > threshold, 1, 0)
    df$Xl1 <- ifelse(df$Xl1 > threshold, 1, 0)
  }
  
  set.seed(seed)
  cat("Now Running SEED", seed, "\n")
  cat("\n")
  cat("Resampling Data", "\n")
  
  # Resampling based on id and store in `boot` dataset
  clusters <- as.numeric(names(table(df$id)))
  index <- sample(1:length(clusters), length(clusters), replace = TRUE)
  bb <- table(clusters[index])
  boot <- NULL
  
  if(seed == 0) {
    # not doing bootstrap
    boot <- df 
  } else {
    for(zzz in 1:max(bb)) {
      # Loop over repeated id
      cc <- df[df$id %in% names(bb[bb %in% c(zzz:max(bb))]), ]
      cc$bid <- paste0(cc$id, zzz)
      boot <- rbind(boot, cc)
    }
  }
  
  
  # Check boot data
  cat("Are resampled data identical to original for seed =", seed, "?:", identical(boot, df),'\n')
  
  boot$last_id <- as.numeric(!duplicated(boot$id, fromLast = T))
  head(boot)
  sum(boot$last_id)
  table(boot[boot$last_id==1,]$R)
  table(boot$X)
  
  # cat("Centering and scaling the time-scale variable jj", '\n')
  boot$jj <- scale(boot$j)
  mean_j <- attributes(boot$jj)$`scaled:center`
  sd_j <- attributes(boot$jj)$`scaled:scale`
  boot$jj <- as.numeric(boot$jj)
  
  cat('\n')
  cat("Fitting parametric models",'\n')
  
  
  cat('\n')
  cat("Fitting T1",'\n')
  mT1_0<-function(k){
    fitT1<-glm(T1~V1+V2+bs(V3,df=3)+V4+V5+V6+V7+V8+V9+V10+bs(V11,df=3)+bs(V12,df=3)+
                 Tr+Xl+Xl1+T1l+bs(T2l,df=3)+bs(T3l,df=3)+Zl+bs(jj,df = 3),
               family=binomial, data=boot, subset=R==0)
    return(fitT1)
  }
  
  mT1_1<-function(k){
    fitT1<-glm(T1~V1+V2+bs(V3,df=3)+V4+V5+V6+V7+V8+V9+V10+bs(V11,df=3)+bs(V12,df=3)+
                 Tr+Xl+Xl1+T1l+bs(T2l,df=3)+bs(T3l,df=3)+Zl+bs(jj,df = 3),
               family=binomial, data=boot, subset=R==1)
    return(fitT1)
  }
  
  # PseudoR2(fitT1)
  
  cat('\n')
  cat("Fitting T2",'\n')
  mT2_0<-function(k){
    fitT2<-gam(T2~V1+V2+s(V3)+V4+V5+V6+V7+V8+V9+V10+s(V11)+s(V12)+
                 Tr+Xl+Xl1+T1l+s(T2l)+s(T3l)+Zl+s(jj),
               data=boot, subset=R==0)
    return(fitT2)
  }
  
  mT2_1<-function(k){
    fitT2<-gam(T2~V1+V2+s(V3)+V4+V5+V6+V7+V8+V9+V10+s(V11)+s(V12)+
                 Tr+Xl+Xl1+T1l+s(T2l)+s(T3l)+Zl+s(jj),
               data=boot, subset=R==1)
    return(fitT2)
  }
  
  cat('\n')
  cat("Fitting T3",'\n')
  mT3_0<-function(k){
    fitT3<-gam(T3~V1+V2+s(V3)+V4+V5+V6+V7+V8+V9+V10+s(V11)+s(V12)+
                 Tr+Xl+Xl1+T1l+s(T2l)+s(T3l)+Zl+s(jj),
               data=boot, subset=R==0)
    return(fitT3)
  }
  
  mT3_1<-function(k){
    fitT3<-gam(T3~V1+V2+s(V3)+V4+V5+V6+V7+V8+V9+V10+s(V11)+s(V12)+
                 Tr+Xl+Xl1+T1l+s(T2l)+s(T3l)+Zl+s(jj),
               data=boot, subset=R==1)
    return(fitT3)
  }
  
  cat('\n')
  cat("Fitting Z",'\n')
  mZ_0<-function(k){
    fitZ<-glm(Z~V1+V2+bs(V3,df=3)+V4+V5+V6+V7+V8+V9+V10+bs(V11,df=3)+bs(V12,df=3)+
                X+Xl+Xl1+T1+T1l+bs(T2,df=3)+bs(T3,df=3)+
                bs(T2l,df=3)+bs(T3l,df=3)+bs(jj,df=3),
              family=binomial, data=boot, subset=Zl==0&R==0)
    return(fitZ)
  }
  
  mZ_1<-function(k){
    fitZ<-glm(Z~V1+V2+bs(V3,df=3)+V4+V5+V6+V7+V8+V9+V10+bs(V11,df=3)+bs(V12,df=3)+
                X+Xl+Xl1+T1+T1l+bs(T2,df=3)+bs(T3,df=3)+
                bs(T2l,df=3)+bs(T3l,df=3)+bs(jj,df=3),
              family=binomial, data=boot, subset=Zl==0&R==1)
    return(fitZ)
  }
  
  # PseudoR2(fitZ)
  
  cat('\n')
  cat("Fitting X",'\n')
  mX_0<-function(k){
    fitX<-glm(X~V1+V2+bs(V3,df=3)+V4+V5+V6+V7+V8+V9+V10+bs(V11,df=3)+bs(V12,df=3)+
                Tr+Xl+Xl1+T1+T1l+bs(T2,df=3)+bs(T3,df=3)+
                bs(T2l,df=3)+bs(T3l,df=3)+Zl+bs(jj,df=3),
              family=binomial, data=boot, subset=R==0)
    return(fitX)
  }
  
  mX_1<-function(k){
    fitX<-glm(X~V1+V2+bs(V3,df=3)+V4+V5+V6+V7+V8+V9+V10+bs(V11,df=3)+bs(V12,df=3)+
                Tr+Xl+Xl1+T1+T1l+bs(T2,df=3)+bs(T3,df=3)+
                bs(T2l,df=3)+bs(T3l,df=3)+Zl+bs(jj,df=3),
              family=binomial, data=boot, subset=R==1)
    return(fitX)
  }
  
  # PseudoR2(fitX)
  
  cat('\n')
  cat("Fitting C",'\n')
  mC_0<-function(k){
    fitC<-glm(C~V1+V2+bs(V3,df=3)+V4+V5+V6+V7+V8+V9+V10+bs(V11,df=3)+bs(V12,df=3)+
                X+Xl+Xl1+T1+T1l+bs(T2,df=3)+bs(T3,df=3)+
                bs(T2l,df=3)+bs(T3l,df=3)+Z+bs(jj,df=3),
              family=binomial, data=boot, subset=R==0)
    return(fitC)
  }
  
  mC_1<-function(k){
    fitC<-glm(C~V1+V2+bs(V3,df=3)+V4+V5+V6+V7+V8+V9+V10+bs(V11,df=3)+bs(V12,df=3)+
                X+Xl+Xl1+T1+T1l+bs(T2,df=3)+bs(T3,df=3)+
                bs(T2l,df=3)+bs(T3l,df=3)+Z+bs(jj,df=3),
              family=binomial, data=boot, subset=R==1)
    return(fitC)
  }
  
  # PseudoR2(fitC)
  
  cat('\n')
  cat("Fitting S",'\n')
  mS_0<-function(k){
    fitS<-glm(S~V1+V2+bs(V3,df=3)+V4+V5+V6+V7+V8+V9+V10+bs(V11,df=3)+bs(V12,df=3)+
                X+Xl+Xl1+T1+T1l+bs(T2,df=3)+bs(T3,df=3)+
                bs(T2l,df=3)+bs(T3l,df=3)+bs(jj,df=3),
              family=binomial, data=boot, subset=Z==0&R==0)
    return(fitS)
  }
  
  mS_1<-function(k){
    fitS<-glm(S~V1+V2+bs(V3,df=3)+V4+V5+V6+V7+V8+V9+V10+bs(V11,df=3)+bs(V12,df=3)+
                X+Xl+Xl1+T1+T1l+T2+T3+T2l+T3l+bs(jj,df=3),
              family=binomial, data=boot, subset=Z==0&R==1)
    return(fitS)
  }
  
  # PseudoR2(fitS)
  
  cat('\n')
  cat("Fitting D",'\n')
  mD_0<-function(k){
    fitD<-glm(D~V1+V2+bs(V3,df=3)+V4+V5+V6+V7+V8+V9+V10+bs(V11,df=3)+bs(V12,df=3)+
                X+Xl+Xl1+T1+T1l+bs(T2,df=3)+bs(T3,df=3)+
                bs(T2l,df=3)+bs(T3l,df=3)+bs(jj,df=3),
              family=binomial, data=boot, subset=Z==1&R==0)
    return(fitD)
  }
  
  mD_1<-function(k){
    fitD<-glm(D~V1+V2+bs(V3,df=3)+V4+V5+V6+V7+V8+V9+V10+bs(V11,df=3)+bs(V12,df=3)+
                X+Xl+Xl1+T1+T1l+T2+T3+T2l+T3l+bs(jj,df=3),
              family=binomial, data=boot, subset=Z==1&R==1)
    return(fitD)
  }
  
  # PseudoR2(fitD)
  
  cat('\n')
  cat("Fitting Y",'\n')
  mY_0<-function(k){
    fitY<-glm(Y~V1+V2+bs(V3,df=3)+V4+V5+V6+V7+V8+V9+V10+bs(V11,df=3)+bs(V12,df=3)+
                Tr+X+Xl+Xl1+T1+T1l+bs(T2,df=3)+bs(T3,df=3)+bs(jj,df=3),
              family=binomial, data=boot, subset=Z==1&Cl==0&R==0)
    return(fitY)
  }
  
  mY_1<-function(k){
    fitY<-glm(Y~V1+V2+bs(V3,df=3)+V4+V5+V6+V7+V8+V9+V10+V11+V12+
                Tr+X+Xl+Xl1+T1+T1l+bs(T2,df=3)+bs(T3,df=3)+bs(jj,df=3),
              family=binomial, data=boot, subset=Z==1&Cl==0&R==1)
    return(fitY)
  }
  
  # PseudoR2(fitY)
  
  mL<-c(mT1_0,mT2_0,mT3_0,mX_0,mZ_0,mC_0,mS_0,mD_0,mY_0,
        mT1_1,mT2_1,mT3_1,mX_1,mZ_1,mC_1,mS_1,mD_1,mY_1)
  
  # start_time <- Sys.time()
  fitR<-lapply(1:18,function(x) mL[[x]](k))
  # end_time <- Sys.time()
  # end_time - start_time
  
  
  m1<-list(fitR[[1]],fitR[[2]],fitR[[3]],fitR[[4]],fitR[[5]],fitR[[6]],fitR[[7]],fitR[[8]],fitR[[9]])
  m2<-list(fitR[[10]],fitR[[11]],fitR[[12]],fitR[[13]],fitR[[14]],fitR[[15]],fitR[[16]],fitR[[17]],fitR[[18]])
  fitR<-list(m1,m2)
  
  
  
  cat('\n')
  cat("Creating monte carlo dataset",'\n')
  
  # Step 1: select baseline covariates
  MC0 <- boot[boot$j==1,]
  
  cat('\n')
  cat("Sampling (with replacement) Monte Carlo Sample from each treatment arm",'\n')
  
  # Sample with replacement from each treatment arm
  spl <- split(MC0, list(MC0$R))
  samples <- lapply(spl, function(x) x[sample(1:nrow(x), montecarlo/length(spl), replace=T),])
  MC <- rbindlist(samples)
  MC$id_ori <- MC$id  
  MC$id <- 1:montecarlo
  
  if(seed == 0){
    cat('\n')
    cat("The Monte Carlo Dataset",'\n')
    print(head(MC))
    
    cat('\n')
    cat("The Monte Carlo Dataset: Randomization Table",'\n')
    print(table(MC$R))
  }
  
  cat('\n')
  cat("Simulating follow-up using parametric fits and baseline resampled data",'\n')
  
  # pgf function for predicting follow-up
  #-----------------------------------------------------------------------------
  # Set up prediction function
  pFunc <- function(mod, ndat) {
    as.numeric(predict(mod, newdata = ndat, type = "response") > runif(1))
  }
  
  
  # pgf function
  pgf <- function(ii, mc_data, length, randomization = NULL, adherence = NULL, 
                  threshold = NULL, censoring = NULL, interaction = NULL) {
    cat("...", ii)
    d <- mc_data
    d <- d[d$id==ii, ]
    lngth <- length
    
    Vp <- Rp <- Trp <- T1p <- T2p <- T3p <- Zp <- Xp <- Cp <- Sp <- Dp <- Yp <- mm <- c_index <- cj <- GA <- numeric()
    
    mm[1] <- j <- 1
    id <- d$id
    id_ori <- d$id_ori
    Vp <- d[, c("V1", "V2", "V3", "V4", "V5", "V6", "V7","V8","V9","V10","V11","V12")]
    # cJ <- 99
    
    # Randomization
    if (is.null(randomization)) {
      Rp <- d$R
    } else {
      Rp <- randomization
    }
    
    # Treatment
    Trp[1] <- ifelse(Rp == 1, "Calcium", "Placebo")
    
    # Adherence
    if (is.null(adherence)) {
      Xp[1] <- d$X
    } else {
      Xp[1] <- adherence
    }
    
    # # Define adherence
    # if(!is.null(threshold)){
    #   Xp <- ifelse(Xp > threshold, 1, 0)
    # }
    
    
    T1p[1] <- d$T1
    T2p[1] <- d$T2
    T3p[1] <- d$T3
    Zp[1] <- d$Z
    
    
    # Conception month index
    if(Zp[1] == 1){cJ <- 1}
    
    GA[1] <- 99
    
    if (Zp[1] == 1) {
      c_index[1] <- 1
    } else {
      c_index[1] <- 99
    }
    
    cj[1] <- 0
    
    # withdrawal
    if(is.null(censoring)){
      Cp[1] <- d$C
    } else{
      Cp[1] <- censoring
    }
    
    # efuwp
    if(Zp[1]==0&Cp[1]==0){
      Sp[1] <- d$S
    } else{
      Sp[1] <- 0
    }
    
    
    # Outcome
    Dp[1] <- Yp[1] <- 0
    
    
    for (l in 2:lngth) {
      if (Yp[l-1]==0 & Dp[l-1]==0 & Cp[l-1]==0 & Sp[l-1]==0 &(l-c_index[l-1])<18) { 
        if (l == 2) {
          Zl1 <- 0
          Xl1 <- Xp[1]
        } else {
          Xl1 <- Xp[l-2]
          Zl1 <- Zp[l-2]
        }
        
        Xl=Xp[l-1]
        Zl=Zp[l-1]
        T1l=T1p[l-1]
        T2l=T2p[l-1]
        T3l=T3p[l-1]
        
        # Time-varying treatment: If >20 weeks of gestation ==> usual care
        if((l-c_index[l-1])<5){
          Trp[l] <- ifelse(Rp == 1, "Calcium", "Placebo")
        } else {
          Trp[l] <- "UsualCare"
        }
        
        
        
        
        # cat("Generating any health problem",'\n')
        dT1p <- data.frame(Vp,  Xl = Xp[l-1], Xl1, Tr=Trp[l], 
                           Zl = Zp[l-1], T1l = T1p[l-1], 
                           T2l = T2p[l-1], T3l = T3p[l-1], 
                           jj=as.numeric((l-mean_j)/sd_j))
        T1p[l] <- pFunc(fitR[[Rp+1]][[1]], dT1p)
        
        # cat("Generating SBP",'\n')
        dT2p <- data.frame(Vp,  Xl = Xp[l-1], Xl1, Tr=Trp[l],
                           Zl = Zp[l-1], T1l = T1p[l-1], 
                           T2l = T2p[l-1], T3l = T3p[l-1], 
                           jj=as.numeric((l-mean_j)/sd_j))
        T2p[l] <- predict(fitR[[Rp+1]][[2]],dT2p)
        
        
        # cat("Generating DBP",'\n')
        dT3p <- data.frame(Vp,  Xl = Xp[l-1], Xl1, Tr=Trp[l],
                           Zl = Zp[l-1], T1l = T1p[l-1], 
                           T2l = T2p[l-1], T3l = T3p[l-1], 
                           jj=as.numeric((l-mean_j)/sd_j))
        T3p[l] <- predict(fitR[[Rp+1]][[3]], dT3p)
        
        
        # cat("Generating Compliance",'\n')
        # if (is.null(adherence)) {
        #   dXp <- data.frame(Vp, Xl = Xp[l-1], Xl1, Tr=Trp[l],
        #                     Zl = Zp[l-1],
        #                     T1 = T1p[l], T1l = T1p[l-1], 
        #                     T2 = T2p[l], T2l = T2p[l-1], 
        #                     T3 = T3p[l], T3l = T3p[l-1],
        #                     jj=as.numeric((l-mean_j)/sd_j))
        #   Xp[l] <- pFunc(fitR[[Rp+1]][[4]], dXp)
        # } else {
        #   Xp[l] <- adherence
        # }
        
        # Compliance to match with the study design
        # We only define adherence protocol until 20 weeks of gestation
        # Therefore, we can set adherence to calcium or placebo before 20 weeks
        if (!is.null(adherence) & (l-c_index[l-1])<5) {
          Xp[l] <- adherence 
        } else {
          dXp <- data.frame(Vp, Xl = Xp[l-1], Xl1, Tr=Trp[l],
                            Zl = Zp[l-1],
                            T1 = T1p[l], T1l = T1p[l-1], 
                            T2 = T2p[l], T2l = T2p[l-1], 
                            T3 = T3p[l], T3l = T3p[l-1],
                            jj=as.numeric((l-mean_j)/sd_j))
          Xp[l] <- pFunc(fitR[[Rp+1]][[4]], dXp)
        }
        
        # Here we set scenario where individuals from placebo group adhere to
        # placebo for certain of months after conception then switch to treatment 
        # for the remaining of follow-up
        
        
        if(!is.null(interaction)) {
          if (!is.null(adherence)) {
            if (adherence == 0) {
              if (l >= (c_index[l] + interaction)) {
                Xp[l] <- 1
              }
            }
          }
        }
        
        
        # cat("Generating Conception",'\n')
        dZp <- data.frame(Vp, X = Xp[l], Xl = Xp[l-1], Xl1,Tr=Trp[l],
                          T1 = T1p[l], T1l = T1p[l-1],
                          T2 = T2p[l], T2l = T2p[l-1], 
                          T3 = T3p[l], T3l = T3p[l-1], 
                          jj=as.numeric((l-mean_j)/sd_j))
        
        if (Zp[l - 1] == 0) {
          Zp[l] <- pFunc(fitR[[Rp+1]][[5]], dZp)
        } else {
          Zp[l] <- 1
        }
        
        
        if (Zp[l] == 1 & Zp[l-1] == 0) {
          c_index[l] <- l
        } else {
          c_index[l] <- c_index[l-1]
        }
        
        # another index but 0 instead of 99
        cj[l] <- c_index[l]
        cj[l] = ifelse(cj[l]==99, 0, cj[l])
        
        # withdrawal
        # cat("Generating Withdrawal",'\n')
        dCp <- data.frame(Vp, X = Xp[l], Xl = Xp[l-1], Xl1,Tr=Trp[l],
                          Z = Zp[l], Zl = Zp[l-1],
                          T1 = T1p[l], T1l = T1p[l-1],
                          T2 = T2p[l], T2l = T2p[l-1], 
                          T3 = T3p[l], T3l = T3p[l-1], 
                          jj=as.numeric((l-mean_j)/sd_j))
        
        if(is.null(censoring)){
          Cp[l] <- pFunc(fitR[[Rp+1]][[6]], dCp)
        } else{
          Cp[l] <- 0
        }
        
        # Restrict to those who withdrawal before 20 weeks
        if(l-c_index[l] > 5){Cp[l] <- 0}
        
        # efuwp
        # cat("Generating EFUWP",'\n')
        dSp <- data.frame(Vp, X = Xp[l], Xl = Xp[l-1], Xl1,Tr=Trp[l],
                          Z = Zp[l], Zl = Zp[l-1],
                          T1 = T1p[l], T1l = T1p[l-1],
                          T2 = T2p[l], T2l = T2p[l-1], 
                          T3 = T3p[l], T3l = T3p[l-1], 
                          jj=as.numeric((l-mean_j)/sd_j))
        
        if(Zp[l]==0& Cp[l]==0 & l < 60){
          Sp[l] <- pFunc(fitR[[Rp+1]][[7]], dSp)
        } else {
          Sp[l] <- 0
        }
        
        # efuwp
        # cat("Generating early pregnancy loss",'\n')
        dDp <- data.frame(Vp, X = Xp[l], Xl = Xp[l-1], Xl1,Tr=Trp[l],
                          Z = Zp[l], Zl = Zp[l-1],
                          T1 = T1p[l], T1l = T1p[l-1],
                          T2 = T2p[l], T2l = T2p[l-1], 
                          T3 = T3p[l], T3l = T3p[l-1], 
                          jj=as.numeric((l-mean_j)/sd_j))
        
        Dp[l] <- 0
        if(Zp[l]==1 & Cp[l]==0 & (l-cj[l]) < 5){
          Dp[l] <- pFunc(fitR[[Rp+1]][[8]],dDp)
        }
        
        
        
        # Generating pre-eclampsia 
        # cat("Generating pre-eclampsia ",'\n')
        dYp <- data.frame(Vp, X = Xp[l], Xl = Xp[l-1], Xl1,Tr=Trp[l],
                          Z = Zp[l], Zl = Zp[l-1],
                          T1 = T1p[l], T1l = T1p[l-1],
                          T2 = T2p[l], T2l = T2p[l-1], 
                          T3 = T3p[l], T3l = T3p[l-1], 
                          jj=as.numeric((l-mean_j)/sd_j))
        
        Yp[l] <- 0
        if(Zp[l]==1 & Cp[l]==0 & Dp[l]==0){
          Yp[l] <- pFunc(fitR[[Rp+1]][[9]],dYp)
        } 
        
        if(Zp[l]==1){
          GA[l] <- l - cj[l]
        } else{
          GA[l] <- 99
        }
        
      } else {
        break
      }
      mm[l] <- l
    }
    
    boot_num <- seed
    gdat <- data.frame(boot_num,id,id_ori,mm,Vp,Rp,T1p,T2p,T3p,Zp,Xp,Cp,Sp,Yp,Dp,Trp,c_index, GA)
    gdat$lastid <- as.numeric(!duplicated(gdat$id, fromLast = T))
    gdat$last <- as.numeric(!(gdat$Cp==0)|!(gdat$Sp==0)|!(gdat$Yp==0)|!(gdat$Dp==0)|gdat$lastid==1|gdat$mm==lngth)
    # gdat <- gdat[gdat$last == 1 & gdat$Zp==1, ] |> as.data.frame()
    return(gdat)
  }
  # Running parallel
  r <-  plyr::llply(1:montecarlo, function(i) pgf(ii = i, mc_data = MC, length = length, censoring = censoring,
                                                  randomization = randomization, adherence = adherence, 
                                                  interaction = interaction), .parallel = TRUE)
  results <- do.call(rbind, r)
  cat('\n')
  cat("End of run for SEED", seed,'\n')
  return(results)
}



#---------- Natural course
#===============================================================================
start_time <- Sys.time()
r_nc <-  lapply(1:10, function(i) g_boot(data = imp, nimp = i, threshold = 999, 
                                         montecarlo = 10000, randomization = NULL,
                                         adherence = NULL, interaction = NULL, 
                                         preg_cens = 12, length = 24, seed = 0))
nc_df <- do.call(rbind, r_nc)
elapsed_time <- Sys.time()-start_time
print(elapsed_time)


mean(nc_df[nc_df$last == 1 & nc_df$Zp==1 & nc_df$Rp==1, ]$Yp)
mean(nc_df[nc_df$last == 1 & nc_df$Zp==1 & nc_df$Rp==0, ]$Yp)

mean(nc_df[nc_df$last == 1 & nc_df$Rp==1, ] $Yp)
mean(nc_df[nc_df$last == 1 & nc_df$Rp==0, ] $Yp)

saveRDS(nc_df, file = "allImpute_preecl_NC_cens12.RDS")

gc()

#---------- ITT
#===============================================================================
start_time <- Sys.time()
r_treat <-  lapply(1:10, function(i) g_boot(data = imp, nimp = i, threshold = 0, 
                                            montecarlo = 5000, randomization = 1,
                                            adherence = NULL, interaction = NULL, 
                                            preg_cens = 12, length = 24, seed = 0))
treated_ITT <- do.call(rbind, r_treat)

r_placebo <-  lapply(1:10, function(i) g_boot(data = imp, nimp = i, threshold = 0,
                                              montecarlo = 5000, randomization = 0,
                                              adherence = NULL, interaction = NULL, 
                                              preg_cens = 12, length = 24, seed = 0))
placebo_ITT <- do.call(rbind, r_placebo)
elapsed_time <- Sys.time()-start_time
print(elapsed_time)

gc()

mean(treated_ITT[treated_ITT$last == 1 & treated_ITT$Zp==1, ] $Yp)
mean(placebo_ITT[placebo_ITT$last == 1 & placebo_ITT$Zp==1, ] $Yp)

mean(treated_ITT[treated_ITT$last == 1,] $Yp)
mean(placebo_ITT[placebo_ITT$last == 1,] $Yp)


save(treated_ITT, placebo_ITT, file = "allImpute_ITT_cens12.RData")


#---------- PP analysis
#===============================================================================
start_time <- Sys.time()
r_treat <-  lapply(1:10, function(i) g_boot(data = imp, nimp = i, threshold = 0.8, 
                                            montecarlo = 5000, randomization = 1,
                                            adherence = 1, interaction = NULL, 
                                            preg_cens = 12, length = 24, seed = 0))
treated_pp <- do.call(rbind, r_treat)

r_placebo <-  lapply(1:10, function(i) g_boot(data = imp, nimp = i, threshold = 0.8,
                                              montecarlo = 5000, randomization = 0,
                                              adherence = 1, interaction = NULL, 
                                              preg_cens = 12, length = 24, seed = 0))
placebo_pp <- do.call(rbind, r_placebo)
elapsed_time <- Sys.time()-start_time
print(elapsed_time)

gc()

mean(treated_pp[treated_pp$last == 1 & treated_pp$Zp==1, ]$Yp)
mean(placebo_pp[placebo_pp$last == 1 & placebo_pp$Zp==1, ]$Yp)

mean(treated_pp[treated_pp$last == 1,]$Yp)
mean(placebo_pp[placebo_pp$last == 1,]$Yp)


save(treated_pp, placebo_pp, file = "allImpute_preecl_PP_cens12.RData")




#---------- Bootstrap for PP
#===============================================================================
bootFunc <- function(seedID){
  r_treat <-  lapply(1:10, function(i) g_boot(data = imp, nimp = i, threshold = 0.8, 
                                              montecarlo = 2000, randomization = 1,
                                              adherence = 1, interaction = NULL, 
                                              preg_cens = 18, length = 36, seed = seedID))
  treated_pp <- do.call(rbind, r_treat)
  
  r_placebo <-  lapply(1:10, function(i) g_boot(data = imp, nimp = i, threshold = 0.8,
                                                montecarlo = 2000, randomization = 0,
                                                adherence = 1, interaction = NULL, 
                                                preg_cens = 18, length = 36, seed = seedID))
  placebo_pp <- do.call(rbind, r_placebo)
  

  # Proportion among overall population
  mTreat_all <- mean(treated_pp[treated_pp$last == 1,]$Yp)
  mContr_all <- mean(placebo_pp[placebo_pp$last == 1,]$Yp)
  
  # Proportion among pregnant women
  mTreat_preg <- mean(treated_pp[treated_pp$last == 1 & treated_pp$Zp==1, ]$Yp)
  mContr_preg <- mean(placebo_pp[placebo_pp$last == 1 & placebo_pp$Zp==1, ]$Yp)
  
  # RD and RR among overall population
  RD_all <- mTreat_all - mContr_all
  RR_all <- mTreat_all/ mContr_all
  
  # RD and RR among pregnant women
  RD_preg <- mTreat_preg - mContr_preg
  RR_preg <- mTreat_preg/ mContr_preg
  
  # Save the results
  return(list(bootnum = seedID,
         mTreat_all  = mTreat_all,
         mContr_all  = mContr_all,
         mTreat_preg = mTreat_preg,
         mContr_preg = mContr_preg,
         RD_all      = RD_all,
         RR_all      = RR_all,
         RD_preg     = RD_preg,
         RR_preg     = RR_preg))
}

start_time <- Sys.time()
df_boot_pp <- NULL
for (seed_boot in c(1:100)) {
  r_boot_pp <- bootFunc(seed_boot)
  df_boot <- t(r_boot_pp) |> as.data.frame()
  df_boot_pp <- rbind(df_boot_pp, df_boot)
}
elapsed_time <- Sys.time()-start_time
print(elapsed_time)

saveRDS(df_boot_pp, file = "allImpute_preecl_boot_PP_cens_100.RDS")

x <- readRDS("allImpute_preecl_boot_PP_cens_100.RDS")
gc()

quantile(unlist(x$mTreat_all), probs = c(0.025, 0.975))
quantile(unlist(x$mContr_all), probs = c(0.025, 0.975))

quantile(unlist(x$mTreat_preg), probs = c(0.025, 0.975))
quantile(unlist(x$mContr_preg), probs = c(0.025, 0.975))

quantile(unlist(x$RD_all), probs = c(0.025, 0.975))
quantile(unlist(x$RD_preg), probs = c(0.025, 0.975))

quantile(unlist(x$RR_all), probs = c(0.025, 0.975))
quantile(unlist(x$RR_preg), probs = c(0.025, 0.975))



#===============================================================================

#---------- Bootstrap for NC
#===============================================================================
bootFunc_NC <- function(seedID){
  r_nc <- lapply(1:10, function(i) g_boot(data = imp, nimp = i, threshold = 999, 
                                           montecarlo = 2000, randomization = NULL,
                                           adherence = NULL, interaction = NULL, 
                                          preg_cens = 18, length = 36, seed = seedID))
  nc_df <- do.call(rbind, r_nc)

  # Proportion among overall population
  mTreat_all <- mean(nc_df[nc_df$last == 1 & nc_df$Rp==1, ]$Yp)
  mContr_all <- mean(nc_df[nc_df$last == 1 & nc_df$Rp==0, ]$Yp)
  
  # Proportion among pregnant women
  mTreat_preg <- mean(nc_df[nc_df$last == 1 & nc_df$Zp==1 & nc_df$Rp==1, ]$Yp)
  mContr_preg <- mean(nc_df[nc_df$last == 1 & nc_df$Zp==1 & nc_df$Rp==0, ]$Yp)
  
  # RD and RR among overall population
  RD_all <- mTreat_all - mContr_all
  RR_all <- mTreat_all/ mContr_all
  
  # RD and RR among pregnant women
  RD_preg <- mTreat_preg - mContr_preg
  RR_preg <- mTreat_preg/ mContr_preg
  
  # Save the results
  return(list(bootnum = seedID,
              mTreat_all  = mTreat_all,
              mContr_all  = mContr_all,
              mTreat_preg = mTreat_preg,
              mContr_preg = mContr_preg,
              RD_all      = RD_all,
              RR_all      = RR_all,
              RD_preg     = RD_preg,
              RR_preg     = RR_preg))
}

start_time <- Sys.time()
df_boot_NC <- NULL

for (seed_boot in c(1:48, 50:94, 96:102)) {
  r_boot_NC <- bootFunc_NC(seed_boot)
  df_boot <- t(r_boot_NC) |> as.data.frame()
  df_boot_NC <- rbind(df_boot_NC, df_boot)
}
elapsed_time <- Sys.time()-start_time
print(elapsed_time)

saveRDS(df_boot_NC, file = "allImpute_preecl_boot_NC_cens_100.RDS")

x <- readRDS("allImpute_preecl_boot_NC_cens_100.RDS")
gc()

quantile(unlist(x$mTreat_all), probs = c(0.025, 0.975))
quantile(unlist(x$mContr_all), probs = c(0.025, 0.975))

quantile(unlist(x$mTreat_preg), probs = c(0.025, 0.975))
quantile(unlist(x$mContr_preg), probs = c(0.025, 0.975))

quantile(unlist(x$RD_all), probs = c(0.025, 0.975))
quantile(unlist(x$RD_preg), probs = c(0.025, 0.975))

quantile(unlist(x$RR_all), probs = c(0.025, 0.975))
quantile(unlist(x$RR_preg), probs = c(0.025, 0.975))






#---------- Bootstrap for ITT
#===============================================================================
bootFunc_ITT <- function(seedID){
  r_treat <-  lapply(1:10, function(i) g_boot(data = imp, nimp = i, threshold = 0, 
                                              montecarlo = 2000, randomization = 1,
                                              adherence = NULL, interaction = NULL, 
                                              preg_cens = 18, length = 36, seed = seedID))
  treated_ITT <- do.call(rbind, r_treat)
  
  r_placebo <-  lapply(1:10, function(i) g_boot(data = imp, nimp = i, threshold = 0,
                                                montecarlo = 2000, randomization = 0,
                                                adherence = NULL, interaction = NULL, 
                                                preg_cens = 18, length = 36, seed = seedID))
  placebo_ITT <- do.call(rbind, r_placebo)
  
  
  # Proportion among overall population
  mTreat_all <- mean(treated_ITT[treated_ITT$last == 1,]$Yp)
  mContr_all <- mean(placebo_ITT[placebo_ITT$last == 1,]$Yp)
  
  # Proportion among pregnant women
  mTreat_preg <- mean(treated_ITT[treated_ITT$last == 1 & treated_ITT$Zp==1, ]$Yp)
  mContr_preg <- mean(placebo_ITT[placebo_ITT$last == 1 & placebo_ITT$Zp==1, ]$Yp)
  
  # RD and RR among overall population
  RD_all <- mTreat_all - mContr_all
  RR_all <- mTreat_all/ mContr_all
  
  # RD and RR among pregnant women
  RD_preg <- mTreat_preg - mContr_preg
  RR_preg <- mTreat_preg/ mContr_preg
  
  # Save the results
  return(list(bootnum = seedID,
              mTreat_all  = mTreat_all,
              mContr_all  = mContr_all,
              mTreat_preg = mTreat_preg,
              mContr_preg = mContr_preg,
              RD_all      = RD_all,
              RR_all      = RR_all,
              RD_preg     = RD_preg,
              RR_preg     = RR_preg))
}


start_time <- Sys.time()
df_boot_ITT <- NULL
for (seed_boot in c(46:48, 50:104)) {
  r_boot_ITT <- bootFunc_ITT(seed_boot)
  df_boot <- t(r_boot_ITT) |> as.data.frame()
  df_boot_ITT <- rbind(df_boot_ITT, df_boot)
}
elapsed_time <- Sys.time()-start_time
print(elapsed_time)


saveRDS(df_boot_ITT, file = "allImpute_preecl_boot_ITT_cens_100.RDS")

x <- readRDS("allImpute_preecl_boot_ITT_cens_100.RDS")
gc()

quantile(unlist(x$mTreat_all), probs = c(0.025, 0.975))
quantile(unlist(x$mContr_all), probs = c(0.025, 0.975))

quantile(unlist(x$mTreat_preg), probs = c(0.025, 0.975))
quantile(unlist(x$mContr_preg), probs = c(0.025, 0.975))

quantile(unlist(x$RD_all), probs = c(0.025, 0.975))
quantile(unlist(x$RD_preg), probs = c(0.025, 0.975))

quantile(unlist(x$RR_all), probs = c(0.025, 0.975))
quantile(unlist(x$RR_preg), probs = c(0.025, 0.975))






