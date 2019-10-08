


tdf <- function(pat_pop,TIME,per_treat){
  
  #Separate the above group by apoe4 numbers
  n <- length(pat_pop$IDp)
  alloc <- round(n*per_treat/100)
  trts <- c("Placebo", "Treatment")
  assignments <- rep(trts, times = c(alloc,n-alloc))
  
  # time_df <- data.frame(TIME)
  # 
  # trial_df <- expand.grid.df(pat_pop, time_df)
  
  
  #Organize patient info into dataframe (row = observation at time point)
  trial_df <- expand.grid(TIME = TIME ,IDp = pat_pop$IDp)
  trial_df$PBLHV <- pat_pop$PBLHV[trial_df$ID]
  trial_df$APOE4 <- pat_pop$APOE4[trial_df$ID]
  trial_df$SEX <- pat_pop$SEX[trial_df$ID]
  trial_df$bAGE <- pat_pop$AGE[trial_df$ID]
  trial_df$bADAS <- pat_pop$S[trial_df$ID]
  trial_df$Treatment <- assignments[trial_df$ID]
  trial_df$Treatment <- factor(trial_df$Treatment, level = trts)
  trial_df <- trial_df[order(trial_df$IDp,trial_df$TIME),]
  
  return(trial_df)
}

simADAS <- function(trial_df,dm_rate,bayes_flag){
  
  covariate_random_df <- covariate_random_calc(trial_df,bayes_flag)
  S0 <- covariate_random_df$S0
  
  
  #Modify rates in treatment group by disease modifying reduction
  rateinds <- which(covariate_random_df$r> 0 & covariate_random_df$Treatment == 'Treatment')
  covariate_random_df$r[rateinds]<- (1-dm_rate/100)*covariate_random_df$r[rateinds]
  
  #Generate mean trajectories with placebo effects
 
  t <- unique(trial_df$TIME)
  n <- length(unique(trial_df$ID))
 # num_obs <- length(trial_df$ID)
  
  muS <- c()
  for (i in 1:length(S0)) {
    for (j in 1:length(t)) {
      muS <- c(muS, abs(S0[i]/((S0[i]^beta + (1-S0[i]^beta)*exp(-beta*covariate_random_df$r[i]*(t[j]/52.1428))))^(1/beta)))
    }
  }
#  pbo <- numeric(num_obs)
 #  for(i in 1:num_obs){
 #    p <- (i-1) %/% length(t) + 1
 #    k <- (i-1) %% length(t) + 1
 #  #  pbo[i] <- beta_bateman*(exp(-kel*t[k])-exp(-keq*t[k]))
 # #   muS[i] <- abs(S0[p]/((S0[p]^beta + (1-S0[p]^beta)*exp(-beta*covariate_random_df$r[p]*(t[k]/52.1428))))^(1/beta))
 #    muS[i] <- S0[p]/(S0[p]^beta + (1-S0[p]^beta)*exp(-beta*covariate_random_df$r[p]*(t[k]/52.1428)))^(1/beta)
 #  }
  muS <-((muS)*(n-1)+.5)/n
  
  ADAS_scores <- rbeta(num_obs,muS*tau,(1-muS)*tau)
  ADAS_df <- data.frame(ADAS=ADAS_scores,Treatment=trial_df$Treatment)
  
  return(ADAS_df)
}








covariate_random_calc <- function(trial_df,bayes_flag){
  
  bl_trial_df <- trial_df[!duplicated(trial_df$ID),]
  num_pat <- length(bl_trial_df$ID)
  
  
  
  #Covariate effects using mean covariate estimates (Frequentist approach)
  if(bayes_flag == "no"){
    
    ### Below is the stan code
    ### rate_cov[i] = theta_r*(1+theta_AGE*(AGE[i]-75))*(1+theta_APOE4_r*(APOE4[i]-0.72))*theta_COMED;
    ### Below is the R code converted from above stan code
    ### rcov <-theta_r*(1+theta_AGE*(bl_trial_df$bAGE-75))*(1+theta_APOE4_r*(bl_trial_df$APOE4-0.72))*(ifelse(bl_trial_df$COMED==1,1,theta_COMED))
    
    ### Below is the stan code
    ### baseline_cov[i] = theta_S0*(1+theta_SEX*(SEX[i]-1))*(1+theta_APOE4_b*(APOE4[i]-0.72));
    ### Below is the R code converted from above stan code
    blcov <- bl_trial_df$bADAS*(1+theta_SEX*(bl_trial_df$SEX))*(1+theta_APOE4_b*(bl_trial_df$APOE4-0.72))
    
    
    ### Below is the stan code
    ### rate_cov[i] = theta_r*(1+theta_AGE*(AGE[i]-75))*(1+theta_APOE4_r*(APOE4[i]-0.72))*(1+theta_HV*(PBLHV[i]));
    ### Below is the R code converted from above stan code
    rcov <-theta_r*(1+theta_AGE*(bl_trial_df$bAGE-75))*(1+theta_APOE4_r*(bl_trial_df$APOE4-0.72))*(1+theta_HV*(bl_trial_df$PBLHV))
    
  } else {
    
    #Covariate effects using uncertainty in covariate estimates (Bayesian approach)
    n2 <- length(list_of_draws$theta_S0)
    inds <- sample(1:n2,num_pat,replace = TRUE)
    
    theta_r_bayes <- list_of_draws$theta_r[inds]
    theta_S0_bayes <- list_of_draws$theta_S0[inds]
    theta_SEX_bayes <- list_of_draws$theta_SEX[inds]
    theta_AGE_bayes <- list_of_draws$theta_AGE[inds]
    theta_APOE4_b_bayes <- list_of_draws$theta_APOE4_b[inds]
    theta_APOE4_r_bayes <- list_of_draws$theta_APOE4_r[inds]
    theta_COMED_bayes <- list_of_draws$theta_COMED[inds]
    
    blcov <- bl_trial_df$bADAS*(1+theta_SEX*(bl_trial_df$SEX))*(1+theta_APOE4_b*(bl_trial_df$APOE4-0.72))
 #   blcov <- theta_S0*(1+theta_SEX*(bl_trial_df$SEX))*(1+theta_APOE4_b*(bl_trial_df$APOE4-0.72))
    rcov <-theta_r*(1+theta_AGE*(bl_trial_df$bAGE-75))*(1+theta_APOE4_r*(bl_trial_df$APOE4-0.72))*(1+theta_HV*(bl_trial_df$PBLHV))
    
    
  }
  
  #Random study and patient effects on baseline and rate
  eta_pb <- rnorm(num_pat,0,omega_pb)
  #eta_pb <- rep(0,num_pat)
 # eta_sb <- rep(rnorm(1,0,omega_sb),num_pat)
  #eta_sb <- rep(0,num_pat)
  eta_pr <- rnorm(num_pat,0,omega_pr)
#  eta_sr <- rep(rnorm(1,0,omega_sr),num_pat)
  
  r <- rcov + eta_pr
  S0 <-blcov*exp(eta_pb)
  
  
  #Check if random effects put the baseline score outside of (0,1) and correct if so by generating new etas.
  oob_inds <- which(S0>1 | S0 < 0)
  while (length(oob_inds) > 0)
  {
    ntemp <- length(oob_inds)
    new_eta <-rnorm(ntemp,0,omega_pb)
    S0[oob_inds] <- blcov[oob_inds]*exp(new_eta)
    oob_inds <- which(S0>1)
  }
  
  #   if (length(r) == 0) {
  #    r <- rep(0, length(S0))
  #   }
  covariate_random_df <- data.frame(r=r,S0=S0,Treatment = bl_trial_df$Treatment)
  
  return(covariate_random_df)
}








load("C:/Users/kmichels/OneDrive - Critical Path Institute/Desktop/Alzheimers/STAN Models/MART Model/data/dat_list.Rdata")


for (dep in c("ADAS11", "CDRSB")) {
  dep <- "ADAS11"
#  filepath <- paste0("C:/Users/kmichels/OneDrive - Critical Path Institute/Desktop/Alzheimers/STAN Models/MART Model/", dep, "_PBLHV_LEAP_cauchy.rds")
 # filepath <- 
    fit <- readRDS(paste0(dep, "_PBLHV_LEAP_cauchy.rds"))
  
  list_of_draws <- vector("list", 12)
  names(list_of_draws) <- c("theta_S0", "theta_r", "theta_SEX", "theta_AGE", "theta_APOE4_b", "theta_APOE4_r", 
                            "theta_HV", "tau", "beta", "omega_pb", "omega_pr")
  
  
  new <- fit@sim
  new2 <- new$samples
  list_of_draws[["theta_S0"]] <- new2[[1]]$theta_S0
  list_of_draws[["theta_r"]] <- new2[[1]]$theta_r
  list_of_draws[["theta_SEX"]] <- new2[[1]]$theta_SEX
  list_of_draws[["theta_AGE"]] <- new2[[1]]$theta_AGE
  list_of_draws[["theta_APOE4_b"]] <- new2[[1]]$theta_APOE4_b
  list_of_draws[["theta_APOE4_r"]] <- new2[[1]]$theta_APOE4_r
  list_of_draws[["theta_HV"]] <- new2[[1]]$theta_HV
  list_of_draws[["tau"]] <- new2[[1]]$tau
  list_of_draws[["beta"]] <- new2[[1]]$beta
  list_of_draws[["omega_pb"]] <- new2[[1]]$omega_pb
  list_of_draws[["omega_pr"]] <- new2[[1]]$omega_pr
  
  # fit <- readRDS("C:/Users/kmichels/OneDrive - Critical Path Institute/Desktop/Alzheimers/STAN Models/MART Model/deliv/report/CDRSB_PBLHV_LEAP_cauchy.rds")
  fit_summary <- summary(fit)
  
#  theta_S0 <- fit_summary$summary[1,1]
  theta_S0 <- 0.118
#  theta_r <- fit_summary$summary[2,1]
  theta_r <- 0.063
#  theta_SEX <- fit_summary$summary[3,1]
  theta_SEX <- -0.099
#  theta_AGE <- fit_summary$summary[4,1]
  theta_AGE <- -0.006
#  theta_APOE4_b <- fit_summary$summary[5,1]
  theta_APOE4_b <- 0.241
#  theta_APOE4_r <- fit_summary$summary[6,1]
  theta_APOE4_r <- 0.390
#  theta_HV <- fit_summary$summary[7,1]
  theta_HV <- -.578
#  omega_pb <- fit_summary$summary[8,1]
  omega_pb <- 0.243
#  omega_pr <- fit_summary$summary[9,1]
  omega_pr <- 0.032
#  tau <- fit_summary$summary[10,1]
  tau <- -0.016
#  tau_trans <- (tau + 1)*80
#  beta <- fit_summary$summary[11,1]
  beta <- 2.785
#  beta_trans <- (beta + 1)*5
  eval(parse(text = paste0("datLST <- dat_list$", dep)))
  pat_pop <- data.frame("IDp" = datLST$IDp, "AGE" = datLST$AGE, "APOE4" = datLST$APOE4, "SEX" = datLST$SEX, "S" = datLST$S, "PBLHV" = datLST$PBLHV)
  TIME <- seq(0,52,by=4)
  per_treat <- 50
  trial_df <- tdf(pat_pop,TIME,per_treat)
  dm_rate <- 30
  bayes_flag <- "yes"
  disease <- simADAS(trial_df, dm_rate, bayes_flag)

  trial <- cbind(trial_df,disease)
  trial <- trial[,-10]

  dat <- data.frame("Time" = as.numeric(trial$TIME), "Disease" = as.numeric(trial$bADAS), "Subject" = as.factor(trial$IDp))
  dat <- unique(dat)
  
}

