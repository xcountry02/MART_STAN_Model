setwd("C:/Users/kmichels/OneDrive - Critical Path Institute/Desktop/Alzheimers/STAN Models/MART Model/data/")

resCSV <- read.csv("predementia_datamart_draft_with_adas11_2019-08-30.csv")

resDAT <- cbind(as.character(resCSV$usubjid), # "USUBJID"
                as.character(resCSV$pstudyid), # "STUDYID"
                resCSV$adas11_score, # "ADAS11"
                as.character(resCSV$gender), # "SEX"
                resCSV$pblage, # "AGE"
                resCSV$daysfrompbl, # "DAYSFROMPBL"
                resCSV$apoe4counts, # "APOE4"
                resCSV$pblmcitype, # "PBLMCITYPE"
                resCSV$mcitype, # "MCITYPE"
                resCSV$apoe1, # "APOE1"
                resCSV$apoe2, # "APOE2"
                resCSV$apoe4carrier, # "APOE4CARRIER"
                resCSV$agescan, # "AGESCAN"
                resCSV$pblhvr_leap, # "PBLHVR_LEAP"
                resCSV$pblhvl_leap, # "PBLHVL_LEAP"
                resCSV$pblhvtotal_leap, # "PBLHVTOTAL_LEAP"
                resCSV$pblhvr_fs, # "PBLHVR_FS"
                resCSV$pblhvl_fs, # "PBLHVL_FS"
                resCSV$pblhvtotal_fs, # "PBLHVTOTAL_FS"
                as.character(resCSV$hvunits), # "HVUNITS"
                resCSV$asfval, # "ASFVAL"
                resCSV$amyloid_positive_pbl, # "AMYLOID_POSITIVE_PBL"
                resCSV$amyloid_positive_pbl_upto_120mo, # "AMYLOAD_POSITIVE_PBL_UPTO_120MO"
                resCSV$cdrsb, # "CDRSB"
                resCSV$weight, # "WEIGHT"
                as.character(resCSV$weightunit), # "WEIGHTUNITS",
                resCSV$height, # "HEIGHT"
                as.character(resCSV$heightunit),# "HEIGHTUNITS"
                as.character(resCSV$visit)) # "VISIT"
res.df <- data.frame(resDAT)

colnames(res.df) <- c("USUBJID", 
                      "STUDYID", 
                      "ADAS11", 
                      "SEX", 
                      "AGE", 
                      "DAYSFROMPBL", 
                      "APOE4", 
                      "PBLMCITYPE", 
                      "MCITYPE", 
                      "APOE1", 
                      "APOE2", 
                      "APOE4CARRIER",
                      "AGESCAN", 
                      "PBLHVR_LEAP", 
                      "PBLHVL_LEAP", 
                      "PBLHVTOTAL_LEAP", 
                      "PBLHVR_FS",
                      "PBLHVL_FS", 
                      "PBLHVTOTAL_FS",
                      "HVUNITS", 
                      "ASFVAL", 
                      "AMYLOID_POSITIVE_PBL",
                      "AMYLOAD_POSITIVE_PBL_UPTO_120MO",
                      "CDRSB",
                      "WEIGHT", 
                      "WEIGHTUNITS",
                      "HEIGHT", 
                      "HEIGHTUNITS",
                      "VISIT") 

res.df[,3] <- as.numeric(as.character(res.df[,3]))
res.df[,5] <- as.numeric(as.character(res.df[,5]))
res.df[,6] <- as.numeric(as.character(res.df[,6]))
res.df[,7] <- as.numeric(as.character(res.df[,7]))
res.df[,8] <- as.numeric(as.character(res.df[,8]))
res.df[,9] <- as.numeric(as.character(res.df[,9]))
res.df[,10] <- as.numeric(as.character(res.df[,10]))
res.df[,11] <- as.numeric(as.character(res.df[,11]))
res.df[,12] <- as.numeric(as.character(res.df[,12]))
res.df[,13] <- as.numeric(as.character(res.df[,13]))
res.df[,14] <- as.numeric(as.character(res.df[,14]))
res.df[,15] <- as.numeric(as.character(res.df[,15]))
res.df[,16] <- as.numeric(as.character(res.df[,16]))
res.df[,17] <- as.numeric(as.character(res.df[,17]))
res.df[,18] <- as.numeric(as.character(res.df[,18]))
res.df[,19] <- as.numeric(as.character(res.df[,19]))
# res.df[,20] <- as.numeric(as.character(res.df[,20]))
res.df[,21] <- as.numeric(as.character(res.df[,21]))
res.df[,22] <- as.numeric(as.character(res.df[,22]))
res.df[,23] <- as.numeric(as.character(res.df[,23]))
res.df[,24] <- as.numeric(as.character(res.df[,24]))
res.df[,25] <- as.numeric(as.character(res.df[,25]))
res.df[,26] <- as.numeric(as.character(res.df[,26]))
# res.df[,27] <- as.numeric(as.character(res.df[,27]))
res.df[,28] <- as.numeric(as.character(res.df[,28]))
# res.df[,29] <- as.numeric(as.character(res.df[,29]))

### Must have a score, no score must be removed
# indBASE <- which(res.df$VISIT == "BASELINE")
# indNA <- which(is.na(res.df$ADAS11) == TRUE)

# indMV <- intersect(indNA, indBASE)
# usub <- res.df$USUBJID[indMV]
# res.df[res.df$USUBJID == usub, ]

# res.df <- res.df[-indNA, ]

### Remove ADAS scofes of less than 0 because that doesn't make sense
# indREM <- which(res.df$ADAS11 < 0)
# res.df <- res.df[-indREM, ]

res.df$SEX_NF <- NA
res.df$SEX_NF[res.df$SEX == "F"] <- 1
res.df$SEX_NF[res.df$SEX == "M"] <- 0

indSEXrem <- which(is.na(res.df$SEX_NF)== TRUE)
if (length(indSEXrem) > 0) {
  res.df <- res.df[-indSEXrem, ]
}


### Age, I'm not going to remove these, I'm going to replace them with the value 89, since the patients are larger than 89
indAGElarge <- which(res.df$AGE == 999)
if (length(indAGElarge) > 0) {
  res.df$AGE[indAGElarge] <- 90
}


### Time
### Still use this. check if complete information, then just add the minimu
indTIMErem <- which(res.df$DAYSFROMPBL  < 0)
if (length(indTIMErem) > 0) {
  res.df <- res.df[-indTIMErem, ]
}



### Convert APOE4 -1000 to 0.72
# indAPOE <- which(res.df$APOE4 == -1000)

### One can remove the ones without values or 
# res.df <- res.df[-indAPOE, ]0
# res.df$APOE4[indAPOE] <- 0.72



### Put the Himppocampus Volume in the model
indPBLHVTOTAL_FSrem <- which(res.df$PBLHVTOTAL_FS == -1000)

### One can remove the ones without values or 
res.df <- res.df[-indPBLHVTOTAL_FSrem, ]


# This makes sure the daysfrompbl are sorted by 
resTMP <- c()
# resTMP1 <- res.df[!res.df$USUBJID %in% uIDS,]
IDs <- unique(res.df$USUBJID)
for (id in IDs) {
  indID <- which(res.df$USUBJID == id)
  tmp <- res.df[indID, ]
  tmp <- tmp[order(tmp$DAYSFROMPBL), ]
  tmp$DAYSFROMPBL <- tmp$DAYSFROMPBL - tmp$DAYSFROMPBL[1]
  if (length(resTMP) == 0) {
    resTMP <- tmp
  } else {
    resTMP <- rbind(resTMP, tmp)
  }

}
res.df <- resTMP

### Convert time to years
res.df$TIME <- res.df$DAYSFROMPBL / 365

### Normalize the 
res.df$PBLHV <- res.df$PBLHVTOTAL_LEAP / res.df$ASFVAL

resLST <- vector("list", 2)
names(resLST) <- c("CDRSB", "ADAS11")
indNA <- which(is.na(res.df$CDRSB) == TRUE)
resLST$CDRSB <- res.df[-indNA,]
indNA <- which(is.na(res.df$ADAS11) == TRUE)
resLST$ADAS11 <- res.df[-indNA,]
indREM <- which(resLST$ADAS11$ADAS11 < 0)
resLST$ADAS11 <- resLST$ADAS11[-indREM, ]


dat_list <- vector("list", 2)
names(dat_list) <- names(resLST)
for (id in names(resLST)) {
  resLST[[id]]$IDp <- NA
#  resLST[[id]]$IDs <- NA
  cnt.id <- 1
  tmpID <- resLST[[id]]$USUBJID[1]
  # cnt.sid <- 1
  # tmpSID <- resLST[[id]]$STUDYID[1]
  for (i in 1:nrow(resLST[[id]])) {
    if (tmpID != resLST[[id]]$USUBJID[i]) {
      tmpID <- resLST[[id]]$USUBJID[i]
      cnt.id <- cnt.id + 1
    }
    resLST[[id]]$IDp[i] <- cnt.id
    
    # if (tmpSID != resLST[[id]]$STUDYID[i]) {
    #   tmpSID <- resLST[[id]]$STUDYID[i]
    #   cnt.sid <- cnt.sid + 1
    # }
    # resLST[[id]]$IDs[i] <- cnt.sid
  }
  
#  M <- cnt.sid
  N <- nrow(resLST[[id]])
  P <- cnt.id

  eval(parse(text = paste0("ttl <- max(resLST[[id]]$", id, ")")))
  eval(parse(text = paste0("S1 <- resLST[[id]]$", id, " / ttl")))
  resLST[[id]]$S <- (S1*(P-1) + 0.5) / P
  
  dat_list[[id]] <- vector("list", 9)
  names(dat_list[[id]]) <- c("N", "P", "IDp", "AGE", "SEX", "APOE4", "time", "S", "PBLHV")
  dat_list[[id]]$N <- N
  dat_list[[id]]$P <- P
#  dat_list[[id]]$M <- M
  dat_list[[id]]$IDp <- resLST[[id]]$IDp
 # dat_list[[id]]$IDs <- resLST[[id]]$IDs
  dat_list[[id]]$AGE <- resLST[[id]]$AGE
  dat_list[[id]]$SEX <- resLST[[id]]$SEX_NF
  dat_list[[id]]$APOE4 <- resLST[[id]]$APOE4
  dat_list[[id]]$time <- resLST[[id]]$TIME
  dat_list[[id]]$S <- resLST[[id]]$S
  dat_list[[id]]$PBLHV <- (resLST[[id]]$PBLHV - mean(resLST[[id]]$PBLHV)) / sd(resLST[[id]]$PBLHV)
#  dat_list[[id]]$PBLHVFS <- (resLST[[id]]$PBLHVTOTAL_FS - mean(resLST[[id]]$PBLHVTOTAL_FS)) / sd(resLST[[id]]$PBLHVTOTAL_FS)
#  dat_list[[id]]$ASFVAL <- resLST[[id]]$ASFVAL
  
}



### Make sure to check that the baseline isn't removed.
# indBASE <- which(res.df$VISIT == "BASELINE")
# indNA <- which(is.na(res.df$ADAS11) == TRUE)

# indMV <- intersect(indNA, indBASE)
# usub <- res.df$USUBJID[indMV]
# res.df[res.df$USUBJID == usub, ]

# res.df <- res.df[-indNA, ]





 # dat_list <- vector("list", 10)
 # names(dat_list) <- c("N", "P", "M", "IDp", "IDs", "AGE", "SEX", "APOE4", "time", "S")
 # dat_list$N <- N
 # dat_list$P <- P
 # dat_list$M <- M
 # dat_list$IDp <- res.df$IDp
 # dat_list$IDs <- res.df$IDs
 # dat_list$AGE <- res.df$AGE
 # dat_list$SEX <- res.df$SEX_NF
 # dat_list$APOE4 <- res.df$APOE4
 # dat_list$time <- res.df$TIME
 # dat_list$S <- res.df$S
 
 
 # dat_list <- vector("list", 10)
 # names(dat_list) <- c("N", "P", "M", "IDp", "IDs", "AGE", "SEX", "APOE4", "time", "S")
 # dat_list$N <- 150
 # dat_list$P <- length(unique(res.df$IDp[c(1:50, 2425:2474, 2690:2739)]))
 # dat_list$M <- length(unique(res.df$IDs[c(1:50, 2425:2474, 2690:2739)]))
 # dat_list$IDp <- res.df$IDp[c(1:50, 2425:2474, 2690:2739)]
 # cnt.id <- 1
 # tmpID <- dat_list$IDp[1]
 # IDp <- rep(NA, length(dat_list$IDp))
 # for (i in 1:length(dat_list$IDp)) {
 #   if (tmpID != dat_list$IDp[i]) {
 #     tmpID <- dat_list$IDp[i]
 #     cnt.id <- cnt.id + 1
 #   }
 #   IDp[i] <- cnt.id
 # }
 # dat_list$IDp <- IDp
 # dat_list$IDs <- res.df$IDs[c(1:50, 2425:2474, 2690:2739)]
 # dat_list$AGE <- res.df$AGE[c(1:50, 2425:2474, 2690:2739)]
 # dat_list$SEX <- res.df$SEX_NF[c(1:50, 2425:2474, 2690:2739)]
 # dat_list$APOE4 <- res.df$APOE4[c(1:50, 2425:2474, 2690:2739)]
 # dat_list$time <- res.df$TIME[c(1:50, 2425:2474, 2690:2739)]
 # dat_list$S <- res.df$S[c(1:50, 2425:2474, 2690:2739)]
 
 save(dat_list, file = "dat_list.Rdata")
 
 
 
 # 
 # 
 # 
 # 
 # 
 # 
 # 
 # 
 # 
 # N <- dat_list$N;             			# number of total Observations
 # P <- dat_list$P;              			# number of subjects
 # M <- dat_list$M;                    # number of studies
 # IDp <- dat_list$IDp;            			# Patient ID 
 # IDs <- dat_list$IDs;            			# Study ID 
 # SEX <- dat_list$SEX;
 # AGE <- dat_list$AGE;
 # APOE4 <- dat_list$APOE4;
 # time <- dat_list$time;      			# time of observation (years)
 # S <- dat_list$S; 
 # PBLHVFS <- dat_list$PBLHVFS;           			# measured Hippocampus Volume FS
 # PBLHVLeap <- dat_list$PBLHVLeap;           		# measured Hippocampus Volume LEAP
 # ASFVAL <- dat_list$ASFVAL; 
 # 
 # 
 # theta_S0 = rnorm(1,.21,.01)
 # theta_r = rnorm(1,.01,0.001)
 # theta_SEX = rnorm(1,.953,0.01)
 # theta_AGE = rnorm(1,.01,0.001)
 # theta_APOE4_b = rnorm(1,.037,0.01)
 # theta_APOE4_r = rnorm(1,.01,0.001)
 # theta_HVFS = rnorm(1,.01,0.001)
 # theta_HVLEAP = rnorm(1,.01,0.001)
 # theta_ASFVAL = rnorm(1,.01,0.001)
 # tau = .01
 # beta = .01
 # omega_pb = rlnorm(1,log(.405)-.5,1)
 # omega_pr = rlnorm(1,log(.206)-.5)
 # omega_sb = rlnorm(1,log(.099)-.5)
 # omega_sr = rlnorm(1,log(.026)-.5)
 # eta_pb = rep(0,dat_list[[2]])
 # eta_pr = rep(0,dat_list[[2]])
 # eta_sb = rep(0,dat_list[[3]])
 # eta_sr = rep(0,dat_list[[3]])
 # kel = rnorm(1,.02,0.01)
 # keq = rnorm(1,.08,0.01)
 # beta_bateman = rnorm(1,0.1,0.001)
 # 
 # baseline_cov <- rep(NA, N);
 # rate_cov <- rep(NA, N);
 # S0 <- rep(NA, N);
 # r <- rep(NA, N);
 # muS <- rep(NA, N);
 # pbo <- rep(NA, N);
 # 
 # tau_trans = (tau+1)*80;
 # beta_trans = (beta +1)*5;
 # beta_bateman_trans = exp(beta_bateman-3.5);
 # keq_trans = exp(keq+1.88);
 # kel_trans = exp(kel+.46);
 # 
 # 
 # 
 # for(i in 1:N) {
 #    
 #   
 #   baseline_cov[i] = theta_S0*(1+theta_SEX*(SEX[i]-1))*(1+theta_APOE4_b*(APOE4[i]-0.72));
 #   
 #   rate_cov[i] = theta_r*(1+theta_AGE*(AGE[i]-73.5))*(1+theta_APOE4_r*(APOE4[i]-0.72))*(1+theta_HVFS*(PBLHVFS[i]))*(1+theta_HVLEAP*(PBLHVLeap[i]))*(1+theta_ASFVAL*(ASFVAL[i]-0.85));
 #   
 #   r[i] = rate_cov[i] + eta_pr[IDp[i]];
 #   
 #   S0[i] = baseline_cov[i]*exp(eta_pb[IDp[i]]);
 #   
 #   
 #   muS[i] = S0[i]/(S0[i]^beta_trans +(1-S0[i]^beta_trans)*exp(-beta_trans*r[i]*time[i]))^(1/beta_trans);
 #   
 # } 
 