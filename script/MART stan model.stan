data{

	int N;             			          // number of total Observations
  int P;              			        // number of subjects
//	int M;                            // number of studies
 	int IDp[N];            			      // Patient ID 
// 	int IDs[N];            			      // Study ID 
 	vector[N] SEX;
 	vector[N] AGE;
 	vector[N] APOE4;
  vector[N] time;      			        // time of observation (years)
	vector[N] S;           			      // measured ADAScog or CDRSB score
 	vector[N] PBLHV;           			  // measured Hippocampus Volume
 	vector[N] ASFVAL;           			// measured Affine Scaling Factor
}

parameters{

  	//Fixed effects and covariates 
  	real<lower=0,upper=1> theta_S0;         // Mean baseline score for average individual, orig no lower
  	real theta_r;                           // Mean progression rate for average individual, orig no lower
  	real theta_SEX;
  	real theta_AGE;
  	real theta_APOE4_b;
  	real theta_APOE4_r;
   	real theta_HV;
   	real theta_ASFVAL;
  	real tau;            
  	real beta;
    
  	//Inter-patient re
  	vector[P] eta_pb;                     	// re for patient baseline
  	vector[P] eta_pr;            		      // re for patient rate
	  real<lower=0> omega_pb;               	// std for patient baseline
	  real<lower=0> omega_pr;               	// std for patient rate
}
transformed parameters{
  
  	vector[N] baseline_cov;
  	vector[N] rate_cov;
  	vector[N] S0;
  	vector[N] r;
    vector<lower=0,upper=1>[N] muS;
    real tau_trans;
    real beta_trans;
    real keq_trans;
    real kel_trans;

    tau_trans = (tau+1)*80;
    beta_trans = (beta +1)*5;
    
    
  
  	for(i in 1:N) {

      	baseline_cov[i] = theta_S0*(1+theta_SEX*(SEX[i]-1))*(1+theta_APOE4_b*(APOE4[i]-0.72));
      	
      	rate_cov[i] = theta_r*(1+theta_AGE*(AGE[i]-75))*(1+theta_APOE4_r*(APOE4[i]-0.72))*(1+theta_HV*(PBLHV[i]))*(1+theta_ASFVAL*(ASFVAL[i]-0.85));
        
        r[i] = rate_cov[i] + eta_pr[IDp[i]];
        
      	S0[i] = baseline_cov[i]*exp(eta_pb[IDp[i]]);
      	
      	
      	muS[i] = S0[i]/(S0[i]^beta_trans +(1-S0[i]^beta_trans)*exp(-beta_trans*r[i]*time[i]))^(1/beta_trans);

  	}
}
model{

	//Priors
    	omega_pb~cauchy(0,10); 
    	omega_pr~cauchy(0,10);
    	eta_pb~cauchy(0,omega_pb);
    	eta_pr~cauchy(0,omega_pr);

    	theta_S0~cauchy(0,10); 
    	theta_r~cauchy(0,10);
    	theta_SEX~cauchy(0,10);
    	theta_AGE~cauchy(0,10);
    	theta_HV~cauchy(0,10);
    	theta_ASFVAL~cauchy(0,10);
    	theta_APOE4_b~cauchy(0,10);
      theta_APOE4_r~cauchy(0,10);
    	tau~cauchy(0,10); 
    	beta~cauchy(0,10);
    	
	// Likelihood
    	S ~beta(muS*tau_trans, (1-muS)*tau_trans);
} 































