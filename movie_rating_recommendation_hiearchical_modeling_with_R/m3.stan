
      data{
        real R[90];
        int G[90];
        int M[90];
        real mew_init[4];
      }
      parameters{
        vector[90] mew;
        vector[4]  mew_ij;
        vector<lower=0, upper=1>[4] sigma2;
        real <lower = 0, upper = 1> tau2_bar;
      }
      model{
        tau2_bar ~ inv_gamma(1,1);
        for (i in 1:4){
          sigma2[i] ~ inv_gamma(1,1);
          mew_ij[i] ~ normal(mew_init[i], 1);
        }
        
        
        for (i in 1:90){
          if(G[i] == 1){
            mew[i] ~ normal(mew_ij[1],sqrt(tau2_bar));
          }
          else if(G[i] == 2){
            mew[i] ~ normal(mew_ij[2],sqrt(tau2_bar));
          }
          else if(G[i] == 3){
            mew[i] ~ normal(mew_ij[3],sqrt(tau2_bar));
          }
          else{
            mew[i] ~ normal(mew_ij[4],sqrt(tau2_bar));
          }
        }
        
        
        for (i in 1:90){
          if(G[i] == 1){
            R[i] ~ normal(mew[i],sqrt(sigma2[1]));
          }
          else if(G[i] == 2){
            R[i] ~ normal(mew[i],sqrt(sigma2[2]));
          }
          else if(G[i] == 3){
            R[i] ~ normal(mew[i],sqrt(sigma2[3]));
          }
          else{
            R[i] ~ normal(mew[i],sqrt(sigma2[4]));
          }
        }
      }
      generated quantities{
        vector[90] lik;
        for ( i in 1:90 ) {
          if(G[i] == 1){
            lik[i] = normal_lpdf( R[i] | mew[i] , sqrt(sigma2[1])  );
        }
        else if(G[i] == 2){
            lik[i] = normal_lpdf( R[i] | mew[i] , sqrt(sigma2[2])  );
        }
        else if(G[i] == 3){
            lik[i] = normal_lpdf( R[i] | mew[i] , sqrt(sigma2[3])  );
        }
        else{
            lik[i] = normal_lpdf( R[i] | mew[i] , sqrt(sigma2[4])  );
            }
          }
        }
     
