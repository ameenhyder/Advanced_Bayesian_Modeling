

      data{
      int n;
      int y1[n];
      int y2[n];
      }
      
      parameters{
      real <lower = 0, upper = 1> p1;
      real <lower = 0, upper = 1> p2;
      }
      
      model{
       p1 ~ normal(0.5,1);
       p2 ~ normal(0.7,1);
      
      for (i in 1:n){
        y1[i] ~ bernoulli_logit(p1);
        y2[i] ~ bernoulli_logit(p2);
      }
      
      }
      
      generated quantities{
      real diff;
      diff = fabs(p1 - p2);
      
      }
      
      
