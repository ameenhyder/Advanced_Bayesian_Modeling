library(rstan)
library(ExtDist)
################ Task 1 part 1 ###################

write("
      generated quantities{
      real times_infected = 0.0;
      real inf = 0.0;
      real num_persons;
      real person_I_meet_infects_me;
      int person_I_meet_is_infected;
      int i = 1;
      
      num_persons  = uniform_rng(25,30);
      
      while (i <= num_persons){
        person_I_meet_is_infected = bernoulli_rng(0.0001);
        person_I_meet_infects_me = uniform_rng(0.4,0.6);
        times_infected += person_I_meet_is_infected * person_I_meet_infects_me;
        i = i + 1;
      }
      
      inf = times_infected > 0;
      
      }",
      "CLProbTask1.stan")


stan_fit <- stan_model(file='CLProbTask1.stan')
simulation_1 <- sampling(stan_fit, algorithm = "Fixed_param", iter = 2000000, warmup = 0)

print(simulation_1, pars = "inf", probs = c(0.025, 0.5, 0.975), digits = 5)

#### the probability of getting infected at any one particular day is 0.271% ##############


################ Task 1 part 2 ###################


param_optimizer <- function(params) {
  alpha <- params[1]
  beta <- params[2]
  
  intended.quantiles <- c(0, 1)
  calculated.quantiles <- qbeta(p=c(0.025, 0.975), shape1=alpha, shape2=beta)
  squared.error.quantiles <- sum((intended.quantiles - calculated.quantiles)^2)
  
  intended.mode <- 0.5
  calculated.mode <- calculate.mode(alpha, beta)
  squared.error.mode <- (intended.mode - calculated.mode)^2
  
  return(squared.error.quantiles + squared.error.mode)
}

calculate.mode <- function(alpha, beta) {
  return((alpha-1) / (alpha+beta-2))
}


starting.params <- c(6,6)

nlm.result <- nlm(f = param_optimizer, p = starting.params)

optimal.alpha <- nlm.result$estimate[1]
optimal.beta <- nlm.result$estimate[2]

optimal.alpha
optimal.beta

############ optimal alpha: 5.03 #################### 
###########  optimal beta:  5.03 ####################

hist(rBeta_ab(10000, optimal.alpha,optimal.beta, a = 0.4, b = 0.6,ncp=0))



################ Task 2 part 1, part 2,  part3, part 4 #################
y1_dummy <- rbinom(100,1,0.54)
y2_dummy <- rbinom(100,1,0.63)
mean(y1_dummy)
mean(y2_dummy)
##diff = numeric(100)

n = 100

write("

      data{
      int n;
      int y1_dummy[n];
      int y2_dummy[n];
      }
      
      parameters{
      real <lower = 0, upper = 1> p1;
      real <lower = 0, upper = 1> p2;
      }
      
      model{
       p1 ~ normal(0.5,1);
       p2 ~ normal(0.7,1);
      
      for (i in 1:n){
        y1_dummy[i] ~ bernoulli(p1);
        y2_dummy[i] ~ bernoulli(p2);
      }
      
      }
      
      generated quantities{
      real diff;
      diff = fabs(p1 - p2);
      
      }
      
      ","CLProbTask2.stan")


dat <- list(
  n = n,
  y1_dummy = y1_dummy,
  y2_dummy = y2_dummy
)

dat

fit2_stan <- rstan::stan("CLProbTask2.stan", data=dat, iter=2000, warmup = 500)
class(fit2_stan)
summary(fit2_stan)$summary
get_posterior_mean(fit2_stan)

################ Bayesian estimate for p1: 0.607. Std Dev for p1: 0.047 #############################
################ Bayesian estimate for p2: 0.0588. Std Dev for p1: 0.046 #############################
################ Bayesian estimate for p1-p2: 0.054. Std Dev for p1-p2: 0.0409 #############################

mean_p1 <- extract(fit2_stan, pars = 'p1', permuted = TRUE, inc_warmup = FALSE)
mean_p2 <- extract(fit2_stan, pars = 'p2', permuted = TRUE, inc_warmup = FALSE)
diff <- extract(fit2_stan, pars = 'diff', permuted = TRUE, inc_warmup = FALSE)




diff <- as.vector(diff$diff) 

mean(diff > 0.05)

############ posterior probability that absolute value of p1-p2 > 0.05 is 0.473 ################

