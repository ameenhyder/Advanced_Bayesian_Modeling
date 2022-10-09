rm(list=ls())
#
library(dplyr) 
library(tidyr)
library(ggplot2)
library('ggthemes')
theme_set(theme_tufte(base_family = 'sans'))
library('rstan')
rstan_options(auto_write = TRUE)
options(mc.cores = 4)
library('bayesplot')
library('rethinking')


## loading data from the csv file

d <- read.csv(file='C:\\Users\\ameen.hyder\\Desktop\\gigs\\hierarchical_models_7_july\\movie.csv')
d$movie <- 1:nrow(d)

head(d)

## making initial plot to visualize ratings of each movie

plot( d$ratings , ylim=c(1,100) , pch=16 , xaxt="n" ,
      xlab="movie" , ylab="ratings" , col=rangi2 )
axis( 1 , at=c(1,30,60,90) , labels=c(1,30,60,90) )


## making data list to be passed to the stan model

dat <- list(
  R = d$ratings,
  G = d$genre,
  M = d$movie,
  mew_init = c(52.125,54,59.83,45.34))

dat

################################# separate model ############################
## In the separate model, the information is not pooled accorss different genres
## hence, each movie will have its own posterior distribution


## prior specificaitons:
## There will be separate standard deciation for each genre as sigma2 that will be gamma distributed with weak priors (1,1). 
## The mean rating for movie (mew) will be normally distributed with mean 50 (universal mean accorss all movies) and tau2
## Each rating will be normally distributed with mean mew and sigma2 (for respective genre)

write("
      data{
        real R[90];     
        int G[90];
        int M[90];
      }
      parameters{
        vector[90] mew;
        vector<lower=0, upper=1>[4] sigma2;
        real <lower = 0, upper = 1> tau2_bar;
      }
      model{
        for (i in 1:4){
          sigma2[i] ~ inv_gamma(1,1);
        }
        tau2_bar ~ inv_gamma(1,1);
        mew ~ normal(50, sqrt(tau2_bar));
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
     ", 
      "m1.stan")

fit_m1 <- rstan::stan("m1.stan", data=dat, iter=5000)

# Print the output of the fit
print(fit_m1, probs = c(0.10, 0.5, 0.9))


posterior <- as.data.frame(fit_m1, pars=c("mew"))
p1 <- stack(posterior)
head(p1)

head(posterior)
str(posterior)

str(p1)

##plot of data
plot( d$ratings , ylim=c(1,100) , pch=16 , xaxt="n" ,
      xlab="movie" , ylab="ratings" , col=rangi2 )
axis( 1 , at=c(1,30,60,90) , labels=c(1,30,60,90) )

# We now overlay the posterior mean estimates for the ratings of each movie

d$mew_1 <- apply(posterior[,1:90], 2, mean)
points(d$mew_1)


############################### hierachical Model ##############################

## each rating will be normally distributed around its genre mean (mew_ij) and genre standard deviation (sigma2)
## mew_ij will be normally distributed around the mean of rating of each genre and 1 standard deviation (weak prior)
## the rating of each movie (i) will be normally distributed around mew_ij and tau2
## the standard deviation for each genre and tau2 will be gamma distributed with weak priors (1,1)

write("
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
     ", 
      "m3.stan")


fit_m3 <- rstan::stan("m3.stan", data=dat, iter=5000)



posterior3 <- as.data.frame(fit_m3, pars=c("mew"))
p3 <- stack(posterior3)
head(p3)

head(posterior3)
str(posterior3)

str(p3)

##plot of data
plot( d$ratings , ylim=c(1,100) , pch=16 , xaxt="n" ,
      xlab="movie" , ylab="ratings" , col=rangi2 )
axis( 1 , at=c(1,30,60,90) , labels=c(1,30,60,90) )

# We now overlay the posterior mean estimates for the ratings of each movie

d$mew_3 <- apply(posterior3[,1:90], 2, mean)
points(d$mew_3)


################################# pool model #################################

## Here all the ratings will be pooled across each genre, hence the mean rating of each movie
## will be normally distributed with mew_bar

write("
      data{
        real R[90];
        int G[90];
        int M[90];
      }
      parameters{
        vector[90] mew;
        real mew_bar;
        real <lower = 0, upper = 1> sigma2;
        real <lower = 0, upper = 1> tau2_bar;
      }
      model{
        sigma2 ~ inv_gamma(1,1);
        tau2_bar ~ inv_gamma(1,1);
        mew_bar ~ normal(50,1);
        mew ~ normal(mew_bar, sqrt(tau2_bar));
        R ~ normal(mew,sqrt(sigma2));
      }
      generated quantities{
        vector[90] log_lik;
        for ( i in 1:90 ) {
          log_lik[i] = normal_lpdf( R[i] | mew[i] , sqrt(sigma2)  );
        }
      }
      ", "m2.stan")



fit_m2 <- rstan::stan("m2.stan", data=dat, iter=5000)

posterior2 <- as.data.frame(fit_m2, pars=c("mew", "mew_bar", "sigma2", "tau2_bar"))
p2 <- stack(posterior2[,1:90])
str(p2)
str(posterior2)

# Plot the output

plot( d$ratings , ylim=c(1,100) , pch=16 , xaxt="n" ,
      xlab="movie" , ylab="ratings" , col=rangi2 )
axis( 1 , at=c(1,30,60,90) , labels=c(1,30,60,90) )

d$mew_2 <- apply(posterior2[,1:90], 2, mean)
points(d$mew_2)



abline( h=mean(posterior2$mew_bar) , lty=2 )

## Add separators for each genre to better visualize the estimates


abline( v=16 , lwd=0.5 )
abline( v=40 , lwd=0.5 )
abline( v=58 , lwd=0.5 )

text( 8 , 0 , "comedy" )
text( 16 + 10 , 0 , "thriller" )
text( 50 , 0 , "horror" )
text( 75 , 0 , "romance" )

# We now examine the inferred population distribution of ratings. 
# We can visualize it by sampling from the posterior distribution. 
# Here we plot 100 Gaussian distributions, one for each of the first 100 
# samples from the posterior  distribution of both $\bar{\mew}$ and $\sigma$. 


plot( NULL , xlim=c(-3,4) , ylim=c(0,0.35) ,
      xlab="odds training" , ylab="Density" )
for ( i in 1:100 )
  curve( dnorm(x,posterior2$mew_bar[i],posterior2$sigma2[i]) , add=TRUE ,
         col=col.alpha("black",0.2) )


# It is useful to transform back to the probability scale. 
# So now we sample 8000 imaginary movies from the posterior distribution.

sim_rating <- rnorm( 8000 , posterior2$mew_bar , posterior2$sigma2 )

#These are transformed back to the probability scale and their density 
#is plotted. 

dens( inv_logit(sim_rating) , lwd=2 , adj=0.1 )


# Here we plot the posterior distribution of the ratings  
# for each movie for the seperate, hierarchical model and pool model 
# For reference, we highlight movie $90$. 



labs1 <- paste('posterior of', c('mew_j', 'mew_90'))
plot_sep <- ggplot(data = p2) +
  geom_density(aes(values, color = (ind=='mew[90]'), group = ind)) +
  labs(x = 'mew', y = '', title = 'pool model', color = '') +
  scale_y_continuous(breaks = NULL) +
  scale_color_manual(values = c('blue','red'), labels = labs1) +
  theme(legend.background = element_blank(), legend.position = c(0.2,0.9))


labs1 <- paste('posterior of', c('mew_j', 'mew_90'))
plot_sep2 <- ggplot(data = p1) +
  geom_density(aes(values, color = (ind=='mew[90]'), group = ind)) +
  labs(x = 'mew', y = '', title = 'Separate model', color = '') +
  scale_y_continuous(breaks = NULL) +
  scale_color_manual(values = c('blue','red'), labels = labs1) +
  theme(legend.background = element_blank(), legend.position = c(0.2,0.9))

labs1 <- paste('posterior of', c('mew_j', 'mew_90'))
plot_sep3 <- ggplot(data = p3) +
  geom_density(aes(values, color = (ind=='mew[90]'), group = ind)) +
  labs(x = 'mew', y = '', title = 'Hierarchical model', color = '') +
  scale_y_continuous(breaks = NULL) +
  scale_color_manual(values = c('blue','red'), labels = labs1) +
  theme(legend.background = element_blank(), legend.position = c(0.2,0.9))




library(gridExtra)
grid.arrange(plot_sep, plot_sep2,plot_sep3, nrow = 1)








