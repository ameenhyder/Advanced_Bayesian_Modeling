##### Task 1 part 1: What is the probability that you become infected with the disease on any one particular day? #######

infect <- 0.0001*30 ## expected number of people infected in 30 people met (E(Bernoli_variable) = np)
not_infect <- 1 - p_infect

p_infected_given_meet_infected_person <- 0.5 ## given in the question
p_infected_given_meet_not_infected_person <- 0

prob_infection_on_a_day <- (infect * p_infected_given_meet_infected_person) + (not_infect * p_infected_given_meet_not_infected_person)
print(prob_infection_on_a_day)

#### Task 1 part 2: Assuming all your days follow this pattern, and that whether you become infected on any one day is ####
#### independent of what happens on other days, what is the probability that you will contract conditionitis ####
#### at least once over a period of 365 days (i.e. a year)? ####

prob_contract_disease_atleast_once = 1 - (1* ((prob_infection_on_a_day)^0) * ((1-prob_infection_on_a_day)^365) ) ## using complement rule: 1 - prob(not getting infected on any day throughout they year)
print(prob_contract_disease_atleast_once)

### Task 2 part 1: What is your average travel time in the morning? ###

A_t_X <- rexp(1000, 4)
A_t_Y <- rexp(1000, 2.5)
X_t_Y <- rexp(1000, 10)
Y_t_X <- rexp(1000, 10) 
X_t_B <- rexp(1000, 3)
Y_t_B <- rexp(1000, 5)

AXB <- A_t_X + X_t_B
AYB <- A_t_Y + Y_t_B
AXYB <- A_t_X + X_t_Y + Y_t_B
AYXB <- A_t_Y + Y_t_X + X_t_B 

travel_time <- pmin(AXB, AYB, AXYB, AYXB)
avg_travel_time <- mean(travel_time)
print(avg_travel_time*60)

## Task 2 part 2: With which probability is each of the four possible routes from A to B taken?

table(apply(cbind(AXB, AYB, AXYB, AYXB), 1, which.min)) / 1000 

## What is the probability that you have to travel more than half an hour?

mean(travel_time >= 0.5)

