rm(list = ls())

library(tidyverse)
library(BradleyTerry2)
library(nlme)
library(ltm)
library(gridExtra)
library(lmtest)
library(pROC)
library(lavaan)
library(rstatix)


printf <- function(msg = "%5.3f", ...) {
  cat(sprintf(msg, ...))
}
inv_logit <- function(x) {
  exp(x)/(1+exp(x))
}


set.seed(7119)
#####################################
#Control analysis to reproduce error#
#####################################

# generate data
simulate_subj = 1000

dat_sim <- matrix(nrow = 0, ncol = 5) %>% as.data.frame()
colnames(dat_sim) <- c("participant", "win1", "win2", "item1", "item2")

dat_sim <- dat_sim %>% 
  mutate(
    item1 = as.factor(item1),
    item2 = as.factor(item2)
  )

for (subj in 1:simulate_subj) {
  
  # try out a simple BT model
  
  k = 36
  true_ability = rnorm(k, 0, 5)
  
  # run the contests
  
  remaining_players <- 2:k
  
  for (i in 1:k) {
    for (j in remaining_players) {
      if(i != j) {
        
        # use a deterministic threshold
        # i.e., ability fully predicts choice without error
        p <- inv_logit(true_ability[i] - true_ability[j])
        
        dat_sim <- dat_sim %>% 
          add_row(
            participant = subj,
            item1 = as.factor(paste("i", i, sep = "")),
            item2 = as.factor(paste("i", j, sep = "")),
            win1 = ifelse(runif(1, min = 0, max = 1) < p, 1, 0),
            win2 = 1 - win1
          )
      }
    }
    # don't repeat items that already fought every other item.
    remaining_players = remaining_players[remaining_players != i]
  }
  
  # update on runtime progress
  if (subj %% 10 == 0){print(paste("Simulating Subject no. ", subj, " out of ", simulate_subj, sep = ""))}
}

# save file so we don't need to run the sim every time.
saveRDS(dat_sim, file = "entropy_deterministic_sim_dat.rds")