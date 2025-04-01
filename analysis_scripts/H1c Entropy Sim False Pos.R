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

n_participants <- 1000
n_items = 36

dat_sim <- matrix(ncol = 3, nrow = 0) %>% as.data.frame()
colnames(dat_sim) <- c("participant", "item", "wins")

total_wins = 190

for (subj in 1:n_participants) {
  
  these_wins <- data.frame( # list storing wins by item index
    item = paste("i", 1:n_items, sep = ""),
    wins = rep(0, n_items)
  ) 
  
  for (i in 1:total_wins) {
    index <- runif(1, min = 0, max = n_items) %>% round()
    these_wins$wins[index] <- these_wins$wins[index] + 1
  }
  
  dat_sim <- rbind(dat_sim, data.frame(participant = rep(subj, n_items), item = paste("i", 1:n_items, sep = ""), wins = these_wins$wins))
}



res_entropy <- matrix(ncol = 7, nrow = 0) %>% as.data.frame()
colnames(res_entropy) <- c("participant", "n_items", "h", "h_threshold", "h_max", "sig", "score")

n_permutations <- 1000

counter <- 0

for (subj in unique(dat_sim$participant)) {
  
  counter <- counter + 1
  print(paste("Computing participant no. ", counter, sep = ""))
  
  this <- dat_sim %>% 
    filter(participant == subj)
  
  total_wins = this$wins %>% sum()
  
  this <- this %>% 
    mutate(
      p = wins / total_wins,
      p = ifelse(p == 0, 0.000000001, p)
    ) %>% 
    mutate(
      h = p * log(p, base = 2)
    )
  entropy <- -sum(this$h)
  
  uniform_entropy <- c()
  
  # create a permuted distribution
  for (perm in 1:n_permutations) {
    
    these_wins <- data.frame( # list storing wins by item index
      item = unique(this$item),
      wins = rep(0, length(unique(this$item)))
    ) 
    
    for (i in 1:total_wins) {
      index <- runif(1, min = 0, max = length(unique(this$item))) %>% round()
      these_wins$wins[index] <- these_wins$wins[index] + 1
    }
    
    these_wins <- these_wins %>% 
      mutate(
        p = wins / total_wins,
      ) %>% 
      filter(p > 0) %>% 
      mutate(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAbElEQVR4Xs2RQQrAMAgEfZgf7W9LAguybljJpR3wEse5JOL3ZObDb4x1loDhHbBOFU6i2Ddnw2KNiXcdAXygJlwE8OFVBHDgKrLgSInN4WMe9iXiqIVsTMjH7z/GhNTEibOxQswcYIWYOR/zAjBJfiXh3jZ6AAAAAElFTkSuQmCC
        h = p * log(p, base = 2)
      )
    uniform_entropy <- c(uniform_entropy, -sum(these_wins$h))
  }
  
  # get quantiles for a significance threshold
  threshold <- quantile(uniform_entropy, probs = c(0.05))
  
  #compute significance
  sig <- entropy < threshold
  
  # compute maximal entropy (uniform distribution)
  p <- 1 / length(unique(this$item))
  h_max <- -length(unique(this$item)) * p * log(p, base = 2)
  
  # save results  
  res_entropy <- res_entropy %>% 
    add_row(
      participant = subj,
      n_items = length(unique(this$item)),
      h = entropy,
      h_threshold = threshold,
      sig = sig,
      h_max = h_max,
      score = 100 * (1 - entropy / h_max)
    )
}

# save results for further reference
saveRDS(res_entropy, file = "entropy_uniform.rds")