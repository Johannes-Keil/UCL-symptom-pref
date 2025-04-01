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

out_entropy <- matrix(nrow = 0, ncol = 6) %>% as.data.frame()
colnames(out_entropy) <- c("participant", "wave", "entropy", "max", "threshold", "sig")

# prep data

# recompute wins & ranks within each participant, but separate by wave

a <- dat %>% 
  group_by(participant, wave, item1) %>% 
  summarise(
    wins1 = sum(win1)
  ) %>% 
  ungroup() %>% 
  dplyr::rename(item = item1)

# some entries are missing, because items only appear in the item1 or item2 column
# for our merge operation below to include these, we need to
# manually add those entries

for (w in 1:2) {
  for (subj in unique(dat$participant)){
    
    item1_list <- filter(dat, participant == subj, wave == w)$item1 %>% unique()
    item2_list <- filter(dat, participant == subj, wave == w)$item2 %>% unique()
    
    for (i in item2_list[!item2_list %in% item1_list]) {
      a <- a %>% add_row(
        participant = subj,
        wave = w,
        item = i,
        wins1 = 0
      )
    }
  }
}

b <- dat %>% 
  group_by(participant, wave, item2) %>% 
  summarise(
    wins2 = sum(win2)
  ) %>% 
  ungroup() %>% 
  dplyr::rename(item = item2)


for (w in 1:2) {
  for (subj in unique(dat$participant)){
    
    item1_list <- filter(dat, participant == subj, wave == w)$item1 %>% unique()
    item2_list <- filter(dat, participant == subj, wave == w)$item2 %>% unique()
    
    for (i in item1_list[!item1_list %in% item2_list]) {
      b <- b %>% 
        add_row(
          participant = subj,
          wave = w,
          item = i,
          wins2 = 0
        )
    }
  }
}

# compute ranks within each participant
wins_participant_wave <- merge(a, b) %>% 
  mutate(wins = wins1 + wins2) %>% 
  arrange(participant) %>% 
  group_by(participant)

for (w in 1:2){
  counter = 0
  for (subj in unique(wins_participant_wave$participant)){
    
    counter <- counter + 1
    print(paste("Computing participant no. ", counter, " wave ", w, sep = ""))
    
    
    this <- wins_participant_wave %>% 
      filter(participant == subj, wave == w)
    
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
    
    n_permutations <- 1000
    
    uniform_entropy <- c()
    
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
        mutate(
          h = p * log(p, base = 2)
        )
      uniform_entropy <- c(uniform_entropy, -sum(these_wins$h))
    }
    
    # get quantiles for a significance threshold
    threshold <- quantile(uniform_entropy, probs = c(0.05))
    
    # in our case, this is significant
    sig <- entropy < threshold
    
    # output
    
    p <- 1 / length(unique(this$item))
    
    max_entropy = -length(unique(this$item)) * p * log(p, base = 2)
    
    out_entropy <- add_row(out_entropy,
                           participant = subj,
                           entropy = entropy,
                           threshold = threshold,
                           max = max_entropy,
                           sig = sig,
                           wave = w
    )
    
  }
}

# compute our entropy scores.

out_entropy <- out_entropy %>% 
  mutate(
    entropy_score = (1 - (entropy / max)) * 100
  ) %>% 
  filter(
    !is.nan(max)
  )


# save results for further reference
saveRDS(out_entropy, file = "entropy_observed.rds")