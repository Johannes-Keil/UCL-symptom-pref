# storage hygiene
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

# load data
dat <- readRDS(file = "dat_pairwise.rds") %>% 
  filter(
    wave == 2,
    rt > 300 & rt < 10000
  )

# for now, select a subset of participants

dat_demo <- readRDS(file = "dat_demo.rds")
dat_filter <- readRDS(file = "dat_filter.rds") %>% 
  filter(
    wave == 2
  ) %>% 
  mutate(
    # from the factor analysis below we know that severity, impact and frequency strongly load on the same underlying factor. So, use the sum-score
    impairment_response = frequency_response + severity_response + impact_response
  )

# which items are 'core'?

core_items <- c("i5", "i14", "i32", "i33", "i34")


# recompute wins & ranks within each participant, but separate by wave

a <- dat %>% 
  group_by(participant, wave, item1) %>% 
  summarise(
    wins1 = sum(win1)
  ) %>% 
  ungroup() %>% 
  dplyr::rename(item = item1)

# some entries are missing, because items only appear in one row
# for our merge operation below to work, we need to fix this
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

dat_w2 <- dat %>% 
  filter(
    wave == 2
  ) %>% 
  mutate(
    item1_rank = NA,
    item2_rank = NA
  )

params <- list(
  participant = c(),
  item = c(),
  alpha = c(),
  rank = c(), 
  p = c(),
  se = c()
)

for (subj in unique(dat$participant)) {
  # pick a participant
  this_dat <- dat_w2 %>% 
    filter(participant == subj)
  
  wins <- wins_participant_wave %>% 
    filter(wave == 2,
           participant == subj)
  wins$rank <- rank(wins$wins)
  wins <- wins %>% arrange(item)
  
  # save parameters in a list for checking
  params$rank <- c(params$rank, wins$rank)
  params$item <- c(params$item, wins$item)
  params$participant <- c(params$participant, rep(subj, nrow(wins)))
  params$wins <- c(params$wins, wins$wins)
  
  # record params in the actual dataset
  for (i in wins$item){
    dat_w2 <- dat_w2 %>% 
      mutate(
        item1_rank = ifelse(item1 == i & participant == subj, wins[wins$item == i,]$rank, item1_rank),
        item2_rank = ifelse(item2 == i & participant == subj, wins[wins$item == i,]$rank, item2_rank)
      )
  }
}

# Now, standardise values for ease of interpretation
dat_w2 <- dat_w2 %>% 
  mutate(
    rank_diff_raw = item1_rank - item2_rank,
    log_rt_raw = log_rt,
    log_rt = scale(log_rt_raw),
    rank_diff = scale(rank_diff_raw)
  )

this_dat <- matrix(ncol = 8, nrow = 0) %>% as.data.frame()
colnames(this_dat) <- c("severity_response_1", "severity_response_2", "frequency_response_1", "frequency_response_2", "impact_response_1", "impact_response_2", "impairment_response_1", "impairment_response_2") 

dat_filter2 <- dat_filter %>% 
  filter(wave == 2)

for (i in 1:nrow(dat_w2)) {
  
  p <- dat_w2$participant[i]
  item1 <- sub("i", "", dat$item1[i])
  item2 <- sub("i", "", dat$item2[i])
  
  s1 <- dat_filter2 %>% 
    filter(participant == p, item_id == item1) %>% 
    dplyr::select(severity_response)
  s2 <- dat_filter2 %>% 
    filter(participant == p, item_id == item2) %>% 
    dplyr::select(severity_response)
  
  f1 <- dat_filter2 %>% 
    filter(participant == p, item_id == item1) %>% 
    dplyr::select(frequency_response)
  f2 <- dat_filter2 %>% 
    filter(participant == p, item_id == item2) %>% 
    dplyr::select(frequency_response)
  
  i1 <- dat_filter2 %>% 
    filter(participant == p, item_id == item1) %>% 
    dplyr::select(impact_response)
  i2 <- dat_filter2 %>% 
    filter(participant == p, item_id == item2) %>% 
    dplyr::select(impact_response)
  
  imp1 <- dat_filter2 %>% 
    filter(participant == p, item_id == item1) %>% 
    dplyr::select(impairment_response)
  imp2 <- dat_filter2 %>% 
    filter(participant == p, item_id == item2) %>% 
    dplyr::select(impairment_response)
  
  this_dat <- this_dat %>%
    add_row(
      severity_response_1 = s1[1, 1],
      severity_response_2 = s2[1, 1],
      frequency_response_1 = f1[1, 1],
      frequency_response_2 = f2[1, 1],
      impact_response_1 = i1[1, 1],
      impact_response_2 = i2[1, 1],
      impairment_response_1 = imp1[1, 1],
      impairment_response_2 = imp2[1, 1]
    )
}

this_dat <- this_dat %>% 
  mutate(
    severity_diff = scale(severity_response_1 - severity_response_2),
    frequency_diff = scale(frequency_response_1 - frequency_response_2),
    impact_diff = scale(impact_response_1 - impact_response_2),
    impairment_diff = scale(impairment_response_1 - impairment_response_2)
  )

dat_w2 <- cbind(dat_w2, this_dat)

plot(dat_w2$rank_diff, dat_w2$impairment_diff)

model_filter <- lm(data = dat_w2, rank_diff ~ impairment_diff)

saveRDS(dat_w2, "combined_filter_w2.rds")

