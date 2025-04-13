library(tidyverse)
#author: Johannes Keil

#######################################
############ DEMOGRAPHIC DATA   #######
#######################################

dat_demo_w1 <- read.csv(file = paste(getwd(), "/many_symptoms_cleaned_data/wave1_data/demo_res_wave1.csv", sep = "")) %>% 
  dplyr::rename(participant = id)

dat_demo_w2 <- read.csv(file = paste(getwd(), "/many_symptoms_cleaned_data/wave2_data/demo_res_wave2.csv", sep = "")) %>% 
  dplyr::rename(participant = id)


items <- read.csv(file = paste(getwd(), "/many_symptoms_cleaned_data/revised_many_symptoms.csv", sep = ""))
items$item_str <- paste("i", 1:52, sep = "")


# ok, participant-ids between the two groups are not identical, although the 
# data should be repeated measures. This is BAD.
# First, try to confirm that actually the same participants were recruited in both cases
# Every participant in wave 2 should also exist in wave 1 (but not the other way round, due to attrition)

# our only demographic clues are age and gender.
dat_demo_w1 <- dat_demo_w1 %>% 
  mutate(dem = paste(sex, age, sep = ""))

# our only demographic clues are age and gender.
dat_demo_w2 <- dat_demo_w2 %>% 
  mutate(dem = paste(sex, age, sep = ""))

# see if all participants are overlapping
# just one is not
dat_demo_w2[!dat_demo_w2$dem %in% dat_demo_w1$dem,]

# this may be because of a birthday.
# the non-matching participant is 24, maybe they were 23 during the first wave?
# the CESD score, age and completion time are highly similar
dat_demo_w1 %>% filter(dem == "f23")

# overall, it seems like participants are the same, but we have no reason to believe that their IDs have the right order
# check

dat_demo_w1$dem == dat_demo_w2$dem

# see what can be salvaged by arranging items according to demographics.
dat_demo_w1 <- arrange(dat_demo_w1,dem)
dat_demo_w2 <- arrange(dat_demo_w2,dem)

# again, there is some overlap in completion time and CESD score between participants at the same order/position
# but this is nothing we can safely rely on to match participants

# turns out that dylan had a file linking both IDs together. Load that file.

dat_id <- read.csv(file = paste(getwd(), "/many_symptoms_cleaned_data/idlut.csv", sep = ""))

# get a list of participants that occur only in one wave
attrition <- dat_id$id_w1[is.na(dat_id$id_w2)]

# check if attrition is now fully accounted for (it is)
nrow(dat_demo_w1) - length(attrition) == nrow(dat_demo_w2)

dat_id2 <- dat_id %>% 
  rename(participant = id_w1)

dat_demo_w1 <- merge(dat_demo_w1, dat_id2)
dat_demo_w2 <- dat_demo_w2 %>% 
  rename(id_w2 = participant)

# merge data from both time-points by the ID used in the second wave
dat_demo <- merge(dat_demo_w1, dat_demo_w2, by = c("id_w2", "sex"), suffixes = c("_w1", "_w2")) %>% 
  dplyr::select(-id_w2)

# add in the participants that only featured in the first wave.
dat_demo_unique <- dat_demo_w1 %>% 
  filter(participant %in% attrition) %>% 
  rename(
    CESD_w1 = CESD,
    duration_w1 = duration,
    age_w1 = age,
    above_threshold_w1 = above_threshold,
    dem_w1 = dem
  ) %>% 
  mutate(
    CESD_w2 = NA,
    duration_w2 =  NA,
    age_w2 = NA,
    above_threshold_w2 = NA,
    dem_w2 = NA
  ) %>% 
  dplyr::select(-id_w2)

# participants are uniformly referred to by their ID during the first wave
dat_demo <- rbind(dat_demo, dat_demo_unique)

# check data quality
# in age, there are three cases that don't match up.
# In two cases, the difference is minor (1 year), in once case it's a typo (age = 32 at w1 42 at w2)
plot(dat_demo$age_w1, dat_demo$age_w2)
dat_demo[dat_demo$age_w1 !=  dat_demo$age_w2,]

# CESD between both timepoints should be highly correlated
# r = 0.8. Seems reasonable.
plot(dat_demo$CESD_w1, dat_demo$CESD_w2)
cor.test(dat_demo$CESD_w1, dat_demo$CESD_w2)

# See how answer speed correlated between both timepoints
# r = 0.3. Again, reasonable
plot(dat_demo$duration_w1, dat_demo$duration_w2)
cor.test(dat_demo$duration_w1, dat_demo$duration_w2)

# conclude that the new id-table does what it should.

# outgoing data
# saveRDS(dat_demo, "dat_demo.rds")

##################
# MAIN TASK DATA #
##################

dat_w1 <- read.csv(file = paste(getwd(), "/many_symptoms_cleaned_data/wave1_data/pairwise_res_wave1_nonulls.csv", sep = "")) %>% 
  as.data.frame() %>% 
  mutate(
    participant = as.double(id),
    item1 = factor(item_0, levels = 1:52, labels = paste("i", 1:52, sep = "")),
    item2 = factor(item_1, levels = 1:52, labels = paste("i", 1:52, sep = "")),
    win2 = choice,
    win1 = ifelse(choice == 0, 1, 0),
    wave = 1
  ) %>% 
  dplyr::select(-id, -item_0, -item_1) %>% 
  filter(
    !is.na(win1) & !is.na(win2)
  )

dat_w2 <- read.csv(file = paste(getwd(), "/many_symptoms_cleaned_data/wave2_data/pairwise_res_wave2_nonulls.csv", sep = "")) %>% 
  as.data.frame() %>% 
  mutate(
    participant = as.double(id),
    item1 = factor(item_0, levels = 1:52, labels = paste("i", 1:52, sep = "")),
    item2 = factor(item_1, levels = 1:52, labels = paste("i", 1:52, sep = "")),
    win2 = choice,
    win1 = ifelse(choice == 0, 1, 0),
    wave = 2
  ) %>% 
  dplyr::select(-id, -item_0, -item_1) %>% 
  filter(
    !is.na(win1) & !is.na(win2)
  )

dat_id2 <- dat_id %>%
  filter(!is.na(id_w2))

# recode participant names to be identical across the two lists.
# This is a potential error source to keep in mind if data problems appear later!
for (i in 1:nrow(dat_id2)){
  dat_w2$participant[dat_w2$participant == dat_id2[i, 2]] <- dat_id2[i, 1] 
}

# outgoing data
dat <- rbind(dat_w1, dat_w2)

# saveRDS(dat, "dat_pairwise.rds")

#########################
## FILTER ITEMS #########
#########################

dat_filter_w1 <- read.csv(file = paste(getwd(), "/many_symptoms_cleaned_data/wave1_data/filter_res_wave1.csv", sep = "")) %>% 
  mutate(
    wave = 1
  ) %>% 
  rename(
    participant = id,
    item = revised_item
  )

dat_filter_w2 <- read.csv(file = paste(getwd(), "/many_symptoms_cleaned_data/wave2_data/filter_res_wave2.csv", sep = "")) %>% 
  mutate(
    wave = 2
  ) %>% 
  rename(
    participant = id
  ) %>% 
  dplyr::select(-null_resp)

# recode participant names to be identical across the two lists.
# This is a potential error source to keep in mind if data problems appear later!
for (i in 1:nrow(dat_id2)){
  dat_filter_w2$participant_new[dat_filter_w2$participant == dat_id2[i, 2]] <- dat_id2[i, 1] 
}

dat_filter <- rbind(dat_filter_w1, dat_filter_w2)

# saveRDS(dat_filter, "dat_filter.rds")

 