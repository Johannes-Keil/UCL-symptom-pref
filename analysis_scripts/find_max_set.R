

###########################
### FINDING THE MAX SET####
###########################
# A constant trouble with our analysis is missingness in items (relevant for the BTM)
# This is introduced systematically by variation in the items included in the contest
# There are 5 'core items', but perhaps we can extend that set by looking at which items are most
# shared across participants.

# find the set of items that is shared most between a group of actors

dat_filter <- readRDS(file = "dat_filter.rds")

# create a list of unique sets

set_list <- list()

for (p in unique(dat_filter$participant)) {
  this <- dat_filter %>% 
    filter(participant == p,
           wave == 1,
           !is.na(frequency_response))
  
  these_items <- list(unique(this$item_id))
  set_list <- append(set_list, these_items)
}


# algorithm 1:

# 1. determine size of maximal set
# 2. for each participant, compute how much the maximal set would increase if that participant was deleted
# 3. delete the participant for which the maximal set increases the most.
# 4. if YES -> do not restore participant & continue from 1. STOP if less than 80 participants remain.
# 5. if NO -> restore participant and repeat from 2

while (length(set_list) > 20) {

  # 1.
  intersection <- 1:52
  
  for (set in set_list){
      intersection <- intersect(intersection, set)
  }
  
  max_set_length <- length(intersection)
  
  #2. 
  intersection_length <- c()
  
  for (deletion_candidate in 1:length(set_list)){
    print(deletion_candidate)
    
    intersection_del <- 1:52
    
    for (set in set_list[-deletion_candidate]){
      intersection_del <- intersect(intersection_del, set)
    }
    
    print(intersection_del)
    print(length(intersection_del) - max_set_length)
    
    intersection_length <- c(intersection_length, length(intersection_del) - max_set_length)
  }
  
  intersection_length
  
  #3.
  # arbitrarily choose to delete a member of the set of deletion candidates
  # in theory, one could use this opportunity for recursion.
  # let's save ourselves that trouble for now.
  is_max <- intersection_length == max(intersection_length)
  delete <- which(is_max) %>% sample(1)
  set_list <- set_list[-delete]
}


# compute final intersection
intersection <- 1:52

for (set in set_list){
  intersection <- intersect(intersection, set)
}

intersection


#Evaluation: The set of shared items seems to be very small. We get only the core items if we stop at 10 participants. Unsuitable.

# Algorithm 2:
# add items to the maximal set step-by-step

item_pool <- c()
remaining_players <- 1:52

while(length(item_pool) < 7){
  
  lengths <- c()
  for (i in remaining_players) {
    new_list <- list()
    
    for (set in set_list){
      
      if (i %in% set){
        new_list <- append(new_list, list(set))
      }
    }
    
    lengths <- c(lengths, length(new_list))
  }
  
  print(lengths)
  
  item_pool <- c(item_pool, which(lengths == max(lengths)))
  print(item_pool)
  remaining_players <- remaining_players[!remaining_players %in% item_pool]
  print(remaining_players)
}


# how many participants remain if we use that item pool
remaining_participants = 0
for (set in set_list){
  if (all(item_pool %in% set)){
    remaining_participants = remaining_participants + 1
  }
}

remaining_participants

# Evaluation: If we only want 7 shared items, we already limit ourselves to 7 participants
# there just isn't enough overlap for this to work.
