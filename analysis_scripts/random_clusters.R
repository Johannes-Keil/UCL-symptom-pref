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

n_participants <- 2
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

# this contains our 'ground truth'
ground_truth <- dat_sim %>% 
  pivot_wider(names_from = "item", values_from = wins) %>% 
  dplyr::select(-participant)

# now experimentally generate our clusters

n_sim = 64

sim <- matrix(ncol = n_items, nrow = 0) %>% as.data.frame()
colnames(sim) <- paste("i", 1:n_items, sep = "")

# items in first cluster
for (i in 1:n_sim){
  noise = rnorm(n = n_items, mean = 0, sd = 3)
  sim <- sim %>% 
    add_row(ground_truth[1, ] + noise) %>% 
    round()
}

#items in second cluster
for (i in 1:n_sim){
  noise = rnorm(n = n_items, mean = 0, sd = 3)
  sim <- sim %>% 
    add_row(ground_truth[2, ] + noise) %>% 
    round()
} 


##############################################
##### HOW SHOULD IT LOOK FOR REAL CLUSTERS?###
##############################################

var_ratio = c()
within = c()

for (i in 1:20){
  clust <- kmeans(sim, i)
  
  within <- c(within, clust$tot.withinss)  
  ss <- clust$betweenss / (clust$tot.withinss + clust$betweenss)
  var_ratio <- c(var_ratio, ss)
}

plot(1:20, within, main = "Elbow Plot Two Clusters", xlab = "n Clusters", ylab = "Total Within SS")
plot(1:20, var_ratio, main = "Between/Total SS", xlab = "n Clusters", ylab = "Ratio Total Between SS / Total SS")

# now also get ward's dendrogram
# at a first look, four or 5 clusters seem reasonable

ward_raw <- hclust(dist(sim), method="ward.D")
plot(ward_raw)

rect.hclust(ward_raw, 2)

# extract two clusters.from the raw data
clust <- sim %>% 
  dist() %>% 
  kmeans(2)

# illustrate our clustering graphically
iso <- sim %>% 
  dist() %>% 
  isoMDS(k = 2)

points <- iso$points %>% as.data.frame()

grouping <- as.factor(paste("c", clust$cluster, sep = "")) %>% as.data.frame()

dat_plot <- cbind(points, grouping)
colnames(dat_plot) <- c("dim1", "dim2", "cluster")

this_plot1 <- ggplot(dat_plot, aes(x = dim1, y = dim2, color = cluster)) + 
  geom_point() +
  ggtitle("Two True Clusters")



################################################################
##### HOW DOES IT LOOK IF THERE AREN'T ACTUALLY ANY CLUSTERS?###
################################################################

# look at only first half of simulated data, so there is only one cluster
sim <- sim[1:n_sim,]

var_ratio = c()
within = c()

for (i in 1:20){
  clust <- kmeans(sim, i)
  
  within <- c(within, clust$tot.withinss)  
  ss <- clust$betweenss / (clust$tot.withinss + clust$betweenss)
  var_ratio <- c(var_ratio, ss)
}

plot(1:20, within, main = "Elbow Plot One Cluster", xlab = "n Clusters", ylab = "Total Within SS")
plot(1:20, var_ratio, main = "Between/Total SS One Cluster", xlab = "n Clusters", ylab = "Ratio Total Between SS / Total SS")

# now also get ward's dendrogram
# at a first look, four or 5 clusters seem reasonable

ward_raw <- hclust(dist(sim), method="ward.D")
plot(ward_raw)

rect.hclust(ward_raw, 2)

# extract two clusters.from the raw data
clust <- sim %>% 
  dist() %>% 
  kmeans(2)

# illustrate our clustering graphically
iso <- sim %>% 
  dist() %>% 
  isoMDS(k = 2)

points <- iso$points %>% as.data.frame()

grouping <- as.factor(paste("c", clust$cluster, sep = "")) %>% as.data.frame()

dat_plot <- cbind(points, grouping)
colnames(dat_plot) <- c("dim1", "dim2", "cluster")

this_plot2 <- ggplot(dat_plot, aes(x = dim1, y = dim2, color = cluster)) + 
  geom_point() +
  ggtitle("One True Cluster")


