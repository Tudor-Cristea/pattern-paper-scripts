# You are basically re-running script (3), but this time on the time-based parition dataset
# In our case we had 3 datasets with 3 partitions (full course, course halves, and course quarters)
# In this case, the sequences are not clicks, but learning sessions/tactics, thus, the analysis is way faster (we did not need an HPC)
# This is an example of how to run multiple clusters for strategies on the course halves data

#Packages necessary
library(dplyr)
library(seqHMM)
library(TraMineR)
library(tidyr)
library(stringr)
options(scipen = 100, digits = 4) # necessary to see the whole ID of students/course, but also for some indicators
source("folder_paths")


# Load the tactics with the halves
dataset<- read.csv(paste0(time_partitions_path, "tactics_halves_90.csv")) %>%
  select(-X)

# I would run the strategies for 3, 4, 5, 6 clusters 
# Create the sequence object (select only the sequences, same as before)
Seqobject <- seqdef(dataset, 2:64)


# Running 3 clusters for strategies (using the 4 tactics datasets)
strats3 = build_mmm(observations = Seqobject, n_clusters = 3)
# Fitting the model
strats3_fit = fit_model(strats3, control_em = list(restart = list(times = 50, n_optimum=50)),  threads =16)
summary(strats3_fit$model)->Summary3
saveRDS(strats3_fit, paste0(strategies_path, "strategies43_halves.RDS"))
# Plotting the models
Grouped3 <- Summary3$most_probable_cluster
dataset$strategies_3<- Grouped3


# Running 4 clusters for strategies
strats4 = build_mmm(observations = Seqobject, n_clusters = 4)
# Fitting the model
strats4_fit = fit_model(strats4, control_em = list(restart = list(times = 50, n_optimum=50)),  threads =16)
summary(strats4_fit$model)->Summary4
saveRDS(strats4_fit, paste0(strategies_path, "strategies44_halves.RDS"))
# Plotting the models
Grouped4 <- Summary4$most_probable_cluster
dataset$strategies_4<- Grouped4


# Running 5 clusters for strategies
strats5 = build_mmm(observations = Seqobject, n_clusters = 5)
# Fitting the model
strats5_fit = fit_model(strats5, control_em = list(restart = list(times = 50, n_optimum=50)),  threads =16)
summary(strats5_fit$model)->Summary5
saveRDS(strats5_fit, paste0(strategies_path, "strategies45_halves.RDS"))
# Plotting the models
Grouped5 <- Summary5$most_probable_cluster
dataset$strategies_5<- Grouped5

# Running 6 clusters for strategies
strats6 = build_mmm(observations = Seqobject, n_clusters = 6)
# Fitting the model
strats6_fit = fit_model(strats6, control_em = list(restart = list(times = 50, n_optimum=50)),  threads =16)
summary(strats6_fit$model)->Summary6
saveRDS(strats6_fit, paste0(strategies_path, "strategies46_halves.RDS"))
# Plotting the models
Grouped6 <- Summary6$most_probable_cluster
dataset$strategies_6<- Grouped6

# Save the dataset with the strategy clusters
write.csv(dataset, paste0(strategies_path, "final_tactics_4.csv"))


# Next, you can re-run script 4 in order to interpret and decide on a final strategy model
