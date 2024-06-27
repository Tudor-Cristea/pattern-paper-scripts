# After running the simple MM models, we need to run and compare multiple MMM models
# This is an example on how to run the MMM with 4 clusters

#Packages
library(tidyverse)
library(TraMineR)
library(seqHMM)
options(scipen = 999)
source("folder_paths")

# We load the final dataset from script (5). This is used to create a sequence object 
dataset <- fread(paste0(data_path, "all_data_final_weeklabel.csv")) %>%
  select(-V1)

# Save multiple scripts, each one running one model. Ideally, run them in parallel (we used an HPC)
# Change below the clusters of the model and the number of cores you want to use
clusters<- 4
cores<- 192

# Create the sequence object
Seqobject <- seqdef(dataset, 7:ncol(dataset))

# I also added a way to check how long each model takes
start_time <- Sys.time()

# Create the model with 4 clusters for the tactics
model = build_mmm(observations = Seqobject, n_clusters = clusters)
# Fitting the model
model_fit = fit_model(model, control_em = list(restart = list(times = 50, n_optimum=50)), threads= cores)
saveRDS(model_fit, paste0(tactics_path, "mmm_model_", clusters, ".RDS"))

#While you do have the model, you also need to associate each session with a cluster/tactic
Summary<- summary(model_fit$model)
Grouped <- Summary$most_probable_cluster
dataset$tactics_4<- Grouped
#Save the dataset with the clusters
write.csv(dataset, paste0(tactics_path, "tactics_4.csv"))

#Save how long this took
end_time <- Sys.time()
elapsed_time <- end_time - start_time
formatted_time <- format(elapsed_time, format = "%H:%M:%S")
write.csv(formatted_time, file= paste0("time_mmm_", clusters, ".csv"))


# After finishing all the models, save all variables representing the clusters in one dataset
# For example, this is for MMM with 3, 4, and 5 clusters
tactics_3 <- fread(paste0(tactics_path, "tactics_3.csv")) %>% select(-V1) %>% distinct()
tactics_4 <- fread(paste0(tactics_path, "tactics_4.csv")) %>% select(sessionid, tactics_4)
tactics_5 <- fread(paste0(tactics_path, "tactics_5.csv")) %>% select(sessionid, tactics_5)

tactics_models<- tactics_3 %>%
  left_join(tactics_4) %>%
  left_join(tactics_5)

write.csv(tactics_models, paste0(tactics_path, "tactics_models.csv"))
