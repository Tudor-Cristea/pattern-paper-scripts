# Use this to get information for the results section (RQ1 and RQ2)
# Get the barplot_data and barplot_data2 datasets from script (4) first
# Also, get the sessioned_data2 dataset from script (1), exactly after filtering the 1 click sessions

#Packages necessary
library(dplyr)
library(stringr)
options(scipen = 100, digits = 4) # necessary to see the whole ID of students/course, but also for some indicators
source("folder_paths")

# Get the tactics models calculated in script (3)
tactics_model<- read.csv(paste0(tactics_path, "tactics_models.csv")) %>% select(-X)


# Get the info for the first paragraph of results (counting the actions)
actions<- table(sessioned_data2$final_labels)
actions # the number of actions 
round(actions/nrow(sessioned_data2)*100, 1) # percentages of actions


# Get Table 1 data

# To get the percentages for the tactics:
table(barplot_data2$tactic)
table(barplot_data2$tactic)/nrow(barplot_data2)

# The dominant and 2nd dominant actions
# Filter per tactic
barplot_data3<- barplot_data2 %>% filter(tactic== "Quiz focused") %>%
  select(-tactic)
obs_state_names <- colnames(barplot_data3)
obs_states_counts <- colSums(barplot_data3[, ]) 
obs_states_perc <- obs_states_counts / sum(obs_states_counts) *100
round(obs_states_perc[order(obs_states_perc, decreasing =T)], 1)

# Session length (in clicks) per tactic:
barplot_data3$Total_Sum <- rowSums(barplot_data3)
mean(barplot_data3$Total_Sum)
median(barplot_data3$Total_Sum)
sd(barplot_data3$Total_Sum)

# To get the time in seconds of the sessions per tactic:
session_time_calc<- tactics_models %>%
  group_by(tactics_4) %>%
  summarise(mean_time= mean(session_time, na.rm=T)/60,
            sd_time= sd(session_time, na.rm=T)/60)
View(session_time_calc)


# Get the whole course strategy MM model
whole_4_MM<- readRDS(paste0(time_partitions, "whole_strategies44_model).RDS")) # run an MM model for the final whole course model (in our case 4 tactics and 4 strategies)

# Get the initial and transition probabilities of the tactics for the whole course data per strategy cluster
whole_4_MM$model$initial_probs
whole_4_MM$model$transition_probs

# Get the actual dataset for the whole course
whole_44_dataset<- read.csv(paste0(time_partitions, "whole_dataset_44.csv")) %>%
  select(-X, -strategies_3, -strategies_5, -strategies_6) # remove the other clusters

# Frequency and percentage
table(whole_44_dataset$strategies_4)
table(whole_44_dataset$strategies_4)/nrow(whole_44_dataset)

# Filter for each strategy cluster to get tactics data (Balanced, Flexible, Grade oriented, Resource oriented)
# You can definitely streamline this (e.g., script 4)
strat_cluster<- "Flexible" # change here the cluster you want the data for

# Filter for the strategy cluster
barplot_data_filter<- whole_44_dataset %>% 
  filter(strategies_4== strat_cluster) %>%  
  select(-strategies_4)

# Add the tactic names
barplot_data <- barplot_data_filter %>%
  transmute(Assignment_focused = rowSums(across(everything(), ~str_count(ifelse(is.na(.), "", .), "Assignment focused"), .names = "Assignment focused")),
            Material_focused = rowSums(across(everything(), ~str_count(ifelse(is.na(.), "", .), "Material focused"), .names = "Material focused")),
            Mixed = rowSums(across(everything(), ~str_count(ifelse(is.na(.), "", .), "Mixed"), .names = "Mixed")),
            Quiz_focused = rowSums(across(everything(), ~str_count(ifelse(is.na(.), "", .), "Quiz focused"), .names = "Quiz focused")),)

# Get the strategy length in sessions
barplot_data$Total_Sum <- rowSums(barplot_data)
mean(barplot_data$Total_Sum)
#median(barplot_data$Total_Sum)
sd(barplot_data$Total_Sum)

# Look at the tactic composition for the strategy cluster
obs_state_names <- colnames(barplot_data)
obs_states_counts <- colSums(barplot_data[, 1:4]) 
obs_states_perc <- obs_states_counts / sum(obs_states_counts) *100
round(obs_states_perc, 1)
sum(round(obs_states_perc, 1))


# Get Table 2 data
# It is the same, but we load the partition datasets instead (we load halves or quarters dataset)
# However, before we start filtering per cluster, we first filter for the targeted partition
