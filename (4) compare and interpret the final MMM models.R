# We use this script to get the parameters and graphs of the final models
# We get the graphs but also the BIC/AIC/log-likelihood
# Make sure you save all the MMM models (.RDS extension) in the same folder

# Packages
library(dplyr)
library(seqHMM)
library(TraMineR)
library(openxlsx)
library(igraph)
library(ggplot2)
library(mHMMbayes)
library(qgraph)
library(data.table)
library(stringr)
library(ggseqplot)
options(scipen = 999)
source("folder_paths")

# Load the final dataset from (3)
tactics_models<- fread(paste0(tactics_path, "tactics_models.csv")) %>% select(-V1)

# Select only the sequence variables. For us it was 7:117
sequence_vars<- 7:117
# Create the object
Seqobject <- seqdef(tactics_models, sequence_vars)
# Let's try to add a nice color palette
# Getting the number of distinct states
Number_of_colors <- length(alphabet(Seqobject))
# creating the color palette
colors <- RColorBrewer::brewer.pal(name = "Paired", n = Number_of_colors)
# Assigning the color palette
cpal(Seqobject) <- colors


# Get the AICs, BIC, log-likelihood for all models
aics_and_bics <- function() {
  # Get a list of RDS files in the specified folder
  rds_files <- list.files(path= mmm_models, pattern = ".RDS", full.names = TRUE)
  
  # Initialize empty data frame to store model names, AIC, and BIC values
  result_df <- data.frame(Model = character(0), AIC = numeric(0), BIC = numeric(0), LOG= numeric(0))
  
  # Loop through the RDS files
  for (i in 1:length(rds_files)) {
    # Read the model from the RDS file
    model_name= i+2 # we started with 3 clusters, thus, we need to adapt 
    col_index= i+117 # similarly, look at which position the cluster variable for the 3 cluster model starts

    # Load the .RDS model
    rds <- readRDS(rds_files[i])

    # Calculate AIC and BIC values
    aic_value <- AIC(rds$model)
    bic_value <- BIC(rds$model)
    log<- rds$logLik

    # Create a data frame with the model name and AIC/BIC values
    model_data <- data.frame(Model = model_name, AIC = aic_value, BIC = bic_value, LOG= log)
    
    # Append the data frame to the result data frame
    result_df <- rbind(result_df, model_data)
  
    
    # We also include the graphs here, so we create them for each model in one script
    #6. PER TACTIC: Temporal state distribution, without accounting for longer sessions
    pdf(file= paste0(tactics_path, "6. PER_TACTIC_", model_name, "mmm_temporal_not_accounting.pdf"))
    seqplot(Seqobject, type = "d", cex.legend = 0.8, ncol = 3, cex.axis = 0.6, legend.prop = 0.2, border = NA, ask= FALSE, group = tactics_models[, ..col_index])
    dev.off()

    #7. PER TACTIC: Temporal state distribution, accounting for longer sessions
    data.reshaped_with_empty <- tactics_models %>% select(-session_time) %>% rowwise %>% mutate_all(function(x){ifelse(is.na(x),"Nothing",x)}) 
    data.seq.empty <- seqdef(data.reshaped_with_empty , sequence_vars, xtstep = 2, # select only the sequence variables
                             cpal=c(colors,"white"),alphabet=c(alphabet(Seqobject),"Nothing")) 
    
    pdf(file= paste0(tactics_path, "7. PER_TACTIC_", model_name, "mmm_temporal_accounting_per_tactic.pdf"))
    seqplot(data.seq.empty,  type= "d", border = NA, use.layout=TRUE, with.legend=T, ncol =3, 
             cex.legend = .8, cex.axis=1.0, xtstep = 1, tick.last = TRUE, legend.prop=0.2, 
            group = tactics_models[, ..col_index])
    dev.off()
  
    
    #8. PER TACTIC: Make the initial + transition probability graph per cluster
    # Save the labels and make sure the clusters are in order (so the visual graphs represent the actual numbers)
    cluster_names <- unique(tactics_models[, ..col_index])
    Labelx <- colnames(rds$model$transition_probs[[1]]) # we need to get the labels
    rds$model$cluster_names <- as.vector(cluster_names) 
    
    # Loop through each cluster and create qgraph per cluster, then save all in one pdf
    pdf(file= paste0(tactics_path, "8. PER_TACTIC_", model_name, "mmm_intial_transition_probs_per_tactic.pdf"))
    
    for (i in 1:model_name) {
      tp <- rds$model$transition_probs[[i]]
      initp <- rds$model$initial_probs[[i]]
      title <- cluster_names[i]
      
     qgraph(tp, cut = 0.15, minimum = 0.05, labels = Labelx,
             edge.labels = TRUE, edge.label.cex = 0.65, color = cpal(Seqobject),
             pie = initp, curveAll = TRUE, theme = "colorblind", title= title,
             label.cex = 1.8, vsize= 8, layout= "groups")
    }
    dev.off()
    
    #9. PER TACTIC: Barplot with the frequency of each state
    barplot_data<- tactics_models[, sequence_vars] # select only the sequence variables
    barplot_data2 <- barplot_data %>%
      transmute(announcement = rowSums(across(everything(), ~str_count(ifelse(is.na(.), "", .), "announcement"), .names = "announcement")),
                assignment = rowSums(across(everything(), ~str_count(ifelse(is.na(.), "", .), "assignment"), .names = "assignment")),
                course_browsing = rowSums(across(everything(), ~str_count(ifelse(is.na(.), "", .), "course_browsing"), .names = "course_browsing")),
                discussion = rowSums(across(everything(), ~str_count(ifelse(is.na(.), "", .), "discussion"), .names = "discussion")),
                learning_material = rowSums(across(everything(), ~str_count(ifelse(is.na(.), "", .), "learning_material"), .names = "learning_material")),
                mandatory_quiz = rowSums(across(everything(), ~str_count(ifelse(is.na(.), "", .), "mandatory_quiz"), .names = "mandatory_quiz")),
                metacognition = rowSums(across(everything(), ~str_count(ifelse(is.na(.), "", .), "metacognition"), .names = "metacognition")),
                optional_quiz = rowSums(across(everything(), ~str_count(ifelse(is.na(.), "", .), "optional_quiz"), .names = "optional_quiz")),
                submission = rowSums(across(everything(), ~str_count(ifelse(is.na(.), "", .), "submission"), .names = "submission")),
                tactic= tactics_models[, ..col_index])
    
    dictionary<- unique(barplot_data2$tactic) %>% unlist()
    
    pdf(file= paste0(tactics_path, "9. PER_TACTIC_", model_name, "mmm_barplot_freq_obs_states.pdf"))
  
    for (i in dictionary) {
      barplot_data3<- barplot_data2 %>% filter(tactic== i) %>%
        select(-tactic)
      obs_state_names <- colnames(barplot_data3)
      obs_states_counts <- colSums(barplot_data3[, ]) 
      obs_states_perc <- obs_states_counts / sum(obs_states_counts) *100
      
      barplot(obs_states_perc,
                main = i,
                ylab = "Percentage", cex.axis = 0.8, cex.names = 0.7, col= colors, las=2,
              ylim= c(0, 60)) 
    }
    dev.off()
    
    # The index plots are too big for .pdf, when using the seqplot function
    # Thus, we recommend using ggseqiplot() and saving them as .png
    # You need to filter them for each cluster 
    # Due to reasons of brevity, we provide one example
    
    png(file= paste0(tactics_path, "index_from_start_4.png"))
    ggseqiplot(Seqobject, sortv = "from.start") # sortv can be removed 
    dev.off()
    
  }

  # Print the result data frame
  print(result_df)
  write.xlsx(result_df, paste0(tactics_path, "tactics_mmm_parameters.xlsx"))
}

# It is easier to do it like this, compared to a for loop
aics_and_bics()