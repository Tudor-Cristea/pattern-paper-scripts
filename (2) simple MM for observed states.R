# We use the reshaped dataset to calculate some simple Markov Models (MM)
# From this we get the initial and transition probabilities for the tactics

#Packages
library(tidyverse)
library(data.table)
library(TraMineR)
library(seqHMM)
library(qgraph)
options(scipen = 999)
source("folder_paths")

# We load the final dataset from script (5). This is used to create a sequence object 
dataset <- fread(paste0(data_path, "all_data_final_weeklabel.csv")) %>%
  select(-V1)

# Select only the sequence variables. For us it was 7:117
sequence_vars<- 7:117
# Create the sequence object
Seqobject <- seqdef(dataset, sequence_vars)
# Let's try to add a nice color palette
# Getting the number of distinct states
Number_of_colors <- length(alphabet(Seqobject))
# Creating the color palette
colors <- RColorBrewer::brewer.pal(name = "Paired", n = Number_of_colors)
# Assigning the color palette
cpal(Seqobject) <- colors

# Building a simple MM model
model = build_mm(observations = Seqobject)
# We will start saving the MM models in special folder
saveRDS(model, paste0(MM_path, "mm_model.RDS"))

# After we saved it the first time, we can start just loading it directly
mm_model<- readRDS(paste0(MM_path, "mm_model.RDS"))

# Checking the model, here we can see the initial and transition probabilities matrices
mm_model

# We will create some graphs to better understand the data at this point
# These graphs represent the 9 actions/clicks (when using MMs, these are known as observed states) 
#1. Graph for the overall distribution of the observed states
pdf(file= paste0(MM_path, "1. obs_states_temporal_distribution.pdf"))
seqplot(Seqobject, type = "d", cex.legend = 0.8, ncol = 1, cex.axis = 1, 
        legend.prop = 0.30, main= "Observed states temporal distribution not accounting for session length",
        xlab =  "Length of sessions", border=T)
dev.off()


#2. We redo the temporal distribution, but account for the longer sessions
# Sometimes the distribution plot is very misleading if there are many short sessions and very long ones. 
# This allows us to better understand the distribution (run both and compare them to see) 
data.reshaped_with_empty <- dataset %>% select(-session_time) %>% rowwise %>% mutate_all(function(x){ifelse(is.na(x),"Nothing",x)}) # Here we replace all NAs with "Nothing"

# We create the new sequence object adding the "Nothing" as an additional event at the end, 
#as well as the color white to represent it so it doesn't show in the plot
data.seq.empty <- seqdef(data.reshaped_with_empty , 7:ncol(data.reshaped_with_empty), xtstep = 2, 
                         cpal=c(colors,"white"),alphabet=c(alphabet(Seqobject),"Nothing")) 

pdf(file= paste0(MM_path, "2. obs_states_temporal_distribution_accounting.pdf"))
seqdplot(data.seq.empty,  border = NA, use.layout=TRUE, with.legend=T, ncol =3, 
         cex.legend = .8, cex.axis= 1, xtstep = 1, tick.last = TRUE, legend.prop=0.25, 
         main="Observed states temporal distribution plot acknowledging session length", xlab =  "Length of sessions")
dev.off()

#3. With this plot we check all the transition probabilities for better visualization
overalltransitions <- seqtrate(Seqobject)
# Using the matrix, we use a novel plotting technique from the TraMinerR package
Labelx <- alphabet(Seqobject) # get the labels to use them as nodes names.

par= c(1,1) # this allows use to save one graph per pdf page
transitionsplot <- qgraph(overalltransitions, cut = 0.25, minimum = 0.05,
                          labels = Labelx, edge.labels = TRUE, edge.label.cex = 0.65,
                          color = cpal(Seqobject), curveAll = TRUE,
                          theme = "colorblind", mar = c(4, 3, 4, 3))


#Also add the initial transition probabilities to the same graph (this is not necessary)
pdf(file= paste0(MM_path, "3. obs_states_initial_transition_probs.pdf"))
overallplot <- qgraph(overalltransitions, cut = 0.15, minimum = 0.05,
                      labels = Labelx, edge.labels = TRUE,
                      edge.label.cex = .8, color = cpal(Seqobject),
                      curveAll = TRUE, theme = "colorblind",
                      pie = mm_model$initial_probs, 
                      title= "Initial and transition probabilities for observed states",
                      label.cex = 1.8, vsize= 8)      
dev.off()


#4. Barplot with the frequency of each click/state
barplot_data<- dataset[, sequence_vars] # select only the variables that represent the clicks

#Calculate the number of each action per session (these are the 9 categories we used)
barplot_data2 <- barplot_data %>%
  transmute(announcement = rowSums(across(everything(), ~str_count(ifelse(is.na(.), "", .), "announcement"), .names = "announcement")),
         assignment = rowSums(across(everything(), ~str_count(ifelse(is.na(.), "", .), "assignment"), .names = "assignment")),
         course_browsing = rowSums(across(everything(), ~str_count(ifelse(is.na(.), "", .), "course_browsing"), .names = "course_browsing")),
         discussion = rowSums(across(everything(), ~str_count(ifelse(is.na(.), "", .), "discussion"), .names = "discussion")),
         learning_material = rowSums(across(everything(), ~str_count(ifelse(is.na(.), "", .), "learning_material"), .names = "learning_material")),
         mandatory_quiz = rowSums(across(everything(), ~str_count(ifelse(is.na(.), "", .), "mandatory_quiz"), .names = "mandatory_quiz")),
         metacognition = rowSums(across(everything(), ~str_count(ifelse(is.na(.), "", .), "metacognition"), .names = "metacognition")),
         optional_quiz = rowSums(across(everything(), ~str_count(ifelse(is.na(.), "", .), "optional_quiz"), .names = "optional_quiz")),
         submission = rowSums(across(everything(), ~str_count(ifelse(is.na(.), "", .), "submission"), .names = "submission")))

obs_state_names <- colnames(barplot_data2)

# Sum the counts for each state across all students
obs_states_counts <- colSums(barplot_data2[, ]) 
# and percentages
obs_states_perc <- obs_states_counts / sum(obs_states_counts) *100
obs_states_perc

# Create a barplot with different colors
pdf(file= paste0(MM_path, "4. barplot_observed_states.pdf"))
barplot(obs_states_perc,
        main = "Frequency of observed states overall", col = colors,
        xlab = "Observed states", ylab = "Percentage",
        cex.axis = 0.8, cex.names = 0.7)#, las= 2) # this turns the labels to vertical
dev.off()


#5. Index plot for all observed states (type= "I") graph
# This graph was  too much in pdf, thus, I saved it in png
# If this still does not work, try the "ggseqplot" package
png(file= paste0(MM_path, "5. index_plot_all_observed_states.png"))
seqplot(Seqobject, type = "I", cex.legend = 0.8, ncol = 3, cex.axis = 0.6, legend.prop = 0.25, border = T)
dev.off()
