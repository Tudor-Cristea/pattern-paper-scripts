# Use this script to prepare the time based partitions before modelling for the strategies
#Packages
library(dplyr)
library(igraph)
library(ggplot2)
library(mHMMbayes)
library(qgraph)
library(stringr)
library(tidyr)
library(data.table)
options(scipen = 999)
source("folder_paths")

# Based on the interpretation from script (4), choose a tactics model (for us it was 4)
model_name<- 4

#Calculate how many tactics are employed for each instance (student/course)
count_non_na <- function(row) {
  sum(!is.na(row))
}

# Make sure to attach the "tactics_x" variable (from the model you want) to the main dataset 
# This way you don't have to load and attach it every time you want to work with the clusters
all_data_final_reshaped<- fread(paste0(tactics_path, "tactics_models.csv")) %>% select(-V1)

# Now look at sequencing the filtered courses into halves and quarters
# You should probably do this before the "reshaping of data". Example:
# w1-9 is first upto ninth week of lectures
# ew1-2 is first or second week of exams
# ew3 is two weeks after exam weeks
first_half_weeks<- c("w0", "w1", "w2", "w3", "w3a", "w4", "w5")
second_half_weeks<- c("w6", "w6a", "w7", "w8", "w9", "ew1", "ew2", "ew3")

all_data_final_reshaped$halves = if_else(all_data_final_reshaped$weeklabel %in% first_half_weeks, 'first_half', 'second_half')

#Calculate how many students have presence in both halves
filtered_ids<- all_data_final_reshaped %>%
  select(course_userid, halves) %>%
  group_by(course_userid, halves) %>%
  summarise(test= n())

string_counts <- table(filtered_ids$course_userid)

# Filter for students that appear twice
strings_appearing_twice <- names(string_counts[string_counts == 2])

filtered_ids2<- filtered_ids %>%
  filter(course_userid %in% strings_appearing_twice) %>%
  select(course_userid) %>%
  unique() 

#We have the course_userid's of the students that are present in both course halves
#Now filter the main dataset and continue
all_data_final_reshaped2<- all_data_final_reshaped %>%
  filter(course_userid %in% filtered_ids2$course_userid)

#Change the course_userid so it contains the course period
all_data_final_reshaped2$course_userid <- ifelse(all_data_final_reshaped2$halves == "first_half",
                         paste0(all_data_final_reshaped2$course_userid, "_1"),
                         ifelse(all_data_final_reshaped2$halves == "second_half",
                                paste0(all_data_final_reshaped2$course_userid, "_2"),
                                all_data_final_reshaped2$course_userid))

#Run the full course strategy also
tactics_full_course <- all_data_final_reshaped %>%
  select(course_userid, tactics_4) %>%
  group_by(course_userid) %>%
  mutate(row_id = row_number()) %>%
  pivot_wider(names_from = row_id, values_from = tactics_4, names_prefix = "Session_") %>%
  ungroup()

tactics_full_course$tactics_count <- rowSums(!is.na(tactics_full_course))
quantile(tactics_full_course$tactics_count, c(0.05, 0.1, 0.50, 0.75, 0.90, 0.95,.98,.99, 1.00)) # We calculate session length distribution

tactics_90<- tactics_full_course[, 1:121] # we decided on keeping only 90th quantile students, when it comes to number of sessions
write.csv(tactics_90, paste0(time_partitions_path, "tactics_90.csv"))


#Continue with the course halves
first_half_sessions<- all_data_final_reshaped2 %>% filter(halves=="first_half")
second_half_sessions<- all_data_final_reshaped2 %>% filter(halves=="second_half")

tactics_halves_1 <- first_half_sessions %>%
  select(course_userid, tactics_4) %>%
  group_by(course_userid) %>%
  mutate(row_id = row_number()) %>%
  pivot_wider(names_from = row_id, values_from = tactics_4, names_prefix = "Session_") %>%
  ungroup()

tactics_halves_2 <- second_half_sessions %>%
  select(course_userid, tactics_4) %>%
  group_by(course_userid) %>%
  mutate(row_id = row_number()) %>%
  pivot_wider(names_from = row_id, values_from = tactics_4, names_prefix = "Session_") %>%
  ungroup()

# Above we reshaped the data per course half, after which we bind them
# This way, we run the model for both halves 
tactics_halves<- tactics_halves_1 %>%
  bind_rows(tactics_halves_2)

#Calculate the tactics per student half (remove 1 for the first var)
tactics_halves$tactics_count <- rowSums(!is.na(tactics_halves))-1
quantile(tactics_halves$tactics_count, c(0.05, 0.1, 0.50, 0.75, 0.90, 0.95,.98,.99, 1.00)) # we calculate session length distribution

tactics_halves_90<- tactics_halves[, 1:64] # for course halves, we also kept only the 90th quantile 
write.csv(tactics_halves_90, paste0(time_partitions_path, "tactics_halves_90.csv"))


#We do the same for course quarters
quarter1_weeks<- c("w0", "w1", "w2")
quarter2_weeks<- c("w3", "w3a", "w4", "w5")
quarter3_weeks<- c("w6", "w6a", "w7", "w8")
quarter4_weeks<- c("w9", "ew1", "ew2", "ew3")

all_data_final_reshaped$quarters = case_when(all_data_final_reshaped$weeklabel %in% quarter1_weeks ~ 'first_quarter',
                                             all_data_final_reshaped$weeklabel %in% quarter2_weeks ~ 'second_quarter',
                                             all_data_final_reshaped$weeklabel %in% quarter3_weeks ~ 'third_quarter',
                                             all_data_final_reshaped$weeklabel %in% quarter4_weeks ~ 'fourth_quarter')

#Calculate how many students have presence in all quarters
filtered_ids<- all_data_final_reshaped %>%
  select(course_userid, quarters) %>%
  group_by(course_userid, quarters) %>%
  summarise(test= n())

string_counts <- table(filtered_ids$course_userid)

# Filter for students that appear 4 times
strings_appearing_4 <- names(string_counts[string_counts == 4])

# Filter your original variable
filtered_ids2<- filtered_ids %>%
  filter(course_userid %in% strings_appearing_4) %>%
  select(course_userid) %>%
  unique() 

#Now filter the main dataset and continue
all_data_final_reshaped2<- all_data_final_reshaped %>%
  filter(course_userid %in% filtered_ids2$course_userid)

all_data_final_reshaped2$course_userid <- ifelse(all_data_final_reshaped2$quarters == "first_quarter",
                                                paste0(all_data_final_reshaped2$course_userid, "_1"),
                                                ifelse(all_data_final_reshaped2$quarters == "second_quarter",
                                                       paste0(all_data_final_reshaped2$course_userid, "_2"),
                                                       if_else(all_data_final_reshaped2$quarters == "third_quarter",
                                                               paste0(all_data_final_reshaped2$course_userid, "_3"),
                                                               if_else(all_data_final_reshaped2$quarters == "fourth_quarter",
                                                                       paste0(all_data_final_reshaped2$course_userid, "_4"),
                                                                       all_data_final_reshaped2$course_userid))))



q1_sessions<- all_data_final_reshaped2 %>% filter(quarters=="first_quarter")
q2_sessions<- all_data_final_reshaped2 %>% filter(quarters=="second_quarter")
q3_sessions<- all_data_final_reshaped2 %>% filter(quarters=="third_quarter")
q4_sessions<- all_data_final_reshaped2 %>% filter(quarters=="fourth_quarter")

tactics_q1 <- q1_sessions %>%
  select(course_userid, tactics_4) %>%
  group_by(course_userid) %>%
  mutate(row_id = row_number()) %>%
  pivot_wider(names_from = row_id, values_from = tactics_4, names_prefix = "Session_") %>%
  ungroup()

tactics_q2 <- q2_sessions %>%
  select(course_userid, tactics_4) %>%
  group_by(course_userid) %>%
  mutate(row_id = row_number()) %>%
  pivot_wider(names_from = row_id, values_from = tactics_4, names_prefix = "Session_") %>%
  ungroup()

tactics_q3 <- q3_sessions %>%
  select(course_userid, tactics_4) %>%
  group_by(course_userid) %>%
  mutate(row_id = row_number()) %>%
  pivot_wider(names_from = row_id, values_from = tactics_4, names_prefix = "Session_") %>%
  ungroup()

tactics_q4 <- q4_sessions %>%
  select(course_userid, tactics_4) %>%
  group_by(course_userid) %>%
  mutate(row_id = row_number()) %>%
  pivot_wider(names_from = row_id, values_from = tactics_4, names_prefix = "Session_") %>%
  ungroup()

tactics_quarters<- tactics_q1 %>%
  bind_rows(tactics_q2)%>%
  bind_rows(tactics_q3)%>%
  bind_rows(tactics_q4)


tactics_quarters$tactics_count <- rowSums(!is.na(tactics_quarters))-1 
quantile(tactics_quarters$tactics_count, c(0.05, 0.1, 0.50, 0.75, 0.90, 0.95,.98,.99, 1.00)) # We calculate session length distribution


#Look at 90% quantile and save only those students (63 for halves, 34 for quarters)
tactics_quarter_90<- tactics_quarters[, 1:35]

# Save the quarters dataset
write.csv(tactics_quarter_90, paste0(time_partitions_path, "tactics_quarter_90.csv"))
