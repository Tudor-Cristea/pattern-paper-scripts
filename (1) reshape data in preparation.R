# At this point, you should have the pseudoanonymized Canvas dataset with all clicks named as clear events (e.g., Discussion, Resources) 
# As explained in the paper, we have 9 clear categories for the clicks
# We take the  dataset with all the events, and prepare it for the pattern analysis
# We need to reshape it so we have the sequences (which will represent the learning sessions/tactics)

#Packages
library(tidyverse)
library(data.table)
library(reshape2)
options(scipen = 999)
# Make another script with all the paths and load it in the beginning of each script
source("folder_paths")


final_dataset<- fread(paste0(data_path, "all_courses_final.csv"), 
                      colClasses= c("user_id"= "character")) 

# Final decisions when defining sessions
# 85% learning session: 725 seconds (12.1 minutes) between clicks before a new session starts
# 90% a session can have a maximum of 111 clicks

# Create sessions
sessioned_data<- final_dataset %>% 
  mutate(timecreated= as.numeric(timestamp)) %>% 
  arrange(timecreated, course_userid) %>% # arranging by time to get data ordered
  group_by(course_userid) %>% # group sessions according to users
  mutate(Time_gap = round(timecreated - lag(timecreated), 2)) %>%  # lag is the time difference between two successive actions
  mutate(new_session = is.na(Time_gap) | Time_gap > 725) %>% # if the time difference is more than 725 seconds, a new session starts
  mutate(session_nr = cumsum(new_session)) %>% # give sessions an ascending number
  mutate(sessionid = paste0(course_userid, "_", "Session_", session_nr)) %>% # get a unique ID for each session
  group_by(course_userid, sessionid) %>%   add_tally(name= "sequence_length") # paste the username to the session to make it more understandable

# Remove one click sessions
sessioned_data2<- sessioned_data %>%
  filter(sequence_length != 1)

# We calculate the sequence lengths (clicks in a session), in order to decide how long sessions should be
# This means we take some of the results here and apply them to the previous "Create sessions"
sessioned_data2<- sessioned_data2 %>% 
  group_by(sessionid) %>% 
  mutate(Sequence=seq_along(sessionid))

# There are no clear rules when when a new session starts (as explained in the paper). We chose based on when the biggest change occurs (large jump in numbers between quantiles)
quantile(sessioned_data$Time_gap, c(0.10, 0.50, 0.75, 0.81, 0.85, 0.90, 0.95, 0.98, 0.99, 1.00), na.rm = T)

# We calculate session length distribution at these percentages (and apply below)
quantile(sessioned_data2$sequence_length, c(0.05, 0.1, 0.50,0.60, 0.75, 0.90, 0.95, .98, .99, 1.00))

# Here we decide the maximum number of clicks per session
Longsessions<- sessioned_data2 %>% 
  arrange(timecreated, course_userid) %>% # arranging by time to get data ordered
  group_by(course_userid) %>% # group sessions according to users
  mutate(new_session =  (Sequence %% 111 ==0) | new_session) %>% 
  mutate(session_seq = cumsum(new_session)) %>% # give sessions an ascending number
  mutate(sessionid = paste0(course_userid, "_", "LSession_", session_seq)) %>% 
  ungroup() %>%
  group_by(sessionid) %>% 
  add_tally(name="Session_length") # Paste the username to the session to make it more understandable

# Here we calculate when the sessions took place (when they started)
Longsessions<- Longsessions %>% 
  select(timestamp: Sequence) %>% 
  group_by(sessionid) %>% 
  mutate(sessiontime=min(timestamp)) %>% 
  arrange(sessiontime) %>%
  ungroup()

# Final dataset used for reshaping
sessionedready<- Longsessions %>% 
  group_by(course_userid, sessionid) %>% # grouping by user and session
  mutate(Sequence = seq_along(sessionid)) %>% # create a sequence of events in each session
  mutate(sequence_length = length(Sequence), session_nr= as.numeric(factor(sessionid))) # calculate length of sequences


# Calculating the session length (in time) for each session
time<- sessionedready %>%
  filter(new_session== "FALSE") %>%
  group_by(sessionid) %>%
  mutate(time= if_else(Time_gap==0, round(diff_time, 2), round(Time_gap, 2))) %>%
  summarize(session_time= sum(time))
mean(time$session_time, na.rm=T)


# We convert the previous dataset's datapoints into sequence objects
# This means that each row represents a unique learning session/tactic
data.reshaped <- dcast(course_userid + sessionid + sessiontime + course_id + weeklabel + session_time~ Sequence, data = sessionedready, value.var = "final_labels")

# You can calculate some descriptive data here, such as number of students, courses, average session length etc.
length(unique(data.reshaped$course_userid))
length(unique(data.reshaped$course_id))

# We save the dataset for the pattern analysis
write.csv(data.reshaped, paste0(data_path, "all_data_final_weeklabel.csv"))
