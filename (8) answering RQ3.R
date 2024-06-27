# This is an example on how to answer RQ3 (only the halves data analysis)
# We will use the dataset containing the sequence for strategy transitions
# In this case, the sequence is made out of the partitions (explained in the README)
# Packages necessary
library(dplyr)
library(ggplot2)
library(broom)
library(car)
library(qgraph)
library(TraMineR)
library(rcompanion)
options(scipen = 100, digits = 4) #necessary to see the whole ID of students/course, but also for some indicators
source("folder_paths")

# Load the strategy transition dataset
change_data<- read.csv(paste0(strategy_change_path, "halves_change.csv")) %>%
  select(-X)

# Get the strategy usage for the halves
round(table(change_data$Half_1)/nrow(change_data)*100,1)
round(table(change_data$Half_2)/nrow(change_data)*100,1)

mm_object <- seqdef(change_data, 2:3)
Number_of_colors <- length(alphabet(mm_object))
colors <- MetBrewer::met.brewer(name = "Paquin", n = 4)
cpal(mm_object) <- colors

# Make an mm model first, for the initial and transition probabilities of the strategies between halves
mm_change<- build_mm(mm_object)
mm_change$initial_probs
mm_change$transition_probs

#Build the transition graph of the strategies between halves
Labelx <- alphabet(mm_object) # get the labels to use them as nodes names.
par(mfrow=c(1, 1))
trans <- mm_change$transition_probs
init <- mm_change$initial_probs
Averagelayout <- averageLayout(trans)

pdf(file= paste0(strategy_change_path, "strat_change_halves_trans.pdf"))
qgraph(trans, cut = 0.15, minimum = 0.05, label.cex= 1.5, labels = Labelx, 
                           edge.labels = TRUE, edge.label.cex = 0.8,
                           color = cpal(mm_object), layout = Averagelayout,
                           pie = init, theme = "colorblind", title = "Strategy change transition probabilities")
dev.off()


# In order to answer RQ3, we will look at how grades differ between the strategy clusters and partitions
# We start with ANOVA of students in first and second half (grades)

# ASSUMPTIONS for ANOVA
# Normality (need to filter the grades per group in each half)

# Create the ANOVA subgroups
# First half
aov_balanced_1<- change_data %>% filter(Half_1=="Balanced")
aov_resource_1<- change_data %>% filter(Half_1=="Resource oriented")
aov_grade_1<- change_data %>% filter(Half_1=="Grade oriented")
aov_flexible_1<- change_data %>% filter(Half_1=="Flexible")

shapiro.test(aov_balanced_1$final_grade) #assumption violated
shapiro.test(aov_resource_1$final_grade) #assumption violated
shapiro.test(aov_grade_1$final_grade) #assumption violated
shapiro.test(aov_flexible_1$final_grade) #assumption violated

# Second half
aov_balanced_2<- change_data %>% filter(Half_2=="Balanced")
aov_resource_2<- change_data %>% filter(Half_2=="Resource oriented")
aov_grade_2<- change_data %>% filter(Half_2=="Grade oriented")
aov_flexible_2<- change_data %>% filter(Half_2=="Flexible")

shapiro.test(aov_balanced_2$final_grade) #assumption violated
shapiro.test(aov_resource_2$final_grade) #assumption violated
shapiro.test(aov_grade_2$final_grade) #assumption violated
shapiro.test(aov_flexible_2$final_grade) #assumption violated

#Homogeneity of variances for each half
car::leveneTest(final_grade ~ Half_1, data = change_data) #assumption violated
car::leveneTest(final_grade ~ Half_2, data = change_data) #assumption violated

# The ANOVA assumptions are violated, thus, we need to use non-parametric tests


# We will apply Kruskal-Wallis to each half
kruskal.test(final_grade ~ Half_1, data = change_data) #sign.
kruskal.test(final_grade ~ Half_2, data = change_data) #sign.

# We continue with post-hoc (Dunn works best when both assumptions are violated)
dunn_anova_1 <- FSA::dunnTest(change_data$final_grade, change_data$Half_1, method = "bonferroni")
print(dunn_anova_1)
# To get effect sizes, we use Cliff's Delta
multiVDA(final_grade ~ Half_1, data=change_data, statistic= "CD")

dunn_anova_2 <- FSA::dunnTest(change_data$final_grade, change_data$Half_2, method = "bonferroni")
print(dunn_anova_2)
# To get effect sizes, we use Cliff's Delta
multiVDA(final_grade ~ Half_2, data=change_data, statistic= "CD")

first_half_grades<- change_data %>%
  group_by(Half_1) %>%
  summarise(mean= mean(final_grade, na.rm=T),
            sd= sd(final_grade, na.rm=T),
            n=n())
first_half_grades

second_half_grades<- change_data %>%
  group_by(Half_2) %>%
  summarise(mean= mean(final_grade, na.rm=T),
            sd= sd(final_grade, na.rm=T),
            n=n())
second_half_grades



# Next, to look at specific transitions, we will create a new variables representing them
change_data$change<- paste(change_data$Half_1, change_data$Half_2, sep= "->")
table(change_data$change)
sort(round(table(change_data$change)/nrow(change_data)*100, 1))


# In our case, we have 16 total different transitions
# We start by looking at general differences between them

# Use Kruskal-Wallis to be safe 
kruskal.test(final_grade ~ change, data = change_data)

# Post-hoc for non-parametric strategy change
dunn_change<- FSA::dunnTest(change_data$final_grade, change_data$change, method = "bonferroni")
print(dunn_change)
# We filter to keep only the transition differences that are significant in final grades
strat_change_sig<- dunn_change$res %>% filter(P.adj< 0.05)
strat_change_sig
# To get effect sizes, we use Cliff's Delta
multiVDA(final_grade ~ change, data=change_data, statistic= "CD")


# We get the grades per transition 
change_grades<- change_data %>%
  group_by(change) %>%
  summarise(grade_mean= round(mean(final_grade, na.rm=T),1),
            grade_median= round(median(final_grade, na.rm=T),1),
            grade_sd= round(sd(final_grade, na.rm=T),1),
            n=n(),
            Percentage= round(n/ncol(change_data)*100, 1))%>%
  arrange(grade_mean)
View(change_grades)

# You can also save all this in a word document
# Create a flextable object
ft <- flextable::flextable(change_grades)
# Create a Word document
doc <- officer::read_docx()
# Insert the flextable into the Word document
doc <- doc %>%
  flextable::body_add_flextable(value = ft)
# Save the Word document
print(doc, target = "change_grades.docx")


# Compare the grades between consistent and inconsistent students
consistent_students<- change_data %>%
  mutate(consistent= case_when(Balanced==2 ~ "consistent",
                           Grade_oriented==2 ~ "consistent",
                           Resource_oriented==2 ~ "consistent",
                           Flexible==2 ~ "consistent",
                           # you can add this when looking at quarters data, to get the Undecided group
                           # Balanced==1 & Grade_oriented==1 & Resource_oriented==1 & Flexible==1 ~ "undecided",
                           .default = "inconsistent"))


consistent_grades<- consistent_students %>%
  group_by(consistent) %>%
  summarise(mean= mean(final_grade, na.rm=T),
            median= median(final_grade, na.rm=T),
            sd= sd(final_grade, na.rm=T),
            n= n())
consistent_grades

# Filter for the two groups and run a Welch two-sample t-test
consistent_t<- consistent_students %>% filter(consistent=="consistent")
inconsistent_t<- consistent_students %>% filter(consistent=="inconsistent")
t.test(consistent_t$final_grade, inconsistent_t$final_grade, var.equal = F)


# Now we compare the consistent students between themselves, based on the strategy they used 
consistent_students_post<- change_data %>%
  mutate(consistent_post= case_when(Balanced==2 ~ "consistent_Balanced",
                               Grade_oriented==2 ~ "consistent_GO",
                               Resource_oriented==2 ~ "consistent_RO",
                               Flexible==2 ~ "consistent_Flexible",
                               .default = "inconsistent")) #%>%
table(consistent_students_post$consistent_post)

# We run a Kruskal-Wallis to check if they differ in final grade
kruskal.test(final_grade~consistent_post, data=consistent_students_post)

# We run Dunn tests as post-hoc
dunn_change<- FSA::dunnTest(consistent_students_post$final_grade, consistent_students_post$consistent_post, method = "bonferroni")
print(dunn_change)
# Again, we filter to keep only sign. p-values
dunn_sign<- dunn_change$res %>% filter(P.adj< 0.05)
print(dunn_sign)
# To get effect sizes, we use Cliff's Delta
multiVDA(final_grade ~ consistent_post, data=consistent_students_post)

# Finally, we get the grades for each subgroup
consistent_post_grades<- consistent_students_post %>%
  group_by(consistent_post) %>%
  summarise(mean= mean(final_grade, na.rm=T),
            median= median(final_grade, na.rm=T),
            sd= sd(final_grade, na.rm=T),
            n=n())
consistent_post_grades