# Dynamics of self-regulated learning: R scripts

R scripts used in the analysis for the paper *Dynamics of self-regulated learning: The effectiveness of students' strategies across course periods* by T.S. Cristea, S. Heikkinen, C. Snijders, M. Saqr, U. Matzat, R. Conijn and A. Kleingeld, published in *Computers & Education* (2025): <https://doi.org/10.1016/j.compedu.2025.105233>. This is Chapter 5 of my PhD dissertation (Eindhoven University of Technology, 2025).

For the data cleaning and pre-processing steps that come before these scripts, see my tutorial (Chapter 3): <https://github.com/Tudor-Cristea/tutorial-for-LA-in-R>

## Data

The scripts work with Canvas data. The student data used in the paper are not included in this repository, because they are personal data that cannot be shared publicly.

## Requirements

R, with these packages:

```r
install.packages(c("tidyverse", "data.table", "reshape2", "TraMineR", "seqHMM", "ggseqplot", "mHMMbayes", "igraph", "qgraph", "broom", "car", "rcompanion", "openxlsx"))
```

## How to use the scripts

The scripts are numbered in the order you should use them, and some have prerequisites, which are described below.

Start by loading the "course_dim" table, which contains the "course_id" of your target course. You can use it to filter the requests in the "requests" table. You should also add the "enrollment_dim" table to filter by role (e.g., students) using the "type" column.

After cleaning the dataset, use script (1) to reshape and prepare it for Markov modelling. If you want to analyse time partitions, use the timestamp variable at this stage to create a column with the week or month of each click, and calculate the length of each session (in seconds).

Script (2) shows how to run a simple Markov model. It calculates the initial and transition probabilities between actions or clicks (the observed states, in Markov modelling terms), and includes some types of graphs that help with interpretation.

Script (3) shows how to run mixture Markov models (MMMs). Script (4) then calculates several parameters (log-likelihood, AIC and BIC) that help you decide on the final models. We also recommend using some of the graphs, as well as theory, when making this decision.

Script (5) prepares the time-based partitions for the strategy clustering. We prepared three datasets: full course, course halves and course quarters. Script (6) is similar to script (3): it runs the clustering that identifies the strategies. We only include the script for the course halves as an example; the others are similar. Once you have the strategy models, you can reuse script (4) to get the parameters and graphs you need to choose the final models.

Next, reshape the data again, this time with sequences made of the strategies students used in each half or quarter, and attach the grades. The result is a dataset with the student_course ID, the strategy used in the first half, the strategy used in the second half and the grades (for quarters, each student has four data points). Then run simple Markov models (script 2) to get the initial and transition probabilities of strategy changes. You can continue with scripts (3) and (4) if you want to model the strategy transitions.

Script (7) shows how to answer the paper's first two research questions (RQ1 and RQ2). It loads some earlier tables (mainly the strategy datasets) and then filters per cluster (for the whole dataset), or per partition and then per cluster (e.g., for the course halves, it first keeps only the first half and then filters for one strategy cluster).

Script (8) shows how to answer RQ3. Again, this is the script used for the course halves.

## Licence and citation

The code is available under the MIT licence (see LICENSE). If you use it, please cite the paper above; the "Cite this repository" button in the sidebar gives the reference.
