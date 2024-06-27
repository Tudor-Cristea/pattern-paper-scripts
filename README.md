The scripts used for the "Temporal Evolution of Self-Regulated Learning in Online Courses: Markov Modelling Across Course Periods". These are all done using Canvas data. Some have prerequisites, which are mentioned below; I also numbered the scripts, so it is easier to follow.

You should start by loading the "course_dim" table which contains the "course_id" of your targeted course. This can be used to filter the requests from the "requests" table. You should also add the "enrollment_dim" table in order to filter for the role (e.g., students) using the "type" column. 

After cleaning the dataset, you can use script (1) in order to shape and prepare the dataset for Markov Modelling. If interested in analyzing time partitions, we recommend you use the timestamp variable to already create a column with the week/month in which the click took place. Also, try to already calculate the length of the sessions (in seconds)

Script (2) shows how to run a simple Markov Model. This model calculates various initial and transition probabilities between actions/clicks (also known as observed states when Markov Modelling). We also attached some types of graphs that can help with interpretation

Script (3) shows how to run Mixture Markov Models (MMMs). Script (4) is then used to calculate various parameters (log-likelihood, AIC, and BIC) which should help make a decision on the final models; we also recommend using some of the graphs, but also theory when making this decision. 

Script (5) is used to prepare the time-based paritions for the strategy clustering. In our case, we prepared three datasets: full course, course halves, and course quarters. Script (6) is similar to script (3) as we run the clustering in order to get the strategies; we only gave as an example the script we used for the course halves but the others are be similar. After you get the strategy models, you can reuse script (4) in order to get the parameters and graphs in order to make a final decision. 

Next, you can reshape the data again, this time the sequence being made out of the strategy students used for halves and quarters. You should also attach the grades to this dataset. This results in a dataset with the student_course ID, the strategy used in the first half, the strategy used in the second half, and the grades (same for quarters but in this case the student has four datapoints). Next you should run simple Markov Models to get the initial and transition probabilities of strategy changes (script 2). You can similarly continue with scripts 3 and 4 if you want to check models of strategy transitions. 

Script (7) shows how to answer RQ1 and RQ2. We load some previous tables (mainly the strategy datasets). We then filter per cluster (for the whole dataset), or per targeted partition followed by cluster (e.g., for half course data, we first filter the whole dataset to keep only the first half, after which we filter for one strategy cluster).

Script (8) shows how to answer RQ3. Again, this is just the script used for the halves data. 