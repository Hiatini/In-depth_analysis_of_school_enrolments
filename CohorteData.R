# Hiatini TEKOHUOTETUA

################################  1.QUESTION   ################################

## Using open data, we will prepare several graphs to illustrate various issues.
## The first question is to determine the number of students in secondary education in 2020, by academy.

###############################################################################

# Load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# Load data from the link
url <- "https://data.education.gouv.fr/explore/dataset/fr-en-lycee_gt-effectifs-niveau-sexe-lv/download/?format=csv&timezone=Europe/Berlin&use_labels_for_header=true"
data <- read.csv2(url, sep =";", stringsAsFactors = TRUE)
View(data)

# Select relevant columns
selected_data <- data %>%
  select(Académie, Rentrée.scolaire, X2ndes.GT, X2ndes.STHR, X2ndes.TMD, X2ndes.BT)

# Filter data for the year 2020
data_2020 <- selected_data %>%
  filter(Rentrée.scolaire == 2020)

# Calculate the number of students for each academy
students_per_academy <- data_2020 %>%
  group_by(Académie) %>%
  summarize(Total_Students = sum(X2ndes.GT, X2ndes.STHR, X2ndes.TMD, X2ndes.BT))

# Arrange the table in descending order of students
students_per_academy <- students_per_academy %>%
  arrange(desc(Total_Students))

# Display the resulting table
View(students_per_academy)

#################################  RESULTS   #################################

# In our top 5, we can observe that Versailles is in first place with 56,887 students in second grade, followed by Créteil with 41,697 students,
# Lille with 33,935 students, Nantes with 31,023 students, and Lyon with 29,457 students. To get a better overview, let's create a histogram illustrating
# our Top Three academies with the most students in secondary education in 2020.

################################################################################

# Take the top three rows
top_three <- head(students_per_academy, 3)

# Create the graph
top_three_graph <- ggplot(top_three, aes(x = reorder(Académie, Total_Students), y = Total_Students)) +
  geom_text(aes(label = Total_Students), vjust = -0.5, size = 3) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Top 3 academies with the most students in secondary education in 2020",
       x = "Academies",
       y = "Students") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(color = "#2E3B4E"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, color = "#2E3B4E"))

# Display the graph
print(top_three_graph)

#################################  RESULTS   #################################
# The distinction between Versailles, Créteil, and Lille is clear. Versailles maintains the first position with the highest number of students in secondary education in 2020.
################################################################################

                                ###############

################################  2.QUESTION   ################################

## Another question we explored concerned the distribution of GT 2nd-grade students according to the LV1 chosen at the start of the 2021 school year.

###############################################################################

# Select relevant columns and rename them
selected_data <- data %>%
  select(Rentrée.scolaire,
         "German" = "X2ndes.GT.LV1.allemand",
         "English" = "X2ndes.GT.LV1.anglais",
         "Spanish" = "X2ndes.GT.LV1.espagnol",
         "Other_languages" = "X2ndes.GT.LV1.autres.langues")

# Filter data for the year 2021
data_2021 <- selected_data %>%
  filter(Rentrée.scolaire == 2021)

# Pivot the data
data_2021_long <- data_2021 %>%
  pivot_longer(
    cols = c(German, English, Spanish, Other_languages),
    names_to = "LV1",
    values_to = "Student_Count"
  )

# Group rows corresponding to the same language
data_2021_long <- data_2021_long %>%
  group_by(LV1) %>%
  summarize(Student_Count = sum(Student_Count)) %>%
  arrange(desc(Student_Count))

View(data_2021_long)

#################################  RESULTS   #################################

# According to our results, English is the most commonly chosen LV1, with over 550,000 students in 2nd grade General and Technological.
# In second place, more than 11,000 students in 2nd grade study German, while 4,499 students opt for Spanish. Finally, in last place,
# we have over 2,000 students studying languages other than those mentioned.

################################################################################

################################  3.QUESTION   ################################

## The last question we want to answer concerns the academies that recorded an increase in the number of students between the
## 2020 and 2021 school years. Which academies experienced an increase in the number of students between these two periods?

###############################################################################

# Filter data
data_2020 <- data %>% filter(Rentrée.scolaire == 2020)
data_2021 <- data %>% filter(Rentrée.scolaire == 2021)

# Calculate the number of students per academy
students_2020 <- data_2020 %>%
  group_by(Académie) %>%
  summarize(Students_2020 = sum(Nombre.d.élèves))

students_2021 <- data_2021 %>%
  group_by(Académie) %>%
  summarize(Students_2021 = sum(Nombre.d.élèves))

# Calculate the difference in student numbers and sort from largest to smallest
difference_students <- inner_join(students_2020, students_2021, by = "Académie") %>%
  mutate(Difference = Students_2021 - Students_2020) %>%
  arrange(desc(Difference))

# Create the final table
final_table <- difference_students %>%
  select(Académie, Difference)

# Calculate the percentage of students gained
final_table <- difference_students %>%
  select(Académie, Difference)

# Display the new final table
View(final_table)

# To highlight our top three graphs, we will use a histogram.

# Take the top three rows
top_three <- head(difference_students, 3)

# Create the graph
top_three_graph <- ggplot(top_three, aes(x = reorder(Académie, Difference), y = Difference)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs
