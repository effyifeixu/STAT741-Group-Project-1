library(tidyverse)
library(haven)
library(ggplot2)

# Load the data
asthma_df <- read_dta("data/family asthma p1 data version2.dta")

# Reshape the data
asthma_tidy_df <- asthma_df %>%
  pivot_longer(
    cols = matches("\\d+$"),           
    names_to = c(".value", "visit"),  
    names_pattern = "([a-z]+)(\\d+)"   
  ) %>%
  select(family, id, visit, everything())

# Print out the columns
columns = colnames(asthma_tidy_df)
columns

# See what is in each of the columns
summary(asthma_tidy_df)

#Research Question Drives what I should look at
#Question 1 has to do with if asthmatics suffer steeper rates of decline or lower levels of lung function independent of smoking?
#First, I think I should visualize and record asthma and smoking data.

#First, how many unique participants do I have
num_unique_participants <- n_distinct(asthma_tidy_df$id)
cat("The number of unique participants is", num_unique_participants, "\n")

# Ok, now lets take a look at the asthma column
ggplot(asthma_tidy_df, aes(x = factor(asthma))) +
  geom_bar(aes(y = after_stat(prop), group = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Proportion of Participants by Asthma Status",
    x = "Group", 
    y = "Proportion"
    )

# Ok, now, let's take a look at the smoking data
ggplot(asthma_tidy_df, aes(x = factor(smkever))) +
  geom_bar(aes(y = after_stat(prop), group = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Proportion of Participants by Smoker Status",
    x = "Group", 
    y = "Proportion"
  )




