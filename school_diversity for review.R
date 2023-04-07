# DataViz Review 

# Load data
school_diversity <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-24/school_diversity.csv")
# Data was taken from the 'Tidytuesday' 
# challenge named 'School diversity' published on 9-24-2019 
# (link: https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-09-24).

# The dataset includes data on the racial diversity of US schools in 1994-1995 and 2016-2017. 
# Diversity was defined by the proportion of students in the dominant racial group. 
# In diverse schools fewer than 75 percent of students are the same race. 
# In Undiverse schools 75 to 90 percent of students are the same race. 
# In extremely undiverse schools one racial group constitutes more than 90 percent of students. 

# Load region data
regiondata <- read.csv(header = TRUE, text = "state,region
AK,West
AL,South
AR,South
AZ,West
CA,West
CO,West
CT,Northeast
DC,South
DE,South
FL,South
GA,South
HI,West
IA,Midwest
ID,West
IL,Midwest
IN,Midwest
KS,Midwest
KY,South
LA,South
MA,Northeast
MD,South
ME,Northeast
MI,Midwest
MN,Midwest
MO,Midwest
MS,South
MT,West
NC,South
ND,Midwest
NE,Midwest
NH,Northeast
NJ,Northeast
NM,West
NV,West
NY,Northeast
OH,Midwest
OK,South
OR,West
PA,Northeast
RI,Northeast
SC,South
SD,Midwest
TN,South
TX,South
UT,West
VA,South
VT,Northeast
WA,West
WI,Midwest
WV,South
WY,West")

# Subset the data to only include the years 1994-1995 and 2016-2017
school_diversity_sub <- subset(school_diversity, SCHOOL_YEAR %in% c("1994-1995", "2016-2017"))

# Calculate the average diversity for each district in 1994-1995
avg_diversity_1994 <- aggregate(diverse ~ ST, data = subset(school_diversity_sub, SCHOOL_YEAR == "1994-1995"), FUN = function(x) mean(x == "Diverse"))

# Calculate the average diversity for each district in 2016-2017
avg_diversity_2016 <- aggregate(diverse ~ ST, data = subset(school_diversity_sub, SCHOOL_YEAR == "2016-2017"), FUN = function(x) mean(x == "Diverse"))

# Merge the two data frames and calculate the difference in diversity
library(dplyr)
diversity_diff <- merge(avg_diversity_1994, avg_diversity_2016, by = "ST", suffixes = c("_1994", "_2016"))
diversity_diff <- left_join(diversity_diff, regiondata, by = c("ST" = "state"))
diversity_diff <- diversity_diff %>%
  mutate(diff = diverse_2016 - diverse_1994)

# I calculated the average diversity of schools in the same state in 1994-1995 and 2016-2017, 
# then calculated the difference between these years that expresses the change 
# in diversity over time in each state (these differences appear on the X-axis in the graph). 
# In order to provide additional information regarding the data, 
# each state is colored according to its location in the country. 
# Therefore, it is possible to observe changes in diversity not only in relation to states, 
# but also regions.

# Plot the results - Graph 1
library(ggplot2)
ggplot(diversity_diff, aes(x = ST, y = diff, fill = region)) +
  geom_bar(stat = "identity") +
  coord_flip() + # switch the x and y axes
  scale_fill_brewer(palette = "Accent") +
  labs(x = "State", y = "Difference in diversity", fill = "Region") +
  ggtitle("Change in Proportion of diversity (1994-1995 to 2016-2017)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10))

# Based on the peer review, the following changes were made:
# To make the X-axis more understandable, I changed the values displayed on it. 
# Instead of showing the change in diversity ratings from 1994-1995 to 2017-2026, 
# I chose to show the diversity ratings for these years. 
# The distinction between the years is made by having the diversity ratings of 1994-1995 in bold, 
# while the diversity ratings of 2017-2026 are not. 
# As a result, it is apparent that diversity is improving in general. #
# Furthermore, by examining each state separately, 
# it is possible to determine how much diversity has improved in each state. 
# In addition, I grouped the states by region to make it easier to identify diversity changes by region.

# Plot the results - Graph 2
library(ggplot2)
ggplot(diversity_diff, aes(x = reorder(ST, region, FUN = median), y = diverse_2016, fill = region)) +
  ggtitle("Change in Diversity in Schools from 1994-1995 (bold colors) to 2016-2017 (unbold colors) by states and regions") +
  geom_bar(stat = "identity", alpha = 0.5) +
  geom_bar(aes(y = diverse_1994), stat = "identity", colour = "gray", size = 0.1) +
  coord_flip() + # switch the x and y axes
  scale_fill_brewer(palette = "Accent") +
  labs(x = "State", y = "diversity rates (1994-1995 & 2016-2017)", fill = "Region") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        axis.text.y = element_text(size = 10))
