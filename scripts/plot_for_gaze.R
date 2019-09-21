############
### Info ###
############
#
## Authors:  Dan Turner (dturner @ northwestern.edu)
#            Timo Roettger (timo.b.roettger @ gmail.com)
#
## Project: Eye tracking during prenuclear pitch accent comprehension
#
##          Plotting
#
##          This file plots the fixation proportion.
#
## Version: 9/19/2019


#############
### Setup ###
#############

library(tidyverse)
library(ggbeeswarm)
library(rstudioapi)
# library(ggpubr)


## Getting the path of your current open file
datapath = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(datapath))
setwd("../processed/")

## Load and merge the data with our acoustic landmarks
# Load processed data
data <- read_csv("ret_processed_stage_2.csv")

# Load acoustic landmarks
setwd("../data/")
landmarks <- read_csv("acoustic_landmarks.csv")

# Join the landmarks and data
data <- full_join(data, landmarks) %>% 
  filter(!is.na(window))

# Order factor levels of window
data$window <- factor(data$window, levels = c("early", "prenuclear", "nuclear", "adverb"))

# Gather proportional values
data <- data %>% 
  gather(response, proportion, 8:11)


#################
### Aggregate ###
#################

# aggregate proportions (overall)
xagg <- data %>% 
  group_by(Condition, window, response) %>% 
  summarise(prop = mean(proportion, na.rm = T))

# aggregate proportions (per subject)
xagg_subj <- data %>% 
  group_by(Condition, window, ID, response) %>% 
  summarise(prop = mean(proportion, na.rm = T))

# aggregate over trials
xagg_trials <- data %>% 
  group_by(Condition, window, response, eyetrial) %>% 
  summarise(prop = mean(proportion, na.rm = T))

############
### Plot ###
############

# Plot aggregated fixations 
ggplot(data = xagg_subj, aes(x = window, y = prop, color = response, fill = response)) +
  geom_line(aes(group = interaction(ID, response)),
            alpha = 0.2, size = 1) +
  geom_line(data = xagg, aes(group = interaction(response)),
            size = 2) +
  geom_point(alpha = 0.2, size = 2) +
  geom_point(data = xagg, 
             size = 3, pch = 21, stroke = 1, color = "black") +
  facet_grid(~ Condition)

# Plot fixations as developing over time
ggplot(data = data, aes(x = eyetrial, y = proportion, color = response, fill = response)) +
  geom_line(aes(group = interaction(ID, response)), alpha = 0.1) +
  geom_smooth(data = xagg_trials, aes(y = prop, group = response)) +
  facet_grid(window ~ Condition)
              



