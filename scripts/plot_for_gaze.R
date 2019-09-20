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
library(ggpubr)
library(brms)

## Getting the path of your current open file
datapath = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(datapath))
setwd("../processed/")

## Load and merge the data with our acoustic landmarks
# Load processed data (OpenSesame (+ Mouse Tracking) + Eye Tracking)
data <- read_csv("ret_processed_stage_2.csv")

# Load acoustic landmarks
setwd("../data/")
landmarks <- read_csv("acoustic_landmarks.csv")

# Join the landmarks and data
data <- full_join(data, landmarks)

#################
### Aggregate ###
#################

# aggregate proportions (overall)
xagg <- data %>% 
  group_by(Condition, window) %>% 
  summarise(Target_prop == mean(Target_prop, na.rm = T),
            SubjComp_prop == mean(SubjComp_prop, na.rm = T),
            ObjComp_prop == mean(ObjComp_prop, na.rm = T),
            Distr_prop == mean(Distr_prop, na.rm = T))

xagg_subj <- data %>% 
  group_by(Condition, window, ID) %>% 
  summarise(Target_prop == mean(Target_prop, na.rm = T),
            SubjComp_prop == mean(SubjComp_prop, na.rm = T),
            ObjComp_prop == mean(ObjComp_prop, na.rm = T),
            Distr_prop == mean(Distr_prop, na.rm = T))





