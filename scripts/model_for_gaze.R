############
### Info ###
############
#
## Authors:  Dan Turner (dturner @ northwestern.edu)
#            Timo Roettger (timo.b.roettger @ gmail.com)
#
## Project: Eye tracking during prenuclear pitch accent comprehension
#
##          Modelling
#
##          Model the proportional preferences
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

# Load processed data (OpenSesame (+ Mouse Tracking) + Eye Tracking)
data <- read_csv("ret_processed_stage_2.csv")

# Join the landmarks and data
data <- data %>% 
  filter(!is.na(window)) %>% 
  mutate(window = factor(window, levels = c("early", "prenuclear", "nuclear", "adverb"))) %>% 
  # generate binary preference score
  mutate(subj_preference = ifelse(((Target_prop + ObjComp_prop) / 2) > ((SubjComp_prop + Distr_prop) / 2), 1, 0),
         obj_preference = ifelse(((Target_prop + SubjComp_prop) / 2) > ((ObjComp_prop + Distr_prop) / 2), 1, 0),
         # centralize eyetrial
         eyetrial.c = scale(eyetrial, scale = F))
         


#############
### Model ###
#############

# set priors
priors_gaze <- c(
  prior(student_t(5, 0, 2), class = Intercept),
  prior(student_t(5, 0, 2), class = b),
  prior(student_t(5, 0, 2), class = b),
  prior(student_t(4, 0, 2), class = sd),
  prior(student_t(4, 0, 2), class = sigma),
  prior(lkj(2), class = cor)
)

# model subject preference
xmdl <- brm(subj_preference ~ Condition * window * trial + 
              # specify maximal model for now
              (1 + Condition * window * trial | ID) + 
              (1 + Condition * window | Target_obj),
            family = "bernoulli", 
            chains = 4,
            iter = 2000,
            cores = 2,
            control = list(adapt_delta = 0.99),
            data = data)

# model object preference
xmdl <- brm(obj_preference ~ Condition * window * trial + 
              # specify maximal model for now
              (1 + Condition * window * trial | ID) + 
              (1 + Condition * window | Target_obj),
            family = "bernoulli", 
            chains = 4,
            iter = 2000,
            cores = 2,
            control = list(adapt_delta = 0.99),
            data = data)

