############
### Info ###
############
#
## Authors:  Dan Turner (dturner @ northwestern.edu)
#            Timo Roettger (timo.b.roettger @ gmail.com)
#
## Project: Eye tracking during prenuclear pitch accent comprehension
#
##          Stage 2
#
##          This file imports ret_processed.csv and enriches the data by calculating fixation durations,
##          proportions of duration per ROI, ROI image and role (i.e. target), and adds other columns to
##          facilitate analysis.
#
## Version: 9/18/2019
#
#############
### Setup ###
#############

## Load packages
library(tidyverse)
library(rstudioapi)

## Getting the path of your current open file
datapath = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(datapath))
setwd("../processed/")

## Load and merge the data with our acoustic landmarks
# Load processed data (OpenSesame (+ Mouse Tracking) + Eye Tracking)
data <- read_csv("ret_processed_stage_1.csv")

# Load acoustic landmarks
setwd("../data/")
landmarks <- read_csv("acoustic_landmarks.csv")

# Join the landmarks and data
data <- full_join(data, landmarks)

# Check our work
table(data$Condition, data$ID)

# Drop landmarks
rm(landmarks)

#####################
### Describe ROIs ###
#####################

## How long was the fixation?
data$fixDur <- data$entime - data$sttime

# Encode ROI location for intuitive matching later
data$roiLoc = NA
data$roiLoc[data$roi_1 == 1] <- "TL"
data$roiLoc[data$roi_2 == 1] <- "TR"
data$roiLoc[data$roi_3 == 1] <- "BL"
data$roiLoc[data$roi_4 == 1] <- "BR"

# Reduce data (from over 300 variables) and look only at test trials
data <- data %>% 
  select(eyetrial, ID, BL_Pic, BR_Pic, TL_Pic, TR_Pic, 
         Comp_obj, Comp_obj_pic, Comp_obj_pos, 
         Comp_subj, Comp_subj_pic, Comp_subj_pos,
         Condition, fixDur,First_obj, First_subj, First_pic, First_sound,
         Target_obj, Target_subj, Target_pos, sttime, entime,
         fixDur, prenuclear_onset, referent_onset, adverb_onset, roiLoc
  ) %>% 
  filter(Condition %in% c("CG", "GG", "GC"))

# Identify the role of each ROI
data$fixTarget <- ifelse(data$Target_pos == "TL_Pic" & data$roiLoc == "TL", 1, 
                         ifelse(data$Target_pos == "TR_Pic" & data$roiLoc == "TR", 1, 
                                ifelse(data$Target_pos == "BL_Pic" & data$roiLoc == "BL", 1, 
                                       ifelse(data$Target_pos == "BR_Pic" & data$roiLoc == "BR", 1, 0))))

data$fixSubjComp <- ifelse(data$Target_pos == "TL_Pic" & data$roiLoc == "TR", 1, 
                           ifelse(data$Target_pos == "TR_Pic" & data$roiLoc == "BR", 1, 
                                  ifelse(data$Target_pos == "BL_Pic" & data$roiLoc == "TL", 1, 
                                         ifelse(data$Target_pos == "BR_Pic" & data$roiLoc == "BL", 1, 0))))

data$fixObjComp <- ifelse(data$Target_pos == "TL_Pic" & data$roiLoc == "BL", 1, 
                          ifelse(data$Target_pos == "TR_Pic" & data$roiLoc == "TL", 1, 
                                 ifelse(data$Target_pos == "BL_Pic" & data$roiLoc == "BR", 1, 
                                        ifelse(data$Target_pos == "BR_Pic" & data$roiLoc == "TR", 1, 0))))

data$fixDist <- ifelse(data$Target_pos == "TL_Pic" & data$roiLoc == "BR", 1, 
                       ifelse(data$Target_pos == "TR_Pic" & data$roiLoc == "BL", 1, 
                              ifelse(data$Target_pos == "BL_Pic" & data$roiLoc == "TR", 1, 
                                     ifelse(data$Target_pos == "BR_Pic" & data$roiLoc == "TL", 1, 0))))

## Copy over the fixation duration based on ROI role
data$fixDurTarget <- ifelse(data$fixTarget == 1, data$fixDur, 0)
data$fixDurSubjComp <- ifelse(data$fixSubjComp == 1, data$fixDur, 0)
data$fixDurObjComp <- ifelse(data$fixObjComp == 1, data$fixDur, 0)
data$fixDurDist <- ifelse(data$fixDist == 1, data$fixDur, 0)

##############################
### Tabulate Fixation Data ###
##############################

## Group ROIs by trial and participant to calculate running fixation duration and proportion

# Lists for loops 
participants <- unique(data$ID)
windows <- c("early", "prenuclear", "nuclear", "adverb") # not used yet
rois <- c("TR", "TL", "BR", "BL") # not used yet

# Initialize variables
data$fixationEnd <- 0

## Calculate a running total for fixation duration within trial and participant, 'fixationEnd'
# By participant
for (participant in participants){
  # By trial
  for (trial in unique(data$eyetrial[data$ID == participant])){
    
    data$fixationEnd[data$ID == participant & data$eyetrial == trial] <- cumsum(data$fixDur[data$ID == participant & data$eyetrial == trial])
    
  } #/trial
} #/participant


## Categorize fixations according to time windows
data$window <- NA

# Early
data$window[data$fixationEnd <= data$prenuclear_onset] <- "early"

# Prenuclear
data$window[data$prenuclear_onset <= data$fixationEnd & data$fixationEnd <= data$referent_onset] <- "prenuclear"

# Nuclear
data$window[data$referent_onset <= data$fixationEnd & data$fixationEnd <= data$adverb_onset] <- "nuclear"

# Adverb
data$window[data$fixationEnd >= data$adverb_onset] <- "adverb"

## Calculate a sum of duration fxiations for each window by trial by participant

# Initialize cols
data$dur_early <- data$dur_prenuclear <- data$dur_nuclear <- data$dur_adverb <- 0

# By participant
for (participant in participants){
  # By trial
  for (trial in unique(data$eyetrial[data$ID == participant])){
    
    data$dur_early[data$ID == participant & data$eyetrial == trial] = sum(data$fixDur[data$eyetrial == trial & data$ID == participant & data$window == "early"])
    data$dur_prenuclear[data$ID == participant & data$eyetrial == trial] = sum(data$fixDur[data$eyetrial == trial & data$ID == participant & data$window == "prenuclear"])
    data$dur_nuclear[data$ID == participant & data$eyetrial == trial] = sum(data$fixDur[data$eyetrial == trial & data$ID == participant & data$window == "nuclear"])
    data$dur_adverb[data$ID == participant & data$eyetrial == trial] = sum(data$fixDur[data$eyetrial == trial & data$ID == participant & data$window == "adverb"])
    
  } #/trial
} #/participant

## Also calculate proportions for fixation duration by window

data$prop_early <- data$dur_early / (data$dur_early + data$dur_adverb + data$dur_nuclear + data$dur_prenuclear)
data$prop_prenuclear <- data$dur_prenuclear / (data$dur_early + data$dur_adverb + data$dur_nuclear + data$dur_prenuclear)
data$prop_nuclear <- data$dur_nuclear/ (data$dur_early + data$dur_adverb + data$dur_nuclear + data$dur_prenuclear)
data$prop_adverb <- data$dur_adverb / (data$dur_early + data$dur_adverb + data$dur_nuclear + data$dur_prenuclear)

#######################################
### Write the output to /processed/ ###
#######################################

# Write the full output
setwd("../processed/")
readr::write_csv(data, "ret_processed_stage_2.csv")

# Cleanup
rm(datapath, participant, participants, rois, trial, windows)

# End