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

fixSum <- data$dur_early + data$dur_adverb + data$dur_nuclear + data$dur_prenuclear

data$prop_early <- data$dur_early / fixSum
data$prop_prenuclear <- data$dur_prenuclear / fixSum
data$prop_nuclear <- data$dur_nuclear/ fixSum
data$prop_adverb <- data$dur_adverb / fixSum

## Create a new data structure for proportion of fixation by roi by trial
data.roi <- setNames(data.frame(matrix(ncol = 2, nrow = length(unique(data$ID))*96 )), c("ID", "eyetrial"))

row = 1 # Row counter

# By participant
for (participant in participants){
  # By trial
  for (trial in unique(data$eyetrial[data$ID == participant])){
    
    # Add trial and participants to data.roi
    data.roi[row, 1] <- participant
    data.roi[row, 2] <- trial
    
    ## ROI by ROI, sum fixations by window
    # TL
    data.roi$TL_early[row] <- sum(na.omit(data$fixDur[data$ID == participant & data$eyetrial == trial & data$roiLoc == "TL" & data$window == "early"]))
    # Prenuclear
    data.roi$TL_prenuclear[row] <- sum(na.omit(data$fixDur[data$ID == participant & data$eyetrial == trial & data$roiLoc == "TL" & data$window == "prenuclear"]))
    #  Nuclear
    data.roi$TL_nuclear[row] <- sum(na.omit(data$fixDur[data$ID == participant & data$eyetrial == trial & data$roiLoc == "TL" & data$window == "nuclear"]))
    # Adverb
    data.roi$TL_adverb[row] <- sum(na.omit(data$fixDur[data$ID == participant & data$eyetrial == trial & data$roiLoc == "TL" & data$window == "adverb"]))
    
    # TR
    data.roi$TR_early[row] <- sum(na.omit(data$fixDur[data$ID == participant & data$eyetrial == trial & data$roiLoc == "TR" & data$window == "early"]))
    # Prenuclear
    data.roi$TR_prenuclear[row] <- sum(na.omit(data$fixDur[data$ID == participant & data$eyetrial == trial & data$roiLoc == "TR" & data$window == "prenuclear"]))
    #  Nuclear
    data.roi$TR_nuclear[row] <- sum(na.omit(data$fixDur[data$ID == participant & data$eyetrial == trial & data$roiLoc == "TR" & data$window == "nuclear"]))
    # Adverb
    data.roi$TR_adverb[row] <- sum(na.omit(data$fixDur[data$ID == participant & data$eyetrial == trial & data$roiLoc == "TR" & data$window == "adverb"]))
    
    # BR
    data.roi$BR_early[row] <- sum(na.omit(data$fixDur[data$ID == participant & data$eyetrial == trial & data$roiLoc == "BR" & data$window == "early"]))
    # Prenuclear
    data.roi$BR_prenuclear[row] <- sum(na.omit(data$fixDur[data$ID == participant & data$eyetrial == trial & data$roiLoc == "BR" & data$window == "prenuclear"]))
    #  Nuclear
    data.roi$BR_nuclear[row] <- sum(na.omit(data$fixDur[data$ID == participant & data$eyetrial == trial & data$roiLoc == "BR" & data$window == "nuclear"]))
    # Adverb
    data.roi$BR_adverb[row] <- sum(na.omit(data$fixDur[data$ID == participant & data$eyetrial == trial & data$roiLoc == "BR" & data$window == "adverb"]))
    
    # BL
    data.roi$BL_early[row] <- sum(na.omit(data$fixDur[data$ID == participant & data$eyetrial == trial & data$roiLoc == "BL" & data$window == "early"]))
    # Prenuclear
    data.roi$BL_prenuclear[row] <- sum(na.omit(data$fixDur[data$ID == participant & data$eyetrial == trial & data$roiLoc == "BL" & data$window == "prenuclear"]))
    #  Nuclear
    data.roi$BL_nuclear[row] <- sum(na.omit(data$fixDur[data$ID == participant & data$eyetrial == trial & data$roiLoc == "BL" & data$window == "nuclear"]))
    # Adverb
    data.roi$BL_adverb[row] <- sum(na.omit(data$fixDur[data$ID == participant & data$eyetrial == trial & data$roiLoc == "BL" & data$window == "adverb"]))
    
    # Sum of all fixations in the trial
    fixSum = sum(data.roi[row,3:18])
    
    ## Convert the above sums to proportions
    # TL
    data.roi$TL_early_prop[row] = data.roi$TL_early[row] / fixSum
    data.roi$TL_prenuclear_prop[row] = data.roi$TL_prenuclear[row] / fixSum
    data.roi$TL_nuclear_prop[row] = data.roi$TL_nuclear[row] / fixSum
    data.roi$TL_adverb_prop[row] = data.roi$TL_adverb[row] / fixSum

    # TR
    data.roi$TR_early_prop[row] = data.roi$TR_early[row] / fixSum
    data.roi$TR_prenuclear_prop[row] = data.roi$TR_prenuclear[row] / fixSum
    data.roi$TR_nuclear_prop[row] = data.roi$TR_nuclear[row] / fixSum
    data.roi$TR_adverb_prop[row] = data.roi$TR_adverb[row] / fixSum
    
    # BR
    data.roi$BR_early_prop[row] = data.roi$BR_early[row] / fixSum
    data.roi$BR_prenuclear_prop[row] = data.roi$BR_prenuclear[row] / fixSum
    data.roi$BR_nuclear_prop[row] = data.roi$BR_nuclear[row] / fixSum
    data.roi$BR_adverb_prop[row] = data.roi$BR_adverb[row] / fixSum
    
    # BL
    data.roi$BL_early_prop[row] = data.roi$BL_early[row] / fixSum
    data.roi$BL_prenuclear_prop[row] = data.roi$BL_prenuclear[row] / fixSum
    data.roi$BL_nuclear_prop[row] = data.roi$BL_nuclear[row] / fixSum
    data.roi$BL_adverb_prop[row] = data.roi$BL_adverb[row] / fixSum
    
    ## Check our work
    #data.roi$fixSum[row] <- fixSum
    #data.roi$proSum[row] <- sum(data.roi[row,19:34])
    
    # Next row
    row = row + 1
  } #/trial
} #/participant

# todo: join the windows with the main dataset by trial and participant 

#######################################
### Write the output to /processed/ ###
#######################################

# Write the full output
setwd("../processed/")
readr::write_csv(data, "ret_processed_stage_2.csv")
readr::write_csv(data.roi, "ret_processed_roi-windows.csv")

# Cleanup
rm(datapath, participant, participants, rois, trial, windows)

# End
