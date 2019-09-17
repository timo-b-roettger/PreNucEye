############
### Info ###
############
#
## Author:  Timo Roettger (timo.b.roettger @ gmail.com)
#
## Project: Eye tracking during prenuclear pitch accent comprehension
#
##        DESCRIPTION
#
## Version: 9/10/2019


#############
### Setup ###
#############

## Load main package (everything else called inline)
library(tidyverse)
library(rstudioapi)

# Getting the path of your current open file
datapath = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(datapath))
setwd("../processed/")

# Load processed data (OpenSesame (+ Mouse Tracking) + Eye Tracking)
xdata <- read_csv("ret_processed.csv")

# Load acoustic landmarks
setwd("../data/")
landmarks <- read_csv("acoustic_landmarks.csv")

# Join the landmarks and data
xdata <- full_join(xdata, landmarks)

# Check our work
table(xdata$Condition, xdata$ID)

# reduce data and look only at test trials
data <- xdata %>% 
  select(eyetrial, ID, BL_Pic, BR_Pic, TL_Pic, TR_Pic, 
         Comp_obj, Comp_obj_pic, Comp_obj_pos, 
         Comp_subj, Comp_subj_pic, Comp_subj_pos,
         Condition, fixDur,First_obj, First_subj, First_pic, First_sound,
         Target_obj, Target_subj, Target_pos, sttime, entime,
         fixDur, roi_1, roi_2, roi_3, roi_4,
         prenuclear_onset, referent_onset, adverb_onset, roiLoc
         ) %>% 
  filter(Condition %in% c("CG", "GG", "GC"))

# define type of fixations
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

data$fixDist <-    ifelse(data$Target_pos == "TL_Pic" & data$roiLoc == "BR", 1, 
                   ifelse(data$Target_pos == "BL_Pic" & data$roiLoc == "BL", 1, 
                   ifelse(data$Target_pos == "BL_Pic" & data$roiLoc == "TR", 1, 
                   ifelse(data$Target_pos == "BR_Pic" & data$roiLoc == "TL", 1, 0))))

data$fixDurTarget <- ifelse(data$fixTarget == 1, data$fixDur, 0)
data$fixDurSubjComp <- ifelse(data$fixSubjComp == 1, data$fixDur, 0)
data$fixDurObjComp <- ifelse(data$fixObjComp == 1, data$fixDur, 0)
data$fixDurDist <- ifelse(data$fixDist == 1, data$fixDur, 0)


binned <- data %>%
  # Assign bin numbers
  group_by(eyetrial, roiLoc) %>%
  mutate(Bin = assign_bins(Time, bin_width = 20)) %>%
  # Add up the looks to each AOI in each bin.
  group_by(ID, Condition, Bin) %>%
  summarise(
    # Round to nearest 10ms (effectively nearest 50ms)
    Time = round(mean(Time), -1),
    ToDistractor = sum(Distractor),
    ToTarget = sum(Target)) %>%
  ungroup


# make factor
data$ID <- as.factor(data$ID)

## Loop the vectors to accumulate end of fixation
data$fixationEnd <- 0

# Looping participants, then eyetrials maximal duration
# works now TR

for (p in unique(data$ID)) {
  for (i in unique(data$eyetrial[data$ID == p])) {
    for (j in 1:nrow(data[data$eyetrial == i & data$ID == p,])) {
       print(paste0(p, "&", i,"&",j))
       if (j == 1) {
         #data[data$eyetrial[data$ID == p] == i,]$fixationEnd[j] =
         #data[data$eyetrial[data$ID == p] == i,]$fixationEnd[j] + data[data$eyetrial[data$ID == p] == i,]$fixDur[j]
         data[data$eyetrial == i & data$ID == p,]$fixationEnd[j] =
         data[data$eyetrial == i & data$ID == p,]$fixDur[j] + data[data$eyetrial == i & data$ID == p,]$fixationEnd[j]
         print(data$fixationEnd[j])
       }
       else {
           #data[data$eyetrial[data$ID == p] == i,]$fixationEnd[j] =
           #data[data$eyetrial[data$ID == p] == i,]$fixDur[j] + data[data$eyetrial[data$ID == p] == i,]$fixationEnd[j - 1]
           data[data$eyetrial == i & data$ID == p,]$fixationEnd[j] =
           data[data$eyetrial == i & data$ID == p,]$fixDur[j] + data[data$eyetrial == i & data$ID == p,]$fixationEnd[j - 1]
           print(data$fixationEnd[j])
       }
     }
  }
}







# Now categorize fixations according to time windows
data$prenuc_window <- NA
data$prenuc_window <- ifelse(data$fixationEnd <= data$prenuclear_onset, "early",
                      ifelse(data$prenuclear_onset <= data$fixationEnd & data$fixationEnd <= data$referent_onset, "prenuclear",
                             ifelse(data$referent_onset <= data$fixationEnd & data$fixationEnd <= data$adverb_onset, "nuclear", "adverb")))



data$window <- NA
data$window <- ifelse(data$fixationEnd <= data$prenuclear_onset, "early",
                      ifelse(data$prenuclear_onset <= data$fixationEnd & data$fixationEnd <= data$referent_onset, "prenuclear",
                             ifelse(data$referent_onset <= data$fixationEnd & data$fixationEnd <= data$adverb_onset, "nuclear", "adverb")))


## Total by trial by window by target

# Initialize cols
data$dur_early <- data$dur_prenuclear <- data$dur_nuclear <- data$dur_adverb <- 0

# Loop all gazes
for (p in unique(data$ID)) {
  for (ostrial in unique(data$eyetrial[data$ID == p])) {
  
    # Go window by window and sum the fixation durations for each, matching by ostrial ()
    data[data$ID == p & data$eyetrial == ostrial,]$dur_early = sum(data$fixDur[data$eyetrial[data$window == "early"] == ostrial])
    data$dur_prenuclear[data$eyetrial == ostrial] = sum(data$fixDur[data$eyetrial[data$window == "prenuclear"] == ostrial])
    data$dur_nuclear[data$eyetrial == ostrial] = sum(data$fixDur[data$eyetrial[data$window == "nuclear"] == ostrial])
    data$dur_adverb[data$eyetrial == ostrial] = sum(data$fixDur[data$eyetrial[data$window == "adverb"] == ostrial])
  }
}

# To do: Now downsample and assign fixation durations only within windows

# To do: Calculate relative fixations within a window to 
# (a) given vs. contrastive subject and
# (b) given vs. constrative object
        

