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
## Version: 9/9/2019


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

# reduce data and look only at test trials
data <- xdata %>% 
  select(eyetrial, ID, BL_Pic, BR_Pic, TL_Pic, TR_Pic, 
         Comp_obj, Comp_obj_pic, Comp_obj_pos, 
         Comp_subj, Comp_subj_pic, Comp_subj_pos,
         Condition, fixDur, fixationSum,
         First_obj, First_subj, First_pic, First_sound,
         Target_obj, Target_subj, Target_pos, sttime, entime,
         topROI, fixDur, roi_1, roi_2, roi_3, roi_4,
         prenuclear_onset, referent_onset, adverb_onset,
         roi_1_sum, roi_2_sum, roi_3_sum, roi_4_sum
         ) %>% 
  filter(Condition %in% c("CG", "GG", "GC"))

# Encode ROI location for intuitive matching
data$roiLoc = NA
data$roiLoc[data$roi_1 == 1] <- "TL"
data$roiLoc[data$roi_2 == 1] <- "TR"
data$roiLoc[data$roi_3 == 1] <- "BL"
data$roiLoc[data$roi_4 == 1] <- "BR"

# define type of fixations
data$fixTarget <- ifelse(data$Target_pos == "tl_pic" & data$roiLoc == "TL", 1, 
                  ifelse(data$Target_pos == "tr_pic" & data$roiLoc == "TR", 1, 
                  ifelse(data$Target_pos == "bl_pic" & data$roiLoc == "BL", 1, 
                  ifelse(data$Target_pos == "br_pic" & data$roiLoc == "BR", 1, 0))))

data$fixSubjComp <- ifelse(data$Target_pos == "tl_pic" & data$roiLoc == "TR", 1, 
                    ifelse(data$Target_pos == "tr_pic" & data$roiLoc == "BR", 1, 
                    ifelse(data$Target_pos == "bl_pic" & data$roiLoc == "TL", 1, 
                    ifelse(data$Target_pos == "br_pic" & data$roiLoc == "BL", 1, 0))))

data$fixObjComp <- ifelse(data$Target_pos == "tl_pic" & data$roiLoc == "BL", 1, 
                   ifelse(data$Target_pos == "tr_pic" & data$roiLoc == "TL", 1, 
                   ifelse(data$Target_pos == "bl_pic" & data$roiLoc == "BR", 1, 
                   ifelse(data$Target_pos == "br_pic" & data$roiLoc == "TR", 1, 0))))

data$fixDist <- ifelse(data$Target_pos == "tl_pic" & data$roiLoc == "BR", 1, 
                   ifelse(data$Target_pos == "tr_pic" & data$roiLoc == "BL", 1, 
                   ifelse(data$Target_pos == "bl_pic" & data$roiLoc == "TR", 1, 
                   ifelse(data$Target_pos == "br_pic" & data$roiLoc == "TL", 1, 0))))

data$fixDurTarget <- ifelse(data$fixTarget == 1, data$fixDur, 0)
data$fixDurSubjComp <- ifelse(data$fixSubjComp == 1, data$fixDur, 0)
data$fixDurObjComp <- ifelse(data$fixObjComp == 1, data$fixDur, 0)
data$fixDurDist <- ifelse(data$fixDist == 1, data$fixDur, 0)

data <- data %>% 
  group_by(eyetrial) %>% 
  mutate(fixDurTarget.s = sum(fixDurTarget, na.rm = T)/fixationSum,
         fixDurObjComp.s = sum(fixDurObjComp, na.rm = T)/fixationSum,
         fixDurSubjComp.s = sum(fixDurSubjComp, na.rm = T)/fixationSum,
         fixDurDist.s = sum(fixDurDist, na.rm = T)/fixationSum) %>% 
  ungroup()

## Loop the vectors to accumulate end of fixation
data$fixationEnd <- 0

for (i in unique(data$eyetrial)) {
  for (j in 1:nrow(data[data$eyetrial == i,])) {
    #print(paste0(i,"&",j))
    if (j == 1) {
      data[data$eyetrial == i,]$fixationEnd[j] = 
        data[data$eyetrial == i,]$fixationEnd[j] + data[data$eyetrial == i,]$fixDur[j]
    } 
    else {
      data[data$eyetrial == i,]$fixationEnd[j] = 
        data[data$eyetrial == i,]$fixDur[j] + data[data$eyetrial == i,]$fixationEnd[j - 1]
      #print(data$fixationEnd[j])
    }
  }
}

# Now categorize fixations according to time windows
data$window <- NA
data$window <- ifelse(data$fixationEnd <= data$prenuclear_onset, "early",
                      ifelse(data$prenuclear_onset <= data$fixationEnd & data$fixationEnd <= data$referent_onset, "prenuclear",
                             ifelse(data$referent_onset <= data$fixationEnd & data$fixationEnd <= data$adverb_onset, "nuclear", "adverb")))


## Total by trial by window by target

# Loop all gazes
for (ostrial in unique(data$eyetrial)){
  
  # Go window by window and sum the fixation durations for each, matching by ostrial ()
  data$dur_early[data$eyetrial == ostrial] = sum(data$fixDur[data$eyetrial[data$window == "early"] == ostrial])
  data$dur_prenuclear[data$eyetrial == ostrial] = sum(data$fixDur[data$eyetrial[data$window == "prenuclear"] == ostrial])
  data$dur_nuclear[data$eyetrial == ostrial] = sum(data$fixDur[data$eyetrial[data$window == "nuclear"] == ostrial])
  data$dur_adverb[data$eyetrial == ostrial] = sum(data$fixDur[data$eyetrial[data$window == "adverb"] == ostrial])
}


# To do: Now downsample and assign fixation durations only within windows

# To do: Calculate relative fixations within a window to 
# (a) given vs. contrastive subject and
# (b) given vs. constrative object
        

