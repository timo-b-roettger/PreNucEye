############
### Info ###
############
#
## Author:  Timo Roettger (timo.b.roettger @ gmail.com)
#
## Project: Eye tracking during prenuclear pitch accent comprehension
#
##        DESCRIPTION bla
#
## Version: 8/25/2019


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

# load in data
xdata <- read_csv("ret_processed.csv")

# load in acoustic landmarks
setwd("../data/")
landmarks <- read_csv("acoustic_landmarks.csv")

# join

xdata <- full_join(xdata, landmarks)

# reduce data and look only at test trials
data <- xdata %>% 
  select(eyetrial, ID, BL_Pic, BR_Pic, TL_Pic, TR_Pic, 
         Comp_obj, Comp_obj_pic, Comp_obj_pos, 
         Comp_subj, Comp_subj_pic, Comp_subj_pos,
         Condition, fixDur, fixationSum,
         First_obj, First_subj, First_pic, First_sound,
         Target_obj, Target_subj, Target_pos, 
         topROI, fixDur, roiLoc,
         prenuclear_onset, referent_onset, adverb_onset,
         roi_1_sum, roi_2_sum, roi_3_sum, roi_4_sum
         ) %>% 
  filter(Condition %in% c("CG", "GG", "GC")) %>% 
  drop_na(roiLoc)


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

# Now categorize fications according to time windows
data$window <- ifelse(data$fixationEnd <= data$prenuclear_onset, "early",
               ifelse(data$prenuclear_onset <= data$fixationEnd & data$fixationEnd <= data$referent_onset, "prenuclear",
               ifelse(data$referent_onset <= data$fixationEnd & data$fixationEnd <= data$adverb_onset, "nuclear", "adverb")))

# To do: Now downsample and assign fixation durations only within windows

# To do: Calculate relative fixations within a window to 
# (a) given vs. contrastive subject and
# (b) given vs. constrative object
        

