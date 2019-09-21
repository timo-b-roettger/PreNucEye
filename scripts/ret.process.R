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
## Version: 9/19/2019
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

# Add 200ms to landmarks as saccade initiation time
landmarks <- landmarks %>% 
  mutate(prenuclear_onset = prenuclear_onset + 200,
         referent_onset = referent_onset + 200,
         adverb_onset = adverb_onset + 200)

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

## Separate fixation duration by acoustic landmark into windows
# Loop all gazes, grouping by participant and trial

# Initialize the columns for windows
data$window_adverb <- data$window_nuclear <- data$window_prenuc <- data$window_early <- 0

# By participant
for (participant in participants){
  # By trial
  for (trial in unique(data$eyetrial[data$ID == participant])){
    # By gaze
    trialgazes = which(data$ID == participant & data$eyetrial == trial)
    for (gaze in trialgazes){
      
      # Retreive acoustic onsets for this trial
      lnmk_prenuc = data$prenuclear_onset[gaze]
      lnmk_referent = data$referent_onset[gaze]
      lnmk_adverb = data$adverb_onset[gaze]
      
      # Some shortcut variables to make the following more readable
      fixStart = data$fixationEnd[gaze] - data$fixDur[gaze]
      fixEnd = data$fixationEnd[gaze]
      fixDur = data$fixDur[gaze]
      
      ## Early OG  [fixations that start and end before the first landmark]
      if (fixStart <= lnmk_prenuc && fixEnd <= lnmk_prenuc) {
        
        # Add the whole fixation to the window
        data$window_early[gaze] = data$window_early[gaze] + fixDur
        
        # Early OF [fixations start before BUT end after the first landmark]
      } else if (fixStart <= lnmk_prenuc && fixEnd >= lnmk_prenuc && fixStart <= lnmk_referent) {
        
        # Add the duration from the start of the fixation to the first landmark to the early window
        data$window_early[gaze] =  data$window_early[gaze] + (lnmk_prenuc - fixStart)
        
        # Add the durtation from the first landmark to the end of the dixation to the prenuc window
        data$window_prenuc[gaze] = fixEnd - lnmk_prenuc
        
      ## Prenuclear OG [fixations that start after the first landmark and end before the second landmark]
      } else if (fixStart >= lnmk_prenuc && fixEnd <= lnmk_referent) {
        
        # Add the whole fixation to the window
        data$window_prenuc[gaze] = data$window_prenuc[gaze] + fixDur
      
      ## Prenuclear OF [fixations that start after the first landmark BUT end after the second landmark] 
      } else if (fixStart >= lnmk_prenuc && fixEnd >= lnmk_referent && fixStart <= lnmk_referent) {
        
        # Add the duration from the fixation start to the second landmark to the first window
        data$window_prenuc[gaze] = data$window_prenuc[gaze] + (lnmk_referent - fixStart)
        
        # Add the later part of the fixation to the later window
        data$window_nuclear[gaze] = fixEnd - lnmk_referent
        
      ## Nuclear OG [fixations that start after the second landmark and end before the third landmark] 
      } else if (fixStart >= lnmk_referent && fixEnd <= lnmk_adverb ) {
        
        # Add the whole fixation to the window
        data$window_nuclear[gaze] = data$window_nuclear[gaze] + fixDur
        
      ## Nuclear OF [fixations the start after the second landmark BUT end after the third landmark]
      } else if (fixStart >= lnmk_referent && fixEnd >= lnmk_adverb && fixStart <= lnmk_adverb) {

        # Add the earlier part of the fixation to the earlier window *problem here*
        data$window_nuclear[gaze] = data$window_nuclear[gaze] + (lnmk_adverb - fixStart)

        # Add the later part of the fixation to the later window
        data$window_adverb[gaze] = fixEnd - lnmk_adverb
        
      ## Adverb OG [fixations start after the third landmark]
      } else if (fixStart >= lnmk_adverb) {
        
        # Add the whole fixation to the window
        data$window_adverb[gaze] = data$window_adverb[gaze] + fixDur
        
      } # end conditionals
      
    } #/gaze
  } #/trial
} #/participant

## Validate the above
table(data$window_early + data$window_prenuc + data$window_nuclear + data$window_adverb == data$fixDur)

## Create a new dataframe for proportion of fixation by roi, summarized by trial (not gaze/fixation)
data.roi <- setNames(data.frame(matrix(ncol = 2, nrow = length(unique(data$ID))*96 )), c("ID", "eyetrial"))

row = 1 # Row counter

# By participant
for (participant in participants){
  # By trial
  for (trial in unique(data$eyetrial[data$ID == participant])){
    
    # Add trial and participants to data.roi
    data.roi[row, 1] <- participant
    data.roi[row, 2] <- trial
    
    # Total fixation time for this trial (across all windows and ROIs)
    fixSum = sum(data$fixDur[data$eyetrial == trial & data$ID == participant])
    
    # TR: Total fixation time for this trial for each window
    fixSum_early = sum(data$window_early[data$eyetrial == trial & data$ID == participant & !is.na(data$roiLoc)])
    fixSum_prenuc = sum(data$window_prenuc[data$eyetrial == trial & data$ID == participant & !is.na(data$roiLoc)])
    fixSum_nuclear = sum(data$window_nuclear[data$eyetrial == trial & data$ID == participant & !is.na(data$roiLoc)])
    fixSum_adverb = sum(data$window_adverb[data$eyetrial == trial & data$ID == participant & !is.na(data$roiLoc)])
    
    # By ROI
    # TL
    data.roi$TL_early[row] = sum(na.omit(data$window_early[data$roiLoc == "TL" & data$eyetrial == trial & data$ID == participant])) / fixSum_early
    data.roi$TL_prenuclear[row] = sum(na.omit(data$window_prenuc[data$roiLoc == "TL" & data$eyetrial == trial & data$ID == participant])) / fixSum_prenuc
    data.roi$TL_nuclear[row] = sum(na.omit(data$window_nuclear[data$roiLoc == "TL" & data$eyetrial == trial & data$ID == participant])) / fixSum_nuclear
    data.roi$TL_adverb[row] = sum(na.omit(data$window_adverb[data$roiLoc == "TL" & data$eyetrial == trial & data$ID == participant])) / fixSum_adverb
    
    # TR
    data.roi$TR_early[row] = sum(na.omit(data$window_early[data$roiLoc == "TR" & data$eyetrial == trial & data$ID == participant])) / fixSum_early
    data.roi$TR_prenuclear[row] = sum(na.omit(data$window_prenuc[data$roiLoc == "TR" & data$eyetrial == trial & data$ID == participant])) / fixSum_prenuc
    data.roi$TR_nuclear[row] = sum(na.omit(data$window_nuclear[data$roiLoc == "TR" & data$eyetrial == trial & data$ID == participant])) / fixSum_nuclear
    data.roi$TR_adverb[row] = sum(na.omit(data$window_adverb[data$roiLoc == "TR" & data$eyetrial == trial & data$ID == participant])) / fixSum_adverb
    
    # BR
    data.roi$BR_early[row] = sum(na.omit(data$window_early[data$roiLoc == "BR" & data$eyetrial == trial & data$ID == participant])) / fixSum_early
    data.roi$BR_prenuclear[row] = sum(na.omit(data$window_prenuc[data$roiLoc == "BR" & data$eyetrial == trial & data$ID == participant])) / fixSum_prenuc
    data.roi$BR_nuclear[row] = sum(na.omit(data$window_nuclear[data$roiLoc == "BR" & data$eyetrial == trial & data$ID == participant])) / fixSum_nuclear
    data.roi$BR_adverb[row] = sum(na.omit(data$window_adverb[data$roiLoc == "BR" & data$eyetrial == trial & data$ID == participant])) / fixSum_adverb
    
    # BL
    data.roi$BL_early[row] = sum(na.omit(data$window_early[data$roiLoc == "BL" & data$eyetrial == trial & data$ID == participant])) / fixSum_early
    data.roi$BL_prenuclear[row] = sum(na.omit(data$window_prenuc[data$roiLoc == "BL" & data$eyetrial == trial & data$ID == participant])) / fixSum_prenuc
    data.roi$BL_nuclear[row] = sum(na.omit(data$window_nuclear[data$roiLoc == "BL" & data$eyetrial == trial & data$ID == participant])) / fixSum_nuclear
    data.roi$BL_adverb[row] = sum(na.omit(data$window_adverb[data$roiLoc == "BL" & data$eyetrial == trial & data$ID == participant])) / fixSum_adverb
    
    # NA
    # Fixations that are not in an ROI, as a proportion of the total fixationd duration
    data.roi$roi_na[row] = sum(data$fixDur[data$eyetrial == trial & data$ID == participant][is.na(data$roiLoc[data$eyetrial == trial & data$ID == participant])]) / fixSum

    # Next row
    row = row + 1
  } #/trial
} #/participant


# Sanity check
data.roi.long.agg <- data.roi %>% 
  gather(window, proportion, 3:19) %>% 
  separate(window, c("position", "window")) %>% 
  group_by(window, ID, eyetrial) %>% 
  summarise(sum = sum(proportion, na.rm = T))

# sum columns should all add up to 1
sum(data.roi.long.agg[data.roi.long.agg$window != "na",]$sum == 1, na.rm = T) / nrow(data.roi.long.agg[data.roi.long.agg$window != "na",])
# at 80%

# Might be due to windows that do not have any fixation which results in 0s
sum(data.roi.long.agg[data.roi.long.agg$window != "na",]$sum == 0, na.rm = T) / nrow(data.roi.long.agg[data.roi.long.agg$window != "na",])
# another 20% (doesn't exactly add up to 100, but probs rounding errors)

## Merge data.roi into data
data.roi.long <- data.roi %>% 
  gather(window, proportion, 3:19) %>%
  separate(window, c("position", "window")) %>%
  full_join(data.roi.long.agg) %>% 
  spread(position, proportion) %>% 
  rename("BL_prop" = BL,
         "BR_prop" = BR,
         "TL_prop" = TL,
         "TR_prop" = TR,) %>% 
  # delete irrelevant rows
  filter(window != "na",
         sum != 0) %>% 
  full_join(data)


# Map proportions onto response categories
data.roi.long$Target_prop <- ifelse(data.roi.long$Target_pos == "TL_Pic", data.roi.long$TL_prop,
                             ifelse(data.roi.long$Target_pos == "TR_Pic", data.roi.long$TR_prop,
                             ifelse(data.roi.long$Target_pos == "BL_Pic", data.roi.long$BL_prop, 
                             ifelse(data.roi.long$Target_pos == "BR_Pic", data.roi.long$BL_prop, NA))))

data.roi.long$SubjComp_prop <- ifelse(data.roi.long$Target_pos == "TL_Pic", data.roi.long$TR_prop,
                             ifelse(data.roi.long$Target_pos == "TR_Pic", data.roi.long$BR_prop,
                             ifelse(data.roi.long$Target_pos == "BL_Pic", data.roi.long$TL_prop, 
                             ifelse(data.roi.long$Target_pos == "BR_Pic", data.roi.long$BL_prop, NA))))


data.roi.long$ObjComp_prop <- ifelse(data.roi.long$Target_pos == "TL_Pic", data.roi.long$BL_prop,
                              ifelse(data.roi.long$Target_pos == "TR_Pic", data.roi.long$TL_prop, 
                              ifelse(data.roi.long$Target_pos == "BL_Pic", data.roi.long$BR_prop,
                              ifelse(data.roi.long$Target_pos == "BR_Pic", data.roi.long$TR_prop, NA))))

data.roi.long$Distr_prop <- ifelse(data.roi.long$Target_pos == "TL_Pic", data.roi.long$BR_prop,
                            ifelse(data.roi.long$Target_pos == "TR_Pic", data.roi.long$BL_prop,
                            ifelse(data.roi.long$Target_pos == "BL_Pic", data.roi.long$TR_prop, 
                            ifelse(data.roi.long$Target_pos == "BR_Pic", data.roi.long$TL_prop, NA))))

# maybe useful to actually code them as being given or contrastive
data.roi.long$GivenSubj_prop <- ifelse(data.roi.long$Condition == "CG", 
                                       data.roi.long$SubjComp_prop + data.roi.long$Distr_prop,
                                ifelse(data.roi.long$Condition == "GC", 
                                       data.roi.long$Target_prop + data.roi.long$ObjComp_prop,
                                ifelse(data.roi.long$Condition == "GG", 
                                       data.roi.long$Target_prop + data.roi.long$ObjComp_prop, "NOPE"
                                )))

data.roi.long$GivenObj_prop <- ifelse(data.roi.long$Condition == "CG", 
                                      data.roi.long$Target_prop + data.roi.long$SubjComp_prop,
                               ifelse(data.roi.long$Condition == "GC", 
                                      data.roi.long$ObjComp_prop + data.roi.long$Distr_prop,
                               ifelse(data.roi.long$Condition == "GG", 
                                       data.roi.long$Target_prop + data.roi.long$SubjComp_prop, "NOPE"
                                             )))

# reduce dataframe to something reasonable
df <- data.roi.long %>% 
  select(ID, eyetrial, window, Condition, sum,
         Target_obj, Target_subj,
         Target_prop, SubjComp_prop, ObjComp_prop, Distr_prop,
         GivenSubj_prop, GivenObj_prop) %>% 
  # delete all redundant rows
  distinct()


#######################################
### Write the output to /processed/ ###
#######################################

# Write the full output
setwd("../processed/")
readr::write_csv(df, "ret_processed_stage_2.csv")

# Cleanup
rm(data.roi, datapath, participant, participants, trial, fixSum, gaze, lnmk_adverb, lnmk_prenuc, lnmk_referent, row, trialgazes, fixDur, fixEnd, fixStart)

# End
