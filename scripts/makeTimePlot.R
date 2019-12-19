############
### Info ###
############
#
## Authors:  Dan Turner (dturner @ northwestern.edu)
#            Timo Roettger (timo.b.roettger @ gmail.com)
#
## Project: Eye tracking during prenuclear pitch accent comprehension
#
##          This file imports stage 1 data and creates a plot across bins.
#
## Version: 12/4/2019
#
#############
### Setup ###
#############

## Load packages
library(rstudioapi)
library(tidyverse)
library(readr)

#remotes::install_github("dmirman/gazer")
#devtools::install_github("dmirman/gazer")
library(gazer)

## Getting the path of your current open file
datapath = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(datapath))
setwd("../processed/")

# Load processed data (OpenSesame (+ Mouse Tracking) + Eye Tracking)
data <- read_csv("ret_processed_stage_1.csv")


##############
### Binify ###
#############ä#

# change data structure to the one needed for the gazeR
data <- data %>% 
  na.omit(fixDur) %>% 
  rename(Subject = ID ,
         CURRENT_FIX_DURATION = "fixDur",
         CURRENT_FIX_START = "sttime",
         CURRENT_FIX_END = "entime")

# bin into 50ms bins (takes 10 seconds on my machine)
temp = binify_fixations(data, binSize = 50, keepCols = c("Subject", "Condition", "Target_obj", "eyetrial", "roiLoc", "lgerror",
                                                         "Target_pos", "Comp_obj_pos", "Comp_subj_pos", "Distractor_pos", 
                                                         "First_pic", "Distractor_pic", "Comp_obj_pic", "Comp_subj_pic",
                                                         "response_mt_maintrack_2nd_02", "response_mt_maintrack_1st_02"), 
                        maxTime = NULL)


## Compile a temporary df
temp2 <- temp %>% 
  group_by(Subject, eyetrial) %>% 
  mutate(minTime = min(Time),
            minTimeBin = min(timeBin)) %>% 
  ungroup() %>% 
  mutate(timeC = Time - minTime,
         timeBinC = timeBin - minTimeBin)

# continue with renamed df
data <- temp2

# Load acoustic landmarks
setwd("../data/")
landmarks <- read_csv("acoustic_landmarks.csv")

# Add 200ms to landmarks as saccade initiation time
landmarks <- landmarks %>% 
  mutate(prenuclear_onset_s = prenuclear_onset + 200,
         referent_onset_s = referent_onset + 200,
         adverb_onset_s = adverb_onset + 200)

# Join the landmarks and data
data <- full_join(data, landmarks) %>% 
  # exclude Training
  filter(Condition != "Training1",
         Condition != "Training2")

# Drop landmarks
rm(landmarks)

##########################
### Exclusion criteria ###
##########################

## exclude if subject made error for trigger sentences in more than 10% of all trials
data$correctTrigger <- ifelse(data$response_mt_maintrack_1st_02 == data$First_pic, 1, 0)

# how many errors overall?
100 - round(((sum(data$correctTrigger) / nrow(data)) * 100), digits = 2)

# calculate correct responses for subjects
triggerError_table <- data %>%
  group_by(Subject) %>%
  summarise(triggerErrorRate = 100 - round(((sum(correctTrigger) / length(correctTrigger)) * 100), digits = 2)) 

print(tbl_df(triggerError_table), n = 50)

# check for 10% errors per subject and mark them
TriggerErrorID = c("")
data$excludeTriggerError <- 0
data$excludeTriggerError[data$Subject %in% TriggerErrorID] <- 1

## exclude if subject clicked wrong "referent" (sentence object) for test sentences in more than 10% of all trials
data$correctReferent <- ifelse(data$response_mt_maintrack_2nd_02 == data$Comp_obj_pic |
                                 data$response_mt_maintrack_2nd_02 == data$Distractor_pic, 0, 1)

# how many errors overall?
100 - round(((sum(data$correctReferent) / nrow(data)) * 100), digits = 2)

# calculate correct responses for subjects
ReferentError_table <- data %>%
  group_by(Subject) %>%
  summarise(RefErrorRate = 100 - round(((sum(correctReferent) / length(correctReferent)) * 100), digits = 2)) 

print(tbl_df(ReferentError_table), n = 50)

# check for 10% errors per subject and mark them
ReferentErrorID = c("")
data$excludeRefError <- 0
data$excludeRefError[data$Subject %in% ReferentErrorID] <- 1

## exclude if eytracker encountered "large error" in more than 10% of all trials of a subject 
# how many errors overall?
round(((sum(data$lgerror) / nrow(data)) * 100), digits = 2)

# calculate correct responses for subjects
LargeError_table <- data %>%
  group_by(Subject) %>%
  summarise(LargeErrorRate = round(((sum(lgerror) / length(lgerror)) * 100), digits = 2)) 

print(tbl_df(LargeError_table), n = 50)

# check for 10% errors per subject and mark them
LargeErrorID = c("")
data$excludeLargeError <- 0
data$excludeLargeError[data$Subject %in% LargeErrorID] <- 1

#########################
### Survey Exclusions ###
#########################

### Self-Reported Challenges
## Vision    4, 18
## Hearing   -
## Reading   10, 18
## Learning  4, 10, 24
## Nat.Lang. 34

# Exclude by participant ID
excluded_IDs = c(4, 10, 18, 24, 34)

# Mark all rows from excluded participants
data$surveyExclude = 0
data$surveyExclude[which(data$Subject %in% excluded_IDs)] <- 1

#####################
### Describe ROIs ###
#####################

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

# code all kind of possible ways to look at it
data$target_subj <- data$fixTarget + data$fixObjComp
data$target_obj <- data$fixTarget + data$fixSubjComp

data$comp_subj <- 1 - data$target_subj
data$comp_obj <-  1 - data$target_obj

data$given_subj <- ifelse(data$Condition == "CG", data$fixSubjComp + data$fixDist,
                          data$fixTarget + data$fixObjComp)
data$given_obj <- ifelse(data$Condition == "CG", data$fixTarget + data$fixSubjComp,
                           ifelse(data$Condition == "GC", data$fixObjComp + data$fixDist, 
                                  data$fixTarget + data$fixSubjComp))

data$given_given <- ifelse(data$Condition == "CG", data$fixSubjComp,
                           ifelse(data$Condition == "GC", data$fixObjComp, 
                                  data$fixTarget))

data$contrastive_given <- ifelse(data$Condition == "GG", data$fixSubjComp,
                           ifelse(data$Condition == "GC", data$fixDist, 
                                  data$fixTarget))

data$given_contrastive <- ifelse(data$Condition == "GG", data$fixObjComp,
                                 ifelse(data$Condition == "CG", data$fixDist, 
                                        data$fixTarget))


data$contrastive_contrastive <- ifelse(data$Condition == "GG", data$fixDist,
                                       ifelse(data$Condition == "CG", data$fixObjComp, 
                                              data$fixSubjComp))

#####################
### Prep for plot ###
#####################

# exclusion
data <- data %>% 
  # exclusion
  filter(excludeTriggerError == 0,
         excludeRefError == 0, 
         excludeLargeError == 0,
         lgerror == 0, 
         surveyExclude == 0)


# add landmarks
lm <- data %>% 
  group_by(Condition) %>% 
  summarise(prenuclear_onset = mean(prenuclear_onset),
            referent_onset = mean(referent_onset),
            adverb_onset = mean(adverb_onset),
            prenuclear_onset_s = mean(prenuclear_onset_s),
            referent_onset_s = mean(referent_onset_s),
            adverb_onset_s = mean(adverb_onset_s)) %>% 
  mutate(Condition = as.factor(Condition),
         Condition = fct_recode(Condition, `NP1 accented` = "CG",
                                `NP2 accented` = "GC",
                                `Both NPs accented` =  "GG")) 

# gather and rename category
plot_agg <- data %>% 
  gather("role", "proportion", 
         c("fixTarget", "fixObjComp", "fixSubjComp", "fixDist", 
           "target_subj", "target_obj", "comp_subj", "comp_obj",
           "given_subj", "given_obj",
           "given_given", "given_contrastive",
           "contrastive_given", "contrastive_contrastive")) %>% 
  mutate(role = as.factor(role),
         role = fct_recode(role, Target = "fixTarget",
                               `Object Competitor` = "fixObjComp",
                               `Subject Competitor` =  "fixSubjComp", 
                               Distractor = "fixDist",
                               `Target 1st NP` = "target_subj",
                               `Target 2nd NP` = "target_obj",
                               `Competitor 1st NP` = "comp_subj",
                               `Competitor 2nd NP` = "comp_obj",
                               `Given 1st NP` = "given_subj",
                               `Given 2nd NP` = "given_obj",
                                `both given` = "given_given",
                                `new NP1, given NP2` = "contrastive_given",
                                `given NP1, new NP2` = "given_contrastive",
                                `both new` = "contrastive_contrastive",
                           
                           ),
         Condition = as.factor(Condition),
         Condition = fct_recode(Condition, `NP1 accented` = "CG",
                                `NP2 accented` = "GC",
                                `Both NPs accented` =  "GG")) %>% 
  group_by(role, Condition, timeC) %>% 
  summarise(prop = mean(proportion, na.rm = TRUE),
            prop.sd = sd(proportion, na.rm = TRUE) / sqrt(length(unique(Subject))) )



# specify colors
TargetCol = "#1b9e77"
ObjCompCol = "#d95f02"
SubjCompCol = "#7570b3"
DistrCol = "#e7298a"



############
### Plot ###
############


# store information for annotation
xagg_text <- data.frame(Condition = unique(plot_agg$Condition))
xagg_text$text_2 <- ifelse(xagg_text$Condition != "NP2 accented", "THINGY that", "thingy that")
xagg_text$text_3 <- ifelse(xagg_text$Condition != "NP1 accented", "BERRIES", "berries")
xagg_text$text_4 <- ifelse(xagg_text$Condition == "Both NPs accented", "again", "instead")

xagg_text$thingy_duration <- ifelse(xagg_text$Condition == "NP2 accented", 325, 377)

lm <- full_join(lm, xagg_text)

# plot all four categories
TimePlot <- 
ggplot(plot_agg[plot_agg$role %in% c("both given",
                                     "new NP1, given NP2",
                                     "given NP1, new NP2",
                                     "both new"),], 
       aes(x = timeC, y = prop, color = role)) +
  geom_rect(data = lm,
            aes(xmin = prenuclear_onset, xmax = prenuclear_onset + thingy_duration, ymin = -Inf, ymax = Inf), 
            color = NA, fill = "grey", alpha = 0.3, inherit.aes = FALSE) +
  geom_point(alpha = 1) +
  #geom_smooth(se=FALSE) +
  geom_ribbon(aes(ymin = prop - prop.sd, ymax = prop + prop.sd, fill = role), color = NA, alpha = 0.2) +
  geom_segment(data = lm, aes(x = prenuclear_onset, xend = prenuclear_onset,
                              y = Inf, yend = -Inf), color = "grey", lty = "dashed") +
  geom_segment(data = lm, aes(x = referent_onset, xend = referent_onset,
                              y = Inf, yend = -Inf), color = "grey", lty = "dashed") +
  geom_segment(data = lm, aes(x = adverb_onset, xend = adverb_onset,
                              y = Inf, yend = -Inf), color = "grey", lty = "dashed") +
  # geom_segment(x = 0, xend = Inf, y = 0, yend = 0, color = "black") +
  # geom_segment(x = 0, xend = Inf, y = 1, yend = 1, color = "black") +
  geom_path(size = 1) +
  # geom_text(data = lm,
  #           aes(x = prenuclear_onset/2), y = -0.2, label = "Now click", size  = 3, inherit.aes = FALSE, hjust = 0.5) +
  # geom_text(data = lm,
  #           aes(x = prenuclear_onset/2), y = -0.25, label = "on the", size  = 3, inherit.aes = FALSE, hjust = 0.5) +
  # geom_text(data = lm,
  #            aes( x = ((referent_onset - prenuclear_onset) / 2) + prenuclear_onset, label = text_2), 
  #            y = -0.2, size = 3, inherit.aes = FALSE,  hjust = 0.5) + 
  # geom_text(data = lm,
  #           aes(x = ((referent_onset - prenuclear_onset) / 2) + (prenuclear_onset)),
  #           y = -0.25, size = 3,  label = "dreams about the", inherit.aes = FALSE,  hjust = 0.5) +
  # geom_text(data = lm,
  #           aes(x = ((adverb_onset - referent_onset) / 2) + (referent_onset), label = text_3),
  #           y = -0.225, size = 3, inherit.aes = FALSE,  hjust = 0.5) +
  # geom_text(data = lm,
  #           aes(x = adverb_onset + 50, label = text_4),
  #           y = -0.225, size = 3, hjust = 0, inherit.aes = FALSE) +
  geom_point(data = lm , pch = 21, size = 6, color = "black", fill = NA, 
             aes(x = prenuclear_onset/2), y = 0.9) +
  geom_point(data = lm , pch = 21, size = 6, color = "black", fill = NA, 
             aes(x =((referent_onset - prenuclear_onset) / 2) + prenuclear_onset), y = 0.9) +
  geom_point(data = lm , pch = 21, size = 6, color = "black", fill = NA, 
             aes(x = ((adverb_onset - referent_onset) / 2) + (referent_onset)), y = 0.9) +
  geom_point(data = lm , pch = 21, size = 6, color = "black", fill = NA, 
             aes(x = adverb_onset + 300), y = 0.9) +
  geom_text(data = lm , size = 4, color = "black",
             aes(x = prenuclear_onset/2), label = "1", y = 0.9) +
  geom_text(data = lm , size = 4, color = "black",
             aes(x =((referent_onset - prenuclear_onset) / 2) + prenuclear_onset), label = "2", y = 0.9) +
  geom_text(data = lm , size = 4, color = "black",
             aes(x = ((adverb_onset - referent_onset) / 2) + (referent_onset)), label = "3", y = 0.9) +
  geom_text(data = lm , size = 4, color = "black",
             aes(x = adverb_onset + 300), label = "4", y = 0.9) +
  #geom_smooth() +
  scale_colour_manual(values = c(SubjCompCol, DistrCol, ObjCompCol, TargetCol)) +
  scale_fill_manual(values = c(SubjCompCol, DistrCol, ObjCompCol, TargetCol)) +
  facet_wrap(. ~ Condition, nrow = 3) +
  scale_y_continuous(expand = c(0, 0), breaks = (c(0, 0.25, 0.5, 0.75, 1)), limits = c(-0.05,1)) +
  scale_x_continuous(limits = c(0,3500)) +
  labs(title = "Fixation proportions across time",
       #subtitle = paste0("ribbons represent SD / ", expression(sqrt(x)), "\n"),
       subtitle = " \n", 
       y = "Average fixation proportion\n",
       x = "\ntime in ms"
  ) +
  theme_classic() + 
  theme(legend.position = "bottom",
        legend.key.height = unit(2,"line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.background = element_rect(fill = "transparent"),
        strip.background = element_blank(),
        strip.text = element_text(size = 16),
        panel.spacing = unit(2, "lines"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent"),
        axis.line = element_blank(),
        axis.line.x = element_blank(),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 18, face = "bold"),
        panel.border = element_rect(colour = "black", fill = NA), 
        plot.margin = unit(c(1,1,1,1),"cm"))


# store plot 
setwd("../plots/")
ggsave(filename = "TimePlot.pdf",
       plot = TimePlot,
       device = "pdf",
       width = 220, 
       height = 350,
       #width = 393.75, 
       #height = 135,
       units = "mm",
       #bg = "transparent",
       dpi = 300)
