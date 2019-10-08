############
### Info ###
############
#
## Authors:  Dan Turner (dturner @ northwestern.edu)
#            Timo Roettger (timo.b.roettger @ gmail.com)
#
## Project: Eye tracking during prenuclear pitch accent comprehension
#
##          Stage 1
#
##          This file imports eyetracking data (.edf) from OpenSesame (.csv),
##          preprocesses the data by specifying regions of interest and counting hits in them.
##
##          It outputs the resulting dataframe as ret_processed.csv" into the
##          directory <your wd>/processed/.
#

## Version: 9/19/2019

#
## Notes
##          * This requires installation of the SRR EDF API, avialable free for download
##          * To run, set your working directory to where the raw data is (.csv and .edf files)
#
#############
### Setup ###
#############

## Install itrackR from Github if it's missing
# devtools::install_github('jashubbard/itrackR', build_vignettes = TRUE)

## Load main package (everything else called inline)
library(itrackR)
library(dplyr)
library(readr)
library(readbulk)
library(rstudioapi)
library(data.table)
library(rstudioapi)

# Getting the path of your current open file
datapath = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(datapath))

# Read the ET data all at once
setwd("../raw_edf/")
ret = itrackr(path = paste0(getwd(),"/"), pattern = '*.edf')

# Read the beh data all at once

# multmerge() courtesy https://stackoverflow.com/questions/30242065/trying-to-merge-multiple-csv-files-in-r
multmerge = function(path){
  filenames=list.files(path=path, full.names=TRUE)
  rbindlist(lapply(filenames, fread), fill = TRUE)
}
setwd("../raw_OSfiles/")
beh = multmerge(getwd())

##################################
### Regions of Interest (ROIs) ###
##################################

### Build the ROIs one by one, starting in TL and going clockwise

## Top Left, ROI 1
ret <- makeROIs(ret, matrix(c(150,150), nrow = 1), shapes = 'circle',radius = 200, names = 1, append = F)

## Top Right, ROI 2
ret <- makeROIs(ret, matrix(c(750,150), nrow = 1), shapes = 'circle',radius = 200, names = 2, append = T)

## Bottom Right, ROI 3
ret <- makeROIs(ret, matrix(c(750,750), nrow = 1), shapes = 'circle', radius = 200, names = 3, append = T)

## Bottom Left, ROI 4
ret <- makeROIs(ret, matrix(c(150,750), nrow = 1), shapes = 'circle', radius = 200, names = 4, append = T)

## Check our work by plotting the ROIs with all fixations for all trials
plot(ret, zoom = FALSE, oneplot = TRUE, crosshairs = FALSE, summarize = 100)
 
## Calculate fixation hits by ROI
ret <- calcHits(ret)

####################################
### Merge ET and behavioral data ###
####################################

### Index the files by trial number (accounting for 0 index vs. 1 index) and participant ID

## Adjust the indexing
beh$count_pygaze_drift_corr <- beh$count_pygaze_drift_corr + 1

## Check our work
range(ret$fixations$eyetrial)
range(beh$count_pygaze_drift_corr)

## Merge beh and ret in ret.dat
ret.dat <- merge.data.frame(ret$fixations, beh, by.x  =  c("eyetrial","ID"), by.y  =  c("count_pygaze_drift_corr","subject_nr"), all  =  TRUE)

## Flag large ET errors
# Find the rows with large ET errors
lgerror <- grep("!MODE", ret$messages$message)

# Setup column to flag errorful rows and flag erroes with '1'
ret.dat$lgerror <- 0
ret.dat$lgerror[ret.dat$key[lgerror]] <- 1

# Stage 2 uses the df name 'data', so let's prep for that before ending
data <- ret.dat

# How long was the fixation?
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
         Condition, fixDur, First_obj, First_subj, First_pic, First_sound,
         Target_obj, Target_subj, Target_pos, sttime, entime,
         fixDur, roiLoc, Distractor_pos, Distractor_pic,
         Condition, fixDur,First_obj, First_subj, First_pic, First_sound,
         Target_obj, Target_subj, Target_pos, sttime, entime,
         fixDur, roiLoc, lgerror, response_mt_maintrack_1st_02, response_mt_maintrack_2nd_02, First_pic
  ) %>% 
  filter(Condition %in% c("CG", "GG", "GC"))

#######################################
### Write the output to /processed/ ###
#######################################

# If no /processed folder, create one
#setwd("../")
#ifelse(!dir.exists(file.path(getwd(), "../processed/")), dir.create(file.path(getwd())), FALSE)

# Cleanup
rm(beh, ret, ret.dat, datapath, lgerror, multmerge)

# Write the full output
setwd("../processed/")
readr::write_csv(data, "ret_processed_stage_1.csv")


# Done
