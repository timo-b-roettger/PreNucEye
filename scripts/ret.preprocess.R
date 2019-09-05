############
### Info ###
############
#
## Author:  Dan Turner (dturner @ northwestern.edu)
#
## Project: Eye tracking during prenuclear pitch accent comprehension
#
##          This file imports eyetracking data (.edf) from OpenSesame (.csv),
##          preprocesses the data by specifying regions of interest, counting hits and adding
##          useful columns to the dataframe. It outputs the resulting dataframe as ret_dat.csv into the
##          directory <your wd>/processed/. If the directory doesn't exist this creates it.
#
## Version: 7/25/2019
#
## Notes
##          * This requires installation of the SRR EDF API, avialable free for download
##          * To run, set your working directory to where the raw data is (.csv and .edf files)
#
#############
### Setup ###
#############

## Install itrackR from Github if it's missing
new.packages <- list.of.packages[!("itrackR" %in% installed.packages()[,"Package"])]
if(length(new.packages)) devtools::install_github('jashubbard/itrackR', build_vignettes = TRUE)

## Load main package (everything else called inline)
library(itrackR)
library(dplyr)
library(readr)
library(readbulk)
library(rstudioapi)

# Getting the path of your current open file
datapath = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(datapath))
setwd("../raw_edf/")

# Set datapath to working directory
datapath <- paste0(getwd(), "/")

# Read the ET data all at once
ret = itrackr(path = datapath, pattern = '*.edf')

# Read the beh data all at once
setwd("../raw_beh/")
beh = lapply(paste0(getwd(), "/", list.files(paste0(getwd(), "/"), pattern = "*.csv")), readr::read_csv) %>% dplyr::bind_rows()

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
plot(ret, zoom = TRUE, oneplot = TRUE)
 
## Calculate fixation hits by ROI
ret <- calcHits(ret)  # doesn't work

####################################
### Merge ET and behavioral data ###
####################################

### Index the files by trial nuber (accounting for 0 index vs. 1 index) and participant ID

## Adjust the indexing
beh$count_pygaze_drift_corr <- beh$count_pygaze_drift_corr + 1

## Check our work
range(beh$count_pygaze_drift_corr)
range(ret$fixations$eyetrial)

## Merge beh and ret in ret.dat
ret.dat <- merge.data.frame(ret$fixations, beh, by.x  =  c("eyetrial","ID"), by.y  =  c("count_pygaze_drift_corr","subject_nr"), all  =  TRUE)

########################################
### Encrich the data with new fields ###
########################################

## How long was the fixation?
ret.dat$fixDur <- ret.dat$entime - ret.dat$sttime

## Code each fixation hit for whether it's in a ROI (might be useful)
ret.dat$roiLoc  =  "NA"
ret.dat$roiLoc  =  ifelse(ret.dat$roi_1  ==  1, "TL", ret.dat$roiLoc)
ret.dat$roiLoc  =  ifelse(ret.dat$roi_2  ==  1, "TR", ret.dat$roiLoc)
ret.dat$roiLoc  =  ifelse(ret.dat$roi_3  ==  1, "BR", ret.dat$roiLoc)
ret.dat$roiLoc  =  ifelse(ret.dat$roi_4  ==  1, "BL", ret.dat$roiLoc)

## Write the messages from the eye tracker to the data frame
for (key in 1:length(ret$messages$key)){
  
  ## Get the message by key
  et_mess <- ret$messages$message[ret$messages$key  ==  key]
  
  ## Write the message by key
  ret.dat$et_mess[ret.dat$key  ==  key] <- et_mess
}

### Sum the fixations by eyetrial

## Build a list with all of the eyetrials (written by tracker)
eyetrials <- unique(ret.dat$eyetrial)

## Loop the eyetrials to sum the ROI fixations
for (eyetrial in eyetrials){
  
  # ROI 1 (TL)
  theSum <- sum(ret.dat$roi_1[ret.dat$eyetrial  ==  eyetrial])
  ret.dat$roi_1_sum[ret.dat$eyetrial  ==  eyetrial] <- theSum
  
  # ROI 2 (TR)
  theSum <- sum(ret.dat$roi_2[ret.dat$eyetrial  ==  eyetrial])
  ret.dat$roi_2_sum[ret.dat$eyetrial  ==  eyetrial] <- theSum
  
  # ROI 3 (BR)
  theSum <- sum(ret.dat$roi_3[ret.dat$eyetrial  ==  eyetrial])
  ret.dat$roi_3_sum[ret.dat$eyetrial  ==  eyetrial] <- theSum
  
  # ROI 4 (BL)
  theSum <- sum(ret.dat$roi_4[ret.dat$eyetrial  ==  eyetrial])
  ret.dat$roi_4_sum[ret.dat$eyetrial  ==  eyetrial] <- theSum
  
  # note the fixation sum
  fixationSum <- sum(ret.dat$fixDur[ret.dat$eyetrial == eyetrial])
  ret.dat$fixationSum[ret.dat$eyetrial  ==  eyetrial] <- fixationSum
}

### For each trial, which ROI had the most fixations?

## Make a df with all of the sums
ret.dat.sums <- data.frame(ret.dat$roi_1_sum, ret.dat$roi_2_sum, ret.dat$roi_3_sum, ret.dat$roi_4_sum)

## Return the column with the most fixations. In the event of a tie, choose randomly which gets written
ret.dat$topROI <- max.col(ret.dat.sums,ties.method = "random")

## Let's backfeed these results to beh
# Initialize the col
beh$topROI <- NA

## Loop beh and populate the new col by coordinating eye trial with drift_corr
for (row in 1:nrow(beh)){
  eyetrial <- beh$count_pygaze_drift_corr[row]
  beh$topROI <- ret.dat$topROI[ret.dat$eyetrial  ==  eyetrial][1]
}

#######################################
### Write the output to /processed/ ###
#######################################

# If no /processed folder, create one
setwd("../")
ifelse(!dir.exists(file.path(getwd(), "/processed/")), dir.create(file.path(getwd())), FALSE)

# Write the full output
setwd("processed/")
readr::write_csv(ret.dat, "ret_processed.csv")
readr::write_csv(ret.dat, "beh_processed.csv")


# Done