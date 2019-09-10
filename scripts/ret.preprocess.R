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
## Version: 9/10/2019
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

# Getting the path of your current open file
datapath = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(datapath))

# Set datapath to working directory
datapath <- paste0(getwd(), "/raw_edf/")

# Read the ET data all at once
setwd("../raw_edf/")
ret = itrackr(path = paste0(getwd(),"/"), pattern = '*.edf')

# Read the beh data all at once
setwd("../raw_OSfiles/")
beh = rbindlist(lapply(list.files(pattern="*.csv"), fread), fill = TRUE)

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

### Index the files by trial nuber (accounting for 0 index vs. 1 index) and participant ID

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

########################################
### Encrich the data with new fields ###
########################################

## How long was the fixation?
ret.dat$fixDur <- ret.dat$entime - ret.dat$sttime

# Encode ROI location for intuitive matching later
ret.dat$roiLoc = NA
ret.dat$roiLoc[ret.dat$roi_1 == 1] <- "TL"
ret.dat$roiLoc[ret.dat$roi_2 == 1] <- "TR"
ret.dat$roiLoc[ret.dat$roi_3 == 1] <- "BL"
ret.dat$roiLoc[ret.dat$roi_4 == 1] <- "BR"

#######################################
### Write the output to /processed/ ###
#######################################

# If no /processed folder, create one
setwd("../")
ifelse(!dir.exists(file.path(getwd(), "../processed/")), dir.create(file.path(getwd())), FALSE)

# Write the beh output
setwd("../raw_beh/")
readr::write_csv(ret.dat, "ret_beh.csv")

# Write the full output
setwd("../processed/")
readr::write_csv(ret.dat, "ret_processed.csv")

# Done