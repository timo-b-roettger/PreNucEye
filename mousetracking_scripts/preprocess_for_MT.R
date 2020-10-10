
############
### Info ###
############
#
## Author:  Timo Roettger (timo.b.roettger @ gmail.com)
#
## Project: Eye tracking during prenuclear pitch accent comprehension
#
##          DESCRIBE
#
## Version: 8/17/2019
#

#############
### Setup ###
#############

## Load packages ####
library(readbulk)
library(mousetrap)
library(tidyverse)
library(rstudioapi)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))

## specify path
setwd("../raw_OSfiles/")
path <- getwd()

# Read & preprocess ####
raw_data <- readbulk::read_opensesame(directory = path)
xdata <- raw_data

# Fix default / Default error
xdata$Condition <- ifelse(xdata$Condition == "default", "Default", xdata$Condition)

# load in acoustic landmarks
setwd("../data/")
acoustics <- read_csv("acoustic_landmarks.csv")

# join data and acoustic landmarks
xdata <- full_join(xdata, acoustics)

# Exclude empty "timestamps_mt_maintrack_1", "xpos_mt_maintrack_1", "ypos_mt_maintrack_1" (quirk of OpenSesame)
# xdata  <- dplyr::select(xdata,-xpos_mt_maintrack_1,-ypos_mt_maintrack_1,-timestamps_mt_maintrack_1)

# Record number of test trials, enabling the calculation
# of exclusion percentages later
n_before <- length(xdata[!(xdata$Condition %in% c("Training1", "Training2")),]$count_trial_posterior)

## transform into mousetrap object ####
# Create a mousetrap data object
mtdata_2nd <- mt_import_mousetrap(
  xdata %>% filter(Condition != "Training1",
                   Condition != "Training2"),
  xpos_label = "xpos_mt_maintrack_2nd_02",
  ypos_label = "ypos_mt_maintrack_2nd_02",
  timestamps_label = "timestamps_mt_maintrack_2nd_02",
  mt_id_label = NULL,
  split = ",",
  duplicates = "remove_first",
  reset_timestamps = TRUE,
  verbose = TRUE
)

# create a PDF with all separate raw trajectories 
# setwd("../plots/")
# mt_plot_per_trajectory("trajectories.pdf", mtdata_2nd, "trajectories")

# Bottoms up
mtdata_2nd <- mt_remap_symmetric(
  mtdata_2nd,
  use = "trajectories",
  save_as = "trajectories",
  dimensions = c("xpos", "ypos"),
  remap_xpos = "no",
  remap_ypos = "no"
)

# start-align
mtdata <- mt_align_start(
  mtdata_2nd,
  use = "trajectories",
  save_as = "trajectories",
  dimensions = c("xpos", "ypos"),
  start = c(0, 0),
  verbose = FALSE
)



## Subset Correct and Incorrect responses ####
mtdata_2nd$data$responded_correct <- ifelse(mtdata_2nd$data$response_mt_maintrack_2nd_02 == mtdata_2nd$data$Target_pic, 1, 0) 

# Record percentage of trials that are correct
round(1- (sum((mtdata_2nd$data$responded_correct)) / n_before), digits = 2)
# note perentage here: 2%

# Time normalize
mtdata_2nd <- mt_time_normalize(
  mtdata_2nd,
  use = "trajectories",
  save_as = "tn_trajectories",
  dimensions = c("xpos", "ypos"),
  timestamps = "timestamps",
  nsteps = 101,
  verbose = FALSE
)

## Calculate derivatives, measures & deviations ####
mtdata_2nd <- mt_derivatives(
  mtdata_2nd,
  use = "tn_trajectories",
  save_as = "der_trajectories",
  dimensions = c("xpos", "ypos"),
  timestamps = "timestamps",
  prefix = "",
  # acc_on_abs_vel = FALSE,
  verbose = FALSE
  # show_progress = NULL,
  # dimension = NULL
)

mtdata_2nd <- mt_measures(
  mtdata_2nd,
  use = "der_trajectories",
  save_as = "measures",
  flip_threshold = 0,
  verbose = TRUE
)


## Create data.frame containing the results ####
results <- mt_export_long(
  mtdata_2nd,
  use = "measures",
  use_variables = c("initiation_time", "AUC", "RT", "acc_max", "acc_max_time", "vel_max", "vel_max_time"),
  use2_variables = c("subject_nr", "Condition", "Target_subj", "Target_obj", "Target_pic",  
                     "Target_pos", "responded_correct")
 )

## merging summary stats and trajectories into a dataframe ####

# convert array tn_trajectories to data_frame
df_tn <- as.data.frame.table(mtdata_2nd$tn_trajectories)
df_tn_der <- as.data.frame.table(mtdata_2nd$der_trajectories)

df_tn <- df_tn %>% 
  spread(key = Var3, value = Freq)
df_tn_der <- df_tn_der %>% 
  spread(key = Var3, value = Freq)

# rename columns for join
colnames(df_tn) <- c("mt_id","var3","timestamps","xpos","ypos","steps")
colnames(df_tn_der) <- c("mt_id","var3","timestamps","xpos","ypos","steps","dist","vel","acc")

# join relevant dataframes
df_data = full_join(df_tn, df_tn_der)
df_data = full_join(df_data, results)

# subset for inferential analysis
df_data_reduced <- df_data[df_data$timestamps == 0,]
df_data_reduced <- df_data_reduced %>%
  dplyr:::select(-c(timestamps, xpos, ypos, steps))

# save to csv
setwd("../processed/")

write.csv(df_data, file = "df_data.csv", row.names = FALSE)
write.csv(df_data_reduced, file = "df_data_reduced.csv", row.names = FALSE)

