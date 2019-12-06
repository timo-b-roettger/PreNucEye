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

## Settings
# How long is the duration window (in ms) do we want to bin fixations by?
binsize = 50

# What is the shortest fixation we want to consider?
bin_min_cutoff = 100

### TR why is there a cutoff for fixations. We want to consider all fixations

## Load packages
library(rstudioapi)
library(ggplot2)
library(readr)

## Getting the path of your current open file
datapath = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(datapath))
setwd("../processed/")

# Load processed data (OpenSesame (+ Mouse Tracking) + Eye Tracking)
data <- read_csv("ret_processed_stage_1.csv")

## Drop extraneous gazes
# TR: there are none?
data <- subset(data, is.na(data$roiLoc) == FALSE)

## Drop short gazes
# TR: why?
#data <- subset(data, fixDur >= bin_min_cutoff)

## Drop incorrect answers
data <- subset(data, response_corr == TRUE)

## Load the landmarks
setwd("../data/")
landmarks <- read_csv("acoustic_landmarks.csv")

## Join the dataframes to add landmarks to data
data <- merge.data.frame(landmarks, data)

## TR: now first we sort the data frame appropriately
data <- data %>% 
  arrange(sttime)

# Drop landmarks
rm(landmarks)

# TR: initiate plot bin (?)
data$plotbin = 0

# The loop (takes about 6 seconds on a fast computer)
# ID by ID
for (subject in unique(data$ID)){
  # Trial by trial
  for (trial in unique(data$eyetrial[data$ID == subject])) {
    
    # Get all fixations for that trial 
    ## TR: gets the indexes, is there a reason the data frame is so scrambled?
    trial_fixes = which(data$ID == subject & data$eyetrial == trial)
    
    # Ensure trial_fixes is in ascending order
    ## TR ascending order does unfortunately not makes sense I think. Because the indices are not matched with the start of fixation
    trial_fixes = trial_fixes[sort.list(trial_fixes)]
    
    # Get absolute trial start and end time of the trial
    ## TR: is this the actual trial start tho? If you exclude the non-ROI fixations above, 
    ## you basically often lose the trial start because people look intot the middle at the beginning
    trial_start = min( data$sttime[trial_fixes] )
    trial_end = max( data$entime[trial_fixes] )
    
    # Bin 0 will be at the prenuclear onset (same for all trial_fixes)
    prenuc_onset = data$prenuclear[trial_fixes[1]]
    
    # Get bins before 0
    zBin = floor( prenuc_onset / binsize )
    
    for (fixation in trial_fixes) {
      
      # Get start and end time of the fixation, relative to the start of the trial,
      #fix_start = data$sttime[fixation] - trial_start
      fix_end   = data$entime[fixation] - trial_start

      # Get the number of binsize bins in this fixation
      nBin = floor( ( fix_end ) / binsize )
      
      # Write the bin number, relative to the zero bin
      # TR: this is not what we want. We want data in each plotbin and not patch 3,6,9 stuff
      data$plotbin[fixation] = nBin - zBin
      
    } # / fixation loop
  } # /eyetrial loop
} # / subject loop


## Code hits by comparing roiLoc to the windows
data$hit_target = ( data$roiLoc == substr(data$Target_pos[1], 1, 2) )
data$hit_comp_obj = ( data$roiLoc == substr(data$Comp_obj_pos[1], 1, 2) )
data$hit_comp_subj = ( data$roiLoc == substr(data$Comp_subj_pos[1], 1, 2) )
data$hit_distractor = ( data$roiLoc == substr(data$Distractor_pos[1], 1, 2) )

# Ifelse tree to assign hit name super fast
data$hit_name = ifelse(data$hit_target == TRUE, "target", 
                       ifelse(data$hit_comp_obj == TRUE, "comp_obj", 
                              ifelse(data$hit_comp_subj == TRUE, "comp_subj", 
                                     ifelse(data$hit_distractor == TRUE, "distractor", "NA"))))

## Write the output
setwd(dirname(datapath))
setwd("../processed/")
write_csv(data, "ret_spaghetti.csv")


## Compile a df to plot

# Empty dataframe for the plot dataframe
plot_df = data.frame(data.frame(matrix(vector(), 0, 5, dimnames=list(c(), c("condition", "role", "bin", "pcnt", "cnt"))), stringsAsFactors=FALSE))

# For each condition, for each ROI role, what is the proportion of fixDur by plotbin?
# 4s on a fast machine
for (condition in unique( data$Condition )){
  for (role in c("target", "comp_obj", "comp_subj", "distractor")){
    for (bin in unique( data$plotbin ) ){
      bins = which(data$plotbin == bin & data$hit_name == role & data$Condition == condition)
      
      # Total duration for the bin across roles
      trial_dur = sum(data$fixDur[data$plotbin == bin & data$Condition == condition])
      
      # Total duration for just this bin in the role-condition combo
      bin_dur = sum( data$fixDur[ bins ] )
      
      # Percent of fixation for this bin across roles within condition
      bin_pcnt = round( bin_dur / trial_dur * 100 )
      
      # Count the number of bins in this cell
      bin_cnt = length( bins )
    
      # Save our results to plot_df
      thisrow = nrow(plot_df) + 1 # Row number
      plot_df[thisrow,] = cbind(condition, role, bin, bin_pcnt, bin_cnt)
      
    }#/bin loop
  } #/row loop
}#/condition loop

## Write the output
setwd(dirname(datapath))
setwd("../processed/")
write_csv(plot_df, "ret_spaghetti.csv")



# TR: quick and dirty plot
# adjust df
plot_df$bin <- as.numeric(plot_df$bin)
plot_df$pcnt <- as.numeric(plot_df$pcnt)


# agg 
plot_agg <- plot_df %>% 
  group_by(condition, bin, role) %>% 
  summarise(pcnt = mean(pcnt, na.rm = TRUE))

ggplot(plot_agg, aes(x = bin, y = pcnt)) +
  geom_point(alpha = 0.3) +
  geom_smooth() +
  facet_grid(role~condition) +
  xlim(-20,100)


