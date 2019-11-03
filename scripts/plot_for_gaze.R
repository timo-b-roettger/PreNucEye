############
### Info ###
############
#
## Authors:  Dan Turner (dturner @ northwestern.edu)
#            Timo Roettger (timo.b.roettger @ gmail.com)
#
## Project: Eye tracking during prenuclear pitch accent comprehension
#
##          Plotting
#
##          This file plots the fixation proportion.
#
## Version: 9/19/2019


#############
### Setup ###
#############

library(tidyverse)
library(ggbeeswarm)
library(rstudioapi)
# library(ggpubr)

## Getting the path of your current open file
datapath = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(datapath))
setwd("../processed/")

## Load and merge the data with our acoustic landmarks
# Load processed data
data <- read_csv("ret_processed_stage_2.csv")

# Load acoustic landmarks
setwd("../data/")
landmarks <- read_csv("acoustic_landmarks.csv")

# Join the landmarks and data
data <- full_join(data, landmarks) %>% 
  filter(!is.na(window))

# rename levels 
data$window <- as.factor(data$window)
levels(data$window) <- c("adverb", "early", "2nd NP", "1st NP")

# exclusion
data <- data %>% 
  filter(!is.na(window)) %>% 
  # exclusion
  filter(excludeTriggerError == 0,
         excludeRefError == 0, 
         excludeLargeError == 0,
         lgerror == 0) %>% 
  mutate(window = factor(window, levels = c("early", "1st NP", "2nd NP", "adverb"))) %>% 
  # generate binary preference score
  mutate(subj_preference = ifelse(((Target_prop + ObjComp_prop) / 2) > ((SubjComp_prop + Distr_prop) / 2), 1, 0),
         obj_preference = ifelse(((Target_prop + SubjComp_prop) / 2) > ((ObjComp_prop + Distr_prop) / 2), 1, 0),
         # centralize eyetrial
         eyetrial.c = scale(eyetrial, scale = T))

# Gather proportional values
data <- data %>% 
  gather(response, proportion, c(8:13, 24:25)) 

# Rename response levels
data <- data %>% 
  mutate(response = as.factor(response),
         response = fct_recode(response, Target = "Target_prop",
                               `Object Competitor` = "ObjComp_prop",
                               `Subject Competitor` =  "SubjComp_prop", 
                               Distractor = "Distr_prop",
                               `Given Subject` = "GivenSubj_prop",
                               `Given Object` = "GivenObj_prop",
                               `Target 1st NP` = "subj_preference",
                               `Target 2nd NP` = "obj_preference"),
         Condition = as.factor(Condition),
         Condition = fct_recode(Condition, `1st NP accent` = "CG",
                                `2nd NP accent` = "GC",
                                `Both NPs have accents` =  "GG"),
         # bin eyetrials into four categories
         trial_bin = ifelse(eyetrial < 41, 1,
                     ifelse(eyetrial >= 41 & eyetrial < 65, 2, 
                     ifelse(eyetrial >= 65 & eyetrial < 89, 3,
                     ifelse(eyetrial >= 89, 4, "error"))))
         )
         
#################
### Aggregate ###
#################

# aggregate proportions (per subject)
xagg_subj <- data %>% 
  group_by(Condition, window, ID, response) %>% 
  summarise(prop = mean(proportion, na.rm = T),
            prenuc_onset = mean(prenuclear_onset, na.rm = T),
            ref_onset = mean(referent_onset, na.rm = T),
            adverb_onset = mean(adverb_onset, na.rm = T)) %>% 
  mutate(window_dummy1 = prenuc_onset / 2,
         window_dummy2 = ((ref_onset - prenuc_onset)/2) + prenuc_onset + 200,
         window_dummy3 = ((adverb_onset - ref_onset)/2) + ref_onset + 200,
         window_dummy4 = adverb_onset + 500 + 200,
         window_dummy = ifelse(window == "early", window_dummy1,
                               ifelse(window == "1st NP", window_dummy2,
                                      ifelse(window == "2nd NP", window_dummy3, window_dummy4)
))) %>% 
  select(-window_dummy1, -window_dummy2, -window_dummy3, -window_dummy4)

# aggregate proportions (per target tiem)
xagg_item <- data %>% 
  group_by(Condition, window, Target_obj, response) %>% 
  summarise(prop = mean(proportion, na.rm = T),
            prenuc_onset = mean(prenuclear_onset, na.rm = T),
            ref_onset = mean(referent_onset, na.rm = T),
            adverb_onset = mean(adverb_onset, na.rm = T))

# aggregate proportions (overall)
xagg <- xagg_subj %>% 
  group_by(Condition, window, response) %>% 
  summarise(prop = mean(prop, na.rm = T),
            prenuc_onset = mean(prenuc_onset, na.rm = T),
            ref_onset = mean(ref_onset, na.rm = T),
            adverb_onset = mean(adverb_onset, na.rm = T),
            window_dummy = mean(window_dummy, na.rm = T))

# aggregate proportions (per subject and trial_bin)
xagg_subj_trialBin <- data %>% 
  group_by(Condition, window, ID, response, trial_bin) %>% 
  summarise(prop = mean(proportion, na.rm = T))

# aggregate proportions (trial_bin)
xagg_trialBin <- xagg_subj_trialBin %>% 
  group_by(Condition, window, response, trial_bin) %>% 
  summarise(prop = mean(prop, na.rm = T))

# aggregate over trials
xagg_trials <- data %>% 
  group_by(Condition, window, response, eyetrial) %>% 
  summarise(prop = mean(proportion, na.rm = T))

############
### Plot ###
############

# specify colors
TargetCol = "#d01c8b"
ObjCompCol = "#4dac26"
SubjCompCol = "#b8e186"
DistrCol = "#636363"

# load posteriors
setwd("../models/")
load("posteriors.RData")

## plot preferences for 1st NP Target or 2nd NP Target 

# store information for annotation
xagg_text <- data.frame(Condition = unique(xagg$Condition))
xagg_text$text_2 <- ifelse(xagg_text$Condition != "2nd NP accent", "THINGY", "thingy")
xagg_text$text_3 <- ifelse(xagg_text$Condition != "1st NP accent", "BERRIES", "berries")
xagg_text$text_4 <- ifelse(xagg_text$Condition == "Both NPs have accents", "again", "instead")

xagg_text <- xagg_text %>%   
  gather(position, text, -Condition) %>% 
  mutate(font_face = c("bold", "plain", "bold", "plain", "bold", "bold", "plain", "plain", "plain"))

colnames(posteriors_prob) <- c("name", "lci", "uci", "proportion", "probability", "Condition", "window", "type", "response")

plot_df <- full_join(posteriors_prob, xagg[xagg$response %in% c("Target 1st NP", "Target 2nd NP"),]) %>% 
  full_join(xagg_text)


# Plot aggregated fixations for Given Subject and Given Object only
Fix_agg_2responses <- 
  ggplot(data = xagg_subj[xagg_subj$response %in%  c("Target 2nd NP", "Target 1st NP"),], 
         aes(x = window_dummy, y = prop, color = response, fill = response)) +
  geom_segment(data = xagg[xagg$response %in%  c("Target 2nd NP", "Target 1st NP"),],
               aes(x = prenuc_onset + 200, xend = prenuc_onset + 200), y = -Inf, yend = Inf, lty = "dashed", colour = "grey") +
  geom_segment(data = xagg[xagg$response %in%  c("Target 2nd NP", "Target 1st NP"),],
               aes(x = ref_onset + 200, xend = ref_onset + 200), y = -Inf, yend = Inf, lty = "dashed", colour = "grey") +
  geom_segment(data = xagg[xagg$response %in%  c("Target 2nd NP", "Target 1st NP"),],
               aes(x = adverb_onset + 200, xend = adverb_onset + 200), y = -Inf, yend = Inf, lty = "dashed", colour = "grey") +
  geom_segment(x = -Inf, y = 0.5, xend = Inf, yend = 0.5,
               lty = "dashed", size = 1, colour = "black") +
  geom_line(aes(group = interaction(ID, response)),
            alpha = 0.1, size = 1) +
  geom_line(data = plot_df, aes(x = window_dummy, y = proportion, color = response, fill = response),
            size = 2) +
  #geom_point(alpha = 0.1, size = 2) +
  geom_errorbar(data = plot_df, 
                  aes(x = window_dummy, ymin = lci, ymax = uci), 
                colour = "black", width = 0.1, inherit.aes = FALSE) +
  geom_ribbon(data = plot_df, aes(ymin = lci, ymax = uci, fill = response), 
              alpha = 0.3, colour = NA) +
  geom_point(data = plot_df, aes(x = window_dummy, y = proportion, color = response, fill = response),
             size = 3, pch = 21, stroke = 1, color = "black", inherit.aes = FALSE) +
  facet_grid(response ~ Condition) +
  scale_colour_manual(values = c(ObjCompCol, TargetCol)) +
  scale_fill_manual(values = c(ObjCompCol, TargetCol)) +
  geom_text(data = plot_df,
    aes(x = prenuc_onset/2), y = -0.1, label = "Now click", size  = 5, inherit.aes = FALSE) +
  geom_text(data = plot_df,
    aes(x = prenuc_onset/2), y = -0.15, label = "on the",size  = 5, inherit.aes = FALSE) +
  geom_text(data = plot_df[plot_df$position == "text_2",],
            aes(label = text, fontface = font_face, x = ((ref_onset - prenuc_onset) / 2) + (prenuc_onset + 200)), 
            y = -0.1, size = 5, color = TargetCol, inherit.aes = FALSE) + 
  annotate("text", x = ((mean(xagg_subj$ref_onset) - mean(xagg_subj$prenuc_onset)) / 2) + mean(xagg_subj$prenuc_onset) + 200,
           y = -0.15, label = "that dreams about the", size  = 3) +
  geom_text(data = plot_df[plot_df$position == "text_3",],
            aes(label = text, fontface = font_face, x = ((adverb_onset - ref_onset) / 2) + (ref_onset + 200)), 
            y = -0.1, size = 5, color = ObjCompCol, inherit.aes = FALSE) + 
  geom_text(data = plot_df[plot_df$position == "text_4",], 
            aes(label = text, fontface = font_face, x = adverb_onset + 500), 
            y = -0.1, size = 5, hjust = 0, inherit.aes = FALSE) + 
  scale_y_continuous(expand = c(0, 0), breaks = (c(0, 0.25, 0.5, 0.75, 1)), limits = c(-0.2,1.1)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0,3500)) +
  labs(title = "Average fixation preference across conditions and windows",
       subtitle = "semitransparent lines represent averages of individual participants\n",
       y = "Average fixation preference for target referents\n"
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
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.title.x = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        plot.margin = unit(c(1,1,1,1),"cm"))


# store plot 
setwd("../plots/")
ggsave(filename = "Fix_agg_2responses.pdf",
       plot = Fix_agg_2responses,
       device = "pdf",
       width = 450, 
       height = 300,
       units = "mm",
       #bg = "transparent",
       dpi = 300)



# Same with facetted response for better overview: 2 responses
Fix_agg_4responses_facet <- 
  ggplot(data = xagg_subj[!xagg_subj$response %in%  c("Given Object", "Given Subject"),], aes(x = window, y = prop, color = response, fill = response)) +
  geom_segment(x = -Inf, y = 0.25, xend = Inf, yend = 0.25,
               lty = "dashed", size = 1, colour = "black") +
  geom_line(aes(group = interaction(ID, response)),
            alpha = 0.1, size = 1) +
  geom_line(data = xagg[!xagg$response %in%  c("Given Object", "Given Subject"),], aes(group = interaction(response)),
            size = 2) +
  geom_point(alpha = 0.1, size = 2) +
  geom_point(data = xagg[!xagg$response %in%  c("Given Object", "Given Subject"),], 
             size = 3, pch = 21, stroke = 1, color = "black") +
  facet_grid(response ~ Condition) +
  scale_colour_manual(values = c(DistrCol, ObjCompCol, SubjCompCol, TargetCol)) +
  scale_fill_manual(values = c(DistrCol, ObjCompCol, SubjCompCol, TargetCol)) +
  scale_y_continuous(expand = c(0, 0), breaks = (c(0, 0.25, 0.5, 0.75, 1)), limits = c(0,1)) +
  labs(title = "Proportion of looks across conditions and windows",
       subtitle = "semitransparent points and lines represent individual participants\n",
       y = "Proportion of fixation duration\n",
       x = "\nTime window"
  ) +
  theme_classic() + 
  theme(legend.position = "right",
        legend.key.height = unit(2,"line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.background = element_rect(fill = "transparent"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        panel.spacing = unit(2, "lines"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent"),
        axis.line.x = element_blank(),
        axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        plot.margin = unit(c(1,1,1,1),"cm"))

# store plot 
setwd("../plots/")
ggsave(filename = "Fix_agg_4responses_facet.pdf",
       plot = Fix_agg_4responses_facet,
       device = "pdf",
       width = 300, 
       height = 250,
       units = "mm",
       #bg = "transparent",
       dpi = 300)


# Plot aggregated fixations for Given Subject and Given Object as a function of trial bin
Fix_agg_2responses_trialBin <- 
  ggplot(data = xagg_subj_trialBin[xagg_subj_trialBin$response %in%  c("Given Object", "Given Subject"),], 
         aes(x = window, y = prop, color = response, fill = response)) +
  geom_segment(x = -Inf, y = 0.5, xend = Inf, yend = 0.5,
               lty = "dashed", size = 1, colour = "black") +
  geom_line(aes(group = interaction(ID, response)),
            alpha = 0.05, size = 1) +
  geom_line(data = xagg_trialBin[xagg_trialBin$response %in%  c("Given Object", "Given Subject"),], aes(group = interaction(response)),
            size = 2) +
  geom_point(alpha = 0.05, size = 2) +
  geom_point(data = xagg_trialBin[xagg_trialBin$response %in%  c("Given Object", "Given Subject"),], 
             size = 3, pch = 21, stroke = 1, color = "black") +
  facet_grid(trial_bin~ Condition) +
  scale_colour_manual(values = c(ObjCompCol, TargetCol)) +
  scale_fill_manual(values = c(ObjCompCol, TargetCol)) +
  scale_y_continuous(expand = c(0, 0), breaks = (c(0, 0.25, 0.5, 0.75, 1)), limits = c(0,1)) +
  labs(title = "Proportion of looks across conditions and windows",
       subtitle = "semitransparent points and lines represent individual participants\n",
       y = "Proportion of fixation duration\n",
       x = "\nTime window"
  ) +
  theme_classic() + 
  theme(legend.position = "right",
        legend.key.height = unit(2,"line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.background = element_rect(fill = "transparent"),
        strip.background = element_blank(),
        strip.text = element_text(size = 16),
        panel.spacing = unit(2, "lines"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent"),
        axis.line.x = element_blank(),
        axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        plot.margin = unit(c(1,1,1,1),"cm"))

# store plot 
setwd("../plots/")
ggsave(filename = "Fix_agg_2responses_trialBin.pdf",
       plot = Fix_agg_2responses_trialBin,
       device = "pdf",
       width = 290, 
       height = 400,
       units = "mm",
       #bg = "transparent",
       dpi = 300)


# Plot fixations as developing over time (ugly, but good so quickly assess)
ggplot(data = data[!data$response %in%  c("Given Object", "Given Subject"),], aes(x = eyetrial, y = proportion, color = response, fill = response)) +
  geom_line(aes(group = interaction(ID, response)), alpha = 0.05) +
  geom_smooth(data = xagg_trials[!xagg_trials$response %in%  c("Given Object", "Given Subject"),], aes(y = prop, group = response)) +
  facet_grid(window ~ Condition)
              
ggplot(data = data[data$response %in%  c("Given Object", "Given Subject"),], aes(x = eyetrial, y = proportion, color = response, fill = response)) +
  geom_line(aes(group = interaction(ID, response)), alpha = 0.05) +
  geom_smooth(data = xagg_trials[xagg_trials$response %in%  c("Given Object", "Given Subject"),], aes(y = prop, group = response)) +
  facet_grid(window ~ Condition)

