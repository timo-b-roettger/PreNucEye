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

# Order factor levels of window
data$window <- factor(data$window, levels = c("early", "prenuclear", "nuclear", "adverb"))

# Gather proportional values
data <- data %>% 
  gather(response, proportion, 8:13) 

# Rename response levels
data <- data %>% 
  mutate(response = as.factor(response),
         response = fct_recode(response, Target = "Target_prop",
                               `Object Competitor` = "ObjComp_prop",
                               `Subject Competitor` =  "SubjComp_prop", 
                               Distractor = "Distr_prop",
                               `Given Subject` = "GivenSubj_prop",
                               `Given Object` = "GivenObj_prop"),
         Condition = as.factor(Condition),
         Condition = fct_recode(Condition, `Subject accent` = "CG",
                                `Object accent` = "GC",
                                `Accent on both` =  "GG"))
         
#################
### Aggregate ###
#################

# aggregate proportions (per subject)
xagg_subj <- data %>% 
  group_by(Condition, window, ID, response) %>% 
  summarise(prop = mean(proportion, na.rm = T))

# aggregate proportions (overall)
xagg <- xagg_subj %>% 
  group_by(Condition, window, response) %>% 
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


# Plot aggregated fixations for 4 categories
Fix_agg_4responses <- 
ggplot(data = xagg_subj[!xagg_subj$response %in%  c("Given Object", "Given Subject"),], 
       aes(x = window, y = prop, color = response, fill = response)) +
  geom_segment(x = -Inf, y = 0.25, xend = Inf, yend = 0.25,
               lty = "dashed", size = 1, colour = "black") +
  geom_line(aes(group = interaction(ID, response)),
            alpha = 0.2, size = 1) +
  geom_line(data = xagg[!xagg$response %in%  c("Given Object", "Given Subject"),], aes(group = interaction(response)),
            size = 2) +
  geom_point(alpha = 0.2, size = 2) +
  geom_point(data = xagg[!xagg$response %in%  c("Given Object", "Given Subject"),], 
             size = 3, pch = 21, stroke = 1, color = "black") +
  facet_grid(~ Condition) +
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
ggsave(filename = "Fix_agg_4responses.pdf",
       plot = Fix_agg_4responses,
       device = "pdf",
       width = 290, 
       height = 150,
       units = "mm",
       #bg = "transparent",
       dpi = 300)

# Plot aggregated fixations for Given Subject and Given Object only
Fix_agg_2responses <- 
  ggplot(data = xagg_subj[xagg_subj$response %in%  c("Given Object", "Given Subject"),], 
         aes(x = window, y = prop, color = response, fill = response)) +
  geom_segment(x = -Inf, y = 0.5, xend = Inf, yend = 0.5,
               lty = "dashed", size = 1, colour = "black") +
  geom_line(aes(group = interaction(ID, response)),
            alpha = 0.2, size = 1) +
  geom_line(data = xagg[xagg$response %in%  c("Given Object", "Given Subject"),], aes(group = interaction(response)),
            size = 2) +
  geom_point(alpha = 0.2, size = 2) +
  geom_point(data = xagg[xagg$response %in%  c("Given Object", "Given Subject"),], 
             size = 3, pch = 21, stroke = 1, color = "black") +
  facet_grid(~ Condition) +
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
ggsave(filename = "Fix_agg_2responses.pdf",
       plot = Fix_agg_2responses,
       device = "pdf",
       width = 290, 
       height = 150,
       units = "mm",
       #bg = "transparent",
       dpi = 300)


# Same with facetted response for better overview: 2 responses
Fix_agg_4responses_facet <- 
  ggplot(data = xagg_subj[!xagg_subj$response %in%  c("Given Object", "Given Subject"),], aes(x = window, y = prop, color = response, fill = response)) +
  geom_segment(x = -Inf, y = 0.25, xend = Inf, yend = 0.25,
               lty = "dashed", size = 1, colour = "black") +
  geom_line(aes(group = interaction(ID, response)),
            alpha = 0.2, size = 1) +
  geom_line(data = xagg[!xagg$response %in%  c("Given Object", "Given Subject"),], aes(group = interaction(response)),
            size = 2) +
  geom_point(alpha = 0.2, size = 2) +
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

# Plot fixations as developing over time
ggplot(data = data, aes(x = eyetrial, y = proportion, color = response, fill = response)) +
  geom_line(aes(group = interaction(ID, response)), alpha = 0.1) +
  geom_smooth(data = xagg_trials, aes(y = prop, group = response)) +
  facet_grid(window ~ Condition)
              



