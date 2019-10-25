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

library(tidyverse)
library(ggbeeswarm)
library(rstudioapi)


# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
setwd("../processed/")

## data set for estimates

## data set for plotting trajectories
d2 <- read.csv("df_data.csv")
d2 <- d2[d2$responded_correct == TRUE,]
#d2 <- d2[d2]

# read in acoustic landmarks for plotting
#landmarks <- read.csv("segmental_landmarks.csv")
#d2 <- full_join(d2, landmarks, by = c("Target","Condition"))

# remap coordinates according to Target_pos to upper left
d2$xpos_r <- ifelse(d2$Target_pos == "TL_Pic", d2$xpos * -1, 
                    ifelse(d2$Target_pos == "BL_Pic", d2$xpos * -1,
                           d2$xpos))

d2$ypos_r <- ifelse(d2$Target_pos == "TR_Pic", d2$ypos * -1, 
                    ifelse(d2$Target_pos == "TL_Pic", d2$ypos * -1,
                           d2$ypos))

# aggregate mean trajectories
xagg <-
  d2 %>%
  group_by(steps, Condition, Target_pos) %>%
  summarise(mean_xpos = mean(xpos, na.rm = T),
            mean_ypos = mean(ypos, na.rm = T),
            mean_time = mean(timestamps, na.rm = T))

xagg_r_subjects <-
  d2 %>%
  group_by(steps, Condition, subject_nr) %>%
  summarise(mean_xpos = mean(xpos_r, na.rm = T),
            mean_ypos = mean(ypos_r, na.rm = T),
            mean_time = mean(timestamps, na.rm = T))

xagg_r <-
  d2 %>%
  group_by(steps, Condition, Target_pos) %>%
  summarise(mean_xpos = mean(xpos_r, na.rm = T),
            mean_ypos = mean(ypos_r, na.rm = T),
            mean_time = mean(timestamps, na.rm = T))


# aggregate mean trajectories for individual subjects
# xagg_subjects <-
#   d2 %>%
#   group_by(steps, Condition, subject_nr) %>%
#   summarise(mean_xpos = mean(xpos, na.rm = T),
#             mean_ypos = mean(ypos, na.rm = T),
#             mean_time = mean(timestamps, na.rm = T))


# store theme for trajectory_time plots
theme_traj_time <- 
  theme_classic() + 
  theme(legend.position = "right",
        legend.key.height = unit(2,"line"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.background = element_rect(fill = "transparent"),
        strip.background = element_blank(),
        panel.spacing = unit(2, "lines"),
        panel.border = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent"),
        strip.text.y = element_text(size = 18, hjust = 0),
        axis.text = element_text(size = 16),
        axis.line = element_blank(),
        axis.title = element_text(size = 18, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"),
        plot.margin = unit(c(0.3,0.3,0.3,0.3),"cm"))

## define colors for plots
subject.col = "#ca0020"
verb.col = "#f4a582"
object.col = "#0571b0"
lexical.col = "#000000"

############################
## plotting xpos and ypos ##
############################
quartz()
  ggplot(xagg, aes(x = mean_xpos, y = mean_ypos, colour = Condition, fill = Condition)) +
  # geom_path(data = xagg_subjects, 
  #           aes(x = -mean_xpos, y = mean_ypos, colour = Focus, group = interaction(subject_nr, Focus)), 
  #           alpha = 0.3, inherit.aes = FALSE) +
  geom_path() +
  geom_point(size = 1.5, colour = "black", stroke = 0.5, show.legend = F, shape = 21) +
  scale_colour_manual("Condition",
                      guide = guide_legend(title = "Condition"),
                      values = c(lexical.col, object.col, verb.col)) +
  scale_fill_manual("Condition",
                    guide = guide_legend(title = "Condition"),
                    values = c(lexical.col, object.col, verb.col)) +
  #scale_x_continuous(breaks = (c(-1,-0.5,0,0.5,1)), limits = c(-1.2,1.2)) + 
  #scale_y_continuous(breaks = (c(-1,-0.5,0,0.5,1)), limits = c(-0.1,1.2)) + 
  facet_wrap( ~ Target_pos, ncol= 2, scale = "free") +
  labs(title = "Mean trajectories",
       subtitle = "semi-transparent lines are subject averages\n",
       x = "\nhorizontal cursor position", 
       y = "vertical cursor position\n"
  ) + 
  guides(colour = guide_legend("Condition", override.aes = list(size = 5))) + 
  theme_traj_time + 
  theme(strip.text.y = element_blank())

## without facets

ggplot(xagg_r, aes(x = mean_xpos, y = mean_ypos, colour = Condition, fill = Condition)) +
  # geom_path(data = xagg_subjects, 
  #           aes(x = -mean_xpos, y = mean_ypos, colour = Focus, group = interaction(subject_nr, Focus)), 
  #           alpha = 0.3, inherit.aes = FALSE) +
  geom_path() +
  geom_point(size = 1.5, colour = "black", stroke = 0.5, show.legend = F, shape = 21) +
  scale_colour_manual("Condition",
                      guide = guide_legend(title = "Condition"),
                      values = c(lexical.col, object.col, verb.col)) +
  scale_fill_manual("Condition",
                    guide = guide_legend(title = "Condition"),
                    values = c(lexical.col, object.col, verb.col)) +
  facet_wrap(~ Target_pos) +
  #scale_x_continuous(breaks = (c(-1,-0.5,0,0.5,1)), limits = c(-1.2,1.2)) + 
  #scale_y_continuous(breaks = (c(-1,-0.5,0,0.5,1)), limits = c(-0.1,1.2)) + 
  labs(title = "Mean trajectories",
       subtitle = "semi-transparent lines are subject averages\n",
       x = "\nhorizontal cursor position", 
       y = "vertical cursor position\n"
  ) + 
  guides(colour = guide_legend("Condition", override.aes = list(size = 5))) + 
  theme_traj_time + 
  theme(strip.text.y = element_blank())

############################
## plotting xpos and time ##
############################

#RF1_space_both <-
ggplot(xagg_r, aes(x = mean_time, y = mean_xpos, colour = Condition, fill = Condition)) +
  # geom_path(data = xagg_subjects, 
  #           aes(x = -mean_xpos, y = mean_ypos, colour = Focus, group = interaction(subject_nr, Focus)), 
  #           alpha = 0.3, inherit.aes = FALSE) +
  geom_path() +
  geom_point(size = 1.5, colour = "black", stroke = 0.5, show.legend = F, shape = 21) +
  scale_colour_manual("Condition",
                      guide = guide_legend(title = "Condition"),
                      values = c(lexical.col, object.col, verb.col)) +
  scale_fill_manual("Condition",
                    guide = guide_legend(title = "Condition"),
                    values = c(lexical.col, object.col, verb.col)) +
  #scale_x_continuous(breaks = (c(-1,-0.5,0,0.5,1)), limits = c(-1.2,1.2)) + 
  #scale_y_continuous(breaks = (c(-1,-0.5,0,0.5,1)), limits = c(-0.1,1.2)) + 
  facet_wrap( ~ Target_pos, ncol= 2, scale = "free") +
  labs(title = "Mean horizontal cursor position",
       subtitle = "semi-transparent lines are subject averages\n",
       x = "\ntime in ms", 
       y = "horizontal cursor position\n"
  ) + 
  guides(colour = guide_legend("Condition", override.aes = list(size = 5))) + 
  theme_traj_time + 
  theme(strip.text.y = element_blank())

############################
## plotting ypos and time ##
############################

ggplot(xagg_r, aes(x = mean_time, y = mean_ypos, colour = Condition, fill = Condition)) +
  # geom_path(data = xagg_subjects, 
  #           aes(x = -mean_xpos, y = mean_ypos, colour = Focus, group = interaction(subject_nr, Focus)), 
  #           alpha = 0.3, inherit.aes = FALSE) +
  geom_path() +
  geom_point(size = 1.5, colour = "black", stroke = 0.5, show.legend = F, shape = 21) +
  scale_colour_manual("Condition",
                      guide = guide_legend(title = "Condition"),
                      values = c(lexical.col, object.col, verb.col)) +
  scale_fill_manual("Condition",
                    guide = guide_legend(title = "Condition"),
                    values = c(lexical.col, object.col, verb.col)) +
  #scale_x_continuous(breaks = (c(-1,-0.5,0,0.5,1)), limits = c(-1.2,1.2)) + 
  #scale_y_continuous(breaks = (c(-1,-0.5,0,0.5,1)), limits = c(-0.1,1.2)) + 
  #facet_wrap( ~ Target_pos, ncol= 2, scale = "free") +
  labs(title = "Mean vertical cursor position",
       subtitle = "semi-transparent lines are subject averages\n",
       x = "\ntime in ms", 
       y = "vertical cursor position\n"
  ) + 
  guides(colour = guide_legend("Condition", override.aes = list(size = 5))) + 
  theme_traj_time + 
  theme(strip.text.y = element_blank())

