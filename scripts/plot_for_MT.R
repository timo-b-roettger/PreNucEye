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
d2$ID <- d2$subject_nr

# Load processed data eye-tracking data for exclusion criteria
data <- read_csv("ret_processed_stage_2.csv")

# merge
d2 <- full_join(d2, data)

#d2 <- d2[d2]

# read in acoustic landmarks for plotting
setwd("../data/")
landmarks <- read.csv("acoustic_landmarks.csv")
d2 <- full_join(d2, landmarks, by = c("Target_obj","Condition"))

# exclude subjects
d2 <- d2 %>% 
  filter(excludeTriggerError == 0,
         excludeRefError == 0, 
         excludeLargeError == 0,
         lgerror == 0)

# rotate coordinates according to Target_pos to top left
d2$xpos_r <- ifelse(d2$Target_pos == "TL_Pic", -d2$ypos, 
                    ifelse(d2$Target_pos == "BL_Pic", -d2$xpos,
                           ifelse(d2$Target_pos == "BR_Pic", d2$ypos,
                           d2$xpos)))

d2$ypos_r <- ifelse(d2$Target_pos == "TL_Pic", d2$xpos, 
                    ifelse(d2$Target_pos == "BL_Pic", -d2$ypos,
                           ifelse(d2$Target_pos == "BR_Pic", -d2$xpos,
                           d2$ypos)))

levels(d2$Target_pos) <- c("Bottom left", "Bottom right", "Top left", "Top right")
d2$Target_pos <- factor(d2$Target_pos, levels = c("Top left", "Top right",
                                                  "Bottom left", "Bottom right"))

# aggregate mean trajectories
xagg <-
  d2 %>%
  filter(!is.na(Target_pos)) %>% 
  group_by(steps, Condition, Target_pos) %>%
  summarise(mean_xpos = mean(xpos, na.rm = T),
            mean_ypos = mean(ypos, na.rm = T),
            mean_time = mean(timestamps, na.rm = T),
            mean_prenuc = mean(prenuclear_onset),
            mean_ref = mean(referent_onset),
            mean_adv = mean(adverb_onset))

# reverse y-axis
xagg <- xagg %>% 
  mutate(mean_ypos = -mean_ypos)
         
xagg_r_subjects <-
  d2 %>%
  filter(!is.na(Target_pos)) %>% 
  mutate(subject_nr = as.factor(subject_nr)) %>% 
  group_by(steps, Condition, subject_nr) %>%
  summarise(mean_xpos = mean(xpos_r, na.rm = T),
            mean_ypos = mean(ypos_r, na.rm = T),
            mean_time = mean(timestamps, na.rm = T),
            mean_prenuc = mean(prenuclear_onset),
            mean_ref = mean(referent_onset),
            mean_adv = mean(adverb_onset))

xagg_r <-
  d2 %>%
  filter(!is.na(Target_pos)) %>% 
  group_by(steps, Condition) %>%
  summarise(mean_xpos = mean(xpos_r, na.rm = T),
            mean_ypos = mean(ypos_r, na.rm = T),
            mean_time = mean(timestamps, na.rm = T),
            mean_prenuc = mean(prenuclear_onset),
            mean_ref = mean(referent_onset),
            mean_adv = mean(adverb_onset))


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
        strip.text = element_text(size = 18, hjust = 0),
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

# define start point
startDF <- data.frame(x = 0, y = 0)

space_facet <- 
ggplot(xagg, aes(x = mean_xpos, y = mean_ypos, colour = Condition, fill = Condition)) +
  geom_point(data = startDF, aes(x = x, y = y), inherit.aes = FALSE, size = 10, alpha = 0.2, colour = "grey") +
  geom_point(data = startDF, aes(x = x, y = y), pch = 21, inherit.aes = FALSE, size = 10, colour = "black") +
  geom_segment(x = -Inf, xend = Inf, y = 0, yend = 0, lty = "dashed", colour = "grey") +
  geom_segment(y = -Inf, yend = Inf, x = 0, xend = 0, lty = "dashed", colour = "grey") +
  geom_path() +
  geom_point(size = 1.5, colour = "black", stroke = 0.1, show.legend = F, shape = 21) +
  scale_colour_manual("Condition",
                      guide = guide_legend(title = "Condition"),
                      values = c(lexical.col, object.col, verb.col)) +
  scale_fill_manual("Condition",
                    guide = guide_legend(title = "Condition"),
                    values = c(lexical.col, object.col, verb.col)) +
  #scale_x_continuous(breaks = (c(-1,-0.5,0,0.5,1)), limits = c(-1.2,1.2)) + 
  #scale_y_continuous(breaks = (c(300,200,100,0,-50)), limits = c(300, -50)) + 
  facet_wrap( ~ Target_pos, ncol= 2, scale = "free") +
  labs(title = "Mean trajectories",
       #subtitle = "semi-transparent lines are subject averages\n",
       x = "\nhorizontal cursor position", 
       y = "vertical cursor position\n"
  ) + 
  guides(colour = guide_legend("Condition", override.aes = list(size = 5))) + 
  theme_traj_time + 
  theme(strip.text.y = element_blank())


## without facets
space_agg <- 
ggplot(xagg_r, aes(x = mean_xpos, y = -mean_ypos, colour = Condition, fill = Condition)) +
  geom_point(data = startDF, aes(x = x, y = y), inherit.aes = FALSE, size = 10, alpha = 0.2, colour = "grey") +
  geom_point(data = startDF, aes(x = x, y = y), pch = 21, inherit.aes = FALSE, size = 10, colour = "black") +
  annotate("text", x = -20, y = 300, label = "object competitor", angle = 90,
           size  = 9, color = "grey", alpha = 0.8, hjust = 1) +
  annotate("text", x = 300, y = -20, label = "subject competitor",
           size  = 9, color = "grey", alpha = 0.8, hjust = 1) +
  geom_segment(x = -Inf, xend = Inf, y = 0, yend = 0, lty = "dashed", colour = "grey") +
  geom_segment(y = -Inf, yend = Inf, x = 0, xend = 0, lty = "dashed", colour = "grey") +
  geom_path() +
  geom_point(size = 1.5, colour = "black", stroke = 0.5, show.legend = F, shape = 21) +
  #geom_point(data = xagg_r_subjects, size = 1, alpha = 0.3) +
  #geom_path(data = xagg_r_subjects, aes(group = interaction(subject_nr, Condition)), size = 0.2, alpha = 0.3) +
  scale_colour_manual("Condition",
                      guide = guide_legend(title = "Condition"),
                      values = c(lexical.col, object.col, verb.col)) +
  scale_fill_manual("Condition",
                    guide = guide_legend(title = "Condition"),
                    values = c(lexical.col, object.col, verb.col)) +
  #facet_wrap( ~ Target_pos, ncol= 2, scale = "free") +
  #scale_x_continuous(breaks = (c(-50,0,100,200,300)), limits = c(-50,300)) + 
  #scale_y_continuous(breaks = (c(-50,0,100,200,300)), limits = c(-50,300)) + 
  labs(title = "Mean trajectories",
       #subtitle = "semi-transparent lines are subject averages\n",
       x = "\nhorizontal cursor position", 
       y = "vertical cursor position\n"
  ) + 
  guides(colour = guide_legend("Condition", override.aes = list(size = 5))) + 
  theme_traj_time + 
  theme(strip.text.y = element_blank())

############################
## plotting xpos and time ##
############################

xpos_time <-
ggplot(xagg_r, aes(x = mean_time, y = mean_xpos, colour = Condition, fill = Condition)) +
  geom_segment(x = -Inf, xend = Inf, y = 0, yend = 0, lty = "dashed", colour = "grey") +
  geom_segment(y = -Inf, yend = Inf, x = 0, xend = 0, lty = "dashed", colour = "grey") +
  annotate("text", x = mean(xagg_r$mean_prenuc)/2, y = -70, label = "Now click on the",
           size  = 4) +
  annotate("text", x = ((mean(xagg_r$mean_ref) - mean(xagg_r$mean_prenuc)) / 2) + mean(xagg_r$mean_prenuc),
           y = -70, label = "THINGY that dreams about the", size  = 4) +
  annotate("text", x = ((mean(xagg_r$mean_adv) - mean(xagg_r$mean_ref)) / 2) + mean(xagg_r$mean_ref),
           y = -70, label = "REFERENT", size  = 4) +
  annotate("text", x = mean(xagg_r$mean_adv) + 50,
           y = -70, label = "again/instead", size  = 4, hjust = 0) +
  geom_segment(x = mean(xagg_r$mean_prenuc), xend = mean(xagg_r$mean_prenuc), y = -Inf, yend = Inf, lty = "dashed", colour = "grey") +
  geom_segment(x = mean(xagg_r$mean_ref), xend = mean(xagg_r$mean_ref), y = -Inf, yend = Inf, lty = "dashed", colour = "grey") +
  geom_segment(x = mean(xagg_r$mean_adv), xend = mean(xagg_r$mean_adv), y = -Inf, yend = Inf, lty = "dashed", colour = "grey") +
  geom_path() +
  geom_point(size = 1.5, colour = "black", stroke = 0.1, show.legend = F, shape = 21) +
  scale_colour_manual("Condition",
                      guide = guide_legend(title = "Condition"),
                      values = c(lexical.col, object.col, verb.col)) +
  scale_fill_manual("Condition",
                    guide = guide_legend(title = "Condition"),
                    values = c(lexical.col, object.col, verb.col)) +
  scale_x_continuous(breaks = (c(0,1000,2000,3000)), limits = c(0,3500)) + 
  scale_y_continuous(breaks = (c(-50,0,100,200,300)), limits = c(-100,300)) + 
  #facet_wrap( ~ Target_pos, ncol= 2, scale = "free") +
  labs(title = "Mean horizontal cursor position",
       #subtitle = "semi-transparent lines are subject averages\n",
       x = "\ntime in ms", 
       y = "horizontal cursor position\n"
  ) + 
  guides(colour = guide_legend("Condition", override.aes = list(size = 5))) + 
  theme_traj_time + 
  theme(strip.text.y = element_blank())

############################
## plotting ypos and time ##
############################

ypos_time <- 
ggplot(xagg_r, aes(x = mean_time, y = -mean_ypos, colour = Condition, fill = Condition)) +
  geom_segment(x = -Inf, xend = Inf, y = 0, yend = 0, lty = "dashed", colour = "grey") +
  geom_segment(y = -Inf, yend = Inf, x = 0, xend = 0, lty = "dashed", colour = "grey") +
  annotate("text", x = mean(xagg_r$mean_prenuc)/2, y = -70, label = "Now click on the",
           size  = 4) +
  annotate("text", x = ((mean(xagg_r$mean_ref) - mean(xagg_r$mean_prenuc)) / 2) + mean(xagg_r$mean_prenuc),
           y = -70, label = "THINGY that dreams about the", size  = 4) +
  annotate("text", x = ((mean(xagg_r$mean_adv) - mean(xagg_r$mean_ref)) / 2) + mean(xagg_r$mean_ref),
           y = -70, label = "REFERENT", size  = 4) +
  annotate("text", x = mean(xagg_r$mean_adv) + 50,
           y = -70, label = "again/instead", size  = 4, hjust = 0) +
  geom_segment(x = mean(xagg_r$mean_prenuc), xend = mean(xagg_r$mean_prenuc), y = -Inf, yend = Inf, lty = "dashed", colour = "grey") +
  geom_segment(x = mean(xagg_r$mean_ref), xend = mean(xagg_r$mean_ref), y = -Inf, yend = Inf, lty = "dashed", colour = "grey") +
  geom_segment(x = mean(xagg_r$mean_adv), xend = mean(xagg_r$mean_adv), y = -Inf, yend = Inf, lty = "dashed", colour = "grey") +
  geom_path() +
  geom_point(size = 1.5, colour = "black", stroke = 0.1, show.legend = F, shape = 21) +
  scale_colour_manual("Condition",
                      guide = guide_legend(title = "Condition"),
                      values = c(lexical.col, object.col, verb.col)) +
  scale_fill_manual("Condition",
                    guide = guide_legend(title = "Condition"),
                    values = c(lexical.col, object.col, verb.col)) +
  scale_x_continuous(breaks = (c(0,1000,2000,3000)), limits = c(0,3500)) + 
  scale_y_continuous(breaks = (c(-50,0,100,200,300)), limits = c(-100,300)) + 
  #facet_wrap( ~ Target_pos, ncol= 2, scale = "free") +
  labs(title = "Mean vertical cursor position",
       #subtitle = "semi-transparent lines are subject averages\n",
       x = "\ntime in ms", 
       y = "vertical cursor position\n"
  ) + 
  guides(colour = guide_legend("Condition", override.aes = list(size = 5))) + 
  theme_traj_time + 
  theme(strip.text.y = element_blank())


# store plot 
setwd("../plots/")
ggsave(filename = "space_facet.pdf",
       plot = space_facet,
       device = "pdf",
       width = 200, 
       height = 200,
       units = "mm",
       #bg = "transparent",
       dpi = 300)

ggsave(filename = "space_agg.pdf",
       plot = space_agg,
       device = "pdf",
       width = 200, 
       height = 200,
       units = "mm",
       #bg = "transparent",
       dpi = 300)

ggsave(filename = "xpos_time.pdf",
       plot = xpos_time,
       device = "pdf",
       width = 300, 
       height = 200,
       units = "mm",
       #bg = "transparent",
       dpi = 300)

ggsave(filename = "ypos_time.pdf",
       plot = ypos_time,
       device = "pdf",
       width = 300, 
       height = 200,
       units = "mm",
       #bg = "transparent",
       dpi = 300)


