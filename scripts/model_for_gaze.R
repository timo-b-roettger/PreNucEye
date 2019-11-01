############
### Info ###
############
#
## Authors:  Dan Turner (dturner @ northwestern.edu)
#            Timo Roettger (timo.b.roettger @ gmail.com)
#
## Project: Eye tracking during prenuclear pitch accent comprehension
#
##          Modelling
#
##          Model the proportional preferences
#
## Version: 11/1/2019


#############
### Setup ###
#############

library(tidyverse)
library(ggbeeswarm)
library(rstudioapi)
library(brms)
# library(ggpubr)


## Getting the path of your current open file
datapath = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(datapath))
setwd("../processed/")

# Load processed data (OpenSesame (+ Mouse Tracking) + Eye Tracking)
data <- read_csv("ret_processed_stage_2.csv")

# Join the landmarks and data
data <- data %>% 
  filter(!is.na(window)) %>% 
  # exclusion
  filter(excludeTriggerError == 0,
         excludeRefError == 0, 
         excludeLargeError == 0,
         lgerror == 0) %>% 
  mutate(window = factor(window, levels = c("early", "prenuclear", "nuclear", "adverb"))) %>% 
  # generate binary preference score
  mutate(subj_preference = ifelse(((Target_prop + ObjComp_prop) / 2) > ((SubjComp_prop + Distr_prop) / 2), 1, 0),
         obj_preference = ifelse(((Target_prop + SubjComp_prop) / 2) > ((ObjComp_prop + Distr_prop) / 2), 1, 0),
         # centralize eyetrial
         eyetrial.c = scale(eyetrial, scale = T))


#############
### Model ###
#############

# set priors
priors_gaze <- c(
  prior(student_t(5, 0, 2), class = Intercept),
  prior(student_t(5, 0, 2), class = b),
  prior(student_t(4, 0, 2), class = sd),
  prior(student_t(4, 0, 2), class = sigma),
  prior(lkj(2), class = cor)
)

# model subject preference
xmdl_subj <- brm(subj_preference ~ Condition * window * eyetrial.c + 
              # specify maximal model for IDs (no correlation assumed)
              (1 + Condition * window * eyetrial.c | ID) + 
              (1 + Condition * window * eyetrial.c | Target_obj),
              family = "bernoulli", 
            inits = 0, 
            chains = 4,
            iter = 4000,
            cores = 28,
            control = list(adapt_delta = 0.99),
            data = data)

# model object preference
xmdl_obj <- brm(obj_preference ~ Condition * window * eyetrial + 
                # specify maximal model for IDs (no correlation assumed)
                  (1 + Condition * window * eyetrial.c | ID) + 
                  (1 + Condition * window * eyetrial.c | Target_obj),
                family = "bernoulli", 
                inits = 0, 
                chains = 4,
                iter = 4000,
                cores = 28,
                control = list(adapt_delta = 0.99),
                data = data)

system("killall R")

## save models for later use
setwd("../models/")
save(xmdl_subj, 
     xmdl_obj, 
     file = "Bayesian_models.RData")


## extract posteriors
psamples_subj = posterior_samples(xmdl_subj) %>% 
  mutate(
    # calculate fixatins in the middle of experiment (eyetrial.c = 0)
    CG_early = b_Intercept,
    GC_early = CG_early + b_ConditionGC,
    GG_early = CG_early + b_ConditionGG,
    CG_prenuc = CG_early + b_windowprenuclear,
    GC_prenuc = CG_prenuc + b_ConditionGC + `b_ConditionGC:windowprenuclear`,
    GG_prenuc = CG_prenuc + b_ConditionGG + `b_ConditionGG:windowprenuclear`,
    CG_nuclear = CG_early + b_windownuclear,
    GC_nuclear = CG_nuclear + b_ConditionGC + `b_ConditionGC:windownuclear`,
    GG_nuclear = CG_nuclear + b_ConditionGG + `b_ConditionGG:windownuclear`,
    CG_adverb = CG_early + b_windowadverb,
    GC_adverb = CG_adverb + b_ConditionGC + `b_ConditionGC:windowadverb`,
    GG_adverb = CG_adverb + b_ConditionGG + `b_ConditionGG:windowadverb`,
    
    # calculate trial number slope
    CG_early_trial = b_eyetrial.c,
    GC_early_trial = CG_early_trial + `b_ConditionGC:eyetrial.c`,
    GG_early_trial = CG_early_trial + `b_ConditionGG:eyetrial.c`,
    CG_prenuc_trial = CG_early_trial + `b_windowprenuclear:eyetrial.c`,
    GC_prenuc_trial = CG_prenuc_trial + `b_ConditionGC:windowprenuclear:eyetrial.c`,
    GG_prenuc_trial = CG_prenuc_trial + `b_ConditionGG:windowprenuclear:eyetrial.c`,
    CG_nuclear_trial = CG_early_trial + `b_windownuclear:eyetrial.c`,
    GC_nuclear_trial = CG_nuclear_trial + `b_ConditionGC:windownuclear:eyetrial.c`,
    GG_nuclear_trial = CG_nuclear_trial + `b_ConditionGG:windownuclear:eyetrial.c`,
    CG_adverb_trial = CG_early_trial + `b_windowadverb:eyetrial.c`,
    GC_adverb_trial = CG_adverb_trial + `b_ConditionGC:windowadverb:eyetrial.c`,
    GG_adverb_trial = CG_adverb_trial + `b_ConditionGG:windowadverb:eyetrial.c`,
    
    # calculate diffs within condition
    diff_CG_early_prenuc = CG_early - CG_prenuc,
    diff_CG_prenuc_nuc = CG_prenuc - CG_nuclear,
    diff_CG_nuc_adverb = CG_nuclear - CG_adverb,
    
    diff_GC_early_prenuc = GC_early - GC_prenuc,
    diff_GC_prenuc_nuc = GC_prenuc - GC_nuclear,
    diff_GC_nuc_adverb = GC_nuclear - GC_adverb,
    
    diff_GG_early_prenuc = GG_early - GG_prenuc,
    diff_GG_prenuc_nuc = GG_prenuc - GG_nuclear,
    diff_GG_nuc_adverb = GG_nuclear - GG_adverb
    )

# define columns for loop
col_names1 = c("CG_early", "GC_early", "GG_early",
              "CG_prenuc", "GC_prenuc", "GG_prenuc",
              "CG_nuclear", "GC_nuclear", "GG_nuclear",
              "CG_adverb", "GC_adverb", "GG_adverb",
              "CG_early_trial", "GC_early_trial", "GG_early_trial",
              "CG_prenuc_trial", "GC_prenuc_trial", "GG_prenuc_trial",
              "CG_nuclear_trial", "GC_nuclear_trial", "GG_nuclear_trial",
              "CG_adverb_trial", "GC_adverb_trial", "GG_adverb_trial",
              "diff_CG_early_prenuc", "diff_CG_prenuc_nuc", "diff_CG_nuc_adverb",
              "diff_GC_early_prenuc", "diff_GC_prenuc_nuc", "diff_GC_nuc_adverb",
              "diff_GG_early_prenuc", "diff_GG_prenuc_nuc", "diff_GG_nuc_adverb"
              )

# create empty vectors
name <- c()
lci <- c()
uci <- c()
mean <- c()
probs <- c()

# loop through col_names1 to extract posterior mean, 95% CI and Pr(beta < 0)
# for (i in 1:length(col_names1)) {
#   lci <- c(lci, round(plogis(coda::HPDinterval(as.mcmc(psamples_subj[[col_names1[i]]]))), 2)[1])
#   uci <- c(uci, round(plogis(coda::HPDinterval(as.mcmc(psamples_subj[[col_names1[i]]]))), 2)[2])
#   mean <- c(mean, round(plogis(mean(psamples_subj[[col_names1[i]]])), 2))
#   name <- c(name, col_names1[i])
#   probs <- c(probs, round(length(which(psamples_subj[[col_names1[i]]] < 0)) / length(psamples_subj[[col_names1[i]]]),2))
# }

for (i in 1:length(col_names1)) {
  lci <- c(lci, round(coda::HPDinterval(as.mcmc(psamples_subj[[col_names1[i]]])), 2)[1])
  uci <- c(uci, round(coda::HPDinterval(as.mcmc(psamples_subj[[col_names1[i]]])), 2)[2])
  mean <- c(mean, round(mean(psamples_subj[[col_names1[i]]]), 2))
  name <- c(name, col_names1[i])
  probs <- c(probs, round(length(which(psamples_subj[[col_names1[i]]] < 0)) / length(psamples_subj[[col_names1[i]]]),2))
}

# save parameters for absolute TTTs
posteriors_1 = data.frame(name, lci, uci, mean, probs)

posteriors_1$cond <- as.factor(c(rep(c("prenuclear", "nuclear", "both"), 8), rep("", 9 )))
posteriors_1$window <- as.factor(c(rep(c("early", "prenuclear", "nuclear", "adverb"), each = 3), 
                                   rep(c("early", "prenuclear", "nuclear", "adverb"), each = 3),
                                 rep(c("diff_early_prenuc", "diff_prenuc_nuc", "diff_nuc_adverb"), 3)))
posteriors_1$type <- as.factor(c(rep("estimate", 12), rep("slope", 12), rep("diff", 9)))


posteriors_1$window = factor(posteriors_1$window , levels = c("early", "prenuclear", "nuclear", "adverb",
                                "diff_early_prenuc", "diff_prenuc_nuc", "diff_nuc_adverb"))

# plot quick and dirty
ggplot(posteriors_1[posteriors_1$type == "estimate",], aes(x = window, y = mean)) +
  geom_segment(x = -Inf, y = 0.5, xend = Inf, yend = 0.5,
               lty = "dashed", size = 1, colour = "black") +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.2) +
  geom_line(aes(group = interaction(cond)), colour = "grey") +
  geom_point(size = 3) +
  facet_grid(~cond) +
  scale_y_continuous(expand = c(0, 0), breaks = (c(0, 0.25, 0.5, 0.75, 1)), limits = c(0,1)) +
  labs(title = "Estimated preference of looks to given subject across conditions and windows",
       #subtitle = "semitransparent points and lines represent individual participants\n",
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




