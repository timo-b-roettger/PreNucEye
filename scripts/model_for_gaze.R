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
## Version: 9/19/2019


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
  prior(student_t(4, 0, 2), class = Intercept),
  prior(student_t(4, 0, 2), class = b),
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
            cores = 4,
            control = list(adapt_delta = 0.99),
            data = data)

# model object preference
xmdl_obj <- brm(obj_preference ~ Condition * window * eyetrial.c + 
                # specify maximal model for IDs (no correlation assumed)
                  (1 + Condition * window * eyetrial.c | ID) + 
                  (1 + Condition * window * eyetrial.c | Target_obj),
                family = "bernoulli", 
                inits = 0, 
                chains = 4,
                iter = 4000,
                cores = 4,
                control = list(adapt_delta = 0.99),
                data = data)

system("killall R")

## save models for later use
setwd("../models/")
save(xmdl_subj, 
     xmdl_obj, 
     file = "Bayesian_models.RData")

##########################
### Extract posteriors ###
##########################

setwd("../models/")
load("Bayesian_models.RData")

## extract posteriors for subject preference
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
    diff_GG_nuc_adverb = GG_nuclear - GG_adverb,
    
    # calculate posteriors for beginning of experiment 
    CG_early_1st = CG_early + min(data$eyetrial.c) * CG_early_trial,
    GC_early_1st = GC_early + min(data$eyetrial.c) * GC_early_trial,
    GG_early_1st = GG_early + min(data$eyetrial.c) * GG_early_trial,
    CG_prenuc_1st = CG_prenuc + min(data$eyetrial.c) * CG_prenuc_trial,
    GC_prenuc_1st = GC_prenuc + min(data$eyetrial.c) * GC_prenuc_trial,
    GG_prenuc_1st = GG_prenuc + min(data$eyetrial.c) * GG_prenuc_trial,
    CG_nuclear_1st = CG_nuclear + min(data$eyetrial.c) * CG_nuclear_trial,
    GC_nuclear_1st = GC_nuclear + min(data$eyetrial.c) * GC_nuclear_trial,
    GG_nuclear_1st = GG_nuclear + min(data$eyetrial.c) * GG_nuclear_trial,
    CG_adverb_1st = CG_adverb + min(data$eyetrial.c) * CG_adverb_trial,
    GC_adverb_1st = GC_adverb + min(data$eyetrial.c) * GC_adverb_trial,
    GG_adverb_1st = GG_adverb + min(data$eyetrial.c) * GG_adverb_trial,
    
    # calculate posteriors for end of experiment 
    CG_early_last = CG_early + max(data$eyetrial.c) * CG_early_trial,
    GC_early_last = GC_early + max(data$eyetrial.c) * GC_early_trial,
    GG_early_last = GG_early + max(data$eyetrial.c) * GG_early_trial,
    CG_prenuc_last = CG_prenuc + max(data$eyetrial.c) * CG_prenuc_trial,
    GC_prenuc_last = GC_prenuc + max(data$eyetrial.c) * GC_prenuc_trial,
    GG_prenuc_last = GG_prenuc + max(data$eyetrial.c) * GG_prenuc_trial,
    CG_nuclear_last = CG_nuclear + max(data$eyetrial.c) * CG_nuclear_trial,
    GC_nuclear_last = GC_nuclear + max(data$eyetrial.c) * GC_nuclear_trial,
    GG_nuclear_last = GG_nuclear + max(data$eyetrial.c) * GG_nuclear_trial,
    CG_adverb_last = CG_adverb + max(data$eyetrial.c) * CG_adverb_trial,
    GC_adverb_last = GC_adverb + max(data$eyetrial.c) * GC_adverb_trial,
    GG_adverb_last = GG_adverb + max(data$eyetrial.c) * GG_adverb_trial
    
    )

# define columns for loop (subj_preference)
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
              "diff_GG_early_prenuc", "diff_GG_prenuc_nuc", "diff_GG_nuc_adverb",
              "CG_early_1st", "GC_early_1st", "GG_early_1st",
              "CG_prenuc_1st", "GC_prenuc_1st", "GG_prenuc_1st",
              "CG_nuclear_1st", "GC_nuclear_1st", "GG_nuclear_1st",
              "CG_adverb_1st", "GC_adverb_1st", "GG_adverb_1st",
              "CG_early_last", "GC_early_last", "GG_early_last",
              "CG_prenuc_last", "GC_prenuc_last", "GG_prenuc_last",
              "CG_nuclear_last", "GC_nuclear_last", "GG_nuclear_last",
              "CG_adverb_last", "GC_adverb_last", "GG_adverb_last"
              )

# create empty vectors(subj_preference)
name <- c()
lci <- c()
uci <- c()
mean <- c()
probs <- c()


# extract posterior means and 95% CIs in log odd space (subj_preference)
for (i in 1:length(col_names1)) {
  lci <- c(lci, round(coda::HPDinterval(as.mcmc(psamples_subj[[col_names1[i]]])), 2)[1])
  uci <- c(uci, round(coda::HPDinterval(as.mcmc(psamples_subj[[col_names1[i]]])), 2)[2])
  mean <- c(mean, round(mean(psamples_subj[[col_names1[i]]]), 2))
  name <- c(name, col_names1[i])
  probs <- c(probs, round(length(which(psamples_subj[[col_names1[i]]] < 0)) / length(psamples_subj[[col_names1[i]]]),2))
}

# save parameters  (subj_preference)
posteriors_1 = data.frame(name, lci, uci, mean, probs)

posteriors_1$cond <- as.factor(c(rep(c("1st NP accent", "2nd NP accent", "Both NPs have accents"), 8), rep("", 9 ), rep(c("1st NP accent", "2nd NP accent", "Both NPs have accents"), 8)))
posteriors_1$window <- as.factor(c(rep(c("early", "1st NP", "2nd NP", "adverb"), each = 3), 
                                   rep(c("early", "1st NP", "2nd NP", "adverb"), each = 3),
                                   rep(c("diff_early_1st", "diff_1st_2nd", "diff_2nd_adverb"), 3),
                                   rep(c("early", "1st NP", "2nd NP", "adverb"), each = 3),
                                   rep(c("early", "1st NP", "2nd NP", "adverb"), each = 3)))
posteriors_1$type <- as.factor(c(rep("estimate", 12), rep("slope", 12), rep("diff", 9), rep("estimate", 24)))


posteriors_1$window = factor(posteriors_1$window , levels = c("early", "1st NP", "2nd NP", "adverb",
                                                              "diff_early_1st", "diff_1st_2nd", "diff_2nd_adverb"))

# extract posterior means and 95% CIs in logit space (subj_preference)
# create empty vectors
name <- c()
lci <- c()
uci <- c()
mean <- c()
probs <- c()

# loop through col_names1 to extract posterior mean, 95% CI and Pr(beta < 0)  (subj_preference)
for (i in 1:length(col_names1)) {
  lci <- c(lci, round(plogis(coda::HPDinterval(as.mcmc(psamples_subj[[col_names1[i]]]))), 2)[1])
  uci <- c(uci, round(plogis(coda::HPDinterval(as.mcmc(psamples_subj[[col_names1[i]]]))), 2)[2])
  mean <- c(mean, round(plogis(mean(psamples_subj[[col_names1[i]]])), 2))
  name <- c(name, col_names1[i])
  probs <- c(probs, round(length(which(psamples_subj[[col_names1[i]]] < 0)) / length(psamples_subj[[col_names1[i]]]),2))
}

# save parameters  (subj_preference)
posteriors_2 = data.frame(name, lci, uci, mean, probs)

posteriors_2$cond <- as.factor(c(rep(c("1st NP accent", "2nd NP accent", "Both NPs have accents"), 8), rep("", 9 ), rep(c("1st NP accent", "2nd NP accent", "Both NPs have accents"), 8)))
posteriors_2$window <- as.factor(c(rep(c("early", "1st NP", "2nd NP", "adverb"), each = 3), 
                                   rep(c("early", "1st NP", "2nd NP", "adverb"), each = 3),
                                   rep(c("diff_early_1st", "diff_1st_2nd", "diff_2nd_adverb"), 3),
                                   rep(c("early", "1st NP", "2nd NP", "adverb"), each = 3),
                                   rep(c("early", "1st NP", "2nd NP", "adverb"), each = 3)))
posteriors_2$type <- as.factor(c(rep("estimate", 12), rep("slope", 12), rep("diff", 9), rep("estimate", 24)))


posteriors_2$window = factor(posteriors_2$window , levels = c("early", "1st NP", "2nd NP", "adverb",
                                                              "diff_early_1st", "diff_1st_2nd", "diff_2nd_adverb"))

posteriors_2 <- posteriors_2 %>% 
  filter(type == "estimate")

posteriors_2$time <- as.factor(rep(c("middle", "beginning", "end"), each = 12))



## extract posteriors for object preference
psamples_obj = posterior_samples(xmdl_obj.prime) %>% 
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
    diff_GG_nuc_adverb = GG_nuclear - GG_adverb,
    
    # calculate posteriors for beginning of experiment 
    CG_early_1st = CG_early + min(data$eyetrial.c) * CG_early_trial,
    GC_early_1st = GC_early + min(data$eyetrial.c) * GC_early_trial,
    GG_early_1st = GG_early + min(data$eyetrial.c) * GG_early_trial,
    CG_prenuc_1st = CG_prenuc + min(data$eyetrial.c) * CG_prenuc_trial,
    GC_prenuc_1st = GC_prenuc + min(data$eyetrial.c) * GC_prenuc_trial,
    GG_prenuc_1st = GG_prenuc + min(data$eyetrial.c) * GG_prenuc_trial,
    CG_nuclear_1st = CG_nuclear + min(data$eyetrial.c) * CG_nuclear_trial,
    GC_nuclear_1st = GC_nuclear + min(data$eyetrial.c) * GC_nuclear_trial,
    GG_nuclear_1st = GG_nuclear + min(data$eyetrial.c) * GG_nuclear_trial,
    CG_adverb_1st = CG_adverb + min(data$eyetrial.c) * CG_adverb_trial,
    GC_adverb_1st = GC_adverb + min(data$eyetrial.c) * GC_adverb_trial,
    GG_adverb_1st = GG_adverb + min(data$eyetrial.c) * GG_adverb_trial,
    
    # calculate posteriors for end of experiment 
    CG_early_last = CG_early + max(data$eyetrial.c) * CG_early_trial,
    GC_early_last = GC_early + max(data$eyetrial.c) * GC_early_trial,
    GG_early_last = GG_early + max(data$eyetrial.c) * GG_early_trial,
    CG_prenuc_last = CG_prenuc + max(data$eyetrial.c) * CG_prenuc_trial,
    GC_prenuc_last = GC_prenuc + max(data$eyetrial.c) * GC_prenuc_trial,
    GG_prenuc_last = GG_prenuc + max(data$eyetrial.c) * GG_prenuc_trial,
    CG_nuclear_last = CG_nuclear + max(data$eyetrial.c) * CG_nuclear_trial,
    GC_nuclear_last = GC_nuclear + max(data$eyetrial.c) * GC_nuclear_trial,
    GG_nuclear_last = GG_nuclear + max(data$eyetrial.c) * GG_nuclear_trial,
    CG_adverb_last = CG_adverb + max(data$eyetrial.c) * CG_adverb_trial,
    GC_adverb_last = GC_adverb + max(data$eyetrial.c) * GC_adverb_trial,
    GG_adverb_last = GG_adverb + max(data$eyetrial.c) * GG_adverb_trial
    
  )

# define columns for loop (obj preference)
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
               "diff_GG_early_prenuc", "diff_GG_prenuc_nuc", "diff_GG_nuc_adverb",
               "CG_early_1st", "GC_early_1st", "GG_early_1st",
               "CG_prenuc_1st", "GC_prenuc_1st", "GG_prenuc_1st",
               "CG_nuclear_1st", "GC_nuclear_1st", "GG_nuclear_1st",
               "CG_adverb_1st", "GC_adverb_1st", "GG_adverb_1st",
               "CG_early_last", "GC_early_last", "GG_early_last",
               "CG_prenuc_last", "GC_prenuc_last", "GG_prenuc_last",
               "CG_nuclear_last", "GC_nuclear_last", "GG_nuclear_last",
               "CG_adverb_last", "GC_adverb_last", "GG_adverb_last"
)

# create empty vectors (object preference)
name <- c()
lci <- c()
uci <- c()
mean <- c()
probs <- c()


# extract posterior means and 95% CIs in log odd space (object preference)
for (i in 1:length(col_names1)) {
  lci <- c(lci, round(coda::HPDinterval(as.mcmc(psamples_obj[[col_names1[i]]])), 2)[1])
  uci <- c(uci, round(coda::HPDinterval(as.mcmc(psamples_obj[[col_names1[i]]])), 2)[2])
  mean <- c(mean, round(mean(psamples_obj[[col_names1[i]]]), 2))
  name <- c(name, col_names1[i])
  probs <- c(probs, round(length(which(psamples_obj[[col_names1[i]]] < 0)) / length(psamples_obj[[col_names1[i]]]),2))
}

# save parameters (object preference)
posteriors_3 = data.frame(name, lci, uci, mean, probs)

posteriors_3$cond <- as.factor(c(rep(c("1st NP accent", "2nd NP accent", "Both NPs have accents"), 8), rep("", 9 ), rep(c("1st NP accent", "2nd NP accent", "Both NPs have accents"), 8)))
posteriors_3$window <- as.factor(c(rep(c("early", "1st NP", "2nd NP", "adverb"), each = 3), 
                                   rep(c("early", "1st NP", "2nd NP", "adverb"), each = 3),
                                   rep(c("diff_early_1st", "diff_1st_2nd", "diff_2nd_adverb"), 3),
                                   rep(c("early", "1st NP", "2nd NP", "adverb"), each = 3),
                                   rep(c("early", "1st NP", "2nd NP", "adverb"), each = 3)))
posteriors_3$type <- as.factor(c(rep("estimate", 12), rep("slope", 12), rep("diff", 9), rep("estimate", 24)))


posteriors_3$window = factor(posteriors_3$window , levels = c("early", "1st NP", "2nd NP", "adverb",
                                                              "diff_early_1st", "diff_1st_2nd", "diff_2nd_adverb"))

# extract posterior means and 95% CIs in logit space (object preference)
# create empty vectors
name <- c()
lci <- c()
uci <- c()
mean <- c()
probs <- c()

# loop through col_names1 to extract posterior mean, 95% CI and Pr(beta < 0) (object preference)
for (i in 1:length(col_names1)) {
  lci <- c(lci, round(plogis(coda::HPDinterval(as.mcmc(psamples_obj[[col_names1[i]]]))), 2)[1])
  uci <- c(uci, round(plogis(coda::HPDinterval(as.mcmc(psamples_obj[[col_names1[i]]]))), 2)[2])
  mean <- c(mean, round(plogis(mean(psamples_obj[[col_names1[i]]])), 2))
  name <- c(name, col_names1[i])
  probs <- c(probs, round(length(which(psamples_obj[[col_names1[i]]] < 0)) / length(psamples_obj[[col_names1[i]]]),2))
}

# save parameters (object preference)
posteriors_4 = data.frame(name, lci, uci, mean, probs)

posteriors_4$cond <- as.factor(c(rep(c("1st NP accent", "2nd NP accent", "Both NPs have accents"), 8), rep("", 9 ), rep(c("1st NP accent", "2nd NP accent", "Both NPs have accents"), 8)))
posteriors_4$window <- as.factor(c(rep(c("early", "1st NP", "2nd NP", "adverb"), each = 3), 
                                   rep(c("early", "1st NP", "2nd NP", "adverb"), each = 3),
                                   rep(c("diff_early_1st", "diff_1st_2nd", "diff_2nd_adverb"), 3),
                                   rep(c("early", "1st NP", "2nd NP", "adverb"), each = 3),
                                   rep(c("early", "1st NP", "2nd NP", "adverb"), each = 3)))
posteriors_4$type <- as.factor(c(rep("estimate", 12), rep("slope", 12), rep("diff", 9), rep("estimate", 24)))


posteriors_4$window = factor(posteriors_4$window , levels = c("early", "1st NP", "2nd NP", "adverb",
                                                              "diff_early_1st", "diff_1st_2nd", "diff_2nd_adverb"))

posteriors_4 <- posteriors_4 %>% 
  filter(type == "estimate")

posteriors_4$time <- as.factor(rep(c("middle", "beginning", "end"), each = 12))


# combine posteriors into one data.frame
posteriors_1$preference <- "Target 1st NP"
posteriors_2$preference <- "Target 1st NP"
posteriors_3$preference <- "Target 2nd NP"
posteriors_4$preference <- "Target 2nd NP"

posteriors_logit <- rbind(posteriors_1, posteriors_3)
posteriors_prob <- rbind(posteriors_2, posteriors_4)

# store posterior objects
setwd("../models/")
save(posteriors_1, posteriors_2, 
     posteriors_3, posteriors_4, 
     posteriors_logit, posteriors_prob,
     file = "posteriors.RData")


# plot quick and dirty - sanity check
ggplot(posteriors_prob[posteriors_prob$type == "estimate",], aes(x = window, y = mean)) +
  geom_segment(x = -Inf, y = 0.5, xend = Inf, yend = 0.5,
               lty = "dashed", size = 1, colour = "black") +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.2) +
  geom_line(aes(group = interaction(cond)), colour = "grey") +
  geom_point(size = 3) +
  facet_grid(preference~cond) +
  #scale_y_continuous(expand = c(0, 0), breaks = (c(0, 0.25, 0.5, 0.75, 1)), limits = c(0,1)) +
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




