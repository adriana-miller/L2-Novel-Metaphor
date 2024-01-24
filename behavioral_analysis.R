#Behavioral Analysis
  
#########
# SETUP #
#########

#set color palette
palette <- c("#0072B2", "#000000" )

#load packages
library(lme4)
library(lmerTest)
library(ggplot2)
library(effectsize)
library(emmeans)
library(papaja)
library(tidyverse)
library(sjPlot)
library(ggpubr)
library(dplyr)

#load bilingual dataset
bilingual <- read.csv("bilingual.csv", header = TRUE)

#display data structure
head(bilingual, 5)

#load monolingual data
monolingual <- read.csv("Creativity_metaphor_behavioural2020.csv", header = TRUE)

#display data structure
head(monolingual, 5)

#####################
# CLEAN UP DATASETS #
#####################

#remove participant 111
bilingual <- bilingual[!(bilingual$id == "111"),]

#remove erroneous button presses (due to technical error)
bilingual <- bilingual %>%
  dplyr::filter(approp_rt > 10 & unusual_rt > 10)

#remove unneeded columns
monolingual <- monolingual[,-c(1,4,6,7,10:13)]
bilingual<-bilingual[,-c(1:10,14:16)]

#separate appropriateness and unusualness
approp <- bilingual[,-c(5)]
unusual <- bilingual[,-c(4)]

#change column names so they match
colnames(bilingual) <- c("Item", "Subject", "Sentence", 
                         "Appropriateness", "Unusualness")
colnames(monolingual) <- c("Subject", "Appropriateness", 
                           "Sentence", "Item", "Unusualness")

#fix sentence names because monolingual dataset has extra spaces after some periods
monolingual$Item <- trimws(monolingual$Item)
length(unique(monolingual$Item))
length(unique(bilingual$Item))

#prepare to change levels
names <- c("Sentence","Appropriateness","Unusualness","Subject","Item")
monolingual[,names] <- lapply(monolingual[,names],factor)
bilingual[,names] <- lapply(bilingual[,names],factor)

#change levels
levels(monolingual$Sentence) <- c("Metaphorical","Anomalous","Literal")

#add experiment variable
bilingual$Experiment <- "English L2"
monolingual$Experiment <- "English L1"

#remove unused monolingual participants
good_subject_mono <- c("1", "2", "5", "7", "9", "10", "11", "12", "14", "15", 
                       "18", "19", "20", "21", "22", "23", "25", "26", "30", "31", 
                       "32", "33", "34", "35", "36", "38", "39", "40", "41", "42", 
                       "43", "44")
monolingual <- monolingual %>% subset(Subject %in% good_subject_mono) %>% droplevels()

#combine datasets
twogroup <- rbind(monolingual,bilingual)
twogroup$Experiment <- factor(twogroup$Experiment)

############################
# BILINGUAL ANALYSIS SETUP #
############################

#code appropriateness
approp <- approp %>%
  mutate(approp_rating = case_when(
    Appropriate.ACC == "Appropriate" ~ 1,
    Appropriate.ACC == "Inappropriate" ~ 0))

#code unusualness
unusual <- unusual %>%
  mutate(unusual_rating = case_when(
    Unusual.ACC == "Unusual" ~ 1,
    Unusual.ACC == "Usual" ~ 0))

#calculate mean appropriateness ratings by participant and condition
approp_mean <- approp %>%
  group_by(id, type) %>%
  summarise_at(vars(approp_rating), list(name = mean))

#calculate mean unusualness ratings by participant and condition
unusual_mean <- unusual %>%
  group_by(type) %>%
  summarise_at(vars(unusual_rating), list(name = mean))

#set up contrast code variables
met_met_v_anom <- 0.5
anom_met_v_anom <- -0.5
lit_met_v_anom <- 0
met_lit_v_all <- -0.5
non_lit_v_all <- -0.5
lit_lit_v_all <- 1

#contrast code appropriateness
approp <- approp %>%
  mutate(type_fac = as.factor(type),
         id_fac = as.factor(id),
         item_fac = as.factor(sentence),
         met_v_anom = case_when(
           type_fac == "Metaphorical" ~ met_met_v_anom,
           type_fac == "Anomalous" ~ anom_met_v_anom,
           type_fac == "Literal" ~ lit_met_v_anom
         ),
         lit_v_all = case_when(
           type_fac == "Metaphorical" ~ met_lit_v_all,
           type_fac == "Anomalous" ~ non_lit_v_all,
           type_fac == "Literal" ~ lit_lit_v_all
         ))

#contrast code unusualness
unusual <- unusual %>%
  mutate(type_fac = as.factor(type),
         id_fac = as.factor(id),
         item_fac = as.factor(sentence),
         met_v_anom = case_when(
           type_fac == "Metaphorical" ~ 0.5,
           type_fac == "Anomalous" ~ -0.5,
           type_fac == "Literal" ~ 0
         ),
         lit_v_all = case_when(
           type_fac == "Metaphorical" ~ -0.5,
           type_fac == "Anomalous" ~ -0.5,
           type_fac == "Literal" ~ 1
         ))

###################################
# BILINGUAL APPROPRIATENESS MODEL #
###################################

#run appropriateness
approp_omnibus <- glmer(approp_rating ~ met_v_anom + lit_v_all +
                          (1|item_fac) + (met_v_anom + lit_v_all|id_fac),
                        data = approp,
                        family = "binomial")

#display summary
summary(approp_omnibus)

#show marginal means for first set of contrasts
#metaphorical vs. anomalous
emmeans(approp_omnibus, 
        ~ met_v_anom, 
        at = list(met_v_anom = c(met_met_v_anom, anom_met_v_anom))) %>%
  summary(type = "response")

#show marginal means for second set of contrasts
#literal vs. metaphor and anomalous
emmeans(approp_omnibus, 
        ~ lit_v_all, 
        at = list(lit_v_all = c(met_lit_v_all, lit_lit_v_all))) %>%
  summary(type = "response")

#run model without first set of contrast codes
met_v_anom <- glmer(approp_rating ~ lit_v_all +
                      (1|item_fac) + (lit_v_all|id_fac),
                    data = approp,
                    family = "binomial")

#model comparison
met_v_anom_comp <- anova(approp_omnibus, met_v_anom)

#run model without second set of contrast codes
lit_v_all <- glmer(approp_rating ~ met_v_anom +
                     (1|item_fac) + (met_v_anom|id_fac),
                   data = approp,
                   family = "binomial")

#model comparison
lit_v_all_comp <- anova(approp_omnibus, lit_v_all)

###############################
# BILINGUAL UNUSUALNESS MODEL #
###############################

#run unusualness
unusual_omnibus <- glmer(unusual_rating ~ met_v_anom + lit_v_all +
                           (1|item_fac) + (met_v_anom + lit_v_all|id_fac),
                         data = unusual,
                         family = "binomial")

#show model summary
summary(unusual_omnibus)

#show marginal means for first set of contrasts
#metaphorical vs. anomalous
emmeans(unusual_omnibus, 
        ~ met_v_anom,
        at = list(met_v_anom = c(met_met_v_anom,anom_met_v_anom))) %>%
  summary(type = "response")

#show marginal means for second set of contrasts
#literal vs. metaphor and anomalous
emmeans(unusual_omnibus,
        ~ lit_v_all,
        at = list(lit_v_all = c(met_lit_v_all, lit_lit_v_all))) %>%
  summary(type = "response")

#run model without first set of contrast codes
met_v_anom <- glmer(unusual_rating ~  + lit_v_all +
                      (1|item_fac) + (lit_v_all|id_fac),
                    data = unusual,
                    family = "binomial")

#model comparison
met_v_anom_comp <- anova(unusual_omnibus, met_v_anom)

#run model without second set of contrast codes
lit_v_all <- glmer(unusual_rating ~  + met_v_anom +
                     (1|item_fac) + (met_v_anom|id_fac),
                   data = unusual,
                   family = "binomial")

#model comparison
lit_v_all_comp <- anova(unusual_omnibus, lit_v_all)

############################
# TWO-GROUP ANALYSIS SETUP #
############################

#set up contrasts for sentence
sentence_contrasts <- matrix(c(1/2,-1/2,0,
                               -1/4,-1/4,1/2),
                             ncol = 2)

#set up contrasts for experiment
experiment_contrasts <- matrix(c(1/2,-1/2),
                               ncol=1)

#contrast code sentence
contrasts(twogroup$Sentence) <- sentence_contrasts

#contrast code experiment
contrasts(twogroup$Experiment) <- experiment_contrasts

#recode unusualness
twogroup$Unusualness <- factor(twogroup$Unusualness, 
                               levels = c("Usual","Unusual"))

##################################
# TWO-GROUP UNUSUALNESS ANALYSIS #
##################################

#run model
maximal_us_contrast <- glmer(Unusualness ~ Sentence*Experiment + 
                               (1+Sentence|Subject) + (1+Experiment|Item), 
                             family="binomial"(link = "logit"),
                             data = twogroup,
                             glmerControl(calc.derivs=FALSE))

#rePCA
summary(rePCA(maximal_us_contrast))

#VarCorr
VarCorr(maximal_us_contrast)

#model summary
summary(maximal_us_contrast)

#post-hoc comparisons
emmeans(maximal_us_contrast, list(pairwise~Sentence|Experiment))
emmeans(maximal_us_contrast, list(pairwise~Experiment|Sentence))

##############################
# TWO-GROUP UNUSUALNESS PLOT #
##############################

#model plot using sjPlot
unusualness_model <- plot_model(maximal_us_contrast, type = "int")

#add features
unusualness_model + 
  theme_apa() + 
  scale_color_manual(values = palette[c(1,2)]) + 
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        text = element_text(size=16)) + 
  ggtitle("Predicted probabilities")

######################################
# TWO-GROUP APPROPRIATENESS ANALYSIS #
######################################

#recode appropriateness
twogroup$Appropriateness <- factor(twogroup$Appropriateness, 
                                   levels = c("Inappropriate","Appropriate"))

#run model
maximal_app_contrast <- glmer(Appropriateness ~ Sentence*Experiment + 
                                (1+Sentence|Subject) + (1+Experiment|Item), 
                              family="binomial"(link = "logit"),
                              data = twogroup, 
                              glmerControl(calc.derivs=FALSE))

#rePCA
summary(rePCA(maximal_app_contrast))

#VarCorr
VarCorr(maximal_app_contrast)

#model summary
summary(maximal_app_contrast)

#post-hoc comparisons
emmeans(maximal_app_contrast, list(pairwise~Sentence|Experiment))
emmeans(maximal_app_contrast, list(pairwise~Experiment|Sentence))

##################################
# TWO-GROUP APPROPRIATENESS PLOT #
##################################
#model plot using sjPlot
appropriateness_model <- plot_model(maximal_app_contrast, type = "int")

#add features
appropriateness_model + 
  theme_apa() + 
  scale_color_manual(values = palette[c(1,2)]) + 
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        text = element_text(size=16)) + 
  ggtitle("Predicted probabilities")
