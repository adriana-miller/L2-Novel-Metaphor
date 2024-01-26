
#########
# SETUP #
#########

#load packages
library("papaja")
library("tidyverse")
library("lmerTest")
library("afex")
library("emmeans")
library("plyr")
library("dplyr")
library("ggplot2")
library("ggpubr")
library("rstatix")

#set color palette
palette <- c("#009E73", "#0072B2", "#000000" )

# Seed for random number generation
set.seed(42)
knitr::opts_chunk$set(cache.extra = knitr::rand_seed)

####################
# PREPARE DATASETS #
####################

#set n400 electrodes (based on bilingual tmax results)
n400_el <- c("FC1","CP1","P3","P7","Cz","Pz","CP2")

#set up monolingual dataset
N4_psyp_verb <-read.csv("metaphor-psyp-verb.csv")
N4_psyp_verb <- N4_psyp_verb[,-c(1)]
N4_psyp_verb$experiment <- "English L1"
names <- c("subject","background","condition","knowledge","electrode","experiment","word")
N4_psyp_verb[,names] <- lapply(N4_psyp_verb[,names],as.factor)

#change levels
levels(N4_psyp_verb$condition) <- c("Novel Metaphorical","Anomalous","Literal")
levels(N4_psyp_verb$knowledge) <- c("engineering knowledge","general knowledge")
levels(N4_psyp_verb$background) <- c("engineers","nonengineers")

#set up bilingual dataset
N4_biling_verb <- read.csv(file = "metaphor-biling-verb.csv")
N4_biling_verb <- N4_biling_verb[,-c(1)]
N4_biling_verb <- N4_biling_verb[,-c(5)]
N4_biling_verb$experiment <- "English L2"
names_biling <- c("subject","condition","knowledge","electrode","experiment")
N4_biling_verb[,names_biling] <- lapply(N4_biling_verb[,names_biling],as.factor)

#change levels
levels(N4_biling_verb$condition) <- c("Literal","Novel Metaphorical","Anomalous")
levels(N4_biling_verb$knowledge) <- c("engineering knowledge","general knowledge")

#set up monolingual dataset for anova
N4_psyp_verb_aov <- N4_psyp_verb %>% 
  subset(electrode %in% n400_el & time >= 350 & time <=550 & !subject == "AB13") %>%
  droplevels()
N4_psyp_verb_aov <- N4_psyp_verb_aov[,-c(3:4)]

#set up bilingual dataset for anova
N4_biling_verb_aov <- N4_biling_verb %>% 
  subset(electrode %in% n400_el & time >= 350 & time <=550) %>% 
  droplevels()

######################
# BILINGUAL ANALYSIS #
######################

#anova for bilingual data only
N4_biling_only_verb_aov <- aov_ez(id = "subject", 
                                  dv = "amplitude", 
                                  within=c("condition"), 
                                  type = 3, 
                                  data = N4_biling_verb_aov)
#model summary
summary(N4_biling_only_verb_aov)

#show marginal means
oneway_biling <- emmeans(N4_biling_only_verb_aov, ~ condition)
oneway_comparison_biling <- emmeans(N4_biling_only_verb_aov, list(pairwise ~ condition), adjust = "bonferroni")
summary(oneway_comparison_biling)

#calculate confidence interval
condition_biling <- N4_biling_verb_aov %>% dplyr::group_by(condition) %>% 
  dplyr::summarise(mean_amp = mean(amplitude),
                   CIlower = Rmisc::CI(amplitude, ci = 0.95)["lower"],
                   CIupper = Rmisc::CI(amplitude, ci = 0.95)["upper"])

######################
# TWO-GROUP ANALYSIS #
######################

#2 (bilingual, monolingual) X 3 (literal, metaphor, anomalous)

#combine monolingual and bilingual datasets
group_analysis <- rbind(N4_psyp_verb_aov,N4_biling_verb_aov)

#run anova
group_n4_aov <- aov_ez(id = "subject", 
                       dv = "amplitude", 
                       between=c("experiment"),
                       within=c("condition"), 
                       type = 3, 
                       data = group_analysis)
#model summary
summary(group_n4_aov)

#show marginal means
oneway <- emmeans(group_n4_aov, ~ condition)
oneway_comparison <- emmeans(group_n4_aov, list(pairwise ~ condition), adjust = "bonferroni")
summary(oneway_comparison)

#calculate confidence interval
condition <- group_analysis %>% dplyr::group_by(condition) %>% 
  dplyr::summarise(mean_amp = mean(amplitude),
                   CIlower = Rmisc::CI(amplitude, ci = 0.95)["lower"],
                   CIupper = Rmisc::CI(amplitude, ci = 0.95)["upper"])

#########
# PLOT  #
#########

#prepare monolingual dataset for plotting
N4_psyp_verb_plot <- N4_psyp_verb %>% 
  subset(electrode %in% n400_el & !subject == "AB13") %>% 
  droplevels()
N4_psyp_verb_plot <- N4_psyp_verb_plot[,-c(3,4)]
names(N4_psyp_verb_plot)==names(N4_biling_verb_plot)

#prepare bilingual dataset for plotting
N4_biling_verb_plot <- N4_biling_verb %>% 
  subset(electrode %in% n400_el) %>% 
  droplevels()

#combine monolingual and bilingual data
n400_verb <- rbind(N4_biling_verb_plot,N4_psyp_verb_plot)

#prepare for plotting
n400_verb_plot <- n400_verb %>% 
  subset(time <= 600 & electrode %in% n400_el) %>% 
  droplevels() %>% 
  dplyr::group_by(time,condition,experiment) %>% 
  dplyr::summarise(mean_amp = mean(amplitude),
                   CIlower = Rmisc::CI(amplitude, ci = 0.95)["lower"],
                   CIupper = Rmisc::CI(amplitude, ci = 0.95)["upper"])

#add experiment
n400_verb_plot$experiment <- factor(n400_verb_plot$experiment, levels = c("English L1","English L2"))

#plot
plot_n400_verb <- ggplot(n400_verb_plot, aes(time, 
                                             mean_amp, 
                                             color = condition, 
                                             group = condition))+
  facet_wrap(~experiment)+
  annotate("rect", xmin = 350, xmax = 550, ymin = -Inf, ymax = Inf, 
           alpha = .1, color = "lightgrey", fill = "white")+
  geom_ribbon(aes(ymin = CIlower, ymax = CIupper, fill = condition), 
              alpha = 0.2, linetype = 0) +
  geom_line(size = 0.5) + 
  guides(fill="none")+
  labs(x = "Time from verb (ms)", y = expression(paste("Amplitude (",mu,"V)")),
       colour = "")+
  theme_apa()+
  geom_vline(xintercept = 0,linetype = "dashed",alpha=0.1)+
  geom_hline(yintercept = 0,linetype = "dashed",alpha=0.1)+
  scale_x_continuous(breaks = c(-200,0,200,400,600,800))+
  theme(text = element_text(size = 18),
        legend.position = "right",
        plot.title = element_text(hjust = 0))+
  scale_fill_manual(values = palette[c(1,2,3)])+
  scale_color_manual(values = palette[c(1,2,3)])+
  scale_y_reverse()

