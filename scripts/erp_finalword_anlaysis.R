
#########
# SETUP #
#########

#load packages
library("papaja")
library("tidyverse")
library("lmerTest")
library("afex")
library("emmeans")

#set color palette
palette <- c("#009E73", "#0072B2", "#000000" )

#seed for random number generation
set.seed(42)
knitr::opts_chunk$set(cache.extra = knitr::rand_seed)

####################
# PREPARE DATASETS #
####################

#set N400 electrodes (from t-max permutation, bilingual data)
n400_el <- c("FC1","CP1","P3","P7","Cz","Pz","CP2")

#set up monolingual dataset
N4_psyp <- read.csv("metaphor-psyp.csv")
N4_psyp <- N4_psyp[,-c(1)]
N4_psyp$experiment <- "English L1"
names <- c("subject","background","condition","knowledge","electrode","experiment")
N4_psyp[,names] <- lapply(N4_psyp[,names],as.factor)

#change levels
levels(N4_psyp$condition) <- c("Novel Metaphorical","Anomalous","Literal")
levels(N4_psyp$knowledge) <- c("engineering knowledge","general knowledge")
levels(N4_psyp$background) <- c("engineers","nonengineers")

#set up bilingual dataset
N4_biling <- read.csv("metaphor-biling.csv")
N4_biling <- N4_biling[,-c(1)]
N4_biling <- N4_biling[,-c(5)]
N4_biling$experiment <- "English L2"

#change names
names_biling <- c("subject","condition","knowledge","electrode","experiment")
N4_biling[,names_biling] <- lapply(N4_biling[,names_biling],as.factor)

#change levels
levels(N4_biling$condition) <- c("Literal","Novel Metaphorical","Anomalous")
levels(N4_biling$knowledge) <- c("engineering knowledge","general knowledge")

#set up monolingual dataset for anova
N4_psyp_anova <- N4_psyp %>% 
  subset(electrode %in% n400_el & time >= 350 & time <=550 & !subject == "AB13") %>%
  droplevels()

#remove unused column
N4_psyp_anova <- N4_psyp_anova[,-c(3)]

#set up bilingual dataset for anova
N4_biling_aov <- N4_biling %>% 
  subset(electrode %in% n400_el & time >= 350 & time <=550) %>% 
  droplevels()

######################
# BILINGUAL ANALYSIS #
######################

#anova for bilingual data only
biling_only_aov <- aov_ez(id = "subject", 
                          dv = "amplitude", 
                          within=c("condition"), 
                          type = 3, 
                          data = N4_biling_aov)
#model summary
summary(biling_only_aov)

#run emmeans
oneway_biling <- emmeans(N4_biling_aov, ~ condition)
oneway_comparison_biling <- emmeans(biling_only_aov, 
                                    list(pairwise ~ condition), 
                                    adjust = "bonferroni")
summary(oneway_comparison_biling)

#calculate confidence interval
condition_biling <- N4_biling_aov %>% dplyr::group_by(condition) %>% 
  dplyr::summarise(mean_amp = mean(amplitude),
                   CIlower = Rmisc::CI(amplitude, ci = 0.95)["lower"],
                   CIupper = Rmisc::CI(amplitude, ci = 0.95)["upper"])

######################
# TWO-GROUP ANALYSIS #
######################

#2 (bilingual, monolingual) X 3 (literal, metaphor, anomalous)

#combine datasets
group_analysis <- rbind(N4_psyp_anova,N4_biling_aov)

#run anova
group_n4_aov <- aov_ez(id = "subject", dv = "amplitude", 
                       between=c("experiment"),
                       within=c("condition"), 
                       type = 3, 
                       data = group_analysis)

#model summary
summary(group_n4_aov)

#show marginal means
oneway <- emmeans(group_n4_aov, ~ condition)
oneway_comparison <- emmeans(group_n4_aov, 
                             list(pairwise ~ condition), 
                             adjust = "bonferroni")
summary(oneway_comparison)

#calculate confidence intervals
condition <- group_analysis %>% dplyr::group_by(condition) %>% 
  dplyr::summarise(mean_amp = mean(amplitude),
                   CIlower = Rmisc::CI(amplitude, ci = 0.95)["lower"],
                   CIupper = Rmisc::CI(amplitude, ci = 0.95)["upper"])

#show marginal means
twoway <- emmeans(group_n4_aov, ~ experiment|condition)
interaction_a <- emmeans(group_n4_aov, list(pairwise ~ experiment|condition), adjust = "bonferroni")
interaction_b <- emmeans(group_n4_aov, list(pairwise ~ condition|experiment), adjust = "bonferroni")
summary(interaction_a)
summary(interaction_b)

#calculate confidence intervals
group_by_condition <- group_analysis %>% dplyr::group_by(experiment, condition) %>% 
  dplyr::summarise(mean_amp = mean(amplitude),
                   CIlower = Rmisc::CI(amplitude, ci = 0.95)["lower"],
                   CIupper = Rmisc::CI(amplitude, ci = 0.95)["upper"])

#########
# PLOT  #
#########

#set up monolingual dataset for graphing
N4_psyp_plot <- N4_psyp %>% 
  subset(electrode %in% n400_el & !subject == "AB13") %>% 
  droplevels()

#remove unused column
N4_psyp_plot <- N4_psyp_plot[,-c(3)]

#set up bilingual dataset for graphing
N4_biling_plot <- N4_biling %>% subset(electrode %in% n400_el) %>% droplevels()

#combine monolingual and bilingual datasets
n400_plotting = rbind(N4_biling_plot,N4_psyp_plot)

#prepare for plotting
n400_plot <- n400_plotting %>% 
  subset(time <= 800) %>% 
  droplevels() %>% 
  dplyr::group_by(time,condition,experiment) %>% 
  dplyr::summarise(mean_amp = mean(amplitude),
                   CIlower = Rmisc::CI(amplitude, ci = 0.95)["lower"],
                   CIupper = Rmisc::CI(amplitude, ci = 0.95)["upper"])

#add experiment
n400_plot$experiment <- factor(n400_plot$experiment, levels = c("English L1","English L2"))

#plot
plot_n400 <- ggplot(n400_plot, aes(time, 
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
  labs(x = "Time from final word of sentence (ms)", y = expression(paste("Amplitude (",mu,"V)")),colour = "")+
  theme_apa()+
  geom_vline(xintercept = 0,linetype = "dashed",alpha=0.1)+
  geom_hline(yintercept = 0,linetype = "dashed",alpha=0.1)+
  scale_x_continuous(breaks = c(-200,0,200,400,600,800))+
  theme(text = element_text(size = 18),
        legend.position = "right",
        plot.title = element_text(hjust = 0))+
  scale_color_manual(values = palette[c(1,2,3)])+
  scale_fill_manual(values = palette[c(1,2,3)])+
  scale_y_reverse()

