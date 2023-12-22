
# Author: Izzy Bedichek
# Date: September 17th, 2023

################# SET-UP #######################################################

# Download instructions for this dataset found at:
# https://higgi13425.github.io/medicaldata/

# UNCOMMENT THE FOLLOWING BLOCK AND RUN BEFORE STARTING
  # remotes::install_github("higgi13425/medicaldata")
  # library(ggplot2)
  # library(medicaldata)
  # library(patchwork)

################# EXPLORATORY FIGURES ##########################################

# Data reference: 
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-
# 07-25/readme.md
# This section uses a data set on scurvy patients treated with different 
# methods over the course of 6 days.

# There are 12 observations and 6 variables: each observation is a seaman under
# treatment and the variables are: treatment, gum_rot_d6, skin_sores_d6, 
# weakness_of_the_knees_d6, lassitude_d6,  fit_for_duty_d6, 
# dosing_regimen_for_scurvy (I looked and saw that the doses were all identical 
# within the treatments, so I'm also ignoring it as it would not add any 
# information and the treatment is more relevant than the dosage), 
# study_id (which I will be ignoring as there are too few individuals in the 
# study to do any meaningful analysis on a full set of observations and their 
# differences). 

# In this exploratory figure-making I will just look at the treatments and 
# the symptoms. 

head(scurvy)
getwd()
base <- ggplot(data = scurvy)

# First I looked at the relationship between different types of symptoms

rot_weak_explore <- base +
  geom_count(mapping = aes(x = gum_rot_d6, y = weakness_of_the_knees_d6))

sores_weak_explore <- base +
  geom_count(mapping = aes(x = skin_sores_d6, y = weakness_of_the_knees_d6))

lassitude_weak_explore <- base +
  geom_count(mapping = aes(x = lassitude_d6, y = weakness_of_the_knees_d6))

lassitude_sores_explore <- base +
  geom_count(mapping = aes(x = lassitude_d6, y = skin_sores_d6))

lassitude_rot_explore <- base +
  geom_count(mapping = aes(x = lassitude_d6, y = gum_rot_d6))

sores_rot_explore <- base +
  geom_count(mapping = aes(x = skin_sores_d6, y = gum_rot_d6))

exploratory_compendium_of_symptom_relationships <- (lassitude_rot_explore + 
                                                      lassitude_sores_explore + 
                                                      lassitude_weak_explore) /
  (rot_weak_explore + sores_weak_explore + sores_rot_explore)

exploratory_compendium_of_symptom_relationships

# There seemed to be a positive correlation between all of them, if there 
# had been more data points, I would have jittered the points, but I felt like 
# the number of data points were low enough that geom_count felt more intuitive

# Next I looked at the relationship between treatments of scurvy and the 
# symptoms

lassitude_treatment_explore <- base +
  geom_count(mapping = aes(x = treatment, y = lassitude_d6)) 

gum_rot_treatment_explore <- base +
  geom_count(mapping = aes(x = treatment, y = gum_rot_d6)) 

skin_sores_treatment_explore <- base +
  geom_count(mapping = aes(x = treatment, y = skin_sores_d6)) 

weakness_of_knees_treatment_explore <- base +
  geom_count(mapping = aes(x = treatment, y = weakness_of_the_knees_d6))

exploratory_compendium_of_symptom_to_treatment_relationships <- 
  (gum_rot_treatment_explore + skin_sores_treatment_explore) / 
  (lassitude_treatment_explore + weakness_of_knees_treatment_explore)

exploratory_compendium_of_symptom_to_treatment_relationships

# I noticed that while there are different combinations of treatments that 
# cause non-severe symptoms after 6 days of treatment, citrus reliably scores
# with symptoms at either none or mild.

# I want to emphasize this so that someone looking at the graphs could notice
# this right away. I will start by using color.

################ PLAYING WITH GEOM FOR EFFECT ##################################

# The big sizes of the severe symptoms were a lot more flashy than that of 
# the mild and none because most treatments didn't work. This is the opposite of
# what I want the reader to pay attention to, so I changed my mind and 
# tried jitter to make the observation sizes more equal in attention-grabbing. 

base +
  geom_jitter(mapping = aes(x = treatment, y = lassitude_d6, 
                           color = factor(lassitude_d6))) +
  facet_wrap(~treatment) +
  theme_bw()

########## STARTING TO BUILD MY PLOTS ##########################################
# I referenced the following to create custom tick marks:
# http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-
# tick-marks-and-labels
# I referenced ?labs for the labs command

build_initial_plot <- base +
  geom_jitter(mapping = aes(x = treatment, y = lassitude_d6, 
                            color = lassitude_d6)) +
  labs(title = "Lassitude Caused by Scurvy: Treatment Efficacy by Type", 
       x = "Treatment", y = "Severity of Lassitude") +
  theme_bw()

build_initial_plot

build_clean_text_plot <- base +
  geom_jitter(mapping = aes(x = treatment, y = gum_rot_d6, 
                            color = gum_rot_d6)) +
  labs(title = "Gum Rot Caused by Scurvy: Treatment Efficacy by Type", 
       x = "Treatment", y = "Severity of Gum Rot") +
  scale_y_discrete(labels = c("None", "Mild", "Moderate", "Severe")) +
  scale_x_discrete(labels = c("Cider", "Citrus", "Dilute Sulfuric Acid",
                              "Purgative Mixture", "Sea Water", "Vinegar")) +
  theme_bw()

build_clean_text_plot

# I referenced the following to remove the aes legend:
# https://www.statology.org/remove-legend-ggplot2/#:~:text=By%20specifying
# %20legend.,all%20legends%20from%20the%20plot.

build_clean_text_no_legend_plot <- base +
  geom_jitter(mapping = aes(x = treatment, y = skin_sores_d6,
                            color = lassitude_d6)) +
  labs(title = 
         "Skin Sores Caused by Scurvy: Treatment Efficacy by Type", 
       x = "Treatment", y = "Severity of Skin Sores") +
  scale_y_discrete(labels = c("None", "Mild", "Moderate", "Severe")) +
  scale_x_discrete(labels = c("Cider", "Citrus", "Dilute Sulfuric Acid",
                              "Purgative Mixture", "Sea Water", "Vinegar")) +
  theme_bw() +
  theme(legend.position = "none")

build_clean_text_no_legend_plot

# I realized that there's too much jitter! The treatment type is not clear
# I adjust the height and width so that the points are only slightly offset

build_clean_text_no_legend_less_jitter_plot <- base +
  geom_jitter(mapping = aes(x = treatment, y = weakness_of_the_knees_d6,
                            color = weakness_of_the_knees_d6), height = 0.05, 
              width = 0.15) +
  labs(title = 
         "Weakness of the Knees Caused by Scurvy: Treatment Efficacy by Type", 
       x = "Treatment", y = "Severity of Weakness") +
  scale_y_discrete(labels = c("None", "Mild", "Moderate", "Severe")) +
  scale_x_discrete(labels = c("Cider", "Citrus", "Dilute Sulfuric Acid",
                              "Purgative Mixture", "Sea Water", "Vinegar")) +
  theme_bw() +
  theme(legend.position = "none")

build_clean_text_no_legend_less_jitter_plot

############## PROBLEM: TICK MARKS CREATING A FALSELY DISPLAYED PLOT ###########
# This gets rid of "Severe" for "Weakness of Knees," because I labelled them in
# order none, mild, moderate, severe, and "Weakness of Knees" is missing a value
# for mild. Because of this, all the data points after none got shifted down and 
# it's displaying the y-values incorrectly! It's also not matching the rest in 
# terms of plot size! I need to set the max and min limits so it displays 
# correctly

# I referenced this:
# https://stackoverflow.com/questions/11335836/increase-number-of-axis-ticks
# To make the plots consistent

build_clean_text_no_legend_less_jitter_fixed_plot <- base +
  geom_jitter(mapping = aes(x = treatment, y = weakness_of_the_knees_d6,
                            color = weakness_of_the_knees_d6), height = 0.05, 
              width = 0.15) +
  labs(title = 
         "Weakness of the Knees Caused by Scurvy: Treatment Efficacy by Type", 
       x = "Treatment", y = "Severity of Weakness") +
  scale_y_discrete(limits = c("0_none", "1_mild", "2_moderate", "3_severe"), 
                   labels = c("None", "Mild", "Moderate", "Severe")) +
  scale_x_discrete(labels = c("Cider", "Citrus", "Dilute Sulfuric Acid",
                              "Purgative Mixture", "Sea Water", "Vinegar")) +
  theme_bw() +
  theme(legend.position = "none")

build_clean_text_no_legend_less_jitter_fixed_plot
