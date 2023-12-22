# Author: Izzy Bedichek
# Date: September 20th, 2023

################# SET-UP #######################################################

# Download instructions for this dataset found at:
# https://higgi13425.github.io/medicaldata/

# UNCOMMENT THE FOLLOWING BLOCK AND RUN BEFORE STARTING
# remotes::install_github("higgi13425/medicaldata")
# library(ggplot2)
# library(medicaldata)
# library(patchwork)

########## RECAP INTRODUCTION ##################################################
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

###### PROTOTYPE WITH PATCHWORK ################################################
# In this script, I will prototype the figure I will submit now that I have made
# a good base and solved a lot of the more basic questions of what I want to
# communicate (that citrus ended up being the only reliable way to curb scurvy
# in sailors in the study from which I got my data) and clarity of that message
# (color, panel arrangement, etc.)

base <- ggplot(data = scurvy)

lassitude_treatment_proto <- base +
  geom_jitter(mapping = aes(x = treatment, y = lassitude_d6,
                            color = lassitude_d6), height = 0.05, 
              width = 0.15) +
  labs(title = 
         "Lassitude Caused by Scurvy: Treatment Efficacy by Type", 
       x = "Treatment", y = "Severity of Lassitude") +
  scale_y_discrete(limits = c("0_none", "1_mild", "2_moderate", "3_severe"), 
                   labels = c("None", "Mild", "Moderate", "Severe")) +
  scale_x_discrete(labels = c("Cider", "Citrus", "Dilute Sulfuric Acid",
                              "Purgative Mixture", "Sea Water", "Vinegar")) +
  theme_bw() +
  theme(legend.position = "none")


weakness_of_knees_treatment_proto <- base +
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


skin_sores_treatment_proto <- base +
  geom_jitter(mapping = aes(x = treatment, y = skin_sores_d6,
                            color = skin_sores_d6), height = 0.05, 
              width = 0.15) +
  labs(title = 
         "Skin Sores Caused by Scurvy: Treatment Efficacy by Type", 
       x = "Treatment", y = "Severity of Skin Sores") +
  scale_y_discrete(limits = c("0_none", "1_mild", "2_moderate", "3_severe"), 
                   labels = c("None", "Mild", "Moderate", "Severe")) +
  scale_x_discrete(labels = c("Cider", "Citrus", "Dilute Sulfuric Acid",
                              "Purgative Mixture", "Sea Water", "Vinegar")) +
  theme_bw() +
  theme(legend.position = "none")

gum_rot_treatment_proto <- base +
  geom_jitter(mapping = aes(x = treatment, y = gum_rot_d6,
                            color = gum_rot_d6), height = 0.05, 
              width = 0.15) +
  labs(title = 
         "Gum Rot Caused by Scurvy: Treatment Efficacy by Type", 
       x = "Treatment", y = "Severity of Gum Rot") +
  scale_y_discrete(limits = c("0_none", "1_mild", "2_moderate", "3_severe"), 
                   labels = c("None", "Mild", "Moderate", "Severe")) +
  scale_x_discrete(labels = c("Cider", "Citrus", "Dilute Sulfuric Acid",
                              "Purgative Mixture", "Sea Water", "Vinegar")) +
  theme_bw() +
  theme(legend.position = "none")


scurvy_compendium_proto <- (lassitude_treatment_proto | 
                              weakness_of_knees_treatment_proto) / 
  (skin_sores_treatment_proto | gum_rot_treatment_proto)

scurvy_compendium_proto

############# AXIS COLOR AND EXTRA EMPHASIS ####################################
# The individual titles worked well for the stand-alone plots, but now that 
# they're combined it's redundant, so I created tags instead of titles for each
# panel

# Reference for making the colors colorblind-friendly:
# https://ggplot2.tidyverse.org/reference/scale_viridis.html
# Reference for highlighting Citrus on the x axis:
# https://r-charts.com/ggplot2/axis

# Here I really wanted to emphasize how Citrus contributes to low 
# end-of-treatment symptom severity. I am aware that the emphasis of the word
# "Citrus" could have been done in a more replicable way. There is no direct 
# connection between the red text and the Citrus variable, I did it manually by 
# counting its position along the x axis, which would not work in larger 
# datasets. This is all to say: readers and replicators beware, this was as 
# close as I could get to the code I wanted but I would do it better if I 
# knew how.

lassitude_treatment <- base +
  geom_jitter(mapping = aes(x = treatment, y = lassitude_d6,
                            color = lassitude_d6), height = 0.05, 
              width = 0.15) +
  labs(tag = "Lassitude", 
       x = "Treatment", y = "Severity of Lassitude") +
  scale_color_viridis_d(option = "plasma") +
  scale_y_discrete(limits = c("0_none", "1_mild", "2_moderate", "3_severe"), 
                   labels = c("None", "Mild", "Moderate", "Severe")) +
  scale_x_discrete(labels = c("Cider", "Citrus", "Dilute Sulfuric Acid",
                              "Purgative Mixture", "Sea Water", "Vinegar")) +
  theme_bw() +
  theme(axis.text.x = element_text(color = c("black", "red", "black", "black", 
                                             "black", "black")), 
        legend.position = "none")


weakness_of_knees_treatment <- base +
  geom_jitter(mapping = aes(x = treatment, y = weakness_of_the_knees_d6,
                            color = weakness_of_the_knees_d6), height = 0.05, 
              width = 0.15) +
  labs(tag = "Weakness of Knees", x = "Treatment", y = "Severity of Weakness") +
  scale_color_viridis_d(option = "plasma") +
  scale_y_discrete(limits = c("0_none", "1_mild", "2_moderate", "3_severe"), 
                   labels = c("None", "Mild", "Moderate", "Severe")) +
  scale_x_discrete(labels = c("Cider", "Citrus", "Dilute Sulfuric Acid",
                              "Purgative Mixture", "Sea Water", "Vinegar")) +
  theme_bw() +
  theme(axis.text.x = element_text(color = c("black", "red", "black", "black", 
                                             "black", "black")), 
        legend.position = "none")


skin_sores_treatment <- base +
  geom_jitter(mapping = aes(x = treatment, y = skin_sores_d6,
                            color = skin_sores_d6), height = 0.05, 
              width = 0.15) +
  labs(tag = "Skin Sores", x = "Treatment", y = "Severity of Skin Sores") +
  scale_color_viridis_d(option = "plasma") +
  scale_y_discrete(limits = c("0_none", "1_mild", "2_moderate", "3_severe"), 
                   labels = c("None", "Mild", "Moderate", "Severe")) +
  scale_x_discrete(labels = c("Cider", "Citrus", "Dilute Sulfuric Acid",
                              "Purgative Mixture", "Sea Water", "Vinegar")) +
  theme_bw() +
  theme(axis.text.x = element_text(color = c("black", "red", "black", "black", 
                                             "black", "black")), 
        legend.position = "none")

gum_rot_treatment <- base +
  geom_jitter(mapping = aes(x = treatment, y = gum_rot_d6,
                            color = gum_rot_d6), height = 0.05, 
              width = 0.15) +
  labs(tags = "Gum Rot", x = "Treatment", y = "Severity of Gum Rot") +
  scale_color_viridis_d(option = "plasma") +
  scale_y_discrete(limits = c("0_none", "1_mild", "2_moderate", "3_severe"), 
                   labels = c("None", "Mild", "Moderate", "Severe")) +
  scale_x_discrete(labels = c("Cider", "Citrus", "Dilute Sulfuric Acid",
                              "Purgative Mixture", "Sea Water", "Vinegar")) +
  theme_bw() +
  theme(axis.text.x = element_text(color = c("black", "red", "black", "black", 
                                             "black", "black")), 
        legend.position = "none")


scurvy_compendium <- (lassitude_treatment | weakness_of_knees_treatment) / 
  (skin_sores_treatment | gum_rot_treatment)

scurvy_compendium

#### FIXING THE WARNING MESSAGE ################################################
# Reference for fixing the warning message:
# Vectorized input to `element_text()` is not officially supported.
# ℹ Results may be unexpected or may change in future versions of ggplot2. 
# that occured

# Reference: 
# https://stackoverflow.com/questions/75981629/proper-way-to-pass-vectorized-
# input-to-element-text

library(ggtext)

lassitude_treatment <- base +
  geom_jitter(mapping = aes(x = treatment, y = lassitude_d6,
                            color = lassitude_d6), height = 0.05, 
              width = 0.15) +
  labs(tag = "Lassitude", 
       x = "Treatment", y = "Severity of Lassitude") +
  scale_color_viridis_d(option = "plasma") +
  scale_y_discrete(limits = c("0_none", "1_mild", "2_moderate", "3_severe"), 
                   labels = c("None", "Mild", "Moderate", "Severe")) +
  scale_x_discrete(labels = c("Cider", "Citrus", "Dilute Sulfuric Acid",
                              "Purgative Mixture", "Sea Water", "Vinegar")) +
  theme_bw() +
  theme(axis.text.x = element_markdown(color = c("black", "red", "black", "black", 
                                             "black", "black")), 
        legend.position = "none")


weakness_of_knees_treatment <- base +
  geom_jitter(mapping = aes(x = treatment, y = weakness_of_the_knees_d6,
                            color = weakness_of_the_knees_d6), height = 0.05, 
              width = 0.15) +
  labs(tag = "Weakness of Knees", x = "Treatment", y = "Severity of Weakness") +
  scale_color_viridis_d(option = "plasma") +
  scale_y_discrete(limits = c("0_none", "1_mild", "2_moderate", "3_severe"), 
                   labels = c("None", "Mild", "Moderate", "Severe")) +
  scale_x_discrete(labels = c("Cider", "Citrus", "Dilute Sulfuric Acid",
                              "Purgative Mixture", "Sea Water", "Vinegar")) +
  theme_bw() +
  theme(axis.text.x = element_markdown(color = c("black", "red", "black", "black", 
                                             "black", "black")), 
        legend.position = "none")


skin_sores_treatment <- base +
  geom_jitter(mapping = aes(x = treatment, y = skin_sores_d6,
                            color = skin_sores_d6), height = 0.05, 
              width = 0.15) +
  labs(tag = "Skin Sores", x = "Treatment", y = "Severity of Skin Sores") +
  scale_color_viridis_d(option = "plasma") +
  scale_y_discrete(limits = c("0_none", "1_mild", "2_moderate", "3_severe"), 
                   labels = c("None", "Mild", "Moderate", "Severe")) +
  scale_x_discrete(labels = c("Cider", "Citrus", "Dilute Sulfuric Acid",
                              "Purgative Mixture", "Sea Water", "Vinegar")) +
  theme_bw() +
  theme(axis.text.x = element_markdown(color = c("black", "red", "black", "black", 
                                             "black", "black")), 
        legend.position = "none")

gum_rot_treatment <- base +
  geom_jitter(mapping = aes(x = treatment, y = gum_rot_d6,
                            color = gum_rot_d6), height = 0.05, 
              width = 0.15) +
  labs(tags = "Gum Rot", x = "Treatment", y = "Severity of Gum Rot") +
  scale_color_viridis_d(option = "plasma") +
  scale_y_discrete(limits = c("0_none", "1_mild", "2_moderate", "3_severe"), 
                   labels = c("None", "Mild", "Moderate", "Severe")) +
  scale_x_discrete(labels = c("Cider", "Citrus", "Dilute Sulfuric Acid",
                              "Purgative Mixture", "Sea Water", "Vinegar")) +
  theme_bw() +
  theme(axis.text.x = element_markdown(color = c("black", "red", "black", "black", 
                                             "black", "black")), 
        legend.position = "none")


scurvy_compendium <- (lassitude_treatment | weakness_of_knees_treatment) / 
  (skin_sores_treatment | gum_rot_treatment)

scurvy_compendium


