# Author: Izzy Bedichek
# Date: September 24th, 2023

################# SET-UP #######################################################

# Download instructions for this dataset found at:
# https://higgi13425.github.io/medicaldata/

# UNCOMMENT THE FOLLOWING BLOCK AND RUN BEFORE STARTING (Command Shift C on Mac)
# remotes::install_github("higgi13425/medicaldata")
# library(ggplot2)
# library(medicaldata)
# library(patchwork)
# library(ggtext)

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


############## FINAL PRODUCT ###################################################
# With updated title and axis color emphasis on multiple categories of working 
# treatment so that I represent the data more honestly

# I referenced :
# https://patchwork.data-imaginist.com/reference/plot_annotation.html
# in order to caption my plots with a relationship description

base <- ggplot(data = scurvy)

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
  theme(axis.text.x = element_markdown(color = c("red", "red", "black", "black", 
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
  theme(axis.text.x = element_markdown(color = c("black", "red", "red", "black", 
                                             "black", "black")), 
        legend.position = "none")


scurvy_compendium <- (lassitude_treatment | weakness_of_knees_treatment) / 
  (skin_sores_treatment | gum_rot_treatment) + plot_annotation(
    title = 
      "After 6 Days, Which Treatment(s) Alleviate The Following Scurvy Symptoms?",
    caption = 
      "From research by James Lind, A Treatise on the Scurvy in 
    Three Parts (1757)",
    theme = theme(plot.title = element_text(size = 20),
                  plot.caption = element_text(size = 12)))

scurvy_compendium

ggsave(filename = "scurvy_treatment_by_symptom_plot.png", 
       path = 
         "/Users/izzybedichek/Desktop/Data Science Fall 2023/Projects/Scurvy Project/outputs",
       height = 12,
       width = 18)
