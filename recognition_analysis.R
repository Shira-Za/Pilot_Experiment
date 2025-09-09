# This script handles the recognition task data analysis
#### Pre-processing: ----
#loading data and libraries:
library(dplyr)
library(tidyverse)
library(readr)

setwd("~/Documents/2nd_Degree/My_Research/data/Pilot_Data_Analysis/rec_data_for_analysis")

# --- Load all CSV files (one per participant) ---
file_list <- list.files(pattern = "_rec_data.csv$")
recognition <- file_list |>
  lapply(read.csv) |>
  bind_rows()

# Make sure correct is numeric (0/1)
recognition <- recognition |>
  mutate(correct = as.logical(correct))

# --- Summarize per participant & phase ---
summary_recog <- recognition |>
  group_by(participant, phase) |>
  summarise(
    n_trials = n(),
    accuracy = mean(correct, na.rm = TRUE),
    mean_rt = mean(reaction_time[correct == 1], na.rm = TRUE),
    sd_rt = sd(reaction_time[correct == 1], na.rm = TRUE),
    n_correct = sum(correct, na.rm = TRUE)
  )

print(summary_recog)

# --- Wide format for paired comparisons ---
summary_wide <- summary_recog |>
  select(participant, phase, accuracy, mean_rt) |>
  pivot_wider(
    names_from = phase,
    values_from = c(accuracy, mean_rt)
  ) |>
  rename(
    accuracy_passive = accuracy_recognition1,
    accuracy_active = accuracy_recognition2,
    mean_rt_passive = mean_rt_recognition1,
    mean_rt_active = mean_rt_recognition2
  )

# Paired t-test on accuracy
t_accuracy <- t.test(summary_wide$accuracy_active,
                     summary_wide$accuracy_passive, paired = TRUE)

# Paired t-test on RTs
t_rt <- t.test(summary_wide$mean_rt_active,
               summary_wide$mean_rt_passive,
               paired = TRUE)

print(t_accuracy)
print(t_rt)

#### Preparing for merge ----
rare_counts <- read.csv("rare_counts_per_image.csv")

library(tidyr)

recognition_long <- recognition |>
  pivot_longer(
    cols = c(left_image, right_image),
    names_to = "position",
    values_to = "image"
  ) |>
  mutate(
    correct_response = ifelse(correct == TRUE & correct_answer == image, 1, 0)
  )

recognition_long <- recognition_long |>
  left_join(rare_counts, by = c("image" = "background_image"))

# Accuracy vs rare counts
cor_accuracy <- cor.test(
  recognition_long$n_rare,
  recognition_long$correct_response,
  method = "spearman"
)

# RT vs rare counts (only correct trials)
cor_rt <- cor.test(
  recognition_long$n_rare[recognition_long$correct_response == 1],
  recognition_long$reaction_time[recognition_long$correct_response == 1],
  method = "spearman"
)

