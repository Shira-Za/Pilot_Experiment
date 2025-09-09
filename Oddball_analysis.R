# This script is meant to analyze behavioral data from the pilot experiment.

#### Pre-processing: ----
#loading data and libraries:
library(dplyr)
library(tidyverse)

setwd("~/Documents/2nd_Degree/My_Research/data/Pilot_Data_Analysis/oddball_data_for_analysis")

# --- Load all CSV files (one per participant) ---
file_list <- list.files(pattern = "_oddball_data.csv$")

for (file in file_list) {
  # Load participant data
  df <- read_csv(file)
  
  # Keep only phase 2 (active)
  df_active <- df %>% filter(phase == "part2")
  
  # Count rare sounds per image
  rare_counts <- df_active %>%
    group_by(background_image) %>%
    summarise(n_rare = sum(sound_type == "rare", na.rm = TRUE),
              .groups = "drop")
  
  # Save per participant
  participant_id <- unique(df$participant)
  write_csv(rare_counts, paste0("rare_count_", participant_id, ".csv"))
}

oddball <- file_list |>
  lapply(read.csv) |>
  bind_rows()

# --- Focus on Phase 2 (active condition) ---
oddball_active <- oddball |>
  filter(phase == "part2")

# --- Define rules for correctness ---
# Example assumptions (please check if these match your task logic):
# - sound_type == "rare" → should respond (hit if they did)
# - sound_type == "freq" → should not respond (false alarm if they did)
# - context == "between_trials" → any keypress is false alarm
oddball_active <- oddball_active |>
  mutate(
    responded = (key_pressed != "" & !is.na(key_pressed)),
    correct_hit = (sound_type == "rare" & responded),
    miss = (sound_type == "rare" & !responded),
    false_alarm = ((sound_type == "freq" & responded) | 
                     (context == "between_trials" & responded)),
    correct_rejection = (sound_type == "freq" & !responded),
    accuracy_trial = (sound_type %in% c("rare", "freq"))
  )

# --- Summarize per participant ---
summary_active <- oddball_active |>
  group_by(participant) |>
  summarise(
    n_trials = sum(accuracy_trial, na.rm = TRUE), #total trial number
    n_hits = sum(correct_hit, na.rm = TRUE), #total hits
    n_misses = sum(miss, na.rm = TRUE), #total misses
    n_false_alarms = sum(false_alarm, na.rm = TRUE), #total false alarms
    n_correct_rejections = sum(correct_rejection, na.rm = TRUE), #total correct rejections
    hit_rate = ifelse((n_hits + n_misses) > 0, n_hits / (n_hits + n_misses), NA_real_), #acc for rare (rare_acc)
    fa_rate = ifelse((n_false_alarms + n_correct_rejections) >0, n_false_alarms / (n_false_alarms + n_correct_rejections), NA_real_),
    total_accuracy = ifelse(n_trials > 0, (n_hits + n_correct_rejections) / n_trials, NA_real_),
    mean_rt_hits = mean(rt_from_sound[correct_hit], na.rm = TRUE),
    sd_rt_hits = sd(rt_from_sound[correct_hit], na.rm = TRUE)
  )

# --- Save as a dataframe with all participants ---
summary_active <- as.data.frame(summary_active)

print(summary_active)

#### Preparing data for merge and further analysis ----
# Count rare sounds per image
rare_counts <- oddball_active |>
  group_by(background_image) |>
  summarise(
    n_rare = sum(sound_type == "rare", na.rm = TRUE)
  )

write.csv(rare_counts, "rare_counts_per_image.csv", row.names = FALSE)

