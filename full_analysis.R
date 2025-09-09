#### Libraries ----
library(dplyr)
library(tidyr)
library(readr)
library(tidyverse)

#### Paths ----
oddball_path <- "~/Documents/2nd_Degree/My_Research/data/Pilot_Data_Analysis/oddball_data_for_analysis"
recognition_path <- "~/Documents/2nd_Degree/My_Research/data/Pilot_Data_Analysis/rec_data_for_analysis"

#### Load and preprocess oddball data (active phase only) ----
oddball_files <- list.files(oddball_path, pattern = "_oddball_data.csv$", full.names = TRUE)
oddball <- lapply(oddball_files, read_csv) %>% bind_rows()

oddball_active <- oddball %>%
  filter(phase == "part2") %>%
  mutate(
    responded = !is.na(key_pressed) & key_pressed != "",
    correct_hit = (sound_type == "rare" & responded),
    miss = (sound_type == "rare" & !responded),
    false_alarm = ((sound_type == "freq" & responded) | (context == "between_trials" & responded)),
    correct_rejection = (sound_type == "freq" & !responded),
    accuracy_trial = (sound_type %in% c("rare", "freq"))
  )

# Summary per participant (hits, FA, RT)
summary_active <- oddball_active %>%
  group_by(participant) %>%
  summarise(
    n_trials = sum(accuracy_trial, na.rm = TRUE),
    n_hits = sum(correct_hit, na.rm = TRUE),
    n_misses = sum(miss, na.rm = TRUE),
    n_false_alarms = sum(false_alarm, na.rm = TRUE),
    n_correct_rejections = sum(correct_rejection, na.rm = TRUE),
    hit_rate = ifelse((n_hits + n_misses) > 0, n_hits / (n_hits + n_misses), NA_real_),
    fa_rate = ifelse((n_false_alarms + n_correct_rejections) > 0, n_false_alarms / (n_false_alarms + n_correct_rejections), NA_real_),
    total_accuracy = ifelse(n_trials > 0, (n_hits + n_correct_rejections) / n_trials, NA_real_),
    mean_rt_hits = mean(rt_from_sound[correct_hit], na.rm = TRUE),
    sd_rt_hits = sd(rt_from_sound[correct_hit], na.rm = TRUE),
    .groups = "drop"
  )

#### Rare counts per image - passive blocks ----
rare_counts_passive <- oddball %>%
  filter(phase == "part1") %>%
  group_by(background_image) %>%
  summarise(n_rare = sum(sound_type == "rare", na.rm = TRUE),
            .groups = "drop")

#### Rare counts per image - active blocks (already in script) ----
rare_counts_active <- oddball %>%
  filter(phase == "part2") %>%
  group_by(background_image) %>%
  summarise(n_rare = sum(sound_type == "rare", na.rm = TRUE),
            .groups = "drop")

#### Load and preprocess recognition data ----
recog_files <- list.files(recognition_path, pattern = "_rec_data.csv$", full.names = TRUE)
recognition <- lapply(recog_files, read_csv) %>% bind_rows()

recognition <- recognition %>%
  mutate(correct = as.logical(correct))

# Summary per participant & phase
summary_recog <- recognition %>%
  group_by(participant, phase) %>%
  summarise(
    n_trials = n(),
    accuracy = mean(correct, na.rm = TRUE),
    mean_rt = mean(reaction_time[correct == TRUE], na.rm = TRUE),
    sd_rt = sd(reaction_time[correct == TRUE], na.rm = TRUE),
    n_correct = sum(correct, na.rm = TRUE),
    .groups = "drop"
  )

# Wide format for paired comparisons
summary_wide <- summary_recog %>%
  select(participant, phase, accuracy, mean_rt) %>%
  pivot_wider(names_from = phase, values_from = c(accuracy, mean_rt)) %>%
  rename(
    accuracy_passive = accuracy_recognition1,
    accuracy_active = accuracy_recognition2,
    mean_rt_passive = mean_rt_recognition1,
    mean_rt_active = mean_rt_recognition2
  )

# Paired t-tests
if(nrow(summary_wide) > 1){
  t_accuracy <- t.test(summary_wide$accuracy_active,
                       summary_wide$accuracy_passive,
                       paired = TRUE)
  t_rt <- t.test(summary_wide$mean_rt_active,
                 summary_wide$mean_rt_passive,
                 paired = TRUE)
  
  print(t_accuracy)
  print(t_rt)
} else {
  message("Only one participant: cannot run paired t-test. Accuracy difference = ", 
          summary_wide$accuracy_active - summary_wide$accuracy_passive)
  }

#t_accuracy <- t.test(summary_wide$accuracy_active, summary_wide$accuracy_passive, paired = TRUE)
#t_rt <- t.test(summary_wide$mean_rt_active, summary_wide$mean_rt_passive, paired = TRUE)

#print(t_accuracy)
#print(t_rt)

#### Link recognition trials to rare counts ----
recognition_long <- recognition %>%
  pivot_longer(
    cols = c(left_image, right_image),
    names_to = "position",
    values_to = "image"
  ) %>%
  mutate(
    correct_response = ifelse(correct == TRUE & correct_answer == image, 1, 0)
  ) %>%
  # Merge rare counts from passive blocks for recognition1 (phase == part1)
  left_join(rare_counts_passive, by = c("image" = "background_image")) %>%
  rename(n_rare_passive = n_rare) %>%
  # Merge rare counts from active blocks for recognition2 (phase == part2)
  left_join(rare_counts_active, by = c("image" = "background_image")) %>%
  rename(n_rare_active = n_rare) %>%
  mutate(
    n_rare_passive = replace_na(n_rare_passive, 0),
    n_rare_active = replace_na(n_rare_active, 0)
  )

# Correlations
cor_accuracy_passive <- cor.test(
  recognition_long$n_rare_passive[recognition_long$phase == "part1"],
  recognition_long$correct_response[recognition_long$phase == "part1"],
  method = "spearman"
)

# Recognition2 (active) accuracy vs active rare counts
cor_accuracy_active <- cor.test(
  recognition_long$n_rare_active[recognition_long$phase == "part2"],
  recognition_long$correct_response[recognition_long$phase == "part2"],
  method = "spearman"
)

# RT correlations (only correct trials)
cor_rt_passive <- cor.test(
  recognition_long$reaction_time[recognition_long$phase == "part1" & recognition_long$correct_response == 1],
  recognition_long$n_rare_passive[recognition_long$phase == "part1" & recognition_long$correct_response == 1],
  method = "spearman"
)

cor_rt_active <- cor.test(
  recognition_long$reaction_time[recognition_long$phase == "part2" & recognition_long$correct_response == 1],
  recognition_long$n_rare_active[recognition_long$phase == "part2" & recognition_long$correct_response == 1],
  method = "spearman"
)

print(cor_accuracy_passive)
print(cor_accuracy_active)
print(cor_rt_passive)
print(cor_rt_active)