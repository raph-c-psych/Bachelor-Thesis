# ---- loading packages ----
library(tidyverse)



# ---- loading data sets ----
raw_axcpt <- read_csv('../bachelor_data/data_raw/deAXCPT-raw.csv')
raw_cuedts <- read_csv('../bachelor_data/data_raw/decuedTS-raw.csv')
raw_sternberg <- read_csv('../bachelor_data/data_raw/desternberg-raw.csv')
raw_stroop <- read_csv('../bachelor_data/data_raw/destroop-raw.csv')



# ---- extracting subjects that completed study ----

# -- step one: looking how many unique subjects

ids_axcpt <- unique(raw_axcpt$ID)
ids_cuedts <- unique(raw_cuedts$ID)
ids_sternberg <- unique(raw_sternberg$ID)
ids_stroop <- unique(raw_stroop$ID)

all_ids <- Reduce(union, list(
  ids_axcpt,
  ids_cuedts,
  ids_sternberg,
  ids_stroop
))
# intersect

length(all_ids)
# 181 unique subjects: more than expected -> looking for complete 
# ??????????


# -- step two: locking for subjects with all sessions

# bind together tasks with ID, session, phase
added_tasks <- rbind(
  raw_axcpt |> select(ID, session, phase) |> mutate(task = 'axcpt'),
  raw_cuedts |> select(ID, session, phase) |> mutate(task = 'cuedts'),
  raw_sternberg |> select(ID, session, phase) |> mutate(task = 'sternberg'),
  raw_stroop |> select(ID, session, phase) |> mutate(task = 'stroop')
)

# summarize by subject session (reactive, ...), phase (retest, ...), task
summary_specific <- added_tasks |>
  group_by(ID, session, phase, task) |>
  summarise(n = n()) |>
  ungroup()

# general summary: count combinations of session, phase and task per subject (shoould be 3 x 2 x 4)
summary_general <- summary_specific |>
  group_by(ID) |>
  summarise(n = n())

# looking how many sessions per person
summary(summary_general$n)
# -> 24 instead of 30 max in dissertation. Why?????
length(unique(summary_general$ID))
# amount of unique subjects again 181

# looking for subjects with all 24 sessions
summary_general |>
  filter(n == 24) |>
  nrow()
# resulting in 122 complete data sets

# extracting subject names
complete_sub <- summary_general |>
  filter(n == 24) |>
  pull(ID)

# checking trial amounts per task x modality
clean_spec_sum <- summary_specific |>
  filter(ID %in% complete_sub) |>
  group_by(ID, session, phase, task) |>
  ungroup()

# now summary for session, phase, task (collapse subject)
clean_spec_sum |>
  filter(task == 'sternberg') |>
  group_by(session, phase) |>
  summarise(
    mean_count = mean(n, na.rm = TRUE),
    max_count = max(n, na.rm = TRUE),
    min_count = min(n, na.rm = TRUE),
    median_count = median(n, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  )
# > no problems, all uniform
clean_spec_sum |>
  filter(task == 'stroop') |>
  group_by(session, phase) |>
  summarise(
    mean_count = mean(n, na.rm = TRUE),
    max_count = max(n, na.rm = TRUE),
    min_count = min(n, na.rm = TRUE),
    median_count = median(n, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  )
# > reactive has more trials but homogenous, baseline test has outlier with 576 to 288
clean_spec_sum |>
  filter(task == 'axcpt') |>
  group_by(session, phase) |>
  summarise(
    mean_count = mean(n, na.rm = TRUE),
    max_count = max(n, na.rm = TRUE),
    min_count = min(n, na.rm = TRUE),
    median_count = median(n, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  )
# > all homogenous except once 432 vs. 216 in proactive retest
clean_spec_sum |>
  filter(task == 'cuedts') |>
  group_by(session, phase) |>
  summarise(
    mean_count = mean(n, na.rm = TRUE),
    max_count = max(n, na.rm = TRUE),
    min_count = min(n, na.rm = TRUE),
    median_count = median(n, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  )
# > all homogenous


# -- step three exploring outliers (stroop: baseline test; axcpt: proactive retest)

# - explorting stroop outlier
# get subject stroop
clean_spec_sum |>
  filter(task == 'stroop' & session == 'baseline'& n != 288)
# 85f47384c3 baseline test  stroop   576

# check stroop in raw data
stroop_outlier <- raw_stroop |>
  filter(ID == '85f47384c3' & session == 'baseline' & phase == 'test')
# not different waves: both 2

# checking trial numbers
table(stroop_outlier$trialNum)
# > all duplicated from 3 - 292, but two missing
setdiff(3:292, stroop_outlier$trialNum)
# 147 and 148 missing. Why?

# check if each trial number (occuring twice) are the same (defined by RT)
stroop_outlier |>
  group_by(trialNum) |>
  summarise(
    n_rt_values = n_distinct(RT),
    .groups = "drop"
  ) |>
  filter(n_rt_values > 1) |>
  nrow()
# 287 with differing rts -> truely two different testings for the same session

# check another way for duplicate rows
stroop_outlier |>
  group_by(across(everything())) |>
  summarise(n = n(), .groups = 'drop') |>
  nrow()
# 575 -> only one row duplicate 


# not clear why duplicate ????? -> exclude 85f47384c3


# - exploring axcpt outlier
# get subject axcpt
clean_spec_sum |>
  filter(task == 'axcpt' & session == 'proactive' & n != 216)
# bf2740d798 proactive retest axcpt   432
# 281f4f3cd7 proactive retest axcpt   422


# - exploring bf2740d798
# checking raw data
axcpt_outlier_1 <- raw_axcpt |>
  filter(ID == 'bf2740d798' & session == 'proactive' & phase == 'retest')
# not different waves: both 1

# checking if duplicate rts
table(axcpt_outlier_1$probeReacTime)

# check if complete duplicated rows
axcpt_outlier_1 |>
  group_by(across(everything())) |>
  summarise(n = n(), .groups = 'drop') #|> View()
axcpt_outlier_1 |>
  group_by(across(everything())) |>
  summarise(n = n(), .groups = 'drop') |>
  nrow()
# 216 unique rows > complete duplicates


# - exploring 281f4f3cd7 proactive retest axcpt   422
# checking raw data
axcpt_outlier_2 <- raw_axcpt |>
  filter(ID == '281f4f3cd7' & session == 'proactive' & phase == 'retest')

# check for duplicates
axcpt_outlier_2 |>
  group_by(across(everything())) |>
  summarise(n = n(), .groups = 'drop') # |> View()
axcpt_outlier_2 |>
  group_by(across(everything())) |>
  summarise(n = n(), .groups = 'drop') |>
  nrow()
# 205 duplicated and 12 single > odd
# > exclude 


# -- step 4: outputting cleaned data

# modify list of complete subjects (exclude one tested twice)
complete_sub <- complete_sub[!complete_sub %in% c("85f47384c3", "281f4f3cd7")]

# clean axcpt
clean_axcpt <- raw_axcpt |>
  filter(ID %in% complete_sub)

# clean cuedts
clean_cuedts <- raw_cuedts |>
  filter(ID %in% complete_sub)

# clean sternberg
clean_sternberg <- raw_sternberg |>
  filter(ID %in% complete_sub)

# clean stroop
clean_stroop <- raw_stroop |>
  filter(ID %in% complete_sub)

# remove duplicates in axcpt: bf2740d798 proactive retest axcpt   432
clean_axcpt <- clean_axcpt |>
  filter(
    !(ID == "bf2740d798" &
      session == "proactive" &
      phase == "retest" &
      duplicated(across(everything())))
  )

# clean environment
rm(list = setdiff(
  ls(),
  c(
    "clean_axcpt",
    "clean_stroop",
    "clean_cuedts",
    "clean_sternberg"
  )
))





# ---- check once again for uniformity ----
check_axcpt <- clean_axcpt |>
  group_by(ID, session, phase, trialType) |>
  summarise(n = n())
table(check_axcpt$n)
table(check_axcpt$trialType)
# > check

check_cuedts <- clean_cuedts |>
  group_by(ID, session, phase, congruency) |>
  summarise(n = n())
table(check_cuedts$n)
table(check_cuedts$congruency)
# > check

check_sternberg <- clean_sternberg |>
  group_by(ID, session, phase, trialType) |>
  summarise(n = n())
table(check_sternberg$n)
table(check_sternberg$trialType)
# > check

check_stroop <- clean_stroop |>
  group_by(ID, session, phase, trialType) |>
  summarise(n = n())
table(check_stroop$n)
table(check_stroop$trialType)



# clean environment
rm(list = setdiff(
  ls(),
  c(
    "clean_axcpt",
    "clean_stroop",
    "clean_cuedts",
    "clean_sternberg"
  )
))





# ---- extracting only relevant conditions for each task ----

# filter axcpt
clean_axcpt <- clean_axcpt |>
  filter(trialType %in% c("BX", "BY"))


# filter cued ts
# > nothing, as congurency always applicable


# filter sternberg
clean_sternberg <- clean_sternberg |>
  filter(trialType %in% c("RN", "NN"))

# filter stroop
# > trialType is congruency


# ---- save numbers of trials with correct response per person, session (task & modality), phase ----
# will be used later to determine how much has to be removed

trial_num_axcpt <- clean_axcpt |>
  filter(probeCorrect == 1) |>
  group_by(ID, session, phase, trialType) |>
  summarise(
    n_before = n(),
    .groups = 'drop'
  )

trial_num_cuedts <- clean_cuedts |>
  filter(ACC == 1) |>
  group_by(ID, session, phase, congruency) |>
  summarise(
    n_before = n(),
    .groups = 'drop'
  )

trial_num_sternberg <- clean_sternberg |>
  filter(probeCorrect == 1) |>
  group_by(ID, session, phase, trialType) |>
  summarise(
    n_before = n(),
    .groups = 'drop'
  )

trial_num_stroop <- clean_stroop |>
  filter(ACC == 1) |>
  group_by(ID, session, phase, trialType) |>
  summarise(
    n_before = n(),
    .groups = 'drop'
  )

# -- short exploration of counts

# axcpt
summary(trial_num_axcpt$n_before)
table(trial_num_axcpt$n_before)
# > conceringly little values in BX (sometimes only one correct)
# check the subjects and see if excluded at the end
trial_num_axcpt |>
  filter(n_before < 10) |>
  pull(ID) |>
  unique()
temp_axcpt <- clean_axcpt |>
  group_by(ID, session, phase, trialType) |>
  summarise(n = n())
table(temp_axcpt$n)
table(temp_axcpt$trialType)
# only 18 & 72 of one trial type in different combinations
# BX only occurs 18 times per session !!!!!
table(temp_axcpt$trialType)
rm(temp_axcpt)


# cuedts
summary(trial_num_cuedts$n_before)
table(trial_num_cuedts$n_before)
# > acceptable

# sternberg
summary(trial_num_sternberg$n_before)
table(trial_num_sternberg$n_before)
# > very concerning
trial_num_sternberg |>
  filter(n_before < 10) |>
  pull(ID) |>
  unique()
# if 10 cutoff then not really possible -> 97 excluded
trial_num_sternberg |>
  filter(n_before < 6) |>
  pull(ID) |>
  unique() 
# 15 excluded
# only 12 & 48 trials per condition per session#
# reactive NN: 12 <-> baseline & proactive RN 12
temp_sternberg <- clean_sternberg |>
  group_by(ID, session, phase, trialType) |>
  summarise(n = n()) #|>
  #View()
table(temp_sternberg$n)
table(temp_sternberg$trialType)
table(temp_sternberg$trialType)
rm(temp_sternberg)


# stroop
summary(trial_num_stroop$n_before)
table(trial_num_stroop$n_before)
# > no problem





# ---- removal RTs of wrong trials, under 200 ms and over 3 SDs ----

# -- axcpt
# removal of RTs of wrong trials and under 200ms
clean_axcpt <- clean_axcpt |>
  mutate(
    probeReacTime = case_when(
      probeCorrect == 1 & probeReacTime >= 200 ~ probeReacTime,
      .default = NA
    )
  )

# create cutoff values (3SDs) per subject, sessionm phase and trial type
axcpt_cutoffs <- clean_axcpt |>
  group_by(ID, session, phase, trialType) |>
  summarise(
    mean_rt = mean(probeReacTime, na.rm = TRUE),
    sd_rt = sd(probeReacTime, na.rm = TRUE),
    upper_limit = mean_rt + 3 * sd_rt,
    .groups = 'drop'
  ) |>
  select(-c(mean_rt, sd_rt))

# join upper cutoffs to clean data and remove over 3SDs
clean_axcpt <- clean_axcpt |>
  right_join(
    axcpt_cutoffs,
    by = c("ID", "session", "phase", "trialType"),
    relationship = "many-to-many"
  ) |>
  mutate(
    probeReacTime = case_when(
      probeReacTime <= upper_limit ~ probeReacTime,
      .default = NA
    )
  )


# -- cued task switching
# removal of RTs of wrong trials and under 200ms
clean_cuedts <- clean_cuedts |>
  mutate(
    RT = case_when(
      ACC == 1 & RT >= 200 ~ RT,
      .default = NA
    )
  )

# create cutoff values (3SDs) per subject, sessionm phase and trial type
cuedts_cutoffs <- clean_cuedts |>
  group_by(ID, session, phase, congruency) |>
  summarise(
    mean_rt = mean(RT, na.rm = TRUE),
    sd_rt = sd(RT, na.rm = TRUE),
    upper_limit = mean_rt + 3 * sd_rt,
    .groups = 'drop'
  ) |>
  select(-c(mean_rt, sd_rt))

# join upper cutoffs to clean data and remove over 3SDs
clean_cuedts <- clean_cuedts |>
  right_join(
    cuedts_cutoffs,
    by = c("ID", "session", "phase", "congruency"),
    relationship = "many-to-many"
  ) |>
  mutate(
    RT = case_when(
      RT <= upper_limit ~ RT,
      .default = NA
    )
  )


# -- sternberg
# removal of RTs of wrong trials and under 200ms
clean_sternberg <- clean_sternberg |>
  mutate(
    RT = case_when(
      probeCorrect == 1 & RT >= 200 ~ RT,
      .default = NA
    )
  )

# create cutoff values (3SDs) per subject, sessionm phase and trial type
sternberg_cutoffs <- clean_sternberg |>
  group_by(ID, session, phase, trialType) |>
  summarise(
    mean_rt = mean(RT, na.rm = TRUE),
    sd_rt = sd(RT, na.rm = TRUE),
    upper_limit = mean_rt + 3 * sd_rt,
    .groups = 'drop'
  ) |>
  select(-c(mean_rt, sd_rt))

# join upper cutoffs to clean data and remove over 3SDs
clean_sternberg <- clean_sternberg |>
  right_join(
    sternberg_cutoffs,
    by = c("ID", "session", "phase", "trialType"),
    relationship = "many-to-many"
  ) |>
  mutate(
    RT = case_when(
      RT <= upper_limit ~ RT,
      .default = NA
    )
  )


# - stroop
# removal of RTs of wrong trials and under 200ms
clean_stroop <- clean_stroop |>
  mutate(
    RT = case_when(
      ACC == 1 & RT >= 200 ~ RT,
      .default = NA
    )
  )

# create cutoff values (3SDs) per subject, sessionm phase and trial type
stroop_cutoffs <- clean_stroop |>
  group_by(ID, session, phase, trialType) |>
  summarise(
    mean_rt = mean(RT, na.rm = TRUE),
    sd_rt = sd(RT, na.rm = TRUE),
    upper_limit = mean_rt + 3 * sd_rt,
    .groups = 'drop'
  ) |>
  select(-c(mean_rt, sd_rt))

# join upper cutoffs to clean data and remove over 3SDs
clean_stroop <- clean_stroop |>
  right_join(
    stroop_cutoffs,
    by = c("ID", "session", "phase", "trialType"),
    relationship = "many-to-many"
  ) |>
  mutate(
    RT = case_when(
      RT <= upper_limit ~ RT,
      .default = NA
    )
  )


# remove objects
rm(axcpt_cutoffs, stroop_cutoffs, sternberg_cutoffs, cuedts_cutoffs)




# ---- check how many trials were excluded ----

# -- axcpt
# check correct trials after removal
trial_num_axcpt2 <- clean_axcpt |>
  filter(!is.na(probeReacTime)) |>
  group_by(ID, session, phase, trialType) |>
  summarise(
    n_after = n(),
    .groups = 'drop'
  )
# join trials before and after
trial_num_axcpt <- trial_num_axcpt |>
  right_join(
    trial_num_axcpt2,
    by = c("ID", "session", "phase", "trialType")
  ) |>
  mutate(kept_percent = (n_after / n_before) * 100)
rm(trial_num_axcpt2)

# check how many trials were excluded
(1 - (sum(trial_num_axcpt$n_after) / sum(trial_num_axcpt$n_before))) * 100
# > 3.17

# saving for checking which to exclude
rt_summary <- trial_num_axcpt  |> mutate(task = 'axcpt')


# -- cued ts
trial_num_cuedts2 <- clean_cuedts |>
  filter(!is.na(RT)) |>
  group_by(ID, session, phase, congruency) |>
  summarise(
    n_after = n(),
    .groups = 'drop'
  )
# join trials before and after
trial_num_cuedts <- trial_num_cuedts |>
  right_join(
    trial_num_cuedts2,
    by = c("ID", "session", "phase", "congruency")
  ) |>
  mutate(kept_percent = (n_after / n_before) * 100)
rm(trial_num_cuedts2)

# check how many trials were excluded
(1 - (sum(trial_num_cuedts$n_after) / sum(trial_num_cuedts$n_before))) * 100
# > 1.52

# saving for checking which to exclude
rt_summary <- rbind(
  rt_summary,
  trial_num_cuedts |> rename(trialType = congruency)  |> mutate(task = 'cuedts')
)


# -- sternberg
trial_num_sternberg2 <- clean_sternberg |>
  filter(!is.na(RT)) |>
  group_by(ID, session, phase, trialType) |>
  summarise(
    n_after = n(),
    .groups = 'drop'
  )
# join trials before and after
trial_num_sternberg <- trial_num_sternberg |>
  right_join(
    trial_num_sternberg2,
    by = c("ID", "session", "phase", "trialType")
  ) |>
  mutate(kept_percent = (n_after / n_before) * 100)
rm(trial_num_sternberg2)

# check how many trials were excluded
(1 - (sum(trial_num_sternberg$n_after) / sum(trial_num_sternberg$n_before))) * 100
# > 1.23

# saving for checking which to exclude
rt_summary <- rbind(
  rt_summary,
  trial_num_sternberg |> mutate(task = 'sternberg')
)


# -- stroop
trial_num_stroop2 <- clean_stroop |>
  filter(!is.na(RT)) |>
  group_by(ID, session, phase, trialType) |>
  summarise(
    n_after = n(),
    .groups = 'drop'
  )
# join trials before and after
trial_num_stroop <- trial_num_stroop |>
  right_join(
    trial_num_stroop2,
    by = c("ID", "session", "phase", "trialType")
  ) |>
  mutate(kept_percent = (n_after / n_before) * 100)
rm(trial_num_stroop2)

# check how many trials were excluded
(1 - (sum(trial_num_stroop$n_after) / sum(trial_num_stroop$n_before))) * 100
# > 1.70

# saving for checking which to exclude
rt_summary <- rbind(
  rt_summary,
  trial_num_stroop |> mutate(task = 'stroop')
)

# remove single objects
rm(trial_num_axcpt, trial_num_cuedts, trial_num_sternberg, trial_num_stroop)




# ---- excluding if half of the trials were excluded ----

# study 1: exclude session type (task x modality)
rt_exclude_1 <- rt_summary |>
  group_by(ID, task, session) |>
  slice_min(kept_percent) |>
  slice_head(n = 1) |> # removes duplicates with same max values
  filter(kept_percent <= 50) # exclude if under 50 percent trials kept
############################################################################################
# RT exclustion
############################################################################################
# study 2: exclude whole participant
rt_exclude_2 <- rt_summary |>
  group_by(ID) |>
  slice_min(kept_percent) |>
  slice_head(n = 1) |> # removes duplicates with same max values
  filter(kept_percent <= 50) # exclude if under 50 percent trials kept




############################################################################################
# Option A error removal: 40 % fixed error rate
############################################################################################
# ---- check participants with over 40% error rate ----
error_axcpt <- clean_axcpt |>
  group_by(ID, session, phase, trialType) |>
  summarise(
    error_rate = 1 - mean(probeCorrect),
    .groups = 'drop'
  ) |>
  mutate(
    task = 'axcpt'
  )

error_cuedts <- clean_cuedts |>
  group_by(ID, session, phase, congruency) |>
  summarise(
    error_rate = 1 - mean(ACC),
    .groups = 'drop'
  ) |>
  mutate(
    task = 'cuedts'
  )

error_sternberg <- clean_sternberg |>
  group_by(ID, session, phase, trialType) |>
  summarise(
    error_rate = 1 - mean(probeCorrect),
    .groups = 'drop'
  ) |>
  mutate(
    task = 'sternberg'
  )

error_stroop <- clean_stroop |>
  group_by(ID, session, phase, trialType) |>
  summarise(
    error_rate = 1 - mean(ACC),
    .groups = 'drop'
  ) |>
  mutate(
    task = 'stroop'
  )

# sum up all the error  lists
error_summary <- rbind(error_axcpt, error_cuedts |> rename(trialType = congruency), error_sternberg, error_stroop)
rm(error_axcpt, error_cuedts, error_sternberg, error_stroop)



# -- study 1: sessions to exclude
error_exclude_1 <- error_summary |>
  group_by(ID, task, session) |>
  slice_max(error_rate, n = 1) |>
  slice_head(n = 1) |> # removes duplicates with same max values
  filter(error_rate >= 0.4) # exclude if over 50 error trials in one condition

############################################################################################
# ER exclustion
############################################################################################

# -- study 2: participants to exclude
error_exclude_2 <- error_summary |>
  group_by(ID) |>
  slice_max(error_rate) |>
  slice_head(n = 1) |> # removes duplicates with same max values
  filter(error_rate >= 0.5) # exclude if over 50 error trials in one condition
# > if 40% then 64!!! if 50% then 38




############################################################################################
# Option B error removal: 3 SDs over mean
############################################################################################
# ---- check participants with high error rate 3SDs ----

# -- axcpt
error_cutoff_axcpt <- clean_axcpt |>
  group_by(ID, session, phase, trialType) |>
  summarise(
    mean_er_within = 1 - mean(probeCorrect),
    .groups = 'drop'
  ) |>
  group_by(session, phase, trialType) |>
  summarise(
    mean_er_between = mean(mean_er_within),
    sd_er_between = sd(mean_er_within),
    upper_limit = mean_er_between + 3 * sd_er_between,
    .groups = 'drop'
  )
error_axcpt <- clean_axcpt |>
  group_by(ID, session, phase, trialType) |>
  summarise(
    error_rate = 1 - mean(probeCorrect),
    .groups = 'drop'
  ) |>
  mutate(
    task = 'axcpt'
  ) |>
  right_join(error_cutoff_axcpt, by = c('session', 'phase', 'trialType'), relationship = 'many-to-many')


# -- cued ts
error_cutoff_cuedts <- clean_cuedts |>
  group_by(ID, session, phase, congruency) |>
  summarise(
    mean_er_within = 1 - mean(ACC),
    .groups = 'drop'
  ) |>
  group_by(session, phase, congruency) |>
  summarise(
    mean_er_between = mean(mean_er_within),
    sd_er_between = sd(mean_er_within),
    upper_limit = mean_er_between + 3 * sd_er_between,
    .groups = 'drop'
  )
error_cuedts <- clean_cuedts |>
  group_by(ID, session, phase, congruency) |>
  summarise(
    error_rate = 1 - mean(ACC),
    .groups = 'drop'
  ) |>
  mutate(
    task = 'cuedts'
  ) |>
  right_join(error_cutoff_cuedts, by = c('session', 'phase', 'congruency'), relationship = 'many-to-many')




# -- sternberg
error_cutoff_sternberg <- clean_sternberg |>
  group_by(ID, session, phase, trialType) |>
  summarise(
    mean_er_within = 1 - mean(probeCorrect),
    .groups = 'drop'
  ) |>
  group_by(session, phase, trialType) |>
  summarise(
    mean_er_between = mean(mean_er_within),
    sd_er_between = sd(mean_er_within),
    upper_limit = mean_er_between + 3 * sd_er_between,
    .groups = 'drop'
  )
error_sternberg <- clean_sternberg |>
  group_by(ID, session, phase, trialType) |>
  summarise(
    error_rate = 1 - mean(probeCorrect),
    .groups = 'drop'
  ) |>
  mutate(
    task = 'sternberg'
  ) |>
  right_join(error_cutoff_sternberg, by = c('session', 'phase', 'trialType'), relationship = 'many-to-many')



# -- stroop
error_cutoff_stroop <- clean_stroop |>
  group_by(ID, session, phase, trialType) |>
  summarise(
    mean_er_within = 1 - mean(ACC),
    .groups = 'drop'
  ) |>
  group_by(session, phase, trialType) |>
  summarise(
    mean_er_between = mean(mean_er_within),
    sd_er_between = sd(mean_er_within),
    upper_limit = mean_er_between + 3 * sd_er_between,
    .groups = 'drop'
  )
error_stroop <- clean_stroop |>
  group_by(ID, session, phase, trialType) |>
  summarise(
    error_rate = 1 - mean(ACC),
    .groups = 'drop'
  ) |>
  mutate(
    task = 'stroop'
  ) |>
  right_join(error_cutoff_stroop, by = c('session', 'phase', 'trialType'), relationship = 'many-to-many')


# sum up all the error  lists
error_summary <- rbind(error_axcpt, error_cuedts |> rename(trialType = congruency), error_sternberg, error_stroop)
rm(error_axcpt, error_cuedts, error_sternberg, error_stroop)



# -- explore all over upper limit
error_exclude_overview <- error_summary |>
  mutate(exclude = error_rate > upper_limit)
error_exclude_overview |>
  filter(exclude == FALSE) |>
  filter(error_rate > 0.5) #|> View()
error_exclude_overview |>
  filter(exclude == TRUE) |>
  filter(error_rate > 0.5) #|> View()
error_exclude_overview |>
  filter(exclude == TRUE) |>
  filter(error_rate <= 0.5) # |> View()
rm(error_exclude_overview)

# -- study 1: sessions to exclude
error_exclude_1 <- error_summary |>
  filter(error_rate > upper_limit) |>
  group_by(ID, session, task) |>
  slice_head(n = 1)

############################################################################################
# ER exclustion
############################################################################################

# -- study 2: participants to exclude
error_exclude_2 <- error_summary |>
  filter(error_rate > upper_limit) |>
  group_by(ID) |>
  slice_head(n = 1)










# ---- check joint exclusion of rt and error rate

# -- study 1: session level
all_exclude_1 <- rbind(
    error_exclude_1 |> select(ID, session, task),
    rt_exclude_1 |> select(ID, session, task)
  ) |>
  distinct()
# > 72 session in this case. Acceptable


# -- study 2: participant level
all_exclude_2 <- rbind(
    error_exclude_2 |> select(ID),
    rt_exclude_2 |> select(ID)
  ) |>
  distinct()
# > 42 session in this case. Acceptable





# ---- outputting relevant data sets for both studies ----

# -- study 1
axcpt_study_1 <- clean_axcpt |>
  anti_join(all_exclude_1 |> filter(task == 'axcpt'), by = c('ID', 'session'))
write.csv(axcpt_study_1, file = '../bachelor_data/data_study_1/axcpt_1.csv')

cuedts_study_1 <- clean_cuedts |>
  anti_join(all_exclude_1 |> filter(task == 'cuedts'), by = c('ID', 'session'))
write.csv(cuedts_study_1, file = '../bachelor_data/data_study_1/cuedts_1.csv')

sternberg_study_1 <- clean_sternberg |>
  anti_join(all_exclude_1 |> filter(task == 'sternberg'), by = c('ID', 'session'))
write.csv(sternberg_study_1, file = '../bachelor_data/data_study_1/sternberg_1.csv')

stroop_study_1 <- clean_stroop |>
  anti_join(all_exclude_1 |> filter(task == 'stroop'), by = c('ID', 'session'))
write.csv(stroop_study_1, file = '../bachelor_data/data_study_1/stroop_1.csv')

(nrow(axcpt_study_1) / nrow(clean_axcpt)) * 100
(nrow(cuedts_study_1) / nrow(clean_cuedts)) * 100
(nrow(sternberg_study_1) / nrow(clean_sternberg)) * 100
(nrow(stroop_study_1) / nrow(clean_stroop)) * 100


# -- study 2
axcpt_study_2 <- clean_axcpt |>
  filter(!(ID %in% all_exclude_2$ID))
write.csv(axcpt_study_2, file = '../bachelor_data/data_study_2/axcpt_2.csv')

cuedts_study_2 <- clean_cuedts |>
  filter(!(ID %in% all_exclude_2$ID))
write.csv(cuedts_study_2, file = '../bachelor_data/data_study_2/cuedts_2.csv')

sternberg_study_2 <- clean_sternberg |>
  filter(!(ID %in% all_exclude_2$ID))
write.csv(sternberg_study_2, file = '../bachelor_data/data_study_2/sternberg_2.csv')

stroop_study_2 <- clean_stroop |>
  filter(!(ID %in% all_exclude_2$ID))
write.csv(stroop_study_2, file = '../bachelor_data/data_study_2/stroop_2.csv')


(nrow(axcpt_study_2) / nrow(clean_axcpt)) * 100
(nrow(cuedts_study_2) / nrow(clean_cuedts)) * 100
(nrow(sternberg_study_2) / nrow(clean_sternberg)) * 100
(nrow(stroop_study_2) / nrow(clean_stroop)) * 100
# > 65% of data kept