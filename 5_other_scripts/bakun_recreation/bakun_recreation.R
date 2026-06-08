library(tidyverse)
library(lme4)
library(sjPlot)
library(boot)
library(psych)


# ---- pre processing data -----
# -- axcpt
# grouping by ID, trialType (congruency), session (reactive/baseline), phase (test/retest)
# ACC, RT
axcpt_data <- read_csv('../bachelor_data/data_study_2/axcpt_2.csv') |>
  group_by(ID, trialType, session, phase) |>
  summarise(
    mean_RT = mean(probeReacTime, na.rm = TRUE),
    ER = 1 - mean(probeCorrect),
    .groups = 'drop'
  ) |>
  mutate(
    trialType = case_when(
      trialType == 'BX' ~ 'Incongruent',
      trialType == 'BY' ~ 'Congruent'
    ),
    task = 'axcpt'
  )

# -- sternberg
# grouping by ID, trialType (congruency), session (reactive/baseline), phase (test/retest)
# ACC, RT
sternberg_data <- read_csv('../bachelor_data/data_study_2/sternberg_2.csv') |>
  group_by(ID, trialType, session, phase) |>
  summarise(
    mean_RT = mean(RT, na.rm = TRUE),
    ER = 1 - mean(probeCorrect),
    .groups = 'drop'
  ) |>
  mutate(
    trialType = case_when(
      trialType == 'RN' ~ 'Incongruent',
      trialType == 'NN' ~ 'Congruent'
    ),
    task = 'sternberg'
  )


# -- stroop
# grouping by ID, trialType (congruency), session (reactive/baseline), phase (test/retest)
# ACC, RT
stroop_data <- read_csv('../bachelor_data/data_study_2/stroop_2.csv') |>
  group_by(ID, trialType, session, phase) |>
  summarise(
    mean_RT = mean(RT, na.rm = TRUE),
    ER = 1 - mean(ACC),
    .groups = 'drop'
  ) |>
  mutate(
    trialType = case_when(
      trialType == '1' ~ 'Incongruent',
      trialType == '2' ~ 'Congruent'
    ),
    task = 'stroop'
  )

# -- cued task swtiching
# grouping by ID, trialType (congruency), session (reactive/baseline), phase (test/retest)
# ACC, RT
cuedts_data <- read_csv('../bachelor_data/data_study_2/cuedts_2.csv') |>
  group_by(ID, congruency, session, phase) |>
  summarise(
    mean_RT = mean(RT, na.rm = TRUE),
    ER = 1 - mean(ACC),
    .groups = 'drop'
  ) |>
  mutate(
    task = 'cuedts'
  ) |>
  rename(trialType = congruency)


# -- adding together tasks
all_data <- rbind(
  axcpt_data,
  cuedts_data,
  sternberg_data,
  stroop_data
)
rm(axcpt_data, cuedts_data, sternberg_data, stroop_data)





# ---- function for ICC ----
calc_corrs <- function(data_temp, rt_weighting){
  # filter out only one task x modality
  data_temp <- data_temp |>
    filter(session == 'baseline' & task == 'axcpt')
  # calculate BIS with choseen weighting
  data_temp <- data_temp |>
    group_by(session, task, phase) |>
    mutate(
      z_RT = as.numeric(scale(mean_RT)),
      z_ER = as.numeric(scale(ER)),
      BIS = - (rt_weighting) * z_RT - (1 - rt_weighting) * z_ER # afterwards could be test retest
    ) |>
    select(-c('z_RT', 'z_ER', 'mean_RT', 'ER')) |>
    ungroup()
  # pivot wider and calculate the cognitive control measure
  data_temp <- data_temp |>
    pivot_wider(
      names_from = trialType,
      values_from = BIS, # can be supplemented with test retest
      names_glue = "BIS_{trialType}"
    ) |>
    mutate(
      CC_measure = BIS_Incongruent - BIS_Congruent
      # CC_retest = BIS_retest_BX - BIS_retest_BY
    )

  # pivot data set for phase
  data_temp <- data_temp |>
    pivot_wider(
      id_cols = c(ID, task, session),
      names_from = phase,
      values_from = CC_measure,
      names_glue = "CC_{phase}"
    )
  # calculate corellations for each task and modality
  ICCs <- data_temp |>
    group_by(task, session) |>
    summarise(
      r = psych::ICC(data.frame(CC_test, CC_retest), lmer = FALSE)$results["Single_fixed_raters", "ICC"],
      n = sum(complete.cases(CC_test, CC_retest)),
      .groups = "drop"
    )

  # return correlation matrix
  return(ICCs)
}




# ---- testing ICC function -----
# calculating correlation matrixes for different weights
weights <- seq(0, 1, by = 0.05)
corr_list <- tibble(
  rt_weight = numeric(),
  task = character(),
  session = character(),
  r = numeric(),
  n = numeric()
)
for (curr_weight in weights) {
  mod_data <- all_data
  curr_corrs <- calc_corrs(mod_data, curr_weight) |>
    mutate(rt_weight = curr_weight)
  corr_list <- rbind(corr_list, curr_corrs)
}
# multiple failed convergences in cuedts reactive and sternberg proactive
# all_data |> filter(task == 'cuedts' & session == 'reactive') |> View()
# curr_corrs <- calc_corrs(mod_data, 1)
# > doenst converge for 0.2, 0.4, 0.7, 0.9
# all_data |> filter(task == 'sternberg' & session == 'proactive') |> View()

# remove unused objects
rm(mod_data, curr_corrs, weights, curr_weight)



# creating fisher z-transformations and grouping for singular sessions
corr_list <- corr_list |>
  mutate(
    z_r = fisherz(r),
    group = interaction(task, session, sep = '-')
  )






# ---- boot strapping regression parameters ----

# bootstrapping function
boot_fun <- function(ids, indices) {

  # get participants ids used for sample
  sampled_ids <- ids[indices]

  # create bootstrap data set
  boot_data <- tibble()

  for(i in seq_along(sampled_ids)) {

    curr_id <- sampled_ids[i]

    curr_data <- all_data |>
      filter(ID == curr_id) |>
      mutate(
        ID = paste0(curr_id, "_boot", i) # i for differing between duplicated participants
      )

    boot_data <- bind_rows(boot_data, curr_data)
  }

  # calculate correlations for all weights
  weights <- c(0, 0.5, 1)

  corr_list <- tibble()

  for(curr_weight in weights) {

    curr_corrs <- calc_corrs(boot_data, curr_weight) |>
      mutate(
        rt_weight = curr_weight
      )

    corr_list <- bind_rows(corr_list, curr_corrs)
  }

  # prepare data for MLM
  corr_list <- corr_list |>
    mutate(
      z_r = fisherz(r),
      group = interaction(task, session, sep = "-")
    )

  # extract coefficients
  rt_r <- corr_list |> filter(rt_weight == 1) |> pull(r)
  bis_r <- corr_list |> filter(rt_weight == 0.5) |> pull(r)
  er_r <- corr_list |> filter(rt_weight == 0) |> pull(r)

  return(c(rt_r, bis_r, er_r))
}

# saving participant ids, so that bootstrapping can be on participant level
participant_ids <- unique(all_data$ID)


set.seed(123)

boot_out <- boot(
  data = participant_ids,
  statistic = boot_fun,
  R = 500,
  parallel = 'multicore',
  ncpus = 7
)


saveRDS(boot_out, 'other_plots/bakun_re.rds')
boot_out <- readRDS("other_plots/bakun_re.rds")

# look at estimates
boot_estimates <- as_tibble(boot_out$t)
names(boot_estimates) <- c("rt_r", "bis_r", "er_r")

# ---- create table of confidence intervals ----
ci_list <- tibble(
  index = c(1, 2, 3),
  type = c("Reaction Time", "BIS", "Error Rate"),
  mean = NA_real_,
  lower = NA_real_,
  upper = NA_real_
)

for (i in ci_list$index) {
  
  ci_out <- boot.ci(boot_out, type = "bca", index = i)
  
  ci_list$mean[ci_list$index == i]  <- mean(boot_out$t[, i], na.rm = TRUE)
  ci_list$lower[ci_list$index == i] <- ci_out$bca[4]
  ci_list$upper[ci_list$index == i] <- ci_out$bca[5]
}

# reorder factor for plot
ci_list <- ci_list |>
  mutate(type = factor(
    type, 
    levels = c("Error Rate", "BIS", "Reaction Time")
    )
  )


# ---- create plot ----
# combined plot
whole_plot <- ggplot(data = ci_list, aes(x = type, y = mean)) +
  # mean data points
  geom_point() +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.2,
    linewidth = 0.8
  ) +
  labs(
    y = "ICC",
    x = 'Type of measure'
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.ticks = element_line(color = "black", linewidth = 0.7),
    panel.grid = element_blank(),      # remove all grid lines
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.line.x = element_line(
      color = "black"
    ),
    axis.line.y = element_line(
      color = "black",
      arrow = arrow(length = unit(0.7, "cm"))
    )
  )



ggsave(plot = whole_plot, filename = 'other_plots/bakun_rectreation.png', height = 5, width = 7)

system("open other_plots/bakun_rectreation.png")
