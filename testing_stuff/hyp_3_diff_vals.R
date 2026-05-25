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





# ---- calculating optimal weights for each task x modality ----
# function for correlating ICC
calc_ICCs <- function(data_temp, rt_weighting){
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

# calculating the weighting in a finer grid
weights <- seq(0, 1, by = 0.01)
icc_list <- tibble(
  rt_weight = numeric(),
  task = character(),
  session = character(),
  r = numeric(),
  n = numeric()
)
for (curr_weight in weights) {
  mod_data <- all_data
  curr_iccs <- calc_ICCs(mod_data, curr_weight) |>
    mutate(rt_weight = curr_weight)
  icc_list <- rbind(icc_list, curr_iccs)
}
rm(curr_iccs, mod_data, curr_weight, weights, calc_ICCs)

# extracting maximal weights
optimal_weights <- icc_list |>
  group_by(task, session) |>
  slice_max(r, n = 1) |>
  select(-c(n, r))
rm(icc_list)





# ---- function: calculating correlations between sessions through pearson ----
calc_corrs <- function(data_temp, rt_weight_list){
  # match individual weighting with task x session
  data_temp <- data_temp |>
    right_join(rt_weight_list, by = c('task', 'session'))
  # calculate BIS with choseen weighting
  data_temp <- data_temp |>
    group_by(session, task) |>
    mutate(
      z_RT = as.numeric(scale(mean_RT)),
      z_ER = as.numeric(scale(ER)),
      BIS = - (rt_weight) * z_RT - (1 - rt_weight) * z_ER # afterwards could be test retest
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

  # pivot data set for task and modality
  data_temp <- data_temp |>
    pivot_wider(
      id_cols = ID,
      names_from = c(task, session),
      values_from = CC_measure,
      names_glue = "CC_{task}_{session}"
    )
  # calculate correlation matrix
  all_corr <- data_temp |>
    select(contains("CC")) |>
    cor(use = "pairwise.complete.obs")

  # return correlation matrix
  return(all_corr)
}



# ---- testing function to create correlations for different weight types ----

# creating weight lists for rt and er
er_weights <- optimal_weights |>
  mutate(rt_weight = 0)
rt_weights <- optimal_weights |>
  mutate(rt_weight = 1)

# calculating correlation matrixes for different weights
types_weight <- c('rt', 'er', 'optimal')
corr_long <- tibble(
  weight_type = character(),
  correlation = numeric(),
  group = character()
)
for (curr_type in types_weight) {
  # take only test data
  mod_data <- all_data |>
    filter(phase == "test")
  # create correlation matrix for current weight between all sessions
  if(curr_type == 'rt'){
    corr_matrix <- calc_corrs(mod_data, rt_weights)
  } else if (curr_type == 'er'){
    corr_matrix <- calc_corrs(mod_data, er_weights)
  } else if (curr_type == 'optimal'){
    corr_matrix <- calc_corrs(mod_data, optimal_weights)
  } 
  
  # create clean long format of correlations
  corr_data <- corr_matrix |>
    as.data.frame() |>
    rownames_to_column("var1") |>
    pivot_longer(
      cols = -var1,
      names_to = "var2",
      values_to = "correlation"
    ) |>
    filter(var1 < var2) |> # exclude duplicates and auto correlations
    mutate(weight_type = curr_type) |>
    mutate(
      group = paste0(str_sub(var1, 4), '-', str_sub(var2, 4)) # create general variable for corr
    ) |>
    select(-c(var1, var2))
  corr_long <- rbind(corr_long, corr_data)
}
rm(corr_data, corr_matrix, mod_data, curr_type, types_weight, er_weights, rt_weights, optimal_weights)

# creating fisher z-transformations and grouping for singular sessions
corr_long <- corr_long |>
  mutate(
    z_r = fisherz(correlation),
  )

# small exploration of different correlations
ggplot(corr_long, aes(x = weight_type, y = z_r)) +
  geom_boxplot()
# > man looks bad for optimal






# ---- multilevel model: rt vs. optimal ----
corr_rt_opt <- corr_long |>
  filter(weight_type %in% c('rt', 'optimal'))
mlm <- lmer(
  z_r ~
    weight_type +
    (1 | group),
  data = corr_rt_opt
)
summary(mlm)

# extract values
fixef(mlm) 

fe_intercept <- fixef(mlm)['(Intercept)']
fe_weighttype <- fixef(mlm)['weight_typert']

sd_re_intercept <- as_tibble(VarCorr(mlm))|> filter(var1 == '(Intercept)') |> pull(sdcor)


# plotting random effects
plot_model(
  mlm,
  type = "re"
)






# ---- boot strapping MLM parameters ----
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

  # creating weight lists for rt and er
  er_weights <- optimal_weights |>
    mutate(rt_weight = 0)
  rt_weights <- optimal_weights |>
    mutate(rt_weight = 1)

  # calculating correlation matrixes for different weights
  types_weight <- c('rt', 'er', 'optimal')
  corr_long <- tibble(
    weight_type = character(),
    correlation = numeric(),
    group = character()
  )
  for (curr_type in types_weight) {
    # take only test data
    mod_data <- boot_data |>
      filter(phase == "test")
    # create correlation matrix for current weight between all sessions
    if(curr_type == 'rt'){
      corr_matrix <- calc_corrs(mod_data, rt_weights)
    } else if (curr_type == 'er'){
      corr_matrix <- calc_corrs(mod_data, er_weights)
    } else if (curr_type == 'optimal'){
      corr_matrix <- calc_corrs(mod_data, optimal_weights)
    } 
    
    # create clean long format of correlations
    corr_data <- corr_matrix |>
      as.data.frame() |>
      rownames_to_column("var1") |>
      pivot_longer(
        cols = -var1,
        names_to = "var2",
        values_to = "correlation"
      ) |>
      filter(var1 < var2) |> # exclude duplicates and auto correlations
      mutate(weight_type = curr_type) |>
      mutate(
        group = paste0(str_sub(var1, 4), '-', str_sub(var2, 4)) # create general variable for corr
      ) |>
      select(-c(var1, var2))
    corr_long <- rbind(corr_long, corr_data)
  }

  # prepare data for MLM
  corr_long <- corr_long |>
    mutate(
      z_r = fisherz(correlation),
      weight_type = factor(weight_type)
    )

  #  RT vs optimal
  corr_rt_opt <- corr_long |>
    filter(weight_type %in% c("rt", "optimal")) |>
    mutate(weight_type = relevel(factor(weight_type), ref = "rt"))

  mlm_rt_opt <- lmer(
    z_r ~ weight_type + (1 | group),
    data = corr_rt_opt
  )

  rt_fe_intercept <- fixef(mlm_rt_opt)['(Intercept)']
  rt_fe_weighttype <- fixef(mlm_rt_opt)['weight_typeoptimal']

  rt_sd_re_intercept <- as_tibble(VarCorr(mlm_rt_opt))|> filter(var1 == '(Intercept)') |> pull(sdcor)


  # ER vs optimal
  corr_er_opt <- corr_long |>
    filter(weight_type %in% c("er", "optimal")) |>
    mutate(weight_type = relevel(factor(weight_type), ref = "er"))

  mlm_er_opt <- lmer(
    z_r ~ weight_type + (1 | group),
    data = corr_er_opt
  )

  er_fe_intercept <- fixef(mlm_er_opt)['(Intercept)']
  er_fe_weighttype <- fixef(mlm_er_opt)['weight_typeoptimal']

  er_sd_re_intercept <- as_tibble(VarCorr(mlm_er_opt))|> filter(var1 == '(Intercept)') |> pull(sdcor)


  # return bootstrap estimates
  return(c(
    rt_fe_intercept, rt_fe_weighttype, rt_sd_re_intercept,
    er_fe_intercept, er_fe_weighttype, er_sd_re_intercept
  ))
}

# saving participant ids, so that bootstrapping can be on participant level
participant_ids <- unique(all_data$ID)


set.seed(123)

boot_out <- boot(
  data = participant_ids,
  statistic = boot_fun,
  R = 5000,
  parallel = 'multicore',
  ncpus = 6
)

saveRDS(boot_out, 'testing_stuff/boot_objects/boot_hyp3_5000.rds')
boot_out <- readRDS("testing_stuff/boot_objects/boot_hyp3_5000.rds")


boot_estimates <- as_tibble(boot_out$t)
names(boot_estimates) <- c('rt_fe_intercept', 'rt_fe_weighttype', 'rt_re_se_intercept', 'er_fe_intercept', 'er_fe_weighttype', 'er_re_se_intercept')


# ---- extracting bca confidence intervals ----

# tibble to save all values
ci_data <- tibble(
  index = 1:6,
  parameter = c('rt_fe_intercept', 'rt_fe_weighttype', 'rt_re_se_intercept', 'er_fe_intercept', 'er_fe_weighttype', 'er_re_se_intercept'),
  t0_estimate = NA_real_,
  upper = NA_real_,
  lower = NA_real_
)

for (i in ci_data$index){
  # calculate ci
  ci_temp <- boot.ci(boot_out, type = "perc", index = i)

  # extract parameters
  ci_data[i, 't0_estimate'] <- unname(ci_temp$t0)
  ci_data[i, 'upper'] <-  unname(ci_temp$percent[1,5])
  ci_data[i, 'lower'] <- unname(ci_temp$percent[1,4])
}



# ---- creating plot ----

whole_plot_data <- boot_estimates |>
  pivot_longer(
    cols = everything(),
    names_to = "parameter",
    values_to = "value"
  ) 
whole_plot <- ggplot(whole_plot_data, aes(x = value)) +
  geom_histogram(
    bins = 30
  ) +
  geom_vline(
    data = ci_data,
    aes(xintercept = lower),
    linetype = "dashed"
  ) +
  geom_vline(
    data = ci_data,
    aes(xintercept = upper),
    linetype = "dashed"
  ) +
  facet_wrap(
    ~ parameter,
    scales = "free"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()

  )


ggsave(plot = whole_plot, filename = 'testing_stuff/boot_objects/whole_plot_hyp3.png', height = 8, width = 10)

system("open testing_stuff/boot_objects/whole_plot_hyp3.png")


