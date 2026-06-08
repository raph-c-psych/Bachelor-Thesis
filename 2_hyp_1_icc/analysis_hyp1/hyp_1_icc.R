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



# ---- exploring quadratic model -----
task_modality <- corr_list |> filter(task == 'cuedts' & session == 'reactive')

ggplot(
  data = task_modality,
  mapping = aes(x = rt_weight, y = z_r)
) +
  geom_point()

# fit quadtatic model
model <- lm(
  z_r ~ rt_weight + I(rt_weight^2),
  data = task_modality
)
summary(model)


# prediction dataframe
pred_dat <- tibble(
  rt_weight = seq(0, 1, length.out = 200)
) |>
  mutate(
    pred = predict(model, newdata = pick(everything()))
  )

# plot
ggplot(task_modality, aes(rt_weight, z_r)) +
  geom_point(size = 2) +
  geom_line(
    data = pred_dat,
    aes(y = pred),
    linewidth = 1
  ) +
  labs(
    x = "RT Weighting",
    y = "Correlation"
  ) +
  theme_minimal()





# ---- multilevel model ----
mlm <- lmer(
  z_r ~
    rt_weight +
    I(rt_weight^2) +
    (1 + rt_weight | group),
  data = corr_list
)
summary(mlm)

fe_intercept <- fixef(mlm)['(Intercept)']
fe_weight <- fixef(mlm)['rt_weight']
fe_weight2 <- fixef(mlm)['I(rt_weight^2)']

sd_re_intercept <- as_tibble(VarCorr(mlm))|> filter(var1 == '(Intercept)' & is.na(var2)) |> pull(sdcor)
sd_re_weight <- as_tibble(VarCorr(mlm))|> filter(var1 == 'rt_weight' & is.na(var2)) |> pull(sdcor)
cor_re <- as_tibble(VarCorr(mlm))|> filter(var1 == '(Intercept)' & var2 == 'rt_weight') |> pull(sdcor)

extract_names <- ranef(mlm)$group |> row.names()

unname(fixef(mlm))

# plotting random effects
plot_model(
  mlm,
  type = "re"
)

# now I should bootstrap on the participant level to get a measure of uncertainty of the whole pipeline


ggplot(corr_list,
       aes(rt_weight, z_r, group = group)) +
  geom_line(alpha = .5) +
  geom_smooth(
    aes(group = 1),
    method = "lm",
    formula = y ~ x + I(x^2),
    linewidth = 1.5
  )
# well not good, but kind of expected





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

  # calculate correlations for all weights
  weights <- seq(0, 1, by = 0.05)

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

  # centering weight
  corr_list <- corr_list |>
    mutate(rt_weight = rt_weight - 0.5)
  

  # fit MLM
  mlm <- lmer(
    z_r ~
      rt_weight +
      I(rt_weight^2) +
      (1 + rt_weight | group),
    data = corr_list
  )

  # extract values of multilevel model
  fe_intercept <- fixef(mlm)['(Intercept)']
  fe_weight <- fixef(mlm)['rt_weight']
  fe_weight2 <- fixef(mlm)['I(rt_weight^2)']

  sd_re_intercept <- as_tibble(VarCorr(mlm))|> filter(var1 == '(Intercept)' & is.na(var2)) |> pull(sdcor)
  sd_re_weight <- as_tibble(VarCorr(mlm))|> filter(var1 == 'rt_weight' & is.na(var2)) |> pull(sdcor)
  cor_re <- as_tibble(VarCorr(mlm))|> filter(var1 == '(Intercept)' & var2 == 'rt_weight') |> pull(sdcor)

  return(c(
    fe_intercept, fe_weight, fe_weight2,
    sd_re_intercept, sd_re_weight, cor_re
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
  ncpus = 7
)

saveRDS(boot_out, '2_hyp_1_icc/analysis_hyp1/boot_out_hyp1_5000.rds')
boot_out <- readRDS("2_hyp_1_icc/analysis_hyp1/boot_out_hyp1_5000.rds")

boot_estimates <- as_tibble(boot_out$t)
names(boot_estimates) <- c(
    "fe_intercept", "fe_weight", "fe_weight2",
    "sd_re_intercept", "sd_re_weight", "cor_re"
  )


# tibble to save all values
ci_data <- tibble(
  index = 1:6,
  parameter = c("fe_intercept", "fe_weight", "fe_weight2", "sd_re_intercept", "sd_re_weight", "cor_re"),
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

# manual one sided cis
boot.ci(boot_out, type = "perc", index = 2, conf = 0.9) # weight linear
boot.ci(boot_out, type = "perc", index = 3, conf = 0.9) # weight quadratic

# inverse fisher z intercept
ci_data[1,3:5]


# assign names extracted from a singular model above
whole_plot_data <- boot_estimates |>
  pivot_longer(
    cols = everything(),
    names_to = "parameter",
    values_to = "value"
  ) |>
  mutate(
    parameter = case_when(
      parameter == 'fe_intercept' ~ 'Fixed Intercept',
      parameter == 'fe_weight' ~ 'Fixed Effect Weight',
      parameter == 'fe_weight2' ~ 'Fixed Effect Weight^2',
      parameter == 'sd_re_intercept' ~ 'Random Intercept (SD)',
      parameter == 'sd_re_weight' ~ 'Random Slope Weight (SD)',
      parameter == 'cor_re' ~ 'Correlation Random Effects'
    ),
    parameter = factor(
      parameter,
      levels = c(
        "Fixed Intercept",
        "Fixed Effect Weight",
        "Fixed Effect Weight^2",
        "Random Intercept (SD)",
        "Random Slope Weight (SD)",
        "Correlation Random Effects"
      )
    )
  )
ci_data_plot <- ci_data |>
  mutate(
    parameter = case_when(
      parameter == 'fe_intercept' ~ 'Fixed Intercept',
      parameter == 'fe_weight' ~ 'Fixed Effect Weight',
      parameter == 'fe_weight2' ~ 'Fixed Effect Weight^2',
      parameter == 'sd_re_intercept' ~ 'Random Intercept (SD)',
      parameter == 'sd_re_weight' ~ 'Random Slope Weight (SD)',
      parameter == 'cor_re' ~ 'Correlation Random Effects'
    ),
    parameter = factor(
      parameter,
      levels = c(
        "Fixed Intercept",
        "Fixed Effect Weight",
        "Fixed Effect Weight^2",
        "Random Intercept (SD)",
        "Random Slope Weight (SD)",
        "Correlation Random Effects"
      )
    )
  )

whole_plot <- ggplot(whole_plot_data, aes(x = value)) +
  geom_histogram(
    bins = 30
  ) +
  geom_vline(
    data = ci_data_plot,
    aes(xintercept = lower),
    linetype = "dashed"
  ) +
  geom_vline(
    data = ci_data_plot,
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
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_line(
      color = "black",
      linewidth = 0.6
    ),
    axis.ticks.length = unit(0.2, "cm"),
    axis.line.x = element_line(
      color = "black",
      linewidth = 0.6
    )
  )


ggsave(plot = whole_plot, filename = '2_hyp_1_icc/analysis_hyp1/hyp_1_distrib.png', height = 8, width = 10)

system("open 2_hyp_1_icc/analysis_hyp1/hyp_1_distrib.png")
