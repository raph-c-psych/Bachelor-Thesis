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





# ---- function: calculating correlations between sessions through pearson ----
calc_corrs <- function(data_temp, rt_weighting){
  # calculate BIS with choseen weighting
  data_temp <- data_temp |>
    group_by(session, task) |>
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





# ---- testing function to create correlations for different weights ----
# calculating correlation matrixes for different weights
weights <- seq(0, 1, by = 0.05)
corr_long <- tibble(
  rt_weight = numeric(),
  correlation = numeric(),
  group = character()
)
for (curr_weight in weights) {
  # take only test data
  mod_data <- all_data |>
    filter(phase == "test")
  # create correlation matrix for current weight between all sessions
  corr_matrix <- calc_corrs(mod_data, curr_weight)
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
    mutate(rt_weight = as.numeric(curr_weight)) |>
    mutate(
      group = paste0(str_sub(var1, 4), '-', str_sub(var2, 4)) # create general variable for corr
    ) |>
    select(-c(var1, var2))
  corr_long <- rbind(corr_long, corr_data)
}
rm(corr_matrix, mod_data, curr_weight, weights, corr_data)



# creating fisher z-transformations and grouping for singular sessions
corr_long <- corr_long |>
  mutate(
    z_r = fisherz(correlation),
  )





# ---- exploring quadratic model -----
single_corr_test <- corr_long |> filter(group == corr_long$group[1])

ggplot(
  data = single_corr_test,
  mapping = aes(x = rt_weight, y = z_r)
) +
  geom_point()

# fit quadtatic model
model <- lm(
  z_r ~ rt_weight + I(rt_weight^2),
  data = single_corr_test
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
ggplot(single_corr_test, aes(rt_weight, z_r)) +
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
  data = corr_long
)
summary(mlm)

unname(fixef(mlm))

# plotting random effects
plot_model(
  mlm,
  type = "re"
)

# now I should bootstrap on the participant level to get a measure of uncertainty of the whole pipeline


ggplot(corr_long,
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

  corr_long <- tibble()

  for (curr_weight in weights) {
    # take only test data
    mod_data <- boot_data |>
      filter(phase == "test")
    # create correlation matrix for current weight between all sessions
    corr_matrix <- calc_corrs(mod_data, curr_weight)
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
      mutate(rt_weight = as.numeric(curr_weight)) |>
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
    data = corr_long
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

saveRDS(boot_out, 'testing_stuff/boot_objects/boot_hyp2_5000.rds')
boot_out <- readRDS("testing_stuff/boot_objects/boot_hyp2_5000.rds")

boot_estimates <- as_tibble(boot_out$t)
names(boot_estimates)

# bca vs. perc (bca if more values maybe)
boot.ci(boot_out, type = "perc", index = 1, conf = 0.9) # linear effect , conf = 0.9
boot.ci(boot_out, type = "perc", index = 2, conf = 0.9) # quadratic effect
boot.ci(boot_out, type = "bca", index = 3) # peak weight

# inspect estimates
peak_vals <- tibble(
  peak = boot_out$t[,2]
)
ggplot(data = peak_vals, aes(peak)) +
  geom_density() 


whole_plot_data <- boot_estimates |>
  pivot_longer(
    cols = everything(),
    names_to = "parameter",
    values_to = "value"
  ) 
ci_data <- whole_plot_data |>
      group_by(parameter) |>
      summarise(
        lower = quantile(value, .025, na.rm = TRUE),
        upper = quantile(value, .975, na.rm = TRUE)
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


ggsave(plot = whole_plot, filename = 'testing_stuff/boot_objects/whole_plot_hyp2.png', height = 8, width = 10)

system("open testing_stuff/boot_objects/whole_plot_hyp2.png")
