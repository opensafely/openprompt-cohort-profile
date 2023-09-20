library(tidyverse)
library(gt)
library(gtsummary)
library(arrow)
library(here)

## create folder for outputs to go to
output_dir_tab <- here("output/tables")
fs::dir_create(output_dir_tab)

## source useful files
source(here("analysis/R_fn/redaction.R"))

## set redaction threshold
threshold <- 7

# read_in_data ------------------------------------------------------------
op_neat <- arrow::read_parquet(here("output/openprompt_raw.gz.parquet"))
op_tpp <- arrow::read_parquet(here("output/openprompt_linked_tpp_edited.gz.parquet"))

op_master <- op_neat %>% 
  left_join(op_tpp, by = "patient_id")

op_table_stats <- op_master %>% 
  filter(survey_response == 1) %>% 
  mutate(
    tpp_data = !is.na(age)
    ) %>% 
  dplyr::select(
    base_gender,
    base_ethnicity,
    base_highest_edu,
    base_disability,
    base_relationship,
    base_hh_income,
    tpp_data,
    age,
    age_cat,
    practice_nuts,
    imd_q5,
    rural_urban,
    hh_size,
    comorbidities,
    n_lc_records,
    all_test_positive,
    no_prev_vacc
    ) %>% 
  # create explicit missing category
  mutate_if(is.factor, ~forcats::fct_explicit_na(.))

var_labels <- list(
  base_gender ~ "Gender (app question)",
  base_ethnicity ~ "Ethnicity (app question)",
  base_highest_edu ~ "Highest education (app question)",
  base_disability ~ "Disability (app question)",
  base_relationship ~ "Relationship status (app question)",
  base_hh_income ~ "Household income (app question)",
  tpp_data ~ "Linked records available",
  age ~ "Age",
  age_cat ~ "Age category",
  practice_nuts ~ "Region",
  imd_q5 ~ "IMD (quintile)",
  rural_urban ~ "Rural/urban classification",
  hh_size ~ "Household compostion (2020)",
  comorbidities ~ "Comorbidities",
  n_lc_records ~ "Number of Long COVID records",
  all_test_positive  ~ "Number of positive COVID-19 test",
  no_prev_vacc ~ "Number of previous COVID-19 vaccinations"
)

var_labels <- var_labels %>%
  set_names(., map_chr(., all.vars))

tab1 <- op_table_stats %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({p25}-{p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    type = list(
      base_gender ~ "categorical",
      base_ethnicity ~ "categorical",
      base_highest_edu ~ "categorical",
      base_disability ~ "categorical",
      base_relationship ~ "categorical",
      base_hh_income ~ "categorical",
      tpp_data ~ "categorical",
      age ~ "continuous",
      age_cat ~ "categorical",
      practice_nuts ~ "categorical",
      imd_q5 ~ "categorical",
      rural_urban  ~ "categorical",
      hh_size ~ "categorical",
      comorbidities ~ "categorical",
      n_lc_records ~ "categorical",
      all_test_positive ~ "categorical",
      no_prev_vacc ~ "categorical"
    ),
    digits = all_continuous() ~ 1
  )

## export as an html for our own review
tab1 %>%
  as_gt() %>%
  gt::gtsave(
    filename = paste0("table1.html"),
    path = fs::path(output_dir_tab)
  )

# redact and export raw statistics ----------------------------------------
raw_stats <- tab1$meta_data %>%
  select(var_label, df_stats) %>%
  unnest(df_stats)

raw_stats_redacted_catgorical <- raw_stats %>%
  filter(!is.na(n)) %>% 
  mutate(
    n=redact_and_round(n, threshold),
    N=redact_and_round(N, threshold),
    p=round(100*n/N,1),
    N_miss = redact_and_round(N_miss, threshold),
    N_obs = redact_and_round(N_obs, threshold),
    p_miss = round(100*N_miss/N_obs,1),
    N_nonmiss = redact_and_round(N_nonmiss, threshold),
    p_nonmiss = round(100*N_nonmiss/N_obs,1),
    var_label = factor(var_label, levels = names(var_labels), labels = map_chr(var_labels, ~last(as.character(.)))),
    variable_levels = replace_na(as.character(variable_levels), "")
  ) %>% 
  mutate(prettyN = as.character(prettyNum(formatC(n, digits = 1, format = "f"), 
                                          big.mark = ",", preserve.width = "none",
                                          drop0trailing = TRUE)),
         stat = paste0(prettyN, " (",p,"%)")) %>% 
  dplyr::select(variable = var_label, 
                level = variable_levels, 
                n, p,
                stat)

raw_stats_redacted_numeric <- raw_stats %>% 
  filter(is.na(n)) %>% 
  mutate_at(c("mean", "p25", "p75"), ~prettyNum(formatC(., digits = 1, format = "f"), 
                                   big.mark = ",", preserve.width = "none",
                                   drop0trailing = TRUE)) %>% 
  mutate(
    stat = paste0(mean, " (", p25, "-", p75, ")"),
    var_label = factor(
      var_label,
      levels = names(var_labels),
      labels = map_chr(var_labels, ~ last(as.character(.)))
    )
  ) %>% 
  dplyr::select(variable = var_label, 
                  level = variable_levels,
                  n = N_nonmiss,
                  p = p_nonmiss,
                  stat) 
  
raw_stats_output <- raw_stats_redacted_catgorical %>% 
  bind_rows(raw_stats_redacted_numeric)

write_csv(raw_stats_output, paste0(here::here("output/tables/table1_stats.csv")))
