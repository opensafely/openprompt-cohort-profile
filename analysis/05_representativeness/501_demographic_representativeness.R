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
source(here("analysis/R_fn/ggplot_theme.R"))

## set redaction threshold
threshold <- 7

# read_in_data ------------------------------------------------------------
op_neat <- arrow::read_parquet(here("output/openprompt_raw.gz.parquet"))
os_demo <- read_csv(here("output/opensafely_tpp_demographics.csv.gz"))

op_master <- op_neat %>% 
  filter(survey_response == 1) %>% 
  dplyr::select(patient_id, base_ethnicity, base_gender, base_hh_income) %>% 
  # merge on the ages of those in openPROMPT with `left_join`
  left_join(dplyr::select(os_demo, patient_id, age, imd), by = "patient_id") %>% 
  rename(op_age = age,
         op_imd = imd) %>% 
  full_join(os_demo, by = "patient_id") %>% 
  mutate(
    sex = factor(sex, 
                 levels = c("female", "intersex", "male", "unknown"),
                 labels = c("female", "intersex/non-binary/other", "male", "refused")),
    ethnicity = factor(
      ethnicity,
      levels = 1:6, 
      labels = c(
        "White",
        "Mixed", 
        "Asian/Asian Brit", 
        "Black/African/Caribbn/Black Brit",
        "Other/not stated",
        "Other/not stated"
      )),
    imd_q5 = cut(imd,
                 breaks = c(32844 * seq(0, 1, 0.2)),
                 labels = c("1 (most deprived)",
                            "2",
                            "3",
                            "4",
                            "5 (least deprived)")
    ),
    op_imd_q5 = cut(op_imd,
                 breaks = c(32844 * seq(0, 1, 0.2)),
                 labels = c("1 (most deprived)",
                            "2",
                            "3",
                            "4",
                            "5 (least deprived)")
    ),
    # create an age category variable for easy stratification
    age_cat = cut(
      age, 
      breaks = c(0, seq(25, 70, 5), Inf),
      labels = c(
        "18-24",
        "25-29",
        "30-34",
        "35-39",
        "40-44",
        "45-49",
        "50-54",
        "55-59",
        "60-64",
        "65-69",
        "70+"
      )),
    op_age_cat = cut(
      op_age, 
      breaks = c(0, seq(25, 70, 5), Inf),
      labels = c(
        "18-24",
        "25-29",
        "30-34",
        "35-39",
        "40-44",
        "45-49",
        "50-54",
        "55-59",
        "60-64",
        "65-69",
        "70+"
      )),
  ) %>% 
  dplyr::select(
    patient_id,
    OpenPROMPT_ethnicity = base_ethnicity,
    OpenPROMPT_gender = base_gender,
    OpenPROMPT_hhincome = base_hh_income,
    OpenPROMPT_age = op_age, 
    OpenPROMPT_agecat = op_age_cat,
    OpenPROMPT_imd = op_imd_q5, 
    OpenSAFELY_ethnicity = ethnicity,
    OpenSAFELY_gender = sex,
    OpenSAFELY_age = age, 
    OpenSAFELY_agecat = age_cat, 
    OpenSAFELY_imd = imd_q5
  )

op_long <- op_master %>% 
  pivot_longer(-patient_id, 
               names_to = c("Var", ".value"), 
               names_sep="_" )

var_labels <- list(
  ethnicity ~ "Ethnicity",
  gender ~ "Gender (OpenPROMPT)/Sex (OpenSAFELY)",
  hhincome ~ "Household income (OpenPROMPT only)",
  imd ~ "Index of Multiple Deprivation",
  age ~ "Age",
  agecat ~ "Age category"
)

var_labels <- var_labels %>%
  set_names(., map_chr(., all.vars))

tab1 <- op_long %>%
  dplyr::select(-patient_id) %>% 
  mutate_at(c("ethnicity", "gender", "imd"), ~stringr::str_to_lower(.)) %>% 
  #mutate_if(is.character, ~forcats::fct_explicit_na(forcats::as_factor(.))) %>% 
  tbl_summary(
    by = Var,
    statistic = list(
      all_continuous() ~ "{mean} ({p25}-{p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    type = list(
      ethnicity ~ "categorical",
      gender ~ "categorical",
      hhincome ~ "categorical",
      imd ~ "categorical",
      age ~ "continuous",
      agecat ~ "categorical"
    ),
    digits = all_continuous() ~ 1, 
  )

tab1 %>%
  as_gt() %>%
  gt::gtsave(
    filename = "table3_represetativeness.html",
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
    trueN=N,
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
                N = trueN,
                stat,
                data_source = by)# %>% 
#pivot_wider(names_from = by, values_from = stat, names_prefix = "survey_")


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
                mean, p25, p75,
                n = N_nonmiss,
                N = N_nonmiss,
                p = p_nonmiss,
                stat,
                data_source = by) #%>% 
#pivot_wider(names_from = by, values_from = stat, names_prefix = "survey_")

raw_stats_output <- raw_stats_redacted_catgorical %>% 
  bind_rows(raw_stats_redacted_numeric)

write_csv(raw_stats_output, here::here("output/tables/table3_rep_stats.csv"))
