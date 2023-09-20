library(tidyverse)
library(janitor)
library(forcats)
library(gtsummary)
library(here)
library(cowplot)

## create folder for outputs to go to
output_dir_tab <- c(here("output/plots"), here("output/tables/"))
for(ff in output_dir_tab){
  fs::dir_create(output_dir_tab)
}

## source useful files
source(here("analysis/R_fn/redaction.R"))
source(here("analysis/R_fn/ggplot_theme.R"))

## set redaction threshold
threshold <- 7

# load data ---------------------------------------------------------------
op_neat <- arrow::read_parquet(here("output/openprompt_raw.gz.parquet"))

# create variable labels  -------------------------------------------------
var_labels <- list(
      days_since_baseline ~ "Days since baseline",
      EuroQol_score ~ "Quality of Life Score [0-100]",
      n_covids ~ "Number of previous COVID-19 infections",
      covid_duration ~ "Duration of longest COVID-19 infection",
      n_vaccines ~ "Number of previous vaccine doses",
      eq5d_mobility ~ "EQ5D: mobility",
      eq5d_selfcare ~ "EQ5D: care for self",
      eq5d_usualactivities ~ "EQ5D: usual activities",
      eq5d_pain_discomfort ~ "EQ5D: pain/discomfort",
      eq5d_anxiety_depression ~ "EQ5D: anxiety/depression",
      work_affected ~ "How much has work been afffected",
      life_affected ~ "How much has life been affected",
      facit_fatigue ~ "FACIT: feel fatigued",
      facit_weak  ~ "FACIT: feel weak", 
      facit_listless ~ "FACIT: feel listless",
      facit_tired ~ "FACIT: feel tired",
      facit_trouble_starting ~ "FACIT: trouble starting things",
      facit_trouble_finishing ~ "FACIT: trouble finishing things",
      facit_energy ~ "FACIT: have energy",
      facit_usual_activities ~ "FACIT: able to do usual activities",
      facit_sleep_during_day ~ "FACIT: need sleep during the day",
      facit_eat ~ "FACIT: too tired to eat",
      facit_need_help ~ "FACIT: need help for usual activities",
      facit_frustrated  ~ "FACIT: frustrated by being too tired",
      facit_limit_social_activity ~ "FACIT: limit social activities",
      covid_history ~ "COVID-19 status",
      recovered_from_covid ~ "Recovered from most recent COVID-19 infection?",
      vaccinated ~ "Have you had at least one COVID-19 vaccine dose?",
      employment_status ~ "Currently employed",
      mrc_breathlessness ~ "MRC breathlessness scale"
)

var_labels <- var_labels %>%
  set_names(., map_chr(., all.vars))

# make table 2 `tab_fup` --------------------------------------------------
op_neat <- op_neat %>% 
  mutate_if(is.factor, ~forcats::fct_explicit_na(.))

tab_fup <- op_neat %>%
  select(-where(is.Date), -patient_id) %>% 
  select(-starts_with("base_")) %>% 
  tbl_summary(
    by = survey_response,
    statistic = list(
      all_continuous() ~ "{mean} ({p25}-{p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    type = list(
      days_since_baseline ~ "continuous",
      EuroQol_score ~ "continuous",
      n_covids ~ "categorical",
      covid_duration ~ "categorical",
      n_vaccines ~ "categorical",
      eq5d_mobility ~ "categorical",
      eq5d_selfcare ~ "categorical",
      eq5d_usualactivities ~ "categorical",
      eq5d_pain_discomfort ~ "categorical",
      eq5d_anxiety_depression ~ "categorical",
      work_affected ~ "categorical",
      life_affected ~ "categorical",
      facit_fatigue ~ "categorical",
      facit_weak  ~ "categorical",
      facit_listless ~ "categorical",
      facit_tired ~ "categorical",
      facit_trouble_starting ~ "categorical",
      facit_trouble_finishing ~ "categorical",
      facit_energy ~ "categorical",
      facit_usual_activities ~ "categorical",
      facit_sleep_during_day ~ "categorical",
      facit_eat ~ "categorical",
      facit_need_help ~ "categorical",
      facit_frustrated  ~ "categorical",
      facit_limit_social_activity ~ "categorical",
      covid_history ~ "categorical",
      recovered_from_covid ~ "categorical",
      vaccinated ~ "categorical",
      employment_status ~ "categorical",
      mrc_breathlessness ~ "categorical"
    ),
    digits = all_continuous() ~ 1
  )

tab_fup %>%
  as_gt() %>%
  gt::gtsave(
    filename = "table2_followup.html",
    path = fs::path(output_dir_tab[2])
  )

raw_stats <- tab_fup$meta_data %>%
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
                survey_response = by)# %>% 
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
                N = N_obs,
                p = p_nonmiss,
                stat,
                survey_response = by) #%>% 
  #pivot_wider(names_from = by, values_from = stat, names_prefix = "survey_")

raw_stats_output <- raw_stats_redacted_catgorical %>% 
  bind_rows(raw_stats_redacted_numeric)

write_csv(raw_stats_output, here::here("output/tables/table2_fup_stats.csv"))

