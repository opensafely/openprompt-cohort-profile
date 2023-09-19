library(tidyverse)
library(lubridate)
library(here)
source(here("analysis/R_fn/summarise_data.R"))

op_tpp <- read_csv(here("output/openprompt_linked_tpp.csv.gz"))

op_tpp_edit <- op_tpp %>% 
  mutate(imd_q5 = cut(
    imd,
    breaks = c(32844 * seq(0, 1, 0.2)),
    labels = c("1 (most deprived)",
               "2",
               "3",
               "4",
               "5 (least deprived)")
  ),
  age_cat = cut(
    age, 
    breaks = c(0, seq(30, 70, 10), Inf),
    labels = c(
      "18-29",
      "30-39",
      "40-49",
      "50-59",
      "60-69",
      "70+"
    )
    ),
  comorbidities = cut(
    comorbid_count, 
    breaks = c(-Inf, 0, 1, 2, Inf),
    labels = c("0", "1", "2+", "2+")
    ),
  hh_size = cut(
    hh_size, 
    breaks = c(-Inf, 0:5, Inf),
    labels = c(as.character(0:4), "5+", "5+")
    ),
  all_test_positive = cut(
    all_test_positive, 
    breaks = c(-Inf, 0:5, Inf),
    labels = c(as.character(0:4), "5+", "5+")
    ),
  n_lc_records = cut(
    n_lc_records, 
    breaks = c(-Inf, 0:1, Inf),
    labels = c("None", "At least 1", "At least 1")
    ),
  no_prev_vacc = cut(
    no_prev_vacc, 
    breaks = c(-Inf, 0:4, Inf),
    labels = c(as.character(0:3), "4+", "4+")
    ),
  rural_urban_detail = factor(
    ifelse(ruc<1 | ruc>8, NA, ruc),
    levels = 1:8,
    labels = c(
      "Urban major conurbation",
      "Urban minor conurbation",
      "Urban city and town",
      "Urban city and town in a sparse setting",
      "Rural town and fringe",
      "Rural town and fringe in a sparse setting",
      "Rural village and dispersed",
      "Rural village and dispersed in a sparse setting"
    )
    ),
  rural_urban = rural_urban_detail
  )

levels(op_tpp_edit$rural_urban) <- c(rep("Urban", 4), rep("Rural", 4))

summarise_data(data_in = op_tpp_edit, filename = "op_tpp")

arrow::write_parquet(op_tpp_edit, here::here("output/openprompt_linked_tpp_edited.gz.parquet"))
