library(tidyverse)
library(lubridate)
library(here)
library(arrow)
library(gtsummary)
library(haven)
library(survival)
library(survminer)
library(cowplot)

## run some functions needed later  
source(here("analysis/R_fn/summarise_data.R"))
source(here("analysis/R_fn/create_rounded_surv_table.R"))
source(here("analysis/R_fn/redaction.R"))
source(here("analysis/R_fn/ggplot_theme.R"))
source(here("analysis/master_mapping.R"))

## create directories for output
fs::dir_create(here("output/data_properties"))
fs::dir_create(here("output/plots"))

# import the day of any response by each participant ----------------------
## make the data long format and keep value == 1 only (i.e. a valid response was recorded to eq5d question)
op_anyresponse <- read_csv(here("output/openprompt_all.csv")) %>% 
  pivot_longer(-patient_id, names_pattern = "day_(.*)", names_to = "day") %>% 
  filter(value == 1) %>% 
  mutate(day = as.numeric(day))

# import selected survey response data ------------------------------------
op_baseline <- read_csv(here("output/openprompt_survey1.csv"),
                        col_types = list(
                          patient_id = "d",
                          creation_date = "D",
                          base_ethnicity = "c",
                          base_highest_edu = "c",
                          base_disability = "c",
                          base_relationship = "c",
                          base_gender = "c",
                          base_hh_income = "c",
                          base_ethnicity_creation_date = "D",
                          base_highest_edu_creation_date = "D",
                          base_disability_creation_date = "D",
                          base_relationship_creation_date = "D",
                          base_gender_creation_date = "D",
                          base_hh_income_creation_date = "D"
                        )) %>% 
  dplyr::select(patient_id, creation_date, starts_with("base_"))

# get index_date as the first recorded of any "base_" variable
op_baseline$index_date <- pmin(
  op_baseline$creation_date,
  op_baseline$base_ethnicity_creation_date,
  op_baseline$base_highest_edu_creation_date,
  op_baseline$base_disability_creation_date,
  op_baseline$base_relationship_creation_date,
  op_baseline$base_gender_creation_date,
  op_baseline$base_hh_income_creation_date,
  na.rm = T
)

# Survey column specification 
research_col_spec <- list(
  patient_id = "d",
  creation_date = "D",
  days_since_baseline = "d",
  covid_history = "c",
  first_covid = "D",
  n_covids = "d",
  recovered_from_covid = "c",
  covid_duration = "d",
  vaccinated = "c",
  n_vaccines = "d",
  first_vaccine_date = "D",
  most_recent_vaccine_date = "D",
  eq5d_mobility = "d",
  eq5d_selfcare = "d",
  eq5d_usualactivities = "d",
  eq5d_pain_discomfort = "d",
  eq5d_anxiety_depression = "d",
  EuroQol_score = "d",
  employment_status = "c",
  work_affected = "d",
  life_affected = "d",
  facit_fatigue = "d",
  facit_weak = "d",
  facit_listless = "d",
  facit_tired = "d",
  facit_trouble_starting = "d",
  facit_trouble_finishing = "d",
  facit_energy = "d",
  facit_usual_activities = "d",
  facit_sleep_during_day = "d",
  facit_eat = "d",
  facit_need_help = "d",
  facit_frustrated = "d",
  facit_limit_social_activity = "d",
  mrc_breathlessness = "c",
  # repeat for _creation_date
  covid_history_creation_date = "D",
  first_covid_creation_date = "D",
  n_covids_creation_date = "D",
  recovered_from_covid_creation_date = "D",
  covid_duration_creation_date = "D",
  vaccinated_creation_date = "D",
  n_vaccines_creation_date = "D",
  first_vaccine_date_creation_date = "D",
  most_recent_vaccine_date_creation_date = "D",
  eq5d_mobility_creation_date = "D",
  eq5d_selfcare_creation_date = "D",
  eq5d_usualactivities_creation_date = "D",
  eq5d_pain_discomfort_creation_date = "D",
  eq5d_anxiety_depression_creation_date = "D",
  EuroQol_score_creation_date = "D",
  employment_status_creation_date = "D",
  work_affected_creation_date = "D",
  life_affected_creation_date = "D",
  facit_fatigue_creation_date = "D",
  facit_weak_creation_date = "D",
  facit_listless_creation_date = "D",
  facit_tired_creation_date = "D",
  facit_trouble_starting_creation_date = "D",
  facit_trouble_finishing_creation_date = "D",
  facit_energy_creation_date = "D",
  facit_usual_activities_creation_date = "D",
  facit_sleep_during_day_creation_date = "D",
  facit_eat_creation_date = "D",
  facit_need_help_creation_date = "D",
  facit_frustrated_creation_date = "D",
  facit_limit_social_activity_creation_date = "D",
  mrc_breathlessness_creation_date = "D"
)

op_survey1 <- read_csv(here("output/openprompt_survey1.csv"), 
                       col_types = research_col_spec) %>% 
  mutate(survey_response = 1) %>% 
  dplyr::select(!starts_with("base_"))

op_survey2 <- read_csv(here("output/openprompt_survey2.csv"), col_types = research_col_spec) %>%
  mutate(survey_response = 2)

op_survey3 <- read_csv(here("output/openprompt_survey3.csv"), col_types = research_col_spec) %>% 
  mutate(survey_response = 3)

op_survey4 <- read_csv(here("output/openprompt_survey4.csv"), col_types = research_col_spec) %>% 
  mutate(survey_response = 4)

# output raw data summaries -----------------------------------------------
summarise_data(data_in = op_baseline, filename = "op_baseline")
summarise_data(data_in = op_survey1, filename = "op_survey1")
summarise_data(data_in = op_survey2, filename = "op_survey2")
summarise_data(data_in = op_survey3, filename = "op_survey3")
summarise_data(data_in = op_survey4, filename = "op_survey4")

# stack research questionnaire responses ----------------------------------
op_surveys <- bind_rows(
  op_survey1, 
  op_survey2,
  op_survey3,
  op_survey4
)

# left join baseline vars -------------------------------------------------
op_raw <- op_baseline %>% 
  left_join(op_surveys, by = "patient_id") %>%
  arrange(patient_id) %>% 
  rename("baseline_creation_date"="creation_date.x") %>% 
  rename("survey_date"="creation_date.y") 

# Output a summary of the raw data ----------------------------------------
summarise_data(data_in = op_raw, filename = "op_raw")

sample_ids <- op_raw %>% 
  filter(!is.na(survey_date)) %>% 
  filter(survey_response >= 2) %>% 
  dplyr::select(patient_id) %>% 
  pull() %>% 
  sample(size = 20, replace = TRUE)

pdf(here::here("output/data_properties/sample_day_lags.pdf"), width = 6, height = 4)
op_raw %>% 
  filter(patient_id %in% sample_ids) %>% 
  ggplot(aes(y = days_since_baseline, x = survey_response, group = patient_id)) +
  geom_line() + 
  geom_point(pch = 1)
dev.off()

# map ctv3codes to the description ----------------------------------------
op_mapped <- op_raw %>% 
  dplyr::select(patient_id, survey_response, where(is_character)) %>% 
  pivot_longer(cols = c(-patient_id, -survey_response), names_to = "varname", values_to = "ctv3_code") %>% 
  left_join(openprompt_mapping, by = c("ctv3_code" = "codes")) %>% 
  pivot_wider(id_cols = c(patient_id, survey_response), names_from = varname, values_from = description)

op_numeric <- op_raw %>% 
  dplyr::select(patient_id, survey_response, where(is.numeric)) 

op_dates <- op_raw %>% 
  dplyr::select(patient_id, survey_response, where(is.Date)) 

op_neat <- op_mapped %>% 
  left_join(op_numeric, by = c("patient_id", "survey_response")) %>% 
  left_join(op_dates, by = c("patient_id", "survey_response"))

# Mapping values for scored assessments -----------------------------------
# some questions are stored as numeric but they correspond to a scored assessment
# between [1, 5] or [0, 10] usually (with some exceptions).
# Need to map these as they are actually categorical vars, not numeric

# there is probably a better way of doing this but this is a manual version that works

# - number of times you have had COVID - Y3a98
op_neat$n_covids <- factor(op_neat$n_covids, levels = 0:6, 
                           labels = c("0",
                                      "1",
                                      "2",
                                      "3",
                                      "4",
                                      "5",
                                      "6+"))
# - length of symptoms - Y3a7f
op_neat$covid_duration <- factor(op_neat$covid_duration, levels = 0:3,
                                 labels = c("Less than 2 weeks",
                                            "2 – 3 weeks",
                                            "4 – 12 weeks",
                                            "More than 12 weeks"))
# - N covid injections - Y3a9e
op_neat$n_vaccines <- factor(op_neat$n_vaccines, levels = 0:6,
                             labels = c("0",
                                        "1",
                                        "2",
                                        "3",
                                        "4",
                                        "5",
                                        "6+"))
# - EQ-5d questions: scored from 1:5 in increasing levels of disability
eq5d_questions <- op_neat %>% dplyr::select(starts_with("eq5d_")) %>% dplyr::select(!contains("creation_date")) %>% names()
op_neat <- op_neat %>% 
  mutate_at(eq5d_questions, ~factor(., levels = 1:5, 
                                    labels = c("none",
                                               "slight", 
                                               "moderate",
                                               "severe",
                                               "unable")))

# - work and productivity questions: Y3a80 & Y3a81
labels_work_and_productivity <- c(
  "0 (No effect on my daily activities)",
  "1",
  "2",
  "3",
  "4",
  "5",
  "6",
  "7",
  "8",
  "9",
  "10 (Completely prevented me from doing my daily activities)"
)
op_neat$work_affected <- factor(op_neat$work_affected, levels = 0:10, labels = labels_work_and_productivity)
op_neat$life_affected <- factor(op_neat$life_affected, levels = 0:10, labels = labels_work_and_productivity)

# - FACIT: 13 questions on fatigue scale

# Note: scores are reversed so that a high score is equivalent to not having a problem
# e.g., "Not at all" for "Do you feel fatigued"
labels_facit_proper <- c(
  "Very much",
  "Quite a bit",
  "Somewhat",
  "A little bit",
  "Not at all"
)
# HOWEVER, there are two questions that need to be scored the other way around
# fun
labels_facit_reverse <- c(
  "Not at all",
  "A little bit",
  "Somewhat",
  "Quite a bit",
  "Very much"
)

facit_questions <- op_neat %>% dplyr::select(starts_with("facit")) %>% dplyr::select(!contains("creation_date")) %>% names()
facit_questions_reverse <- c("facit_energy", "facit_usual_activities")
facit_questions_proper <- facit_questions[!facit_questions %in% facit_questions_reverse]

op_neat <- op_neat %>% 
  mutate_at(all_of(facit_questions_proper), ~factor(., levels = 0:4, 
                                                    labels = labels_facit_proper)) %>% 
  mutate_at(all_of(facit_questions_reverse), ~factor(., levels = 0:4, 
                                                     labels = labels_facit_reverse))


# TODO: convert all strings to factor
# Ethnicity:
op_neat$base_ethnicity <- factor(op_neat$base_ethnicity, 
                                 levels = c("White",
                                            "Mixed",
                                            "Asian/Asian Brit",
                                            "Black/African/Caribbn/Black Brit",
                                            "Other/not stated",
                                            "NA"),
                                 labels = c("White",
                                            "Mixed",
                                            "Asian/Asian Brit",
                                            "Black/African/Caribbn/Black Brit",
                                            "Other/not stated",
                                            "Other/not stated"))


# Eductaion
op_neat$base_highest_edu <- factor(op_neat$base_highest_edu, 
                                   levels = c("less than primary school",
                                              "primary school completed",
                                              "secondary / high school completed",
                                              "college / university completed",
                                              "post graduate degree",
                                              "NA",
                                              "Refused"),
                                   labels = c("None/less than primary school",
                                              "Primary School",
                                              "Secondary/high school",
                                              "College/university",
                                              "Postgraduate qualification",
                                              "Not stated",
                                              "Not stated"))

# Disability
op_neat$base_disability <- factor(op_neat$base_disability, 
                                  levels = c("None",
                                             "Disability",
                                             "Refused",
                                             "NA"),
                                  labels = c("No",
                                             "Yes",
                                             "Not stated",
                                             "Not stated"))

# Relationship status
op_neat$base_relationship <- factor(op_neat$base_relationship, 
                                    levels = c("Single person",
                                               "Cohabiting",
                                               "Married/civil partner",
                                               "Separated",
                                               "Divorced/person whose civil partnership has been dissolved",
                                               "Widowed/surviving civil partner",
                                               "Marital/civil state not disclosed",
                                               "NA"
                                    ),
                                    labels = c("Single person",
                                               "Cohabiting",
                                               "Married/civil partner",
                                               "Separated",
                                               "Divorced/dissolved civil partnership",
                                               "Widowed/surviving civil partner",
                                               "Not stated",
                                               "Not stated"
                                    ))

# Gender - Male/Female only 
op_neat$base_gender <- factor(op_neat$base_gender, 
                              levels = c("Male",
                                         "Female",
                                         "Intersex/non-binary/other",
                                         "Refused",
                                         "NA"),
                              labels = c("Male",
                                         "Female",
                                         "Intersex/non-binary/other",
                                         "Refused",
                                         "Not stated"))

# Income 
op_neat$base_hh_income <- factor(op_neat$base_hh_income, 
                                 levels = c("£6,000-12,999",
                                            "£13,000-18,999",
                                            "£19,000-25,999",
                                            "£26,000-31,999",
                                            "£32,000-47,999",
                                            "£48,000-63,999",
                                            "£64,000-95,999",
                                            "£96,000",
                                            "Unknown income",
                                            "NA"
                                 ),
                                 labels = c("£6,000-12,999",
                                            "£13,000-18,999",
                                            "£19,000-25,999",
                                            "£26,000-31,999",
                                            "£32,000-47,999",
                                            "£48,000-63,999",
                                            "£64,000-95,999",
                                            "£96,000",
                                            "Not stated",
                                            "Not stated"
                                 ))

# Employment Status
op_neat$employment_status <- as_factor(op_neat$employment_status)

# Covid history 
op_neat$covid_history <- factor(op_neat$covid_history,
                                levels = c(
                                  "My test for COVID-19 was positive",
                                  "I think I have already had COVID-19 (coronavirus) disease",
                                  "Suspected COVID-19",
                                  "I am unsure if I currently have or have ever had COVID-19",
                                  "I do not think I currently have or have ever had COVID-19",
                                  "I prefer not to say if I currently have or have ever had COVID-19",
                                  "NA"
                                ),
                                labels = c(
                                  "Yes (+ve test)",
                                  "Yes (medical advice",
                                  "Yes (suspected)",
                                  "Unsure",
                                  "No",
                                  "Not stated",
                                  "Not stated"
                                ))

# Covid recovery 
op_neat$recovered_from_covid <- factor(op_neat$recovered_from_covid,
                                       levels = c(
                                         "I feel I have fully recovered from my (latest) episode of COVID-19",
                                         "I feel I have not fully recovered from my (latest) episode of COVID-19",
                                         "NA"
                                       ),
                                       labels = c(
                                         "Yes, back to normal",
                                         "No, still have symptoms",
                                         "Not stated"
                                       ))

# Vaccinated
op_neat$vaccinated <- factor(op_neat$vaccinated, 
                             levels = c(
                               "I have had at least one COVID-19 vaccination",
                               "I have not had any COVID-19 vaccinations",
                               "I prefer not to say if I have had any COVID-19 vaccinations",
                               "NA"
                             ),
                             labels = c(
                               "Yes",
                               "No",
                               "Not stated",
                               "Not stated"
                             ))

# MRC breathlessness
op_neat$mrc_breathlessness <- factor(op_neat$mrc_breathlessness,
                                     levels = c(
                                       "Grade 1: I only get breathless with strenuous exercise",
                                       "Grade 2: I get short of breath when hurrying on level ground or walking up a slight hill",
                                       "Grade 3: On level ground, I walk slower than people of my age because of breathlessness, or I have to stop for breath when walking at my own pace on the level",
                                       "Grade 4: I stop for breath after walking about 100 yards or after a few minutes on level ground",
                                       "Grade 5: I am too breathless to leave the house or I am breathless when dressing/undressing"
                                     ))

# Output summary of the tidied up dataset ---------------------------------
summarise_data(data_in = op_neat, filename = "op_mapped")

# output data -------------------------------------------------------------
arrow::write_parquet(op_neat, sink = here::here("output/openprompt_raw.gz.parquet"))

# plot distribution of day0 -----------------------------------------------
p1a <- ggplot(op_neat, aes(x = index_date)) +
  geom_density(fill = "gray", col = "gray50") +
  theme_ali() +
  labs(x = "First response date",
       y = "Density")
ggsave(p1a, filename = here::here("output/plots/p1a_index_dates.jpeg"),
       width=12, height = 6, units="in")
ggsave(p1a, filename = here::here("output/plots/p1a_index_dates.tiff"),
       width=12, height = 6, units="in")


# plot the distriubtion of selected responses in ALL the data -------------
op_offset <- op_neat %>% 
  # keep the id, index_date, and all the response dates
  dplyr::select(patient_id, index_date, ends_with("creation_date")) %>% 
  # make all the response dates long format and drop NA vals
  pivot_longer(cols = -c(patient_id, index_date), names_pattern = "(.*)_creation_date") %>% 
  drop_na() %>% 
  # calculate offset variable
  mutate(offset = as.numeric(value - index_date)) 

p1b <- ggplot(op_offset, aes(x = offset)) +
  geom_histogram(col = "darkblue", fill = "blue", alpha = 0.4, linewidth = 0.4, bins = 50) +
  geom_vline(xintercept = 30, lty = 2) + 
  geom_vline(xintercept = 60, lty = 2) + 
  geom_vline(xintercept = 90, lty = 2) +
  xlim(c(-5, 120)) +
  ylim(c(0, NA)) +
  theme_ali() +
  labs(x = "Days since index", 
       y = "Included responses")

ggsave(p1b, filename = here::here("output/plots/p1b_recorded_question_responses.jpeg"),
       width=12, height = 6, units="in")

# plot the distribution of ANY response to Eq5d compulsory question -------
p1c <- ggplot(op_anyresponse, aes(x = day)) +
  geom_histogram(col = "darkred", fill = "red", alpha = 0.4, linewidth = 0.4, bins = 50) + 
  geom_vline(xintercept = 30, lty = 2) + 
  geom_vline(xintercept = 60, lty = 2) + 
  geom_vline(xintercept = 90, lty = 2) +
  xlim(c(-5, 120)) +
  ylim(c(0, NA)) +
  theme_ali() +
  labs(x = "Days since index",
       y = "Any response")

ggsave(p1c, filename = here::here("output/plots/p1c_anyresponse_hist.jpeg"),
       width=12, height = 6, units="in")

# plot reverse KM for loss to follow up -----------------------------------
op_max_fup <- op_neat %>% 
  group_by(patient_id) %>% 
  filter(survey_response == max(survey_response)) %>% 
  mutate(offset = as.numeric(survey_date - index_date) + 1,
         status = 1) %>% 
  dplyr::select(patient_id, survey_response, time = offset, status)

surv_data <- create_rounded_surv_table(op_max_fup)

p1d <- surv_data %>%
  ggplot(aes(x = time, y = surv)) + geom_step(size = 0.5) +
  geom_rect(aes(xmin=lagtime, xmax = time, ymin=(surv.ll), ymax=(surv.ul)), alpha=0.1, colour="transparent") +
  ylim(0,1) +
  #scale_y_continuous(expand = expansion(mult=c(0,0.02))) +
  labs(
    x="Time since first response (days)",
    y = "Lost to follow up",
    title = "") +
  theme_ali() +
  guides(fill="none")

ggsave(p1d, filename = here::here("output/plots/p1d_ltfu.jpeg"),
       width=12, height = 6, units="in")

# combine follow up plots -------------------------------------------------
p1 <- plot_grid(p1a, p1b, p1d, p1c, nrow = 2, labels = "AUTO")

ggsave(p1, filename = here::here("output/plots/p1_fup.jpeg"),
       width=12, height = 6, units="in")
