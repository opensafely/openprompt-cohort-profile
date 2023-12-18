library(tidyverse)
library(janitor)
library(forcats)
library(here)
library(cowplot)
library(ggvenn)

## create folder for outputs to go to
output_dir_tab <- c(here("output/plots"), here("output/tables/"))
for(ff in output_dir_tab){
  fs::dir_create(output_dir_tab)
}

## source useful files
source(here("analysis/R_fn/redaction.R"))
source(here("analysis/R_fn/ggplot_theme.R"))
source(here("analysis/R_fn/geom_venn.R"))

## set redaction threshold
threshold <- 7

# load data ---------------------------------------------------------------
op_neat <- arrow::read_parquet(here("output/openprompt_raw.gz.parquet"))
op_tpp <- arrow::read_parquet(here("output/openprompt_linked_tpp_edited.gz.parquet"))

op_master <- op_neat %>% 
  left_join(op_tpp, by = "patient_id") 

vacced_agreement <- op_master %>% 
  dplyr::select(patient_id, survey_response, n_vaccines, vaccinated, no_prev_vacc) %>% 
  # remove those without linked data
  filter(!is.na(no_prev_vacc)) %>% 
  # keep first survey response only
  filter(survey_response == 1) %>% 
  # create binary ever/never vacced for OP and TPP
  mutate(op_vacced = vaccinated == "Yes",
         tpp_vacced = no_prev_vacc != "0")

covid_agreement <- op_master %>% 
  filter(survey_response == 1) %>% 
  dplyr::select(all_test_positive, all_covid_hosp, total_primarycare_covid, covid_history, n_covids) %>% 
  filter(!is.na(all_test_positive)) %>% 
  mutate(op_covid = covid_history == "Yes (+ve test)",
         tpp_covid = all_test_positive != 0 | all_covid_hosp > 0 | total_primarycare_covid > 0)

longcovid_agreement <- op_master %>% 
  filter(survey_response == 1) %>% 
  dplyr::select(recovered_from_covid, covid_duration, n_distinct_lc_records) %>% 
  # need to also filter out NA responses to the OpenPROMPT question which was optional
  filter(!is.na(n_distinct_lc_records) & !is.na(recovered_from_covid)) %>% 
  mutate(op_longcovid = recovered_from_covid == "No, still have symptoms",
         tpp_longcovid = n_distinct_lc_records > 0)
  

# function to calculate KAppa and look for agreement   --------------------
kappa_calc <- function(df, var1, var2){
  ##
  x <- df[,var1] %>% pull()
  y <- df[,var2] %>% pull()
  tab <- table(x, y)
  a <- tab[1,1]
  b <- tab[1,2]
  c <- tab[2,1]
  d <- tab[2,2]
  N <- a+b+c+d
  # proportion observed agreement
  po <- sum(a+d)/N
  
  # proportion expected agreement
  pyes <- (sum(c+d)/N) * (sum(b+d)/N)
  pno <- (sum(a+b)/N) * (sum(a+c)/N)
  pe <- pyes + pno
  
  # return kappa
  k <- (po - pe)/(1 - pe)
 
  kplot <- k %>% signif(2) %>% prettyNum()
  cap <- paste0("Cohen's kappa = ", kplot)
  pA <- ggplot(df) + 
    geom_venn_v10(aes(A = get(var1), B = get(var2)), fill_color = two_cols, set_names = c("OpenPROMPT", "OpenSAFELY"), show_outside = "always") +
    coord_fixed() +
    theme_void()
  
  pB <- ggplot(df) + 
    geom_venn_v10(aes(A = get(var1), B = get(var2)), fill_color = two_cols, set_names = c("", ""), auto_scale = TRUE, show_percentage = FALSE, text_size = 0.0001, stroke_size = 0.1) + 
    coord_fixed() +
    theme_void() +
    theme(panel.border = element_rect(colour = "gray40", fill = NA),
          panel.background = element_rect(fill = "gray99"))
  
  pvenn <- ggdraw(clip = "off") +
    draw_plot(pA, scale = 0.98, hjust = 0.1, vjust = 0.1) +
    draw_plot(pB, x = 0.3, y = 0.1, scale = 0.45) +
    draw_plot_label(cap, x = 0.1, y = 0.1, size = 9, hjust = 0, fontface = "plain")
  
  return(list(tab = tab, p = prop.table(tab), kappa = k, venn = pvenn))
}

# vars for plotting
# possible_vars <- c("op_vacced", "tpp_vacced", 
#                    "op_covid", "tpp_covid",
#                    "op_longcovid", "tpp_longcovid")
# colours we want plotted - different red and blue vibes
# possible_colour <- c("firebrick", "paleturquoise1", "indianred1", "darkslategray2", "orangered2","cadetblue1")
# # assign names to colours
# names(possible_colour) <- possible_vars

vacc <- kappa_calc(vacced_agreement, "op_vacced", "tpp_vacced")
covid <- kappa_calc(covid_agreement, "op_covid", "tpp_covid")
longcovid <- kappa_calc(longcovid_agreement, "op_longcovid", "tpp_longcovid")

ggsave(filename = here::here("output/plots/vennA.pdf"), vacc$venn, width = 6, height = 4)
ggsave(filename = here::here("output/plots/vennB.pdf"), covid$venn, width = 6, height = 4)
ggsave(filename = here::here("output/plots/vennC.pdf"), longcovid$venn, width = 6, height = 4)

label_list <- sapply(c("A: vaccinated (yes/no)", "B: COVID-19 infection", "C: long COVID"), 
                     function(xx){stringr::str_pad(xx, 25, side = "right")})
pcombo <- plot_grid(
  vacc$venn, 
  covid$venn,
  longcovid$venn,
  labels = label_list,
  ncol = 1, 
  align = "hv",
  label_x = -0.1
  )

ggsave(filename = here::here("output/plots/venn.pdf"), pcombo, width = 8, height = 14,bg = "white")
ggsave(filename = here::here("output/plots/venn.jpeg"), dpi = 450, pcombo, width = 8, height = 14, bg = "white")

# export the data ---------------------------------------------------------
vacc_out <- cbind(vacc$tab, vacc$p) %>% as.data.frame() 
colnames(vacc_out) <- c("opensafely_false", "opensafely_true", "prop_F", "prop_T")
covid_out <- cbind(covid$tab, covid$p) %>% as.data.frame()
colnames(covid_out) <- c("opensafely_false", "opensafely_true", "prop_F", "prop_T")
longcovid_out <- cbind(longcovid$tab, longcovid$p) %>% as.data.frame()
colnames(longcovid_out) <- c("opensafely_false", "opensafely_true", "prop_F", "prop_T")

dataout <- bind_rows(
  mutate(vacc_out, var = c("vaccination", "")),
  mutate(covid_out, var = c("COVID-19 infection", "")),
  mutate(longcovid_out, var = c("Long COVID", ""))
) %>% 
  mutate(result = rep(c("openprompt_false", "openprompt_true"), 3)) %>% 
  dplyr::select(var, result, everything())

rownames(dataout) <- NULL

dataout$opensafely_false <- redact_and_round(dataout$opensafely_false, redact_threshold = 7)
dataout$opensafely_true <- redact_and_round(dataout$opensafely_true, redact_threshold = 7)
dataout$prop_F <- signif(dataout$prop_F, 2)
dataout$prop_T <- signif(dataout$prop_T, 2)

write_csv(dataout, here::here("output/tables/table3_agreement.csv"))
