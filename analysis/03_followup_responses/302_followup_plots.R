library(tidyverse)
library(janitor)
library(RColorBrewer)
library(forcats)
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


# check if data exists before running into errors -------------------------
if (!file.exists(here("output/tables/table2_fup_stats.csv"))) {
  stop( "You need to release output/table2_fup_stats.csv from the secure server" )
} else{
  
  ## import the followupstats
  tab2stats <- read_csv(here("output/tables/table2_fup_stats.csv"))
  
  ## export the table for easy copy and paste life
  table2 <- tab2stats %>% 
    dplyr::select(variable, level, stat, survey_response) %>% 
    pivot_wider(names_from = survey_response, values_from = stat, names_prefix = "survey_") %>% 
    group_by(variable) %>% 
    mutate(row_n = 1:n()) %>% 
    ungroup() %>% 
    mutate(variable_lab = ifelse(row_n > 1, "", variable)) %>% 
    dplyr::select(-row_n, -variable) %>% 
    dplyr::select(variable_lab, everything())
  
  table2_n <- tab2stats %>% 
    ## different numbers of people will have answered the various questions
    group_by(survey_response) %>% 
    summarise(stat = prettyNum(median(N), big.mark = ",")) %>%
    ungroup() %>% 
    mutate(variable = "N") %>% 
    ## format for bind_rows
    pivot_wider(id_cols = variable, names_from = survey_response, values_from = stat, names_prefix = "survey_") %>% 
    mutate(level = "") %>% 
    dplyr::select(variable_lab = variable, level, everything())
  
  table2 <- table2_n %>% 
    bind_rows(table2)
  
  write_csv(table2, here("output/tables/table2_formatted.csv"))
  
  ## EQ5D bar charts 
  pd <- position_dodge2(width = 0.9)
  
  catvars <-
    c(
      "EQ5D",
      "FACIT",
      "breathlessness",
      "COVID-19 status",
      "Duration of longest COVID-19 infection",
      "Have you had at least one COVID-19 vaccine dose?",
      "Number of previous COVID-19 infections",
      "Recovered from most recent COVID-19 infection?"
    )
  pals <- c("Greens", "Reds", "Oranges", rep("Blues", 5))
  names(pals) <- catvars
  
  plot_n_or_p <- function(yy){
    plot_some_categorical_variables <- function(data_in, stringsearch, yaxis = "n"){
      data_plot <- data_in %>% 
        filter(stringr::str_detect(variable, stringsearch)) %>% 
        dplyr::select(variable, level, n, p, survey_response)
      
      data_plot$level <- forcats::as_factor(data_plot$level)
      levels(data_plot$level) <- stringr::str_to_title(levels(data_plot$level))
      if(stringsearch == "breathlessness"){
        levels(data_plot$level) <- stringr::str_sub(levels(data_plot$level), 1, 7)
      }
      data_plot$nice_n <- prettyNum(data_plot$n, big.mark = ",")
      
      legend <- ifelse(stringr::str_detect(stringsearch, "COVID"), "right", "bottom")
      barplot <- ggplot(data_plot, aes(x = survey_response, y = get(yaxis), label = nice_n, colour = level, fill = level)) +
        geom_col(alpha = 0.4, position = pd) +
        facet_wrap(~variable, ncol = 1) +
        scale_fill_brewer(palette = pals[stringsearch]) +
        scale_colour_brewer(palette = pals[stringsearch]) +
        labs(y = "n", x = "Survey round", colour = "Response", fill = "Response") +
        theme_ali() +
        theme(strip.text = element_text(face = "bold", hjust = 0, colour = "gray20"),
              legend.position = legend,
              panel.grid.major.y = element_blank(),
              axis.line=element_line())
      if(yaxis == "p"){
        barplot +
          ylim(c(0,100)) +
          geom_label(fill = NA, colour = "black", position = pd, label.size = NA, vjust = 0)
      }else{
        barplot
      }
    }
    
    p3a <- plot_some_categorical_variables(tab2stats, catvars[1], yaxis = yy)
    p3b <- plot_some_categorical_variables(tab2stats, catvars[2], yaxis = yy)
    p3c <- plot_some_categorical_variables(tab2stats, catvars[3], yaxis = yy)
    p3da <- plot_some_categorical_variables(tab2stats, catvars[4], yaxis = yy)
    p3db <- plot_some_categorical_variables(tab2stats, catvars[5], yaxis = yy)
    p3dc <- plot_some_categorical_variables(tab2stats, catvars[6], yaxis = yy)
    p3dd <- plot_some_categorical_variables(tab2stats, catvars[7], yaxis = yy)
    p3de <- plot_some_categorical_variables(tab2stats, catvars[8], yaxis = yy)
  
    ## 3 columns. FACIT/(EQ5D,MRC)/COVIDs 
    p3 <- cowplot::plot_grid(
      ## column 1 - FACIT only
      cowplot::plot_grid(p3b, labels = "A"),
      ## column 2 - Eq5d, MRC
      cowplot::plot_grid(p3a, p3c, ncol = 1, labels = c("B", "C"), rel_heights = c(0.8, 0.2)),
      ## column 3 - COVIDs
      cowplot::plot_grid(p3da, p3db, p3dc, p3dd, p3de,
                         ncol = 1, labels = LETTERS[4:8]),
      ## combine together in a blaze of glory
      ncol = 3
    )  
    p3
  }
  p3_n <- plot_n_or_p("n")
  ggsave(filename=here::here("output", "plots","openprompt_figure3.tiff"), p3_n, dpi=450, width = 21, height = 10, units = "in", bg = "white")
  ggsave(filename=here::here("output", "plots","openprompt_figure3.png"),  p3_n, dpi=450, width = 21, height = 10, units = "in", bg = "white")
  
  p3_p <- plot_n_or_p("p")
  ggsave(filename=here::here("output", "plots","openprompt_figure3_proportion.tiff"), p3_p, dpi=450, width = 26, height = 10, units = "in", bg = "white")
  ggsave(filename=here::here("output", "plots","openprompt_figure3_proportion.png"),  p3_p, dpi=450, width = 26, height = 10, units = "in", bg = "white")
}
  