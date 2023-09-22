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
  
  ## import the represertnativeness data
  tab3stats <- read_csv(here("output/tables/table3_rep_stats.csv"))
  
  ## export the table for easy copy and paste life
  table3 <- tab3stats %>% 
    dplyr::select(variable, level, stat, data_source) %>% 
    pivot_wider(names_from = data_source, values_from = stat, names_prefix = "survey_") %>% 
    group_by(variable) %>% 
    mutate(row_n = 1:n()) %>% 
    ungroup() %>% 
    mutate(variable_lab = ifelse(row_n > 1, "", variable)) %>% 
    dplyr::select(-row_n, -variable) %>% 
    dplyr::select(variable_lab, everything())
  
  
  table3_n <- tab3stats %>% 
    ## different numbers of people will have answered the various questions
    group_by(data_source) %>% 
    summarise(stat = prettyNum(max(N), big.mark = ",")) %>%
    ungroup() %>% 
    mutate(variable = "N") %>% 
    ## format for bind_rows
    pivot_wider(id_cols = variable, names_from = data_source, values_from = stat, names_prefix = "survey_") %>% 
    mutate(level = "") %>% 
    dplyr::select(variable_lab = variable, level, everything())
  
  table3 <- table3_n %>% 
    bind_rows(table3)
  
  write_csv(table3, here("output/tables/table3_formatted.csv"))
  
  ## EQ5D bar charts 
  
  # age profile comparison --------------------------------------------------
  age_cat_plot <- tab3stats %>% 
    filter(variable == "Age category") %>% 
    dplyr::select(variable, level, data_source, p)
  
  p_age <- ggplot(age_cat_plot, aes(x = level, y = p, group = data_source, colour = data_source, linetype = data_source)) + 
    geom_point(pch = 1, size = 5) + 
    geom_line(linewidth = 1.1) +
    scale_colour_manual(values = c("indianred1", "darkslategray4")) + 
    labs(x = "Age category", y = "%", colour = "Data source", linetype = "Data source") +
    theme_ali() +
    theme(panel.grid.major.y = element_blank(),
          legend.position = "top") 
  
  ## and the rest
  wrapper <- function(string) {
    stringr::str_wrap(string, width = 25)
  }
  
  tab3plot <- tab3stats %>% 
    filter(is.na(mean)) %>% 
    mutate(level_temp = paste(variable, level, sep = "_"),
           level_temp2 = factor(level_temp, levels = level_temp, labels = level))
  
  # position dodge
  pd <- position_dodge2(width = 0.9)
  
  # explicitly order the facet grid
  facet_order <- unique(data_in$variable)[order(unique(data_in$variable), decreasing = FALSE)]
  if(sum(facet_order=="Linked records available")==1){
    facet_order <- c("Linked records available", facet_order[facet_order!="Linked records available"])
  }
  
  # get x-axis labels
  labeller <- tab3plot %>% 
    group_by(level_temp) %>% 
    slice(1) %>% 
    dplyr::select(level, level_temp)
  
  labeller2 <- stringr::str_to_title(labeller$level)
  names(labeller2) <- labeller$level_temp
  
  barplotting <- function(var, plot_legend = FALSE){
    
    lp <- ifelse(plot_legend, "top", "none")
    
    tab3plot %>% 
      filter(variable == var) %>% 
    ggplot(aes(x = level_temp, y = p, fill = data_source)) + 
      geom_col(position = pd) +
      scale_x_discrete(labels = labeller2) + 
      scale_fill_manual(values = c("indianred1", "darkslategray4")) +
      labs(x = {var}, y = "Percent (%)", fill = "Data source") +
      theme_classic() +
      theme(
        panel.background = element_blank(), 
        title = element_text(size = 9),
        strip.background = element_rect(colour = NA, fill = NA),
        strip.text.x = element_text(face = "bold", size = 9),
        strip.text.y.left = element_text(size = 9, angle = 0, hjust = 0, vjust = 0.9, face = 3),
        strip.placement = "outside",
        panel.border = element_rect(fill = NA, color = NA),
        axis.line=element_line(),
        legend.position = lp,
        legend.title = element_text(face = "bold", size = 10),
        axis.text.x = element_text(face = "bold", angle = 10, hjust = 0.95),
        panel.grid.major.y = element_blank(), 
        axis.title = element_text(size = 10, face = "bold"),
        plot.title = element_text(
          face = "bold",
          hjust = 0.5,
          size = 13
        )
      ) 
  }
  #p1 <- barplotting("Age category", plot_legend = TRUE)
  p2 <- barplotting("Ethnicity")
  p3 <- barplotting("Gender (OpenPROMPT)/Sex (OpenSAFELY)")
  p4 <- barplotting("Household income (OpenPROMPT only)")
  p5 <- barplotting("Index of Multiple Deprivation")
  
  figure4 <- cowplot::plot_grid(
    plot_grid(p_age, labels = LETTERS[1], ncol = 1),
    plot_grid(p5, p3, p4, p2, ncol = 2, labels = LETTERS[2:5]),
    ncol = 1, rel_heights = c(0.4, 0.7)
  )
  
  ggsave(filename=here::here("output", "plots","openprompt_figure4.tiff"), figure4, dpi=450, width = 13, height = 8, units = "in", bg = "white")
  ggsave(filename=here::here("output", "plots","openprompt_figure4.png"),  figure4, dpi=450, width = 13, height = 8, units = "in", bg = "white")

}

