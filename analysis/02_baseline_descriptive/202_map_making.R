library(tidyverse)
library(sf)
library(janitor)
library(forcats)
library(here)
library(cowplot)

## create folder for outputs to go to
output_dir_tab <- here("output/plots")
fs::dir_create(output_dir_tab)

## source useful files
source(here("analysis/R_fn/redaction.R"))
source(here("analysis/R_fn/ggplot_theme.R"))

## set redaction threshold
threshold <- 7

# read_in_data ------------------------------------------------------------
if(!file.exists(here("data/NUTS_Level_1_January_2018_FEB_in_the_United_Kingdom.shp"))) {
  stop( "You need to download the NUTS_Level_1_January_2018_FEB_in_the_United_Kingdom.shp files from https://geoportal.statistics.gov.uk" )
} else if (!file.exists(here("output/tables/table1_stats.csv"))) {
  stop( "You need to release output/table1_stats.csv from the secure server" )
} else{
  ## import the regional distribution stats 
  table1_stats <- read_csv(here("output/tables/table1_stats.csv"))
  
  ## make the nice formatted table 1
  table1 <- table1_stats %>% 
    group_by(variable) %>% 
    mutate(row_n = 1:n()) %>% 
    ungroup() %>% 
    mutate(variable_lab = ifelse(row_n > 1, "", variable)) %>% 
    dplyr::select(-row_n, -variable, -n, -p) %>% 
    dplyr::select(variable_lab, everything())
  
  write_csv(table1, here("output/tables/table1_formatted.csv"))
  
  ## p1A - the map!
  region_dist <- table1_stats %>% 
    filter(variable == "Region") %>% 
    rename("region_dist" = "p",
           "region" = "level")
  
  nuts_shp <- st_read("data/NUTS_Level_1_January_2018_FEB_in_the_United_Kingdom.shp")
  
  nuts_shp_eng <- nuts_shp %>% 
    filter(stringr::str_detect(nuts118nm, "England|Yorkshire|London")) %>% 
    mutate(region = stringr::str_remove_all(nuts118nm, " \\(England\\)"))
  
  coverage_plot <- nuts_shp_eng %>%
    left_join(region_dist, by="region") %>% 
    ggplot(aes(geometry = geometry, fill=region_dist)) +
    geom_sf(lwd = .25, colour='black') +
    geom_sf_label(aes(label = paste0(round(region_dist,1),"%")),
                  label.size = 0.1, 
                  colour = "white",
                  label.r = unit(0.5, "lines"),
                  fun.geometry = st_centroid,
                  show.legend = F) +
    theme(legend.position = "none",
          legend.text.align = 1,
          panel.background=element_rect(fill="white")) + 
    #guides(fill=guide_legend(title="OpenPROMPT cohort (%)")) + 
    xlab("") + ylab("")
  
  ggsave(filename=here::here("output", "plots","openprompt_dist_map.tiff"),coverage_plot,dpi=600,width = 20,height = 20, units = "cm")
  
  ## p1B - age distribution
  categorical_data <- table1_stats %>% 
    filter(!is.na(level)) %>% 
    mutate(app_or_tpp = factor(str_detect(variable, "app question"), labels = c("TPP", "App")),
           level_temp = paste(variable, level, sep = "_"),
           level_temp2 = factor(level_temp, levels = level_temp, labels = level)) 
  
  # little function to suppress the facet_grid labels so they are not repeated
  wrapper <- function(string) {
    stringr::str_wrap(string, width = 25)
  }
  
  barcharts <- function(data_in){
    
    # colour pallette
    pal <- ifelse(data_in$app_or_tpp[1]=="TPP", "A", "D")
    
    # position dodge
    pd <- position_dodge2(width = 0.9)
    
    # explicitly order the facet grid
    facet_order <- unique(data_in$variable)[order(unique(data_in$variable), decreasing = FALSE)]
    if(sum(facet_order=="Linked records available")==1){
      facet_order <- c("Linked records available", facet_order[facet_order!="Linked records available"])
    }
    # get x-axis labels
    labeller <- data_in %>% 
      group_by(level_temp) %>% 
      slice(1) %>% 
      dplyr::select(level, level_temp)
    
    labeller2 <- labeller$level
    names(labeller2) <- labeller$level_temp
    
    data_in %>% 
      arrange(variable, level) %>% 
      mutate_if(is_character, ~factor(.)) %>% 
      mutate(level_temp = forcats::fct_reorder(.f = level_temp, .x = levels(level_temp), .desc = TRUE)) %>% 
      mutate(nice_n = prettyNum(n, big.mark = ","),
             label = paste0(level_temp2, ": ", nice_n),
             variable = factor(variable, levels = facet_order)) %>% 
      # make the plot
      ggplot(aes(x = level_temp, y = p, fill = level_temp)) +
      geom_col(alpha = 0.8, lty = 0, position = pd) +
      scale_fill_viridis_d(option = pal) +
      geom_label(aes(label = nice_n), fill = NA, colour = "black", position = pd, label.size = NA, hjust = 0) +
      labs(x = "", y = "Percent (%)") +
      coord_flip() +
      scale_y_continuous(limits=c(0,100)) +
      scale_x_discrete(labels = labeller2) + 
      guides(fill = "none", label = "none") +
      facet_grid(
        rows = vars(variable),
        scales = "free",
        space = "free",
        switch = "y", 
        labeller = labeller(variable = wrapper)
      ) +
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
        legend.position = "right",
        legend.title = element_text(face = "bold", size = 10),
        axis.text.x = element_text(face = "bold"),
        panel.grid.major.y = element_blank(), 
        axis.title = element_text(size = 10, face = "bold"),
        plot.title = element_text(
          face = "bold",
          hjust = 0.5,
          size = 13
        )
      ) 
  }
  tpp_data <- categorical_data %>% filter(app_or_tpp == "TPP", variable != "Region")
  app_data <- categorical_data %>% filter(app_or_tpp == "App")
  p1b <- barcharts(tpp_data)
  p1c <- barcharts(app_data)
 
  ## p1d - the continuous data
  continuous_data <- table1_stats %>% 
    filter(is.na(level)) %>% 
    mutate(stat = stringr::str_split(stat, n = 4, pattern = "\\(|-|\\)")) %>% 
    unnest(stat) %>% 
    mutate(stat = as.numeric(stat),
           name = c("mean", "p25", "p75", NA)) %>% 
    filter(!is.na(stat)) %>% 
    pivot_wider(values_from = stat)
    
  p1d <- ggplot(continuous_data, aes(x = variable, y = mean)) +
    geom_errorbar(aes(ymin = p25, ymax = p75), colour = "darkblue", width = 0.25) +
    geom_point(col = "darkblue", size = 20, shape = 18) +
    labs(x = "", y = "Mean (IQR)") +
    ylim(c(0,100)) +
    coord_flip() +
    theme_ali() +
    theme(axis.line.y = element_blank(),
          panel.border = element_rect(fill = NA, colour = "white"),
          panel.grid = element_blank())
  
  ## combine the plots and output
  figure2 <- cowplot::plot_grid(
    cowplot::plot_grid(p1d, coverage_plot, nrow = 2, ncol = 1, labels = c("A", "C"), rel_heights = c(0.4, 1)),
    cowplot::plot_grid(p1c, p1b, ncol = 1, labels = c("B", "D"), rel_heights = c(1.025, 1)), 
    ncol = 2, nrow = 1, rel_widths = c(0.7, 1))
  
  ggsave(filename=here::here("output", "plots","openprompt_figure2.tiff"), figure2, dpi=450, width = 14, height = 12, units = "in", bg = "white")
  ggsave(filename=here::here("output", "plots","openprompt_figure2.png"),  figure2, dpi=450, width = 14, height = 12, units = "in", bg = "white")
}
