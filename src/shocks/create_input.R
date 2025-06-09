#!/home/luigi.cesarini/.conda/envs/r_symi/bin/R

suppressPackageStartupMessages(library(dplyr))

library(sf) %>% suppressPackageStartupMessages() %>% suppressWarnings()
library(glue) %>% suppressPackageStartupMessages() %>% suppressWarnings()
library(readxl) %>% suppressPackageStartupMessages() %>% suppressWarnings()
library(stringr) %>% suppressPackageStartupMessages() %>% suppressWarnings()
library(jsonlite) %>% suppressPackageStartupMessages()  %>% suppressWarnings()


# library(ggplot2)
# library(gganimate)

# ggplot(mtcars, aes(factor(cyl), mpg)) + 
#   geom_boxplot() + 
#   # Here comes the gganimate code
#   transition_states(
#     gear,
#     transition_length = 2,
#     state_length = 1
#   ) +
#   enter_fade() + 
#   exit_shrink() +
#   ease_aes('sine-in-out')

# anim_save("filename.gif", animation = last_animation())

setwd("/mnt/beegfs/lcesarini/2025_p_irio")

source("src/shocks/functions_R.R")

args=commandArgs(trailingOnly=TRUE)

#Arguments needed are:
# 1) year
# 2) id event
# The region is later extracted from the list of files for the given ID and the year

DEBUG=FALSE
if (DEBUG) {
    year <- 2017
    id_event <- 40863
}else {
    year <- args[1]
    id_event <- args[2]
}




files <- list.files(glue('out/vector/{year}/'),glue("EVENT_{id_event}_{year}*"),full.names=TRUE)

regions <- lapply(files,function(g) g %>% stringr::str_split('_') %>% unlist() %>% .[4] ) %>% unlist()  

for (reg in regions){
  
  df_hit <- st_read(glue('out/vector/{year}/EVENT_{id_event}_{year}_{reg}_River_ul.gpkg'),quiet=TRUE)
  df_addetti_reg <- read.csv(glue("out/shocks/tot/tot_addetti_by_ateco_{reg}.csv"))

  df_agg_sect_hit <- get_rt_hit(df_hit,df_addetti_reg)

  df_addetti_reg %>% 
      group_by(n_sector) %>% 
      summarise(tot_addetti_hit=sum(addetti_ul)) -> df_addetti_reg_tot
  #Pivot table to get the addetti hit in each week for each sector
  # df_addetti_reg_tot %>% print()

  df_agg_sect_hit %>% 
      reshape2::dcast(n_sector ~ hazus_weeks, value.var = "addetti_ul", fill = 0) -> df_agg_sect_hit_wide

  left_join(df_addetti_reg_tot[,'n_sector'],df_agg_sect_hit_wide,by="n_sector")-> df_agg_sect_hit_wide

  df_agg_sect_hit_wide[is.na(df_agg_sect_hit_wide)] <- 0


  #get the total addetti hit by sector
  df_agg_sect_hit %>% 
      group_by(n_sector) %>% 
      summarise(tot_addetti_hit=sum(addetti_ul))  %>% 
      left_join(.,df_agg_sect_hit_wide,by="n_sector")-> df_agg_sect_hit_tot

  # left_join(df_addetti_reg_tot,df_agg_sect_hit_wide,by="n_sector")-> df_agg_sect_hit_tot

  write.csv(df_agg_sect_hit_tot,glue("test/tot_addetti_by_sector_intermediate_{id_event}_{year}_{reg}.csv"),row.names=FALSE)
}
