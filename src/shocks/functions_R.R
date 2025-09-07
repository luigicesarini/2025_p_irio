suppressPackageStartupMessages(library(dplyr))

library(sf) %>% suppressPackageStartupMessages() %>% suppressWarnings()
library(glue) %>% suppressPackageStartupMessages() %>% suppressWarnings()
library(readxl) %>% suppressPackageStartupMessages() %>% suppressWarnings()
library(stringr) %>% suppressPackageStartupMessages() %>% suppressWarnings()
library(jsonlite) %>% suppressPackageStartupMessages() %>% suppressWarnings()

setwd("/mnt/beegfs/lcesarini/2025_p_irio")

corr_sect <- jsonlite::fromJSON("res/correspondance_sector.json")
resto_time_hazus <- read.csv("res/shocks/restoration_times_hazus.csv")


return_resto_time <- function(df,sector,flood_depth){
  df %>% 
    filter(Sector == sector) %>% pull(flood_depth) -> breaks
  df %>% 
    filter(Sector == sector) %>% pull(restoration_time) -> resto_time

  breaks %>% cut(.,breaks=c(-Inf,.)) -> intervals

  interval_for_wh <- cut(flood_depth, breaks = c(-Inf,breaks))

  index <- which(intervals == interval_for_wh)

  return(resto_time[min(index,length(resto_time))])
}


get_rt_hit  <- function(all_hit,tot_addetti_by_reg){
    all_hit <- left_join(all_hit,tot_addetti_by_reg %>% select(-addetti_ul),by=c("ateco_ul_2007"))  %>% st_set_geometry(NULL)

    all_hit$hazus_weeks <- c(0)
    all_hit$wh_f <- all_hit$WD * 3.28084

    for (i in 1:nrow(all_hit)) {
        # print(i)
        resto_time_list <- c()

        SECTOR <- all_hit$hazus_sector[i]

        if(grepl("+", SECTOR, ignore.case = FALSE, perl = FALSE)) SECTOR  <- str_split(SECTOR, "[+]")[[1]]
        
        
        FLOOD_DEPTH <- all_hit$wh_f[i] 

        for( j in seq_len(length(SECTOR))) resto_time_list[j] <- (return_resto_time(resto_time_hazus,SECTOR[j],FLOOD_DEPTH))


        all_hit$hazus_weeks[i] <- mean(resto_time_list)

    }

    # write.csv(all_hit,"/mnt/beegfs/lcesarini/IRIO_FLOOD_MODEL/DATA/HAZUS/all_hit_luigi.csv")

    weeks<-as.data.frame(table(all_hit$hazus_weeks))

    # Hit local unit by sector: ATECO first, and hazur time restoration later
    all_hit %>% 
        mutate(hazus_weeks=round(hazus_weeks,0)) %>% 
        summarise(
            addetti_ul=sum(addetti_ul),
            .by=c("ateco_ul_2007","hazus_weeks")
        )  %>% 
        left_join(.,df_addetti_reg[,c("ateco_ul_2007","n_sector")]) %>% 
        summarise(
            addetti_ul=sum(addetti_ul),
            .by=c("n_sector","hazus_weeks")
        ) -> df_agg_sect_hit

    return(df_agg_sect_hit)

}



get_num_claims <- function(weeks,nsect=43,xx,res_xx,type="S") {
  col_var <- glue::glue("DT_{type}_ww") %>% as.character()
  size_var <- glue::glue("sizeBI_{type}_ww") %>% as.character()
  res_size_var <- glue::glue("res_sizeBI_{type}_ww") %>% as.character()
  tot_var <- glue::glue("tot_sizeBI_{type}_ww") %>% as.character()

  num <- data.frame(matrix(0, nrow=nsect, ncol=(1+(nrow(weeks)+1))))

  colnames(num)<-c(
      "irpet_n",
      as.numeric(as.character(weeks$Var1)),
      max(as.numeric(as.character(weeks$Var1)))+1
      )

  num[,1]<-c(1:nsect)

  num <- reshape2::melt(num,id.vars="irpet_n",variable.name=col_var,factorsAsStrings=TRUE) 

  num <- num %>% mutate(across(where(is.factor), ~ as.numeric(as.character(.x))))


  bind_rows(xx,
  res_xx %>% rename(size_var=res_size_var)) -> binded
  #   summarise(glue("tot_sizeBI_{type}_ww") = sum(glue("sizeBI_{type}_ww")), .by = c('irpet_n',glue('DT_{type}_ww'))) -> tot_xx
  #   group_by(irpet_n, !!sym(col_var)) %>%
  #     summarise(!!sym(tot_var) := sum(!!sym(size_var)), .groups="drop") -> tot_xx 

  # left_join(num,tot_xx, by=c('irpet_n',col_var)) %>% 
  #     select(-value) %>% 
  #     reshape2::dcast(irpet_n ~ get(col_var))) -> num

  # num[is.na(num)] <- 0

  return(c(num,binded))
}



