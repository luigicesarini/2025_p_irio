library(glue)
library(dplyr)
library(ggplot2)
library(reshape2)
library(data.table)

setwd("/mnt/beegfs/lcesarini/2025_p_irio")

sigla_region <- 'TOS'

readRDS(glue("test/Output_{sigla_region}_2025.rds")) -> result

#What is inside the result object?
#######################################################
# 1.simvec,
# 2.prod_sectors,
# 3.prod_sectors_m,
# 4.prod_sectors_q,
# 5.expenditure_q,
# 6.GDP,
# 7.GDP2,
# 8.GDP_regions,
# 9.GDP_regions2,
# 10.GDP_q,
# 11.Cons,
# 12.Inv,
# 13.sales,
# 14.sales_sectors,
# 15.intensity11,
# 16.intensity12,
# 17.intensity21,
# 18.intensity22,
# 19.bottlenecks,
# 20.delivered,
# 21.capacitySectors,
# 22.capacityRegions,
# 23.demandTotal,
# 24.demandRegions,
# 25.demandSectors,
# 26.producibleTotal,
# 27.producibleRegions,
# 28.producibleSectors,
# 29.consTotal,
# 30.consRegions,
# 31.consSectors,
# 32.ratioTotal,
# 33.ratioRegions,
# 34.ratioSectors,
# 35.GDPShocks,
# 36.outputShocks,
# 37.outputShocksRegions,
# 38.outputShocksSectors,
# 39.ratioShocks,
# 40.ratioShocksRegions,
# 41.ratioShocksSectors,
# 42.outputTotal
#######################################################
#######################################################
#######################################################
#######################################################




result[[2]] %>% dim()
result[[3]] %>% dim()
result[[4]] %>% dim()
result[[8]][,1]


# GDP by region as in Fig.5
result[[9]] %>% 
    data.frame() %>% 
    tibble::rownames_to_column(var="Weeks") %>% 
        melt(id.vars="Weeks") %>% 
        # filter(variable=='X8') %>% 
        ggplot(aes(x=as.numeric(Weeks),y=value)) +
        geom_line(aes(col=as.factor(variable))) +
        theme_minimal() +
        labs(title="GDP by region",x="Region",y="GDP")
        # coord_cartesian(xlim=c(0,108),ylim=c(90,105)) +
        # scale_x_continuous(breaks=seq(0,108,12)) +
        # scale_y_continuous(breaks=seq(90,105,2))

ggsave("out/figures/gdp_by_region.png", width=8, height=5, dpi=300)


result[[37]][1:52,] %>% apply(., 2, mean)
result[[33]][52:104,] %>% apply(., 2, mean)


result[[28]] %>% dim()
result[[38]] %>% dim()


result[[28]] %>% 
    data.frame() %>% 
    tibble::rownames_to_column(var="Weeks") %>% 
        melt(id.vars="Weeks") %>% 
        # filter(variable=='X8') %>% 
        ggplot(aes(x=as.numeric(Weeks),y=value)) +
        geom_line() +
        theme_minimal() +
        facet_wrap(~variable,scales="free_y") +
        labs(title="Producible by sector",x="Region",y="Producible")
        # coord_cartesian(xlim=c(0,108),ylim=c(90,105)) +
        # scale_x_continuous(breaks=seq(0,108,12)) +
        # scale_y_continuous(breaks=seq(90,105,2))



