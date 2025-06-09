library(sf)
library(tmap)
library(dplyr)
library(reshape2)
library(ggplot2)
 
 

data.frame(result[[8]]) %>% 
tibble::rownames_to_column(var="weeks") %>%
melt() %>% 
ggplot(.,aes(x=as.integer(weeks),y=value,col=as.factor(variable)))+
geom_line()+
facet_wrap(~variable,scales="free_y")+
theme_minimal()


plot(data.frame(result[[6]])[,1],type='l')
