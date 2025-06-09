suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))

DTB <- readRDS("//mnt/beegfs/lcesarini/IRIO_FLOOD_MODEL/DATA/MARCELLO/DTB.rds")
# the weakly data on BI are wrong

# Event: flood May 2023
all_hit<-subset(DTB,DTB$wh_m>0)
all_hit[is.na(all_hit)]<-0
DTB[is.na(DTB)]<-0


# downtime in weeks
all_hit$DT_S_ww <- all_hit$DT_S %/% 7
all_hit$resDT_S_ww <- (all_hit$DT_S %% 7 ) %>% round()
# (all_hit$DT_S %% 7 )  %>% head()
all_hit$DT_M_ww <- all_hit$DT_M %/% 7
all_hit$resDT_M_ww <- (all_hit$DT_M %% 7 ) %>% round()

all_hit$DT_I_ww <- all_hit$DT_I %/% 7
all_hit$resDT_I_ww <- (all_hit$DT_I %% 7 ) %>% round()


# ###
# GP == 'total' means that the GP_xx is the gross profit reduction in percentage for the entire period of downtime,
# thus, by dividign the GP_xx for the number of downtime weeks, we get theBI interruption over the 
# entire period split weekly.
# For example:
# a GP of 0.5 for 2 weeks of downtime would require
# GP="percday"
# if (GP == "percday") {
    
#     all_hit$BI_S_ww<-(all_hit$GP_S*all_hit$DT_S) / (all_hit$DT_S/7)
#     all_hit$BI_M_ww<-(all_hit$GP_M*all_hit$DT_M) / (all_hit$DT_M/7)
#     all_hit$BI_I_ww<-(all_hit$GP_I*all_hit$DT_I) / (all_hit$DT_I/7)

# }else if (GP == "total") {
   
#     all_hit$BI_S_ww<-(all_hit$GP_S) / (all_hit$DT_S/7)
#     all_hit$BI_M_ww<-(all_hit$GP_M) / (all_hit$DT_M/7)
#     all_hit$BI_I_ww<-(all_hit$GP_I) / (all_hit$DT_I/7)

# }

# Business interruption per week of downtime:
Days_ww =7

all_hit$BI_S_ww<-all_hit$GP_S*Days_ww/Days_ww
all_hit$BI_M_ww<-all_hit$GP_M*Days_ww/Days_ww
all_hit$BI_I_ww<-all_hit$GP_I*Days_ww/Days_ww

all_hit$resBI_S_ww<-all_hit$GP_S* all_hit$resDT_S_ww/Days_ww
all_hit$resBI_M_ww<-all_hit$GP_M* all_hit$resDT_M_ww/Days_ww
all_hit$resBI_I_ww<-all_hit$GP_I* all_hit$resDT_I_ww/Days_ww

weeks_S <- as.data.frame(table(all_hit$DT_S_ww)[-1])
weeks_M <- as.data.frame(table(all_hit$DT_M_ww)[-1])
weeks_I <- as.data.frame(table(all_hit$DT_I_ww)[-1])
## Now, for every downtime week, calculate the weighted average of BI by sector (weights = n.employees)

# step 1: make 3 different dataframes
nsect=43

barBI_S_ww <- matrix(NA,nrow = nsect, ncol = (length(weeks_S$Var1)+1))
rownames(barBI_S_ww) <- c(1:nsect)
#colnames(barBI_S_ww) <- t(c(weeks_S$Var1,27))
barBI_S_ww <- as.data.frame(barBI_S_ww)
barBI_S_ww$irpet_n<-c(1:nsect)

barBI_M_ww <- matrix(NA,nrow = nsect, ncol = (length(weeks_M$Var1)+1))
rownames(barBI_M_ww) <- c(1:nsect)
#colnames(barBI_M_ww) <- t(weeks_M$Var1)
barBI_M_ww <- as.data.frame(barBI_M_ww)
barBI_M_ww$irpet_n<-c(1:nsect)

barBI_I_ww <- matrix(NA,nrow = nsect, ncol = (length(weeks_I$Var1)+1))
rownames(barBI_I_ww) <- c(1:nsect)
#colnames(barBI_I_ww) <- t(weeks_I$Var1)
barBI_I_ww <- as.data.frame(barBI_I_ww)
barBI_I_ww$irpet_n<-c(1:nsect)

# dim(barBI_I_ww) 43 x 26, sector by weeks of downtime
# step 2: associate every ul BI to the corresponding n. of employees
all_hit$sizeBI_S_ww <- all_hit$BI_S_ww * all_hit$addetti_ul 
all_hit$res_sizeBI_S_ww <- all_hit$resBI_S_ww * all_hit$addetti_ul

all_hit$sizeBI_M_ww <- all_hit$BI_M_ww * all_hit$addetti_ul 
all_hit$res_sizeBI_M_ww <- all_hit$resBI_M_ww * all_hit$addetti_ul

all_hit$sizeBI_I_ww <- all_hit$BI_I_ww * all_hit$addetti_ul 
all_hit$res_sizeBI_I_ww <- all_hit$resBI_I_ww * all_hit$addetti_ul


xx_S <- aggregate(sizeBI_S_ww ~ irpet_n+DT_S_ww, data=all_hit, FUN=sum)  %>% filter(DT_S_ww != 0)
xx_M <- aggregate(sizeBI_M_ww ~ irpet_n+DT_M_ww, data=all_hit, FUN=sum)  %>% filter(DT_M_ww != 0)
xx_I <- aggregate(sizeBI_I_ww ~ irpet_n+DT_I_ww, data=all_hit, FUN=sum)  %>% filter(DT_I_ww != 0)


res_xx_S <- aggregate(res_sizeBI_S_ww ~ irpet_n+DT_S_ww, data=all_hit, FUN=sum)  %>% filter(DT_S_ww != 0)
res_xx_M <- aggregate(res_sizeBI_M_ww ~ irpet_n+DT_M_ww, data=all_hit, FUN=sum)  %>% filter(DT_M_ww != 0)
res_xx_I <- aggregate(res_sizeBI_I_ww ~ irpet_n+DT_I_ww, data=all_hit, FUN=sum)  %>% filter(DT_I_ww != 0)



# useless lines below
# num_S[1:3,2:24]<-0
# num_S[24:25,2:24]<-0
# num_S[39,2:24]<-0
# num_S[41:42,2:24]<-0
# colnames(num_S)<-c("irpet_n","hit_dt_5weeks","hit_dt_6weeks","hit_dt_7weeks","hit_dt_8weeks","hit_dt_9weeks",
#                    "hit_dt_10weeks","hit_dt_11weeks","hit_dt_12weeks","hit_dt_13weeks","hit_dt_14weeks",
#                    "hit_dt_15weeks","hit_dt_16weeks","hit_dt_17weeks","hit_dt_18weeks","hit_dt_19weeks",
#                    "hit_dt_20weeks","hit_dt_21weeks","hit_dt_22weeks","hit_dt_23weeks","hit_dt_24weeks",
#                    "hit_dt_25weeks","hit_dt_27weeks","hit_dt_28weeks")




num_S <- data.frame(matrix(0, nrow=nsect, ncol=(1+(nrow(weeks_S)+1))))
colnames(num_S)<-c("irpet_n",as.numeric(levels(weeks_S$Var1)),max(as.numeric(levels(weeks_S$Var1)))+1)
num_S[,1]<-c(1:nsect)
num_S <- reshape2::melt(num_S,id.vars="irpet_n",variable.name="DT_S_ww",factorsAsStrings=TRUE) 

num_S$DT_S_ww <- num_S$DT_S_ww %>% as.character() %>% as.numeric()
# all_hit$DT_S_ww %>% unique() %>% sort()

# num_S %>% head()
# xx_S %>% head()

# num_S %>% str()
# xx_S %>% str()



left_join(num_S,xx_S, by=c('irpet_n','DT_S_ww')) %>% 
    select(-value) %>% 
    reshape2::dcast(irpet_n ~ DT_S_ww) -> num_S


num_S[is.na(num_S)] <- 0

saveRDS(all_hit,"/mnt/beegfs/lcesarini/2025_p_irio/test/all_hit_luigi.rds")
saveRDS(xx_S,"/mnt/beegfs/lcesarini/2025_p_irio/test/xx_S_luigi.rds")
saveRDS(res_xx_S,"/mnt/beegfs/lcesarini/2025_p_irio/test/res_xx_S_luigi.rds")
saveRDS(num_S,"/mnt/beegfs/lcesarini/2025_p_irio/test/num_S_luigi.rds")

# View(num_S)

# num_S_jle <- read.csv("/mnt/beegfs/lcesarini/num_S_jle.csv")

# print(head(num_S_jle))

# diff=-num_S[,] + num_S_jle[,] 

# diff[diff < 1e-6] = 0 
# View(diff)

