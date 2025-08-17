suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))

"%!in%" <- Negate("%in%")
setwd("/mnt/beegfs/lcesarini/2025_p_irio")

source("src/shocks/functions_R.R")

DTB <- readRDS("//mnt/beegfs/lcesarini/IRIO_FLOOD_MODEL/DATA/MARCELLO/DTB.rds")
# the weakly data on BI are wrong

# Event: flood May 2023
all_hit<-subset(DTB,DTB$wh_m>0)
all_hit[is.na(all_hit)]<-0
DTB[is.na(DTB)]<-0


# downtime in weeks
all_hit$DT_S[1:10] %% 7
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
#questa divisione e moltiplicazione pretty useless
all_hit$BI_S_ww<-all_hit$GP_S*Days_ww/Days_ww
all_hit$BI_M_ww<-all_hit$GP_M*Days_ww/Days_ww
all_hit$BI_I_ww<-all_hit$GP_I*Days_ww/Days_ww

all_hit$resBI_S_ww<-all_hit$GP_S* all_hit$resDT_S_ww/Days_ww
all_hit$resBI_M_ww<-all_hit$GP_M* all_hit$resDT_M_ww/Days_ww
all_hit$resBI_I_ww<-all_hit$GP_I* all_hit$resDT_I_ww/Days_ww

weeks_S <- as.data.frame(table(all_hit$DT_S_ww))[-1,]
weeks_M <- as.data.frame(table(all_hit$DT_M_ww))[-1,]
weeks_I <- as.data.frame(table(all_hit$DT_I_ww))[-1,]
## Now, for every downtime week, calculate the weighted average of BI by sector (weights = n.employees)

# step 1: make 3 different dataframes
nsect=43


# step 2: associate every ul BI to the corresponding n. of employees
all_hit$sizeBI_S_ww <- all_hit$BI_S_ww * all_hit$addetti_ul 
all_hit$res_sizeBI_S_ww <- all_hit$resBI_S_ww * all_hit$addetti_ul

all_hit$sizeBI_M_ww <- all_hit$BI_M_ww * all_hit$addetti_ul 
all_hit$res_sizeBI_M_ww <- all_hit$resBI_M_ww * all_hit$addetti_ul

all_hit$sizeBI_I_ww <- all_hit$BI_I_ww * all_hit$addetti_ul 
all_hit$res_sizeBI_I_ww <- all_hit$resBI_I_ww * all_hit$addetti_ul

xx_S <- aggregate(sizeBI_S_ww ~ irpet_n+DT_S_ww, data=all_hit, FUN=sum)  #%>% filter(DT_S_ww != 0)
xx_M <- aggregate(sizeBI_M_ww ~ irpet_n+DT_M_ww, data=all_hit, FUN=sum)  #%>% filter(DT_M_ww != 0)
xx_I <- aggregate(sizeBI_I_ww ~ irpet_n+DT_I_ww, data=all_hit, FUN=sum)  #%>% filter(DT_I_ww != 0)


res_xx_S <- aggregate(res_sizeBI_S_ww ~ irpet_n+DT_S_ww, data=all_hit, FUN=sum)  #%>% filter(DT_S_ww != 0)
res_xx_M <- aggregate(res_sizeBI_M_ww ~ irpet_n+DT_M_ww, data=all_hit, FUN=sum)  #%>% filter(DT_M_ww != 0)
res_xx_I <- aggregate(res_sizeBI_I_ww ~ irpet_n+DT_I_ww, data=all_hit, FUN=sum)  #%>% filter(DT_I_ww != 0)

# GENERATE NUM for S,M,I
# S
# add a plus one to the residual to assign it to the next week
res_xx_S %>% 
    mutate(
        DT_S_ww=case_when(
            DT_S_ww==0 ~ 0,
            TRUE ~ DT_S_ww + 1
        )
    ) -> res_xx_S



num_S <- data.frame(matrix(0, nrow=nsect, ncol=(1+(nrow(weeks_S)+1))))
colnames(num_S)<-c(
    "irpet_n",
    as.numeric(as.character(weeks_S$Var1)),
    max(as.numeric(as.character(weeks_S$Var1)))+1
    )
num_S[,1]<-c(1:nsect)
num_S <- reshape2::melt(num_S,id.vars="irpet_n",variable.name="DT_S_ww",factorsAsStrings=TRUE) 

num_S$DT_S_ww <- num_S$DT_S_ww %>% as.character() %>% as.numeric()


bind_rows(xx_S,res_xx_S %>% rename("sizeBI_S_ww"="res_sizeBI_S_ww")) %>% 
    summarise(tot_sizeBI_S_ww = sum(sizeBI_S_ww), .by = c('irpet_n','DT_S_ww')) -> tot_xx_S

left_join(num_S,tot_xx_S, by=c('irpet_n','DT_S_ww')) %>% 
    select(-value) %>% 
    reshape2::dcast(irpet_n ~ DT_S_ww) -> num_S

num_S[is.na(num_S)] <- 0

# M
# add a plus one to the residual to assign it to the next week
res_xx_M %>% 
    mutate(
        DT_M_ww=case_when(
            DT_M_ww==0 ~ 0,
            TRUE ~ DT_M_ww + 1
        )
    ) -> res_xx_M



num_M <- data.frame(matrix(0, nrow=nsect, ncol=(1+(nrow(weeks_M)+1))))
colnames(num_M)<-c(
    "irpet_n",
    as.numeric(as.character(weeks_M$Var1)),
    max(as.numeric(as.character(weeks_M$Var1)))+1
    )
num_M[,1]<-c(1:nsect)
num_M <- reshape2::melt(num_M,id.vars="irpet_n",variable.name="DT_M_ww",factorsAsStrings=TRUE) 

num_M$DT_M_ww <- num_M$DT_M_ww %>% as.character() %>% as.numeric()


bind_rows(xx_M,res_xx_M %>% rename("sizeBI_M_ww"="res_sizeBI_M_ww")) %>% 
    summarise(tot_sizeBI_M_ww = sum(sizeBI_M_ww), .by = c('irpet_n','DT_M_ww')) -> tot_xx_M

left_join(num_M,tot_xx_M, by=c('irpet_n','DT_M_ww')) %>% 
    select(-value) %>% 
    reshape2::dcast(irpet_n ~ DT_M_ww) -> num_M

num_M[is.na(num_M)] <- 0

# I
# add a plus one to the residual to assign it to the next week
res_xx_I %>% 
    mutate(
        DT_I_ww=case_when(
            DT_I_ww==0 ~ 0,
            TRUE ~ DT_I_ww + 1
        )
    ) -> res_xx_I



num_I <- data.frame(matrix(0, nrow=nsect, ncol=(1+(nrow(weeks_I)+1))))
colnames(num_I)<-c(
    "irpet_n",
    as.numeric(as.character(weeks_I$Var1)),
    max(as.numeric(as.character(weeks_I$Var1)))+1
    )
num_I[,1]<-c(1:nsect)
num_I <- reshape2::melt(num_I,id.vars="irpet_n",variable.name="DT_I_ww",factorsAsStrings=TRUE) 

num_I$DT_I_ww <- num_I$DT_I_ww %>% as.character() %>% as.numeric()


bind_rows(xx_I,res_xx_I %>% rename("sizeBI_I_ww"="res_sizeBI_I_ww")) %>% 
    summarise(tot_sizeBI_I_ww = sum(sizeBI_I_ww), .by = c('irpet_n','DT_I_ww')) -> tot_xx_I

left_join(num_I,tot_xx_I, by=c('irpet_n','DT_I_ww')) %>% 
    select(-value) %>% 
    reshape2::dcast(irpet_n ~ DT_I_ww) -> num_I

num_I[is.na(num_I)] <- 0

# saveRDS(all_hit,"/mnt/beegfs/lcesarini/2025_p_irio/test/all_hit_luigi.rds")
# saveRDS(xx_S,"/mnt/beegfs/lcesarini/2025_p_irio/test/xx_S_luigi.rds")
# saveRDS(res_xx_S,"/mnt/beegfs/lcesarini/2025_p_irio/test/res_xx_S_luigi.rds")
# saveRDS(num_S,"/mnt/beegfs/lcesarini/2025_p_irio/test/num_S_luigi.rds")

# find all the ul by sector in the region and join

empl_totSect <- DTB %>% summarise(tot_addetti=sum(addetti_ul),.by=irpet_n)

denom <- left_join(data.frame(irpet_n=1:43),empl_totSect,by='irpet_n')  %>% mutate(tot_addetti=tidyr::replace_na(tot_addetti,0))


# Cumulative sum from week 5 to final weeks

#make BI in percentage for S
perc_BI <- num_S
for (i in 1:nsect) {
   perc_BI[i,2:24] <- rev(cumsum(as.vector(num_S[i,24:2]))) / denom[i,2]
}

output_shock_S <- data.frame(matrix(NA, nrow=43, ncol=101))
colnames(output_shock_S) <- c("irpet_n",1:100)

output_shock_S[,match(colnames(perc_BI),colnames(output_shock_S))] <- perc_BI


col_data <- which(colnames(output_shock_S) %in% colnames(perc_BI))[-1]
col_NA <- which(colnames(output_shock_S) %!in% colnames(perc_BI))
#a tutte le settimane dopo l'ultima metto 0
for (i in col_NA) {
    if (all((col_data-1) < 0)) {
        output_shock_S[,i] <- 0
    }else{
        diff_idx <- abs(col_data-i)
        output_shock_S[,i] <- output_shock_S[,col_data[which.min(diff_idx)]]
    }
}

# add zeros to the weeks after the last week of addetti
output_shock_S[is.na(output_shock_S)]<-0
output_shock_S[,(max(col_data)+1):dim(output_shock_S)[2]]<-0

#make BI in percentage for M
perc_BI <- num_M
for (i in 1:nsect) {
   perc_BI[i,2:24] <- rev(cumsum(as.vector(num_M[i,24:2]))) / denom[i,2]
}

output_shock_M <- data.frame(matrix(NA, nrow=43, ncol=101))
colnames(output_shock_M) <- c("irpet_n",1:100)

output_shock_M[,match(colnames(perc_BI),colnames(output_shock_M))] <- perc_BI


col_data <- which(colnames(output_shock_M) %in% colnames(perc_BI))[-1]
col_NA <- which(colnames(output_shock_M) %!in% colnames(perc_BI))
#a tutte le settimane dopo l'ultima metto 0
for (i in col_NA) {
    if (all((col_data-1) < 0)) {
        output_shock_M[,i] <- 0
    }else{
        diff_idx <- abs(col_data-i)
        output_shock_M[,i] <- output_shock_M[,col_data[which.min(diff_idx)]]
    }
}

# add zeros to the weeks after the last week of addetti
output_shock_M[is.na(output_shock_M)]<-0
output_shock_M[,(max(col_data)+1):dim(output_shock_M)[2]]<-0


#make BI in percentage for I
perc_BI <- num_I
for (i in 1:nsect) {
   perc_BI[i,2:24] <- rev(cumsum(as.vector(num_I[i,24:2]))) / denom[i,2]
}

output_shock_I <- data.frame(matrix(NA, nrow=43, ncol=101))
colnames(output_shock_I) <- c("irpet_n",1:100)

output_shock_I[,match(colnames(perc_BI),colnames(output_shock_I))] <- perc_BI


col_data <- which(colnames(output_shock_I) %in% colnames(perc_BI))[-1]
col_NA <- which(colnames(output_shock_I) %!in% colnames(perc_BI))
#a tutte le settimane dopo l'ultima metto 0
for (i in col_NA) {
    if (all((col_data-1) < 0)) {
        output_shock_I[,i] <- 0
    }else{
        diff_idx <- abs(col_data-i)
        output_shock_I[,i] <- output_shock_I[,col_data[which.min(diff_idx)]]
    }
}

add zeros to the weeks after the last week of addetti
output_shock_I[is.na(output_shock_I)]<-0
output_shock_I[,(max(col_data)+1):dim(output_shock_I)[2]]<-0

write.csv(output_shock_S,file='out/shocks/claims/output_shocks_EROMgeoloc_S.csv',row.names=FALSE)
write.csv(output_shock_M,file='out/shocks/claims/output_shocks_EROMgeoloc_M.csv',row.names=FALSE)
write.csv(output_shock_I,file='out/shocks/claims/output_shocks_EROMgeoloc_I.csv',row.names=FALSE)


################################################################################################
################################################################################################
###############  STOCK INVENTORIES   ###########################################################
###################(Loss Ratio)#################################################################
################################################################################################

resDays_S_ww = round(all_hit$resDT_S_ww*Days_ww)  #these are days of the last week of shock (<=7)
resDays_M_ww = round(all_hit$resDT_M_ww*Days_ww)  #these are days of the last week of shock (<=7)
resDays_I_ww = round(all_hit$resDT_I_ww*Days_ww)  #these are days of the last week of shock (<=7)

#METHOD 1
#PER IL MOMENTO MANCA IL METODO 1 nel drive. In caso si sistema.
# # FOR I
# all_hit$LR_I_ww<-all_hit$LR_I*Days_ww/Days_ww
# all_hit$resLR_I_ww<-all_hit$LR_I*resDays_I_ww/Days_ww

# # step 2: associate every ul LR to the corresponding n. of employees
# all_hit$sizeLR_I_ww <- all_hit$LR_I_ww * all_hit$addetti_ul 
# all_hit$res_sizeLR_I_ww <- all_hit$resLR_I_ww * all_hit$addetti_ul

# all_hit %>% 
#     summarise(AvLossRatio=sum(sizeLR_I_ww),.by=c(irpet_n,DT_I_ww)) %>% 
#     data.frame() -> num_I





#METHOD 2
# For S
## For the week of the shock event, calculate the weighted average of LR by sector (weights = n.employees)

# associate every ul LR to the corresponding n. of employees
all_hit$sizeLR_S <- all_hit$LR_S * all_hit$addetti_ul 

# for every sector create numerators for the weighted averages:
all_hit %>% 
    summarise(AvLossRatio=sum(sizeLR_S),.by=irpet_n) %>% 
    data.frame() -> num_S

joined_S <- left_join(denom,num_S,by="irpet_n")

LR_shock_EROMgeoloc_S <- matrix(NA, nrow=43, ncol=101)
LR_shock_EROMgeoloc_S[,1]<-c(1:43)
#colnames(output_shock_EROMgeoloc_S) <- names
LR_shock_EROMgeoloc_S[,2]<-as.matrix(joined_S$AvLossRatio/joined_S$tot_addetti) 
LR_shock_EROMgeoloc_S[is.na(LR_shock_EROMgeoloc_S)]<-0  

## save loss ratio (LR) shock
write.csv(LR_shock_EROMgeoloc_S,file='out/shocks/claims/LR_shocks_EROMgeoloc_S_met2.csv')

# For M
## For the week of the shock event, calculate the weighted average of LR by sector (weights = n.employees)

# associate every ul LR to the corresponding n. of employees
all_hit$sizeLR_M <- all_hit$LR_M * all_hit$addetti_ul 

# for every sector create numerators for the weighted averages:
all_hit %>% 
    summarise(AvLossRatio=sum(sizeLR_M),.by=irpet_n) %>% 
    data.frame() -> num_M

joined_M <- left_join(denom,num_M,by="irpet_n")

LR_shock_EROMgeoloc_M <- matrix(NA, nrow=43, ncol=101)
LR_shock_EROMgeoloc_M[,1]<-c(1:43)
#colnames(output_shock_EROMgeoloc_M) <- names
LR_shock_EROMgeoloc_M[,2]<-as.matrix(joined_M$AvLossRatio/joined_M$tot_addetti) 
LR_shock_EROMgeoloc_M[is.na(LR_shock_EROMgeoloc_M)]<-0  

## save loss ratio (LR) shock
write.csv(LR_shock_EROMgeoloc_M,file='out/shocks/claims/LR_shocks_EROMgeoloc_M_met2.csv')

## For the week of the shock event, calculate the weighted average of LR by sector (weights = n.employees)

# step 1
barLR_I <- matrix(NA,nrow = nsect, ncol = 1)
rownames(barLR_I) <- c(1:nsect)
barLR_I <- as.data.frame(barLR_I)
barLR_I$irpet_n<-c(1:nsect)

# step 2: associate every ul LR to the corresponding n. of employees
all_hit$sizeLR_I <- all_hit$LR_I * all_hit$addetti_ul 

# for every sector create numerators for the weighted averages:
all_hit %>% 
    summarise(AvLossRatio=sum(sizeLR_I),.by=irpet_n) %>% 
    data.frame() -> num_I

joined_I <- left_join(denom,num_I,by="irpet_n")

LR_shock_EROMgeoloc_I <- matrix(NA, nrow=43, ncol=101)
LR_shock_EROMgeoloc_I[,1]<-c(1:43)
#colnames(output_shock_EROMgeoloc_I) <- names
LR_shock_EROMgeoloc_I[,2]<-as.matrix(joined_I$AvLossRatio/joined_I$tot_addetti) 
LR_shock_EROMgeoloc_I[is.na(LR_shock_EROMgeoloc_I)]<-0  

## save loss ratio (LR) shock
write.csv(LR_shock_EROMgeoloc_I,file='out/shocks/claims/LR_shocks_EROMgeoloc_I_met2.csv')

