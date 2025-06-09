library(matrixStats)
library(readxl)
library(readr)
library(dplyr)
library(glue)

setwd("/mnt/beegfs/lcesarini/2025_p_irio/")

TradLeontief=0  #0=modified Leontief 
#TradLeontief=1  #1=traditional Leontief

#read the file containing all the confugurations tested in the estimation
Sample<-as.data.frame(read.csv("res/model/Sample_FullParamSpace_REV1_DiffCons.csv",header=TRUE),header=FALSE)
#select the winning cobination identified by the estimation for the two model specifications
if(TradLeontief==1){
        params=as.numeric(Sample[12722,]) #winning parameter combination for trad leontief 
      }else{
        params=as.numeric(Sample[16725,]) #winning parameter combination for modified Leontief
      }

#periods to simulate
rounds=108

source("src/model/model_final.R")

#import the IO table
IO <- readRDS("res/model/IO_ITAregions.rds")

sigla_region <- 'TOS'
id_start_region <- which(grepl(sigla_region,rownames(IO))) %>% min()
id_end_region <- which(grepl(sigla_region,rownames(IO))) %>% max()

# paste(id_start_region,id_end_region) %>% print()

measures <- readRDS("res/model/measures_spese_flood.rds")
#import the H matrix
coefficienti_consumo <- readRDS("res/model/H.rds")
#import the epsilon for the modified Leontief model
epsilon<-readRDS("res/model/matrix_gamma_flood_43sect.rds")

##Labor shocks using Hazus restoration times:
# LD_Emilia_hazus <- read_csv("res/model/lab_shocks_EROMgeoloc.csv")
# LD_Emilia_hazus %>% View()
# head(LD_Emilia_hazus)
# dim(LD_Emilia_hazus)
LD_Emilia_hazus2 <- read_csv("out/shocks/hit/2017/tot_addetti_by_ateco_perc_40863_2017_Toscana.csv") %>% suppressMessages()

LD_Emilia_hazus2 %>% 
  select(-addetti_ul) %>% 
  add_row(n_sector=2,.before=1) %>% 
  add_row(n_sector=1,.before=1) -> LD_Emilia_hazus2

LD_Emilia_hazus2[is.na(LD_Emilia_hazus2)]  <- 0

##Labor shocks using insurance claims' business interruption data related to machinery damages:
LD_Emilia_ins_M <- read_csv("res/model/output_shocks_EROMgeoloc_M.csv") %>% suppressMessages()
##Shocks to inventories using insurance claims' loss ratio:
LossRatio_Emilia_I <- read_csv("res/model/LR_shocks_EROMgeoloc_I_met2.csv") %>% suppressMessages()


# choose scenario to run (1=hazus downtime periods; 2=insurance claims)
scenario=1
nodemand=0

result<-runModel(
  IO,measures,coefficienti_consumo,params,rounds,
  LD_Emilia_hazus2,LD_Emilia_ins_M,
  LossRatio_Emilia_I,scenario,epsilon,
  id_start_region,id_end_region)

saveRDS(result, glue("test/Output_{sigla_region}_2025.rds"))

