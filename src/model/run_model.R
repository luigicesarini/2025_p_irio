library(matrixStats)
library(readxl)
library(readr)

TradLeontief=0  #0=modified Leontief 
#TradLeontief=1  #1=traditional Leontief

#read the file containing all the confugurations tested in the estimation
Sample<-as.data.frame(read.csv("resources/Sample_FullParamSpace_REV1_DiffCons.csv",header=TRUE),header=FALSE)
#select the winning cobination identified by the estimation for the two model specifications
if(TradLeontief==1){
        params=as.numeric(Sample[12722,]) #winning parameter combination for trad leontief 
      }else{
        params=as.numeric(Sample[16725,]) #winning parameter combination for modified Leontief
      }

#periods to simulate
rounds=108

source("EMILIA_ModelFinal.R")

#import the IO table
IO <- readRDS("resources/IO_ITAregions.rds")

measures <- readRDS("resources/measures_spese_flood.rds")
#import the H matrix
coefficienti_consumo <- readRDS("resources/H.rds")
#import the epsilon for the modified Leontief model
epsilon<-readRDS("resources/matrix_gamma_flood_43sect.rds")

##Labor shocks using Hazus restoration times:
LD_Emilia_hazus <- read_csv("resources/lab_shocks_EROMgeoloc.csv")

##Labor shocks using insurance claims' business interruption data related to machinery damages:
LD_Emilia_ins_M <- read_csv("resources/output_shocks_EROMgeoloc_M.csv")
##Shocks to inventories using insurance claims' loss ratio:
LossRatio_Emilia_I <- read_csv("resources/LR_shocks_EROMgeoloc_I_met2.csv")


# choose scenario to run (1=hazus downtime periods; 2=insurance claims)
scenario=1
nodemand=0

result<-runModel(IO,measures,coefficienti_consumo,params,rounds,LD_Emilia_hazus,LD_Emilia_ins_M,LossRatio_Emilia_I,scenario,epsilon)