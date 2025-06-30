suppressWarnings(suppressPackageStartupMessages(library(glue)))
suppressWarnings(suppressPackageStartupMessages(library(dplyr)))
suppressWarnings(suppressPackageStartupMessages(library(readr)))
suppressWarnings(suppressPackageStartupMessages(library(readxl)))
suppressWarnings(suppressPackageStartupMessages(library(this.path)))
suppressWarnings(suppressPackageStartupMessages(library(matrixStats)))

# Function to go up N directories
go_up_n_dirs <- function(path = this.path(), n = 1) {
  for (i in seq_len(n)) {
    path <- dirname(path)
  }
  return(path)
}

args=commandArgs(trailingOnly=TRUE)
# args1 is year args 2 is id of the event

setwd(go_up_n_dirs(n = 3))

source("src/model/model_final.R")

TradLeontief=0  #0=modified Leontief 
#TradLeontief=1  #1=traditional Leontief

#####################################################
# START THE IMPORT OF ALL THE FILE NECESSARY TO RUN THE MODEL
#####################################################

#read the file containing all the confugurations tested in the estimation
Sample<-as.data.frame(read.csv("res/model/Sample_FullParamSpace_REV1_DiffCons.csv",header=TRUE),header=FALSE)
#select the winning cobination identified by the estimation for the two model specifications
if(TradLeontief==0){
        params=as.numeric(Sample[12722,]) #winning parameter combination for trad leontief 
      }else{
        params=as.numeric(Sample[16725,]) #winning parameter combination for modified Leontief
      }

#periods to simulate
rounds=108

#import the IO table
IO <- readRDS("res/model/IO_ITAregions.rds")

#NOT EXACLTY SURE HOW TO CALL THEM
measures <- readRDS("res/model/measures_spese_flood.rds")
#import the H matrix
coefficienti_consumo <- readRDS("res/model/H.rds")
#import the epsilon for the modified Leontief model
epsilon<-readRDS("res/model/matrix_gamma_flood_43sect.rds")

#Load the file to match name of the regions with their own acronym
sigle_reg <- read.csv("res/correspondance_regions.csv")

##Labor shocks using Hazus restoration times:
# The procedure is generalized for events containing multiple regions:
# 1. For a specific  ID event, find the shocks related to each region
# 2. Extract the name of the regions involved and get the corresponding acronym




#Find all the regions linked to an eventID
ID <- args[1]
YEAR <- args[2]
file_shock_reg <- list.files(glue('out/shocks/hit/{YEAR}/'),glue('{ID}'),full.names=TRUE)[c(1:10,12:13)]
file_shock_reg <- list.files(glue('out/shocks/hit/{YEAR}/'),glue('{ID}'),full.names=TRUE)[11]

# ID <- 'EROM'
# file_shock_reg <- list.files('out/shocks/hit/',glue('{ID}'),full.names=TRUE)

lapply(file_shock_reg,function(g){
  bn <- basename(g)
  strsplit(bn,'\\.') %>% .[[1]] %>% .[1] %>% strsplit('_') %>% .[[1]] %>% .[length(.)]
}) %>% unlist() -> name_regions


if (sum(is.na(match(name_regions,sigle_reg$Nome))) > 0) {
   print('There are some errors in the correspondance_regions.csv file. Most likely grammar.')
}

sigle_event <- sigle_reg$Sigla[match(name_regions,sigle_reg$Nome)]

#shocks for all sectors:
#initializethe matrix with labor reduction with dimension [(43*20), rounds]
LD<-matrix(0,nrow=(measures$nSectors*(measures$nItalianRegions-1)), ncol=rounds+1)
#initialize the vector that will contain all the indices of the regions involved
vector_idx <- c()

# Then we start a loop that creates a matrix of shock for each region
# and fills the LD matrix
for(i in seq_len(length(file_shock_reg))){
  shocks<-matrix(0,nrow = measures$nSectors, ncol = rounds+1)
  shocks[,1]<-c(1:43)
  #labor reduction for the specific region
  LD_reg <- read_csv(file_shock_reg[i]) %>% suppressMessages()

  #find the indices for the matrix
  id_start_region <- which(grepl(sigle_event[i],rownames(IO))) %>% min()
  id_end_region <- which(grepl(sigle_event[i],rownames(IO))) %>% max()

  vector_idx  <- c(vector_idx,id_start_region:id_end_region)

  #Create the labor reduction for the region and put 0 where there are NA
  if (ID!="EROM") {
    LD_reg %>% 
      select(-addetti_ul) %>% 
      add_row(n_sector=2,.before=1) %>% 
      add_row(n_sector=1,.before=1) -> LD_reg

    LD_reg[is.na(LD_reg)]  <- 0
     
    shocks[,2:101]<-as.matrix(LD_reg[,2:101])

    # Substitute the regional labor reduction to the big matrix with all labor reduction
    #find the indices for the matrix
    LD[id_start_region:id_end_region,2:(rounds+1)]<-as.matrix(shocks[,-1])
  
  }else{
    shocks[,2:101]<-as.matrix(LD_reg[,3:102])
    LD[id_start_region:id_end_region,2:(rounds+1)]<-as.matrix(shocks[,-1])  
  }


}

LD<-as.data.frame(LD)

##Labor shocks using insurance claims' business interruption data related to machinery damages:
LD_Emilia_ins_M <- read_csv("res/model/output_shocks_EROMgeoloc_M.csv") %>% suppressMessages()
##Shocks to inventories using insurance claims' loss ratio:
LossRatio_Emilia_I <- read_csv("res/model/LR_shocks_EROMgeoloc_I_met2.csv") %>% suppressMessages()


# choose scenario to run (1=hazus downtime periods; 2=insurance claims)
scenario=1
nodemand=0
# print(length(file_shock_reg))
result<-runModel(
  IO,measures,coefficienti_consumo,params,rounds,
  LD,LD_Emilia_ins_M,
  LossRatio_Emilia_I,scenario,epsilon,
  vector_idx,length(file_shock_reg)
)

saveRDS(result, glue("test/Output_{ID}_{basename(file_shock_reg[1]) %>% strsplit('_') %>% unlist()  %>% .[7]}.rds"))


# which(grepl('EXT',rownames(IO))) %>% range()
# which(grepl('ERO',rownames(IO))) %>% range()


# IO[861:903,] %>% mean()
# IO[302:344,] %>% mean()

# IO[vector_idx,] %>% dim()

# colnames(LD)
