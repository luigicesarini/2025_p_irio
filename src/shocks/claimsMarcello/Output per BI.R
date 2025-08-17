# =========================================================================
# STIMA BUSINESS INTERUPTION ATTIVITà ECONOMICHE DA ALLUVIONE

# Created by: MARCELLO 
# Created: 2023-10-16
#
# Last revised by: ...
# Last revised: ...
# =========================================================================

###LIBRERIE
library(sf)
library(zoo)
library(readxl) 
library(ggplot2)
library(tidyverse)
library(furniture)
library(tidyverse)

"%!in%" <- Negate("%in%")
setwd("/mnt/beegfs/lcesarini/2025_p_irio")

source("src/shocks/functions_R.R")

corr_sect <- jsonlite::fromJSON("res/correspondance_sector.json")

###CARICO Database 
#Evento in Emilia Romagna 2023 (aziende e altezza acqua)
# iniziamo da 3 eventi
# Toscana: 2017 - 40863
# Veneto
# Piemonte Liguria: 2014 - 40829
# DTB=emilia_geocoded_flooded_step2

path_evento="out/vector/2017/EVENT_40863_2017_Toscana_River_ul.gpkg"
entire_region=sf::st_read("../2024_IRIO_EQ/res/Toscana_geocoded.gpkg")
DTB=sf::st_read(path_evento)
dim(DTB)
left_join(
  DTB,
  corr_sect %>% rename("Sectors"="name_ita") %>% select(Sectors,ING_sector),
  by="Sectors"
)  %>% 
filter(ING_sector!="na")-> DTB
DTB=subset(DTB, !is.na(ING_sector))
dim(DTB)

###CARICO MATRICI VULNERABILITà BI
multiplesheets <- function(fname) { 
  
  # getting info about all excel sheets 
  sheets <- readxl::excel_sheets(fname) 
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x)) 
  data_frame <- lapply(tibble, as.data.frame) 
  
  # assigning names to data frames 
  names(data_frame) <- sheets 
  
  # print data frame 
  print(data_frame) 
} 
path_LR <- "src/shocks/claimsMarcello/01_FLOOD - LossRatio - ATECO.xlsx"
path_DT <- "src/shocks/claimsMarcello/02_FLOOD - DownTime - ATECO.xlsx"
path_GP <- "src/shocks/claimsMarcello/03_FLOOD - Reduction GrossProduct - ATECO.xlsx"
path_BI <- "src/shocks/claimsMarcello/04_FLOOD - Business interuption - ATECO.xlsx"
LR=multiplesheets(path_LR) # Loss Ratio
DT=multiplesheets(path_DT) # Downtime
GP=multiplesheets(path_GP) # Variazione of Gross Production
BI=multiplesheets(path_BI) # Business Interuption

## INPUT ALTEZZE E SETTORE
# Esempio di vettore di altezze e vettore di asset 
# altezze <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)  
# asset_da_cercare <- c("COM_PD",	"COM_GD",	"COM_IN",	"Media2",	"COM_OF",	
#                       "COM_RS",	"IND_HV","IND_LG",	"Media1",	"IND_MG")  

#Da simulazione evento Emilia Romagna 2023
# altezze <- DTB$wh_m #water depth in metri
altezze <- DTB$WD #water depth in metri
altezze[is.na(altezze)] <- 0
asset_da_cercare <- DTB$ING_sector #settori economici come da nomenclatura di Mario
#asset_da_cercare[is.na(asset_da_cercare)] <- "IND_CS" #sostituisco gli NA

### CALCOLO LR USANDO ALTEZZE E ASSET
# Inizializza un vettore "appoggio" per memorizzare i valori di LR interpolati
valori_LR_interpolati <- numeric(length(altezze))

#STRUCTURE (ES. i=298875)
for (i in 1:length(altezze)) {
  altezza_causale <- altezze[i]
  asset_corrente <- asset_da_cercare[i]
  
  # Verifica se l'altezza in input è presente nel dataframe
  if (altezza_causale %in% LR$Building$WaterDepth) {
    # Se l'altezza è presente nel dataframe, ottieni il valore di LR direttamente per l'asset specifico
    valore_LR_interpolato <- LR$Building[LR$Building$WaterDepth == altezza_causale, asset_corrente]
  } else {
    # Se l'altezza non è presente nel dataframe, esegui l'interpolazione per l'asset specifico utilizzando na.approx
    valore_LR_interpolato <- na.approx(LR$Building[[asset_corrente]], x = LR$Building$WaterDepth, xout = altezza_causale)
  }
  
  # Memorizza il valore di LR interpolato nel vettore risultante
  valori_LR_interpolati[i] <- valore_LR_interpolato
}
DTB$LR_S <-valori_LR_interpolati

#MACHINERY (ES. i=298875)
for (i in 1:length(altezze)) {
  altezza_causale <- altezze[i]
  asset_corrente <- asset_da_cercare[i]
  
  # Verifica se l'altezza in input è presente nel dataframe
  if (altezza_causale %in% LR$Machinery$WaterDepth) {
    # Se l'altezza è presente nel dataframe, ottieni il valore di LR direttamente per l'asset specifico
    valore_LR_interpolato <- LR$Machinery[LR$Machinery$WaterDepth == altezza_causale, asset_corrente]
  } else {
    # Se l'altezza non è presente nel dataframe, esegui l'interpolazione per l'asset specifico utilizzando na.approx
    valore_LR_interpolato <- na.approx(LR$Machinery[[asset_corrente]], x = LR$Machinery$WaterDepth, xout = altezza_causale)
  }
  
  # Memorizza il valore di LR interpolato nel vettore risultante
  valori_LR_interpolati[i] <- valore_LR_interpolato
}
DTB$LR_M <-valori_LR_interpolati

#INVENTORY (ES. i=298875)
for (i in 1:length(altezze)) {
  altezza_causale <- altezze[i]
  asset_corrente <- asset_da_cercare[i]
  
  # Verifica se l'altezza in input è presente nel dataframe
  if (altezza_causale %in% LR$Inventory$WaterDepth) {
    # Se l'altezza è presente nel dataframe, ottieni il valore di LR direttamente per l'asset specifico
    valore_LR_interpolato <- LR$Inventory[LR$Inventory$WaterDepth == altezza_causale, asset_corrente]
  } else {
    # Se l'altezza non è presente nel dataframe, esegui l'interpolazione per l'asset specifico utilizzando na.approx
    valore_LR_interpolato <- na.approx(LR$Inventory[[asset_corrente]], x = LR$Inventory$WaterDepth, xout = altezza_causale)
  }
  
  # Memorizza il valore di LR interpolato nel vettore risultante
  valori_LR_interpolati[i] <- valore_LR_interpolato
}
DTB$LR_I <-valori_LR_interpolati


####### 
#PLOT delle distribuzioni di LR
#variabili di appoggio per i plot
app1 <- DTB[DTB$LR_S > 0, ]
app2 <- DTB[DTB$LR_M > 0, ]
app3 <- DTB[DTB$LR_I > 0, ]
app4 <- DTB[DTB$WD > 0, ]

# ggplot() +
#   geom_density(data = app1, aes(x = LR_S, fill = "LR_S"), alpha = 0.5) +
#   geom_density(data = app2, aes(x = LR_M, fill = "LR_M"), alpha = 0.5) +
#   geom_density(data = app3, aes(x = LR_M, fill = "LR_I"), alpha = 0.5) +
#   #geom_density(data = app4, aes(x = wh_m, fill = "H water"), alpha = 0.5) +
  
#   labs(x = "LR", y = "Densità", title = "Sovrapposizione delle Distribuzioni delle varie componenti Loss Ration") +
#   scale_fill_manual(values = c("LR_S" = "blue", "LR_M" = "red", "LR_I"="green")) +
#   theme_minimal()



### CALCOLO DOWNTIME USANDO ALTEZZE E ASSET
# Inizializza un vettore "appoggio" per memorizzare i valori di LR interpolati
valori_DT_interpolati <- numeric(length(altezze))

#STRUCTURE (ES. i=298875)
for (i in 1:length(altezze)) {
  LR_S_causale <- DTB$LR_S[i]
  asset_corrente <- DTB$ING_sector[i]
  
  # Verifica se l'altezza in input è presente nel dataframe
  if (LR_S_causale %in% DT$Building$LR) {
    # Se l'altezza è presente nel dataframe, ottieni il valore di LR direttamente per l'asset specifico
    valore_DT_interpolato <- DT$Building[DT$Building$LR == LR_S_causale, asset_corrente]
  } else {
    # Se l'altezza non è presente nel dataframe, esegui l'interpolazione per l'asset specifico utilizzando approx
    dati_interpolati <- approx(DT$Building$LR, DT$Building[[asset_corrente]], xout = LR_S_causale)
    valore_DT_interpolato <- dati_interpolati$y
  }
  
  # Memorizza il valore di LR interpolato nel vettore risultante
  valori_DT_interpolati[i] <- valore_DT_interpolato
}
DTB$DT_S <-valori_DT_interpolati

#MACHINERY
for (i in 1:length(altezze)) {
  LR_M_causale <- DTB$LR_M[i]
  asset_corrente <- DTB$ING_sector[i]
  
  # Verifica se l'altezza in input è presente nel dataframe
  if (LR_M_causale %in% DT$Building$LR) {
    # Se l'altezza è presente nel dataframe, ottieni il valore di LR direttamente per l'asset specifico
    valore_DT_interpolato <- DT$Building[DT$Building$LR == LR_M_causale, asset_corrente]
  } else {
    # Se l'altezza non è presente nel dataframe, esegui l'interpolazione per l'asset specifico utilizzando approx
    dati_interpolati <- approx(DT$Building$LR, DT$Building[[asset_corrente]], xout = LR_M_causale)
    valore_DT_interpolato <- dati_interpolati$y
  }
  
  # Memorizza il valore di LR interpolato nel vettore risultante
  valori_DT_interpolati[i] <- valore_DT_interpolato
}
DTB$DT_M <-valori_DT_interpolati

#INVENTORY
for (i in 1:length(altezze)) {
  LR_I_causale <- DTB$LR_I[i]
  asset_corrente <- DTB$ING_sector[i]
  
  # Verifica se l'altezza in input è presente nel dataframe
  if (LR_I_causale %in% DT$Building$LR) {
    # Se l'altezza è presente nel dataframe, ottieni il valore di LR direttamente per l'asset specifico
    valore_DT_interpolato <- DT$Building[DT$Building$LR == LR_I_causale, asset_corrente]
  } else {
    # Se l'altezza non è presente nel dataframe, esegui l'interpolazione per l'asset specifico utilizzando approx
    dati_interpolati <- approx(DT$Building$LR, DT$Building[[asset_corrente]], xout = LR_I_causale)
    valore_DT_interpolato <- dati_interpolati$y
  }
  
  # Memorizza il valore di LR interpolato nel vettore risultante
  valori_DT_interpolati[i] <- valore_DT_interpolato
}
DTB$DT_I <-valori_DT_interpolati

# ggplot() +
#   geom_density(data = DTB, aes(x = DT_S, fill = "DT_S"), alpha = 0.5) +
#   geom_density(data = DTB, aes(x = DT_M, fill = "DT_M"), alpha = 0.5) +
#   geom_density(data = DTB, aes(x = DT_I, fill = "DT_I"), alpha = 0.5) +
#   labs(x = "DOWNTIME (days)", y = "Densità", title = "Sovrapposizione delle Distribuzioni delle varie componenti DOWNTIME") +
#   scale_fill_manual(values = c("DT_S" = "blue", "DT_M" = "red", "DT_I"="green")) +
#   theme_minimal()


### CALCOLO GROSS PRODUCT VARIATION USANDO ALTEZZE E ASSET
# Inizializza un vettore "appoggio" per memorizzare i valori di LR interpolati
valori_GP_interpolati <- numeric(length(altezze))

#STRUCTURE (ES. i=298875)
for (i in 1:length(altezze)) {
  LR_S_causale <- DTB$LR_S[i]
  asset_corrente <- DTB$ING_sector[i]
  
  # Verifica se l'altezza in input è presente nel dataframe
  if (LR_S_causale %in% GP$Building$LR) {
    # Se l'altezza è presente nel dataframe, ottieni il valore di LR direttamente per l'asset specifico
    valore_GP_interpolato <- GP$Building[GP$Building$LR == LR_S_causale, asset_corrente]
  } else {
    # Se l'altezza non è presente nel dataframe, esegui l'interpolazione per l'asset specifico utilizzando approx
    dati_interpolati <- approx(GP$Building$LR, GP$Building[[asset_corrente]], xout = LR_S_causale)
    valore_GP_interpolato <- dati_interpolati$y
  }
  
  # Memorizza il valore di LR interpolato nel vettore risultante
  valori_GP_interpolati[i] <- valore_GP_interpolato
}
DTB$GP_S <-valori_GP_interpolati

#MACHINERY
for (i in 1:length(altezze)) {
  LR_M_causale <- DTB$LR_M[i]
  asset_corrente <- DTB$ING_sector[i]
  
  # Verifica se l'altezza in input è presente nel dataframe
  if (LR_M_causale %in% GP$Building$LR) {
    # Se l'altezza è presente nel dataframe, ottieni il valore di LR direttamente per l'asset specifico
    valore_GP_interpolato <- GP$Building[GP$Building$LR == LR_M_causale, asset_corrente]
  } else {
    # Se l'altezza non è presente nel dataframe, esegui l'interpolazione per l'asset specifico utilizzando approx
    dati_interpolati <- approx(GP$Building$LR, GP$Building[[asset_corrente]], xout = LR_M_causale)
    valore_GP_interpolato <- dati_interpolati$y
  }
  
  # Memorizza il valore di LR interpolato nel vettore risultante
  valori_GP_interpolati[i] <- valore_GP_interpolato
}
DTB$GP_M <-valori_GP_interpolati

#INVENTORY
for (i in 1:length(altezze)) {
  LR_I_causale <- DTB$LR_I[i]
  asset_corrente <- DTB$ING_sector[i]
  
  # Verifica se l'altezza in input è presente nel dataframe
  if (LR_I_causale %in% GP$Building$LR) {
    # Se l'altezza è presente nel dataframe, ottieni il valore di LR direttamente per l'asset specifico
    valore_GP_interpolato <- GP$Building[GP$Building$LR == LR_I_causale, asset_corrente]
  } else {
    # Se l'altezza non è presente nel dataframe, esegui l'interpolazione per l'asset specifico utilizzando approx
    dati_interpolati <- approx(GP$Building$LR, GP$Building[[asset_corrente]], xout = LR_I_causale)
    valore_GP_interpolato <- dati_interpolati$y
  }
  
  # Memorizza il valore di LR interpolato nel vettore risultante
  valori_GP_interpolati[i] <- valore_GP_interpolato
}
DTB$GP_I <-valori_GP_interpolati

# ggplot() +
#   geom_density(data = DTB, aes(x = GP_S, fill = "GP_S"), alpha = 0.5) +
#   geom_density(data = DTB, aes(x = GP_M, fill = "GP_M"), alpha = 0.5) +
#   geom_density(data = DTB, aes(x = GP_I, fill = "GP_I"), alpha = 0.5) +
#   labs(x = "GROSS PRODCUT (ratio)", y = "Densità", title = "Sovrapposizione delle Distribuzioni delle varie componenti GROSS PRODUCT VARIATION") +
#   scale_fill_manual(values = c("GP_S" = "blue", "GP_M" = "red", "GP_I"="green")) +
#   theme_minimal()


### BUSINESS INTERUPTION
#Structure
DTB$BI_S_ww <- DTB$GP_S*DTB$DT_S/7
DTB$BI_S_yy <- DTB$GP_S*DTB$DT_S/360

#Machinery
DTB$BI_M_ww <- DTB$GP_M*DTB$DT_M/7
DTB$BI_M_yy <- DTB$GP_M*DTB$DT_M/360

#Inventory
DTB$BI_I_ww <- DTB$GP_I*DTB$DT_I/7
DTB$BI_I_yy <- DTB$GP_I*DTB$DT_I/360

# ggplot() +
#   geom_density(data = DTB, aes(x = BI_S_ww, fill = "BI_S_ww"), alpha = 0.5) +
#   geom_density(data = DTB, aes(x = BI_M_ww, fill = "BI_M_ww"), alpha = 0.5) +
#   geom_density(data = DTB, aes(x = BI_I_ww, fill = "BI_I_ww"), alpha = 0.5) +
#   labs(x = "Business Continutiy (ratio weekly)", y = "Densità", title = "Sovrapposizione delle Distribuzioni delle varie componenti BUSINESS CONTINUTIY") +
#   scale_fill_manual(values = c("BI_S_ww" = "blue", "BI_M_ww" = "red", "BI_I_ww"="green")) +
#   theme_minimal()

#Esportare il Dataframe
colnames(entire_region)
colnames(DTB)
left_join(
  entire_region %>% st_set_geometry(NULL),
  DTB %>% select(row_id,WD,LR_S:BI_I_yy) %>% st_set_geometry(NULL),
  by='row_id') %>% 
saveRDS(., file = glue::glue('./out/shocks/claims/marcello/{stringr::str_replace(basename(path_evento),".gpkg",".rds")}'))


# #altro testo
# #######
# # Altezza causale specifica
# altezza_causale <- 0.65  # Sostituisci questo valore con l'altezza specifica che stai cercando
# altezza_causale <- c(4.5,0.2)  # Sostituisci questo valore con l'altezza specifica che stai cercando

# # Asset specifico
# asset_da_cercare <- "COM_OF"  # Sostituisci con il nome dell'asset che ti interessa

# # Verifica se l'altezza causale è presente nel dataframe
# if (altezza_causale %in% LR$Building$WaterDepth) {
#   # Se l'altezza è presente nel dataframe, ottieni il valore di LR direttamente per l'asset specifico
#   valore_LR <- LR$Building[LR$Building$WaterDepth == altezza_causale, asset_da_cercare]
# } else {
#   # Se l'altezza non è presente nel dataframe, esegui l'interpolazione per l'asset specifico utilizzando na.approx
#   valore_LR <- na.approx(LR$Building[[asset_da_cercare]], x = LR$Building$WaterDepth, xout = altezza_causale)
# }









