library(dplyr)
library(readxl)
num_S_step2 <- read_excel("/mnt/beegfs/lcesarini/IRIO_FLOOD_MODEL/DATA/CLAIMS/num_S_step2.xlsx")
num_S_step <- read_excel("/mnt/beegfs/lcesarini/IRIO_FLOOD_MODEL/DATA/CLAIMS/num_S.xlsx")

num_S_step[4,3:25]  %>% as.numeric() %>% sum()
num_S_step2[4,2:24]  %>% as.numeric() 

all_hit_jle     <- readRDS("/mnt/beegfs/lcesarini/2025_p_irio/test/all_hit_jle.rds")
all_hit_lui     <- readRDS("/mnt/beegfs/lcesarini/2025_p_irio/test/all_hit_luigi.rds")


xx_S_jle     <- readRDS("/mnt/beegfs/lcesarini/2025_p_irio/test/xx_S_jle.rds")
res_xx_S_jle <- readRDS("/mnt/beegfs/lcesarini/2025_p_irio/test/res_xx_S_jle.rds")
num_S_jle    <- readRDS("/mnt/beegfs/lcesarini/2025_p_irio/test/num_S_jle.rds")

xx_S_lui     <- readRDS("/mnt/beegfs/lcesarini/2025_p_irio/test/xx_S_luigi.rds")
res_xx_S_lui <- readRDS("/mnt/beegfs/lcesarini/2025_p_irio/test/res_xx_S_luigi.rds")
num_S_lui    <- readRDS("/mnt/beegfs/lcesarini/2025_p_irio/test/num_S_luigi.rds")


dim(xx_S_jle)
dim(xx_S_lui)

all(xx_S_jle==xx_S_lui)

dim(res_xx_S_jle)
dim(res_xx_S_lui)

all(res_xx_S_jle==res_xx_S_lui)

dim(num_S_lui)
dim(num_S_jle)

all(num_S_jle[,2:24]==num_S_lui[,2:24])


all_hit_lui %>% dim()
all_hit_jle %>% dim()

all_hit_lui[4,]


all(all_hit$DT_S==all_hit2$DT_S)
all(all_hit$DT_S_ww==all_hit2$DT_S_ww)

all(all_hit$resDT_S_ww[1]==all_hit2$resDT_S_ww[1])

all_hit$DT_S[1] %/%7 
all_hit2$DT_S[1]/7 

# Luigi
(all_hit$DT_S[1] %% 7 ) %>% round()
# Jlenia
all_hit2$DT_S[1]/7  - floor(all_hit2$DT_S_ww[1])



all_hit$BI_S_ww[1]
all_hit2$BI_S_ww[1]

all(all_hit$BI_S_ww==all_hit2$BI_S_ww)
all(all_hit$resBI_S_ww==all_hit2$resBI_S_ww)

all(all_hit$BI_M_ww==all_hit2$BI_M_ww)
all(all_hit$resBI_M_ww==all_hit2$resBI_M_ww)

all(all_hit$BI_I_ww==all_hit2$BI_I_ww)
all(all_hit$resBI_I_ww==all_hit2$resBI_I_ww)

weeks_S==weeks_S2



all(all_hit$sizeBI_S_ww==all_hit2$sizeBI_S_ww)
all(all_hit$res_sizeBI_S_ww==all_hit2$res_sizeBI_S_ww)

all(all_hit$irpet_n==all_hit2$irpet_n)
all(all_hit$DT_S_ww==all_hit2$DT_S_ww)

dim(all_hit)
dim(all_hit2)

dim(xx_S)
dim(xx_S2)
all(xx_S==xx_S2)

dim(res_xx_S)
dim(res_xx_S2)
all(res_xx_S==res_xx_S2)

max(xx_S$sizeBI_S_ww)==max(xx_S2$x)
mean(xx_S$sizeBI_S_ww)==mean(xx_S2$x)
median(xx_S$sizeBI_S_ww)==median(xx_S2$x)
max(res_xx_S$res_sizeBI_S_ww)==max(res_xx_S2$x)
mean(res_xx_S$res_sizeBI_S_ww)==mean(res_xx_S2$x)
median(res_xx_S$res_sizeBI_S_ww)==median(res_xx_S2$x)


xx_S %>% filter(irpet_n==6)
xx_S2 %>% filter(Group.1==6)
res_xx_S %>% filter(irpet_n==6)
res_xx_S2 %>% filter(Group.1==6)

num_S1 %>% as.data.frame() %>% filter(irpet_n==6) %>% t()
num_S2 %>% as.data.frame() %>% filter(irpet_n==6) %>% t()

dim(num_S)
dim(num_S2)

all(num_S$irpet_n==num_S2[,1])

for(i in 1:43){
    if(any(num_S[i,2:24]!=num_S2[i,2:24])) print(i)
}

num_S[6,6]==num_S2[6,6]
colnames(num_S)[2:24]
plot(unlist(num_S[6,2:24]),type='l',col="red",lwd=2)
lines(unlist(num_S2[6,2:24]),type='l',col='green',lwd=2)
axis(1, at = seq(1, 23, by = 1),labels=colnames(num_S)[2:24])   




DTB %>% filter(wh_m >0) %>% head() %>% View


library(sf)
sf_obj <- st_read("/mnt/beegfs/lcesarini/2024_IRIO_EQ/res/db_asia_geocoded.gpkg")


dim(sf_obj)
colnames(sf_obj)

sum(is.na(sf_obj$Sectors))

unique(sf_obj$Sectors)



corr_sect <- jsonlite::fromJSON("/mnt/beegfs/lcesarini/2025_p_irio/res/correspondance_sector.json")
df_corr <- corr_sect %>% dplyr::rename(Sectors=name_in_asia) %>% dplyr::select(Sectors,id)

"%!in%" <- Negate("%in%")

which(sf_obj$Sectors %!in% df_corr$Sectors)
all(sf_obj$Sectors %in% df_corr$Sectors)

sf_obj$Sectors[2366255]



