runModel<-function(IO,measures,coefficienti_consumo,params,rounds,LD_Emilia_hazus,LD_Emilia_ins_M,LossRatio_Emilia_I,scenario,epsilon){

library(matrixStats)

n=measures$n
nSectors=measures$nSectors
nFD=measures$nFD
nItalianRegions=measures$nItalianRegions
nbRounds=rounds      #some sectors have a restoration time of 100 weeks
IO=IO/52
IO[IO<0]=0
production=rowSums(IO[1:n,])
production[production==0]<-0.00000001
Z = as.matrix(IO[1:n,1:n])
Z[Z<0]=0
x <- diag(1/production)
A <- Z %*% x
## to avoid "ITA external" messing things up
A[861:903,]<-0
ArtIndices=c(nSectors*(0:(nItalianRegions-1)))
ArtIndices=ArtIndices+(nSectors-1)

## Final demand 
finalDemandItaTot=rowSums(IO[1:n,(n+1):(n+nFD*nItalianRegions)])           #final demand without changes in inventories and exports (The total demand faced by domestic sectors is the sum of intermediate demand)
Exports=(IO[1:(nItalianRegions*nSectors),ncol(IO)])                        #ncol(IO) means column=1010
finalDemandTot=rowSums(IO[1:n,(n+1):ncol(IO)])                             #final demand with exports and changes in inventories
Inv_indices=c(1:nItalianRegions)                                           #create a vector for investment index
Inv_indices=Inv_indices*4                                                  #starting from final demand (from column 904), gross investment are in column 907 (907-903 = 4). Gross investment in each region are placed every 4 columns
Inv_indices=Inv_indices+c(0:(nItalianRegions-1))
Investment=rowSums(IO[1:(nItalianRegions*nSectors),(n+Inv_indices)])       #aggregate investment by sector. (n+Inv_indices) returns the exact number of column corresponding to gross investment

Cons_indices=c(rep(1,nItalianRegions))                    
indices_all=c(rep(1,nItalianRegions))

for(i in 1:nItalianRegions){
  Cons_indices[((i-1)*1+1):((i-1)*1+1)]=Cons_indices[((i-1)*1+1):((i-1)*1+1)]+nFD*(i-1)  
  indices_all[(i)]=indices_all[(i)]+nFD*(i-1)  
}

cons_columns=IO[1:(nItalianRegions*nSectors),n+Cons_indices]       #households' consumption per region
cons_all=IO[1:(nItalianRegions*nSectors),n+indices_all]
cons=rowSums(IO[1:(nItalianRegions*nSectors),(n+Cons_indices)])    #households' consumption on aggregate        #aggregate consumption by sector

finalDemandExo=finalDemandTot-cons-Exports-Investment              #demand coming from abroad is exogenous but is "added" to final demand (that is why it is here subtracted)           #exogenous demand          

cons_regions<-array(data=NA,dim=c((nItalianRegions*nSectors),nItalianRegions))
cons_regions <- cons_columns


if(nodemand){
  cons_regions_exo=cons_regions
}else{
  cons_regions_endo<-array(data=NA,dim=c((nItalianRegions*nSectors),nItalianRegions))
  for(i in 1:nrow(coefficienti_consumo)){
    coeffs=coefficienti_consumo[i,]
    for(j in 1:nItalianRegions){
      cons_regions_endo[i,j]<-coeffs[((j-1)*nSectors+1):((j-1)*nSectors+nSectors)]%*%production[((j-1)*nSectors+1):((j-1)*nSectors+nSectors)]
    } #bridge matrix: the vector of past gross output is linked to the vector of final consumption induced by employees' compensation
  }
  cons_regions_exo=cons_regions-cons_regions_endo
}

Inv_shares=Investment/sum(Investment)                            #i^bar / iota' i^bar (pag. 13-14)
Investment=sum(Investment)

GDPinit=sum(IO[1:n,(n+1):(ncol(IO))])+sum(IO[905,(n+1):(ncol(IO))])-sum(IO[906,1:n]) #GDP initial: sum of all inputs + taxes/(subsidies) - imports 
Taxes=sum(IO[905,(n+1):(ncol(IO))])                              #aggregate taxes 
Tax=(IO[905,(n+1):(ncol(IO))])
Taxes_regions=c(rep(0,20))
for(i in 1:(nItalianRegions-1)){
  Taxes_regions[i]<-sum(Tax[((i-1)*nFD+1):((i-1)*nFD+nFD)])
}
importCoeffs=IO[906,1:n]/production                              #calculate input coefficient concerning imports

outputIO<-rowSums(IO[1:n,])
GDPShocks=c(rep(0,nbRounds))
outputTotal=c(rep(0,nbRounds))
outputShocks=c(rep(0,nbRounds))
outputShocksSectors=array(0,dim=c(nbRounds,43))
outputShocksRegions=array(0,dim=c(nbRounds,20))
ratioShocks=c(rep(0,nbRounds))
ratioShocksSectors=array(0,dim=c(nbRounds,43))
ratioShocksRegions=array(0,dim=c(nbRounds,20))

nbParameters=4
parametersNames=c("speedInv","Inv","timeScale","Exp")            
parameters=array(0,dim=c(n,nbParameters), dimnames=list(c(),parametersNames))
inventParams<-params[3:(nSectors+2)]       #for the 43 sectors    
parameters[,"timeScale"]=rep(t(inventParams),nItalianRegions)
parameters[,"speedInv"]=1/parameters[,"timeScale"]
parameters[,"Inv"]=t(params[1])
parameters[,"Exp"]=t(params[2])
if(nodemand){
  parameters[,"Inv"]=0
}

#we initialize some variables to track the results over time and across the different scenarios
sales=array(0,dim=c(nbRounds,(nItalianRegions-1)))
sales_sectors=array(0,dim=c(nbRounds,43))
capacity=array(0,dim=c(nbRounds,nItalianRegions*nSectors))
capacitySectors=array(0,dim=c(nbRounds,43))
capacityRegions=array(0,dim=c(nbRounds,20))
capacity_EROM=array(0,dim=c(nbRounds,43))
capacity2=array(0,dim=c(nbRounds,nItalianRegions*nSectors))
capacity_EROM2=array(0,dim=c(nbRounds,43))
demandTotal=c(rep(0,nbRounds))
demandSectors=array(0,dim=c(nbRounds,43))
demandRegions=array(0,dim=c(nbRounds,20))
expectationsRegions=array(0,dim=c(nbRounds,20))
producible=array(0,dim=c(nbRounds,nItalianRegions*nSectors))
producibleTotal=c(rep(0,nbRounds))
producibleSectors=array(0,dim=c(nbRounds,43))
producibleRegions=array(0,dim=c(nbRounds,20))
consTotal=c(rep(0,nbRounds))
consRegions=array(0,dim=c(nbRounds,20))
consSectors=array(0,dim=c(nbRounds,43))
ratio=array(0,dim=c(nbRounds,nSectors*nItalianRegions))
ratioTotal=c(rep(0,nbRounds))
ratioRegions=array(0,dim=c(nbRounds,20))
ratioDemandRegions=array(0,dim=c(nbRounds,20))
ratioSectors=array(0,dim=c(nbRounds,43))
ratioDemandSectors=array(0,dim=c(nbRounds,43))
baselineOutput=rowSums(IO[1:903,])
sectorAvail=c(1:43)
prod_sectors=array(0,dim=c(nbRounds,length(sectorAvail)))
prod_sectors_EROM=array(0,dim=c(nbRounds,length(sectorAvail)))
prod_sectors_regions=array(0,dim=c(nbRounds,length(sectorAvail)))
GDP=c(rep(0,nbRounds))
GDP_regions=array(0,dim=c(nbRounds,(nItalianRegions-1)))
Inv=c(rep(0,nbRounds))
Cons=c(rep(0,nbRounds))
c_all_delivered=c(rep(0,nbRounds))

#Labor needed for production purposes normalized to 100  but, as for other inputs, also labor in excess is maintained
#(otherwise it wouldn't be possible to increase production due to a labor constraints)
laborAvailable1=matrix(100,nrow=1, ncol=n)
#The leontief coefficient for labor is calculated on workers actually needed for production (i.e. 100)
laborLeontiefCoefficients=(laborAvailable1)/production     #(production=rowSums(IO[1:n,]))
laborLeontiefCoefficientsEmilia=laborLeontiefCoefficients[302:344]

#Same logic for the PRODUCTION SHOCKS, expressed as % deviation from initial labor available to each industry, normalized to 100 (plus 20 to keep excess capacity)
productionShock=matrix(0, nrow=n, ncol=nbRounds)

#idem for INVENTORY SHOCKS
inventoryShock=matrix(0, nrow=n, ncol=nbRounds)

#SHOCK TO AGRICULTURE (same for both scenarios):

#stime da ultimo Report Emilia Romagna: https://protezionecivile.regione.emilia-romagna.it/notizie/2023/novembre/post-alluvione-in-arrivo-106-milioni-di-euro-per-sostenere-le-imprese-agricole-dell2019emilia-romagna
#assmpt: the loss is equally distributed over 31 weeks (31=weighted average of downtime, see Beatrice's data)
#LossAgri<-1500/52 #old loss and assumptions
LossAgri<-912/31 
OutputAgri_afterFlood<-production[302]-LossAgri
laborAgri_afterFlood<-laborLeontiefCoefficientsEmilia[1]*OutputAgri_afterFlood  #weekly
laborShock_Agri<-(laborAvailable1[1,1]-laborAgri_afterFlood)/100     #percentage variation in labor

#shocks for all sectors:
shocks<-matrix(0,nrow = nSectors, ncol = nbRounds+1)
shocks[,1]<-c(1:43)

if(scenario==1){
  #HAZUS
  LD_Emilia<-LD_Emilia_hazus[,-1]
  shocks[,2:101]<-as.matrix(LD_Emilia[,2:101])
  LD_Emilia<-as.data.frame(shocks)
  LD_Emilia[1,2:32]<- laborShock_Agri 
  Empl_inv<-LD_Emilia[,2]*100   #copy and paste as column into excel file "EROM_histoR_hazus" (folder HAZUS)
  LR<-as.data.frame(matrix(0,nrow=(nSectors*(nItalianRegions-1)), ncol=nbRounds+1))
}

if(scenario==2){
  #INSURANCE CLAIMS
  LD_Emilia<-LD_Emilia_ins_M[,-1]
  shocks[,2:101]<-as.matrix(LD_Emilia[,2:101])
  LD_Emilia<-as.data.frame(shocks)
  LD_Emilia[is.na(LD_Emilia)]<-0
  LD_Emilia[1,2:32]<- laborShock_Agri
  LD_Emilia[2,]<- 0  
  LD_Emilia_hazus <- LD_Emilia_hazus[,-1]
  LD_Emilia[3,] <- LD_Emilia_hazus[3,]
  LD_Emilia[24:25,] <- LD_Emilia_hazus[24:25,]
  LD_Emilia[39:42,] <- LD_Emilia_hazus[39:42,]
  LD_Emilia[,102:nbRounds] <- 0
  Empl_inv<-LD_Emilia[,2]*100   #copy and paste as column into excel file "EROM_histoR_ing" (folder CLAIMS)
  #shocks to inventories
  LR_Emilia<-LossRatio_Emilia_I[,-1]
  LR<-matrix(0,nrow=(nSectors*(nItalianRegions-1)), ncol=nbRounds+1)
  LR_Emilia<-LR_Emilia[,-1]
  shocks_I<-matrix(0,nrow=nrow(LR_Emilia), ncol=ncol(LR_Emilia)+8)
  shocks_I[,1:100]<-as.matrix(LR_Emilia)
  LR[302:344,2:(nbRounds+1)]<-as.matrix(shocks_I)
  LR<-as.data.frame(LR)
}

#the following is to run after having decided which shock to use:
LD<-matrix(0,nrow=(nSectors*(nItalianRegions-1)), ncol=nbRounds+1)
LD_Emilia<-LD_Emilia[,-1]
LD[302:344,2:(nbRounds+1)]<-as.matrix(LD_Emilia)
LD<-as.data.frame(LD)


#labor shocks
productionShock[1:((nItalianRegions-1)*nSectors),5]=-LD$V2
productionShock[1:((nItalianRegions-1)*nSectors),6]=-(LD$V3-LD$V2)
productionShock[1:((nItalianRegions-1)*nSectors),7]=-(LD$V4-LD$V3)
productionShock[1:((nItalianRegions-1)*nSectors),8]=-(LD$V5-LD$V4)
productionShock[1:((nItalianRegions-1)*nSectors),9]=-(LD$V6-LD$V5)
productionShock[1:((nItalianRegions-1)*nSectors),10]=-(LD$V7-LD$V6)
productionShock[1:((nItalianRegions-1)*nSectors),11]=-(LD$V8-LD$V7)
productionShock[1:((nItalianRegions-1)*nSectors),12]=-(LD$V9-LD$V8)
productionShock[1:((nItalianRegions-1)*nSectors),13]=-(LD$V10-LD$V9)
productionShock[1:((nItalianRegions-1)*nSectors),14]=-(LD$V11-LD$V10)
productionShock[1:((nItalianRegions-1)*nSectors),15]=-(LD$V12-LD$V11)
productionShock[1:((nItalianRegions-1)*nSectors),16]=-(LD$V13-LD$V12)
productionShock[1:((nItalianRegions-1)*nSectors),17]=-(LD$V14-LD$V13)
productionShock[1:((nItalianRegions-1)*nSectors),18]=-(LD$V15-LD$V14)
productionShock[1:((nItalianRegions-1)*nSectors),19]=-(LD$V16-LD$V15)
productionShock[1:((nItalianRegions-1)*nSectors),20]=-(LD$V17-LD$V16)
productionShock[1:((nItalianRegions-1)*nSectors),21]=-(LD$V18-LD$V17)
productionShock[1:((nItalianRegions-1)*nSectors),22]=-(LD$V19-LD$V18)
productionShock[1:((nItalianRegions-1)*nSectors),23]=-(LD$V20-LD$V19)
productionShock[1:((nItalianRegions-1)*nSectors),24]=-(LD$V21-LD$V20)
productionShock[1:((nItalianRegions-1)*nSectors),25]=-(LD$V22-LD$V21)
productionShock[1:((nItalianRegions-1)*nSectors),26]=-(LD$V23-LD$V22)
productionShock[1:((nItalianRegions-1)*nSectors),27]=-(LD$V24-LD$V23)
productionShock[1:((nItalianRegions-1)*nSectors),28]=-(LD$V25-LD$V24)
productionShock[1:((nItalianRegions-1)*nSectors),29]=-(LD$V26-LD$V25)
productionShock[1:((nItalianRegions-1)*nSectors),30]=-(LD$V27-LD$V26)
productionShock[1:((nItalianRegions-1)*nSectors),31]=-(LD$V28-LD$V27)
productionShock[1:((nItalianRegions-1)*nSectors),32]=-(LD$V29-LD$V28)
productionShock[1:((nItalianRegions-1)*nSectors),33]=-(LD$V30-LD$V29)
productionShock[1:((nItalianRegions-1)*nSectors),34]=-(LD$V31-LD$V30)
productionShock[1:((nItalianRegions-1)*nSectors),35]=-(LD$V32-LD$V31)
productionShock[1:((nItalianRegions-1)*nSectors),36]=-(LD$V33-LD$V32)
productionShock[1:((nItalianRegions-1)*nSectors),37]=-(LD$V34-LD$V33)
productionShock[1:((nItalianRegions-1)*nSectors),38]=-(LD$V35-LD$V34)
productionShock[1:((nItalianRegions-1)*nSectors),39]=-(LD$V36-LD$V35)
productionShock[1:((nItalianRegions-1)*nSectors),40]=-(LD$V37-LD$V36)
productionShock[1:((nItalianRegions-1)*nSectors),41]=-(LD$V38-LD$V37)
productionShock[1:((nItalianRegions-1)*nSectors),42]=-(LD$V39-LD$V38)
productionShock[1:((nItalianRegions-1)*nSectors),43]=-(LD$V40-LD$V39)
productionShock[1:((nItalianRegions-1)*nSectors),44]=-(LD$V41-LD$V40)
productionShock[1:((nItalianRegions-1)*nSectors),45]=-(LD$V42-LD$V41)
productionShock[1:((nItalianRegions-1)*nSectors),46]=-(LD$V43-LD$V42)
productionShock[1:((nItalianRegions-1)*nSectors),47]=-(LD$V44-LD$V43)
productionShock[1:((nItalianRegions-1)*nSectors),48]=-(LD$V45-LD$V44)
productionShock[1:((nItalianRegions-1)*nSectors),49]=-(LD$V46-LD$V45)
productionShock[1:((nItalianRegions-1)*nSectors),50]=-(LD$V47-LD$V46)
productionShock[1:((nItalianRegions-1)*nSectors),51]=-(LD$V48-LD$V47)
productionShock[1:((nItalianRegions-1)*nSectors),52]=-(LD$V49-LD$V48)
productionShock[1:((nItalianRegions-1)*nSectors),53]=-(LD$V50-LD$V49)
productionShock[1:((nItalianRegions-1)*nSectors),54]=-(LD$V51-LD$V50)
productionShock[1:((nItalianRegions-1)*nSectors),55]=-(LD$V52-LD$V51)
productionShock[1:((nItalianRegions-1)*nSectors),56]=-(LD$V53-LD$V52)
productionShock[1:((nItalianRegions-1)*nSectors),57]=-(LD$V54-LD$V53)
productionShock[1:((nItalianRegions-1)*nSectors),58]=-(LD$V55-LD$V54)
productionShock[1:((nItalianRegions-1)*nSectors),59]=-(LD$V56-LD$V55)
productionShock[1:((nItalianRegions-1)*nSectors),60]=-(LD$V57-LD$V56)
productionShock[1:((nItalianRegions-1)*nSectors),61]=-(LD$V58-LD$V57)
productionShock[1:((nItalianRegions-1)*nSectors),62]=-(LD$V59-LD$V58)
productionShock[1:((nItalianRegions-1)*nSectors),63]=-(LD$V60-LD$V59)
productionShock[1:((nItalianRegions-1)*nSectors),64]=-(LD$V61-LD$V60)
productionShock[1:((nItalianRegions-1)*nSectors),65]=-(LD$V62-LD$V61)
productionShock[1:((nItalianRegions-1)*nSectors),66]=-(LD$V63-LD$V62)
productionShock[1:((nItalianRegions-1)*nSectors),67]=-(LD$V64-LD$V63)
productionShock[1:((nItalianRegions-1)*nSectors),68]=-(LD$V65-LD$V64)
productionShock[1:((nItalianRegions-1)*nSectors),69]=-(LD$V66-LD$V65)
productionShock[1:((nItalianRegions-1)*nSectors),70]=-(LD$V67-LD$V66)
productionShock[1:((nItalianRegions-1)*nSectors),71]=-(LD$V68-LD$V67)
productionShock[1:((nItalianRegions-1)*nSectors),72]=-(LD$V69-LD$V68)
productionShock[1:((nItalianRegions-1)*nSectors),73]=-(LD$V70-LD$V69)
productionShock[1:((nItalianRegions-1)*nSectors),74]=-(LD$V71-LD$V70)
productionShock[1:((nItalianRegions-1)*nSectors),75]=-(LD$V72-LD$V71)
productionShock[1:((nItalianRegions-1)*nSectors),76]=-(LD$V73-LD$V72)
productionShock[1:((nItalianRegions-1)*nSectors),77]=-(LD$V74-LD$V73)
productionShock[1:((nItalianRegions-1)*nSectors),78]=-(LD$V75-LD$V74)
productionShock[1:((nItalianRegions-1)*nSectors),79]=-(LD$V76-LD$V75)
productionShock[1:((nItalianRegions-1)*nSectors),80]=-(LD$V77-LD$V76) 
productionShock[1:((nItalianRegions-1)*nSectors),81]=-(LD$V78-LD$V77)
productionShock[1:((nItalianRegions-1)*nSectors),82]=-(LD$V79-LD$V78)
productionShock[1:((nItalianRegions-1)*nSectors),83]=-(LD$V80-LD$V79)
productionShock[1:((nItalianRegions-1)*nSectors),84]=-(LD$V81-LD$V80)
productionShock[1:((nItalianRegions-1)*nSectors),85]=-(LD$V82-LD$V81)
productionShock[1:((nItalianRegions-1)*nSectors),86]=-(LD$V83-LD$V82)
productionShock[1:((nItalianRegions-1)*nSectors),87]=-(LD$V84-LD$V83)
productionShock[1:((nItalianRegions-1)*nSectors),88]=-(LD$V85-LD$V84)
productionShock[1:((nItalianRegions-1)*nSectors),89]=-(LD$V86-LD$V85)
productionShock[1:((nItalianRegions-1)*nSectors),90]=-(LD$V87-LD$V86)
productionShock[1:((nItalianRegions-1)*nSectors),91]=-(LD$V88-LD$V87)
productionShock[1:((nItalianRegions-1)*nSectors),92]=-(LD$V89-LD$V88)
productionShock[1:((nItalianRegions-1)*nSectors),93]=-(LD$V90-LD$V89)
productionShock[1:((nItalianRegions-1)*nSectors),94]=-(LD$V91-LD$V90)
productionShock[1:((nItalianRegions-1)*nSectors),95]=-(LD$V92-LD$V91)
productionShock[1:((nItalianRegions-1)*nSectors),96]=-(LD$V93-LD$V92)
productionShock[1:((nItalianRegions-1)*nSectors),97]=-(LD$V94-LD$V93)
productionShock[1:((nItalianRegions-1)*nSectors),98]=-(LD$V95-LD$V94)
productionShock[1:((nItalianRegions-1)*nSectors),99]=-(LD$V96-LD$V95)
productionShock[1:((nItalianRegions-1)*nSectors),100]=-(LD$V97-LD$V96) 
productionShock[1:((nItalianRegions-1)*nSectors),101]=-(LD$V98-LD$V97)
productionShock[1:((nItalianRegions-1)*nSectors),102]=-(LD$V99-LD$V98)
productionShock[1:((nItalianRegions-1)*nSectors),103]=-(LD$V100-LD$V99)
productionShock[1:((nItalianRegions-1)*nSectors),104]=-(LD$V101-LD$V100)
productionShock[1:((nItalianRegions-1)*nSectors),105]=-(LD$V102-LD$V101)

if(nodemand){
  productionShock=matrix(0, nrow=n, ncol=nbRounds)
}

#We translate % in levels
for (i in 1:nbRounds){
  productionShock[,i]=laborAvailable1*productionShock[,i]
}

#Shock to the stock of inventories (only for insurance claims, otherwise=0)
inventoryShock[1:((nItalianRegions-1)*nSectors),5]=LR$V2   #do not put -LR$V2


#re-initialize variables
laborAvailable=laborAvailable1
delivered=matrix(0, nrow=n, ncol=nbRounds)
availableInputs=Z*rep(parameters[,"timeScale"], rep.int(nrow(Z),length(parameters[,"timeScale"])))
availableImports=IO[906,1:n]*parameters[,"timeScale"] 
#Production delivered by each sector to each sector (initially equal to Z)
productionDelivered=Z
#Goods ordered for delivery next period (has the same format of Z)
orders=Z
#The inputs needed to produce the amount corresponding to total expected demand (final and intermediate) (has the same format of Z)
inputsRequiredGivenDemand=Z

fd=matrix(finalDemandTot,nrow=n, ncol=1)
productionDemand=matrix(0,nbRounds,n)
pastDemand=matrix(0,n,max(nbRounds,max(parameters[,"Exp"])))
bottlenecks=matrix(0,n,nbRounds)
bottlenecks_EROM=matrix(0,nSectors,nbRounds)
bottlenecks_inputs=matrix(0,n,nbRounds)
bottlenecks_inputs_EROM=matrix(0,nSectors,nbRounds)
check1=matrix(0,n,nbRounds)
check2=matrix(0,n,nbRounds)
check3=matrix(0,n,nbRounds)
check4=matrix(0,n,nbRounds)
check5=matrix(0,n,nbRounds)
check6=matrix(0,n,nbRounds)
Num_hiringSectors=matrix(0,nbRounds)
Num_hiringSectors_EROM=matrix(0,nbRounds)
pastDemand[1:n,]=rowSums(orders)+fd
expectations=rowMeans(pastDemand)
expectations2=expectations
demand=rowSums(orders)+fd
pastfd=fd
demandL=demand
productionDelivered=matrix(data=as.numeric(c(1:((nItalianRegions)*nSectors))),ncol=((nItalianRegions)*nSectors))
productionDelivered[1,]=production
fdexo=finalDemandExo
epsilon_max=max(epsilon) 
epsilon<-matrix(rep(t(epsilon),21),ncol=ncol(epsilon),byrow=TRUE)
epsilon<-matrix(rep(epsilon),n,nrow=nrow(epsilon),byrow=FALSE)
matrix_demand=t(matrix(rep(demand,n),ncol=n))   #desired output matrix
maxOutputGivenIntermediateInputsLeontief=availableInputs/A    #what can be produced using the stock of inputs
maxOutputGivenIntermediateInputsLeontief[is.nan(maxOutputGivenIntermediateInputsLeontief)]=Inf

if(TradLeontief==1){
  epsilon[,]<-epsilon_max
  weightedOutput=matrix_demand*(1-(epsilon/epsilon_max))+(epsilon/epsilon_max)*maxOutputGivenIntermediateInputsLeontief
}else{
  weightedOutput=matrix_demand*(1-(epsilon/epsilon_max))+(epsilon/epsilon_max)*pmin(matrix_demand,maxOutputGivenIntermediateInputsLeontief)
}

maxOutputGivenLabour=laborAvailable/laborLeontiefCoefficients
maxOutputGivenImports=availableImports/importCoeffs
maxOutputGivenImports[is.nan(maxOutputGivenImports)]=Inf
maxOutputFeasible=pmin(maxOutputGivenLabour,pmin(maxOutputGivenImports,colMins(weightedOutput)))


#THE SIMULATION
for(t in 1:nbRounds){
  laborAvailable=pmin(100,laborAvailable)
  inputsStart=availableInputs
  importsStart=availableImports
  #every sector observes its final demand (fd)
  fd[,1]=fdexo
  inv=Inv_shares*(Investment+parameters[1,"Inv"]*(sum(productionDelivered[1,1:(nItalianRegions*nSectors)])-sum(production[1:(nItalianRegions*nSectors)])))
  Exp=as.numeric(Exports)
  fd[,1]=fd[,1]+inv
  c_all=cons_all
  
  if(nodemand){
    cons_regions<-cons_regions_exo
  }else{
    cons_regions_endo<-array(data=NA,dim=c((nItalianRegions*nSectors),nItalianRegions))
    for(i in 1:nrow(coefficienti_consumo)){
      coeffs=coefficienti_consumo[i,]
      for(j in 1:nItalianRegions){
        cons_regions_endo[i,j]<-coeffs[((j-1)*nSectors+1):((j-1)*nSectors+nSectors)]%*%productionDelivered[((j-1)*nSectors+1):((j-1)*nSectors+nSectors)]
      }
    }
    cons_regions<-cons_regions_exo+cons_regions_endo
  }
  cons_regions[is.nan(cons_regions)]<-0  
  
  consTotal[t]=sum(cons_regions)

  for(i in 1:(nItalianRegions-1)){
    consRegions[t,i]<-sum(cons_regions[((i-1)*nSectors+1):((i-1)*nSectors+nSectors),])
  }
  for(i in 1:nSectors){
    consSectors[t,i]<-sum(cons_regions[((0:(nItalianRegions-1))*nSectors+i),])
  }
  
  fd[,1]=fd[,1]+rowSums(cons_regions)
  consumption=rowSums(cons_regions)
  pastlaborAvailable=laborAvailable
  laborAvailable=laborAvailable+t(productionShock[,t])
  outputVariationLabourShock=(laborAvailable-pastlaborAvailable)/laborLeontiefCoefficients
  
  if(t==1){
    outputIO_t=outputIO
    for(i in 1:(nItalianRegions-1)){
      outputShocksRegions[t,i]<-sum(outputIO_t[((i-1)*nSectors+1):((i-1)*nSectors+nSectors)])
      ratioShocksRegions[t,i]<-sum(outputIO[((i-1)*nSectors+1):((i-1)*nSectors+nSectors)])/sum(outputIO_t[((i-1)*nSectors+1):((i-1)*nSectors+nSectors)])
    }
    for(i in 1:nSectors){
      outputShocksSectors[t,i]<-sum(outputIO_t[nSectors*(0:(nItalianRegions-1))+i])
      ratioShocksSectors[t,i]<-sum(outputIO[nSectors*(0:(nItalianRegions-1))+i])/sum(outputIO_t[nSectors*(0:(nItalianRegions-1))+i])
    }
    outputShocks[t]<-sum(outputIO_t)
    ratioShocks[t]<-sum(outputIO)/sum(outputIO_t)
  }else{
    outputIO_t=outputIO_t+outputIO*t(productionShock[,t])/100
    for(i in 1:(nItalianRegions-1)){
      outputShocksRegions[t,i]<-sum(outputIO_t[((i-1)*nSectors+1):((i-1)*nSectors+nSectors)])
      ratioShocksRegions[t,i]<-sum(outputIO[((i-1)*nSectors+1):((i-1)*nSectors+nSectors)])/sum(outputIO_t[((i-1)*nSectors+1):((i-1)*nSectors+nSectors)])
    }
    for(i in 1:nSectors){
      outputShocksSectors[t,i]<-sum(outputIO_t[nSectors*(0:(nItalianRegions-1))+i])
      ratioShocksSectors[t,i]<-sum(outputIO[nSectors*(0:(nItalianRegions-1))+i])/sum(outputIO_t[nSectors*(0:(nItalianRegions-1))+i])
    }
    outputShocks[t]<-sum(outputIO_t)
    ratioShocks[t]<-sum(outputIO)/sum(outputIO_t)
  }
  
  
  for(i in 1:n){
    orderVariationLabourShock=outputVariationLabourShock*A[i,]
    expectations[i]=pastfd[i]+sum(orders[i,]+orderVariationLabourShock)
    expectations2[i]=mean(pastDemand[i,(dim(pastDemand)[2]-parameters[i,"Exp"]+1):dim(pastDemand)[2]])
  }
  maxOutputGivenLabour=laborAvailable/laborLeontiefCoefficients
  capacity[t,]=maxOutputGivenLabour
  capacity_EROM[t,]=maxOutputGivenLabour[1,302:344]
  expectations_l=pmin(expectations,maxOutputGivenLabour)     #eq3
  
  vec_inventoryShock<-as.numeric(1-t(inventoryShock[,t]))
  availableInputs2<-availableInputs * rep(vec_inventoryShock, rep.int(nrow(availableInputs),length(vec_inventoryShock)))
  availableInputs2[ArtIndices,]<-Inf
  
  matrix_demand=t(matrix(rep(expectations,n),ncol=n))   #desired output matrix
  maxOutputGivenIntermediateInputsLeontief=availableInputs2/A
  maxOutputGivenIntermediateInputsLeontief[is.nan(maxOutputGivenIntermediateInputsLeontief)]=Inf
  
  if(TradLeontief==1){
    epsilon[,]<-epsilon_max
    weightedOutput=matrix_demand*(1-(epsilon/epsilon_max))+(epsilon/epsilon_max)*maxOutputGivenIntermediateInputsLeontief
  }else{
    weightedOutput=matrix_demand*(1-(epsilon/epsilon_max))+(epsilon/epsilon_max)*pmin(matrix_demand,maxOutputGivenIntermediateInputsLeontief)
  }
  
  maxOutputGivenImports=availableImports/importCoeffs
  maxOutputGivenImports[is.nan(maxOutputGivenImports)]=Inf
  maxOutputFeasible=pmin(maxOutputGivenLabour,pmin(maxOutputGivenImports,colMins(weightedOutput)))
  
  producible[t,]=maxOutputFeasible
  producibleTotal[t]<-sum(maxOutputFeasible)
  for(i in 1:(nItalianRegions-1)){
    producibleRegions[t,i]<-sum(maxOutputFeasible[1,((i-1)*nSectors+1):((i-1)*nSectors+nSectors)])
  }
  for(i in 1:nSectors){
    producibleSectors[t,i]<-sum(maxOutputFeasible[1,nSectors*(0:(nItalianRegions-1))+i])
  }
  ratio[t,]<-maxOutputFeasible[1,]
  
  #if productioin constrained by labor and demand>production feasible, then allow sectors not subject to labor input exogenous constraints 
  #(i.e. those having >= workers than at the beginning) to hire additional workers
  hiringSectors=which(((laborAvailable[1,]/laborLeontiefCoefficients[1,])<pmin(maxOutputGivenImports,colMins(weightedOutput)))&(laborAvailable[1,]>=laborAvailable1[1,])&(expectations>t(maxOutputFeasible)))
  hiringSectors_EROM=which(((laborAvailable[1,302:344]/laborLeontiefCoefficients[1,302:344])<pmin(maxOutputGivenImports[302:344],colMins(weightedOutput[302:344,302:344])))&(laborAvailable[1,302:344]>=laborAvailable1[1,302:344])&(expectations[302:344]>t(maxOutputFeasible[302:344])))
  Num_hiringSectors[t]<-length(hiringSectors)
  Num_hiringSectors_EROM[t]<-length(hiringSectors_EROM)
  laborAvailable[hiringSectors]=expectations[hiringSectors]*laborLeontiefCoefficients[hiringSectors]
  maxOutputGivenLabour=laborAvailable/laborLeontiefCoefficients
  capacity2[t,]=maxOutputGivenLabour
  capacity_EROM2[t,]=maxOutputGivenLabour[1,302:344]
  
  expectations_l=pmin(expectations,maxOutputGivenLabour)
  maxOutputFeasible=pmin(maxOutputGivenLabour,pmin(maxOutputGivenImports,colMins(weightedOutput)))
  bottlenecks[,t]=colMins(weightedOutput)
  bottlenecks_EROM[,t]=colMins(weightedOutput[302:344,302:344])
  xxx<-round(colMins(maxOutputGivenIntermediateInputsLeontief),digits=2)
  yyy<-round(colMins(weightedOutput),digits=2)
  for (i in 1:(nSectors*nItalianRegions)) {
    if(round(maxOutputFeasible[1,i],digits=2)==round(maxOutputGivenLabour[1,i],digits=2)){
      check1[i,t]<-1   #feasible output is constrained by labor
    } 
    if (round(maxOutputFeasible[1,i],digits=2)==yyy[i] & round(maxOutputFeasible[1,i],digits=2)!=round(maxOutputGivenLabour[1,i],digits=2) & yyy[i]==round(matrix_demand[1,i],digits=2)){
      check2[i,t]<-1   #feasible output not constrained by labor and equal to demand (no connection b/w sectors: epsilon/epsilon_max=0)
    }
    if (round(maxOutputFeasible[1,i],digits=2)==yyy[i] & round(maxOutputFeasible[1,i],digits=2)!=round(maxOutputGivenLabour[1,i],digits=2) & yyy[i]<round(matrix_demand[1,i],digits=2)){
      check3[i,t]<-1   #feasible output not constrained by labor and given by the interconnection b/w demand and stock of inputs (i.e. inputs shortages matter)
    }
    if (xxx[i]==0){
      check4[i,t]<-1
    }
  }
  
  check1_EROM<-check1[302:344,]
  check2_EROM<-check2[302:344,]
  check3_EROM<-check3[302:344,]
  check4_EROM<-check4[302:344,]
  
  #every sector computes the inputs required to satisfy its expected total demand given by observed current period final demand and past-period orders by opther sectors
  inputsRequiredGivenDemand=A * rep(expectations_l, rep.int(nrow(A),length(expectations_l)))
  inputsRequiredGivenDemand2=A * rep(expectations2, rep.int(nrow(A),length(expectations2)))
  importsRequiredGivenDemand=importCoeffs*expectations_l
  importsRequiredGivenDemand2=importCoeffs*expectations2
  totalExpectedDemand=sum(expectations_l[1:(nItalianRegions*nSectors)])
  
  #every sector demands inputs based on what he needed to produce desired outpout, inventories left from previous period, and targeted inventories given the new expected demand
  #these inputs will be available for next period production. Current production is based on past inventories (i.e. past period orders)
  orderMat=inputsRequiredGivenDemand2 * rep(parameters[,"timeScale"], rep.int(nrow(inputsRequiredGivenDemand2),length(parameters[,"timeScale"])))
  orderMat=orderMat-availableInputs
  futureOrders=orderMat * rep(parameters[,"speedInv"], rep.int(nrow(orderMat),length(parameters[,"speedInv"])))
  orders=pmax(matrix(0,n,n),inputsRequiredGivenDemand+futureOrders)
  
  importsDeviation=importsRequiredGivenDemand2*parameters[,"timeScale"]
  importsDeviation=importsDeviation-availableImports
  futureImports=parameters[,"speedInv"]*importsDeviation
  ordersImports=pmax(0,importsRequiredGivenDemand+futureImports)
  
  #the demand received by each sector in the period is its fd plus orders received from other sectors
  demand=fd+rowSums(orders)
  for (i in 1:(nSectors*nItalianRegions)) {
    matcheck=matrix_demand[1,]
    if((demand[i]+Exp[i])>matcheck[i]){
      check5[i,t]<-1   #actual demand is greater than expected demand (sectors might have produced less)
    }
  }
  check5_EROM<-check5[302:344,]
  pastfd=fd+Exp
  
  demandTotal[t]<-sum(demand+Exp)
  ratio[t,]<-(demand[,1]+Exp)/ratio[t,]
  ratioTotal[t]<-sum(baselineOutput/sum(baselineOutput)*ratio[t,],na.rm = TRUE)
  
  for(i in 1:(nItalianRegions-1)){
    demandRegions[t,i]<-sum(demand[((i-1)*nSectors+1):((i-1)*nSectors+nSectors),1]+Exp[((i-1)*nSectors+1):((i-1)*nSectors+nSectors)])
    ratioRegions[t,i]<-sum(baselineOutput[((i-1)*nSectors+1):((i-1)*nSectors+nSectors)]/sum(baselineOutput[((i-1)*nSectors+1):((i-1)*nSectors+nSectors)])*ratio[t,((i-1)*nSectors+1):((i-1)*nSectors+nSectors)],na.rm = TRUE)
  }
  for(i in 1:nSectors){
    demandSectors[t,i]<-sum(demand[nSectors*(0:(nItalianRegions-1))+i,1]+Exp[nSectors*(0:(nItalianRegions-1))+i])
    ratioSectors[t,i]<-sum(baselineOutput[nSectors*(0:(nItalianRegions-1))+i]/sum(baselineOutput[nSectors*(0:(nItalianRegions-1))+i])*ratio[t,nSectors*(0:(nItalianRegions-1))+i],na.rm = TRUE)
  }
  
  for(i in 1:(nItalianRegions-1)){
    expectationsRegions[t,i]<-sum(expectations[((i-1)*nSectors+1):((i-1)*nSectors+nSectors)])
  }
  
  
  #production takes place after sectors observe demand
  Exp=pmin(Exp,maxOutputFeasible)
  matrix_demand=t(matrix(rep(demand+Exp,n),ncol=n))   #desired output matrix
  if(TradLeontief==1){
    epsilon[,]<-epsilon_max
    weightedOutput=matrix_demand*(1-(epsilon/epsilon_max))+(epsilon/epsilon_max)*maxOutputGivenIntermediateInputsLeontief
  }else{
    weightedOutput=matrix_demand*(1-(epsilon/epsilon_max))+(epsilon/epsilon_max)*pmin(matrix_demand,maxOutputGivenIntermediateInputsLeontief)
  }
  maxOutputFeasible=pmin(maxOutputGivenLabour,pmin(maxOutputGivenImports,colMins(weightedOutput)))
  
  
  #the production actually delivered 
  if(TradLeontief==1){
    productionDelivered=Exp+pmin(maxOutputFeasible-Exp,demand)
  }else{
    productionDelivered=maxOutputFeasible
  }
  bottlenecks[,t]=bottlenecks[,t]==productionDelivered
  bottlenecks_EROM[,t]=bottlenecks_EROM[,t]==productionDelivered[,302:344]
  for(i in 1:n){
    if(bottlenecks[i,t]==1 & check3[i,t]==1){
      bottlenecks_inputs[i,t]=1
    }
  }
  for(i in 1:nSectors){
    if(bottlenecks_EROM[i,t]==1 & check3_EROM[i,t]==1){
      bottlenecks_inputs_EROM[i,t]=1
    }
  }
  
  delivered[,t]=t(productionDelivered)
  outputTotal[t]=sum(productionDelivered)
  for(i in 1:(nItalianRegions-1)){
    sales[t,i]=sum(delivered[((i-1)*nSectors+1):((i-1)*nSectors+nSectors),t])
    capacityRegions[t,i]=sum(capacity[t,((i-1)*nSectors+1):((i-1)*nSectors+nSectors)])
  }
  for(i in 1:length(sectorAvail)){
    sec=sectorAvail[i]
    for(j in 1:nItalianRegions){
      prod_sectors[t,i]=prod_sectors[t,i]+productionDelivered[1,(sectorAvail[i]+nSectors*(j-1))]
    }
  }
  
  for(i in 1:length(sectorAvail)){
    sec2=sectorAvail[i]
    prod_sectors_EROM[t,i]=prod_sectors_EROM[t,i]+productionDelivered[1,(sectorAvail[i]+301)]
  }
  
  for(i in 1:nSectors){
    sec=i
    for(j in 1:nItalianRegions){
      sales_sectors[t,i]=sales_sectors[t,i]+productionDelivered[1,(sec+nSectors*(j-1))]
      capacitySectors[t,i]=capacitySectors[t,i]+capacity[t,(sec+nSectors*(j-1))]
    }
  }
  
  #is there a supply constraint?
  constraint=t(productionDelivered-Exp)/demand
  constraint[demand==0]=1
  #what is the ratio between capacity of production and demand?
  productionDemand[t,]=maxOutputFeasible/t(demand+Exp)
  productionDemandAggregate=sum(maxOutputFeasible[1,1:(nItalianRegions*nSectors)])/sum(demand[1:(nItalianRegions*nSectors)]+Exp[1:(nItalianRegions*nSectors)])
  
  #production consumes inputs of production...
  availableInputs=availableInputs-A * rep(productionDelivered, rep.int(nrow(A),length(productionDelivered)))
  availableInputs[ArtIndices,]<-pmax(0,availableInputs[ArtIndices,])
  availableImports=availableImports-importCoeffs*productionDelivered
  availableImports=as.vector(availableImports)
  
  #at the end of the period/beginning of next period inputs are replenished with new inputs 
  #ordered at the beginning and delivered at the end after production takes place
  ordersDelivered=orders
  ordersDelivered=orders*as.vector(constraint)
  importsDelivered=ordersImports
  
  #Inventories are replenished...
  availableInputs=availableInputs+ordersDelivered
  availableImports=availableImports+importsDelivered
  changeInventories=availableInputs-inputsStart
  changeImports=availableImports-importsStart
  fdDelivered=fd*as.vector(constraint)
  fdDelivered=fdDelivered+Exp
  GDP[t]=sum(fdDelivered)+sum(changeInventories)-sum(importsDelivered)+sum(changeImports)+Taxes
  if(t==1){
    fdBaseline=fdDelivered
    fdShocks=fdBaseline
    restBaseline=sum(changeInventories)-sum(importsDelivered)+sum(changeImports)+Taxes
    GDPShocks[t]=sum(fdDelivered)+sum(changeInventories)-sum(importsDelivered)+sum(changeImports)+Taxes
  }else{
    fdShocks=fdShocks+fdBaseline*productionShock[,t]/100
    GDPShocks[t]=sum(fdShocks)+restBaseline
  }
  for(i in 1:(nItalianRegions-1)){
    GDP_regions[t,i]=sum(fdDelivered[((i-1)*nSectors+1):((i-1)*nSectors+nSectors),1])+sum(changeInventories[,((i-1)*43+1):((i-1)*43+43)])-sum(importsDelivered[((i-1)*43+1):((i-1)*43+43)])+sum(changeImports[((i-1)*43+1):((i-1)*43+43)])+Taxes_regions[i]
  }
  Inv[t]=sum(inv*as.vector(constraint))
  consumption=consumption*as.vector(constraint)
  c_all=rowSums(c_all)
  c_all=c_all*as.vector(constraint)
  c_all_delivered[t]=sum(c_all)
  Cons[t]=sum(consumption)
  demand=demand+Exp
  pastDemand=cbind(pastDemand[,2:dim(pastDemand)[2]],demand)
  
  
  #END OF SIMULATION LOOP
}

sales_rs=delivered[1:860,1]
bi=LD$V2
intensity11=c(rep(0,20))
for(i in 1:20){
  ld=0
  for(j in 1:43){
    ld=ld+bi[43*(i-1)+j]*sales_rs[43*(i-1)+j]/(sum(sales_rs[(43*(i-1)+1):(43*(i-1)+43)]))
  }
  intensity11[i]=ld
}

intensity12=c(rep(0,43))
for(i in 1:43){
  ld=0
  for(j in 1:20){
    ld=ld+bi[43*(j-1)+i]*sales_rs[43*(j-1)+i]/sales_sectors[1,i]
  }
  intensity12[i]=ld
}

sales_rs=delivered[1:860,1]
bi=c(rep(0,860))
for(i in 1:length(bi)){
  bi[i]=max(LD[i,5:6])
}
intensity21=c(rep(0,20))
for(i in 1:20){
  ld=0
  for(j in 1:43){
    ld=ld+bi[43*(i-1)+j]*sales_rs[43*(i-1)+j]/(sum(sales_rs[(43*(i-1)+1):(43*(i-1)+43)]))
  }
  intensity21[i]=ld
}

intensity22=c(rep(0,43))
for(i in 1:43){
  ld=0
  for(j in 1:20){
    ld=ld+bi[43*(j-1)+i]*sales_rs[43*(j-1)+i]/sales_sectors[1,i]
  }
  intensity22[i]=ld
}  

# 27 is the number of months we consider 
prod_sectors_m=array(0,dim=c(27,length(sectorAvail)))
prod_sectors_m[1,]=colSums(prod_sectors[1:4,])
prod_sectors_m[2,]=colSums(prod_sectors[5:8,])
prod_sectors_m[3,]=colSums(prod_sectors[9:12,])
prod_sectors_m[4,]=colSums(prod_sectors[13:16,])
prod_sectors_m[5,]=colSums(prod_sectors[17:20,])
prod_sectors_m[6,]=colSums(prod_sectors[21:24,])
prod_sectors_m[7,]=colSums(prod_sectors[25:28,])
prod_sectors_m[8,]=colSums(prod_sectors[29:32,])
prod_sectors_m[9,]=colSums(prod_sectors[33:36,])
prod_sectors_m[10,]=colSums(prod_sectors[37:40,])
prod_sectors_m[11,]=colSums(prod_sectors[41:44,])
prod_sectors_m[12,]=colSums(prod_sectors[45:48,])
prod_sectors_m[13,]=colSums(prod_sectors[49:52,])
prod_sectors_m[14,]=colSums(prod_sectors[53:56,])
prod_sectors_m[15,]=colSums(prod_sectors[57:60,])
prod_sectors_m[16,]=colSums(prod_sectors[61:64,])
prod_sectors_m[17,]=colSums(prod_sectors[65:68,])
prod_sectors_m[18,]=colSums(prod_sectors[69:72,])
prod_sectors_m[19,]=colSums(prod_sectors[73:76,])
prod_sectors_m[20,]=colSums(prod_sectors[77:80,])
prod_sectors_m[21,]=colSums(prod_sectors[81:84,])
prod_sectors_m[22,]=colSums(prod_sectors[85:88,])
prod_sectors_m[23,]=colSums(prod_sectors[89:92,])
prod_sectors_m[24,]=colSums(prod_sectors[93:96,])
prod_sectors_m[25,]=colSums(prod_sectors[97:100,])
prod_sectors_m[26,]=colSums(prod_sectors[101:104,])
prod_sectors_m[27,]=colSums(prod_sectors[105:108,])

# one 10 is "number of quarters"; the other 17 is the number of service sectors
prod_sectors_q=array(0,dim=c(9,17))
prod_sectors_q[1,]=colSums(prod_sectors_m[1:3,27:43])
prod_sectors_q[2,]=colSums(prod_sectors_m[4:6,27:43])
prod_sectors_q[3,]=colSums(prod_sectors_m[7:9,27:43])
prod_sectors_q[4,]=colSums(prod_sectors_m[10:12,27:43])
prod_sectors_q[5,]=colSums(prod_sectors_m[13:15,27:43])
prod_sectors_q[6,]=colSums(prod_sectors_m[16:18,27:43])
prod_sectors_q[7,]=colSums(prod_sectors_m[19:21,27:43])
prod_sectors_q[8,]=colSums(prod_sectors_m[22:24,27:43])
prod_sectors_q[9,]=colSums(prod_sectors_m[24:27,27:43])


prod_sectors_q[,1]=prod_sectors_q[,1]/(prod_sectors[1,27]*12)*100
prod_sectors_q[,2]=prod_sectors_q[,2]/(prod_sectors[1,28]*12)*100
prod_sectors_q[,3]=prod_sectors_q[,3]/(prod_sectors[1,29]*12)*100
prod_sectors_q[,4]=prod_sectors_q[,4]/(prod_sectors[1,30]*12)*100
prod_sectors_q[,5]=prod_sectors_q[,5]/(prod_sectors[1,31]*12)*100
prod_sectors_q[,6]=prod_sectors_q[,6]/(prod_sectors[1,32]*12)*100
prod_sectors_q[,7]=prod_sectors_q[,7]/(prod_sectors[1,33]*12)*100
prod_sectors_q[,8]=prod_sectors_q[,8]/(prod_sectors[1,34]*12)*100
prod_sectors_q[,9]=prod_sectors_q[,9]/(prod_sectors[1,35]*12)*100
prod_sectors_q[,10]=prod_sectors_q[,10]/(prod_sectors[1,36]*12)*100
prod_sectors_q[,11]=prod_sectors_q[,11]/(prod_sectors[1,37]*12)*100
prod_sectors_q[,12]=prod_sectors_q[,12]/(prod_sectors[1,38]*12)*100
prod_sectors_q[,13]=prod_sectors_q[,13]/(prod_sectors[1,39]*12)*100
prod_sectors_q[,14]=prod_sectors_q[,14]/(prod_sectors[1,40]*12)*100
prod_sectors_q[,15]=prod_sectors_q[,15]/(prod_sectors[1,41]*12)*100
prod_sectors_q[,16]=prod_sectors_q[,16]/(prod_sectors[1,42]*12)*100
prod_sectors_q[,17]=prod_sectors_q[,17]/(prod_sectors[1,43]*12)*100

prod_sectors_m[1,]=prod_sectors_m[1,]/4
prod_sectors_m[2,]=prod_sectors_m[2,]/4
prod_sectors_m[3,]=prod_sectors_m[3,]/4
prod_sectors_m[4,]=prod_sectors_m[4,]/4
prod_sectors_m[5,]=prod_sectors_m[5,]/4
prod_sectors_m[6,]=prod_sectors_m[6,]/4
prod_sectors_m[7,]=prod_sectors_m[7,]/4
prod_sectors_m[8,]=prod_sectors_m[8,]/4
prod_sectors_m[9,]=prod_sectors_m[9,]/4
prod_sectors_m[10,]=prod_sectors_m[10,]/4
prod_sectors_m[11,]=prod_sectors_m[11,]/4
prod_sectors_m[12,]=prod_sectors_m[12,]/4
prod_sectors_m[13,]=prod_sectors_m[13,]/4
prod_sectors_m[14,]=prod_sectors_m[14,]/4
prod_sectors_m[15,]=prod_sectors_m[15,]/4
prod_sectors_m[16,]=prod_sectors_m[16,]/4
prod_sectors_m[17,]=prod_sectors_m[17,]/4
prod_sectors_m[18,]=prod_sectors_m[18,]/4
prod_sectors_m[19,]=prod_sectors_m[19,]/4
prod_sectors_m[20,]=prod_sectors_m[20,]/4
prod_sectors_m[21,]=prod_sectors_m[21,]/4
prod_sectors_m[22,]=prod_sectors_m[22,]/4
prod_sectors_m[23,]=prod_sectors_m[23,]/4
prod_sectors_m[24,]=prod_sectors_m[24,]/4
prod_sectors_m[25,]=prod_sectors_m[25,]/4
prod_sectors_m[26,]=prod_sectors_m[26,]/4
prod_sectors_m[27,]=prod_sectors_m[27,]/4


for(i in 1:ncol(prod_sectors)){
  prod_sectors[,i]=prod_sectors[,i]/prod_sectors[1,i]*100
}  #weekly

for(i in 1:ncol(prod_sectors_EROM)){
  prod_sectors_EROM[,i]=prod_sectors_EROM[,i]/prod_sectors_EROM[1,i]*100
}  #weekly Emilia


for(i in 1:ncol(prod_sectors_m)){
  prod_sectors_m[,i]=prod_sectors_m[,i]/prod_sectors_m[1,i]*100
}

# Maximum declines over the minimum downtime (weeks)
if(scenario==1){
  minDT=4      
}
if(scenario==2){
  minDT=4
}

declines_minDT=rep(0,43)
for(i in 1:length(declines_minDT)){
  declines_minDT[i]=(min(prod_sectors[1:(4+minDT),i])-100)/100
}
declinesEROM_minDT=rep(0,43)
for(i in 1:length(declinesEROM_minDT)){
  declinesEROM_minDT[i]=(min(prod_sectors_EROM[1:(4+minDT),i])-100)/100
}

# Maximum declines in one year
declines_1y=rep(0,43)
for(i in 1:length(declines_1y)){
  declines_1y[i]=(min(prod_sectors[1:52,i])-100)/100
}
declinesEROM_1y=rep(0,43)
for(i in 1:length(declinesEROM_1y)){
  declinesEROM_1y[i]=(min(prod_sectors_EROM[1:52,i])-100)/100
}

# Maximum declines in the entire simulated period
declines=rep(0,43)
for(i in 1:length(declines)){
  declines[i]=(min(prod_sectors[1:108,i])-100)/100
}
declinesEROM=rep(0,43)
for(i in 1:length(declinesEROM)){
  declinesEROM[i]=(min(prod_sectors_EROM[1:108,i])-100)/100
}

# Production loss during the minimum downtime period
lossProd_minDT=(colSums(prod_sectors[1:(4+minDT),])-((4+minDT)*100))/((4+minDT)*100)    #4+minDT: the flood is in period 5 but period 5 is included in the downtime (1st week of downtime after flood is period 5)
lossProd_EROM_minDT=(colSums(prod_sectors_EROM[1:(4+minDT),])-((4+minDT)*100))/(4+minDT*100)  
# Production loss over 1 year
lossProd_1y=(colSums(prod_sectors[1:52,])-5200)/5200        
lossProd_EROM_1y=(colSums(prod_sectors_EROM[1:52,])-5200)/5200
#Production loss over the entire simulation
lossProd=(colSums(prod_sectors[1:108,])-10800)/10800
lossProd_EROM=(colSums(prod_sectors_EROM[1:108,])-10800)/10800

# Production loss during the minimum downtime period
lossProd_minDT2=((colSums(prod_sectors[4:(4+minDT),])-(minDT*prod_sectors[4,]))/((1+minDT)*prod_sectors[4,]))*100   #4+minDT: the flood is in period 5 but period 5 is included in the downtime (1st week of downtime after flood is period 5)
lossProd_EROM_minDT2=((colSums(prod_sectors_EROM[4:(4+minDT),])-((1+minDT)*prod_sectors_EROM[4,]))/((1+minDT)*prod_sectors_EROM[4,]))*100  
# Production loss over 1 year
lossProd_1y2=((colSums(prod_sectors[4:56,])-53*prod_sectors[4,])/(53*prod_sectors[4,]))*100       
lossProd_EROM_1y2=((colSums(prod_sectors_EROM[4:56,])-53*prod_sectors_EROM[4,])/(53*prod_sectors_EROM[4,]))*100
#Production loss over the entire simulation
lossProd2=((colSums(prod_sectors[4:108,])-105*prod_sectors[4,])/(105*prod_sectors[4,]))*100
lossProd_EROM2=((colSums(prod_sectors_EROM[4:108,])-105*prod_sectors_EROM[4,])/(105*prod_sectors_EROM[4,]))*100

#TO STUDY DIRECT/INDIRECT EFFECTS
#Capacity (changing due to labor shocks)
for(i in 1:ncol(capacity_EROM)){
  capacity_EROM[,i]=capacity_EROM[,i]/capacity_EROM[1,i]*100
  capacity_EROM2[,i]=capacity_EROM2[,i]/capacity_EROM2[1,i]*100
}  #weekly capacity Emilia


expenditure_m=array(0,dim=c(27,1))
expenditure_m[1,]=sum(c_all_delivered[1:4])
expenditure_m[2,]=sum(c_all_delivered[5:8])
expenditure_m[3,]=sum(c_all_delivered[9:12])
expenditure_m[4,]=sum(c_all_delivered[13:16])
expenditure_m[5,]=sum(c_all_delivered[17:20])
expenditure_m[6,]=sum(c_all_delivered[21:24])
expenditure_m[7,]=sum(c_all_delivered[25:28])
expenditure_m[8,]=sum(c_all_delivered[29:32])
expenditure_m[9,]=sum(c_all_delivered[33:36])
expenditure_m[10,]=sum(c_all_delivered[37:40])
expenditure_m[11,]=sum(c_all_delivered[41:44])
expenditure_m[12,]=sum(c_all_delivered[45:48])
expenditure_m[13,]=sum(c_all_delivered[49:52])
expenditure_m[14,]=sum(c_all_delivered[53:56])
expenditure_m[15,]=sum(c_all_delivered[57:60])
expenditure_m[16,]=sum(c_all_delivered[61:64])
expenditure_m[17,]=sum(c_all_delivered[65:68])
expenditure_m[18,]=sum(c_all_delivered[69:72])
expenditure_m[19,]=sum(c_all_delivered[73:76])
expenditure_m[20,]=sum(c_all_delivered[77:80])
expenditure_m[21,]=sum(c_all_delivered[81:84])
expenditure_m[22,]=sum(c_all_delivered[85:88])
expenditure_m[23,]=sum(c_all_delivered[89:92])
expenditure_m[24,]=sum(c_all_delivered[93:96])
expenditure_m[25,]=sum(c_all_delivered[97:100])
expenditure_m[26,]=sum(c_all_delivered[101:104])
expenditure_m[27,]=sum(c_all_delivered[105:108])

expenditure_q=array(0,dim=c(9,1))
expenditure_q[1,]=sum(expenditure_m[1:3,])
expenditure_q[2,]=sum(expenditure_m[4:6,])
expenditure_q[3,]=sum(expenditure_m[7:9,])
expenditure_q[4,]=sum(expenditure_m[10:12,])
expenditure_q[5,]=sum(expenditure_m[13:15,])
expenditure_q[6,]=sum(expenditure_m[16:18,])
expenditure_q[7,]=sum(expenditure_m[19:21,])
expenditure_q[8,]=sum(expenditure_m[22:24,])
expenditure_q[9,]=sum(expenditure_m[25:27,])

expenditure_q[,1]=expenditure_q[,1]/(c_all_delivered[1]*12)*100

expenditure_m[1,]=expenditure_m[1,]/4
expenditure_m[2,]=expenditure_m[2,]/4
expenditure_m[3,]=expenditure_m[3,]/4
expenditure_m[4,]=expenditure_m[4,]/4
expenditure_m[5,]=expenditure_m[5,]/4
expenditure_m[6,]=expenditure_m[6,]/4
expenditure_m[7,]=expenditure_m[7,]/4
expenditure_m[8,]=expenditure_m[8,]/4
expenditure_m[9,]=expenditure_m[9,]/4
expenditure_m[10,]=expenditure_m[10,]/4
expenditure_m[11,]=expenditure_m[11,]/4
expenditure_m[12,]=expenditure_m[12,]/4
expenditure_m[13,]=expenditure_m[13,]/4
expenditure_m[14,]=expenditure_m[14,]/4
expenditure_m[15,]=expenditure_m[15,]/4
expenditure_m[16,]=expenditure_m[16,]/4
expenditure_m[17,]=expenditure_m[17,]/4
expenditure_m[18,]=expenditure_m[18,]/4
expenditure_m[19,]=expenditure_m[19,]/4
expenditure_m[20,]=expenditure_m[20,]/4
expenditure_m[21,]=expenditure_m[21,]/4
expenditure_m[22,]=expenditure_m[22,]/4
expenditure_m[23,]=expenditure_m[23,]/4
expenditure_m[24,]=expenditure_m[24,]/4
expenditure_m[25,]=expenditure_m[25,]/4
expenditure_m[26,]=expenditure_m[26,]/4
expenditure_m[27,]=expenditure_m[27,]/4

for(i in 1:ncol(expenditure_m)){
  expenditure_m[,i]=expenditure_m[,i]/expenditure_m[1,i]*100
}

lossExpenditure_q=(sum(expenditure_q[1:9,])-900)/900

GDP2<-GDP/GDP[1]*100

GDP_regions2<-GDP_regions
for(i in 1:ncol(GDP_regions2)){
  GDP_regions2[,i]=GDP_regions2[,i]/GDP_regions2[1,i]*100
}  #weekly

# Regional GDP losses during the minimum downtime period
lossGDPreg_minDT=(colSums(GDP_regions2[1:(4+minDT),])-((4+minDT)*100))/((4+minDT)*100)    #4+minDT: the flood is in period 5 but period 5 is included in the downtime (1st week of downtime after flood is period 5)
# Regional production loss over 1 year
lossGDPreg_1y=(colSums(GDP_regions2[1:52,])-5200)/5200        
# Regional production loss over the entire simulation
lossGDPreg=(colSums(GDP_regions2[1:108,])-10800)/10800

# Regional GDP losses during the minimum downtime period
lossGDPreg_minDT2=((colSums(GDP_regions2[4:(4+minDT),])-((1+minDT)*GDP_regions2[4,]))/((1+minDT)*GDP_regions2[4,]))*100    #4+minDT: the flood is in period 5 but period 5 is included in the downtime (1st week of downtime after flood is period 5)
# Regional production loss over 1 year
lossGDPreg_1y2=((colSums(GDP_regions2[4:56,])-(53*GDP_regions2[4,]))/(53*GDP_regions2[4,]))*100        
# Regional production loss over the entire simulation
lossGDPreg2=((colSums(GDP_regions2[4:108,])-(105*GDP_regions2[4,]))/(105*GDP_regions2[4,]))*100

#Annualized loss:
annz_loss = (colSums(GDP_regions2[1:108,])-10800)/5200


GDP_q=c(rep(0,9))
GDP_q[1]<-sum(GDP[1:12])
GDP_q[2]<-sum(GDP[13:24])
GDP_q[3]<-sum(GDP[25:36])
GDP_q[4]<-sum(GDP[37:48])
GDP_q[5]<-sum(GDP[49:60])
GDP_q[6]<-sum(GDP[61:72])
GDP_q[7]<-sum(GDP[73:84])
GDP_q[8]<-sum(GDP[85:96])
GDP_q[9]<-sum(GDP[97:108])

GDP_q=GDP_q/(GDP[1]*12)*100

GDP_qr=array(data=0,dim=c(9,(nItalianRegions-1)))
GDP_qr[1,]<-colSums(GDP_regions[1:12,])
GDP_qr[2,]<-colSums(GDP_regions[13:24,])
GDP_qr[3,]<-colSums(GDP_regions[25:36,])
GDP_qr[4,]<-colSums(GDP_regions[37:48,])
GDP_qr[5,]<-colSums(GDP_regions[49:60,])
GDP_qr[6,]<-colSums(GDP_regions[61:72,])
GDP_qr[7,]<-colSums(GDP_regions[73:84,])
GDP_qr[8,]<-colSums(GDP_regions[85:96,])
GDP_qr[9,]<-colSums(GDP_regions[97:108,])

for(i in 1:(nItalianRegions-1)){
  GDP_qr[,i]=GDP_qr[,i]/(GDP_regions[1,i]*12)*100
}

simvec=c(lossProd_minDT,lossProd_EROM_minDT,lossProd_1y,lossProd_EROM_1y,lossProd,lossProd_EROM,lossExpenditure_q)

 return(list(simvec,prod_sectors,prod_sectors_m,prod_sectors_q,expenditure_q,GDP,GDP2,GDP_regions,GDP_regions2,GDP_q,Cons,Inv,sales,sales_sectors,intensity11,intensity12,intensity21,intensity22,bottlenecks,delivered,capacitySectors,capacityRegions,demandTotal,demandRegions,demandSectors,producibleTotal,producibleRegions,producibleSectors,consTotal,consRegions,consSectors,ratioTotal,ratioRegions,ratioSectors,GDPShocks,outputShocks,outputShocksRegions,outputShocksSectors,ratioShocks,ratioShocksRegions,ratioShocksSectors,outputTotal))

}