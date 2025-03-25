#Rx Fire and Drought analyis
#create data summary tables
#1/27/2017

rm(list = ls())

#=========================================================================
library(tidyverse)
library(xlsx)

#setwd('U:/Team/private/vanMantgem/Projects/SWCSC/Fire_treeMort/8_Analysis/')
setwd('E:/vanMantgem/Projects/SWCSC/Fire_treeMort/8_Analysis/')

dat <- read.csv('DroughtRxMort_AnalysisData_final.csv')

#=========================================================================
#Table 1.  Park-level summaries

pdat <- dat %>%
  group_by(ParkID) %>%
  summarise(
    Plot.cnt = length(unique(PlotID))
    )
pdat

ldat <- dat %>%
  group_by(ParkID) %>%
  summarise(
    Lat = round(mean(Lat),2),
    Long = round(mean(Long),2),
    Elev = round(mean(Elev),0)
  )

pdat <- inner_join(pdat, ldat, by='ParkID')

cdat <- dat %>%
  group_by(ParkID) %>%
  summarise(
    tmin.ref = round(mean(tmin.ref, na.rm=T),1),
    tmax.ref = round(mean(tmax.ref, na.rm=T),1),
    ppt.ref = round(mean(ppt.ref, na.rm=T),0)
  )
pdat <- inner_join(pdat, cdat, by='ParkID')

#tree data
tdat <- dat %>%
  group_by(ParkID) %>%
  summarize(
    abco.live.cnt = length(Status[Species=='ABCO' & Status==0]),
    abco.dead.cnt = length(Status[Species=='ABCO' & Status==1]),
    
    pipo.live.cnt = length(Status[Species=='PIPO' & Status==0]),
    pipo.dead.cnt = length(Status[Species=='PIPO' & Status==1]),
    
    psme.live.cnt = length(Status[Species=='PSME' & Status==0]),
    psme.dead.cnt = length(Status[Species=='PSME' & Status==1])
  )

pdat <- inner_join(pdat, tdat, by='ParkID')

#=========================================================================
#Table 2.  Species-level summaries

se = function(x) {sqrt(var(x, na.rm=T)/length(x))}

sdat <- dat %>%
  group_by(Species, Status) %>%
  summarize(
   Tree.cnt = length(TreeID),
   
   Hegyi.spp = round(mean(Hegyi, na.rm=T),2),
   Hegyi.se = round(se(Hegyi),2),
      
   DBH.spp = round(mean(DBH, na.rm=T),1),
   DBH.se = round(se(DBH),1),
      
   BT.spp = round(mean(BT, na.rm=T),1),
   BT.se = round(se(BT),1),
   
   PCVS.spp = round(mean(CrownScorchPercent, na.rm=T),0),
   PCVS.se = round(se(CrownScorchPercent),0),
   
   MaxCharHt.spp = round(mean(MaxCharHt, na.rm=T),1),
   MaxCharHt.se = round(se(MaxCharHt),1),
   
   Growth.spp = round(mean(gro.10, na.rm=T),1),
   Growth.se = round(se(gro.10),1)
   )
sdat <- data.frame(sdat)


#write to output file
write.xlsx(pdat, "Tables1&2_final.xlsx", sheetName="Table1", append=F)
write.xlsx(sdat, "Tables1&2_final.xlsx", sheetName="Table2", append=T)  
