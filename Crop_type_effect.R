setwd

GISWRRLPath <- ""
GISWRRLName <- ""
filepath0 <- paste(GISWRRLPath, GISWRRLName, ".csv", sep="")
dfsites_all <- read.csv(filepath0)

library(gamlss)
library(parameters)

#names(dfsites_all)

#complete model

dfsites_all <- read.csv(filepath0)
dfsites_all <- dfsites_all[dfsites_all$Status != "AWB" ,] 

dfsites_all$Status <- as.factor(dfsites_all$Status)
dfsites_all$Year_Perlodes <- as.factor(dfsites_all$Year_Perlodes)
dfsites_all$MZB_Typ1 <- as.factor(dfsites_all$MZB_Typ1)
dfsites_all$Federal_State <- as.factor(dfsites_all$Federal_State)
dfsites_all$Datum_Phylib <- as.factor(dfsites_all$Datum_Phylib)
#names(dfsites_all)
#2018
dfsites_all$Cereals1     <- dfsites_all$Cat_WWH_18 +dfsites_all$Cat_WBA_18 + dfsites_all$Cat_SBA_18 + dfsites_all$Cat_OWC_18 + dfsites_all$Cat_OSC_18 +dfsites_all$Cat_SOA_18 + dfsites_all$Cat_WRY_18
dfsites_all$Intensive_Cereals <- dfsites_all$Cat_WWH_18 +dfsites_all$Cat_WBA_18 + dfsites_all$Cat_SBA_18 + dfsites_all$Cat_OWC_18 + dfsites_all$Cat_OSC_18
dfsites_all$Extensive_Cereals <- dfsites_all$Cat_SOA_18 + dfsites_all$Cat_WRY_18 
dfsites_all$Permanent_crops   <- dfsites_all$Cat_GRP_18 + dfsites_all$Cat_ORC_18 + dfsites_all$Cat_HOP_18
dfsites_all$Oilseeds       <- dfsites_all$Cat_WAR_18 + dfsites_all$Cat_SUN_18
dfsites_all$Maize       <- dfsites_all$Cat_MAG_18 + dfsites_all$Cat_MAI_18
dfsites_all$Grassland   <- dfsites_all$Cat_GRA_18
dfsites_all$Vegetables  <- dfsites_all$Cat_SUB_18 + dfsites_all$Cat_POT_18 + dfsites_all$Cat_CAR_18 + dfsites_all$Cat_ASP_18 + dfsites_all$Cat_ONI_18 + dfsites_all$Cat_STR_18 + dfsites_all$Cat_LEG_18 + dfsites_all$Cat_OTV_18
dfsites_all$Forest        <- dfsites_all$CatCropWald + dfsites_all$Cat_SWF_18
dfsites_all$Urban       <- dfsites_all$CatCrop11P

#

#2017
dfsites_all$Cereals1     <- dfsites_all$Cat_WWH_17 +dfsites_all$Cat_WBA_17 + dfsites_all$Cat_SBA_17 + dfsites_all$Cat_OWC_17 + dfsites_all$Cat_OSC_17 +dfsites_all$Cat_SOA_17 + dfsites_all$Cat_WRY_17
dfsites_all$Intensive_Cereals <- dfsites_all$Cat_WWH_17 +dfsites_all$Cat_WBA_17 + dfsites_all$Cat_SBA_17 + dfsites_all$Cat_OWC_17 + dfsites_all$Cat_OSC_17
dfsites_all$Extensive_Cereals <- dfsites_all$Cat_SOA_17 + dfsites_all$Cat_WRY_17 
dfsites_all$Permanent_crops   <- dfsites_all$Cat_GRP_17 + dfsites_all$Cat_ORC_17 + dfsites_all$Cat_HOP_17
dfsites_all$Oilseeds       <- dfsites_all$Cat_WRA_17 + dfsites_all$Cat_SUN_17
dfsites_all$Maize       <- dfsites_all$Cat_MAG_17 + dfsites_all$Cat_MAI_17
dfsites_all$Grassland   <- dfsites_all$Cat_GRA_17
dfsites_all$Vegetables  <- dfsites_all$Cat_SUB_17 + dfsites_all$Cat_POT_17 + dfsites_all$Cat_CAR_17 + dfsites_all$Cat_ASP_17 + dfsites_all$Cat_ONI_17 + dfsites_all$Cat_STR_17 + dfsites_all$Cat_LEG_17 + dfsites_all$Cat_OTV_17
dfsites_all$Forest        <- dfsites_all$CatCropWald + dfsites_all$Cat_SWF_17
dfsites_all$Urban       <- dfsites_all$CatCrop11P

#dfsites1
dfsites1 <- dfsites_all[complete.cases(dfsites_all$Rivertype),]

#please run them after each other cause they overwrite

#invertebrates
dfsites1<- na.omit(dfsites1[c("Cereals1","Intensive_Cereals","Extensive_Cereals","Permanent_crops",
                              "Oilseeds","Maize","Grassland","Vegetables","Forest","Urban", "AD_Ergebnis_HMWB", "Rivertype", "MZB_Typ1", "Year_Perlodes","Federal_State","Status")])
#diatoms
dfsites1<- na.omit(dfsites1[c("Cereals1","Intensive_Cereals","Extensive_Cereals","Permanent_crops",
                              "Oilseeds","Maize","Grassland","Vegetables","Forest","Urban", "Diatomeen_Index", "Rivertype", "MZB_Typ1", "Datum_Phylib","Federal_State","Status")])
#macrophytes
dfsites1<- na.omit(dfsites1[c("Cereals1","Intensive_Cereals","Extensive_Cereals","Permanent_crops",
                              "Oilseeds","Maize","Grassland","Vegetables","Forest","Urban", "Index.Makrophyten", "Rivertype", "MZB_Typ1", "Datum_Phylib","Federal_State","Status")])


dfsites1$Cereals1 <- dfsites1$Cereals1/100
dfsites1$Intensive_Cereals <- dfsites1$Intensive_Cereals/100
dfsites1$Extensive_Cereals <- dfsites1$Extensive_Cereals/100
dfsites1$Permanent_crops <- dfsites1$Permanent_crops/100
dfsites1$Oilseeds <- dfsites1$Oilseeds/100
dfsites1$Maize <- dfsites1$Maize/100
dfsites1$Grassland <- dfsites1$Grassland/100
dfsites1$Vegetables <- dfsites1$Vegetables/100
dfsites1$Forest <- dfsites1$Forest/100
dfsites1$Urban <- dfsites1$Urban/100

#invertebrates
mod1_inv <-gamlss(AD_Ergebnis_HMWB~Permanent_crops+Vegetables+Maize+Intensive_Cereals+Oilseeds+
                Extensive_Cereals+Grassland+Urban+Forest+random(MZB_Typ1) + random(Year_Perlodes) + random(Federal_State) + random(Status), 
              data=dfsites1, family =BEINF)


#diatoms
mod1_dia <-gamlss(Diatomeen_Index~Permanent_crops+Vegetables+Maize+Intensive_Cereals+Oilseeds+
                Extensive_Cereals+Grassland+Urban+Forest+random(MZB_Typ1) + random(Datum_Phylib) + random(Federal_State) + random(Status), 
              data=dfsites1, family =BEINF)
#macrophytes
mod1_mac <-gamlss(Index.Makrophyten~Permanent_crops+Vegetables+Maize+Intensive_Cereals+Oilseeds+
                Extensive_Cereals+Grassland+Urban+Forest+random(MZB_Typ1) + random(Datum_Phylib) + random(Federal_State) + random(Status), 
              data=dfsites1, family =BEINF)

#plot invertebrates
tiff("", units="in", width=9.4, height=7.3, res=500)

plot(model_parameters(mod1_inv),  show_labels = TRUE, size_text =5, size_point = 1)
dev.off()
#plot diatoms

tiff("", units="in", width=9.4, height=7.3, res=500)

plot(model_parameters(mod1_dia),  show_labels = TRUE, size_text =5, size_point = 1)
dev.off()

#plot macrophytes
tiff("", units="in", width=9.4, height=7.3, res=500)

plot(model_parameters(mod1_mac),  show_labels = TRUE, size_text =5, size_point = 1)
dev.off()


####################### reduced model run them separately as they overwrite

names(dfsites1)
dfsites1 <- dfsites_all[complete.cases(dfsites_all$Rivertype),]
#invertebrates
dfsites1<- na.omit(dfsites1[c("Cat_Agri_without_grassl","Cat_GRA_17","CatCropWald","Cat_SWF_17","CatCrop11P",
                              "AD_Ergebnis_HMWB", "Rivertype", "MZB_Typ1", "Year_Perlodes","Federal_State","Status")])

#macrophytes
dfsites1<- na.omit(dfsites1[c("Cat_Agri_without_grassl","Cat_GRA_17","CatCropWald","Cat_SWF_17","CatCrop11P",
                              "Index.Makrophyten", "Rivertype", "MZB_Typ1", "Datum_Phylib","Federal_State","Status")])

#diatoms
dfsites1<- na.omit(dfsites1[c("Cat_Agri_without_grassl","Cat_GRA_17","CatCropWald","Cat_SWF_17","CatCrop11P",
                              "Diatomeen_Index", "Rivertype", "MZB_Typ1", "Datum_Phylib","Federal_State","Status")])


dfsites1$Forest        <- dfsites1$CatCropWald + dfsites1$Cat_SWF_17
dfsites1$Urban       <- dfsites1$CatCrop11P

dfsites1$Cat_Agri_without_grassl <- dfsites1$Cat_Agri_without_grassl/100
dfsites1$Cat_GRA_17 <- dfsites1$Cat_GRA_17/100
dfsites1$Forest <- dfsites1$Forest/100
dfsites1$Urban <- dfsites1$Urban/100


#Invertebrates
mod1_inv_red <-gamlss(AD_Ergebnis_HMWB~Cat_Agri_without_grassl+Cat_GRA_17+Forest+Urban+
                +random(MZB_Typ1) + random(Year_Perlodes) + random(Federal_State) + random(Status), 
              data=dfsites1, family =BEINF)
#macrophytes
mod1_mac_red <-gamlss(Index.Makrophyten~Cat_Agri_without_grassl+Cat_GRA_17+Forest+Urban+
                +random(MZB_Typ1) + random(Datum_Phylib) + random(Federal_State) + random(Status), 
              data=dfsites1, family =BEINF)
#diatoms
mod1_dia_red <-gamlss(Diatomeen_Index~Cat_Agri_without_grassl+Cat_GRA_17+Forest+Urban+
                +random(MZB_Typ1) + random(Datum_Phylib) + random(Federal_State) + random(Status), 
              data=dfsites1, family =BEINF)


parameters(mod1_inv_red)
parameters(mod1_mac_red)
parameters(mod1_dia_red)
