


#' ---
#' title: "DU data work-up"
#' author: "Emily Ury"
#' last update: "May 5th, 2022"
#' ---
#' 


setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Wetland_P_model_data")

library(tidyverse)


x <- read.csv("raw_data_all.csv", head = TRUE)

#######################

## data set-up
x$Water_year <- as.factor(x$Water_year)
x$Date <- as.Date(x$Date)
x$Wetland_ID <- ordered(x$Wetland_ID, levels = c("MO", "BL", "MA", "KE", "DY", "LL", "FE", "OH"))
## merge multiple inflows and outflows for volume and solutes
##
###### inflows = Inlet + Surface + Tile + Precip    ## at MA, inlet = culvert
###### outflows = Outflow + Outflow Leak (FE and MA only (MA is a spillway))

flows.combine <- x %>%
  pivot_wider(names_from = Station, 
              values_from = c(Flow_volume_m3_day, TP_kg_day, TDP_kg_day, SRP_kg_day, PP_kg_day,
                              TKN_kg_day, DKN_kg_day, NO3_kg_day, NO2_kg_day, NH3_kg_day,
                              TN_kg_day, TDN_kg_day, PN_kg_day, DIN_kg_day)) %>%
  rowwise() %>%
  mutate(Precip = Flow_volume_m3_day_Inflow_Precipitation) %>%
  mutate(Qin = sum(Flow_volume_m3_day_Inflow_Inlet, Flow_volume_m3_day_Inflow_Tile, Flow_volume_m3_day_Inflow_Surface, 
                      Flow_volume_m3_day_Inflow_Precipitation, na.rm = TRUE)) %>%
  mutate(Qout = sum(Flow_volume_m3_day_Outflow, Flow_volume_m3_day_Outflow_Leak, na.rm = TRUE)) %>%
  mutate(TPin = sum(TP_kg_day_Inflow_Inlet, TP_kg_day_Inflow_Tile, TP_kg_day_Inflow_Surface,
                     TP_kg_day_Inflow_Precipitation, na.rm = TRUE)) %>%
  mutate(TPout = sum(TP_kg_day_Outflow, TP_kg_day_Outflow_Leak, na.rm = TRUE)) %>%
  mutate(SRPin = sum(SRP_kg_day_Inflow_Inlet, SRP_kg_day_Inflow_Tile, SRP_kg_day_Inflow_Surface,
                      SRP_kg_day_Inflow_Precipitation, na.rm = TRUE)) %>%
  mutate(SRPout = sum(SRP_kg_day_Outflow, SRP_kg_day_Outflow_Leak, na.rm = TRUE)) %>%
  mutate(TNin = sum(TN_kg_day_Inflow_Inlet, TN_kg_day_Inflow_Tile, TN_kg_day_Inflow_Surface,
                     TN_kg_day_Inflow_Precipitation, na.rm = TRUE)) %>%
  mutate(TNout = sum(TN_kg_day_Outflow, TN_kg_day_Outflow_Leak, na.rm = TRUE)) %>%
  mutate(NO3in = sum(NO3_kg_day_Inflow_Inlet, NO3_kg_day_Inflow_Tile, NO3_kg_day_Inflow_Surface,
                     NO3_kg_day_Inflow_Precipitation, na.rm = TRUE)) %>%
  mutate(NO3out = sum(NO3_kg_day_Outflow, NO3_kg_day_Outflow_Leak, na.rm = TRUE)) 


summary_select <- flows.combine %>%
  select(Wetland_ID, Water_year, Month,   Day, Date, 
         Precip,   Qin,  Qout,  TPin,  TPout,  SRPin, SRPout,  TNin, TNout, NO3in, NO3out)

##write.csv(summary_select, "DU_summary_select.csv")


flow.select <- flows.combine %>%
  select(Wetland_ID, Water_year, Month,   Day, Date, 
         Precip,   Qin,  Qout)  %>%
  pivot_wider(names_from = Wetland_ID, values_from = c(Precip, Qin, Qout)) # %>%

write.csv(flow.select, "Water_balance_data.csv")












### calcualte mass removal, percent removal and concentration (mg/L)
rem.calcs <- flows.combine %>% 
  rowwise() %>%
  mutate(flow.atten = sum(VOL.IN, -VOL.OUT, na.rm = TRUE)) %>%
  mutate(flow.atten.percent = flow.atten/VOL.IN*100) %>%
  mutate(TP.rem = sum(TP.IN, -TP.OUT, na.rm = TRUE)) %>%
  mutate(TP.rem.percent = TP.rem/TP.IN*100) %>%
  mutate(TDP.rem = sum(TDP.IN, -TDP.OUT, na.rm = TRUE)) %>%
  mutate(TDP.rem.percent = TDP.rem/TDP.IN*100) %>%
  mutate(SRP.rem = sum(SRP.IN, -SRP.OUT, na.rm = TRUE)) %>%
  mutate(SRP.rem.percent = SRP.rem/SRP.IN*100) %>%
  mutate(PP.rem = sum(PP.IN, -PP.OUT, na.rm = TRUE)) %>%
  mutate(PP.rem.percent = PP.rem/PP.IN*100) %>%
  mutate(TP.conc.IN = TP.IN/VOL.IN*1000) %>%
  mutate(TP.conc.OUT = TP.OUT/VOL.OUT*1000) %>%
  mutate(TDP.conc.IN = TDP.IN/VOL.IN*1000) %>%
  mutate(TDP.conc.OUT = TDP.OUT/VOL.OUT*1000) %>%
  mutate(SRP.conc.IN = SRP.IN/VOL.IN*1000) %>%
  mutate(SRP.conc.OUT = SRP.OUT/VOL.OUT*1000) %>%
  mutate(PP.conc.IN = PP.IN/VOL.IN*1000) %>%
  mutate(PP.conc.OUT = PP.OUT/VOL.OUT*1000) 



#### add site info
aux.d <- read.csv("Wetland_Info.csv", head = TRUE)
names(aux.d) <- c("Wetland_ID", "Name", "Established", "Area", "Volume", "CA",
                  "CA_WA", "Crop18", "Crop19", "Crop20", "Crop21", "SOM", "SBP", "UBP")
aux.d$Wetland_ID <- ordered(aux.d$Wetland_ID, levels = c("MO", "BL", "MA", "KE", "DY", "LL", "FE", "OH"))

data <- rem.calcs  %>%
  left_join(aux.d, by = "Wetland_ID")

subset <- data %>%
  filter(Wetland_ID == "OH" | Wetland_ID == "MA" |
           Wetland_ID == "KE" | Wetland_ID == "FE")

Y1 <- data[which(data$Water_year == "2019"),]
Y2 <- data[which(data$Water_year == "2021"),]



