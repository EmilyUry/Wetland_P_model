

#' ---
#' title: "Water balance calculations"
#' author: "Emily Ury"
#' last update: "May 9th, 2022"
#' ---
#' 


#setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Wetland_P_model_data")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Wetland_P_model_data")


library(tidyverse)



## Call in Temp and calculate ET using the Blaney-Criddle eqauation
ET <- read.csv("ET_Calc.csv") %>%
  select(c("DateTime", "MeanTempC", "pValue")) %>%
  rename(Date = DateTime) %>%
  mutate(PET_mm_day = pValue*(0.457*MeanTempC + 8.128))


## Call in the AET value from TerraClimate for alternate estimate of AET
WB <- read.csv("Water_balance_data.csv") %>%
  select(c("Date", "Month", "Water_year", "AET_mm")) %>%
  left_join(ET, by = "Date") %>%
  mutate(AET_mm_day = AET_mm/30)  ## mm/month --> mm/day


## Call in Wetland area and volume  
info <- read.csv("Wetland_Info.csv") %>%
  rename(Wetland_ID = WETLAND_ID, Area_m2 = Basin.Area..ha., Vol_m3 = Basin.Volume..m3.) %>%
  select(Wetland_ID, Area_m2, Vol_m3)

## Join everything together and calculate ET over wetland area 
data <- read.csv("DU_summary_select.csv") %>%
  select("Wetland_ID", "Water_year", "Month", "Day", "Date", "Precip", "Qin", "Qout", 
         "TPin", "TPout", "SRPin", "SRPout" ) %>%
  left_join(info, by = "Wetland_ID") %>%
  left_join(WB, by = c("Date", "Month", "Water_year")) %>%
  mutate(PET_m3_day = PET_mm_day/1000*Area_m2*10000) %>%
  mutate(AET_m3_day = AET_mm_day/1000*Area_m2*10000) %>%
  mutate(Dummy_ET = PET_m3_day/4) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(Site_year = paste(Wetland_ID, Water_year, sep = ""))

 rm(WB); rm(ET); rm(info)



## SKIP THIS #####
 ########calculating DV/DT for OH only
{

OH19 <- data %>%
  filter(Wetland_ID == "OH") %>%
  filter(Water_year == 2019)
plot(OH19$Date, OH19$AET_m3_day, type = "l",
     xlab = "date", ylab = "ET (m3/day)")
points(OH19$Date, OH19$PET_m3_day, type = "l", col = "red")
points(OH19$Date, OH19$Dummy_ET, type = "l", col = "blue")
legend("topleft", c("B-C approx", "TerraClimate", "Dummy data"), lty = 1, col = c("red", "black", "blue"))


Qin <- OH19$Qin
Qout <- OH19$Qout
ET  <- OH19$Dummy_ET
ET[is.na(ET)] <- 0   ## replace NA with zero
dt <- 1
V <- rep(0, length(Qin))  # makes a vector of all zeros
V[1] <- OH19$Vol_m3[1]    # replace the first zero w volume at T1

for(t in 2:length(Qin)) {
  V[t] <- V[t-1] + (Qin[t-1] - Qout[t-1] - ET[t-1])*dt
}
dVdt <- c(0, (V[2:length(V)] - V[1:(length(V)-1)]))  #m3/day

plot(OH19$Date, V, type = 'l')
plot(OH19$Date, dVdt, type = 'l')

OH19$V_calc <- V
OH19$dVdt <- dVdt
}

################################################################ ^save



#### writing a function to calculate V and dV/dt


##### create a function for calculating Volume at the daily timestep
V.calc <- function(x) {
  Qin <- x$Qin
  Qout <- x$Qout
  ET  <- x$Dummy_ET
  ET[is.na(ET)] <- 0   ## replace NA with zero
  dt <- 1
  V <- rep(0, length(Qin))  # makes a vector of all zeros
  V[1] <- x$Vol_m3[1]
  V <- rep(0, length(Qin))
  V[1] <- x$Vol_m3[1]
  for(t in 2:length(Qin)) {
    V[t] <- V[t-1] + (Qin[t-1] - Qout[t-1] - ET[t-1])*dt
  }
V}


### apply function over each site-year, one at a time

Site_year <- unique(data$Site_year) ## create a vector of Site-year names
output <- list()
for(i in 1:16){
  df <- data[which(data$Site_year == Site_year[i]),]
  V <- V.calc(df)
  df$Vcalc_m3 <- V   # add V (calculate volume in m3 as a new variable)
  df$dVdt <- c(0, (V[2:length(V)] - V[1:(length(V)-1)]))  #calculate dV/dt and as a new variable
  output[[i]] <- df
  }
data2 <- do.call("rbind", output)  ## combine dataframes for each site-year back into one dataframe


### plot the wetland volumes for all Site-years
ggplot(data2, (aes( x = Date, y = Vcalc_m3))) +
  geom_line()+
  facet_wrap(.~Site_year, scales = "free" )

### plot the wetland volumes for all Site-years
ggplot(data2, (aes( x = Date, y = dVdt))) +
  geom_line()+
  facet_wrap(.~Site_year, scales = "free" )


#### START HERE with Concentration prediction













