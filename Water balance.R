

#' ---
#' title: "Water balance calculations"
#' author: "Emily Ury"
#' last update: "May 9th, 2022"
#' ---
#' 


#setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Wetland_P_model_data")
setwd("C:/Users/uryem/OneDrive - University of Waterloo/Wetlands_local/Wetland_P_model_data")


library(tidyverse)


pValue <- 0.24697   ## constant for the B-C ET calculation (based on wetland Longitude)

## Call in Temp and calculate ET using the Blaney-Criddle eqauation
ET <- read.csv("ET_Calc.csv") %>%
  select(c("DateTime", "MeanTempC")) %>%
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
  mutate(Dummy_ET = PET_m3_day/5) %>%
  mutate(Date = as.Date(Date))











## calculating DV/DT for OH only


OH19 <- data %>%
  filter(Wetland_ID == "OH") %>%
  filter(Water_year == 2019)
# plot(OH19$Date, OH19$AET_m3_day, type = "l",
#      xlab = "date", ylab = "ET (m3/day)")
# points(OH19$Date, OH19$PET_m3_day, type = "l", col = "red")
# points(OH19$Date, OH19$Dummy_ET, type = "l", col = "blue")
# legend("topleft", c("B-C approx", "TerraClimate", "Dummy data"), lty = 1, col = c("red", "black", "blue"))


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


################################################################ ^save






#### writing a function to calculate V and dV/dt
#Vint <- rep(info$Vol_m3,each = 2)

V.calc <- function(x) {
  V <- rep(0, length(Qin))
  V[1] <- x$Vol_m3[1]
  for(t in 2:length(Qin)) {
    V[t] <- V[t-1] + (Qin[t-1] - Qout[t-1] - ET[t-1])*dt
  }
V}

OH19$Vcalc_m3 <- V.calc(OH19)




OH19$dVdt <- c(0, (V[2:length(V)] - V[1:(length(V)-1)]))
output


data$Wetland_year <- paste(data$Wetland_ID, data$Water_year, sep = "")
array <- split(data, f = data$Wetland_year)


array$BL2019$Vcalc_m3 <- V.calc(array$BL2019)

dVdt <- c(0, (V[2:length(V)] - V[1:(length(V)-1)]))  #m3/day



tapply(OH.Q, 1, V.calc)







########## calculate  change in C over time (for just one wetland-year)






