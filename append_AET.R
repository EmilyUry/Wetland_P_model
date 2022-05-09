

### append AET to water balance data sheet

setwd("C:/Users/Emily Ury/OneDrive - University of Waterloo/Wetlands_local/Wetland_P_model_data")

library(tidyverse)


WB <- read.csv("Water_balance_data.csv")
AET <- read.csv("TerraClimate_AET_region.csv") %>%
  select(-c("Date"))



output <- AET %>%
  left_join(WB, by = c("Water_year", "Month"))



write.csv(output, "Water_balance_data.csv")
