# Data Envelopment Analysis (DEA) in Latin American and Caribbean (LAC) countries
# Author: Dr. Igor Tona Peres, Professor of Industrial Engineering, PUC-Rio
# igor.peres@puc-rio.br
# Last updated: 07/18/2024
library(tidyverse)
library(deaR)
library(caret)
library(AER)
library(readxl)

setwd("C:\\Users\\igor.peres\\OneDrive - puc-rio.br\\GitHub\\DEA LAC")

#Normalized dataset
data = readxl::read_excel("DEA_dataset.xlsx")

#DEA 2017 - VRS model - Input Oriented
data2017 = data%>%
  filter(Year=="2017")
dea_data_2017 <- read_data(datadea = data2017, inputs = 2:5, outputs = 6:7)
dea_results_io_vrs_2017 <- model_basic(data = dea_data_2017, orientation = "io", rts = "vrs")
summary(dea_results_io_vrs_2017)
# summary(dea_results_io_vrs, exportExcel = TRUE, filename =  "DEA_model2017.csv")


#DEA 2012 - VRS model - Input Oriented
data2012 = data%>%
  filter(Year=="2012")
dea_data_2012 <- read_data(datadea = data2012, inputs = 2:5, outputs = 6:7)
dea_results_io_vrs_2012 <- model_basic(data = dea_data_2012, orientation = "io", rts = "vrs")
summary(dea_results_io_vrs_2012)
# summary(dea_results_io_vrs, exportExcel = TRUE, filename =  "DEA_model2012.csv")


#DEA 2007 - VRS model - Input Oriented
data2007 = data%>%
  filter(Year=="2007")
dea_data_2007 <- read_data(datadea = data2007, inputs = 2:5, outputs = 6:7)
dea_results_io_vrs_2007 <- model_basic(data = dea_data_2007, orientation = "io", rts = "vrs")
summary(dea_results_io_vrs_2007)
# summary(dea_results_io_vrs_2007, exportExcel = TRUE, filename =  "DEA_model2007.csv")


#Linear Regression --------------------------------------------
model_LE = lm(formula = LE ~ HE + HB + PHY + NUR,data=data2017)
summary(model_LE)

model_IMR = lm(formula = IMR ~ HE + HB + PHY + NUR,data=data2017)
summary(model_IMR)


#Tobit Regression ---------------------------------------------
data2017 = data2017%>%
  arrange(Country)

efficiencies = summary(dea_results_io_vrs_2017)
efficiencies = efficiencies%>%
  select(DMU,efficiencies.eff)

data_tobit = inner_join(data2017,efficiencies,by = c("Country"="DMU"))

data_tobit = data_tobit%>%
  mutate(efficiencies.eff = round(efficiencies.eff,3),
         dea_score_adjusted = (1/efficiencies.eff)-1
         )

model_tobit = tobit(formula = dea_score_adjusted ~ HE + HB + PHY + NUR, data = data_tobit, left = 0)
summary(model_tobit)
result_tobit = summary(model_tobit)
result_tobit_coef = result_tobit$coefficients

# write.csv(result_tobit_coef, file = "result_tobit_coef.csv")

