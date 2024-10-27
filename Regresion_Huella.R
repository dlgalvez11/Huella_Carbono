setwd("Dirección")
options(scipen=0)

library(dplyr)
library(vtable)
library(skimr)
library(psych)
library(Hmisc)
library(haven)
library(openxlsx)
library(tidyverse)
library(ggplot2)
library(ggpubr) 
library(sjPlot)
library(GGally)
library(gridExtra)
library(corrplot)
library(car)
library(ggplot2)
library(plot3D)
library(plot3Drgl)
library(palmerpenguins)
library(parameters)
library(ggstatsplot)
library(olsrr)
library(dplyr)
library(plyr)
library(ggpubr)
library(Hmisc)
library(purrr)
library(effects)
library(gridExtra)


T0_Calefaccion <- read.xlsx("T0_Calefaccion.xlsx")
T1_Calefaccion <- read.xlsx("T11_Calefaccion.xlsx")
T2_Calefaccion <- read.xlsx("T2_Calefaccion.xlsx")

T0_Energia <- read.xlsx("T0_Energia.xlsx")
T1_Energia <- read.xlsx("T11_Energia.xlsx")
T2_Energia <- read.xlsx("T2_Energia.xlsx")


######### CALEFACCION
Calefaccion_T0_v2 <- lm(formula = T0_Calefaccion ~ Distancia.Euclidiana + Temperatura.promedio + 
                          Mts.Cuadrados.Vivienda + N.personas.hogar + Act_Aislacion_S + 
                          Albanileria + Madera + Electrica + Electrica_Gas + Electrica_Otro + 
                          Gas + Gas_Otro + Lena + Lena_Parafina + Otro + Parafina + 
                          Parafina_Electrica + Parafina_Electrica_Otro, data = T0_Calefaccion)
summary(Calefaccion_T0_v2)
vif_valores <- vif(Calefaccion_T0_v2)
print(vif_valores)


Calefaccion_T1_v2 <- lm(formula = T11_Calefaccion ~ Densidad.barrio + Distancia.Network + 
                          Distancia.Euclidiana + Renta_C1 + Renta_C3 + Mts.Cuadrados.Vivienda + 
                          N.personas.hogar + Madera + Lena + Lena_Electrica + Lena_Electrica_Gas + 
                          Lena_Electrica_Otro + Lena_Gas + Lena_Parafina + Lena_Parafina_Electrica +
                          Lena_Parafina_Electrica_Gas + Lena_Parafina_Gas + Lena_Petroleo + Parafina_Electrica + Petroleo, data = T1_Calefaccion)

summary(Calefaccion_T1_v2)
vif_valores <- vif(Calefaccion_T1_v2)
print(vif_valores)



###### ENERGIA 
Energia_T0_v2 <- lm(formula = T0_Electricidad ~ Renta_C3 + Renta_E + Temperatura.promedio + Mts.Cuadrados.Vivienda + 
                      N.personas.hogar + Act_Calefactores_Si + Madera + Lena_Otro + Petroleo + Electrica +
                      Parafina_Electrica + Carbon_Gas, data = T0_Energia)
summary(Energia_T0_v2)
vif_valores <- vif(Energia_T0_v2)
print(vif_valores)


Energia_T1_v2 <- lm(formula = T11_Electricidad ~ Distancia.Network + Distancia.Euclidiana + 
                      Renta_total + Renta_C1 + Renta_C2 + Temperatura.promedio + 
                      Mts.Cuadrados.Vivienda + N.personas.hogar + Electrica + Electrica_Gas + Lena_Carbon +
                      Lena_Electrica_Otro + Lena_Gas + Lena_Parafina + Lena_Parafina_Electrica + Parafina_Electrica +
                      Petroleo, data = T1_Energia)
summary(Energia_T1_v2)
vif_valores <- vif(Energia_T1_v2)
print(vif_valores)


