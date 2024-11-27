# importation de spackage por la manipulation des données-----
 library(tidyverse)

# DAATAAAAAAATTTTTTTTTTTTTTTTTAAAAAAAAAAAAAAAAA
data = read.csv("data_Thesis_I.csv")
data |> str()#structure
data |> skimr::skim()
data |> dlookr::normality()

## COMMMMMEEENNNNTTTTT PROCEDONS NOUS ALORS??????
### Abondance et divrsite globales (ecosystem)
### Abondance et indices pour chaque unités

