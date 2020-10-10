library(dplyr)
library(readr)
library(compare)
library(forcats)
library(lubridate)
library(chron)
library(stringdist)
library(naniar)
library(DataCombine)
library(tidyr)
library(data.table)


# --------------------- Importação dos bancos --------------------- #

# Importa banco Cognos VIT e REF_MUN
data <- "04-10"
cognos_vit <- read.csv2("~/Dropbox/Base OESP/04-10_FATO_VIT.csv")
cognos_aut <- read.csv2("~/Dropbox/Base OESP/04-10_FATO_AUT.csv")

# Importa banco Completo
completo_vit <- read.csv("~/Dropbox/Base OESP/Base_R/Base_R/new_completo_vit_04-10.csv",
                         header = T, stringsAsFactors = FALSE)
completo_aut <- read.csv("~/Dropbox/Base OESP/Base_R/Base_R/new_completo_aut_04-10.csv",
                         header = T, stringsAsFactors = FALSE)

# Importa referência dos Municípios:
ref_mun <- read.csv("~/Dropbox/Base OESP/Base_R/Base_R/ref_mun.csv", 
                    header = T, stringsAsFactors = FALSE)
ref_bairro <- read.csv2("~/Dropbox/Base OESP/Base_R/Base_R/ref_bairro.csv", 
                       header = T, stringsAsFactors = FALSE)
ref_bairro <- ref_bairro[,2:5]

#Importa diária
diaria <- read.csv("~/Dropbox/Base OESP/Base_R/Base_R/diaria_04_10.csv",
                    header = T, stringsAsFactors = FALSE)

source("script_vit.R")
source("script_aut.R")

# Confere

#Pq o confere não vem com tds os municipios???
confere <- vit %>% 
  group_by(ANO_FATO, MES_FATO, MUN) %>% 
  mutate(n_oc = n()) %>% 
  select(MUN, ANO_FATO, MES_FATO, n_oc) %>% 
  mutate(MES_FATO = fct_collapse(MES_FATO, Jan = "Janeiro", 
                                 Fev = "Fevereiro", 
                                 Mar = "Março", 
                                 Mai = "Maio", 
                                 Abr = "Abril", 
                                 Jun = "Junho", 
                                 Jul = "Julho", 
                                 Ago = "Agosto", 
                                 Set = "Setembro", 
                                 Out = "Outubro",
                                 Nov = "Novembro", 
                                 Dez = "Dezembro")) %>% 
  mutate(MES_ANO = paste0(MES_FATO, "_", ANO_FATO)) %>% 
  ungroup() %>% 
  select(-ANO_FATO, -MES_FATO)

confere <- unique(confere) 

confere <- confere %>% 
              pivot_wider(names_from = "MES_ANO", values_from = "n_oc") %>% 
              select(-Nov_2019, -Dez_2019) %>% 
              arrange(MUN) %>% 
              replace(is.na(.), 0)
              
mun <- data.frame(MUN = ref_mun$MUN) 
mun <- setDT(mun)[!confere$MUN, on = names(mun)]

matrix_mun <- data.frame(matrix(, nrow = dim(mun)[1], ncol = dim(confere)[2]))
matrix_mun$X1 <- mun$MUN
colnames(matrix_mun) <- dimnames(confere)[[2]] 

matrix_mun <- matrix_mun %>% 
  replace(is.na(.), 0)

confere <- rbind(confere, matrix_mun)

confere <- confere %>% 
              arrange(MUN) %>% 
              mutate(Total = rowSums(.[2:21])) %>% 
              bind_rows(summarise_all(., funs(if(is.numeric(.)) sum(.) else "Rio Grande do Sul(O,V)")))

diaria <- diaria %>% 
  rename(dimnames(confere)[[2]])

#comparar confere com diaria

# Exportar para .csv

write.csv2(vit, paste0("new_completo_vit_", data, ".csv", sep = ""))
write.csv2(dash_vit, paste("dashboard_vit_", data, ".csv", sep=""))

write.csv2(aut, paste0("new_completo_aut_", data, ".csv", sep = ""))
write.csv2(dash_aut, paste("dashboard_aut_", data, ".csv", sep=""))