# --------------------- Arrumando as horas --------------------- #


to.times <- function(x) times(paste0(x, ":00"))

cognos_aut$Hora.Fato <- to.times(cognos_aut$Hora.Fato)
cognos_aut$Hora.Registro <- to.times(cognos_aut$Hora.Registro)

completo_aut$HORA_COM <- times(completo_aut$HORA_COM)
completo_aut$H_FATO <- times(completo_aut$H_FATO)

# --------------------- Padronização das colunas das bases --------------------- #

# Alterar o nome das variáveis no banco Cognos com referẽncia ao banco completo:
# Base Autor:

colnames(cognos_aut)[which(colnames(cognos_aut) == "Participacao")] = "TIPO_PARTIC"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Descr.Fato")] = "TIP_FATO"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Nro.Ocor")] = "NUM_OCOR"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Orgão.Registro")] = "NUM_ORG_REGIST"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Flagrante")] = "FLAGRANTE"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Data.Fato")] = "DATA_FATO"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Hora.Fato")] = "H_FATO"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Município")] = "MUN"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Codigo.Fato")] = "NUM_FATO"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Tipo.Fato")] = "TENTATIVA"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Bairro")] = "BAIRRO"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Ano.Registro")] = "ANO_REGIST"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Data.Registro")] = "DATA_COM" # INCERTO
colnames(cognos_aut)[which(colnames(cognos_aut) == "Hora.Registro")] = "HORA_COM" # INCERTO
colnames(cognos_aut)[which(colnames(cognos_aut) == "Latitude")] = "LAT"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Longitude")] = "LNG"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Nome")] = "N_ACU"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Rg")] = "RG_ACU"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Sexo")] = "SX_ACU"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Data.Nascto")] = "DN_ACU"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Idade.Participante")] = "ID_ACU"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Cor.Pele")] = "COR_ACU"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Estado.Civil")] = "EC_ACU"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Condicao.Fisica")] = "COND_FIS_ACU"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Profissao")] = "PROF_ACU"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Dia.Semana.Fato")] = "D_SEMANA"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Instrucao.Participante")] = "ESC_ACU"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Historico.Ocor")] = "HIST"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Endereço")] = "LOG"
colnames(cognos_aut)[which(colnames(cognos_aut) == "Nro.Endereço")] = "NUM"

# --------------------- XXXXXXXXXXXXXX --------------------- #

# Arrumar a data fato, data registro e data de nascimento

# Autor:

cognos_aut$DN_ACU <- gsub("\\s[^ ]+", "", cognos_aut$DN_ACU) # removendo a hora da data
cognos_aut$DATA_FATO <- gsub("\\s[^ ]+", "", cognos_aut$DATA_FATO) # removendo a hora da data
cognos_aut$DATA_COM <- gsub("\\s[^ ]+", "", cognos_aut$DATA_COM) # removendo a hora da data

cognos_aut$DN_ACU <- format(as.Date(cognos_aut$DN_ACU),'%d/%m/%Y')
cognos_aut$DATA_COM <- format(as.Date(cognos_aut$DATA_COM),'%d/%m/%Y')
cognos_aut$DATA_FATO <- format(as.Date(cognos_aut$DATA_FATO),'%d/%m/%Y')

# Checkando:
#cognos_aut$DN_ACU
#cognos_aut$DATA_COM
#cognos_aut$DATA_FATO

# Mudar as variáves Flagrante e Tipo FATO
cognos_aut$FLAGRANTE <- cognos_aut$FLAGRANTE %>%
  fct_collapse("Não" = "Sem Flagrante",
               "Sim" = "Com Flagrante")

#cognos_aut$TENTATIVA <- cognos_aut$TENTATIVA %>%
#  fct_collapse("Não" = "Consumado",
#               "Sim" = "Tentativa")

# --------------------- XXXXXXXXXXXXXX --------------------- #


# Criar variavel "COD_OC" nos bancos Cognos

cognos_aut$ANO_FATO <- format(as.Date(cognos_aut$DATA_FATO, format = "%d/%m/%Y"), format = "%Y")
cognos_aut$COD_OC <- paste0(cognos_aut$NUM_ORG_REGIST, "_", cognos_aut$NUM_OCOR, "_",
                            cognos_aut$ANO_FATO, sep = "")

completo_aut$COD_OC <- paste0(completo_aut$NUM_ORG_REGIST, "_", completo_aut$NUM_OCOR, "_",
                              completo_aut$ANO_FATO, sep = "")

# Criar variavel codigo AUTOR

cognos_aut <- cognos_aut %>%
  group_by(COD_OC) %>%
  mutate(COD_ACU = paste0("ACU", formatC(1:n(), width = 2, flag = "0")))%>%
  ungroup()

completo_aut <- completo_aut %>%
  group_by(COD_OC) %>%
  mutate(COD_ACU = paste0("ACU", formatC(1:n(), width = 2, flag = "0")))%>%
  ungroup()

# Criar variável que irá unir as bases:
cognos_aut$COD <- paste0(cognos_aut$COD_OC, "_", cognos_aut$COD_ACU, sep = "")
completo_aut$COD <- paste0(completo_aut$COD_OC, "_", completo_aut$COD_ACU, sep = "")

completo_aut <- completo_aut[,-c(which(colnames(completo_aut) == "COD_OC"),
                                 which(colnames(completo_aut) == "COD_ACU"),
                                 which(colnames(completo_aut) == "ANO_FATO"))]

# Seleciona apenas as colunas no Completo que não tem no Cognos
completo_aut_sem_cognos <- completo_aut[, -which(names(completo_aut) %in% names(cognos_aut))]
completo_aut_sem_cognos$COD <- completo_aut$COD

# Une os dois com "_join" (chave - COD) - cria um com FATO+AUTOR
#checkpoint 

aut <- left_join(cognos_aut, completo_aut_sem_cognos, by = "COD")

# --------------------- XXXXXXXXXXXXXX --------------------- #
aut <- aut %>%
  select(-NC)

aut <- aut %>%
  mutate(H_FATO_1 = paste0(hours(H_FATO),":00:00")) %>%
  mutate(T_FATO = ifelse(hours(H_FATO) > 0 & hours(H_FATO) <= 6, "Madrugada",
                  ifelse(hours(H_FATO) > 6 & hours(H_FATO) <= 12, "Manhã",
                  ifelse(hours(H_FATO) > 12 & hours(H_FATO) <= 18, "Tarde",
                  ifelse(hours(H_FATO) > 18, "Noite",
                  ifelse(hours(H_FATO) == 0, "Noite", " ")))))) %>%
  mutate(D_SEMANA = c("Domingo", "Segunda", "Terça", "Quarta", "Quinta",
                      "Sexta", "Sábado")[as.POSIXlt(DATA_FATO, format = "%d/%m/%Y")$wday+1]) %>%
  mutate(MES_FATO = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto",
                      "Setembro", "Outubro", "Novembro", "Dezembro")[as.POSIXlt(DATA_FATO, format = "%d/%m/%Y")$mon+1]) %>%
  mutate(FE_ACU = ifelse(ID_ACU < 12, "Menor de 12 anos",
                  ifelse(ID_ACU >= 12 & ID_ACU <= 17, "12 a 17 anos",
                  ifelse(ID_ACU >= 18 & ID_ACU <= 24, "18 a 24 anos",
                  ifelse(ID_ACU >= 25 & ID_ACU <= 29, "25 a 29 anos",
                  ifelse(ID_ACU >= 30 & ID_ACU <= 34, "30 a 34 anos",
                  ifelse(ID_ACU >= 35 & ID_ACU <= 39, "35 a 39 anos",
                  ifelse(ID_ACU >= 40 & ID_ACU <= 44, "40 a 44 anos",
                  ifelse(ID_ACU >= 45 & ID_ACU <= 49, "45 a 49 anos",
                  ifelse(ID_ACU >= 50 & ID_ACU <= 54, "50 a 54 anos",
                  ifelse(ID_ACU >= 55 & ID_ACU <= 59, "55 a 59 anos ",
                  ifelse(ID_ACU >= 60, "a partir de 60 anos", NA)))))))))))) %>%
  mutate(FE_ACU = ifelse(is.na(FE_ACU), "Não informado", FE_ACU)) %>%
  mutate(PERF_ACU = ifelse(FE_ACU == "Menor de 12 anos" | FE_ACU == "12 a 17 anos", "Criança e adolescente",
                    ifelse(FE_ACU == "18 a 24 anos" | FE_ACU == "25 a 29 anos", "Jovem",
                    ifelse(FE_ACU == "30 a 34 anos" | FE_ACU == "35 a 39 anos" | FE_ACU == "40 a 44 anos" |
                           FE_ACU == "45 a 49 anos" | FE_ACU == "50 a 54 anos" | FE_ACU == "55 a 59 anos", "Adulto",
                    ifelse(FE_ACU == "a partir de 60 anos", "Idoso",
                    ifelse(FE_ACU == "Não informado", "Não informado", NA)))))) %>%
  mutate(FX_H_FATO = ifelse(T_FATO == "Manhã", "06:01 às 12:00",
                     ifelse(T_FATO == "Tarde", "12:01 às 18:00",
                     ifelse(T_FATO == "Noite", "18:01 às 00:00",
                     ifelse(T_FATO == "Madrugada", "00:01 às 06:00", " ")))))

#Atualiza colunas dos municipios e bairros
aut <- aut[,-c(which(colnames(aut) == "MUN_1"),
               which(colnames(aut) == "COD_IBGE_MUN"),
               which(colnames(aut) == "MUNPRIO"),
               which(colnames(aut) == "RS_SEG"),
               which(colnames(aut) == "ASSOC_MUN"),
               which(colnames(aut) == "MESSOR"),
               which(colnames(aut) == "COD_IBGE_MESO"),
               which(colnames(aut) == "MICROR"),
               which(colnames(aut) == "COD_IBGE_MICRO"),
               which(colnames(aut) == "COREDE"),
               which(colnames(aut) == "COD_COREDE"),
               which(colnames(aut) == "AGLO_URB_INT"),
               which(colnames(aut) == "COD_AG"),
               which(colnames(aut) == "BAIRRO_1"),
               which(colnames(aut) == "BAIRRO_2"))]
aut <- left_join(aut, ref_mun, by = "MUN")

aut <- aut %>%
  mutate(BAIRRO_temp = tolower(BAIRRO))

aut <- left_join(aut, ref_bairro, by = c("BAIRRO_temp" = "BAIRRO", "MUN1" = "MUN"), na_matchers = "never")

aut <- aut %>%
  select(-(BAIRRO_temp))

aut <- unique(aut)

#Contagem (NUM_AUT - nos 2 bancos)
cont <- aut %>%
  group_by(COD_OC) %>%
  mutate(NUM_AUT = n()) %>%
  select(COD_OC, NUM_AUT)

cont <- unique(cont)

aut <- aut %>%
  select(-NUM_AUT)

aut <- left_join(aut, cont, by = "COD_OC")

aut <- aut %>%
  mutate(M_AUTOR = ifelse(NUM_AUT > 1, "Sim",
                          ifelse(NUM_AUT <= 1, "Não", NA)))

#Contagem (NUM_VIT_MORTAS - nos 2 bancos)
cont_vit_mort <- vit %>%
  group_by(COD_OC) %>%
  mutate(NUM_VIT_MORTAS = n()) %>%
  select(COD_OC, NUM_VIT_MORTAS)

cont_vit_mort <- unique(cont_vit_mort)

aut <- aut %>%
  select(-NUM_VIT_MORTAS)

aut <- left_join(aut, cont_vit_mort, by= "COD_OC")

#NA para idades > 100 anos e data de nascimento de 1900
aut <- aut %>%
  replace_with_na(replace = list(ID_ACU = seq(101,120, by =1))) %>%
  mutate(DN_ACU = ifelse(DN_ACU > as.Date("01/01/1920"), DN_ACU, NA))

# --------------------- XXXXXXXXXXXXXX --------------------- #
#Arrumar as variáveis: COR_VIT, EC_VIT, COR_ACU, EC_ACU
# Padrões das variáveis:

###  Cor Pele:
# Banco autor:
levels(aut$COR_ACU)
levels(aut$COR_ACU)[levels(aut$COR_ACU) == "Índio"] <- "Indígena"
levels(aut$COR_ACU)[levels(aut$COR_ACU) == "Mulato"] <- "Parda"
levels(aut$COR_ACU)[levels(aut$COR_ACU) == "Sarara"] <- "Preta"
levels(aut$COR_ACU)[levels(aut$COR_ACU) == ""] <- "NA"

### Escolaridade:
# Banco autor:
levels(aut$ESC_ACU)
levels(aut$ESC_ACU)[levels(aut$ESC_ACU) == "Semialfabetizado"] <- "Semi-alfabetizado"

### Estado civil:
# Banco autor:
levels(aut$EC_ACU)
levels(aut$EC_ACU)[levels(aut$EC_ACU) == "Solteiro"] <- "Solteiro(a)"
levels(aut$EC_ACU)[levels(aut$EC_ACU) == "Casado"] <- "Casado(a)"
levels(aut$EC_ACU)[levels(aut$EC_ACU) == "Divorciado"] <- "Divorciado(a)"
levels(aut$EC_ACU)[levels(aut$EC_ACU) == "Viúvo"] <- "Viúvo(a)"
levels(aut$EC_ACU)[levels(aut$EC_ACU) == "Separado"] <- "Divorciado(a)"
# levels(aut$EC_ACU)[levels(aut$EC_ACU) == "Amigado"]


### Nome:
# - Onde houver 'não informado', 'sem informação', 'XXX' padronizar para 'Não informado'.

target = c("... ...",
           "A IDENTIFICAR",
           "Desconhecido de tal",
           "DES CONHECIDO",
           "FULANO DE TAL",
           "FULNO DE TAL",
           "INDIVIDUO DESCONHECIDO",
           "INDIVÍDUO MNÃO IDENTIFICADO",
           "INDIVIDUO NAO IDENTIFICADO",
           "INDIVIDUO NAO IDENTIFICADO",
           "INDIVIDUO NAO IDENTIFICADO",
           "NAO APRESENTADO",
           "NAO CADASTRADO",
           "NAO FOI IDENTIFICADA",
           "NÃO FOI IDENTIFICADO",
           "NAO FOI POSSIVEL IDENTIFICAR",
           "Não Idendificado",
           "NAO IDENTIFCADO",
           "NAO IDENTIFICADA",
           "NAO IDENTIFICADO",
           "não identificado",
           "Não identificado",
           "Não Identificado",
           "NÃO IDENTIFICADO",
           "NAO IDENTIFICOU",
           "NAO INFORMADO",
           "NÃO SE IDENTIFICOU",
           "O DESCONHECIDO",
           "SEM IDENTIFICACAO",
           "sem informacoes",
           "SEM NOME",
           "VITIMA FATAL NAO IDENTIFICADA",
           "VÍTIMA NÃO IDENTIFICADA",
           "VITIMA SEM IDENTIFICACAO",
           "XX XX",
           "XX XXXXX")

# Banco autor:

aut$N_ACU = as.character(aut$N_ACU)

for (i in 1:length(target)) {
  aut$N_ACU[which(aut$N_ACU == target[i])] = "Não informado"
}

#TIPO_LOCAL
aut$TIPO_LOCAL <- fct_collapse(aut$TIPO_LOCAL,
                               "Estabelecimento comercial" = c("Estabelecimento comercial", "Estabelecimento Comercial"),
                               "Estabelecimento penal" = c("Estabelecimento penal", "Estabelecimento Penal"),
                               "Interior do automóvel" = c("Interior do automóvel", "Interior do automóvel "),
                               "Via pública" = c("Via pública", "Via Pública"),
                               "Zona rural" = c("Zona rural", "Zona Rural"))

#M_UTIL_AUT
aut$M_UTIL_AUT <- fct_collapse(aut$M_UTIL_AUT, "Arma branca" = c("arma branca", "Arma branca", "Arma Branca"),
                               "Ferramentas" = c("ferramentas", "Ferramentas"))


##Ordenar as colunas
aut <- aut %>%
  select(COD, COD_OC, COD_ACU, NUM_FATO, NUM_OCOR, TIP_FATO, NUM_ORG_REGIST, N_ORG_REGIST, ORIGEM_COM, DATA_COM,
         HORA_COM, ANO_REGIST, FLAGRANTE, TENTATIVA, COMUM, COND, LAUDO, NUM_AUT, NUM_VIT, NUM_VIT_MORTAS, NUMTEST,
         TIPO_TEST, ANO_FATO, DATA_FATO, MES_FATO, D_SEMANA, H_FATO, H_FATO_1, T_FATO, FX_H_FATO, MUN, MUN1, COD_IBGE_MUN,
         RS_SEG, ASSOC_MUN, MESSOR, COD_IBGE_MESO, MICROR, COD_IBGE_MICRO, COREDE, COD_COREDE, AGLO_URB_INT, COD_AG, A_MUN,
         LOG, NUM, COMP, CEP, BAIRRO, BAIRRO_1, BAIRRO_2, LAT, LNG, P_REF, TIPO_LOCAL, M_UTIL_AUT, RECUR, NUM_AGRE, M_AUTOR,
         OBJ, ADTNT, MOT, CADAVER, R_VIT_AUT, MID, INFORMANTE, N_INFORMANTE, HIST, OBS, N_ACU, RG_ACU, CPF_ACU, SX_ACU,
         DN_ACU, ID_ACU, FE_ACU, PERF_ACU, ORIEN_SX_ACU, COR_ACU, EC_ACU, UE_ACU, FIL_ACU, NAT_ACU, NAC_ACU, COND_FIS_ACU,
         ALCUNHA_ACU, TAT_ACU, ESC_ACU, END_RES_ACU, MUN_ACU, BAIRRO_ACU, PROF_ACU, END_PROF_ACU, MUN_PROF_ACU, BAIRRO_PROF_ACU,
         N_PAI_ACU, ESC_PAI_ACU, HIS_AC_PAI_ACU, HIS_AC_VI_DOM_VIT, N_MAE_ACU, ESC_MAE_ACU, HIS_AC_MAE_ACU, VIOL_DOM_MAE_ACU, MP_ACU,
         MI_ACU, HIS_MI_ACU, ENVTRAF_ACU, FOR_ACU, ANTEPOL_ACU, ANTECRIM_ACU, TIPO_PARTIC)
aut <- arrange(aut, by = DATA_FATO)

#cria dashboard
dash_aut <- aut %>%
  select(COD, NUM_ORG_REGIST,ANO_FATO,NUM_OCOR,TIP_FATO,TENTATIVA,DATA_FATO,MUN,BAIRRO,LOG,NUM,
         ID_VIT,SX_ACU,FE_ACU, COND_FIS_ACU, COR_ACU,H_FATO,ESC_ACU,D_SEMANA,RS_SEG,ASSOC_MUN,
         H_FATO_1,FX_H_FATO,T_FATO,MUN1,BAIRRO_1,BAIRRO_2,HIST,TIPO_LOCAL,M_UTIL_AUT,MES_FATO,
         PERF_VIT,NUM_VIT_MORTAS, TIPO_PARTIC)
dash_aut <- arrange(dash_aut, by = DATA_FATO)
