# --------------------- Arrumando as horas --------------------- #

to.times <- function(x) times(paste0(x, ":00"))

cognos_vit$Hora.Fato <- to.times(cognos_vit$Hora.Fato)
cognos_vit$Hora.Registro <- to.times(cognos_vit$Hora.Registro)

completo_vit$HORA_COM <- times(completo_vit$HORA_COM)
completo_vit$H_FATO <- times(completo_vit$H_FATO)

# --------------------- Padronização das colunas das bases --------------------- #

# Alterar o nome das variáveis no banco Cognos com referẽncia ao banco completo:
# Base Vitima:

colnames(cognos_vit)[which(colnames(cognos_vit) == "Participacao")] = "TIPO_PARTIC"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Descr.Fato")] = "TIP_FATO"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Nro.Ocor")] = "NUM_OCOR"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Orgão.Registro")] = "NUM_ORG_REGIST"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Flagrante")] = "FLAGRANTE"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Data.Fato")] = "DATA_FATO"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Hora.Fato")] = "H_FATO"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Município")] = "MUN"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Codigo.Fato")] = "NUM_FATO"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Tipo.Fato")] = "TENTATIVA"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Bairro")] = "BAIRRO"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Ano.Registro")] = "ANO_REGIST"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Data.Registro")] = "DATA_COM" # INCERTO
colnames(cognos_vit)[which(colnames(cognos_vit) == "Hora.Registro")] = "HORA_COM" # INCERTO
colnames(cognos_vit)[which(colnames(cognos_vit) == "Latitude")] = "LAT"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Longitude")] = "LNG"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Nome")] = "N_VIT"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Rg")] = "RG_VIT"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Sexo")] = "SX_VIT"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Data.Nascto")] = "DN_VIT"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Idade.Participante")] = "ID_VIT"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Cor.Pele")] = "COR_VIT"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Estado.Civil")] = "EC_VIT"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Condicao.Fisica")] = "COND_FIS_VIT"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Profissao")] = "PROF_VIT"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Dia.Semana.Fato")] = "D_SEMANA"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Instrucao.Participante")] = "ESC_VIT"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Historico.Ocor")] = "HIST"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Endereço")] = "LOG"
colnames(cognos_vit)[which(colnames(cognos_vit) == "Nro.Endereço")] = "NUM"



# --------------------- XXXXXXXXXXXXXX --------------------- #

# Arrumar a data fato, data registro e data de nascimento

# Vítima:

cognos_vit$DN_VIT <- gsub("\\s[^ ]+", "", cognos_vit$DN_VIT) # removendo a hora da data
cognos_vit$DATA_FATO <- gsub("\\s[^ ]+", "", cognos_vit$DATA_FATO) # removendo a hora da data
cognos_vit$DATA_COM <- gsub("\\s[^ ]+", "", cognos_vit$DATA_COM) # removendo a hora da data

cognos_vit$DN_VIT <- format(as.Date(cognos_vit$DN_VIT),'%d/%m/%Y')
cognos_vit$DATA_COM <- format(as.Date(cognos_vit$DATA_COM),'%d/%m/%Y')
cognos_vit$DATA_FATO <- format(as.Date(cognos_vit$DATA_FATO),'%d/%m/%Y')


# Checkando:
#cognos_vit$DN_VIT
#cognos_vit$DATA_COM
#cognos_vit$DATA_FATO


# Mudar as variáves Flagrante e Tipo FATO
cognos_vit$FLAGRANTE <- cognos_vit$FLAGRANTE %>%
  fct_collapse("Não" = "Sem Flagrante",
               "Sim" = "Com Flagrante")


# --------------------- XXXXXXXXXXXXXX --------------------- #


# Criar variavel "COD_OC" nos bancos Cognos

cognos_vit$ANO_FATO <- format(as.Date(cognos_vit$DATA_FATO, format = "%d/%m/%Y"), format = "%Y")
cognos_vit$COD_OC <- paste0(cognos_vit$NUM_ORG_REGIST, "_", cognos_vit$NUM_OCOR, "_",
                            cognos_vit$ANO_FATO, sep = "")

completo_vit$COD_OC <- paste0(completo_vit$NUM_ORG_REGIST, "_", completo_vit$NUM_OCOR, "_",
                              completo_vit$ANO_FATO, sep = "")



# Criar variavel codigo VIT
cognos_vit <- cognos_vit %>%
  group_by(COD_OC) %>%
  mutate(COD_VIT = paste0("VIT", formatC(1:n(), width = 2, flag = "0"))) %>%
  ungroup()

completo_vit <- completo_vit %>%
  group_by(COD_OC) %>%
  mutate(COD_VIT = paste0("VIT", formatC(1:n(), width = 2, flag = "0")))%>%
  ungroup()


# Criar variável que irá unir as bases:

cognos_vit$COD <- paste0(cognos_vit$COD_OC, "_", cognos_vit$COD_VIT, sep = "")
completo_vit$COD <- paste0(completo_vit$COD_OC, "_", completo_vit$COD_VIT, sep = "")


completo_vit <- completo_vit[,-c(which(colnames(completo_vit) == "COD_OC"),
                                 which(colnames(completo_vit) == "COD_VIT"),
                                 which(colnames(completo_vit) == "ANO_FATO"))]


# Seleciona apenas as colunas no Completo que não tem no Cognos

completo_vit_sem_cognos <- completo_vit[, -which(names(completo_vit) %in% names(cognos_vit))]
completo_vit_sem_cognos$COD <- completo_vit$COD

# Une os dois com "_join" (chave - COD) - cria um com FATO+VIT
vit <- left_join(cognos_vit, completo_vit_sem_cognos, by = "COD")

# --------------------- XXXXXXXXXXXXXX --------------------- #

#Fórmula para os NAs de (utilizando "forcats"):
#T_FATO, FX_H_FATO,COD_IBGE_MUN,MUNPRIO,ASSOC_MUN,MESSOR,MESO,MICROR,COD_IBGE_MICRO,COD_COREDE,AGLO_URB_INT,COD_AG,PERF_VIT,PERF_ACU
#referencia - https://stackoverflow.com/questions/24459752/can-dplyr-package-be-used-for-conditional-mutating

vit <- vit %>%
  select(-X)

vit <- vit %>%
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
  mutate(FE_VIT = ifelse(ID_VIT < 12, "Menor de 12 anos",
                  ifelse(ID_VIT >= 12 & ID_VIT <= 17, "12 a 17 anos",
                  ifelse(ID_VIT >= 18 & ID_VIT <= 24, "18 a 24 anos",
                  ifelse(ID_VIT >= 25 & ID_VIT <= 29, "25 a 29 anos",
                  ifelse(ID_VIT >= 30 & ID_VIT <= 34, "30 a 34 anos",
                  ifelse(ID_VIT >= 35 & ID_VIT <= 39, "35 a 39 anos",
                  ifelse(ID_VIT >= 40 & ID_VIT <= 44, "40 a 44 anos",
                  ifelse(ID_VIT >= 45 & ID_VIT <= 49, "45 a 49 anos",
                  ifelse(ID_VIT >= 50 & ID_VIT <= 54, "50 a 54 anos",
                  ifelse(ID_VIT >= 55 & ID_VIT <= 59, "55 a 59 anos",
                  ifelse(ID_VIT >= 60, "a partir de 60 anos", NA)))))))))))) %>%
  mutate(FE_VIT = ifelse(is.na(FE_VIT), "Não informado", FE_VIT)) %>%
  mutate(PERF_VIT = ifelse(FE_VIT == "Menor de 12 anos" | FE_VIT == "12 a 17 anos", "Criança e adolescente",
                    ifelse(FE_VIT == "18 a 24 anos" | FE_VIT == "25 a 29 anos", "Jovem",
                    ifelse(FE_VIT == "30 a 34 anos" | FE_VIT == "35 a 39 anos" | FE_VIT == "40 a 44 anos" |
                           FE_VIT == "45 a 49 anos" | FE_VIT == "50 a 54 anos" | FE_VIT == "55 a 59 anos", "Adulto",
                    ifelse(FE_VIT == "a partir de 60 anos", "Idoso",
                    ifelse(FE_VIT == "Não informado", "Não informado", NA)))))) %>%
  mutate(FX_H_FATO = ifelse(T_FATO == "Manhã", "06:01 às 12:00",
                     ifelse(T_FATO == "Tarde", "12:01 às 18:00",
                     ifelse(T_FATO == "Noite", "18:01 às 00:00",
                     ifelse(T_FATO == "Madrugada", "00:01 às 06:00", " ")))))

#Municipios
vit <- vit[,-c(which(colnames(vit) == "MUN1"),
               which(colnames(vit) == "COD_IBGE_MUN"),
               which(colnames(vit) == "RS_SEG"),
               which(colnames(vit) == "ASSOC_MUN"),
               which(colnames(vit) == "MESSOR"),
               which(colnames(vit) == "COD_IBGE_MESO"),
               which(colnames(vit) == "MICROR"),
               which(colnames(vit) == "COD_IBGE_MICRO"),
               which(colnames(vit) == "COREDE"),
               which(colnames(vit) == "COD_COREDE"),
               which(colnames(vit) == "AGLO_URB_INT"),
               which(colnames(vit) == "COD_AG"),
               which(colnames(vit) == "BAIRRO_1"),
               which(colnames(vit) == "BAIRRO_2"))]
vit <- left_join(vit, ref_mun, by = "MUN")

#Bairro
vit <- vit %>%
  mutate(BAIRRO_temp = tolower(BAIRRO))

vit <- left_join(vit, ref_bairro, by = c("BAIRRO_temp" = "BAIRRO", "MUN1" = "MUN"), na_matchers = "never")

vit <- vit %>%
  select(-(BAIRRO_temp))

vit <- unique(vit)

# --------------------- XXXXXXXXXXXXXX --------------------- #

#Contagem (NUM_AUT - nos 2 bancos)
cont <- aut %>%
  group_by(COD_OC) %>%
  mutate(NUM_AUT = n()) %>%
  select(COD_OC, NUM_AUT)

cont <- unique(cont)

vit <- vit %>%
  select(-NUM_AUT)

vit <- left_join(vit, cont, by = "COD_OC")

vit <- vit %>%
  mutate(M_AUTOR = ifelse(NUM_AUT > 1, "Sim",
                   ifelse(NUM_AUT <= 1, "Não", NA)))

#Contagem (NUM_VIT_MORTAS - nos 2 bancos)
cont_vit_mort <- vit %>%
  group_by(COD_OC) %>%
  mutate(NUM_VIT_MORTAS = n()) %>%
  select(COD_OC, NUM_VIT_MORTAS)

cont_vit_mort <- unique(cont_vit_mort)

vit <- vit %>%
  select(-NUM_VIT_MORTAS)

vit <- left_join(vit, cont_vit_mort, by= "COD_OC")

#NA para idades > 100 anos e data de nascimento de 1900
vit <- vit %>%
  replace_with_na(replace = list(ID_VIT = seq(101,120, by =1))) %>%
  mutate(DN_VIT = ifelse(DN_VIT > as.Date("01/01/1920"), DN_VIT, NA))


# --------------------- XXXXXXXXXXXXXX --------------------- #
#Arrumar as variáveis: COR_VIT, EC_VIT, COR_ACU, EC_ACU
# Padrões das variáveis:

###  Cor Pele:

# Banco vítima:
levels(vit$COR_VIT)
levels(vit$COR_VIT)[levels(vit$COR_VIT) == "Albino"] <- "Branca"
levels(vit$COR_VIT)[levels(vit$COR_VIT) == "Índio"] <- "Indígena"
levels(vit$COR_VIT)[levels(vit$COR_VIT) == "Mulato"] <- "Parda"
levels(vit$COR_VIT)[levels(vit$COR_VIT) == "Sarara"] <- "Preta"
levels(vit$COR_VIT)[levels(vit$COR_VIT) == ""] <- "Sem informação"

### Escolaridade:

levels(vit$ESC_VIT)
levels(vit$ESC_VIT)[levels(vit$ESC_VIT) == "Semialfabetizado"] <- "Semi-alfabetizado"

### Estado civil:

levels(vit$EC_VIT)
levels(vit$EC_VIT)[levels(vit$EC_VIT) == "Solteiro"] <- "Solteiro(a)"
levels(vit$EC_VIT)[levels(vit$EC_VIT) == "Casado"] <- "Casado(a)"
levels(vit$EC_VIT)[levels(vit$EC_VIT) == "Divorciado"] <- "Divorciado(a)"
levels(vit$EC_VIT)[levels(vit$EC_VIT) == "Viúvo"] <- "Viúvo(a)"
levels(vit$EC_VIT)[levels(vit$EC_VIT) == "Separado"] <- "Divorciado(a)"
levels(vit$EC_VIT)[levels(vit$EC_VIT) == "Desquitado"] <- "Divorciado(a)"
# levels(vit$EC_VIT)[levels(vit$EC_VIT) == "Amigado"]


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

vit$N_VIT = as.character(vit$N_VIT)

for (i in 1:length(target)) {
  vit$N_VIT[which(vit$N_VIT == target[i])] = "Não informado"
}

#TIPO_LOCAL
vit$TIPO_LOCAL <- fct_collapse(vit$TIPO_LOCAL,
                               "Estabelecimento comercial" = c("Estabelecimento comercial", "Estabelecimento Comercial"),
                               "Estabelecimento penal" = c("Estabelecimento penal", "Estabelecimento Penal"),
                               "Interior do automóvel" = c("Interior do automóvel", "Interior do automóvel "),
                               "Via pública" = c("Via pública", "Via Pública"),
                               "Zona rural" = c("Zona rural", "Zona Rural"))

#M_UTIL_AUT
vit$M_UTIL_AUT <- fct_collapse(vit$M_UTIL_AUT, "Arma branca" = c("arma branca", "Arma branca", "Arma Branca"),
                               "Ferramentas" = c("ferramentas", "Ferramentas"))


# Ordenar as colunas

vit <- vit %>%
  select(COD, COD_OC, COD_VIT, NUM_FATO, NUM_OCOR, TIP_FATO, NUM_ORG_REGIST, N_ORG_REGIST, ORIGEM_COM, DATA_COM,
         HORA_COM, ANO_REGIST, FLAGRANTE, TENTATIVA, COMUM, COND, LAUDO, NUM_AUT, NUM_VIT, NUM_VIT_MORTAS, NUMTEST,
         TIPO_TEST, ANO_FATO, DATA_FATO, MES_FATO, D_SEMANA, H_FATO, H_FATO_1, T_FATO, FX_H_FATO, MUN, MUN1, COD_IBGE_MUN,
         RS_SEG, ASSOC_MUN, MESSOR, COD_IBGE_MESO, MICROR, COD_IBGE_MICRO, COREDE, COD_COREDE, AGLO_URB_INT, COD_AG, A_MUN,
         LOG, NUM, COMP, CEP, BAIRRO, BAIRRO_1, BAIRRO_2, LAT, LNG, P_REF, TIPO_LOCAL, M_UTIL_AUT, RECUR, NUM_AGRE, M_AUTOR,
         OBJ, ADTNT, MOT, CADAVER, R_VIT_AUT, MID, INFORMANTE, N_INFORMANTE, HIST, OBS, N_VIT, RG_VIT, CPF_VIT, SX_VIT,
         DN_VIT, ID_VIT, FE_VIT, PERF_VIT, ORIEN_SX_VIT, COR_VIT, EC_VIT, UE_VIT, FIL_VIT, NAT_VIT, NAC_VIT, COND_FIS_VIT, 
         ALCUNHA_VIT, TAT_VIT, ESC_VIT, END_RESVIT, MUN_VIT, BAIRRO_VIT, PROF_VIT, END_PROFVIT, MUN_PROF_VIT, BAIRRO_PROF_VIT,
         N_PAIVIT, ESC_PAIVIT, HIS_AC_PAI_VIT, HIS_AC_VI_DOM_VIT, N_MAEVIT, ESC_MAEVIT, HIS_AC_MAE_VIT, VIOL_DOM_MAE_VIT, MP_VIT,
         MI_VIT, HIS_MI_VIT, ENVTRAF_VIT, FOR_VIT, ANTEPOL_VIT, ANTECRIM_VIT)
vit <- arrange(vit, by = DATA_FATO)

#cria dashboard
dash_vit <- vit %>%
  select(COD, NUM_ORG_REGIST,ANO_FATO,NUM_OCOR,TIP_FATO,TENTATIVA,DATA_FATO,MUN,BAIRRO,LOG,NUM,
         ID_VIT,SX_VIT,FE_VIT,COND_FIS_VIT,COR_VIT,H_FATO,ESC_VIT,D_SEMANA,RS_SEG,ASSOC_MUN,
         H_FATO_1,FX_H_FATO,T_FATO,MUN1,BAIRRO_1,BAIRRO_2,HIST,TIPO_LOCAL,M_UTIL_AUT,MES_FATO,
         PERF_VIT,NUM_VIT_MORTAS)
dash_vit <- arrange(dash_vit, by = DATA_FATO)
