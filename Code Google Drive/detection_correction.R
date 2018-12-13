### import data test
data<-read.csv(file="")

### Extraction des data dans des vectors
CNI1_vector <- as.vector(data$C_NI1)
CPO1_vector <- as.vector(data$C_P01)
CO21_vector <- as.vector(data$C_O21)
CSI1_vector <- as.vector(data$C_SI1)
CSAL1_vector <- as.vector(data$CSAL1)

CSAT1_vector <- as.vector(data$CSAT1)
ETC01_vector <- as.vector(data$ETC01)
ELU1_vector <- as.vector(data$E_LU1)
EO21_vector <- as.vector(data$E_O21)
EPH1_vector <- as.vector(data$E_PH1)

ETU1_vector <- as.vector(data$E_TU1)
ECHL1_vector <- as.vector(data$ECHL1)
ETA_vector <- as.vector(data$E__TA)
XMAHH_vector <- as.vector(data$XMAHH)

### Calcul de toutes les valeurs de la boite a moustache stocker dans une variable table
info_CNI1<- summary(CNI1_vector,probs=seq(0,1,0.1))
info_CPO1<- summary(CPO1_vector,probs=seq(0,1,0.1))
info_CO21<- summary(CO21_vector,probs=seq(0,1,0.1))
info_CSI1<- summary(CSI1_vector,probs=seq(0,1,0.1))
info_CSAL1<- summary(CSAL1_vector,probs=seq(0,1,0.1))

info_CSAT1<- summary(CSAT1_vector,probs=seq(0,1,0.1))
info_ETC01<- summary(ETC01_vector,probs=seq(0,1,0.1))
info_ELU1<- summary(ELU1_vector,probs=seq(0,1,0.1))
info_EO21<- summary(EO21_vector,probs=seq(0,1,0.1))
info_EPH1<- summary(EPH1_vector ,probs=seq(0,1,0.1))

info_ETU1<- summary(ETU1_vector,probs=seq(0,1,0.1))
info_ECHL1<- summary(ECHL1_vector,probs=seq(0,1,0.1))
info_ETA<- summary(ETA_vector,probs=seq(0,1,0.1))
info_XMAHH<- summary(XMAHH_vector,probs=seq(0,1,0.1))

### Recuperation du premier quartile
CNI1_first_quart <- info_CNI1[2]
CPO1_first_quart <- info_CPO1[2]
CO21_first_quart <- info_CO21[2]
CSI1_first_quart <- info_CSI1[2]
CSAL1_first_quart <- info_CSAL1[2]

CSAT1_first_quart <- info_CSAT1[2]
ETC01_first_quart <- info_ETC01[2]
ELU1_first_quart <- info_ELU1[2]
EO21_first_quart <- info_EO21[2]
EPH1_first_quart <- info_EPH1[2]

ETU1_first_quart <- info_ETU1[2]
ECHL1_first_quart <- info_ECHL1[2]
ETA_first_quart <- info_ETA[2]
XMAHH_first_quart <- info_XMAHH[2]

### Recuperation du troisieme quartile
CNI1_third_quart <- info_CNI1[5]
CPO1_third_quart <- info_CPO1[5]
CO21_third_quart <- info_CO21[5]
CSI1_third_quart <- info_CSI1[5]
CSAL1_third_quart <- info_CSAL1[5]

CSAT1_third_quart <- info_CSAT1[5]
ETC01_third_quart <- info_ETC01[5]
ELU1_third_quart <- info_ELU1[5]
EO21_third_quart <- info_EO21[5]
EPH1_third_quart <- info_EPH1[5]

ETU1_third_quart <- info_ETU1[5]
ECHL1_third_quart <- info_ECHL1[5]
ETA_third_quart <- info_ETA[5]
XMAHH_third_quart <- info_XMAHH[5]

### Recuperation presence NA
dim_vector_info_CNI1 <- length(info_CNI1)
dim_vector_info_CPO1 <- length(info_CPO1)
dim_vector_info_CO21 <- length(info_CO21)
dim_vector_info_CSI1 <- length(info_CSI1)
dim_vector_info_CSAL1 <- length(info_CSAL1)

dim_vector_info_CSAT1 <- length(info_CSAT1)
dim_vector_info_ETC01 <- length(info_ETC01)
dim_vector_info_ELU1 <- length(info_ELU1)
dim_vector_info_EO21 <- length(info_EO21)
dim_vector_info_EPH1 <- length(info_EPH1)

dim_vector_info_ETU1 <- length(info_ETU1)
dim_vector_info_ECHL1 <- length(info_ECHL1)
dim_vector_info_ETA <- length(info_ETA)
dim_vector_info_XMAHH <- length(info_XMAHH)

### Correction NA par valeur moyenne
if(dim_vector_info_CNI1 == 7){
  CNI1_vector[is.na(CNI1_vector)] <- info_CNI1[4]
}
if(dim_vector_info_CPO1 == 7){
  CPO1_vector[is.na(CPO1_vector)] <- info_CPO1[4]
}
if(dim_vector_info_CO21 == 7){
  CO21_vector[is.na(CO21_vector)] <- info_CO21[4]
}
if(dim_vector_info_CSI1 == 7){
  CSI1_vector[is.na(CSI1_vector)] <- info_CSI1[4]
}
if(dim_vector_info_CSAL1 == 7){
  CSAL1_vector[is.na(CSAL1_vector)] <- info_CSAL1[4]
}

if(dim_vector_info_CSAT1 == 7){
  CSAT1_vector[is.na(CSAT1_vector)] <- info_CSAT1[4]
}
if(dim_vector_info_ETC01 == 7){
  ETC01_vector[is.na(ETC01_vector)] <- info_ETC01[4]
}
if(dim_vector_info_ELU1 == 7){
  ELU1_vector[is.na(ELU1_vector)] <- info_ELU1[4]
}
if(dim_vector_info_EO21 == 7){
  EO21_vector[is.na(EO21_vector)] <- info_EO21[4]
}
if(dim_vector_info_EPH1 == 7){
  EPH1_vector[is.na(EPH1_vector)] <- info_EPH1[4]
}

if(dim_vector_info_ETU1 == 7){
  ETU1_vector[is.na(ETU1_vector)] <- info_ETU1[4]
}
if(dim_vector_info_ECHL1 == 7){
  ECHL1_vector[is.na(ECHL1_vector)] <- info_ECHL1[4]
}
if(dim_vector_info_ETA == 7){
  ETA_vector[is.na(ETA_vector)] <- info_ETA[4]
}
if(dim_vector_info_XMAHH == 7){
  XMAHH_vector[is.na(XMAHH_vector)] <- info_XMAHH[4]
}



### Vecteur de boolean inf 1e quartile
CNI1_verifier_inf<- as.vector(CNI1_vector < CNI1_first_quart)
CPO1_verifier_inf<- as.vector(CPO1_vector < CPO1_first_quart)
CO21_verifier_inf<- as.vector(CO21_vector < CO21_first_quart)
CSI1_verifier_inf<- as.vector(CSI1_vector < CSI1_first_quart)
CSAL1_verifier_inf<- as.vector(CSAL1_vector < CSAL1_first_quart)

CSAT1_verifier_inf<- as.vector(CSAT1_vector < CSAT1_first_quart)
ETC01_verifier_inf<- as.vector(ETC01_vector < ETC01_first_quart)
ELU1_verifier_inf<- as.vector(ELU1_vector < ELU1_first_quart)
EO21_verifier_inf<- as.vector(EO21_vector < EO21_first_quart)
EPH1_verifier_inf<- as.vector(EPH1_vector < EPH1_first_quart)

ETU1_verifier_inf<- as.vector(ETU1_vector < ETU1_first_quart)
ECHL1_verifier_inf<- as.vector(ECHL1_vector < ECHL1_first_quart)
ETA_verifier_inf<- as.vector(ETA_vector < ETA_first_quart)
XMAHH_verifier_inf<- as.vector(XMAHH_vector < XMAHH_first_quart)

### Vecteur de boolean sup 3e quartile
CNI1_verifier_sup<- as.vector(CNI1_vector > CNI1_third_quart)
CPO1_verifier_sup<- as.vector(CPO1_vector > CPO1_third_quart)
CO21_verifier_sup<- as.vector(CO21_vector > CO21_third_quart)
CSI1_verifier_sup<- as.vector(CSI1_vector > CSI1_third_quart)
CSAL1_verifier_sup<- as.vector(CSAL1_vector > CSAL1_third_quart)

CSAT1_verifier_sup<- as.vector(CSAT1_vector > CSAT1_third_quart)
ETC01_verifier_sup<- as.vector(ETC01_vector > ETC01_third_quart)
ELU1_verifier_sup<- as.vector(ELU1_vector > ELU1_third_quart)
EO21_verifier_sup<- as.vector(EO21_vector > EO21_third_quart)
EPH1_verifier_sup<- as.vector(EPH1_vector > EPH1_third_quart)

ETU1_verifier_sup<- as.vector(ETU1_vector > ETU1_third_quart)
ECHL1_verifier_sup<- as.vector(ECHL1_vector > ECHL1_third_quart)
ETA_verifier_sup<- as.vector(ETA_vector > ETA_third_quart)
XMAHH_verifier_sup<- as.vector(XMAHH_vector > XMAHH_third_quart)

### Compteurs de valeurs aberrantes inf
count_1 <- 0
count_2 <- 0
count_3 <- 0
count_4 <- 0
count_5 <- 0
count_6 <- 0
count_7 <- 0
count_8 <- 0
count_9 <- 0
count_10 <- 0
count_11 <- 0
count_12 <- 0
count_13 <- 0
count_14 <- 0

### Comptage valeurs aberrantes inf
for (i in 1:length(CNI1_verifier_inf)) {
  if(CNI1_verifier_inf[i] == TRUE){
    count_1 <- count_1 +1
  }
}
for (i in 1:length(CPO1_verifier_inf)) {
  if(CPO1_verifier_inf[i] == TRUE){
    count_2 <- count_2 +1
  }
}
for (i in 1:length(CO21_verifier_inf)) {
  if(CO21_verifier_inf[i] == TRUE){
    count_3 <- count_3 +1
  }
}
for (i in 1:length(CSI1_verifier_inf)) {
  if(CSI1_verifier_inf[i] == TRUE){
    count_4 <- count_4 +1
  }
}
for (i in 1:length(CSAL1_verifier_inf)) {
  if(CSAL1_verifier_inf[i] == TRUE){
    count_5 <- count_5 +1
  }
}

for (i in 1:length(CSAT1_verifier_inf)) {
  if(CSAT1_verifier_inf[i] == TRUE){
    count_6 <- count_6 +1
  }
}
for (i in 1:length(ETC01_verifier_inf)) {
  if(ETC01_verifier_inf[i] == TRUE){
    count_7 <- count_7 +1
  }
}
for (i in 1:length(ELU1_verifier_inf)) {
  if(ELU1_verifier_inf[i] == TRUE){
    count_8 <- count_8 +1
  }
}
for (i in 1:length(EO21_verifier_inf)) {
  if(EO21_verifier_inf[i] == TRUE){
    count_9 <- count_9 +1
  }
}
for (i in 1:length(EPH1_verifier_inf)) {
  if(EPH1_verifier_inf[i] == TRUE){
    count_10 <- count_10 +1
  }
}

for (i in 1:length(ETU1_verifier_inf)) {
  if(ETU1_verifier_inf[i] == TRUE){
    count_11 <- count_11 +1
  }
}
for (i in 1:length(ECHL1_verifier_inf)) {
  if(ECHL1_verifier_inf[i] == TRUE){
    count_12 <- count_12 +1
  }
}
for (i in 1:length(ETA_verifier_inf)) {
  if(ETA_verifier_inf[i] == TRUE){
    count_13 <- count_13 +1
  }
}
for (i in 1:length(XMAHH_verifier_inf)) {
  if(XMAHH_verifier_inf[i] == TRUE){
    count_14 <- count_14 +1
  }
}


### Addition des compteurs des valeurs aberrantes inf
count_va_inf <- count_1 + count_2 + count_3 + count_4 + count_5 + count_6 + count_7 + count_8 + count_9 + count_10 + count_11 + count_12 + count_13 + count_14 


### Compteurs de valeurs aberrantes sup
count_1 <- 0
count_2 <- 0
count_3 <- 0
count_4 <- 0
count_5 <- 0
count_6 <- 0
count_7 <- 0
count_8 <- 0
count_9 <- 0
count_10 <- 0
count_11 <- 0
count_12 <- 0
count_13 <- 0
count_14 <- 0


### Comptage valeurs aberrantes sup
for (i in 1:length(CNI1_verifier_sup)) {
  if(CNI1_verifier_sup[i] == TRUE){
    count_1 <- count_1 +1
  }
}
for (i in 1:length(CPO1_verifier_sup)) {
  if(CPO1_verifier_sup[i] == TRUE){
    count_2 <- count_2 +1
  }
}
for (i in 1:length(CO21_verifier_sup)) {
  if(CO21_verifier_sup[i] == TRUE){
    count_3 <- count_3 +1
  }
}
for (i in 1:length(CSI1_verifier_sup)) {
  if(CSI1_verifier_sup[i] == TRUE){
    count_4 <- count_4 +1
  }
}
for (i in 1:length(CSAL1_verifier_sup)) {
  if(CSAL1_verifier_sup[i] == TRUE){
    count_5 <- count_5 +1
  }
}

for (i in 1:length(CSAT1_verifier_sup)) {
  if(CSAT1_verifier_sup[i] == TRUE){
    count_6 <- count_6 +1
  }
}
for (i in 1:length(ETC01_verifier_sup)) {
  if(ETC01_verifier_sup[i] == TRUE){
    count_7 <- count_7 +1
  }
}
for (i in 1:length(ELU1_verifier_sup)) {
  if(ELU1_verifier_sup[i] == TRUE){
    count_8 <- count_8 +1
  }
}
for (i in 1:length(EO21_verifier_sup)) {
  if(EO21_verifier_sup[i] == TRUE){
    count_9 <- count_9 +1
  }
}
for (i in 1:length(EPH1_verifier_sup)) {
  if(EPH1_verifier_sup[i] == TRUE){
    count_10 <- count_10 +1
  }
}

for (i in 1:length(ETU1_verifier_sup)) {
  if(ETU1_verifier_sup[i] == TRUE){
    count_11 <- count_11 +1
  }
}
for (i in 1:length(ECHL1_verifier_sup)) {
  if(ECHL1_verifier_sup[i] == TRUE){
    count_12 <- count_12 +1
  }
}
for (i in 1:length(ETA_verifier_sup)) {
  if(ETA_verifier_sup[i] == TRUE){
    count_13 <- count_13 +1
  }
}
for (i in 1:length(XMAHH_verifier_sup)) {
  if(XMAHH_verifier_sup[i] == TRUE){
    count_14 <- count_14 +1
  }
}

### Addition des compteurs des valeurs aberrantes sup
count_va_sup <- count_1 + count_2 + count_3 + count_4 + count_5 + count_6 + count_7 + count_8 + count_9 + count_10 + count_11 + count_12 + count_13 + count_14 

### Addition des deux compteurs de valeurs aberrantes
count_va <- count_va_sup + count_va_inf


### Recuperer les indices va sup
indice_erreur_CNI1_sup <- which(CNI1_verifier_sup %in% c(TRUE))
indice_erreur_CPO1_sup <- which(CPO1_verifier_sup %in% c(TRUE))
indice_erreur_CO21_sup <- which(CO21_verifier_sup %in% c(TRUE))
indice_erreur_CSI1_sup <- which(CSI1_verifier_sup %in% c(TRUE))
indice_erreur_CSAL1_sup <- which(CSAL1_verifier_sup %in% c(TRUE))

indice_erreur_CSAT1_sup <- which(CSAT1_verifier_sup %in% c(TRUE))
indice_erreur_ETC01_sup <- which(ETC01_verifier_sup %in% c(TRUE))
indice_erreur_ELU1_sup <- which(ELU1_verifier_sup %in% c(TRUE))
indice_erreur_EO21_sup <- which(EO21_verifier_sup %in% c(TRUE))
indice_erreur_EPH1_sup <- which(EPH1_verifier_sup %in% c(TRUE))

indice_erreur_ETU1_sup <- which(ETU1_verifier_sup %in% c(TRUE))
indice_erreur_ECHL1_sup <- which(ECHL1_verifier_sup %in% c(TRUE))
indice_erreur_ETA_sup <- which(ETA_verifier_sup %in% c(TRUE))
indice_erreur_XMAHH_sup <- which(XMAHH_verifier_sup %in% c(TRUE))

### Recuperer les indices va inf
indice_erreur_CNI1_inf <- which(CNI1_verifier_inf %in% c(TRUE))
indice_erreur_CPO1_inf <- which(CPO1_verifier_inf %in% c(TRUE))
indice_erreur_CO21_inf <- which(CO21_verifier_inf %in% c(TRUE))
indice_erreur_CSI1_inf <- which(CSI1_verifier_inf %in% c(TRUE))
indice_erreur_CSAL1_inf <- which(CSAL1_verifier_inf %in% c(TRUE))

indice_erreur_CSAT1_inf <- which(CSAT1_verifier_inf %in% c(TRUE))
indice_erreur_ETC01_inf <- which(ETC01_verifier_inf %in% c(TRUE))
indice_erreur_ELU1_inf <- which(ELU1_verifier_inf %in% c(TRUE))
indice_erreur_EO21_inf <- which(EO21_verifier_inf %in% c(TRUE))
indice_erreur_EPH1_inf <- which(EPH1_verifier_inf %in% c(TRUE))

indice_erreur_ETU1_inf <- which(ETU1_verifier_inf %in% c(TRUE))
indice_erreur_ECHL1_inf <- which(ECHL1_verifier_inf %in% c(TRUE))
indice_erreur_ETA_inf <- which(ETA_verifier_inf %in% c(TRUE))
indice_erreur_XMAHH_inf <- which(XMAHH_verifier_inf %in% c(TRUE))

if (count_va > 0){
  print("Afficher alerte valeurs aberrantes !")
}



