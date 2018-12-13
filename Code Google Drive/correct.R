### import data test
data<-read.csv(file="test.csv")

### Extraction des data dans des vectors
taille_vector <- as.vector(data$Taille)
age_vector <- as.vector(data$Age)


### Calcul de toutes les valeurs de la boite à moustache stocker dans une variable table
info_taille<- summary(taille_vector,probs=seq(0,1,0.1))
info_age<- summary(age_vector,probs=seq(0,1,0.1))

### Récupération du premier quartile
taille_first_quart <- info_taille[2]
age_first_quart <- info_age[2]

### Récupération du troisième quartile
taille_third_quart <- info_taille[5]
age_third_quart <- info_age[5]

### Récupération présence NA
dim_vector_info_taille <- length(info_taille)
dim_vector_info_age <- length(info_age)


if(dim_vector_info_age == 7){
  print("Présence dans age de valeurs NA")
  ### Remplacement des NA par la valeur mediane
  age_vector[is.na(age_vector)] <- info_age[3]
  print("NA corrigees")
}else   
  print("Pas de valeur NA")

if(dim_vector_info_taille == 7){
  print("Présence dans taille de valeurs NA")
  ### Remplacement des NA par la valeur mediane
  taille_vector[is.na(taille_vector)] <- info_taille[3]
  print("NA corrigees")
}else   
  print("Pas de valeur NA")


### Compteurs de valeurs aberrantes
count_1 <- 0
count_2 <- 0


### Vecteur de boolean
taille_verifier_sup<- as.vector(taille_vector > taille_third_quart)
age_verifier_sup<- as.vector(age_vector > age_third_quart)


### Comptage valeurs aberrantes taille
for (i in 1:length(taille_verifier_sup)) {
  if(taille_verifier_sup[i] == TRUE){
    count_1 <- count_1 +1
  }
}

### Comptage valeurs aberrantes age
for (i in 1:length(age_verifier_sup)) {
  if(age_verifier_sup[i] == TRUE){
    count_2 <- count_2 +1
  }
}

### Récuperer les indices
indice_erreur_taille <- which(taille_verifier_sup %in% c(TRUE))
indice_erreur_age <- which(age_verifier_sup %in% c(TRUE))


### Addition des compteurs des valeurs aberrantes
count_va <- count_1 + count_2


### Affichage fenetre pour informer de presence d'erreurs
if (count_va > 0){
  print("Afficher alerte valeurs aberrantes !")
}else
  print("Pas de valeurs aberrantes !")


