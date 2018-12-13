### import data test
data<-read.csv(file="test.csv")

### détection des données manquantes
flag <- FALSE
lignes <- c()
colonnes <- c()
ligne <- 1
colonne <- 1

for (tab in data)
  {
    for (val in tab)
    {
      if(is.na(val))
      {
        flag<-TRUE
        colonnes <- c(colonnes,colonne)
        lignes <- c(lignes,ligne)
      }
      ligne <- ligne + 1
    }
  colonne <- colonne + 1
  ligne <- 1
}

print(flag)
singularity <- list(colonnes=colonnes,lignes=lignes)

print(singularity)