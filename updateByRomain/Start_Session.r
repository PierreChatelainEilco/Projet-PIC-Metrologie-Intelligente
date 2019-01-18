library(uHMM);

# Charger les bibliotheques
paquets=c("tcltk","tcltk2","tkrplot", "HMM", "clValid", "class", "cluster","FactoMineR", "corrplot", "chron");
for(paquet in paquets)
{
  library(paquet,character.only = T);
}

# Charger les fichiers R
repertoireSource="C:\\Users\\vergo\\Desktop\\R_Project\\uHMM\\R";
fichiers = list.files(path=repertoireSource,pattern=".r",full.names = T);
for(fichier in fichiers)
{
  source(fichier);
}
uHMMinterface()

