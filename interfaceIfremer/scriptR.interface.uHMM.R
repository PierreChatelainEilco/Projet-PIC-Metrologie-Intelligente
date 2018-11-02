# installer les paquets ncessaires à l'interface uHMM
# etape inutile si telechargement via le CRAN
listP=c("tcltk2","chron","tkrplot","FactoMineR","HMM","corrplot","class","cluster","clValid")
install.packages(listP)

# pour installer l'interface depuis le fichier tar.gz
#modifier repTarGZ selon l'emplacement du fichier tar.gz
repTarGZ="~/Code/interfaceIfremer/"; 
install.packages(paste(repTarGZ,"uHMM_1.0.tar.gz",sep=""), repos = NULL, type = "source")

#lancement de l'interface
library(uHMM)
uHMMinterface()
