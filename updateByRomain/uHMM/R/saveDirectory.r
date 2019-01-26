#' saveDirectory
#' @title Import file function
#' @description Let a MMCNS interface user select his save directory, and create a button to fix data 
#' @param dispTab tab in which buttons and texts must be displayed
#' @param output name of the variable in which save directory path will be stored.
#' @param tm a one row dataframe containing text to display in the interface.
#' @param uHMMenv environment in which paths are stored.
#' @author Paul
#' @import tcltk tcltk2

#Fonction pour le dossier de sauvegarde
.saveDirectory<-function(dispTab=dispTab,output,tm,uHMMenv){ #,fileName,fixSumRow=5){
  
  theDirectory <- tclvalue(tkchooseDirectory(mustexist=TRUE,title=tm$titleDirectoryWindow))
  
  assign(output,theDirectory,envir=uHMMenv); ### sortie<-theDirectory
  
  afficheDossier<-tklabel(dispTab,text=theDirectory)
  tkgrid(afficheDossier,row=8,column=1,sticky="w")
  
  # Creation des sous-repertoires
  #Repertoire des figures
  if (dir.exists(paste(theDirectory,tm$diagramsRepertoryLabel,sep=""))==0){
    dir.create(path=paste(theDirectory,tm$diagramsRepertoryLabel,sep=""), recursive = TRUE)
  }
  assign("diagramsOutput",paste(theDirectory,tm$diagramsRepertoryLabel,sep=""),envir=uHMMenv) #sortieFigures
  
  #Repertoire des tableaux
  if (dir.exists(paste(theDirectory,tm$tablesRepertoryLabel,sep=""))==0){
    dir.create(path=paste(theDirectory,tm$tablesRepertoryLabel,sep=""), recursive = TRUE)
  }
  assign("tablesOutput",paste(theDirectory,tm$tablesRepertoryLabel,sep=""),envir=uHMMenv) #sortieTableaux
  
  #Repertoire des resultats R
  if (dir.exists(paste(theDirectory,tm$rfilesRepertoryLabel,sep=""))==0){
    dir.create(path=paste(theDirectory,tm$rfilesRepertoryLabel,sep=""), recursive = TRUE)
  }
  assign("rfilesOutput",paste(theDirectory,tm$rfilesRepertoryLabel,sep=""),envir=uHMMenv) #sortieFichiersR
  
}