#' @title Import tab of the uHMM interface
#' @description This function generates the import tab of the \code{\link{uHMMinterface}}, in which the user can import a data file and select a save directory.
#' @param userType character : either "default" or "expert"
#' @param leftMargin left magin size of interface tabs.
#' @param tm a one row dataframe containing text to display in the interface.
#' @param console frame of the uHMM interface in which messages should be displayed. 
#' @param graphicFrame frame of the uHMM interface in which graphics should be dispayed.
#' @param win1 frame of the uHMM interface containing main tabs.
#' @param uHMMenv environment in which data and intermediate results are stored.
#' @import tcltk tcltk2
#' @export

.importTab<-function(userType,tm,leftMargin=30, console,graphicFrame,win1,uHMMenv){
  
  tkfocus(win1$env$import)
  
  # Mise en place du bouton pour rechercher la base de donnees au format TXT
  browseButton<-tk2button(win1$env$import,text=tm$importTxtLabel,image="data",compound = "left")
  browseButton.fun <- function(){
    imp<-.importFile(win1$env$import,dispFileRow=0,fixSumRow=5,tm=tm,console=console,win1=win1)

    assign("rawData",imp$impData,envir=uHMMenv)
    assign("rawMoments",dateProcessing(uHMMenv$rawData),envir=uHMMenv)
    assign("firstDataFile",imp$fileName,envir=uHMMenv)
    
    #Décocher MarelCarnot
    tclvalue(exampleSet) <- 0
  }
  tkconfigure(browseButton,command=browseButton.fun);
  tkgrid(browseButton,row=0,column=0,sticky="w",padx=c(leftMargin,0),pady=c(20,0))
  
  # Import MarelCarnot data
  importMarel <- tk2checkbutton(win1$env$import, text = tm$IstepImportExample)
  importMarel.fun <- function()
  {
    if (!exists("rawData",where=uHMMenv) & tclvalue(exampleSet)=="1"){
      assign("firstDataFile","MarelCarnot",uHMMenv)
      assign("rawData",MarelCarnot,envir=uHMMenv)
      assign("rawMoments",dateProcessing(uHMMenv$rawData),envir=uHMMenv)
    }else if(exists("rawData",where=uHMMenv) & tclvalue(exampleSet)=="1"){
      #Afficher le jeu de donnees va être remplacer par MarelCarnot
      assign("firstDataFile","MarelCarnot",uHMMenv)
      assign("rawData",MarelCarnot,envir=uHMMenv)
      assign("rawMoments",dateProcessing(uHMMenv$rawData),envir=uHMMenv)
    }
  }
  exampleSet <- tclVar("0")
  tkconfigure(importMarel, variable = exampleSet)
  tkconfigure(importMarel, command = importMarel.fun)
  tkgrid(importMarel, row=1,column=0,sticky="w",padx=c(leftMargin*2,0),pady=c(0,10))
  
  # Directory selection
  directoryButton <- tk2button(win1$env$import,text=tm$directoryButtonLabel,image="save",compound = "left")
  directoryButton.fun <- function(){
    .saveDirectory(win1$env$import, output="saveDirectory",tm=tm,uHMMenv=uHMMenv)#,fileName=firstDataFile,fixSumRow=5)
  }
  tkconfigure(directoryButton, command=directoryButton.fun)
  tkgrid(directoryButton, column=0, row=2,sticky="w",padx=c(leftMargin,0))
  
  #Creation du bouton permettant de faire un summary
  summaryButton<-tk2button(win1$env$import,text=tm$summaryLabel,image="loupe",compound = "left")
  summaryButton.fun <-function(){
    if(!exists("rawData",where=uHMMenv)){
      tkmessageBox(message=tm$noFileMsg,type="ok",icon="info", title=tm$warningLabel)
    }else{
      if(exists("tablesOutput",where=uHMMenv)){
        .unSummary(uHMMenv$rawData,uHMMenv$tablesOutput,summaryLabel=tm$summaryLabel)
      }else{
        .unSummary(uHMMenv$rawData,summaryLabel=tm$summaryLabel)
        }
    }
  }
  tkconfigure(summaryButton,command=summaryButton.fun)
  tkgrid(summaryButton,row=5,column=0)
  
  #Creation du bloc precisant ce que l'on veut comme fichier TXT
  AdviceFrame <- tkwidget(win1$env$import,"labelframe",text=tm$titleImportFrame,padx=30,pady=8, relief = "groove") # cadre de texte
  tkgrid(AdviceFrame, columnspan=3, row=3, sticky="w",padx=c(leftMargin,0),pady=c(10,10))
  tkgrid(tk2label(AdviceFrame, text =tm$textImportFrame)) 
  
######  
  
  #Creation du cadre de validation du fichier demande
  tkgrid(tklabel(win1$env$import, text="      "), column=1, row=6)
  
  nextTabFrame <- tkwidget(win1$env$import,"labelframe",text=tm$titleNextFrame,borderwidth = 0)
  tkgrid(nextTabFrame , column=0,row=7,padx=c(leftMargin,0),pady=c(50,0),sticky="w")
  
  metrologyButton <- tk2button(nextTabFrame,text=tm$metrologyTabLabel,image="run",compound = "left")
  metrologyButton.fun <- function(){
    if(!exists("rawData",where=uHMMenv)){
      tkmessageBox(message=tm$noFileMsg,type="ok",icon="info", title=tm$warningLabel)
    }else if(uHMMenv$saveDirectory==""){
      tkmessageBox(message=tm$noDirectoryMsg,type="ok",icon="info", title=tm$warningLabel)
    }else{
      .metrologyTab(leftMargin=leftMargin,userType=userType,tm=tm,
                    console=console,graphicFrame=graphicFrame,win1=win1,
                    uHMMenv=uHMMenv)
      tk2notetab.select(win1$env$nb, tm$metrologyTabLabel)
    }
  }
  tkconfigure(metrologyButton,command=metrologyButton.fun)
  tkgrid(metrologyButton,row=10,column=1)
  
  tkgrid(tklabel(win1$env$import, text="      "), column=1, row=8)
}
  