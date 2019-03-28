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

.importTab<-function(userType,tm,leftMargin=30,console,graphicFrame,win1,uHMMenv){
  
  tkfocus(win1$env$import)
  
  #Mise en place du bouton pour rechercher la base de donnees au format TXT
  browseButton<-tk2button(win1$env$import,text=tm$importTxtLabel,image="data",compound = "left")
  browseButton.fun<-function(){
    imp<-.importFile(win1$env$import,dispFileRow=0,fixSumRow=5,tm=tm,console=console,win1=win1)

    assign("rawData",imp$impData,envir=uHMMenv)
    assign("rawMoments",.dateProcessing(uHMMenv$rawData),envir=uHMMenv)
    assign("firstDataFile",imp$fileName,envir=uHMMenv)
    
    #Décocher MarelCarnot
    tclvalue(exampleSet)<-0
  }
  tkconfigure(browseButton,command=browseButton.fun)
  tkgrid(browseButton,row=0,column=0,sticky="w",padx=c(leftMargin,0),pady=c(20,0))
  
  # Mise en place du bouton pour rechercher la base de donnees au format TXT pour la gamme capteur
  browseButton<-tk2button(win1$env$import,text=tm$importTxtGamme,image="data",compound = "left")
  browseButton.fun <- function(){
    imp<-.importFile(win1$env$import,dispFileRow=0,fixSumRow=5,tm=tm,console=console,win1=win1)
    
    assign("rawData",imp$impData,envir=uHMMenv)
    assign("rawMoments",dateProcessing(uHMMenv$rawData),envir=uHMMenv)
    assign("firstDataFile",imp$fileName,envir=uHMMenv)
    
    #Décocher MarelCarnot
    tclvalue(exampleSet1) <- 0
  }
  tkconfigure(browseButton,command=browseButton.fun);
  tkgrid(browseButton,row=1,column=0,sticky="w",padx=c(leftMargin,0),pady=c(20,0))
  
  # Mise en place du bouton pour rechercher la base de donnees au format TXT pour la gamme de rotation
  browseButton<-tk2button(win1$env$import,text=tm$importTxtRotation,image="data",compound = "left")
  browseButton.fun <- function(){
    imp<-.importFile(win1$env$import,dispFileRow=0,fixSumRow=5,tm=tm,console=console,win1=win1)
    
    assign("rawData",imp$impData,envir=uHMMenv)
    assign("rawMoments",dateProcessing(uHMMenv$rawData),envir=uHMMenv)
    assign("firstDataFile",imp$fileName,envir=uHMMenv)
  }
  tkconfigure(browseButton,command=browseButton.fun);
  tkgrid(browseButton,row=2,column=0,sticky="w",padx=c(leftMargin,0),pady=c(20,0))
  
  # Import MarelCarnot data
  importMarel <- tk2checkbutton(win1$env$import, text = tm$IstepImportExample)
  importMarel.fun <- function()
  {
    if (!exists("rawData",where=uHMMenv) & tclvalue(exampleSet)=="1"){
      assign("firstDataFile","MarelCarnot",uHMMenv)
      assign("rawData",MarelCarnot,envir=uHMMenv)
      assign("rawMoments",.dateProcessing(uHMMenv$rawData),envir=uHMMenv)
    }else if(exists("rawData",where=uHMMenv) & tclvalue(exampleSet)=="1"){
      #Afficher le jeu de donnees va ?tre remplacer par MarelCarnot
      assign("firstDataFile","MarelCarnot",uHMMenv)
      assign("rawData",MarelCarnot,envir=uHMMenv)
      assign("rawMoments",.dateProcessing(uHMMenv$rawData),envir=uHMMenv)
    }
  }
  exampleSet <- tclVar("0")
  tkconfigure(importMarel, variable = exampleSet)
  tkconfigure(importMarel, command = importMarel.fun)
  tkgrid(importMarel, row=0,column=1,sticky="w",padx=c(leftMargin*2,0),pady=c(20,0))
  
  
  # Import Sensor Range data
  # importSensor <- tk2checkbutton(win1$env$import, text = tm$IstepImportSensorExample)
  # importSensor.fun <- function()
  # {
  #   if (!exists("rawData",where=uHMMenv) & tclvalue(exampleSet)=="1"){
  #     assign("firstDataFile","MarelCarnotSensor",uHMMenv)
  #     assign("rawData",MarelCarnotSensor,envir=uHMMenv)
  #     assign("rawMoments",dateProcessing(uHMMenv$rawData),envir=uHMMenv)
  #   }else if(exists("rawData",where=uHMMenv) & tclvalue(exampleSet)=="1"){
  #     #Afficher le jeu de donnees va ?tre remplacer par MarelCarnot
  #     assign("firstDataFile","MarelCarnotSensor",uHMMenv)
  #     assign("rawData",MarelCarnotSensor,envir=uHMMenv)
  #     assign("rawMoments",dateProcessing(uHMMenv$rawData),envir=uHMMenv)
  #   }
  # }
  # exampleSet1 <- tclVar("0")
  # tkconfigure(importSensor, variable = exampleSet1)
  # tkconfigure(importSensor, command = importSensor.fun)
  # tkgrid(importSensor, row=1,column=1,sticky="w",padx=c(leftMargin*2,0),pady=c(20,0))
  
  # Directory selection
  directoryButton <- tk2button(win1$env$import,text=tm$directoryButtonLabel,image="save",compound = "left")
  directoryButton.fun<-function(){
    
    .saveDirectory(win1$env$import, output="saveDirectory",tm=tm,uHMMenv=uHMMenv)#,fileName=firstDataFile,fixSumRow=5)
    
    # Fix button
    fixButton<-tk2button(buttonsFrame,text=tm$fixDataLabel,image="fix",compound = "left",width=9)
    fixButton.fun<-function(){
      if(!exists("saveDirectory",where=uHMMenv)){
        tkmessageBox(message=tm$noDirectoryMsg,type="ok",icon="info", title=tm$warningLabel)
        
      }else if(!exists("rawData",where=uHMMenv)){
        tkmessageBox(message=tm$noFileMsg,type="ok",icon="info", title=tm$warningLabel)
        
      }else if(uHMMenv$saveDirectory==""){
        tkmessageBox(message=tm$noFileMsg,type="ok",icon="info", title=tm$warningLabel)
        
      }else{
        fixedData<-.unFixdata(impData=uHMMenv$rawData,dispTab=win1$env$import,fileName=uHMMenv$firstDataFile,tm=tm,
                               output=paste(uHMMenv$saveDirectory,tm$rawDataRepertory,tm$tablesRepertory,sep=""),console=console,win1=win1,rowNum=0)
        assign("rawData",fixedData$impData,envir=uHMMenv)
        assign("firstDataFile",fixedData$newFileName,envir=uHMMenv)
      }
    }
    tkconfigure(fixButton,command=fixButton.fun)
    tkgrid(fixButton,row=1,column=2,padx=10)

    # Report button
    reportButton <- tk2button(buttonsFrame,text=tm$reportLabel,image="fix",compound = "left",width=9)
    reportButton.fun <- function(){
      tkmessageBox(message="Chaque chose en son temps petit scarabé !",type="ok",icon="info",title="C'est pour bientôt !")
    }
    tkconfigure(reportButton,command=reportButton.fun)
    tkgrid(reportButton,row=1,column=3,padx=10)
  }
  tkconfigure(directoryButton,command=directoryButton.fun)
  tkgrid(directoryButton, column=0, row=8,sticky="w",padx=c(leftMargin,0))
  
  
  buttonsFrame <- tkwidget(win1$env$import,"labelframe",text="",borderwidth = 0)
  tkgrid(buttonsFrame,row=13,column=0,padx=c(leftMargin,0),sticky="w")
  
  #Creation du bouton permettant de faire un summary
  summaryButton<-tk2button(buttonsFrame,text=tm$summaryLabel,image="loupe",compound ="left",width=9)
  summaryButton.fun<-function(){
    
    if(!exists("rawData",where=uHMMenv)){
      tkmessageBox(message=tm$noFileMsg,type="ok",icon="info", title=tm$warningLabel)
      
    }else if(exists("saveDirectory",where=uHMMenv)){
        .unSummary(uHMMenv$rawData,directory=paste(uHMMenv$saveDirectory,tm$rawDataRepertory,tm$tablesRepertory,sep=""),summaryLabel=tm$summaryLabel)
      
    }else{
        .unSummary(uHMMenv$rawData,summaryLabel=tm$summaryLabel)
        
    }
  }
  tkconfigure(summaryButton,command=summaryButton.fun)
  tkgrid(summaryButton,row=1,column=1,padx=10)
  
  
  #Creation du bloc precisant ce que l'on veut comme fichier TXT
  
  AdviceFrame <- tkwidget(win1$env$import,"labelframe",text=tm$titleImportFrame,padx=30,pady=8, relief = "groove") # cadre de texte
  tkgrid(AdviceFrame, columnspan=3, row=10, sticky="w",padx=c(leftMargin,0),pady=c(10,10))
  tkgrid(tk2label(AdviceFrame, text =tm$textImportFrame)) 
  

  ######  
  
  #Creation du cadre de validation du fichier demande
  tkgrid(tklabel(win1$env$import, text="      "), column=1, row=6)
  
  nextTabFrame <- tkwidget(win1$env$import,"labelframe",text=tm$titleNextFrame,borderwidth = 0)
  tkgrid(nextTabFrame , column=0,row=14,padx=c(leftMargin,0),pady=c(50,0),sticky="w")
  
  metrologyButton <- tk2button(nextTabFrame,text=tm$metrologyTabLabel,image="run",compound = "left")
  metrologyButton.fun <- function(){
    if(!exists("rawData",where=uHMMenv)){
      tkmessageBox(message=tm$noFileMsg,type="ok",icon="info", title=tm$warningLabel)
    }else if(!exists("saveDirectory",where=uHMMenv)){
      tkmessageBox(message=tm$noDirectoryMsg,type="ok",icon="info", title=tm$warningLabel)
    }else{
      .metrologyTab(leftMargin=leftMargin,userType=userType,tm=tm,
                    console=console,graphicFrame=graphicFrame,win1=win1,
                    uHMMenv=uHMMenv)
      tk2notetab.select(win1$env$nb, tm$metrologyTabLabel)
    }
  }
  tkconfigure(metrologyButton,command=metrologyButton.fun)
  tkgrid(metrologyButton,row=1,column=1)
  
  tkgrid(tklabel(win1$env$import, text="      "), column=1, row=14)
}
  