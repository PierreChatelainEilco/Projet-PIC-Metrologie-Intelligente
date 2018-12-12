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

.metrologyTab<-function(userType,tm,leftMargin=30,console,graphicFrame,win1,uHMMenv){
  
  tkfocus(win1$env$metrology)
  
  #### Fix button
  fixButton<-tk2button(win1$env$metrology,text=tm$fixDataLabel,image="fix",compound = "left")
  fixButton.fun <- function(){

    if(!exists("rawData",where=uHMMenv)){

      tkmessageBox(message=tm$noFileMsg,type="ok",icon="info", title=tm$warningLabel)

    }else{
      fixedData<-.unFixdata(impData=uHMMenv$rawData,dispTab=win1$env$import,fileName=uHMMenv$firstDataFile,tm=tm,
                            output=uHMMenv$saveDirectory,console=console,win1=win1,rowNum=0)
      assign("rawData",fixedData$impData,envir=uHMMenv)
      assign("firstDataFile",fixedData$newFileName,envir=uHMMenv)
    }
  }
  tkconfigure(fixButton,command=fixButton.fun)
  tkgrid(fixButton,row=5,column=1)

  # Next tab
  nextTabFrame <- tkwidget(win1$env$metrology,"labelframe",text=tm$titleNextFrame,borderwidth = 0)
  tkgrid(nextTabFrame , column=0,row=7,padx=c(leftMargin,0),pady=c(50,0),sticky="w")

  classifButton<-tk2button(nextTabFrame,text=tm$classificationTabLabel,image="run",compound = "left")
  classifButton.fun<-function(){

    if(!exists("rawData",where=uHMMenv)){
      assign("rawData",MarelCarnot,envir=uHMMenv)
      assign("firstDataFile","MarelCarnot",uHMMenv)
      assign("rawMoments",dateProcessing(MarelCarnot),envir=uHMMenv)
    }

    if(!exists("rawData",where=uHMMenv)){
      tkmessageBox(message=tm$noFileMsg,type="ok",icon="info", title=tm$warningLabel)
    }
    else if(uHMMenv$saveDirectory==""){
      tkmessageBox(message=tm$noDirectoryMsg,type="ok",icon="info", title=tm$warningLabel)
    }else{

      .variableTab(leftMargin=leftMargin,userType=userType,tm=tm,
                   console=console,graphicFrame=graphicFrame,win1=win1,
                   uHMMenv=uHMMenv)
      tk2notetab.select(win1$env$nb, tm$variableTabLabel)
      tkinsert(console,"1.0","\n---------------------------------------\n")

    }
  }
  tkconfigure(classifButton,command=classifButton.fun)
  tkgrid(classifButton,row=7,column=1)


  modelingButton<-tk2button(nextTabFrame,text=tm$modelingTabLabel,image="run",compound = "left")
  modelingButton.fun<-function(){

    if(!exists("rawData",where=uHMMenv)){
      assign("rawData",MarelCarnot,envir=uHMMenv)
      assign("firstDataFile","MarelCarnot",uHMMenv)
      assign("rawMoments",dateProcessing(MarelCarnot),envir=uHMMenv)
    }

    if(!exists("rawData",where=uHMMenv)){
      tkmessageBox(message=tm$noFileMsg,type="ok",icon="info", title=tm$warningLabel)
    }
    else if(uHMMenv$saveDirectory==""){
      tkmessageBox(message=tm$noDirectoryMsg,type="ok",icon="info", title=tm$warningLabel)
    }else{

      .modelingTab(leftMargin=leftMargin,tm=tm,
                   console=console,graphicFrame=graphicFrame,win1=win1,
                   uHMMenv=uHMMenv)
      tk2notetab.select(win1$env$nb, tm$modelingTabLabel)
      tkinsert(console,"1.0","\n---------------------------------------\n")
    }
  }
  tkconfigure(classifButton,command=classifButton.fun)
  tkgrid(modelingButton,row=8,column=1)


  predictButton<-tk2button(nextTabFrame,text=tm$predictionTabLabel,image="run",compound = "left")
  predictButton.fun<-function(){

    if(!exists("rawData",where=uHMMenv)){
      assign("rawData",MarelCarnot,envir=uHMMenv)
      assign("firstDataFile","MarelCarnot",uHMMenv)
      assign("rawMoments",dateProcessing(MarelCarnot),envir=uHMMenv)
    }

    if(!exists("rawData",where=uHMMenv)){
      tkmessageBox(message=tm$noFileMsg,type="ok",icon="info", title=tm$warningLabel)
    }
    else if(uHMMenv$saveDirectory==""){
      tkmessageBox(message=tm$noDirectoryMsg,type="ok",icon="info", title=tm$warningLabel)
    }else{
      assign("rawValidData",uHMMenv$rawData,envir=uHMMenv)
      assign("rawValidMoments",uHMMenv$rawMoments,envir=uHMMenv)
      .predictionTab(leftMargin=leftMargin,tm=tm,
                     console=console,graphicFrame=graphicFrame,win1=win1,
                     uHMMenv=uHMMenv)
      .periodSelectionFrame(data=uHMMenv$rawValidData,tm=tm,
                            leftMargin=leftMargin,uHMMenv=uHMMenv,win1=win1)
      tk2notetab.select(win1$env$nb, tm$predictionTabLabel)

      fileLab<-tklabel(win1$env$prediction,text=uHMMenv$firstDataFile)
      tkgrid(fileLab,row=3,column=1,sticky="w")

      tkinsert(console,"1.0","\n---------------------------------------\n")
    }

  }
  tkconfigure(predictButton,command=predictButton.fun)
  tkgrid(predictButton,row=9,column=1)
}
