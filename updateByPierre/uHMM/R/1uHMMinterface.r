#' @title Graphical Interface to Build an uHMM
#' @description A user-friendly interface to detect usual or extreme events in a dataset and to characterize their dynamic,
#'  by building an unsupervised Hidden Markov Model.
#' @param uHMMenv an environment in which data and results will be stored. If NULL, a local environment will be created.
#' @return Results are saved in the directory chosen by the user.
#' @source Rousseeuw, Kevin, et al. "Hybrid hidden Markov model for marine environment monitoring." Selected Topics in Applied Earth Observations and Remote Sensing, IEEE Journal of 8.1 (2015): 204-213.
#' @import tcltk tcltk2 
#' @importFrom tkrplot tkrplot
#' @export

uHMMinterface<-function(uHMMenv=NULL){
  
  if (is.null(uHMMenv)){
    uHMMenv <- new.env(hash = TRUE, size = NA)
  }
  
  # language selection window
  languageWindow <- tktoplevel()
  tktitle(languageWindow) <- "uHMM interface"
  .logoFrame(languageWindow)
  
  # Language selection frame
  languageWindow$languageBlock <- tkwidget(languageWindow, "labelframe")
  tkconfigure(languageWindow$languageBlock, text=paste("Please select your language/\nVeuillez s",intToUtf8(0233),"lectionner votre langue",sep=""))
  tkconfigure(languageWindow$languageBlock, borderwidth=0)
  tkgrid(languageWindow$languageBlock, padx=c(100,100), pady=c(50,50), column=1, row=1) 
  
  # Engish Button
  languageWindow$languageBlock$englishButton <- tkbutton(languageWindow$languageBlock)
  languageWindow$languageBlock$englishButton.fun <- function(){
    tkdestroy(languageWindow)
    tm <- .languageManagement("en")
    mainWindow <- tktoplevel()
    tktitle(mainWindow) <- tm$mainWindowTitle  
    .firstTab(mainWindow,tm,"en",uHMMenv,leftMargin=30,hscaleGraphicFrame=1.2,vscaleGraphicFrame=1.2)
  }
  tkconfigure(languageWindow$languageBlock$englishButton, text="English/Anglais")
  tkconfigure(languageWindow$languageBlock$englishButton, compound="left")
  tkconfigure(languageWindow$languageBlock$englishButton, command=languageWindow$languageBlock$englishButton.fun)
  tkgrid(languageWindow$languageBlock$englishButton,padx=20)
  
  # French Button
  languageWindow$languageBlock$frenchButton <- tkbutton(languageWindow$languageBlock)
  languageWindow$languageBlock$frenchButton.fun <- function(){
    tkdestroy(languageWindow) 
    tm <- .languageManagement("fr")
    mainWindow <- tktoplevel()
    #tkwm.geometry(mainWindow, "1350x760")
    tktitle(mainWindow) <- tm$mainWindowTitle  
    .firstTab(mainWindow,tm,"fr",uHMMenv,leftMargin=30,hscaleGraphicFrame=1.2,vscaleGraphicFrame=1.2)
  }
  tkconfigure(languageWindow$languageBlock$frenchButton, text=paste("French/Fran",intToUtf8(0231),"ais",sep=""))
  tkconfigure(languageWindow$languageBlock$frenchButton, compound="left")
  tkconfigure(languageWindow$languageBlock$frenchButton, command=languageWindow$languageBlock$frenchButton.fun)
  tkgrid(languageWindow$languageBlock$frenchButton,padx=20)

}
