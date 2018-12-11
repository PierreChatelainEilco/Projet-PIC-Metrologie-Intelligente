#' @title First tab of the uHMM interface
#' @description A user-friendly interface to detect usual or extreme events in a dataset and to characterize their dynamic, by building an unsupervised Hidden Markov Model
#' @param mainWindow the mainWindow of the uHMMinterface.
#' @param tm a one row dataframe containing text to display in the interface.
#' @param language character, either "en" (for English-speaking user) of "fr" (for French-speaking user).
#' @param uHMMenv an environment in which data and results will be stored.
#' @param leftMargin left magin size of interface tabs.
#' @param hscaleGraphicFrame the hscale parameter value of the tkplot function used to create the graphic frame.
#' @param vscaleGraphicFrame the vscale parameter value of the tkplot function used to create the graphic frame.
#' @return Results are saved in the directory chosen by the user.
#' @source Rousseeuw, Kevin, et al. "Hybrid hidden Markov model for marine environment monitoring." Selected Topics in Applied Earth Observations and Remote Sensing, IEEE Journal of 8.1 (2015): 204-213.
#' @import tcltk tcltk2 
#' @importFrom tkrplot tkrplot


.firstTab<-function(mainWindow,tm,language,uHMMenv,leftMargin=30,hscaleGraphicFrame=1.2,vscaleGraphicFrame=1.2){  
  if (language=="fr"){
    fontOverviewIntro <- tkfont.create(family = "Arial", size = 12, slant = "italic")
    tcl("image","create","photo", "HMMscheme", file =file.path(path.package("uHMM"),"figures", "schema_hmm_FR.gif", fsep = .Platform$file.sep))
  }else{
    fontOverviewIntro <- tkfont.create(family = "Arial", size = 16, slant = "italic")
    tcl("image","create","photo", "HMMscheme", file =file.path(path.package("uHMM"),"figures", "schema_hmm_EN.gif", fsep = .Platform$file.sep))
  }
  
  # Affichage logos
  .logoFrame(mainWindow)
  
  # Msg Frame  
  mainWindow$msgFrame <- tkwidget(mainWindow,"labelframe")
  tkconfigure(mainWindow$msgFrame, borderwidth = 0)
  tkgrid(mainWindow$msgFrame, column=2,row=1)
  #tkwm.geometry(msgFrame, "600x180")
  
  # msg frame title
  mainWindow$msgFrame$titleMsgFrameFrame <- tkwidget(mainWindow$msgFrame,"labelframe")
  tkconfigure(mainWindow$msgFrame$titleMsgFrameFrame,borderwidth = 0)
  #tkconfigure(mainWindow$msgFrame$titleMsgFrameFrame,background = "#ffffff")
  tkgrid(mainWindow$msgFrame$titleMsgFrameFrame,sticky="w")
  tkgrid(tk2label(mainWindow$msgFrame$titleMsgFrameFrame,text="Messages :"))
  
  mainWindow$msgFrame$scrx <- tk2scrollbar(mainWindow$msgFrame,orientation = "horizontal")
  tkconfigure(mainWindow$msgFrame$scrx, command = function(...) tkxview(console, ...))
  
  mainWindow$msgFrame$scry <- tk2scrollbar(mainWindow$msgFrame, orientation = "vertical")
  tkconfigure(mainWindow$msgFrame$scry, command = function(...) tkyview(console, ...))
  
  mainWindow$msgFrame$console <- tk2text(mainWindow$msgFrame)
  tkconfigure(mainWindow$msgFrame$console, width = 70)
  tkconfigure(mainWindow$msgFrame$console, height = 12)
  tkconfigure(mainWindow$msgFrame$console, wrap = "none")
  tkconfigure(mainWindow$msgFrame$console, xscrollcommand = function(...) tkset(scrx, ...))
  tkconfigure(mainWindow$msgFrame$console, yscrollcommand = function(...) tkset(scry, ...))
  
  tkgrid(mainWindow$msgFrame$console, mainWindow$msgFrame$scry, sticky = "nsew",pady=c(0,0))
  tkgrid.rowconfigure(mainWindow$msgFrame, mainWindow$msgFrame$console, weight = 1)
  tkgrid.columnconfigure(mainWindow$msgFrame, mainWindow$msgFrame$console, weight = 1)
  tkgrid(mainWindow$msgFrame$scrx, sticky = "ew")
  
# Graphic Frame
  mainWindow$graphicFrame <- tkwidget(mainWindow,"labelframe")
  tkconfigure(mainWindow$graphicFrame, borderwidth = 0)
  tkgrid(mainWindow$graphicFrame, column=2,row=2,rowspan=2,sticky="w")
  
  mainWindow$graphicFrame$GF <- tkrplot(mainWindow$graphicFrame,hscale=hscaleGraphicFrame,vscale=vscaleGraphicFrame,function(){par(bg = "white");plot(1,col="white",axes=FALSE,xlab=NA,ylab=NA)})
  tkgrid(mainWindow$graphicFrame$GF, row=0, column=1, sticky="w")  

# Tabs
  mainWindow$win1 <- tkwidget(mainWindow,"labelframe")
  tkconfigure(mainWindow$win1, borderwidth = 0)
  tkgrid(mainWindow$win1, column=1, row=1, rowspan=2)
  
  mainWindow$win1$nb <- tk2notebook(mainWindow$win1)
  tkconfigure(mainWindow$win1$nb, tabs = c(tm$overviewTabLabel,tm$importTabLabel,tm$variableTabLabel,tm$classificationTabLabel,tm$modelingTabLabel,tm$predictionTabLabel))
  tkpack(mainWindow$win1$nb, fill = "both", expand = TRUE)
  
  mainWindow$win1$nb$overview       <- tk2notetab(mainWindow$win1$nb, tm$overviewTabLabel)
  mainWindow$win1$nb$import         <- tk2notetab(mainWindow$win1$nb, tm$importTabLabel)
  mainWindow$win1$nb$variables      <- tk2notetab(mainWindow$win1$nb, tm$variableTabLabel)
  mainWindow$win1$nb$classification <- tk2notetab(mainWindow$win1$nb, tm$classificationTabLabel)
  mainWindow$win1$nb$modelisation   <- tk2notetab(mainWindow$win1$nb, tm$modelingTabLabel)
  mainWindow$win1$nb$prediction     <- tk2notetab(mainWindow$win1$nb, tm$predictionTabLabel)
  
  
  ### Overview tab
  mainWindow$win1$nb$overview$overviewIntro <- tklabel(mainWindow$win1$nb$overview)
  tkconfigure(mainWindow$win1$nb$overview$overviewIntro, text=tm$overviewIntro)
  tkconfigure(mainWindow$win1$nb$overview$overviewIntro, font = fontOverviewIntro)
  tkgrid(mainWindow$win1$nb$overview$overviewIntro, column=0,row=1, padx=c(20,20),pady = c(50,50),columnspan=10)
  
  # Scheme  
  schemeFont<-tkfont.create(slant = "italic",size=10)
  mainWindow$win1$nb$overview$schemeFrame <- tkwidget(mainWindow$win1$nb$overview,"labelframe")
  tkconfigure(mainWindow$win1$nb$overview$schemeFrame, text=tm$titleScheme)
  tkgrid(mainWindow$win1$nb$overview$schemeFrame, columnspan=1,rowspan=3,column=0, row=4,padx=leftMargin, sticky="w")
  
  # import
  mainWindow$win1$nb$overview$schemeFrame$cadreVisualisation<-tkwidget(mainWindow$win1$nb$overview$schemeFrame,"labelframe")
  tkgrid(mainWindow$win1$nb$overview$schemeFrame$cadreVisualisation, columnspan=1, row=4,padx=c(10,10),pady=c(10,0))
  
  mainWindow$win1$nb$overview$schemeFrame$cadreVisualisation$importScheme <- tklabel(mainWindow$win1$nb$overview$schemeFrame$cadreVisualisation)
  tkconfigure(mainWindow$win1$nb$overview$schemeFrame$cadreVisualisation$importScheme, text=tm$importScheme)
  tkconfigure(mainWindow$win1$nb$overview$schemeFrame$cadreVisualisation$importScheme, font = schemeFont)
  tkgrid(mainWindow$win1$nb$overview$schemeFrame$cadreVisualisation$importScheme,pady=3,column = 0, row = 4)
  
  tkgrid(ttklabel(mainWindow$win1$nb$overview$schemeFrame , image="imageID", compound="image"), columnspan=1, row=5)
  
  # variable selection
  mainWindow$win1$nb$overview$schemeFrame$cadreVariables <- tkwidget(mainWindow$win1$nb$overview$schemeFrame,"labelframe")
  tkgrid(mainWindow$win1$nb$overview$schemeFrame$cadreVariables, columnspan=1, row=6)
  tkgrid(tk2label(mainWindow$win1$nb$overview$schemeFrame$cadreVariables, text=tm$variableScheme,font = schemeFont),pady=3,column = 0, row = 4)
  tkgrid(ttklabel(mainWindow$win1$nb$overview$schemeFrame, image="imageID", compound="image"), columnspan=1, row=7)
  
  # classification
  mainWindow$win1$nb$overview$schemeFrame$cadreClassification <- tkwidget(mainWindow$win1$nb$overview$schemeFrame,"labelframe")
  tkgrid(mainWindow$win1$nb$overview$schemeFrame$cadreClassification, columnspan=1, row=8)
  tkgrid(tklabel(mainWindow$win1$nb$overview$schemeFrame$cadreClassification, text=tm$classificationScheme,font = schemeFont),pady=3,column = 0, row = 4)
  tkgrid(ttklabel(mainWindow$win1$nb$overview$schemeFrame, image="imageID", compound="image"), columnspan=1, row=9)
  
  # modelisation
  mainWindow$win1$nb$overview$schemeFrame$cadreModeling <- tkwidget(mainWindow$win1$nb$overview$schemeFrame,"labelframe")
  tkgrid(mainWindow$win1$nb$overview$schemeFrame$cadreModeling, columnspan=1, row=10)
  tkgrid(tklabel(mainWindow$win1$nb$overview$schemeFrame$cadreModeling, text=tm$modelingScheme,font = schemeFont),pady=3,column = 0, row = 4)
  tkgrid(ttklabel(mainWindow$win1$nb$overview$schemeFrame, image="imageID", compound="image"), columnspan=1, row=11)
  
  # prediction
  mainWindow$win1$nb$overview$schemeFrame$cadrePrediction<-tkwidget(mainWindow$win1$nb$overview$schemeFrame,"labelframe")
  tkgrid(mainWindow$win1$nb$overview$schemeFrame$cadrePrediction, columnspan=1, row=12,pady=c(0,10))
  tkgrid(tklabel(mainWindow$win1$nb$overview$schemeFrame$cadrePrediction, text=tm$predictionScheme,font = schemeFont),pady=3,column = 0, row = 4)
  
  #Recommended equipment Frame
  mainWindow$win1$nb$overview$recoFrame<-tkwidget(mainWindow$win1$nb$overview,"labelframe",text=tm$titleEquipment)
  tkgrid(mainWindow$win1$nb$overview$recoFrame, columnspan=1,column=1, row=4,sticky="w")
  tkgrid(tk2label(mainWindow$win1$nb$overview$recoFrame,text=tm$textEquipment), columnspan=1, row=4)  
  
  #Language selection block
  
  mainWindow$win1$nb$overview$languageBlock <- tkwidget(mainWindow$win1$nb$overview,"labelframe",text=tm$titleLanguageLabel)
  tkgrid(mainWindow$win1$nb$overview$languageBlock,column=1,row=5,sticky="w") 
  
  languageVar2 <- tclVar(language)

  mainWindow$win1$nb$overview$languageBlock$englishButton <- tkradiobutton(mainWindow$win1$nb$overview$languageBlock) 
  mainWindow$win1$nb$overview$languageBlock$frenchButton <- tkradiobutton(mainWindow$win1$nb$overview$languageBlock)
  
  # config des boutons radio. Une seule variable tcl pour 2 boutons
  tkconfigure(mainWindow$win1$nb$overview$languageBlock$englishButton,variable=languageVar2,value="en", text=tm$englishLabel)
  tkconfigure(mainWindow$win1$nb$overview$languageBlock$frenchButton,variable=languageVar2,value="fr", text=tm$frenchLabel)
  tkgrid(mainWindow$win1$nb$overview$languageBlock$englishButton, row=2,padx=20, sticky="w")
  tkgrid(mainWindow$win1$nb$overview$languageBlock$frenchButton, row=3,padx=20, sticky="w")
  
  #User type Frame
  
  mainWindow$win1$nb$overview$userFrame <- tkwidget(mainWindow$win1$nb$overview,"labelframe",text=tm$titleUserLabel)
  tkgrid(mainWindow$win1$nb$overview$userFrame,column=1,row=6,sticky="w") 
  
  user <- tclVar("default")
  
  mainWindow$win1$nb$overview$userFrame$defaultButton <- tkradiobutton(mainWindow$win1$nb$overview$userFrame) 
  mainWindow$win1$nb$overview$userFrame$expertButton <- tkradiobutton(mainWindow$win1$nb$overview$userFrame)
  
  # config des boutons radio. Une seule variable tcl pour 2 boutons
  tkconfigure(mainWindow$win1$nb$overview$userFrame$defaultButton,variable=user,value="default", text=tm$standardLabel)
  tkconfigure(mainWindow$win1$nb$overview$userFrame$expertButton,variable=user,value="expert", text=tm$expertLabel)
  tkgrid(mainWindow$win1$nb$overview$userFrame$defaultButton, row=2,padx=20, sticky="w")
  tkgrid(mainWindow$win1$nb$overview$userFrame$expertButton, row=3,padx=20, sticky="w")
  
  # Start button  
  mainWindow$win1$nb$overview$StartButton<-tk2button(mainWindow$win1$nb$overview)
  mainWindow$win1$nb$overview$StartButton.fun <- function(){
    if (language!=tclvalue(languageVar2)){
      tm<-.languageManagement(tclvalue(languageVar2))
    }
    tk2notetab.select(win1$env$nb, tm$importTabLabel)
    userType<-tclvalue(user)
    .importTab(leftMargin=leftMargin,userType=userType,tm=tm,
               console=console,graphicFrame=graphicFrame,win1=win1,uHMMenv=uHMMenv
    )
  } 
  tkconfigure(mainWindow$win1$nb$overview$StartButton,text=tm$startLabel)
  tkconfigure(mainWindow$win1$nb$overview$StartButton,image="run")
  tkconfigure(mainWindow$win1$nb$overview$StartButton,compound="left")
  tkconfigure(mainWindow$win1$nb$overview$StartButton,command=mainWindow$win1$nb$overview$StartButton.fun)
  tkgrid(mainWindow$win1$nb$overview$StartButton,column = 1, row = 14,padx = leftMargin, pady = c(50,120),sticky="w")
  
}  
  