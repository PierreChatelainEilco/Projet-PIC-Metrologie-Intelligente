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

.metrologyTab<-function(userType,tm,leftMargin=30,
                     console,graphicFrame,win1,uHMMenv){
  
  tkfocus(win1$env$metrology)
  
  
}
