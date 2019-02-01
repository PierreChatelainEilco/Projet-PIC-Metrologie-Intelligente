#fichier de données 
globalfile <- NULL
 filedata <- reactive({
   inFile <- input$file
   if (is.null(inFile)) {
    # User has not uploaded a file yet
     return(NULL)
   }
   globalfile <<- read.csv(inFile$datapath, as.is=TRUE)
   return(NULL)
})

#This previews the CSV data file
output$table <- renderTable({
  call.me = filedata()
  globalfile
})


observeEvent(input$suivant, {
  updateNavbarPage(session,"uHMM",
                   selected = "onglet2")
  output$table2 <- renderTable({
    globalfile <-input$dataset
  })
})