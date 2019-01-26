#plotting theme for ggplot2
.theme<- theme(
  axis.line = element_line(colour = 'gray', size = .75),
  panel.background = element_blank(),
  plot.background = element_blank()
)


#if( Valeurs manquantes/Valeurs aberrantes){ 
observeEvent(input$popup, {
  showModal(modalDialog(
    title = "alerte","Des anomalies ont \UE9t\UE9 d\UE9tect\UE9\U65s lors de la lecture des donn\UE9\U65s !",
    br(),
    p("Nombre de valeurs manquantes: $value1"),
    p("Nombre de valeurs aberrantes: $value2"),
    br(),
    p(em("Souhaitez-vous les corriger ?"), style = "color:blue"),
    footer = modalButton("Ignorer"), modalButton("Corriger")
  )
  )
})


