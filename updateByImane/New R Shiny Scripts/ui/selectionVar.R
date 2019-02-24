tabPanel(title ="S\UE9lection des Variables",value="onglet3",
  #inputs
  sidebarPanel(
    selectInput("",h4("S\UE9lection des variables"),choices=list()),
    hr(),
    h4("S\UE9lection de la p\UE9riode d'apprentissage"),
    textInput("","De"),
    textInput("",""),
    textInput("","à"),
    textInput("","")
  ),
  #outputs
  mainPanel(
    h3("Variables incluses dans le mod\UE6le"),
    hr(),
    h3("Analyse exploratoire"),
    actionButton("","Graphiques"),
    actionButton("","Bowplots"),
    actionButton("","Corr\UE9lations"),
    actionButton("","ACP")
  )
)
