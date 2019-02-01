tabPanel(
  title = "Importation des fichiers",value="onglet1",
  sidebarPanel(
    
    fileInput("file", h4("Importer un fichier"),accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    
    
    fileInput("file2", h4("Importer la gamme capteur (optionnel)"), accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    fileInput("file3", h4("Importer le fichier de rotation des capteurs (optionnel)"),accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    
    basicPage(
      actionButton('afficherTab', 'Aperçu',
                   class = "btn-primary",
                   compound="center"
      )
    
  )),
  mainPanel(
    
    
      tabsetPanel(id="inTabset",
                tabPanel(title = "Besoin d'aide ?", h4("Important : comment importer vos donn\UE9\U65s en 8 \UE9tapes"),
                         br(),
                         p("Les donn\UE9\U65s doivent \UEAtre \UE9\U63hantillonn\UE9\U65s \UE0 pas constant"),
                         p("\nLes donn\UE9\U65s doivent \UEAtre dans un fichier .txt", span(em("(sauvegarder en tant que '.txt' dans votre feuille de calculs)"), style = "color:blue"),""),
                         p("\nLe s\UE9parateur d\UE9\U63imal doit \UEAtre le caract\UE8re '.' \nLes valeurs manquantes doivent \UEAtre labellis\UE9\U65s 'NA'"),
                         p("\nLes dates des observations doivent \UEAtre renseign\UE9\U65s dans une colonne intitul\UE9\U65 'Dates'"),
                         p("\nCes dates doivent \UEAtre au format 'AAAA-MM-JJ",span(em("(format ISO 8601)")),""),
                         p("\nLes heures des observations doivent \UEAtre renseign\UE9\U65s dans une colonne intitul\UE9\U65 'Hours'"),
                         p("\nCes heures doivent \UEAtre au format 'HH:MM:SS'",span(em("(format ISO 8601)")),"")
                         
                )
                
    )
  ), 
  actionButton('suivant', 'Suivant', compound="center")
  
)