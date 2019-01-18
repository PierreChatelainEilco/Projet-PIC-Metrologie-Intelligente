library(shiny)

importTab <- tabItem(tabName = "importTab",
  fluidRow(
    box(title="Importer un jeu de données")
  ),
  fluidRow(
    box(title="Important : Commant importer vos données en 8 étapes",class="col-12",
        tags$ol(
          tags$li("Les données doivent être échantillonnées à pas constant"),
          tags$li("Les données doivent être dans un fichier .txt (sauvegarder en tant que '.txt' dans votre feuille de calculs"),
          tags$li("Le séparateur décimal doit être le caractère '.'"),
          tags$li("Les valeurs manquantes doivent être labellisées 'NA'"),
          tags$li("Les dates des observation doivent être renseignées dans une colonne intitulé 'Dates'"),
          tags$li("Ces date doivent être au format 'AAAA-MM-JJ' (format ISO 8601)"),
          tags$li("Les heures des observations doivent être renseignées dans une colonne intitulée 'Hours'"),
          tags$li("Ces heures doivent être au format 'HH:MM:SS' (format ISO 8601)")
        )
      )
  ),fluidRow(),fluidRow())
