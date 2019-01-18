library(shiny)

firstTab <- tabItem(tabName = "firstTab",
  tags$div(class="jumbotron",
           #tags$h1("uHMM interface"),
           tags$p("Le but de cette interface est de détecter des événements usuels ou extremes dans un jeu de données, et de caractériser leur dynamique (en construisant un modele de Markov)")
  ),
  fluidRow(
    box(title="Schéma de l'interface",
        tags$ol(
          tags$li("Import des données"),
          tags$li("Métrologie"),
          tags$li("Sélection des variables"),
          tags$li("Classification"),
          tags$li("Modélisation de séries temporelles"),
          tags$li("Prédiction")
        )
    ),
    box(title = "Equipement recommandé",
        "CPU : X_86 64 bits", br(),
        "Vitesse : 3000MHZ", br(),
        "OS : Windows/Linux", br(),
        "Mémoire : 8GB", br(),
        "Place sur le disque dur pour sauvegarde : ", br(),
        "Logiciel : R 3.2.3"
    )
  ),
  fluidRow(
    box(title = "Sélection de la langue",
        radioButtons("langue","",
                     choices = list("Francais" = 1, "Anglais" = 2), 
                     selected = 1)
    ),
    box(title = "Sélection du mode utilisateur",
        radioButtons("usermode","",
                     choices = list("Standard (Recommandé)" = 1, "Expert" = 2), 
                     selected = 1 )
    )
  )
)