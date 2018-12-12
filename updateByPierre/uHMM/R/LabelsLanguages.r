#' @title Language management for uHMM interface
#' @description This function generates the text to display in the uHMM interface.
#' @param language string which indicate the language of the user (whether "en" or "fr")
#' @author Paul Ternynck
#' @return Data frame containing uHMM texts in the required language.
#' @details none

.languageManagement<-function(language){
  
  id<-c(
    "mainWindowTitle","overviewIntro","overviewTabLabel","importTabLabel","metrologyTabLabel","variableTabLabel","classificationTabLabel","modelingTabLabel","predictionTabLabel","titleScheme","importScheme","metrologyScheme","variableScheme","classificationScheme","modelingScheme","predictionScheme","titleEquipment","textEquipment","titleLanguageLabel","englishLabel","frenchLabel","titleUserLabel","standardLabel","expertLabel","startLabel","importTxtLabel","titleImportFrame","textImportFrame","titleNextFrame","noFileMsg","noDirectoryMsg","warningLabel","titleDirectoryWindow","directoryButtonLabel","summaryLabel","fixDataLabel","fixedInFileName","titleParamList","allParamLabel","noParamMsg","titleExploratoryFrame","datasetVariableListTitle","modelVariableListTitle","titlePlotWindow","plotButtonLabel","boxplotButtonLabel","displayButtonLabel","titlePcaCircle","dimensionWarning","correlationButtonLabel","pcaLabel","twoParamsWarning","titleBoxplotWindow","titlePeriodFrame","fromLabel","toLabel","yesLabel","noLabel","runLabel","corrplotGlobalTitle","corrplotTitle","completeObservations","selectedVariables","selectedPeriodFrom","toMinLabel","textClassificationDefault","normalizationFrameTitle","normalizationFrameText","selectMethodTitle","KmeansLabel","SpectralLabel","SpectralAdviceLabel","vectorialQuantization","explainedVariance","stateNumberLabel","autoStateNumberLabel","stopCriteriaLabel","principalEigenValueLabel","gapLabel","classificationResultsButtonLabel","titleClassifImportFrame","textClassifImportFrame","titleClassifResultsWindow","textClassifResultsWindow","titleClassifWarning","textClassifWarning","textEstimateMMCWindow","titleEstimateMMCWindow","titleProgramIsOverWindow","textProgramIsOverWindow","textVarianceErrorWindow","titleVarianceErrorWindow","textModelingResultsWindow","titleModelingResultsWindow","modelingResultsButtonLabel","titleModelingImportFrame","textModelingImportFrame","importPredictionTxtLabel","titleImportPredictionFrame","textImportPredictionFrame","RunPredictionButtonLabel","textPredictionResultsWindow","titlePredictionResultsWindow","seqClassifTitle","seqClassifFileTitle","seqModelTitle","CstepNormalization","CstepFastSpectralJordan","CstepComputingK","CstepSymbols","CstepSaving","CstepDone","CstepGraphics","CstepDuration","secondsLabel","CstepVariables","CstepObservations","CstepNAnumber","CstepDetectedStates","CstepKmeansHW","CstepClassifResults","VstepNA","IstepColumns","IstepRows","IstepIncompleteRows","IstepNA","IstepFixedPath","MstepStates","MstepSymbols","MstepDuration","MstepAssignSymbols","MstepViterbi","MstepEstimateModel","MstepDone","PstepAssignSymbols","PstepViterbi","PstepGraphics","PstepDone","seqFileJPG","seqPredictionTitle","selectedRawDataRepertory","outOfLabel","nbPrototypesLabel","nbGroupsInSamplesLabel","computeMNCUTLabel","computeSilLabel","diagramsRepertoryLabel","tablesRepertoryLabel","rfilesRepertoryLabel","titlePeriodFrame2","overallNA","MstepParameterEstimation","PstepModelingWarning","MstepClassificationWarning","IstepImportExample","confirmLabel","confirmCloseMsg","confirmResetMsg"
  )
  
  if(language=="en"){
    textToDisplay<-c(
      "uHMM interface",
      "The aim of this interface is to detect usual or extreme events in a dataset,\n and to characterize their dynamic (by building a Markov model).",
      "Overview",
      "Import",
      "Metrology",
      "Variable selection",
      "Classification",
      "Time series modeling",
      "Prediction",
      "Interface scheme",
      "        Data import        ",
      "         Metrology         ",
      "   Variable selection   ",
      "       Classification       ",
      "Time series modeling",
      "         Prediction         ",
      "Recommended equipment",
      "CPU: X_86 64 bits speed 3000 MHz\nOS: Windows/Linux\nMemory: 8 GB\nDisk space for save: about...\nSoftware: R 3.2.3",
      "Change Language",
      "English",
      "French",
      "User mode selection",
      "Standard (recommended)",
      "Expert",
      "Start",
      "Import TXT file",
      "Important : how to well import your data in 8 steps",
      "Data must be regularly sampled\nYour data must be in a .txt file (save as '.txt' format in your Spreadsheet)\nDecimal separtor must be '.'\nMissing value must be 'NA'\nDates must be in  'yyyy-mm-dd' (ISO 8601 time format)\nDates column -> Dates\nHours must be in  'hh:mm:ss' (ISO 8601 time format)\nHours column -> Hours)",
      "Next step",
      "No file selected!",
      "No save directory selected!",
      "Warning!",
      "Choose a save directory, then click OK.",
      "Select your save directory",
      "Summary",
      "Fix Data",
      "_fixed",
      "Variable selection",
      "- All variables -",
      "No parameter selected!",
      "Exploratory analysis",
      "Dataset variables",
      "Variables included in the model",
      "Plot",
      "Plots",
      "Boxplots",
      "Display",
      "PCA correlation circle",
      "The number of dimensions is between 0 and ",
      "Correlations",
      "PCA",
      "Select at least two parameters!",
      "Boxplot",
      "Learning period selection",
      "From",
      "To",
      "Yes",
      "No",
      "Run",
      "Global correlation matrix",
      "Correlation matrix",
      "complete observations: ",
      "Selected variables: ",
      "Selected period: from ",
      " to ",
      "Here are the parameters used by the interface for standard users:\n     - state detection is realized on standardized data ;\n     - classification method used is the spectral classification ;\n     - optimal number of states is computed by the interface, using the gap between eigen values as criteria.\n\nTo select alternative options, please launch the interface as expert user.",
      "Normalization",
      "Should data be normalized?",
      "Select your classification method",
      "K-means",
      "Spectral classification",
      "If you don't know the structure of your data,\nyou should select spectral classification",
      "Vectorial quantization",
      "Explained variance (in %): ",
      "number of states: ",
      "0 = Automatic",
      "Stop criteria",
      "Principal eigen values",
      "Gap",
      "Import results from a previous classification",
      "Important : which file to import",
      "The file should be saved as an archive in the repertory '/Rfiles' from your previous session,\nIts name should be 'classificationOutput_'+date",
      "Classification is over",
      "Would you like to display a classification result report?",
      "Start computation",
      "Computation time may take up to several hours (depending on the capacities of computer and the dataset size).\nWould you like to start computation anyway?",
      "Would you like to estimate a Markov model from the classification?",
      "Continue?",
      "End of program",
      "The program is over!",
      "Percentage of explained variance is not correct!",
      "Error!",
      "Would you like to display a modeling result report?",
      "Modeling is over",
      "Import results from a previous modeling",
      "Important : which file to import",
      "The file should be saved as an archive in the repertory '/Rfiles' from your previous session,\nIts name should be 'MarkovEstimationOutput_'+date",
      "Import another TXT dataset",
      "Important : how to well import your data in 9 steps",
      "Column names and measurement units have to match exactly with initial dataset",
      "Predict new data states",
      "Would you like to display a prediction result report?",
      "Prediction is over",
      "States sequencing",
      "states_sequencing.jpg",
      "State sequence estimated \nby the Markov model",
      "Normalization ...\n",
      "Launching spectral classification (Jordan Fast Spectral algorithm)...\n",
      "Computation of optimal number of states ...\n",
      "Applying K-means algorithm to detect symbols ...\n",
      "Saving results ...\n",
      "Classification done ...\n",
      "Creating graphics ...\n",
      "Classifiation duration: ",
      "seconds",
      "Number of variables: ",
      "Number of complete observations: ",
      " with at least 1 NA)",
      "Number of detected states: ",
      "Launching classification (Hartigan-Wong K-means algorithm) ...\n",
      "Classification results saved in: ",
      " missing values in ",
      "Number of columns: ",
      "Number of rows: ",
      "Number of incomplete rows: ",
      " with at least 1 NA",
      "Fixed data saved in: ",
      "Number of states:",
      "Number of symbols: ",
      "Modeling duration: ",
      "Assignation of data symbols ...",
      "Launching state estimation (Viterbi algorithm) ...",
      "Estimated model saved in: ",
      "Modeling done ...\n",
      "Assignation of new data symbols ...",
      "Launching state estimation (Viterbi algorithm) ...",
      "Creating graphics ...\n",
      "Prediction done \n",
      "state_sequencing.jpg",
      "State sequence predicted\nby the Markov model",
      "SelectedRawData/",
      "out of",
      "Nb of prototypes selected by Kmeans/Elbow =",
      "Nb of groups detected in samples - gap =",
      "compute MNCUT",
      "compute sil...",
      "/Diagrams/",
      "/Tables/",
      "/Rfiles/",
      "Prediction period selection",
      "complete observations if all variables are taken",
      "Estimation of transition and emission matrices ...",
      "No modeling results loaded",
      "No classification results loaded",
      "Import the example dataset (MarelCarnot)",
      "Confirm close",
      "Are you sure you want to close the interface?",
      "Are you sure you want to reset the interface?"
    )
  }
  
  if(language=="fr"){
    textToDisplay<-c(
      "Interface uHMM",
      "Le but de cette interface est de d\UE9tecter des \UE9v\UE9nements usuels ou extr\UEAmes dans un jeu de donn\UE9\U65s,\n et de caract\UE9riser leur dynamique (en construisant un mod\UE8le de Markov)",
      "Vue d'ensemble",
      "Import",
      "M\UE9trologie",
      "S\UE9lection des variables",
      "Classification",
      "Mod\UE9lisation de s\UE9ries temporelles",
      "Pr\UE9\U64iction",
      "Sch\UE9ma de l'interface",
      "           Import des donn\UE9\U65s           ",
      "                M\UE9trologie                 ",
      "        S\UE9lection des variables         ",
      "                 Classification                 ",
      "Mod\UE9lisation de s\UE9ries temporelles",
      "                   Pr\UE9\U64iction                   ",
      "Equipement recommand\UE9",
      "CPU : X_86 64 bits \nVitesse : 3000 MHz \nOS : Windows/Linux \nM\UE9moire : 8 GB \nPlace sur le disque dur pour sauvegarde : environ...\nLogiciel : R 3.2.3",
      "S\UE9lection de la langue",
      "Anglais",
      "Fran\UE7\U61is",
      "S\UE9lection du mode utilisateur",
      "Standard (recommand\UE9)",
      "Expert",
      "D\UE9marrer",
      "Importer un fichier TXT",
      "Important : comment importer vos donn\UE9\U65s en 8 \UE9tapes",
      "Les donn\UE9\U65s doivent \UEAtre \UE9\U63hantillonn\UE9\U65s \UE0 pas constant \nLes donn\UE9\U65s doivent \UEAtre dans un fichier .txt (sauvegarder en tant que '.txt' dans votre feuille de calculs) \nLe s\UE9parateur d\UE9\U63imal doit \UEAtre le caract\UE8re '.' \nLes valeurs manquantes doivent \UEAtre labellis\UE9\U65s 'NA' \nLes dates des observations doivent \UEAtre renseign\UE9\U65s dans une colonne intitul\UE9\U65 'Dates' \nCes dates doivent \UEAtre au format 'AAAA-MM-JJ (format ISO 8601) \nLes heures des observations doivent \UEAtre renseign\UE9\U65s dans une colonne intitul\UE9\U65 'Hours' \nCes heures doivent \UEAtre au format 'HH:MM:SS' (format ISO 8601)",
      "Prochaine \UE9tape",
      "Aucun fichier s\UE9lectionn\UE9 !",
      "Aucun r\UE9pertoire de sauvegarde s\UE9lectionn\UE9 !",
      "Attention !",
      "Choisissez un r\UE9pertoire de sauvegarde, puis cliquez sur OK",
      "S\UE9lectionnez votre r\UE9pertoire de sauvegarde",
      "R\UE9sum\UE9",
      "Correction",
      "_corrige",
      "S\UE9lection des variables ",
      "- Toutes les variables -",
      "Aucun param\UE8tre s\UE9lectionn\UE9 !",
      "Analyse exploratoire",
      "Variables du jeu de donn\UE9\U65s ",
      "Variables incluses dans le mod\UE8le ",
      "Graphique",
      "Graphiques",
      "Boxplots",
      "Visualiser",
      "Cercle des corr\UE9lations de l'ACP",
      "Le nombre de dimensions est compris entre 0 et ",
      "Corr\UE9lations",
      "ACP",
      "S\UE9lectionnez au moins deux param\UE8tres !",
      "Boxplot",
      "S\UE9lection de la p\UE9riode d'apprentissage ",
      "De",
      "\UE0",
      "Oui",
      "Non",
      "Lancer",
      "Matrice des corr\UE9lations globale",
      "Matrice des corr\UE9lations",
      "observations compl\UE8tes : ",
      "Variables s\UE9lectionn\UE9\U65s :",
      "P\UE9riode s\UE9lectionn\UE9\U65 : de ",
      " \UE0 ",
      "Voici les param\UE8tres utilis\UE9s par l'interface pour les utilisateurs standards :\n     - la d\UE9tection des \UE9tats est r\UE9\U61lis\UE9\U65 sur les donn\UE9\U65s standardis\UE9\U65s ;\n     - la m\UE9thode de classification utilis\UE9\U65 est la classification spectrale ;\n     - le nombre d'\UE9tats optimal est calcul\UE9 par l'interface, en utilisant le crit\UE8re du gap (\UE9\U63\U61rt entre les valeurs propres).\n\nPour modifier ces options, veuillez lancer l'interface en mode expert.",
      "Normalisation",
      "Voulez-vous normaliser les donn\UE9\U65s ?",
      "Choisissez votre m\UE9thode de classification",
      "K-means",
      "Classification spectrale",
      "Si vous ne connaissez pas la structure de vos donn\UE9\U65s,\nil est conseill\UE9 d'utiliser la classification spectrale",
      "Quantification vectorielle",
      "Variance expliqu\UE9\U65 (en %) : ",
      "nombre d'\UE9tats : ",
      "0 = Automatique",
      "Crit\UE8re d'arr\UEAt",
      "Valeurs propres principales",
      "Gap",
      "Importer les r\UE9sultats d'une pr\UE9\U63\UE9\U64\U65nte classification",
      "Important : quel fichier importer",
      "Le fichier devrait \UEAtre sauvegard\UE9 en tant qu'archive dans le r\UE9pertoire '/FichiersR' de votre session pr\UE9\U63\UE9\U64\U65nte,\nSon nom devrait \UEAtre 'classificationOutput_'+date",
      "La classification est termin\UE9\U65",
      "Voulez-vous afficher un rapport sur les r\UE9sultats de la classification ?",
      "Lancer les calculs",
      "La proc\UE9\U64ure peut prendre jusqu'\UE0 plusieurs heures (selon les capacit\UE9s de votre ordinateur et la taille de vos don\UE9\U65s).\nVoulez-vous tout de m\UEAme lancer la proc\UE9\U64ure ?",
      "Voulez-vous estimer un mod\UE8le de Markov \UE0 partir de la classification ?",
      "Continuer ?",
      "Fin du programme",
      "Le programme est termin\UE9 !",
      "La valeur du pourcentage de variance expliqu\UE9\U65 est incorrecte !",
      "Erreur !",
      "Voulez-vous afficher un rapport sur les r\UE9sultats de la mod\UE9lisation ?",
      "La mod\UE9lisation est termin\UE9\U65",
      "Importer les r\UE9sultats d'une pr\UE9\U63\UE9\U64\U65nte mod\UE9lisation",
      "Important : quel fichier importer",
      "Le fichier devrait \UEAtre sauvegard\UE9 en tant qu'archive dans le r\UE9pertoire '/FichiersR' de votre session pr\UE9\U63\UE9\U64\U65nte,\nSon nom devrait \UEAtre 'MarkovEstimationOutput_'+date",
      "Importer un autre fichier de donn\UE9\U65s TXT",
      "Important : comment importer vos donn\UE9\U65s en 9 \UE9tapes",
      "Les noms de colonnes et les unit\UE9s de mesure doivent correspondre exactement avec le jeu de donn\UE9\U65s initial",
      "Pr\UE9\U64ire l'\UE9tat des nouvelles donn\UE9\U65s",
      "Voulez-vous afficher un rapport sur les r\UE9sultats de la pr\UE9\U64iction ?",
      "La pr\UE9\U64iction est termin\UE9\U65",
      "S\UE9quencement des \UE9tats",
      "sequencement_etats.jpg",
      "S\UE9quencement des \UE9tats estim\UE9s\npar le mod\UE8le de Markov",
      "Normalisation ...\n",
      "Lancement de la classification spectrale (algorithme Fast Spectral de Jordan)...\n",
      "Calcul du nombre d'\UE9tats optimal ...\n",
      "Application de l'algorithme K-means pour d\UE9tecter les symboles ...\n",
      "Sauvegarde des resultats ...\n",
      "Classification termin\UE9\U65 ...\n",
      "Cr\UE9\U61tion des graphiques ...\n",
      "Dur\UE9\U65 de la classification : ",
      "secondes",
      "Nombre de variables : ",
      "Nombre d'observations compl\UE8tes : ",
      " avec au moins 1 NA)",
      "Nombre d'\UE9tats d\UE9tect\UE9s : ",
      "Lancement de la classification (algorithme K-means de Hartigan-Wong) ...\n",
      "R\UE9sultats de la classification sauvegard\UE9s dans : ",
      " valeurs manquantes dans ",
      "Nombre de colonnes : ",
      "Nombre de lignes : ",
      "Nombre de lignes incompl\UE8tes : ",
      " avec au moins 1 NA",
      "Donn\UE9\U65s corrig\UE9\U65s sauvegard\UE9\U65s dans : ",
      "nombre d'\UE9tats : ",
      "Nombre de symboles : ",
      "Dur\UE9\U65 de la mod\UE9lisation : ",
      "Attribution des symboles aux donn\UE9\U65s ...",
      "Lancement de l'estimation des \UE9tats (algorithme de Viterbi) ...",
      "Mod\UE8le estim\UE9 sauvegard\UE9 dans : ",
      "Mod\UE9lisation termin\UE9\U65 ...\n",
      "Attribution des symboles aux nouvelles donn\UE9\U65s ...",
      "Lancement de l'estimation des \UE9tats (algorithme de Viterbi) ...",
      "Cr\UE9\U61tion des graphiques ...\n",
      "Pr\UE9\U64iction termin\UE9\U65 \n",
      "sequencement_etats.jpg",
      "S\UE9quencement des \UE9tats pr\UE9\U64its\npar le mod\UE8le de Markov",
      "DonneesBrutesSelectionnees/",
      "sur",
      "Nb prototypes s\UE9lectionn\UE9s par Kmeans/Elbow =",
      "Nb groupes d\UE9t\UE9\U63t\UE9s dans les \UE9\U63hantillons - gap =",
      "calcul MNCUT",
      "calcul sil...",
      "/Figures/",
      "/Tableaux/",
      "/FichiersR/",
      "S\UE9lection de la p\UE9riode \UE0 pr\UE9\U64ire ",
      "observations compl\UE8tes si toutes les variables sont prises",
      "Estimation des matrices de transition et d'\UE9mission ...",
      "Aucun r\UE9sultat de mod\UE9lisation charg\UE9",
      "Aucun r\UE9sultat de classification charg\UE9",
      "Importer le jeu de donn\UE9\U65s d'exemple (MarelCarnot)",
      "Confirmer la fermeture",
      "\UCAtes-vous s\UFBr de vouloir fermer l'interface ?",
      "\UCAtes-vous s\UFBr de vouloir r\UE9initialiser l'interface ?"
    )
  }
  tm<-data.frame(t(textToDisplay))
  colnames(tm)<-id
  tm<-lapply(tm, as.character)
  return(tm)
}

