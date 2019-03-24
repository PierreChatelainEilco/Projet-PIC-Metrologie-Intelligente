# Projet-PIC-Metrologie-Intelligente

Veuillez taper les commandes à la main.
**Pas de Sourcetree !**

## Pour cloner le projet

La première fois seulement ! <br />
Dans un dossier vide, à l'aide de git bash par exemple, taper `git clone ...`

## Pour créer une nouvelle branche

Pour chaque nouvelle branche, suivre les étapes suivantes : 

1. Créer votre branche localement

    `git branch maBranche`
    
2. Placer vous sur votre nouvelle branche

    `git checkout maBranche`
    
3. Envoyer votre branche locale sur GitHub

    `git push --set-upstream origin maBranche`
    
4. Récupérer les données à jour de la branche master de GitHub

    `git pull origin master`
    
## Pour récupérer les données

Etape à faire régulièrement pour avoir les fichiers à jour.<br />
Pour récupérer les données d'une branche GitHub sur votre branche locale, suivre les étapes suivantes.

1. Placer vous sur votre branche locale

    `git branch maBrancheLocale`
    
2. Récupérer les données à jour de la branche GitHub

    `git pull origin maBrancheGitHub`

## Pour envoyer les données

A chaque temps forts (Pull, Branch, Fonction, Modification d'un nouveau fichier, Fin d'une session de travail ...).<br />
Pour envoyer les données sur GitHub, suivre les étapes suivantes :

1. Obtenir la liste des fichiers modifiés

    `git status`
    
2. Ajouter les fichiers modifiés

    `git add fichier1 fichier2 ...`
    
3. Résumer les changements

    `git commit -m "Blabla fichier1"`

4. Envoyer les changements à GitHub

    `git push`

## Fusionner les branches

**N'effectuer pas de merge !**<br />Utiliser le système de pull request de GitHub pour fusionner les branches !

# Notes

Utiliser ce fichier pour découvrir le fonctionnement du Markdown.

Pierre Chatelain

branche develop