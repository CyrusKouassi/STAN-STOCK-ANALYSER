---
title: "STAN – STock ANalyser"
author: "Cyrus Ingrid Berenice Kouassi"
date: "`r Sys.Date()`"
output:
  html_document:
    toc: true
    toc_depth: 3
    toc_float:
      collapsed: true
    number_sections: true
    theme: flatly
    highlight: tango
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

---

# Présentation du projet

**STAN (STock ANalyser)** est une application interactive développée avec **R** et **Shiny** permettant d'explorer, visualiser et analyser des séries financières.

L'application permet à un utilisateur de :

- télécharger automatiquement des données boursières depuis **Yahoo Finance** via le package `quantmod` ;
- analyser l'évolution du prix d'un actif sur une période personnalisable ;
- calculer des indicateurs financiers clés (volatilité, CAGR) ;
- visualiser les données avec des graphiques interactifs **Plotly** ;
- modéliser la tendance de long terme à l'aide d'une **régression log-linéaire** ;
- contrôler automatiquement la **qualité des données** ;
- importer ses propres séries de prix au format **CSV**.

L'application a été développée dans le cadre du cours **Logiciels Avancés – R** du **Master Mathématiques appliquées Statistiques, Université Clermont Auvergne**.

---

**L'objectif** de ce projet est de proposer un outil simple et autonome permettant de :

- explorer et visualiser des données financières ;
- analyser les performances d'un actif sur différents horizons ;
- illustrer des concepts statistiques et financiers fondamentaux ;
- prendre en main un workflow complet de traitement de données sous R/Shiny.

L'application est conçue pour être **accessible à un utilisateur débutant**, sans connaissance préalable de la finance ou de la programmation.

---

# Fonctionnalités principales et Configuration requise

| Fonctionnalité | Description |
|---|---|
| Téléchargement automatique | Données depuis Yahoo Finance via `quantmod` |
| Indicateurs financiers | Volatilité annualisée, CAGR |
| Analyse de performance | Performances sur 1 mois, 6 mois, 1 an, 3 ans, 5 ans |
| Modélisation | Régression log-linéaire + bandes de dispersion |
| Visualisation | Graphiques interactifs avec **Plotly** |
| Import CSV | Chargement de séries personnalisées |
| Contrôle qualité | Vérification automatique des données |
| Gestion des tickers | Base locale de symboles boursiers |

---

**Environnement**

- **R version 4.2 ou supérieure**
- **RStudio** (recommandé)
- **Connexion internet** (nécessaire pour Yahoo Finance)

**Packages utilisés**

Les packages suivants sont **installés automatiquement au premier lancement** si absents :

```r
packages <- c(
  "shiny",
  "shinydashboard", 
  "plotly",
  "DT",
  "dplyr",
  "readr",
  "lubridate",
  "quantmod",
  "zoo",
  "htmltools",
  "scales",
  "rmarkdown"
)

```

---

# Installation et lancement

**Structure du projet**

```
STAN/
├── app.R          # Fichier principal Shiny
├── global.R       # Fonctions de traitement des données
├── README.Rmd     # Ce document
├── data/          # Données téléchargées (générées automatiquement)
├── imports/       # Fichiers CSV importés par l'utilisateur
└── www/
    └── style.css  # Feuille de style personnalisée
```

**Lancement**

Dans **RStudio**, cliquer sur le bouton **Run App**, ou exécuter dans la console :

```r
shiny::runApp()
```

L'application s'ouvre automatiquement dans le navigateur par défaut.

---

# Prise en main 


## Page d'accueil

À l'ouverture, la page d'accueil présente :

- le nom et la description de l'application ;
- les fonctionnalités disponibles ;
- un bouton **Commencer votre analyse** pour accéder au tableau de bord.

## Workflow typique

1. Aller dans l'onglet **Réglages**
2. Saisir un ticker (ex. `AAPL`, `MC.PA`)
3. Choisir une période d'analyse
4. Explorer les onglets **Indicateurs**, **Performance** et **Graphique**


## Description des onglets

### Réglages

Paramètres de l'analyse :

- **Ticker** : symbole boursier Yahoo Finance

```
Exemples : AAPL, MSFT, TSLA, AIR.PA, MC.PA, BNP.PA
```

- **Période** : dates de début et de fin de l'analyse
### Vue générale

Résumé synthétique de l'analyse en cours :

- dernier prix observé
- performance récente
- position par rapport à la tendance (en sigma)
- nombre d'observations disponibles



### Indicateurs

Tableau de bord des indicateurs financiers :

- dernier prix de clôture
- date de la dernière observation
- volatilité annualisée (écart-type des rendements log)
- CAGR (taux de croissance annuel composé)

### Performance

Performance du titre sur plusieurs horizons :

| Horizon | Description |
|---|---|
| 1 mois | Variation du prix sur 30 jours |
| 6 mois | Variation sur 180 jours |
| 1 an | Variation sur 1 an |
| 3 ans | Variation sur 3 ans |
| 5 ans | Variation sur 5 ans |

### Graphique

Graphique interactif Plotly affichant :

- le **prix observé** (série historique)
- la **tendance log-linéaire** estimée
- les **bandes de dispersion** (± 1σ et ± 2σ autour de la tendance)

Voir la section [Utilisation du graphique](#graphique-interactif) pour le détail des interactions.

### Gestion des données

Cet onglet centralise la gestion des données :

- mise à jour des données pour un ticker existant
- ajout d'un nouveau ticker à la base locale
- import d'un fichier CSV personnel

### Qualité des données

Rapport automatique de qualité incluant :

- présence des colonnes requises
- validité et format des dates
- détection des valeurs manquantes
- détection des doublons
- vérification de l'ordre chronologique
- taille minimale de la série

---

# Analyse financière

## Volatilité

La volatilité est calculée comme l'**écart-type annualisé des rendements logarithmiques journaliers** :

$$\sigma = \text{sd}\left(\ln\frac{P_t}{P_{t-1}}\right) \times \sqrt{252}$$

où $P_t$ est le prix de clôture au jour $t$ et 252 est le nombre de jours de bourse par an.

## CAGR

Le **CAGR** (*Compound Annual Growth Rate*) est le taux de croissance annuel composé :

$$\text{CAGR} = \left(\frac{P_{\text{final}}}{P_{\text{initial}}}\right)^{1/n} - 1$$

où $n$ est la durée de la période en années.

## Régression log-linéaire

Le modèle de tendance estimé est :

$$\ln(P_t) = \alpha + \beta \cdot t + \varepsilon_t$$

où :

- $\alpha$ est la constante ;
- $\beta$ est la pente de la tendance (taux de croissance continu) ;
- $\varepsilon_t \sim \mathcal{N}(0, \sigma^2)$ est le résidu.

L'application en déduit :

- la **position actuelle** du prix en nombre de sigma par rapport à la tendance ;
- une **projection théorique à 1 an** et à **5 ans** (intervalle de confiance à ± 1σ).

---

# Import de fichiers CSV {#import}

## Format minimal accepté

```
Date,Close
```

## Format complet recommandé

```
Date,Open,High,Low,Close,Volume
```

## Exemple de fichier valide

```
Date,Open,High,Low,Close,Volume
2022-01-03,100.0,103.5,99.2,102.8,1500000
2022-01-04,102.8,105.1,101.5,104.2,1320000
2022-01-05,104.2,104.8,100.9,101.6,1180000
```

## Colonnes reconnues

Les colonnes suivantes sont automatiquement détectées (insensible à la casse) :

`Date`, `Open`, `High`, `Low`, `Close`, `Volume`, `Adjusted`

> L'application nettoie automatiquement les données importées (valeurs manquantes, doublons, ordre chronologique).

---

# Gestion et stockage des données

Les données téléchargées depuis Yahoo Finance sont enregistrées localement dans le dossier `data/`, un fichier par ticker :

```
data/
├── AAPL.csv
├── AIR.PA.csv
├── BNP.PA.csv
└── TSLA.csv
```

Cette organisation permet de **relancer l'application hors connexion** en utilisant les données déjà téléchargées.

---

# Contrôle de la qualité des données {#qualite}

Le module de qualité vérifie automatiquement les points suivants :

| Contrôle | Description |
|---|---|
| Colonnes | Présence des colonnes `Date` et `Close` a minima |
| Format des dates | Dates parsables au format `YYYY-MM-DD` |
| Valeurs manquantes | Détection et comptage des `NA` |
| Doublons | Détection des lignes ou dates dupliquées |
| Ordre chronologique | Vérification du tri croissant des dates |
| Taille minimale | Série d'au moins 30 observations requise |

Un résumé de qualité est affiché après chaque import ou mise à jour.

---

# Utilisation du graphique interactif {#graphique-interactif}

Le graphique est réalisé avec **Plotly**. Les interactions disponibles sont :

| Action | Effet |
|---|---|
| Clic + glisser | Zoom sur une zone |
| Double-clic | Réinitialiser le zoom |
| Clic sur la légende | Afficher / masquer une courbe |
| Survol | Afficher les valeurs au point |
| Bouton caméra | Exporter le graphique en PNG |

---

# Utilisation autonome

L'application est entièrement autonome. Un utilisateur peut, sans intervention extérieure :

1. Lancer l'application avec `shiny::runApp()`
2. Sélectionner ou ajouter un ticker
3. Choisir la période d'analyse
4. Consulter les indicateurs et performances
5. Explorer le graphique interactif
6. Importer ses propres données CSV
7. Vérifier la qualité des données

---

# Conclusion {.unnumbered .unlisted}

**STAN – STock ANalyser** est un outil pédagogique et interactif pour l'exploration de séries financières sous R/Shiny. Il couvre l'ensemble du workflow : acquisition des données, calcul d'indicateurs, modélisation statistique, visualisation et contrôle qualité.

L'application a été conçue pour être **simple, autonome et extensible**, et constitue une base solide pour tout projet d'analyse financière sous R.

---

