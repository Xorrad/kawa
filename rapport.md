# interpréteur du language Kawa

> La perfection est atteinte, non pas lorsqu'il n'y a plus rien à ajouter, mais lorsqu'il n'y a plus rien à retirer.  
— Antoine De Saint-Exupéry

## Sommaire

- [Introduction](#introduction)
- [Fonctionnalités](#fonctionnalités)
- [Difficultés](#difficultes)

## Introduction
Projet du cours de Compilation pour l'année 2024-2025 sur un langage orienté objet fictif nommé **Kawa**
réalisé par:
- Aymeric Emond <<aymeric.emond@etu-upsaclay.fr>>


## Fonctionnalités

### Bases
Toutes les fonctionnalités principales du language (**arithmétique**, **variables**, **instructions**, **classes**, **méthodes**, **héritage**) ont été implémentées et fonctionnent à priori parfaitement aux quatres étapes: **analyse lexicale**, **analyse grammaticale**, **vérification des types**, **interprétation**.  

### Champs immuables
Ajout du trait `final` pour les variables globales, locales et les attributs de classes permettant de définir une variable comme immuable après initialization.

### Égalité structurelle
Ajout des opérateurs d'égalité structurelle `===` et `=/=` avec les propriétés indiquées sur le sujet.

### Déclaration avec valeur initiale
Permet lors de la déclaration d'une variable ou d'un attribut, de lui fournir une valeur initiale, sous la forme.
```
var int x = 1;
```

### Déclarations en série
Ajout de la possibilité de déclarer simultanément plusieurs variables du même type, sous la forme
```
var int x, y, z;
```
On peut également utilisé des valeurs initiales pour chaque variable:
```
var int x = 1, y = 2;
```

### Test de type
Ajout d'un opérateur binaire **instanceof** testant le type dynamique d'un objet.





## Difficultés
La principale difficulté rencontrée était de se régler les conflits lors de l'analyse grammaticale.  

Puis j'ai également perdu un certain temps à déterminer la porté des différents environnements, ainsi que la distribution des  attributs et méthodes pour les classes héritants d'une autre.  

Enfin, comme la plupart du temps, j'ai avancé trop lentement dans le développement et je n'ai pas eu le temps d'implémenter toutes les fonctionnalités que j'aurai souhaite ajouté.