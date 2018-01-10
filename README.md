# Projet de Compilation : Petit Rust #

Date : 10 Janvier 2018

## Utilisation ##

```
usage: prustc [options] <file.rs>

options include:
  --parse-only   stop after parsing
  --type-only   stop after typing
  --no-asm   stop after borrow checking
  -o  <file>   specify output file name
  --help  Display this help message
```

Ce compilateur pour Petit Rust, nommé **prustc** dans la suite, se compile tout
simplement avec la commande `make`. Par ailleurs, la commande `make clean`
permet de nettoyer les fichiers générés par la compilation (exécutable compris)

L'exécution se déroule en cinq étapes, chacune implémentée dans un fichier distinct :
  - Parsing           (lexer.mll, parser.mly)
  - Typing            (typer.ml)
  - Borrow checking   (borrow\_checker.ml)
  - Precompiling      (precompiler.ml)
  - Compiling         (compiler.ml)

Il est possible d'interrompre l'exécution après chacune des trois premières étapes,
avec les options respectivement --parse-only, --type-only, et --no-asm

Lors de la cinquième étape, un fichier "file.s" est crée dans le même répertoire
que le fichier "file.rs" donné en entrée (nom identique, extension .s).
Ce fichier contient le code assembleur correspondant au résultat de l'étape
de compilation. Il peut être compilé avec `gcc file.s`

NB: Sur certaines machines, le flag "-static" est nécessaire pour cette dernière étape.
    Cette option étant omise dans les fichiers de tests par défaut,
    il se peut que certaines compilations échouent avec un message de gcc à
    propos d'un problème d'édition de liens. Ajouter cette option règle le problème.


En cas d'erreur lors de l'analyse lexicale ou syntaxique, ou de typage, le
compilateur terminera avec le code d'erreur 1, et affichera sur la sortie
**stderr** un message d'erreur indiquant l'endroit exact du fichier qui
produit cette erreur (ligne et colonnes), ainsi qu'une description de
l'erreur en question.

Ce compilateur n'est toutefois pas terminé, et ne vérifie pas toutes les
spéficiations de Petit Rust. Plus de détails sont donnés par la suite.


## Description détaillée ##

### Lexer && Parser ###

Un arbre de syntaxe abstraite est construit en lisant le fichier d'entrée.
Il comprend des indications de localisation pour chaque identifiant, chaque
fonction et chaque constante, qui seront utilisés pour détailler les erreurs
dans la suite de l'exécution.

### Typer ###

Une vérification du typage correct de l'arbre est alors effectuée.
Afin de faciliter la suite des opérations, le type de chaque noeud de l'arbre
est gardé en mémoire. Pour cela, un deuxième arbre de syntaxe est construit.

### Borrow checker ###

Ainsi qu'énoncé dans les spécifications du langage, un vérification de l'accès
aux ressources est effectuée. N'ayant pu mener cette étape à bien par manque de
temps, j'ai pris le parti d'ignorer sciemment certaines erreurs afin de garantir
le moins de faux négatifs possibles : un fichier correct ne doit en aucun cas
être rejeté par le borrow checker, pour pouvoir continuer sur l'étape de compilation,
même si cela implique que certains fichiers incorrects sont acceptés.

Les structures et vecteurs imbriqués par exemple ne sont pas correctement vérifiés,
et la portée des variables n'est pas prise en compte.

### Compiler ###

Ici aussi, l'étape est incomplète, les vecteurs et les operateurs de
référencement / déréférencement ne sont pas supportés, ainsi que les instructions return.
Le reste des opérations devrait néanmoins être compilé correctement.
La compilation est loin d'être efficace, et son utilité est limitée étant donnée
que le principal avantage de Rust (borrow checking) est ignoré, et que les références
ne fonctionnent pas. Des programmes simples et relativement intéressants peuvent
tout de même être compilés et permetter de s'amuser un peu ;)
