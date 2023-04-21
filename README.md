# Projet d'algorithmie Eternity II - 1 étudiant

Ce projet est une implémentation en OCaml d'un solveur de pseudo-puzzles Eternity II.
Il génère une solution de puzzle, la mélange et résout le mélange en utilisant un algorithme de backtracking.
Deux algorithmes de résolution sont implémentés: le premier, `solve`, est un algorithme naïf, et peut prendre du temps sur les puzzles de grandes tailles.
Le second, `solve_all`, est plus optimisé et plus performant pour les puzzles de grande taille.
Par défaut, la méthode `solve_all` est utilisée, mais vous pouvez décommenter la ligne utilisant `solve` et commenter la ligne qui utilisait `solve_all` pour utiliser `solve`.

Notez que j'ai testé sur des puzzles de taille maximale de 12x12, comme indiqué dans le sujet.

## Prérequis

Pour compiler et exécuter ce projet, vous devez disposer d'OCaml et des bibliothèques CSV et Unix.
Vous pouvez les installer en utilisant OPAM :

```bash
opam install csv
opam install unix
```

**Veillez à bien mettre à jour l'environnement courant si cela ne marche pas.**

```bash
eval $(opam env)
```

## Compilation

Pour compiler le projet, exécutez simplement la commande make à la racine du projet :

```bash	
make
```
Vous pouvez nettoyer tous les fichiers générés par la compilation, ainsi que tous les fichiers SVG générés par le programme en exécutant la commande make clean :

```bash
make clean
```
## Utilisation

Pour utiliser le programme, vous devez exécuter la commande suivante :

```bash
./projet
```
Les paramètres de configuration pour chaque programme peuvent être modifiés dans le fichier `config.txt`.

Le programme affichera un menu avec trois options:

1. Générer un puzzle aléatoire, le mélanger et le résoudre avec l'algorithme `solve_all` (par défaut) ou solve (**décommenter la ligne correspondante dans le fichier eternity.ml**). La taille du puzzle est configurable dans le fichier `config.txt`. Les résultats sont affichés dans le terminal et les fichiers SVG sont générés dans le répertoire `/Puzzle_p1`.
2. Résoudre un puzzle importé. Le fichier doit être placé dans le répertoire `/puzzle_importe`. Des exemples sont fournis pour illustrer le format à respecter (Si vous copiez-coller, pensez à supprimer les derniers points-virgules des lignes " ; " et laisser l'espace ). Vous pouvez réutiliser les puzzles générés avec le programme 1. Les résultats sont affichés dans le terminal et les fichiers SVG sont générés dans le répertoire `/Puzzle_p2`.
3. Générer une liste de puzzles aléatoires et les résoudre avec l'algorithme `solve_all` (par défaut) ou solve (**décommenter si nécessaire**). Les tailles des puzzles sont configurables dans le fichier config.txt. Les résultats ne sont pas affichés dans le terminal en raison du nombre de puzzles générés, mais les fichiers SVG sont générés dans le répertoire `/Puzzle_p3`. Les résultats sont également enregistrés le fichier CSV `results.csv` à la racine du projet. Pour présenter les résultats sous forme de tableau une fois dans excel, ouvrez excel, puis allé dans `Données` -> `À partir d'un fichier texte/CSV` -> `Sélectionner le fichier results.csv` -> `Mettre le délimiteur sur , (virgule)` -> `Charger`. Vous obtiendrez alors un tableau avec 2 colonne (taille, temps).


## Programme de démonstration

Je n'ai pas fais explicitement des programmes de démontration. Il sont directement intégrés dans la fonction main du programme. 
Pour les tester compiler simplement le projet et lancer le programme.

## Auteurs

* **Rémy Auloy**


