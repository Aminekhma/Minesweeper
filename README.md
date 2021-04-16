# Minesweeper

Le but du projet est d’implémenter le jeu du démineur. Il n’y a rien à installer à part le projet en lui-même, ainsi que Node.js et Yarn (ou NPM).

# Installation du projet

```bash
git clone git@github.com:ghivert/minesweeper.git
cd minesweeper
yarn global add create-elm-app # npm install -g create-elm-app
yarn install # npm install
```

# Le lancer

```bash
yarn start # npm start
```

# En cas de bug

```bash
yarn global remove elm-app create-elm-app
rm -rf node_modules
yarn global add create-elm-app
yarn install
```

# Instructions

Pour rappel, le démineur est un jeu sur une grille consistant à cliquer sur des cases de la grille en évitant de déclencher une bombe.

Lors du clic sur une case, celle-ci peut être vide, avoir un numéro, ou une bombe.

Si la case est vide, alors il faut la révéler et révéler toutes les cases connectées aux alentours.
Si la case contient un numéro, alors il faut la révéler.
Si la case contient une bombe, alors il faut arrêter le jeu et révéler tout le plateau.

Le numéro dans les cases numérotés correspond aux nombres de bombes adjacentes dans toutes les directions.

## Bonus

- Ajouter un moyen de sauvegarder / restaurer sa progression.
- Ajouter un moyen de comptabiliser son score (temps passé, cases cliquées).
- Ajouter un système d'indices quand on ne sait pas quoi jouer.
- Ce qui vous inspire !

# Un exemple ?

On trouve de nombreux démineurs en ligne : [comme ici](http://minesweeperonline.com/) ou encore [directement sur Google](https://www.google.com/search?q=minesweeper&oq=minesweeper&aqs=chrome..69i57j46j0l2j46j0l3.1466j0j7&sourceid=chrome&ie=UTF-8).

# Implémentation

Le frontend et tout le déroulement est laissé à la libre appréciation de chacun. Le code de génération de l’emplacement aléatoire des mines est fourni dans le fichier `Mine.elm` et un exemple est fourni directement dans le `Main.elm`.
