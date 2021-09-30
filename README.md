# chess-challenge

## description
The goal of this program is to parse a chess game described in PGN and provide functions which can perform the following actions:
- receive move # as input and return current board state
- load games into state and navigate through games

All games must be stored in `./data/` folder and be in PGN format. Sometimes PGN files have additional data so the following sections will be read:
- metadata
  - any line in the beginning of the game with the following structure: `[KEY: "Value"]`
- game data
  - one line container the PGN moveset: `1. Nf3 Nf6...`

Each game will be comprised of and only these two sections. Any whitespace between games is not needed but helpful for humans.

## running
Requirements:
- GHC
- stack

Commands to get started

```sh
stack setup # will install appropriate GHC
```

```sh
stack run chess-statification # Will build and run app
```

## data model
The state of the chess-challenge program will be (loosely) defined as this:
```
state: {
  games: game[],
  selected: Number
  numberOfGames: Number,
}

game: {
  id: Number,
  gameStates: gameState[],
  players: playerInfo
  result: < "1-0" | "0-1" | "1/2-1/2" | null >
}

gameState: {
  id: Number,
  pieces: piece[],
  status: < check | checkmate | stalemate | normal >,
  turn: < white | black >
}

piece: {
  id: uid,
  rank: <"p" | "h" | "b" | "r" | "k" | "q" >,
  color: < "white" | "black" >
  location: location,
}

location: {
  row: < 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 >,
  col: < 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 >,
  captured: Boolean
}
```

A sample would look like this:
```
state: {
  games: [
    {
      id: 0,
      gameStates: [
        {
          id: 0,
          pieces: [
            {
              id: 1,
              rank: "p",
              color: "white",
              location: {
                row: 1,
                col: 0,
              } 
            }
            ... all pieces...
          ],
          check: false,
          checkmate: false,
          stalemate: false,
        }
      ],
      totalGameStates: 1,
      black: "Collin",
      white: "Lucy",
      result: null
    }
  ],
  selected: 0,
  numberOfGames: 1
}
```