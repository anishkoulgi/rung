# Court-Piece

[Court Piece](https://en.wikipedia.org/wiki/Court_piece) / Rung is a card game played played with a set of standard 52 cards. It is a 4 player game in teams of 2. Cards are dealt in batches of 5-3-3-2 to each player. At the start, a suit is chosen by a player called rung. In each round, players must follow the suit if possible and the highest card of the rung or highest card of the suit takes the point. The smallest card number is 2 and the highest is ace. The team which reaches 7 points first wins the game.

# Goal
We will be creating a multiplayer terminal UI based game in Haskell using the brick library. The game would have 2 modes: host and player mode. We would first start a host application containing all the game logic and state. Clients can then connect to the host using the player mode. After each move(event), the host will evaluate the game state and broadcast the player's state.

- Create a host application having all the game logic(Back end).
- Add networking to support multiplayer(Middleware/Connector).
- Write client/player code for handling game UI(Front end).
- Add unit testing for the game logic and client UI.
- Bonus: Add fault-tolerance to the clients.

# Architecture
## Server Side State Diagram

<div align="center">
<img src="https://github.com/anishkoulgi/rung/assets/48756374/f7563048-7545-4e13-8ad4-d9bba5960f5c">
<br>
</div>

 * The server initalises socket and waits for all clients to connect.
 * Then it creates a gamestate for a new game and broadcasts the PlayerState to each of the four players.
 * It waits for each player to perform an action, validates it and then broadcasts updated state to the four players.

## Client Side State Diagram

<div align="center">
<img src="https://github.com/anishkoulgi/rung/assets/48756374/78b7a5d0-bf83-40b0-a467-88a7418081f2">
<br>
</div>

* We request the user to enter their name, IP address of the server and send the name to the server.
* We then wait for the server to send PlayerState information for this player.
* At every point in time, we showcase current cards on hand, the team-wise points distribution, current round cards.
* When player selects a card, we send the card selected and the name of the player back to the server.

## UI Wireframe

<div align="center">
<img src="https://github.com/anishkoulgi/rung/assets/48756374/a641334f-5fa5-4aa0-a92a-129cd6b8749a">
<br>
</div>

# Progress Report (Dec 1, 2024)

1. We have completed the implementation of the game logic.
2. We have made the initial setup for client server integration using sockets but its not yet fully integrated with gamelogic.
3. On the UI we have created the pages for initial views on the clients but the main client game page is work in progress.

We are on track to complete the basic game functionality but we are unsure about adding support for multiple games on the same server and the fault tolerent components.

# Libraries/Dependencies
- [brick](https://hackage.haskell.org/package/brick)
- [network.Socket](https://hackage.haskell.org/package/network-3.1.4.0/docs/Network-Socket.html)
- [lens](https://hackage.haskell.org/package/lens-tutorial-1.0.4/docs/Control-Lens-Tutorial.html)
- [quickcheck](https://hackage.haskell.org/package/QuickCheck)

# Team
- Anish Koulgi
- Nipun Wahi
- Mahesh Bharadwaj Kannan
