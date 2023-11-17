# Court-Piece

[Court Piece](https://en.wikipedia.org/wiki/Court_piece) / Rung is a card game played played with a set of standard 52 cards. It is a 4 player game in teams of 2. Cards are dealt in batches of 5-3-3-2 to each player. At the start, a suit is chosen by a player called rung. In each round, players must follow the suit if possible and the highest card of the rung or highest card of the suit takes the point. The smallest card number is 2 and the highest is ace. The team which reaches 7 points first wins the game.

# Goal
We will be creating a multiplayer terminal UI based game in Haskell using the brick library. The game would have 2 modes: host and player mode. We would first start a host application containing all the game logic and state. Clients can then connect to the host using the player mode. After each move(event), the host will evaluate the game state and broadcast the player's state.

- Create a host application having all the game logic(Back end).
- Add networking to support multiplayer(Middleware/Connector).
- Write client/player code for handling game UI(Front end).
- Add unit testing for the game logic and client UI.
- Bonus: Add fault-tolerance to the clients.

# Libraries/Dependencies
- [brick](https://hackage.haskell.org/package/brick)
- [network.Socket](https://hackage.haskell.org/package/network-3.1.4.0/docs/Network-Socket.html)
- [lens](https://hackage.haskell.org/package/lens-tutorial-1.0.4/docs/Control-Lens-Tutorial.html)
- [quickcheck](https://hackage.haskell.org/package/QuickCheck)

# Team
- Anish Koulgi
- Nipun Wahi
- Mahesh Bharadwaj Kannan
