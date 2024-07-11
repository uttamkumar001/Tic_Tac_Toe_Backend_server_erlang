# Tic Tac Toe Server

This project is a backend server for the classic Tic Tac Toe game designed to handle multiple clients. The server listens for client connections on a user-defined port, manages player matchmaking, and handles game logic.

## Table of Contents

- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
- [Modules](#modules)
  - [game_logic.erl](#game_logicerl)
  - [match_making.erl](#match_makingerl)
- [Contributing](#contributing)
- [License](#license)

## Features

- Listens for client connections on a user-defined port
- Manages a queue of players for matchmaking
- Prevents duplicate entries in the matchmaking queue
- Pairs players and creates a game table process when there are enough players
- Handles game logic and message packets for ongoing games

## Installation

To run the Tic Tac Toe server locally, follow these steps:

1. **Clone the repository**:

    ```sh
    git clone https://github.com/username/tic-tac-toe-server.git
    cd tic-tac-toe-server
    ```

2. **Install dependencies**:

    Ensure you have Erlang installed. For installation instructions, visit [Erlang's official site](https://www.erlang.org/downloads).

3. **Compile the Erlang files**:

    ```sh
    erlc game_logic.erl
    erlc match_making.erl
    ```

4. **Run the server**:

    Start the Erlang shell and load the compiled modules:

    ```sh
    erl
    ```

    In the Erlang shell, start the application:

    ```erlang
    1> c(game_logic).
    2> c(match_making).
    3> application:start(tic_tac_toe).
    ```

The server should now be running and ready to accept client connections.

## Usage

### Starting the Server

1. **Run the server**:

    ```erlang
    erl -sname tic_tac_toe_server -setcookie your_cookie
    ```

    Replace `your_cookie` with a secure cookie value.

2. **Listening on a Port**:

    When the server starts, it will listen for client connections on the port specified by the user.

### Matchmaking and Game Flow

1. **Player Joins**:

    When a player sends a join request, the server adds the player to the matchmaking queue. If the player is already in the queue, an error message is sent to the player.

2. **Pairing Players**:

    The server checks the matchmaking queue every two seconds. If the queue has more than one player, the server pairs the first two players and creates a `table_id` for them. This `table_id` is a process created by the game supervisor.

3. **Game Start**:

    The `table_id` is returned to the players, and the game starts on that process. The game process will handle further game logic by casting message packets to the `game_logic` module.

## Modules

### `game_logic.erl`

This module handles the core game logic, including:
- Validating moves
- Checking for game completion
- Determining the winner
- Managing the game state

### `match_making.erl`

This module manages matchmaking, including:
- Adding players to the queue
- Checking the queue every two seconds
- Pairing players when there are enough players in the queue
- Creating game tables for paired players

## Contributing

Contributions are welcome! Please follow these steps to contribute:

1. Fork the repository
2. Create a new branch (`git checkout -b feature-branch`)
3. Make your changes
4. Commit your changes (`git commit -m 'Add new feature'`)
5. Push to the branch (`git push origin feature-branch`)
6. Create a pull request

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

