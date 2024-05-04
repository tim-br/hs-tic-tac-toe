
# hs-tic-tac-toe-net

A multiplayer Tic Tac Toe game implemented in Haskell, using TLS for secure network communication. Players connect to the game server and interact in real-time to play against each other. The game supports secure communications and requires both a client and server to handle game sessions with TLS encryption.

## Requirements

- Haskell Stack
- OpenSSL (for testing and local development)

## How to Run

1. **Build the Project:**
   Use the following command to compile the project:
   ```bash
   stack build --system-ghc

## Generating TLS Certificates

If you want to deploy your own instance and manage TLS yourself, you'll need to generate your own certificates:

Generate a Private Key:

```
openssl genpkey -algorithm RSA -out server.key
```

Create a Certificate Signing Request (CSR):

```
openssl req -new -key server.key -out server.csr
```

Generate a Self-Signed Certificate:
```
openssl x509 -req -days 365 -in server.csr -signkey server.key -out server.crt
```
These certificates are needed to secure the connection between the server and clients.

## Run the Server:
To start the server, use:

```
stack exec hs-tic-tac-toe-net-exe -- --cert=/app/certs/server.crt --key=/app/certs/server.key 0.0.0.0 9700
```
## Connect as a Client:

To connect to your local instance run, passing in the server you created.

```
openssl s_client -connect localohost:9700 -CAfile server.crt -verify 1 -verify_return_error -brief
```

After downloading server.crt from the root of the GitHub repository, you can initiate a connection to the public server. If there's no one to play against, run the command in two separate terminal windows or tabs to start a game with yourself.

```
openssl s_client -connect hs-tic-tac-toe.nauths.io:443 -CAfile server.crt -verify 1 -verify_return_error -brief
```

## Gameplay

Players take turns entering their moves in the format "row column", for example, "1 2" to place a mark in row 1, column 2. The game checks for win conditions after each move and declares the winner or a draw when the game ends.

## Development

This project uses Haskell Stack for managing the Haskell environment and dependencies. It is recommended to familiarize yourself with the Stack commands and project structure for effective development.

I made numerous mistakes with respect to architecture as I am still familiarizing myself with haskell.

Likewise, there are likely bugs with respect to users connecting and disconnecting suddenly. 

These mistakes likely not be fixed in this iteration as I am planning to port this to ssh and fix the bugs and other issues in that iteration.
