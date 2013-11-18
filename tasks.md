# Protocol Parser Implementation
Implementation of the Protocol in Haskell and Go.
Only the parser is needed, generation of protocol conforming messages will be
implemented (mostly) separately.

Client (Golang) implementation only has to implement a client specific subset of
the protocol parser.

Invalid messages should be flagged as such and must not have any undesirable
side effects. For example if the client receives a admin flagged message, it
should be ignored and not interpreted as something else.

# Client (Go)
- Flags / optcodes parser (only non-admin "Receive" commands)
- Number field parser
- (Interactive) Command Line Interface - The user has to have a way to
  subscribe to topics, part, send messages, ask for the servers topic list
  etc. This implements the command line parser, not the actual message sending
  / dispatch functionality
- Message compression / decompression
- Message / Command marshalling - Creating message headers for given message
- Message sending - Client has to be able to send message to the server. This
  includes the actual sending over sockets.
- Message receiving / display - Messages sent to the client have to be displayed
  on StdOut including the topic or other information. Same goes for topic
  lists etc.

# Server (Haskell)
Problematic: No way to call into Go from Haskell, some (most?) Client features
have to be duplicated.

- Flags / optcodes parser (only non-admin "Receive" commands)
- Number field parser
- Client handling - Clients have to be registered and their topics
  managed. Should include functions or behaviors to ask for / get clients
  registered to specific topics etc.
- Admin commands handler - No actual functionality, only the dispatching and
  message receiving have to be implemented.
- Regular commands handler - Sending and receiving regular commands and
    dispatching to correspondingly functions.
- Message compression / decompression
- Message / Command marshalling - Creating message headers for given message
- Message forwarding - Server has to be able to send / forward message to
  clients. This includes the actual sending over sockets. Servers must not
  send messages to the client the message originated from.
- GUI implementation for server statistics like currently connected clients,
  sent messages, packet sizes and other fancy stuff :)
