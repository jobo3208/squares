# squares

squares is an abstraction for the "immaculate grid" game, along with a web
frontend to play whatever implementation you can dream up.

Simply implement the `GridBackend` protocol and the `get-backend` multimethod,
add an entry to `squares.edn`, and you've got an immaculate grid game.

The `squares.backend.cities` backend is provided as a simple example.

## pred gen

For some implementations, it's trivial to generate random games that are highly
likely to be winnable and fun. For others, it may be easier to describe the
*types* of predicates you want your game to have, and let squares handle the
rest. The `squares.pred-gen` namespace supports this approach. Here's how it
works:

  - you tell squares:
     1. how to get a random sample of your data
     2. how to generate candidate predicates from that sample
  - squares creates possible games from these predicates and narrows them down
    by removing games that are impossible, too easy, or too redundant
  - squares chooses a game at random from those remaining

The `squares.backend.discs` backend (disc golf discs) is an example that uses
pred gen.
