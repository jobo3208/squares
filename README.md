# squares

squares is an abstraction for the "immaculate grid" game, along with a web
frontend to play whatever implementation you can dream up.

Simply implement the `GridBackend` protocol and the `get-backend` multimethod,
add an entry to `squares.edn`, and you've got an immaculate grid game.

The `squares.backend.cities` backend is provided as an example.
