### `INIT`

In the `INIT` state, a client sends a `JoinMessage` to join a named room,
identifying itself with a string (not guaranteed/required to be unique).

```
// JoinMessage

{
  "kind": "join",
  "payload": {
    "name": PlayerName,
    "room": RoomName
  }
}
```

It then transitions to the `INIT_WAITING` state.

### `INIT_WAITING`

In the `INIT_WAITING` state, the client is waiting indefinitely to hear from the
second player.

#### Case 1: `RECV JoinMessage`

If the client hears a `JoinMessage`, it was the first to join. In this case, it
sends a `StartGameMessage` to indicate the game has started.

```
// StartGameMessage

{
  "kind": "start-game",
  "payload": {
    "seed": int
  }
}
```

Then, it transitions to the `TURN` state.

#### Case 2: `RECV StartGame`

If the client hears a `StartGameMessage`, it was the second to join. In this
case, the game has started.

It transitions to the `TURN` state.

### `TURN`

In the `TURN` state, the game is underway.

When the local player takes his/her turn, the client sends a `TurnMessage`.

```
// TurnMessage

{
  "kind": "turn",
  "payload": {
    "commands": [Command]
  }
}
```

Then, it checks to see if meanwhile it has received a corresponding
`TurnMessage` from the opponent. If it hasn't, it waits indefinitely for one to
arrive.

Once it has arrived, the client updates the game state with the set of
`Command`s issued this turn.

If the game is not over, the client loops back to the `TURN` state. Otherwise,
it transitions to a terminal `END` state.

---

### Type Appendix

```elm
type alias PlayerName = String
type alias RoomName   = String
```
