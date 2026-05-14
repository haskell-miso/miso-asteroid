:ramen: :rocket: 🪨 miso-asteroid 
====================

This game was written in Claude Sonnet 4.6 with the prompt (below).

> Can you make the game of asteriods in sample-app/Main.hs in miso?

See it [live](https://asteroid.haskell-miso.org).

<a href="https://asteroid.haskell-miso.org">
  <img width="833" height="439" alt="image" src="https://github.com/user-attachments/assets/30cf2650-3c4a-4295-bf55-bd672174597f" />
</a>

### Development

Call `nix develop` to enter a shell with [GHC 9.12.2](https://haskell.org/ghc)

```bash
$ nix develop
```

Once in the shell, you can call `cabal run` to start the development server and view the application at http://localhost:8080

### Build (Web Assembly)

```bash
$ nix develop .#wasm --command bash -c "make"
```

### Build (JavaScript)

```bash
$ nix develop .#ghcjs --command bash -c "build"
```

### Serve

To host the built application you can call `serve`

```bash
$ nix develop .#wasm --command bash -c "serve"
```

### Clean

```bash
$ nix develop .#wasm --command bash -c "make clean"
```

This comes with a GitHub action that builds and auto hosts the example.
