# Bitcoin BR Chrome Extension

This repo was based on the structure of the code for a [bitcoin price checking extension][base-extension]
by [jouderianjr][source-repo].

### Running Locally:

Clone this repo.

```
git clone https://github.com/correlation-one/???  # todo: fix this
```

Install all dependencies:
```
yarn install
```

### Serve locally:
```
yarn start
```
* Access app at `http://localhost:8080/`
* Get coding! The entry point file is `src/elm/Main.elm`
* Browser will refresh automatically on any file changes..


### Build & bundle for prod:
```
yarn build
```

* Files are saved into the `/dist` folder
* To check it, open `dist/index.html`

### Run Tests

```
elm test
```

[source-repo]: https://github.com/jouderianjr/bitcoin-br-chrome-extension
[base-extension]: https://chrome.google.com/webstore/detail/bitcoin-br/keoihaeoogphapkdoijfnfboggimfdkj?hl=pt-BR
