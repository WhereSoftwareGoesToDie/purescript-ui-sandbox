# Purescript UI sandbox

You can use this to log in and log out, authenticating against a "remote" API

© Anchor 2015

## Includes

* A client written in PureScript
* A server written in Haskell (which uses the Servant library, and some gnarly
  WAI stuff to handle CORS)

## Requirements

* Haskell & Cabal
* [Purescript](http://www.purescript.org): `cabal install purescript`
* NPM
* [Bower](http://bower.io): `npm install -g bower`

## Running the client

`make run`

## Building the server

```bash
cd server
cabal sandbox init
cabal install
```

## Running the server

`cd server && cabal exec metadata-api`

## What usernames & passwords work

* ("jim@anchor.net.au", "bob")
* ("hi@anchor.net.au", "there")
* ("jane@anchor.net.au", "doe")
* ("hoob@anchor.net.au", "adoob")
