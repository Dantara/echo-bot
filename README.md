# Echo-bot

This repository contains simple echo bot for VK and Telegram.

This application build on top of [req](https://hackage.haskell.org/package/req) to proceed request to messengers servers and uses [Tagless Final](http://okmij.org/ftp/tagless-final/index.html) approach to abstract business logic over concrete implementation.

*NOTE*: The application was created for education purpose.

### Prerequisites

This project relies on the [Haskell Stack tool](https://docs.haskellstack.org/en/stable/README/).

## Config

This applications uses external config file called `bot.config` to set up its behavour.

The sample config is placed into root directory of this repository.

*NOTE*: The `log_level` can be either `debug`, `info`, `warning` or `error`. 
Delays are specified in miliseconds.

## Build

To build this project simply run

```sh
stack build
```

This will install all dependencies, including a proper version of GHC.

## Run

This project has one executable that you can run with

``` sh
stack exec echo-bot-exe
```

## Test

Tests of this project relies on [Tasty](https://hackage.haskell.org/package/tasty)
framework with [HUnit](https://hackage.haskell.org/package/HUnit) 
and [Hedgehog](https://hackage.haskell.org/package/hedgehog) addons for unit tests 
and property-based ones.

To run tests simply run

```sh
stack test
```
