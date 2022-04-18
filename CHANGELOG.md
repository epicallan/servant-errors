# Changelog

`servant-errors` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

## 0.1.0.0

* Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/epicallan/servant-errors/releases

## 0.1.0.1

* Add package dependence bounds

[1]: https://pvp.haskell.org
[2]: https://github.com/epicallan/servant-errors/releases

## 0.1.1.0

* support more GHC versions (8.2 - 8.6)

## 0.1.2.0

* fix reversed object key fields in errors

## 0.1.3.0

* fixes PlainText HasErrorBody instance
* couple of code refactors

## 0.1.3.1

* export encoding helper functions

## 0.1.5.0

* add GHC 8.8 support

## 0.1.6.0

* Fix: use provided content-type


## 0.1.7.0

* Pass old headers to new response when contentType header be added in `newResponse`.
