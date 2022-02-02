# Revision history for beam-automigrate

## Unreleased

* Add `showMigration`
* Extend allowable version bounds for aeson and splitmix
* Add support for postgres' oid column type
* Add support for nullary products to GEnums, GColumns, GTableConstraintColumns and GDefaultTableSchema

## 0.1.2.0

* Escape sql identifiers that are on the [postgres reserved keywords list](https://www.postgresql.org/docs/current/sql-keywords-appendix.html)

## 0.1.1.0

* Escape sql identifiers only when required by the [postgres syntax rules](https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-SYNTAX-IDENTIFIERS)

## 0.1.0.1

* Loosen time version bounds
* readme: Fix warning about a transaction already running
* Support for GHC 8.10

## 0.1.0.0

* Initial release. Generate schemas and migrations for beam databases. See limitations in [README.md](README.md)
