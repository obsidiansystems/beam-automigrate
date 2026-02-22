# Revision history for beam-automigrate

## Unreleased

* `foreignKeyOnPk` now overrides the foreign key that FK discovery finds for the same column, so its `ON DELETE` and `ON UPDATE` actions take effect instead of a second `NO ACTION` key being added. The constraint is now named after the table it is defined on (`<table>_<column>_fkey`, as FK discovery names it) rather than after the referenced table, which also keeps two keys to the same table from sharing a name. Existing databases see the old constraint dropped and the new one added on the next migration.

## 0.1.7.0

* beam-automigrate will now only manage sequences within the search path, just as it only manages such tables.

## 0.1.6.0

* Fix instance HasSchemaConstraints build failure with GHC 9.2.8

## 0.1.5.0

* Add ltree column type
* Add vector column type
* Fix ghc 9.2.8 build

## 0.1.4.0

* [#52](https://github.com/obsidiansystems/beam-automigrate/pull/52) Support sql arrays
* Loosen some version bounds

## 0.1.3.0

* [#47](https://github.com/obsidiansystems/beam-automigrate/pull/47): Generate postgres enum types in schema when using `Nullable PgEnum` values.
* Add `showMigration`
* Extend allowable version bounds for aeson, base, dlist, pretty-simple, and splitmix
* Add support for postgres' oid column type
* Add `calcMigrationSteps` function to compute the `Diff` that will be performed by a migration without altering the database.
* Support GHC 9.0.2
* Fix an issue where names starting with an upper case letter and containing no other characters requiring escaping would not be properly escaped

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
