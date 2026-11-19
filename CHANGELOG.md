# Revision history for beam-automigrate

## 0.2.0.0

* Run database tests in separate temporary databases to protect unrelated databases.
* Limit sequence management to the search path, as for tables.
* **Breaking API change:** `renderDataType`, `editToSqlCommand`, and `prettyEditSQL` now return `Either RenderError` results.
  Update callers to handle `Left` before using the rendered SQL.
  Invalid names, unsupported types, and invalid edit structures return typed errors.
  Default expressions remain trusted SQL that PostgreSQL checks.
* Migration execution renders all edits before issuing DDL. Rendering errors therefore occur before any database changes.
* SQL rendering quotes enum and extension type names. Temporal precision precedes `WITH/WITHOUT TIME ZONE`.
  Executable SQL no longer contains `<UNSAFE>` markers. Safety checks remain separate from SQL rendering.
* Identifiers are always double-quoted and string literals are emitted as escaped E-strings.
* **Breaking change:** Generated identifiers longer than 63 UTF-8 bytes now use a deterministic FNV-1a suffix within PostgreSQL's 63-byte limit.
  This includes companion sequence names. The new names can differ from names that PostgreSQL previously truncated.
  Review migration plans for existing schemas before execution. Generated names within the byte limit remain unchanged.
* **Breaking API change:** Move `mkSequenceName` from `Database.Beam.AutoMigrate.Types` to `Database.Beam.AutoMigrate.Util`.
  Replace direct imports from `Types` with imports from `Database.Beam.AutoMigrate` or `Database.Beam.AutoMigrate.Util`.
* Require GHC 9.10 or newer. Test with GHC 9.10.3 and remove the GHC 8.x build configurations.
* Use the maintained Haskell setup action and a current Nixpkgs pin for development and CI.
* `UserDefinedFk` suppresses outgoing inference only on the annotated table types, preserving unrelated and incoming foreign keys.
* Composite foreign keys preserve structural column correspondence, including nullable references to keys containing embedded keys.

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
