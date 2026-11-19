# Revision history for beam-automigrate

## 0.2.0.0

* **Breaking behavior change:** `foreignKeyOnPk` replaces inferred foreign keys for the same local columns.
  The declared `ON DELETE` and `ON UPDATE` actions replace the inferred `NO ACTION` actions.
  This extends the manual-override fix contributed in [#69](https://github.com/obsidiansystems/beam-automigrate/pull/69).
  Existing databases can require constraint replacement. Review migration plans before execution.

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
* **Breaking behavior change:** `UserDefinedFk` now disables automatic foreign key inference only for outgoing references from the annotated table type.
  References from other table types remain intact, including references to the annotated type.
  Existing schemas can gain foreign key constraints that earlier versions omitted.
* **Breaking behavior change:** Infer composite foreign keys by matching field positions, not by sorting column names independently.
  Preserve this correspondence through embedded keys and nullable references.
  Migration plans can replace incorrectly paired constraints. Existing rows might violate the corrected constraints.
  Review the migration plan and affected rows before execution.
* **Breaking API change:** `foreignKeyOn` now requires a non-empty list of scalar `References` pairs with compatible types.
  Update each call to supply one `References` pair for each local and referenced column.
  Use `foreignKeyOnNullablePk` for nullable whole-key references.
* Check manual declarations against the completed schema. Manual foreign keys replace inferred keys only for the same local columns.
* **Breaking behavior change:** Generate foreign key constraint names from both tables and every column pair.
  Existing constraint names can change. Review migration plans before execution.
* **Breaking API change:** `fromAnnotatedDbSettings` now returns `Either [ValidationFailed] Schema`.
  Handle errors for duplicate tables, columns, sequences, constraints, and invalid foreign keys before using the schema.
* Normalize equivalent PostgreSQL column types before planning and rendering.
  This covers default CHAR/BIT widths, NATIONAL CHAR/VARCHAR, DECIMAL/FLOAT aliases, numeric scale zero, and text-backed enums.
* **Breaking behavior change:** Reject invalid type widths, precisions, array ranks, and vector dimensions with contextual errors.
  Review type declarations and migration plans before execution.
* **Breaking API change:** Store each column default in one slot as a complete SQL expression with sequence and enum dependencies.
  Update callers to use the new default representation and `ColumnDefaultChanged` edit.
  `defaultsTo` replaces the existing default. A default-only change produces one edit.
  Serial defaults use bounded sequence names and PostgreSQL-compatible escaping.
  Read default dependencies from `pg_depend` instead of parsing expression text.
* **Breaking behavior change:** Reject explicit defaults that conflict with a generated serial sequence.
  Schema derivation returns `CompanionSequenceConflictsWithDefault` with the affected table and column.
  Remove the explicit default to retain automatic sequence generation.
  Referencing keys can keep explicit defaults without generating another sequence.
* Preserve native enum definitions through table fields, nullable values, and vectors.
  Schema derivation now includes the enum definitions required by nullable enum arrays.

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
