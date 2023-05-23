# Revision history for beam-automigrate

## Unreleased

* Added runMigrationsWithEditUpdateAndHooks and tryRunMigrationsWithEditUpdateAndHooks, that take Pg actions to run before and after the auto-migration in the same transaction.
* Major overhaul in how constraints and sequences are diffed
  * haskell/database constraints are now diffed based on *what* they constrain; instead of being an exact match.  inconsequential differences (in particular, the names of constraints, in most cases) no longer require any expensive DML.
  * Sequences ownership is now handled with the `OWNED BY table_name.column_name` mechanism instead of parsing the owner out of the sequence name.  unowned sequences (which might be used for purposes other than surrogate keys, for example) are still supported internally.
  * beam-automigrate will now allow postgres to choose the names of constraints and sequences when possible.  In cases where that's not possible or desired, there is now a straightforward mechanism for end users to specify the name explicitly (see `foreignKeyWithOptions`).
  * constraint options are always optional, with `Default` instances for `UniqueConstraintOptions` and `ForeignKeyConstraintOptions`.  As of this writing, the defaults currently mean "let postgres decide" for constraint names and cascade behaviors (the latter being NO ACTION).
  * Foreign keys can now be configured to reference columns that are different from the table's primary key.  in general this feature is much easier to use; passing a function that projects out the desired columns from both tables.
  * foreign keys can now be partially nullable. (see `foreignKeyOnNullable`)
  * Many instances of database entities being dropped and recreated instead of altered are now supported.
* [#47](https://github.com/obsidiansystems/beam-automigrate/pull/47): Generate postgres enum types in schema when using `Nullable PgEnum` values.

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
