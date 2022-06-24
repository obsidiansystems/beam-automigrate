beam-automigrate
================
[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![Hackage](https://img.shields.io/hackage/v/beam-automigrate.svg)](https://hackage.haskell.org/package/beam-automigrate) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/beam-automigrate/badge)](https://matrix.hackage.haskell.org/#/package/beam-automigrate)   [![Github CI](https://github.com/obsidiansystems/beam-automigrate/workflows/github-action/badge.svg)](https://github.com/obsidiansystems/beam-automigrate/actions) [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/obsidiansystems/beam-automigrate/blob/master/LICENSE)

Automatic migrations for [beam](https://hackage.haskell.org/package/beam-core) databases!

![Auto-migration example animation](https://i.imgur.com/xuPyUfg.gif)

Table of Contents
-----------------

* [Table of Contents](#table-of-contents)
* [Getting started (User guide/reference)](#getting-started-user-guidereference)
   * [Deriving an AnnotatedDatabaseSettings](#deriving-an-annotateddatabasesettings)
   * [Overriding the defaults of an AnnotatedDatabaseSettings](#overriding-the-defaults-of-an-annotateddatabasesettings)
   * [Deriving a Schema](#deriving-a-schema)
   * [Generating an automatic migration](#generating-an-automatic-migration)
* [Current design (10_000ft overview)](#current-design-10_000ft-overview)
   * [Deriving information from a DatabaseSettings](#deriving-information-from-a-databasesettings)
   * [Deriving information from an AnnotatedDatabaseSettings](#deriving-information-from-an-annotateddatabasesettings)
   * [What is implemented](#what-is-implemented)
   * [Shortcomings and limitations](#shortcomings-and-limitations)
* [Contributors](#contributors)

Getting started (User guide/reference)
======================================

This example is a literate haskell source file. You can run it interactively
with the following command:

```bash
$ cabal repl readme
```

If you're using [nix](https://nixos.org/nix), you can enter a shell with the
appropriate dependencies with the following command:

```bash
$ nix-shell
```

From that nix-shell, you can run `cabal repl readme`.

To run the example code, run `main` inside of the cabal repl.

For a more examples, refer to the `examples` folder.

Deriving an AnnotatedDatabaseSettings
-------------------------------------

Deriving an `AnnotatedDatabaseSettings` for a Haskell database type is a matter of calling
`defaultAnnotatedDbSettings`. For example, given:

``` haskell

> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE DeriveAnyClass #-}
> {-# LANGUAGE TypeApplications #-}
> {-# LANGUAGE TypeFamilies #-}
> import Prelude hiding ((.))
> import Control.Category ((.))
> import Data.Proxy (Proxy(..))
> import Database.Beam.Postgres
> import Database.Beam.Schema
> import Database.Beam (val_, primaryKey)
> import qualified Database.Beam.AutoMigrate as BA
> import Database.PostgreSQL.Simple as Pg
> import Gargoyle.PostgreSQL.Connect
> import GHC.Generics
> import Data.Default.Class
> import Data.Pool (withResource)
> import Data.Text
>
> data CitiesT f = City
>   { ctCity     :: Columnar f Text
>   , ctLocation :: Columnar f Text
>   , ctCapital  :: Columnar f Bool
>   }
>   deriving (Generic, Beamable)
>
> data WeatherT f = Weather
>   { wtId             :: Columnar f Int
>   , wtCity           :: PrimaryKey CitiesT f
>   , wtTempLo         :: Columnar f Int
>   , wtTempHi         :: Columnar f Int
>   }
>   deriving (Generic, Beamable)
>
> data ForecastDB f = ForecastDB
>   { dbCities   :: f (TableEntity CitiesT)
>   , dbWeathers :: f (TableEntity WeatherT)
>   }
>   deriving (Generic, Database be)
>
> instance Table CitiesT where
>   data PrimaryKey CitiesT f = CityID (Columnar f Text)
>     deriving (Generic, Beamable)
>   primaryKey = CityID . ctCity
>
> instance Table WeatherT where
>   data PrimaryKey WeatherT f = WeatherID (Columnar f Int)
>     deriving (Generic, Beamable)
>   primaryKey = WeatherID . wtId

```

Then calling `defaultAnnotatedDbSettings` will yield:

```haskell

> forecastDB :: BA.AnnotatedDatabaseSettings Postgres ForecastDB
> forecastDB = BA.defaultAnnotatedDbSettings defaultDbSettings

```

Where `defaultDbSettings` is the classic function from `beam-core`.

Overriding the defaults of an `AnnotatedDatabaseSettings`
---------------------------------------------------------

It is likely that the end user would like to attach extra meta-information to an `AnnotatedDatabaseSettings`.
For example, the user might want to specify which is the default value for a nullable field, or simple express
things like uniqueness constraints or foreign keys (when the inference algorithm cannot proceed due to
ambiguity). To do this, we can piggyback on the familiar API from `beam-core`. For example, we can do something like this:

```haskell

> annotatedDB :: BA.AnnotatedDatabaseSettings Postgres ForecastDB
> annotatedDB = BA.defaultAnnotatedDbSettings defaultDbSettings `withDbModification` dbModification
>   { dbCities =
>       BA.annotateTableFields tableModification { ctCapital = BA.defaultsTo $ val_ False }
>         <> BA.uniqueConstraintOn [BA.U ctCity, BA.U ctLocation]
>   , dbWeathers = BA.annotateTableFields tableModification <>
>       BA.foreignKeyOnWithOptions (dbCities defaultDbSettings) wtCity primaryKey def { BA.onDelete = BA.Cascade, BA.onUpdate = BA.Restrict}
>   }

```

**This is where we deviate from beam-migrate.** Beam-migrate forces you to specify "annotations" for each
and every field of each and every column, making everything a bit verbose. Here we decided instead to use the
_other_ API that `beam-core` offers, that is typically used to modify the field names for a standard
`DatabaseSettings`, but it turns out its types are general enough to be used for an `AnnotatedDatabaseSettings`,
just by adding some extra functions (the ones prefixed with `BA`, the rest is the standard `beam-core` API).

This allows us to still override defaults when we need to without all the extra boilerplate. This API and
these combinators simply manipulates under the hood the particular `TableFieldSchema` associated to each
column, so that the information contained within can be "spliced back" when we generate a `Schema` out of
an `AnnotatedDatabaseSettings`.

Deriving a Schema
-----------------

Once we have an `AnnotatedDatabaseSettings`, the next step is to generate a `Schema`. This can be done
simply by calling `fromAnnotatedDbSettings`, like so:

```
> hsSchema :: BA.Schema
> hsSchema = BA.fromAnnotatedDbSettings annotatedDB (Proxy @'[])

```

This will generate something like this:

```haskell
Schema
    { schemaTables = fromList
        [
            ( TableName { tableName = "cities" }
            , Table
                { tableConstraints = fromList
                    [ PrimaryKey "cities_pkey"
                        ( fromList
                            [ ColumnName { columnName = "city" } ]
                        )
                    ]
                , tableColumns = fromList
                    [
                        ( ColumnName { columnName = "city" }
                        , Column
                            { columnType = SqlStdType ( DataTypeChar True Nothing Nothing )
                            , columnConstraints = fromList [ NotNull ]
                            }
                        )
                    ,
                        ( ColumnName { columnName = "location" }
                        , Column
                            { columnType = SqlStdType ( DataTypeChar True Nothing Nothing )
                            , columnConstraints = fromList [ NotNull ]
                            }
                        )
                    ]
                }
            )
        ,
            ( TableName { tableName = "weathers" }
            , Table
                { tableConstraints = fromList
                    [ PrimaryKey "weathers_pkey"
                        ( fromList
                            [ ColumnName { columnName = "id" } ]
                        )
                    , ForeignKey "weathers_city__city_fkey"
                        ( TableName { tableName = "cities" } )
                        ( fromList
                            [
                                ( ColumnName { columnName = "city__city" }
                                , ColumnName { columnName = "city" }
                                )
                            ]
                        ) NoAction NoAction
                    ]
                , tableColumns = fromList
                    [
                        ( ColumnName { columnName = "city__city" }
                        , Column
                            { columnType = SqlStdType ( DataTypeChar True Nothing Nothing )
                            , columnConstraints = fromList [ NotNull ]
                            }
                        )
                    ,
                        ( ColumnName { columnName = "id" }
                        , Column
                            { columnType = SqlStdType DataTypeInteger
                            , columnConstraints = fromList [ NotNull ]
                            }
                        )
                    ,
                        ( ColumnName { columnName = "temp_hi" }
                        , Column
                            { columnType = SqlStdType DataTypeInteger
                            , columnConstraints = fromList [ NotNull ]
                            }
                        )
                    ,
                        ( ColumnName { columnName = "temp_lo" }
                        , Column
                            { columnType = SqlStdType DataTypeInteger
                            , columnConstraints = fromList [ NotNull ]
                            }
                        )
                    ]
                }
            )
        ]
    , schemaEnumerations = fromList []
    }
```

Notable things to notice:

* Foreign keys have been automatically inferred;
* Each column is mapped to a sensible SQL datatype;
* Non `Maybe/Nullable` types have a `NotNull` constraint added;

Generating an automatic migration
---------------------------------

Once a `Schema` has been generated, in order to automatically migrate your schema, it is sufficient to write
something like this:

```haskell

>
> readmeDbTransaction :: (Connection -> IO a) -> IO a
> readmeDbTransaction f = withDb "readme-db" $ \pool ->
>   withResource pool $ \conn ->
>     Pg.withTransaction conn $ f conn
>
> exampleShowMigration :: IO ()
> exampleShowMigration = readmeDbTransaction $ \conn ->
>   runBeamPostgres conn $
>     BA.printMigration $ BA.migrate conn hsSchema
>
> exampleAutoMigration :: IO ()
> exampleAutoMigration = withDb "readme-db" $ \pool ->
>   withResource pool $ \conn ->
>     BA.tryRunMigrationsWithEditUpdate annotatedDB conn
>
> main :: IO ()
> main = do
>   putStrLn "----------------------------------------------------"
>   putStrLn "MIGRATION PLAN (if migration needed):"
>   putStrLn "----------------------------------------------------"
>   exampleShowMigration
>   putStrLn "----------------------------------------------------"
>   putStrLn "MIGRATE?"
>   putStrLn "----------------------------------------------------"
>   putStrLn "Would you like to run the migration on the database in the folder \"readme-db\" (will be created if it doesn't exist)? (y/n)"
>   response <- getLine
>   case response of
>     "y" -> exampleAutoMigration
>     "Y" -> exampleAutoMigration
>     _ -> putStrLn "Exiting"
>

```

The `exampleAutoMigration` function will try to generate another `Schema`, this time from the Postgres database and
the two `Schema`s will be "diffed together" in order to compute the list of edits necessary to migrate from
the DB to the Haskell database. To begin with, we can call `exampleShowMigration` to "preview" the full
SQL command that will be run:

```
Ok, 12 modules loaded.
-> import Database.Beam.Migrate.Example.ForeignKeys
-> exampleShowMigration
CREATE TABLE "cities" (city VARCHAR NOT NULL, location VARCHAR NOT NULL);

CREATE TABLE "weathers" (city__city VARCHAR NOT NULL, id INT NOT NULL, temp_hi INT NOT NULL, temp_lo INT NOT NULL);

ALTER TABLE "cities" ADD CONSTRAINT "cities_pkey" PRIMARY KEY (city);

ALTER TABLE "weathers" ADD CONSTRAINT "weathers_pkey" PRIMARY KEY (id);

ALTER TABLE "weathers" ADD CONSTRAINT "weathers_city__city_fkey" FOREIGN KEY (city__city) REFERENCES "cities"(city);

```

Once we are satisfied with visual inspection, we can run it via `exampleAutoMigration`. If all is well, we
can now check how the DB has been migrated. If we try to call `exampleShowMigration` again, no output should
be shown, because the DB is now up-to-date with the Haskell types.

Current design (10_000ft overview)
==================================

Beam itself provides a `DatabaseSettings` type. A value of this type is typically derived generically from
the Haskell datatypes representing the database schema, but can be amended. The primary purpose of
`DatabaseSettings` is to provide a mapping between Haskell names for tables and columns and the corresponding
DB-side names.

On top of this, we provide an `AnnotatedDatabaseSettings` type. This is similar in spirit to the
`CheckedDatabaseSettings` provided by the original `beam-migrate`, but a bit simpler. On top of `DatabaseSettings`,
the `AnnotatedDatabaseSettings` contain additional information, in particular constraints that tables and
columns must satisfy. Once again, a value of this type can be derived generically, but the information
can be amended.

Both `DatabaseSettings` and `AnnotatedDatabaseSettings` follow the structure of the Haskell datatypes
comprising the schema, and are therefore quite strongly typed.

From an `AnnotatedDatabaseSettings` value, we can internally derive a `Schema`. This is a straightforward
representation of a DB schema without any type-level magic.

We can similarly generate a `Schema` value for the DB schema currently stored in the database. We can then
diff the two `Schema`s to get a `Diff` which determines a list of `Edit`s. Such edits can be applied in a
particular (prioritised) order to migrate the DB schema to the Haskell schema.

There are two important stages in the library, the first one being when we call `defaultAnnotatedDbSettings`
and the second one when we call `fromAnnotatedDbSettings`. The first function converts from a `DatabaseSettings`
into an `AnnotatedDatabaseSettings` whereas the latter convert from an `AnnotatedDatabaseSettings` into a
`Schema`. Both uses generic-derivation but in a different way and for different purposes.

Deriving information from a DatabaseSettings
--------------------------------------------

During this phase we "zip" all the tables together and we essentially convert each `DatabaseEntity` into an
`AnnotatedDatabaseEntity`. The latter is ever so slightly similar to the former but crucially it embeds extra
information. One way to see this is that exactly as a `DatabaseEntity` carry around a `TableSettings` which
carries meta-information on the naming of each particular column for each table, an `AnnotatedDatabaseEntity`
carries what's called a `TableSchema`, which is defined as:

```haskell
-- | A table schema.
type TableSchema tbl =
    tbl (TableFieldSchema tbl)

-- | A schema for a field within a given table
data TableFieldSchema (tbl :: (* -> *) -> *) ty where
    TableFieldSchema
      ::
      { tableFieldName :: Text
      , tableFieldSchema :: FieldSchema ty }
      -> TableFieldSchema tbl ty

data FieldSchema ty where
  FieldSchema :: ColumnType
              -> Set ColumnConstraint
              -> FieldSchema ty
```

Looking at this, the similarity with a `TableSettings` is quite obvious:

```haskell
type TableSettings tbl = tbl (TableField tbl)
```

Here is where the second generic-derivation algorithm comes in, and it "maps" each `TableField` with a new
`TableFieldSchema`, which is initialised with "stock" default values for the `ColumnType`
and `Set ColumnConstraint`. There values are automatically inferred thanks to the `HasDefaultSqlDataType` and
`HasSchemaConstraints` typeclasses defined over at `Database.Beam.Migrate.Compat`. **This gives us a concrete
anchor point for the user to further annotate the database and override each individual table & column with
extra information.**

This is described later on in the "Overriding the defaults of an `AnnotatedDatabaseSettings`" section.

Deriving information from an AnnotatedDatabaseSettings
------------------------------------------------------

This is the phase where we traverse the generic representation of an `AnnotatedDatabaseSettings` in order to
infer the `Schema`. It is **during this phase that we try to discover Foreign keys**.

Foreign key discovery can fail statically. If foreign key discovery fails, one should have the possibility
to override `AnnotatedDatabaseSettings` before running the transformation to `Schema` to manually provide the
necessary hints.

What is implemented
-------------------

- [x] Support for JSON, JSONB and Range types;
- [x] Support for automatically inferring FKs (if unambiguous);
- [x] Support for annotating tables and fields via the standard, familiar `beam-core` API
      (e.g. add a table/column constraint);
- [x] Support for running a migration to mutate the database;

Shortcomings and limitations
----------------------------

- [ ] Deriving a particular instance using `deriving via` could hamper `Schema` discovery. For example
  let's imagine we have:

```haskell
data Foo = Bar | Baz deriving HasDefaultSqlDataType via (DbEnum Foo)

data MyTable f = MyTable {
  myTableFoo :: Columnar f Foo
}
```

This won't correctly infer `Foo` is a `DbEnum`, at the moment, as this information is derived directly from
the types of each individual columns.

- [ ] There is no support yet for specifying FKs in case the discovery algorithm fails due to ambiguity;
- [ ] There is no support yet for manual migrations;
- [ ] There is no support/design for "composable databases";
- [ ] Some parts of the library are Pg-specific.

Contributors
============

This library was originally written for [Obsidian Systems](https://obsidian.systems) by Alfredo Di Napoli and Andres Löh of [Well-Typed](https://www.well-typed.com/). Other contributors include Dan Bornside, Sean Chalmers, Ryan Trinkle, and Ali Abrar of Obsidian Systems.
