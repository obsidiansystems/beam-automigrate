Table of Contents
=================

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

# Getting started (User guide/reference)

For a complete working example refer to the `examples` folder. There is also
a smaller-scale example available at `example/ForeignKeys.hs`, which has been used
to write this guide.

## Deriving an AnnotatedDatabaseSettings

Deriving an `AnnotatedDatabaseSettings` for a Haskell database type is a matter of calling 
`defaultAnnotatedDbSettings`. For example, given:

``` haskell
data CitiesT f = Flower
  { ctCity     :: Columnar f Text
  , ctLocation :: Columnar f Text
  }
  deriving (Generic, Beamable)

data WeatherT f = Weather
  { wtId             :: Columnar f Int
  , wtCity           :: PrimaryKey CitiesT f
  , wtTempLo         :: Columnar f Int
  , wtTempHi         :: Columnar f Int
  }
  deriving (Generic, Beamable)

data ForecastDB f = ForecastDB
  { dbCities   :: f (TableEntity CitiesT)
  , dbWeathers :: f (TableEntity WeatherT)
  }
  deriving (Generic, Database be)

-- ...
-- instance Beam.Table CitiesT where ...
-- ...

```

Then calling `defaultAnnotatedDbSettings` will yield:

```
forecastDB :: AnnotatedDatabaseSettings Postgres ForecastDB
forecastDB = defaultAnnotatedDbSettings defaultDbSettings
```

Where `defaultDbSettings` is the classic function from `beam-core`.

## Overriding the defaults of an `AnnotatedDatabaseSettings`

It is likely that the end user would like to attach extra meta-information to an `AnnotatedDatabaseSettings`.
For example, the user might want to specify which is the default value for a nullable field, or simple express
things like uniqueness constraints or foreign keys (when the inference algorithm cannot proceed due to
ambiguity). To do this, we can piggyback on the familiar API from `beam-core`. For example, given an
imaginary `FlowerDB`, we can do something like this:

```haskell
annotatedDB :: AnnotatedDatabaseSettings Postgres FlowerDB
annotatedDB = defaultAnnotatedDbSettings flowerDB `withDbModification` dbModification
  { dbFlowers   = annotateTableFields tableModification { flowerDiscounted = defaultsTo True }
               <> annotateTableFields tableModification { flowerPrice = defaultsTo 10.0 }
  , dbLineItems = (addTableConstraints $ 
      S.fromList [ Unique "db_line_unique" (S.fromList ["flower_id", "order_id"])
                 , ForeignKey "lineItemOrderID_fkey" (TableName "orders") mempty NoAction NoAction
                 ])
               <> annotateTableFields tableModification { lineItemDiscount = defaultsTo False }
  }
```

**This is where we deviate from beam-migrate.** Beam-migrate forces you to specify "annotations" for each
and every field of each and every column, making everything a bit verbose. Here we decided instead to use the
_other_ API that `beam-core` offers, that is typically used to modify the field names for a standard
`DatabaseSettings`, but it turns out its types are general enough to be used for an `AnnotatedDatabaseSettings`,
just by adding some extra functions (in this case **we provide** the `annotateTableFields`, `defaultsTo` and
`addTableConstraints` functions, the rest is the standard `beam-core` API).

This allows us to still override defaults when we need to without all the extra boilerplate. This API and
these combinators simply manipulates under the hood the particular `TableFieldSchema` associated to each
column, so that the information contained within can be "spliced back" when we generate a `Schema` out of
an `AnnotatedDatabaseSettings`.

## Deriving a Schema

Once we have an `AnnotatedDatabaseSettings`, the next step is to generate a `Schema`. This can be done
simply by calling `fromAnnotatedDbSettings`, like so:

```
hsSchema :: Schema
hsSchema = fromAnnotatedDbSettings forecastDB (Proxy @'[])
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

## Generating an automatic migration

Once a `Schema` has been generated, in order to automatically migrate your schema, it is sufficient to write
something like this:

```haskell
exampleShowMigration :: IO ()
exampleShowMigration = withBeamTestDb printMigration

withBeamTestDb :: (Migration Pg -> Pg ()) -> IO ()
withBeamTestDb action = do
  let connInfo = "host=localhost port=5432 dbname=beam-test-forecast-db"
  bracket (Pg.connectPostgreSQL connInfo) Pg.close $ \conn ->
    Pg.withTransaction conn $ runBeamPostgres conn $ do
      let mig = migrate conn hsSchema
      action mig

exampleAutoMigration :: IO ()
exampleAutoMigration = withBeamTestDb runMigration
```

The `runMigration` function will try to generate another `Schema`, this time from the Postgres database and
the two `Schema`s will be "diffed together" in order to compute the list of edits necessary to migrate from
the DB to the Haskell database. To begin with, we can call `exampleShowMigration` to "preview" the full
SQL command that will be run:

```
Ok, 12 modules loaded.
> import Database.Beam.Migrate.Example.ForeignKeys 
> exampleShowMigration 
CREATE TABLE "cities" (city VARCHAR NOT NULL, location VARCHAR NOT NULL);

CREATE TABLE "weathers" (city__city VARCHAR NOT NULL, id INT NOT NULL, temp_hi INT NOT NULL, temp_lo INT NOT NULL);

ALTER TABLE "cities" ADD CONSTRAINT "cities_pkey" PRIMARY KEY (city);

ALTER TABLE "weathers" ADD CONSTRAINT "weathers_pkey" PRIMARY KEY (id);

ALTER TABLE "weathers" ADD CONSTRAINT "weathers_city__city_fkey" FOREIGN KEY (city__city) REFERENCES "cities"(city);

> 
```

Once we are satisfied with visual inspection, we can run it via `exampleAutoMigration`. If all is well, we
can now check how the DB has been migrated. If we try to call `exampleShowMigration` again, no output should
be shown, because the DB is now up-to-date with the Haskell types.

# Current design (10_000ft overview)

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

## Deriving information from a DatabaseSettings

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

## Deriving information from an AnnotatedDatabaseSettings

This is the phase where we traverse the generic representation of an `AnnotatedDatabaseSettings` in order to
infer the `Schema`. It is **during this phase that we try to discover Foreign keys**.

Foreign key discovery can fail statically. If foreign key discovery fails, one should have the possibility
to override `AnnotatedDatabaseSettings` before running the transformation to `Schema` to manually provide the
necessary hints.

## What is implemented

- [x] Support for JSON, JSONB and Range types;
- [x] Support for automatically inferring FKs (if unambiguous); 
- [x] Support for annotating tables and fields via the standard, familiar `beam-core` API
      (e.g. add a table/column constraint);
- [x] Support for running a migration to mutate the database;

## Shortcomings and limitations

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
