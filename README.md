
# Current design (10_000ft overview)

We provide an `AnnotatedDatabaseSettings` type which is similar in spirit to `CheckedDatabaseSettings`,
but simpler. We use this type to annotated a `DatabaseSettings` with extra information (e.g. any particular
table/column constraint) and we derive a `Schema` from this type automatically, via `GHC.Generics`.

We generate one `Schema` for the Haskell datatypes composing your database, and one for the DB schema
currently stored in the database. The `Diff` of the two determines a list of `Edit`s, which can be applied
in a particular (prioritised) order to migrate the DB schema to the Haskell schema.

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
```

Then calling `defaultAnnotatedDbSettings` will yield:

```
forecastDB :: AnnotatedDatabaseSettings Postgres ForecastDB
forecastDB = defaultAnnotatedDbSettings defaultDbSettings
```

Where `defaultDbSettings` is the classic function from `beam-core`.

## Deriving a Schema

Once we have an `AnnotatedDatabaseSettings`, the next step is to generate a `Schema`. This can be done
simply by calling `fromAnnotatedDbSettings`, like so:

```
hsSchema :: Schema
hsSchema = fromAnnotatedDbSettings forecastDB
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

Once a `Schema` has been generated, in order to automatically migrate your schema, it is sufficient to do:

```haskell
exampleAutoMigration :: IO ()
exampleAutoMigration = do
  let connInfo = "host=localhost port=5432 dbname=beam-test-db"
  bracket (Pg.connectPostgreSQL connInfo) Pg.close $ \conn ->
    Pg.withTransaction conn $ runBeamPostgresDebug putStrLn conn $
      runMigration (migrate conn hsSchema)
```

The `runMigration` function will try to generate another `Schema`, this time from the Postgres database and
the two `Schema`s will be "diffed together" in order to compute the list of edits necessary to migrate from
the DB to the Haskell database.

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
