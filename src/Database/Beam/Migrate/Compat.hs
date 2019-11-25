{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.Beam.Migrate.Compat where

import           Data.Proxy
import           Data.Text                      ( Text )
import           Data.Scientific                ( Scientific )
import           Data.Time.Calendar             ( Day )
import           Data.Time                      ( TimeOfDay )
import           Data.Int
import           Data.Time                      ( UTCTime )
import           Data.Word

import           Database.Beam.Backend.SQL

import           Database.Beam.Migrate.Types    ( ColumnType
                                                )

{- | This is a module which adapts and simplifies certain things normally provided by "beam-migrate", but
     without the extra complication of importing and using the library itself.
-}

class HasDefaultSqlDataType ty where

  -- | Provide a data type for the given type
  defaultSqlDataType :: Proxy ty       -- ^ Concrete representation of the type
                     -> Bool           -- ^ 'True' if this field is in an embedded
                                       --   key or table, 'False' otherwise
                     -> ColumnType


-- FIXME(adn) Make this user-defined.

--  -- | Provide arbitrary constraints on a field of the requested type. See
--  -- 'FieldCheck' for more information on the formatting of constraints.
--  defaultSqlDataTypeConstraints
--    :: Proxy ty -- ^ Concrete representation of the type
--    -> Bool     -- ^ 'True' if this field is embedded in a
--                --   foreign key, 'False' otherwise. For
--                --   example, @SERIAL@ types in postgres get a
--                --   @DEFAULT@ constraint, but @SERIAL@ types in
--                --   a foreign key do not.
--    -> [ SchemaConstraint ]
--  defaultSqlDataTypeConstraints _ _ = []


instance HasDefaultSqlDataType ty => HasDefaultSqlDataType (Maybe ty) where
  defaultSqlDataType _ = defaultSqlDataType (Proxy @ty)

instance HasDefaultSqlDataType Int where
  defaultSqlDataType _ _ = intType
instance HasDefaultSqlDataType Int32 where
  defaultSqlDataType _ _ = intType
instance HasDefaultSqlDataType Int16 where
  defaultSqlDataType _ _ = intType
instance HasDefaultSqlDataType Int64 where
  defaultSqlDataType _ _ = bigIntType

instance HasDefaultSqlDataType Word where
  defaultSqlDataType _ _ = numericType (Just (10, Nothing))

instance HasDefaultSqlDataType Word16 where
  defaultSqlDataType _ _ = numericType (Just (5, Nothing))
instance HasDefaultSqlDataType Word32 where
  defaultSqlDataType _ _ = numericType (Just (10, Nothing))
instance HasDefaultSqlDataType Word64 where
  defaultSqlDataType _ _ = numericType (Just (20, Nothing))

instance HasDefaultSqlDataType Text where
  defaultSqlDataType _ _ = varCharType Nothing Nothing
instance HasDefaultSqlDataType SqlBitString where
  defaultSqlDataType _ _ = varBitType Nothing

instance HasDefaultSqlDataType Double where
  defaultSqlDataType _ _ = doubleType

instance HasDefaultSqlDataType Scientific where
  defaultSqlDataType _ _ = numericType (Just (20, Just 10))

instance HasDefaultSqlDataType Day where
  defaultSqlDataType _ _ = dateType

instance HasDefaultSqlDataType TimeOfDay where
  defaultSqlDataType _ _ = timeType Nothing False

instance HasDefaultSqlDataType Bool where
  defaultSqlDataType _ _ = booleanType

instance HasDefaultSqlDataType UTCTime where
  defaultSqlDataType _ _ = timestampType Nothing False
