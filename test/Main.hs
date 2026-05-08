{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}
module Main (main) where
import Diff qualified
import Migration
import Generics.SOP
import GHC.Generics qualified as GHC
import System.Exit (exitFailure)

data From = From
  { a :: Int
  , b :: Bool
  , c :: NestedFrom
  , e :: ()
  }
  deriving stock (Show, Eq, GHC.Generic)

instance Generic From
instance HasDatatypeInfo From

data To = To
  { a :: Int
  , b :: String
  , d :: NestedTo
  , f :: ()
  }
  deriving stock (Show, Eq, GHC.Generic)

instance Generic To
instance HasDatatypeInfo To

data NestedFrom = NestedFrom
  { g :: Bool
  , h :: NestedStructurallyEqualFrom
  }
  deriving stock (Show, Eq, GHC.Generic)

instance Generic NestedFrom
instance HasDatatypeInfo NestedFrom

data NestedTo = NestedTo
  { i :: String
  , h :: NestedStructurallyEqualTo
  }
  deriving stock (Show, Eq, GHC.Generic)

instance Generic NestedTo
instance HasDatatypeInfo NestedTo

data NestedStructurallyEqualFrom = NestedStructurallyEqualFrom
  { k :: Int
  , l :: String
  }
  deriving stock (Show, Eq, GHC.Generic)

instance Generic NestedStructurallyEqualFrom
instance HasDatatypeInfo NestedStructurallyEqualFrom

data NestedStructurallyEqualTo = NestedStructurallyEqualTo
  { k :: Int
  , l :: String
  }
  deriving stock (Show, Eq, GHC.Generic)

instance Generic NestedStructurallyEqualTo
instance HasDatatypeInfo NestedStructurallyEqualTo

type MyRecordMigration = 
  '[ Diff.Modify "b" Bool String
  , Diff.Rename "c" NestedFrom "d" NestedTo
  , Diff.Delete "e"
  , Diff.Add "f" ()
  ]

myRecordMigration :: RecordMigration MyRecordMigration
myRecordMigration =
  modify @"b" show
  :* Rename @"c" @"d" migrateNested
  :* Delete @"e"
  :* Add @"f" ()
  :* Nil

migrateMyRecord :: From -> To
migrateMyRecord = migrate myRecordMigration

type MyNestedMigration =
  '[ Diff.Rename "g" Bool "i" String
  ]

myNestedMigration :: RecordMigration MyNestedMigration
myNestedMigration =
  Rename @"g" @"i" show
  :* Nil

migrateNested :: NestedFrom -> NestedTo
migrateNested = migrate myNestedMigration

main :: IO ()
main = do
  let orig = From 23 False (NestedFrom True (NestedStructurallyEqualFrom 42 "hello")) ()
      migrated = migrateMyRecord orig
      expected = To 23 "False" (NestedTo "True" (NestedStructurallyEqualTo 42 "hello")) ()
  if migrated == expected
    then putStrLn "Success"
    else do
      print migrated
      print expected
      exitFailure
