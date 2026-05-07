{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}
module Main (main) where
import Diff qualified
import Migration
import Generics.SOP
import GHC.Generics qualified as GHC
import Elaborated (ElaborateRecordDiff)
import Generics.SOP.Record
import System.Exit (exitFailure)

data From = From
  { a :: Int
  , b :: Bool
  , c :: Maybe ()
  , e :: ()
  }
  deriving stock (Show, Eq, GHC.Generic)

instance Generic From
instance HasDatatypeInfo From

data To = To
  { a :: Int
  , b :: String
  , d :: Either () ()
  , f :: ()
  }
  deriving stock (Show, Eq, GHC.Generic)

instance Generic To
instance HasDatatypeInfo To

type MyRecordMigration = ElaborateRecordDiff (RecordCodeOf From) (RecordCodeOf To)
  '[ Diff.Rename "c" (Maybe ()) "d" (Either () ())
  , Diff.Delete "e"
  , Diff.Add "f" ()
  ]

myRecordMigration :: RecordMigration MyRecordMigration
myRecordMigration =
  Keep @"a" id
  :* Keep @"b" show
  :* Rename @"c" @"d" (maybe (Left ()) Right)
  :* Delete @"e"
  :* Add @"f" ()
  :* Nil

migrateMyRecord :: From -> To
migrateMyRecord = migrate myRecordMigration

main :: IO ()
main = do
  let orig = From 23 False (Just ()) ()
      migrated = migrateMyRecord orig
      expected = To 23 "False" (Right ()) ()
  if migrated == expected
    then putStrLn "Success"
    else do
      print migrated
      print expected
      exitFailure
