{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Migration (module Migration) where

import Generics.SOP
import Generics.SOP.Record
import Elaborated (ElaboratedRecordDiff)
import Elaborated qualified


data RecordMigrationStep (diff :: ElaboratedRecordDiff) where
  Keep :: forall field a b . (a -> b) -> RecordMigrationStep ('Elaborated.Keep field a b)
  Rename :: forall fieldFrom fieldTo a b . (a -> b) -> RecordMigrationStep ('Elaborated.Rename fieldFrom a fieldTo b)
  Delete :: RecordMigrationStep ('Elaborated.Delete field)
  Add :: forall field a . a -> RecordMigrationStep ('Elaborated.Add field a)


type RecordMigration (diff :: [ElaboratedRecordDiff]) = NP RecordMigrationStep diff

class  MigratableCode (fromCode :: RecordCode) (toCode :: RecordCode) (diff :: [ElaboratedRecordDiff]) where
  migration :: RecordMigration diff -> Record fromCode -> Record toCode

instance MigratableCode '[] '[] '[] where
  migration Nil Nil = Nil

instance (MigratableCode fromCode toCode diff) => MigratableCode ('(field, tyFrom) : fromCode) ('(field, tyTo) : toCode) ('Elaborated.Keep field tyFrom tyTo : diff) where
  migration (Keep f :* diff) (P a :* as) = P (f a) :* migration diff as

instance MigratableCode fromCode toCode diff => MigratableCode ('(fieldFrom, tyFrom) : fromCode) ('(fieldTo, tyTo) : toCode) ('Elaborated.Rename fieldFrom tyFrom fieldTo tyTo : diff) where
  migration (Rename f :* diff) (P a :* as) = P (f a) :* migration diff as

instance MigratableCode fromCode toCode diff => MigratableCode ('(field, ty) : fromCode) toCode ('Elaborated.Delete field : diff)  where
  migration (Delete :* diff) (P _ :* as) = migration diff as

instance MigratableCode fromCode toCode diff => MigratableCode fromCode ('(field, ty) : toCode) ('Elaborated.Add field ty : diff)  where
  migration (Add a :* diff) as = P a :* migration diff as

class Migratable from to diff where
  migrate :: RecordMigration diff -> from -> to

instance {-# OVERLAPPING #-} Migratable a a '[] where
  migrate = const id

instance (IsRecord from (RecordCodeOf from), IsRecord to (RecordCodeOf to), MigratableCode (RecordCodeOf from) (RecordCodeOf to) diff) => Migratable from to diff where
  migrate diff = fromRecord . migration diff . toRecord
