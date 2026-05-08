{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Migration (module Migration) where

import Generics.SOP
import Generics.SOP.Record
import Elaborated
import Diff (Diff)
import Diff qualified


data RecordMigrationStep (diff :: Diff) where
  Rename :: forall fieldFrom fieldTo a b . (a -> b) -> RecordMigrationStep ('Diff.Rename fieldFrom a fieldTo b)
  Delete :: RecordMigrationStep ('Diff.Delete field)
  Add :: forall field a . a -> RecordMigrationStep ('Diff.Add field a)

modify :: forall field a b . (a -> b) -> RecordMigrationStep (Diff.Modify field a b)
modify = Rename

type RecordMigration (diff :: [Diff]) = NP RecordMigrationStep diff

class  MigratableCode (fromCode :: RecordCode) (toCode :: RecordCode) (diff :: [Diff]) (padding :: [Padding]) where
  migration :: Proxy padding -> RecordMigration diff -> Record fromCode -> Record toCode

unconsPadding :: Proxy (Skip : padding) -> Proxy padding
unconsPadding _ = Proxy

instance MigratableCode '[] '[] '[] '[] where
  migration Proxy Nil Nil = Nil

instance (Migratable tyFrom tyTo '[], MigratableCode fromCode toCode diff padding) => MigratableCode ('(field, tyFrom) : fromCode) ('(field, tyTo) : toCode) diff (Skip : padding) where
  migration _ diffs (P a :* as) = P (migrate Nil a) :* migration (Proxy @padding) diffs as

instance MigratableCode fromCode toCode diff padding => MigratableCode ('(fieldFrom, tyFrom) : fromCode) ('(fieldTo, tyTo) : toCode) ('Diff.Rename fieldFrom tyFrom fieldTo tyTo : diff) (Perform : padding) where
  migration _ (Rename f :* diff) (P a :* as) = P (f a) :* migration (Proxy @padding) diff as

instance MigratableCode fromCode toCode diff padding => MigratableCode ('(field, ty) : fromCode) toCode ('Diff.Delete field : diff) (Perform : padding)  where
  migration _ (Delete :* diff) (P _ :* as) = migration (Proxy @padding) diff as

instance MigratableCode fromCode toCode diff padding => MigratableCode fromCode ('(field, ty) : toCode) ('Diff.Add field ty : diff) (Perform : padding)  where
  migration _ (Add a :* diff) as = P a :* migration (Proxy @padding) diff as

class Migratable from to diff where
  migrate :: RecordMigration diff -> from -> to

instance {-# OVERLAPPING #-} Migratable a a '[] where
  migrate = const id

type RecordMigratable from to diff = (IsRecord from (RecordCodeOf from), IsRecord to (RecordCodeOf to), MigratableCode (RecordCodeOf from) (RecordCodeOf to) diff (Elaborate (RecordCodeOf from) (RecordCodeOf to) diff))

instance RecordMigratable from to diff => Migratable from to diff where
  migrate diff = fromRecord . migration (Proxy @(Elaborate (RecordCodeOf from) (RecordCodeOf to) diff)) diff . toRecord
