module Diff (module Diff) where
import GHC.TypeLits (Symbol)
import Data.Kind (Type)



data Diff = Rename Symbol Type Symbol Type | Delete Symbol | Add Symbol Type

type Modify field a b = Rename field a field b
