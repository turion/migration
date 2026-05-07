module Elaborated (module Elaborated) where

import Diff qualified
import GHC.TypeError (TypeError, ErrorMessage (..))
import GHC.Base (Symbol)
import Data.Kind (Type)
import Generics.SOP.Record

-- FIXME Keep could be an alias for Rename
data ElaboratedRecordDiff = Keep Symbol Type Type | Rename Symbol Type Symbol Type | Delete Symbol | Add Symbol Type

-- FIXME If i keep the types here I need to check them ideally for a better error message
-- FIXME to get rid of them I need to take the types away from Keep

type family ElaborateRecordDiff (recordCodeFrom :: RecordCode) (recordCodeTo :: RecordCode) (diff :: [Diff.Diff]) :: [ElaboratedRecordDiff] where
  ElaborateRecordDiff '[] '[] '[] = '[]
  ElaborateRecordDiff '[] _ '[] = TypeError ('Text "recordCodeFrom and diff empty, but recordCodeTo not")
  ElaborateRecordDiff '[] '[] (diff ': diffs) = TypeError ('Text "Couldn't apply diff anywhere:" :<>: ShowType diff)
  ElaborateRecordDiff _ '[] '[] = TypeError ('Text "recordCodeTo and diff empty, but recordCodeFrom not")

  ElaborateRecordDiff ('(fiFrom, tyFrom) ': recordCodeFrom) ('(fiTo, tyTo) ': recordCodeTo) ('Diff.Rename fiFrom tyFrom fiTo tyTo ': diff) = Rename fiFrom tyFrom fiTo tyTo : ElaborateRecordDiff recordCodeFrom recordCodeTo diff
  ElaborateRecordDiff ('(fiFrom, tyFrom) ': recordCodeFrom) ('(fiTo, tyTo) ': recordCodeTo) ('Diff.Rename fiFrom tyFrom fiToWrong tyTo ': diff) = TypeError
    ('Text "Tried to rename, but target name doesn't match" :$$:
     'Text "Expected:" :<>: Text fiTo :$$:
     'Text "Actual" :<>: Text fiToWrong
    )
  ElaborateRecordDiff ('(fiFrom, _) ': recordCodeFrom) ('(fiTo, _) ': recordCodeTo) ('Diff.Rename fiFromWrong tyFrom fiTo tyTo ': diff) = TypeError
    ('Text "Tried to rename, but target name doesn't match" :$$:
     'Text "Expected:" :<>: Text fiTo :$$:
     'Text "Actual" :<>: Text fiFromWrong
    )

  ElaborateRecordDiff ('(fiFrom, _) ': recordCodeFrom) ('(fiFrom, _) ': recordCodeTo) ('Diff.Delete fiFrom ': diff) = TypeError
    ('Text "Tried to delete, but stays the same:" :<>: Text fiFrom)
  ElaborateRecordDiff ('(fiFrom, _) ': recordCodeFrom) recordCodeTo (' Diff.Delete fiFrom ': diff) = Delete fiFrom : ElaborateRecordDiff recordCodeFrom recordCodeTo diff

  ElaborateRecordDiff recordCodeFrom ('(fiTo, tyTo) ': recordCodeTo) ('Diff.Add fiTo tyTo : diff) = Add fiTo tyTo ': ElaborateRecordDiff recordCodeFrom recordCodeTo diff

  ElaborateRecordDiff ('(fi, tyFrom) ': recordCodeFrom) ('(fi, tyTo) ': recordCodeTo) diff = Keep fi tyFrom tyTo ': ElaborateRecordDiff recordCodeFrom recordCodeTo diff
