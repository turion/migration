module Elaborated (module Elaborated) where

import Diff qualified
import GHC.TypeError (TypeError, ErrorMessage (..))
import Generics.SOP.Record

data Padding = Skip | Perform

-- FIXME If i keep the types here I need to check them ideally for a better error message
-- FIXME to get rid of them I need to take the types away from Keep

type family Elaborate (recordCodeFrom :: RecordCode) (recordCodeTo :: RecordCode) (diff :: [Diff.Diff]) :: [Padding] where
  Elaborate '[] '[] '[] = '[]
  Elaborate '[] _ '[] = TypeError ('Text "recordCodeFrom and diff empty, but recordCodeTo not")
  Elaborate '[] '[] (diff ': diffs) = TypeError ('Text "Couldn't apply diff anywhere:" :<>: ShowType diff)
  Elaborate _ '[] '[] = TypeError ('Text "recordCodeTo and diff empty, but recordCodeFrom not")

  Elaborate ('(fiFrom, tyFrom) ': recordCodeFrom) ('(fiTo, tyTo) ': recordCodeTo) ('Diff.Rename fiFrom tyFrom fiTo tyTo ': diff) = Perform : Elaborate recordCodeFrom recordCodeTo diff
  Elaborate ('(fiFrom, tyFrom) ': recordCodeFrom) ('(fiTo, tyTo) ': recordCodeTo) ('Diff.Rename fiFrom tyFromWrong fiTo tyTo ': diff) = TypeError
    ('Text "Couldn't match source types of a migration rename from " :<>: ShowType fiFrom :<>: Text " to " :<>: ShowType fiTo :<>: Text ":" :$$:
     'Text "Expected: " :<>: ShowType tyFrom :$$:
     'Text "  Actual: " :<>: ShowType tyFromWrong
    )
  Elaborate ('(fiFrom, tyFrom) ': recordCodeFrom) ('(fiTo, tyTo) ': recordCodeTo) ('Diff.Rename fiFrom tyFrom fiTo tyToWrong ': diff) = TypeError
    ('Text "Couldn't match target types of a migration rename from " :<>: ShowType fiFrom :<>: Text " to " :<>: ShowType fiTo :<>: Text ":" :$$:
     'Text "Expected: " :<>: ShowType tyTo :$$:
     'Text "  Actual: " :<>: ShowType tyToWrong
    )
  Elaborate ('(fiFrom, tyFrom) ': recordCodeFrom) ('(fiTo, tyTo) ': recordCodeTo) ('Diff.Rename fiFrom tyFromWrong fiTo tyToWrong ': diff) = TypeError
    ('Text "Couldn't match source types of a migration rename from " :<>: ShowType fiFrom :<>: Text " to " :<>: ShowType fiTo :<>: Text ":" :$$:
     'Text "Expected: " :<>: ShowType tyFrom :$$:
     'Text "  Actual: " :<>: ShowType tyFromWrong :$$:
     'Text "Couldn't match target types of a migration rename from " :<>: ShowType fiFrom :<>: Text " to " :<>: ShowType fiTo :<>: Text ":" :$$:
     'Text "Expected: " :<>: ShowType tyTo :$$:
     'Text "  Actual: " :<>: ShowType tyToWrong
    )
  Elaborate ('(fiFrom, tyFrom) ': recordCodeFrom) ('(fiTo, tyTo) ': recordCodeTo) ('Diff.Rename fiFrom tyFrom fiToWrong tyTo ': diff) = TypeError
    ('Text "Tried to rename, but target name doesn't match" :$$:
     'Text "Expected: " :<>: Text fiTo :$$:
     'Text "Actual: " :<>: Text fiToWrong
    )
  Elaborate ('(fiFrom, _) ': recordCodeFrom) ('(fiTo, _) ': recordCodeTo) ('Diff.Rename fiFromWrong tyFrom fiTo tyTo ': diff) = TypeError
    ('Text "Tried to rename, but source name doesn't match" :$$:
     'Text "Expected: " :<>: Text fiFrom :$$:
     'Text "Actual: " :<>: Text fiFromWrong
    )

  Elaborate ('(fiFrom, _) ': recordCodeFrom) ('(fiFrom, _) ': recordCodeTo) ('Diff.Delete fiFrom ': diff) = TypeError
    ('Text "Tried to delete, but stays the same:" :<>: Text fiFrom)
  Elaborate ('(fiFrom, _) ': recordCodeFrom) recordCodeTo (' Diff.Delete fiFrom ': diff) = Perform : Elaborate recordCodeFrom recordCodeTo diff

  Elaborate recordCodeFrom ('(fiTo, tyTo) ': recordCodeTo) ('Diff.Add fiTo tyTo : diff) = Perform ': Elaborate recordCodeFrom recordCodeTo diff

  Elaborate ('(fi, tyFrom) ': recordCodeFrom) ('(fi, tyTo) ': recordCodeTo) diff = Skip ': Elaborate recordCodeFrom recordCodeTo diff
