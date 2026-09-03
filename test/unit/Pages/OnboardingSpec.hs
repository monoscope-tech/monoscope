module Pages.OnboardingSpec (spec) where

import Data.Text qualified as T
import Relude
import Test.Hspec


-- | Tailwind cannot see the language guide's visibility class: 'Pages.Onboarding'
-- builds it by concatenation as
-- @"group-has-[#check-" <> l.slug <> ":checked]/pg:block"@, and the scanner only
-- reads literal text. Those rules exist in the built CSS solely because a comment
-- block in the same file spells every one out in full, marked "DO NOT DELETE".
--
-- Nothing linked the two. Add a 'Language' and forget the comment and that guide
-- panel silently never becomes visible — no compile error, no failing test, and
-- the breakage surfaces only in a production CSS build, on the onboarding page,
-- for exactly one language. This is the missing link.
spec :: Spec
spec = describe "Onboarding language guides" do
  it "everyLanguageSlug_hasAScannerVisibleTailwindClass" do
    src <- decodeUtf8 @Text <$> readFileBS "src/Pages/Onboarding.hs"
    let occurrencesAfter tok = drop 1 $ T.splitOn tok src
        slugs = ordNub [T.takeWhile (/= '"') s | s <- occurrencesAfter "Language \""]
        spelledOut = ordNub [T.takeWhile (/= ':') s | s <- occurrencesAfter "group-has-[#check-"]
    -- Guards the guard: if either pattern stops matching, the coverage check
    -- below would pass vacuously on two empty lists.
    length slugs `shouldSatisfy` (> 5)
    length spelledOut `shouldSatisfy` (> 5)
    sort (filter (`notElem` spelledOut) slugs) `shouldBe` []
