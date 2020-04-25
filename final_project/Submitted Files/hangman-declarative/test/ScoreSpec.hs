module ScoreSpec where

import Score
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Monoid" $ do
    it "mempty is neutral for mappend" $
      (mempty `mappend` Cons 0 Empty) `shouldBe` Cons 0 Empty
  describe "Functor" $ do
    it "" $
      (fmap (+1) (Cons 0 $ Cons 1 Empty)) `shouldBe` (Cons 1 $ Cons 2 Empty)
  describe "Applicative Functor" $ do
    it "" $
      ((Cons (+1) Empty) <*> (Cons 0 $ Cons 1 Empty)) `shouldBe` (Cons 1 $ Cons 2 Empty)
  describe "Monad" $ do
    it "" $
      ((Cons (+1) Empty) =>> (Cons 0 $ Cons 1 Empty)) `shouldBe` (Cons 1 $ Cons 2 Empty)
