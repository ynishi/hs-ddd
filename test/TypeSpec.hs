{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module TypeSpec
  ( spec
  ) where

import           Test.Hspec
import           Type

newtype OrderId =
  OrderId String
  deriving (Show, Eq)

newtype OrderIdError =
  OrderIdError String
  deriving (Show, Eq)

instance DomainError OrderIdError

instance SimpleType String OrderId OrderIdError where
  mk :: String -> Either OrderIdError OrderId
  mk s
    | length (words s) == 0 = Left (OrderIdError "must not empty")
    | length s > 12 = Left (OrderIdError "max length is 12")
    | otherwise = Right (OrderId s)

data OrderQuantity =
  OrderQuantity Int
  deriving (Show, Eq)

data OrderQuantityError =
  OrderQuantityError String
  deriving (Show)

instance DomainError OrderQuantityError

instance SimpleType Int OrderQuantity OrderQuantityError where
  mk :: Int -> Either OrderQuantityError OrderQuantity
  mk n
    | n <= 0 = Left (OrderQuantityError "must over 0")
    | n > 1000 = Left (OrderQuantityError "max size is 1000")
    | otherwise = Right (OrderQuantity n)

instance ValueObject Int OrderQuantity OrderQuantityError

spec :: Spec
spec = do
  describe "SimpleType" $ do
    it "create success when valid input" $ do
      let x = mk "a1" :: Either OrderIdError OrderId
      x `shouldBe` Right (OrderId "a1")
    it "create fail when empty string" $ do
      let x = mk "" :: Either OrderIdError OrderId
      x `shouldBe` Left (OrderIdError "must not empty")
    it "create fail when space only string" $ do
      let x = mk "  " :: Either OrderIdError OrderId
      x `shouldBe` Left (OrderIdError "must not empty")
    it "create fail when empty string" $ do
      let x = mk (replicate 13 'a') :: Either OrderIdError OrderId
      x `shouldBe` Left (OrderIdError "max length is 12")
