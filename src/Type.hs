{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Type where

class (Show t) =>
      SimpleType i t e
  where
  mk :: i -> Either e t

class (SimpleType i t e, Eq t) =>
      ValueObject i t e


class (SimpleType i t e, ValueObject i' t' e') =>
      Entity i t e i' t' e'
  where
  getID :: t -> t'
  getKey :: t -> String
  getKey = show

type Aggregate i t e = SimpleType i t e

class DTO a where
  encode :: a -> String
  decode :: String -> a

class (DTO d, SimpleType i t e) =>
      Event d i t e
  where
  toDTO :: t -> d
  fromDTO :: d -> t

class (SimpleType i t e) =>
      Command i t e a
  where
  slot :: t
  run :: IO a

class (Event d i t e, Command i' t' e' a, Traversable ts) =>
      Workflow d i t e i' t' e' a ts


data BoundedContext
  = CoreDomain
  | SubDomain
  | SupportDomain
  deriving (Show)

data Context
  = Context [Context]
  | ACL BoundedContext
        BoundedContext
  | ToUp BoundedContext
         BoundedContext
  | ToDown BoundedContext
           BoundedContext
  deriving (Show)

class (Show a) =>
      DomainError a
