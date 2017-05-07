module Lib
    ( MyReaderT (..)
    ) where

newtype MyReaderT r m a = MyReaderT { runMyReaderT :: r -> m a }
