{-# language ForeignFunctionInterface #-}

module Foo.Manual ( foo_manual ) where

import Control.Monad
import Foreign.C.Types

foo_manual :: IO ()
foo_manual = do
    r <- foo_wrapper
    unless (r == (0::CInt)) $
      error "Whoops!"

foreign import ccall foo_wrapper :: IO CInt