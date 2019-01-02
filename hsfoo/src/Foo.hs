{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

module Foo ( foo ) where

import Control.Monad
import Foreign.C.Types
import qualified Language.C.Inline.Unsafe as CU
import qualified Language.C.Inline.Cpp as C

C.context C.cppCtx
C.include "foo.hpp"
C.include "exception"

foo :: IO ()
foo = do
    r <- [CU.block| int
      {
        try
        {
          foo();
          return 0;
        }
        catch (const std::exception & e)
        {
          return 1;
        }
      }
    |]
    unless (r == (0::CInt)) $
      error "Whoops!"
