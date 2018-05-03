{-# OPTIONS -Wall #-}


module Demo01 where

import FactX.FactOutput

demo01 = runFactOutput "facts.pl" proc
  where
    proc = do { tellCommentString "facts.pl"
              ; return () }

