{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  FactX.FactOutput
-- Copyright   :  (c) Stephen Tetley 2018
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- 
--
--------------------------------------------------------------------------------


module FactX.FactOutput 
  (
    FactOutput
  , runFactOutput
  , tellComment
  , tellCommentString
  ) where

import Data.Text.Lazy.Builder
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text

import Data.Semigroup


newtype FactOutput a = FactOutput { 
        getFactOutput :: Builder -> (Builder,a) }

instance Functor FactOutput where
  fmap f ma = FactOutput $ \s -> 
      let (s1,a) = getFactOutput ma s in (s1, f a)

instance Applicative FactOutput where
  pure a = FactOutput $ \s -> (s,a)
  mf <*> ma = FactOutput $ \s -> 
      let (s1,f) = getFactOutput mf s
          (s2,a) = getFactOutput ma s1
      in (s2, f a)
                    

instance Monad FactOutput where
  return = pure
  ma >>= k = FactOutput $ \s -> 
      let (s1,a) = getFactOutput ma s in getFactOutput (k a) s1


instance Semigroup a => Semigroup (FactOutput a) where
  ma <> mb = FactOutput $ \s ->
      let (s1,a) = getFactOutput ma s
          (s2,b) = getFactOutput mb s1
      in (s2, a <> b)
          

instance (Semigroup a, Monoid a) => Monoid (FactOutput a) where
  mempty = FactOutput $ \s -> (s,mempty)
  mappend = (<>)

runFactOutput :: FilePath -> FactOutput a -> IO a
runFactOutput path ma = 
   do { let (s,a) = getFactOutput ma mempty
      ; Text.writeFile path $ toLazyText s
      ; return a }


tellLine :: Text.Text -> FactOutput () 
tellLine txt = FactOutput $ \s -> (s <> fromLazyText txt <> singleton '\n', ())

tellLineString :: String -> FactOutput () 
tellLineString str = FactOutput $ \s -> (s <> fromString str <> singleton '\n', ())

tellComment :: Text.Text -> FactOutput ()
tellComment txt = mapM_ (tellLine . mkcomment) $ Text.lines txt
  where
    mkcomment s = Text.singleton '%' <> Text.singleton ' ' <> s

tellCommentString :: String -> FactOutput ()
tellCommentString str = mapM_ (tellLineString . mkcomment) $ lines str
  where
    mkcomment s = '%':' ':s
