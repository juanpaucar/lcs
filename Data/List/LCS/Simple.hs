
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.LCS.Simple
-- Copyright   :  (c) Ian Lynagh 2005
-- License     :  BSD or GPL v2
-- 
-- Maintainer  :  igloo@earth.li
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a simple, stupid and (most of all) slow implementation of the
-- Data.List.LCS interface.
-----------------------------------------------------------------------------

module Data.List.LCS.Simple (lcs) where

lcs :: Ord a => [a] -> [a] -> [a]
lcs xs ys = snd $ lcs' xs ys

lcs' :: Ord a => [a] -> [a] -> (Int, [a])
lcs' (x:xs) (y:ys)
 | x == y = case lcs' xs ys of
                (len, zs) -> (len + 1, x:zs)
 | otherwise = let r1@(l1, _) = lcs' (x:xs) ys
                   r2@(l2, _) = lcs' xs (y:ys)
               in if l1 >= l2 then r1 else r2
lcs' [] _ = (0, [])
lcs' _ [] = (0, [])

