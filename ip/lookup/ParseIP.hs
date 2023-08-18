{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module ParseIP where

import Control.Applicative
import Control.Monad
import Data.Bits (shiftL, toIntegralSized)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.Word
import IPTypes
import Text.Read

guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded f a = if f a then pure a else empty

-- | Checks if the list has the given length
--
-- >>> 4 `isLengthOf` [1,2,3,4]
-- True
-- >>> 0 `isLengthOf` []
-- True
-- >>> 0 `isLengthOf` [1,2,3,4]
-- False
isLengthOf :: Int -> [a] -> Bool
isLengthOf n xs = length xs == n

buildIP :: [Word8] -> IP
buildIP = buildIP_foldr

buildIP_foldr :: [Word8] -> IP
buildIP_foldr = IP . fst . foldr go (0, 1)
  where
    go b (s, k) = (s + fromIntegral b * k, k * 256)

buildIP_foldl :: [Word8] -> IP
buildIP_foldl = IP . foldl (\s b -> s * 256 + fromIntegral b) 0

buildIP_foldl_shl :: [Word8] -> IP
buildIP_foldl_shl = IP . foldl (\s b -> shiftL s 8 + fromIntegral b) 0

-- | Parses the IP address given as a 'String'
--
-- >>> parseIP "0.0.0.0"
-- Just 0.0.0.0
--
-- >>> parseIP "192.168.3.15"
-- Just 192.168.3.15
--
-- >>> parseIP "not an IP address"
-- Nothing
parseIP :: String -> Maybe IP
parseIP =
  guarded (4 `isLengthOf`) . splitOn "."
    >=> mapM (readMaybe @Integer >=> toIntegralSized)
    >=> pure . buildIP

parseIPRange :: String -> Maybe IPRange
parseIPRange =
  guarded (2 `isLengthOf`) . splitOn ","
    >=> mapM parseIP
    >=> listToIPRange
  where
    listToIPRange [a, b]
      | a <= b = pure (IPRange a b)
    listToIPRange _ = empty

parseIPRanges :: String -> Either ParseError IPRangeDB
parseIPRanges = fmap IPRangeDB . mapM parseLine . zip [1 ..] . lines
  where
    parseLine (ln, s) = case parseIPRange s of
      Nothing -> Left (ParseError ln)
      Just ipr -> Right ipr

parseValidIPs :: String -> [IP]
parseValidIPs = catMaybes . map parseIP . lines

parseValidIPRanges :: String -> IPRangeDB
parseValidIPRanges = IPRangeDB . catMaybes . map parseIPRange . lines
