module Read where

import qualified Text.Read as R
import qualified Text.ParserCombinators.ReadP as RP
import qualified Text.ParserCombinators.ReadPrec as RPrec

type ReadP = RP.ReadP
type ReadPrec = RPrec.ReadPrec

readPrecChar :: ReadPrec Char
readPrecChar = RPrec.get

readPString :: ReadP String
readPString = RP.munch $ const True

-- read a specific string
readPString1 :: String -> ReadP String
readPString1 = RP.string

readPrecString :: ReadPrec String
readPrecString = RPrec.lift readPString

-- read a specific string
readPrecString1 :: String -> ReadPrec String
readPrecString1 = RPrec.lift . readPString1

-- tuples
read2 :: (Read a, Read b) => ReadPrec (a, b)
read2 = do
  a <- R.readPrec; b <- R.readPrec;
  return (a, b)

readPrecs2 :: ReadPrec a -> ReadPrec b -> ReadPrec (a, b)
readPrecs2 a b = do
  a <- a; b <- b;
  return (a, b)

readPrecs3 :: ReadPrec a -> ReadPrec b -> ReadPrec c -> ReadPrec (a, b, c)
readPrecs3 a b c = do
  a <- a; b <- b; c <- c;
  return (a, b, c)

readPrecs4 :: ReadPrec a -> ReadPrec b -> ReadPrec c -> ReadPrec d
  -> ReadPrec (a, b, c, d)
readPrecs4 a b c d = do
  a <- a; b <- b; c <- c; d <- d;
  return (a, b, c, d)
