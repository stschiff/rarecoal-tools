{-# LANGUAGE OverloadedStrings #-}

module Rarecoal.FreqSum (parseFreqSum, FreqSumEntry(..), FreqSumHeader(..), printFreqSum) where

import Control.Error (Script, throwE)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Strict (runStateT)
import Control.Monad.Trans.Class (lift)
import qualified Data.Attoparsec.Text as A
import Data.Char (isAlphaNum)
import Data.List (intercalate)
import Data.Text (unpack)
import Pipes (Producer, next, runEffect, (>->))
import Pipes.Attoparsec (parse, parsed)
import qualified Pipes.Prelude as P
import qualified Pipes.Text.IO as PT
import System.IO (Handle)

data FreqSumHeader = FreqSumHeader {
    fshNames :: [String],
    fshCounts :: [Int]
} deriving (Eq)

instance Show FreqSumHeader where
    show (FreqSumHeader names nCounts) = "#CHROM\tPOS\tREF\tALT\t" ++ intercalate "\t" tuples
      where
        tuples = zipWith (\n c -> n ++ "(" ++ show c ++ ")") names nCounts

data FreqSumEntry = FreqSumEntry {
    fsChrom  :: String,
    fsPos    :: Int,
    fsRef    :: Char,
    fsAlt    :: Char,
    fsCounts :: [Int]
}

instance Show FreqSumEntry where
    show (FreqSumEntry chrom pos ref alt counts) =
        intercalate "\t" [chrom, show pos, [ref], [alt], intercalate "\t" . map show $ counts]

parseFreqSum :: Handle -> Script (FreqSumHeader, Producer FreqSumEntry Script ())
parseFreqSum handle = do
    let prod = PT.fromHandle handle
    (res, rest) <- runStateT (parse parseFreqSumHeader) prod
    header <- case res of
        Nothing -> throwE "freqSum file exhausted"
        Just (Left e) -> throwE ("freqSum file parsing error: " ++ show e)
        Just (Right h) -> return h
    return (header, parsed parseFreqSumEntry rest >>= liftErrors)
  where
    liftErrors res = case res of
        Left (e, prod) -> do
            Right (chunk, _) <- lift $ next prod
            let msg = show e ++ unpack chunk
            lift . throwE $ msg
        Right () -> return ()

parseFreqSumHeader :: A.Parser FreqSumHeader
parseFreqSumHeader = do
    tuples <- A.string "#CHROM\tPOS\tREF\tALT\t" >> A.sepBy' tuple A.space <* A.endOfLine
    let names = map (unpack . fst) tuples
        counts = map snd tuples
    return $ FreqSumHeader names counts
  where
    tuple = (,) <$> A.takeWhile (\c -> isAlphaNum c || c == '_') <* A.char '(' <*> A.decimal <* A.char ')'

parseFreqSumEntry :: A.Parser FreqSumEntry
parseFreqSumEntry = FreqSumEntry <$> word <* A.skipSpace <*> A.decimal <* A.skipSpace <*>
                                     A.letter <* A.skipSpace <*> A.letter <* A.skipSpace <*>
                                     counts <* A.endOfLine
  where
    word = unpack <$> A.takeWhile1 (A.notInClass "\n\t\r")
    counts = (A.signed A.decimal) `A.sepBy` A.char '\t'

printFreqSum :: MonadIO io => (FreqSumHeader, Producer FreqSumEntry io ()) -> io ()
printFreqSum (fsh, prod) = do
    liftIO . print $ fsh
    runEffect $ prod >-> P.map show >-> P.stdoutLn
