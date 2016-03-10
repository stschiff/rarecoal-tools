{-# LANGUAGE OverloadedStrings #-}

import Rarecoal.FreqSum (FreqSumEntry(..), FreqSumHeader(..))

import Control.Applicative ((<|>))
import Control.Error (runScript, scriptIO, Script, throwE)
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (runStateT)
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Options.Applicative as OP
import Pipes (next, Producer)
import Pipes.Attoparsec (parsed, parse)
import Pipes.Prelude (foldM')
import qualified Pipes.Text.IO as PT
import System.IO (Handle, stdin)

data VCFheader = VCFheader [T.Text] [String] -- simple comment lines, sample names
data VCFentry = VCFentry T.Text Int T.Text T.Text [(Char,Char)]
                deriving (Show)

main :: IO ()
main = OP.execParser opts >> run
  where
    opts = OP.info (OP.helper <*> pure ()) (OP.progDesc "convert a multi-sample VCF, read from \
                                                         \stdin, into a freqSum file")

run :: IO () 
run = runScript $ do
    (VCFheader _ names, vcfProd) <- parseVCF stdin
    let fsHeader = FreqSumHeader names (replicate (length names) 2)
    scriptIO . putStrLn . show $ fsHeader
    _ <- foldM' processVCFentry (return 0) (const (return ())) vcfProd
    return ()

parseVCF :: Handle -> Script (VCFheader, Producer VCFentry Script ())
parseVCF handle = do
    let prod = PT.fromHandle handle
    (res, rest) <- runStateT (parse parseVCFheader) prod
    header <- case res of
        Nothing -> throwE "vcf file exhausted"
        Just (Left e) -> throwE ("vcf file parsing error: " ++ show e)
        Just (Right h) -> return h
    return (header, parsed parseVCFentry rest >>= liftErrors)
  where
    liftErrors res = case res of
        Left (e, prod) -> do
            Right (chunk, _) <- lift $ next prod
            let msg = show e ++ T.unpack chunk
            lift . throwE $ msg
        Right () -> return ()

parseVCFheader :: A.Parser VCFheader
parseVCFheader = VCFheader <$> A.many' doubleCommentLine <*> singleCommentLine
  where
    doubleCommentLine = do
        c1 <- A.string "##"
        s <- A.takeTill A.isEndOfLine <* A.endOfLine
        return $ T.append c1 s
    singleCommentLine = do
        void $ A.char '#'
        s <- A.takeTill A.isEndOfLine <* A.endOfLine
        let fields = T.splitOn "\t" s
        return . drop 9 . map T.unpack $ fields

parseVCFentry :: A.Parser VCFentry
parseVCFentry = do
    chrom <- word
    void tab
    pos <- A.decimal
    void tab
    void word
    void tab
    ref <- word
    void tab
    alt <- word
    void tab
    void $ A.count 4 (word >> tab)
    genotypes <- genotype `A.sepBy1` tab
    void A.endOfLine
    return $ VCFentry chrom pos ref alt genotypes
  where
    word = A.takeTill (\a -> a `elem` ['\r', '\t', '\n', ' '])
    tab = A.char '\t'

genotype :: A.Parser (Char, Char)
genotype = do
    gen1 <- (A.char '0' <|> A.char '1' <|> A.char '.')
    void (A.char '/' <|> A.char '|')
    gen2 <- (A.char '0' <|> A.char '1' <|> A.char '.')
    _ <- A.takeTill (\a -> (a `elem` ['\r', '\t', '\n', ' ']))
    return (gen1, gen2)

processVCFentry :: Int -> VCFentry -> Script Int
processVCFentry lastPos (VCFentry chrom pos ref alt genotypes) = do
    if lastPos == pos || T.length ref > 1 || T.length alt > 1 then
        return pos
    else do
        let gens = [[g1, g2] | (g1, g2) <- genotypes]
        let counts = [if any (=='.') c then -1 else length $ filter (=='1') c | c <- gens]
            fs = FreqSumEntry (T.unpack chrom) pos (T.head ref) (T.head alt) counts
        scriptIO $ print fs
        return pos

