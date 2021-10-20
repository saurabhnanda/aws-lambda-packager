{-# LANGUAGE FlexibleContexts #-}

module AwsLambdaPackager where

import Text.Parsec as Parsec hiding ((<|>))
import Text.Parsec.Char
import Data.Functor (void)
import qualified Data.List as DL
import qualified Data.ByteString.Lazy as BSL

type SavedListFile = FilePath
data LddLine = LddOnlyName String
             | LddOnlyPath String
             | LddNameAndPath String String
             deriving (Eq, Show)

hexChecksum :: (Stream s m Char) => ParsecT s u m String
hexChecksum = do
  string "(0x"
  checksum <- Parsec.many (satisfy (/= ')'))
  pure $ "(0x" <> checksum

lddNameAndPath :: (Stream s m Char) => ParsecT s u m LddLine
lddNameAndPath = do
  spaces
  libName <- Parsec.many (satisfy (/= ' '))
  spaces
  void $ string "=>"
  spaces
  libPath <- Parsec.many (satisfy (/= ' '))
  spaces
  void hexChecksum
  manyTill anyChar endOfLine
  pure $ LddNameAndPath libName libPath

lddOnlyName :: (Stream s m Char) => ParsecT s u m LddLine
lddOnlyName = do
  spaces
  libName <- Parsec.many (satisfy (/= ' '))
  spaces
  void $ string "=>"
  spaces
  void hexChecksum
  manyTill anyChar endOfLine
  pure $ LddOnlyName libName

lddOnlyPath :: (Stream s m Char) => ParsecT s u m LddLine
lddOnlyPath = do
  spaces
  libName <- Parsec.many (satisfy (/= ' '))
  spaces
  void hexChecksum
  manyTill anyChar endOfLine
  pure $ LddOnlyPath libName

lddOutputParser :: (Stream s m Char) => ParsecT s u m [LddLine]
lddOutputParser = Parsec.many $ choice [ try lddNameAndPath
                                       , try lddOnlyPath
                                       , try lddOnlyName
                                       ]

missingLibraryList :: SavedListFile
                   -> [LddLine]
                   -> IO [FilePath]
missingLibraryList listFile reqd = do
  preInstalled <- lines <$> (readFile listFile)
  pure $ (DL.\\) (DL.concatMap extractPath reqd) preInstalled
  where
    extractPath x = case x of
      LddOnlyName _ -> []
      LddOnlyPath _ -> []
      LddNameAndPath n p -> [p]

parseLddOutput :: BSL.ByteString
               -> Either ParseError [LddLine]
parseLddOutput = parse lddOutputParser ""
