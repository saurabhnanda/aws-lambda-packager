{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

-- import System.Process.Typed
import System.Directory (copyFile)
import System.Posix.Files (isDirectory)
import Options.Applicative as Opt
import Control.Monad
import System.Directory.Tree
import System.IO
import qualified Data.List as DL
import Text.Parsec as Parsec hiding ((<|>))
import Text.Parsec.Char
import Data.Functor (void)
import System.Process.Typed (readProcessStdout, shell)
import System.Exit (ExitCode(..))

type ExecutableFile = FilePath
type SavedListFile = FilePath
type OutputDir = FilePath

data CliCommand = CmdShowDefaults
                | CmdSaveLibraryList ListArgs
                | CmdPrintCustomLibraries SavedListFile ExecutableFile
                | CmdCopyCustomLibraries SavedListFile ExecutableFile OutputDir
                deriving (Eq, Show)

data ListArgs = ListArgs
  { argLibDirs :: [FilePath]
  , argIgnoreDefaultDirs :: Bool
  , argOutputFile :: FilePath
  } deriving (Eq, Show)


saveLibraryListParser :: Parser CliCommand
saveLibraryListParser = CmdSaveLibraryList <$> listArgsParser

listArgsParser :: Parser ListArgs
listArgsParser = ListArgs
  <$> (Opt.many libDirsParser)
  <*> ignoreDefaultDirsParser
  <*> outputFileParser
  where
    libDirsParser = strOption $
      long "lib-dir" <>
      short 'd' <>
      help "library directory to be considered. All subdirectories within this directory will be RECURSIVELY considered, automatically"
    outputFileParser = strOption $
      long "output-file" <>
      short 'o' <>
      help "TODO complete this"
    ignoreDefaultDirsParser = flag False True $
      long "ignore-default-dirs" <>
      help "TODO document this"

printLibrariesParser :: Parser CliCommand
printLibrariesParser = CmdPrintCustomLibraries
  <$> ( strOption $
        long "list" <>
        short 'l' <>
        help "TODO - complete this"
      )
  <*> ( strOption $
        long "file" <>
        short 'f' <>
        help "TODO - complete this"
      )

copyLibariesParser :: Parser CliCommand
copyLibariesParser = CmdCopyCustomLibraries
  <$> ( strOption $
        long "list" <>
        short 'l' <>
        help "TODO - complete this"
      )
  <*> ( strOption $
        long "file" <>
        short 'f' <>
        help "TODO - complete this"
      )
  <*> ( strOption $
        long "output-dir" <>
        short 'o' <>
        help "TODO - complete this"
      )

cliCommandParser :: Parser CliCommand
cliCommandParser =
  hsubparser (command "save-library-list" $ info saveLibraryListParser $ progDesc "TODO - complete this")
  <|> hsubparser (command "show-defaults" $ info (pure CmdShowDefaults) (progDesc "TODO - complete this"))
  <|> hsubparser (command "print-custom-libraries" $ info printLibrariesParser (progDesc "TODO - complete this"))
  <|> hsubparser (command "copy-custom-libraries" $ info copyLibariesParser (progDesc "TODO - complete this"))

cliArgsParserOpts :: ParserInfo CliCommand
cliArgsParserOpts = info
  (cliCommandParser <**> helper)
  (fullDesc <> progDesc "TODO complete this")

defaultLibDirs :: [FilePath]
defaultLibDirs =
  [ "/lib64"
  , "/usr/lib64"
  , "/opt/lib"
  ]

applyDefaults :: ListArgs -> ListArgs
applyDefaults x@ListArgs{..} = case argIgnoreDefaultDirs of
  True -> x
  False -> x{argLibDirs=argLibDirs ++ defaultLibDirs}

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


----
---
----
getLddOutput :: FilePath
             -> IO [LddLine]
getLddOutput fname = do
  (ex, o) <- readProcessStdout (shell cmd)
  case ex of
    ExitFailure e -> error $ "ldd (commmand, error): ("  <> cmd <> ", " <> show e <> ")"
    ExitSuccess -> case parse lddOutputParser "" o of
      Left e -> error $ show e
      Right r -> pure r
  where
    cmd = "ldd " <> fname

saveLibraryList :: ListArgs
                -> IO ()
saveLibraryList ListArgs{..} =
  withFile argOutputFile WriteMode $ \outFile -> do
    void $ forM argLibDirs $ readDirectoryWith $ \fname -> do
      hPutStrLn outFile (fname `seq` fname)

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

copyLibraries :: FilePath
              -> [FilePath]
              -> IO ()
copyLibraries d fs =
  forM_ fs $ \f -> copyFile f d

printList :: (Show a) => [a] -> IO ()
printList xs = forM_ xs (putStrLn . show)

main :: IO ()
main = (execParser cliArgsParserOpts) >>= \case
  CmdShowDefaults ->
    printList defaultLibDirs
  CmdSaveLibraryList listArgs ->
    saveLibraryList $ applyDefaults listArgs
  CmdPrintCustomLibraries listFile exeFile ->
    (getLddOutput listFile) >>= (missingLibraryList listFile) >>= printList
  CmdCopyCustomLibraries listFile exeFile outputDir ->
    (getLddOutput listFile) >>= (missingLibraryList listFile) >>= (copyLibraries outputDir)
