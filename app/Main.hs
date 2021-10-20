{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

-- import System.Process.Typed
import System.Directory (copyFileWithMetadata, createDirectoryIfMissing)
import Options.Applicative as Opt
import Control.Monad
import System.Directory.Tree
import System.IO
import qualified Data.List as DL
import Data.Functor (void)
import System.Process.Typed (readProcessStdout, shell)
import System.Exit (ExitCode(..))
import System.FilePath.Posix (takeFileName, (</>))
import AwsLambdaPackager

type ExecutableFile = FilePath
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




----
---
----
getLddOutput :: FilePath
             -> IO [LddLine]
getLddOutput fname = do
  (ex, o) <- readProcessStdout (shell cmd)
  case ex of
    ExitFailure e -> error $ "ldd (commmand, error): ("  <> cmd <> ", " <> show e <> ")"
    ExitSuccess -> case parseLddOutput o of
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

copyLibraries :: FilePath
              -> [FilePath]
              -> IO ()
copyLibraries d fs = do
  createDirectoryIfMissing True d
  forM_ fs $ \f -> copyFileWithMetadata f (d </> takeFileName f)

printList :: [String] -> IO ()
printList xs = forM_ xs putStrLn

main :: IO ()
main = (execParser cliArgsParserOpts) >>= \case
  CmdShowDefaults ->
    printList defaultLibDirs
  CmdSaveLibraryList listArgs ->
    saveLibraryList $ applyDefaults listArgs
  CmdPrintCustomLibraries listFile exeFile ->
    (getLddOutput exeFile) >>= (missingLibraryList listFile) >>= printList
  CmdCopyCustomLibraries listFile exeFile outputDir ->
    (getLddOutput exeFile) >>= (missingLibraryList listFile) >>= (copyLibraries outputDir)

