module Free.AlaCarte.Test where

import Data.Text qualified as T
import Free.AlaCarte
import Relude
import Test.Hspec

data FileSystem a
  = MyReadFile FilePath (Either UnicodeException Text -> a)
  | MyWriteFile FilePath Text a
  deriving (Functor)

instance Exec FileSystem where
  execAlgebra (MyReadFile path next) = do
    rawFileContents <- readFileBS path
    next (decodeUtf8' rawFileContents)
  execAlgebra (MyWriteFile path fileContents next) = do
    _ <- writeFileText path fileContents
    next

myReadFile :: (FileSystem :<: f) => FilePath -> Free f (Either UnicodeException Text)
myReadFile path = injectFree (MyReadFile path Pure)

myWriteFile :: (FileSystem :<: f) => FilePath -> Text -> Free f ()
myWriteFile path fileContents = injectFree (MyWriteFile path fileContents (Pure ()))

data Logger a = LogInfo Text a deriving (Functor)

instance Exec Logger where
  execAlgebra (LogInfo message next) = putTextLn message >> next

logInfo :: (Logger :<: f) => Text -> Free f ()
logInfo message = injectFree (LogInfo message (Pure ()))

freealacarteSpec :: SpecWith ()
freealacarteSpec = do
  describe "Free a la Carte" $ do
    describe "using the FileSystem free monad" $ do
      it "can read from file successfully " $ do
        fileContents <- exec @FileSystem $ myReadFile "./resources/test/file-0.txt"
        second T.strip fileContents `shouldBe` Right "Free a la Carte"
      it "can write to file successfully" $ do
        exec @FileSystem $ myWriteFile "./resources/test/some-test-file.txt" "Free a la Carte"
    describe "using the Logger free monad" $ do
      it "can log to stdout" $ do
        exec @Logger $ logInfo "Free a la Carte"
    describe "using FileSystem :+: Logger monads" $ do
      it "can read a file's contents to stdout" $ do
        exec @(FileSystem :+: Logger) $ readFileContentsToStdout "./resources/test/file-0.txt"

readFileContentsToStdout :: (Logger :<: f, FileSystem :<: f) => FilePath -> Free f ()
readFileContentsToStdout path = do
  maybeFileContents <- myReadFile path
  either (logInfo . T.pack . show) logInfo maybeFileContents
