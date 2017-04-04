{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Brick
import Data.Maybe
import Data.Proxy
import Graphics.Vty (Key(..), Event(..), defAttr)
import Options.Generic
import System.Environment
import System.IO
import Magnifique hiding (Other)
import Magnifique.App
import qualified Data.ByteString.Lazy as BS
import qualified Magnifique.Aeson as Aeson
import qualified Magnifique.Aeson.App as Aeson
import qualified Magnifique.Typeable as Generic

data Type = Aeson | GenericTest | Other
  deriving (Generic, Read)

instance ParseField Type

data Args = Args
  { _file :: Maybe String <?> "Input file"
  , _type :: Maybe Type <?> "Input type"
  } deriving Generic

instance ParseRecord Args where
  parseRecord = parseRecordWithModifiers defaultModifiers{fieldNameModifier=drop 1}

data Tree = N Tree Tree | L
  deriving Generic

fib :: Int -> Tree
fib n | n < 1 = L
fib n = N (fib (n - 1)) (fib (n - 2))

data TreeX

type instance Generic.CxOf TreeX Tree = Generic.GenericCo TreeX Tree

main = do
  args <- getRecord "magnifique"
  case fromMaybe Aeson (unHelpful (_type args)) of
    GenericTest -> do
      let z = Generic._unzip (Proxy :: Proxy (Generic.CxOf TreeX Tree)) (fib 13)
      defaultMain Generic.magnifiqueApp z
      return ()
    Aeson | Just file <- unHelpful (_file args) -> do
      str <- BS.readFile file
      case Aeson.eitherDecode str of
        Left e -> print e
        Right v -> do
          let s = Aeson._unzip v
          defaultMain Aeson.magnifiqueApp s
          return ()
    Other | Just file <- unHelpful (_file args) -> do
      str <- readFile file
      case expressionParse str of
        Left e -> print e
        Right es -> do
          let s = _unzip (Parens (CommaSeparated [es]))
          defaultMain magnifiqueApp s
          return ()
