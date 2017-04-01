{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Brick
import Data.Maybe
import Graphics.Vty (Key(..), Event(..), defAttr)
import Options.Generic
import System.Environment
import System.IO
import Magnifique hiding (Other)
import Magnifique.App.Common
import qualified Data.ByteString.Lazy as BS
import qualified Magnifique.Aeson as Aeson
import qualified Magnifique.App.Aeson as Aeson

data Type = Aeson | Other
  deriving (Generic, Read)

instance ParseField Type

data Args = Args
  { _file :: String <?> "Input file"
  , _type :: Maybe Type <?> "Input type"
  } deriving Generic

instance ParseRecord Args where
  parseRecord = parseRecordWithModifiers defaultModifiers{fieldNameModifier=drop 1}

main = do
  args <- getRecord "magnifique"
  case fromMaybe Aeson (unHelpful (_type args)) of
    Aeson -> do
      str <- BS.readFile (unHelpful (_file args))
      case Aeson.eitherDecode str of
        Left e -> print e
        Right v -> do
          let s = Aeson._unzip v
          defaultMain Aeson.magnifiqueApp s
          return ()
    Other -> do
      str <- readFile (unHelpful (_file args))
      case expressionParse str of
        Left e -> print e
        Right es -> do
          let s = _unzip (Parens (CommaSeparated [es]))
          defaultMain magnifiqueApp s
          return ()

magnifiqueApp :: App Zipper () String
magnifiqueApp = App
  { appDraw = \s ->
      [txt (exprToText (cozip s))]
  , appChooseCursor = \_ _ -> Nothing
  , appHandleEvent = \s e -> case e of
      VtyEvent e -> case e of
        EvKey KUp [] -> continue' (moveUp s)
        EvKey KDown [] -> continue' (moveDown s)
        EvKey KLeft [] -> continue' (moveLeft s)
        EvKey KRight [] -> continue' (moveRight s)
        EvKey (KChar 'q') [] -> halt s
       where continue' Nothing = continue s
             continue' (Just s) = continue s
      _ -> continue s
  , appStartEvent = return
  , appAttrMap = magnifiqueAttrMap
  }
