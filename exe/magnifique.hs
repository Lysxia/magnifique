import Brick
import Graphics.Vty (Key(..), Event(..), defAttr)
import System.Environment
import System.IO
import Magnifique

main = do
  file : _ <- getArgs
  str <- readFile file
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

magnifiqueAttrMap _ = attrMap defAttr []
