module Magnifique.App.Common
  ( magnifiqueAttrMap

  -- * Brick
  , txt
  , continue
  , halt
  , BrickEvent(..)

  -- * VTY
  , App(..)
  , Key(..)
  , Event(..)
  ) where

import Brick
import Graphics.Vty (Key(..), Event(..), defAttr)

magnifiqueAttrMap :: AttrMap
magnifiqueAttrMap = attrMap defAttr []
