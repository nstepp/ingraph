{-
	InGraph - Ingress link optimizer

    Copyright (C) 2013  Nigel D. Stepp

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

    Nigel Stepp <stepp@atistar.net>
	http://www.atistar.net/~stepp/contactxfer/

    $Id: DataDict.hs 844 2013-06-28 22:55:53Z stepp $
-}

{-# LANGUAGE TypeFamilies #-}

-- |Data Dictionary module.
-- The data dictionary allows widgets and controls
-- to be collected in a single place.
module DataDict where

import Data.Maybe
import qualified Data.Map as Map
import Graphics.UI.WX
import Graphics.UI.WXCore

-- * Types

data WidgetConfig = WidgetConfig {
    cfgButtons :: Map.Map ButtonLabel (Button ()),
    cfgText :: Map.Map TextLabel (StaticText ()),
    cfgSliders :: Map.Map SliderLabel (Slider ()),
    cfgSpinners :: Map.Map SpinLabel (SpinCtrl ())
    }

class ConfigItem a where
    type Value a :: *
    getCfg :: WidgetConfig -> a -> Value a

instance ConfigItem ButtonLabel where
    type Value ButtonLabel = Button ()
    getCfg cfg key = fromJust $ Map.lookup key $ cfgButtons cfg

instance ConfigItem TextLabel where
    type Value TextLabel = StaticText ()
    getCfg cfg key = fromJust $ Map.lookup key $ cfgText cfg

instance ConfigItem SliderLabel where
    type Value SliderLabel = Slider ()
    getCfg cfg key = fromJust $ Map.lookup key $ cfgSliders cfg

instance ConfigItem SpinLabel where
    type Value SpinLabel = SpinCtrl ()
    getCfg cfg key = fromJust $ Map.lookup key $ cfgSpinners cfg

-- |Available buttons
data ButtonLabel = OptButton
            | ResetButton deriving (Eq,Show,Ord)
-- |Available texts
data TextLabel = EAPText
          | FAPText
          | RatioText
          | LinkText
          | FieldText deriving (Eq,Show,Ord)
-- |Available controls
data SliderLabel = GainSlider deriving (Eq,Show,Ord)
-- |Available spin controls
data SpinLabel = IterationSpin deriving (Eq,Show,Ord)

