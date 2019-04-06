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

-- |Data Dictionary module.
-- The data dictionary allows widgets and controls
-- to be collected in a single place.
module DataDict where

import Data.Maybe

-- * Types

-- |Available buttons
data Button = OptButton
            | ResetButton deriving (Eq,Show)
-- |Available texts
data Text = EAPText
          | FAPText
          | RatioText
          | LinkText
          | FieldText deriving (Eq,Show)
-- |Available controls
data Control = GainSlider deriving (Eq,Show)
-- |Available spin controls
data Spin = IterationSpin deriving (Eq,Show)

-- |A dictionary key
data DictKey = ButtonKey Button
             | TextKey Text
             | ControlKey Control
             | SpinKey Spin deriving (Eq,Show)

-- |An individual dictionary. Multiple dictionaries are
-- collected into the data dictionary.
data Dict a = Dict [(DictKey,a)] deriving (Show)

-- |The data dictionary itself is a collection of buttons, text,
-- and GUI controls.
data DataDict a b c d =
    DataDict { buttonDict :: Dict a,
               textDict :: Dict b,
               controlDict :: Dict c,
               spinDict :: Dict d}

-- * Functions

emptyDict = Dict []

-- |Look up an entry in an individual dictionary
lookupEntry :: DictKey -> Dict a -> Maybe a
lookupEntry key (Dict d) =
    lookup key d

-- |Get an entry from an individual dictionary. If the entry
-- doesn't exist, this will throw an exception.
getEntry :: DictKey -> Dict a -> a
getEntry key d =
    fromJust (lookupEntry key d)

-- |Add an entry to an individual dictionary. If the entry
-- already exists, nothing happens
addEntry :: DictKey -> a -> Dict a -> Dict a
addEntry key val (Dict d) =
    case (lookup key d) of
        Nothing -> Dict ((key,val):d)
        Just v -> Dict d


-- * Dictionary specific gets

-- |Get a button from the data dictionary
getButton :: Button -> DataDict a b c d -> a
getButton k dd = getEntry (ButtonKey k) (buttonDict dd)

-- |Get a text from the data dictionary
getText :: Text -> DataDict a b c d -> b
getText k dd = getEntry (TextKey k) (textDict dd)

-- |Get a control from the data dictionary
getControl :: Control -> DataDict a b c d -> c
getControl k dd = getEntry (ControlKey k) (controlDict dd)

-- |Get a spin control from the data dictionary
getSpin :: Spin -> DataDict a b c d -> d
getSpin k dd = getEntry (SpinKey k) (spinDict dd)


