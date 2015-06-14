module Models where

import Prelude
import Data.Monoid

type Degree = Number

data Point = Point { x :: Number
                   , y :: Number }

instance pointSemigroup :: Semigroup Point where
  (<>) (Point {x = a, y = b}) (Point {x = c, y = d}) = Point { x: (a + c), y: (b + d) }

instance pointMonoid :: Monoid Point where
  mempty = Point {x: 0, y: 0}

data Ship = Ship { name :: String
                 , direction :: Degree
                 , speed :: Point
                 , position :: Point }
