module Models where

import Prelude
import Data.Monoid
import Math

import DDD

type Degree = Number

data Point = Point { x :: Number
                   , y :: Number }

instance pointShow :: Show Point where
  show (Point {x=x, y=y}) = "{x: " ++ show x ++ ", y: " ++ show y ++ "}"

instance pointSemigroup :: Semigroup Point where
  (<>) (Point {x = a, y = b}) (Point {x = c, y = d}) = Point { x: (a + c), y: (b + d) }

instance pointMonoid :: Monoid Point where
  mempty = Point {x: 0, y: 0}

data Ship = Ship { name :: String
                 , direction :: Degree
                 , speed :: Point
                 , position :: Point }

instance entityShip :: Entity Ship String where
  identify (Ship { name = name }) = name

rotateShip :: Degree -> Ship -> Ship
rotateShip n (Ship s@{direction = dir}) = Ship $ s { direction = (n + dir) % 360 }

accelShip :: Number -> Ship -> Ship
accelShip n (Ship s@{direction = dir, speed = speed}) = Ship $ s { speed = incr <> speed }
                where incr = Point { x: n * (cos dir), y: n * (sin dir)}
