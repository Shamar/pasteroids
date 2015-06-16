module Models where

import Prelude (Show, show, Semigroup, (<>), ($), (++), (+), (%), (*), (/))
import Data.Monoid (Monoid, mempty)
import Math (sin, cos, pi)

import DDD

type Degree = Number

data Point = Point { x :: Number
                   , y :: Number }

instance pointShow :: Show Point where
  show (Point p) = "{x: " ++ show p.x ++ ", y: " ++ show p.y ++ "}"

instance pointSemigroup :: Semigroup Point where
  (<>) (Point {x = a, y = b}) (Point {x = c, y = d}) = Point { x: (a + c), y: (b + d) }

instance pointMonoid :: Monoid Point where
  mempty = Point {x: 0, y: 0}

data Ship = Ship { name :: String
                 , direction :: Degree
                 , speed :: Point
                 , position :: Point }

instance shipEntity :: Entity Ship String where
  identify (Ship s) = s.name

instance shipShow :: Show Ship where
  show (Ship s)= s.name ++ " {direction: " ++ show s.direction ++ ", speed: " ++ show s.speed ++ ", position: "++ show s.position ++" }"

rotateShip :: Degree -> Ship -> Ship
rotateShip n (Ship s) = Ship $ s { direction = (n + s.direction) % 360 }

accelShip :: Number -> Ship -> Ship
accelShip n (Ship s) = Ship $ s { speed = incr <> s.speed }
                where incr = Point { x: n * dirCos, y: n * dirSin}
                      dirSin = sin $ pi * (s.direction / 180)
                      dirCos = cos $ pi * (s.direction / 180)
