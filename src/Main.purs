module Main where

import Debug.Trace
import Models

p1 = Point {x: 10, y: 0}
p2 = Point {x: 0, y: 20}

s1 = Ship {name: "testShip", direction: 0, speed: p1, position: p2}

main = do
  trace $ show $ p1 <> p2
  trace $ show $ accelShip 10 $ rotateShip 90 $ s1
