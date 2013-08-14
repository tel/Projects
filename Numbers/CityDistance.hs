
module Main where

-- We'll pretend like we have Google-Geocode installed
-- (https://github.com/mrd/geocode-google)


-- import Geography.Geocoding.Google (geoEncode)

import Control.Applicative
import Control.Arrow

data GeocodeError = NotFound deriving (Show)

type Loc = (Double, Double)

geoEncode :: String -> IO (Either GeocodeError Loc)
geoEncode = undefined

dist :: Loc -> Loc -> Double
dist loc1 loc2 =
  haversine (la2 - la1) + (cos la1) * (cos la2) * haversine (lo2 - lo1)
  where haversine th = let x = sin (th / 2) in x*x
        (la1, la2) = (d2r *** d2r) loc1
        (lo1, lo2) = (d2r *** d2r) loc2
        d2r = (* (pi/180))

main :: IO ()
main = do
  putStrLn "Enter first city"
  c1 <- geoEncode =<< getLine
  putStrLn "Enter second city"
  c2 <- geoEncode =<< getLine

  case dist <$> c1 <*> c2 of
    Left ge -> putStrLn "Unable to locate the citites. Check the city names."
    Right d -> do
      putStr "Distance is "
      print d
