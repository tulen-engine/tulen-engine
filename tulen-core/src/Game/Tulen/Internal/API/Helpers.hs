module Game.Tulen.Internal.API.Helpers(
    ToAPI(..)
  , FromAPI(..)
  ) where

-- | Convertor to API type
class ToAPI a b where
  toAPI :: a -> b

-- | Convertor from API type
class FromAPI a b where
  fromAPI :: b -> a
