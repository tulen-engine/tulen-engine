module Game.Tulen.Internal.Utils(
    guardJust
  , whenNothing
  ) where

  -- | Fail with helpfull message when encounter 'Nothing'
  guardJust :: Monad m => String -> Maybe a -> m a
  guardJust msg Nothing = fail $ "Unrecoverable error! " ++ msg
  guardJust _ (Just a) = pure a

  -- | Helper to run code when value is nothing
  whenNothing :: Monad m => Maybe a -> b -> m b -> m b
  whenNothing Nothing _ f = f
  whenNothing (Just _) a _ = return a
