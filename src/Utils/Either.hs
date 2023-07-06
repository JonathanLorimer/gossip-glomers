module Utils.Either where

onLeftA :: Applicative m => Either e a -> (e -> m a) -> m a
onLeftA (Left e) action = action e
onLeftA (Right a) _ = pure a
