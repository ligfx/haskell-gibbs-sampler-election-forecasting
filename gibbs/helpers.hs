module Gibbs.Helpers (rollM, zipMapM) where

import Control.Applicative ((<$>), (<*>))

(&&&) f g x = (,) <$> f x <*> g x

rollM 0 f initial = return []
rollM n f initial = (initial :) <$> (f initial >>= rollM (n - 1) f)

zipMapM f = mapM $ return &&& f