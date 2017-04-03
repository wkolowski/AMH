{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module New where

class (Ord quality) => TSP solution quality where
	asses :: solution -> quality

instance (Ord quality) => Eq solution where
	x == y = asses x == asses y

instance Ord solution where
	compare x y = compare (asses x) (asses y)
