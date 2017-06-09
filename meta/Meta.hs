module Meta

class (Ord b) => Meta a b where
	asses :: a -> IO b
	--compare :: a -> a -> IO a


class (Meta a b) => RndMeta a b where
	random :: IO a

class (Meta a b) => GreedyMeta a b where
	greedy :: IO a




