
-- Pair
data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)


-- Labelled
data Labelled e a = Labelled e a deriving Show

instance Functor (Labelled e) where
  fmap f (Labelled e a) = Labelled e (f a)


-- OneOrTwo
data OneOrTwo a = One a | Two a a 
  deriving Show

instance Functor OneOrTwo where
  fmap f (One a) = One (f a)
  fmap f (Two a b) = Two (f a) (f b)


-- Either
data EitherX e a = LeftX e | RightX a
  deriving Show

instance Functor (EitherX e) where
  fmap f (LeftX e) = LeftX e
  fmap f (RightX a) = RightX (f a)


-- MultiTree
data MultiTree a = Leaf | Node a [MultiTree a]
  deriving Show

instance Functor MultiTree where
  fmap f Leaf = Leaf
  fmap f (Node a children) = Node (f a) (map (fmap f) children)


-- Stream
data Stream a = Cons a (Stream a) deriving Show

instance Functor Stream where
  fmap f (Cons a s) = Cons (f a) (fmap f s)