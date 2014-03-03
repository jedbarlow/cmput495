import Data.Monoid

newtype Writer w a = Writer { runWriter :: (a,w) }

instance (Monoid w) => Monad (Writer w) where
    return a             = Writer (a, mempty)
    (Writer (a,w)) >>= f = let (a',w') = runWriter $ f a
                             in Writer (a', w `mappend` w')


inc i    = Writer (i + 1, "incremented. ")
dec i    = Writer (i - 1, "decremented. ")
double i = Writer (i * 2, "doubled. ")

