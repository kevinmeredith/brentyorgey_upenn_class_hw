--Unfortunately, monads do not compose as nicely as applicative 
--functors (yet another reason to use Applicative if you don’t 
--need the full power that Monad provides)

--http://stackoverflow.com/questions/29453915/composing-monads-v-applicative-functors

newtype Compose f g x = Compose { getCompose :: f (g x) }

instance (Functor f, Functor g) => Functor (Compose f g) where
	fmap f (Compose fgx) = Compose (fmap (fmap f) fgx)