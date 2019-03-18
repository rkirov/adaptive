import Control.Monad.Adaptive
import Data.Char
import Control.Monad.Adaptive.Ref
import Control.Monad(ap,when)
import Data.IORef(IORef)

type InIO m a = m IO IORef a
type IOMod a = InIO Modifiable a

data List' a m r = Nil | Cons a (Modifiable m r (List' a m r)) deriving Eq

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = do a <- ma
                return (f a)

-- algorithm copied from the paper
filter' :: (Eq a, NewMod n m r) => (a -> Bool) -> Modifiable m r (List' a m r) -> n m r (Modifiable m r (List' a m r))
filter' f l = newMod (filt l)
  where
    filt l = do
      l' <- readMod l
      case l' of
        Nil -> return Nil
        Cons h r ->
          if f h then Cons h `liftM` newMod (filt r)
                 else filt r

qsort' :: (Ord a, NewMod n m r) => Modifiable m r (List' a m r) -> n m r (Modifiable m r (List' a m r))
qsort' l = newMod (qs l Nil) where
  qs l rest = do
    l' <- readMod l
    case l' of
      Nil -> return rest
      Cons h r -> do
        l <- filter' (< h) r
        g <- filter' (>= h) r
        gs <- newMod (qs g rest)
        qs l (Cons h gs)

-- there is no readMod in A, we have to make an empty
-- newMod in order to 'enter' C where we can read Mods
-- 
-- TODO: Extend this to printing the whole list
-- trivial recursive extension runs into problems with
-- Adaptive vs Changable
printHead :: (Num a, Show a) => Modifiable IO IORef (List' a IO IORef) ->
                                Adaptive IO IORef (Modifiable IO IORef ())
printHead l = newMod $ do
      s <- readMod l 
      case s of
        Nil -> inM (putStrLn "head: nil")
        Cons v r -> inM (putStrLn $ "head: " ++ show v)

main = run $ do 
   nil <- newMod $ return Nil
   l1 <- newMod $ return $ Cons 1 nil
   l2 <- newMod $ return $ Cons 3 l1 
   l3 <- newMod $ return $ Cons 2 l2
   -- l3 is [2,3,1]
   inM (putStrLn "start")  
   -- sorted list is [1,2,3]
   sortedL <- qsort' l3
   inM (putStrLn "sorted")  
   printHead sortedL
   inM (putStrLn "change")  
   -- cut the tail, original list now [2,3]
   change l1 Nil
   inM (putStrLn "propagate")  
   propagate
   -- even the IO effect of printing the head is propagated
   -- and head is reprinted to be 2
