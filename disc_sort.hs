import Prelude
import Data.Bits
import Data.Char
import Data.Array
import Data.List as DL
import Criterion.Main


data Order t where
   NatO  :: Int -> Order Int
   TrivO :: Order t
   SumL  :: Order t1 -> Order t2 -> Order (Either t1 t2)
   ProdL :: Order t1 -> Order t2 -> Order (t1, t2)
   MapO  :: (t1 -> t2) -> Order t2 -> Order t1
   ListL :: Order t -> Order [t]
   Inv   :: Order t -> Order t

ordUnit :: Order ()
ordUnit = TrivO

ordNat8 :: Order Int
ordNat8 = NatO 255

ordNat16 :: Order Int
ordNat16 = NatO 65535

ordInt32W :: Order Int
ordInt32W = MapO tag (SumL (NatO 2147483648) (NatO 2147483647))
   where tag i = if i < 0 then Left (-i) else Right i

ordInt32 :: Order Int
ordInt32 = MapO (splitW . (+ (-2147483648))) (ProdL ordNat16 ordNat16)

splitW :: Int -> (Int, Int)
splitW x = (shiftR x 16 .&. 65535, x .&. 65535)

ordBool :: Order Bool
ordBool = MapO bool2sum (SumL ordUnit ordUnit)
   where bool2sum :: Bool -> Either () ()
         bool2sum False = Left ()
         bool2sum True = Right ()

ordChar8 :: Order Char
ordChar8 = MapO ord ordNat8

ordChar16 :: Order Char
ordChar16 = MapO ord ordNat16

fromList :: [t] -> Either () (t, [t])
fromList [] = Left ()
fromList (x : xs) = Right (x, xs)

listL :: Order t -> Order [t]
listL r = MapO fromList (SumL ordUnit (ProdL r (listL r)))

ordString8 :: Order String
ordString8 = listL ordChar8

ordString16 :: Order String
ordString16 = listL ordChar16

type Disc k = forall v. [(k, v)] -> [[v]]

sdisc :: Order k -> Disc k
sdisc _ [] = []
sdisc _ [(_, v)] = [[v]]
sdisc (NatO n) xs = sdiscNat n xs
sdisc TrivO xs = [[ v | (_, v) <- xs ]]
sdisc (SumL r1 r2) xs = sdisc r1 [ (k, v) | (Left k, v) <- xs ]
                        ++ sdisc r2 [ (k, v) | (Right k, v) <- xs ]
sdisc (ProdL r1 r2) xs =
   [ vs | ys <- sdisc r1 [ (k1,(k2,v)) | ((k1,k2), v) <-xs],
          vs <- sdisc r2 ys ]
sdisc (MapO f r) xs = sdisc r [ (f k, v) | (k, v) <- xs ]
sdisc (ListL r) xs = sdisc (listL r) xs
sdisc (Inv r) xs = reverse (sdisc r xs)

sdiscNat :: Int -> Disc Int
sdiscNat n xs = filter (not . null) (bdiscNat n update xs)
   where update vs v = v : vs
bdiscNat :: Int -> ([v] -> v -> [v]) -> [(Int, v)] -> [[v]]
bdiscNat (n :: Int) update xs = map reverse (elems (accumArray update [] (0, n-1) xs))

--------------------------------------------------
-- Boilerplate
--------------------------------------------------

getWords :: FilePath -> IO [String]
getWords path = do
   contents <- readFile path
   return (words contents)

main = do
   ws <- getWords "/tmp/p"
   zws <- return $ zip ws ws
   -- defaultMain [ bench "sdisc" $ whnf (sdisc ordString16) zws
   --            , bench "sort"  $ whnf DL.sort ws ]
   let as = ["AA","BCA","BA","BB","AAC","AB","BCA"]
   print $ sdisc ordString8 $ zip as as
   print "Done"
