import Data.List
import System.IO

newton n 0 = 1
newton n k = if n==k then 1 else newton (n-1)(k-1) + newton(n-1) k

trojkat n = zmiana n [0..n]

zmiana n []=[]
zmiana n (x:xs) = newton n x : zmiana n xs
