import Prelude hiding (sum)
import Data.List (foldl', sort, sortOn, partition, intersperse)
import Text.Read (readMaybe)

sum = foldl' (+) 0
ssum = foldl' (++) ""

-- TUPLES MUST BE STRICTLY INCREASING IN FIRST ENTRY!
fuse :: Ord a => [(a, Integer)] -> [(a, Integer)]
fuse = \vs -> case vs of
    [] -> []
    (t, r) : [] -> (t, r) : []
    (t, r) : (t', r') : vs'' ->
        if t == t' then fuse $ (t, r + r') : vs''
        else (t, r) : (fuse $ (t', r') : vs'')

showEtc :: [(Integer, Integer)] -> String
showEtc = \vs -> 
    --  showMand :: (Integer, Integer) -> String
    let showMand = \(d, e) -> case (d, e) of
            (1, 1) -> "(1-1/p)"
            (1, e) -> "(1-1/p)^" ++ show e
            (d, 1) -> "(1-1/p^" ++ show d ++ ")"
            (d, e) -> "(1-1/p^" ++ show d ++ ")^" ++ show e
    in  case vs of
            [] -> "()"
            vs -> (ssum . intersperse " * " . fmap showMand) vs

etc :: [(Integer, Integer)]  -> [(Integer, Integer)]
etc = \vs ->
    --  etcR :: Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
    let etcR = \acc -> \ws -> seq acc $ case ws of
            [] -> []
            (r, m) : [] -> fmap (\d -> (d, m + acc)) [r,r-1..1]
            (r, m) : (r', m') : ws'' -> (fmap (\d -> (d, m + acc)) [r,r-1..r'+1]) ++ (etcR (acc + m) ((r', m') : ws''))
    in  (reverse . (etcR 0) . fuse . fmap (\r -> (r, 1)) . reverse . sort . fmap snd) vs

-- TUPLES MUST BE FILTERED AND STRICTLY INCREASING IN FIRST ENTRY!
pExp :: [(Integer, Integer)] -> Integer
pExp = \vs -> 
    --  sto :: [Integer]
    let sto = fmap (\(t, r) -> (t + 1) * r) vs
    --  pExpPR :: Integer -> [(Integer, Integer)] -> Integer
        pExpPR = \acc -> \vs -> seq acc $ case vs of
            [] -> acc
            (t, r) : vs' -> pExpPR (acc + (t + 1) * r ^ 2 + 2 * sum (take (length vs') sto) * r) vs' 
    in  pExpPR 0 $ reverse vs

showGroup :: [(Integer, Integer)] -> String
showGroup = \vs ->
    --  showBpand :: (Integer, Integer) -> String
    let showBpand = \(t, r) -> case (t, r) of
            (0, 1) -> "(Z/p)"
            (0, r) -> "(Z/p)^" ++ show r
            (t, 1) -> "(Z/p^" ++ show (t + 1) ++ ")"
            (t, r) -> "(Z/p^" ++ show (t + 1) ++ ")^" ++ show r
    in  case vs of
            [] -> "0"
            vs -> (ssum . intersperse " \\oplus ". fmap showBpand) vs

getExps :: IO [(Integer, Integer)]
getExps =
    --  getExpsR :: IO [(Integer, Integer)]
    let getExpsR = do
            line <- getLine
            case line of
                [] -> return []
                line -> case readMaybe line of
                    Just (t, r) -> fmap ((t, r) :) getExpsR
                    _ -> do  putStrLn "I didn't catch that! Try again."
                             getExpsR
    in  do  putStrLn $ "Enter G presented as a biproduct by encoding it as a sequence of \\newline-separated pairs, where the pair \"(t, r)\" encodes the biproductand (Z/p^{t+1})^r.\n" ++
                       "Return an empty line when finished."
            getExpsR

main :: IO ()
main =
    --  mainR :: IO ()
    let mainR = do  
            (exps, chaff) <- fmap (partition (\(t, r) -> t >= 0 && r >= 1) . fuse . sortOn fst) getExps
            case chaff of [] -> return (); chaff -> putStrLn $ "The following input (net) values were ignored: " ++ show chaff ++ "\n"
            case exps of
                [] -> putStrLn $ "The Abelian p-group 0 has 1 automorphism."
                exps -> putStrLn $ "The Abelian p-group " ++ showGroup exps ++ " has p^" ++ show (pExp exps) ++ " * " ++ (showEtc . etc) exps ++ " automorphisms."
            putStrLn "Return an empty line to continue or type any character to quit."
            c <- getChar
            case c of
                '\n' -> mainR
                _ -> do putStrLn "\n\nBye!"; return ()
    in  do  putStrLn "Given a finitely-generated Abelian p-group G, this program computes |Aut(G)| as a rational function of the prime p.\n"
            mainR
