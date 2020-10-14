import System.Environment
import Data.List
import Data.Maybe
import Control.Monad


data Exp = Input String | Neg Exp | And Exp Exp | Or Exp Exp
  deriving (Eq, Ord)

data Bit = One | Zero | Dash
  deriving (Eq, Ord)

type Term = [Bit]

type Minterm = ([Int],Term)

type TruthTable = ([String], [Term])

instance Show Exp where
  show (Neg (Input s)) = s ++ "'"
  show (Neg exp)       = "(" ++ show exp ++ ")'"
  show (Input str)     = str 
  show (And exp1 exp2) = show exp1 ++ show exp2
  show (Or  exp1 exp2) = show exp1 ++ "+" ++ show exp2

instance Show Bit where
  show One  = "1"
  show Zero = "0"
  show Dash = "_"

--------------------------------------------------------------------------------
--
-- Input/Output
--
--------------------------------------------------------------------------------

-- Parses input file as a string and returns a truthtable
parseInput :: String -> TruthTable
parseInput fileStr
  = (vars, table)
  where lines      = splitByDelimiter '\n' fileStr
        vars       = splitByDelimiter ' ' (head lines)
        splitLines = map (splitByDelimiter ' ') (tail.init $lines)
        table      = map (map toBit) splitLines
        
        -- Turns a string into a bit
        toBit :: String -> Bit
        toBit "1"  = One
        toBit _    = Zero 

-- Splits input string by delimiter
splitByDelimiter :: Char -> String -> [String]
-- first argument is the separator character
-- second argument is the string to be split
-- returns a list of words
splitByDelimiter delim str
  = foldr testDelim [[]] str
  where testDelim :: Char -> [String] -> [String]
        testDelim c strs@(hd:tl)
          | c == delim = []:strs
          | otherwise  = (c:hd):tl

--------------------------------------------------------------------------------

-- Extracts minterms from a truth table
getMinterms :: TruthTable -> [Minterm]
getMinterms (_,table) 
  = zip indices terms
  where terms   = map init (filter ((One==).last) table)
        indices = map (:[]) [1..]

-- Transforms a Bit into a coresponding expression
bitToExp :: String -> Bit -> Exp
-- first argument is the name of the coresponding variable
-- second argument is the bit
-- returns an expression
bitToExp name One  = Input name
bitToExp name Zero = Neg (Input name)
bitToExp  _   Dash = Input "" 

-- Turns a minterm into an expression
mintermToExp :: [String] -> Minterm -> Exp
-- first argument is a list of variable names
-- second argument is the minterm to be processed
-- returns an expression coresponding to the given minterms and the given vars
mintermToExp vars (_,term)
  = foldr1 And conjuncts
  where namePairs = zip vars term
        conjuncts = map (uncurry bitToExp) namePairs

getMintermExpression :: TruthTable -> Exp
getMintermExpression table@(vars, _)
  = foldr1 Or products
  where minterms   = map snd (getMinterms table)
        minterms'  = map (zip vars) minterms
        minterms'' = map (map $uncurry bitToExp) minterms'
        products   = map (foldr1 And) minterms''


--------------------------------------------------------------------------------
--
-- Minterm Optimization
--
--------------------------------------------------------------------------------

-- Matches two minterms
match :: Minterm -> Minterm -> Maybe Minterm
-- first two arguments are the minterms to be matched
-- returns a minterm under the Just constructor if the two given minterms differ
-- by only one bit. The result has the corresponding bit turned into a Dash
-- Otherwise return Nothing
match (ids1,bits1) (ids2,bits2) 
  | numMatches == 1 = Just (ids1++ids2, zipWith replace bits1 bits2)
  | otherwise       = Nothing
  where matchces   = zipWith (/=) bits1 bits2
        numMatches = length.(filter id) $matchces
        
        -- helper function to replace the different bit by a dash
        replace :: Bit -> Bit -> Bit
        replace x y
          | x /= y    = Dash
          | otherwise = x


-- Performs one optimization step on the given set of minterms
optimizeStep :: [Minterm] -> ([Minterm], [Minterm])
-- first argument is the list of minterms to be optimized
-- returns a pair consisting of:
--     - the mathced minterms
--     - the minterms which had no mathces (prime implicants)
optimizeStep terms
  = (nubBy (\x y -> snd x == snd y) combined, prime)
  where (combined, prime) = optimizeStep' terms
        
        optimizeStep' :: [Minterm] -> ([Minterm], [Minterm])
        optimizeStep' []
          = ([],[])
        optimizeStep' (mt:mts)
          | null matches = (optim, mt:essen)
          | otherwise    = (matches ++ optim, essen)
          where (optim, essen) = optimizeStep' mts
                matches        = mapMaybe (match mt) terms
                
                getMatches :: Minterm -> [Minterm] -> [Minterm]
                getMatches m = mapMaybe (match m)

-- Optimizes a set of minterms by repeating optimizeStep
optimizeMinterms :: [Minterm] -> [Minterm]
optimizeMinterms terms
  | null op   = ess
  | otherwise = ess ++ optimizeMinterms op
  where (op,ess) = optimizeStep terms


-- Optimizes a function given by a truth table
optimizeFunction :: TruthTable -> Exp
-- first argument is the truth table
-- returns the smallest expression that is still in the normal form
optimizeFunction table@(vars,_)
  = foldr1 Or (map (mintermToExp vars) solution)
  where minterms  = getMinterms table
        minterms' = optimizeMinterms minterms
        solution = head.sortOn length $(filter isSolution (powerset minterms'))
        
        -- chech whether a set of minterms is a solution
        isSolution :: [Minterm] -> Bool
        isSolution mts = length (nub $concatMap fst mts) == length minterms


-- Misc

-- Given a list, returns its powerset
powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

-- Counts the number of ones in a term
countOnes :: Term -> Int
countOnes = length.(filter (One==))

-- groups elements in a list by the values returned by a function
groupElements :: (Ord b) => (a -> b) -> [a] -> [(b,[a])]
-- first argument is the function that generates keys
-- second argument is a list
-- returns a list of pairs of keys and elements that have that key,
-- ordered by the key
groupElements func list
  = map (\x -> (func.head $x,x)) grouped
  where sorted  = sortOn func list
        grouped = groupBy (\ x y -> func x == func y) sorted


main :: IO ()
main
  = do
        args <- getArgs                  -- IO [String]
        progName <- getProgName          -- IO String
        putStrLn "Minterm logic minimizer using the Quine–McCluskey algorithm"
        putStrLn "The arguments are:"  
        mapM putStrLn args  
        
        putStrLn "The optimized solution is: " 
        file <- readFile (head args)
        let table@(v,t) = parseInput file
          in putStrLn (show.optimizeFunction $table)
