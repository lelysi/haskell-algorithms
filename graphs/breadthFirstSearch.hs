module BreadthFirstSearch where

import qualified Data.Map as Map
import Data.Maybe

-- breadthFirstSearch algorithm
-- takes entity, graph of dependencies and function to check if entity is needed
breadthFirstSearch :: 
    Ord a => Eq a => a -> Map.Map a [a] -> (a -> Bool) -> Bool
breadthFirstSearch x m f | f x = True
                         | otherwise = searchResult
    where searchResult = isJust mapSearch && searchHelper (fromJust mapSearch) m [] f
          mapSearch = Map.lookup x m

searchHelper :: Ord a => Eq a => [a] -> Map.Map a [a] -> [a] -> (a -> Bool) -> Bool
searchHelper [] _ _ _ = False
searchHelper (x:xs) m checked f | f x = True
                                | x `elem` checked = searchHelper xs m checked f
                                | otherwise = searchHelper (xs ++ fromMaybe [] (Map.lookup x m)) m (x : checked) f

-- test
-- check if Person has connection to President
newtype Person = Person { isPresident :: Bool } deriving (Eq, Ord)

alice = Person False
bob = Person False
carl = Person False
victor = Person False
donald = Person True

listOfPersonsConnections :: Map.Map Person [Person]
listOfPersonsConnections = Map.fromList [(alice, [bob, carl]), (bob, []), (carl, [alice, bob, victor])]
listOfPersonsConnectionsWithPresident :: Map.Map Person [Person]
listOfPersonsConnectionsWithPresident = Map.fromList [(alice, [bob, carl]), (bob, []), (carl, [alice, bob, victor]), (victor, [donald])]

main :: IO ()
main = do
    print (if not (breadthFirstSearch alice listOfPersonsConnections isPresident) then "OK" else "ERROR")
    print (if breadthFirstSearch alice listOfPersonsConnectionsWithPresident isPresident then "OK" else "ERROR")