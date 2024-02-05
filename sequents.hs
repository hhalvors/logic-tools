-- Goal: create a CODE for the structure of propositional wffs that
-- does not depend on the particular atomic sentences that they are
-- constructed from. Create a many-to-one translation from WFF to
-- CODE. Create a function that takes a CODE and a list of Atoms and
-- returns the corresponding WFF

import Data.Hashable (hash)

-- Define data types for propositional logic
data PropLogic = Atom String
               | Not PropLogic
               | And PropLogic PropLogic
               | Or PropLogic PropLogic
               | Imply PropLogic PropLogic
               deriving (Show)

-- the function below does NOT do it. We don't care which atoms occur,
-- but we do care about which atoms match each other

decompose :: Prop -> [(Int, Prop)]
decompose prop = decompose' prop 0
  where
    decompose' :: Prop -> Int -> [(Int, Prop)]
    decompose' p depth = case p of
        Atom _ -> [(depth, p)]
        And p1 p2 -> (depth, p) : decompose' p1 (depth + 1) ++ decompose' p2 (depth + 1)
        Or p1 p2 -> (depth, p) : decompose' p1 (depth + 1) ++ decompose' p2 (depth + 1)
        Not p1 -> (depth, p) : decompose' p1 (depth + 1)
        If p1 p2 -> (depth, p) : decompose' p1 (depth + 1) ++ decompose' p2 (depth + 1)


-- Function to collect all atoms from a Prop
atomsInProp :: Prop -> [String]
atomsInProp (Atom a) = [a]
atomsInProp (And p1 p2) = atomsInProp p1 ++ atomsInProp p2
atomsInProp (Or p1 p2) = atomsInProp p1 ++ atomsInProp p2
atomsInProp (Not p) = atomsInProp p
atomsInProp (If p1 p2) = atomsInProp p1 ++ atomsInProp p2        

import qualified Data.Map as Map
import Data.Maybe (isJust, fromMaybe)

type AtomEquivalence = Map.Map String String

-- Function to check if two Props have the same structure with consistent atomic equivalences
sameStructure :: Prop -> Prop -> Bool
sameStructure p1 p2 = isJust $ compareProps p1 p2 Map.empty
  where
    compareProps :: Prop -> Prop -> AtomEquivalence -> Maybe AtomEquivalence
    compareProps (Atom a1) (Atom a2) eqs =
      case Map.lookup a1 eqs of
        Just a2' -> if a2' == a2 then Just eqs else Nothing
        Nothing -> Just $ Map.insert a1 a2 eqs
    compareProps (And p1a p1b) (And p2a p2b) eqs =
      compareProps p1a p2a eqs >>= \newEqs -> compareProps p1b p2b newEqs
    compareProps (Or p1a p1b) (Or p2a p2b) eqs =
      compareProps p1a p2a eqs >>= \newEqs -> compareProps p1b p2b newEqs
    compareProps (Not p1') (Not p2') eqs =
      compareProps p1' p2' eqs
    compareProps (If p1a p1b) (If p2a p2b) eqs =
      compareProps p1a p2a eqs >>= \newEqs -> compareProps p1b p2b newEqs
    compareProps _ _ _ = Nothing
        


-- Function to encode a propositional logic sentence into a structure code
encodeStructure :: PropLogic -> String
encodeStructure (Atom _) = "0"  -- All atoms are encoded the same way
encodeStructure (Not p) = "1" ++ encodeStructure p
encodeStructure (And p1 p2) = "2" ++ encodeStructure p1 ++ encodeStructure p2
encodeStructure (Or p1 p2) = "3" ++ encodeStructure p1 ++ encodeStructure p2
encodeStructure (Imply p1 p2) = "4" ++ encodeStructure p1 ++ encodeStructure p2

-- Function to generate a unique identifier (e.g., a hash) from the structure code
uniqueIdentifier :: PropLogic -> Int
uniqueIdentifier = hash . encodeStructure

-- Example usage
exampleSentence = Imply (And (Atom "A") (Atom "B")) (Atom "C")
exampleCode = encodeStructure exampleSentence
exampleIdentifier = uniqueIdentifier exampleSentence
