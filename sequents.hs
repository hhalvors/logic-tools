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
