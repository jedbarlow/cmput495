import Data.Char

elmerFudd :: String -> String
elmerFudd = map (\c -> if c == 'r' || c == 'R' then 'w' else toLower c)
