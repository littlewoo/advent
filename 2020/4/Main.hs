
import System.Environment
import Data.List.Split
import qualified Data.Map as Map
import Text.Regex

main :: IO ()
main = do
            args <- getArgs
            text <- readFile (args !! 0)
            print (validate text)

-- map of field names to the validation regex and a function for validating the string
mandatoryFields :: [(String, (Regex, Maybe [String] -> Bool))]
mandatoryFields =   [("byr", (mkRegex "^([0-9]{4})$",                   numberBetween (1920, 2002))),
                     ("iyr", (mkRegex "^([0-9]{4})$",                   numberBetween (2010, 2020))),
                     ("eyr", (mkRegex "^([0-9]{4})$",                   numberBetween (2020, 2030))),
                     ("hgt", (mkRegex "([0-9]*)(cm|in)",                validateHeight)),
                     ("hcl", (mkRegex "^#([0-9a-f]{6})$",                (/= Nothing))),
                     ("ecl", (mkRegex "(amb|blu|brn|gry|grn|hzl|oth)",  (/= Nothing))),
                     ("pid", (mkRegex "^([0-9]{9})$",                   (/= Nothing)))]
optionalFields :: [(String, (Regex, Maybe String -> Bool))]
optionalFields =   [("cid", (mkRegex ".*",                   (/= Nothing)))]

validate :: String -> ([(Map.Map String String, Bool)], Int)
validate s = let vs = (map (\r -> (r,isValid r)) (map convertInputToMap (splitOn "\n\n" s))) in (vs, length (filter (\(m, valid) -> valid) vs))

isValid :: Map.Map String String -> Bool
isValid m = length (filter (validateField m) mandatoryFields) == length mandatoryFields

validateField :: Map.Map String String -> (String, (Regex, Maybe [String] -> Bool)) -> Bool
validateField m (k, (regex, validateValue)) = let mVal = Map.lookup k m in 
                                                           case mVal of 
                                                               Nothing  -> False
                                                               Just v   -> validateValue (matchRegex regex v) 

convertInputToMap :: String -> Map.Map String String
convertInputToMap s = Map.fromList (map extractKeyValue (words s))

extractKeyValue :: String -> (String, String)
extractKeyValue s = let ns = (/= ':') in (takeWhile ns s, tail (dropWhile ns s))

numberBetween :: (Int, Int) -> Maybe [String] -> Bool
numberBetween _ Nothing                   = False
numberBetween (lower, upper) (Just (s:_)) = let n = ((read :: String -> Int) s) in (n >= lower) && (n <= upper)

-- validate the matches from the height regex
validateHeight :: Maybe [String] -> Bool
validateHeight Nothing        = False
validateHeight (Just matches) = (((matches !! 1) == "in") && (numberBetween (59, 76) (Just matches))) || (((matches !! 1) == "cm") && (numberBetween (150, 193) (Just matches)))
