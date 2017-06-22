module WordCount exposing (..)

import Dict
import List
import Maybe
import Regex
import String


parseWords : String -> List String
parseWords =
    Regex.find Regex.All (Regex.regex "\\w+") >> List.map (\v -> String.toLower (v.match))


wordCount : String -> Dict.Dict String Int
wordCount string =
    List.foldl
        (\k acc ->
            if not (Dict.member k acc) then
                Dict.insert k 1 acc
            else
                Dict.update k (Maybe.map ((+) 1)) acc
        )
        Dict.empty
        (parseWords string)
