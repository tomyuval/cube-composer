module Levels.Chapter4 where

import Data.List (List(..), (:), partition, concat)
import Data.Maybe (Maybe(..))

import Helper (fromArray, (:->), (:>))
import ListHelper (contains)
import Transformer (replaceSingle)
import Types (Chapter, Transformer, Cube(..), Difficulty(..))

partitionContains :: Cube -> Transformer
partitionContains cube wall =
    let parts = partition (contains cube) wall
    in concat (parts.no : parts.yes : Nil)

chapter4 :: Chapter
chapter4 = {
    name: "Chapter 4",

    transformers: fromArray [
        "replaceYbyR" :> {
            name: "map {Yellow}↦{Red}",
            function: replaceSingle Yellow Red
        },
        "replaceRbyC" :> {
            name: "map {Red}↦{Cyan}",
            function: replaceSingle Red Cyan
        },
        "replaceCbyY" :> {
            name: "map {Cyan}↦{Yellow}",
            function: replaceSingle Cyan Yellow
        },
        "partitionContainsC" :> {
            name: "partition (contains {Cyan})",
            function: partitionContains Cyan
        },
        "partitionContainsR" :> {
            name: "partition (contains {Red})",
            function: partitionContains Red
        }
    ],

    levels: fromArray [
        "4.1" :-> {
            name: "X",
            help: Nothing,
            difficulty: Hard,
            initial: [[Brown, Brown, Red, Red], [Brown, Brown, Brown, Cyan], [Brown, Yellow, Yellow, Yellow], [Brown, Brown, Brown, Red], [Brown, Brown, Cyan, Cyan], [Brown, Brown, Yellow, Yellow]],
            target: [[Brown, Cyan, Cyan, Cyan], [Brown, Brown, Cyan, Cyan], [Brown, Brown, Brown, Yellow], [Brown, Brown, Yellow, Yellow], [Brown, Brown, Cyan, Cyan], [Brown, Brown, Brown, Cyan]]
        },
        "4.2" :-> {
            name: "X",
            help: Nothing,
            difficulty: Hard,
            initial: [[Brown, Brown, Red, Red], [Brown, Brown, Brown, Cyan], [Brown, Yellow, Yellow, Yellow], [Brown, Brown, Brown, Red], [Brown, Brown, Cyan, Cyan], [Brown, Brown, Yellow, Yellow]],
            target: [[Brown, Brown, Cyan, Cyan], [Brown, Brown, Brown, Cyan], [Brown, Red, Red, Red], [Brown, Brown, Red, Red], [Brown, Brown, Brown, Red], [Brown, Brown, Red, Red]]
        },
        "4.3" :-> {
            name: "X",
            help: Nothing,
            difficulty: Hard,
            initial: [[Brown, Brown, Red, Red], [Brown, Brown, Brown, Cyan], [Brown, Yellow, Yellow, Yellow], [Brown, Brown, Brown, Red], [Brown, Brown, Cyan, Cyan], [Brown, Brown, Yellow, Yellow]],
            target: [[Brown, Brown, Brown, Yellow], [Brown, Brown, Yellow, Yellow], [Brown, Cyan, Cyan, Cyan], [Brown, Brown, Cyan, Cyan], [Brown, Brown, Cyan, Cyan], [Brown, Brown, Brown, Cyan]]
        },
        "4.4" :-> {
            name: "X",
            help: Nothing,
            difficulty: Hard,
            initial: [[Brown, Brown, Red, Red], [Brown, Brown, Brown, Cyan], [Brown, Yellow, Yellow, Yellow], [Brown, Brown, Brown, Red], [Brown, Brown, Cyan, Cyan], [Brown, Brown, Yellow, Yellow]],
            target: [[Brown, Red, Red, Red], [Brown, Brown, Red, Red], [Brown, Brown, Cyan, Cyan], [Brown, Brown, Brown, Cyan], [Brown, Brown, Brown, Red], [Brown, Brown, Red, Red]]
        },
        "4.5" :-> {
            name: "X",
            help: Nothing,
            difficulty: Hard,
            initial: [[Brown, Brown, Red, Red], [Brown, Brown, Brown, Cyan], [Brown, Yellow, Yellow, Yellow], [Brown, Brown, Brown, Red], [Brown, Brown, Cyan, Cyan], [Brown, Brown, Yellow, Yellow]],
            target: [[Brown, Red, Red, Red], [Brown, Brown, Red, Red], [Brown, Brown, Brown, Yellow], [Brown, Brown, Yellow, Yellow], [Brown, Brown, Red, Red], [Brown, Brown, Brown, Red]]
        },
        "4.6" :-> {
            name: "X",
            help: Nothing,
            difficulty: Hard,
            initial: [[Brown, Brown, Red, Red], [Brown, Brown, Brown, Cyan], [Brown, Yellow, Yellow, Yellow], [Brown, Brown, Brown, Red], [Brown, Brown, Cyan, Cyan], [Brown, Brown, Yellow, Yellow]],
            target: [[Brown, Yellow, Yellow, Yellow], [Brown, Brown, Yellow, Yellow], [Brown, Brown, Cyan, Cyan], [Brown, Brown, Brown, Cyan], [Brown, Brown, Brown, Yellow], [Brown, Brown, Yellow, Yellow]]
        }
    ]
}
