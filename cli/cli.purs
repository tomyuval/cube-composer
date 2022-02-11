module Main (main) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array as A
import Data.Int (fromString)
import Data.List (List, fromFoldable, drop, mapMaybe, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (fromCharArray, joinWith, toCharArray)
import Data.Traversable (for, for_)

import Types (Cube(..), Wall)
import PotentialLevelExplorer (potentialLevels)
import Solver (solve)
import Levels (allChapters, levelTitle, getLevel, getChapter, allLevelIds)
import Levels.Chapter0 (chapter0)

foreign import argv :: Array String

type ExplorationSpace = {
    chapterNumber :: Int,
    initial :: Wall
}

ttyColor :: Cube -> Int
ttyColor Yellow = 0
ttyColor Orange = 1
ttyColor Brown = 2
ttyColor Red = 3
ttyColor Cyan = 4

showList :: forall a. (Show a) => List a -> String
showList xs = show (A.fromFoldable xs :: Array a)

showList2 :: forall a. (Show a) => List (List a) -> String
showList2 xss = show ((A.fromFoldable <<< map A.fromFoldable) xss :: Array (Array a))

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = case parseArguments (fromFoldable argv) of
    Nothing -> solveAllLevels
    Just explorationSpace -> explorePossibleLevels explorationSpace.chapterNumber explorationSpace.initial

parseArguments :: List String -> Maybe ExplorationSpace
parseArguments arguments = case arguments !! 2 of
    Nothing -> Nothing
    Just chapterNumber -> Just {
        chapterNumber: fromMaybe 0 $ fromString chapterNumber,
        initial: map (mapMaybe parseCube <<< fromFoldable <<< toCharArray) (drop 3 arguments)
    }

parseCube :: Char -> Maybe Cube
parseCube 'Y' = Just Yellow
parseCube 'O' = Just Orange
parseCube 'B' = Just Brown
parseCube 'R' = Just Red
parseCube 'C' = Just Cyan
parseCube _ = Nothing

showCube :: Cube -> Char
showCube Yellow = 'Y'
showCube Orange = 'O'
showCube Brown = 'B'
showCube Red = 'R'
showCube Cyan = 'C'

solveAllLevels :: forall eff. Eff (console :: CONSOLE | eff) Unit
solveAllLevels = void do
    for allLevelIds $ \lid -> do
        let chapter = getChapter lid
            level = getLevel lid
            solutions = solve lid

        log $ levelTitle lid level
        log $ "  Initial: " <> showList2 (map (map ttyColor) level.initial)
        log $ "  Target:  " <> showList2 (map (map ttyColor) level.target)
        log $ "  Solutions: "
        for_ solutions $ \sol ->
            log $ "    " <> showList sol
        log ""

explorePossibleLevels :: forall eff. Int -> Wall -> Eff (console :: CONSOLE | eff) Unit
explorePossibleLevels chapterNumber initial = void do
    let chapter = fromMaybe chapter0 $ allChapters !! chapterNumber
    for (potentialLevels chapter.transformers initial) $ \potentialLevel -> do
        log $ "Target: " <>
              (joinWith " "
                        (A.fromFoldable (map (fromCharArray <<< map showCube <<< A.fromFoldable)
                                             potentialLevel.target)))
        log $ showList2 potentialLevel.target
        log $ "Steps: " <> showList2 potentialLevel.moves
        log "---"
