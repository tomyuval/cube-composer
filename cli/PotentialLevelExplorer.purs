module PotentialLevelExplorer (
    potentialLevels
    ) where

import Prelude
import Data.List (List(..), concatMap, filter, length, (:))
import Data.Map (fromFoldableWith, toUnfoldable)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))

import Transformer (transformed)
import Types (Transformer, TransformerId, TransformerRecord, Wall)

type TransformerInfo = { id :: TransformerId, function :: Transformer }
type PotentialLevel = { moves :: List (List TransformerId), target :: Wall }

getTransformerInfos :: SM.StrMap TransformerRecord -> List TransformerInfo
getTransformerInfos recordById = map (\(Tuple id record) -> { id: id, function: record.function })
                                     (SM.toUnfoldable recordById)

allOrderedSubsets :: forall a. List a -> List (List a)
allOrderedSubsets Nil = Nil : Nil
allOrderedSubsets (x : xs) = concatMap (\s -> s : (do partition <- allPartitions s
                                                      pure $ partition.firsts <> (x : partition.lasts)))
                                       (allOrderedSubsets xs)

allPartitions :: forall a. List a -> List { firsts :: List a, lasts :: List a }
allPartitions Nil = { firsts: Nil, lasts: Nil } : Nil
allPartitions list@(x : xs) = { firsts: Nil, lasts: list } : do partition <- allPartitions xs
                                                                pure { firsts: x : partition.firsts,
                                                                       lasts: partition.lasts }

potentialLevels :: SM.StrMap TransformerRecord -> Wall -> List PotentialLevel
potentialLevels transformers initial = -- { moves: ("abc" : "def" : Nil) : ("xyz" : Nil) : Nil, target: Nil } : Nil
    transformers #
    getTransformerInfos #
    allOrderedSubsets #
    map (\infos -> Tuple (map (_.id) infos) (transformed (map (_.function) infos) initial)) #
    map (\(Tuple transformerIds final) -> Tuple final (transformerIds : Nil)) #
    fromFoldableWith (<>) #
    toUnfoldable #
    filter (\(Tuple _ moves) -> length moves == 1) #
    map (\(Tuple final moves) -> { moves: moves, target: final })
