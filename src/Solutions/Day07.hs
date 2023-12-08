module Solutions.Day07 (Card (..), day07, parser, part1, part2) where

import Data.List (sortBy, partition)
import qualified Data.Map as M
import Lib.AOC (runSolution)
import Lib.Common (nTimes)
import Lib.Map (getCounts)

-- Main fn to execute both parts
day07 :: IO ()
day07 = runSolution "07" parser part1 part2

-- Parsing
type Input = [(Hand, Int)]

parser :: String -> Input
parser = map ((\[w1, w2] -> (map toCard w1, read w2)) . words) . lines

-- Data types
data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | T | J | Q | K | A deriving (Show, Eq, Ord)

toCard :: Char -> Card
toCard '2' = Two
toCard '3' = Three
toCard '4' = Four
toCard '5' = Five
toCard '6' = Six
toCard '7' = Seven
toCard '8' = Eight
toCard '9' = Nine
toCard 'T' = T
toCard 'J' = J
toCard 'Q' = Q
toCard 'K' = K
toCard 'A' = A
toCard _ = error "Invalid card char"

data HandType = HighCard | OnePair | TwoPairs | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
  deriving (Show, Eq, Ord)

type Hand = [Card]

-- Part 1
calcHandType :: Hand -> HandType
calcHandType hand =
  let counts = sortBy (\x1 x2 -> compare (snd x2) (snd x1)) $ M.toList $ getCounts hand
   in case counts of
        [(_, 5)] -> FiveOfAKind
        ((_, 4) : _) -> FourOfAKind
        [(_, 3), (_, 2)] -> FullHouse
        ((_, 3) : _) -> ThreeOfAKind
        ((_, 2) : (_, 2) : _) -> TwoPairs
        ((_, 2) : _) -> OnePair
        _ -> HighCard

-- Generic card comparer to allow for different rules for part1 and 2
compareCardsWith :: (Card -> Card -> Ordering) -> Hand -> Hand -> Ordering
compareCardsWith comparer h1 h2 = head $ 
  [ordering | (c1, c2) <- zip h1 h2, let ordering = comparer c1 c2, ordering /= EQ]

-- Generic hand comparer to allow for different rules for part1 and 2
compareHandWith :: (Hand -> HandType) -> (Card -> Card -> Ordering) -> Hand -> Hand -> Ordering
compareHandWith handTypeCalculator cardComparer h1 h2 = 
  case compare (handTypeCalculator h1) (handTypeCalculator h2) of
      -- If the hand types are equal, then we start comparing their individual cards instead
      EQ -> compareCardsWith cardComparer h1 h2
      -- If the hand types are not equal, we return the GT or LT ordering
      result -> result

scoreHands :: (Hand -> Hand -> Ordering) -> [(Hand, Int)] -> [Int]
scoreHands handComparer input = 
  let rankedHands = sortBy (\x1 x2 -> handComparer (fst x1) (fst x2)) input
  in zipWith (curry (\((_, bid), rank) -> bid * rank)) rankedHands [1 .. length rankedHands]

part1 :: Input -> Int
part1 = sum . scoreHands (compareHandWith calcHandType compare)

-- Part 2
improveTypeWithJoker :: HandType -> HandType
improveTypeWithJoker HighCard = OnePair
improveTypeWithJoker OnePair = ThreeOfAKind
improveTypeWithJoker TwoPairs = FullHouse
improveTypeWithJoker ThreeOfAKind = FourOfAKind
improveTypeWithJoker FullHouse = FourOfAKind
improveTypeWithJoker FourOfAKind = FiveOfAKind
improveTypeWithJoker FiveOfAKind = FiveOfAKind

-- We calculdate the type of the Hand whilst ignoring the Joker cards. Then for N number of jokers in the Hand,
-- we apply the improveTypeWithJoker function which knows what is the best improvement that a Joker can give when 
-- given a particular hand type
calcHandTypeWithJokerRule :: Hand -> HandType
calcHandTypeWithJokerRule h = 
  let (jokers, nonJokers) = partition (== J) h
   in nTimes (length jokers) improveTypeWithJoker (calcHandType nonJokers)

compareCardsWithJokerRule :: Card -> Card -> Ordering
compareCardsWithJokerRule J J = EQ
compareCardsWithJokerRule J _ = LT
compareCardsWithJokerRule _ J = GT
compareCardsWithJokerRule c1 c2 = compare c1 c2

part2 :: Input -> Int
part2 = sum . scoreHands (compareHandWith calcHandTypeWithJokerRule compareCardsWithJokerRule)
