import Control.Monad.RWS
import System.Random
import System.Random.Stateful ()

type Dice = Int

type DiceGame =
  RWS
    (Int, Int) -- Reader (dice bounds)
    [Dice] -- Writer (a history of rolls)
    StdGen -- State (random generator)

dice :: DiceGame Dice
dice = do
  bs <- ask
  r <- state (uniformR bs)
  tell [r]
  pure r

doubleDice :: DiceGame (Dice, Dice)
doubleDice = (,) <$> dice <*> dice

dices :: Int -> DiceGame [Dice]
dices n = replicateM n dice

diceGame :: DiceGame (Dice, Dice)
diceGame = dice >> dices 5 >> replicateM 2 (dices 3) >> dices 10 >> doubleDice

main :: IO ()
main = newStdGen >>= print . evalRWS diceGame (1, 6)
