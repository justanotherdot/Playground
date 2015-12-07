import System.Random
import Control.Monad.State

data Player = Person { health :: Int
                       agility :: Int
                       strength :: Int } deriving (Eq)

data MonsTyp = Orc
               Hydra
               SlimeMold
               Brigand deriving (Eq)

type Health = Int
type Attack = Int
data Enemy = Monster MonsTyp Health Attack

instance Show MonsTyp where
  show Orc = "orc"
  show Hydra = "Hydra"
  show SlimeMold = "SlimeMold"
  show Brigand = "Brigand"

instance Show Enemy where
  show Monster Orc _ a = "A wicked orc with a level " ++ show a ++ " club"
  show Monster Hydra _ a = "A malicious hydra with " ++ show a " heads."
  show Monster SlimeMold _ a = "A slime mold with a sliminess of " ++ show a
  show Monster m _ _ = "A fierce " ++ show m


main :: IO ()
main = undefined

playerDead :: Player -> Bool
playerDead p = let {_, h, _} = p
                in if h <= 0 then True else False

monsterDead :: Monster -> Bool
monsterDead m = let Monster _ h _ = m
                 in if h <= 0 then True else False

monsTypes :: [MonsTyp]
monsTypes = [Orc, Hydra, SlimeMold, Brigand]

player :: Player
player = Person { health = 30, agility = 30, strength = 30 }

type MinRange = Int
type MaxRange = Int
randomSt :: (RandomGen g, Random a) => (MinRange, MaxRange) -> State g a
randomSt r = State randomR r

monster :: State StdGen Enemy
monster = do
  t <- randomSt (1, 4)
  h <- randomSt (1, 10)
  a <- randomSt (1, )
  return Monster (monsTypes !! t) h a
