{-# LANGUAGE DerivingVia #-}

module MiniPatternsHandbook.Newtype where

newtype Health = Health Int deriving (Num) via Int

newtype Armor = Armor Int deriving (Num) via Int

newtype Attack = Attack Int deriving (Num) via Int

newtype Dexterity = Dexterity Int deriving (Num) via Int

newtype Strength = Strength Int deriving (Num) via Int

newtype Damage = Damage Int deriving (Num) via Int

newtype Defense = Defense Int deriving (Num) via Int

data Player = Player
  { playerHealth :: Health,
    playerArmor :: Armor,
    playerAttack :: Attack,
    playerDexterity :: Dexterity,
    playerStrength :: Strength
  }

calculatePlayerDamage :: Attack -> Strength -> Damage
calculatePlayerDamage (Attack a) (Strength s) = Damage $ a + s

calculatePlayerDefense :: Armor -> Dexterity -> Defense
calculatePlayerDefense (Armor a) (Dexterity d) = Defense $ a * d

calculateHit :: Damage -> Defense -> Health -> Health
calculateHit (Damage damage) (Defense defense) (Health health) =
  Health $ health + defense - damage

-- The second player hits first player and the new first player is returned
hitPlayer :: Player -> Player -> Player
hitPlayer player1 player2 =
  let damage =
        calculatePlayerDamage (playerAttack player2) (playerStrength player2)
      defense =
        calculatePlayerDefense (playerArmor player1) (playerDexterity player1)
      newHealth = calculateHit damage defense (playerHealth player1)
   in player1 {playerHealth = newHealth}
