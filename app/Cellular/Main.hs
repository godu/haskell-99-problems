{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module Main where

import Data.List (nubBy)
import Debug.Trace (traceShowId)
import Graphics.Gloss
  ( Display (InWindow),
    Picture (Color),
    blue,
    circleSolid,
    lineLoop,
    pictures,
    rectanglePath,
    simulate,
    translate,
    white,
    withAlpha,
  )
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Linear
  ( Additive ((^+^), (^-^)),
    Metric (norm),
    V2 (..),
    (*^),
    (^*),
    (^/),
  )
import System.Random
  ( Random (randomR, randomRs),
    RandomGen (split),
    newStdGen,
  )

type Position = V2 Float

type Velocity = V2 Float

type Index = Int

type TimeStep = Float

data Particle = Particle
  { index :: Index,
    position :: Position,
    velocity :: Velocity
  }
  deriving (Show)

instance Eq Particle where
  a == b = index a == index b

type Model = [Particle]

initialModel :: RandomGen g => g -> Model
initialModel = modelRandom 16

squareLatticeModel :: Int -> Model
squareLatticeModel n = zipWith3 Particle indexes positions velocities
  where
    indexes = [1 .. (n ^ 2)]
    positions = squareLattice n n
    velocities = replicate (n ^ 2) (V2 0.0 0.0)

squareLattice :: Int -> Int -> [Position]
squareLattice _ 0 = []
squareLattice dim acc = latticeRow dim dim yPosition ++ squareLattice dim (acc - 1)
  where
    dy = bLength / fromIntegral (dim + 1)
    yPosition = bLength / 2 - (fromIntegral acc * dy)

latticeRow :: Int -> Int -> Float -> [Position]
latticeRow _ 0 _ = []
latticeRow dim acc yPosition = V2 xPosition yPosition : latticeRow dim (acc - 1) yPosition
  where
    dx = aLength / fromIntegral (dim + 1)
    xPosition = aLength / 2 - (fromIntegral acc * dx)

modelRandom :: RandomGen g => Int -> g -> [Particle]
modelRandom n g = take n $ traceShowId $ nubBy isOverlapped $ zipWith3 Particle indexes positions velocities
  where
    indexes = [1 ..]
    (g', g'') = split g
    positions = randomPos n g'
    velocities = randomVel n g''
    isOverlapped (Particle _ (V2 x y) _) (Particle _ (V2 x' y') _) =
      sqrt ((x' - x) ^ 2 + (y' - y) ^ 2) <= dotSize * 2

randomVel :: RandomGen g => Int -> g -> [Velocity]
randomVel n g = take n $ randomRs (-0.2, 0.2) g

genPos :: RandomGen g => g -> (Position, g)
genPos g = (position, g'')
  where
    (xGen, g') = randomR (- aLengthHalf, aLengthHalf) g
    (yGen, g'') = randomR (- bLengthHalf, bLengthHalf) g'
    position = V2 xGen yGen
    aLengthHalf = aLength / 2 - dotSize
    bLengthHalf = bLength / 2 - dotSize

randomPos :: RandomGen g => Int -> g -> [Position]
randomPos 0 _ = []
randomPos n g = newPos : randomPos (n -1) g'
  where
    (newPos, g') = genPos g

main :: IO ()
main = do
  seed <- newStdGen
  simulate
    windowDisplay
    white
    simulationRate
    (initialModel seed)
    drawingFunc
    updateFunc
  where
    windowDisplay :: Display
    windowDisplay = InWindow "MD in Haskell" (800, 800) (200, 800)

    simulationRate :: Int
    simulationRate = 120

dotSize :: Float
dotSize = 0.1

aLength, bLength :: Float
aLength = 7.0
bLength = 7.0

drawingFunc :: Model -> Picture
drawingFunc = pictures . (drawWalls :) . fmap drawParticle

drawWalls :: Picture
drawWalls = lineLoop $ rectanglePath (toPixels aLength) (toPixels bLength)

drawParticle :: Particle -> Picture
drawParticle (Particle _ (V2 x y) _) =
  translate x' y' $ color (circleSolid $ toPixels dotSize)
  where
    x' = toPixels x
    y' = toPixels y
    color = Color (withAlpha 0.8 blue)

toPixels :: Float -> Float
toPixels = (* 100.0)

updateFunc :: ViewPort -> TimeStep -> Model -> Model
updateFunc _ = verletStep

newton :: TimeStep -> Particle -> Particle
newton dt (Particle index position velocity) = Particle index position' velocity
  where
    position' = position + velocity ^* dt

boundaryCondition :: Particle -> V2 Float
boundaryCondition (Particle _ (V2 x y) _)
  | (x' > aLength / 2) && (y' > bLength / 2) = V2 (-1) (-1)
  | x' > aLength / 2 = V2 (-1) 1
  | y' > bLength / 2 = V2 1 (-1)
  | otherwise = V2 1 1
  where
    x' = abs x + dotSize
    y' = abs y + dotSize

newtonBound :: Float -> Particle -> Particle
newtonBound dt particle@(Particle index position velocity) = Particle index position' velocity'
  where
    transVec = boundaryCondition particle
    velocity' = transVec * velocity
    position' = position + velocity' ^* dt

type Force = V2 Float

type Acceleration = V2 Float

verletStep :: TimeStep -> Model -> Model
verletStep dt particles = newParticules
  where
    oldForces = calcForces particles
    oldAccelerations = fmap (^/ mass) oldForces
    newPositions = updatePositions dt particles oldAccelerations
    newForces = calcForces newPositions
    newAccelerations = fmap (^/ mass) newForces
    addedForces = oldAccelerations ^+^ newAccelerations
    newParticules = updateVelocities dt newPositions addedForces

updatePosition :: TimeStep -> Particle -> Acceleration -> Particle
updatePosition dt (Particle index position velocity) acceleration = Particle index newPosition velocity
  where
    newPosition = position ^+^ velocityParticle ^+^ accelerationParticle
    velocityParticle = velocity ^* dt
    accelerationParticle = acceleration ^* (0.5 * (dt ** 2))

updateVelocity :: TimeStep -> Particle -> Acceleration -> Particle
updateVelocity dt particle@(Particle index position velocity) acceleration = Particle index position velocity'
  where
    transVector = boundaryCondition particle
    velocity' = transVector * (velocity + (0.5 * dt) *^ acceleration)

updatePositions, updateVelocities :: TimeStep -> [Particle] -> [Force] -> [Particle]
updatePositions dt = zipWith (updatePosition dt)
updateVelocities dt = zipWith (updateVelocity dt)

calcForceBetween :: Particle -> Particle -> Force
calcForceBetween a@(Particle _ positionA _) b@(Particle _ positionB _)
  | a == b = V2 0.0 0.0
  | otherwise = rep - att
  where
    rep = repulsion positionA positionB
    att = attraction positionA positionB

epsilon = 12.57

sigma = 0.335

mass = 18

sigma6 = sigma ** 6

sigma12 = sigma ** 12

repulsion, attraction :: Position -> Position -> Force
repulsion positionA positionB = (epsilon * 48.0 * sigma12 / divisor) *^ r
  where
    divisor = norm r ^ 14
    r = positionB ^-^ positionA
attraction positionA positionB = (epsilon * 24.0 * sigma6 / divisor) *^ r
  where
    divisor = norm r ^ 8
    r = positionB ^-^ positionA

calcForceOnOne :: Particle -> [Particle] -> [Force]
calcForceOnOne particle = fmap (calcForceBetween particle)

calcForceAcc :: [Particle] -> [Particle] -> [Force]
calcForceAcc [] _ = []
calcForceAcc [particle] particles = calcForceOnOne particle particles
calcForceAcc (p : articles) particles = calcForceOnOne p particles ^+^ calcForceAcc articles particles

calcForces :: [Particle] -> [Force]
calcForces particles = calcForceAcc particles particles
