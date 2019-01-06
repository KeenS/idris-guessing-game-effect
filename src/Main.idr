module Main

import Effects
import Effect.Random
import Effect.StdIO
import Data.String

||| get an integer from user.
||| wheh the input is not an integer, take another input
getInteger : Eff Integer [STDIO]
getInteger = do
  input <- getStr
  case parseInteger input of
    Just i => pure i
    Nothing => do
      putStrLn "Input wasn't an integer."
      putStr "Please input an integer: "
      getInteger

||| the game
game : Eff () [STDIO, RND]
game = do
  -- set the random seed
  srand 111
  -- generate an random number
  n <- rndInt 0 100
  -- enter the loop
  loop n
where
   mutual
    -- game loop
    loop : Integer -> Eff () [STDIO]
    loop n = do
      -- prompt
      putStr "Guess it: "
      input <- getInteger
      -- compare the input and the secret number
      case compare input n of
        GT => tooBig n
        EQ => gotIt
        LT => tooSmall n
    tooBig : Integer -> Eff() [STDIO]
    tooBig n = do
      putStrLn "Too big"
      -- when greater than, continue loop
      loop n
    tooSmall : Integer -> Eff() [STDIO]
    tooSmall n = do
      putStrLn "Too small"
      -- when smaller than, continue loop
      loop n
    gotIt : Eff() [STDIO]
    -- when exact, finish
    gotIt = putStrLn "You got it"

main : IO ()
main = do
  run game
