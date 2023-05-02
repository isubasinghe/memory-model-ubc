{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

{- 
    There are two ways to reason about pointers, one being to reason about pointers
-}

import Control.Monad.State.Strict
import Data.BitVector.Sized hiding (and)
import qualified Data.Map.Strict as M
import GHC.Natural (Natural)

type Nat = Natural
type Variable = String
type Word8 = (BV 8)
type Word64 = (BV 64)
type MemSort = M.Map Word64 Word8

data SHMCond
  = BarrierAcquirePerformed
  | BarrierReleasePerformed
  | CacheInvalidationPerformed


data Undefined = Undefined

type LocalOwnershipInfo =  Undefined
type SharedMemConditions = M.Map Word64 SHMCond

data MemState = MemState
  { mappings :: MemSort,
    ownership :: LocalOwnershipInfo,
    shmemc :: SharedMemConditions
  }

data AddrType
  = ThreadLocal
  | SharedDMA
  | Shared

data Addr = Addr
  { loc :: Word64,
    ty :: AddrType
  }

type MState = State MemState

palignvalidPre :: Nat -> Bool
palignvalidPre alignment = case alignment of
  1 -> True
  2 -> True
  4 -> True
  8 -> True
  16 -> True
  32 -> True
  64 -> True
  _ -> False

palignvalid :: Addr -> Nat -> MState Bool
palignvalid addr alignment =
  if not $ palignvalidPre alignment
    then error "precondition not satisfied"
    else pure checkAlignment
  where
    addrLoc :: BV 64
    addrLoc = loc addr
    rightShifted = lshr (knownNat @64) addrLoc alignment
    leftShifted = shl (knownNat @64) rightShifted alignment
    checkAlignment :: Bool
    checkAlignment = leftShifted == addrLoc

memexists :: Addr -> MState Bool
memexists addr = do
  let addrLoc = loc addr
  st <- get
  let mappings' = mappings st
  pure $ M.member addrLoc mappings'

nonaliased :: Addr -> MState Bool
nonaliased addr = do
  st <- get
  -- what the fuck does this look like?
  undefined

pvalid :: Addr -> MState Bool
pvalid addr = do
  memcond <- memexists addr
  aliasedcond <- nonaliased addr
  pure $ memcond && aliasedcond

loadWord8Pre :: MemState -> Addr -> Bool
loadWord8Pre st addr = 
  let 
    addrTy = ty addr
    addrLoc = loc addr
    shcond = shmemc st 
    mappings' = mappings st
    valid = M.member addrLoc mappings'

  in case addrTy of 
      ThreadLocal -> valid
      Shared -> False 
      SharedDMA -> False

-- mem acc
loadWord8 :: Addr -> MState Word8
loadWord8 addr = do 
  st <- get
  if not $ loadWord8Pre st addr
    then error "precondition not satisfied"
    else do 
      let addrLoc = loc addr
      let mappings' = mappings st
      case M.lookup addrLoc mappings' of 
        Just val -> pure val 
        _ -> error "precondition definition error"

-- mem update
storeWord8 :: Addr -> Word8 -> MState Word8
storeWord8 addr val = undefined

main :: IO ()
main = do
  putStrLn "This is not executable, yet anyway"
