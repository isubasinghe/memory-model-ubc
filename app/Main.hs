{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import Data.Word ( Word8)
import Data.BitVector.Sized hiding (and)
import GHC.Natural (Natural)

type Nat = Natural

type MemSort = M.Map (BV 64) Word8

data SHMCond 
  = BarrierAcquirePerformed
  | BarrierReleasePerformed
  | CacheInvalidationPerformed

type LocalOwnership = M.Map (BV 64) Integer
type SharedMemConditions = M.Map (BV 64)SHMCond

data MemState = MemState 
    { mappings :: MemSort
    , ownership :: LocalOwnership
    , shmemc :: SharedMemConditions
    }

data AddrType 
    = ThreadLocal
    | SharedDMA 
    | Shared

data Addr = Addr
    { loc :: BV 64
    , ty :: AddrType}

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
palignvalid addr alignment = if not $ palignvalidPre alignment then 
                                error "precondition not satisfied"
                              else 
                                pure checkAlignment
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
  undefined


pvalid :: Addr -> MState Bool 
pvalid addr = do  
            memcond <- memexists addr
            aliasedcond <- nonaliased addr
            pure $ memcond && aliasedcond

loadWord8Pre :: Addr -> Bool 
loadWord8Pre addr = False

-- mem acc
loadWord8 :: Addr -> MState Word8
loadWord8 addr = if not $ loadWord8Pre addr then 
                    error "precondition not satisfied"
                  else 
                    pure 0

-- mem update
storeWord8 :: Addr -> Word8 -> MState Word8 
storeWord8 addr val = undefined 

main :: IO ()
main = do 
  putStrLn "This is not executable, yet anyway"
