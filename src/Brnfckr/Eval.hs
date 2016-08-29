

module Brnfckr.Eval where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.ST
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.Class
import Data.Char
import Data.Maybe
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as VM


import Brnfckr.Parse


type BrainFuck s a = ExceptT BrainFuckError (StateT (MWorld s) (WriterT Output (ST s))) a
data MWorld s = MW { mRam :: MRam s, mPc :: {-# UNPACK #-} !Int, mDataInput :: Input }
type MRam s = UM.MVector s MemWord
data World = W { mem :: !Ptr, dataInput :: Input }
data Ptr = Ptr !Int (U.Vector MemWord)
type Input = [MemWord]
type Output = [MemWord]


memSize :: Int
memSize = 30000


instance Show World where
  show (W (Ptr pc ram) i) =
    let ls = V.toList $ V.take pc ram
        rs = V.toList $ V.drop (pc + 1) ram
        p = ram V.! pc
        left = unwords . map show . reverse $ take 3 ls
        right = unwords . map show $ take 3 rs
        brain = unwords ["[", left, "", show p, "", right, "]"]
    in unwords [ "World {"
               , brain
               , show i
               , "}"
               ]


ptrIncr, ptrDecr :: Int -> BrainFuck s ()
ptrIncr 0 = return ()
ptrIncr i = lift $ modify $ \w@MW{ mPc = pc } -> w { mPc = pc + i }
ptrDecr i = ptrIncr (-i)

getPc :: BrainFuck s Int
getPc = lift $ fmap mPc get

getRam :: BrainFuck s (UM.MVector s MemWord)
getRam = lift $ fmap mRam get

scanIncr, scanDecr :: BrainFuck s ()
scanIncr = do
  MW { mRam = ram', mPc = pc } <- lift get
  ram <- U.unsafeFreeze $ VM.drop pc ram'
  let idx = fromJust (V.elemIndex 0 ram)
  when (idx > 0) $ ptrIncr idx
scanDecr = do
  MW { mRam = ram', mPc = pc } <- lift get
  ram <- U.unsafeFreeze ram'
  let pls = V.reverse . V.take (pc + 1) $ ram
  let idx = fromJust (V.elemIndex 0 pls)
  when (idx > 0) $ ptrDecr idx

valIncr, valDecr :: MemWord -> BrainFuck s ()
valIncr i = do
  MW { mRam = ram, mPc = pc } <- lift get
  VM.modify ram (+ i) pc
valDecr i = do
  MW { mRam = ram, mPc = pc } <- lift get
  VM.modify ram (subtract i) pc

valOutput :: BrainFuck s ()
valOutput = do
  MW { mRam = ram, mPc = pc } <- lift get
  val <- ram `VM.read` pc
  lift . lift $ tell [val]

valInput :: BrainFuck s ()
valInput = do
  mem@MW { mRam = ram, mPc = pc, mDataInput = input } <- lift get
  case input of
    (byte:rest) -> do
        VM.write ram pc byte
        lift $ put mem { mDataInput = rest }
    _ -> throwE InsufficientInput

valSet :: MemWord -> BrainFuck s ()
valSet byte = do
  MW { mRam = ram, mPc = pc } <- lift get
  VM.write ram pc byte

runLoop :: [Term] -> BrainFuck s ()
runLoop terms = do
  MW { mRam = ram } <- lift get
  let prog = eval terms
      go p = do
        MW { mPc = pc } <- lift get :: BrainFuck s (MWorld s)
        byte <- ram `VM.read` pc
        when (byte /= 0) (p >> go p)
  prog `seq` go prog

compress :: [Term] -> [Term]
compress terms = snd $ fix run ([], terms)
  where
      smoosh = foldr go []
      run f (prev, cur) = if cur == prev
                          then (cur, cur)
                          else f (cur, smoosh cur)
      go (ValIncr j) (ValIncr i:rest) = ValIncr (i+j) : rest
      go (ValDecr j) (ValIncr i:rest) = ValIncr (i-j) : rest
      go (ValIncr j) (ValDecr i:rest) = ValDecr (i-j) : rest
      go (ValDecr j) (ValDecr i:rest) = ValDecr (i+j) : rest
      go (ValIncr _) (ValSet  i:rest) = ValSet  i     : rest
      go (ValDecr _) (ValSet  i:rest) = ValSet  i     : rest
      go (ValSet  j) (ValIncr i:rest) = ValSet  (j+i) : rest
      go (ValSet  j) (ValDecr i:rest) = ValSet  (j-i) : rest
      go (PtrIncr j) (PtrIncr i:rest) = PtrIncr (i+j) : rest
      go (PtrDecr j) (PtrIncr i:rest) = PtrIncr (i-j) : rest
      go (PtrIncr j) (PtrDecr i:rest) = PtrDecr (i-j) : rest
      go (PtrDecr j) (PtrDecr i:rest) = PtrDecr (i+j) : rest
      go (Loop t) acc = case compress t of
        [ValIncr _] -> ValSet 0 : acc
        [ValDecr _] -> ValSet 0 : acc
        -- If the value to be set is non-zero, the loop does not terminate
        [ValSet 0] -> ValSet 0 : acc
        [PtrIncr 1] -> ScanIncr : acc
        [PtrDecr 1] -> ScanDecr : acc
        compressed -> Loop compressed : acc
      go next acc = next : acc

eval :: [Term] -> BrainFuck s ()
eval = mapM_ f
  where
      f (ValIncr i) = valIncr i
      f (ValDecr i) = valDecr i
      f (ValSet i)  = valSet i
      f (PtrIncr i) = ptrIncr i
      f (PtrDecr i) = ptrDecr i
      f ScanIncr = scanIncr
      f ScanDecr = scanDecr
      f (Loop e) = runLoop e
      f ValInput = valInput
      f ValOutput = valOutput

optimizeBrainFuck :: Either BrainFuckError [Term] -> Either BrainFuckError [Term]
optimizeBrainFuck = fmap compress

runBrainFuck' :: Either BrainFuckError [Term] -> String
              -> ((Either BrainFuckError (), World), Output)
runBrainFuck' parseResult stream =
  case parseResult of
    Right ast -> runST $ do
      ram <- VM.replicate memSize 0
      let mWorld = MW { mRam = ram, mPc = 0, mDataInput = inputBytes }
          run = runWriterT . flip runStateT mWorld . runExceptT
      ((result, mWorld), output) <- run $ (eval ast)
      world <- freezeWorld mWorld
      return ((result, world), output)
    Left msg -> ((Left msg, emptyWorld), [])
  where
    inputBytes = map (fromIntegral . ord) stream
    emptyWorld = W { mem = Ptr 0 U.empty, dataInput = [] }
    freezeWorld (MW { mRam = ram, mPc = pc, mDataInput = d }) = do
      r <- V.freeze ram
      let mem = Ptr pc r
      return $ W { mem = mem, dataInput = d }

runBrainFuck :: String -> String -> ((Either BrainFuckError (), World), String)
runBrainFuck source stream = render output
  where
    output = runBrainFuck' (compress <$> parseBrainFuck source) stream
    toChars = map (chr . fromIntegral)
    render = fmap toChars

executeString :: String -> String -> Maybe String
executeString source stream = case runBrainFuck source stream of
  ((Left _, _), _) -> Nothing
  ((_, _), s) -> Just s
