

module Brnfckr.Eval where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Control.Monad.Writer.Strict
import Data.Char
import Data.Word
import Data.Maybe
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as VM


import Brnfckr.Parse


type BrainFuck a = ExceptT BrainFuckError (StateT World (Writer Output)) a
data World = W { mem :: !Ptr, dataInput :: Input }
data Ptr = Ptr !Int (U.Vector Word8)
type Input = [Word8]
type Output = [Word8]


memSize :: Int
memSize = 30000

defaultPtr :: Ptr
defaultPtr = Ptr 0 (V.replicate memSize 0)

defaultWorld :: World
defaultWorld = W { mem = defaultPtr, dataInput = [] }


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

getMem :: BrainFuck Ptr
getMem = lift $ fmap mem get

setMem :: Ptr -> BrainFuck ()
setMem p = lift $ modify $ \w -> w { mem = p }

ptrIncr, ptrDecr :: Int -> BrainFuck ()
ptrIncr 0 = return ()
ptrIncr i = do
  Ptr pc ram <- getMem
  setMem $ Ptr (pc + i) ram
ptrDecr i = ptrIncr (-i)

scanIncr, scanDecr :: BrainFuck ()
scanIncr = do
  Ptr pc ram <- getMem
  let prs = V.drop pc ram
      idx = fromJust (V.elemIndex 0 prs)
  when (idx > 0) $ ptrIncr idx
scanDecr = do
  Ptr pc ram <- getMem
  let pls = V.reverse . V.take (pc + 1) $ ram
      idx = fromJust (V.elemIndex 0 pls)
  when (idx > 0) $ ptrDecr idx

valIncr, valDecr :: Word8 -> BrainFuck ()
valIncr i = do
  Ptr pc ram <- getMem
  let newVal = (ram V.! pc) + i
      newRam = runST $ do
        v <- V.thaw ram
        VM.write v pc newVal
        V.freeze v
  setMem $ Ptr pc newRam
valDecr i = do
  Ptr pc ram <- getMem
  let newVal = (ram V.! pc) - i
      newRam = runST $ do
        v <- V.thaw ram
        VM.write v pc newVal
        V.freeze v
  setMem $ Ptr pc newRam

valOutput :: BrainFuck ()
valOutput = do
  Ptr pc ram <- getMem
  tell [ram V.! pc]

valInput :: BrainFuck ()
valInput = do
  W { mem = Ptr pc ram, dataInput = input } <- lift get
  case input of
    (byte:rest) -> do
          let newRam = runST $ do
                v <- V.thaw ram
                VM.write v pc byte
                V.freeze v
          lift $ put W { mem = Ptr pc newRam, dataInput = rest }
    _ -> throwE InsufficientInput

valSet :: Word8 -> BrainFuck ()
valSet byte = do
  Ptr pc ram <- getMem
  let newRam = runST $ do
        v <- V.thaw ram
        VM.write v pc byte
        V.freeze v
  setMem $ Ptr pc newRam

runLoop :: [Term] -> BrainFuck ()
runLoop terms =
  let prog = eval terms
      go p = do
        Ptr pc ram <- getMem
        let byte = ram V.! pc
        when (byte /= 0) (p >> go p)
  in prog `seq` go prog

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

eval :: [Term] -> BrainFuck ()
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
    Right ast -> run (eval ast)
    Left msg -> ((Left msg, emptyWorld), [])
  where
    inputBytes = map (fromIntegral . ord) stream
    run = runWriter . flip runStateT world . runExceptT
    world = defaultWorld { dataInput = inputBytes }
    emptyWorld = world { mem = Ptr 0 U.empty }

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
