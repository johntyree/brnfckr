

module Brnfckr.Eval where

import Control.Monad
import Data.Foldable
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Control.Monad.Writer.Strict
import qualified Data.Sequence as S
import Data.Char
import Data.Word
import Data.List
import Data.Maybe

import Brnfckr.Parse


type BrainFuck a = ExceptT BrainFuckError (StateT World (Writer Output)) a
data World = W { mem :: !Ptr, dataInput :: Input }
data Ptr = Ptr !Int (S.Seq Word8)
type Input = [Word8]
type Output = [Word8]


defaultPtr :: Ptr
defaultPtr = Ptr 0 (S.replicate 30000 0)

defaultWorld :: World
defaultWorld = W { mem = defaultPtr, dataInput = [] }


instance Show World where
  show (W (Ptr cur ram) i) =
    let (ls, centerRight) = S.splitAt cur ram
        p S.:< rs = S.viewl centerRight
        left = unwords . map show . reverse $ take 3 $ toList ls
        right = unwords . map show $ take 3 $ toList rs
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
  Ptr cur rs <- getMem
  setMem $ Ptr (cur + i) rs
ptrDecr i = ptrIncr (-i)

scanIncr, scanDecr :: BrainFuck ()
scanIncr = do
  Ptr cur ram <- getMem
  let centerRight = S.drop cur ram
      Just idx = S.elemIndexL 0 centerRight
  when (idx > 0) $ ptrIncr idx
scanDecr = do
  Ptr cur ram <- getMem
  let leftCenter = S.take (cur + 1) ram
      Just idx = S.elemIndexR 0 leftCenter
  when (idx > 0) $ ptrDecr idx

valIncr, valDecr :: Word8 -> BrainFuck ()
valIncr i = do
  Ptr cur ram <- getMem
  setMem $ Ptr cur (S.adjust (+ i) cur ram)
valDecr i = do
  Ptr cur ram <- getMem
  setMem $ Ptr cur (S.adjust (subtract i) cur ram)

valOutput :: BrainFuck ()
valOutput = do
  Ptr cur ram <- getMem
  tell [S.index ram cur]

valInput :: BrainFuck ()
valInput = do
  W { mem = Ptr cur r, dataInput = i } <- lift get
  case i of
    (byte:rest) -> let ram = S.update cur byte r
                    in lift $ put W { mem = Ptr cur ram, dataInput = rest }
    _ -> throwE InsufficientInput

valSet :: Word8 -> BrainFuck ()
valSet byte = lift $ modify $ \w@W { mem = Ptr cur r } ->
  let ram = S.update cur byte r
   in w { mem = Ptr cur ram }

runLoop :: [Term] -> BrainFuck ()
runLoop terms =
  let prog = eval terms
      go p = do
        Ptr cur ram <- getMem
        let b = S.index ram cur
        when (b /= 0) (p >> go p)
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
    emptyWorld = world { mem = Ptr 0 S.empty }

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
