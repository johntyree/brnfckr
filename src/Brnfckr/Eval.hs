

module Brnfckr.Eval where


import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Control.Monad.Writer.Strict
import Data.Char
import Data.Word
import Data.List
import Data.Maybe

import Brnfckr.Parse


type BrainFuck a = ExceptT BrainFuckError (StateT World (Writer Output)) a
data World = W { mem :: Ptr, dataInput :: Input }
data Ptr = Ptr [Word8] Word8 [Word8]
type Input = [Word8]
type Output = [Word8]


instance Show World where
  show (W (Ptr ls p rs) i) =
    let left = unwords . map show . reverse $ take 3 ls
        right = unwords . map show $ take 3 rs
        brain = unwords ["[", left, "", show p, "", right, "]"]
    in unwords [ "World {"
               , brain
               , show i
               , "}"
               ]

getMem :: BrainFuck Ptr
getMem = lift $ mem <$> get

setMem :: Ptr -> BrainFuck ()
setMem p = lift $ modify $ \w -> w { mem = p }

ptrIncr, ptrDecr :: Int -> BrainFuck ()
ptrIncr 0 = return ()
ptrIncr i = do
  Ptr ls p rs <- getMem
  let (shifted, p':rs') = splitAt (i-1) rs
      ls' = reverse shifted ++ (p : ls)
  setMem $ Ptr ls' p' rs'

ptrDecr 0 = return ()
ptrDecr i = do
  Ptr ls p rs <- getMem
  let (shifted, p':ls') = splitAt (i-1) ls
      rs' = reverse shifted ++ (p : rs)
  setMem $ Ptr ls' p' rs'

scanIncr, scanDecr :: BrainFuck ()
scanIncr = do
  Ptr _ p rs <- getMem
  let idx = fromJust (elemIndex 0 (p:rs))
  when (idx > 0) $ ptrIncr idx
scanDecr = do
  Ptr ls p _ <- getMem
  let idx = fromJust (elemIndex 0 (p:ls))
  when (idx > 0) $ ptrDecr idx

valIncr, valDecr :: Word8 -> BrainFuck ()
valIncr i = do
  Ptr ls p rs <- getMem
  setMem $ Ptr ls (p + i) rs
valDecr i = do
  Ptr ls p rs <- getMem
  setMem $ Ptr ls (p - i) rs

valOutput :: BrainFuck ()
valOutput = do
  Ptr _ p _ <- getMem
  tell [p]

valInput :: BrainFuck ()
valInput = do
  W { mem = Ptr ls _ rs, dataInput = i } <- lift get
  case i of
    (byte:rest) -> lift $ put W { mem = Ptr ls byte rs, dataInput = rest }
    _ -> throwE InsufficientInput

valSet :: Word8 -> BrainFuck ()
valSet i = lift $ modify $ \w@(W {mem = Ptr l _ r}) ->
  w { mem = Ptr l i r }

runLoop :: [Term] -> BrainFuck ()
runLoop terms =
  let prog = eval terms
      go p = do
        Ptr _ b _ <- getMem
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

runBrainFuck :: String -> String -> ((Either BrainFuckError (), World), String)
runBrainFuck source stream =
  case parseBrainFuck source of
    Right terms -> render <$> run (eval (compress terms))
    Left e -> parseFail e
  where
    parseFail msg = ((Left msg, world { mem = Ptr [] 0 [] }), "")
    run = runWriter . flip runStateT world . runExceptT
    world = W { mem = initPtr, dataInput = inputBytes }
    initPtr = Ptr (repeat 0) 0 (repeat 0)
    toChars = map (chr . fromIntegral)
    inputBytes = map (fromIntegral . ord) stream
    render = toChars

executeString :: String -> String -> Maybe String
executeString source stream = case runBrainFuck source stream of
  ((Left _, _), _) -> Nothing
  ((_, _), s) -> Just s


