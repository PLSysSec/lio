module LambdaReview where

import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.List
import Data.Monoid

import LIO.TCB
import LIO.LIORef
import LIO.MonadLIO
import LIO.DCLabel
import LIO.DCLabel.NanoEDSL

type DCLabeled = LrefD DCLabel
type DCRef     = LIORef DCLabel

-- ^ Clss to with sideffectful show
class DCShowTCB s where
  dcShowTCB :: s -> DC String

dcPutStrLnTCB :: String -> DC ()
dcPutStrLnTCB = ioTCB . putStrLn

type Name = String
type Password = String
type Content = String
type Reviews = String

type Id     = Int
data Paper  = Paper  (DCLabeled Content)
data Review = Review { content   :: DCLabeled Content
                     , conflicts :: [User]
                     }

data User = User { name :: Name
                 , password :: Password
                 }

instance DCShowTCB User where
  dcShowTCB u = do
    return $  "Name: " ++ (name u) ++ "\n"
           ++ "Password: " ++ (password u)

data ReviewEnt =  ReviewEnt { paperId :: Id
                            , paper   :: DCRef Paper
                            , review  :: DCRef Review
                            }

instance DCShowTCB ReviewEnt where
  dcShowTCB r = do
    (Paper pap) <- readLIORefTCB (paper r)
    rev <- readLIORefTCB (review r)
    return $  "ID:" ++ (show . paperId $ r)
           ++ "\nPaper:" ++ (showTCB pap)
           ++ "\nReviews:" ++ (showTCB . content $ rev)
           ++ "\nConflicts: [" ++ (show ((map name) . conflicts $ rev)) ++ "]"


-- State related
type ReviewState = ([User], [ReviewEnt])

emptyReviewState :: ReviewState
emptyReviewState = ([],[])

type ReviewDC = StateT ReviewState DC 

runReviewDC :: ReviewDC a -> ReviewState -> DC (a, ReviewState)
runReviewDC m s = runStateT m s

evalReviewDC :: ReviewDC a -> IO (a, DCLabel)
evalReviewDC m = evalDC $ do
  (a, s') <- runReviewDC m emptyReviewState
  return a
--

-- ^ Get all users
getUsers :: ReviewDC [User]
getUsers = get >>= return . fst

-- ^ Get all review entries
getReviews :: ReviewDC [ReviewEnt]
getReviews = get >>= return . snd

-- ^ Write new users
putUsers :: [User] -> ReviewDC ()
putUsers us = do
  rs <- getReviews
  put (us, rs)

-- ^ Write new reviews
putReviews :: [ReviewEnt] -> ReviewDC ()
putReviews rs = do
  us <- getUsers
  put (us, rs)

-- ^ Find user by name
findUser :: Name -> ReviewDC (Maybe User)
findUser n = do
  users <- getUsers
  return $ find (\u -> name u == n) users 

-- ^ Add new (fresh) user
addUser :: Name -> Password -> ReviewDC ()
addUser n p = do
  u <- findUser n
  unless (isJust u) $ do
    let newUser = User { name = n
                       , password = p}
    us <- getUsers
    putUsers (newUser:us)

-- ^ Print users
printUsersTCB :: ReviewDC ()
printUsersTCB = do
 users <- getUsers
 mapM (liftLIO . dcShowTCB) users >>=
   liftLIO . dcPutStrLnTCB . (intercalate "\n--\n")

-- ^ Print papers and reviews
printReviewsTCB :: ReviewDC ()
printReviewsTCB = do
 reviews <- getReviews
 mapM (liftLIO . dcShowTCB) reviews >>=
   liftLIO . dcPutStrLnTCB . (intercalate "\n--\n")

-- | Generate privilege from a string
genPrivTCB :: String -> DCPrivs
genPrivTCB = mintTCB . Principal

-- ^ Make \"unforgeable\" secrecy principal
mkUSecPrincipal i = "__P" ++ (show i) ++ "__"

-- ^ Create new paper given id and content
newReviewEnt :: Id -> Content -> ReviewDC ReviewEnt
newReviewEnt pId content = do
  let emptyLabel = exprToDCLabel NoPrincipal NoPrincipal 
      p1  = "P" ++ show pId
      p1u = mkUSecPrincipal pId
      reviewLabel = exprToDCLabel p1u NoPrincipal
      rLabel = exprToDCLabel NoPrincipal p1
      privs = mconcat $ map genPrivTCB [p1, p1u]
  liftLIO $ do
    lPaperCont <- lrefPD privs emptyLabel content
    lEmptyRev  <- lrefPD privs reviewLabel ""
    rPaper  <- newLIORefP privs rLabel (Paper lPaperCont)
    rReview <- newLIORefP privs rLabel (Review lEmptyRev [])
    return $ ReviewEnt pId rPaper rReview

-- ^ Adda new paper to be reviewed
addPaper content = do
  reviews <- getReviews
  let pId = 1 + (length reviews)
  ent <- newReviewEnt pId content
  putReviews (ent:reviews)


main = evalReviewDC $ do
 addUser "deian" "password"
 addUser "alejandro" "pass"
 addPaper "Paper content"
 addPaper "Another paper content"
 printUsersTCB
 printReviewsTCB



-- Add paper
-- Add reviewer: can read any paper or non-conflicting review 
-- Add conflict: should prevent a reviewer from reading the reviews
-- Remove conflict: should now allow the reviewer to read reviews
-- Assign reviewer: assing a reviewer to a paper
-- Login
