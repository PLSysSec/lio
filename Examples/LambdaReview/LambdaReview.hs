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
data Review = Review (DCLabeled Content)

data User = User { name :: Name
                 , password :: Password }

instance Eq User where
  u1 == u2 = name u1 == name u2

instance DCShowTCB User where
  dcShowTCB u = do
    return $  "Name: " ++ (name u) ++ "\n"
           ++ "Password: " ++ (password u)

data ReviewEnt =  ReviewEnt { paperId :: Id
                            , paper   :: DCRef Paper
                            , review  :: DCRef Review }

instance Eq ReviewEnt where
  r1 == r2 = paperId r1 == paperId r2

instance DCShowTCB ReviewEnt where
  dcShowTCB r = do
    (Paper pap)  <- readLIORefTCB (paper r)
    (Review rev) <- readLIORefTCB (review r)
    return $  "ID:" ++ (show . paperId $ r)
           ++ "\nPaper:" ++ (showTCB pap)
           ++ "\nReviews:" ++ (showTCB $ rev)


-- State related
data ReviewState = ReviewState { users :: [User]
                               , reviewEntries :: [ReviewEnt]
                               , privs :: DCPrivs }

emptyReviewState :: ReviewState
emptyReviewState = ReviewState [] [] mempty

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
getUsers = get >>= return . users

-- ^ Get all review entries
getReviews :: ReviewDC [ReviewEnt]
getReviews = get >>= return . reviewEntries

-- ^ Get priviliges
getPrivs :: ReviewDC DCPrivs
getPrivs = get >>= return . privs

-- ^ Write new users
putUsers :: [User] -> ReviewDC ()
putUsers us = do
  rs <- getReviews
  p <- getPrivs
  put $ ReviewState us rs p

-- ^ Write new reviews
putReviews :: [ReviewEnt] -> ReviewDC ()
putReviews rs = do
  us <- getUsers
  p <- getPrivs
  put $ ReviewState us rs p

-- ^ Write new privs
putPrivs :: DCPrivs -> ReviewDC ()
putPrivs p = do
  us <- getUsers
  rs <- getReviews
  put $ ReviewState us rs p

-- ^ Find review entry by id
findReview :: Id -> ReviewDC (Maybe ReviewEnt)
findReview pId = do
  reviews <- getReviews
  return $ find (\e -> paperId e == pId) reviews

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


-- ^ Create new paper given id and content
newReviewEnt :: Id -> Content -> ReviewDC ReviewEnt
newReviewEnt pId content = do
  let emptyLabel = exprToDCLabel NoPrincipal NoPrincipal 
      p1 = "Paper" ++ (show pId)
      r1 = "Review" ++ (show pId)
      emptyLabel2 = exprToDCLabel (("Alice" .\/. "Bob") ./\. r1)
                                  (("Alice" .\/. "Bob") ./\. r1)  -- TODO: CHANGE!!
      pLabel = exprToDCLabel p1 NoPrincipal
      rLabel = exprToDCLabel r1 r1
      privs = mconcat $ map genPrivTCB [p1, r1, "Alice"]
  liftLIO $ do
    lPaperCont <- lrefPD privs emptyLabel content
    lEmptyRev  <- lrefPD privs emptyLabel2 "" -- TODO: CHANGE!!
    rPaper  <- newLIORefP privs pLabel (Paper lPaperCont)
    rReview <- newLIORefP privs rLabel (Review lEmptyRev)
    return $ ReviewEnt pId rPaper rReview

-- ^ Adda new paper to be reviewed
addPaper :: Content -> ReviewDC ()
addPaper content = do
  reviews <- getReviews
  let pId = 1 + (length reviews)
  ent <- newReviewEnt pId content
  putReviews (ent:reviews)

-- ^ Given a paper number return the paper
readPaper :: Id -> ReviewDC (Either String Content)
readPaper pId = do
  mRev <- findReview pId 
  case mRev of 
    Nothing -> return $ Left "Invalid Id"
    Just rev -> do p <- doReadPaper rev
                   return $ Right p
   where doReadPaper rev = liftLIO $ do
             (Paper lPaper) <- readLIORef (paper rev)
             openRD lPaper

-- ^ Given a paper number return the review
readReview :: Id -> ReviewDC (Either String Content)
readReview pId = do
  mRev <- findReview pId 
  case mRev of 
    Nothing -> return $ Left "Invalid Id"
    Just rev -> do r <- doReadReview rev
                   return $ Right r
   where doReadReview rev = liftLIO $ do
             (Review lReview) <- readLIORef (review rev)
             openRD lReview

withNoCreepP :: DCPrivs -> DC a -> DC a
withNoCreepP p m = do
  curL <- currentLabel
  res <- m
  curL' <- currentLabel
  setLabelP p (lostar p curL' curL)
  return res

appendToReview :: Id -> Content -> ReviewDC (Either String ())
appendToReview pId content = do
  mRev <- findReview pId 
  case mRev of 
    Nothing -> return $ Left "Invalid Id"
    Just rev -> do privs <- getPrivs
                   doWriteReview privs rev content
                   return $ Right ()
   where doWriteReview privs rev content = liftLIO $ do
             (l, curCont) <- withNoCreepP privs $ do 
               (Review lReview) <- readLIORef (review rev)
               r <- openRD lReview
               return (labelOfRD lReview, r)
             lReview' <- lrefD l (curCont++content)
             writeLIORef (review rev) (Review lReview')

main = evalReviewDC $ do
  addUser "deian" "password"
  addUser "alejandro" "pass"
  addPaper "Paper content"
  addPaper "Another paper content"
--  liftLIO $ setLabelTCB aliceLabel
  let u = "Bob"
  putPrivs $ genPrivTCB u
  liftLIO . setLabelTCB $ exprToDCLabel NoPrincipal (u ./\. "Review1")
  liftLIO . setClearanceTCB $ exprToDCLabel (u ./\. "Review1") NoPrincipal
  lc <- liftLIO $ currentLabel
  liftLIO . dcPutStrLnTCB $ "CurLabel = " ++ (show lc)
  appendToReview 1 "good"
--  readPaper 1 >>= liftLIO . dcPutStrLnTCB . show
--  readPaper 2 >>= liftLIO . dcPutStrLnTCB . show
--  printUsersTCB
--  printReviewsTCB



-- Add paper
-- Add reviewer: can read any paper or non-conflicting review 
-- Add conflict: should prevent a reviewer from reading the reviews
-- Remove conflict: should now allow the reviewer to read reviews
-- Assign reviewer: assing a reviewer to a paper
-- Login
