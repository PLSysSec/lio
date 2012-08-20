{-# LANGUAGE Unsafe #-}
{-# LANGUAGE OverloadedStrings,
             MultiParamTypeClasses,
             GeneralizedNewtypeDeriving,
             ScopedTypeVariables #-}

{- | 

Basic review system API.

Must compile LIO with @--flags="toLabeled"@.

This is a prototype/toy implementation.  A more serious implementation
will be implemented using the Hails framework.

-}

module LambdaChair.TCB (
  --- * Admin actions
    runReviewDC
  , emptyReviewState
  , addUser
  , addPaper
  , addConflict
  , addAssignment
  , asUser
  --- * User actions
  , findPaper
  , retrievePaper, readPaper
  , retrieveReview, readReview
  , appendToReview
  , reviewDCPutStrLn 
  -- TCB
  , printUsersTCB
  , printReviewsTCB
  , reviewDCPutStrLnTCB 
  , dcPutStrLnTCB 
  ) where

import Prelude hiding (catch)
import Control.Monad
import Control.Exception (SomeException, ErrorCall(..))
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Maybe
import Data.List

import LIO
import LIO.TCB
import LIO.Privs.TCB
import LIO.LIORef
import LIO.LIORef.TCB (readLIORefTCB)
import LIO.DCLabel
import LIO.DCLabel.Privs.TCB

import qualified Data.ByteString.Char8 as C


type ErrorStr  = String

-- | Class with sideffectful show
class DCShowTCB s where
  dcShowTCB :: s -> DC String

-- | Print to standard output
dcPutStrLnTCB :: String -> DC ()
dcPutStrLnTCB = ioTCB . putStrLn

-- | Read from standard input
dcGetLineTCB :: DC String
dcGetLineTCB = ioTCB getLine

-- | A name
type Name      = String
-- | A password
type Password  = String
-- | Paper/Rewview content
type Content   = String
-- | A review log
type ReviewLog = String

-- | Paper id
type Id     = Int
-- | A paper is just a wrapper for its contents
data Paper  = Paper  Content
-- | A review is just a wrapper for its contents
data Review = Review ReviewLog

-- | A user contains a name, password a list of conflicting papers and
-- list of papers to review (i.e., assignments).
data User = User { name :: Name
                 , password :: Password
                 , conflicts :: [Id]
                 , assignments :: [Id] }

instance Eq User where
  u1 == u2 = name u1 == name u2

instance DCShowTCB User where
  dcShowTCB u = do
    return $  "Name: " ++ (name u) ++ "\n"
           ++ "Password: " ++ (password u) ++ "\n"
           ++ "Conflicts: " ++ (show . conflicts $ u) ++ "\n"
           ++ "Assignments: " ++ (show . assignments $ u)

-- | Areview entry contains the paper id, paper contents and review
-- log.
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
           ++ "\nPaper:" ++ pap
           ++ "\nReviews:" ++ rev


-- Internal state of the 'ReviewDC' monad.
data ReviewState = ReviewState { users :: [User]
                               , reviewEntries :: [ReviewEnt]
                               , curUser :: Maybe Name }

-- | Emtpy state.
emptyReviewState :: ReviewState
emptyReviewState = ReviewState [] [] Nothing

-- | Monad in which all review actions are executed.
newtype ReviewDC a = ReviewDC (StateT ReviewState DC a)
  deriving (Monad)

-- | Lift a 'DC' into thew 'ReviewDC' monad.
instance MonadLIO DCLabel ReviewDC where
  liftLIO = ReviewDC . lift

-- | Get internal state
get' :: ReviewDC ReviewState
get' = ReviewDC . StateT $ \s -> return (s,s)

-- | Update internal state
put' :: ReviewState -> ReviewDC ()
put' s = ReviewDC . StateT $ \_ -> return ((),s)

-- | Execute a review action
runReviewDC :: ReviewDC a -> ReviewState -> DC (a, ReviewState)
runReviewDC (ReviewDC m) s = runStateT m s

--

-- | Get all users
getUsers :: ReviewDC [User]
getUsers = users `liftM` get'

-- | Get all review entries
getReviews :: ReviewDC [ReviewEnt]
getReviews = reviewEntries `liftM` get'

-- | Get priviliges
getCurUserName :: ReviewDC (Maybe Name)
getCurUserName = curUser `liftM` get'

-- | Get current user name
getCurUser :: ReviewDC (Maybe User)
getCurUser = do
  n <- getCurUserName
  maybe (return Nothing) findUser n

-- | Get priviliges of the user executing the action
getPrivs :: ReviewDC DCPriv
getPrivs = do 
  u <- getCurUser
  return $ maybe noPriv (mintTCB . dcPrivDesc . name) u

-- | Updat users
putUsers :: [User] -> ReviewDC ()
putUsers us = do
  rs <- getReviews
  u <- getCurUserName
  put' $ ReviewState us rs u

-- | Update reviews
putReviews :: [ReviewEnt] -> ReviewDC ()
putReviews rs = do
  us <- getUsers
  u <- getCurUserName
  put' $ ReviewState us rs u

-- | Set current user
putCurUserName :: Name -> ReviewDC ()
putCurUserName u = do
  us <- getUsers
  rs <- getReviews
  put' $ ReviewState us rs (Just u)

-- | Remove current user
clearCurUserName :: ReviewDC ()
clearCurUserName = do
  us <- getUsers
  rs <- getReviews
  put' $ ReviewState us rs Nothing

-- | Find review entry by id
findReview :: Id -> ReviewDC (Maybe ReviewEnt)
findReview pId = do
  reviews <- getReviews
  return $ find (\e -> paperId e == pId) reviews

-- | Find user by name
findUser :: Name -> ReviewDC (Maybe User)
findUser n = do
  us <- getUsers
  return $ find (\u -> name u == n) us

-- | Add new (fresh) user
addUser :: Name -> Password -> ReviewDC ()
addUser n p = do
  u <- findUser n
  unless (isJust u) $ do
    let newUser = User { name = n
                       , password = p
                       , conflicts = []
                       , assignments = [] }
    us <- getUsers
    putUsers (newUser:us)

-- | Add conflicting paper to user
addConflict :: Name -> Id -> ReviewDC ()
addConflict n i = do
  usr <- findUser n
  pap <- findReview i
  case (usr, pap) of
    (Just u, Just _) -> 
      if i `elem` (assignments u)
        then return ()
        else do let u' = u { conflicts = i : (conflicts u)}
                usrs <- getUsers
                putUsers $ u' : (filter (/= u) usrs)
    _ -> return ()

-- | Assign a paper for the user to review
addAssignment :: Name -> Id -> ReviewDC ()
addAssignment n i = do
  usr <- findUser n
  pap <- findReview i
  case (usr, pap) of
    (Just u, Just _) -> 
      if i `elem` (conflicts u)
        then return ()
        else do let u' = u { assignments = i : (assignments u)}
                usrs <- getUsers
                putUsers $ u' : (filter (/= u) usrs)
    _ -> return ()

-- | Print users
printUsersTCB :: ReviewDC ()
printUsersTCB = do
 us <- getUsers
 mapM (liftLIO . dcShowTCB) us >>=
   reviewDCPutStrLnTCB . (intercalate "\n--\n")

-- ^ Print papers and reviews
printReviewsTCB :: ReviewDC ()
printReviewsTCB = do
 reviews <- getReviews
 mapM (liftLIO . dcShowTCB) reviews >>=
   reviewDCPutStrLnTCB . (intercalate "\n--\n")

-- ^ Create new paper given id and content
newReviewEnt :: Id -> Content -> ReviewDC ReviewEnt
newReviewEnt pId content = do
  let p1 = toComponent $ "Paper" ++ (show pId)
      r1 = toComponent $ "Review" ++ (show pId)
      pLabel = dcLabel dcTrue p1 
      rLabel = dcLabel r1 r1 
      privs = mintTCB $ dcPrivDesc (p1 /\ r1)
  liftLIO $ do
    rPaper  <- newLIORefP privs pLabel (Paper content)
    rReview <- newLIORefP privs rLabel (Review "")
    return $ ReviewEnt pId rPaper rReview

-- ^ Adda new paper to be reviewed
addPaper :: Content -> ReviewDC Id
addPaper content = do
  reviews <- getReviews
  let pId = 1 + (length reviews)
  ent <- newReviewEnt pId content
  putReviews (ent:reviews)
  return pId


-- ^ Given a paper number return the paper
retrievePaper :: Id -> ReviewDC (Either ErrorStr Content)
retrievePaper pId = do
  mu <- getCurUser
  case mu of
    Nothing -> return $ Left "Need to be logged in"
    Just u -> do
      mRev <- findReview pId 
      case mRev of 
        Nothing -> return $ Left "Invalid Id"
        Just rev -> let as = assignments u
                        priv = mintTCB . fromList $ map id2cat as
                    in  doReadPaper priv rev
       where doReadPaper priv rev = liftLIO $ do
                 (Paper lPaper) <- readLIORefP priv (paper rev)
                 return (Right lPaper)
             id2cat i = [principal . C.pack $ "Review"++(show i)]

-- ^ Given a paper number print the paper
-- NOTE: in the paper, the functionality of @readPaper@ corresponds to
-- that of @retrievePaper@; here, we print out the content.
readPaper :: Id -> ReviewDC ()
readPaper i = retrievePaper i >>= \r -> reviewDCPutStrLn $ show r


-- ^ Given a paper/review number return the review, if the entry exists
retrieveReview :: Id -> ReviewDC (Either ErrorStr Content)
retrieveReview pId = do
  mRev <- findReview pId 
  case mRev of 
    Nothing -> return $ Left "Invalid Id"
    Just rev -> do mu <- getCurUser
                   case mu of
                    Nothing -> return $ Left "Must login first"
                    Just _ -> doReadReview rev
   where doReadReview rev = liftLIO $ do
             (Review r) <- readLIORef (review rev)
             return (Right r)

-- ^ Given a paper/review number print the review, if the entry exists
readReview :: Id -> ReviewDC ()
readReview i = retrieveReview i >>= \r -> reviewDCPutStrLn $ show r

-- ^ Computer the label of the output' channel
getOutputChLbl :: ReviewDC (DCLabel) 
getOutputChLbl = do
  mu <- getCurUser
  case mu of
    Nothing -> liftLIO $ throwLIO (ErrorCall "No user is logged in.")
    Just u -> do
      as <- getReviews >>= return . map paperId -- all reviews
      let cs = conflicts u -- conflicting reviews
          c_cat = map id2conf_cat (cs) -- conflicting categories
          nc_cat = map id2cat (as \\ cs) -- noconflicting categories
      return $ dcLabel (fromList $ c_cat ++ nc_cat) dcTrue
        where id2cat i = [ principal . C.pack $ "Review"++(show i)]
              id2conf_cat i = [ principal . C.pack $ "Review" ++ (show i)
                              , principal $ "CONFLICT" ]
          
-- ^ Print if the current label flows to the output channel label, i.e.,
-- there is no conflict of interest.
dcPutStrLn :: DCLabel -> Content -> DC ()
dcPutStrLn lo cont = do
  l <- getLabel
  if l `canFlowTo` lo
    then dcPutStrLnTCB cont
    else throwLIO . ErrorCall $ "Trying to print conflicting review:\n" ++
                               (show l) ++ " [/= " ++ (show lo)

-- ^ Main printing function. Print to a labeled output channel.
reviewDCPutStrLn :: String -> ReviewDC ()
reviewDCPutStrLn s = do 
  l <- getOutputChLbl
  liftLIO $ dcPutStrLn l $ "-> "++ s

-- | Print line to standard output
reviewDCPutStrLnTCB :: String -> ReviewDC ()
reviewDCPutStrLnTCB = liftLIO . dcPutStrLnTCB

-- ^ Given a paper number and review content append to the current review.
appendToReview :: Id -> Content -> ReviewDC (Either ErrorStr ())
appendToReview pId content = do
  mRev <- findReview pId 
  case mRev of 
    Nothing -> return $ Left "Invalid Id"
    Just rev -> do privs <- getPrivs
                   _ <- doWriteReview privs rev
                   return $ Right ()
   where doWriteReview privs rev = liftLIO $ do
           toLabeledP privs top $ do
             (Review rs) <- readLIORef (review rev)
             -- restrict writes: 
             writeLIORef (review rev) (Review (rs++content))

-- ^ Set the current label to the assignments
assign2curLabel :: [Id] -> ReviewDC() 
assign2curLabel as = liftLIO $ do
  let l = dcLabel dcTrue (fromList $ map id2cat as)
  setLabelP allPrivTCB l
        where id2cat i = [principal . C.pack $ "Review"++(show i)]
  
-- ^ Safely execute untrusted code
safeExecTCB :: ReviewDC () -> ReviewDC ()
safeExecTCB m = do
  s <- get'
  s' <- liftLIO $ do
    cc <- getClearance
    cl <- getLabel
    (_, s') <- (runReviewDC m s) `catchLIO` 
                  (\(_::SomeException) -> do
                        dcPutStrLnTCB "-> ERROR: IFC violated\n"
                        return ((), s))
    setClearanceP allPrivTCB cc
    setLabelP allPrivTCB cl
    return s'
  put' s'

-- ^ Execute on behalf of user
asUser :: Name -> ReviewDC a -> ReviewDC ()
asUser n m = do
  putCurUserName n
  mu <- getCurUser
  case mu of
    Nothing -> return ()
    Just u -> do
      reviewDCPutStrLnTCB $ "| Hi, "++ (name u)++".\n| Password>"
      p <- liftLIO dcGetLineTCB
      if p /= (password u)
        then reviewDCPutStrLnTCB "| Failed, try again" >> asUser n m
        else do
          reviewDCPutStrLnTCB $ "| Executing on behalf of "++(name u)++"...\n"
          safeExecTCB $ assign2curLabel (assignments u) >> m >> return ()
          clearCurUserName

-- | Given a paper prefix return either an error string, if the paper cannot be
-- found or the paper id.
findPaper :: String -> ReviewDC (Either ErrorStr Id)
findPaper s = do
  revs <- getReviews
  res <- mapM (simpleMatch s) revs >>= return . find isJust
  case res of
    Nothing -> return . Left $ "Could not find paper"
    Just i -> return . Right $ fromJust i
      -- ^ Fing paper by checking for prefix
    where simpleMatch :: String -> ReviewEnt -> ReviewDC (Maybe Id)
          simpleMatch m ent = do
            (Paper pap)  <- liftLIO $ readLIORefTCB (paper ent)
            if m `isPrefixOf` pap
              then return . Just $ paperId ent
              else return Nothing
