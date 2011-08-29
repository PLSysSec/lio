{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LambdaChair ( evalReviewDC
                   , addUser
                   , addPaper
                   , addConflict
                   , addAssignment
                   , asUser
                   ---
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
import Control.Monad.State
import Data.Maybe
import Data.List
import Data.Monoid

import LIO.TCB
import LIO.LIORef
import LIO.MonadLIO
import LIO.MonadCatch (throwIO)
import LIO.DCLabel
import DCLabel.Safe
import DCLabel.TCB (Disj(..), Conj(..), Label(..))
import DCLabel.PrettyShow


type ErrorStr  = String
type DCLabeled = Labeled DCLabel
type DCRef     = LIORef DCLabel

-- ^ Clss to with sideffectful show
class DCShowTCB s where
  dcShowTCB :: s -> DC String

dcPutStrLnTCB :: String -> DC ()
dcPutStrLnTCB = ioTCB . putStrLn

dcGetLineTCB :: DC String
dcGetLineTCB = ioTCB getLine

type Name = String
type Password = String
type Content = String
type Reviews = String

type Id     = Int
data Paper  = Paper  Content
data Review = Review Content

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


-- State related
data ReviewState = ReviewState { users :: [User]
                               , reviewEntries :: [ReviewEnt]
                               , curUser :: Maybe Name }

emptyReviewState :: ReviewState
emptyReviewState = ReviewState [] [] Nothing

newtype ReviewDC a = ReviewDC (StateT ReviewState DC a)
  deriving (Monad, MonadFix)

liftReviewDC :: DC a -> ReviewDC a
liftReviewDC = ReviewDC . liftLIO

get' :: ReviewDC ReviewState
get' = ReviewDC . StateT $ \s -> return (s,s)

put' :: ReviewState -> ReviewDC ()
put' s = ReviewDC . StateT $ \_ -> return ((),s)

runReviewDC :: ReviewDC a -> ReviewState -> DC (a, ReviewState)
runReviewDC (ReviewDC m) s = runStateT m s

evalReviewDC :: ReviewDC a -> IO (a, DCLabel)
evalReviewDC m = evalDC $ do
  (a, s') <- runReviewDC m emptyReviewState
  return a
--

-- ^ Get all users
getUsers :: ReviewDC [User]
getUsers = get' >>= return . users

-- ^ Get all review entries
getReviews :: ReviewDC [ReviewEnt]
getReviews = get' >>= return . reviewEntries

-- ^ Get priviliges
getCurUserName :: ReviewDC (Maybe Name)
getCurUserName = get' >>= return . curUser

-- ^ Get curren tuser name
getCurUser :: ReviewDC (Maybe User)
getCurUser = do
  n <- getCurUserName
  maybe (return Nothing) findUser n

-- ^ Get priviliges
getPrivs :: ReviewDC DCPrivTCB
getPrivs = do 
  u <- getCurUser
  return $ maybe mempty (genPrivTCB . name) u

-- ^ Write new users
putUsers :: [User] -> ReviewDC ()
putUsers us = do
  rs <- getReviews
  u <- getCurUserName
  put' $ ReviewState us rs u

-- ^ Write new reviews
putReviews :: [ReviewEnt] -> ReviewDC ()
putReviews rs = do
  us <- getUsers
  u <- getCurUserName
  put' $ ReviewState us rs u

-- ^ Write new privs
putCurUserName :: Name -> ReviewDC ()
putCurUserName u = do
  us <- getUsers
  rs <- getReviews
  put' $ ReviewState us rs (Just u)

-- ^ Clear user naem
clearCurUserName :: ReviewDC ()
clearCurUserName = do
  us <- getUsers
  rs <- getReviews
  put' $ ReviewState us rs Nothing

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
                       , password = p
                       , conflicts = []
                       , assignments = [] }
    us <- getUsers
    putUsers (newUser:us)

-- ^ Add conflicting paper to user
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

-- ^ Assign a paper for the user to review
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

-- ^ Print users
printUsersTCB :: ReviewDC ()
printUsersTCB = do
 users <- getUsers
 mapM (liftReviewDC . dcShowTCB) users >>=
   reviewDCPutStrLnTCB . (intercalate "\n--\n")

-- ^ Print papers and reviews
printReviewsTCB :: ReviewDC ()
printReviewsTCB = do
 reviews <- getReviews
 mapM (liftReviewDC . dcShowTCB) reviews >>=
   reviewDCPutStrLnTCB . (intercalate "\n--\n")

-- | Generate privilege from a string
genPrivTCB :: NewPriv a => a -> DCPrivTCB
genPrivTCB = mintTCB . newPriv 

-- ^ Create new paper given id and content
newReviewEnt :: Id -> Content -> ReviewDC ReviewEnt
newReviewEnt pId content = do
  let p1 = "Paper" ++ (show pId)
      r1 = "Review" ++ (show pId)
      pLabel = newDC (<>) p1 
      rLabel = newDC r1 r1 
      privs = genPrivTCB (p1 ./\. r1)
  liftReviewDC $ do
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
                        priv = genPrivTCB (listToLabel $ map id2cat as)
                    in  doReadPaper priv rev
       where doReadPaper priv rev = liftReviewDC $ do
                 (Paper lPaper) <- readLIORefP priv (paper rev)
                 return (Right lPaper)
             id2cat i = MkDisj [principal $ "Review"++(show i)]

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
                    Just u -> doReadReview rev
   where doReadReview rev = liftReviewDC $ do
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
    Nothing -> liftReviewDC $ throwIO (ErrorCall "No user is logged in.")
    Just u -> do
      as <- getReviews >>= return . map paperId -- all reviews
      let cs = conflicts u -- conflicting reviews
          c_cat = map id2conf_cat (cs) -- conflicting categories
          nc_cat = map id2cat (as \\ cs) -- noconflicting categories
      return $ newDC (listToLabel $ c_cat ++ nc_cat) (<>)
        where id2cat i = MkDisj [ principal $ "Review"++(show i)]
              id2conf_cat i = MkDisj [ principal $ "Review" ++ (show i)
                                     , principal $ "CONFLICT" ]
          
-- ^ Print if the current label flows to the output channel label, i.e.,
-- there is no conflict of interest.
dcPutStrLn :: DCLabel -> Content -> DC ()
dcPutStrLn lo cont = do
  l <- getLabel
  if l `leq` lo
    then dcPutStrLnTCB cont
    else throwIO . ErrorCall $ "Trying to print conflicting review:\n" ++
                               (prettyShow l) ++ " [/= " ++ (prettyShow lo)

-- ^ Main printing function. Print to a labeled output channel.
reviewDCPutStrLn :: String -> ReviewDC ()
reviewDCPutStrLn s = do 
  l <- getOutputChLbl
  liftReviewDC $ dcPutStrLn l $ "-> "++ s

reviewDCPutStrLnTCB :: String -> ReviewDC ()
reviewDCPutStrLnTCB = liftReviewDC . dcPutStrLnTCB

-- ^ Given a paper number and review content append to the current review.
appendToReview :: Id -> Content -> ReviewDC (Either ErrorStr ())
appendToReview pId content = do
  mRev <- findReview pId 
  case mRev of 
    Nothing -> return $ Left "Invalid Id"
    Just rev -> do privs <- getPrivs
                   doWriteReview privs rev content
                   return $ Right ()
   where doWriteReview privs rev content = liftReviewDC $ do
           toLabeledP privs ltop $ do
             (Review rs) <- readLIORef (review rev)
             -- restrict writes: 
             writeLIORef (review rev) (Review (rs++content))

-- ^ Set the current label to the assignments
assign2curLabel :: [Id] -> ReviewDC() 
assign2curLabel as = liftReviewDC $ do
  let l = newDC (<>) (listToLabel $ map id2cat as)
  setLabelTCB l
        where id2cat i = MkDisj [principal $ "Review"++(show i)]
  
-- ^ Safely execute untrusted code
safeExecTCB :: ReviewDC () -> ReviewDC ()
safeExecTCB m = do
  s <- get'
  s' <- liftReviewDC $ do
    cc <- getClearance
    cl <- getLabel
    (_, s') <- (runReviewDC m s) `catch` 
                  (\(e::SomeException) -> do
                        dcPutStrLnTCB "-> ERROR: IFC violated\n"
                        return ((), s))
    lowerClrTCB cc
    setLabelTCB cl
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
      p <- liftReviewDC dcGetLineTCB
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
          simpleMatch s ent = do
            (Paper pap)  <- liftReviewDC $ readLIORefTCB (paper ent)
            if s `isPrefixOf` pap
              then return . Just $ paperId ent
              else return Nothing
