module LambdaReview where

import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.List

import LIO.TCB
import LIO.LIORef
import LIO.DCLabel
import LIO.DCLabel.NanoEDSL

type DCLabeled = LrefD DCLabel
type DCRef     = LIORef DCLabel

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

data ReviewEnt =  ReviewEnt { paperId :: Id
                            , paper   :: DCRef Paper
                            , review  :: DCRef Review
                            }

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

-- ^ Add user
addUser :: Name -> Password -> ReviewDC ()
addUser n p = do
  u <- findUser n
  unless (isJust u) $ do
    let newUser = User { name = n
                       , password = p}
    us <- getUsers
    putUsers (newUser:us)






main = do
  (a,_) <- evalReviewDC $
    return ()
  return a



-- Add paper
-- Add reviewer: can read any paper or non-conflicting review 
-- Add conflict: should prevent a reviewer from reading the reviews
-- Remove conflict: should now allow the reviewer to read reviews
-- Assign reviewer: assing a reviewer to a paper
-- Login
