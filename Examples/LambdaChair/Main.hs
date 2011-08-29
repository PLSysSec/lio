import LIO.TCB
import LIO.DCLabel
import LIO.LIO
import LambdaChair
import DCLabel.PrettyShow
import DCLabel.Safe

import AliceCode as Alice
import BobCode as Bob

printL m = do
  (_, l) <- m
  putStrLn . prettyShow $ l

main = printL . evalReviewDC $ do
  addUser "Alice" "password"
  
  p1 <- addPaper "Flexible Dynamic..."
  p2 <- addPaper "A Static..."
  
  addAssignment "Alice" p1
  addAssignment "Alice" p2
  
  asUser "Alice" $ Alice.mainReview
  
  addUser "Bob" "password"
  
  addAssignment "Bob" p2
  addConflict "Bob" p1

  asUser "Bob" $ Bob.mainReview
