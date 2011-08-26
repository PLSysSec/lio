import LIO.TCB
import LIO.DCLabel
import LIO.LIO
import LambdaChair
import DCLabel.PrettyShow
import DCLabel.Safe

import AliceCode as Alice
import BobCode as Bob

dome = printL . evalDC $ do
  let p = mintTCB (singleton "P1") :: DCPrivTCB
      l = newDC (<>) ("P1")
      lcur = newDC (<>) ("R1" ./\. "R2")
  dcPutStrLnTCB . prettyShow $ p
  dcPutStrLnTCB . prettyShow $ l
  dcPutStrLnTCB . prettyShow $ lcur
  dcPutStrLnTCB . prettyShow $ lostar p l lcur

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
