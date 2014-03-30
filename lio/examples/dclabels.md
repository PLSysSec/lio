<style>
blockquote blockquote p { font-size: 10pt; }
</style>

In this post we're going to work through some exercises to internalize LIO+DCLabel concepts: current label, LIORefs, privileges, and clearance.

Throughout we are going to execute an `example` action that you will be asked to complete. Our `main` action, defined below, simply executes the `example` action with an initial current label and clearance (`initialLabels`).

```active-haskell
:name=main
:noexec

import Prelude hiding (catch)
import Data.Monoid
import Data.List (intercalate)
import Control.Monad (unless, forM)

import LIO
import LIO.LIORef
import LIO.DCLabel
import LIO.Concurrent

import LIO.Run
import LIO.TCB (ioTCB)

main = do
  result <- evalLIO example initialLabelState
  print result

fillIn :: String -> a 
fillIn msg = error $ "Missing: " ++ msg
```

### Current label and DC labels

First, we're going to look at how the current label with DC labels interacts with reads and writes without privileges.  Since reference are conceptually the simplest objects we can read and write to, our examples will revolve around them.

Hence, we're going to define a helper wrapper that creates two references and executes an action with these references curried.

```active-haskell
:requires=main
:name=withRefs
:noexec

withRefs :: DCLabel  -- Label of first  ref
         -> DCLabel  -- Label of second ref
         -> (LIORef DCLabel Int -> LIORef DCLabel Int -> DC a)
         -> DC a
withRefs l1 l2 act = do
  ref1 <- newLIORef l1 3
  ref2 <- newLIORef l2 4
  act ref1 ref2

initialLabelState ::  LIOState DCLabel
initialLabelState = LIOState { lioLabel     = True  %% False
                             , lioClearance = False %% True }
```

Here, we use an initial current label and clearance corresponding to the DC labels bottom (`True %% False`) and top (`False %% True`) lattice elements, respectively.

> > **Note:** In general you want to use `dcPublic` as the initial current label, since a high integrity current label (the `False`) allows code to create labeled objects of "high" integrity without using privileges.

When we combine data from two differently-labeled entities, the label of the data is computed with the  _join_, or _least upper bound_ (lub). This label captures the secrecy of both sources: to read data labeled as such you need the consent of the principals in both both labels. Dually, the integrity component captures the fact that the data is a combination of the two sources and thus data labeled as such is less trustworthy than both sources.

In LIO, the current label protects everything in scope. Hence, when we read from a labeled entity and thus incorporate data originally-protected by this entity's label, we must to raise the current label to ensure that the current label can also protect this new data. Moreover, we must reflect in the current label the fact that data of certain integrity was incorporated and thus may influence the rest of the computation. We raise the label to the join of the current label and the entity label.

Complete the next two examples by computing the join by hand. In these examples, we first incorporate data from `ref1` into the current context and then from `ref2`. In both cases the current label is raised to the least restricting label that still protects the data we're incorporating into the context.

#### Example 1

```active-haskell
:requires=withRefs

example :: DC Bool
example = do
  let l1 =  ("Alice" \/ "Bob") /\  "Carla" %% True
            -- data sensitive to Carla and either Alice or Bob
      l2 =  "Alice" /\  "Dave" %% True
            -- data sensitive to both Alice and Dave

  withRefs l1 l2 $ \ref1 ref2 -> do
     readLIORef ref1
     lcur <- getLabel
     unless (lcur == l1) $ fail "Unexpected current label"
     readLIORef ref2
     lcur' <- getLabel
     unless (lcur' == l1 `lub` l2) $ fail "Unexpected current label"

  l3 <- getLabel
  return $ l3 == fillIn "label that preserves privacy of ref1 and ref2"
```

#### Example 2

```active-haskell
:requires=withRefs

example :: DC Bool
example = do
  let l1 =  "Alice" \/ "Bob"  %% True
            -- data sensitive to either Alice or Bob
      l2 =  "Carla" \/  "Dave" %% True
            -- data sensitive to either Carla or Dave

  withRefs l1 l2 $ \ref1 ref2 -> do
     readLIORef ref1
     readLIORef ref2

  l3 <- getLabel
  return $ l3 == fillIn "label that preserves privacy of ref1 and ref2"
```

-----

Compute the integrity components of the joins:
 
#### Example 3

```active-haskell
:requires=withRefs

example :: DC Bool
example = do
  let l1 =  True %% "Alice" \/ "Bob"
            -- data created by Alice or Bob
      l2 =  True %% "Carla"
            -- data created by Carla

  withRefs l1 l2 $ \ref1 ref2 -> do
     readLIORef ref1
     readLIORef ref2

  l3 <- getLabel
  return $ l3 == fillIn "label that reflects untrustworhty l1 and l2 data has been incorporated in the context"
```

#### Example 4

```active-haskell
:requires=withRefs

example :: DC Bool
example = do
  let l1 =  "Alice" /\ "Bob"  %% True
            -- data sensitive to both Alice and Bob, and
            -- created by Alice
      l2 =  "Carla" \/  "Dave" %% "Alice" /\ "Bob"
            -- data sensitive to either Carla or Dave, and
            -- endorsed by both Alice and Bob

  withRefs l1 l2 $ \ref1 ref2 -> do
     readLIORef ref1
     readLIORef ref2

  l3 <- getLabel
  return $ l3 == fillIn "label that reflects untrustworhty l1 and l2 data has been incorporated in the context"
```

-----

As mentioned, when creating a reference with `newLIORef` the label of the reference must be able the current label and below the current clearance. The implementation is something like this:

```haskell
newLIORef :: DCLabel -> a -> LIORef DCLabel a
newLIORef l a = do
  lcur <- getLabel
  ccur <- getClearance
  unless (lcur `canFlowTo` l && ccur `canFlowTo` ccur) $ fail "invalid label"
  ... -- actually create reference
```

The same check is used by `writeLIORef` when writing to a reference.

In the above examples, this always held true because our initial current label was the bottom element and we created the references before reading any sensitive data. Once we read sensitive data we should not be able to arbitrarily write to less-sensitive entities. Similarly, once we read data that is less trustworthy that our current context then we should not be able to affect more trustworthy entities. The next two examples consider these two scenarios.


#### Example 5

```active-haskell
:requires=withRefs

example :: DC ()
example = do
  let l1 =  "Alice" %% "Alice" \/ "Bob"
      l2 =  "Bob"   %% "Alice" \/ "Bob"

  withRefs l1 l2 $ \ref1 ref2 -> do
     v1 <- readLIORef ref1
     writeLIORef ref1 $ v1 + 1
     readLIORef ref2
     writeLIORef ref2  v1
```

* What is the `canFlowTo` check that doesn't hold?

* Modify `l1` by adding to the secrecy component a single disjunct/conjunct that will prevent the exception from being thrown. Intuitively, why does this work?

* Modify `l2` by adding to the secrecy component a single disjunct/conjunct that will prevent the exception from being thrown. Intuitively, why does this work?

#### Example 6

```active-haskell
:requires=withRefs

example :: DC ()
example = do
  let l1 =  "Alice" /\ "Bob"  %% "Alice"
      l2 =  "Alice" /\ "Bob"  %% "Bob"

  withRefs l1 l2 $ \ref1 ref2 -> do
     v1 <- readLIORef ref1
     writeLIORef ref1 $ v1 + 1
     readLIORef ref2
     writeLIORef ref2  v1
```

* What is the `canFlowTo` check that doesn't hold?

* Modify `l1` by adding to the integrity component a single disjunct/conjunct that will prevent the exception from being thrown. Intuitively, why does this work?

* Modify `l2` by adding to the integrity component a single disjunct/conjunct that will prevent the exception from being thrown. Intuitively, why does this work?

-----

### Privileges

In examples 5 and 6 we modified the labels of the source/destination references to allow certain writes after reads. In a real application we may not be able to do this. Moreover, by changing the label of a resource we may be permitting reads/writes that should otherwise not be allowed. Instead, let's use privileges to explicitly declassify and endorse data.

Recall that privileges are used to bypass restrictions by effectively removing/ignoring a principal (really, a disjunct) from the secrecy component of a label and adding a principal (really, a disjunct) to the integrity component. For example, this is done when we compare the current label with the label of a reference (`writeLIORefP`) to check if we can write to the reference.  To be flexible, it's also used when we raise raise the current label (e..g, with `readLIORefP`); in this case we use privileges to avoid "over-tainting". (Of course, in some cases you may want to taint the context!)

In the examples below we're going to use the public label as the initial current label and top as the clearance (the default state), and use a minting function that will create a privilege from a principal.

```active-haskell
:requires=main
:name=mint

initialLabelState :: LIOState DCLabel
initialLabelState = dcDefaultState

-- | Create a privilege corresponding to the list of principal
mint :: [Principal] -> DC DCPriv
mint ps = ioTCB . privInit $ cFromList $ map (\p -> dFromList [p]) ps
```

Here `dFromList` is used to convert each principal to a singleton disjunctive clause; `cFromList` turns these into a conjunctive clause, i.e., a CNF formula (which is also a label component). Recall that a disjunctive clause in a privilege corresponds to a delegated privilege (not used here) and the conjunction of such clauses means that the privilege can be used to "speak on behalf" of multiple principals.

> > **Security note:** In actual code you should never expose something like `mint` to untrusted code!



#### Example 7

Complete the example below by filling in the first argument to `canFlowToPM` with the principals needed for the flow to hold. 

```active-haskell
:requires=mint

example :: DC ()
example = do
  canFlowToPM 1 [] ("Alice" /\ "Dan" %% "Joe") ("Dan" %% "Joe" \/ "Hugo")
  canFlowToPM 2 [] ("Joe" /\ "Clara" %% "Joe") ("Dan" /\ "Alice" %% "Joe" \/ "Hugo")
  canFlowToPM 3 [] ("Joe" /\ "Clara" %% "Joe") (True %% True)
                
  canFlowToPM 4 [] (True %% True) ("Alice" %% "Joe")
  canFlowToPM 5 [] (True %% True) ("Alice" %% "Joe" /\ "Hugo")
  canFlowToPM 6 [] ("Joe" /\ "Alice" %% "Alice") ("Joe" /\ "Alice" /\ ("Bob" \/ "Hugo") %% "Joe" /\ "Alice")
  
  where canFlowToPM nr prin l1 l2 = do
          priv <- mint $ map principal prin
          unless (canFlowToP priv l1 l2) $ fail $ show nr ++ ": insufficient privileges"
```

Let's now revisit examples 5 and 6 to use privileges instead of modifying the end-point labels.

#### Example 8

```active-haskell
:requires=mint

example :: DC ()
example = do
  let l1 =  "Alice" %% "Alice" \/ "Bob"
      l2 =  "Bob"   %% "Alice" \/ "Bob"

  priv <- mint [fillIn "single principal"]
  ref1 <- newLIORefP priv l1 3
  ref2 <- newLIORefP priv l2 4

  v1 <- readLIORef ref1
  writeLIORef ref1 $ v1 + 1
  readLIORef ref2
  writeLIORef ref2  v1
```

* Modify only the write(s) to use privileges. Intuitively, why does this work?

* Is any information leaked by using privileges?

* Will this work if you only modify the read(s)? What if you use the privilege of both principals?

Though the exception throw by LIO contains label information, you may want to use the trusted action `getLabel >>= ioTCB . print` to inspect the current label.

#### Example 9

```active-haskell
:requires=mint

example :: DC DCLabel
example = do
  let l1 =  "Alice" /\ "Bob"  %% "Alice"
      l2 =  "Alice" /\ "Bob"  %% "Bob"

  ref1 <- newLIORefP noPrivs  l1 3
  ref2 <- newLIORefP noPrivs l2 4

  v1 <- readLIORefP noPrivs ref1
  writeLIORefP      noPrivs ref1 $ v1 + 1
  readLIORefP       noPrivs ref2
  writeLIORefP      noPrivs ref2  v1
  getLabel
```
* Modify the example to use the least privilege for each operation.

* Modify the example to use the least privilege for each operation, while ensuring the end current label is the public label.

> > *Note:* Privileges are monoids; you can use `mempty` for the empty/no privilege and `mappend` to combine them.

-----

### Clearance

Thus far, we've seen that LIO permissively let's us read arbitrary data (from e.g., labeled references), but raises the current label and in turn restricts us from writing this data arbitrarily. But sometimes---especially when considering malicious code---we may want to be more strict and simply disallow a piece of code from even accessing certain data. For example, suppose we're building a web server and want to make sure that the server returns a (meaningful) response if the request handler only read data that the current user is allowed to see. One approach is to check the current label before returning the response. This may be okay in some cases, but depending on the scenario it may also allow for *external timing* leaks, where user Alice uses a stopwatch to measure the amount of time it takes to get a response: if the request handler read Bob's data and delays producing a response according to some secret then Alice will learn something about Bob (even though the current label may, for example, be `"Alice" /\ "Bob" %% True` and our server replaced the response with a 400-level error message).

Clearance serves an upper bound on the current label (thus limiting reads) and is also checked when we create or write to objects (limiting writes to anything below).

```active-haskell
:requires=mint

example :: DC [(String, String)]
example = do
  -- Create "DB"
  aliceRef <- newLIORef ("Alice" %% True) "Alice likes Carla"
  bobRef   <- newLIORef ("Bob"   %% True) "Bob likes Alice"
  abRef    <- newLIORef ("Alice" \/ "Bob" \/ "Carla" %% True) "Hey guys!"

  -- Handle requests from Alice and Bob
  aliceRespR <- handleReq "Alice" $ readAllAct [bobRef, aliceRef, abRef]
  bobRespR   <- handleReq "Bob"   $ readAllAct [bobRef, aliceRef, abRef]
  
  -- Wait for respones:
  aliceResp <- lWait aliceRespR
  bobResp   <- lWait bobRespR
  return [aliceResp, bobResp]

-- Given a username and action that produces a response
-- return a labeled result that to which the response will be written
handleReq :: String -> DC String -> DC (LabeledResult DCLabel (String, String))
handleReq user act = do
  clr <- getClearance
  lFork clr $ do 
    resp <- act
    return (user, resp)

-- Given a "DB" read and concatnate all entries
readAllAct :: [LIORef DCLabel String] -> DC String
readAllAct db = do
  messages <- forM db readLIORef
  return $ intercalate ";" messages
```

* Modify `handleReq` to ensure that `act` cannot access the data the other than that of the "current user"; at this point the program should throw an exception.

* Modify `readAllAct` to address the exception. The result of executing this program should be:

```haskell
[("Alice","Alice likes Carla;Hey guys!"),("Bob","Bob likes Alice;Hey guys!")]
```

-----

### More on creating labels

Before we used some function form the `LIO.DCLabel` module to create privileges and label components (`CNF`s). For example, in `mint` we used `dFromList` and `cFromList`. The former converts a list of principals into a disjunction, i.e., `dFromList [p1, p2, ...] == p1 \/ p2 \/ ...`, while the latter converts a list of disjunctions into a conjunction, i.e., `cFromList [d1, d2, ...] == d1 /\ d2 /\ ...`.  Define functions that compute the disjunction and conjunction of any formula: 

#### Example 10

```active-haskell
:requires=mint

dFromList' :: ToCNF a => [a] -> CNF
dFromList' = fillIn "define without the above combinators"

example :: DC ()
example = do
  check 1 $ "A" \/ "B" \/ "C" \/ "D" == dFromList' ["A" \/ "B", "C" \/ "D"]
  check 2 $ let ps = map principal ["A", "B", "C"]
            in toCNF (dFromList ps) == dFromList' ps
  where check nr a = unless a  $  fail $ show nr ++ ": bad implementation"
```

#### Example 11

```active-haskell
:requires=mint

cFromList' :: ToCNF a => [a] -> CNF
cFromList' = fillIn "define without the above combinators"

example :: DC ()
example = do
  check 1 $ ("A" \/ "B") /\ "C" /\ "D" == cFromList' ["A" \/ "B", "C" /\ "D"]
  check 2 $ let ps = map (\p -> principal [p]) ['A' .. 'Z']
            in (cFromList $ map (\p -> dFromList [p]) ps) == cFromList' ps
  where check nr a = unless a  $  fail $ show nr ++ ": bad implementation"
```
