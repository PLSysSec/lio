<style>
blockquote blockquote p { font-size: 10pt; }
</style>

In this post we're going to work through some exercises to internalize [LIO](http://hackage.haskell.org/package/lio) and [DCLabel](http://hackage.haskell.org/package/lio-0.11.4.1/docs/LIO-DCLabel.html) concepts, including current label, LIORefs and privileges.

Throughout we are going to execute an `example` action that you will be asked to complete. Our `main` action, defined below, simply executes the `example` action with an initial current label and clearance of `initialLabels`.

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

The initial LIO state that we will use in the next several examples is:

```active-haskell
:requires=main
:name=highIntegrityInit
:noexec

initialLabelState ::  LIOState DCLabel
initialLabelState = LIOState { lioLabel     = True  %% False
                             , lioClearance = False %% True }
```

`initialLabelState` sets the current label to the DC label `True %% False`. For this label, the secrecy component is `True`, which implies that the current information secrecy is public. The integrity part is `False`, which we use in the examples to ease the creation of high integrity objects without involving privileges. (See note below)

The clearance label is set to `False %% True`, which should be understood as having no clearance privileges.

DC labels form a lattice, with the assignments above corresponding respectively to the bottom and top elements.

> > **Note:** In general, you want to use `dcPublic = True %% True` as the initial current label. A high label with a high integrity component (the `False` here) will allow the creation of "high" integrity labeled objects without using any privileges. This is convenient for the next simple examples until we introduce privileges, but such a label will likely be not safe in real world environments.

### Current label and DC labels

We're going to look at how the current label evolves when doing reads and writes to labeled data without privileges.  We will use labeled references [`LIORef`](http://hackage.haskell.org/package/lio-0.11.4.1/docs/LIO-LIORef.html#t:LIORef) to that purpose.

When we combine data from two differently-labeled entities `ref1` and `ref2`, the resulting label will be computed using the _join_, or _least upper bound_ (lub) operation. The new label will capture the secrecy of both entities: to read the combined data, you need the consent of the principals in both `ref1` and `ref2` labels. Dually, the integrity component captures the fact that the data is a combination of the two sources and thus data labeled as such is less trustworthy than data coming from each individual source.

In LIO, the current label protects everything in scope. Hence, when we read from a labeled entity, data originally-protected by this entity's label is brought into scope: we must to raise the current label to capture the increased secrecy requirements. Moreover, the current label must take into account that data of certain integrity was incorporated and thus may influence the rest of the computation. Thus, the current label is updated or _raised_ to its join with the entity label.

Now, you will compute a few label joins by hand. In the following examples, we bring into scope data from `ref1` and `ref2`. When doing this, LIO raises the current label to the least restricting label that jointly protects `ref1` and `ref2`. Your job is to write down the label computed by LIO:

#### Example 1

```active-haskell
:requires=highIntegrityInit

example :: DC Bool
example = do
  let l1 =  ("Alice" \/ "Bob") /\ "Carla" %% True
            -- data sensitive to Carla and either Alice or Bob
      l2 =  "Alice" /\ "Dave" %% True
            -- data sensitive to both Alice and Dave

  -- create refs:
  ref1 <- newLIORef l1 3
  ref2 <- newLIORef l2 4
  --

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
:requires=highIntegrityInit

example :: DC Bool
example = do
  let l1 =  "Alice" \/ "Bob"  %% True
            -- data sensitive to either Alice or Bob
      l2 =  "Carla" \/  "Dave" %% True
            -- data sensitive to either Carla or Dave

  -- create refs:
  ref1 <- newLIORef l1 3
  ref2 <- newLIORef l2 4
  --

  readLIORef ref1
  readLIORef ref2

  l3 <- getLabel
  return $ l3 == fillIn "label that preserves privacy of ref1 and ref2"
```

-----

Now, compute the integrity component of the current label:
 
#### Example 3

```active-haskell
:requires=highIntegrityInit

example :: DC Bool
example = do
  let l1 =  True %% "Alice" \/ "Bob"
            -- data created by Alice or Bob
      l2 =  True %% "Carla"
            -- data created by Carla

  -- create refs:
  ref1 <- newLIORef l1 3
  ref2 <- newLIORef l2 4
  --

  readLIORef ref1
  readLIORef ref2

  l3 <- getLabel
  return $ l3 == fillIn "label that reflects untrustworhty l1 and l2 data has been incorporated in the context"
```

#### Example 4

```active-haskell
:requires=highIntegrityInit

example :: DC Bool
example = do
  let l1 =  "Alice" /\ "Bob"  %% True
            -- data sensitive to both Alice and Bob, and
            -- created by Alice
      l2 =  "Carla" \/  "Dave" %% "Alice" /\ "Bob"
            -- data sensitive to either Carla or Dave, and
            -- endorsed by both Alice and Bob

  -- create refs:
  ref1 <- newLIORef l1 3
  ref2 <- newLIORef l2 4
  --

  readLIORef ref1
  readLIORef ref2

  l3 <- getLabel
  return $ l3 == fillIn "label that reflects untrustworhty l1 and l2 data has been incorporated in the context"
```

-----

When creating a reference with `newLIORef`, it takes into account the current label and clearance. The label of the new reference must be above the current label and below the current clearance. The implementation is something like this:

```haskell
newLIORef :: DCLabel -> a -> LIORef DCLabel a
newLIORef l a = do
  lcur <- getLabel
  ccur <- getClearance
  unless (lcur `canFlowTo` l && ccur `canFlowTo` ccur) $ fail "invalid label"
  ... -- actually create reference
```

The same check is performed in `writeLIORef` when writing to a reference.

In the above examples, the `canFlowTo` condition always held true because our initial current label was the bottom element and we created the references before reading any sensitive data. Once we read sensitive data we should not be able to arbitrarily write to less-sensitive entities. Similarly, once we read data that is less trustworthy than our current context then we should not be able to affect more trustworthy entities. The next two examples illustrate these two scenarios.


#### Example 5

```active-haskell
:requires=highIntegrityInit

example :: DC ()
example = do
  let l1 =  "Alice" %% "Alice" \/ "Bob"
      l2 =  "Bob"   %% "Alice" \/ "Bob"

  -- create refs:
  ref1 <- newLIORef l1 3
  ref2 <- newLIORef l2 4
  --

  v1 <- readLIORef ref1
  writeLIORef ref1 $ v1 + 1
  readLIORef ref2
  writeLIORef ref2  v1
```

When you execute this code you will get an error of the form:

```haskell
LabelError { lerrContext      = ["writeLIORef"]
           , lerrFailure      = "guardAllocP"
           , lerrCurLabel     = "Alice" /\ "Bob" %% ("Alice" \/ "Bob")
           , lerrCurClearance = False %% True
           , lerrPrivs        = []
           , lerrLabels       = ["Bob" %% ("Alice" \/ "Bob")] }
```

What do all these fields mean?

* `lerrContext` gives us a "stack-trace". If we wrap an LIO action `act` by `withContext "act"` then LIO will keep track of this when it throws an exception, so we can more easily figure out where the exception was thrown.

* `lerrFailure` gives us the name of the actual function that failed (since the context may contain multiple names).

* `lerrCurLabel` and `lerrCurClearance` respectively contain the current label and clearance at the point of failure.

* `lerrPrivs` tells us what privileges were exercised.

* `lerrLabels` gives us a list of labels involved.

The above exception tells us that the `guardAllocP` function used by `writeLIORef` failed.  If we look at `guardAllocP` (with empty privileges) we can find out that the function throws an exception when the current label can not flow to the label of the object. In this case,

```haskell
"Alice" /\ "Bob" %% ("Alice" \/ "Bob") `canFlowTo` "Bob" %% ("Alice" \/ "Bob")
```

Intuitively, the second `writeLIORef` fails because we have read data  needing both Alice and Bob privileges (secrecy component of `"Alice" /\ "Bob"`) and we are subsequently trying to write to a reference that only requires privileges for Bob. Would that check not fail, we could leak Alice's data.

With this in mind:

* Modify `l1` by adding to the secrecy component a single disjunct/conjunct that will prevent the exception from being thrown. Intuitively, why does this work?

* Modify `l2` by adding to the secrecy component a single disjunct/conjunct that will prevent the exception from being thrown. Intuitively, why does this work?

#### Example 6

```active-haskell
:requires=highIntegrityInit

example :: DC ()
example = do
  let l1 =  "Alice" /\ "Bob"  %% "Alice"
      l2 =  "Alice" /\ "Bob"  %% "Bob"

  -- create refs:
  ref1 <- newLIORef l1 3
  ref2 <- newLIORef l2 4
  --

  v1 <- readLIORef ref1
  writeLIORef ref1 $ v1 + 1
  readLIORef ref2
  writeLIORef ref2  v1
```

Now,

* What is the `canFlowTo` check that doesn't hold?

* Modify `l1` by adding to the integrity component a single disjunct/conjunct that will prevent the exception from being thrown. Intuitively, why does this work?

* Modify `l2` by adding to the integrity component a single disjunct/conjunct that will prevent the exception from being thrown. Intuitively, why does this work?

-----

### Privileges

In examples 5 and 6, we repaired the `LabelError` exceptions by modifying the labels of the references itself to become more permissive. In a real application, we may not want to do this, by changing the label of a resource we may be permitting reads/writes that should otherwise not be allowed. Instead, in this section we will use privileges to explicitly declassify and endorse data while keeping our resources appropriately labeled.

Privileges are used to bypass secrecy and integrity restrictions. For the secrecy part, a privilege effectively removes/ignores a principal (really, a disjunct). For instance, if we got privileges for `Alice`, we can make the label `Alice /\ Bob %% True` flow to `Bob %% True`, this can be seen as `Alice` making the information public from her point of view. For the integrity part, a privilege adds a principal (really, a disjunct) to the integrity component. For instance, if we have privileges for `Alice`, we can make the label `True %% Bob` flow to `True %% Alice /\ Bob`, meaning that `Alice` endorses now the given information.

Privileges are used in `writeLIORefP` to check if we can write to the reference. To be flexible, they are also used when raising the current label (e..g, with `readLIORefP`); in this case, we use privileges to avoid "over-tainting". (Of course, in some cases you may want to taint the context!)

We will run the examples below with the public label as the initial current label. The clearance will be different than from the previous cases, we will use the top element which basically means no clearance (the default state). We also define a minting function `mint` for creating a privilege from a list of principals. Note that `mint` uses `ioTCB` which is usually unavailable to regular `LIO` code, otherwise such code could create arbitrary privileges bypassing the security restrictions.

```active-haskell
:requires=main
:name=mint
:noexec

initialLabelState :: LIOState DCLabel
initialLabelState = dcDefaultState

-- | Create a privilege corresponding to the list of principals
mint :: [Principal] -> DC DCPriv
mint ps = ioTCB . privInit $ cFromList $ map (\p -> dFromList [p]) ps
```

Here `dFromList` is used to convert each principal to a singleton disjunctive clause; `cFromList` turns these into a conjunctive clause, i.e., a CNF formula (which is also a label component). Recall that a disjunctive clause in a privilege corresponds to a delegated privilege (not used here) and the conjunction of such clauses means that the privilege can be used to "speak on behalf" of multiple principals.

> > **Security reminder:** In actual code you should never expose something like `mint` to untrusted code!



#### Example 7

Complete the example below by filling in the first argument to `canFlowToPM` with the minimum list of principals needed for the flow to hold.

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

Now,

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

Now,

* Modify the example to use the least privilege for each operation.

* Modify the example to use the least privilege for each operation, while ensuring the end current label is the public label.

> > *Note:* Privileges are monoids; you can use `mempty` for the empty/no privilege and `mappend` to combine them.
