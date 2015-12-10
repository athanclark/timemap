timemap
=======

A mutable `Data.HashMap`-like structure, where each entity is implicitly indexed
by their last-modified time. Useful for keyed caches.

## Usage

```haskell
import qualified Data.TimeMap as TM

main :: IO ()
main = do
  -- create a new, empty reference
  mapRef <- TM.newTimeMap

  -- insert a new key/value pair in the map. Note that
  -- `someKey` should implement `Hashable`. This also
  -- sets the creation time of the value to "now".
  TM.insert someKey 0 mapRef

  -- adjusts the value of `someKey` to `1`. Note that
  -- also resets the creation time of the value to "now".
  TM.adjust (+1) someKey mapRef

  -- wait a second
  threadDelay 1000000

  -- delete all values older than 1 second from now.
  TM.filterFromNow 1 mapRef

  -- Will return `Nothing`
  TM.lookup someKey mapRef

  return ()
```

## How it works

There are two internal maps for a `TimeMap k a`:

- a "time-indexed" map reference: `TVar (Map UTCTime (HashSet k))`
- a hashtable of value references: `HashTable k (UTCTime, TVar a)`, which is also
  mutable (uses `ST`)

### Insertion

Inserting a new value first performs a lookup on the hashtable to see if the key
already exists:

- If it doesn't, make a new TVar for the value, and get the current time, the insert
  that into the hashtable. Then, insert the time you used and the key itself into
  the time-indexed multimap.
- If it does, remove the entry from the time-indexed multimap for the old time, before
  doing the same thing as the first bullet (except we `writeTVar` instead of making
  a new one).

### Lookups

These don't have to interact with the time map. All you need to do is lookup the
key in the hashmap, and pull the value out of the TVar if it exists.

### Filtering

To filter out all entries older than some time, you have to use the `Data.Map.splitLookup`
function to split the time-indexed multimap into the entries you want to delete from
the hashtable, and the ones you want to keep. You simply `writeTVar` the map you want
to keep, but for the ones you want to delete, you need to get all the `elems` of
that map. Then, for every key that we need to delete, we delete it from the
hashtable.

## How to run tests

```bash
stack test
```

## Benchmarks

```bash
stack bench --benchmark-arguments="--output profile.html"
```

You can also view the results on my box
[here](https://htmlpreview.github.io/?https://github.com/athanclark/timemap/blob/master/profile.html).
