module FunSets where
import Test.HUnit

-- data Set = Int Bool
type Set = Int -> Bool

contains :: Set -> Int -> Bool

singletonSet :: Int -> Set

-- add
(<+) :: Set -> Int -> Set

union :: Set -> Set -> Set

intersect :: Set -> Set -> Set

-- what is in set one that is not in set 2
diff :: Set -> Set -> Set

filt :: Set -> ( Int -> Bool ) -> Set

forall :: Set -> ( Int -> Bool ) -> Bool

exists :: Set -> ( Int -> Bool ) -> Bool

mp :: Set -> ( Int -> Int ) -> Set


contains s int = s int

singletonSet a = (==a)

union s1 s2 = \int -> (contains s1 int) || ( contains s2 int )

(<+) s1 int = union s1 $ singletonSet int

intersect s1 s2 = \int -> (contains s1 int ) && ( contains s2 int )

diff s1 s2 = \int -> (contains s1 int) && ( not (contains s2 int) )

filt s1 predicate = \int -> ( contains s1 int ) && predicate int

forall s1 predicate =
    iter( -1000 )
    where
      iter :: Int -> Bool
      iter int =
        if ( int > 1000 ) then
            True
        else
            if ( ( contains s1 int ) && ( not (predicate int) ) ) then
                False
            else
                iter( int + 1 )

exists s1 predicate = not $ forall s1 $ (\int -> not $ predicate int )


mp s1 f = \int -> exists s1 ( \it -> f int == it )

-- Do stuff

s1 = singletonSet 1
s2 = singletonSet 2
s3 = singletonSet 3
someEvens = s2 <+ 4 <+ 6 <+ 8 <+ 10
someOdds = s1 <+ 3 <+ 5 <+ 7 <+ 9

-- tests

tContains1 = TestCase( assertEqual "s1 contains 1" ( s1 `contains` 1) True )
tContains2 = TestCase( assertEqual "s1 doesn't contain 2" ( s1 `contains` 2) False)

tAdd = TestCase $ do
    let n = s1 <+ 666
    assertEqual "666 was added" (n `contains` 666) True

tUnion = TestCase $ do
    let u = union s1 s2
    let t = u `contains` 1 && u `contains` 2
    assertEqual "union of s1 and s2 contains 2 and 1" t True

tDiff = TestCase $ do
    let f = s1 <+ 2 <+ 3 <+ 4
    let s = s2 <+ 4 <+ 6 <+ 8 <+ 9
    let d = diff f s
    assertEqual "has 1" (d `contains` 1) True
    assertEqual "has 3" (d `contains` 3) True
    assertEqual "missing 6" (d `contains` 6) False

tMap = TestCase $ do
    let n = someOdds `mp` (+1)
    let t = n `contains` 2 && n `contains` 4 && ( not $ n `contains` 5 )
    assertEqual "mapping odds to evens" t True

tests = TestList[ tContains1, tContains2, tUnion, tAdd, tDiff, tMap ]

--main =
--    let s = singletonSet 5
--    let c = contains s 5
--    print c