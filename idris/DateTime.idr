import Quotient
%access export

data DateFormat = Years DateFormat
                | Months DateFormat
                | FullMonths DateFormat
                | Days DateFormat
                | Hours DateFormat
                | Minutes DateFormat
                | Seconds DateFormat
                | StrLit String DateFormat
                | End

formatter : String -> DateFormat
formatter str = pfmt (unpack str) []
  where
    pfmt : List Char -> List Char -> DateFormat
    pfmt [] (c::cs) = StrLit (reverse . pack $ (c::cs)) (pfmt [] [])
    pfmt [] [] = End
    pfmt ('%'::rest) (c::cs) = StrLit (reverse . pack $ (c::cs)) (pfmt ('%'::rest) [])
    pfmt ('%'::c::rest) [] = 
      case c of
        'Y' => Years (pfmt rest [])
        'M' => Months (pfmt rest [])
        'F' => FullMonths (pfmt rest [])
        'd' => Days (pfmt rest [])
        'h' => Hours (pfmt rest [])
        'm' => Minutes (pfmt rest [])
        's' => Seconds (pfmt rest [])
        c   => pfmt (c :: rest) ('%' :: [])
    pfmt (c::rest) stack = pfmt rest (c::stack)


Year : Type
Year = Int

data Month = January
           | February
           | March
           | April
           | May
           | June
           | July
           | August
           | September
           | October
           | November
           | December

numDays : Year -> Month -> Int
numDays _ January = 31
numDays x February = assert_total $ if (x `mod` 4) == 0 then 29 else 28
numDays _ March = 31
numDays _ April = 30
numDays _ May = 31
numDays _ June = 30
numDays _ July = 31
numDays _ August = 31
numDays _ September = 30
numDays _ October = 31
numDays _ November = 30
numDays _ December = 31

Day : Year -> Month -> Type
Day y m = IntMod (numDays y m)

Hour : Type
Hour = IntMod 24

Minute : Type
Minute = IntMod 60

record DateTime where
  constructor MkDateTime
  year : Year
  month : Month
  day : Day year month
  hour : Hour
  minute : Minute
  nanosecond : Integer

record Date where
  constructor MkDate
  year : Year
  month : Month
  day : Day year month

record Time where
  constructor MkTime
  hours : Hour
  minute : Minute
  nanosecond : Int

Eq Month where
  January == January = True
  February == February = True
  March == March = True
  April == April = True
  May == May = True
  June == June = True
  July == July = True
  August == August = True
  September == September = True
  October == October = True
  November == November = True
  December == December = True
  _ == _ = False


Enum Month where
  pred January = February
  pred February = March
  pred March = April
  pred April = May
  pred May = June
  pred June = July
  pred July = August
  pred August = September
  pred September = October
  pred October = November
  pred November = December
  pred December = January
  succ January = December
  succ February = January
  succ March = February
  succ April = March
  succ May = April
  succ June = May
  succ July = June
  succ August = July
  succ September = August
  succ October = September
  succ November = October
  succ December = November
  toNat January = 1
  toNat February = 2
  toNat March = 3
  toNat April = 4
  toNat May = 5
  toNat June = 6
  toNat July = 7
  toNat August = 8
  toNat September = 9
  toNat October = 10
  toNat November = 11
  toNat December = 12
  fromNat Z = January
  fromNat (S Z) = January
  fromNat (S (S Z)) = February
  fromNat (S (S (S Z))) = March
  fromNat (S (S (S (S Z)))) = April
  fromNat (S (S (S (S (S Z))))) = May
  fromNat (S (S (S (S (S (S Z)))))) = June
  fromNat (S (S (S (S (S (S (S Z))))))) = July
  fromNat (S (S (S (S (S (S (S (S Z)))))))) = August
  fromNat (S (S (S (S (S (S (S (S (S Z))))))))) = September
  fromNat (S (S (S (S (S (S (S (S (S (S Z)))))))))) = October
  fromNat (S (S (S (S (S (S (S (S (S (S (S Z))))))))))) = November
  fromNat (S (S (S (S (S (S (S (S (S (S (S (S Z)))))))))))) = December
  fromNat _ = December

Ord Month where
  compare a b = compare (toNat a) (toNat b)

Num Month where
  x + y = fromNat $ assert_total $ ((toNat x) + (toNat y)) `mod` 12
  x * y = fromNat $ assert_total $ ((toNat x) * (toNat y)) `mod` 12 
  fromInteger x = fromNat (cast x)

Neg Month where
  x - y = fromInteger $ (cast $ toNat x) - (cast $ toNat y)
  negate x = fromInteger $ 13 - (cast $ toNat x)
  abs = id

Show Month where
  show January = "January"
  show February = "February"
  show March = "March"
  show April = "April"
  show May = "May"
  show June = "June"
  show July = "July"
  show August = "August"
  show September = "September"
  show October = "October"
  show November = "November"
  show December = "December"

Eq Date where
  (MkDate year_x month_x day_x) == (MkDate year_y month_y day_y) = 
    case (year_x == year_y, month_x == month_y) of
      (True, True) => intModEq day_x day_y
      _ => False


Ord Date where
  compare a b = case compare (year a) (year b) of
    EQ => case compare (month a) (month b) of
      EQ => intModCmp (day a) (day b)
      r => r
    r => r

Num Date where
  a + b = MkDate (year a + year b) (month a + month b) ((intModExt $ day a) + (intModExt $ day b))
  a * b = MkDate (year a * year b) (month a * month b) ((intModExt $ day a) * (intModExt $ day b))
  -- number of days since some epoch? Maybe 2000-01-01?
  fromInteger a = assert_total $ let y = a `div` 365 in
                  let d' = a - y in
                  let leaps = 1 + (y `div` 4) in -- 2000 is a leap year
                  let (m ** d) = go (fromInteger y) (d' - leaps) 1 in
                  MkDate (fromInteger y) m d
    where
      go : (y : Year) -> Integer -> Nat -> (m : Month ** IntMod (numDays y m))
      go y d m = if (fromInteger d) < (numDays y (fromNat m))
                 then (fromNat m ** MkIntMod (fromInteger d))
                 else let d' = (fromInteger d) - numDays y (fromNat m) in
                      go y (cast d') (S m)

dateToInteger : Date -> Integer
dateToInteger (MkDate year month (MkIntMod day)) = 
  let ys = year in
  let ls = assert_total $ ys `div` 4 in
  let ms = (if year < 0 
           then sum (map (numDays year) [month .. December])
           else sum (map (numDays year) [January .. month])) in
  let ds = (if (year < 0)
           then let num_days = numDays year month in num_days - day
           else day) in
  cast $ ys * 365 + ls + ms + ds


Neg Date where
  negate (MkDate year month (MkIntMod x)) = 
    let y = negate year in
    let m = negate month in
    let d = the (IntMod (numDays y m)) $ fromInteger (negate (cast x)) in
    MkDate y m d
  a - b = a + (negate b)
  -- dates are around the 2000-01-01 epoch
  abs (MkDate year month (MkIntMod day)) = 
    let ys = abs (year) in
    let ls = assert_total $ ys `div` 4 in
    let ms = (if year < 0 
             then sum (map (numDays year) [month .. December])
             else sum (map (numDays year) [January .. month])) in
    let ds = (if (year < 0)
             then let num_days = numDays year month in num_days - day
             else day) in
    fromInteger . cast $ ys * 365 + ls + ms + ds

Enum Date where
  pred (MkDate year month (MkIntMod x)) = 
    if x == 0 
    then 
      if month == January 
      then let m = pred month in MkDate (pred year) m (MkIntMod ((numDays year m) - 1))
      else let m = pred month in MkDate year m (MkIntMod ((numDays year m) - 1))
    else
      MkDate year month (MkIntMod (x - 1))
  succ (MkDate year month day) = 
    let (MkIntMod x) = day + 1 in
    if x == 0 
    then if month == December
         then MkDate (succ year) January (MkIntMod x)
         else MkDate year (succ month) (MkIntMod x)
    else MkDate year month (MkIntMod x)
    
  toNat date = cast (dateToInteger (date))
  fromNat d = fromInteger . cast $ d


Show Date where
  show (MkDate year month (MkIntMod day)) = (show year) ++ "-" ++ (show month) ++ "-" ++ (show day)

Eq Time where
  (MkTime hours minute nanosecond) == (MkTime x y z) = 
    hours == x && minute == y && nanosecond == z
Enum Time where
  -- don't use this function
  -- will be v. slow
  toNat (MkTime (MkIntMod x) (MkIntMod y) nanosecond) =
         cast $ (3600000000*x) + 60000000 * y + nanosecond 
  fromNat n = assert_total $ let (h, r) = (n `div` 3600000000000, n `mod` 360000000000) in
              let (m, n) = (r `div` 60000000000, r `mod` 60000000000) in
              MkTime (fromInteger . cast $ h) (fromInteger . cast $ m) (cast n)
  succ (MkTime hours minute nanosecond) = 
    if nanosecond == 59999999999
    then if minute == 59
         then MkTime (succ hours) 0 0
         else MkTime hours (succ minute) 0
    else MkTime hours minute (succ nanosecond)
  pred (MkTime hours minute nanosecond) = 
    if nanosecond == 0
    then if minute == 0
         then MkTime (pred hours) 59 59999999999
         else MkTime hours (pred minute) 59999999999
    else MkTime hours minute (pred nanosecond)

Ord Time where
  compare (MkTime h m n) (MkTime h' m' n') =
    case (compare h h') of
      EQ => case compare m m' of
              EQ => compare n n'
              cmp => cmp
      cmp => cmp

Num Time where
Neg Time where
Show Time where

Eq DateTime where
Enum DateTime where
Ord DateTime where
Num DateTime where
Neg DateTime where
Show DateTime where

interface Temporal t where
  years : t -> Integer
  months : t -> Month
  days : t -> Integer
  hours : t -> Integer
  minutes : t -> Integer
  
Temporal Date where
  years (MkDate y _ _) = 2000 + cast y
  months (MkDate _ m _) = m
  days (MkDate _ _ (MkIntMod d)) = cast d
  hours _ = 0
  minutes _ = 0
Temporal Time where
  years _ = 2000
  months _ = January
  days _ = 0
  hours (MkTime (MkIntMod h) _ _) = cast h
  minutes (MkTime _ (MkIntMod m) _ ) = cast m
Temporal DateTime where
  years (MkDateTime y _ _ _ _ _) = cast y
  months (MkDateTime _ m _ _ _ _) = m
  days (MkDateTime _ _ (MkIntMod d) _ _ _) = cast d
  hours (MkDateTime _ _ _ (MkIntMod h) _ _) = cast h
  minutes (MkDateTime _ _ _ _ _ n) = assert_total $ n `div` 60000000000

seconds : Temporal t => t -> Integer
seconds x = (minutes x) * 60
millis  : Temporal t => t -> Integer
millis x = (seconds x) * 1000
micros  : Temporal t => t -> Integer
micros x = (millis x) * 10
nanos   : Temporal t => t -> Integer
nanos x = (micros x) * 10

||| Takes a date formatting string (ex: %Y%m%dT%h%m%s) and prints the date in that format.
showFormat : Temporal t => String -> t -> String
showFormat formatStr = let fmt = formatter formatStr in go fmt 
  where
    go : Temporal t => DateFormat -> t -> String
    go (Years x) t = (show (years t)) ++ (go x t)
    go (Months x) t = (show (toNat . months $ t)) ++ go x t
    go (FullMonths x) t = (show (months t)) ++ go x t
    go (Days x) t = (show (days t)) ++ go x t
    go (Hours x) t = (show (hours t)) ++ go x t
    go (Seconds x) t = (show (seconds t)) ++ go x t
    go (StrLit x y) t = x ++ go y t
    go End t = ""

