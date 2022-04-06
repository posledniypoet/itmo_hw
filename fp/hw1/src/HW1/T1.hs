module HW1.T1
  ( Day (..),
    nextDay,
    afterDays,
    isWeekend,
    daysToParty,
  )
where
import Numeric.Natural
data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Show, Eq)

dec :: Natural -> Natural
dec n = n - 1

inc :: Natural -> Natural
inc n = n + 1

nextDay :: Day -> Day
nextDay Sunday = Monday
nextDay Monday = Tuesday
nextDay Tuesday = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday = Friday
nextDay Friday = Saturday
nextDay Saturday = Sunday

afterDays :: Natural -> Day -> Day
afterDays 0 d = d
afterDays n d = afterDays (dec n) (nextDay d)

isWeekend :: Day -> Bool
isWeekend Sunday = True
isWeekend Saturday = True
isWeekend _ = False

daysToParty :: Day -> Natural
daysToParty d = (count d 0)

count :: Day -> Natural -> Natural
count Friday n = n
count d n' = count (nextDay d) (inc n')