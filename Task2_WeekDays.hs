{-# LANGUAGE InstanceSigs #-}

module Task2_WeekDays where
import Test.Tasty.HUnit
import Test.QuickCheck

data WeekDay = Mon
    | Tue
    | Wed
    | Thu
    | Fri
    | Sat
    | Sun
    deriving Show

instance Arbitrary WeekDay where
    arbitrary = elements [Mon, Tue, Wed, Thu, Fri, Sat, Sun]

instance Eq WeekDay where
    (==) :: WeekDay -> WeekDay -> Bool
    day1 == day2 = (show day1 == show day2)

nextDay :: WeekDay -> WeekDay
nextDay Mon = Tue
nextDay Tue = Wed
nextDay Wed = Thu
nextDay Thu = Fri
nextDay Fri = Sat
nextDay Sat = Sun
nextDay Sun = Mon

unit_nextDay_Mon :: Assertion
unit_nextDay_Mon = nextDay Mon @?= Tue

unit_nextDay_Tue :: Assertion
unit_nextDay_Tue = nextDay Tue @?= Wed

unit_nextDay_Wed :: Assertion
unit_nextDay_Wed = nextDay Wed @?= Thu

unit_nextDay_Thu :: Assertion
unit_nextDay_Thu = nextDay Thu @?= Fri

unit_nextDay_Fri :: Assertion
unit_nextDay_Fri = nextDay Fri @?= Sat

unit_nextDay_Sat :: Assertion
unit_nextDay_Sat = nextDay Sat @?= Sun

unit_nextDay_Sun :: Assertion
unit_nextDay_Sun = nextDay Sun @?= Mon

afterDays :: Integer -> WeekDay -> WeekDay
afterDays 0 d = d
afterDays i d = nextDay (afterDays (i `mod` 7 - 1) d)

prop_afterDays_7 :: Integer -> WeekDay -> Property
prop_afterDays_7 i d = afterDays (i * 7) d === d

isWeekend :: WeekDay -> Bool
isWeekend Mon = False
isWeekend Tue = False
isWeekend Wed = False
isWeekend Thu = False
isWeekend Fri = False
isWeekend Sat = True
isWeekend Sun = True

unit_isWeekend_Mon :: Assertion
unit_isWeekend_Mon = isWeekend Mon @?= False

unit_isWeekend_Tue :: Assertion
unit_isWeekend_Tue = isWeekend Tue @?= False

unit_isWeekend_Wed :: Assertion
unit_isWeekend_Wed = isWeekend Wed @?= False

unit_isWeekend_Thu :: Assertion
unit_isWeekend_Thu = isWeekend Thu @?= False

unit_isWeekend_Fri :: Assertion
unit_isWeekend_Fri = isWeekend Fri @?= False

unit_isWeekend_Sat :: Assertion
unit_isWeekend_Sat = isWeekend Sat @?= True

unit_isWeekend_Sun :: Assertion
unit_isWeekend_Sun = isWeekend Sun @?= True

daysToParty :: WeekDay -> Integer
daysToParty Fri = 0
daysToParty d = (daysToParty (nextDay d)) + 1

unit_daysToParty_Fri :: Assertion
unit_daysToParty_Fri = daysToParty Fri @?= 0

unit_daysToParty_Sat :: Assertion
unit_daysToParty_Sat = daysToParty Sat @?= 6

unit_daysToParty_Sun :: Assertion
unit_daysToParty_Sun = daysToParty Sun @?= 5

unit_daysToParty_Mon :: Assertion
unit_daysToParty_Mon = daysToParty Mon @?= 4

unit_daysToParty_Tue :: Assertion
unit_daysToParty_Tue = daysToParty Tue @?= 3

unit_daysToParty_Wed :: Assertion
unit_daysToParty_Wed = daysToParty Wed @?= 2

unit_daysToParty_Thu :: Assertion
unit_daysToParty_Thu = daysToParty Thu @?= 1