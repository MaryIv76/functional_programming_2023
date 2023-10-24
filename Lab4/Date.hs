import Data.Char (isDigit)


data Date = Date Int Int Int 


instance Show Date where
  show (Date day month year) = show day ++ "." ++ show month ++ "." ++ show year


instance Eq Date where
  (Date day1 month1 year1) == (Date day2 month2 year2) = day1 == day2 && month1 == month2 && year1 == year2

instance Ord Date where
  compare (Date day1 month1 year1) (Date day2 month2 year2)
    | year1 /= year2    = compare year1 year2
    | month1 /= month2  = compare month1 month2
    | otherwise         = compare day1 day2


instance Read Date where
  readsPrec _ input = 
    let (day, rest1) = span isDigit input
        dayInt = read day :: Int
        (del1:rest2) = rest1
        (month, rest3) = span isDigit rest2
        monthInt = read month :: Int
        (del2:rest4) = rest3
        (year, rest5) = span isDigit rest4
        yearInt = read year :: Int
          in
      if del1 == '.' && del2 == '.' && dayInt >= 1 && dayInt <= 30 && monthInt >= 1 && monthInt <= 12 && yearInt >= 0 then
        [(Date dayInt monthInt yearInt, rest5)]
        else []



normalizeDate (Date day month year)
  | day > 30 = normalizeDate (Date (day - 30) (month + 1) year)
  | month > 12 = normalizeDate (Date day (month - 12) (year + 1))
  | otherwise = Date day month year


addDays (Date day month year) n = normalizeDate(Date (day + n) month year)


subtractDates1 (Date day1 month1 year1) (Date day2 month2 year2) = Date (day1 - day2) (month1 - month2) (year1 - year2)
subtractDates2 (Date day1 month1 year1) (Date day2 month2 year2) = normalizeDate(Date ((year1 - year2) * 365 + (month1 - month2) * 30 + (day1 - day2)) 0 0)
subtractDates3 (Date day1 month1 year1) (Date day2 month2 year2) = (year1 - year2) * 365 + (month1 - month2) * 30 + (day1 - day2)
