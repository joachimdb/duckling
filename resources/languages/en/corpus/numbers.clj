(
  ; Context map
  {}

  "0"
  "naught"
  "nought"
  "zero"
  "nil"
  (tkn {:dim :number
        :value 0})

  "1"
  "one"
  "single"
  (tkn {:dim :number
        :value 1})

  "2"
  "two"
  "a pair"
  (tkn {:dim :number
        :value 2})

  "33"
  "thirty three"
  "0033"
  (tkn {:dim :number
        :value 33})
  
  "14"
  "fourteen"
  (tkn {:dim :number
        :value 14})
  
  "16"
  "sixteen"
  (tkn {:dim :number
        :value 16})

  "17"
  "seventeen"
  (tkn {:dim :number
        :value 17})

  "18"
  "eighteen"
  (tkn {:dim :number
        :value 18})

  "1.1"
  "1.10"
  "01.10"
  (tkn {:dim :number
        :value 1.10})

  "0.77"
  ".77"
  (tkn {:dim :number
        :value 0.77})

  
  "100,000"
  "100000"
  "100K"
  "100k"
  (tkn {:dim :number
        :value 100000})
  
  "3M"
  "3000K"
  "3000000"
  "3,000,000"
  (tkn {:dim :number
        :value 3000000})
  
  "1,200,000"
  "1200000"
  "1.2M"
  "1200K"
  ".0012G"
  (tkn {:dim :number
        :value 1200000})

  "- 1,200,000"
  "-1200000"
  "minus 1,200,000"
  "negative 1200000"
  "-1.2M"
  "-1200K"
  "-.0012G"
  (tkn {:dim :number
        :value -1200000})

  "5 thousand"
  "five thousand"
  (tkn {:dim :number
        :value 5000})

  "one twenty two"
  (tkn {:dim :number
        :value 122})

  "two hundred thousand"
  (tkn {:dim :number
        :value 200000})

  "twenty-one thousand eleven"
  (tkn {:dim :number
        :value 21011})

  "seven hundred twenty-one thousand twelve"
  "seven hundred twenty-one thousand and twelve"
  (tkn {:dim :number
        :value 721012})

  "thirty-one million two hundred fifty-six thousand seven hundred twenty-one"
  (tkn {:dim :number
        :value 31256721})

  "4th"
  "fourth"
  (tkn {:dim :ordinal
        :value 4})
 
)

