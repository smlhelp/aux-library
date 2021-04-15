signature TIMING =
sig

    type day = int
    datatype month = Jan | Feb | Mar | Apr | May | Jun 
                   | Jul | Aug | Sep | Oct | Nov | Dec
    type year = int
    type date

    exception Invalid

    val year : date -> year
    val month : date -> month
    val day : date -> day

    val date : year * month * day -> date
    val fromEightDigit : string -> date

    val dateToString : date -> string
    val eightDigit : date -> string
    val compare : date * date -> order

    val century : year -> string
    val leapYear : year -> bool
 

    type timezone
    type offset

    val offsetCmpZero : offset -> order

    val hourMin : int * int -> offset
    val UTCPlus : int * int-> timezone
    val UTCMinus : int * int -> timezone
    val Local : timezone

    val intToMonth : int -> month
    val monthToInt : month -> int
    val monthToString : month -> string
   
    val monthSucc : month -> month
    val monthPred : month -> month
    val yearSucc : year -> year
    val yearPred : year -> year
    val dateSucc : date -> date
    val datePred : date -> date

    val numDays : month * year -> int
    val monthCmp : month * month -> order

    val diff : date * date -> int

    datatype weekday = Sunday | Monday | Tuesday | Wednesday
                 | Thursday | Friday | Saturday

    val weekdaySucc : weekday -> weekday
    val weekdayPred : weekday -> weekday

    val dayOfWeek : timezone -> weekday
    val today : timezone -> date
    

    type time
    type point = time * timezone
 
    val mkTime : int*int*int*int*int*int -> time
    val dateTime : date * int * int * int -> time

    val dayOf : time -> date
    val hour : time -> int
    val minute : time -> int
    val second : time -> int

    val now : timezone -> point
    val timeToString : time -> string
    val pointToString : point -> string

    val secPlus : time * int -> time
    val minPlus : time * int -> time
    val hourPlus : time * int -> time

    val minDiff : time * time -> int
    val hourDiff : time * time -> int

    type duration

    exception Negative

    val absDiff : time * time -> duration
    val durCmp : duration * duration -> order

    val Minutes : int -> duration
    val Seconds : int -> duration
    val CentiSeconds : int -> duration
    val durPlus : duration * duration -> duration

    val silentCountdown : duration -> unit
    val intervalCountdown : duration -> duration -> unit
    val stopwatch : duration -> unit
  
end
