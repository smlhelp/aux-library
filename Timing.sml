infixr $
fun f $ x = f x
signature TIMING = sig
type day = int
datatype month = Jan | Feb | Mar | Apr 
               | May | Jun | Jul | Aug 
               | Sep | Oct | Nov | Dec
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

datatype weekday = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday


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
    val durToString : duration -> string

    val wait : duration -> (unit -> 'a) -> 'a
    val timeIterate : ('a ref -> (unit -> 'b) -> (unit -> 'b) -> 'b) 
                    -> 'a 
                    -> duration 
                    -> (unit -> 'b)
                    -> 'b
    val intervalCountdown : duration -> duration -> unit
    val stopwatch : duration -> unit
end
structure Timing :> TIMING=
struct
   
type day = int
datatype month = Jan | Feb | Mar | Apr 
               | May | Jun | Jul | Aug 
               | Sep | Oct | Nov | Dec
type year = int
type date = year * month * day

exception Invalid

val year = fn (YY,_,_) => YY
val month = fn (_,MM,_) => MM
val day = fn (_,_,DD) => DD

fun leapYear YY = 
((YY mod 4 = 0) andalso (YY mod 100 <> 0))
    orelse
(YY mod 400 = 0)

fun numDays (MM,YY) =
    case MM of
      Sep => 30
    | Apr => 30 | Jun => 30 | Nov => 30
    
    | Jan => 31 | Mar => 31 | May => 31
    | Jul => 31 | Aug => 31 | Oct => 31
    | Dec => 31

    | Feb=> if (leapYear YY) 
            then 29 
            else 28

fun date (YY,MM,DD):date = 
  let
    val _ = (YY <> 0) orelse raise Invalid
    val _ = (
                (0 < DD) andalso 
                (DD <= (numDays (MM,YY)))
            ) 
            orelse raise Invalid
  in
    (YY,MM,DD)
  end

    fun intToMonth n =
      case n of
        1 => Jan | 2 => Feb | 3 => Mar | 4 => Apr | 5 => May | 6 => Jun 
      | 7 => Jul | 8 => Aug | 9 => Sep | 10 => Oct | 11 => Nov | 12 => Dec
      | _ => raise Invalid

    fun monthToInt MM = 
      case MM of Jan => 1 | Feb => 2 | Mar => 3 | Apr => 4 | May => 5 | Jun => 6
         | Jul => 7 | Aug => 8 | Sep => 9 | Oct => 10 | Nov => 11 | Dec => 12

    fun monthToString MM = 
      case MM of Jan => "January" | Feb => "February" | Mar => "March" 
         | Apr => "April" | May => "May" | Jun => "June" | Jul => "July" 
         | Aug => "August" | Sep => "September" | Oct => "October" 
         | Nov => "November" | Dec => "December"

fun monthSucc MM = 
  case MM of 
    Jan => Feb | Feb => Mar | Mar => Apr 
  | Apr => May | May => Jun | Jun => Jul 
  | Jul => Aug | Aug => Sep | Sep => Oct 
  | Oct => Nov | Nov => Dec | Dec => Jan
fun monthPred MM = 
  case MM of 
    Jan => Dec | Feb => Jan | Mar => Feb 
  | Apr => Mar | May => Apr | Jun => May 
  | Jul => Jun | Aug => Jul | Sep => Aug 
  | Oct => Sep | Nov => Oct | Dec => Nov

   
fun yearSucc ~1 = 1
  | yearSucc n = n+1

    fun yearPred 1 = ~1
      | yearPred n = n-1

(* datePred : date -> date *)
fun datePred (YY,Jan,1) =
        (yearPred YY,Dec,31)
  | datePred (YY,MM,1) = 
        (YY, monthPred MM, 
         numDays(monthPred MM,YY))
  | datePred (YY,MM,DD) = (YY,MM,DD-1)
(* dateSucc : date -> date *)
fun dateSucc (YY,Dec,31) = 
        (yearSucc YY,Jan,1)
  | dateSucc (YY,MM,DD) = 
        if DD = (numDays (MM,YY))
        then (YY,monthSucc MM,1)
        else (YY,MM,DD+1)



    (* More advanced string-processing code. Kindly ignore (for now)  *)
        fun isDigits s = (s<>"") andalso
            foldr (fn (b1,b2) => b2 andalso b1) true (map Char.isDigit (String.explode s))
        val fst = fn (a,b) => a

        (* REQUIRES: isDigits s orelse s=="~" ^ s' where isDigits s'*)
        fun toInt s =
          (case (String.isPrefix "~" s) of
            true =>
              let
                  val rest = String.extract(s,1,NONE) 
                    handle Subscript => raise Invalid
              in
                ~(toInt rest)
              end
          | false =>
              let 
                  val _ = (isDigits s) orelse raise Invalid
                  fun digitToInt d = (Char.ord d) - 48
                  fun cmb (c,(res,mult)) = 
                    ((mult * (digitToInt c)) + res, mult*10)
              in
                fst $ (foldr cmb (0,1)) $ (String.explode) $ s
              end)
    (* End advanced code *)

    fun fromEightDigit s =
      (fn [YS,MS,DS] => date(toInt YS, intToMonth(toInt MS), toInt DS) | _ =>
        raise Invalid)
      $ (String.tokens (fn c => c = #"-") s)
     
   fun zeroPad (s,l) = case (Int.compare (String.size s,l)) of
                            LESS => zeroPad ("0"^s,l)
                          | _ => s

   fun eightDigit (YY,MM,DD) =
     (zeroPad(Int.toString YY,4)) ^ "-" ^ (zeroPad(Int.toString(monthToInt MM),2))
     ^ "-" ^ (zeroPad(Int.toString DD,2))
    
   fun dateToString (YY,MM,DD) =
    let 
      fun yearFormat YY = if YY<0 
                          then (Int.toString (Int.abs YY))^" BCE"
                          else Int.toString YY
    in                      
      (Int.toString DD) ^ " " ^ (monthToString MM) ^ " " ^ (yearFormat YY)
    end
 
    

   (* Useful HOF *)
  fun lex2 (cmpA : 'a * 'a -> order) (cmpB : 'b * 'b -> order) =
    fn ((a1,b1),(a2,b2)) =>
      (case (cmpA(a1,a2),cmpB(b1,b2)) of
        (EQUAL,y)=>y
       |(x,_) => x)
  fun lex3 cmpA cmpB cmpC =
    fn ((a1,b1,c1),(a2,b2,c2)) =>
      (case (cmpA(a1,a2),cmpB(b1,b2),cmpC(c1,c2)) of
        (EQUAL,EQUAL,z) => z
       |(EQUAL,y,_) => y
       |(x,_,_) => x)

  fun monthCmp(M1,M2) = Int.compare(monthToInt M1,monthToInt M2)

  val compare = lex3 Int.compare monthCmp Int.compare

  fun century YY = 
    case (Int.sign YY, YY div 100, ((YY div 100)+1) mod 10) of
        (~1,_,_) => century(Int.abs YY) ^ " BCE"
    |   (0,_,_) => raise Invalid
    (* English is fun *)
    |   (_,NN,1) => (Int.toString (NN+1)) ^ "st"
    |   (_,NN,2) =>  (Int.toString (NN+1)) ^ "nd"
    |   (_,NN,3) =>  (Int.toString (NN+1)) ^ "rd"
    |   (_,NN,_) =>  (Int.toString (NN+1)) ^ "th"


   type offset = int*int
   
   fun hourMin (hh,mm) =
     let
       val _ = (Int.sameSign (hh,mm)) orelse (mm=0) orelse raise Invalid
       val _ = (Int.abs(mm) < 60) orelse raise Invalid
       val _ = (Int.abs(hh) <= 12) orelse raise Invalid
     in
       (hh,mm)
     end

   datatype timezone = Offset of offset | Local
   val UTCPlus = fn Off => Offset(hourMin Off)
   fun UTCMinus(hh,mm) = UTCPlus(~hh,~mm)

   fun offsetCmpZero (hh,mm) =
     lex2 Int.compare Int.compare ((hh,mm),(0,0))

   fun diff (date1 as (Y1,M1,D1), date2 as (Y2,M2,D2)) = 
     case (Int.compare(Y1,Y2),monthCmp(M1,M2)) of
        (LESS,_) => (case (leapYear Y1, leapYear(yearSucc Y1), date1) of
                        (true,_,(_,Feb,29)) => 365 + diff((yearSucc Y1,Feb,28),date2)
                       |(true,_,(_,Feb,_)) => 366 + diff((yearSucc Y1,M1,D1),date2)
                       |(true,_,(_,Jan,_)) => 366 + diff((yearSucc Y1,M1,D1),date2)
                       |(_,true,(_,Jan,_)) => 365 + diff((yearSucc Y1,M1,D1),date2)
                       |(_,true,(_,Feb,_)) => 365 + diff((yearSucc Y1,M1,D1),date2)
                       |(_,true,_)         => 366 + diff((yearSucc Y1,M1,D1),date2)
                       | _                 => 365 + diff((yearSucc Y1,M1,D1),date2))
       |(EQUAL,LESS) => (numDays(M1,Y1)) + diff((Y1,monthSucc M1,D1),date2)
       |(EQUAL,GREATER) => 
           diff(date1,(Y1,monthPred M2,D2)) - (numDays(monthPred M2,Y2))
       |(EQUAL,EQUAL) => D2 - D1
       |(GREATER,_) => ~(diff(date2,date1))
                        
        fun fromSMLMonth Date.Jan = Jan
          | fromSMLMonth Date.Feb = Feb
          | fromSMLMonth Date.Mar = Mar
          | fromSMLMonth Date.Apr = Apr
          | fromSMLMonth Date.May = May
          | fromSMLMonth Date.Jun = Jun
          | fromSMLMonth Date.Jul = Jul
          | fromSMLMonth Date.Aug = Aug
          | fromSMLMonth Date.Sep = Sep
          | fromSMLMonth Date.Oct = Oct
          | fromSMLMonth Date.Nov = Nov
          | fromSMLMonth Date.Dec = Dec
 
datatype weekday = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
fun weekdaySucc W = case W of
    Sunday => Monday 
  | Monday => Tuesday 
  | Tuesday =>  Wednesday 
  | Wednesday => Thursday 
  | Thursday => Friday 
  | Friday => Saturday 
  | Saturday => Sunday

fun weekdayPred W = case W of
    Sunday => Saturday | Monday => Sunday | Tuesday => Monday | Wednesday => Tuesday | Thursday => Wednesday | Friday => Thursday | Saturday => Friday

    type time = date * int * int * int
    type point = time * timezone


    fun intervalCmp (a,b) x =
      case (x<a,x<=b) of
        (true,_) => LESS
      | (false,true) => EQUAL
      | (false,false) => GREATER

    fun dateTime(D,hh,mm,ss) = case 
      (intervalCmp (0,59) ss, intervalCmp (0,59) mm, intervalCmp (0,23) hh) of
         (LESS,_,_) => dateTime(D,hh,mm-1,ss+60)
        |(GREATER,_,_) => dateTime(D,hh,mm+1,ss-60)
        |(EQUAL,LESS,_) => dateTime(D,hh-1,mm+60,ss)
        |(EQUAL,GREATER,_) => dateTime(D,hh+1,mm-60,ss)
        |(EQUAL,EQUAL,LESS) => dateTime(datePred D,hh+24,mm,ss)
        |(EQUAL,EQUAL,GREATER) => dateTime(dateSucc D,hh-24,mm,ss)
        |(EQUAL,EQUAL,EQUAL) => (D,hh,mm,ss)

    fun mkTime (YY,MM,DD,hh,mm,ss) =
      dateTime(date(YY,intToMonth MM,DD),hh,mm,ss)

    fun dayOf (D,_,_,_) = D
    fun hour  (_,hh,_,_) = hh
    fun minute (_,_,mm,_) = mm
    fun second (_,_,_,ss) = ss

    fun hourPlus ((D,hh,mm,ss),n) = dateTime(D,hh+n,mm,ss)
    fun minPlus ((D,hh,mm,ss),n) = dateTime(D,hh,mm+n,ss)
    fun secPlus ((D,hh,mm,ss),n) = dateTime(D,hh,mm,ss+n)

    fun getSMLTimeOffset Zone = 
      let
        val timezoneFunction = 
          case Zone of
            Local => Date.fromTimeLocal
          | (Offset(hh,mm)) => 
                    (fn smlTime0 => 
                        Date.fromTimeUniv(
                            Time.+(
                                smlTime0,
                                Time.fromSeconds(IntInf.fromInt ((60*mm)+(3600*hh)))
                            )
                        )
                    )
       in
        timezoneFunction (Time.now())
       end

    fun now Zone =
      let
        val smlTimeOffset = getSMLTimeOffset Zone
      in
        (dateTime((Date.year smlTimeOffset, 
                 fromSMLMonth(Date.month smlTimeOffset), 
                 Date.day smlTimeOffset),
                 Date.hour smlTimeOffset,
                 Date.minute smlTimeOffset,
                 Date.second smlTimeOffset),
         Zone)
      end
    fun dayOfWeek Zone = 
      case (Date.weekDay(getSMLTimeOffset Zone)) of
          Date.Sun => Sunday
        | Date.Mon => Monday
        | Date.Tue => Tuesday
        | Date.Wed => Wednesday
        | Date.Thu => Thursday
        | Date.Fri => Friday
        | Date.Sat => Saturday

    fun today Zone = dayOf(fst(now Zone))

    fun twoDigitFormat i = zeroPad(Int.toString i,2)
    fun timeToString (D,hh,mm,ss) =
            (eightDigit D) ^ " " 
            ^ (twoDigitFormat hh) ^ ":" 
            ^ (twoDigitFormat mm) ^ ":" 
            ^ (twoDigitFormat ss)

    fun minDiff (T1,T2) =
      (24*60*(diff(dayOf T1,dayOf T2)))
                   +
      (60 * ((hour T2) - (hour T1)))
                   +
      ((minute T2) - (minute T1))

    fun hourDiff (T1,T2) =
      (24*(diff(dayOf T1,dayOf T2)))
                   +
      ((hour T2) - (hour T1))



    fun pointToString (TT,Offset(hh,mm)) = 
      let
        val UTCstring = "(UTC"
                        ^ (if Int.min(hh,mm)>=0 then "+" else "-")
                        ^ (Int.toString(Int.abs hh)) ^ ":"
                        ^ (twoDigitFormat mm) ^ ")"
      in
        (timeToString TT) ^ " " ^ UTCstring
      end
     | pointToString (TT,Local) = 
      let
        val minOff = minDiff(fst(now(UTCPlus(0,0))),fst(now Local))
        val (hh,mm) = (minOff div 60,minOff mod 60)
        val UTCstring = "(UTC"
                        ^ (if Int.min(hh,mm)>=0 then "+" else "-")
                        ^ (Int.toString(Int.abs hh)) ^ ":"
                        ^ (twoDigitFormat mm) ^ ")"
      in
        (timeToString TT) ^ " " ^ UTCstring
      end

    type duration = int

    exception Negative

    fun absDiff (T1,T2) =
      Int.abs( (6000*minDiff(T1,T2)) + (100*((second T2) - (second T1))))
    val durCmp = Int.compare

    fun Minutes n = if n<0 then raise Negative else n*6000
    fun Seconds n = if n<0 then raise Negative else n*100
    fun CentiSeconds n = if n<0 then raise Negative else n
    val durPlus = op +
    fun durToString n = 
      case (n>=6000, n mod 100) of
        (true,_) => (Int.toString(n div 6000)) ^ " minutes, " ^ durToString(n mod 6000)
      | (_,0) => (Int.toString(n div 100)) ^ " seconds"
      | (_,r) => (Int.toString(n div 100)) ^ "." 
                 ^ (zeroPad(Int.toString r,2)) ^ " seconds"

    fun finish () = ignore(
      SMLofNJ.IntervalTimer.setIntTimer NONE;
      Signals.setHandler (Signals.sigALRM, Signals.IGNORE))
    fun finally f final =
      f () before ignore (final ())
      handle e => (final (); raise e)
    val timer = SMLofNJ.IntervalTimer.setIntTimer

    fun wait 0 sk = sk ()
      | wait D sk=
          let 
            val t = Time.fromMilliseconds (IntInf.fromInt (10*D))
            fun doit k =
              let 
                fun handler _ = k
                val _ = Signals.setHandler (Signals.sigALRM,
                                              Signals.HANDLER handler)
                val () = timer (SOME t)
              in 
                Signals.pause() 
              end
              val _ = finally (fn () => SMLofNJ.Cont.callcc doit) finish
          in 
            sk() 
          end
 

    fun timeIterate step init inter done =
      let
        val counter = ref init
        fun loop k = step counter (fn () => wait inter (fn () => loop k)) k
      in
        loop done
      end

    (* REQ: inter>0 *)
    fun intervalCountdown inter length = 
      let
        fun step counter continue kill = 
          case !counter of
            0 => kill()
          | r => (print ((durToString r) ^ " remaining!\n"); 
                  counter:=r-inter;
                  continue())
        val leftover = length mod inter
      in
        wait leftover (fn () => 
        timeIterate step (length-leftover) inter (fn () => print "Done!\n"))
      end


    fun stopwatch inter = 
      let
        fun step counter continue _ = (
          print ((durToString (!counter)) ^ " elapsed!\n"); 
          counter:=(!counter)+inter;
          continue())
      in
        timeIterate step 0 inter (fn () => raise Fail "Should not reach this point")
      end
end
(*
fun datePrettyPrint pps D = PrettyPrint.string pps (Timing.dateToString D)

val _ = CompilerPPTable.install_pp ["Timing","date"] datePrettyPrint*)
