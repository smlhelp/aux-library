# Timing module

## Overview
Provide common utilites relating to date, time, and timing. Largely functions as porcelain for the SMLNJ basis modules `Date` and `Time`, but also provides functions like `stopwatch` and `wait` which have the effect of causing specific time delays (which are implemented using more sophisticated features of SMLNJ). Serve as a good example of the use of custom `datatype`s  in SML (e.g. the `month`, `timezone`, and `weekday` types), and general functional programming skills (e.g. pattern matching, use of shortcircuiting, use of the `order` type, some higher-order functions). 

Some of the functions (specifically `getSMLTimeOffset`, `now`, `dayOfWeek`, `wait`, `silentCountdown`, `intervalCountdown`, and `stopwatch`) contain features of SMLNJ not covered in 15-150. 
