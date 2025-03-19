DECLARE x: INTEGER
INPUT x
DECLARE digsum : INTEGER
digsum <- 0
WHILE x > 0
    digsum <- digsum + (x MOD 10)
    x <- x DIV 10
ENDWHILE

FOR i <- 1 TO 1 DO
    OUTPUT "sum = " , digsum
NEXT i