DECLARE n : INTEGER
INPUT n
// With large values of n, we will have integer overflow

FOR i <- 0 TO n
    // calculates the i-th fibonacci number
    DECLARE f0 : INTEGER
    DECLARE f1 : INTEGER
    DECLARE f2 : INTEGER
    f0 <- 0
    f1 <- 1

    FOR j <- 1 TO i
        f2 <- f0
        f0 <- f1
        f1 <- f0 + f2          
    NEXT j

    OUTPUT f0
NEXT i