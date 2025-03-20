// finding all primes numbers up to 1000
DECLARE composite : ARRAY[1:1000] OF BOOLEAN
FOR i <- 1 TO 1000
    composite[i] <- FALSE
NEXT i

FOR i <- 2 TO 1000
    IF NOT composite[i] THEN
        OUTPUT i
        FOR j <- i*i TO 1000 STEP i
            composite[j] <- TRUE
        NEXT j
    ENDIF
NEXT i