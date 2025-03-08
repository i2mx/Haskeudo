// finding all primes numbers up to 500
DECLARE composite : ARRAY[1:500] OF BOOLEAN
FOR i = 1 TO 500 DO
    composite[i] <- FALSE
NEXT i

FOR i = 2 TO 500 DO
    IF NOT composite[i] THEN
        OUTPUT i
        FOR j = i*i TO 500 STEP i DO
            composite[j] <- TRUE
        NEXT j
    ENDIF
NEXT i