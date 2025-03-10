DECLARE arr : ARRAY[1:10] OF INTEGER

FOR i = 1 TO 10 DO
    DECLARE x : INTEGER
    INPUT arr[i]
    OUTPUT arr[i], " ", arr[i] * arr[i]
NEXT i
