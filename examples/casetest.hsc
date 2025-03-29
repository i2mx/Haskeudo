FUNCTION f(x : INTEGER) RETURNS INTEGER
    RETURN x * x
ENDFUNCTION

DECLARE x : INTEGER
INPUT x

CASE OF x
    1 : OUTPUT "x is 1"
    2 : OUTPUT "x is 2"
    3 : OUTPUT "x is 3"
        OUTPUT "this is my favourite number"
    4 : OUTPUT "x is 4"
ENDCASE

CASE OF x
    1 : OUTPUT "x is 1"
    2 : OUTPUT "x is 2"
    3 : OUTPUT "x is 3"
        OUTPUT "this is my favourite number"
    4 : OUTPUT "x is 4"
    OTHERWISE : OUTPUT "x is not 1, 2, 3 or 4"
ENDCASE