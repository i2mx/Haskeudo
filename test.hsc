FUNCTION s() RETURNS INTEGER
    RETURN 1
ENDFUNCTION

FUNCTION a(x : INTEGER) RETURNS INTEGER
    RETURN x
ENDFUNCTION

PROCEDURE greet(name : STRING)
    OUTPUT "Hello ", name
ENDPROCEDURE

OUTPUT s()
CALL greet("World")