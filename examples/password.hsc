DECLARE Password : STRING
OUTPUT "Enter password: "
INPUT Password

IF Password = "admin" THEN
    OUTPUT "Access granted"
ELSE
    OUTPUT "Access denied"
ENDIF