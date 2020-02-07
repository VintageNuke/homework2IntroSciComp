FUNCTION power(a, b)
    IMPLICIT NONE
    !The obvious method is boring. Implemented the efficient square root method from Wikipedia. https://en.wikipedia.org/wiki/Exponentiation_by_squaring
    INTEGER(KIND=4), INTENT(IN)  :: a, b
    INTEGER(KIND=4) :: x, y, n, power

    !Y has to be initialized to the multiplicative identity first
    y = 1
    x = a
    n = b

    DO WHILE (y > 0)
        !If even, square the base, and divide the exponent by 2
        IF (MOD(y, 2) .EQ. 0) THEN
            x = x * x
            n = n / 2
        !If odd, then make it into the form x * x^(n-1) where the outside x becomes y for this section.
        ELSE
            y = x * y
            x = x * x
            n = (n-1) / 2
        ENDIF
    END DO
    !Check if the original input was 0, if not calculate the answer
    IF (b == 0) THEN
       power = 1
    ELSE
       power = x * y
    ENDIF
END FUNCTION power

END MODULE

PROGRAM printPower
IMPLICIT NONE
    print*,  power(2,5)
    print*,  power(3,2)
    print*,  power(4,2)
    print*,  power(5,1)
    print*,  power(2,0)

END PROGRAM printPower
