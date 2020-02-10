

! 1. MODULE funcs
! 2. MODULE solvers
! 3. PROGRAM MAIN


MODULE func

        CONTAINS
        
        FUNCTION f1(x)
                IMPLICIT NONE
                REAL(KIND=8) :: f1
                REAL(KIND=8), INTENT(IN) :: x
                f1 = x**3 - 8
        END FUNCTION f1

        FUNCTION fp1(x)
                IMPLICIT NONE
                REAL(KIND=8) :: fp1
                REAL(KIND=8), INTENT(IN) :: x
                fp1 = 2*x**2
        END FUNCTION

END MODULE func


MODULE solvers
        CONTAINS

        SUBROUTINE bisect( xa, xb, f, tolerr, maxiter, xc, abserr, iter   )
                IMPLICIT NONE
                REAL(KIND=8), INTENT(IN) :: xa, xb, tolerr
                INTEGER(KIND=4), INTENT(IN) :: maxiter

                ! Output variables
                REAL(KIND=8), INTENT(OUT) :: xc, abserr
                INTEGER(KIND=4),INTENT(OUT) :: iter

                ! Declare xc_old
                REAL(KIND=8) :: xc_old, f
                REAL(KIND=8) :: fafc, xs(2,0:maxiter)

                iter = 0
                xc = xb + xa
                xs(1,0) = xa
                xs(2,0) = xb
                abserr = HUGE(1.0d0)                
                IF (f(xa) * f(xb) > 0.0) RETURN

                OPEN(UNIT=25, FILE='bisect.dat', STATUS='REPLACE', FORM='FORMATTED')
                WRITE(UNIT=25, *) 'root error iterations'
                

                DO WHILE (abserr > tolerr .and. iter < maxiter)

                        ! specify  xc_old = xc
                        xc_old = xc
                        xc = 0.5 * (xs(1,iter)+xs(2,iter))

                        ! Move to bottom of loop... OOPS!

                        fafc = f(xs(1,iter)) * f(xc)
                        
                        IF (fafc < 0.0) THEN
                                ! Change LHS index to iter+1
                                ! Change RHS index to iter
                                xs(2,iter+1) = xc
                                xs(1,iter+1) = xs(1,iter)

                        ELSEIF (fafc > 0.0) THEN
                                ! Fix indices here the same
                                xs(1,iter+1) = xc
                                xs(2,iter+1) = xs(2,iter)

                        ELSE
                                xc_old = xc
                        ENDIF

                ! Finish defining abserr expression
                abserr = ABS( (xc - xc_old) / xc_old)

                ! Update iteration count here, makes better sense
                iter = iter + 1

                ! Write to bisect.dat
                WRITE(UNIT=25, *)  xc, ' ', abserr, ' ', iter

                ENDDO
                CLOSE(25)
        END SUBROUTINE bisect


        SUBROUTINE newton(f,fp,x0,tolerr,maxiter,xc,abserr,iter)
        IMPLICIT NONE

                REAL(KIND=8), INTENT(IN)     :: x0, tolerr
                INTEGER(KIND=4), INTENT(IN)  :: maxiter

                REAL(KIND=8), INTENT(OUT)    :: xc, abserr
                INTEGER(KIND=4), INTENT(OUT) :: iter

                REAL(KIND=8)                 :: f, fp, xs(0:maxiter), xn

                abserr = HUGE(1.0d0)
                xs(0) = x0
                iter = 0
                
                ! Open a new file to print current iterations
                OPEN(UNIT = 25, FILE='newton.dat', STATUS='REPLACE',FORM='FORMATTED')
                WRITE(25,*) 'root error iterations'

                DO WHILE (abserr > tolerr .and. iter < maxiter)
                        xn = xs(iter)
                        xs(iter+1) = xn - f(xn) / fp(xn)
                        abserr = ABS((xs(iter+1) - xn) / xn)
                        iter = iter + 1
                        ! Write current error stuff
                        WRITE(25,*) xc, ' ', abserr, ' ', iter
                ENDDO
                xc = xs(iter)
                CLOSE(25)
        END SUBROUTINE newton

END MODULE solvers

PROGRAM main
USE func
USE solvers
IMPLICIT NONE
        ! Declare maxiter, tolerr
        integer(kind=4), parameter :: maxiter = 1000
        real(kind=8), parameter :: tolerr = 1.d-3 ! 0.001
        ! Declare xa, xb
        real(kind=8) :: xa=0.0, xb=3.0, x0 = 1.0

        ! Declare iter, xstar, abserr
        real(kind=8) :: xstar, abserr
        integer(kind=4) :: iter

        ! Call bisection method subroutine
        CALL bisect(xa, xb, f1, tolerr, maxiter, xstar, abserr, iter)

        ! Print out to screen xstar, iter, abserr
        print *, 'BISECTION'
        print *, "root =", xstar
        print *, "abserr =", abserr
        print *, "iterations =", iter
        
        print *, ""
        CALL newton(f1,fp1,x0,tolerr,maxiter,xstar,abserr,iter)
        print *, "NEWTON"
        print *, "root =", xstar
        print *, "abserr =", abserr
        print *, "iterations =", iter


END PROGRAM main




