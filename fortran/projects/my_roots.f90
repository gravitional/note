PROGRAM roots
! Purpose:
! This program solves for the roots of a quadratic equation of the form
! A * X**2 + B * X + C = 0. It calculates the answers regardless of the
! type of roots that the equation possesses (Fortran 95/2003 style).
!
   IMPLICIT NONE
! Declare the variables used in this program
   REAL :: a ! Coefficient of X**2 term of equation
   REAL :: b ! Coefficient of X term of equation
   REAL :: c ! Constant term of equation
   REAL :: discriminant ! Discriminant of the equation
   REAL :: imag_part ! Imaginary part of equation (for complex roots)
   REAL :: real_part ! Real part of equation (for complex roots)
   REAL :: x1 ! First solution of equation (for real roots)
   REAL :: x2 ! Second solution of equation (for real roots)
! Prompt the user for the coefficients of the equation
   WRITE (*,*) 'This program solves for the roots of a quadratic '
   WRITE (*,*) 'equation of the form A * X**2 + B * X + C = 0. '
   WRITE (*,*) 'Enter the coefficients A, B, and C:'
   READ (*,*) a, b, c
! Echo back coefficients
   WRITE (*,*) 'The coefficients A, B, and C are: ', a, b, c
! Calculate discriminant
   discriminant = b**2 - 4. * a * c
! Solve for the roots, depending upon the value of the discriminant
   IF ( discriminant > 0. ) THEN ! there are two real roots, so...
      X1 = ( -b + sqrt(discriminant) ) / ( 2. * a )
      X2 = ( -b - sqrt(discriminant) ) / ( 2. * a )
      WRITE (*,*) 'This equation has two real roots:'
      WRITE (*,*) 'X1 = ', x1
      WRITE (*,*) 'X2 = ', x2
   ELSE IF ( discriminant == 0. ) THEN ! there is one repeated root, so...
      x1 = ( -b ) / ( 2. * a )
      WRITE (*,*) 'This equation has two identical real roots:'
      WRITE (*,*) 'X1 = X2 = ', x1
   ELSE ! there are complex roots, so ...
      real_part = ( -b ) / ( 2. * a )
      imag_part = sqrt ( abs ( discriminant ) ) / ( 2. * a )
      WRITE (*,*) 'This equation has complex roots:'
      WRITE (*,*) 'X1 = ', real_part, ' +i ', imag_part
      WRITE (*,*) 'X2 = ', real_part, ' -i ', imag_part
   END IF
END PROGRAM roots
