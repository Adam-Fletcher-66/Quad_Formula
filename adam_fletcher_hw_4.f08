! Name: Adam_Fletcher
! Date: 02/18/2022
! Purpose: Calculate the quadratic formula for double, real, and complex roots

program quad_formula
  implicit none

                 ! Variable Dictionary
  real :: a, b, c             ! Variables of quadratic coefficients inputed
  real :: quad_p              ! Value of real root when radical is summed
  real :: quad_m              ! Value of real root when radical is subtracted  
  real :: radicand            ! Value of real radicand
  real :: radicand_i          ! Value of imaginary radicand
  real :: den                 ! Value of denominator in quad formula

  write(*,*) "Input the coefficients (a,b,c) of the quadratic polynomial."
  write(*,*) "Press 'enter' after inputing each coefficient:"
  read(*,*) a, b , c          ! Reads in coefficient values

  radicand = (b**2 -4.0 * a * c)
                              ! Calculates radicand

  den = 2.0 * a               ! Calculates denominator
     


  if(radicand<0.0) then
     
     radicand_i = -1.0 * radicand
                              ! Makes radicand real in order to calculate

     
     write(*,*) "The complex roots are:(",-b / den,"+", sqrt(radicand_i) / den &
                 ,"i),(", -b / den,"-", sqrt(radicand_i) / den, "i)"
                              ! Displays complex roots

  elseif(radicand==0.0) then
     
     quad_p = (-b + sqrt(radicand)) / den
                              ! Calculates real double root 

     write(*,*) "One real root exists. The double root value is: x =", quad_p
                              ! Displays double root answer

  elseif(radicand>0.0) then
     
     quad_p = (-b + sqrt(radicand)) / den
                              ! Calculates real root when summed

     quad_m = (-b - sqrt(radicand)) / den
                              ! Calculates real root when subtracted 

     write(*,*) "The real roots are: x =", quad_p, ",", quad_m
                              ! Displays 2 real roots

  endif                       ! Stops 'If' execution
  
  
  stop 0                      ! Stops program execution
  
  
end program quad_formula

