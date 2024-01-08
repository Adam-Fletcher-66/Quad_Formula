! Name: Adam_Fletcher
! Date: 02/25/2022
! Purpose: Calculate the quadratic formula for double, real, complex and &
!          single roots in the case of a=0

program quad_formula_2
  implicit none

                 ! Variable Dictionary
  real :: a, b, c             ! Variables of quadratic coefficients inputed
  real :: quad_p              ! Value of real root when radical is summed
  real :: quad_m              ! Value of real root when radical is subtracted  
  real :: radicand            ! Value of real radicand
  real :: radicand_i          ! Value of imaginary radicand
  real :: den                 ! Value of denominator in quad formula
  real :: x                   ! Single variable when a=0

  write(*,*) "Input the coefficients (a,b,c) of the quadratic polynomial."
  write(*,*) "Press 'enter' after inputing each coefficient:"
  read(*,*) a, b , c                         ! Reads in coefficient values

  radicand = (b**2 -4.0 * a * c)             ! Calculates radicand

  den = 2.0 * a                              ! Calculates denominator
     


  if(radicand<0.0) then
     
     radicand_i = -1.0 * radicand            ! Makes radicand a real number

     write(*,*) "The complex roots are:(",-b / den,"+", sqrt(radicand_i) / den &
                 ,"i),(", -b / den,"-", sqrt(radicand_i) / den, "i)"
                                             ! Displays complex roots
  else
     if(a==0) then

        x = (-c) / b                         ! Calculates single real root

        write(*,*) "Only one real root exists: x =", x

     else
        if(radicand==0.0) then
           
        quad_p = (-b + sqrt(radicand)) / den ! Calculates real double root
        
        write(*,*) "The real double root value is: x =", quad_p
                                             ! Displays double root answer
        else
           if(radicand>0.0) then
     
             quad_p = (-b + sqrt(radicand)) / den
                                             ! Calculates real root when summed
             quad_m = (-b - sqrt(radicand)) / den
                                             ! Calculates when subtracted 
             write(*,*) "Two real roots exist: x =", quad_p, ",", quad_m
                                             ! Displays 2 real roots

          endif
       endif
    endif
  endif                                      ! Stops each 'if' execution
  
  
  stop 0                                     ! Stops program execution
  
  
end program quad_formula_2

