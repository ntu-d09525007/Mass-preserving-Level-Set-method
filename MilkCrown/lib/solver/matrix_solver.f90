MODULE MATRIX_SOLVER
IMPLICIT NONE

CONTAINS

SUBROUTINE TWIN_DEC( A, B, AA, BB, IS, IE )
IMPLICIT NONE
real(8), DIMENSION(3,IS:IE) :: A,  B
real(8), DIMENSION(3,IS:IE) :: AA, BB
!--------------
INTEGER ::  I, IS, IE
real(8) ::  den , SC1 , SC2
 
 A (1,IS) = 0.0_8
 AA(1,IS) = 0.0_8
 A (3,IE) = 0.0_8
 AA(3,IE) = 0.0_8

 B (1,IS) = 0.0_8
 BB(1,IS) = 0.0_8
 B (3,IE) = 0.0_8
 BB(3,IE) = 0.0_8

 DO i = IS+1, IE
   Den = A(2,i-1)*BB(2,i-1) - AA(2,i-1)*B(2,i-1)
   SC1 = -( AA(2,i-1)*B(1,i) - A(1,i)*BB(2,i-1) )/Den
   SC2 =  (  B(1,i)*A(2,i-1) - B(2,i-1)*A(1,i)  )/Den

   A(1,i) = SC1
   A(2,i) = A(2,i) - ( SC1*A(3,i-1) + SC2*AA(3,i-1) )

   B(1,i) = SC2
   B(2,i) = B(2,i) - ( SC1*B(3,i-1) + SC2*BB(3,i-1) )
  !=========================================
   SC1 = -( AA(2,i-1)*BB(1,i) - AA(1,i)*BB(2,i-1) )/Den
   SC2 =  ( BB(1,i)*A(2,i-1)  -  B(2,i-1)*AA(1,i) )/Den

   AA(1,i) = SC1
   AA(2,i) = AA(2,i) - (SC1*A(3,i-1) + SC2*AA(3,i-1))

   BB(1,i) = SC2
   BB(2,i) = BB(2,i) - (SC1*B(3,i-1) + SC2*BB(3,i-1))
 END DO

END SUBROUTINE TWIN_DEC

SUBROUTINE TWIN_BKS( A, B, AA, BB, S, SS, IS, IE )
IMPLICIT NONE
real(8), DIMENSION(3,IS:IE) :: A,  B
real(8), DIMENSION(3,IS:IE) :: AA, BB
real(8), DIMENSION(IS:IE)   :: S , SS
!--------------
INTEGER :: I, IS, IE
real(8) :: Den, SolS, SolSS

 DO i = IS+1, IE
   S(i)  = S(i)  -(  A(1,i)*S(i-1) +  B(1,i)*SS(i-1) )
   SS(i) = SS(i) -( AA(1,i)*S(i-1) + BB(1,i)*SS(i-1) )
 END DO

 Den   = A(2,IE)*BB(2,IE) - AA(2,IE)*B(2,IE)
 SolS  = -( B(2,IE)*SS(IE) - BB(2,IE)*S(IE) )/Den
 SolSS =  ( A(2,IE)*SS(IE) - AA(2,IE)*S(IE) )/Den
 S(IE)  = SolS
 SS(IE) = SolSS

 DO i = IE-1, IS, -1
   S(i)  = S(i)  - (  A(3,i)*SolS +  B(3,i)*SolSS )
   SS(i) = SS(i) - ( AA(3,i)*SolS + BB(3,i)*SolSS )

   Den   = A(2,i)*BB(2,i) - AA(2,i)*B(2,i)
   SolS  = -( B(2,i)*SS(i) - BB(2,i)*S(i))/Den
   SolSS =  ( A(2,i)*SS(i) - AA(2,i)*S(i))/Den
   S(i)  = SolS
   SS(i) = SolSS
 END DO

END SUBROUTINE TWIN_BKS

END MODULE MATRIX_SOLVER
