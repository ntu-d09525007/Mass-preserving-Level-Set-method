subroutine UCCD(N,dx,u,f,fx)
USE PRECISION
implicit none
integer :: n,I
real(kind=dp) :: dx
real(kind=dp),dimension(1:n) :: U,f,fx,fxx,su,ssu,sd,ssd
real(kind=dp),dimension(3,n) :: AU,AAU,BU,BBU
real(kind=dp),dimension(3,n) :: AD,AAD,BD,BBD

 call ASSIGN_CCD_UD_TWIN(N , Dx , AU , BU , AAU , BBU, AD , BD , AAD , BBD)
 CALL TWIN_DEC( AU, BU, AAU, BBU )
 CALL TWIN_DEC( AD, BD, AAD, BBD ) 
 
 CALL ASSIGN_CCD_UD_TWIN_SRC(N , Dx , F , SU , SSU , SD , SSD)
 CALL TWIN_BKS( AU, BU, AAU, BBU, SU , SSU )
 CALL TWIN_BKS( AD, BD, AAD, BBD, SD , SSD )
 
 DO I = 1, N
 	
 	IF( U(I) >= 0.0_DP )THEN
 		FX(I) = SU(I)
 	ELSE
 	  FX(I) = SD(I)
 	END IF
 	
 	FXX(i) = 0.5_DP*(SSU(I)+SSD(I))
 	
 END DO
 
end subroutine

subroutine CCD(N,dx,f,fx,fxx)
USE PRECISION
implicit none
integer :: n
real(kind=dp) :: dx
real(kind=dp),dimension(1:n) :: f,fx,fxx,ff,s,ss
real(kind=dp),dimension(3,n) :: A,AA,B,BB

 CALL ASSIGN_CCD_TWIN(N , Dx , A , B , AA , BB  )
 CALL TWIN_DEC( A, B, AA, BB )  
 
 ff=f
 !ff(1) = ( 16.0_DP * f(1) - 15.0_DP * f(2)   + 5.0_DP * f(3)   - f(4)   ) / 5.0_DP
 !ff(N) = ( 16.0_DP * f(N) - 15.0_DP * f(N-1) + 5.0_DP * f(N-2) - f(N-3) ) / 5.0_DP
 !FF(1) = ( 8.0_DP * F(1)  - 6.0_DP * F(2)   + F(3)    ) / 3.0_DP
 !FF(N) = ( 8.0_DP * F(N)  - 6.0_DP * F(N-1) + F(N-2)  ) / 3.0_DP
 call ASSIGN_CCD_TWIN_SRC(N , Dx , ff , fx , fxx)
 CALL TWIN_BKS( A, B, AA, BB, fx , fxx )
 
end subroutine

SUBROUTINE ASSIGN_CCD_TWIN(N , Dx , A , B , AA , BB)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Routine to assign CCD coeff.
!  AU , BU , CU are for the upwind coeff.
!  AD , BD , CD are for the downwind coeff.
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE PRECISION
IMPLICIT NONE
INTEGER,INTENT(IN) :: N
REAL(KIND=DP),INTENT(IN ) :: Dx
REAL(KIND=DP),DIMENSION(:,:),INTENT(OUT) :: A , B , AA , BB
REAL(KIND=DP) :: A1 , A3 , B1 , B3
REAL(KIND=DP) :: AA1 , AA3 , BB1 , BB3

INTEGER:: I

A1 =  7.0_DP / 16.0_DP
A3 =  7.0_DP / 16.0_DP
B1 =  1.0_DP / 16.0_DP
B3 = -1.0_DP / 16.0_DP


!For uxx eq.
AA1 = -9.0_DP / 8.0_DP
AA3 =  9.0_DP / 8.0_DP
BB1 = -1.0_DP / 8.0_DP
BB3 = -1.0_DP / 8.0_DP


!For I = 1
!I
 !For U_X
 A(2,1) = 1.0_DP
 B(2,1) = 0.0_DP
 !For U_XX
 AA(2,1) = 0.0_DP
 BB(2,1) = 1.0_DP
!I+1
 !For U_X
 A(3,1) = 2.0_DP
 B(3,1) = -Dx
 !For U_XX
 AA(3,1) = -2.5_DP / Dx
 BB(3,1) =  8.5_DP

!For I = 2 , N-1
DO I = 2 , N-1
!I-1
 !For U_X
  A(1,I) = A1
  B(1,I) = B1 * Dx
 !For U_XX
  AA(1,I) = AA1 / Dx
  BB(1,I) = BB1
!I
  !For U_X
  A(2,I) = 1.0_DP
  B(2,I) = 0.0_DP
  !For U_XX
  AA(2,I) = 0.0_DP
  BB(2,I) = 1.0_DP
!I+1
  !For U_X
  A(3,I) = A3
  B(3,I) = B3 * Dx
  !For U_XX
  AA(3,I) = AA3 / Dx
  BB(3,I) = BB3
END DO

!For I = N
!I-1
 !For U_X
 A(1,N) = 2.0_DP
 B(1,N) = Dx
 !For U_XX
 AA(1,N) = 2.5_DP / Dx
 BB(1,N) = 8.5_DP
!I
 !For U_X
 A(2,N) = 1.0_DP
 B(2,N) = 0.0_DP
 !For U_XX
 AA(2,N) = 0.0_DP
 BB(2,N) = 1.0_DP

 END SUBROUTINE ASSIGN_CCD_TWIN
 
SUBROUTINE ASSIGN_CCD_UD_TWIN(N , Dx , A , B , AA , BB, AD , BD , AAD , BBD)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Routine to assign CCD coeff.
!  AU , BU , CU are for the upwind coeff.
!  AD , BD , CD are for the downwind coeff.
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE PRECISION
IMPLICIT NONE
INTEGER,INTENT(IN) :: N
REAL(KIND=DP)                 ,INTENT(IN ) :: Dx
REAL(KIND=DP),DIMENSION(:,:),INTENT(OUT) :: A , B , AA , BB
REAL(KIND=DP),DIMENSION(:,:),INTENT(OUT) :: AD , BD , AAD , BBD

REAL(KIND=DP) :: A1 , B1 , B2 , B3
REAL(KIND=DP) :: A3D , B1D , B2D , B3D
REAL(KIND=DP) :: AA1 , AA3 , BB1 , BB3

INTEGER:: I


!  !For ux eq., a>0
!  A1 =  0.875_DP
!  B1 =  0.12512823415990895606_DP
!  B2 = -0.24871765840091043936_DP
!  B3 =  0.00012823415990895606360_DP

!  !For ux eq., a<0
!  A3D =  0.875_DP
!  B1D = -0.00012823415990895606360_DP
!  B2D =  0.24871765840091043936_DP
!  B3D = -0.12512823415990895606_DP

!  !For ux eq., a>0
!  A1 =  0.875_DP
!  B1 =  0.12547425010813624084_DP
!  B2 = -0.24525749891863759159_DP
!  B3 =  0.00047425010813624083774_DP
!
!  !For ux eq., a<0
!  A3D =  0.875_DP
!  B1D = -0.00047425010813624083774_DP
!  B2D =  0.24525749891863759159_DP
!  B3D = -0.12547425010813624084_DP
  
!!!!  M - DRPCCD   
  !For ux eq., a>0
  A1 =  0.875_DP
  B1 =  0.125100442885824_DP
  B2 = -0.248995571141759_DP
  B3 =  0.0001004428858241_DP

  !For ux eq., a<0
  A3D =  0.875_DP
  B1D = -0.0001004428858241_DP
  B2D =  0.248995571141759_DP
  B3D = -0.125100442885824_DP


  !For uxx eq.
  AA1 = -9.0_DP / 8.0_DP
  AA3 =  9.0_DP / 8.0_DP
  BB1 = -1.0_DP / 8.0_DP
  BB3 = -1.0_DP / 8.0_DP

!For Upwind
  !For I = 1
  !I
   !For U_X
   A(2,1) = 1.0_DP
   B(2,1) = 0.0_DP
   !For U_XX
   AA(2,1) = 0.0_DP
   BB(2,1) = 1.0_DP
  !I+1
   !For U_X
   A(3,1) = 2.0_DP
   B(3,1) = -Dx
   !For U_XX
   AA(3,1) = -2.5_DP / Dx
   BB(3,1) =  8.5_DP

  !For I = 2 , N-1
  DO I = 2 , N-1
  !I-1
   !For U_X
    A(1,I) = A1
    B(1,I) = B1 * Dx
   !For U_XX
    AA(1,I) = AA1 / Dx
    BB(1,I) = BB1
  !I
    !For U_X
    A(2,I) = 1.0_DP
    B(2,I) = B2 * Dx
    !For U_XX
    AA(2,I) = 0.0_DP
    BB(2,I) = 1.0_DP
  !I+1
    !For U_X
    A(3,I) = 0.0_DP
    B(3,I) = B3 * Dx
    !For U_XX
    AA(3,I) = AA3 / Dx
    BB(3,I) = BB3
  END DO

  !For I = N
  !I-1
   !For U_X
   A(1,N) = 2.0_DP
   B(1,N) = Dx
   !For U_XX
   AA(1,N) = 2.5_DP / Dx
   BB(1,N) = 8.5_DP
  !I
   !For U_X
   A(2,N) = 1.0_DP
   B(2,N) = 0.0_DP
   !For U_XX
   AA(2,N) = 0.0_DP
   BB(2,N) = 1.0_DP


!For Downwind
  !For I = 1
  !I
   !For U_X
   AD(2,1) = 1.0_DP
   BD(2,1) = 0.0_DP
   !For U_XX
   AAD(2,1) = 0.0_DP
   BBD(2,1) = 1.0_DP
  !I+1
   !For U_X
   AD(3,1) = 2.0_DP
   BD(3,1) = -Dx
   !For U_XX
   AAD(3,1) = -2.5_DP / Dx
   BBD(3,1) =  8.5_DP

  !For I = 2 , N-1
  DO I = 2 , N-1
  !I-1
   !For U_X
    AD(1,I) = 0.0_DP
    BD(1,I) = B1D * Dx
   !For U_XX
    AAD(1,I) = AA1 / Dx
    BBD(1,I) = BB1
  !I
    !For U_X
    AD(2,I) = 1.0_DP
    BD(2,I) = B2D * Dx
    !For U_XX
    AAD(2,I) = 0.0_DP
    BBD(2,I) = 1.0_DP
  !I+1
    !For U_X
    AD(3,I) = A3D
    BD(3,I) = B3D * Dx
    !For U_XX
    AAD(3,I) = AA3 / Dx
    BBD(3,I) = BB3
  END DO

  !For I = N
  !I-1
   !For U_X
   AD(1,N) = 2.0_DP
   BD(1,N) = Dx
   !For U_XX
   AAD(1,N) = 2.5_DP / Dx
   BBD(1,N) = 8.5_DP
  !I
   !For U_X
   AD(2,N) = 1.0_DP
   BD(2,N) = 0.0_DP
   !For U_XX
   AAD(2,N) = 0.0_DP
   BBD(2,N) = 1.0_DP

END SUBROUTINE ASSIGN_CCD_UD_TWIN
 
SUBROUTINE TWIN_DEC( A, B, AA, BB )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Routine to solve the CCD matrix by twin-elimination method.
!
!Input is the CCD matrix.
!Output is the twin-forward elimination matrix.
!Note that the coefficients for elimination
! A(1,:) , B(1,:) , AA(1,:) and BB(1,:)
!are stored in the same locations.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE PRECISION
IMPLICIT NONE
 REAL(KIND=DP), INTENT(INOUT), DIMENSION(:,:) :: A,  B
 REAL(KIND=DP), INTENT(INOUT), DIMENSION(:,:) :: AA, BB
!--------------
 INTEGER       ::  i, n
 REAL(KIND=DP) ::  den , SC1 , SC2

 n = SIZE(A,DIM=2)

 A (1,1) = 0.0_DP
 AA(1,1) = 0.0_DP
 A (3,n) = 0.0_DP
 AA(3,n) = 0.0_DP

 B (1,1) = 0.0_DP
 BB(1,1) = 0.0_DP
 B (3,n) = 0.0_DP
 BB(3,n) = 0.0_DP

 DO i = 2, N
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
 
SUBROUTINE ASSIGN_CCD_TWIN_SRC(N , Dx , U , S , SS)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Routine to assign CCD source term.
!  DU are for the upwind coeff.
!  DD are for the downwind coeff.
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE PRECISION
IMPLICIT NONE
INTEGER,INTENT(IN) :: N
REAL(KIND=DP)             ,INTENT(IN ) :: Dx
REAL(KIND=DP),DIMENSION(:),INTENT(IN ) :: U
REAL(KIND=DP),DIMENSION(:),INTENT(OUT) :: S , SS

REAL(KIND=DP) :: C1 , C3
REAL(KIND=DP) :: C1D , C2D , C3D
REAL(KIND=DP) :: CC1 , CC2 , CC3

INTEGER:: I


 !For ux eq., a>0
 C1 = -15.0_DP / 16.0_DP
 C3 =  15.0_DP / 16.0_DP
 
 
 !For uxx eq.
 CC1 =  3.0_DP
 CC2 = -6.0_DP
 CC3 =  3.0_DP


!For I = 1
 !U_X
  S(1)  = (-3.5_DP * U(1) + 4.0_DP * U(2) - 0.5_DP * U(3)) / Dx
 !U_XX
  SS(1) = ( 34.0_DP / 3.0_DP * U(1) - 83.0_DP / 4.0_DP * U(2) &
        &  +10.0_DP * U(3) -7.0_DP/12.0_DP * U(4) ) / (Dx**2)

!For I = 2 , N-1
DO I = 2 , N-1
	!U_X
  S(I)  = (C1*U(I-1) + C3*U(I+1)) / Dx
  !U_XX
  SS(I) = (CC1*U(I-1) + CC2*U(I) + CC3*U(I+1)) / (Dx**2)
END DO

!For I = N
 !U_X
 S(N)  = -(-3.5_DP * U(N) + 4.0_DP * U(N-1) - 0.5_DP * U(N-2)) / Dx
 !U_XX
 SS(N) = ( 34.0_DP / 3.0_DP * U(N) - 83.0_DP / 4.0_DP * U(N-1) &
       &  +10.0_DP * U(N-2) -7.0_DP/12.0_DP * U(N-3) ) / (Dx**2)

END SUBROUTINE ASSIGN_CCD_TWIN_SRC

SUBROUTINE ASSIGN_CCD_UD_TWIN_SRC(N , Dx , U , S , SS , SD , SSD)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Routine to assign CCD source term.
!  DU are for the upwind coeff.
!  DD are for the downwind coeff.
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE PRECISION
IMPLICIT NONE
INTEGER,INTENT(IN) :: N
REAL(KIND=DP)             ,INTENT(IN ) :: Dx
REAL(KIND=DP),DIMENSION(:),INTENT(IN ) :: U
REAL(KIND=DP),DIMENSION(:),INTENT(OUT) :: S , SS
REAL(KIND=DP),DIMENSION(:),INTENT(OUT) :: SD , SSD

REAL(KIND=DP) :: C1 , C2 , C3
REAL(KIND=DP) :: C1D , C2D , C3D
REAL(KIND=DP) :: CC1 , CC2 , CC3

INTEGER:: I

!  !For ux eq., a>0
!  C1 = -1.9359611900810925272_DP
!  C2 =  1.9969223801621850545_DP
!  C3 = -0.060961190081092527237_DP

!  !For ux eq., a<0
!  C1D =  0.060961190081092527237_DP
!  C2D = -1.9969223801621850545_DP
!  C3D =  1.9359611900810925272_DP


 ! !For ux eq., a>0
 ! C1 = -1.9318089987023651093_DP
 ! C2 =  1.9886179974047302197_DP
 ! C3 = -0.056808998702365109925_DP
 !
 ! !For ux eq., a<0
 ! C1D =  0.056808998702365109925_DP
 ! C2D = -1.9886179974047302197_DP
 ! C3D =  1.9318089987023651093_DP
  
   !For ux eq., a>0
  C1 = -1.93629468537011_DP
  C2 =  1.99758937102742_DP
  C3 = -0.061294685370110_DP

  !For ux eq., a<0
  C1D =  0.061294685370110_DP
  C2D = -1.99758937102742_DP
  C3D =  1.93629468537011_DP


  !For uxx eq.
  CC1 =  3.0_DP
  CC2 = -6.0_DP
  CC3 =  3.0_DP

!For upwind
  !For I = 1
   !U_X
    S(1)  = (-3.5_DP * U(1) + 4.0_DP * U(2) - 0.5_DP * U(3)) / Dx
   !U_XX
    SS(1) = ( 34.0_DP / 3.0_DP * U(1) - 83.0_DP / 4.0_DP * U(2) &
          &  +10.0_DP * U(3) -7.0_DP/12.0_DP * U(4) ) / (Dx**2)

  !For I = 2 , N-1
  DO I = 2 , N-1
  	!U_X
    S(I)  = (C1*U(I-1) + C2*U(I) + C3*U(I+1)) / Dx
    !U_XX
    SS(I) = (CC1*U(I-1) + CC2*U(I) + CC3*U(I+1)) / (Dx**2)
  END DO

  !For I = N
   !U_X
   S(N)  = -(-3.5_DP * U(N) + 4.0_DP * U(N-1) - 0.5_DP * U(N-2)) / Dx
   !U_XX
   SS(N) = ( 34.0_DP / 3.0_DP * U(N) - 83.0_DP / 4.0_DP * U(N-1) &
         &  +10.0_DP * U(N-2) -7.0_DP/12.0_DP * U(N-3) ) / (Dx**2)



!For Downwind
  !For I = 1
   !U_X
    SD(1)  = (-3.5_DP * U(1) + 4.0_DP * U(2) - 0.5_DP * U(3)) / Dx
   !U_XX
    SSD(1) = ( 34.0_DP / 3.0_DP * U(1) - 83.0_DP / 4.0_DP * U(2) &
          &  +10.0_DP * U(3) -7.0_DP/12.0_DP * U(4) ) / (Dx**2)

  !For I = 2 , N-1
  DO I = 2 , N-1
  	!U_X
    SD(I)  = (C1D*U(I-1) + C2D*U(I) + C3D*U(I+1)) / Dx
    !U_XX
    SSD(I) = (CC1*U(I-1) + CC2*U(I) + CC3*U(I+1)) / (Dx**2)
  END DO

  !For I = N
   !U_X
   SD(N)  = -(-3.5_DP * U(N) + 4.0_DP * U(N-1) - 0.5_DP * U(N-2)) / Dx
   !U_XX
   SSD(N) = ( 34.0_DP / 3.0_DP * U(N) - 83.0_DP / 4.0_DP * U(N-1) &
         &  +10.0_DP * U(N-2) -7.0_DP/12.0_DP * U(N-3) ) / (Dx**2)

END SUBROUTINE ASSIGN_CCD_UD_TWIN_SRC
  
SUBROUTINE TWIN_BKS( A, B, AA, BB, S , SS )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Routine to solve the CCD matrix by twin-elimination method.
!
!Input is the twin-forward elimination matrix and CCD source terms.
!Output is solution.
!Note that twin-forward elimination matrix is never changed.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
USE PRECISION
 IMPLICIT NONE
 REAL(KIND=DP), INTENT(IN   ), DIMENSION(:,:) :: A,  B
 REAL(KIND=DP), INTENT(IN   ), DIMENSION(:,:) :: AA, BB
 REAL(KIND=DP), INTENT(INOUT), DIMENSION(:) :: S , SS
!--------------
 INTEGER       :: i, n
 REAL(KIND=DP) :: Den, SolS, SolSS

 n = SIZE(A,DIM=2)

 DO i = 2, N
   S(i)  = S(i)  -(  A(1,i)*S(i-1) +  B(1,i)*SS(i-1) )
   SS(i) = SS(i) -( AA(1,i)*S(i-1) + BB(1,i)*SS(i-1) )
 END DO

 Den   = A(2,n)*BB(2,n) - AA(2,n)*B(2,n)
 SolS  = -( B(2,n)*SS(n) - BB(2,n)*S(n) )/Den
 SolSS =  ( A(2,n)*SS(n) - AA(2,n)*S(n) )/Den
 S(n)  = SolS
 SS(n) = SolSS

 DO i = N-1, 1,-1

   S(i)  = S(i)  - (  A(3,i)*SolS +  B(3,i)*SolSS )
   SS(i) = SS(i) - ( AA(3,i)*SolS + BB(3,i)*SolSS )

   Den   = A(2,i)*BB(2,i) - AA(2,i)*B(2,i)
   SolS  = -( B(2,i)*SS(i) - BB(2,i)*S(i))/Den
   SolSS =  ( A(2,i)*SS(i) - AA(2,i)*S(i))/Den
   S(i)  = SolS
   SS(i) = SolSS

 END DO



  END SUBROUTINE TWIN_BKS