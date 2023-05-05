SUBROUTINE SOLVE_NS()
USE PRECISION
USE FLUID_PROPERTIES
USE LS_DATA
IMPLICIT NONE
INTEGER :: INITER,I,J,K

  CALL AMURHO()
  CALL FIND_CURVATURE()

  INITER = 0

  DO
  
    INITER = INITER + 1
  
    !$OMP PARALLEL DO
    DO K = -2, NODE_Z+3
    DO J = -2, NODE_Y+3
    DO I = -2, NODE_X+3
      U_TMP(I,J,K) = U(I,J,K)
      V_TMP(I,J,K) = V(I,J,K)
      W_TMP(I,J,K) = W(I,J,K)
    END DO
    END DO
    END DO
    !$OMP END PARALLEL DO

    CALL SECOND_ORDER_UPWIND
    ! CALL QUICK
    
    IF( REC_MASS==0 )RETURN
    CALL Adams_Bashforth()

    CALL SOLVE_PRESSURE_POISSON()
    CALL SOLVE_MOMENTUM()
	
    CALL CHECK_CONVERGENCE(INITER)
    
    CALL VELOCITY_BC()
	
    IF( INITER>3 )EXIT
	
  END DO


END SUBROUTINE

SUBROUTINE CHECK_CONVERGENCE(INITER)
USE PRECISION
USE PROBLEM_DEF
USE FLUID_PROPERTIES
IMPLICIT NONE
INTEGER :: INITER,I,J,K
REAL(DP) :: RES

  CALL FIND_ERROR(L2_U,U,U_TMP,2)
  CALL FIND_ERROR(L2_V,V,V_TMP,2)
  CALL FIND_ERROR(L2_W,W,W_TMP,2)
 
  CALL FIND_ERROR(LINF_U,U,U_TMP,3)
  CALL FIND_ERROR(LINF_V,V,V_TMP,3)
  CALL FIND_ERROR(LINF_W,W,W_TMP,3)
  
  IF( L2_U+L2_V+L2_W < TOL_M )INITER = INITER + 5

 DIV = 0.0_DP
 
 !$omp parallel do REDUCTION(MAX:DIV),PRIVATE(RES)
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
 DO I = 1, NODE_X
 	
    RES=(U(I,J,K)-U(I-1,J,K))/DX &
       +(V(I,J,K)-V(I,J-1,K))/DY &
       +(W(I,J,K)-W(I,J,K-1))/DZ
       
    DIV = MAX(DIV,ABS(RES))
       
  ENDDO
  ENDDO
  ENDDO
  !$omp end parallel do    
  
  
END SUBROUTINE

SUBROUTINE VELOCITY_BC()
USE PRECISION
USE PROBLEM_DEF
USE FLUID_PROPERTIES
USE IBM_DATA
implicit none
integer :: i,j,k

  CALL BCU(U)
  CALL BCV(V)
  CALL BCW(W)

END SUBROUTINE

SUBROUTINE SOLVE_MOMENTUM()
USE PRECISION
USE PROBLEM_DEF
USE FLUID_PROPERTIES
IMPLICIT NONE
INTEGER :: I,J,K

!$omp parallel do
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
 DO I = 1, NODE_X-1
   U1(I,J,K)= -U1(I,J,K)-2.0*(P(I+1,J,K)-P(I,J,K))/(DX*(RHO(I,J,K)+RHO(I+1,J,K)))
   U1(I,J,K)=  U_OLD(I,J,K)+U1(I,J,K)*DT
   U(i,j,k) =  U1(i,j,k)
 END DO
 END DO
 END DO
!$omp end parallel do  

!$omp parallel do
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y-1 
 DO I = 1, NODE_X	  
   V1(I,J,K)= -V1(I,J,K)-2.0*(P(I,J+1,K)-P(I,J,K))/(DY*(RHO(I,J,K)+RHO(I,J+1,K)))
   V1(I,J,K)=  V_OLD(I,J,K)+V1(I,J,K)*DT
   V(i,j,k) =  V1(i,j,k)
 END DO
 END DO
 END DO
!$omp end parallel do  

!$omp parallel do
 DO K = 1, NODE_Z-1
 DO J = 1, NODE_Y
 DO I = 1, NODE_X 
   W1(I,J,K)= -W1(I,J,K)-2.0*(P(I,J,K+1)-P(I,J,K))/(DZ*(RHO(I,J,K)+RHO(I,J,K+1)))
   W1(I,J,K)=  W_OLD(I,J,K)+W1(I,J,K)*DT
   W(i,j,k) =  W1(i,j,k)
 END DO
 END DO
 END DO
 !$omp end parallel do  

END SUBROUTINE

SUBROUTINE Adams_Bashforth()
USE PRECISION
USE PROBLEM_DEF
USE FLUID_PROPERTIES
IMPLICIT NONE
INTEGER :: I,J,K

 !$OMP PARALLEL DO
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
 DO I = 1, NODE_X
  U1(I,J,K) = 1.5_DP*US(I,J,K) - 0.5_DP*US_OLD(I,J,K)
  V1(I,J,K) = 1.5_DP*VS(I,J,K) - 0.5_DP*VS_OLD(I,J,K)
  W1(I,J,K) = 1.5_DP*WS(I,J,K) - 0.5_DP*WS_OLD(I,J,K)
 END DO
 END DO
 END DO
 !$OMP END PARALLEL DO

END SUBROUTINE

 
