SUBROUTINE FIND_NORMALS(FI,VF)
USE PRECISION
USE PROBLEM_DEF
USE LS_DATA
USE VOF_DATA
IMPLICIT NONE
INTEGER :: I,J,K,P,Q,R
REAL(DP) :: A
REAL(DP),DIMENSION(-2:NODE_X+3,-2:NODE_Y+3,-2:NODE_Z+3) :: FI,VF

 IF( VOF_SOLVER .EQ. 1)THEN
 	
 	 IF( INTERFACE_METHOD .EQ. 3 )THEN
 	 	  !$OMP PARALLEL DO
 	 	  DO K = -2, NODE_Z+3
 	 	  DO J = -2, NODE_Y+3
 	 	  DO I = -2, NODE_X+3
 	 	  	FI(I,J,K) = 2.0*VF(I,J,K)-1.0
 	 	  ENDDO
 	 	  ENDDO
 	 	  ENDDO
 	 	  !$OMP END PARALLEL DO
 	 	  
 	 	  CALL LS_REDISTANCE(FI,2)
 	 ENDIF
 	
   !$OMP PARALLEL DO
   DO K = 0, NODE_Z
   DO J = 0, NODE_Y
   DO I = 0, NODE_X
     NORMAL_X(I,J,K)=0.5*(FI(I+1,J,K)-FI(I-1,J,K))/DX
     NORMAL_Y(I,J,K)=0.5*(FI(I,J+1,K)-FI(I,J-1,K))/DY
     NORMAL_Z(I,J,K)=0.5*(FI(I,J,K+1)-FI(I,J,K-1))/DZ   	
   ENDDO
   ENDDO
   ENDDO 	
   !$OMP END PARALLEL DO

 ELSE

   !$OMP PARALLEL DO PRIVATE(P,Q,R)
   DO K = 0, NODE_Z
   DO J = 0, NODE_Y
   DO I = 0, NODE_X
   	
   	 VOF_X(I,J,K)=0.0
   	 VOF_Y(I,J,K)=0.0
   	 VOF_Z(I,J,K)=0.0
   	
     DO P = 0, 1
     DO Q = 0, 1
     	 VOF_X(I,J,K)=VOF_X(I,J,K)+0.25*(VF(I+1,J+P,K+Q)-VF(I,J+P,K+Q))/DX
     	 VOF_Y(I,J,K)=VOF_Y(I,J,K)+0.25*(VF(I+P,J+1,K+Q)-VF(I+P,J,K+Q))/DY
     	 VOF_Z(I,J,K)=VOF_Z(I,J,K)+0.25*(VF(I+P,J+Q,K+1)-VF(I+P,J+Q,K))/DZ
     ENDDO
     ENDDO

   ENDDO
   ENDDO
   ENDDO 	
   !$OMP END PARALLEL DO

   !$OMP PARALLEL DO PRIVATE(P,Q,R)
   DO K = 0, NODE_Z
   DO J = 0, NODE_Y
   DO I = 0, NODE_X
   
     NORMAL_X(I,J,K)=0.0
     NORMAL_Y(I,J,K)=0.0
     NORMAL_Z(I,J,K)=0.0
     
     DO P = -1, 0
     DO Q = -1, 0
     DO R = -1, 0
     	 NORMAL_X(I,J,K)=NORMAL_X(I,J,K)+0.125_DP*VOF_X(I+P,J+Q,K+R)
     	 NORMAL_Y(I,J,K)=NORMAL_Y(I,J,K)+0.125_DP*VOF_Y(I+P,J+Q,K+R)
     	 NORMAL_Z(I,J,K)=NORMAL_Z(I,J,K)+0.125_DP*VOF_Z(I+P,J+Q,K+R)
     ENDDO
     ENDDO
     ENDDO
     
   ENDDO
   ENDDO
   ENDDO 	
   !$OMP END PARALLEL DO
 	  	
 ENDIF
 
END SUBROUTINE

SUBROUTINE NORMALS_NORMALIZE()
USE PRECISION
USE LS_DATA
USE PROBLEM_DEF
IMPLICIT NONE
INTEGER :: I,J,K
REAL(DP) :: A

  !$OMP PARALLEL DO PRIVATE(A)
  DO K = 0, NODE_Z
  DO J = 0, NODE_Y
  DO I = 0, NODE_X
     A = DSQRT( NORMAL_X(I,J,K)**2+NORMAL_Y(I,J,K)**2+NORMAL_Z(I,J,K)**2 )
     NORMAL_X(I,J,K) = NORMAL_X(I,J,K) / A
     NORMAL_Y(I,J,K) = NORMAL_Y(I,J,K) / A
     NORMAL_Z(I,J,K) = NORMAL_Z(I,J,K) / A
  END DO
  END DO
  END DO
  !$OMP END PARALLEL DO

 
   CALL BC3D(NORMAL_X)
   CALL BC3D(NORMAL_Y)
   CALL BC3D(NORMAL_Z)
 	 
END SUBROUTINE