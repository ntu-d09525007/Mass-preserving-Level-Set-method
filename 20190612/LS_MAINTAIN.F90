SUBROUTINE LS_MAINTAIN()
USE PRECISION
USE LS_DATA
USE VOF_DATA
USE DUMMY_DATA
USE PROBLEM_DEF
IMPLICIT NONE
INTEGER :: I,J,K


  IF(INTERFACE_METHOD==0)THEN
  	
    IF(MOD(ITER,5)==0)THEN
	    CALL LS_REDISTANCE(PHI,1) 
    ENDIF
  	
  ELSE IF( INTERFACE_METHOD==2 )THEN
  	
    IF( MOD(ITER,10)==0 )THEN
    	
  	    !$OMP PARALLEL DO
  	    DO K = -2, NODE_Z+3
  	    DO J = -2, NODE_Y+3
  	    DO I = -2, NODE_X+3
  	    	PHI_V(I,J,K) = 2.0_DP*VOF(I,J,K)-1.0_DP
  	    END DO
  	    END DO
  	    END DO
  	    !$OMP END PARALLEL DO
  	    
  	    CALL LS_REDISTANCE(PHI_V,2)

  	    !$OMP PARALLEL DO
  	    DO K = -2, NODE_Z+3
  	    DO J = -2, NODE_Y+3
  	    DO I = -2, NODE_X+3
  	    	IF( ABS(PHI(I,J,K))<INTERFACE_WIDTH )PHI(I,J,K)=PHI_V(I,J,K)
  	    END DO
  	    END DO
  	    END DO
  	    !$OMP END PARALLEL DO

  	    CALL LS_REDISTANCE(PHI,1)
  	    
    ELSE IF( MOD(ITER,5)==0 )THEN
  	
        CALL LS_REDISTANCE(PHI,1)
        
    END IF
  	      	
  	
  END IF
  
END SUBROUTINE
