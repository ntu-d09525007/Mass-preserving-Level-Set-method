SUBROUTINE AMURHO()
USE PRECISION
USE FLUID_PROPERTIES
USE PROBLEM_DEF
USE LS_DATA
USE VOF_DATA
IMPLICIT NONE
INTEGER :: I,J,K

 IF( INTERFACE_METHOD == 0 )THEN
    
  CALL HEAVY_F(PHI)
    
    !$OMP PARALLEL DO
  DO K = 1, NODE_Z
  DO J = 1, NODE_Y
  DO I = 1, NODE_X
    RHO(I,J,K) = HEAVY(I,J,K) + RATIO_RHO*(1.0-HEAVY(I,J,K))
    AMU(I,J,K) = HEAVY(I,J,K) + RATIO_AMU*(1.0-HEAVY(I,J,K))

    rho(i,j,k) = ratio_rho * heavy(i,j,k) + (1.0-heavy(i,j,k))
    amu(i,j,k) = ratio_amu * heavy(i,j,k) + (1.0-heavy(i,j,k))
  END DO
  END DO
  END DO
    !$OMP END PARALLEL DO
    
 ELSE
    
    !$OMP PARALLEL DO
  DO K = 1, NODE_Z
  DO J = 1, NODE_Y
  DO I = 1, NODE_X
    RHO(I,J,K) = VOF(I,J,K) + RATIO_RHO*(1.0-VOF(I,J,K))
    AMU(I,J,K) = VOF(I,J,K) + RATIO_AMU*(1.0-VOF(I,J,K))
  END DO
  END DO
  END DO
    !$OMP END PARALLEL DO
    
 END IF
 
  CALL BC3D(RHO)
  CALL BC3D(AMU)

END SUBROUTINE
