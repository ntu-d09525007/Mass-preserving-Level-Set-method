SUBROUTINE READ_DATA
USE PRECISION 
USE PROBLEM_DEF
USE FLUID_PROPERTIES
USE LS_DATA
USE VOF_DATA
USE IBM_DATA
IMPLICIT NONE
INTEGER :: I,J,K,rd,err
CHARACTER(6) :: NAME
REAL(DP) :: TMP
 ITER=0
 READ_OPER = 0
 RETURN
 WRITE(*,*)'Iter->'
 read(*,*)RD
 ITER = RD
 if( rd==0 )then
 	 RETURN 
 else if ( rd>0 )then
 	 READ_OPER = 1
   WRITE(NAME,'(I6.6)')RD
   OPEN(UNIT=99,FILE='Nesq_3D_'//NAME//'_.plt',STATUS="OLD",IOSTAT=ERR)
   IF(ERR/=0)THEN
    WRITE(*,*)'Error occurs when opening file'
    STOP
   END IF
 	 DO I = 1,3
 	 READ(99,*)
 	 END DO
   SELECT CASE(IBM_SOLVER)
     CASE(0) 
       DO K = 0, NODE_Z+1
       DO J = 0, NODE_Y+1
       DO I = 0, NODE_X+1
         READ(99,*)TMP,TMP,TMP,U(I,J,K),V(I,J,K),W(I,J,K),PHI(I,J,K),VOF(I,J,K)
       END DO
       END DO
   		 END DO
     CASE(1)
       DO K = 0, NODE_Z+1
       DO J = 0, NODE_Y+1
       DO I = 0, NODE_X+1
         READ(99,*)TMP,TMP,TMP,U(I,J,K),V(I,J,K),W(I,J,K),PHI(I,J,K),VOF(I,J,K),SOLID_VOF(I,J,K)
       END DO
       END DO
       END DO
    END SELECT
    
 	 CLOSE(99)
 	 
 	 CALL VELOCITY_BC()
   CALL BC3D(PHI)
   CALL BC3D(VOF)
   CALL BC3D(SOLID_VOF)
 else
 	 WRITE(*,*)'Error input'
 	 STOP 
 end if
 	

END SUBROUTINE