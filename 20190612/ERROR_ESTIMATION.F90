SUBROUTINE ERROR_ESTIMATION()
USE PRECISION
USE PROBLEM_DEF
USE LS_DATA
USE VOF_DATA
IMPLICIT NONE
INTEGER :: I,J,K
REAL(DP) :: TMPA, TMPB, TMPC

 goto 100

 VOL_LS = 0.0_DP
 VOL_VOF = 0.0_DP
 
 CALL HEAVY_F(PHI)
 
 !$OMP PARALLEL DO REDUCTION(+:VOL_LS,VOL_VOF)
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
 DO I = 1, NODE_X
   VOL_LS = VOL_LS + HEAVY(I,J,K)
   VOL_VOF = VOL_VOF + VOF(I,J,K)
 END DO
 END DO
 END DO
 !$OMP END PARALLEL DO
 
 EM_LS = ABS(IVOL_LS - VOL_LS)/IVOL_LS
 EM_VOF = ABS(IVOL_VOF - VOL_VOF)/IVOL_vof
 
 EI_LS = 0.0_DP
 EI_VOF = 0.0_DP
 
 !$OMP PARALLEL DO REDUCTION(+:EI_LS,EI_VOF), PRIVATE(TMPA,TMPB,TMPC)
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
 DO I = 1, NODE_X
 
   TMPA = 0.0_DP!MAX(VOF(I,J,K)-0.5_DP,0.0_DP)
   TMPB = 0.0_DP!MAX(VOF_I(I,J,K)-0.5_DP,0.0_DP)
   
   IF(VOF(I,J,K)>=0.5)TMPA=1.0_DP
   IF(VOF_I(I,J,K)>=0.5)TMPB=1.0_DP
   
   EI_VOF = EI_VOF + ABS(TMPA-TMPB)
 
   TMPA = HEAV(PHI(I,J,K),TMPC)    
   TMPB = HEAV(PHI_I(I,J,K),TMPC)  
   
   EI_LS = EI_LS + ABS(TMPA-TMPB)
   
 END DO
 END DO
 END DO
 !$OMP END PARALLEL DO
 
 EI_VOF = EI_VOF / REAL(NODE_X*NODE_Y*NODE_Z,KIND=DP)
 EI_LS = EI_LS / REAL(NODE_X*NODE_Y*NODE_Z,KIND=DP)
 
100 IF( GRID_CNT .EQ. 1 )THEN
   WRITE(64,*)"============================================="
   WRITE(64,*)"-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*"
   IF(METHOD_CNT.EQ.2)THEN
   	WRITE(64,*)"Classical LS"
   ELSE
   	WRITE(64,*)"Mass-preserving LS"
   ENDIF
   WRITE(64,*)"-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*"
 ENDIF
  
  WRITE(64,'(A25,I5,A2,I5,A2,I5)')"Grid number:",NODE_X,"x",NODE_Y,"x",NODE_Z
  WRITE(64,*)""
 
 IF( INTERFACE_METHOD.EQ.0  )THEN
   !WRITE(64,'(A25,2ES15.4)')"Volume loss:",em_ls
   WRITE(64,'(A25,2ES15.4)')"Averaged loss:",em_ls_a
   !write(64,'(A25,2ES15.4)')"Interface error:",ei_ls
 ELSE 
   !WRITE(64,'(A25,2ES15.4)')"Volume loss:",EM_VOF
   WRITE(64,'(A25,2ES15.4)')"Averaged loss:",EM_VOF_A
   !write(64,'(A25,2ES15.4)')"Interface error:",EI_VOF
 END IF
   WRITE(64,*)""
   WRITE(64,*)"CPU TIME:",CPU_COST
   WRITE(64,*)""
   
END SUBROUTINE