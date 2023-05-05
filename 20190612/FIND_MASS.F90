SUBROUTINE FIND_MASS()
USE PRECISION
USE PROBLEM_DEF
USE FLUID_PROPERTIES
USE LS_DATA
USE VOF_DATA
IMPLICIT NONE
INTEGER :: I,J,K,F(1:NODE_Z),CNT,NUM,KK,ks,ke
real(8) :: dv
CHARACTER(3) :: MET,GRI
real(8),dimension(:),allocatable :: mass
logical :: switch, finish
real(8) :: px, py, pz


 MASS_LS = 0.0_DP
 MASS_VOF = 0.0_DP

 VOL_LS = 0.0_DP
 VOL_VOF = 0.0_DP
 
 CALL AMURHO()
 
 !$OMP PARALLEL DO REDUCTION(+:MASS_LS,MASS_VOF,VOL_LS,VOL_VOF, px, py, pz)
 DO K = 1, NODE_Z
    F(K) = -1
 DO J = 1, NODE_Y
 DO I = 1, NODE_X
 
     VOL_LS = VOL_LS + HEAVY(I,J,K)
     VOL_VOF = VOL_VOF + VOF(I,J,K)
     
     MASS_LS = MASS_LS + HEAVY(I,J,K)*RHO(I,J,K)
     MASS_VOF = MASS_VOF + VOF(I,J,K)*RHO(I,J,K)

     px = px + HEAVY(I,J,K) * UH(i,j,k)
     py = py + HEAVY(I,J,K) * VH(i,j,k)
     pz = pz + HEAVY(I,J,K) * WH(i,j,k)

     IF( Delta(I,J,K) > EPS ) F(K) = 1
     
 END DO
 END DO
 END DO
 !$OMP END PARALLEL DO

 CNT=0
 DO K = 1, NODE_Z-1
    IF( F(K)*F(K+1) < 0 )CNT=CNT+1
 ENDDO
 CNT=CNT/2

 ALLOCATE(MASS(CNT))

dv = DX*DY*DZ
kk=1
do num = 1, cnt

    switch=.false.
    finish = .false.

    do k = kk, NODE_Z-1
        if(finish)exit
        if( f(k)*f(k+1) < 0 )then
            if(.not.switch)then
                ks=k
                switch=.true.
            else
                ke=k+1
                finish=.true.
                kk=k+1
            endif
        endif
    enddo

    mass(num)=0.0d0
    do k = ks, ke
    do j = 1, NODE_Y
    do i = 1, NODE_X
        mass(num)=mass(num)+rho(I,J,K)*heavy(I,J,K)*dv
    enddo
    enddo
    enddo

enddo
 
 EM_LS_A = EM_LS_A + ABS(1.0-VOL_LS/IVOL_LS)*DT/TIME_TO_STOP
 EM_VOF_A = EM_VOF_A + ABS(1.0-VOL_VOF/IVOL_vof)*DT/TIME_TO_STOP

 
 IF( REC_MASS==0 )THEN

! #############################################################

 PHI_TMP = PHI

 PHI = PHI1
 CALL AMURHO()
 B1_iM = 0.0
 !$OMP PARALLEL DO REDUCTION(+:B1_iM)
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
 DO I = 1, NODE_X
     B1_iM = B1_iM + HEAVY(I,J,K)*RHO(I,J,K)
 END DO
 END DO
 END DO
 !$OMP END PARALLEL DO

 PHI = PHI2
 CALL AMURHO()
 B2_iM = 0.0
 !$OMP PARALLEL DO REDUCTION(+:B2_iM)
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
 DO I = 1, NODE_X
     B2_iM = B2_iM + HEAVY(I,J,K)*RHO(I,J,K)
 END DO
 END DO
 END DO
 !$OMP END PARALLEL DO

 PHI = PHI_TMP
 ! #############################################################
    
    WRITE(MET,'(I3.3)')METHOD_CNT
    WRITE(GRI,'(I3.3)')GRID_CNT
 
    IMASS_LS = MASS_LS
    IMASS_VOF = MASS_VOF
    
    IVOL_LS = VOL_LS
    IVOL_VOF = VOL_VOF

    B1_M = MASS(1)
    B2_M = MASS(2)
    
    CLOSE(11)
    CLOSE(12)
    
    !OPEN(UNIT=11,FILE='MASS LOSS_'//Met//'_'//GRI//'_.PLT')
    !OPEN(UNIT=11,FILE='MASS LOSS_'//MET//'.PLT')
    OPEN(UNIT=11,FILE='MASS LOSS.PLT')
    WRITE(11,*)'VARIABLES = "T" "LOSS OF LS(%)" "LOSS OF VOF(%)" '
    
    !OPEN(UNIT=12,FILE='VOL LOSS_'//Met//'_'//GRI//'_.PLT')
    !OPEN(UNIT=12,FILE='VOL LOSS_'//MET//'.PLT')
    OPEN(UNIT=12,FILE='VOL LOSS.PLT')
    ! WRITE(12,*)'VARIABLES = "T" "LOSS OF LS(%)" "LOSS OF VOF(%)" '

    OPEN(UNIT=13,FILE='BUBBLE MASS.PLT')
    WRITE(13,*)'VARIABLES ="T" "B1" "B2" '
    
    REC_MASS = 1

    EM_LS_A  = 0.0_DP
    EM_VOF_A = 0.0_DP
    
 END IF
 
  WRITE(11,*)TIME,(1.0-MASS_LS/IMASS_LS)*100!,(1.0-MASS_VOF/IMASS_VOF)*100
  WRITE(12,*)TIME,(1.0-VOL_LS/IVOL_LS)*100!,(1.0-VOL_VOF/IVOL_VOF)*100 
  WRITE(13,*)TIME,(MASS(1)-B1_M)/B1_M*100,(MASS(2)-B2_M)/B2_M*100

  WRITE(*,*)CNT,(MASS(1)-B1_M)/B1_M*100,(MASS(2)-B2_M)/B2_M*100
  write(*,'(3ES15.4)')px, py, pz
  
END SUBROUTINE
