SUBROUTINE LS_REDISTANCE(TARGET,BTN)
USE PRECISION
USE PROBLEM_DEF
USE LS_DATA
USE DUMMY_DATA
IMPLICIT NONE
INTEGER :: BTN,I,J,K
REAL(DP),DIMENSION(-2:NODE_X+3,-2:NODE_Y+3,-2:NODE_Z+3) :: TARGET
REAL(DP) :: AMX, RED_TIME, ERR

IF( BTN/=1 .and. btn/=4 )THEN
    
  !$OMP PARALLEL DO
  DO K = 1, NODE_Z
  DO J = 1, NODE_Y
  DO I = 1, NODE_X
      SGN(I,J,K) = TARGET(I,J,K)
  END DO
  END DO
  END DO
  !$OMP END PARALLEL DO
    
  CALL GRADFI(TARGET)
  
  AMX = MAXVAL(GRAD(1:NODE_X,1:NODE_Y,1:NODE_Z))
  
  !$OMP PARALLEL DO
  DO K = 1, NODE_Z
  DO J = 1, NODE_Y
  DO I = 1, NODE_X
      TARGET(I,J,K) = TARGET(I,J,K) / AMX
  END DO
  END DO
  END DO
  !$OMP END PARALLEL DO
  
END IF

  CALL SGNF(TARGET) 
  
  RED_TIME = 0.0_DP
  
  DO
    
    RED_TIME = RED_TIME + RDT
    
    !$OMP PARALLEL DO
    DO K = 1, NODE_Z
    DO J = 1, NODE_Y
    DO I = 1, NODE_X
        PHI_TMP(I,J,K) = TARGET(I,J,K)
    END DO
    END DO
    END DO
    !$OMP END PARALLEL DO
    
    CALL LS_REDIS(TARGET,BTN)
    
    CALL FIND_ERROR(ERR,PHI_TMP,TARGET,2)
    
    IF( BTN==0 .AND. (RED_TIME > 1.5*MAX(XL,YL,ZL)) )EXIT
    IF( BTN/=0 .and. BTN/=5 .AND. (RED_TIME > 2.0*INTERFACE_WIDTH) )EXIT
    
  END DO

END SUBROUTINE

SUBROUTINE LS_REDIS(TARGET,BTN)
USE PRECISION
USE PROBLEM_DEF
USE LS_DATA
USE DUMMY_DATA
IMPLICIT NONE
INTEGER :: BTN,I,J,K
REAL(DP),DIMENSION(-2:NODE_X+3,-2:NODE_Y+3,-2:NODE_Z+3) :: TARGET

 CALL GRADFI(TARGET)
 CALL RIS_MASS(TARGET,BTN)
 CALL LS_REDIS_SORCE(S0)
 
 !$omp parallel do
 do k=1,NODE_Z
 do j=1,NODE_Y
 do i=1,NODE_X
   TARGET(i,j,k)=TARGET(i,j,k)-RDT*S0(i,j,k)
 enddo
 enddo
 enddo
 !$omp end parallel do
 
 call bc3d(TARGET)

 CALL GRADFI(TARGET)
 CALL RIS_MASS(TARGET,BTN)
 CALL LS_REDIS_SORCE(S1)
 
 !$omp parallel do
 do k=1,NODE_Z
 do j=1,NODE_Y
 do i=1,NODE_X
   TARGET(i,j,k)=TARGET(i,j,k)-RDT/4.0_DP*(-3.0_DP*S0(i,j,k)+S1(i,j,k))
 enddo
 enddo
 enddo
 !$omp end parallel do
 
 call bc3d(TARGET)

 CALL GRADFI(TARGET)
 CALL RIS_MASS(TARGET,BTN)
 CALL LS_REDIS_SORCE(S2)
 
 !$omp parallel do
 do k=1,NODE_Z
 do j=1,NODE_Y
 do i=1,NODE_X
   TARGET(i,j,k)=TARGET(i,j,k)-RDT/12.0_DP*(-S0(i,j,k)-S1(i,j,k)+8.0_DP*S2(i,j,k))
 enddo
 enddo
 enddo
 !$omp end parallel do
 
 call bc3d(TARGET)
   
END SUBROUTINE

SUBROUTINE LS_REDIS_SORCE(S)
USE PRECISION
USE PROBLEM_DEF
USE DUMMY_DATA
USE LS_DATA
IMPLICIT NONE
INTEGER :: I,J,K
REAL(DP),DIMENSION(-2:NODE_X+3,-2:NODE_Y+3,-2:NODE_Z+3) :: S

 !$omp parallel do
 do k=1,NODE_Z
 do j=1,NODE_Y
 do i=1,NODE_X
     S(i,j,k)=(SGN(i,j,k)-C(i,j,k))*GRAD(i,j,k)-SGN(i,j,k)
 enddo
 enddo
 enddo
 !$omp end parallel do

END SUBROUTINE

SUBROUTINE RIS_MASS(TARGET,BTN)
USE PRECISION
USE PROBLEM_DEF
USE DUMMY_DATA
USE LS_DATA
IMPLICIT NONE
INTEGER :: I,J,K,BTN
REAL(DP),DIMENSION(-2:NODE_X+3,-2:NODE_Y+3,-2:NODE_Z+3) :: TARGET
REAL(DP) :: SUMA, SUMB

 IF( BTN/=1 .AND. BTN/=4 )THEN
    
    !$omp parallel do
      do k=1,NODE_Z
      do j=1,NODE_Y
      do i=1,NODE_X
       C(I,J,K) = 0.0_DP
    enddo
    enddo
    enddo
    !$omp end parallel do
    
 ELSE
    
    CALL HEAVY_F(TARGET)
    
    !$omp parallel do
      do k=1,NODE_Z
      do j=1,NODE_Y
      do i=1,NODE_X
       A(i,j,k)= (SGN(i,j,k)*DELTA(i,j,k))*(GRAD(i,j,k)-1.0_dp)
         B(i,j,k)= GRAD(i,j,k)*DELTA(i,j,k)**2
    enddo
    enddo
    enddo
    !$omp end parallel do
      
    call bc3d(a)
    call bc3d(b)    
    
      !$omp parallel do private(suma,sumb)
     do k=1,NODE_Z
     do j=1,NODE_Y
       do i=1,NODE_X

        suma=51.0_dp*a(i,j,k)+sum(a(i-1:i+1,j-1:j+1,k-1:k+1))
        sumb=51.0_dp*b(i,j,k)+sum(b(i-1:i+1,j-1:j+1,k-1:k+1))

      if(sumb /=0.0_dp ) then
            c(i,j,k)=suma/sumb
      else
            c(i,j,k)=0.0_dp
          end if
      
      c(i,j,k) = c(i,j,k)*DELTA(i,j,k)
      
    enddo
      enddo
      enddo
      !$omp end parallel do  
  
 END IF

END SUBROUTINE

SUBROUTINE GRADFI(TARGET)
USE PRECISION
USE PROBLEM_DEF
USE LS_DATA
USE DUMMY_DATA
IMPLICIT NONE
INTEGER :: I,J,K
REAL(DP),DIMENSION(-2:NODE_X+3,-2:NODE_Y+3,-2:NODE_Z+3) :: TARGET
     
  CALL FV_DERI(TARGET)
  
  !$OMP PARALLEL DO
  DO K = 1, NODE_Z
  DO J = 1, NODE_Y
  DO I = 1, NODE_X
    GRAD(I,J,K) = gradf(up(I,J,K),um(I,J,K),vp(I,J,K),vm(I,J,K),wp(I,J,K),wm(I,J,K),SGN(I,J,K))     
  END DO  
  END DO
  END DO
  !$OMP END PARALLEL DO

END SUBROUTINE

subroutine FV_DERI(TARGET)
USE PRECISION
USE PROBLEM_DEF
USE DUMMY_DATA
IMPLICIT NONE
INTEGER :: I,J,K
REAL(DP),DIMENSION(-2:NODE_X+3,-2:NODE_Y+3,-2:NODE_Z+3) :: TARGET

!$OMP PARALLEL DO
DO K = 1, NODE_Z
DO J = 1, NODE_Y
DO I = 1, NODE_X
  F(I,J,K) = (TARGET(I,J,K)-TARGET(I-1,J,K))/DX
  G(I,J,K) = (TARGET(I,J,K)-TARGET(I,J-1,K))/DY
  H(I,J,K) = (TARGET(I,J,K)-TARGET(I,J,K-1))/DZ
 END DO
 END DO
 END DO
!$OMP END PARALLEL DO

 CALL BC3D(F)
 CALL BC3D(G)
 CALL BC3D(H)

!$OMP PARALLEL DO
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
  CALL weno_js(F(-2:NODE_X+3,j,k),UP(-2:NODE_X+3,j,k),UM(-2:NODE_X+3,j,k),NODE_X)
 END DO
 END DO
 !$OMP END PARALLEL DO
 
 !$OMP PARALLEL DO
 DO K = 1, NODE_Z
 DO I = 1, NODE_X
  CALL weno_js(G(i,-2:NODE_Y+3,k),VP(i,-2:NODE_Y+3,k),VM(i,-2:NODE_Y+3,k),NODE_Y)
 END DO
 END DO
!$OMP END PARALLEL DO

 !$OMP PARALLEL DO
 DO J = 1, NODE_Y
 DO I = 1, NODE_X
  CALL weno_js(H(i,j,-2:NODE_Z+3),WP(i,j,-2:NODE_Z+3),WM(i,j,-2:NODE_Z+3),NODE_Z)
 END DO
 END DO
 !$OMP END PARALLEL DO

end subroutine

function gradf(up,um,vp,vm,wp,wm,fi0)
USE PRECISION
IMPLICIT NONE
REAL(KIND=DP) :: GRADF    
real(KIND=DP)  :: up,um,vp,vm,wp,wm,upm,upp,umm,ump,vpm,vpp,vmm,vmp
real(KIND=DP)  :: wpm,wpp,wmm,wmp,fi0

  upm=-MIN(up,0.0_DP)
  upp= MAX(up,0.0_DP)
  umm=-MIN(um,0.0_DP)
  ump= MAX(um,0.0_DP)
  vpm=-MIN(vp,0.0_DP)
  vpp= MAX(vp,0.0_DP)
  vmm=-MIN(vm,0.0_DP)
  vmp= MAX(vm,0.0_DP)
  wpm=-MIN(wp,0.0_DP)
  wpp= MAX(wp,0.0_DP)
  wmm=-MIN(wm,0.0_DP)
  wmp= MAX(wm,0.0_DP)
  
if (fi0 >=0.0_DP ) then
  gradf=sqrt(MAX(upm,ump)**2+MAX(vpm,vmp)**2+MAX(wpm,wmp)**2)
else
  gradf=sqrt(MAX(upp,umm)**2+MAX(vpp,vmm)**2+MAX(wpp,wmm)**2)
endif

end FUNCTION 

SUBROUTINE HEAVY_F(TARGET)
USE PRECISION
USE PROBLEM_DEF
USE LS_DATA
IMPLICIT NONE
INTEGER :: I,J,K
REAL(DP),DIMENSION(-2:NODE_X+3,-2:NODE_Y+3,-2:NODE_Z+3) :: TARGET

 !$OMP PARALLEL DO
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
 DO I = 1, NODE_X
   HEAVY(I,J,K) = HEAV(TARGET(I,J,K),DELTA(I,J,K))   
   IF( HEAVY(I,J,K)<EPS )HEAVY(I,J,K)=0.0 
 END DO
 END DO
 END DO
 !$OMP END PARALLEL DO
 
 CALL BC3D(HEAVY)

END SUBROUTINE

FUNCTION HEAV(X,HP)  
USE PRECISION
USE LS_DATA
IMPLICIT NONE     
REAL(KIND=DP) :: HEAV, X, HP

IF(X > INTERFACE_WIDTH) THEN
  HEAV=1.0_DP
  HP=0.0_DP
ELSE IF(X < -INTERFACE_WIDTH) THEN
  HEAV=0.0_DP
  HP=0.0_DP
ELSE
  HEAV=0.5_DP*(1.0_DP+X/INTERFACE_WIDTH+1.0_DP/PI*SIN(PI*X/INTERFACE_WIDTH))
  HP=0.5_DP*(1.0_DP/INTERFACE_WIDTH+1.0_DP/INTERFACE_WIDTH*COS(PI*X/INTERFACE_WIDTH))
ENDIF

END FUNCTION HEAV 

FUNCTION SGNN(X)  
USE PRECISION  
IMPLICIT NONE     
REAL(KIND=DP) :: SGNN,X,HP,H

H=HEAV(X,HP)
SGNN=2.0_DP*(H-0.5_DP)

END FUNCTION SGNN

subroutine sgnf(TARGET) 
USE PRECISION
USE PROBLEM_DEF
USE LS_DATA
IMPLICIT NONE
INTEGER :: I,J,K
REAL(DP),DIMENSION(-2:NODE_X+3,-2:NODE_Y+3,-2:NODE_Z+3) :: TARGET

 !$OMP PARALLEL DO
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
 DO I = 1, NODE_X
     SGN(i,j,K)=SGNN(TARGET(I,J,K))
 Enddo
 Enddo
 ENDDO
 !$omp end parallel do

end subroutine sgnf
