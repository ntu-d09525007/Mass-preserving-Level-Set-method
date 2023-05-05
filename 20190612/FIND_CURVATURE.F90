SUBROUTINE FIND_CURVATURE
use precision
USE PROBLEM_DEF
USE LS_DATA
USE VOF_DATA
IMPLICIT NONE

  IF(ST_FORCE.EQ.0)RETURN
  
  !IF( METHOD_CNT.EQ.1 )THEN
    !CALL LS_CENTRAL(PHI)
    CALL LS_CCD(PHI)
  !ELSE
  !  CALL VOF_CENTRAL(VOF)
  !ENDIF
  
END SUBROUTINE

SUBROUTINE VOF_CENTRAL(TARGET)
USE PRECISION
USE VOF_DATA
USE PROBLEM_DEF
USE LS_DATA
IMPLICIT NONE
INTEGER :: I,J,K
REAL(DP),DIMENSION(-2:NODE_X+3,-2:NODE_Y+3,-2:NODE_Z+3) :: TARGET

 CALL FIND_NORMALS(PHI,TARGET)
 CALL NORMALS_NORMALIZE
 
 !$OMP PARALLEL DO
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
 DO I = 1, NODE_X	
 	 CURV(I,J,K) = 0.5*(NORMAL_X(I+1,J,K)-NORMAL_X(I-1,J,K))/DX + &
 	               0.5*(NORMAL_Y(I,J+1,K)-NORMAL_Y(I,J-1,K))/DY + &
 	               0.5*(NORMAL_Z(I,J,K+1)-NORMAL_Z(I,J,K-1))/DZ
 ENDDO
 ENDDO
 ENDDO
 !$OMP END PARALLEL DO
 
 CALL BC3D(CURV)
 
END SUBROUTINE

SUBROUTINE LS_CENTRAL(TARGET)
USE PRECISION
USE PROBLEM_DEF
USE LS_DATA
USE DUMMY_DATA
IMPLICIT NONE
INTEGER :: I,J,K
REAL(DP) :: FX,FY,FZ,FXX,FYY,FZZ,FXY,FXZ,FYZ
REAL(DP),DIMENSION(-2:NODE_X+3,-2:NODE_Y+3,-2:NODE_Z+3) :: TARGET

     CALL HEAVY_F(TARGET)
   
    !$OMP PARALLEL DO PRIVATE(FX,FY,FZ,FXX,FYY,FZZ,FXY,FXZ,FYZ)
    DO K = 1, NODE_Z
    DO J = 1, NODE_Y
    DO I = 1, NODE_X
    
       fx=0.5_DP/dx*(TARGET(i+1,j,k)-TARGET(i-1,j,k))
       fy=0.5_DP/dy*(TARGET(i,j+1,k)-TARGET(i,j-1,k))
       fz=0.5_DP/dz*(TARGET(i,j,k+1)-TARGET(i,j,k-1))        
    
       fxx=1.0_DP/dx**2*(TARGET(i+1,j,k)-2.0_DP*TARGET(i,j,k)+TARGET(i-1,j,k))
       fyy=1.0_DP/dy**2*(TARGET(i,j+1,k)-2.0_DP*TARGET(i,j,k)+TARGET(i,j-1,k))
       fzz=1.0_DP/dz**2*(TARGET(i,j,k+1)-2.0_DP*TARGET(i,j,k)+TARGET(i,j,k-1))
    
       fxy=0.25_DP/(dx*dy)*(TARGET(i+1,j+1,k)-TARGET(i+1,j-1,k) &
             -              TARGET(i-1,j+1,k)+TARGET(i-1,j-1,k))
       fxz=0.25_DP/(dx*dz)*(TARGET(i+1,j,k+1)-TARGET(i+1,j,k-1) &
             -              TARGET(i-1,j,k+1)+TARGET(i-1,j,k-1))
       fyz=0.25_DP/(dy*dz)*(TARGET(i,j+1,k+1)-TARGET(i,j-1,k+1) &
             -              TARGET(i,j+1,k-1)+TARGET(i,j-1,k-1))
    
       CURV(i,j,k)=(fxx*(fy**2+fz**2)+fyy*(fx**2+fz**2)+fzz*(fx**2+fy**2) &
                   - 2.0_DP*(fxy*fx*fy+fxz*fx*fz+fyz*fy*fz)) &
                   / (fx**2+fy**2+fz**2+EPS)**1.5 
    
       NORMAL_X(I,J,K) = FX !/ DSQRT(FX**2+FY**2+FZ**2+EPS)
       NORMAL_Y(I,J,K) = FY !/ DSQRT(FX**2+FY**2+FZ**2+EPS)
       NORMAL_Z(I,J,K) = FZ !/ DSQRT(FX**2+FY**2+FZ**2+EPS)
    
    END DO
    END DO
    END DO
    !$OMP END PARALLEL DO
 
   CALL BC3D(CURV)
   CALL BC3D(NORMAL_X)
   CALL BC3D(NORMAL_Y)
   CALL BC3D(NORMAL_Z)
 
END SUBROUTINE

SUBROUTINE LS_CCD(TARGET)
USE PRECISION
USE PROBLEM_DEF
USE LS_DATA
USE DUMMY_DATA
IMPLICIT NONE
INTEGER :: I,J,K
REAL(DP) :: FX,FY,FZ,FXX,FYY,FZZ,FXY,FXZ,FYZ
REAL(DP),DIMENSION(-2:NODE_X+3,-2:NODE_Y+3,-2:NODE_Z+3) :: TARGET

 CALL HEAVY_F(TARGET)

 !$OMP PARALLEL DO
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
 	CALL CCD(NODE_X,Dx,TARGET(1:NODE_X,J,K),A(1:NODE_X,J,K),F(1:NODE_X,J,K))
 END DO
 END DO
 !$OMP END PARALLEL DO
 
 !$OMP PARALLEL DO
 DO K = 1, NODE_Z
 DO I = 1, NODE_X
 	CALL CCD(NODE_Y,DY,TARGET(I,1:NODE_Y,K),B(I,1:NODE_Y,K),G(I,1:NODE_Y,K))
 END DO
 END DO
 !$OMP END PARALLEL DO
 
 !$OMP PARALLEL DO
 DO J = 1, NODE_Z
 DO I = 1, NODE_X
 	CALL CCD(NODE_Z,DZ,TARGET(I,J,1:NODE_Z),C(I,J,1:NODE_Z),H(I,J,1:NODE_Z))
 END DO
 END DO
 !$OMP END PARALLEL DO
 
!============================================= 

 ! FZ_X
 !$OMP PARALLEL DO
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
 	CALL CCD(NODE_X,Dx,C(1:NODE_X,J,K),UP(1:NODE_X,J,K),UM(1:NODE_X,J,K))
 END DO
 END DO
 !$OMP END PARALLEL DO
 
 ! FX_Y
 !$OMP PARALLEL DO
 DO K = 1, NODE_Z
 DO I = 1, NODE_X
 	CALL CCD(NODE_Y,DY,A(I,1:NODE_Y,K),VP(I,1:NODE_Y,K),VM(I,1:NODE_Y,K))
 END DO
 END DO
 !$OMP END PARALLEL DO
 
 ! FY_Z
 !$OMP PARALLEL DO
 DO J = 1, NODE_Z
 DO I = 1, NODE_X
 	CALL CCD(NODE_Z,DZ,B(I,J,1:NODE_Z),WP(I,J,1:NODE_Z),WM(I,J,1:NODE_Z))
 END DO
 END DO
 !$OMP END PARALLEL DO

 !$OMP PARALLEL DO PRIVATE(FX,FY,FZ,FXX,FYY,FZZ,FXY,FXZ,FYZ)
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
 DO I = 1, NODE_X
 
    fx=A(I,J,K)
    fy=B(I,J,K)
    fz=C(I,J,K)       

    fxx=F(I,J,K)
    fyy=G(I,J,K)
    fzz=H(I,J,K)

    fxy=VP(I,J,K)
    fxz=UP(I,J,K)
    fyz=WP(I,J,K)
    
    CURV(i,j,k)=(fxx*(fy**2+fz**2)+fyy*(fx**2+fz**2)+fzz*(fx**2+fy**2) &
                - 2.0_DP*(fxy*fx*fy+fxz*fx*fz+fyz*fy*fz)) &
                / (fx**2+fy**2+fz**2+EPS)**1.5 

    NORMAL_X(I,J,K) = FX !/ DSQRT(FX**2+FY**2+FZ**2+EPS)
    NORMAL_Y(I,J,K) = FY !/ DSQRT(FX**2+FY**2+FZ**2+EPS)
    NORMAL_Z(I,J,K) = FZ !/ DSQRT(FX**2+FY**2+FZ**2+EPS)
 
 END DO
 END DO
 END DO
 !$OMP END PARALLEL DO
 
 CALL BC3D(CURV)
 CALL BC3D(NORMAL_X)
 CALL BC3D(NORMAL_Y)
 CALL BC3D(NORMAL_Z)

END SUBROUTINE
