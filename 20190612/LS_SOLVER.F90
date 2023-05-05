subroutine SOLVE_LS()
USE PRECISION
USE PROBLEM_DEF
USE LS_DATA
implicit none
integer :: i,j,k
real(8) :: heav1, heav2, tmp

 IF(INTERFACE_METHOD==1 .OR. INTERFACE_METHOD==3)RETURN
 
 PHI = PHI1
 CALL LS_RK3_SOLVER
 CALL LS_MAINTAIN
 CALL MPLS(B1_iM)
 PHI1 = PHI

 PHI = PHI2
 CALL LS_RK3_SOLVER
 CALL LS_MAINTAIN
 CALL MPLS(B2_iM)
 PHI2 = PHI

 !$omp parallel do
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
 DO I = 1, NODE_X
   phi(i,j,k) = max(phi1(i,j,k), phi2(i,j,k))
 enddo
 ENDDO
 enddo
 !$omp end parallel do  

CALL BC_LS()

CALL LS_REDISTANCE(PHI,1)

end subroutine

SUBROUTINE LS_RK3_SOLVER()
USE PRECISION
USE PROBLEM_DEF
USE LS_DATA, ONLY : PHI
USE DUMMY_DATA, ONLY : S0,S1,S2,SS0,SS1,SS2
IMPLICIT NONE
INTEGER :: I,J,K
 
 CALL CELL_FACE_VELOCITY()

 CALL LS_sorce(S0)
 
 !$omp parallel do
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
 DO I = 1, NODE_X
   PHI(i,j,K) = PHI(i,j,K) + dt*s0(i,j,K)
 enddo
 ENDDO
 enddo
 !$omp end parallel do  
 
 CALL BC_LS()
 
 CALL LS_sorce(S1)

 !$omp parallel do
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
 DO I = 1, NODE_X
   PHI(i,j,K) = PHI(i,j,K) + dt/4.0_DP*(-3.0_DP*s0(i,j,K)+s1(i,j,K))
 enddo
 ENDDO
 enddo
 !$omp end parallel do  

 CALL BC_LS()
 
 CALL LS_sorce(S2)
 
 !$omp parallel do
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
 DO I = 1, NODE_X
   PHI(i,j,K)=PHI(i,j,K)+dt/12.0_DP*(-s0(i,j,K)-s1(i,j,K)+8.0_DP*s2(i,j,K))
 enddo
 enddo
 ENDDO
 !$omp end parallel do
 
 CALL BC_LS()
  
END SUBROUTINE

SUBROUTINE CELL_FACE_VELOCITY()
USE PRECISION
USE FLUID_PROPERTIES
USE PROBLEM_DEF
IMPLICIT NONE
INTEGER :: I,J,K

!$OMP PARALLEL DO
DO K = 1, NODE_Z
DO J = 1, NODE_Y
DO I = 1, NODE_X
    UH(I,J,K) = 0.5_DP*(U(I,J,K)+U(I-1,J,K))
    VH(I,J,K) = 0.5_DP*(V(I,J,K)+V(I,J-1,K))
    WH(I,J,K) = 0.5_DP*(W(I,J,K)+W(I,J,K-1))
END DO
END DO
END DO
!$OMP END PARALLEL DO

 CALL BC3D(UH)
 CALL BC3D(VH)
 CALL BC3D(WH)

END SUBROUTINE

subroutine LS_sorce(S)
USE PRECISION
USE PROBLEM_DEF
USE DUMMY_DATA
USE FLUID_PROPERTIES
USE LS_DATA
IMPLICIT NONE
INTEGER :: I,J,K
REAL(DP),DIMENSION(-2:NODE_X+3,-2:NODE_Y+3,-2:NODE_Z+3) :: S

if( LS_SOLVER < 10 )then

!$OMP PARALLEL DO
DO K = -2, NODE_Z+3
DO J = -2, NODE_Y+3
DO I = -2, NODE_X+3
 up(I,J,K) = 0.5*(uH(I,J,K)+abs(uH(I,J,K)))*PHI(I,J,K)
 um(I,J,K) = 0.5*(uH(I,J,K)-abs(uH(I,J,K)))*PHI(I,J,K)
 vp(I,J,K) = 0.5*(vH(I,J,K)+abs(vH(I,J,K)))*PHI(I,J,K)
 vm(I,J,K) = 0.5*(vH(I,J,K)-abs(vH(I,J,K)))*PHI(I,J,K)
 wp(I,J,K) = 0.5*(wH(I,J,K)+abs(wH(I,J,K)))*PHI(I,J,K)
 wm(I,J,K) = 0.5*(wH(I,J,K)-abs(wH(I,J,K)))*PHI(I,J,K)
end do               
end do
END DO               
!$OMP END PARALLEL DO
                  
 !$OMP PARALLEL DO
 DO K = 1, NODE_Z 
 DO J = 1, NODE_Y 
    call split_sorce(um(:,j,k),up(:,j,k),fp(:,j,k),fm(:,j,k),NODE_X,LS_SOLVER)  
 end do
 end do
 !$OMP END PARALLEL DO
 
 !$OMP PARALLEL DO
 DO K = 1, NODE_Z
 DO I = 1, NODE_X
    call split_sorce(vm(i,:,k),vp(i,:,k),gp(i,:,k),gm(i,:,k),NODE_Y,LS_SOLVER) 
 END DO
 END DO
 !$OMP END PARALLEL DO

 !$OMP PARALLEL DO
 DO J = 1, NODE_Y
 DO I = 1, NODE_X
    call split_sorce(wm(i,j,:),wp(i,j,:),hp(i,j,:),hm(i,j,:),NODE_Z,LS_SOLVER) 
 END DO
 END DO
 !$OMP END PARALLEL DO

!$OMP PARALLEL DO
DO K = -2, NODE_Z+3
DO J = -2, NODE_Y+3
DO I = -2, NODE_X+3
 F(I,J,K) = FP(I,J,K)  +  FM(I,J,K)
 G(I,J,K) = GP(I,J,K)  +  GM(I,J,K)
 H(I,J,K) = HP(I,J,K)  +  HM(I,J,K)
end do               
end do
END DO               
!$OMP END PARALLEL DO

  
 !$omp parallel do
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
 DO I = 1, NODE_X
     s(i,j,k) = -(F(i,j,k)-F(i-1,j,k))/dx -(G(i,j,k)-G(i,j-1,k))/dy -(H(i,j,k)-H(i,j,k-1))/dz    
 end do
 end do
 end do
 !$omp end parallel do

else if( LS_SOLVER==10 )then
    
 !$OMP PARALLEL DO
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
    call UCCD(NODE_X,dx,UH(1:NODE_X,j,k),PHI(1:NODE_X,j,k),F(1:NODE_X,j,k))
 end do
 end do
 !$OMP END PARALLEL DO
    
 !$OMP PARALLEL DO
 DO K = 1, NODE_Z
 DO I = 1, NODE_X
    call UCCD(NODE_Y,dy,VH(i,1:NODE_Y,k),PHI(i,1:NODE_Y,k),G(i,1:NODE_Y,k))
 end do
 end do
 !$OMP END PARALLEL DO

 !$OMP PARALLEL DO
 DO J = 1, NODE_Y
 DO I = 1, NODE_X
    call UCCD(NODE_Z,dz,WH(i,j,1:NODE_Z),PHI(i,j,1:NODE_Z),H(i,j,1:NODE_Z))
 end do
 end do
 !$OMP END PARALLEL DO

 !$omp parallel do
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
 DO I = 1, NODE_X
     s(i,j,k) = -( F(i,j,k)*UH(i,j,k) + G(i,j,k)*VH(i,j,k) + H(i,j,k)*WH(i,j,k) )    
 end do
 end do
 end do
 !$omp end parallel do
    
end if
  
end subroutine

subroutine split_sorce(f,g,fp,gm,n,btn)
USE PRECISION
implicit none
integer :: n,btn
real(kind=dp),dimension(-2:n+3) :: f,g,fp,fm,gp,gm

if( btn==0 )then
 call weno_js(f,fp,fm,N)
 call weno_js(g,gp,gm,N)
else if( btn==1 )then
 call weno_z(f,fp,fm,N)
 call weno_z(g,gp,gm,N) 
else if( btn==2 )then
 call weno_m(f,fp,fm,N)
 call weno_m(g,gp,gm,N)
else if( btn==3 )then
 call ocrweno(f,fp,fm,N)
 call ocrweno(g,gp,gm,N) 
else if( btn==4 )then
 call ocrweno_LD(f,fp,fm,N)
 call ocrweno_LD(g,gp,gm,N)
end if

end subroutine

SUBROUTINE MPLS(im)
USE PRECISION
USE PROBLEM_DEF
USE LS_DATA
USE FLUID_PROPERTIES, only : rho
USE DUMMY_DATA, ONLY : A
IMPLICIT NONE
INTEGER :: I,J,K,IT
REAL(DP) :: FS,  TMP
real(dp), optional :: im


DO IT = 1, 5

 CALL AMURHO()
 CALL LS_CCD(PHI)

 VOL_LS=0.0
 MASS_LS=0.0
 FS=0.0
 
 !$OMP PARALLEL DO
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
 DO I = 1, NODE_X
   A(I,J,K) = 1.0
   GRAD(I,J,K) = DSQRT(NORMAL_X(I,J,K)**2+NORMAL_Y(I,J,K)**2+NORMAL_Z(I,J,K)**2)
 ENDDO
 ENDDO
 ENDDO
 !$OMP END PARALLEL DO
 
 !$OMP PARALLEL DO REDUCTION(+:MASS_LS,VOL_LS,FS)
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
 DO I = 1, NODE_X  
   VOL_LS = VOL_LS + HEAVY(I,J,K)
   MASS_LS = MASS_LS + HEAVY(I,J,K)*RHO(I,J,K)

   !FS = FS + (2.0*(1.0-RATIO_RHO)*HEAVY(i,j,k)+RATIO_RHO)*DELTA(I,J,K)**2*GRAD(I,J,K)
   FS = FS + (2.0*(RATIO_RHO-1.0)*HEAVY(i,j,k)+1.0)*DELTA(I,J,K)**2*GRAD(I,J,K)*A(I,J,K)
 END DO
 END DO
 END DO
 !$OMP END PARALLEL DO

if(present(im))then
    FS=(im-MASS_LS)/(DT*FS)
else
    FS=(IMASS_LS-MASS_LS)/(DT*FS)
endif

 !$OMP PARALLEL DO
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
 DO I = 1, NODE_X
   PHI(I,J,K) = PHI(I,J,K) + FS*A(I,J,K)*DELTA(I,J,K)*GRAD(I,J,K)*DT
 ENDDO
 ENDDO
 ENDDO
 !$OMP END PARALLEL DO

ENDDO

END SUBROUTINE
