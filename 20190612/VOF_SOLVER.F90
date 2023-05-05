subroutine SOLVE_VOF()
USE PRECISION
USE PROBLEM_DEF
USE VOF_DATA, ONLY : VOF, VVOF
IMPLICIT NONE
INTEGER :: I,J,K

 if( INTERFACE_METHOD==0 )RETURN 
 
 !CALL THINC_SOLVER(VVOF)
 CALL THINC_SOLVER(VOF)
 
 
end subroutine

SUBROUTINE THINC_SOLVER(VOF)
USE PRECISION
USE PROBLEM_DEF
IMPLICIT NONE
REAL(DP),DIMENSION(-2:NODE_X+3,-2:NODE_Y+3,-2:NODE_Z+3) :: VOF

 CALL WLIC_COF(VOF)
 CALL SPLIT_X(VOF)
 CALL SPLIT_Y(VOF)
 CALL SPLIT_Z(VOF)

END SUBROUTINE

SUBROUTINE WLIC_COF(VOF)
USE PRECISION
USE PROBLEM_DEF
USE LS_DATA, ONLY : PHI, NORMAL_X, NORMAL_Y, NORMAL_Z
USE VOF_DATA, ONLY : VOF_OLD, VOF_SX, VOF_SY, VOF_SZ, VOF_SOLVER
IMPLICIT NONE
INTEGER :: I,J,K
REAL(DP) :: A
REAL(DP),DIMENSION(-2:NODE_X+3,-2:NODE_Y+3,-2:NODE_Z+3) :: VOF

!$OMP PARALLEL DO
DO K = -2, NODE_Z+3
DO J = -2, NODE_Y+3
DO I = -2, NODE_X+3
 VOF_OLD(I,J,K) = VOF(I,J,K)
ENDDO
ENDDO
ENDDO
!$OMP END PARALLEL DO

 CALL FIND_NORMALS(PHI,VOF)

 CALL NORMALS_NORMALIZE

IF( VOF_SOLVER .LT. 3 )THEN

!$OMP PARALLEL DO PRIVATE(A)
DO K = 0, NODE_Z
DO J = 0, NODE_Y
DO I = 0, NODE_X
	 
   A = ABS(NORMAL_X(I,J,K)) + ABS(NORMAL_Y(I,J,K)) + ABS(NORMAL_Z(I,J,K))
	
   VOF_SX(I,J,K) = ABS(NORMAL_X(I,J,K)) / A
   VOF_SY(I,J,K) = ABS(NORMAL_Y(I,J,K)) / A
   VOF_SZ(I,J,K) = ABS(NORMAL_Z(I,J,K)) / A
  
END DO
END DO
END DO
!$OMP END PARALLEL DO

ELSE
	
	VOF_SX=1.0
	VOF_SY=1.0
	VOF_SZ=1.0
	
ENDIF

END SUBROUTINE

SUBROUTINE SPLIT_X(VOF)
USE PRECISION
USE PROBLEM_DEF
USE FLUID_PROPERTIES
USE LS_DATA
USE VOF_DATA, ONLY : VOF_OLD, VOF_SX, VOF_SOLVER
USE DUMMY_DATA
implicit none
integer :: i,j,k,ii,ib,BTN
real(kind=dp) :: a1,a3,xc,beta,alpha,isgn,a4,a5
REAL(DP),DIMENSION(-2:NODE_X+3,-2:NODE_Y+3,-2:NODE_Z+3) :: VOF

 !$omp parallel do private( a1,a3,a4,a5,xc,alpha,isgn,ib,ii,beta )
 do k = 0, NODE_Z
 do j = 0, NODE_Y
 do i = 0, NODE_X
 	
 	if( u(i,j,k)>0.0 )then
 		ii = i
 		isgn = 1.0
 	else
 	  ii = i+1
 	  isgn = 0.0
 	end if
 	
 	if( VOF_SOLVER==4 )then
	  beta = 2.3*ABS(NORMAL_X(ii,j,K)) + 0.01 
 	else 
 	  beta = 2.3_dp
  end if


 	if( VOF(ii,j,k) > 1.0-ePS .or. VOF(ii,j,k) < ePS ) then
 		
 		F(i,j,k) = VOF(ii,j,k) * u(i,j,k) * dt
 	
 	else
 	  
 	  ib = max(1,ii-1)
 	  
 	  if( VOF(ib,j,k) < VOF(ii+1,j,k) )then
 	  	alpha = 1.0
 	  else
 	  	alpha = -1.0
 	  end if
 	  
 	  a1 = Exp( beta * ( 2.0 * VOF(ii,j,k) - 1.0 ) / alpha )
    a3 = Exp(beta)
    xc = 0.5 / beta * Log( ( a3 * a3 - a1 * a3 ) / ( a1 * a3 - 1.0 ) )
    a4 = Cosh( beta * ( isgn - U(i,j,k) * dt / dx - xc ) )
    a5 = Cosh( beta * ( isgn - xc ) )
 	  
    F(i,j,k) = 0.5 * ( U(i,j,k) * dt - alpha * Dx / beta * Log( a4 / a5 ) )

    F(i,j,k) = F(i,j,k) * VOF_SX(ii,j,k) + VOF(ii,j,k) * U(i,j,k) * dt * (1.0 - VOF_Sx(ii,j,k))
    
  end if 
 	 	
 end do
 end do
 end do
 !$omp end parallel do
 
 !$omp parallel do 
 do k = 1, NODE_Z
 do j = 1, NODE_Y
 do i = 1, NODE_X
    VOF(i,j,k) = VOF(i,j,k) - ( F(i,j,k) - F(i-1,j,k) ) / Dx + VOF_OLD(I,J,K)*DT*(U(I,J,K)-U(I-1,J,K))/DX 
  End Do
  End Do
  end do
 !$omp end parallel do
  
 
end subroutine

SUBROUTINE SPLIT_Y(VOF)
USE PRECISION
USE PROBLEM_DEF
USE FLUID_PROPERTIES
USE LS_DATA
USE VOF_DATA, ONLY : VOF_OLD, VOF_SY, VOF_SOLVER
USE DUMMY_DATA
implicit none
integer :: i,j,k,ii,ib,BTN
real(kind=dp) :: a1,a3,xc,beta,alpha,isgn,a4,a5
REAL(DP),DIMENSION(-2:NODE_X+3,-2:NODE_Y+3,-2:NODE_Z+3) :: VOF
 
 !$omp parallel do private( a1,a3,a4,a5,xc,alpha,isgn,ib,ii,beta )
 do k = 0, NODE_Z
 do j = 0, NODE_Y
 do i = 0, NODE_X
 	
 	if( V(i,j,k)>0.0 )then
 		ii = j
 		isgn = 1.0
 	else
 	  ii = j+1
 	  isgn = 0.0
 	end if
 	
 	
 	if( VOF_SOLVER==4 )then
	  beta = 2.3*ABS(NORMAL_Y(I,II,K)) + 0.01 
 	else 
 	  beta = 2.3_dp
    end if

 	if( VOF(i,ii,k) > 1.0-EPS .or. VOF(i,ii,k) < EPS ) then
 		
 		G(i,j,k) = VOF(i,ii,k) * V(i,j,k) * dt
 	
 	else
 	  
 	  ib = max(1,ii-1)
 	  
 	  if( VOF(i,ib,k) < VOF(i,ii+1,k) )then
 	  	alpha = 1.0
 	  else
 	  	alpha = -1.0
 	  end if
 	  
 	a1 = Exp( beta * ( 2.0 * VOF(i,ii,k) - 1.0 ) / alpha )
    a3 = Exp(beta)
    xc = 0.5 / beta * Log( ( a3 * a3 - a1 * a3 ) / ( a1 * a3 - 1.0 ) )
    a4 = Cosh( beta * ( isgn - V(i,j,k) * dt / dx - xc ) )
    a5 = Cosh( beta * ( isgn - xc ) )
 	  
    G(i,j,k) = 0.5 * ( V(i,j,k) * dt - alpha * Dx / beta * Log( a4 / a5 ) )

    G(i,j,k) = G(i,j,k) * VOF_SY(i,ii,k) + VOF(i,ii,k) * V(i,j,k) * dt * (1.0 - VOF_SY(i,ii,k))
    
  end if 
 	 	
 end do
 end do
 end do
 !$omp end parallel do
 
 !$omp parallel do 
 do k = 1, NODE_Z
 do j = 1, NODE_Y
 do i = 1, NODE_X
    VOF(i,j,k) = VOF(i,j,k) - ( G(i,j,k) - G(i,j-1,k) ) / Dx + VOF_OLD(I,J,K)*DT*(V(I,J,K)-V(I,J-1,K))/DY
  End Do
  End Do
  end do
 !$omp end parallel do
 

end subroutine

SUBROUTINE SPLIT_Z(VOF)
USE PRECISION
USE PROBLEM_DEF
USE FLUID_PROPERTIES
USE LS_DATA
USE VOF_DATA, ONLY : VOF_OLD, VOF_SZ, VOF_SOLVER
USE DUMMY_DATA
implicit none
integer :: i,j,k,ii,ib,BTN
real(kind=dp) :: a1,a3,xc,beta,alpha,isgn,a4,a5
REAL(DP),DIMENSION(-2:NODE_X+3,-2:NODE_Y+3,-2:NODE_Z+3) :: VOF
 
 !$omp parallel do private( a1,a3,a4,a5,xc,alpha,isgn,ib,ii,beta )
 do k = 0, NODE_Z
 do j = 0, NODE_Y
 do i = 0, NODE_X
 	
 	if( W(i,j,k)>0.0 )then
 		ii = k
 		isgn = 1.0
 	else
 	  ii = k+1
 	  isgn = 0.0
 	end if
 	
 	if( VOF_SOLVER==4 )then
	  beta = 2.3*ABS(NORMAL_Z(i,j,II)) + 0.01 
 	else 
 	  beta = 2.3_dp
    end if

 	if( VOF(i,j,ii) > 1.0-EPS .or. VOF(i,j,ii) < EPS ) then
 		
 		H(i,j,k) = VOF(i,j,ii) * W(i,j,k) * dt
 	
 	else
 	  
 	  ib = max(1,ii-1)
 	  
 	  if( VOF(i,j,ib) < VOF(i,j,ii+1) )then
 	  	alpha = 1.0
 	  else
 	  	alpha = -1.0
 	  end if
 	  
 	  a1 = Exp( beta * ( 2.0 * VOF(i,j,ii) - 1.0 ) / alpha )
    a3 = Exp(beta)
    xc = 0.5 / beta * Log( ( a3 * a3 - a1 * a3 ) / ( a1 * a3 - 1.0 ) )
    a4 = Cosh( beta * ( isgn - W(i,j,k) * dt / dx - xc ) )
    a5 = Cosh( beta * ( isgn - xc ) )
 	  
    H(i,j,k) = 0.5 * ( W(i,j,k) * dt - alpha * Dx / beta * Log( a4 / a5 ) )

    H(i,j,k) = H(i,j,k) * VOF_SZ(i,j,ii) + VOF(i,j,ii) * W(i,j,k) * dt * (1.0 - VOF_SZ(i,j,ii))
    
   end if 
 	 	
 end do
 end do
 end do
 !$omp end parallel do
 
 !$omp parallel do 
 do k = 1, NODE_Z
 do j = 1, NODE_Y
 do i = 1, NODE_X
    VOF(i,j,k) = VOF(i,j,k) - ( H(i,j,k) - H(i,j,k-1) ) / Dx + VOF_OLD(I,J,K)*DT*(W(I,J,K)-W(I,J,K-1))/DZ
    VOF(I,J,K) = MIN(VOF(I,J,K),1.0)
    IF(VOF(I,J,K)<EPS)VOF(I,J,K)=0.0
  End Do
  End Do
  end do
 !$omp end parallel do
  
 CALL BC3D(VOF)
 
end subroutine
