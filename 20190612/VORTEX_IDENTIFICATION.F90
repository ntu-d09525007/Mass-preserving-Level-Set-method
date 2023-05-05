SUBROUTINE VORTEX_IDENTIFICATION()
USE PRECISION
USE PROBLEM_DEF
USE FLUID_PROPERTIES, only : U,V,W
USE DUMMY_DATA, ONLY : F,G
IMPLICIT NONE
INTEGER :: I,J,K
REAL(DP) :: UX,UY,UZ,VX,VY,VZ,WX,WY,WZ,Q,R

 !$OMP PARALLEL DO PRIVATE(UX,UY,UZ,VX,VY,VZ,WX,WY,WZ,Q,R)
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
 DO I = 1, NODE_X
 
 	UX = (U(I,J,K)-U(I-1,J,K))/DX
	UY = 0.25*(U(I,J+1,K)+U(I-1,J+1,K)-U(I,J-1,K)-U(I-1,J-1,K))/DY
	UZ = 0.25*(U(I,J,K+1)+U(I-1,J,K+1)-U(I,J,K-1)-U(I-1,J,K-1))/DZ
	
	VX = 0.25*(V(I+1,J,K)+V(I+1,J-1,K)-V(I-1,J,K)-V(I-1,J-1,K))/DX
	VY = (V(I,J,K)-V(I,J-1,K))/DY
	VZ = 0.25*(V(I,J,K+1)+V(I,J-1,K+1)-V(I,J,K-1)-V(I,J-1,K-1))/DZ
	
	WX = 0.25*(W(I+1,J,K)+W(I+1,J,K-1)-W(I-1,J,K)-W(I-1,J,K-1))/DX
	WY = 0.25*(W(I,J+1,K)+W(I,J+1,K-1)-W(I,J-1,K)-W(I,J-1,K-1))/DY
	WZ = (W(I,J,K)-W(I,J,K-1))/DZ
	
	Q = -( 0.5*(Ux**2+Vy**2+Wz**2) + Uy*Vx + Uz*Wx + Vz*Wy )
	R = Uz*Vy*Wx - Uy*Vz*Wx - Uz*Vx*Wy + Ux*Vz*Wy + Uy*Vx*Wz - Ux*Vy*Wz

	F(I,J,K) = Q+eps
	G(I,J,K) = (Q/3.0)**3.0 + (R/2.0)**2.0
	
 ENDDO
 ENDDO
 ENDDO
 !$OMP END PARALLEL DO

END SUBROUTINE