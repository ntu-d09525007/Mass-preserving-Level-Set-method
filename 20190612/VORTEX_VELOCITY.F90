SUBROUTINE VORTEX_VELOCITY()
USE PRECISION
USE PROBLEM_DEF
USE FLUID_PROPERTIES
IMPLICIT NONE
INTEGER :: I,J,K
REAL(DP) :: XS,YS,ZS

 !$OMP PARALLEL DO PRIVATE(XS,YS,ZS)
 DO K = 0, NODE_Z
 DO J = 0, NODE_Y
 DO I = 0, NODE_X
 
  xs=0.5*(x(i)+x(i+1))
  ys=0.5*(y(j)+y(j+1))
  zs=0.5*(z(k)+z(k+1))

  u(i,j,k)= 2.0*sin(pi*xs)**2*sin(2.0*pi*y(j))*sin(2.0*pi*z(k))*cos(pi*time/TIME_TO_STOP)
  v(i,j,k)=    -sin(2.0*pi*x(i))*sin(pi*ys)**2*sin(2.0*pi*z(k))*cos(pi*time/TIME_TO_STOP)
  w(i,j,k)=    -sin(2.0*pi*x(i))*sin(2.0*pi*y(j))*sin(pi*zs)**2*cos(pi*time/TIME_TO_STOP)

  xs=X(I)
  ys=Y(J)
  zs=Z(K)

  uh(i,j,k)= 2.0*sin(pi*xs)**2*sin(2.0*pi*ys)*sin(2.0*pi*zs)*cos(pi*time/TIME_TO_STOP)
  vh(i,j,k)=    -sin(2.0*pi*xs)*sin(pi*ys)**2*sin(2.0*pi*zs)*cos(pi*time/TIME_TO_STOP)
  wh(i,j,k)=    -sin(2.0*pi*xs)*sin(2.0*pi*ys)*sin(pi*zs)**2*cos(pi*time/TIME_TO_STOP)
   
 END DO
 END DO
 END DO
 !$OMP END PARALLEL DO
 
 CALL BC3D(UH)
 CALL BC3D(VH)
 CALL BC3D(WH)
 

END SUBROUTINE