subroutine weno_z(f,fp,fm,Nx)
USE PRECISION
implicit none
integer :: Nx, i
real(DP),dimension(-2:Nx+3) :: f, fp, fm
real(DP) :: a1,a2,a3,b1,b2,b3,w1,w2,w3

!$OMP PARALLEL DO PRIVATE(B1,B2,B3,A1,A2,A3,W1,W2,W3)
do i = 0, Nx
	
	b1 = 13.0_DP*(f(i-2)-2.0_DP*f(i-1)+f(i))**2 + 3.0_DP*(f(i-2)-4.0_DP*f(i-1)+3.0_DP*f(i))**2
	b2 = 13.0_DP*(f(i-1)-2.0_DP*f(i)+f(i+1))**2 + 3.0_DP*(f(i-1)-f(i+1))**2
	b3 = 13.0_DP*(f(i)-2.0_DP*f(i+1)+f(i+2))**2 + 3.0_DP*(3.0_DP*f(i)-4.0_DP*f(i+1)+f(i+2))**2
	
	a1 = 1.0_DP*(1.0_DP+abs(b3-b1)/(EPS+b1))
	a2 = 6.0_DP*(1.0_DP+abs(b3-b1)/(EPS+b2))
	a3 = 3.0_DP*(1.0_DP+abs(b3-b1)/(EPS+b3))
	
	w1 = a1/(a1+a2+a3)
	w2 = a2/(a1+a2+a3)
	w3 = a3/(a1+a2+a3)
	
	fm(i) = w1/3.0_DP*f(i-2) - (7.0_DP*w1+w2)/6.0_DP*f(i-1) + (11.0_DP*w1+5.0_DP*w2+2.0_DP*w3)/6.0_DP*f(i) &
			+ (2.0_DP*w2+5.0_DP*w3)/6.0_DP*f(i+1) - w3/6.0_DP*f(i+2)
			
end do
!$OMP END PARALLEL DO
!$OMP PARALLEL DO PRIVATE(B1,B2,B3,A1,A2,A3,W1,W2,W3)
do i = 0, Nx
	
	b3 = 13.0_DP*(f(i-1)-2.0_DP*f(i)  +f(i+1))**2 + 3.0_DP*(f(i-1)-4.0_DP*f(i)+3.0_DP*f(i+1))**2
	b2 = 13.0_DP*(f(i)  -2.0_DP*f(i+1)+f(i+2))**2 + 3.0_DP*(f(i)-f(i+2))**2
	b1 = 13.0_DP*(f(i+1)-2.0_DP*f(i+2)+f(i+3))**2 + 3.0_DP*(3.0_DP*f(i+1)-4.0_DP*f(i+2)+f(i+3))**2
	
  a1 = 1.0_DP * ( 1.0_DP + abs(b1-b3)/(EPS+b1) )
  a2 = 6.0_DP * ( 1.0_DP + abs(b1-b3)/(EPS+b2) )
  a3 = 3.0_DP * ( 1.0_DP + abs(b1-b3)/(EPS+b3) )	
	
  w1 = a1 / ( a1+a2+a3)
  w2 = a2 / ( a1+a2+a3)
  w3 = a3 / ( a1+a2+a3)
 	
 	fp(i) = w3*(-f(i-1)+5.0_DP*f(i)+2.0_DP*f(i+1))/6.0_DP &
 				 +w2*(2.0_DP*f(i)+5.0_DP*f(i+1)-f(i+2))/6.0_DP &
 				 +w1*(11.0_DP*f(i+1)-7.0_DP*f(i+2)+2.0_DP*f(i+3))/6.0_DP	
	
	
end do
!$OMP END PARALLEL DO
end subroutine

subroutine weno_js(f,fp,fm,Nx)
USE PRECISION
implicit none
integer :: Nx, i
real(DP),dimension(-2:Nx+3) :: f, fp, fm
real(DP) :: a1,a2,a3,b1,b2,b3,w1,w2,w3

!$OMP PARALLEL DO PRIVATE(B1,B2,B3,A1,A2,A3,W1,W2,W3)
do i = 0, Nx
	
	b1 = 13.0_DP*(f(i-2)-2.0*f(i-1)+f(i))**2 + 3.0_DP*(f(i-2)-4*f(i-1)+3.0*f(i))**2
	b2 = 13.0_DP*(f(i-1)-2.0*f(i)+f(i+1))**2 + 3.0_DP*(f(i-1)-f(i+1))**2
	b3 = 13.0_DP*(f(i)-2.0*f(i+1)+f(i+2))**2 + 3.0_DP*(3.0*f(i)-4.0*f(i+1)+f(i+2))**2
	
	a1 = 1.0_DP/(EPS+b1)**2
	a2 = 6.0_DP/(EPS+b2)**2
	a3 = 3.0_DP/(EPS+b3)**2
	
	w1 = a1/(a1+a2+a3)
	w2 = a2/(a1+a2+a3)
	w3 = a3/(a1+a2+a3)
	
	fm(i) = w1/3.0_DP*f(i-2) - (7.0_DP*w1+w2)/6.0_DP*f(i-1) + (11.0_DP*w1+5.0_DP*w2+2.0_DP*w3)/6.0_DP*f(i) &
			+ (2.0_DP*w2+5.0_DP*w3)/6.0_DP*f(i+1) - w3/6.0_DP*f(i+2)
			
end do
!$OMP END PARALLEL DO
!$OMP PARALLEL DO PRIVATE(B1,B2,B3,A1,A2,A3,W1,W2,W3)	
do i = 0, Nx
	
	b3 = 13.0*(f(i-1)-2.0*f(i)  +f(i+1))**2 + 3.0*(f(i-1)-4.0*f(i)+3.0*f(i+1))**2
	b2 = 13.0*(f(i)  -2.0*f(i+1)+f(i+2))**2 + 3.0*(f(i)-f(i+2))**2
	b1 = 13.0*(f(i+1)-2.0*f(i+2)+f(i+3))**2 + 3.0*(3.0*f(i+1)-4.0*f(i+2)+f(i+3))**2
	
  a1 = 1.0/(EPS+b1)**2
  a2 = 6.0/(EPS+b2)**2
  a3 = 3.0/(EPS+b3)**2	
	
  w1 = a1 / (a1+a2+a3)
  w2 = a2 / (a1+a2+a3)
  w3 = a3 / (a1+a2+a3)
 	
 	fp(i) = w3*(-f(i-1)+5.0*f(i)+2.0*f(i+1))/6.0 &
 				 +w2*(2.0*f(i)+5.0*f(i+1)-f(i+2))/6.0 &
 				 +w1*(11.0*f(i+1)-7.0*f(i+2)+2.0*f(i+3))/6.0	
	
	
end do
!$OMP END PARALLEL DO
end subroutine

subroutine weno_m(f,fp,fm,Nx)
USE PRECISION
implicit none
integer :: Nx, i
real(DP),dimension(-2:Nx+3) :: f, fp, fm
real(DP) :: a1,a2,a3,b1,b2,b3,w1,w2,w3

!$OMP PARALLEL DO PRIVATE(B1,B2,B3,A1,A2,A3,W1,W2,W3)
do i = 0, Nx
	
	b1 = (13.0*(f(i-2)-2.0*f(i-1)+f(i))**2 + 3.0*(f(i-2)-4*f(i-1)+3.0*f(i))**2)/12.0
	b2 = (13.0*(f(i-1)-2.0*f(i)+f(i+1))**2 + 3.0*(f(i-1)-f(i+1))**2)/12.0
	b3 = (13.0*(f(i)-2.0*f(i+1)+f(i+2))**2 + 3.0*(3.0*f(i)-4.0*f(i+1)+f(i+2))**2)/12.0
	
	a1 = 0.1/(EPS+b1)**2
	a2 = 0.6/(EPS+b2)**2
	a3 = 0.3/(EPS+b3)**2
	
	w1 = a1/(a1+a2+a3)
	w2 = a2/(a1+a2+a3)
	w3 = a3/(a1+a2+a3)
	
	w1 = g(w1,1)
	w2 = g(w2,2)
	w3 = g(w3,3)
	
	fm(i) = w1/3.0*f(i-2) - (7.0*w1+w2)/6.0*f(i-1) + (11.0*w1+5.0*w2+2.0*w3)/6.0*f(i) &
			+ (2.0*w2+5.0*w3)/6.0*f(i+1) - w3/6.0*f(i+2)
			
end do
!$OMP END PARALLEL DO
!$OMP PARALLEL DO PRIVATE(B1,B2,B3,A1,A2,A3,W1,W2,W3)	
do i = 0, Nx
	
	b3 = (13.0*(f(i-1)-2.0*f(i)  +f(i+1))**2 + 3.0*(f(i-1)-4.0*f(i)+3.0*f(i+1))**2)/12.0
	b2 = (13.0*(f(i)  -2.0*f(i+1)+f(i+2))**2 + 3.0*(f(i)-f(i+2))**2)/12.0
	b1 = (13.0*(f(i+1)-2.0*f(i+2)+f(i+3))**2 + 3.0*(3.0*f(i+1)-4.0*f(i+2)+f(i+3))**2)/12.0
	
  a1 = 0.1/(EPS+b1)**2
  a2 = 0.6/(EPS+b2)**2
  a3 = 0.3/(EPS+b3)**2
		
  w1 = a1 / (a1+a2+a3)
  w2 = a2 / (a1+a2+a3)
  w3 = a3 / (a1+a2+a3)
  
	a1 = g(w1,1)
	a2 = g(w2,2)
	a3 = g(w3,3)
 
  w1 = a1 / (a1+a2+a3)
  w2 = a2 / (a1+a2+a3)
  w3 = a3 / (a1+a2+a3)	
  
 	fp(i) = w3*(-f(i-1)+5.0*f(i)+2.0*f(i+1))/6.0 &
 				 +w2*(2.0*f(i)+5.0*f(i+1)-f(i+2))/6.0 &
 				 +w1*(11.0*f(i+1)-7.0*f(i+2)+2.0*f(i+3))/6.0	
	
	
end do
!$OMP END PARALLEL DO
end subroutine

function g(a,k)
USE PRECISION
implicit none
real(DP) ::g,a,w
integer :: k

 if( k==1 ) then
 	 w = 1.0/10.0
 else if( k==2 )then
 	 w = 6.0/10.0
 else
 	 w = 3.0/10.0
 end if
 
 	 g = a*(w+w**2-3.0*w*a+a**2)/(w**2+a*(1.0-2.0*w))

end function

subroutine weno7(f,fp,fm,Nx)
USE PRECISION
implicit none
integer :: Nx, i
real(DP),dimension(-2:Nx+3) :: f,fp,fm
real(dp) :: b0,b1,b2,b3,a0,a1,a2,a3,w0,w1,w2,w3
real(dp) :: c0,c1,c2,c3

 c3 = 1.0/35.0
 c2 = 12.0/35.0
 c1 = 18.0/35.0
 c0 = 4.0/35.0

!$OMP PARALLEL DO PRIVATE(B0,B1,B2,B3,A0,A1,A2,A3,W0,W1,W2,W3)
do i = 1, Nx
	
 b3 = f(i-3)*(547*f(i-3)-3882*f(i-2)+4642*f(i-1)-1854*f(i)) +&
      f(i-2)*(7043*f(i-2)-17246*f(i-1)+7042*f(i)) + &
	  f(i-1)*(11003*f(i-1)-9402*f(i)) +&
	  2107*f(i)**2
	  
 b2 = f(i-2)*(267*f(i-2)-1642*f(i-1)+1602*f(i)-494*f(i+1)) +&
      f(i-1)*(2843*f(i-1)-5966*f(i)+1922*f(i+1)) + &
	  f(i)*(3443*f(i)-2522*f(i+1)) +&
	  547*f(i+1)**2
 
 b1 = f(i-1)*(547*f(i-1)-2522*f(i)+1922*f(i+1)-494*f(i+2)) +&
      f(i)*(3443*f(i)-5966*f(i+1)+1602*f(i+2)) +&
	  f(i+1)*(2843*f(i+1)-1642*f(i+2)) +&
	  267*f(i+2)**2
	  
 b0 = f(i)*(2107*f(i)-9402*f(i+1)+7042*f(i+2)-1854*f(i+3)) +&
      f(i+1)*(11003*f(i+1)-17246*f(i+2)+4642*f(i+3)) +&
	  f(i+2)*(7043*f(i+2)-3882*f(i+3)) +&
	  547*f(i+3)**2
 
 a0 = c0/(EPS+b0)**3
 a1 = c1/(EPS+b1)**3
 a2 = c2/(EPS+b2)**3
 a3 = c3/(EPS+b3)**3
 
 w0 = a0/(a0+a1+a2+a3)
 w1 = a1/(a0+a1+a2+a3)
 w2 = a2/(a0+a1+a2+a3)
 w3 = a3/(a0+a1+a2+a3)
 
 fm(i) = w3*(-1.0/4.0 *f(i-3)+13.0/12.0*f(i-2)-23.0/12.0*f(i-1)+25.0/12.0*f(i))  &
        +w2*( 1.0/12.0*f(i-2)- 5.0/12.0*f(i-1)+13.0/12.0*f(i)  + 1.0/4.0*f(i+1)) &
		+w1*(-1.0/12.0*f(i-1)+ 7.0/12.0*f(i)  + 7.0/12.0*f(i+1)-1.0/12.0*f(i+2)) &
		+w0*( 1.0/4.0 *f(i)  +13.0/12.0*f(i+1)- 5.0/12.0*f(i+2)+1.0/12.0*f(i+3))
		
end do
!$OMP END PARALLEL DO
!====================================

  i = 0

	b1 = 13.0*(f(i-2)-2.0*f(i-1)+f(i))**2 + 3.0*(f(i-2)-4*f(i-1)+3.0*f(i))**2
	b2 = 13.0*(f(i-1)-2.0*f(i)+f(i+1))**2 + 3.0*(f(i-1)-f(i+1))**2
	b3 = 13.0*(f(i)-2.0*f(i+1)+f(i+2))**2 + 3.0*(3.0*f(i)-4.0*f(i+1)+f(i+2))**2
	
	a1 = 1.0/(EPS+b1)**2
	a2 = 6.0/(EPS+b2)**2
	a3 = 3.0/(EPS+b3)**2
	
	w1 = a1/(a1+a2+a3)
	w2 = a2/(a1+a2+a3)
	w3 = a3/(a1+a2+a3)
	
	fm(i) = w1/3.0*f(i-2) - (7.0*w1+w2)/6.0*f(i-1) + (11.0*w1+5.0*w2+2.0*w3)/6.0*f(i) &
			+ (2.0*w2+5.0*w3)/6.0*f(i+1) - w3/6.0*f(i+2)
			
!====================================

!$OMP PARALLEL DO PRIVATE(B0,B1,B2,B3,A0,A1,A2,A3,W0,W1,W2,W3)
do i = 0, Nx-1
	
 b0 = f(i-2)*(547*f(i-2)-3882*f(i-1)+4642*f(i)-1854*f(i+1)) +&
      f(i-1)*(7043*f(i-1)-17246*f(i)+7042*f(i+1)) + &
	    f(i  )*(11003*f(i)-9402*f(i+1)) +&
	    2107*f(i+1)**2
	  
 b1 = f(i-1)*(267*f(i-1)-1642*f(i)+1602*f(i+1)-494*f(i+2)) +&
      f(i)*(2843*f(i)-5966*f(i+1)+1922*f(i+2)) + &
	  f(i+1)*(3443*f(i+1)-2522*f(i+2)) +&
	  547*f(i+2)**2
 
 b2 = f(i)*(547*f(i)-2522*f(i+1)+1922*f(i+2)-494*f(i+3)) +&
      f(i+1)*(3443*f(i+1)-5966*f(i+2)+1602*f(i+3)) +&
	  f(i+2)*(2843*f(i+2)-1642*f(i+3)) +&
	  267*f(i+3)**2
	  
 b3 = f(i+1)*(2107*f(i+1)-9402*f(i+2)+7042*f(i+3)-1854*f(i+4)) +&
      f(i+2)*(11003*f(i+2)-17246*f(i+3)+4642*f(i+4)) +&
	    f(i+3)*(7043*f(i+3)-3882*f(i+4)) +&
	    547*f(i+4)**2
 
 a0 = c0/(EPS+b0)**3
 a1 = c1/(EPS+b1)**3
 a2 = c2/(EPS+b2)**3
 a3 = c3/(EPS+b3)**3
 
 w0 = a0/(a0+a1+a2+a3)
 w1 = a1/(a0+a1+a2+a3)
 w2 = a2/(a0+a1+a2+a3)
 w3 = a3/(a0+a1+a2+a3)
 
 fp(i) = w3*(25.0/12.0 *f(i+1)-23.0/12.0*f(i+2)+13.0/12.0*f(i+3)-1.0/4.0*f(i+4))  &
        +w2*( 1.0/4.0*f(i)+ 13.0/12.0*f(i+1) -  5.0/12.0*f(i+2)+ 1.0/12.0*f(i+3)) &
		+w1*(-1.0/12.0*f(i-1)+ 7.0/12.0*f(i)  + 7.0/12.0*f(i+1)-1.0/12.0*f(i+2)) &
		+w0*( 1.0/12.0 *f(i-2)-5.0/12.0*f(i-1)+13.0/12.0*f(i)+1.0/4.0*f(i+1))
		
end do
!$OMP END PARALLEL DO
!========================================================

  i = Nx

	b3 = 13.0*(f(i-1)-2.0*f(i)  +f(i+1))**2 + 3.0*(f(i-1)-4.0*f(i)+3.0*f(i+1))**2
	b2 = 13.0*(f(i)  -2.0*f(i+1)+f(i+2))**2 + 3.0*(f(i)-f(i+2))**2
	b1 = 13.0*(f(i+1)-2.0*f(i+2)+f(i+3))**2 + 3.0*(3.0*f(i+1)-4.0*f(i+2)+f(i+3))**2
	
  a1 = 1.0/(EPS+b1)**2
  a2 = 6.0/(EPS+b2)**2
  a3 = 3.0/(EPS+b3)**2	
	
  w1 = a1 / (a1+a2+a3)
  w2 = a2 / (a1+a2+a3)
  w3 = a3 / (a1+a2+a3)
 	
 	fp(i) = w3*(-f(i-1)+5.0*f(i)+2.0*f(i+1))/6.0 &
 		   +w2*(2.0*f(i)+5.0*f(i+1)-f(i+2))/6.0 &
 		   +w1*(11.0*f(i+1)-7.0*f(i+2)+2.0*f(i+3))/6.0	
		   
!========================================================

end subroutine

subroutine weno_bc(f,fp,fm,Nx)
USE PRECISION
implicit none
integer :: Nx, i
real(DP),dimension(-2:Nx+3) :: f, fp, fm
real(DP) :: a1,a2,a3,b1,b2,b3,w1,w2,w3

!$OMP PARALLEL DO PRIVATE(B1,B2,B3,A1,A2,A3,W1,W2,W3)
do i = 0, Nx, Nx
	
	b1 = 13.0*(f(i-2)-2.0*f(i-1)+f(i))**2 + 3.0*(f(i-2)-4*f(i-1)+3.0*f(i))**2
	b2 = 13.0*(f(i-1)-2.0*f(i)+f(i+1))**2 + 3.0*(f(i-1)-f(i+1))**2
	b3 = 13.0*(f(i)-2.0*f(i+1)+f(i+2))**2 + 3.0*(3.0*f(i)-4.0*f(i+1)+f(i+2))**2
	
	a1 = 1.0*(1.0+abs(b3-b1)/(EPS+b1))
	a2 = 6.0*(1.0+abs(b3-b1)/(EPS+b2))
	a3 = 3.0*(1.0+abs(b3-b1)/(EPS+b3))
	
	w1 = a1/(a1+a2+a3)
	w2 = a2/(a1+a2+a3)
	w3 = a3/(a1+a2+a3)
	
	fm(i) = w1/3.0*f(i-2) - (7.0*w1+w2)/6.0*f(i-1) + (11.0*w1+5.0*w2+2.0*w3)/6.0*f(i) &
			+ (2.0*w2+5.0*w3)/6.0*f(i+1) - w3/6.0*f(i+2)
			
end do
!$OMP END PARALLEL DO
!$OMP PARALLEL DO PRIVATE(B1,B2,B3,A1,A2,A3,W1,W2,W3)	
do i = 0, Nx, Nx
	
	b3 = 13.0*(f(i-1)-2.0*f(i)  +f(i+1))**2 + 3.0*(f(i-1)-4.0*f(i)+3.0*f(i+1))**2
	b2 = 13.0*(f(i)  -2.0*f(i+1)+f(i+2))**2 + 3.0*(f(i)-f(i+2))**2
	b1 = 13.0*(f(i+1)-2.0*f(i+2)+f(i+3))**2 + 3.0*(3.0*f(i+1)-4.0*f(i+2)+f(i+3))**2
	
  a1 = 1.0 * ( 1.0 + abs(b1-b3)/(EPS+b1) )
  a2 = 6.0 * ( 1.0 + abs(b1-b3)/(EPS+b2) )
  a3 = 3.0 * ( 1.0 + abs(b1-b3)/(EPS+b3) )	
	
  w1 = a1 / ( a1+a2+a3)
  w2 = a2 / ( a1+a2+a3)
  w3 = a3 / ( a1+a2+a3)
 	
 	fp(i) = w3*(-f(i-1)+5.0*f(i)+2.0*f(i+1))/6.0 &
 				 +w2*(2.0*f(i)+5.0*f(i+1)-f(i+2))/6.0 &
 				 +w1*(11.0*f(i+1)-7.0*f(i+2)+2.0*f(i+3))/6.0	
	
	
end do
!$OMP END PARALLEL DO
end subroutine

subroutine ocrweno(f,fp,fm,Nx)
USE PRECISION
implicit none
integer :: Nx, i
real(DP),dimension(-2:Nx+3) :: f, fp, fm
real(DP) :: a1,a2,a3,b1,b2,b3,w1,w2,w3,c1,c2,c3
real(DP),dimension(1:Nx-1) :: A,B,C,S


	!c1 = 2.0
	!c2 = 5.0
	!c3 = 3.0
	
	c1 = 0.2089141306_DP
	c2 = 0.4999999998_DP
	c3 = 0.2910858692_DP

 call weno_bc(f,fp,fm,Nx)

!$OMP PARALLEL DO PRIVATE(B1,B2,B3,A1,A2,A3,W1,W2,W3)
do i = 1, Nx-1
	
	b1 = 13.0*(f(i-2)-2.0*f(i-1)+f(i))**2   + 3.0*(    f(i-2)-4*f(i-1)+3.0*f(i))**2
	b2 = 13.0*(f(i-1)-2.0*f(i)  +f(i+1))**2 + 3.0*(    f(i-1)  -f(i+1))**2
	b3 = 13.0*(f(i)  -2.0*f(i+1)+f(i+2))**2 + 3.0*(3.0*f(i)-4.0*f(i+1)+f(i+2))**2
	
	a1 = c1*(  1.0 +  abs(b3-b1) / (EPS+b1)  )
	a2 = c2*(  1.0 +  abs(b3-b1) / (EPS+b2)  )
	a3 = c3*(  1.0 +  abs(b3-b1) / (EPS+b3)  )
	
	w1 = a1/(a1+a2+a3)
	w2 = a2/(a1+a2+a3)
	w3 = a3/(a1+a2+a3)	
	
	A(i) = (2.0*w1+w2)/3.0
	B(i) = (w1+2.0*(w2+w3))/3.0
	C(i) =  w3/3.0
	S(i) = w1/6.0*f(i-1) + (5.0*(w1+w2)+w3)/6.0*f(i) + (w2+5.0*w3)/6.0*f(i+1)
	
end do
!$OMP END PARALLEL DO
  S(1) = S(1) - A(1)*fm(0)
  S(Nx-1) = S(Nx-1) - C(Nx-1)*fm(Nx)
  
  
  call solve_tridiagonal(A,B,C,S,fm(1:Nx-1),1,Nx-1)
!$OMP PARALLEL DO PRIVATE(B1,B2,B3,A1,A2,A3,W1,W2,W3)  
do i = 1, Nx-1
	
	b3 = 13.0*(f(i-1)-2.0*f(i)  +f(i+1))**2 + 3.0*(f(i-1)-4.0*f(i)+3.0*f(i+1))**2
	b2 = 13.0*(f(i)  -2.0*f(i+1)+f(i+2))**2 + 3.0*(f(i)-f(i+2))**2
	b1 = 13.0*(f(i+1)-2.0*f(i+2)+f(i+3))**2 + 3.0*(3.0*f(i+1)-4.0*f(i+2)+f(i+3))**2
	
	a1 = c1*(1.0+abs(b3-b1)/(EPS+b1))
	a2 = c2*(1.0+abs(b3-b1)/(EPS+b2))
	a3 = c3*(1.0+abs(b3-b1)/(EPS+b3))
	
	w1 = a1/(a1+a2+a3)
	w2 = a2/(a1+a2+a3)
	w3 = a3/(a1+a2+a3)	
	
	A(i) = (w3)/3.0
	B(i) = (w1+2.0*(w2+w3))/3.0
	C(i) = (w2+2.0*w1)/3.0
	S(i) = (5.0*w3+w2)/6.0*f(i) + (w3+5.0*(w2+w1))/6.0*f(i+1) + w1/6.0*f(i+2)
	
end do
!$OMP END PARALLEL DO
  S(1) = S(1) - A(1)*fp(0)
  S(Nx-1) = S(Nx-1) - C(Nx-1)*fp(Nx)
  
  call solve_tridiagonal(A,B,C,S,fp(1:Nx-1),1,Nx-1)
  
end subroutine

subroutine ocrweno_LD(f,fp,fm,Nx)
implicit none
integer :: Nx, i, btn
real(DP),dimension(-2:Nx+3) :: f, fp, fm
real(DP) :: a1,a2,a3,a4,b1,b2,b3,b4,w1,w2,w3,w4,c1,c2,c3,c4
real(DP),dimension(1:Nx-1) :: A,B,C,S

	  call weno_bc(f,fp,fm,Nx)
	
	!  0 -- FP
	!  1 -- FM

    c4 = -0.02209625116_dp
	  c3 = 0.2779037488_dp
	  c2 = 0.5220962512_dp
	  c1 = 0.2220962512_dp
		
	  ! right to left
	  
	  !$OMP PARALLEL DO PRIVATE(B1,B2,B3,B4,A1,A2,A3,A4,W1,W2,W3,W4)  
	  do i = 1, Nx-1
			b4 = 13.0*(f(i-2)-2.0*f(i-1)+f(i)  )**2 + 3.0*(-3.0*f(i-2)+8.0*f(i-1)-5.0*f(i))**2
			b3 = 13.0*(f(i-1)-2.0*f(i)  +f(i+1))**2 + 3.0*(f(i-1)-4.0*f(i)+3.0*f(i+1))**2
			b2 = 13.0*(f(i)  -2.0*f(i+1)+f(i+2))**2 + 3.0*(f(i)-f(i+2))**2
			b1 = 13.0*(f(i+1)-2.0*f(i+2)+f(i+3))**2 + 3.0*(3.0*f(i+1)-4.0*f(i+2)+f(i+3))**2
			b4 = max(b3,b4)
	
			a1 = c1*(  1.0 +  abs(b4-b1) / (EPS+b1)  )
			a2 = c2*(  1.0 +  abs(b4-b1) / (EPS+b2)  )
			a3 = c3*(  1.0 +  abs(b4-b1) / (EPS+b3)  )
			a4 = c4*(  1.0 +  abs(b4-b1) / (EPS+b4)  )
	
			w1 = a1/(a1+a2+a3+a4)
			w2 = a2/(a1+a2+a3+a4)
			w3 = a3/(a1+a2+a3+a4)	
			w4 = a4/(a1+a2+a3+a4)
	
			A(i) = (w3+2.0*w4)/3.0 !0.966
			B(i) = (w1+2.0*(w2+w3)+w4)/3.0
			C(i) = (w2+2.0*w1)/3.0 !0.233
			S(i) = w4/6.0*f(i-1) + (5.0*(w3+w4)+w2)/6.0*f(i) + (w3+5.0*(w2+w1))/6.0*f(i+1) + w1/6.0*f(i+2)
	
	  end do
	  !$OMP END PARALLEL DO
  	S(1) = S(1) - A(1)*fp(0)
  	S(Nx-1) = S(Nx-1) - C(Nx-1)*fp(Nx)  
  	call solve_tridiagonal(A,B,C,S,fp(1:Nx-1),1,Nx-1)
	
	  ! left to right
	  
	  
	  !$OMP PARALLEL DO PRIVATE(B1,B2,B3,B4,A1,A2,A3,A4,W1,W2,W3,W4)
	  do i = 1, Nx-1
	
			b1 = 13.0*(f(i-2)-2.0*f(i-1)+f(i))**2   + 3.0*(    f(i-2)-4*f(i-1)+3.0*f(i))**2
			b2 = 13.0*(f(i-1)-2.0*f(i)  +f(i+1))**2 + 3.0*(    f(i-1)  -f(i+1))**2
			b3 = 13.0*(f(i)  -2.0*f(i+1)+f(i+2))**2 + 3.0*(3.0*f(i)-4.0*f(i+1)+f(i+2))**2
			b4 = 13.0*(f(i+1)-2.0*f(i+2)+f(i+3))**2 + 3.0*(-5.0*f(i+1)+8.0*f(i+2)-3.0*f(i+3))**2
			b4 = max(b3,b4)
	
			a1 = c1*(  1.0 +  abs(b4-b1) / (EPS+b1)  )
			a2 = c2*(  1.0 +  abs(b4-b1) / (EPS+b2)  )
			a3 = c3*(  1.0 +  abs(b4-b1) / (EPS+b3)  )
			a4 = c4*(  1.0 +  abs(b4-b1) / (EPS+b4)  )
	
			w1 = a1/(a1+a2+a3+a4)
			w2 = a2/(a1+a2+a3+a4)
			w3 = a3/(a1+a2+a3+a4)	
			w4 = a4/(a1+a2+a3+a4)
	
			A(i) = (2.0*w1+w2)/3.0 !0.966
			B(i) = (w1+2.0*(w2+w3)+w4)/3.0 !1.8 
			C(i) =  (w3+2.0*w4)/3.0 !0.23
			S(i) =   w1/6.0*f(i-1) + (5.0*(w1+w2)+w3)/6.0*f(i) + (w2+5.0*(w3+w4))/6.0*f(i+1) + w4/6.0*f(i+2)
	
	  end do
	  !$OMP END PARALLEL DO
 	  S(1) = S(1) - A(1)*fm(0)
  	S(Nx-1) = S(Nx-1) - C(Nx-1)*fm(Nx)
  	call solve_tridiagonal(A,B,C,S,fm(1:Nx-1),1,Nx-1)
	
  
  
end subroutine