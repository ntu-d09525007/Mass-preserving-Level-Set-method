 subroutine solve_tridiagonal(A,B,C,S,X,M,N)
!cccccccccccccccccccccccccccccccccc
!
! A, first  coefficient matrix
! B, second coefficient matrix
! C, third  coefficient matrix
! S, sorce matrix
! X, solution
! [M,N], index domain
!
!cccccccccccccccccccccccccccccccccc
implicit none
INTEGER,PARAMETER :: DP = SELECTED_REAL_KIND(p=15)
integer :: M, N, i
real(kind=dp),dimension(m:n) :: A,B,C,S,X
 
 C(M) = C(M)/ B(M)
 S(M) = S(M)/ B(M)
 
 do i = M+1, N, 1
	 
	 C(i) = C(i) / ( B(I)-A(I)*C(I-1) )
	 S(I) = (S(I) - A(i)*S(I-1))/(B(i)-A(I)*C(i-1))

 end do
 
 X(N) = S(N)
 
 do i = N-1, M, -1
	 
	 X(i) = S(i) - C(i)*X(i+1)	 
	 
 end do


end subroutine

subroutine Solve_periodic_tridiagonal(A,B,C,S,X,M,N)
!ccccccccccccccccccccccccccc
!
! Solve it with Sherman-Morrison formula
!
!ccccccccccccccccccccccccccc
implicit none
INTEGER,PARAMETER :: DP = SELECTED_REAL_KIND(p=15)
integer :: M, N, i
real(kind=dp),dimension(m:n) :: A,B,C,S,X
real(kind=dp),dimension(m:n) :: AA,BB,CC
real(kind=dp),dimension(m:n) :: U,V,Y,Q
real(kind=dp) :: tmp1, tmp2

 AA = A
 BB = B
 CC = C
 
 U = 0.0
 V = 0.0
 
 !-------------------------
  BB(M) = 2.0*B(M)
  BB(N) = B(N) + A(M)*C(N)/B(M)
 !-------------------------
  U(M) = -B(M)
  U(N) = C(N)
 !-------------------------
  V(M) = 1.0
  V(N) = -A(M)/B(M)
 !-------------------------
 
 call solve_tridiagonal(AA,BB,CC,S,Y,M,N)
 call solve_tridiagonal(AA,BB,CC,U,Q,M,N)

 X = Y - sum(V*Y)/(1.0+sum(V*Q)) * Q
 
 
 end subroutine