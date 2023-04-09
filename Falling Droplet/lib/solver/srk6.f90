subroutine tsolver_roots_solve_srk6(p,err)
implicit none
class(tsolver_roots) :: p
integer :: i,j,k
real(8),intent(out) :: err
real(8) :: coef1_1, coef1_2, coef1_3
real(8) :: coef2_1, coef2_2, coef2_3
real(8) :: coef3_1, coef3_2, coef3_3
real(8) :: cc

cc = dsqrt(0.6_8)*0.5_8

coef1_1 = 5.0_8/36.0_8; coef1_2 = 2.0_8/9.0_8 + 2.0_8*cc/3.0_8; coef1_3 = 5.0_8/36.0_8 + cc/3.0_8
coef2_1 = 5.0_8/36.0_8 - 5.0_8*cc/12.0_8; coef2_2 = 2.0_8/9.0_8; coef2_3 = 5.0_8/36.0_8 + 5.0_8*cc/12.0_8 
coef3_1 = 5.0_8/36.0_8 - cc/3.0_8; coef3_2 = 2.0_8/9.0_8 - 2.0_8*cc/3.0_8; coef3_3 = 5.0_8/36.0_8

err = 0.0_8

!$omp parallel do collapse(3)
do k = p%ks, p%ke
do j = p%js, p%je
do i = p%is, p%ie

    p%ss1(i,j,k) = p%s1(i,j,k)
    p%ss2(i,j,k) = p%s2(i,j,k)
    p%ss3(i,j,k) = p%s3(i,j,k)

    p%s1(i,j,k) = p%target(i,j,k) + p%dt*( coef1_1*p%l1(i,j,k) + coef1_2*p%l2(i,j,k) + coef1_3*p%l3(i,j,k) )
    p%s2(i,j,k) = p%target(i,j,k) + p%dt*( coef2_1*p%l1(i,j,k) + coef2_2*p%l2(i,j,k) + coef2_3*p%l3(i,j,k) )
    p%s3(i,j,k) = p%target(i,j,k) + p%dt*( coef3_1*p%l1(i,j,k) + coef3_2*p%l2(i,j,k) + coef3_3*p%l3(i,j,k) )

    p%s1(i,j,k) = p%w * p%s1(i,j,k) + (1.0_8-p%w) * p%ss1(i,j,k) 
    p%s2(i,j,k) = p%w * p%s2(i,j,k) + (1.0_8-p%w) * p%ss2(i,j,k) 
    p%s3(i,j,k) = p%w * p%s3(i,j,k) + (1.0_8-p%w) * p%ss3(i,j,k) 

enddo
enddo
enddo
!$omp end parallel do

!$omp parallel do collapse(3), reduction(max:err)
do k = p%ks+p%ghc, p%ke-p%ghc
do j = p%js+p%ghc, p%je-p%ghc
do i = p%is+p%ghc, p%ie-p%ghc

err = max( err, abs( p%s1(i,j,k)-p%ss1(i,j,k) ), abs( p%s2(i,j,k)-p%ss2(i,j,k) ), abs( p%s3(i,j,k)-p%ss3(i,j,k) ) )

enddo
enddo
enddo
!$omp end parallel do
 
 
end subroutine 

subroutine tsolver_data_solve_srk6(p,err)
implicit none
class(tsolver_data) :: p
real(8), intent(out) :: err
real(8) :: err1, err2, err3

 call p%x%solve_srk6(err1)

 if( p%is_vector_solver )then
    call p%y%solve_srk6(err2)
    call p%z%solve_srk6(err3)
    err = max( err1, err2, err3 )
 else
    err = err1
 endif

end subroutine 

subroutine tsolver_roots_final_srk6(p)
implicit none
class(tsolver_roots) :: p
integer :: i,j,k

!$omp parallel do collapse(3)
do k = p%ks, p%ke
do j = p%js, p%je
do i = p%is, p%ie
    p%target(i,j,k) = p%target(i,j,k) + p%dt/18.0_8 * ( 5.0_8*p%l1(i,j,k) + 8.0_8*p%l2(i,j,k) + 5.0_8*p%l3(i,j,k) )
enddo
enddo
enddo
!$omp end parallel do

end subroutine

subroutine tsolver_data_final_srk6(p)
implicit none
class(tsolver_data) :: p
integer :: i,j

 call p%x%final_srk6

 if( p%is_vector_solver )then
     call p%y%final_srk6
     call p%z%final_srk6
 endif
 
 p%is_vector_solver = .false.
 
 end subroutine