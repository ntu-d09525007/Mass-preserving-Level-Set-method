subroutine ns_velbc()
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k

call vel_bc

end subroutine