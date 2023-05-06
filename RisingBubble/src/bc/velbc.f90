subroutine vel_bc
use all
implicit none
integer :: id

!$omp parallel do
do id = 0, p%glb%threads-1
    
    call p%of(id)%velbc(p%of(id)%loc%vel%x%now,p%of(id)%loc%vel%y%now,p%of(id)%loc%vel%z%now)

enddo
!$omp end parallel do

call pt%vel%sync

end subroutine

subroutine nvel_bc
use all
implicit none
integer :: id

!$omp parallel do
do id = 0, p%glb%threads-1
    
    call p%of(id)%nvelbc(p%of(id)%loc%nvel%x%now,p%of(id)%loc%nvel%y%now,p%of(id)%loc%nvel%z%now)

enddo
!$omp end parallel do

call pt%nvel%sync

end subroutine