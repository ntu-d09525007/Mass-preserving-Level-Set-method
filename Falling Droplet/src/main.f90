module all
use tree
implicit none
save
type(manager) :: p 
end module all

program main
use all
implicit none
real(8) :: dt

call problem_init

do 
    
    p%glb%time = p%glb%time + p%glb%dt 
    p%glb%iter = p%glb%iter + 1 
  
    call p%switch
 
    call interface_solver
    call ns_solver
    call plot

    call ls_mv
    if( mod(p%glb%iter,5)==0 )then
        write(*,*)"========================================="
        write(*,'(A,",",I8,",",F15.6)')trim(p%glb%name),p%glb%iter,p%glb%time
        write(*,*)''
        call print_NS_info
        call print_LS_info
        call print_CPU_info
        call print_energy_info
        call find_momentum
        write(*,*)"========================================="
    endif
    call output

if( p%glb%time > p%glb%t2s ) exit

end do
 
contains

include 'included.f90'

end program main
