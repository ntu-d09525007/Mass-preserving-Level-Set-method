module all
use ptr_tree
use tree
implicit none
save
type(manager) :: p 
type(ptr_family) :: pt
end module all

program main
use all
implicit none

call problem_init

do 
    
    if( p%glb%time + p%glb%dt > p%glb%t2s )then
        p%glb%dt = p%glb%t2s - p%glb%time + 1.0d-10
    endif
    
    p%glb%time = p%glb%time + p%glb%dt 
    p%glb%iter = p%glb%iter + 1 
    call p%sync 
  
    call deforming_field_velocity
    call p%switch
 
    call interface_solver
    !call ns_solver
    call plot

    call p%ls_mv
    if( mod(p%glb%iter,5)==0 )then
        write(*,*)"========================================="
        write(*,'(A,",",I8,",",F15.6)')trim(p%glb%name),p%glb%iter,p%glb%time
        write(*,*)''
        !call print_NS_info
        call print_LS_info
        call print_CPU_info
        !call find_momentum
        write(*,*)"========================================="
    endif
    call output 

    if( p%glb%time + 1.0d-10 > p%glb%t2s ) exit
    
enddo

    call calculate_interface_loss

contains

include 'included.f90'

end program main
