subroutine interface_solver()
use all
implicit none

if(p%glb%method==1)then
    call classical_ls()
else if (p%glb%method==2)then
    call mpls()
else if (p%glb%method==3)then
    call clsvof()
endif

end subroutine 

subroutine classical_ls()
use all
implicit none

!call level_set_symplectic_solver 
call level_set_rk3_solver
if(mod(p%glb%iter,5).eq.0)call level_set_rk3_redis(1)

end subroutine

subroutine mpls()
use all
implicit none

!call level_set_symplectic_solver 
call level_set_rk3_solver
 if(mod(p%glb%iter,5).eq.0)call level_set_rk3_redis(1)
call mass_preserving_level_set

end subroutine

subroutine clsvof()
use all
implicit none

call vof_wlic_solver
!call level_set_symplectic_solver 
call level_set_rk3_solver

if(mod(p%glb%iter,20).eq.0)then
    call clsvof_recon
else if(mod(p%glb%iter,5).eq.0)then
    call level_set_rk3_redis(1)
endif

end subroutine
