subroutine ns_ab_diff_source
use all
implicit none

call ns_ab_diff_sec
!call ns_ab_diff_uccd
    
end subroutine

subroutine ns_ab_adv_source
implicit none

call ns_ab_adv_source_sec
!call ns_ab_adv_source_quick
!call ns_ab_adv_source_uccd
    
end subroutine

subroutine ns_ab_setup
use all
implicit none
integer :: id,i,j,k

call ns_ab_diff_source
call ns_ab_adv_source

!$omp parallel do collapse(3)
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie

    p%loc%velsrc%x%now(i,j,k) = p%loc%velsrc%x%now(i,j,k) + p%loc%velsrc%x%tmp(i,j,k)
    p%loc%velsrc%y%now(i,j,k) = p%loc%velsrc%y%now(i,j,k) + p%loc%velsrc%y%tmp(i,j,k)
    p%loc%velsrc%z%now(i,j,k) = p%loc%velsrc%z%now(i,j,k) + p%loc%velsrc%z%tmp(i,j,k)

end do    
end do
end do
!$omp end parallel do

end subroutine

subroutine ns_ab_solver_SOR
use all
implicit none
integer :: iter,initer
integer :: id,i,j,k
real(8) :: tol

call ppe_sor_init
    
iter=0
p%glb%piter=0
    
do 
    
    iter=iter+1 
    
    call ns_linearize
    call ns_ab_setup
    call ns_ab_predictor
    call ppe_sor_solver(p%glb%p_tol)

    if(iter>0)exit
        
end do

call ns_check_convergence_vel

end subroutine

subroutine ns_ab_predictor
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k
real(8) :: src

!$omp parallel do collapse(3), private(src)      
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie
        
    src = 1.50*p%loc%velsrc%x%now(i,j,k) - 0.5d0*p%loc%velsrc%x%old(i,j,k)             
    p%loc%vel%x%now(i,j,k) = p%loc%vel%x%old(i,j,k) + p%glb%dt * src
    
    src = 1.50*p%loc%velsrc%y%now(i,j,k) - 0.5d0*p%loc%velsrc%y%old(i,j,k)   
    p%loc%vel%y%now(i,j,k) = p%loc%vel%y%old(i,j,k) + p%glb%dt * src
        
    src = 1.50*p%loc%velsrc%z%now(i,j,k) - 0.5d0*p%loc%velsrc%z%old(i,j,k)    
    p%loc%vel%z%now(i,j,k) = p%loc%vel%z%old(i,j,k) + p%glb%dt * src
        
end do
end do 
end do
!$omp end parallel do
    
call velbc(p%loc%vel%x%now,p%loc%vel%y%now,p%loc%vel%z%now)
    
end subroutine

