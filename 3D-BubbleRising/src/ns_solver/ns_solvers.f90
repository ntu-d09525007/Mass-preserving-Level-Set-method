subroutine ns_solver
use all
implicit none
integer(8) :: cpustart, cpuend

call system_clock(cpustart)

call ns_init

call ns_ab_solver

call ns_check_convergence_div

call p%node_vel
call pt%nvel%sync

call system_clock(cpuend)
p%glb%ns = p%glb%ns + real(cpuend-cpustart,kind=8)/real(p%glb%cpurate,kind=8)
    
end subroutine

subroutine ns_init
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k

call p%rho_mu
call p%curv
call pt%normals%sync
    
end subroutine

subroutine ns_linearize
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k

!$omp parallel do private(i,j,k)
do id = 0, p%glb%threads-1
    
    !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k)
    do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc
    do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
    do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
        p%of(id)%loc%vel%x%tmp(i,j,k) = p%of(id)%loc%vel%x%now(i,j,k) 
        p%of(id)%loc%vel%y%tmp(i,j,k) = p%of(id)%loc%vel%y%now(i,j,k) 
        p%of(id)%loc%vel%z%tmp(i,j,k) = p%of(id)%loc%vel%z%now(i,j,k) 
    end do
    end do
    end do
    !$omp end parallel do
  
enddo       
!$omp end parallel do
        
end subroutine

subroutine ns_relaxation
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k

!$omp parallel do private(i,j,k)
do id = 0, p%glb%threads-1
    
    !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k)
    do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc
    do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
    do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
        p%of(id)%loc%vel%x%now(i,j,k) = 0.5d0 * ( p%of(id)%loc%vel%x%now(i,j,k) + p%of(id)%loc%vel%x%tmp(i,j,k) )
        p%of(id)%loc%vel%y%now(i,j,k) = 0.5d0 * ( p%of(id)%loc%vel%y%now(i,j,k) + p%of(id)%loc%vel%y%tmp(i,j,k) )
        p%of(id)%loc%vel%z%now(i,j,k) = 0.5d0 * ( p%of(id)%loc%vel%z%now(i,j,k) + p%of(id)%loc%vel%z%tmp(i,j,k) )
    end do
    end do
    end do
    !$omp end parallel do
  
enddo       
!$omp end parallel do
        
end subroutine

subroutine ns_check_convergence_div
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k
real(8) :: div, sumdiv
real(8) :: ux,vy,wz

div=0.0d0
sumdiv=0.0d0

!$omp parallel do private(i,j,k,ux,vy,wz), reduction(max:div), reduction(+:sumdiv)
do id = 0, p%glb%threads-1
    
    !$omp parallel do num_threads(p%glb%nthreads) collapse(3) &
    !$omp& private(i,j,k,ux,vy,wz), reduction(max:div), reduction(+:sumdiv)
    do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    do j = p%of(id)%loc%js, p%of(id)%loc%je
    do i = p%of(id)%loc%is, p%of(id)%loc%ie
    
        ux = (p%of(id)%loc%vel%x%now(i,j,k)-p%of(id)%loc%vel%x%now(i-1,j,k))/p%glb%dx
        vy = (p%of(id)%loc%vel%y%now(i,j,k)-p%of(id)%loc%vel%y%now(i,j-1,k))/p%glb%dy
        wz = (p%of(id)%loc%vel%z%now(i,j,k)-p%of(id)%loc%vel%z%now(i,j,k-1))/p%glb%dz 
            
        div = max( div, abs(ux+vy+wz) ) 
        sumdiv = sumdiv + abs(ux+vy+wz)
                                        
    end do
    end do
    end do
    !$omp end parallel do
  
enddo   
!$omp end parallel do
  
p%glb%vel_sdiv = sumdiv / (p%glb%node_x*p%glb%node_y*p%glb%node_z)
p%glb%vel_div = div
    
end subroutine

subroutine ns_check_convergence_vel
use all
!$ use omp_lib
implicit none
integer :: id,i,j,k
real(8) :: linf, l2f

linf=0.0d0
l2f=0.0d0

!$omp parallel do private(i,j,k), reduction(max:linf), reduction(+:l2f)
do id = 0, p%glb%threads-1
    
    !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k), reduction(max:linf), reduction(+:l2f)
    do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    do j = p%of(id)%loc%js, p%of(id)%loc%je
    do i = p%of(id)%loc%is, p%of(id)%loc%ie
    
        linf = max(linf,abs(p%of(id)%loc%vel%x%now(i,j,k)-p%of(id)%loc%vel%x%tmp(i,j,k)))
        linf = max(linf,abs(p%of(id)%loc%vel%y%now(i,j,k)-p%of(id)%loc%vel%y%tmp(i,j,k)))
        linf = max(linf,abs(p%of(id)%loc%vel%z%now(i,j,k)-p%of(id)%loc%vel%z%tmp(i,j,k)))
        
        l2f = l2f + (p%of(id)%loc%vel%x%now(i,j,k)-p%of(id)%loc%vel%x%tmp(i,j,k))**2.0d0
        l2f = l2f + (p%of(id)%loc%vel%y%now(i,j,k)-p%of(id)%loc%vel%y%tmp(i,j,k))**2.0d0
        l2f = l2f + (p%of(id)%loc%vel%z%now(i,j,k)-p%of(id)%loc%vel%z%tmp(i,j,k))**2.0d0
                                        
    end do
    end do
    end do
    !$omp end parallel do
  
enddo   
!$omp end parallel do

l2f = dsqrt( l2f / (3.0d0*p%glb%node_x*p%glb%node_y*p%glb%node_z) )

p%glb%ns_linf = linf
p%glb%ns_l2f = l2f
    
end subroutine


