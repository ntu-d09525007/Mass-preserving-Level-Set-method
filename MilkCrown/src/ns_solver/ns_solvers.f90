subroutine ns_solver
use all
implicit none
integer(8) :: cpustart, cpuend

    call system_clock(cpustart)
    
    call ns_init
    
    call ns_ab_solver_SOR
    !call ns_split_solver
    
    call ns_check_convergence_div
    call node_vel
    
    call calculate_energy()
    
    call system_clock(cpuend)
    p%glb%ns = p%glb%ns + real(cpuend-cpustart,kind=8)/real(p%glb%cpurate,kind=8)
    
end subroutine

subroutine ns_init
use all
implicit none

call rho_mu
if( p%glb%btn_sf > 0 )then
    call curv
endif
    
end subroutine

subroutine ns_linearize
use all
!$ use omp_lib
implicit none
integer :: i,j,k

!$omp parallel do collapse(3)  
do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
    p%loc%vel%x%tmp(i,j,k) = p%loc%vel%x%now(i,j,k) 
    p%loc%vel%y%tmp(i,j,k) = p%loc%vel%y%now(i,j,k) 
    p%loc%vel%z%tmp(i,j,k) = p%loc%vel%z%now(i,j,k) 
end do
end do
end do   
!$omp end parallel do

   
call find_stag_vel(  p%loc%tdata%x%s1, p%loc%tdata%y%s1, p%loc%tdata%z%s1, &
                    &p%loc%tdata%x%s2, p%loc%tdata%y%s2, p%loc%tdata%z%s2, &
                    &p%loc%vel%x%tmp, p%loc%vel%y%tmp, p%loc%vel%z%tmp )


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

!$omp parallel do collapse(3), private(ux,vy,wz), reduction(max:div), reduction(+:sumdiv) 
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie

    ux = (p%loc%vel%x%now(i,j,k)-p%loc%vel%x%now(i-1,j,k))/p%glb%dx
    vy = (p%loc%vel%y%now(i,j,k)-p%loc%vel%y%now(i,j-1,k))/p%glb%dy
    wz = (p%loc%vel%z%now(i,j,k)-p%loc%vel%z%now(i,j,k-1))/p%glb%dz 
        
    div = max( div, abs(ux+vy+wz) ) 
    sumdiv = sumdiv + abs(ux+vy+wz)
                                    
end do
end do
end do
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
id=0

!$omp parallel do collapse(3), reduction(max:linf), reduction(+:l2f)
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie

    linf = max(linf,abs(p%loc%vel%x%now(i,j,k)-p%loc%vel%x%tmp(i,j,k)))
    linf = max(linf,abs(p%loc%vel%y%now(i,j,k)-p%loc%vel%y%tmp(i,j,k)))
    linf = max(linf,abs(p%loc%vel%z%now(i,j,k)-p%loc%vel%z%tmp(i,j,k)))
    
    l2f = l2f + (p%loc%vel%x%now(i,j,k)-p%loc%vel%x%tmp(i,j,k))**2.0d0
    l2f = l2f + (p%loc%vel%y%now(i,j,k)-p%loc%vel%y%tmp(i,j,k))**2.0d0
    l2f = l2f + (p%loc%vel%z%now(i,j,k)-p%loc%vel%z%tmp(i,j,k))**2.0d0
                                    
end do
end do
end do
!$omp end parallel do

l2f = dsqrt( l2f / (3.0d0*p%glb%node_x*p%glb%node_y*p%glb%node_z) )

p%glb%ns_linf = linf
p%glb%ns_l2f = l2f
    
end subroutine

subroutine node_vel()
use all
implicit none
integer :: id,i,j,k
  
    !$omp parallel do collapse(3)
    do k = p%loc%ks, p%loc%ke
    do j = p%loc%js, p%loc%je
    do i = p%loc%is, p%loc%ie
        p%loc%nvel%x%now(i,j,k) = 0.5d0 * ( p%loc%vel%x%now(i-1,j,k) + p%loc%vel%x%now(i,j,k) )
        p%loc%nvel%y%now(i,j,k) = 0.5d0 * ( p%loc%vel%y%now(i,j-1,k) + p%loc%vel%y%now(i,j,k) )
        p%loc%nvel%z%now(i,j,k) = 0.5d0 * ( p%loc%vel%z%now(i,j,k-1) + p%loc%vel%z%now(i,j,k) )
    end do
    end do
    end do
    !$omp end parallel do

    call nvelbc(p%loc%nvel%x%now,p%loc%nvel%y%now,p%loc%nvel%z%now)

end subroutine


! subroutine ibm_bc()
! use all
! implicit none
! integer :: i,j,k,ii,jj,kk,ug
! real(8) :: solid,x,y,z

! p%loc%ibm%z = p%loc%ibm%z + p%loc%ibm%w*p%glb%dt

! ug=30
! !$omp parallel do collapse(3), private(ii,jj,kk,x,y,z)
! do k = p%loc%ks, p%loc%ke
! do j = p%loc%js, p%loc%je
! do i = p%loc%is, p%loc%ie

!     p%loc%ibm%solid%now(i,j,k)=0.0d0

!     do ii = 1, ug
!     do jj = 1, ug
!     do kk = 1, ug
        
!         x = 0.5d0*( p%glb%x(i,j,k)+p%glb%x(i-1,j,k) ) + real(ii,8)*p%glb%dx/real(ug,8)
!         y = 0.5d0*( p%glb%y(i,j,k)+p%glb%y(i,j-1,k) ) + real(jj,8)*p%glb%dy/real(ug,8)
!         z = 0.5d0*( p%glb%z(i,j,k)+p%glb%z(i,j,k-1) ) + real(kk,8)*p%glb%dz/real(ug,8)
        
!         ! dambreak -- rising gate
!         !========================================
!         if( x>5.0d0/3.0d0 .and. x<5.0d0/3.0d0+2.0d0*p%glb%dx .and. z>p%loc%ibm%z )then
!             p%loc%ibm%solid%now(i,j,k) = p%loc%ibm%solid%now(i,j,k) + 1.0d0/real(ug,8)**3.0d0
!         endif

!     end do
!     end do
!     end do

! enddo
! enddo
! enddo
! !$omp end parallel do

! 110 call bc(p%loc%ibm%solid%now)

! !$omp parallel do collapse(3), private(solid)
! do k = p%loc%ks, p%loc%ke
! do j = p%loc%js, p%loc%je
! do i = p%loc%is, p%loc%ie

!     solid = 0.5d0*( p%loc%ibm%solid%now(i,j,k)+p%loc%ibm%solid%now(i+1,j,k) )
!     p%loc%vel%x%now(i,j,k) = (1.0-solid)*p%loc%vel%x%now(i,j,k)

!     solid = 0.5d0*( p%loc%ibm%solid%now(i,j,k)+p%loc%ibm%solid%now(i,j+1,k) )
!     p%loc%vel%y%now(i,j,k) = (1.0-solid)*p%loc%vel%y%now(i,j,k)

!     solid = 0.5d0*( p%loc%ibm%solid%now(i,j,k)+p%loc%ibm%solid%now(i,j,k+1) )
!     p%loc%vel%z%now(i,j,k) = (1.0-solid)*p%loc%vel%z%now(i,j,k) + solid*p%loc%ibm%w

! enddo
! enddo
! enddo
! !$omp end parallel do

! call velbc(p%loc%vel%x%now,p%loc%vel%y%now,p%loc%vel%z%now)

! end subroutine
