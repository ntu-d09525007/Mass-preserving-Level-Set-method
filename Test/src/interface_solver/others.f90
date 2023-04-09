subroutine ls_funs()
use all
implicit none
integer :: id, i, j, k
real(8) :: x, heavy, hp, pi, eps

eps = 1.0d-12
pi = dacos(-1.0_8)
    
!$omp parallel do collapse(3), private(x,heavy,hp)
do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc

    x = p%loc%phi%now(i,j,k) / p%glb%ls_wid
    
    if( x > 1.0_8-eps )then
        heavy = 1.0_8
        hp = 0.0_8
    else if ( x < -1.0_8+eps )then
        heavy = 0.0_8
        hp = 0.0_8
    else
        heavy = 0.5_8 * (1.0_8 + x + dsin(pi*x) / pi )
        hp = 0.5_8 * ( 1.0_8 + dcos(pi*x) ) / p%glb%ls_wid 
    endif
    
    heavy = max(min(heavy,1.0d0),0.0d0) 
    
    p%loc%heavy%now(i,j,k) = heavy
    p%loc%delta%now(i,j,k) = hp
    p%loc%sign%now(i,j,k) = 2.0_8*heavy-1.0_8
    
end do
end do
end do
!$omp end parallel  do

end subroutine

subroutine rho_mu()
use all
implicit none
integer :: id,i,j,k
real(8) :: heavy

call ls_funs

!$omp parallel do collapse(3), private(heavy)
do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc        
do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
    
    heavy = p%loc%vof%now(i,j,k)
    if( p%glb%method .ne. 3)heavy = p%loc%heavy%now(i,j,k)
    
    p%loc%rho%now(i,j,k) = heavy + p%glb%rho_12 * (1.0_8 - heavy )
    p%loc%mu%now(i,j,k)  = heavy + p%glb%mu_12  * (1.0_8 - heavy )
    
end do
end do
end do  
!$omp end parallel do
    
end subroutine

subroutine ls_mv
use all
implicit none
integer :: id, i, j, k
real(8) :: mass, vol, rho
real(8) :: dv

dv = p%glb%dx * p%glb%dy * p%glb%dz

call rho_mu

!===========================  LS 

mass = 0.0_8; vol=0.0_8

!$omp parallel do collapse(3), private(rho), reduction(+:mass,vol)    
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie
    
    rho = p%loc%heavy%now(i,j,k) + p%glb%rho_12 * (1.0d0 - p%loc%heavy%now(i,j,k))
    mass = mass + rho*p%loc%heavy%now(i,j,k)*dv
    vol = vol + p%loc%heavy%now(i,j,k)*dv

enddo
enddo
enddo
!$omp end parallel do

p%glb%mass = mass
p%glb%vol = vol

!===========================  VOF 

mass = 0.0_8; vol=0.0_8

!$omp parallel do collapse(3), private(rho), reduction(+:mass,vol)     
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie
    
    rho = p%loc%vof%now(i,j,k) + p%glb%rho_12 * (1.0d0-p%loc%vof%now(i,j,k))
    mass = mass + rho*p%loc%vof%now(i,j,k)*dv
    vol = vol + p%loc%vof%now(i,j,k)*dv

enddo
enddo
enddo
!$omp end parallel do

p%glb%massv = mass
p%glb%volv = vol


end subroutine