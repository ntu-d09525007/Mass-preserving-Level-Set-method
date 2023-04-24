subroutine mass_preserving_level_set
use all
!$ use omp_lib
implicit none
integer :: i,j,k,id,iter
real(8) :: lam, plam

do iter = 1, 20
    
    call p%surface_norms2
    call pt%normals%sync
    call p%ls_mv()
    
    lam = 0.0_8
    
    !$omp parallel do private(i,j,k,plam), reduction(+:lam)
    do id = 0, p%glb%threads-1
        
        !$omp parallel do num_threads(p%glb%nthreads) collapse(3) &
        !$omp& private(i,j,k,plam), reduction(+:lam)
        do k = p%of(id)%loc%ks, p%of(id)%loc%ke
        do j = p%of(id)%loc%js, p%of(id)%loc%je
        do i = p%of(id)%loc%is, p%of(id)%loc%ie
        
            p%of(id)%loc%grad%now(i,j,k) = dsqrt( p%of(id)%loc%normals%x%now(i,j,k)**2.0_8 + &
                                                & p%of(id)%loc%normals%y%now(i,j,k)**2.0_8 + &
                                                & p%of(id)%loc%normals%z%now(i,j,k)**2.0_8 )
            
            plam =  p%of(id)%loc%delta%now(i,j,k)**2.0_8 * p%of(id)%loc%grad%now(i,j,k) 
            
            if( p%glb%inverse )then
                plam = plam * ( 2.0d0*(p%glb%rho_12-1.0d0)*p%of(id)%loc%heavy%now(i,j,k) + 1.0d0 )*p%glb%dx*p%glb%dy*p%glb%dz
            else
                plam = plam * ( 2.0d0*(1.0d0-p%glb%rho_12)*p%of(id)%loc%heavy%now(i,j,k) + p%glb%rho_12 )*p%glb%dx*p%glb%dy*p%glb%dz
            endif

            lam = lam + plam
            
        end do
        end do
        end do
        !$omp end parallel do
    
    enddo   
    !$omp end parallel do
    
    lam = ( p%glb%imass - p%glb%mass ) / lam

    !$omp parallel do private(i,j,k)
    do id = 0, p%glb%threads-1

        !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k)
        do k = p%of(id)%loc%ks, p%of(id)%loc%ke
        do j = p%of(id)%loc%js, p%of(id)%loc%je
        do i = p%of(id)%loc%is, p%of(id)%loc%ie
        
            p%of(id)%loc%phi%now(i,j,k) = p%of(id)%loc%phi%now(i,j,k) + lam * p%of(id)%loc%delta%now(i,j,k) * p%of(id)%loc%grad%now(i,j,k)
            
        end do
        end do
        end do
        !$omp end parallel do
        
        call p%of(id)%bc(0,p%of(id)%loc%phi%now)
    
    enddo   
    !$omp end parallel do
    
    call pt%phi%sync

    if( abs(p%glb%imass - p%glb%mass)/p%glb%imass < 1.0d-10 )exit
    
end do  

end subroutine

