subroutine mass_preserving_level_set
use all
!$ use omp_lib
implicit none
integer :: i,j,k,id,iter
real(8) :: lam, plam

id=0

do iter = 1, 20
    
    call ls_mv()
    call surface_norms2()
    
    lam = 0.0_8
    
    !$omp parallel do collapse(3), private(plam), reduction(+:lam)   
    do k = p%loc%ks, p%loc%ke
    do j = p%loc%js, p%loc%je
    do i = p%loc%is, p%loc%ie
    
        p%loc%grad%now(i,j,k) = dsqrt( p%loc%normals%x%now(i,j,k)**2.0_8 + &
                                     & p%loc%normals%y%now(i,j,k)**2.0_8 + &
                                     & p%loc%normals%z%now(i,j,k)**2.0_8 )
        
        plam =  p%loc%delta%now(i,j,k)**2.0_8 * p%loc%grad%now(i,j,k) 
        plam = plam * ( 2.0_8*(1.0_8-p%glb%rho_12)*p%loc%heavy%now(i,j,k) + p%glb%rho_12 )*p%glb%dx*p%glb%dy*p%glb%dz
        
        lam = lam + plam
        
    end do
    end do
    end do 
    !$omp end parallel do
    
    lam = ( p%glb%imass - p%glb%mass ) / lam 

    !$omp parallel do collapse(3)     
    do k = p%loc%ks, p%loc%ke
    do j = p%loc%js, p%loc%je
    do i = p%loc%is, p%loc%ie
    
        p%loc%phi%now(i,j,k) = p%loc%phi%now(i,j,k) + lam * p%loc%delta%now(i,j,k) * p%loc%grad%now(i,j,k)
        
    end do
    end do
    end do
    !$omp end parallel do

    call bc(p%loc%phi%now)
    call ls_mv()
    if( abs(p%glb%imass-p%glb%mass)/p%glb%imass < 1.0d-12 )exit
       
end do  

end subroutine

