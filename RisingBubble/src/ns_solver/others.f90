subroutine ls_funs()
use all
implicit none
integer :: id, i, j, k
real(8) :: x, heavy, hp, pi, eps

eps = 1.0d-12
pi = dacos(-1.0_8)
    
!$omp parallel do private(i,j,k,x,heavy,hp)
do id = 0, p%glb%threads-1

    !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k,x,heavy,hp)
    do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc
    do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
    do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
    
        x = p%of(id)%loc%phi%now(i,j,k) / p%glb%ls_wid
        
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
        
        p%of(id)%loc%heavy%now(i,j,k) = heavy
        p%of(id)%loc%delta%now(i,j,k) = hp
        p%of(id)%loc%sign%now(i,j,k) = 2.0_8*heavy-1.0_8
        
    end do
    end do
    end do
    !$omp end parallel do

enddo
!$omp end parallel  do

end subroutine

subroutine rho_mu
use all
implicit none
integer :: id,i,j,k
real(8) :: heavy

call ls_funs

!$omp parallel do private(i,j,k,heavy)
do id = 0, p%glb%threads-1
    
    !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k,heavy)
    do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc        
    do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
    do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
        
        heavy = p%of(id)%loc%vof%now(i,j,k)
        if( p%glb%method .ne. 3)heavy = p%of(id)%loc%heavy%now(i,j,k)

        if( .not. p%glb%inverse ) then
            p%of(id)%loc%rho%now(i,j,k) = heavy + p%glb%rho_12 * (1.0_8 - heavy )
            p%of(id)%loc%mu%now(i,j,k)  = heavy + p%glb%mu_12  * (1.0_8 - heavy )
        else
            p%of(id)%loc%rho%now(i,j,k) = heavy * p%glb%rho_12 + (1.0_8 - heavy )
            p%of(id)%loc%mu%now(i,j,k)  = heavy * p%glb%mu_12 + (1.0_8 - heavy )
        endif
        
    end do
    end do
    end do
    !$omp end parallel do

enddo       
!$omp end parallel do
    
end subroutine

subroutine marker_mv
use all
implicit none
integer :: i, j, k, id, mid
real(8) :: mass, vol, massv, volv, dv, rho, marker, x, pi

if( p%glb%merged )then
    do mid = 1, 2
        do id = 0, p%glb%threads-1
            p%of(id)%loc%marker(mid)%mass = 0.0
            p%of(id)%loc%marker(mid)%vol = 0.0
            p%of(id)%loc%marker(mid)%massv = 0.0
            p%of(id)%loc%marker(mid)%volv = 0.0
        enddo
    enddo
else

    dv = p%glb%dx * p%glb%dy * p%glb%dz
    pi = dacos(-1.0d0)

    do mid = 1, 2
        
        mass = 0.0d0
        massv= 0.0d0    
        vol  = 0.0d0
        volv = 0.0d0

        !$omp parallel do private(i,j,k,rho,marker,x), reduction(+:mass,vol,massv,volv)    
        do id = 0, p%glb%threads-1

            !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k,rho,marker,x), reduction(+:mass,vol,massv,volv)    
            do k = p%of(id)%loc%ks, p%of(id)%loc%ke
            do j = p%of(id)%loc%js, p%of(id)%loc%je
            do i = p%of(id)%loc%is, p%of(id)%loc%ie

                x = p%of(id)%loc%marker(mid)%lsf%now(i,j,k) / p%glb%ls_wid

                if( x > 1.0 - 1.0d-10)then
                    marker = 1.0
                else if( x < -1.0 + 1.0d-10)then
                    marker = 0.0d0
                else
                    marker = 0.5_8 * (1.0_8 + x + dsin(pi*x) / pi )
                endif

                if( .not. p%glb%inverse ) then
                    rho = marker + p%glb%rho_12 * (1.0_8 - marker )
                else
                    rho = marker * p%glb%rho_12 + (1.0_8 - marker )
                endif

                mass = mass + rho * marker * dv
                vol = vol + marker * dv

                !-------------------------------------------------------

                marker = p%of(id)%loc%marker(mid)%vof%now(i,j,k)

                if( .not. p%glb%inverse ) then
                    rho = marker + p%glb%rho_12 * (1.0_8 - marker )
                else
                    rho = marker * p%glb%rho_12 + (1.0_8 - marker )
                endif

                massv = massv + rho * marker * dv
                volv = volv + marker * dv

            enddo
            enddo
            enddo
            !$omp end parallel do

        enddo
        !$omp end parallel do

        do id = 0, p%glb%threads-1
            p%of(id)%loc%marker(mid)%mass = mass
            p%of(id)%loc%marker(mid)%vol = vol
            p%of(id)%loc%marker(mid)%massv = massv
            p%of(id)%loc%marker(mid)%volv = volv
        enddo

    enddo
    
endif

end subroutine

subroutine ls_mv
use all
implicit none
integer :: id, i, j, k
real(8) :: mass, vol, rho
real(8) :: dv, marker

dv = p%glb%dx * p%glb%dy * p%glb%dz

call ls_funs

!===========================  LS 

mass = 0.0_8; vol=0.0_8

!$omp parallel do private(i,j,k,rho,marker), reduction(+:mass,vol)    
do id = 0, p%glb%threads-1
    
    !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k,rho,marker), reduction(+:mass,vol)  
    do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    do j = p%of(id)%loc%js, p%of(id)%loc%je
    do i = p%of(id)%loc%is, p%of(id)%loc%ie
        
        vol = vol + p%of(id)%loc%heavy%now(i,j,k) * dv
        if( .not. p%glb%inverse )then
            rho = p%of(id)%loc%heavy%now(i,j,k) + p%glb%rho_12 * (1.0d0 - p%of(id)%loc%heavy%now(i,j,k))
        else
            rho = p%glb%rho_12 * p%of(id)%loc%heavy%now(i,j,k) + (1.0d0 - p%of(id)%loc%heavy%now(i,j,k))
        endif
        mass = mass + rho * p%of(id)%loc%heavy%now(i,j,k) * dv
    
    enddo
    enddo
    enddo
    !$omp end parallel do

enddo
!$omp end parallel do

p%glb%mass = mass
p%glb%vol = vol

!===========================  VOF 

mass = 0.0_8; vol=0.0_8

!$omp parallel do private(i,j,k,rho,marker), reduction(+:mass,vol)    
do id = 0, p%glb%threads-1
    
    !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k,rho,marker), reduction(+:mass,vol)   
    do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    do j = p%of(id)%loc%js, p%of(id)%loc%je
    do i = p%of(id)%loc%is, p%of(id)%loc%ie
        
        vol = vol + p%of(id)%loc%vof%now(i,j,k) * dv
        if( .not. p%glb%inverse )then
            rho = p%of(id)%loc%vof%now(i,j,k) + p%glb%rho_12 * (1.0d0 - p%of(id)%loc%vof%now(i,j,k))
        else
            rho = p%glb%rho_12 * p%of(id)%loc%vof%now(i,j,k) + (1.0d0 - p%of(id)%loc%vof%now(i,j,k))
        endif
        mass = mass + rho * p%of(id)%loc%vof%now(i,j,k) * dv
    
    enddo
    enddo
    enddo
    !$omp end parallel do

enddo
!$omp end parallel do

p%glb%massv = mass
p%glb%volv = vol

call p%sync

end subroutine