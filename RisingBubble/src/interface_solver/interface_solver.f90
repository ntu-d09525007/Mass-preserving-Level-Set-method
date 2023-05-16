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
if(mod(p%glb%iter,p%glb%t2red).eq.0)call level_set_rk3_redis(1)

end subroutine

subroutine mpls()
use all
implicit none
integer :: id,i,j,k,mid
real(8) :: tmp1, tmp2
logical :: reconstruct

!call level_set_symplectic_solver 

if( .not. p%glb%merged )then

    do mid = 1, 2

        tmp1 = p%glb%imass
        tmp2 = p%glb%ivol

        p%glb%imass = p%of(0)%loc%marker(mid)%imass
        p%glb%ivol = p%of(0)%loc%marker(mid)%ivol

        !$omp parallel do private(i,j,k)
        do id = 0, p%glb%threads-1

            !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k)
            do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc
            do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
            do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
                p%of(id)%loc%phi%now(i,j,k) = p%of(id)%loc%marker(mid)%lsf%now(i,j,k)
            enddo
            enddo
            enddo
            !$omp end parallel do

        enddo
        !$omp end parallel do

        call level_set_rk3_solver
        if(mod(p%glb%iter,p%glb%t2red).eq.0)call level_set_rk3_redis(1)
        call mass_preserving_level_set

        !$omp parallel do private(i,j,k)
        do id = 0, p%glb%threads-1

            !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k)
            do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc
            do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
            do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
                p%of(id)%loc%marker(mid)%lsf%now(i,j,k) = p%of(id)%loc%phi%now(i,j,k)
            enddo
            enddo
            enddo
            !$omp end parallel do

        enddo
        !$omp end parallel do

        p%glb%imass = tmp1
        p%glb%ivol = tmp2

    enddo

    reconstruct = .false.

    !$omp parallel do private(i,j,k,tmp1,tmp2) reduction(.or.:reconstruct)
    do id = 0, p%glb%threads-1

        !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k,tmp1,tmp2) reduction(.or.:reconstruct)
        do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc
        do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
        do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
            tmp1 = p%of(id)%loc%marker(1)%lsf%now(i,j,k) + p%glb%ls_wid
            tmp2 = p%of(id)%loc%marker(2)%lsf%now(i,j,k) + p%glb%ls_wid
            reconstruct = reconstruct .or. (tmp1 * tmp2 > 0.0d0 .and. tmp1 + tmp2 > 0.0)
            if( tmp1 * tmp2 > 0.0 )then
                if( tmp1 + tmp2 > 0.0 )then
                    p%of(id)%loc%phi%now(i,j,k) = min(tmp1, tmp2) - p%glb%ls_wid
                else
                    p%of(id)%loc%phi%now(i,j,k) = max(tmp1, tmp2) - p%glb%ls_wid
                endif
            else
                p%of(id)%loc%phi%now(i,j,k) = max(tmp1, tmp2) - p%glb%ls_wid
            endif
        enddo
        enddo
        enddo
        !$omp end parallel do

        call p%of(id)%bc(0, p%of(id)%loc%phi%now)

    enddo
    !$omp end parallel do

    call pt%phi%sync

    if( reconstruct )then
        call level_set_rk3_redis(1)
        call mass_preserving_level_set
        call level_set_rk3_redis(1)
        p%glb%merged = .true.
    endif

else

    call level_set_rk3_solver
    if(mod(p%glb%iter,p%glb%t2red).eq.0)call level_set_rk3_redis(1)
    call mass_preserving_level_set

endif

end subroutine

subroutine clsvof()
use all
implicit none
integer :: id,i,j,k,mid
logical :: reconstruct
real(8) :: tmp1, tmp2


if( .not. p%glb%merged )then

    do mid = 1, 2

        !$omp parallel do private(i,j,k)
        do id = 0, p%glb%threads-1

            !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k)
            do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc
            do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
            do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
                p%of(id)%loc%phi%now(i,j,k) = p%of(id)%loc%marker(mid)%lsf%now(i,j,k)
                p%of(id)%loc%vof%now(i,j,k) = p%of(id)%loc%marker(mid)%vof%now(i,j,k)

                p%of(id)%loc%marker(mid)%vof%tmp(i,j,k) = p%of(id)%loc%vof%old(i,j,k)
                p%of(id)%loc%vof%old(i,j,k) = p%of(id)%loc%marker(mid)%vof%old(i,j,k)
            enddo
            enddo
            enddo
            !$omp end parallel do

        enddo
        !$omp end parallel do

        call vof_wlic_solver
        !call level_set_symplectic_solver 
        call level_set_rk3_solver

        if(mod(p%glb%iter,p%glb%t2red).eq.0)then
            call clsvof_recon
        endif

        !$omp parallel do private(i,j,k)
        do id = 0, p%glb%threads-1

            !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k)
            do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc
            do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
            do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
                p%of(id)%loc%marker(mid)%lsf%now(i,j,k) = p%of(id)%loc%phi%now(i,j,k)
                p%of(id)%loc%marker(mid)%vof%now(i,j,k) = p%of(id)%loc%vof%now(i,j,k)

                p%of(id)%loc%vof%old(i,j,k) = p%of(id)%loc%marker(mid)%vof%tmp(i,j,k)
            enddo
            enddo
            enddo
            !$omp end parallel do

        enddo
        !$omp end parallel do

    enddo

    !$omp parallel do private(i,j,k,tmp1,tmp2) reduction(.or.:reconstruct)
    do id = 0, p%glb%threads-1

        !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k,tmp1,tmp2) reduction(.or.:reconstruct)
        do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc
        do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
        do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
            tmp1 = p%of(id)%loc%marker(1)%lsf%now(i,j,k) + p%glb%ls_wid
            tmp2 = p%of(id)%loc%marker(2)%lsf%now(i,j,k) + p%glb%ls_wid
            reconstruct = reconstruct .or. (tmp1 * tmp2 > 0.0d0 .and. tmp1 + tmp2 > 0.0)
            if( tmp1 * tmp2 > 0.0 )then
                if( tmp1 + tmp2 > 0.0 )then
                    p%of(id)%loc%phi%now(i,j,k) = min(tmp1, tmp2) - p%glb%ls_wid
                else
                    p%of(id)%loc%phi%now(i,j,k) = max(tmp1, tmp2) - p%glb%ls_wid
                endif
            else
                p%of(id)%loc%phi%now(i,j,k) = max(tmp1, tmp2) - p%glb%ls_wid
            endif

            p%of(id)%loc%vof%now(i,j,k) = min(p%of(id)%loc%marker(1)%vof%now(i,j,k) + p%of(id)%loc%marker(2)%vof%now(i,j,k),&
                                            & 1.0d0)

        enddo
        enddo
        enddo
        !$omp end parallel do

        call p%of(id)%bc(0, p%of(id)%loc%phi%now)
        call p%of(id)%bc(0, p%of(id)%loc%vof%now)

    enddo
    !$omp end parallel do

    call pt%phi%sync
    call pt%vof%sync

    if( reconstruct )then
        call clsvof_recon
        p%glb%merged = .true.
    endif

else

    call vof_wlic_solver
    !call level_set_symplectic_solver 
    call level_set_rk3_solver

    if(mod(p%glb%iter,p%glb%t2red).eq.0)then
        call clsvof_recon
    endif

endif

end subroutine
