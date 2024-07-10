subroutine problem_init()
use all
!$ use omp_lib
implicit none
integer :: id, i, j, k, ug, ii,jj,kk, mid
real(8) :: x, y, z, dx, dy, theta, l
CHARACTER(100) :: NAME_OF_FILE
    
    NAME_OF_FILE="default.txt"
    
    ! WRITE(*,*)"============================================"
    ! WRITE(*,*)'Returning the files in directory "/input" '
    ! WRITE(*,*)
    ! CALL SYSTEM("ls ./input")
    ! WRITE(*,*)
    ! WRITE(*,*)"============================================"
    ! WRITE(*,*)'Selet an input file (<d> : default.txt), with full name'
    ! WRITE(*,*)
    ! READ(*,*)NAME_OF_FILE
    !if( name_of_file == 'd' )name_of_file = "default.txt"
    
    call p%init( "./input/"//trim(name_of_file) )
    call pt%init(p)
        
    call p%show

    theta = 0.0d0
    l = 0.0
    dx = l * dcos(theta) * 0.5
    dy = l * dsin(theta) * 0.5

    ug=30
    !$omp parallel do private(i,j,k,ii,jj,kk,x,y,z)
    do id = 0, p%glb%threads-1
        
        !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k,ii,jj,kk,x,y,z)
        do k = p%of(id)%loc%ks, p%of(id)%loc%ke
        do j = p%of(id)%loc%js, p%of(id)%loc%je
        do i = p%of(id)%loc%is, p%of(id)%loc%ie

            p%of(id)%loc%vof%now(i,j,k) = 0.0d0
            p%of(id)%loc%marker(1)%vof%now(i,j,k) = 0.0d0
            p%of(id)%loc%marker(2)%vof%now(i,j,k) = 0.0d0

            do ii = 1, ug
            do jj = 1, ug
            do kk = 1, ug
                
                x = 0.5d0*( p%glb%x(i,j,k)+p%glb%x(i-1,j,k) ) + real(ii,8)*p%glb%dx/real(ug,8)
                y = 0.5d0*( p%glb%y(i,j,k)+p%glb%y(i,j-1,k) ) + real(jj,8)*p%glb%dy/real(ug,8)
                z = 0.5d0*( p%glb%z(i,j,k)+p%glb%z(i,j,k-1) ) + real(kk,8)*p%glb%dz/real(ug,8)

                if(  sqrt((x-0.5)**2+(y-0.75)**2) <= 0.15 )then
                    p%of(id)%loc%vof%now(i,j,k) = p%of(id)%loc%vof%now(i,j,k) +  1.0d0/real(ug,8)**3.0d0
                end if

                if( abs(x-0.5) <= 0.025 .and. y< 0.8 .and. (x-0.5)**2+(y-0.75)**2 <= 0.15**2 )then
                    p%of(id)%loc%vof%now(i,j,k) = p%of(id)%loc%vof%now(i,j,k) -  1.0d0/real(ug,8)**3.0d0
                end if
                
            end do
            end do
            end do
        
            x = p%glb%x(i,j,k)
            y = p%glb%y(i,j,k)
            z = p%glb%z(i,j,k)

            p%of(id)%loc%vel%x%now(i,j,k) = 0.0d0
            p%of(id)%loc%vel%y%now(i,j,k) = 0.0d0
            p%of(id)%loc%vel%z%now(i,j,k) = 0.0d0

            ! p%of(id)%loc%phi%now(i,j,k) = -sqrt((x-0.5)**2+(y-0.75)**2) + 0.15
            p%of(id)%loc%phi%now(i,j,k) = p%of(id)%loc%vof%now(i,j,k) * 2.0 - 1.0
            
        end do
        end do
        end do
        !$omp end parallel do
        
        call p%of(id)%bc(0,p%of(id)%loc%phi%now)
        call p%of(id)%bc(0,p%of(id)%loc%vof%now)

        call p%of(id)%bc(0,p%of(id)%loc%vel%x%now)
        call p%of(id)%bc(0,p%of(id)%loc%vel%y%now)
        call p%of(id)%bc(0,p%of(id)%loc%vel%z%now)

        call p%of(id)%bc(0,p%of(id)%loc%marker(1)%lsf%now)
        call p%of(id)%bc(0,p%of(id)%loc%marker(2)%lsf%now)

        call p%of(id)%bc(0,p%of(id)%loc%marker(1)%vof%now)
        call p%of(id)%bc(0,p%of(id)%loc%marker(2)%vof%now)

    enddo
    !$omp end parallel do

    call pt%vel%sync
    call pt%phi%sync
    call pt%vof%sync
    call pt%marker(1)%sync
    call pt%marker(2)%sync

    call level_set_rk3_redis(0)

    call node_vel
    call ns_init
    !---------------------------------------------------

    !$omp parallel do 
    do id = 0, p%glb%threads-1
        call p%of(id)%loc%init  
    enddo
    !$omp end parallel do

    call second_order

    write(*,*) "plotting"
    call plot
    write(*,*) "plot finished"

    p%glb%ls_adv = 0.0d0
    p%glb%ls_red = 0.0d0
    p%glb%ppe    = 0.0d0
    p%glb%ns     = 0.0d0
    p%glb%syn    = 0.0d0
    p%glb%red_error = 0.0d0

    p%glb%loss_mass_avg = 0.0d0
    p%glb%loss_mass_max = 0.0d0
    p%glb%loss_vol_avg = 0.0d0
    p%glb%loss_vol_max = 0.0d0
    p%glb%loss_int = 0.0d0

    p%glb%loss_mass_avgv = 0.0d0
    p%glb%loss_mass_maxv = 0.0d0
    p%glb%loss_vol_avgv = 0.0d0
    p%glb%loss_vol_maxv = 0.0d0
    p%glb%loss_intv = 0.0d0

    p%glb%iter = 0
    p%glb%time = 0.0d0

    p%glb%Ev = -1.0d0

    call ls_mv
    p%glb%ivol = p%glb%vol
    p%glb%imass = p%glb%mass
    p%glb%ivolv = p%glb%volv
    p%glb%imassv = p%glb%massv
    call p%sync

    call marker_mv
    do id = 0, p%glb%threads-1
        do mid = 1, 2
            p%of(id)%loc%marker(mid)%imass = p%of(id)%loc%marker(mid)%mass
            p%of(id)%loc%marker(mid)%ivol = p%of(id)%loc%marker(mid)%vol
            p%of(id)%loc%marker(mid)%imassv = p%of(id)%loc%marker(mid)%massv
            p%of(id)%loc%marker(mid)%ivolv = p%of(id)%loc%marker(mid)%volv
        enddo
    enddo

    p%glb%merged = .true.

end subroutine