module tree
!$ use omp_lib
use branches
implicit none

type filemanager
integer :: ls_mv, damdata, energy
end type filemanager

type manager
type(global) :: glb
type(filemanager) :: fil
type(job) :: loc
contains
procedure show => manager_show
procedure read => manager_read
procedure init => manager_init
procedure switch => manager_switch
procedure plot => manager_plot
end type manager

contains 

subroutine manager_show(p)
implicit none
class(manager) :: p
integer :: id, level

    write(*,*)" --- Problem information --- "
    write(*,'(A20)')p%glb%name
    write(*,'(A20,F15.8)')"dx:",p%glb%dx
    write(*,'(A20,F15.8)')"dy:",p%glb%dy
    write(*,'(A20,F15.8)')"dz:",p%glb%dz
    write(*,'(A20,F15.8)')"dt:",p%glb%dt
    write(*,'(A20,F15.4)')"Re:",p%glb%re
    write(*,'(A20,F15.4)')"We:",p%glb%we
    write(*,'(A20,F15.4)')"Fr:",p%glb%fr
    write(*,'(A20,F10.4)')"Density ratio:",p%glb%rho_12
    write(*,'(A20,F10.4)')"Viscosity ratio:",p%glb%mu_12
    write(*,'(A20,I5,A3,I5,A3,I5)')"Grids:",p%glb%node_x,"x",p%glb%node_y,"x",p%glb%node_z
    
    write(*,'(A20,I8)')"Overlap layer",p%glb%ghc
    write(*,*)" --- SubDomain Information  --- "

    write(*,'(A20,I4,A3,I4)')"X index:",p%loc%is,"~",p%loc%ie
    write(*,'(A20,I4,A3,I4)')"Y index:",p%loc%js,"~",p%loc%je
    write(*,'(A20,I4,A3,I4)')"Z index:",p%loc%ks,"~",p%loc%ke

end subroutine

subroutine manager_read(p,path)
implicit none
class(manager) :: p
character(*) :: path

 open(unit=526,file=trim(path),status='old')
 
 read(526,*)
 read(526,*)p%glb%method
 read(526,*)
 read(526,*)p%glb%threads
 read(526,*)
 read(526,*)p%glb%num_of_plot
 read(526,*)
 read(526,*)p%glb%name
 read(526,*)
 read(526,*)p%glb%ug
 read(526,*)
 read(526,*)p%glb%ghc
 read(526,*)
 read(526,*)p%glb%xstart, p%glb%xend
 read(526,*)
 read(526,*)p%glb%ystart, p%glb%yend
 read(526,*)
 read(526,*)p%glb%zstart, p%glb%zend
 read(526,*)
 read(526,*)p%glb%t2s, p%glb%t2p
 read(526,*)
 read(526,*)p%glb%dt, p%glb%rdt
 read(526,*)
 read(526,*)p%glb%p_tol, p%glb%p_w1, p%glb%p_w2, p%glb%p_b
 read(526,*)
 read(526,*)p%glb%t_tol, p%glb%t_w
 read(526,*)
 read(526,*)p%glb%ls_wid
 read(526,*)
 read(526,*)p%glb%how_to_paras
 read(526,*)
 read(526,*)p%glb%rho_1, p%glb%mu_1
 read(526,*)
 read(526,*)p%glb%rho_2, p%glb%mu_2
 read(526,*)
 read(526,*)p%glb%sigma, p%glb%btn_sf
 read(526,*)
 read(526,*)p%glb%g, p%glb%btn_g
 read(526,*)
 read(526,*)p%glb%gx,p%glb%gy,p%glb%gz
 read(526,*)
 read(526,*)p%glb%L, p%glb%U, p%glb%T
 read(526,*)
 read(526,*)p%glb%Re, p%glb%Fr, p%glb%We
 read(526,*)
 read(526,*)p%glb%rho_12, p%glb%mu_12
 read(526,*)
 read(526,*)p%glb%ubc(1), p%glb%ubc(2)
 read(526,*)
 read(526,*)p%glb%vbc(1), p%glb%vbc(2)
 read(526,*)
 read(526,*)p%glb%wbc(1), p%glb%wbc(2) 
 
 close(unit=526)
 
end subroutine

subroutine manager_init(p,path)
implicit none
class(manager) :: p
character(*) :: path
integer :: max_threads
integer :: i, j, k, id
real(8) :: mag

    call p%read(path)

    write(*,*)"finish read data from file"
    
    p%fil%energy = 14
    open(unit=p%fil%energy,file="./out/"//trim(p%glb%name)//"_Energy.plt")
    write(p%fil%energy,*)'variables = "T" "Es" "Ek" "Ep" "Ev" '

    p%fil%ls_mv = 15
    open(unit=p%fil%ls_mv,file="./out/"//trim(p%glb%name)//"_MassLoss.plt")
    write(p%fil%ls_mv,*)'variables = "T" "Loss of mass (LS)" "Loss of mass (VOF)" '

    p%fil%damdata = 16
    open(unit=p%fil%damdata,file="./out/"//trim(p%glb%name)//"_DamData.plt")
    write(p%fil%damdata,*)'variables = "T" "Damfront" "Wall" '
    
    call omp_set_dynamic(.false.)
    if( p%glb%threads < 0 )then
        call omp_set_num_threads(omp_get_max_threads())
    else
        call omp_set_num_threads(min(p%glb%threads,omp_get_max_threads()))
    endif

    write(*,*)"finish allocating nmumber of jobs"
    
    p%glb%node_x = p%glb%ug * ( p%glb%xend - p%glb%xstart )
    p%glb%node_y = p%glb%ug * ( p%glb%yend - p%glb%ystart )
    p%glb%node_z = p%glb%ug * ( p%glb%zend - p%glb%zstart )
    
    allocate( p%glb%x(0:p%glb%node_x+1,0:p%glb%node_y+1,0:p%glb%node_z+1), &
              p%glb%y(0:p%glb%node_x+1,0:p%glb%node_y+1,0:p%glb%node_z+1), &
              p%glb%z(0:p%glb%node_x+1,0:p%glb%node_y+1,0:p%glb%node_z+1) )

    write(*,*)"finish allocating public grids"
    
    p%glb%dx = ( p%glb%xend - p%glb%xstart ) / p%glb%node_x
    p%glb%dy = ( p%glb%yend - p%glb%ystart ) / p%glb%node_y
    p%glb%dz = ( p%glb%zend - p%glb%zstart ) / p%glb%node_z
        
    !$omp parallel do
    do i = 1, p%glb%node_x
        p%glb%x(i,:,:) = p%glb%xstart + (i-0.5)*p%glb%dx
    enddo
    !$omp end parallel do
    
    !$omp parallel do
    do j = 1, p%glb%node_y
        p%glb%y(:,j,:) = p%glb%ystart + (j-0.5)*p%glb%dy
    enddo
    !$omp end parallel do
    
    !$omp parallel do
    do k = 1, p%glb%node_z
        p%glb%z(:,:,k) = p%glb%zstart + (k-0.5)*p%glb%dz
    enddo
    !$omp end parallel do
    
    p%glb%x(0,:,:)=p%glb%xstart; p%glb%x(p%glb%node_x+1,:,:)=p%glb%xend
    p%glb%y(:,0,:)=p%glb%ystart; p%glb%y(:,p%glb%node_y+1,:)=p%glb%yend
    p%glb%z(:,:,0)=p%glb%zstart; p%glb%z(:,:,p%glb%node_z+1)=p%glb%zend

    write(*,*)"finish assigning grid data"
    
    p%glb%dt = p%glb%dt * p%glb%dx
    p%glb%rdt = p%glb%rdt * p%glb%dx
    p%glb%ls_wid = p%glb%ls_wid * p%glb%dx
    
    p%glb%p_tol = 0.1_8 ** p%glb%p_tol
    p%glb%t_tol = 0.1_8 ** p%glb%t_tol

    p%glb%p_b = 0.1_8 ** p%glb%p_b

    write(*,*)"finsih numeric setting"

    select case ( p%glb%how_to_paras )
        case (1)
            continue
        case (2) ! L
            p%glb%U = dsqrt( p%glb%L * p%glb%g )
            p%glb%T = p%glb%L / p%glb%U
        case (3) ! L+U
            p%glb%T = p%glb%L / p%glb%U
        case (4) ! L+T
            p%glb%U = p%glb%L / p%glb%T
        case (5) ! U+T
            p%glb%L = p%glb%U * p%glb%T
        case default
            write(*,*)"Error >> Wrong parameter selector "
            stop
    end select 
    
    if( p%glb%how_to_paras > 1 )then
        p%glb%mu_12 = p%glb%mu_2 / p%glb%mu_1
        p%glb%rho_12 = p%glb%rho_2 / p%glb%rho_1
        p%glb%re = p%glb%rho_1 * p%glb%u * p%glb%L / p%glb%mu_1
        p%glb%we = p%glb%rho_1 * p%glb%u**2.0d0 * p%glb%L / p%glb%sigma
        p%glb%fr = p%glb%u**2.0d0 / ( p%glb%g * p%glb%L )
    endif

    p%glb%energy_unit = p%glb%rho_1 * p%glb%g * p%glb%L**4.0

    write(*,*)"finish calculating problem parameters"

    call p%loc%alloc(p%glb%node_x,p%glb%node_y,p%glb%node_z,p%glb%ghc,&
                            p%glb%dx,p%glb%dy,p%glb%dz,p%glb%dt,p%glb%t_w)

    write(*,*)"finish initializing job data"
    
    p%glb%time = 0.0_8
    p%glb%iter = 0
    p%glb%pid = 0
    
    p%glb%ls_adv = 0.0d0
    p%glb%ls_red = 0.0d0
    p%glb%ppe    = 0.0d0
    p%glb%ns     = 0.0d0
    p%glb%syn    = 0.0d0
    
    mag = dsqrt(p%glb%gx**2.0d0+p%glb%gy**2.0d0+p%glb%gz**2.0d0)
    
    p%glb%gx = p%glb%gx / mag
    p%glb%gy = p%glb%gy / mag
    p%glb%gz = p%glb%gz / mag
    
    call system_clock( count_rate=p%glb%cpurate )
    
end subroutine

subroutine manager_switch(p)
implicit none
class(manager) :: p
integer :: id

call p%loc%phi%switch
call p%loc%vof%switch

call p%loc%rho%switch
call p%loc%mu%switch

call p%loc%delta%switch
call p%loc%heavy%switch
call p%loc%sign%switch

call p%loc%normals%switch

call p%loc%vel%switch
call p%loc%nvel%switch
call p%loc%velsrc%switch

call p%loc%p%switch

end subroutine

subroutine manager_plot(p)
implicit none
class(manager) :: p
integer :: ix,iy,iz,i,j,k,id
character(3) :: name
real(8) :: x,y,z

 write(name,'(I3.3)')p%glb%pid
 open(unit=777,file="./out/"//trim(p%glb%name)//"_mesh.vtk")  
 WRITE(777,'(A)')"# vtk DataFile Version 3.0"
 write(777,'(A)')"vtk TEST"
 WRITE(777,'(A)')"ASCII"
 WRITE(777,'(A)')"DATASET STRUCTURED_POINTS"
 WRITE(777,'(A,3I6)')"DIMENSIONS ",p%glb%node_x,p%glb%node_y,p%glb%node_z
 WRITE(777,'(A,3ES15.4)')"SPACING ",p%glb%dx, p%glb%dy, p%glb%dz  
 WRITE(777,'(A,3ES15.4)')"ORIGIN ",p%glb%x(1,1,1),p%glb%y(1,1,1),p%glb%z(1,1,1)
 WRITE(777,'(A,I12)')"POINT_DATA ",(p%glb%node_x)*(p%glb%node_y)*(p%glb%node_z)
 
 close(777)
 
end subroutine

end module tree
