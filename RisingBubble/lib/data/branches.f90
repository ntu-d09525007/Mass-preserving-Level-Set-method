module branches
use roots
use ccd_solvers
use time_solver
use mutligrid_root
implicit none

type global
character(30) :: name
integer :: level
integer :: method
integer :: how_to_paras
integer :: grid_x, grid_y, grid_z, threads, nthreads ! numbers of CPUs 
integer :: node_x, node_y, node_z
integer :: num_of_plots
integer :: ug ! grid per unit length
integer :: ghc ! ghost cell
integer :: ubc(2), vbc(2), wbc(2)
integer :: pid, iter, piter
integer(8) :: cpurate
real(8) :: xstart, xend, ystart, yend, zstart, zend
real(8) :: time, t2s, t2p ! time to stop/plot
real(8) :: dx, dy, dz, dt, rdt
real(8) :: mass, vol, imass, ivol
real(8) :: massv, volv, imassv, ivolv
real(8) :: loss_mass_avg, loss_mass_avgv
real(8) :: loss_mass_max, loss_mass_maxv
real(8) :: loss_vol_avg, loss_vol_avgv
real(8) :: loss_vol_max, loss_vol_maxv
real(8) :: loss_int, loss_intv
real(8) :: Ek, Ep, Es, Ev, Ek0, Ep0, Es0, energy_unit
real(8) :: px, py, pz
real(8) :: xc, yc, zc
real(8) :: re, we, fr
real(8) :: t_w, t_tol
real(8) :: p_w1, p_w2, p_tol, p_b
REAL(8) :: ls_wid
real(8) :: L, T, U ! characteristic length, time, velocity
real(8) :: G, sigma ! gravity, surface tension
real(8) :: gx, gy, gz
real(8) :: mu_1, mu_2, rho_1, rho_2
real(8) :: mu_12, rho_12
real(8) :: red_error
real(8) :: vel_div, vel_sdiv, ns_linf, ns_l2f, ppe_linf
real(8) :: btn_sf, btn_g
real(8) :: ls_adv, ls_red, ppe, ns, syn
real(8),dimension(:,:,:),allocatable :: x, y, z
integer,allocatable :: id(:,:,:)
logical :: xper, yper, zper, inverse
end type global

type interface_function
real(8) :: vol, mass, ivol, imass
real(8) :: volv, massv, ivolv, imassv
type(time_recorded) :: lsf, vof
end type interface_function

type local
integer :: id, idx, idy, idz
integer :: is, ie, js, je, ks, ke
type(dum_matrices)  :: coe
type(time_recorded) :: heavy, delta, grad, sign
type(time_recorded) :: phi, p, rho, mu, vof
type(time_recorded_derivatives) :: normals
type(time_recorded_vec) :: vel, nvel, velsrc
type(interface_function), dimension(:) :: marker(2)
!-------------------------------------------
! type(time_recorded_vec) :: vort, lamb
! type(time_recorded_vec) :: vort_adv, vort_tws, vort_baro, vort_visc
! type(time_recorded) :: q_cri, omega_cri, lamb_div
!-------------------------------------------
type(tensor) :: vel_ten!, vor_ten, p_ten, rho_ten
!-------------------------------------------
type(tsolver_data) :: tdata
type(ccd_manager) :: ccdsolvers
!-------------------------------------------
type(multigrid_root),dimension(:),allocatable :: mg
contains
procedure init => job_loc_init
end type local

type job
type(global) :: glb
type(local)  :: loc
contains
procedure init => job_init
procedure bc => job_bc
procedure velbc => job_vel_bc
procedure nvelbc => job_nvel_bc
procedure find_stag_vel => job_find_stag_vel
procedure find_tensor => job_find_tensor
procedure find_gradient => job_find_gradient
end type job

contains

include './branches_vector.f90'
include './branches_velbc.f90'

subroutine job_init(p,id,gx,gy,gz)
implicit none
class(job) :: p
integer :: gx,gy,gz
integer :: id, IS, IE, JS, JE, KS, KE
integer :: nx, ny, nz, level

P%LOC%ID = ID

if( gz>=gy .and. gz>=gx )then
    p%loc%idz = id / (gx*gy)
    p%loc%idy = (id - p%loc%idz*gx*gy) / gx
    p%loc%idx = (id - p%loc%idz*gx*gy - p%loc%idy*gx)
else if( gy>=gx .and. gy>=gz )then
    p%loc%idy = id / (gx*gz)
    p%loc%idz = (id - p%loc%idy*gx*gz) / gx
    p%loc%idx = (id - p%loc%idy*gx*gz - p%loc%idz*gx)
else
    p%loc%idx = id / (gz*gy)
    p%loc%idy = (id - p%loc%idx*gz*gy) / gz
    p%loc%idz = (id - p%loc%idx*gz*gy - p%loc%idy*gz)
endif

P%LOC%IS =  P%LOC%IDx * P%GLB%NODE_X / gX + 1
P%LOC%IE = (P%LOC%IDx+1)*P%GLB%NODE_X / gx

P%LOC%JS =  P%LOC%IDy * P%GLB%NODE_Y / gY + 1
P%LOC%JE = (P%LOC%IDy+1)*P%GLB%NODE_Y / gy

P%LOC%KS =  P%LOC%IDz * P%GLB%NODE_Z / gZ + 1
P%LOC%KE = (P%LOC%IDz+1)*P%GLB%NODE_Z / gz

IF(P%LOC%IE-P%LOC%IS+1<P%GLB%GHC .OR. P%LOC%JE-P%LOC%JS+1<P%GLB%GHC .OR. P%LOC%KE-P%LOC%KS+1<P%GLB%GHC)THEN
    WRITE(*,*)"Data Synchorziation will fail for this number of threads and grids settings."
    pause
ENDIF

IS = P%LOC%IS - P%GLB%GHC; IE = P%LOC%IE + P%GLB%GHC
JS = P%LOC%JS - P%GLB%GHC; JE = P%LOC%JE + P%GLB%GHC
KS = P%LOC%KS - P%GLB%GHC; KE = P%LOC%KE + P%GLB%GHC

! coefficients matrices
CALL P%LOC%COE%ALLOC(IS,IE,JS,JE,KS,KE)

! level set method
CALL P%LOC%PHI%ALLOC(IS,IE,JS,JE,KS,KE)

call p%loc%marker(1)%lsf%alloc(is,ie,js,je,ks,ke)
call p%loc%marker(2)%lsf%alloc(is,ie,js,je,ks,ke)

call p%loc%marker(1)%vof%alloc(is,ie,js,je,ks,ke)
call p%loc%marker(2)%vof%alloc(is,ie,js,je,ks,ke)

CALL P%LOC%VOF%ALLOC(IS,IE,JS,JE,KS,KE)
CALL P%LOC%heavy%ALLOC(IS,IE,JS,JE,KS,KE)
CALL P%LOC%sign%ALLOC(IS,IE,JS,JE,KS,KE)
CALL P%LOC%delta%ALLOC(IS,IE,JS,JE,KS,KE)
CALL P%LOC%grad%ALLOC(IS,IE,JS,JE,KS,KE)

CALL P%LOC%NORMALS%ALLOC(IS,IE,JS,JE,KS,KE)

! fluid varaible
CALL P%LOC%P%ALLOC(IS,IE,JS,JE,KS,KE)
CALL P%LOC%RHO%ALLOC(IS,IE,JS,JE,KS,KE)
CALL P%LOC%MU%ALLOC(IS,IE,JS,JE,KS,KE)

CALL P%LOC%VEL%ALLOC(IS,IE,JS,JE,KS,KE)
CALL P%LOC%NVEL%ALLOC(IS,IE,JS,JE,KS,KE)
CALL P%LOC%VELSRC%ALLOC(IS,IE,JS,JE,KS,KE)

! solvers
CALL P%LOC%tdata%ALLOC(IS,IE,JS,JE,KS,KE,P%GLB%DT,P%GLB%t_w,P%GLB%GHC)
CALL P%LOC%ccdsolvers%init(IS,IE,JS,JE,KS,KE,P%GLB%DX,P%GLB%DY,P%GLB%DZ,P%GLB%DT)

! vortex identification
! call p%loc%vort%alloc(is,ie,js,je,ks,ke)
! call p%loc%lamb%alloc(is,ie,js,je,ks,ke)

! call p%loc%q_cri%alloc(is,ie,js,je,ks,ke)
! call p%loc%omega_cri%alloc(is,ie,js,je,ks,ke)

! call p%loc%lamb_div%alloc(is,ie,js,je,ks,ke)

!tensor
call p%loc%vel_ten%alloc(3,is,ie,js,je,ks,ke)
! call p%loc%vor_ten%alloc(3,is,ie,js,je,ks,ke)
! call p%loc%p_ten%alloc(1,is,ie,js,je,ks,ke)
! call p%loc%rho_ten%alloc(1,is,ie,js,je,ks,ke)

!vorticity production
! call p%loc%vort_adv%alloc(is,ie,js,je,ks,ke)
! call p%loc%vort_tws%alloc(is,ie,js,je,ks,ke)
! call p%loc%vort_baro%alloc(is,ie,js,je,ks,ke)
! call p%loc%vort_visc%alloc(is,ie,js,je,ks,ke)

if( p%glb%level>0 )then

! multigrid 
allocate(p%loc%mg(p%glb%level))
nx = (p%loc%ie-p%loc%is+1)
ny = (p%loc%je-p%loc%js+1)
nz = (p%loc%ke-p%loc%ks+1)

do level = 1, p%glb%level

    p%loc%mg(level)%idx = p%loc%idx
    p%loc%mg(level)%idy = p%loc%idy
    p%loc%mg(level)%idz = p%loc%idz
    
    p%loc%mg(level)%gx = p%glb%grid_x
    p%loc%mg(level)%gy = p%glb%grid_y
    p%loc%mg(level)%gz = p%glb%grid_z
    
    if(mod(nx,2**(level-1)).eq.0 .and. nx/2**(level-1)>0)then
        p%loc%mg(level)%nx = nx/2**(level-1)
        p%loc%mg(level)%dx = p%glb%dx*2.0**(level-1) 
    else
        write(*,'(A,I2,I3)')"Multigrid level is too high for x, current level:",level,nx,2**(level-1)
        stop
    endif
    if(mod(ny,2**(level-1)).eq.0 .and. ny/2**(level-1)>0)then
        p%loc%mg(level)%ny = ny/2**(level-1)
        p%loc%mg(level)%dy = p%glb%dy*2.0**(level-1) 
    else
        write(*,'(A,I2,I3)')"Multigrid level is too high for y, current level:",level,ny,2**(level-1)
        stop
    endif
    if(mod(nz,2**(level-1)).eq.0 .and. nz/2**(level-1)>0)then
        p%loc%mg(level)%nz = nz/2**(level-1)
        p%loc%mg(level)%dz = p%glb%dz*2.0**(level-1) 
    else
        write(*,'(A,I2,I3)')"Multigrid level is too high for z, current level:",level,nz,2**(level-1)
        stop
    endif
    
    call p%loc%mg(level)%init
    
enddo

endif

end subroutine

subroutine job_loc_init(pp)
implicit none
class(local) :: pp

    call pp%phi%init
    call pp%vof%init
    
    call pp%heavy%init
    call pp%delta%init
    call pp%grad%init
    call pp%sign%init
    
    call pp%p%init
    call pp%rho%init
    call pp%mu%init
    call pp%normals%init
    
    call pp%vel%init
    call pp%nvel%init
    call pp%velsrc%init

    call pp%marker(1)%lsf%init
    call pp%marker(2)%lsf%init

    call pp%marker(1)%vof%init
    call pp%marker(2)%vof%init

end subroutine

subroutine job_bc(p,order,dat)
implicit none
class(job) :: p
real(8), dimension(p%loc%is-p%glb%ghc:p%loc%ie+p%glb%ghc,&
                  &p%loc%js-p%glb%ghc:p%loc%je+p%glb%ghc,&
                  &p%loc%ks-p%glb%ghc:p%loc%ke+p%glb%ghc) :: dat
integer :: order, i, j, k

 if( order == 0 ) then
    
    do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
    do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
    do i = 1, p%glb%ghc
        dat(p%loc%is-i,j,k) = dat(p%loc%is,j,k)
        dat(p%loc%ie+i,j,k) = dat(p%loc%ie,j,k)
    enddo
    enddo
    enddo
    
    do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
    do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
    do j = 1, p%glb%ghc
        dat(i,p%loc%js-j,k) = dat(i,p%loc%js,k)
        dat(i,p%loc%je+j,k) = dat(i,p%loc%je,k)
    enddo
    enddo
    enddo

    do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
    do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
    do k = 1, p%glb%ghc
        dat(i,j,p%loc%ks-k) = dat(i,j,p%loc%ks)
        dat(i,j,p%loc%ke+k) = dat(i,j,p%loc%ke)
    enddo
    enddo
    enddo
        
 else 
 
    write(*,*)" Wrong order for boundary condition, stop the program."
    stop
    
 endif
    

end subroutine

subroutine job_find_stag_vel(p,u,v,w,uu,vv,ww,us,vs,ws)
implicit none
class(job) :: p
real(8), dimension(p%loc%is-p%glb%ghc:p%loc%ie+p%glb%ghc,&
                  &p%loc%js-p%glb%ghc:p%loc%je+p%glb%ghc,&
                  &p%loc%ks-p%glb%ghc:p%loc%ke+p%glb%ghc) :: u,v,w,uu,vv,ww,us,vs,ws
integer :: i, j, k

!===============================================================
!  Us_t + Us*U_x +  V*U_y +  W*U_z = 0
!  Vs_t +  U*V_x + Vs*V_y + WW*V_z = 0
!  Ws_t + UU*W_x + VV*W_y + Ws*W_z = 0
!===============================================================

do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie

    V(i,j,k) = 0.25d0*( Vs(i,j,k)+Vs(i,j-1,k)+Vs(i+1,j,k)+Vs(i+1,j-1,k) )
    W(i,j,k) = 0.25d0*( Ws(i,j,k)+Ws(i,j,k-1)+Ws(i+1,j,k)+Ws(i+1,j,k-1) )

    U(i,j,k) = 0.25d0*( Us(i,j,k)+Us(i-1,j,k)+Us(i,j+1,k)+Us(i-1,j+1,k) )
   WW(i,j,k) = 0.25d0*( Ws(i,j,k)+Ws(i,j,k-1)+Ws(i,j+1,k)+Ws(i,j+1,k-1) )

   UU(i,j,k) = 0.25d0*( Us(i,j,k)+Us(i-1,j,k)+Us(i,j,k+1)+Us(i-1,j,k+1) )
   VV(i,j,k) = 0.25d0*( Vs(i,j,k)+Vs(i,j-1,k)+Vs(i,j,k+1)+Vs(i,j-1,k+1) )

end do
end do
end do

end subroutine

end module branches


