module branches
use roots
use ccd_solvers
use time_solver
implicit none

type global
character(40) :: name
integer :: threads
integer :: level
integer :: method
integer :: how_to_paras
integer :: node_x, node_y, node_z
integer :: num_of_plot
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
real(8) :: re, we, fr
real(8) :: t_w, t_tol
real(8) :: p_w1, p_w2, p_tol, p_b
REAL(8) :: ls_wid
real(8) :: L, T, U ! characteristic length, time, velocity
real(8) :: energy_unit, Es, Ek, Ep, Ev
real(8) :: Es0, Ek0, Ep0
real(8) :: G, sigma ! gravity, surface tension
real(8) :: gx,gy,gz
real(8) :: mu_1, mu_2, rho_1, rho_2
real(8) :: mu_12, rho_12
real(8) :: vel_div, vel_sdiv, ns_linf, ns_l2f, ppe_linf
real(8) :: btn_sf, btn_g
real(8) :: ls_adv, ls_red, ppe, ns, syn
real(8),dimension(:,:,:),allocatable :: x, y, z
end type global

! type ibm_data
! real(8) :: x,y,z
! real(8) :: u,v,w
! type(time_recorded) :: solid
! end type ibm_data

type job
integer :: is, ie, js, je, ks, ke
! type(ibm_data) :: ibm
type(dum_matrices)  :: coe
type(time_recorded) :: heavy, delta, grad, sign
type(time_recorded) :: phi, p, rho, mu, vof
type(time_recorded_derivatives) :: normals
type(time_recorded_vec) :: vel, nvel, velsrc
!-------------------------------------------
type(time_recorded_vec) :: vort, lamb
type(time_recorded_vec) :: vort_adv, vort_tws, vort_baro, vort_visc
type(time_recorded) :: q_cri, omega_cri, lamb_div
!-------------------------------------------
type(tensor) :: vel_ten, vor_ten, p_ten, rho_ten, mu_ten
!-------------------------------------------
type(tsolver_data) :: tdata
type(ccd_manager) :: ccdsolvers
contains
procedure alloc => job_init
procedure init => job_loc_init
end type job

contains

subroutine job_init(p,nx,ny,nz,ghc,dx,dy,dz,dt,t_w)
implicit none
class(job) :: p
real(8) :: dx,dy,dz,dt,t_w
integer :: IS, IE, JS, JE, KS, KE
integer :: nx, ny, nz, ghc

p%IS = 1
p%IE = nx

p%JS = 1
p%JE = ny

p%KS = 1
p%KE = nz

IS = p%IS - GHC; IE = p%IE + GHC
JS = p%JS - GHC; JE = p%JE + GHC
KS = p%KS - GHC; KE = p%KE + GHC

! coefficients matrices
CALL p%COE%ALLOC(IS,IE,JS,JE,KS,KE)

! level set method
CALL p%PHI%ALLOC(IS,IE,JS,JE,KS,KE)

CALL p%VOF%ALLOC(IS,IE,JS,JE,KS,KE)
CALL p%heavy%ALLOC(IS,IE,JS,JE,KS,KE)
CALL p%sign%ALLOC(IS,IE,JS,JE,KS,KE)
CALL p%delta%ALLOC(IS,IE,JS,JE,KS,KE)
CALL p%grad%ALLOC(IS,IE,JS,JE,KS,KE)

CALL p%NORMALS%ALLOC(IS,IE,JS,JE,KS,KE)

! fluid varaible
CALL p%P%ALLOC(IS,IE,JS,JE,KS,KE)
CALL p%RHO%ALLOC(IS,IE,JS,JE,KS,KE)
CALL p%MU%ALLOC(IS,IE,JS,JE,KS,KE)

CALL p%VEL%ALLOC(IS,IE,JS,JE,KS,KE)
CALL p%NVEL%ALLOC(IS,IE,JS,JE,KS,KE)
CALL p%VELSRC%ALLOC(IS,IE,JS,JE,KS,KE)

! solvers
CALL p%tdata%ALLOC(IS,IE,JS,JE,KS,KE,DT,t_w,GHC)
p%ccdsolvers%x%is=is; p%ccdsolvers%x%ie=ie; p%ccdsolvers%x%dx=dx; p%ccdsolvers%x%dt=dt
p%ccdsolvers%y%is=js; p%ccdsolvers%y%ie=je; p%ccdsolvers%y%dx=dy; p%ccdsolvers%y%dt=dt
p%ccdsolvers%z%is=ks; p%ccdsolvers%z%ie=ke; p%ccdsolvers%z%dx=dz; p%ccdsolvers%z%dt=dt

! vortex identification
call p%vort%alloc(is,ie,js,je,ks,ke)
call p%lamb%alloc(is,ie,js,je,ks,ke)

call p%q_cri%alloc(is,ie,js,je,ks,ke)
call p%omega_cri%alloc(is,ie,js,je,ks,ke)

call p%lamb_div%alloc(is,ie,js,je,ks,ke)

!tensor
call p%vel_ten%alloc(3,is,ie,js,je,ks,ke)
call p%vor_ten%alloc(3,is,ie,js,je,ks,ke)
call p%p_ten%alloc(1,is,ie,js,je,ks,ke)
call p%rho_ten%alloc(1,is,ie,js,je,ks,ke)
call p%mu_ten%alloc(1,is,ie,js,je,ks,ke)

!vorticity production
call p%vort_adv%alloc(is,ie,js,je,ks,ke)
call p%vort_tws%alloc(is,ie,js,je,ks,ke)
call p%vort_baro%alloc(is,ie,js,je,ks,ke)
call p%vort_visc%alloc(is,ie,js,je,ks,ke)

end subroutine

subroutine job_loc_init(p)
implicit none
class(job) :: p

call p%phi%init
call p%vof%init

call p%heavy%init
call p%delta%init
call p%grad%init
call p%sign%init

call p%p%init
call p%rho%init
call p%mu%init
call p%normals%init

call p%vel%init
call p%nvel%init
call p%velsrc%init

end subroutine

end module branches


