module roots
implicit none

type dum_matrices
integer :: is, ie, js ,je, ks, ke
real(8),dimension(:,:,:),allocatable :: r,l,f,b,u,d,c,src
contains 
procedure alloc => dum_matrices_alloc
end type dum_matrices

type time_recorded
integer :: is, ie, js ,je, ks, ke
real(8),dimension(:,:,:),allocatable :: now, old, old2, tmp
contains
procedure alloc => time_recorded_alloc
procedure init => time_recorded_init
procedure switch => time_recorded_switch
end type time_recorded

type time_recorded_derivatives
integer :: is, ie, js ,je, ks, ke
type(time_recorded) :: x, y, z, xx, yy, zz, xy, xz, yz, curv
contains
procedure alloc => time_recorded_derivatives_alloc
procedure switch => time_recorded_derivatives_switch
procedure init => time_recorded_derivatives_init
end type time_recorded_derivatives

type time_recorded_vec
integer :: is, ie, js ,je, ks, ke
type(time_recorded) :: x, y, z
contains
procedure alloc => time_recorded_vec_alloc
procedure init => time_recorded_vec_init
procedure switch => time_recorded_vec_switch
end type time_recorded_vec

type tensor
integer :: order 
integer :: is, ie, js ,je, ks, ke
real(8), dimension(:,:,:), allocatable :: x,y,z,tmp
real(8), dimension(:,:,:), allocatable :: xx,xy,xz,yx,yy,yz,zx,zy,zz
real(8), dimension(:,:,:), allocatable :: xxx,xyy,xzz,yxx,yyy,yzz,zxx,zyy,zzz
contains
procedure alloc => tensor_alloc
end type tensor

!====================================================================================!
contains 
!====================================================================================!

subroutine tensor_alloc(p,order,is,ie,js,je,ks,ke)
implicit none
class(tensor) :: p
integer :: is,ie,js,je,ks,ke,order

p%order = order

p%is = is; p%ie = ie
p%js = js; p%je = je
p%ks = ks; p%ke = ke

if( order == 3)then

allocate( p%xx(is:ie,js:je,ks:ke), p%xy(is:ie,js:je,ks:ke), p%xz(is:ie,js:je,ks:ke), &
          p%yx(is:ie,js:je,ks:ke), p%yy(is:ie,js:je,ks:ke), p%yz(is:ie,js:je,ks:ke), &
          p%zx(is:ie,js:je,ks:ke), p%zy(is:ie,js:je,ks:ke), p%zz(is:ie,js:je,ks:ke) )

allocate( p%xxx(is:ie,js:je,ks:ke), p%xyy(is:ie,js:je,ks:ke), p%xzz(is:ie,js:je,ks:ke), &
          p%yxx(is:ie,js:je,ks:ke), p%yyy(is:ie,js:je,ks:ke), p%yzz(is:ie,js:je,ks:ke), &
          p%zxx(is:ie,js:je,ks:ke), p%zyy(is:ie,js:je,ks:ke), p%zzz(is:ie,js:je,ks:ke) )

else if ( order == 1 )then

allocate( p%x(is:ie,js:je,ks:ke), p%y(is:ie,js:je,ks:ke), p%z(is:ie,js:je,ks:ke), p%tmp(is:ie,js:je,ks:ke)  )

endif

end subroutine

subroutine dum_matrices_alloc(p,is,ie,js,je,ks,ke)
implicit none
class(dum_matrices) :: p
integer, intent(in) :: is,ie,js,je,ks,ke

p%is = is; p%ie = ie
p%js = js; p%je = je
p%ks = ks; p%ke = ke

allocate( p%r(is:ie,js:je,ks:ke), p%l(is:ie,js:je,ks:ke), p%f(is:ie,js:je,ks:ke), &
        & p%b(is:ie,js:je,ks:ke), p%c(is:ie,js:je,ks:ke), p%src(is:ie,js:je,ks:ke), &
        & p%u(is:ie,js:je,ks:ke), p%d(is:ie,js:je,ks:ke) )

end subroutine 

subroutine time_recorded_alloc(p,is,ie,js,je,ks,ke)
implicit none
class(time_recorded) :: p
integer, intent(in) :: is,ie,js,je,ks,ke

p%is = is; p%ie = ie
p%js = js; p%je = je
p%ks = ks; p%ke = ke 

allocate( p%now(is:ie,js:je,ks:ke), p%old(is:ie,js:je,ks:ke), p%old2(is:ie,js:je,ks:ke), p%tmp(is:ie,js:je,ks:ke) )

end subroutine

subroutine time_recorded_derivatives_alloc(p,is,ie,js,je,ks,ke)
implicit none
class(time_recorded_derivatives) :: p
integer, intent(in) :: is,ie,js,je,ks,ke

p%is = is; p%ie = ie
p%js = js; p%je = je
p%ks = ks; p%ke = ke 

call p%x%alloc(is,ie,js,je,ks,ke)
call p%y%alloc(is,ie,js,je,ks,ke)
call p%z%alloc(is,ie,js,je,ks,ke)

call p%xx%alloc(is,ie,js,je,ks,ke)
call p%yy%alloc(is,ie,js,je,ks,ke)
call p%zz%alloc(is,ie,js,je,ks,ke)

call p%xy%alloc(is,ie,js,je,ks,ke)
call p%xz%alloc(is,ie,js,je,ks,ke)
call p%yz%alloc(is,ie,js,je,ks,ke)

call p%curv%alloc(is,ie,js,je,ks,ke)

end subroutine

subroutine time_recorded_vec_alloc(p,is,ie,js,je,ks,ke)
implicit none
class(time_recorded_vec) :: p
integer, intent(in) :: is,ie,js,je,ks,ke

p%is = is; p%ie = ie
p%js = js; p%je = je
p%ks = ks; p%ke = ke 

call p%x%alloc(is,ie,js,je,ks,ke)
call p%y%alloc(is,ie,js,je,ks,ke)
call p%z%alloc(is,ie,js,je,ks,ke)

end subroutine

subroutine time_recorded_init(p)
implicit none
integer :: i,j,k
class(time_recorded) :: p

!$omp parallel do collapse(3)
do k = p%ks, p%ke
do j = p%js, p%je
do i = p%is, p%ie
    p%old(i,j,k) = p%now(i,j,k)
    p%old2(i,j,k) = p%now(i,j,k)
    p%tmp(i,j,k) = p%now(i,j,k)
end do
end do 
end do
!$omp end parallel do

end subroutine

subroutine time_recorded_derivatives_init(p)
implicit none
class(time_recorded_derivatives) :: p

call p%x%init()
call p%y%init()
call p%z%init()

call p%xx%init()
call p%yy%init()
call p%zz%init()

call p%xy%init()
call p%xz%init()
call p%yz%init()

call p%curv%init()

end subroutine

subroutine time_recorded_vec_init(p)
implicit none
class(time_recorded_vec) :: p

call p%x%init
call p%y%init
call p%z%init
 
end subroutine

subroutine time_recorded_switch(p)
implicit none
integer :: i,j,k
class(time_recorded) :: p

!$omp parallel do collapse(3)
do k = p%ks, p%ke
do j = p%js, p%je
do i = p%is, p%ie
  p%old2(i,j,k) = p%old(i,j,k)
  p%old(i,j,k) = p%now(i,j,k)
end do
end do 
end do
!$omp end parallel do
 
end subroutine

subroutine time_recorded_derivatives_switch(p)
implicit none
class(time_recorded_derivatives) :: p

call p%x%switch
call p%y%switch
call p%z%switch

call p%xx%switch
call p%yy%switch
call p%zz%switch

call p%xy%switch
call p%xz%switch
call p%yz%switch

call p%curv%switch
    
end subroutine

subroutine time_recorded_vec_switch(p)
implicit none
class(time_recorded_vec) :: p

call p%x%switch
call p%y%switch
call p%z%switch

end subroutine

end module roots

