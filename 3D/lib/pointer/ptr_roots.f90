module ptr_roots
use tree
implicit none

type pointer_child
integer :: is, ie, js, je, ks, ke, ghc
real(8), dimension(:,:,:), pointer :: dat
end type pointer_child

type pointer_parent
integer :: gx, gy, gz
integer(8) :: cpurate
real(8) :: cputime
logical :: xper, yper, zper
type(pointer_child),allocatable :: of(:,:,:)
contains
procedure init => ptrpart_init
procedure sync => ptrpart_sync
procedure reset => ptrpart_reset
procedure check_per =>ptrpart_check_periodic
end type pointer_parent

type pointer_vector_parent
type(pointer_parent),allocatable :: nodes(:)
integer :: num
integer(8) :: cpurate
real(8) :: cputime
contains
procedure init => ptrvecpart_init
procedure sync => ptrvecpart_sync
procedure reset => ptrvecpart_reset
procedure check_per =>ptrvecpart_check_periodic
end type pointer_vector_parent

type pointer_mg_child
integer :: n
real(8), dimension(:,:,:), pointer :: dat
end type pointer_mg_child

type pointer_mg_child2
type(pointer_mg_child),allocatable :: at(:)
end type pointer_mg_child2

type pointer_mg_parent
integer :: gx, gy, gz
integer(8) :: cpurate
real(8) :: cputime
logical :: xper, yper, zper
type(pointer_mg_child2),allocatable :: of(:,:,:)
contains
procedure init => ptrmgpart_init
procedure sync => ptrmgpart_sync
procedure reset => ptrmgpart_reset
end type pointer_mg_parent

contains 

subroutine ptrpart_init(p,src)
implicit none
class(pointer_parent) :: p
type(manager) :: src
integer :: idx,idy,idz,id

    p%cputime = 0.0d0
    p%cpurate = src%glb%cpurate
    
    p%gx = src%glb%grid_x
    p%gy = src%glb%grid_y
    p%gz = src%glb%grid_z

    p%xper = src%glb%xper
    p%yper = src%glb%yper
    p%zper = src%glb%zper
    
    allocate( p%of(0:p%gx-1,0:p%gy-1,0:p%gz-1) )
    
    !$omp parallel do collapse(3), private(id)
    do idz = 0, p%gz-1
    do idy = 0, p%gy-1
    do idx = 0, p%gx-1

        id = src%glb%id(idx,idy,idz)
        
        p%of(idx,idy,idz)%is = src%of(id)%loc%is
        p%of(idx,idy,idz)%ie = src%of(id)%loc%ie 
        p%of(idx,idy,idz)%js = src%of(id)%loc%js
        p%of(idx,idy,idz)%je = src%of(id)%loc%je
        p%of(idx,idy,idz)%ks = src%of(id)%loc%ks
        p%of(idx,idy,idz)%ke = src%of(id)%loc%ke
        
        p%of(idx,idy,idz)%ghc = src%glb%ghc
    
    enddo
    enddo
    enddo
    !$omp end parallel do

end subroutine

subroutine ptrvecpart_init(p,src,num)
implicit none
class(pointer_vector_parent) :: p 
type(manager) :: src
integer :: num, comp

    p%cputime = 0.0d0
    p%cpurate = src%glb%cpurate
    p%num = num
    
    allocate( p%nodes(1:p%num) )
    
    do comp = 1, p%num
        call p%nodes(comp)%init(src)
    enddo
    
end subroutine

subroutine ptrmgpart_init(p,src)
implicit none
class(pointer_mg_parent) :: p
type(manager) :: src
integer :: idx,idy,idz,id,level

p%cputime = 0.0d0
p%cpurate = src%glb%cpurate

p%gx = src%glb%grid_x
p%gy = src%glb%grid_y
p%gz = src%glb%grid_z

p%xper = src%glb%xper
p%yper = src%glb%yper
p%zper = src%glb%zper

allocate( p%of(0:p%gx-1,0:p%gy-1,0:p%gz-1) )

!$omp parallel do collapse(3), private(id,level)
do idz = 0, p%gz-1
do idy = 0, p%gy-1
do idx = 0, p%gx-1

    id = src%glb%id(idx,idy,idz)

    allocate( p%of(idx,idy,idz)%at(src%glb%level) )

    do level = 1, src%glb%level
        p%of(idx,idy,idz)%at(level)%n = src%of(id)%loc%mg(level)%nx
    enddo

enddo
enddo
enddo
!$omp end parallel do

end subroutine

subroutine ptrpart_sync(p)
implicit none
class(pointer_parent) :: p
integer :: idx,idy,idz,i,j,k,ghc
integer(8) :: cpustart, cpuend

    call system_clock(cpustart)

    ghc = p%of(0,0,0)%ghc

    !$omp parallel do private(i,j,k), collapse(3)
    do idz = 0, p%gz-1
    do idy = 0, p%gy-1
    do idx = 0, p%gx-1
        
        ! x direction
        if(idx<p%gx-1)then
            
            do k = p%of(idx,idy,idz)%ks-p%of(idx,idy,idz)%ghc, p%of(idx,idy,idz)%ke+p%of(idx,idy,idz)%ghc
            do j = p%of(idx,idy,idz)%js-p%of(idx,idy,idz)%ghc, p%of(idx,idy,idz)%je+p%of(idx,idy,idz)%ghc
            do i = p%of(idx,idy,idz)%ie+1, p%of(idx,idy,idz)%ie+p%of(idx,idy,idz)%ghc
                p%of(idx,idy,idz)%dat(i,j,k) = p%of(idx+1,idy,idz)%dat(i,j,k)
            end do
            end do
            end do
        
        endif

        if(idx>0)then
            
            do k = p%of(idx,idy,idz)%ks-p%of(idx,idy,idz)%ghc, p%of(idx,idy,idz)%ke+p%of(idx,idy,idz)%ghc
            do j = p%of(idx,idy,idz)%js-p%of(idx,idy,idz)%ghc, p%of(idx,idy,idz)%je+p%of(idx,idy,idz)%ghc
            do i = p%of(idx,idy,idz)%is-1, p%of(idx,idy,idz)%is-p%of(idx,idy,idz)%ghc, -1
                p%of(idx,idy,idz)%dat(i,j,k) = p%of(idx-1,idy,idz)%dat(i,j,k)
            end do
            end do
            end do
        
        endif
        
        ! y direction
        if(idy<p%gy-1)then
            
            do k = p%of(idx,idy,idz)%ks-p%of(idx,idy,idz)%ghc, p%of(idx,idy,idz)%ke+p%of(idx,idy,idz)%ghc
            do i = p%of(idx,idy,idz)%is-p%of(idx,idy,idz)%ghc, p%of(idx,idy,idz)%ie+p%of(idx,idy,idz)%ghc
            do j = p%of(idx,idy,idz)%je+1, p%of(idx,idy,idz)%je+p%of(idx,idy,idz)%ghc
                p%of(idx,idy,idz)%dat(i,j,k) = p%of(idx,idy+1,idz)%dat(i,j,k)
            end do
            end do
            end do
        
        endif
        
        if(idy>0)then
            
            do k = p%of(idx,idy,idz)%ks-p%of(idx,idy,idz)%ghc, p%of(idx,idy,idz)%ke+p%of(idx,idy,idz)%ghc
            do i = p%of(idx,idy,idz)%is-p%of(idx,idy,idz)%ghc, p%of(idx,idy,idz)%ie+p%of(idx,idy,idz)%ghc
            do j = p%of(idx,idy,idz)%js-1, p%of(idx,idy,idz)%js-p%of(idx,idy,idz)%ghc, -1
                p%of(idx,idy,idz)%dat(i,j,k) = p%of(idx,idy-1,idz)%dat(i,j,k)
            end do
            end do
            end do
        
        endif
        
        ! z direction
        if(idz<p%gz-1)then
        
            do j = p%of(idx,idy,idz)%js-p%of(idx,idy,idz)%ghc, p%of(idx,idy,idz)%je+p%of(idx,idy,idz)%ghc
            do i = p%of(idx,idy,idz)%is-p%of(idx,idy,idz)%ghc, p%of(idx,idy,idz)%ie+p%of(idx,idy,idz)%ghc       
            do k = p%of(idx,idy,idz)%ke+1, p%of(idx,idy,idz)%ke+p%of(idx,idy,idz)%ghc
                p%of(idx,idy,idz)%dat(i,j,k) = p%of(idx,idy,idz+1)%dat(i,j,k)
            end do
            end do
            end do
        
        endif

        if(idz>0)then
            
            do j = p%of(idx,idy,idz)%js-p%of(idx,idy,idz)%ghc, p%of(idx,idy,idz)%je+p%of(idx,idy,idz)%ghc
            do i = p%of(idx,idy,idz)%is-p%of(idx,idy,idz)%ghc, p%of(idx,idy,idz)%ie+p%of(idx,idy,idz)%ghc
            do k = p%of(idx,idy,idz)%ks-1, p%of(idx,idy,idz)%ks-p%of(idx,idy,idz)%ghc, -1
                p%of(idx,idy,idz)%dat(i,j,k) = p%of(idx,idy,idz-1)%dat(i,j,k)
            end do
            end do
            end do
        
        endif

    end do
    end do
    end do
    !$omp end parallel do

    if(p%xper)then
        
        !$omp parallel do private(i,j,k), collapse(2)
        do idz = 0, p%gz-1
        do idy = 0, p%gy-1

            do k = p%of(0,idy,idz)%ks - ghc, p%of(0,idy,idz)%ke + ghc
            do j = p%of(0,idy,idz)%js - ghc, p%of(0,idy,idz)%je + ghc
            do i = 1, ghc
                p%of(0,idy,idz)%dat(1-i,j,k) = p%of(p%gx-1,idy,idz)%dat(p%of(p%gx-1,idy,idz)%ie+1-i,j,k)
                p%of(p%gx-1,idy,idz)%dat(p%of(p%gx-1,idy,idz)%ie+i,j,k) = p%of(0,idy,idz)%dat(i,j,k)
            enddo
            enddo
            enddo

        enddo
        enddo
        !$omp end parallel do

    endif

    if(p%yper)then

        !$omp parallel do private(i,j,k), collapse(2)
        do idz = 0, p%gz-1
        do idx = 0, p%gx-1

            do k = p%of(idx,0,idz)%ks - ghc, p%of(idx,0,idz)%ke + ghc
            do i = p%of(idx,0,idz)%is - ghc, p%of(idx,0,idz)%ie + ghc
            do j = 1, ghc
                p%of(idx,0,idz)%dat(i,1-j,k) = p%of(idx,p%gy-1,idz)%dat(i,p%of(idx,p%gy-1,idz)%je+1-j,k)
                p%of(idx,p%gy-1,idz)%dat(i,p%of(idx,p%gy-1,idz)%je+j,k) = p%of(idx,0,idz)%dat(i,j,k)
            enddo
            enddo
            enddo

        enddo
        enddo
        !$omp end parallel do


    endif

    if(p%zper)then

        !$omp parallel do private(i,j,k), collapse(2)
        do idy = 0, p%gy-1
        do idx = 0, p%gx-1

            do j = p%of(idx,idy,0)%js - ghc, p%of(idx,idy,0)%je + ghc
            do i = p%of(idx,idy,0)%is - ghc, p%of(idx,idy,0)%ie + ghc
            do k = 1, ghc
                p%of(idx,idy,0)%dat(i,j,1-k) = p%of(idx,idy,p%gz-1)%dat(i,j,p%of(idx,idy,p%gz-1)%ke+1-k)
                p%of(idx,idy,p%gz-1)%dat(i,j,p%of(idx,idy,p%gz-1)%ke+k) = p%of(idx,idy,0)%dat(i,j,k)
            enddo
            enddo
            enddo

        enddo
        enddo
        !$omp end parallel do


    endif
    
    call system_clock(cpuend)
    p%cputime = p%cputime + real(cpuend-cpustart,kind=8)/real(p%cpurate,kind=8)

end subroutine

subroutine ptrpart_check_periodic(p)
implicit none
class(pointer_parent) :: p
integer :: idx,idy,idz,i,j,k,ghc
real(8) :: errx,erry,errz

ghc = p%of(0,0,0)%ghc

if(p%xper)then
    
    errx=0.0d0        
    !$omp parallel do private(i,j,k), collapse(2), reduction(+:errx)
    do idz = 0, p%gz-1
    do idy = 0, p%gy-1

        do k = p%of(0,idy,idz)%ks - ghc, p%of(0,idy,idz)%ke + ghc
        do j = p%of(0,idy,idz)%js - ghc, p%of(0,idy,idz)%je + ghc
        do i = 1, ghc

            errx = errx + (p%of(0,idy,idz)%dat(1-i,j,k)-p%of(p%gx-1,idy,idz)%dat(p%of(p%gx-1,idy,idz)%ie+1-i,j,k))**2.0d0
            errx = errx + (p%of(p%gx-1,idy,idz)%dat(p%of(p%gx-1,idy,idz)%ie+i,j,k)-p%of(0,idy,idz)%dat(i,j,k))**2.0d0
            
        enddo
        enddo
        enddo

    enddo
    enddo
    !$omp end parallel do

endif

if(p%yper)then

    erry=0.0d0
    !$omp parallel do private(i,j,k), collapse(2), reduction(+:erry)
    do idz = 0, p%gz-1
    do idx = 0, p%gx-1

        do k = p%of(idx,0,idz)%ks - ghc, p%of(idx,0,idz)%ke + ghc
        do i = p%of(idx,0,idz)%is - ghc, p%of(idx,0,idz)%ie + ghc
        do j = 1, ghc

            erry = erry + (p%of(idx,0,idz)%dat(i,1-j,k)-p%of(idx,p%gy-1,idz)%dat(i,p%of(idx,p%gy-1,idz)%je+1-j,k))**2.0d0
            erry = erry + (p%of(idx,p%gy-1,idz)%dat(i,p%of(idx,p%gy-1,idz)%je+j,k)-p%of(idx,0,idz)%dat(i,j,k))**2.0d0

        enddo
        enddo
        enddo

    enddo
    enddo
    !$omp end parallel do

endif

if(p%zper)then

    errz=0.0d0
    !$omp parallel do private(i,j,k), collapse(2), reduction(+:errz)
    do idy = 0, p%gy-1
    do idx = 0, p%gx-1

        do j = p%of(idx,idy,0)%js - ghc, p%of(idx,idy,0)%je + ghc
        do i = p%of(idx,idy,0)%is - ghc, p%of(idx,idy,0)%ie + ghc
        do k = 1, ghc

            errz = errz + (p%of(idx,idy,0)%dat(i,j,1-k)-p%of(idx,idy,p%gz-1)%dat(i,j,p%of(idx,idy,p%gz-1)%ke+1-k))**2.0d0
            errz = errz + (p%of(idx,idy,p%gz-1)%dat(i,j,p%of(idx,idy,p%gz-1)%ke+k)-p%of(idx,idy,0)%dat(i,j,k))**2.0d0

        enddo
        enddo
        enddo

    enddo
    enddo
    !$omp end parallel do

endif

write(*,*)errx+erry+errz

end subroutine

subroutine ptrvecpart_check_periodic(p)
implicit none
class(pointer_vector_parent) :: p
integer :: id,i,j,k
integer(8) :: cpustart, cpuend
integer :: comp

do comp = 1, p%num
    call p%nodes(comp)%check_per
enddo

end subroutine

subroutine ptrvecpart_sync(p)
implicit none
class(pointer_vector_parent) :: p
integer :: id,i,j,k
integer(8) :: cpustart, cpuend
integer :: comp
    
    call system_clock(cpustart)

    do comp = 1, p%num
        call p%nodes(comp)%sync
    enddo
    
    call system_clock(cpuend)
    p%cputime = p%cputime + real(cpuend-cpustart,kind=8)/real(p%cpurate,kind=8)
    
end subroutine

subroutine ptrmgpart_sync(p,level)
implicit none
class(pointer_mg_parent) :: p
integer,intent(in) :: level
integer :: idx,idy,idz,i,j,k,n
integer(8) :: cpustart, cpuend

call system_clock(cpustart)

n = p%of(0,0,0)%at(level)%n

!$omp parallel do collapse(3), private(i,j,k)
do idx = 0, p%gx-1
do idy = 0, p%gy-1
do idz = 0, p%gz-1

    if( idx>0 )then
        do k = 1, n
        do j = 1, n
            p%of(idx,idy,idz)%at(level)%dat(0,j,k) = p%of(idx-1,idy,idz)%at(level)%dat(n,j,k)
        enddo
        enddo
    else
        do k = 1, n
        do j = 1, n
            p%of(idx,idy,idz)%at(level)%dat(0,j,k) = p%of(idx,idy,idz)%at(level)%dat(1,j,k)
        enddo
        enddo
    endif

    if( idx<p%gx-1 )then
        do k = 1, n
        do j = 1, n
            p%of(idx,idy,idz)%at(level)%dat(n+1,j,k) = p%of(idx+1,idy,idz)%at(level)%dat(1,j,k)
        enddo
        enddo
    else
        do k = 1, n
        do j = 1, n
            p%of(idx,idy,idz)%at(level)%dat(n+1,j,k) = p%of(idx,idy,idz)%at(level)%dat(n,j,k)
        enddo
        enddo
    endif

    !--------------------------

    if( idy>0 )then
        do k = 1, n
        do i = 1, n
            p%of(idx,idy,idz)%at(level)%dat(i,0,k) = p%of(idx,idy-1,idz)%at(level)%dat(i,n,k)
        enddo
        enddo
    else
        do k = 1, n
        do i = 1, n
            p%of(idx,idy,idz)%at(level)%dat(i,0,k) = p%of(idx,idy,idz)%at(level)%dat(i,1,k)
        enddo
        enddo
    endif

    if( idy<p%gy-1 )then
        do k = 1, n
        do i = 1, n
            p%of(idx,idy,idz)%at(level)%dat(i,n+1,k) = p%of(idx,idy+1,idz)%at(level)%dat(i,1,k)
        enddo
        enddo
    else
        do k = 1, n
        do i = 1, n
            p%of(idx,idy,idz)%at(level)%dat(i,n+1,k) = p%of(idx,idy,idz)%at(level)%dat(i,n,k)
        enddo
        enddo
    endif

    !--------------------------

    if( idz>0 )then
        do j = 1, n
        do i = 1, n
            p%of(idx,idy,idz)%at(level)%dat(i,j,0) = p%of(idx,idy,idz-1)%at(level)%dat(i,j,n)
        enddo
        enddo
    else
        do j = 1, n
        do i = 1, n
            p%of(idx,idy,idz)%at(level)%dat(i,j,0) = p%of(idx,idy,idz)%at(level)%dat(i,j,1)
        enddo
        enddo
    endif

    if( idz<p%gz-1 )then
        do j = 1, n
        do i = 1, n
            p%of(idx,idy,idz)%at(level)%dat(i,j,n+1) = p%of(idx,idy,idz+1)%at(level)%dat(i,j,1)
        enddo
        enddo
    else
        do j = 1, n
        do i = 1, n
            p%of(idx,idy,idz)%at(level)%dat(i,j,n+1) = p%of(idx,idy,idz)%at(level)%dat(i,j,n)
        enddo
        enddo
    endif

enddo
enddo
enddo
!$omp end parallel do

if(p%xper)then
        
    !$omp parallel do private(i,j,k), collapse(2)
    do idz = 0, p%gz-1
    do idy = 0, p%gy-1

        do k = 1, n
        do j = 1, n
            p%of(0,idy,idz)%at(level)%dat(0,j,k) = p%of(p%gx-1,idy,idz)%at(level)%dat(n,j,k)
            p%of(p%gx-1,idy,idz)%at(level)%dat(n+1,j,k) = p%of(0,idy,idz)%at(level)%dat(1,j,k)
        enddo
        enddo

    enddo
    enddo
    !$omp end parallel do

endif

if(p%yper)then
        
    !$omp parallel do private(i,j,k), collapse(2)
    do idz = 0, p%gz-1
    do idx = 0, p%gx-1

        do k = 1, n
        do i = 1, n
            p%of(idx,0,idz)%at(level)%dat(i,0,k) = p%of(idx,p%gy-1,idz)%at(level)%dat(i,n,k)
            p%of(idx,p%gy-1,idz)%at(level)%dat(i,n+1,k) = p%of(idx,0,idz)%at(level)%dat(i,1,k)
        enddo
        enddo

    enddo
    enddo
    !$omp end parallel do

endif

if(p%zper)then
        
    !$omp parallel do private(i,j,k), collapse(2)
    do idy = 0, p%gy-1
    do idx = 0, p%gx-1

        do j = 1, n
        do i = 1, n
            p%of(idx,idy,0)%at(level)%dat(i,j,0) = p%of(idx,idy,p%gz-1)%at(level)%dat(i,j,n)
            p%of(idx,idy,p%gz-1)%at(level)%dat(i,j,n+1) = p%of(idx,idy,0)%at(level)%dat(i,j,1)
        enddo
        enddo

    enddo
    enddo
    !$omp end parallel do

endif

call system_clock(cpuend)
p%cputime = p%cputime + real(cpuend-cpustart,kind=8)/real(p%cpurate,kind=8)

end subroutine

subroutine ptrpart_reset(p)
implicit none
class(pointer_parent) :: p

p%cputime=0.0d0

end subroutine

subroutine ptrvecpart_reset(p)
implicit none
class(pointer_vector_parent) :: p
integer :: comp

p%cputime=0.0d0
do comp = 1, p%num
    call p%nodes(comp)%reset
enddo

end subroutine

subroutine ptrmgpart_reset(p)
implicit none
class(pointer_mg_parent) :: p

p%cputime = 0.0d0

end subroutine

end module ptr_roots



