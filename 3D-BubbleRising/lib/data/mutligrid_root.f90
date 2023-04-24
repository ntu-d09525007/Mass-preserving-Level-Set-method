module mutligrid_root

type multigrid_root
integer :: nx, ny, nz, n
integer :: idx, idy, idz, gx, gy, gz
real(8)  :: dx, dy, dz
real(8), dimension(:,:,:), allocatable :: sol, res, pol, src
real(8), dimension(:,:,:), allocatable :: node, node2
real(8), dimension(:), allocatable :: i, j, k
real(8) :: L2norm, L2norm0
contains
procedure :: init => mg_init
end type multigrid_root

contains

subroutine mg_init(p)
implicit none
class(multigrid_root) :: p
integer :: i,j,k

allocate(p%sol(0:p%nx+1,0:p%ny+1,0:p%nz+1), p%src(0:p%nx+1,0:p%ny+1,0:p%nz+1), &
        &p%res(0:p%nx+1,0:p%ny+1,0:p%nz+1), p%pol(0:p%nx+1,0:p%ny+1,0:p%nz+1) )
            
p%n = p%nx * p%ny * p%nz
allocate( p%node(p%nx,p%ny,p%nz), p%i(p%n), p%j(p%n), p%k(p%n))

do k = 1, p%nz
do j = 1, p%ny
do i = 1, p%nx
    p%node(i,j,k) = (k-1)*p%nx*p%ny + (j-1)*p%nx + i
    p%i(p%node(i,j,k)) = i
    p%j(p%node(i,j,k)) = j
    p%k(p%node(i,j,k)) = k
enddo
enddo
enddo

end subroutine

end module mutligrid_root
