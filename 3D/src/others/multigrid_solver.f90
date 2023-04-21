subroutine multigrid_relax(level,num)
use all
implicit none
integer :: relax_iter, num, id, level
integer :: i,j,k
real(8) :: dx,dy,dz,a

dx = p%of(0)%loc%mg(level)%dx
dy = p%of(0)%loc%mg(level)%dy
dz = p%of(0)%loc%mg(level)%dz

a = -2.0d0/dx**2.0d0 -2.0d0/dy**2.0d0 -2.0d0/dz**2.0d0
!a = -2.0d0/dx**2.0d0 -2.0d0/dy**2.0d0

do relax_iter = 1, num

    call pt%mg%sync(level) 
        
    !$omp parallel do private(i,j,k)
    do id = 0, p%glb%threads-1      
        
        do k = 1, p%of(id)%loc%mg(level)%nz
        do j = 1, p%of(id)%loc%mg(level)%ny
        do i = 1, p%of(id)%loc%mg(level)%nx
    
            ! 3D
            p%of(id)%loc%mg(level)%sol(i,j,k) = ( p%of(id)%loc%mg(level)%src(i,j,k) &
            & - (p%of(id)%loc%mg(level)%sol(i+1,j,k)+p%of(id)%loc%mg(level)%sol(i-1,j,k))/dx**2.0d0 &
            & - (p%of(id)%loc%mg(level)%sol(i,j+1,k)+p%of(id)%loc%mg(level)%sol(i,j-1,k))/dy**2.0d0 &
            & - (p%of(id)%loc%mg(level)%sol(i,j,k+1)+p%of(id)%loc%mg(level)%sol(i,j,k-1))/dz**2.0d0 ) / a
                
            ! 2D
            ! p%of(id)%loc%mg(level)%sol(i,j,k) = ( p%of(id)%loc%mg(level)%src(i,j,k) &
            ! & - (p%of(id)%loc%mg(level)%sol(i+1,j,k)+p%of(id)%loc%mg(level)%sol(i-1,j,k))/dx**2.0d0 &
            ! & - (p%of(id)%loc%mg(level)%sol(i,j+1,k)+p%of(id)%loc%mg(level)%sol(i,j-1,k))/dy**2.0d0 )/a
        enddo
        enddo
        enddo

    enddo   
    !$omp end parallel do
            
    call multigrid_compatibility_sol(level)
            
enddo
    
call pt%mg%sync(level)
    
end subroutine

subroutine multigrid_restriction(level)
use all
implicit none
integer :: id,level
integer :: i,j,k

call multigrid_relax(level,2)
call multigrid_residual(level,.false.)

!$omp parallel do private(i,j,k)
do id = 0, p%glb%threads-1      
    
    do k = 1, p%of(id)%loc%mg(level+1)%nz
    do j = 1, p%of(id)%loc%mg(level+1)%ny
    do i = 1, p%of(id)%loc%mg(level+1)%nx
    
        ! 3D
        p%of(id)%loc%mg(level+1)%src(i,j,k) = &
        &( p%of(id)%loc%mg(level)%res(2*i-1,2*j-1,2*k-1)+p%of(id)%loc%mg(level)%res(2*i,2*j-1,2*k-1)+ &
        &  p%of(id)%loc%mg(level)%res(2*i-1,2*j  ,2*k-1)+p%of(id)%loc%mg(level)%res(2*i,2*j  ,2*k-1)+ &
        &  p%of(id)%loc%mg(level)%res(2*i-1,2*j-1,2*k)  +p%of(id)%loc%mg(level)%res(2*i,2*j-1,2*k  )+ &
        &  p%of(id)%loc%mg(level)%res(2*i-1,2*j  ,2*k)  +p%of(id)%loc%mg(level)%res(2*i,2*j  ,2*k  )) / 8.0d0
        
        ! 2D
        ! p%of(id)%loc%mg(level+1)%src(i,j,k) = &
         ! &( p%of(id)%loc%mg(level)%res(2*i-1,2*j-1,2*k-1)+p%of(id)%loc%mg(level)%res(2*i,2*j-1,2*k-1)+ &
         ! &  p%of(id)%loc%mg(level)%res(2*i-1,2*j  ,2*k-1)+p%of(id)%loc%mg(level)%res(2*i,2*j  ,2*k-1)) / 4.0d0
         
        p%of(id)%loc%mg(level+1)%sol(i,j,k) = 0.0d0
        
    enddo
    enddo
    enddo

enddo    
!$omp end parallel do
        
call multigrid_compatibility_src(level+1)

end subroutine

subroutine multigrid_final_exact(show)
use all
implicit none
integer :: level
logical :: show

level=p%glb%level    
call p%mg_solve_exact(show)
call multigrid_compatibility_sol(level)
call multigrid_residual(level,.false.)
if(show)write(*,*)"Residual:",p%of(0)%loc%mg(level)%l2norm

end subroutine

subroutine multigrid_prolongation(level)
use all 
implicit none
integer :: id,level
integer :: i,j,k
real(8) :: mx,my,mz

call pt%mg%sync(level+1)
        
!$omp parallel do private(i,j,k,mx,my,mz)
do id = 0, p%glb%threads-1

    do k = 1, p%of(id)%loc%mg(level+1)%nz
    do j = 1, p%of(id)%loc%mg(level+1)%ny
    do i = 1, p%of(id)%loc%mg(level+1)%nx
    
        ! 3D
        mx = 0.5d0*( p%of(id)%loc%mg(level+1)%sol(i+1,j,k)-p%of(id)%loc%mg(level+1)%sol(i-1,j,k) )
        my = 0.5d0*( p%of(id)%loc%mg(level+1)%sol(i,j+1,k)-p%of(id)%loc%mg(level+1)%sol(i,j-1,k) )
        mz = 0.5d0*( p%of(id)%loc%mg(level+1)%sol(i,j,k+1)-p%of(id)%loc%mg(level+1)%sol(i,j,k-1) )
        p%of(id)%loc%mg(level)%pol(2*i-1,2*j-1,2*k-1) = p%of(id)%loc%mg(level+1)%sol(i,j,k) - 0.25d0*mx - 0.25d0*my - 0.25d0*mz
        p%of(id)%loc%mg(level)%pol(2*i-1,2*j  ,2*k-1) = p%of(id)%loc%mg(level+1)%sol(i,j,k) - 0.25d0*mx + 0.25d0*my - 0.25d0*mz
        p%of(id)%loc%mg(level)%pol(2*i  ,2*j-1,2*k-1) = p%of(id)%loc%mg(level+1)%sol(i,j,k) + 0.25d0*mx - 0.25d0*my - 0.25d0*mz
        p%of(id)%loc%mg(level)%pol(2*i  ,2*j  ,2*k-1) = p%of(id)%loc%mg(level+1)%sol(i,j,k) + 0.25d0*mx + 0.25d0*my - 0.25d0*mz
        p%of(id)%loc%mg(level)%pol(2*i-1,2*j-1,2*k)   = p%of(id)%loc%mg(level+1)%sol(i,j,k) - 0.25d0*mx - 0.25d0*my + 0.25d0*mz
        p%of(id)%loc%mg(level)%pol(2*i-1,2*j  ,2*k)   = p%of(id)%loc%mg(level+1)%sol(i,j,k) - 0.25d0*mx + 0.25d0*my + 0.25d0*mz
        p%of(id)%loc%mg(level)%pol(2*i  ,2*j-1,2*k)   = p%of(id)%loc%mg(level+1)%sol(i,j,k) + 0.25d0*mx - 0.25d0*my + 0.25d0*mz
        p%of(id)%loc%mg(level)%pol(2*i  ,2*j  ,2*k)   = p%of(id)%loc%mg(level+1)%sol(i,j,k) + 0.25d0*mx + 0.25d0*my + 0.25d0*mz

        ! 2D
        ! mx = 0.5d0*( p%of(id)%loc%mg(level+1)%sol(i+1,j,k)-p%of(id)%loc%mg(level+1)%sol(i-1,j,k) )
        ! my = 0.5d0*( p%of(id)%loc%mg(level+1)%sol(i,j+1,k)-p%of(id)%loc%mg(level+1)%sol(i,j-1,k) )        
        ! p%of(id)%loc%mg(level)%pol(2*i-1,2*j-1,2*k-1) = p%of(id)%loc%mg(level+1)%sol(i,j,k) - 0.25d0*mx - 0.25d0*my 
        ! p%of(id)%loc%mg(level)%pol(2*i-1,2*j  ,2*k-1) = p%of(id)%loc%mg(level+1)%sol(i,j,k) - 0.25d0*mx + 0.25d0*my 
        ! p%of(id)%loc%mg(level)%pol(2*i  ,2*j-1,2*k-1) = p%of(id)%loc%mg(level+1)%sol(i,j,k) + 0.25d0*mx - 0.25d0*my 
        ! p%of(id)%loc%mg(level)%pol(2*i  ,2*j  ,2*k-1) = p%of(id)%loc%mg(level+1)%sol(i,j,k) + 0.25d0*mx + 0.25d0*my 
        
    enddo
    enddo
    enddo

    do k = 1, p%of(id)%loc%mg(level)%nz
    do j = 1, p%of(id)%loc%mg(level)%ny
    do i = 1, p%of(id)%loc%mg(level)%nx 
        p%of(id)%loc%mg(level)%sol(i,j,k) = p%of(id)%loc%mg(level)%sol(i,j,k) + p%of(id)%loc%mg(level)%pol(i,j,k)
    enddo
    enddo
    enddo

enddo    
!$omp end parallel do

call multigrid_relax(level,2)

end subroutine

subroutine multigrid_residual(level,reset)
use all
implicit none
integer :: level,id,i,j,k,n
real(8) :: dx,dy,dz,l2norm
logical :: reset

call pt%mg%sync(level)

n = p%glb%threads * p%of(0)%loc%mg(level)%n

dx = p%of(0)%loc%mg(level)%dx
dy = p%of(0)%loc%mg(level)%dy
dz = p%of(0)%loc%mg(level)%dz
    
l2norm = 0.0d0

!$omp parallel do private(i,j,k), reduction(+:l2norm)
do id = 0, p%glb%threads-1
    
    do k = 1, p%of(id)%loc%mg(level)%nz
    do j = 1, p%of(id)%loc%mg(level)%ny
    do i = 1, p%of(id)%loc%mg(level)%nx
        ! 3D
        p%of(id)%loc%mg(level)%res(i,j,k) = p%of(id)%loc%mg(level)%src(i,j,k) &
        &   - (p%of(id)%loc%mg(level)%sol(i+1,j,k)-2.0d0*p%of(id)%loc%mg(level)%sol(i,j,k)+p%of(id)%loc%mg(level)%sol(i-1,j,k))/dx**2.0d0 &
        &   - (p%of(id)%loc%mg(level)%sol(i,j+1,k)-2.0d0*p%of(id)%loc%mg(level)%sol(i,j,k)+p%of(id)%loc%mg(level)%sol(i,j-1,k))/dy**2.0d0 &
        &   - (p%of(id)%loc%mg(level)%sol(i,j,k+1)-2.0d0*p%of(id)%loc%mg(level)%sol(i,j,k)+p%of(id)%loc%mg(level)%sol(i,j,k-1))/dz**2.0d0

        ! 2D
        ! p%of(id)%loc%mg(level)%res(i,j,k) = p%of(id)%loc%mg(level)%src(i,j,k) &
        ! &   - (p%of(id)%loc%mg(level)%sol(i+1,j,k)-2.0d0*p%of(id)%loc%mg(level)%sol(i,j,k)+p%of(id)%loc%mg(level)%sol(i-1,j,k))/dx**2.0d0 &
        ! &   - (p%of(id)%loc%mg(level)%sol(i,j+1,k)-2.0d0*p%of(id)%loc%mg(level)%sol(i,j,k)+p%of(id)%loc%mg(level)%sol(i,j-1,k))/dy**2.0d0
                                    
        L2norm = L2norm + p%of(id)%loc%mg(level)%res(i,j,k)**2.0d0
    enddo
    enddo
    enddo

enddo
!$omp end parallel do

l2norm = dsqrt( l2norm / real(n,kind=8) )
!l2norm = dsqrt( l2norm / real(nx*ny,8) )

if(reset)then
    !$omp parallel do 
    do id = 0, p%glb%threads-1
        p%of(id)%loc%mg(level)%l2norm0 = l2norm
    enddo
    !$omp end parallel do
else
    !$omp parallel do 
    do id = 0, p%glb%threads-1
        p%of(id)%loc%mg(level)%l2norm0 = p%of(id)%loc%mg(level)%l2norm
        p%of(id)%loc%mg(level)%l2norm = l2norm
    enddo
    !$omp end parallel do 
endif

end subroutine

subroutine multigrid_compatibility_src(level)
use all
implicit none
integer :: id,i,j,k,level
integer :: n
real(8) :: s

n = p%glb%threads * p%of(0)%loc%mg(level)%n

s=0.0d0
!$omp parallel do private(i,j,k), reduction(+:s)   
do id = 0, p%glb%threads-1
    
    do k = 1, p%of(id)%loc%mg(level)%nz
    do j = 1, p%of(id)%loc%mg(level)%ny
    do i = 1, p%of(id)%loc%mg(level)%nx
        s = s + p%of(id)%loc%mg(level)%src(i,j,k)
    enddo
    enddo
    enddo
 
enddo   
!$omp end parallel do

s=s/real(n,8)
!s=s/(nx*ny)

!$omp parallel do private(i,j,k)
do id = 0, p%glb%threads-1
            
    do k = 1,  p%of(id)%loc%mg(level)%nz
    do j = 1,  p%of(id)%loc%mg(level)%ny
    do i = 1,  p%of(id)%loc%mg(level)%nx    
        p%of(id)%loc%mg(level)%src(i,j,k) = p%of(id)%loc%mg(level)%src(i,j,k) - s
    enddo
    enddo
    enddo

enddo
!$omp end parallel do

end subroutine

subroutine multigrid_compatibility_sol(level)
use all
implicit none
integer :: id,i,j,k,level
integer :: n
real(8) :: s

n = p%glb%threads * p%of(0)%loc%mg(level)%n

s=0.0d0
!$omp parallel do private(i,j,k), reduction(+:s)   
do id = 0, p%glb%threads-1
    
    do k = 1, p%of(id)%loc%mg(level)%nz
    do j = 1, p%of(id)%loc%mg(level)%ny
    do i = 1, p%of(id)%loc%mg(level)%nx
        s = s + p%of(id)%loc%mg(level)%sol(i,j,k)
    enddo
    enddo
    enddo
 
enddo   
!$omp end parallel do

s=s/real(n,kind=8)
!s=s/(nx*ny)

!$omp parallel do private(i,j,k)
do id = 0, p%glb%threads-1
            
    do k = 1,  p%of(id)%loc%mg(level)%nz
    do j = 1,  p%of(id)%loc%mg(level)%ny
    do i = 1,  p%of(id)%loc%mg(level)%nx    
        p%of(id)%loc%mg(level)%sol(i,j,k) = p%of(id)%loc%mg(level)%sol(i,j,k) - s
    enddo
    enddo
    enddo

enddo
!$omp end parallel do

end subroutine

subroutine multigrid_v_cycle(show)
use all
implicit none
integer :: level
logical :: show

do level = 1, p%glb%level-1
    call multigrid_restriction(level) ! get new source
enddo

call multigrid_final_exact(show)

do level = p%glb%level-1, 1, -1
    call multigrid_prolongation(level) ! get new solution
enddo

end subroutine

subroutine multigrid_w_cycle(show)
use all
implicit none
integer :: level, level2
logical :: show

do level = 1, p%glb%level-1
    call multigrid_restriction(level) ! get new source
enddo

call multigrid_final_exact(show)

do level2 = 1, p%glb%level-2
    do level = p%glb%level-1, p%glb%level-level2, -1
        call multigrid_prolongation(level)
    enddo
    if( level2==p%glb%level-2 )exit
    do level = p%glb%level-level2, p%glb%level-1
        call multigrid_restriction(level)
    enddo
    call multigrid_final_exact(show)
enddo

do level2 = p%glb%level-2, 1, -1
    do level = p%glb%level-level2, p%glb%level-1
        call multigrid_restriction(level)
    enddo
    call multigrid_final_exact(show)
    if( level2==1 )exit
    do level = p%glb%level-1, p%glb%level-level2+1, -1
        call multigrid_prolongation(level)
    enddo
enddo

do level = p%glb%level-1, 1, -1
    call multigrid_prolongation(level) ! get new solution
enddo

end subroutine

subroutine multigrid_full_cycle_init()
use all
implicit none
integer :: level,i,j,k,id

do level = 1, p%glb%level-1
    !$omp parallel do private(i,j,k)
    do id = 0, p%glb%threads-1      
        
        do k = 1, p%of(id)%loc%mg(level+1)%nz
        do j = 1, p%of(id)%loc%mg(level+1)%ny
        do i = 1, p%of(id)%loc%mg(level+1)%nx
        
            p%of(id)%loc%mg(level+1)%src(i,j,k) = &
            &( p%of(id)%loc%mg(level)%src(2*i-1,2*j-1,2*k-1)+p%of(id)%loc%mg(level)%src(2*i,2*j-1,2*k-1)+ &
            &  p%of(id)%loc%mg(level)%src(2*i-1,2*j  ,2*k-1)+p%of(id)%loc%mg(level)%src(2*i,2*j  ,2*k-1)+ &
            &  p%of(id)%loc%mg(level)%src(2*i-1,2*j-1,2*k)  +p%of(id)%loc%mg(level)%src(2*i,2*j-1,2*k  )+ &
            &  p%of(id)%loc%mg(level)%src(2*i-1,2*j  ,2*k)  +p%of(id)%loc%mg(level)%src(2*i,2*j  ,2*k  )) / 8.0d0
             
            p%of(id)%loc%mg(level+1)%sol(i,j,k) = 0.0d0
            
        enddo
        enddo
        enddo

    enddo    
    !$omp end parallel do
enddo

!$omp parallel do private(i,j,k)
do id = 0, p%glb%threads-1
    do k = 1, p%of(id)%loc%mg(1)%nz
    do j = 1, p%of(id)%loc%mg(1)%ny
    do i = 1, p%of(id)%loc%mg(1)%nx
        p%of(id)%loc%mg(1)%sol(i,j,k)=0.0d0
    enddo
    enddo
    enddo
enddo
!$omp end parallel do 

end subroutine

subroutine multigrid_full_V_cycle(show,num)
use all
implicit none
integer :: level,level2,iter,num
logical :: show

call multigrid_full_cycle_init
call multigrid_final_exact(show)

do level = 1, p%glb%level-1

    do iter = 1, num
        do level2 = p%glb%level-1, p%glb%level-level
            call multigrid_prolongation(level2)
        enddo
        do level2 = p%glb%level-level, p%glb%level-1
            call multigrid_restriction(level2)
        enddo
        call multigrid_final_exact(show)
    enddo
    
enddo

do level = p%glb%level-1, 1, -1
    call multigrid_prolongation(level) 
enddo

end subroutine
