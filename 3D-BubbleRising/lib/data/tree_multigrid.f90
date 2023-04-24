subroutine manager_mg_setup(p)
implicit none
class(manager) :: p
integer :: i,j,k,id,level,ind,ii,jj,kk
integer :: nx,ny,nz,n,info
integer :: imax,imin,jmax,jmin,kmax,kmin
real(8) :: dx,dy,dz

    !write(*,*)"start multigrid setup"

    level = p%glb%level
    
    p%mg%n  = p%glb%threads*p%of(0)%loc%mg(level)%n
    p%mg%nx = p%glb%grid_x*p%of(0)%loc%mg(level)%nx
    p%mg%ny = p%glb%grid_y*p%of(0)%loc%mg(level)%ny
    p%mg%nz = p%glb%grid_z*p%of(0)%loc%mg(level)%nz
    
    write(*,'(A,4I6)')"Local A size",p%of(0)%loc%mg(level)%n,p%of(0)%loc%mg(level)%nx,p%of(0)%loc%mg(level)%ny,p%of(0)%loc%mg(level)%nz
    write(*,'(A,4I6)')"Global A size",p%mg%n,p%mg%nx,p%mg%ny,p%mg%nz
    
    allocate(p%mg%A(p%mg%n,p%mg%n),p%mg%AA(p%mg%n,p%mg%n),p%mg%B(p%mg%n),p%mg%ipiv(p%mg%n))
    allocate(p%mg%i(p%mg%n),p%mg%j(p%mg%n),p%mg%k(p%mg%n),p%mg%node(p%mg%nx,p%mg%ny,p%mg%nz))
    allocate(p%mg%error(p%mg%n),p%mg%sol(p%mg%n))
    
    dx = p%of(0)%loc%mg(level)%dx
    dy = p%of(0)%loc%mg(level)%dy
    dz = p%of(0)%loc%mg(level)%dz
    
    do i = 1, p%mg%n
    do j = 1, p%mg%n
        p%mg%A(i,j)=0.0d0
    enddo
    enddo
    
    imax=0; imin=10000
    jmax=0; jmin=10000
    kmax=0; kmin=10000
    do i = 1, p%mg%n    
        id = (i-1) / p%of(0)%loc%mg(level)%n
        ! global index of local A
        ind = i - id*p%of(0)%loc%mg(level)%n     
        
        ! global (i,j,k) of global A
        ii = p%of(id)%loc%mg(level)%i(ind) + p%of(0)%loc%mg(level)%nx*p%of(id)%loc%idx 
        jj = p%of(id)%loc%mg(level)%j(ind) + p%of(0)%loc%mg(level)%ny*p%of(id)%loc%idy
        kk = p%of(id)%loc%mg(level)%k(ind) + p%of(0)%loc%mg(level)%nz*p%of(id)%loc%idz
        
        p%mg%i(i) = ii
        p%mg%j(i) = jj
        p%mg%k(i) = kk
        p%mg%node(ii,jj,kk) = i
        
        p%mg%A(i,i) = -2.0d0/dx**2.0d0-2.0d0/dy**2.0d0-2.0d0/dz**2.0d0
        
        imax=max(imax,ii); imin=min(imin,ii)
        jmax=max(jmax,jj); jmin=min(jmin,jj)
        kmax=max(kmax,kk); kmin=min(kmin,kk)
    end do
    
    write(*,'(A15,2I10)')"I matrix",imin,imax
    write(*,'(A15,2I10)')"J matrix",jmin,jmax
    write(*,'(A15,2I10)')"K matrix",kmin,kmax
    
    do i = 1, p%mg%n
    
        if(p%mg%i(i)>1)then
            p%mg%A(i, p%mg%node(p%mg%i(i)-1,p%mg%j(i),p%mg%k(i)) ) = 1.0d0/dx**2.0d0
        else
            if(p%glb%xper)then
                p%mg%A(i, p%mg%node(imax,p%mg%j(i),p%mg%k(i)) ) = 1.0d0/dx**2.0d0
            else
                p%mg%A(i,i) = p%mg%A(i,i) + 1.0d0/dx**2.0d0
            endif
        endif
        
        if(p%mg%i(i)<p%mg%nx)then
            p%mg%A(i, p%mg%node(p%mg%i(i)+1,p%mg%j(i),p%mg%k(i)) ) = 1.0d0/dx**2.0d0
        else
            if(p%glb%xper)then
                p%mg%A(i, p%mg%node(1,p%mg%j(i),p%mg%k(i)) ) = 1.0d0/dx**2.0d0
            else
                p%mg%A(i,i) = p%mg%A(i,i) + 1.0d0/dx**2.0d0
            endif
        endif
        
        if(p%mg%j(i)>1)then
            p%mg%A(i, p%mg%node(p%mg%i(i),p%mg%j(i)-1,p%mg%k(i)) ) = 1.0d0/dy**2.0d0
        else
            if(p%glb%yper)then
                p%mg%A(i, p%mg%node(p%mg%i(i),jmax,p%mg%k(i)) ) = 1.0d0/dy**2.0d0
            else
                p%mg%A(i,i) = p%mg%A(i,i) + 1.0d0/dy**2.0d0
            endif
        endif
        
        if(p%mg%j(i)<p%mg%ny)then
            p%mg%A(i, p%mg%node(p%mg%i(i),p%mg%j(i)+1,p%mg%k(i)) ) = 1.0d0/dy**2.0d0
        else
            if(p%glb%yper)then
                p%mg%A(i, p%mg%node(p%mg%i(i),1,p%mg%k(i)) ) = 1.0d0/dy**2.0d0
            else
                p%mg%A(i,i) = p%mg%A(i,i) + 1.0d0/dy**2.0d0
            endif
        endif   

        if(p%mg%k(i)>1)then
            p%mg%A(i, p%mg%node(p%mg%i(i),p%mg%j(i),p%mg%k(i)-1) ) = 1.0d0/dz**2.0d0
        else
            if(p%glb%zper)then
                p%mg%A(i, p%mg%node(p%mg%i(i),p%mg%j(i),kmax) ) = 1.0d0/dz**2.0d0
            else
                p%mg%A(i,i) = p%mg%A(i,i) + 1.0d0/dz**2.0d0
            endif
        endif
        
        if(p%mg%k(i)<p%mg%nz)then
            p%mg%A(i, p%mg%node(p%mg%i(i),p%mg%j(i),p%mg%k(i)+1) ) = 1.0d0/dz**2.0d0
        else
            if(p%glb%zper)then
                p%mg%A(i, p%mg%node(p%mg%i(i),p%mg%j(i),1) ) = 1.0d0/dz**2.0d0
            else
                p%mg%A(i,i) = p%mg%A(i,i) + 1.0d0/dz**2.0d0
            endif
        endif   
        
    end do

    ! do i = 1, p%mg%n
    !     write(*,'("(",I2,",",I2,",",I2,")  ",20F7.2)')p%mg%i(i),p%mg%j(i),p%mg%k(i),p%mg%A(i,:)
    ! enddo
    
    end subroutine

subroutine manager_mg_solve_exact(p,show)
implicit none
class(manager) :: p
integer :: i,j,k,id,level
integer :: ii,jj,kk,ind,info
real(8) :: error, tmp
logical :: show

level = p%glb%level

do i = 1, p%mg%n
    id =  (i-1) / p%of(0)%loc%mg(level)%n
    ind = i - id*p%of(0)%loc%mg(level)%n
    ii = p%of(id)%loc%mg(level)%i(ind)
    jj = p%of(id)%loc%mg(level)%j(ind)
    kk = p%of(id)%loc%mg(level)%k(ind)
    p%mg%B(i) = p%of(id)%loc%mg(level)%src(ii,jj,kk)
enddo

do i = 1, p%mg%n
do j = 1, p%mg%n
    p%mg%AA(i,j) = p%mg%A(i,j)
end do
end do

call dgesv(p%mg%n,1,p%mg%AA,p%mg%n,p%mg%ipiv,p%mg%B,p%mg%n,info)
if(info.ne.0)stop "Something Wrong with Multigrid Solver!!"

do i = 1, p%mg%n
    id = (i-1) / p%of(0)%loc%mg(level)%n
    ind = i - id*p%of(0)%loc%mg(level)%n
    ii = p%of(id)%loc%mg(level)%i(ind)
    jj = p%of(id)%loc%mg(level)%j(ind)
    kk = p%of(id)%loc%mg(level)%k(ind)
    p%of(id)%loc%mg(level)%sol(ii,jj,kk) = p%mg%B(i) 
enddo

call p%mg_find_error(show)

end subroutine

subroutine manager_mg_find_error(p,show)
class(manager) :: p
integer :: i,j,k,id,level
integer :: ii,jj,kk,ind,info
real(8) :: error, tmp
logical :: show

level = p%glb%level

do i = 1, p%mg%n
    id = (i-1) / p%of(0)%loc%mg(level)%n
    ind = i - id*p%of(0)%loc%mg(level)%n
    ii = p%of(id)%loc%mg(level)%i(ind)
    jj = p%of(id)%loc%mg(level)%j(ind)
    kk = p%of(id)%loc%mg(level)%k(ind)
    p%mg%sol(i) = p%of(id)%loc%mg(level)%sol(ii,jj,kk)
enddo

error=0.0d0
do i = 1, p%mg%n
    id = (i-1) / p%of(0)%loc%mg(level)%n
    ind = i - id*p%of(0)%loc%mg(level)%n
    ii = p%of(id)%loc%mg(level)%i(ind)
    jj = p%of(id)%loc%mg(level)%j(ind)
    kk = p%of(id)%loc%mg(level)%k(ind) 
    tmp= p%of(id)%loc%mg(level)%src(ii,jj,kk)
    do j = 1, p%mg%n
        tmp = tmp - p%mg%A(i,j)*p%mg%sol(j)
    enddo
    p%mg%error(i) = tmp
    error = max(error,abs(tmp))
enddo

if(show)write(*,*)"Exact solver (LAPACK) error:",error
if(error>1.0d-10)call p%mg_solve_correct

end subroutine

subroutine manager_mg_solve_correct(p)
implicit none
class(manager) :: p
integer :: i,j,k,id,level
integer :: ii,jj,kk,ind,info

level = p%glb%level

do i = 1, p%mg%n
do j = 1, p%mg%n
    p%mg%AA(i,j) = p%mg%A(i,j)
end do
end do

call dgesv(p%mg%n,1,p%mg%AA,p%mg%n,p%mg%ipiv,p%mg%error,p%mg%n,info)
if(info.ne.0)stop "Something Wrong with Multigrid Solver!!"

do i = 1, p%mg%n
    id = (i-1) / p%of(0)%loc%mg(level)%n
    ind = i - id*p%of(0)%loc%mg(level)%n
    ii = p%of(id)%loc%mg(level)%i(ind)
    jj = p%of(id)%loc%mg(level)%j(ind)
    kk = p%of(id)%loc%mg(level)%k(ind)
    p%of(id)%loc%mg(level)%sol(ii,jj,kk) = p%of(id)%loc%mg(level)%sol(ii,jj,kk) + p%mg%error(i) 
enddo

end subroutine