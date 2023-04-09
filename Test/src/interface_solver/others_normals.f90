subroutine surface_norms()
use all
implicit none
integer :: id,I,J,k  

!$omp parallel do collapse(2)
do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc

    call p%loc%ccdsolvers%x%solve("ccd",p%loc%phi%now(:,j,k),p%loc%normals%x%now(:,j,k),p%loc%normals%xx%now(:,j,k))
                                                            
enddo
enddo
!$omp end parallel do

!$omp parallel do collapse(2)
do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc

    call p%loc%ccdsolvers%y%solve("ccd",p%loc%phi%now(i,:,k),p%loc%normals%y%now(i,:,k),p%loc%normals%yy%now(i,:,k))
                                                                    
enddo
enddo
!$omp end parallel do

!$omp parallel do collapse(2)
do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc

    call p%loc%ccdsolvers%z%solve("ccd",p%loc%phi%now(i,j,:),p%loc%normals%z%now(i,j,:),p%loc%normals%zz%now(i,j,:))
                                                                    
enddo
enddo   
!$omp end parallel do    
        
!===========================================

!$omp parallel do collapse(2)
do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc

    call p%loc%ccdsolvers%y%solve("ccd",p%loc%normals%x%now(i,:,k),p%loc%normals%xy%now(i,:,k),p%loc%normals%curv%now(i,:,k))
                                                                    
enddo
enddo
!$omp end parallel do

!$omp parallel do collapse(2)
do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc

    call p%loc%ccdsolvers%z%solve("ccd",p%loc%normals%x%now(i,j,:),p%loc%normals%xz%now(i,j,:),p%loc%normals%curv%now(i,j,:))
                                                                    
enddo
enddo 
!$omp end parallel do

!$omp parallel do collapse(2)
do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc

    call p%loc%ccdsolvers%z%solve("ccd",p%loc%normals%y%now(i,j,:),p%loc%normals%yz%now(i,j,:),p%loc%normals%curv%now(i,j,:))
                                                                    
enddo
enddo
!$omp end parallel do

!===========================================

call bc(p%loc%normals%x%now)
call bc(p%loc%normals%xx%now)

call bc(p%loc%normals%y%now)
call bc(p%loc%normals%yy%now)

call bc(p%loc%normals%z%now)
call bc(p%loc%normals%zz%now)
 
call bc(p%loc%normals%xy%now)
call bc(p%loc%normals%xz%now)
call bc(p%loc%normals%yz%now)

end subroutine

subroutine surface_norms2()
use all
implicit none
integer :: id,I,J,k  

!$omp parallel do collapse(3)
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie

    p%loc%normals%x%now(i,j,k) = 0.5d0*( p%loc%phi%now(i+1,j,k) - p%loc%phi%now(i-1,j,k) )/p%glb%dx
    p%loc%normals%y%now(i,j,k) = 0.5d0*( p%loc%phi%now(i,j+1,k) - p%loc%phi%now(i,j-1,k) )/p%glb%dy
    p%loc%normals%z%now(i,j,k) = 0.5d0*( p%loc%phi%now(i,j,k+1) - p%loc%phi%now(i,j,k-1) )/p%glb%dz

    p%loc%normals%xx%now(i,j,k) = ( p%loc%phi%now(i+1,j,k) - 2.0d0*p%loc%phi%now(i,j,k) - p%loc%phi%now(i-1,j,k) )/p%glb%dx**2.0d0
    p%loc%normals%yy%now(i,j,k) = ( p%loc%phi%now(i,j+1,k) - 2.0d0*p%loc%phi%now(i,j,k) - p%loc%phi%now(i,j-1,k) )/p%glb%dy**2.0d0
    p%loc%normals%zz%now(i,j,k) = ( p%loc%phi%now(i,j,k+1) - 2.0d0*p%loc%phi%now(i,j,k) - p%loc%phi%now(i,j,k-1) )/p%glb%dz**2.0d0
    
    p%loc%normals%xy%now(i,j,k) = ( p%loc%phi%now(i+1,j+1,k)+p%loc%phi%now(i-1,j-1,k) &
                                       & - p%loc%phi%now(i-1,j+1,k)-p%loc%phi%now(i+1,j-1,k) )/(4.0d0*p%glb%dx*p%glb%dy)
                                       
    p%loc%normals%xz%now(i,j,k) = ( p%loc%phi%now(i+1,j,k+1)+p%loc%phi%now(i-1,j,k-1) &
                                       & - p%loc%phi%now(i-1,j,k+1)-p%loc%phi%now(i+1,j,k-1) )/(4.0d0*p%glb%dx*p%glb%dz)
                                       
    p%loc%normals%yz%now(i,j,k) = ( p%loc%phi%now(i,j+1,k+1)+p%loc%phi%now(i,j-1,k-1) &
                                       & - p%loc%phi%now(i,j+1,k-1)-p%loc%phi%now(i,j-1,k+1) )/(4.0d0*p%glb%dz*p%glb%dy)
end do
end do
end do
!$omp end parallel do

!===========================================

call bc(p%loc%normals%x%now)
call bc(p%loc%normals%xx%now)

call bc(p%loc%normals%y%now)
call bc(p%loc%normals%yy%now)

call bc(p%loc%normals%z%now)
call bc(p%loc%normals%zz%now)
 
call bc(p%loc%normals%xy%now)
call bc(p%loc%normals%xz%now)
call bc(p%loc%normals%yz%now)
    
end subroutine

subroutine curv()
use all
implicit none
integer :: id,i,j,k
real(8) :: fx,fxx,fy,fyy,fz,fzz,fxy,fxz,fyz

call surface_norms

!$omp parallel do collapse(3), private(fx,fxx,fy,fyy,fz,fzz,fxy,fxz,fyz)
do k = p%loc%ks, p%loc%ke
do j = p%loc%js, p%loc%je
do i = p%loc%is, p%loc%ie

    fx  = p%loc%normals%x%now(i,j,k)
    fy  = p%loc%normals%y%now(i,j,k)
    fz  = p%loc%normals%z%now(i,j,k)
    
    fxx = p%loc%normals%xx%now(i,j,k)
    fyy = p%loc%normals%yy%now(i,j,k)
    fzz = p%loc%normals%zz%now(i,j,k)
    
    fxy = p%loc%normals%xy%now(i,j,k)
    fxz = p%loc%normals%xz%now(i,j,k)
    fyz = p%loc%normals%yz%now(i,j,k)
    
    p%loc%normals%curv%now(i,j,k) = ( (fyy+fzz)*fx**2.0d0+(fxx+fzz)*fy**2.0d0+(fxx+fyy)*fz**2.0d0 &
                                        &- 2.0d0*(fxy*fx*fy+fxz*fx*fz+fyz*fy*fz) ) / (fx**2.0d0+fy**2.0d0+fz**2.0d0+1.0d-12)**1.5d0      
end do
end do
end do
!$omp end parallel do

call bc(p%loc%normals%curv%now)
    
end subroutine
