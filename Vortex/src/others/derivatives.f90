subroutine surface_norms
use all
!$ use omp_lib
implicit none
integer :: id,I,J,k,lid

    !$omp parallel do private(i,j,k,lid)
    do id = 0, p%glb%threads-1
        
        !$omp parallel do num_threads(p%glb%nthreads) collapse(2) private(i,j,k,lid)
        do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc
        do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
            
            lid = 0
            !$ lid = OMP_GET_THREAD_NUM()

            call p%of(id)%loc%ccdsolvers(lid)%x%solve("ccd", p%of(id)%loc%phi%now(:,j,k),&
                        &p%of(id)%loc%normals%x%now(:,j,k),p%of(id)%loc%normals%xx%now(:,j,k) )
                                                                    
        enddo
        enddo
        !$omp end parallel do
        
        !$omp parallel do num_threads(p%glb%nthreads) collapse(2) private(i,j,k,lid)
        do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc
        do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
            
            lid = 0
            !$ lid = OMP_GET_THREAD_NUM()

            call p%of(id)%loc%ccdsolvers(lid)%y%solve("ccd", p%of(id)%loc%phi%now(i,:,k),&
                        &p%of(id)%loc%normals%y%now(i,:,k),p%of(id)%loc%normals%yy%now(i,:,k) )
                                                                            
        enddo
        enddo
        !$omp end parallel do
        
        !$omp parallel do num_threads(p%glb%nthreads) collapse(2) private(i,j,k,lid)
        do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
        do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
            
            lid = 0
            !$ lid = OMP_GET_THREAD_NUM()

            call p%of(id)%loc%ccdsolvers(lid)%z%solve("ccd", p%of(id)%loc%phi%now(i,j,:),&
                        &p%of(id)%loc%normals%z%now(i,j,:),p%of(id)%loc%normals%zz%now(i,j,:) )
                                                                            
        enddo
        enddo
        !$omp end parallel do
                
        !===========================================
        
        !$omp parallel do num_threads(p%glb%nthreads) collapse(2) private(i,j,k,lid)
        do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc
        do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
            
            lid = 0
            !$ lid = OMP_GET_THREAD_NUM()

            call p%of(id)%loc%ccdsolvers(lid)%y%solve("ccd", p%of(id)%loc%normals%x%now(i,:,k),&
                            &p%of(id)%loc%normals%xy%now(i,:,k),p%of(id)%loc%normals%curv%now(i,:,k) )
                                                                            
        enddo
        enddo
        !$omp end parallel do
        
        !$omp parallel do num_threads(p%glb%nthreads) collapse(2) private(i,j,k,lid)
        do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
        do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
            
            lid = 0
            !$ lid = OMP_GET_THREAD_NUM()

            call p%of(id)%loc%ccdsolvers(lid)%z%solve("ccd", p%of(id)%loc%normals%x%now(i,j,:),&
                            &p%of(id)%loc%normals%xz%now(i,j,:),p%of(id)%loc%normals%curv%now(i,j,:) )
                                                                            
        enddo
        enddo 
        !$omp end parallel do
        
        !$omp parallel do num_threads(p%glb%nthreads) collapse(2) private(i,j,k,lid)
        do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
        do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
            
            lid = 0
            !$ lid = OMP_GET_THREAD_NUM()

            call p%of(id)%loc%ccdsolvers(lid)%z%solve("ccd", p%of(id)%loc%normals%y%now(i,j,:),&
                            &p%of(id)%loc%normals%yz%now(i,j,:),p%of(id)%loc%normals%curv%now(i,j,:) )
                                                                            
        enddo
        enddo
        !$omp end parallel do

        !===========================================
        
        call p%of(id)%bc(0,p%of(id)%loc%normals%x%now)
        call p%of(id)%bc(0,p%of(id)%loc%normals%xx%now)
        
        call p%of(id)%bc(0,p%of(id)%loc%normals%y%now)
        call p%of(id)%bc(0,p%of(id)%loc%normals%yy%now)
       
        call p%of(id)%bc(0,p%of(id)%loc%normals%z%now)
        call p%of(id)%bc(0,p%of(id)%loc%normals%zz%now)
         
        call p%of(id)%bc(0,p%of(id)%loc%normals%xy%now)
        call p%of(id)%bc(0,p%of(id)%loc%normals%xz%now)
        call p%of(id)%bc(0,p%of(id)%loc%normals%yz%now)
     
    enddo       
    !$omp end parallel do

call pt%normals%sync
    
end subroutine

subroutine surface_norms_sec
use all
implicit none
integer :: id,I,J,k  

!$omp parallel do private(i,j,k)
do id = 0, p%glb%threads-1
    
    !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k)
    do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    do j = p%of(id)%loc%js, p%of(id)%loc%je
    do i = p%of(id)%loc%is, p%of(id)%loc%ie
    
        p%of(id)%loc%normals%x%now(i,j,k) = 0.5d0*( p%of(id)%loc%phi%now(i+1,j,k) - p%of(id)%loc%phi%now(i-1,j,k) )/p%glb%dx
        p%of(id)%loc%normals%y%now(i,j,k) = 0.5d0*( p%of(id)%loc%phi%now(i,j+1,k) - p%of(id)%loc%phi%now(i,j-1,k) )/p%glb%dy
        p%of(id)%loc%normals%z%now(i,j,k) = 0.5d0*( p%of(id)%loc%phi%now(i,j,k+1) - p%of(id)%loc%phi%now(i,j,k-1) )/p%glb%dz
    
        p%of(id)%loc%normals%xx%now(i,j,k) = ( p%of(id)%loc%phi%now(i+1,j,k) - 2.0d0*p%of(id)%loc%phi%now(i,j,k) - p%of(id)%loc%phi%now(i-1,j,k) )/p%glb%dx**2.0d0
        p%of(id)%loc%normals%yy%now(i,j,k) = ( p%of(id)%loc%phi%now(i,j+1,k) - 2.0d0*p%of(id)%loc%phi%now(i,j,k) - p%of(id)%loc%phi%now(i,j-1,k) )/p%glb%dy**2.0d0
        p%of(id)%loc%normals%zz%now(i,j,k) = ( p%of(id)%loc%phi%now(i,j,k+1) - 2.0d0*p%of(id)%loc%phi%now(i,j,k) - p%of(id)%loc%phi%now(i,j,k-1) )/p%glb%dz**2.0d0
        
        p%of(id)%loc%normals%xy%now(i,j,k) = ( p%of(id)%loc%phi%now(i+1,j+1,k)+p%of(id)%loc%phi%now(i-1,j-1,k) &
                                           & - p%of(id)%loc%phi%now(i-1,j+1,k)-p%of(id)%loc%phi%now(i+1,j-1,k) )/(4.0d0*p%glb%dx*p%glb%dy)
                                           
        p%of(id)%loc%normals%xz%now(i,j,k) = ( p%of(id)%loc%phi%now(i+1,j,k+1)+p%of(id)%loc%phi%now(i-1,j,k-1) &
                                           & - p%of(id)%loc%phi%now(i-1,j,k+1)-p%of(id)%loc%phi%now(i+1,j,k-1) )/(4.0d0*p%glb%dx*p%glb%dz)
                                           
        p%of(id)%loc%normals%yz%now(i,j,k) = ( p%of(id)%loc%phi%now(i,j+1,k+1)+p%of(id)%loc%phi%now(i,j-1,k-1) &
                                           & - p%of(id)%loc%phi%now(i,j+1,k-1)-p%of(id)%loc%phi%now(i,j-1,k+1) )/(4.0d0*p%glb%dz*p%glb%dy)
    end do
    end do
    end do
    !$omp end parallel do

    !===========================================
    
    call p%of(id)%bc(0,p%of(id)%loc%normals%x%now)
    call p%of(id)%bc(0,p%of(id)%loc%normals%xx%now)
    
    call p%of(id)%bc(0,p%of(id)%loc%normals%y%now)
    call p%of(id)%bc(0,p%of(id)%loc%normals%yy%now)
   
    call p%of(id)%bc(0,p%of(id)%loc%normals%z%now)
    call p%of(id)%bc(0,p%of(id)%loc%normals%zz%now)
     
    call p%of(id)%bc(0,p%of(id)%loc%normals%xy%now)
    call p%of(id)%bc(0,p%of(id)%loc%normals%xz%now)
    call p%of(id)%bc(0,p%of(id)%loc%normals%yz%now)
 
end do      
!$omp end parallel do

call pt%normals%sync
    
end subroutine

subroutine curv
use all
implicit none
integer :: id,i,j,k
real(8) :: fx,fxx,fy,fyy,fz,fzz,fxy,fxz,fyz

call surface_norms

!$omp parallel do private(i,j,k,fx,fxx,fy,fyy,fz,fzz,fxy,fxz,fyz)
do id = 0, p%glb%threads-1   
    
    !$omp parallel do num_threads(p%glb%nthreads) collapse(3) private(i,j,k,fx,fxx,fy,fyy,fz,fzz,fxy,fxz,fyz)
    do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    do j = p%of(id)%loc%js, p%of(id)%loc%je
    do i = p%of(id)%loc%is, p%of(id)%loc%ie
    
        fx  = p%of(id)%loc%normals%x%now(i,j,k)
        fy  = p%of(id)%loc%normals%y%now(i,j,k)
        fz  = p%of(id)%loc%normals%z%now(i,j,k)
        
        fxx = p%of(id)%loc%normals%xx%now(i,j,k)
        fyy = p%of(id)%loc%normals%yy%now(i,j,k)
        fzz = p%of(id)%loc%normals%zz%now(i,j,k)
        
        fxy = p%of(id)%loc%normals%xy%now(i,j,k)
        fxz = p%of(id)%loc%normals%xz%now(i,j,k)
        fyz = p%of(id)%loc%normals%yz%now(i,j,k)
        
        p%of(id)%loc%normals%curv%now(i,j,k) = ( (fyy+fzz)*fx**2.0d0+(fxx+fzz)*fy**2.0d0+(fxx+fyy)*fz**2.0d0 &
                                            &- 2.0d0*(fxy*fx*fy+fxz*fx*fz+fyz*fy*fz) ) / (fx**2.0d0+fy**2.0d0+fz**2.0d0+1.0d-12)**1.5d0      
    end do
    end do
    end do
    !$omp end parallel do
    
    call p%of(id)%bc(0,p%of(id)%loc%normals%curv%now)
    
enddo
!$omp end parallel do

call pt%normals%sync

end subroutine