subroutine manager_ls_funs(p)
implicit none
class(manager) :: p
integer :: id, i, j, k
real(8) :: x, heavy, hp, pi, eps

eps = 1.0d-12
pi = dacos(-1.0_8)
    
!$omp parallel do private(i,j,k,x,heavy,hp)
do id = 0, p%glb%threads-1
    
    do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc
    do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
    do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
    
        x = p%of(id)%loc%phi%now(i,j,k) / p%glb%ls_wid
        
        if( x > 1.0_8-eps )then
            heavy = 1.0_8
            hp = 0.0_8
        else if ( x < -1.0_8+eps )then
            heavy = 0.0_8
            hp = 0.0_8
        else
            heavy = 0.5_8 * (1.0_8 + x + dsin(pi*x) / pi )
            hp = 0.5_8 * ( 1.0_8 + dcos(pi*x) ) / p%glb%ls_wid 
        endif
        
        heavy = max(min(heavy,1.0d0),0.0d0) 
        
        p%of(id)%loc%heavy%now(i,j,k) = heavy
        p%of(id)%loc%delta%now(i,j,k) = hp
        p%of(id)%loc%sign%now(i,j,k) = 2.0_8*heavy-1.0_8
        
    end do
    end do
    end do
    
enddo
!$omp end parallel  do

end subroutine

subroutine manager_rho_mu(p)
implicit none
class(manager) :: p
integer :: id,i,j,k
real(8) :: heavy

call p%ls_funs

!$omp parallel do private(i,j,k,heavy)
do id = 0, p%glb%threads-1
    
    do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc        
    do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
    do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
        
        heavy = p%of(id)%loc%vof%now(i,j,k)
        if( p%glb%method .ne. 3)heavy = p%of(id)%loc%heavy%now(i,j,k)

        !heavy = p%of(id)%loc%heavy%now(i,j,k)
        
        p%of(id)%loc%rho%now(i,j,k) = heavy + p%glb%rho_12 * (1.0_8 - heavy )
        p%of(id)%loc%mu%now(i,j,k)  = heavy + p%glb%mu_12  * (1.0_8 - heavy )
        
    end do
    end do
    end do
 
enddo       
!$omp end parallel do
    
    
end subroutine

subroutine manager_ls_mv(p)
implicit none
class(manager) :: p
integer :: id, i, j, k
real(8) :: mass, vol, rho
real(8) :: dv

dv = p%glb%dx * p%glb%dy * p%glb%dz

call p%rho_mu

!===========================  LS 

mass = 0.0_8; vol=0.0_8

!$omp parallel do private(i,j,k,rho), reduction(+:mass,vol)    
do id = 0, p%glb%threads-1
    
    do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    do j = p%of(id)%loc%js, p%of(id)%loc%je
    do i = p%of(id)%loc%is, p%of(id)%loc%ie
        
        rho = p%of(id)%loc%heavy%now(i,j,k) + p%glb%rho_12 * (1.0d0 - p%of(id)%loc%heavy%now(i,j,k))
        mass = mass + rho*p%of(id)%loc%heavy%now(i,j,k)*dv
        vol = vol + p%of(id)%loc%heavy%now(i,j,k)*dv
    
    enddo
    enddo
    enddo

enddo
!$omp end parallel do

p%glb%mass = mass
p%glb%vol = vol

!===========================  VOF 

mass = 0.0_8; vol=0.0_8

!$omp parallel do private(i,j,k,rho), reduction(+:mass,vol)    
do id = 0, p%glb%threads-1
    
    do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    do j = p%of(id)%loc%js, p%of(id)%loc%je
    do i = p%of(id)%loc%is, p%of(id)%loc%ie
        
        rho = p%of(id)%loc%vof%now(i,j,k) + p%glb%rho_12 * (1.0d0-p%of(id)%loc%vof%now(i,j,k))
        mass = mass + rho*p%of(id)%loc%vof%now(i,j,k)*dv
        vol = vol + p%of(id)%loc%vof%now(i,j,k)*dv
    
    enddo
    enddo
    enddo

enddo
!$omp end parallel do

p%glb%massv = mass
p%glb%volv = vol

call p%sync


end subroutine

subroutine manager_surface_norms(p)
implicit none
class(manager) :: p
integer :: id,I,J,k  
    
    !$omp parallel do private(i,j,k)
    do id = 0, p%glb%threads-1
        
        do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc
        do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
        
            call p%of(id)%loc%ccdsolvers%x%solve("ccd", p%of(id)%loc%phi%now(:,j,k),&
                        &p%of(id)%loc%normals%x%now(:,j,k),p%of(id)%loc%normals%xx%now(:,j,k) )
                                                                    
        enddo
        enddo
        
        do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc
        do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
        
            call p%of(id)%loc%ccdsolvers%y%solve("ccd", p%of(id)%loc%phi%now(i,:,k),&
                        &p%of(id)%loc%normals%y%now(i,:,k),p%of(id)%loc%normals%yy%now(i,:,k) )
                                                                            
        enddo
        enddo
        
        do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
        do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
        
            call p%of(id)%loc%ccdsolvers%z%solve("ccd", p%of(id)%loc%phi%now(i,j,:),&
                        &p%of(id)%loc%normals%z%now(i,j,:),p%of(id)%loc%normals%zz%now(i,j,:) )
                                                                            
        enddo
        enddo     
                
        !===========================================
        
        do k = p%of(id)%loc%ks-p%glb%ghc, p%of(id)%loc%ke+p%glb%ghc
        do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
        
            call p%of(id)%loc%ccdsolvers%y%solve("ccd", p%of(id)%loc%normals%x%now(i,:,k),&
                            &p%of(id)%loc%normals%xy%now(i,:,k),p%of(id)%loc%normals%curv%now(i,:,k) )
                                                                            
        enddo
        enddo
        
        do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
        do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
        
            call p%of(id)%loc%ccdsolvers%z%solve("ccd", p%of(id)%loc%normals%x%now(i,j,:),&
                            &p%of(id)%loc%normals%xz%now(i,j,:),p%of(id)%loc%normals%curv%now(i,j,:) )
                                                                            
        enddo
        enddo 
        
        do j = p%of(id)%loc%js-p%glb%ghc, p%of(id)%loc%je+p%glb%ghc
        do i = p%of(id)%loc%is-p%glb%ghc, p%of(id)%loc%ie+p%glb%ghc
        
            call p%of(id)%loc%ccdsolvers%z%solve("ccd", p%of(id)%loc%normals%y%now(i,j,:),&
                            &p%of(id)%loc%normals%yz%now(i,j,:),p%of(id)%loc%normals%curv%now(i,j,:) )
                                                                            
        enddo
        enddo

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
    
end subroutine

subroutine manager_surface_norms_sec(p)
implicit none
class(manager) :: p
integer :: id,I,J,k  

!$omp parallel do private(i,j,k)
do id = 0, p%glb%threads-1
    
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
    
end subroutine

subroutine manager_curv(p)
implicit none
class(manager) :: p
integer :: id,i,j,k
real(8) :: fx,fxx,fy,fyy,fz,fzz,fxy,fxz,fyz

call p%surface_norms

!$omp parallel do private(i,j,k,fx,fxx,fy,fyy,fz,fzz,fxy,fxz,fyz)
do id = 0, p%glb%threads-1   
    
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
    
    call p%of(id)%bc(0,p%of(id)%loc%normals%curv%now)
    
enddo
!$omp end parallel do

end subroutine