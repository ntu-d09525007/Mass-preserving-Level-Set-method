subroutine job_vel_bc(p,u,v,w)
! doi.org/10.1063/1.1761178
implicit none
class(job) :: p
integer :: i,j,k
real(8), dimension(p%loc%is-p%glb%ghc:p%loc%ie+p%glb%ghc,&
                  &p%loc%js-p%glb%ghc:p%loc%je+p%glb%ghc,&
                  &p%loc%ks-p%glb%ghc:p%loc%ke+p%glb%ghc) :: u,v,w
real(8) :: src

!==========================================
!  X-direction velocity boundary condition
!==========================================

if( p%loc%idx == 0 .and. .not. p%glb%xper )then

    if( p%glb%ubc(1) == 1 )then
    
        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
        do i = 1, p%glb%ghc
            u(p%loc%is-i,j,k) = - u(p%loc%is-2+i,j,k)
            v(p%loc%is-i,j,k) = - v(p%loc%is-1+i,j,k)
            w(p%loc%is-i,j,k) = - w(p%loc%is-1+i,j,k)
        end do  
        u(p%loc%is-1,j,k) = 0.0d0
        end do
        end do
    
    else if ( p%glb%ubc(1) == 2)then

        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
        do i = 1, p%glb%ghc
            u(p%loc%is-i,j,k) = u(p%loc%is-2+i,j,k)
            v(p%loc%is-i,j,k) = v(p%loc%is-1+i,j,k)
            w(p%loc%is-i,j,k) = w(p%loc%is-1+i,j,k)
        end do  
        u(p%loc%is-1,j,k) = 0.0d0
        end do
        end do

    else if ( p%glb%ubc(1) == 3)then

        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc

            src = p%loc%vel%x%old(p%loc%is-1,j,k)*(u(p%loc%is+1,j,k)-u(p%loc%is,j,k))/p%glb%dx*p%glb%dt

            u(p%loc%is-1,j,k) = p%loc%vel%x%old(p%loc%is-1,j,k) - src

            do i = 2, p%glb%ghc
                u(p%loc%is-i,j,k) = 2.0d0*u(p%loc%is-i+1,j,k)-u(p%loc%is-i+2,j,k)
            end do

            do i = 1, p%glb%ghc
                v(p%loc%is-i,j,k) = - v(p%loc%is-1+i,j,k)
                w(p%loc%is-i,j,k) = - w(p%loc%is-1+i,j,k)
            end do

        end do
        end do
                
    endif

endif

if ( p%loc%idx == p%glb%grid_x-1 .and. .not. p%glb%xper )then

     if( p%glb%ubc(2) == 1 )then
    
        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
        do i = 1, p%glb%ghc
            u(p%loc%ie+i,j,k) = - u(p%loc%ie  -i,j,k)
            v(p%loc%ie+i,j,k) = - v(p%loc%ie+1-i,j,k)
            w(p%loc%ie+i,j,k) = - w(p%loc%ie+1-i,j,k)
        end do  
        u(p%loc%ie,j,k) = 0.0d0
        end do
        end do
    
    else if ( p%glb%ubc(2) == 2)then

        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
        do i = 1, p%glb%ghc
            u(p%loc%ie+i,j,k) = u(p%loc%ie  -i,j,k)
            v(p%loc%ie+i,j,k) = v(p%loc%ie+1-i,j,k)
            w(p%loc%ie+i,j,k) = w(p%loc%ie+1-i,j,k)
        end do  
        u(p%loc%ie,j,k) = 0.0d0
        end do
        end do

    else if ( p%glb%ubc(2) == 3)then

        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
            
            src = p%loc%vel%x%old(p%loc%ie,j,k)*(u(p%loc%ie-1,j,k)-u(p%loc%ie-2,j,k))/p%glb%dx*p%glb%dt

            u(p%loc%ie,j,k) = p%loc%vel%x%old(p%loc%ie,j,k) - src

            do i = 1, p%glb%ghc
                u(p%loc%ie+i,j,k) = 2.0d0*u(p%loc%ie+i-1,j,k)-u(p%loc%ie+i-2,j,k)
                v(p%loc%ie+i,j,k) = - v(p%loc%ie+1-i,j,k)
                w(p%loc%ie+i,j,k) = - w(p%loc%ie+1-i,j,k)
            end do

        end do 
        end do 
                
    endif
        
endif

!==========================================
!  Y-direction velocity boundary condition
!==========================================

if( p%loc%idy==0 .and. .not. p%glb%yper )then

    if( p%glb%vbc(1) == 1 )then
    
        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
        do j = 1, p%glb%ghc
            u(i,p%loc%js-j,k) = - u(i,p%loc%js-1+j,k)
            v(i,p%loc%js-j,k) = - v(i,p%loc%js-2+j,k)
            w(i,p%loc%js-j,k) = - w(i,p%loc%js-1+j,k)
        enddo
        v(i,p%loc%js-1,k) = 0.0d0
        enddo
        enddo
            
    else if ( p%glb%vbc(1) == 2 )then

        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
        do j = 1, p%glb%ghc
            u(i,p%loc%js-j,k) = u(i,p%loc%js-1+j,k)
            v(i,p%loc%js-j,k) = v(i,p%loc%js-2+j,k)
            w(i,p%loc%js-j,k) = w(i,p%loc%js-1+j,k)
        enddo
        v(i,p%loc%js-1,k) = 0.0d0
        enddo
        enddo

    else if ( p%glb%vbc(1) == 3 )then

        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc

            src = p%loc%vel%y%old(i,p%loc%js-1,k)*(v(i,p%loc%js+1,k)-v(i,p%loc%js,k))/p%glb%dy*p%glb%dt

            v(i,p%loc%js-1,k) = p%loc%vel%y%old(i,p%loc%js-1,k) - src

            do j = 2, p%glb%ghc
                v(i,p%loc%js-j,k) = 2.0d0*v(i,p%loc%js-j+1,k) - v(i,p%loc%js-j+2,k)
            enddo

            do j = 1, p%glb%ghc
                u(i,p%loc%js-j,k) = - u(i,p%loc%js-1+j,k)
                w(i,p%loc%js-j,k) = - w(i,p%loc%js-1+j,k)
            enddo

        enddo
        enddo

    endif

endif
    
if (p%loc%idy==p%glb%grid_y-1 .and. .not. p%glb%yper )then

    if( p%glb%vbc(2) == 1 )then
    
        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
        do j = 1, p%glb%ghc
            u(i,p%loc%je+j,k) = - u(i,p%loc%je+1-j,k)
            v(i,p%loc%je+j,k) = - v(i,p%loc%je-j,k)
            w(i,p%loc%je+j,k) = - w(i,p%loc%je+1-j,k)
        enddo
        v(i,p%loc%je,k) = 0.0d0
        enddo
        enddo
            
    else if ( p%glb%vbc(2) == 2 )then

        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
        do j = 1, p%glb%ghc
            u(i,p%loc%je+j,k) = u(i,p%loc%je+1-j,k)
            v(i,p%loc%je+j,k) = v(i,p%loc%je-j,k)
            w(i,p%loc%je+j,k) = w(i,p%loc%je+1-j,k)
        enddo
        v(i,p%loc%je,k) = 0.0d0
        enddo
        enddo

    else if ( p%glb%vbc(2) == 3 )then

        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc

            src = p%loc%vel%y%old(i,p%loc%je,k)*(v(i,p%loc%je-1,k)-v(i,p%loc%je-2,k))/p%glb%dy*p%glb%dt

            v(i,p%loc%je,k) = p%loc%vel%y%old(i,p%loc%je,k) - src

            do j = 1, p%glb%ghc
                u(i,p%loc%je+j,k) = - u(i,p%loc%je+1-j,k)
                v(i,p%loc%je+j,k) = 2.0d0*v(i,p%loc%je+j-1,k) - v(i,p%loc%je+k-2,k)
                w(i,p%loc%je+j,k) = - w(i,p%loc%je+1-j,k)
            enddo

        enddo
        enddo

    endif
    
endif

!==========================================
!  Z-direction velocity boundary condition
!==========================================

if( p%loc%idz==0 .and. .not. p%glb%zper )then

    if( p%glb%wbc(1) == 1 )then
    
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
        do k = 1, p%glb%ghc
            u(i,j,p%loc%ks-k) = - u(i,j,p%loc%ks-1+k)
            v(i,j,p%loc%ks-k) = - v(i,j,p%loc%ks-1+k)
            w(i,j,p%loc%ks-k) = - w(i,j,p%loc%ks-2+k)
        enddo
        w(i,j,p%loc%ks-1)=0.0d0
        enddo
        enddo
            
    else if ( p%glb%wbc(1) == 2 )then

        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
        do k = 1, p%glb%ghc
            u(i,j,p%loc%ks-k) = u(i,j,p%loc%ks-1+k)
            v(i,j,p%loc%ks-k) = v(i,j,p%loc%ks-1+k)
            w(i,j,p%loc%ks-k) = w(i,j,p%loc%ks-2+k)
        enddo
        w(i,j,p%loc%ks-1)=0.0d0
        enddo
        enddo

    else if ( p%glb%wbc(1) == 3 )then

        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc

            src = p%loc%vel%z%old(i,j,p%loc%ks-1)*(w(i,j,p%loc%ks+1)-w(i,j,p%loc%ks))/p%glb%dz*p%glb%dt

            w(i,j,p%loc%ks-1) = p%loc%vel%z%old(i,j,p%loc%ks-1) - src

            do k = 2, p%glb%ghc
                w(i,j,p%loc%ks-k) = 2.0d0*w(i,j,p%loc%ks-k+1) - w(i,j,p%loc%ks-k+2)
            enddo

            do k = 1, p%glb%ghc
                u(i,j,p%loc%ks-k) = - u(i,j,p%loc%ks-1+k)
                v(i,j,p%loc%ks-k) = - v(i,j,p%loc%ks-1+k)
            enddo

        enddo
        enddo
                                
    endif

endif

if (p%loc%idz==p%glb%grid_z-1 .and. .not. p%glb%zper )then

    if( p%glb%wbc(2) == 1 )then
    
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
        do k = 1, p%glb%ghc
            u(i,j,p%loc%ke+k) = - u(i,j,p%loc%ke+1-k)
            v(i,j,p%loc%ke+k) = - v(i,j,p%loc%ke+1-k)
            w(i,j,p%loc%ke+k) = - w(i,j,p%loc%ke-k)
        enddo
        w(i,j,p%loc%ke)=0.0d0
        enddo
        enddo
            
    else if ( p%glb%wbc(2) == 2 )then

        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
        do k = 1, p%glb%ghc
            u(i,j,p%loc%ke+k) = u(i,j,p%loc%ke+1-k)
            v(i,j,p%loc%ke+k) = v(i,j,p%loc%ke+1-k)
            w(i,j,p%loc%ke+k) = w(i,j,p%loc%ke-k)
        enddo
        w(i,j,p%loc%ke)=0.0d0
        enddo
        enddo

    else if ( p%glb%wbc(2) == 3 )then

        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc

            src = p%loc%vel%z%old(i,j,p%loc%ke)*(w(i,j,p%loc%ke-1)-w(i,j,p%loc%ke-2))/p%glb%dz*p%glb%dt

            w(i,j,p%loc%ke) = p%loc%vel%z%old(i,j,p%loc%ke) - src

            do k = 1, p%glb%ghc
                u(i,j,p%loc%ke+k) = - u(i,j,p%loc%ke+1-k)
                v(i,j,p%loc%ke+k) = - v(i,j,p%loc%ke+1-k)
                w(i,j,p%loc%ke+k) = 2.0d0*w(i,j,p%loc%ke+k-1) - w(i,j,p%loc%ke+k-2)
            enddo

        enddo
        enddo

    endif
    
endif

end subroutine
                  
subroutine job_nvel_bc(p,u,v,w)
! doi.org/10.1063/1.1761178
implicit none
class(job) :: p
real(8), dimension(p%loc%is-p%glb%ghc:p%loc%ie+p%glb%ghc,&
                  &p%loc%js-p%glb%ghc:p%loc%je+p%glb%ghc,&
                  &p%loc%ks-p%glb%ghc:p%loc%ke+p%glb%ghc) :: u,v,w
integer :: i,j,k

!==========================================
!  X-direction velocity boundary condition
!==========================================

if( p%loc%idx == 0 .and. .not. p%glb%xper )then

    if( p%glb%ubc(1) == 1 )then
    
        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc       
        do i = 1, p%glb%ghc
            u(p%loc%is-i,j,k) = - u(p%loc%is-1+i,j,k)
            v(p%loc%is-i,j,k) = - v(p%loc%is-1+i,j,k)
            w(p%loc%is-i,j,k) = - w(p%loc%is-1+i,j,k)
        end do  
        end do
        end do
    
    else if ( p%glb%ubc(1) == 2)then

        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc       
        do i = 1, p%glb%ghc
            u(p%loc%is-i,j,k) = u(p%loc%is-1+i,j,k)
            v(p%loc%is-i,j,k) = v(p%loc%is-1+i,j,k)
            w(p%loc%is-i,j,k) = w(p%loc%is-1+i,j,k)
        end do  
        end do
        end do

    else if ( p%glb%ubc(1) == 3)then

        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc       
        do i = 1, p%glb%ghc
            u(p%loc%is-i,j,k) = - u(p%loc%is-1+i,j,k)
            v(p%loc%is-i,j,k) = - v(p%loc%is-1+i,j,k)
            w(p%loc%is-i,j,k) = w(p%loc%is,j,k)
        end do
        end do
        end do

    endif

endif 

if ( p%loc%idx == p%glb%grid_x-1 .and. .not. p%glb%xper )then

    if( p%glb%ubc(2) == 1 )then
    
        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc       
        do i = 1, p%glb%ghc
            u(p%loc%ie+i,j,k) = - u(p%loc%ie+1-i,j,k)
            v(p%loc%ie+i,j,k) = - v(p%loc%ie+1-i,j,k)
            w(p%loc%ie+i,j,k) = - w(p%loc%ie+1-i,j,k)
        end do  
        end do
        end do
    
    else if ( p%glb%ubc(2) == 2)then

        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc       
        do i = 1, p%glb%ghc
            u(p%loc%ie+i,j,k) = u(p%loc%ie+1-i,j,k)
            v(p%loc%ie+i,j,k) = v(p%loc%ie+1-i,j,k)
            w(p%loc%ie+i,j,k) = w(p%loc%ie+1-i,j,k)
        end do  
        end do
        end do

    else if ( p%glb%ubc(2) == 3)then

        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc       
        do i = 1, p%glb%ghc
            u(p%loc%ie+i,j,k) = - u(p%loc%ie+1-i,j,k)
            v(p%loc%ie+i,j,k) = - v(p%loc%ie+1-i,j,k)
            w(p%loc%ie+i,j,k) = w(p%loc%ie,j,k)
        end do  
        end do
        end do

    endif
        
endif

!==========================================
!  Y-direction velocity boundary condition
!==========================================

if( p%loc%idy==0 .and. .not. p%glb%yper )then

    if( p%glb%vbc(1) == 1 )then
        
        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do j = 1, p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
            u(i,p%loc%js-j,k) = - u(i,p%loc%js-1+j,k)
            v(i,p%loc%js-j,k) = - v(i,p%loc%js-1+j,k)
            w(i,p%loc%js-j,k) = - w(i,p%loc%js-1+j,k)
        enddo
        enddo
        enddo
                
    else if ( p%glb%vbc(1) == 2 )then

        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do j = 1, p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
            u(i,p%loc%js-j,k) = u(i,p%loc%js-1+j,k)
            v(i,p%loc%js-j,k) = v(i,p%loc%js-1+j,k)
            w(i,p%loc%js-j,k) = w(i,p%loc%js-1+j,k)
        enddo
        enddo
        enddo

    else if ( p%glb%vbc(1) == 3 )then

        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do j = 1, p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
            u(i,p%loc%js-j,k) = - u(i,p%loc%js-1+j,k)
            v(i,p%loc%js-j,k) = - v(i,p%loc%js-1+j,k)
            w(i,p%loc%js-j,k) = w(i,p%loc%js,k)
        enddo
        enddo
        enddo

    endif

endif

if (p%loc%idy==p%glb%grid_y-1 .and. .not. p%glb%yper )then

    if( p%glb%vbc(2) == 1 )then
        
        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do j = 1, p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
            u(i,p%loc%je+j,k) = - u(i,p%loc%je+1-j,k)
            v(i,p%loc%je+j,k) = - v(i,p%loc%je+1-j,k)
            w(i,p%loc%je+j,k) = - w(i,p%loc%je+1-j,k)
        enddo
        enddo
        enddo
                
    else if ( p%glb%vbc(2) == 2 )then

        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do j = 1, p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
            u(i,p%loc%je+j,k) = u(i,p%loc%je+1-j,k)
            v(i,p%loc%je+j,k) = v(i,p%loc%je+1-j,k)
            w(i,p%loc%je+j,k) = w(i,p%loc%je+1-j,k)
        enddo
        enddo
        enddo

    else if ( p%glb%vbc(2) == 3 )then

        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do j = 1, p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
            u(i,p%loc%je+j,k) = - u(i,p%loc%je+1-j,k)
            v(i,p%loc%je+j,k) = - v(i,p%loc%je+1-j,k)
            w(i,p%loc%je+j,k) = w(i,p%loc%je,k)
        enddo
        enddo
        enddo

    endif

endif

!==========================================
!  Z-direction velocity boundary condition
!==========================================

if( p%loc%idz==0 .and. .not. p%glb%zper )then

    if( p%glb%wbc(1) == 1 )then
        
        do k = 1, p%glb%ghc
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
            u(i,j,p%loc%ks-k) = - u(i,j,p%loc%ks-1+k)
            v(i,j,p%loc%ks-k) = - v(i,j,p%loc%ks-1+k)
            w(i,j,p%loc%ks-k) = - w(i,j,p%loc%ks-1+k)
        enddo
        enddo
        enddo

    else if ( p%glb%wbc(1) == 2 )then

        do k = 1, p%glb%ghc
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
            u(i,j,p%loc%ks-k) =  u(i,j,p%loc%ks-1+k)
            v(i,j,p%loc%ks-k) =  v(i,j,p%loc%ks-1+k)
            w(i,j,p%loc%ks-k) =  w(i,j,p%loc%ks-1+k)
        enddo
        enddo
        enddo

    else if ( p%glb%wbc(1) == 3 )then

        do k = 1, p%glb%ghc
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
            u(i,j,p%loc%ks-k) = - u(i,j,p%loc%ks-1+k)
            v(i,j,p%loc%ks-k) = - v(i,j,p%loc%ks-1+k)
            w(i,j,p%loc%ks-k) =  w(i,j,p%loc%ks)
        enddo
        enddo
        enddo

    endif

endif

if (p%loc%idz==p%glb%grid_z-1 .and. .not. p%glb%zper )then

    if( p%glb%wbc(2) == 1 )then

        do k = 1, p%glb%ghc
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
            u(i,j,p%loc%ke+k) = - u(i,j,p%loc%ke+1-k)
            v(i,j,p%loc%ke+k) = - v(i,j,p%loc%ke+1-k)
            w(i,j,p%loc%ke+k) = - w(i,j,p%loc%ke+1-k)
        enddo
        enddo
        enddo
                
    else if ( p%glb%wbc(2) == 2 )then

        do k = 1, p%glb%ghc
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
            u(i,j,p%loc%ke+k) =  u(i,j,p%loc%ke+1-k)
            v(i,j,p%loc%ke+k) =  v(i,j,p%loc%ke+1-k)
            w(i,j,p%loc%ke+k) =  w(i,j,p%loc%ke+1-k)
        enddo
        enddo
        enddo

    else if ( p%glb%wbc(2) == 3 )then

        do k = 1, p%glb%ghc
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
            u(i,j,p%loc%ke+k) = - u(i,j,p%loc%ke+1-k)
            v(i,j,p%loc%ke+k) = - v(i,j,p%loc%ke+1-k)
            w(i,j,p%loc%ke+k) =  w(i,j,p%loc%ke)
        enddo
        enddo
        enddo

    endif

endif 
    
end subroutine