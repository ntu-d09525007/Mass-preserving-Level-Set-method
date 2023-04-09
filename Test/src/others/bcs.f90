subroutine bc(dat)
use all
implicit none
real(8),dimension(p%loc%is-p%glb%ghc:p%loc%ie+p%glb%ghc,&
                  p%loc%js-p%glb%ghc:p%loc%je+p%glb%ghc,&
                  p%loc%ks-p%glb%ghc:p%loc%ke+p%glb%ghc) :: dat
integer :: i,j,k

    !$omp parallel do collapse(2)
    do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
    do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
    do i = 1, p%glb%ghc
        dat(p%loc%is-i,j,k) = dat(p%loc%is,j,k)
        dat(p%loc%ie+i,j,k) = dat(p%loc%ie,j,k)
    enddo
    enddo
    enddo
    !$omp end parallel do

    !$omp parallel do collapse(2)    
    do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
    do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
    do j = 1, p%glb%ghc
        dat(i,p%loc%js-j,k) = dat(i,p%loc%js,k)
        dat(i,p%loc%je+j,k) = dat(i,p%loc%je,k)
    enddo
    enddo
    enddo
    !$omp end parallel do

    !$omp parallel do collapse(2)
    do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
    do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
    do k = 1, p%glb%ghc
        dat(i,j,p%loc%ks-k) = dat(i,j,p%loc%ks)
        dat(i,j,p%loc%ke+k) = dat(i,j,p%loc%ke)
    enddo
    enddo
    enddo
    !$omp end parallel do

end subroutine

subroutine velbc(u,v,w)
! doi.org/10.1063/1.1761178
use all
implicit none
real(8),dimension(p%loc%is-p%glb%ghc:p%loc%ie+p%glb%ghc,&
                  p%loc%js-p%glb%ghc:p%loc%je+p%glb%ghc,&
                  p%loc%ks-p%glb%ghc:p%loc%ke+p%glb%ghc) :: u,v,w
integer :: i,j,k

!==========================================
!  X-direction velocity boundary condition
!==========================================

if( p%glb%ubc(1) == 1 )then

    !$omp parallel do collapse(2)
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
    !$omp end parallel do

else if ( p%glb%ubc(1) == 2)then

    !$omp parallel do collapse(2)
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
    !$omp end parallel do
            
endif

 if( p%glb%ubc(2) == 1 )then

    !$omp parallel do collapse(2)
    do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
    do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
    do i = 1, p%glb%ghc
        u(p%loc%ie+i,j,k) = - u(p%loc%ie-i,j,k)
        v(p%loc%ie+i,j,k) = - v(p%loc%ie+1-i,j,k)
        w(p%loc%ie+i,j,k) = - w(p%loc%ie+1-i,j,k)
    end do  
    u(p%loc%ie,j,k) = 0.0d0
    end do
    end do
    !$omp end parallel do

else if ( p%glb%ubc(2) == 2)then

    !$omp parallel do collapse(2)
    do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
    do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
    do i = 1, p%glb%ghc
        u(p%loc%ie+i,j,k) = u(p%loc%ie-i,j,k)
        v(p%loc%ie+i,j,k) = v(p%loc%ie+1-i,j,k)
        w(p%loc%ie+i,j,k) = w(p%loc%ie+1-i,j,k)
    end do  
    u(p%loc%ie,j,k) = 0.0d0
    end do
    end do
    !$omp end parallel do
            
endif

!==========================================
!  Y-direction velocity boundary condition
!==========================================

if( p%glb%vbc(1) == 1 )then

    !$omp parallel do collapse(2)
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
    !$omp end parallel do
        
else if ( p%glb%vbc(1) == 2 )then

    !$omp parallel do collapse(2)
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
    !$omp end parallel do

endif

if( p%glb%vbc(2) == 1 )then

    !$omp parallel do collapse(2)
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
    !$omp end parallel do
        
else if ( p%glb%vbc(2) == 2 )then

    !$omp parallel do collapse(2)
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
    !$omp end parallel do
            
endif

!==========================================
!  Z-direction velocity boundary condition
!==========================================

if( p%glb%wbc(1) == 1 )then

    !$omp parallel do collapse(2)
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
    !$omp end parallel do
        
else if ( p%glb%wbc(1) == 2 )then

    !$omp parallel do collapse(2)
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
    !$omp end parallel do
            
endif

if( p%glb%wbc(2) == 1 )then

    !$omp parallel do collapse(2)
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
    !$omp end parallel do
        
else if ( p%glb%wbc(2) == 2 )then

    !$omp parallel do collapse(2)
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
    !$omp end parallel do
            
endif

end subroutine

subroutine nvelbc(u,v,w)
! doi.org/10.1063/1.1761178
use all
implicit none
real(8),dimension(p%loc%is-p%glb%ghc:p%loc%ie+p%glb%ghc,&
                  p%loc%js-p%glb%ghc:p%loc%je+p%glb%ghc,&
                  p%loc%ks-p%glb%ghc:p%loc%ke+p%glb%ghc) :: u,v,w
integer :: i,j,k

!==========================================
!  X-direction velocity boundary condition
!==========================================

if( p%glb%ubc(1) == 1 )then

    !$omp parallel do collapse(2)
    do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
    do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc       
    do i = 1, p%glb%ghc
        u(p%loc%is-i,j,k) = - u(p%loc%is-1+i,j,k)
        v(p%loc%is-i,j,k) = - v(p%loc%is-1+i,j,k)
        w(p%loc%is-i,j,k) = - w(p%loc%is-1+i,j,k)
    end do  
    end do
    end do
    !$omp end parallel do


else if ( p%glb%ubc(1) == 2)then

    !$omp parallel do collapse(2)
    do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
    do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc       
    do i = 1, p%glb%ghc
        u(p%loc%is-i,j,k) = u(p%loc%is-1+i,j,k)
        v(p%loc%is-i,j,k) = v(p%loc%is-1+i,j,k)
        w(p%loc%is-i,j,k) = w(p%loc%is-1+i,j,k)
    end do  
    end do
    end do
    !$omp end parallel do
            
endif

if( p%glb%ubc(2) == 1 )then

    !$omp parallel do collapse(2)
    do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
    do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc       
    do i = 1, p%glb%ghc
        u(p%loc%ie+i,j,k) = - u(p%loc%ie+1-i,j,k)
        v(p%loc%ie+i,j,k) = - v(p%loc%ie+1-i,j,k)
        w(p%loc%ie+i,j,k) = - w(p%loc%ie+1-i,j,k)
    end do  
    end do
    end do
    !$omp end parallel do

else if ( p%glb%ubc(2) == 2)then

    !$omp parallel do collapse(2)
    do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
    do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc       
    do i = 1, p%glb%ghc
        u(p%loc%ie+i,j,k) = u(p%loc%ie+1-i,j,k)
        v(p%loc%ie+i,j,k) = v(p%loc%ie+1-i,j,k)
        w(p%loc%ie+i,j,k) = w(p%loc%ie+1-i,j,k)
    end do  
    end do
    end do
    !$omp end parallel do
            
endif

!==========================================
!  Y-direction velocity boundary condition
!==========================================

if( p%glb%vbc(1) == 1 )then

    !$omp parallel do collapse(2)   
    do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
    do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
    do j = 1, p%glb%ghc
        u(i,p%loc%js-j,k) = - u(i,p%loc%js-1+j,k)
        v(i,p%loc%js-j,k) = - v(i,p%loc%js-1+j,k)
        w(i,p%loc%js-j,k) = - w(i,p%loc%js-1+j,k)
    enddo
    enddo
    enddo
    !$omp end parallel do
            
else if ( p%glb%vbc(1) == 2 )then

    !$omp parallel do collapse(2)   
    do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
    do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
    do j = 1, p%glb%ghc
        u(i,p%loc%js-j,k) = u(i,p%loc%js-1+j,k)
        v(i,p%loc%js-j,k) = v(i,p%loc%js-1+j,k)
        w(i,p%loc%js-j,k) = w(i,p%loc%js-1+j,k)
    enddo
    enddo
    enddo
    !$omp end parallel do
                
endif

if( p%glb%vbc(2) == 1 )then
    
    !$omp parallel do collapse(2)   
    do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
    do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
    do j = 1, p%glb%ghc
        u(i,p%loc%je+j,k) = - u(i,p%loc%je+1-j,k)
        v(i,p%loc%je+j,k) = - v(i,p%loc%je+1-j,k)
        w(i,p%loc%je+j,k) = - w(i,p%loc%je+1-j,k)
    enddo
    enddo
    enddo
    !$omp end parallel do
            
else if ( p%glb%vbc(2) == 2 )then

     !$omp parallel do collapse(2)  
    do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
    do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
    do j = 1, p%glb%ghc
        u(i,p%loc%je+j,k) = u(i,p%loc%je+1-j,k)
        v(i,p%loc%je+j,k) = v(i,p%loc%je+1-j,k)
        w(i,p%loc%je+j,k) = w(i,p%loc%je+1-j,k)
    enddo
    enddo
    enddo
    !$omp end parallel do
                
endif

!==========================================
!  Z-direction velocity boundary condition
!==========================================

if( p%glb%wbc(1) == 1 )then
    
    !$omp parallel do collapse(2)  
    do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
    do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
    do k = 1, p%glb%ghc
        u(i,j,p%loc%ks-k) = - u(i,j,p%loc%ks-1+k)
        v(i,j,p%loc%ks-k) = - v(i,j,p%loc%ks-1+k)
        w(i,j,p%loc%ks-k) = - w(i,j,p%loc%ks-1+k)
    enddo
    enddo
    enddo
    !$omp end parallel do

else if ( p%glb%wbc(1) == 2 )then

    !$omp parallel do collapse(2) 
    do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
    do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
    do k = 1, p%glb%ghc
        u(i,j,p%loc%ks-k) =  u(i,j,p%loc%ks-1+k)
        v(i,j,p%loc%ks-k) =  v(i,j,p%loc%ks-1+k)
        w(i,j,p%loc%ks-k) =  w(i,j,p%loc%ks-1+k)
    enddo
    enddo
    enddo
    !$omp end parallel do
                
endif

if( p%glb%wbc(2) == 1 )then

    !$omp parallel do collapse(2) 
    do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
    do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
    do k = 1, p%glb%ghc
        u(i,j,p%loc%ke+k) = - u(i,j,p%loc%ke+1-k)
        v(i,j,p%loc%ke+k) = - v(i,j,p%loc%ke+1-k)
        w(i,j,p%loc%ke+k) = - w(i,j,p%loc%ke+1-k)
    enddo
    enddo
    enddo
    !$omp end parallel do
            
else if ( p%glb%wbc(2) == 2 )then

    !$omp parallel do collapse(2) 
    do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
    do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
    do k = 1, p%glb%ghc
        u(i,j,p%loc%ke+k) =  u(i,j,p%loc%ke+1-k)
        v(i,j,p%loc%ke+k) =  v(i,j,p%loc%ke+1-k)
        w(i,j,p%loc%ke+k) =  w(i,j,p%loc%ke+1-k)
    enddo
    enddo
    enddo
    !$omp end parallel do
                
endif
 
    
end subroutine

subroutine find_stag_vel(u,v,w,uu,vv,ww,us,vs,ws)
use all
implicit none
real(8),dimension(p%loc%is-p%glb%ghc:p%loc%ie+p%glb%ghc,&
                  p%loc%js-p%glb%ghc:p%loc%je+p%glb%ghc,&
                  p%loc%ks-p%glb%ghc:p%loc%ke+p%glb%ghc) :: u,v,w,uu,vv,ww,us,vs,ws
integer :: i, j, k

    !===============================================================
    !  Us_t + Us*U_x +  V*U_y +  W*U_z = 0
    !  Vs_t +  U*V_x + Vs*V_y + WW*V_z = 0
    !  Ws_t + UU*W_x + VV*W_y + Ws*W_z = 0
    !===============================================================
 
    !$omp parallel do collapse(3)
    do k = p%loc%ks, p%loc%ke
    do j = p%loc%js, p%loc%je
    do i = p%loc%is, p%loc%ie

        V(i,j,k) = 0.25d0*( Vs(i,j,k)+Vs(i,j-1,k)+Vs(i+1,j,k)+Vs(i+1,j-1,k) )
        W(i,j,k) = 0.25d0*( Ws(i,j,k)+Ws(i,j,k-1)+Ws(i+1,j,k)+Ws(i+1,j,k-1) )

        U(i,j,k) = 0.25d0*( Us(i,j,k)+Us(i-1,j,k)+Us(i,j+1,k)+Us(i-1,j+1,k) )
       WW(i,j,k) = 0.25d0*( Ws(i,j,k)+Ws(i,j,k-1)+Ws(i,j+1,k)+Ws(i,j+1,k-1) )

       UU(i,j,k) = 0.25d0*( Us(i,j,k)+Us(i-1,j,k)+Us(i,j,k+1)+Us(i-1,j,k+1) )
       VV(i,j,k) = 0.25d0*( Vs(i,j,k)+Vs(i,j-1,k)+Vs(i,j,k+1)+Vs(i,j-1,k+1) )

    end do
    end do
    end do
    !$omp end parallel do
 
!==========================================
!  X-direction velocity boundary condition
!==========================================

    if( p%glb%ubc(1) == 1 )then
    
        !$omp parallel do collapse(2)
        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc       
        do i = 1, p%glb%ghc
            V(p%loc%is-i,j,k) = - V(p%loc%is-2+i,j,k)
            W(p%loc%is-i,j,k) = - W(p%loc%is-2+i,j,k)
            
             U(p%loc%is-i,j,k) = - U(p%loc%is-1+i,j,k)
            WW(p%loc%is-i,j,k) = - WW(p%loc%is-1+i,j,k)
            
            UU(p%loc%is-i,j,k) = - UU(p%loc%is-1+i,j,k)
            VV(p%loc%is-i,j,k) = - VV(p%loc%is-1+i,j,k)
        end do  
        V(p%loc%is-1,j,k) = 0.0d0
        W(p%loc%is-1,j,k) = 0.0d0
        end do
        end do
        !$omp end parallel do
    
    else if ( p%glb%ubc(1) == 2)then

        !$omp parallel do collapse(2)
        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc       
        do i = 1, p%glb%ghc
            V(p%loc%is-i,j,k) = V(p%loc%is-2+i,j,k)
            W(p%loc%is-i,j,k) = W(p%loc%is-2+i,j,k)
            
             U(p%loc%is-i,j,k) = U(p%loc%is-1+i,j,k)
            WW(p%loc%is-i,j,k) = WW(p%loc%is-1+i,j,k)
            
            UU(p%loc%is-i,j,k) = UU(p%loc%is-1+i,j,k)
            VV(p%loc%is-i,j,k) = VV(p%loc%is-1+i,j,k)
        end do  
        V(p%loc%is-1,j,k) = 0.0d0
        W(p%loc%is-1,j,k) = 0.0d0
        end do
        end do
        !$omp end parallel do
                
    endif

    if( p%glb%ubc(2) == 1 )then
    
        !$omp parallel do collapse(2)
        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc       
        do i = 1, p%glb%ghc
            V(p%loc%ie+i,j,k) = - V(p%loc%ie-i,j,k)
            W(p%loc%ie+i,j,k) = - W(p%loc%ie-i,j,k)
            
            U(p%loc%ie+i,j,k)  = - U(p%loc%ie+1-i,j,k)
            WW(p%loc%ie+i,j,k) = - WW(p%loc%ie+1-i,j,k)

            UU(p%loc%ie+i,j,k) = - UU(p%loc%ie+1-i,j,k)
            VV(p%loc%ie+i,j,k) = - VV(p%loc%ie+1-i,j,k)

        end do  
        V(p%loc%ie,j,k) = 0.0d0
        W(p%loc%ie,j,k) = 0.0d0
        end do
        end do
        !$omp end parallel do
    
    else if ( p%glb%ubc(2) == 2)then

        !$omp parallel do collapse(2)
        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc       
        do i = 1, p%glb%ghc
            V(p%loc%ie+i,j,k) = V(p%loc%ie-i,j,k)
            W(p%loc%ie+i,j,k) = W(p%loc%ie-i,j,k)
            
            U(p%loc%ie+i,j,k)  = U(p%loc%ie+1-i,j,k)
            WW(p%loc%ie+i,j,k) = WW(p%loc%ie+1-i,j,k)

            UU(p%loc%ie+i,j,k) = UU(p%loc%ie+1-i,j,k)
            VV(p%loc%ie+i,j,k) = VV(p%loc%ie+1-i,j,k)
        end do  
        V(p%loc%ie,j,k) = 0.0d0
        W(p%loc%ie,j,k) = 0.0d0
        end do
        end do
        !$omp end parallel do
                
    endif
            
!==========================================
!  Y-direction velocity boundary condition
!==========================================

    if( p%glb%vbc(1) == 1 )then
        
        !$omp parallel do collapse(2)
        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
        do j = 1, p%glb%ghc
            U(i,p%loc%js-j,k) = - U(i,p%loc%js-2+j,k)
            WW(i,p%loc%js-j,k) = - WW(i,p%loc%js-2+j,k)

            UU(i,p%loc%js-j,k) = - UU(i,p%loc%js-1+j,k)
            VV(i,p%loc%js-j,k) = - VV(i,p%loc%js-1+j,k)
 
            V(i,p%loc%js-j,k) = - V(i,p%loc%js-1+j,k)
            W(i,p%loc%js-j,k) = - W(i,p%loc%js-1+j,k)
        enddo
        U(i,p%loc%js-1,k) = 0.0d0
        WW(i,p%loc%js-1,k) = 0.0d0
        enddo
        enddo
        !$omp end parallel do
                
    else if ( p%glb%vbc(1) == 2 )then

        !$omp parallel do collapse(2)
        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
        do j = 1, p%glb%ghc
            U(i,p%loc%js-j,k) = U(i,p%loc%js-2+j,k)
            WW(i,p%loc%js-j,k) = WW(i,p%loc%js-2+j,k)

            UU(i,p%loc%js-j,k) = UU(i,p%loc%js-1+j,k)
            VV(i,p%loc%js-j,k) = VV(i,p%loc%js-1+j,k)
 
            V(i,p%loc%js-j,k) = V(i,p%loc%js-1+j,k)
            W(i,p%loc%js-j,k) = W(i,p%loc%js-1+j,k)
        enddo
        U(i,p%loc%js-1,k) = 0.0d0
        WW(i,p%loc%js-1,k) = 0.0d0
        enddo
        enddo
        !$omp end parallel do
                    
    endif

    if( p%glb%vbc(2) == 1 )then
        
        !$omp parallel do collapse(2)
        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc  
        do j = 1, p%glb%ghc     
            U(i,p%loc%je+j,k) = - U(i,p%loc%je-j,k)
            WW(i,p%loc%je+j,k) = - WW(i,p%loc%je-j,k)
            
            UU(i,p%loc%je+j,k) = - UU(i,p%loc%je+1-j,k)
            VV(i,p%loc%je+j,k) = - VV(i,p%loc%je+1-j,k)
            
            V(i,p%loc%je+j,k) = - V(i,p%loc%je+1-j,k)
            W(i,p%loc%je+j,k) = - W(i,p%loc%je+1-j,k)
        enddo
        U(i,p%loc%je,k) = 0.0d0
        WW(i,p%loc%je,k) = 0.0d0
        enddo
        enddo
        !$omp end parallel do
                
    else if ( p%glb%vbc(2) == 2 )then

        !$omp parallel do collapse(2)
        do k = p%loc%ks-p%glb%ghc, p%loc%ke+p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
        do j = 1, p%glb%ghc            
            U(i,p%loc%je+j,k) = U(i,p%loc%je-j,k)
            WW(i,p%loc%je+j,k) = WW(i,p%loc%je-j,k)
            
            UU(i,p%loc%je+j,k) = UU(i,p%loc%je+1-j,k)
            VV(i,p%loc%je+j,k) = VV(i,p%loc%je+1-j,k)
            
            V(i,p%loc%je+j,k) = V(i,p%loc%je+1-j,k)
            W(i,p%loc%je+j,k) = W(i,p%loc%je+1-j,k)
        enddo
        U(i,p%loc%je,k) = 0.0d0
        WW(i,p%loc%je,k) = 0.0d0
        enddo
        enddo
        !$omp end parallel do
                    
    endif

!==========================================
!  Z-direction velocity boundary condition
!==========================================

    if( p%glb%wbc(1) == 1 )then
        
        !$omp parallel do collapse(2)
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
        do k = 1, p%glb%ghc     
            UU(i,j,p%loc%ks-k) = - UU(i,j,p%loc%ks-2+k)
            VV(i,j,p%loc%ks-k) = - VV(i,j,p%loc%ks-2+k)
            
            U(i,j,p%loc%ks-k) = - U(i,j,p%loc%ks-1+k)
            V(i,j,p%loc%ks-k) = - V(i,j,p%loc%ks-1+k)
            
            W(i,j,p%loc%ks-k) = - W(i,j,p%loc%ks-1+k)
            WW(i,j,p%loc%ks-k) = - WW(i,j,p%loc%ks-1+k)
        enddo
        UU(i,j,p%loc%ks-1) = 0.0d0
        VV(i,j,p%loc%ks-1) = 0.0d0
        enddo
        enddo
        !$omp end parallel do
                
    else if ( p%glb%wbc(1) == 2 )then

        !$omp parallel do collapse(2)
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
        do k = 1, p%glb%ghc     
            UU(i,j,p%loc%ks-k) = UU(i,j,p%loc%ks-2+k)
            VV(i,j,p%loc%ks-k) = VV(i,j,p%loc%ks-2+k)
            
            U(i,j,p%loc%ks-k) = U(i,j,p%loc%ks-1+k)
            V(i,j,p%loc%ks-k) = V(i,j,p%loc%ks-1+k)
            
            W(i,j,p%loc%ks-k)  = W(i,j,p%loc%ks-1+k)
            WW(i,j,p%loc%ks-k) = WW(i,j,p%loc%ks-1+k)
        enddo
        UU(i,j,p%loc%ks-1) = 0.0d0
        VV(i,j,p%loc%ks-1) = 0.0d0
        enddo
        enddo
        !$omp end parallel do
                    
    endif

    if( p%glb%wbc(2) == 1 )then

        !$omp parallel do collapse(2)
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
        do k = 1, p%glb%ghc     
            UU(i,j,p%loc%ke+k) = - UU(i,j,p%loc%ke-k)
            VV(i,j,p%loc%ke+k) = - VV(i,j,p%loc%ke-k)
            
            u(i,j,p%loc%ke+k) = - u(i,j,p%loc%ke+1-k)
            v(i,j,p%loc%ke+k) = - v(i,j,p%loc%ke+1-k)
            
            W(i,j,p%loc%ke+k) = - W(i,j,p%loc%ke+1-k)
            WW(i,j,p%loc%ke+k) = - WW(i,j,p%loc%ke+1-k)
        enddo
        UU(i,j,p%loc%ke) = 0.0d0
        VV(i,j,p%loc%ke) = 0.0d0
        enddo
        enddo
        !$omp end parallel do
                
    else if ( p%glb%wbc(2) == 2 )then

        !$omp parallel do collapse(2)
        do j = p%loc%js-p%glb%ghc, p%loc%je+p%glb%ghc
        do i = p%loc%is-p%glb%ghc, p%loc%ie+p%glb%ghc
        do k = 1, p%glb%ghc     
            UU(i,j,p%loc%ke+k) = UU(i,j,p%loc%ke-k)
            VV(i,j,p%loc%ke+k) = VV(i,j,p%loc%ke-k)
            
            u(i,j,p%loc%ke+k) = u(i,j,p%loc%ke+1-k)
            v(i,j,p%loc%ke+k) = v(i,j,p%loc%ke+1-k)
            
            W(i,j,p%loc%ke+k) = W(i,j,p%loc%ke+1-k)
            WW(i,j,p%loc%ke+k) = WW(i,j,p%loc%ke+1-k)
        enddo
        UU(i,j,p%loc%ke) = 0.0d0
        VV(i,j,p%loc%ke) = 0.0d0
        enddo
        enddo
        !$omp end parallel do
                    
    endif
    
end subroutine
