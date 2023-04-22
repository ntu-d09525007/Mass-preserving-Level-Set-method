subroutine deforming_field_velocity()
use all 
!$ use omp_lib
implicit none
integer :: i,j,k,id
real(8) :: x, y, z, xx, yy, zz, pi, ct, linf, linf2

pi = dacos(-1.0_8)
ct = dcos( pi*p%glb%time / p%glb%t2s )

!$omp parallel do private(i,j,k,x,y,z,xx,yy,zz)
do id = 0, p%glb%threads-1
    
    do k = p%of(id)%loc%ks, p%of(id)%loc%ke
    do j = p%of(id)%loc%js, p%of(id)%loc%je
    do i = p%of(id)%loc%is, p%of(id)%loc%ie
    
        x = p%glb%x(i,j,k)
        y = p%glb%y(i,j,k)
        z = p%glb%z(i,j,k)
        
        xx = 0.5_8 * ( p%glb%x(i,j,k) + p%glb%x(i+1,j,k) )
        yy = 0.5_8 * ( p%glb%y(i,j,k) + p%glb%y(i,j+1,k) )
        zz = 0.5_8 * ( p%glb%z(i,j,k) + p%glb%z(i,j,k+1) )
        
        p%of(id)%loc%nvel%x%now(i,j,k) =   2.0d0*dsin(pi*x)**2.0d0*dsin(2.0d0*pi*y)*dsin(2.0d0*pi*z) * ct
        p%of(id)%loc%nvel%y%now(i,j,k) =      -  dsin(2.0d0*pi*x)*dsin(pi*y)**2.0d0*dsin(2.0d0*pi*z) * ct
        p%of(id)%loc%nvel%z%now(i,j,k) =      -  dsin(2.0d0*pi*x)*dsin(2.0d0*pi*y)*dsin(pi*z)**2.0d0 * ct
        
        p%of(id)%loc%vel%x%now(i,j,k) =   2.0d0*dsin(pi*xx)**2.0d0*dsin(2.0d0*pi*y)*dsin(2.0d0*pi*z) * ct
        p%of(id)%loc%vel%y%now(i,j,k) =      -  dsin(2.0d0*pi*x)*dsin(pi*yy)**2.0d0*dsin(2.0d0*pi*z) * ct
        p%of(id)%loc%vel%z%now(i,j,k) =      -  dsin(2.0d0*pi*x)*dsin(2.0d0*pi*y)*dsin(pi*zz)**2.0d0 * ct
        
    enddo
    enddo
    enddo
    
    call p%of(id)%bc(0,p%of(id)%loc%nvel%x%now)
    call p%of(id)%bc(0,p%of(id)%loc%nvel%y%now)
    call p%of(id)%bc(0,p%of(id)%loc%nvel%z%now)
    
    call p%of(id)%bc(0,p%of(id)%loc%vel%x%now)
    call p%of(id)%bc(0,p%of(id)%loc%vel%y%now)
    call p%of(id)%bc(0,p%of(id)%loc%vel%z%now)

enddo
!$omp end parallel do

call pt%vel%sync    
call pt%nvel%sync   

end subroutine

