subroutine plot
USE Lib_VTK_IO
use all
implicit none 
integer :: E_IO, idx,idy,idz
integer :: nn, nx1,nx2,ny1,ny2,nz1,nz2
integer :: i,j,k
character(3) :: name

if ( abs(p%glb%time - p%glb%pid * p%glb%t2p) > p%glb%dt ) return

!call vortex_dynamics

write(name,'(I3.3)')p%glb%pid

E_IO = VTK_INI_XML(output_format='raw', filename='./out/'//trim(p%glb%name)//'_'//name//'.vts', &
                   mesh_topology='StructuredGrid', nx1=1, nx2=p%glb%node_x, ny1=1, ny2=p%glb%node_y, nz1=1, nz2=p%glb%node_z)

do idx = 0, p%glb%num_of_plot-1
do idy = 0, p%glb%num_of_plot-1
do idz = 0, p%glb%num_of_plot-1

    nx1 = idx*p%glb%node_x/p%glb%num_of_plot+1
    nx2 = (idx+1)*p%glb%node_x/p%glb%num_of_plot
    if(idx>0)nx1=nx1-1

    ny1 = idy*p%glb%node_y/p%glb%num_of_plot+1
    ny2 = (idy+1)*p%glb%node_y/p%glb%num_of_plot
    if(idy>0)ny1=ny1-1

    nz1 = idz*p%glb%node_z/p%glb%num_of_plot+1
    nz2 = (idz+1)*p%glb%node_z/p%glb%num_of_plot
    if(idz>0)nz1=nz1-1

    nn=(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)

    E_IO = VTK_GEO_XML(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, NN=nn, &
                       X=p%glb%x(nx1:nx2,ny1:ny2,nz1:nz2),Y=p%glb%y(nx1:nx2,ny1:ny2,nz1:nz2),Z=p%glb%z(nx1:nx2,ny1:ny2,nz1:nz2))
                       
    E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'open')
    E_IO = VTK_VAR_XML(NC_NN = nn, varname = 'Phi', var = p%loc%phi%now(nx1:nx2,ny1:ny2,nz1:nz2) )
    ! E_IO = VTK_VAR_XML(NC_NN = nn, varname = 'Solid', var = p%loc%ibm%solid%now(nx1:nx2,ny1:ny2,nz1:nz2) )
    E_IO = VTK_VAR_XML(NC_NN = nn, varname = 'Pressure', var = p%loc%p%now(nx1:nx2,ny1:ny2,nz1:nz2) )
    E_IO = VTK_VAR_XML(NC_NN = nn, varname = 'Velocity', varX = p%loc%nvel%x%now(nx1:nx2,ny1:ny2,nz1:nz2),&
                                                        &varY = p%loc%nvel%y%now(nx1:nx2,ny1:ny2,nz1:nz2),&
                                                        &varZ = p%loc%nvel%z%now(nx1:nx2,ny1:ny2,nz1:nz2))
    E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'close')
    E_IO = VTK_GEO_XML()

enddo
enddo
enddo

E_IO = VTK_END_XML()

p%glb%pid = p%glb%pid + 1

end subroutine