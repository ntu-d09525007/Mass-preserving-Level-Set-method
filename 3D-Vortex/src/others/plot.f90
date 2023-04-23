subroutine plot
USE Lib_VTK_IO
use all
implicit none 
integer :: E_IO, id
integer :: nn, nx1,nx2,ny1,ny2,nz1,nz2
integer :: i,j,k
character(3) :: name

if ( abs(p%glb%time - p%glb%pid * p%glb%t2p) > p%glb%dt ) return

!call vortex_dynamics

write(name,'(I3.3)')p%glb%pid

E_IO = VTK_INI_XML(output_format='raw', filename='./out/'//trim(p%glb%name)//'_'//name//'.vts', &
                   mesh_topology='StructuredGrid', nx1=1, nx2=p%glb%node_x, ny1=1, ny2=p%glb%node_y, nz1=1, nz2=p%glb%node_z)

do id = 0, p%glb%threads-1
nx1 = p%of(id)%loc%is; nx2=p%of(id)%loc%ie
ny1 = p%of(id)%loc%js; ny2=p%of(id)%loc%je
nz1 = p%of(id)%loc%ks; nz2=p%of(id)%loc%ke

nn=(nx2-nx1+1)*(ny2-ny1+1)*(nz2-nz1+1)

E_IO = VTK_GEO_XML(nx1=nx1, nx2=nx2, ny1=ny1, ny2=ny2, nz1=nz1, nz2=nz2, NN=nn, &
                   X=p%glb%x(nx1:nx2,ny1:ny2,nz1:nz2),Y=p%glb%y(nx1:nx2,ny1:ny2,nz1:nz2),Z=p%glb%z(nx1:nx2,ny1:ny2,nz1:nz2))
                   
E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'open')
E_IO = VTK_VAR_XML(NC_NN = nn, varname = 'Phi', var = p%of(id)%loc%phi%now(nx1:nx2,ny1:ny2,nz1:nz2) )
E_IO = VTK_VAR_XML(NC_NN = nn, varname = 'VOF', var = p%of(id)%loc%vof%now(nx1:nx2,ny1:ny2,nz1:nz2) )
E_IO = VTK_VAR_XML(NC_NN = nn, varname = 'Velocity', varX = p%of(id)%loc%nvel%x%now(nx1:nx2,ny1:ny2,nz1:nz2),&
                                                    &varY = p%of(id)%loc%nvel%y%now(nx1:nx2,ny1:ny2,nz1:nz2),&
                                                    &varZ = p%of(id)%loc%nvel%z%now(nx1:nx2,ny1:ny2,nz1:nz2))
E_IO = VTK_DAT_XML(var_location = 'node', var_block_action = 'close')
E_IO = VTK_GEO_XML()
enddo

E_IO = VTK_END_XML()

p%glb%pid = p%glb%pid + 1
call p%sync

end subroutine