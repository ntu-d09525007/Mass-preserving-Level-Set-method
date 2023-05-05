SUBROUTINE TO_FILE()
USE PROBLEM_DEF
IMPLICIT NONE
CHARACTER(3) :: NAM

 WRITE(NAM,'(I3.3)')METHOD_CNT

 OPEN(UNIT=999,FILE='PROBLEM PARAS_'//NAM//'.TXT')
 
 WRITE(999,'(A20,F5.1,A3,F5.1,A3,F5.1)')'Domain Size:',xL,' x ',yL,' x ',zL
 WRITE(999,'(A20,I5,A3,i5,A3,i5)')'Grid Number:',Node_x,' x ',Node_y,' x ',Node_z
 WRITE(999,'(A20,I3)')'Unit Grid:',UNIT_GRID
 write(999,*)''
 WRITE(999,'(A20,F10.8)')'Mesh Size:',dx
 write(999,'(A20,F10.8)')'Time Step:',dt
 write(999,*)''
 write(999,'(A20,ES15.4)')'Chara. Length(m):',CHAR_LENGTH
 write(999,'(A20,ES15.4)')'Chara. Vel.(m/s):',CHAR_VELOCITY
 write(999,'(A20,ES15.4)')'Chara. Time(s):',CHAR_TIME
 write(999,*)''
 write(999,'(A20,F15.4)')'Reynolds number:',Re
 write(999,'(A20,F15.4)')'Weber number:',We
 write(999,'(A20,F15.4)')'Froude number:',FR
 write(999,'(A20,F15.4)')'Density ratio:',RATIO_RHO
 write(999,'(A20,F15.4)')'Viscosity ratio:',RATIO_AMU
 write(999,*)''
 write(999,'(A20,F15.4)')'Stop at:',TIME_TO_STOP
 write(999,'(A20,F15.4)')'Plot time step:',TIME_TO_PLOT
 
 select case(INTERFACE_METHOD)
   case(0)  
     WRITE(999,*)'Level Set Method'
   case(1)  
     WRITE(999,*)'Volume of Fluid Method'
   case(2)  
     WRITE(999,*)'CLSVOF Method'
   case(3)  
     WRITE(999,*)'VOSET Method'
 end select

 select case(IBM_SOLVER)
   case(0)
     write(999,*)'IBM Method is NOT included'
   case(1) 
     write(999,*)'IBM Method IS included'
 end select 

 select case(ST_FORCE)
   case(0)
     write(999,*)'Surface Tension model is NOT included'
   case(1) 
     write(999,*)'Surface Tension model IS included < no scaling >'
   case(2) 
     write(999,*)'Surface Tension model IS included < with scaling >'
 end select 
 
 select case(G_FORCE)
   case(0)
     write(999,*)'Gravity is NOT included'
   case(1) 
     write(999,*)'Gravity IS included'
 end select  

 select case(VEL_BC)
   case(0)
     write(999,*)'No slip condition'
   case(1) 
     write(999,*)'Slip condition'
   case(2) 
     write(999,*)'Outflow condition'
 end select  
 
 CLOSE(999)
 
END SUBROUTINE

SUBROUTINE TO_FILE2()
USE PROBLEM_DEF
IMPLICIT NONE
CHARACTER(3) :: NAM

 WRITE(NAM,'(I3.3)')METHOD_CNT

 TE = WTIME()
 CPU_COST = CPU_COST + TE - TS
 
 OPEN(UNIT=999,FILE='PROBLEM PARAS_'//NAM//'.TXT')
 
 WRITE(999,'(A20,F15.4)')"CPU TIME=",CPU_COST
 write(999,*)''
 WRITE(999,'(A20,F5.1,A3,F5.1,A3,F5.1)')'Domain Size:',xL,' x ',yL,' x ',zL
 WRITE(999,'(A20,I5,A3,i5,A3,i5)')'Grid Number:',Node_x,' x ',Node_y,' x ',Node_z
 WRITE(999,'(A20,I3)')'Unit Grid:',UNIT_GRID
 write(999,*)''
 WRITE(999,'(A20,F10.8)')'Mesh Size:',dx
 write(999,'(A20,F10.8)')'Time Step:',dt
 write(999,*)''
 write(999,'(A20,ES15.4)')'Chara. Length(m):',CHAR_LENGTH
 write(999,'(A20,ES15.4)')'Chara. Vel.(m/s):',CHAR_VELOCITY
 write(999,'(A20,ES15.4)')'Chara. Time(s):',CHAR_TIME
 write(999,*)''
 write(999,'(A20,F15.4)')'Reynolds number:',Re
 write(999,'(A20,F15.4)')'Weber number:',We
 write(999,'(A20,F15.4)')'Froude number:',FR
 write(999,'(A20,F15.4)')'Density ratio:',RATIO_RHO
 write(999,'(A20,F15.4)')'Viscosity ratio:',RATIO_AMU
 write(999,*)''
 write(999,'(A20,F15.4)')'Stop at:',TIME_TO_STOP
 write(999,'(A20,F15.4)')'Plot time step:',TIME_TO_PLOT
 
 select case(INTERFACE_METHOD)
   case(0)  
     WRITE(999,*)'Level Set Method'
   case(1)  
     WRITE(999,*)'Volume of Fluid Method'
   case(2)  
     WRITE(999,*)'CLSVOF Method'
   case(3)  
     WRITE(999,*)'VOSET Method'
 end select

 select case(IBM_SOLVER)
   case(0)
     write(999,*)'IBM Method is NOT included'
   case(1) 
     write(999,*)'IBM Method IS included'
 end select 

 select case(ST_FORCE)
   case(0)
     write(999,*)'Surface Tension model is NOT included'
   case(1) 
     write(999,*)'Surface Tension model IS included < no scaling >'
   case(2) 
     write(999,*)'Surface Tension model IS included < with scaling >'
 end select 
 
 select case(G_FORCE)
   case(0)
     write(999,*)'Gravity is NOT included'
   case(1) 
     write(999,*)'Gravity IS included'
 end select  

 select case(VEL_BC)
   case(0)
     write(999,*)'No slip condition'
   case(1) 
     write(999,*)'Slip condition'
   case(2) 
     write(999,*)'Outflow condition'
 end select  
 
 CLOSE(999)
 
END SUBROUTINE