SUBROUTINE SECOND_ORDER_UPWIND()
IMPLICIT NONE

 CALL VELA()
 CALL VELB()
 CALL VELC()

END SUBROUTINE

SUBROUTINE VELA()
USE PRECISION
USE PROBLEM_DEF
USE FLUID_PROPERTIES
USE LS_DATA
use vof_data
IMPLICIT NONE
INTEGER :: I,J,K
REAL(DP) :: DX2,DY2,DZ2,RE2X,RE2Y,RE2Z
REAL(DP) :: UP,UM,VV,VP,VM,WW,WP,WM,CM,CR,CC,DETF,CURVK,FINX,A1,A2,A3,A4,A5,A6,A7,TMP
    
  DX2=DX*DX
  DY2=DY*DY
  DZ2=DZ*DZ
  RE2X=RE*DX2
  RE2Y=RE*DY2
  RE2Z=RE*DZ2
 
 !$omp parallel do private(up,um,vv,vp,vm,ww,wp,wm,cm,cr,cc,detf,curvk,finx,a1,a2,a3,a4,a5,a6,a7,TMP)
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y
 DO I = 1, NODE_X-1

    UP=0.5_DP*(U(I,J,K)+ABS(U(I,J,K)))
    UM=0.5_DP*(U(I,J,K)-ABS(U(I,J,K)))
    
    VV=0.25_DP*(V(I+1,J,K)+V(I,J,K)+V(I+1,J-1,K)+V(I,J-1,K))
    VP=0.5_DP*(VV+ABS(VV))
    VM=0.5_DP*(VV-ABS(VV))
    
    WW=0.25_DP*(W(I+1,J,K)+W(I,J,K)+W(I+1,J,K-1)+W(I,J,K-1))
    WP=0.5_DP*(WW+ABS(WW))
    WM=0.5_DP*(WW-ABS(WW))
    
    CM=(AMU_OLD(I,J,K)+AMU_OLD(I+1,J,K))
    CR=(RHO_OLD(I,J,K)+RHO_OLD(I+1,J,K))
    CC=CM/CR
    
    !detf = 0.5_DP*(DELTA(i,j,k)+DELTA(i+1,j,k))
    TMP = HEAV(0.5_DP*(PHI(I,J,K)+PHI(I+1,J,K)),DETF)
    curvk= 0.5_DP*(curv(i,j,k)+curv(i+1,j,k))
    finx = 0.5_DP*(NORMAL_X(i,j,k)+NORMAL_X(i+1,j,k))
    
    IF(INTERFACE_METHOD.eq.1)THEN
        DETF=1.0
        FINX=(VOF(I+1,J,K)-VOF(I,J,K))/DX
    ENDIF
 

    A1=0.5_DP/DX*(UP*(3.0_DP*U_OLD(I,J,K)-4.0_DP*U_OLD(I-1,J,K)+U_OLD(I-2,J,K)) &
                 +UM*(-U_OLD(I+2,J,K)+4.0_DP*U_OLD(I+1,J,K)-3.0_DP*U_OLD(I,J,K)))
    
    A2=0.5_DP/DY*(VP*(3.0_DP*U_OLD(I,J,K)-4.0_DP*U_OLD(I,J-1,K)+U_OLD(I,J-2,K)) &
          +VM*(-U_OLD(I,J+2,K)+4.0_DP*U_OLD(I,J+1,K)-3.0_DP*U_OLD(I,J,K)))
    
    A3=0.5_DP/DZ*(WP*(3.0_DP*U_OLD(I,J,K)-4.0_DP*U_OLD(I,J,K-1)+U_OLD(I,J,K-2)) &
          +WM*(-U_OLD(I,J,K+2)+4.0_DP*U_OLD(I,J,K+1)-3.0_DP*U_OLD(I,J,K)))
       
    A4=1.0_DP/RE2X*CC*(U_OLD(I+1,J,K)-2.0_DP*U_OLD(I,J,K)+U_OLD(I-1,J,K))
   
    A5=1.0_DP/RE2Y*CC*(U_OLD(I,J+1,K)-2.0_DP*U_OLD(I,J,K)+U_OLD(I,J-1,K))
   
    A6=1.0_DP/RE2Z*CC*(U_OLD(I,J,K+1)-2.0_DP*U_OLD(I,J,K)+U_OLD(I,J,K-1))
    
    if(ST_FORCE==0)THEN
        A7=0.0_dp
    ELSE IF(ST_FORCE==1)THEN
        A7=2.0_DP/(CR*WE)*curvk*detf*finx
    ELSE IF(ST_FORCE==2)THEN
        A7=2.0_DP/(CR*WE)*curvk*detf*finx*2.0_DP*TMP
    ENDIF
    
    US(I,J,K)=A1+A2+A3-A4-A5-A6+A7

  ENDDO
  ENDDO
  ENDDO
  !$omp end parallel do

  END SUBROUTINE VELA 
  
  
  
 SUBROUTINE VELB()
 USE PRECISION
 USE PROBLEM_DEF
 USE FLUID_PROPERTIES
 USE LS_DATA
 use vof_data
 IMPLICIT NONE
 INTEGER :: I,J,K
 REAL(DP) :: DX2,DY2,DZ2,RE2X,RE2Y,RE2Z
 REAL(DP) :: UU,UP,UM,VP,VM,WW,WP,WM,CM,CC,CR,DETF,FINY,CURVK,B1,B2,B3,B4,B5,B6,B7,TMP
 
  DX2=DX*DX
  DY2=DY*DY
  DZ2=DZ*DZ
  RE2X=RE*DX2
  RE2Y=RE*DY2
  RE2Z=RE*DZ2
 
 !$omp parallel do private(UU,up,um,vp,vm,ww,wp,wm,cm,cc,cr,detf,finy,curvk,b1,b2,b3,b4,b5,b6,b7,TMP)
 DO K = 1, NODE_Z
 DO J = 1, NODE_Y-1
 DO I = 1, NODE_X

    UU=0.25_DP*(U(I,J+1,K)+U(I,J,K)+U(I-1,J+1,K)+U(I-1,J,K))
    UP=0.5_DP*(UU+ABS(UU))
    UM=0.5_DP*(UU-ABS(UU))
    
    VP=0.5_DP*(V(I,J,K)+ABS(V(I,J,K)))
    VM=0.5_DP*(V(I,J,K)-ABS(V(I,J,K)))
    
    WW=0.25_DP*(W(I,J+1,K)+W(I,J,K)+W(I,J+1,K-1)+W(I,J,K-1))
    WP=0.5_DP*(WW+ABS(WW))
    WM=0.5_DP*(WW-ABS(WW))
    
    CM=(AMU_OLD(I,J,K)+AMU_OLD(I,J+1,K))
    CR=(RHO_OLD(I,J,K)+RHO_OLD(I,J+1,K))
    CC=CM/CR
    
    !detf = 0.5_DP*(DELTA(i,j,k)+DELTA(i,j+1,k))
    TMP = HEAV(0.5_DP*(PHI(I,J,K)+PHI(I,J+1,K)),DETF)
    curvk= 0.5_DP*(curv(i,j,k)+curv(i,j+1,k))
    finy = 0.5_DP*(NORMAL_Y(i,j,k)+NORMAL_Y(i,j+1,k))   

    IF(INTERFACE_METHOD.eq.1)THEN
        DETF=1.0
        FINY=(VOF(I,J+1,K)-VOF(I,J,K))/DX
    ENDIF
    
    B1=0.5_DP/DX*(UP*(3.0_DP*V_OLD(I,J,K)-4.0_DP*V_OLD(I-1,J,K)+V_OLD(I-2,J,K)) &
          +UM*(-V_OLD(I+2,J,K)+4.0_DP*V_OLD(I+1,J,K)-3.0_DP*V_OLD(I,J,K)))
    
    B2=0.5_DP/DY*(VP*(3.0_DP*V_OLD(I,J,K)-4.0_DP*V_OLD(I,J-1,K)+V_OLD(I,J-2,K)) &
          +VM*(-V_OLD(I,J+2,K)+4.0_DP*V_OLD(I,J+1,K)-3.0_DP*V_OLD(I,J,K)))
    
    B3=0.5_DP/DZ*(WP*(3.0_DP*V_OLD(I,J,K)-4.0_DP*V_OLD(I,J,K-1)+V_OLD(I,J,K-2)) &
          +WM*(-V_OLD(I,J,K+2)+4.0_DP*V_OLD(I,J,K+1)-3.0_DP*V_OLD(I,J,K)))
   
    B4=1.0_DP/RE2X*CC*(V_OLD(I+1,J,K)-2.0_DP*V_OLD(I,J,K)+V_OLD(I-1,J,K))
   
    B5=1.0_DP/RE2Y*CC*(V_OLD(I,J+1,K)-2.0_DP*V_OLD(I,J,K)+V_OLD(I,J-1,K))
   
    B6=1.0_DP/RE2Z*CC*(V_OLD(I,J,K+1)-2.0_DP*V_OLD(I,J,K)+V_OLD(I,J,K-1))
    
    if(ST_FORCE==0)THEN
        B7=0.0_dp
    ELSE IF(ST_FORCE==1)THEN
        B7=2.0/(CR*WE)*curvk*detf*finy
    ELSE IF(ST_FORCE==2)THEN
        B7=2.0/(CR*WE)*curvk*detf*finy*2.0_DP*TMP
    ENDIF

    VS(I,J,K)=B1+B2+B3-B4-B5-B6+B7

  ENDDO
  ENDDO
  ENDDO
  !$omp end parallel do

  END SUBROUTINE VELB 
                
SUBROUTINE VELC()
 USE PRECISION
 USE PROBLEM_DEF
 USE FLUID_PROPERTIES
 USE LS_DATA
 use vof_data
 IMPLICIT NONE
 INTEGER :: I,J,K
 REAL(DP) :: DX2,DY2,DZ2,RE2X,RE2Y,RE2Z,FR2
 REAL(DP) :: UU,UP,UM,VV,VP,VM,WP,WM,CM,CC,CR,DETF,FINZ,CURVK,C1,C2,C3,C4,C5,C6,C7,TMP
 
  DX2=DX*DX
  DY2=DY*DY
  DZ2=DZ*DZ
  RE2X=RE*DX2
  RE2Y=RE*DY2
  RE2Z=RE*DZ2 

  FR2=FR**2
 
 !$omp parallel do private(uu,up,um,vv,vp,vm,wp,wm,cm,cr,cc,detf,curvk,finz,c1,c2,c3,c4,c5,c6,c7,TMP)
 DO K = 1, NODE_Z-1
 DO J = 1, NODE_Y
 DO I = 1, NODE_X

    UU=0.25_DP*(U(I,J,K+1)+U(I,J,K)+U(I-1,J,K+1)+U(I-1,J,K))
    UP=0.5_DP*(UU+ABS(UU))
    UM=0.5_DP*(UU-ABS(UU))
    
    VV=0.25_DP*(V(I,J,K+1)+V(I,J,K)+V(I,J-1,K+1)+V(I,J-1,K))
    VP=0.5_DP*(VV+ABS(VV))
    VM=0.5_DP*(VV-ABS(VV))
    
    WP=0.5_DP*(W(I,J,K)+ABS(W(I,J,K)))
    WM=0.5_DP*(W(I,J,K)-ABS(W(I,J,K)))
    
    CM=(AMU_OLD(I,J,K)+AMU_OLD(I,J,K+1))
    CR=(RHO_OLD(I,J,K)+RHO_OLD(I,J,K+1))
    CC=CM/CR
    
    !detf=0.5_DP*(DELTA(i,j,k)+DELTA(i,j,k+1))
    TMP = HEAV(0.5_DP*(PHI(I,J,K)+PHI(I,J,K+1)),DETF)
    curvk=0.5_DP*(curv(i,j,k)+curv(i,j,k+1))
    finz=0.5_DP*(NORMAL_Z(i,j,k)+NORMAL_Z(i,j,k+1))
    
    IF(INTERFACE_METHOD.eq.1)THEN
        DETF=1.0
        FINZ=(VOF(I,J,K+1)-VOF(I,J,K))/DZ
    ENDIF
    

    C1=0.5_DP/DX*(UP*(3.0_DP*W_OLD(I,J,K)-4.0_DP*W_OLD(I-1,J,K)+W_OLD(I-2,J,K)) &
          +UM*(-W_OLD(I+2,J,K)+4.0_DP*W_OLD(I+1,J,K)-3.0_DP*W_OLD(I,J,K)))
    
    C2=0.5_DP/DY*(VP*(3.0_DP*W_OLD(I,J,K)-4.0_DP*W_OLD(I,J-1,K)+W_OLD(I,J-2,K)) &
          +VM*(-W_OLD(I,J+2,K)+4.0_DP*W_OLD(I,J+1,K)-3.0_DP*W_OLD(I,J,K)))
    
    C3=0.5_DP/DZ*(WP*(3.0_DP*W_OLD(I,J,K)-4.0_DP*W_OLD(I,J,K-1)+W_OLD(I,J,K-2)) &
          +WM*(-W_OLD(I,J,K+2)+4.0_DP*W_OLD(I,J,K+1)-3.0_DP*W_OLD(I,J,K)))

    C4=1.0_DP/RE2X*CC*(W_OLD(I+1,J,K)-2.0_DP*W_OLD(I,J,K)+W_OLD(I-1,J,K))
  
    C5=1.0_DP/RE2Y*CC*(W_OLD(I,J+1,K)-2.0_DP*W_OLD(I,J,K)+W_OLD(I,J-1,K))
  
    C6=1.0_DP/RE2Z*CC*(W_OLD(I,J,K+1)-2.0_DP*W_OLD(I,J,K)+W_OLD(I,J,K-1))

    if(ST_FORCE==0)THEN
        C7=0.0_dp
    ELSE IF(ST_FORCE==1)THEN
        C7=2.0_DP/(CR*WE)*curvk*detf*finz
    ELSE IF(ST_FORCE==2)THEN
        C7=2.0_DP/(CR*WE)*curvk*detf*finz*2.0_DP*TMP
    ENDIF
    
    WS(I,J,K)=C1+C2+C3-C4-C5-C6+C7
    
    IF( G_FORCE==1 )WS(I,J,K)=WS(I,J,K)+1.0_DP/FR2
    
  ENDDO
  ENDDO
  ENDDO
  !$omp end parallel do

  END SUBROUTINE VELC
