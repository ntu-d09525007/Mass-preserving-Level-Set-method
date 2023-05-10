#include "textures.inc"     
#include "colors.inc"
#include "functions.inc"     


#declare milk= material {
texture {
    pigment { color rgbt <0.8627, 0.851, 0.8039, 1> }
    finish {
      ambient 0.0
      diffuse 0.0
      reflection {
        0.0, 1.0
        //metallic
      }

      specular 0.4
      roughness 0.002
    }
  }
  interior {
    ior 1.5
    dispersion 1.01
    fade_distance 0.1
    fade_power 2
    media {
        scattering { 
            2 <0.5, 0.65, 0.4> 
            extinction  1.0
        }       
    }   
  }     
}

#declare gate=material{
texture{Silver3}
}
  

//===================================================================

global_settings {
	ambient_light color rgb <1.0, 1.0, 1.0>
	assumed_gamma 1.5    
	photons { count 5000 }
}                              

background { color rgb <0.320000, 0.340000, 0.430000>}
                      

#declare TANK_THICKNESS = 0.02;
#declare TANK_XL = 5.0;
#declare TANK_YL = 1.0;
#declare TANK_ZL = 2.0;  
#declare x0 = -3.0;
#declare y0 = -3.0;
#declare z0 = 0;

#declare tank= 
difference{              
box{<-TANK_THICKNESS+x0,-TANK_THICKNESS+y0,-TANK_THICKNESS+z0>,<x0+TANK_XL+TANK_THICKNESS,y0+TANK_YL+TANK_THICKNESS,z0+TANK_THICKNESS+TANK_ZL>}
box{<x0,y0,z0>,<x0+TANK_XL, y0+TANK_YL, z0+TANK_ZL>}  
texture{Glass3}
}       
       
       
//object{tank}   

//=================================   

#declare TEX_WOOD = texture {

	pigment {
        	wood

	        color_map { 
        		[0.1 color rgb <0.60, 0.35, 0.20>]
        		[0.9 color rgb <0.90, 0.65, 0.30>]
                        [1.0 color rgb <0.60, 0.35, 0.20>]
                }
        	turbulence <0.06, 0.1, 1000>
        	scale <0.03, 0.03, 0.1>
        	rotate <1, 1, 0>
	}

	normal {
		bumps 0.4
		scale 0.4
	}
}

#declare wood_box = 
box{<-2*TANK_XL, -2*TANK_YL, -10*TANK_THICKNESS>, <3.0*TANK_XL, 3.0*TANK_YL, -TANK_THICKNESS>
texture{TEX_WOOD}}


//object{wood_box}