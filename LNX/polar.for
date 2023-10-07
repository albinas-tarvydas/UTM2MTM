c     ----------------------------------------------   P O L A R

       PROGRAM POLAR
       IMPLICIT REAL*8 (A-H,O-Z)
       REAL*8           LAM,LAT,LON
       DATA A         / 6378206.398d0 /
       DATA B         / 6356583.798d0 /
       DATA RHO       / 57.295779513082d0 /

       open( 1,file='geopos' )
       open( 2,file='PoL',status='new',form='formatted' )
C       open( 6,file='PoL.lst')  ! Del all 'C' to Activate                                    

       PRINT '( A$ )','   Enter  CM  -=>: ' 
       READ (5,*) CM

C       WRITE(6,5) A,B,CM
       CM      = CM/RHO                  
       E       = SQRT(1 - B/A*B/A)
       C       = 2*A/(1 + E)**((1 - E)/2)
     +              /(1 - E)**((1 + E)/2)

   10  CONTINUE
       READ(1,*,END=20) N,LAT,LON
       PHI     = LAT/RHO
       LAM     = LON/RHO                 
       P       = C*TAN(45/RHO - PHI/2)*
     +           ((1 + E*SIN(PHI))
     +           /(1 - E*SIN(PHI)))**(E/2)
       X       = +P*SIN(CM - LAM)
       Y       = -P*COS(CM - LAM)
       SF      = 1 + (1 + E)**(1 - 2*E)*(1 - E)**
     +           (1 + 2*E)*(X*X + Y*Y)/4/A/A
       Y       = Y + 10000000.0d0
       X       = X + 10000000.0d0
C       WRITE(2,15) N,Y,X
C       WRITE(6,25) N,LAT,LON,Y,X,SF            !
       GO TO 10

   20  STOP ' < POLAR Completed :>  PoL ,lst (optional) >'
    5  FORMAT('      * POLAR STEREOGRAPHIC *'//,
     +         18X'PARAMETERS :    A = 'F12.3,/,
     +         18X'                B = 'F12.3,/,
     +         18X'               CM = 'F12.3,//,
     +         10X,'LAT',5X,'LON',8X,'Y',14X,'X',15X,'SF',/)
   15  FORMAT( I4,1X,'(',F11.3,'d0,',F11.3,'d0)' )       
   25  FORMAT(1X,I4,2F8.1,2F15.3,F15.7)
       END
