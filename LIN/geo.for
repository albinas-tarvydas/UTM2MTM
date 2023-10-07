C      ----------------------------------------------------   G E O
C      This Program generates the GeoE/W.min & .max Files for
C      input to UTM's , MTM's and Mercs programs as follows :
C      GeoE/W.min - Geographics spaced at  1  degree interval
C      GeoE/W.max - Geographics spaced at  .5 degree interval 
C....  NS=start at, ND = # of degrees+1 in 1/2 ZONE, INCrement  

       PROGRAM GEO
       REAL*4    LAT   , ALAT  , LON    , ALON , INC 
       DATA      NS/1/ , ND/4/ , END/1/ , INC/1/ 

       PRINT '(A$)',' Enter UTM Zone # , LAT(from) & LAT(to) -=>: '
       READ(5,*)  ZONE , ALAT , ALATto ! TOROTO :  17 42 75

       K    = ALATto - ALAT + 1        ! K = LATs Range in UTM ZONE
       CM   = 177-ZONE*6               ! CM of this UTM ZONE
       ALON = CM - 3.0                 ! Start at ZONEs W LONgitude
       OPEN(6,file='GeoE.min')         ! Open listing file
       
  10   NE   = K                        ! NE = Loop end (set to K)
       LON  = ALON
       DO L = 1,ND                     ! ND = 3 + 1 (even degries)                    
          LAT  = ALAT
          DO I = NS,NE                 ! NS = Loop start(from DATA)          
             WRITE(6,15) I , LAT , LON
             LAT = LAT + INC           ! INCrement val=1(from DATA)
          END DO
          NS  = NE  + 1
          NE  = NE  + K
          LON = LON + INC
      END DO
      IF(END.EQ.0) GOTO 20   
      IF(ALON.NE.CM) OPEN(6,file='GeoE.max')
      IF(ALON.EQ.CM) OPEN(6,file='GeoW.max')
      NS  = 1
      INC = INC/2                       ! Set INCrement to 1/2 degree
      ND  = 7                           ! Num of 1/2 degs in 1/2 UTM
      K   = 2*K-1                       ! K = Totl num of 1/2 degrs
      END = 0
      GOTO 10

   20 IF(ALON.EQ.CM) GOTO 50
      ALON = CM
      OPEN(6,file='GeoW.min')
      INC = 1                          ! INCrement is set to 1
      NS  = 1
      ND  = 4
      K   = (K+1)/2                    ! Reset K to its initial value
      END = 1
      GOTO 10
   
  50  STOP ' Geo Completed -=> GeoE.min & .max , GeoW.min & .max'
  15  FORMAT( I5,',',2F8.2 )
      END    
