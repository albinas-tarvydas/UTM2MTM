
C      ----------------------------------------------------   G E O
C      This Program generates the GeoE/W.min & .max Files for
C      input to UTM's , MTM's and Mercs programs as follows :
C      GeoE/W.min - Geographics spaced at  1  degree interval
C      GeoE/W.max - Geographics spaced at  .5 degree interval 
C....  NS=start at, ND = # of degrees+1 in 1/2 ZONE, INCrement  

       PROGRAM GEO
       REAL*4    LAT   , LON, INC 
       DATA      NS/1/ , ND/4/ , END/1/ , INC/1/ 

       PRINT '(A$)',' Enter UTM Zone # , LAT(from) & LAT(to) -=>: '
       READ(5,*)  ZONE , ALAT , ALATO 

       K    = ALATO - ALAT + 1         ! K = LATs Range in UTM ZONE
       CM   = 177-ZONE*6               ! CM of this UTM ZONE
       ALON = CM + 3                   ! Start at ZONEs W LONgitude
       OPEN(6,file='GeoW.min')         ! Open listing file
       
  10   NE   = K                        ! NE = Loop end (set to K)
       LON  = ALON
       DO L = 1,ND                     ! ND = 3 + 1 (full degrees)                    
          LAT  = ALAT
          DO I = NS,NE                 ! NS = Loop start(from DATA)          
             WRITE(6,15) I , LAT , LON
             LAT = LAT + INC           ! INCrement val=1(from DATA)
          END DO
          NS  = NE  + 1
          NE  = NE  + K
          LON = LON - INC
      END DO
      IF(END.EQ.0) GOTO 20   
      IF(ALON.NE.CM) OPEN(6,file='GeoW.max')
      IF(ALON.EQ.CM) OPEN(6,file='GeoE.max')
      NS  = 1
      INC = INC/2                       ! Set INCrement to .5 degree
      ND  = 7                           ! Num of .5 degr in 1/2 UTM
      K   = 2*K-1                       ! K = Total num of .5 degree
      END = 0
      GOTO 10

   20 IF(ALON.EQ.CM) GOTO 30
      ALON = CM
      OPEN(6,file='GeoE.min')
      INC = 1                          ! INCrement is set to 1
      NS  = 1
      ND  = 4
      K   = (K+1)/2                    ! Reset K to its initial value
      END = 1
      GOTO 10

  30  OPEN(6,file='posW')
      LON    = CM + 3
      DO I = 1,2  
         WRITE(6,15) I , ALAT , LON
         WRITE(6,15) I , (ALAT+ALATO)/2 , LON
         WRITE(6,15) I , ALATO , LON
         LON = CM      
      END DO

      OPEN(6,file='posE')
      DO I = 1,2
         WRITE(6,15) I , ALAT , LON
         WRITE(6,15) I , (ALAT+ALATO)/2 , LON
         WRITE(6,15) I , ALATO , LON
         LON = CM - 3
      END DO      
   
      STOP ' Geo Completed -=> GeoW/E.min & .max , posW & posE '
  15  FORMAT( I5,',',2F8.2 )
      END    
