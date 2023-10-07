c      ....................................................    G E O P
C      This Program generates the GeoPos Files as input
C      to Polar, Mercs and Compts programs as follows :
C      GeoPos.min - Geographics spaced at  1 degree interval
C      GeoPos.max - Geographics spaced at .5 degree interval 
C      GeoPos.cmp - Geographics for 9 Common Pts (arguments) 

       PROGRAM GEOP
       REAL*4  LAT , ALAT , LON , INC/1/
       DATA    NS/1/ , LE/7/ 

       PRINT '(A$)',' Enter UTM Zone # , LAT(from) & LAT(to) -=>: '
       READ(5,*)  ZONE , ALAT , ALATO 

       K    = ALATO - ALAT + 1         ! K = LATs Range in UTM ZONE
       CM   = 177-ZONE*6               ! CM of this UTM ZONE
       LON  = CM + 3                   ! Start at ZONEs W LONgitude

       OPEN(6,file='geop.min')       
  10   NE   = K
       LON  = CM + 3
       DO L = 1,LE
          LAT = ALAT
          DO I = NS,NE
            WRITE(6,15) I , LAT , LON
            LAT = LAT + INC 
          END DO
          NS  = NE  + 1
          NE  = NE  + K
          LON = LON - INC
      END DO
      IF(END.EQ.0) GOTO 20
      OPEN(6,file='geop.max')
      INC = 0.5
      NS  = 1
      LE  = 13
      K   = 2*K-1
      END = 0
      GOTO 10

  20  OPEN(6,file='geop.cmp')
      LON    = CM + 3
      DO I = 1,3  
         WRITE(6,15) I , ALAT , LON
         WRITE(6,15) I , (ALAT+ALATO)/2 , LON
         WRITE(6,15) I , ALATO , LON
         LON = LON - 3    
      END DO

      STOP' GEOP Completed -=> GeoP.max .mim & .cmp'
  15  FORMAT( I5,',',2F8.2 )
      END    
