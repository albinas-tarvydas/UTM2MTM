c    ---------------------------------------------------------  X M W U T 
c...  This routine transforms the selected info in MTMw to UTM

      BLOCK DATA 
      IMPLICIT     DOUBLE COMPLEX (U-V)  
      COMMON/ARGS/ U(6),V(6)             
      DATA U/(4652046.692d0,375731.349d0),(6487091.342d0,412550.459d0),
     +       (8326498.728d0,456653.632d0),(4652046.692d0,624268.651d0),
     +       (6487091.342d0,587449.541d0),(8326498.728d0,543346.368d0)/
      DATA V/(4653917.463d0,251528.228d0),(6488072.911d0,325180.594d0),
     +       (8325644.124d0,413358.998d0),(4649562.676d0,500000.000d0),
     +       (6484169.199d0,500000.000d0),(8323452.589d0,500000.000d0)/
      END
      PROGRAM XMWUT
      COMMON/ARGS/  U(6),V(6)
      COMPLEX*16    Z , W , U , V , FZ , Z0 , ZT 
      CHARACTER*10  CD 

      OPEN(1,file='mTw')                          ! MTMw grid coords
      OPEN(2,file='uTw')                          ! UTMw grid coords
      OPEN(6,file='xutmW.lst')                    ! List file 
      WRITE(6,15)
 10   CONTINUE
      READ(1,*,end=50) CD , Z                     ! Get MTMw coords
      READ(2,*,end=50) CD , W                     ! Get UTMw coords   
      Z0 = Z                                      ! Get a data point.
      FZ = (0.0,0.0)                           
      DO I  = 1,6                                 ! Go to the complex
         ZT = (1.0,0.0)                           ! polynomial and get
         DO J = 1,6                               ! the estimate of 
            IF(J.NE.I) ZT = ZT *                  ! its value ...
     +        ((Z0 - U(J)) / (U(I) - U(J)))
         END DO
         FZ = FZ + V(I) * ZT
      END DO
      WRITE(6,25) CD , FZ , Z
      GOTO 10   

 50   STOP ' <* XMW-UT Completed :> xutmW.lst *>'
 15   FORMAT(80('='),/,'*',19x,'*',6x,'UTMw DATA',6x,'*',8x,
     +       '(  TRANSFORMED FROM  MTMw   )',/,80('_'),/,25x,'Y',10x,
     +       'X',8x,'<=--'11x,'Y',10X'X',/,43('-'),' NAD 27 ',29('-'))
 25   FORMAT('Id:',5x,A,' =',F12.3,F11.3,8x,'( Z=',F12.3,F11.3,' )')
      END 