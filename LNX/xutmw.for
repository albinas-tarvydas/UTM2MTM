c    --------------------------------------------------------- X U T M W
c...  This routine transforms the selected info from UTM to MTMw     

      BLOCK DATA 
      IMPLICIT   DOUBLE COMPLEX (U-V)    
      COMMON/ARGS/ U(6) , V(6)             
      DATA U/(4649562.676d0,500000.000d0),(6484169.199d0,500000.000d0),
     +       (8323452.589d0,500000.000d0),(4653917.463d0,251528.228d0),
     +       (6488072.911d0,325180.594d0),(8325644.124d0,413358.998d0)/
      DATA V/(4652046.692d0,624268.651d0),(6487091.342d0,587449.541d0),
     +       (8326498.728d0,543346.368d0),(4652046.692d0,375731.349d0),
     +       (6487091.342d0,412550.459d0),(8326498.728d0,456653.632d0)/
      END
      PROGRAM XUTW
      COMMON/ARGS/ U(6) , V(6)
      COMPLEX*16   Z , W , U , V , FZ , Z0 , ZT
      CHARACTER*10 CD

      OPEN(1,file='uTw')                          ! UTMw grid coords
      OPEN(2,file='mTw')                          ! MTMw grid coords
      OPEN(6,file='xmtmW.lst')                    ! List file
      WRITE(6,15)

 10   CONTINUE                                         
      READ(1,*,END=50) CD , Z                     ! Load UTMe coords
      READ(2,*,END=50) CD , W                     ! Load MTMe coords
      Z0 = Z      
      IF(AIMAG(Z0).GT.500000.d0) GOTO 10          ! Skip if from UTMw
      FZ = (0.0,0.0) 
      DO I  = 1,6                                 ! Go to the complex
         ZT = (1.0,0.0)                           ! polynomial and get                          
         DO J = 1,6                               ! the estimate of 
            IF(J.NE.I) ZT = ZT *                  ! its value ...
     +      ((Z0 - U(J)) / (U(I) - U(J)))
         END DO
         FZ = FZ + V(I) * ZT
      END DO
      WRITE(6,25)  CD , FZ , Z     
      GOTO 10      

 50   STOP ' <* xUT-MW Completed :> xmtmW.lst *> '
 15   FORMAT(80('='),/,'*',19x,'*',6x,'MTMw DATA',6x,'*',8x,
     +       '(  TRANSFORMED FROM  UTMw   )',/,80('_'),/,25x,'Y',10x,
     +       'X',8x,'<=--'11x,'Y',10X'X',/,43('-'),' NAD 27 ',29('-'))
 25   FORMAT('Id:',5x,A,' =',F12.3,F11.3,8x,'( Z=',F12.3,F11.3,' )')
      END

