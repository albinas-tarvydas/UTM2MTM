c    --------------------------------------------------------   M E U T
c...  This program transforms MTMe coordinates to UTM
c...  over a UTM zone, extending fron Latitude 42 to 75    

      BLOCK DATA 
      IMPLICIT     DOUBLE COMPLEX (U-V)  ! On Start Common Points are
      COMMON/ARGS/ U(6) , V(6)           ! Initialised by  Block Data
      DATA U/(4652046.692d0,375731.349d0),(6487091.342d0,412550.459d0),
     +       (8326498.728d0,456653.632d0),(4652046.692d0,624268.651d0),
     +       (6487091.342d0,587449.541d0),(8326498.728d0,543346.368d0)/
      DATA U/(4649562.676d0,500000.000d0),(6484169.199d0,500000.000d0),
     +       (8323452.589d0,500000.000d0),(4653917.463d0,748471.772d0),
     +       (6488072.911d0,674819.406d0),(8325644.124d0,586641.002d0)/
      END
      PROGRAM MEUT 
      COMPLEX*16   Z , W , U , V , FZ , Z0 , ZT
      COMMON/ARGS/ U(6) , V(6)

      OPEN(1,file='mTe')                          ! mTe grid coords
      OPEN(2,file='uTe')                          ! uTe grid coords
      OPEN(6,file='utmE.lst')                     ! List file 
      WRITE(6,15)
      WRITE(6,25)

 10   CONTINUE
      READ(1,*,end=50) K , Z                      ! Get mTe coords
      READ(2,*,end=50) K , W                      ! Get uTe coords
      Z0 = Z                                      ! Get a data point.
      FZ = (0.0,0.0)                           
      DO I  = 1,6                                 ! Go to the complex
         ZT = (1.0,0.0)                           ! polynomial and get
         DO J = 1,6                               ! the estimated of 
            IF(J.NE.I) ZT = ZT *                  ! its value ...
     +        ((Z0 - U(J)) / (U(I) - U(J)))
         END DO
         FZ = FZ + V(I) * ZT
      END DO
      WRITE(6,35) K,Z,FZ,(W-FZ)                   ! D = grid-calc
      GOTO 10   
      
 50   STOP ' <* ME-UT Completed :> utmE.lst *>'
 15   FORMAT(79('='),/,'* TRANSFORM:',6x,'MTMe',11x,'TO',13x,'UTMe',
     +       14x,'(Grid-Calc) *')
 25   FORMAT(79('_'),/,12x,'Y',10X,'X',9X,'-=>',6x,
     +       'Y',10X,'X',13x,'DY',5x,'DX',/,30('-'),'  NAD27 ',41('-'))
 35   FORMAT(I4,' Z=',F12.3,F11.3,'  F(Z)=',F12.3,F11.3,'  D=',2(F7.3))
      END
