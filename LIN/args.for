c    ---------------------------------------------------  A R G S
c...  This routine compiles common point (ARG) values to
c...  be included in BLOC DATA of an apropriate program.
      
      PROGRAM ARGS
      COMPLEX*16 Z(6) , W(6) , U(6) , V(6) 
      DATA       K/6/

      open(1,file='uTw')
      open(2,file='mtw')
      open(3,file='uTe')
      open(4,file='mTe')
      open(6,file='args.lst')

      READ(1,*) (ID,Z(I),I=1,K)                ! Load uTw coords
      READ(2,*) (ID,W(I),I=1,K)                ! Load mTw coords
      READ(3,*) (ID,U(I),I=1,K)                ! Load uTe coords
      READ(4,*) (ID,V(I),I=1,K)                ! Load mTe coords

      write(6,*) 'uTw'
      do I = 1,K
         write(6,5) Z(I)
      end do

      write(6,*) 'mTw'
      do I = 1,K
         write(6,5) W(I)
      end do 

      write(6,*) 'uTe'
      do I = 1,K
         write(6,5) U(I)
      end do  

      write(6,*) 'mTe'
      do I = 1,K
         write(6,5) V(I)
      end do


      STOP ' <* ARGS Completed :> args.lst *>'
   5  format('(',F11.3,'d0,',F10.3,'d0),')
      END
