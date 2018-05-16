cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c This subroutine searches through a wind profile to determine
c whether the wind profile contains a LLJ, a SLLJ or SWE.  This
c distinction is based upon Bonner's classification (1968):  
c      Category 1: 12 m/s <= V < 16 m/s & dV >= 6 m/s
c      Category 2: 16 m/s <= V < 20 m/s & dV >= 8 m/s
c      Category 3: 20 m/s <= V          & dV >= 10 m/s
c The wind maximum (V) must occur in the lowest 1.5 km, and dV is
c the minimum difference between the level of max. wind and 3 km.
c
c Input variables:
c ! input variables must be bottom-up in array indexing
c      WS - 4D array containing the Wind Speed at each level (m/s)
c      WD - 4D array containing the Wind Direction at each level
c      HGT - 4D array containing the height AGL of the each level (m)
c Output variable: 3D (time,ny,nx)
c      LLJCAT - exclusive LLJ categories
c!not  LLJOVR - overlapping LLJ categories
c!not  WE - wind event
c      MAXU - max LLJ speed
c      MAXD - max LLJ speed direction
c      MAXSHR - max shear above LLJ
c      MAXHGT - llj height
c
C NCLFORTSTART
      subroutine swellj(ws,maxu,
     &                   nt,nl,ny,nx,fillval)

       integer nt,nl,ny,nx
       integer lljcat(nt,ny,nx)
       real fillval
       real ws(nt,nl,ny,nx)
       real wd(nt,nl,ny,nx)
       real sigma(nl)
       real maxu(nt,ny,nx)
       real maxd(nt,ny,nx)
       real maxshr(nt,ny,nx)
       real maxsigma(nl)
C NCLEND
c in regcm we have sigma, not hght, so need to change that 
       real dif


      print *, "youve made it here"
c WE ARE GOING TO ASSUME THAT THE FILL VALUE OF THE INPUT IS -999.
c MAKE SURE IT IS

       if(fillval.ne.-999.)then
         WRITE(*,*) 'FILL VALUE MUST EQ -999 IN LLJ FUNCTION =',fillval
         stop
       endif

c LOOP OVER GRID AND NUMBER OF TIMES

       do m = 1,nt
       do j = 1,ny
       do i = 1,nx


c      k=k-1
      maxu(m,j,i) = 0.0
c       continue
        do l = 13,nl 
         if (ws(m,l,j,i) .gt. maxu(m,j,i)) then 
                maxu(m,j,i)=ws(m,l,j,i)
c               maxhght(m,j,i)=sigma(l)
         endif 
         enddo




      end do 
      end do
      end do 

      print *, "end :)" 
        return
      

        end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

