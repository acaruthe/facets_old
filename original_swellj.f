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
       subroutine swellj(ws,wd,hgt,maxu,maxd,maxshr,maxhgt,lljcat,
     &                   nt,nl,ny,nx,fillval)

       integer nt,nl,ny,nx
       integer lljcat(nt,ny,nx)
       real fillval
       real ws(nt,ny,nx,nl)
       real wd(nt,ny,nx,nl)
       real hgt(nt,ny,nx,nl)
       real maxu(nt,ny,nx)
       real maxd(nt,ny,nx)
       real maxshr(nt,ny,nx)
       real maxhgt(nt,ny,nx)

C NCLEND

       real dif

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

c----- The "K" loop is over the lowest 1.5 km.
c----- Find the max wind below 1.5 km 

         k=-999
         do l = 1,nl
c          write(*,*) "m,j,i,l,hgt: ",m,j,i,l,hgt(m,j,i,l)
           if(hgt(m,j,i,l) .ne.-999.)then
             k = l
             goto 5
           endif
         end do
5        continue

c        WRITE(*,*) "K = ",k

         if(k.eq.-999)then
           maxu(m,j,i)=fillval
           maxd(m,j,i)=fillval
           maxhgt(m,j,i)=fillval
           maxshr(m,j,i)=fillval
           goto 25
         endif

         k=k-1 
         maxu(m,j,i) = 0.0
10       continue
           k=k+1
c        write(*,*) "m,i,j,k,hgt,ws: ",m,i,j,k,hgt(m,i,j,k),ws(m,i,j,k)
           if (hgt(m,j,i,k) .le. 1500.) then
             if (ws(m,j,i,k) .gt. maxu(m,j,i)) then
                maxu(m,j,i)=ws(m,j,i,k)
                maxd(m,j,i)=wd(m,j,i,k)
                maxhgt(m,j,i)=hgt(m,j,i,k)
             endif
c          WRITE(*,*) "K,M,HGT,WS,MAXU =",k,m,hgt(m,j,i,k), 
c    &                ws(m,j,i,k),maxu(m,j,i)
             goto 10
           endif 
       
c find the maximum wind speed difference in the layer bounded by
c the height of the max wind speed and 3km

         maxshr(m,j,i) = 0.0
20       continue
           if (hgt(m,j,i,k).le.3000.)then
             if (ws(m,j,i,k).ne.fillval)then
               dif = maxu(m,j,i) - ws(m,j,i,k)
               if (dif.gt.maxshr(m,j,i)) maxshr(m,j,i) = dif
             endif
             k=k+1
             goto 20
           endif
25       continue

         lljcat(m,j,i) = 0
c--------Check for category 1 with lower shear
         if(maxu(m,j,i).ge.12.)then
           if(maxshr(m,j,i).ge.4.)then
             if(maxu(m,j,i).lt.16.)then
               lljcat(m,j,i) = 4
             endif
           endif
         endif

c--------Check for categor 2 with lower shear
         if(maxu(m,j,i).ge.16.)then
           if(maxshr(m,j,i).ge.4.)then
             if(maxu(m,j,i).lt.20.)then
               lljcat(m,j,i) = 5
             endif
           endif
         endif

c--------Check for category 3 with lower shear
         if(maxu(m,j,i).ge.20.)then
           if(maxshr(m,j,i).ge.4.)then
             lljcat(m,j,i) = 6
           endif
         endif

c--------Check for category 1
         if(maxu(m,j,i).ge.12.)then
           if(maxshr(m,j,i).ge.6.)then
             if(maxu(m,j,i).lt.16.)then
               lljcat(m,j,i) = 1
             endif
           endif
         endif
 
c--------Check for category 2
         if(maxu(m,j,i).ge.16.)then
           if(maxshr(m,j,i).ge.8.)then
             if(maxu(m,j,i).lt.20.)then
               lljcat(m,j,i) = 2
             endif
           endif
         endif
 
c--------Check for category 3
         if(maxu(m,j,i).ge.20.)then
           if(maxshr(m,j,i).ge.10.)then
             lljcat(m,j,i) = 3
           endif
         endif

      enddo
      enddo
      enddo
 
        return

        end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

