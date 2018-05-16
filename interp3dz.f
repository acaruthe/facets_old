cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c this subroutine interpolates a 4D variable (time,lev,lat,lon)
c that has pressure levels initally to 1 height level
c input = 4D var (varin), 4D geopotential height in m (zin),
c the location (loc) in m, and predefined output var (varout),
c and supporting info
c assumes data is bottom up (must be or results will be funny)
c uses same method for interpolation as wrf_interp_3dz
c weighted linear
c output = 3D (time,lat,lon)
c msb 11/2010
c
C NCLFORTSTART
       subroutine interp3dz(varin,zin,loc,varout,
     &                   nt,nl,ny,nx,fillval)

       integer nt,nl,ny,nx
       real fillval
       real loc
       real varin(nt,ny,nx,nl)
       real zin(nt,ny,nx,nl)
       real varout(nt,ny,nx)

C NCLEND

       real w1,w2
       integer m,k,j,i,kp

c      print *,'in subroutine interpp3dz'

c LOOP OVER GRID AND NUMBER OF TIMES

       do m = 1,nt
       do j = 1,ny
       do i = 1,nx

c        kp = nl

c        do while ( ( interp.eqv. .false.) .and. (kp.ge.2) )
c        do while (kp.ge.2)
         do kp = nl,2,-1
 
c        print *,'in do loop, m,j,i,kp,z,z-1 =',m,j,i,kp,
c    &            zin(m,j,i,kp),zin(m,j,i,kp-1)

           if( (zin(m,j,i,kp).ne.fillval)   .and.
     &         (zin(m,j,i,kp-1).ne.fillval) .and.
     &         (zin(m,j,i,kp-1).le.loc)       .and.
     &         (zin(m,j,i,kp).gt.loc)  )then
       
c            print *,'in if, kp =',kp
             w2 = (loc-zin(m,j,i,kp-1))/(zin(m,j,i,kp)-zin(m,j,i,kp-1))
             w1 = 1. - w2
             varout(m,j,i) = w1*varin(m,j,i,kp-1) + w2*varin(m,j,i,kp)
             goto 10
           end if
     
c          kp = kp-1
     
         enddo

10       continue

       enddo
       enddo
       enddo
 
       return

       end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

