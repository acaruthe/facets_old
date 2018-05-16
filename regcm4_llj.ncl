;calculates wind related from 3D variables and writes them to a file
;for entire domain and 3 hrly
;calculates 5 LLJ variables
;calculates a sfc->1-km, 3km, 5km, and 6km shear too
;uses  U, V, UAS, VAS, Z (must have this calculated already)
;set up for 28 vertical levels!
;DOING THIS MONTHLY AND OUTPUTTING MONTHLY FILES
;STITCH TOGETHER LATER WITH NCO


 load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
 load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_csm.ncl"
 load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRF_contributed.ncl"
 load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
 load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
 load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
; load "/glade/u/home/bukovsky/ncl/phd/my_ncl_funct/calc_climate.ncl"
; load "/glade/u/home/bukovsky/ncl/phd/my_ncl_funct/calc_stat.ncl"
; load "/glade/u/home/bukovsky/ncl/phd/my_ncl_funct/my_plot_functions.ncl"
; load "/glade/u/home/bukovsky/ncl/phd/my_ncl_funct/convert.ncl"
; load "/glade/u/home/bukovsky/ncl/phd/WRF/my_wrf_functs/convertWRF.ncl"
; load "/glade/u/home/bukovsky/ncl/phd/WRF/my_wrf_functs/calc_wrf_msb.ncl"
; load "/glade/u/home/bukovsky/ncl/my_narccap_functs/narccap_plotting.ncl"
; load "/glade/u/home/bukovsky/ncl/my_narccap_functs/narccap_functs.ncl"

begin

 external UTIL17 "/ptmp/acaruthe/facets/swellj.so"
; external UTIL18 "/glade/u/home/bukovsky/ncl/my_narccap_functs/interp3dz.so"
;----------------------------------------------------------------------
;read in regcm variables 

f1 = addfile("infile.nc", "r") ;weve pre-defined infile in the command window 

   time = f1->time
   nt = dimsizes(time)   
 
   uvar = f1->ua
   vvar = f1->va
   zvar = f1->sigma

   lat2d = f1->xlat(:,:)
   lon2d = f1->xlon(:,:)

   uvar!0      = "time"
   uvar!1        = "lev"
   uvar!2      = "lat"
   uvar!3      = "lon"


   uvar@lat2d = lat2d
   uvar@lon2d = lon2d


   vvar!0      = "time"
   vvar!1      = "level"
   vvar!2      = "lat"
   vvar!3      = "lon"

   vvar@lat2d = lat2d
   vvar@lon2d = lon2d

   nlev = dimsizes(zvar)

   xlon = f1->xlon
   xlat = f1->xlat

   xc = f1->jx
   yc = f1->iy

   nx = dimsizes(xc)
   ny = dimsizes(yc)


   uvar@_FillValue = -999
   vvar@_FillValue = -999
   zvar@_FillValue = -999

   pname = uvar@grid_mapping
       proj = f1->$pname$
       print(proj)


f2 = addfile("sfcinfile.nc", "r") ;regcm has separate sfc files 

   usvar = f2->uas(5::6,0,:,:)
   vsvar = f2->vas(5::6,0,:,:) 

   lat2d = f2->xlat(:,:)
   lon2d = f2->xlon(:,:)

   usvar@lat2d = lat2d
   usvar@lon2d = lon2d
  
   vsvar@lat2d = lat2d
   vsvar@lon2d = lon2d

   usvar!0      = "time"
   usvar!1      = "lat"
   usvar!2      = "lon"

   vsvar!0      = "time"
   vsvar!1      = "lat"
   vsvar!2      = "lon"

   usvar@_FillValue = -999
   vsvar@_FillValue = -999

   
;note there are 6x the data in the sfc file, so we want every 6 sfc wind 
;atm files begin at month 01 06Z
;sfc begin at month 01 00z, so bring in every 6th time starting at 6z

print("brought in variables")


   date = cd_calendar(time,-3)
     ;option 3 YYYYMMDDHH
   yyyymm_all = cd_calendar(time,-1)
      ; YYYYMM

;DOING THIS PER FILE CHUNK AND THEN A DO LOOP OVER ALL TIMES IN IT HAS PROVEN
;TO BE TOO MUCH... SO WE ARE GOING TO DO THIS PER MONTH NOW INSTEAD

   datestart = cd_calendar(time(0),0)
      ;array of years, months, days, hours, minutes, seconds
   yyyystart = datestart(0,0)
      ; 0 is years 
   mmstart   = datestart(0,1)
      ; 1 is months 
   dateend = cd_calendar(time(nt-1),0)

print(datestart)
print(dateend)

   yyyymmstart = cd_calendar(time(0),-1)
     ; option 1 is YYYYMM
   yyyymmend   = cd_calendar(time(nt-1),-1)
     ; option 1 is YYYYMM

   
 
;START LOOP

   m = 0	;counter


;----------------------------------------------------------------------
;COMPUTE SFC-XKM SHEAR & LLJ STUFF


wks = gsn_open_wks("png","wtest")
 colormap = "BlueDarkOrange18"
 gsn_define_colormap(wks,colormap)



     res                     = True              ; Plot mods desired.
    res@gsnDraw             = True             ; We will panel later.
    res@gsnFrame            = True


   res@mpOutlineBoundarySets = "GeophysicalAndUSStates"

    res@gsnMaximize         = True              ; Maximize plot
    res@gsnSpreadColors     = True              ; use full range of colormap

    res@mpMaxLatF           =  50              ; Set accordingly
    res@mpMinLatF           =  20
    res@mpMinLonF           =  -130
    res@mpMaxLonF           =  -60

    res@cnFillOn            = True              ; color plot desired
    res@cnLinesOn           = False             ; turn off contour lines
    res@cnLineLabelsOn      = False             ; turn off contour lines

    res@lbLabelBarOn        = True              ; Labelbar will be in panel


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;LLJ

     msp  = usvar
     mwd  = usvar
     mshr   = usvar
     mhgt   = usvar
     lljcat = floattoint(usvar)

 
     msp  = msp@_FillValue
     mwd  = mwd@_FillValue
     mshr   = mshr@_FillValue
     mhgt   = mhgt@_FillValue
     lljcat = lljcat@_FillValue

     msp@long_name = "Maximum LLJ Speed"
     mwd@long_name = "Wind Direction of Max LLJ Speed"
     mshr@long_name = "Maximum Shear above the LLJ Speed Max"
     mhgt@long_name = "Height of LLJ Speed Max"
     lljcat@long_name = "LLJ Category"

     msp@standard_name = "Maximum LLJ Speed"
     mwd@standard_name = "Wind Direction of Max LLJ Speed"
     mshr@standard_name = "Maximum Shear above the LLJ Speed Max"
     mhgt@standard_name = "Height of LLJ Speed Max"
     lljcat@standard_name = "LLJ Category"

     msp@units = "m s-1"
     mwd@units = "deg"
     mshr@units = "m s-1"
     mhgt@units = "m"
     lljcat@units = "none"

;floattoint doesn't copy anything, and copy_VarMeta won't work because fillval is float,
;so do stuff here

     copy_VarCoords(usvar,lljcat)
     lljcat@grid_mapping = usvar@grid_mapping
     lljcat@coordinates = usvar@coordinates

;calculate wind speed and dir for whole atmos

 r2d = 45.0/atan(1.0)           ;conversion factor (radians to degrees)


     windspeed = (uvar*uvar + vvar*vvar)^0.5
     winddir   = atan2(uvar,vvar) * r2d + 180. ;give winddir in degrees 

     msp!0 = "time"
     msp!1 = "yc"
     msp!2 = "xc"

     mwd!0 = "time"
     mwd!1 = "yc"
     mwd!2 = "xc"
 
     mshr!0 = "time"
     mshr!1 = "yc"
     mshr!2 = "xc"

     mhgt!0 = "time"
     mhgt!1 = "yc"
     mhgt!2 = "xc"

     lljcat!0 = "time"
     lljcat!1 = "yc"
     lljcat!2 = "xc"

     windspeed!0 = "time"
     windspeed!1 = "lev"
     windspeed!2 = "yc"
     windspeed!3 = "xc"
 
     winddir!0 = "time"
     winddir!1 = "lev"
     winddir!2 = "yc"
     winddir!3 = "xc"

     msp@lat2d = xlat
     msp@lon2d = xlon
 
     mwd@lat2d = xlat
     mwd@lon2d = xlon

     mshr@lat2d = xlat
     mshr@lon2d = xlon

     lljcat@lat2d = xlat
     lljcat@lon2d = xlon

     windspeed@lat2d = xlat
     windspeed@lon2d = xlon
 
     winddir@lat2d = xlat
     winddir@lon2d = xlon

;;;;;; LLJ time!;;;;;;;

print("attempting subroutine now!")


;for whatever reason, the dimensions of the vars need to be reversed to be read into fortran
;coding is weird. 
;instead of time, lev, yc, xc,
;xc, yc, lev, time

     UTIL17::swellj(windspeed(xc|:,yc|:,lev|:,time|:),winddir(xc|:,yc|:,lev|:,time|:), \
                    zvar(kz|:),msp(xc|:,yc|:,time|:),                \
                    mwd(xc|:,yc|:,time|:),mshr(xc|:,yc|:,time|:),                      \
                    mhgt(xc|:,yc|:,time|:),lljcat(xc|:,yc|:,time|:),                   \
                    nt,nlev,ny,nx,zvar@_FillValue)               
     delete(windspeed)
     delete(winddir)
     

;SHEAR calculation
; 3km, and 1km wind 


; im just taking a guess here ... not sure I need the interp3dz subroutine since regcm is sigma based
; couldnt instead i just take the closest sigma level to the approx hght? 
; could eventually use hypsometric eqn to calculate hght, if there isnt a func already to do so
; well start with the simple way, then ask later

 u3 = uvar(:,10,:,:)

 u1 = uvar(:,13,:,:)

 v3 = vvar(:,10,:,:)

 v1 = vvar(:,13,:,:)


     delete(uvar)
     delete(vvar)
     delete(zvar)

;U and V differences

     du3 = (/u3 - usvar/)
     du1 = (/u1 - usvar/)
     dv3 = (/v3 - vsvar/)
     dv1 = (/v1 - vsvar/)
 
;BULK SHEAR = MAGNITUDE OF DIFFERENCE VECTOR...

     shear3 = du3
     shear3 = shear3@_FillValue
     shear3@long_name = "Sfc to 3km Bulk Shear"
     shear3@standard_name = "Sfc to 3km bulk shear"
     shear3@units = "m s-1"
     shear3@info = "Interpolation of wind speed to 3km for this calculation using a weighted linear interpolation (same as that done in NCL function wrf_interp_3d_z, but addapted for use with NARCCAP pressure level data)."


     shear3 = (/(du3*du3 + dv3*dv3)^0.5/)

     shear3@lat2d = xlat
     shear3@lon2d = xlon

     shear1 = du1
     shear1 = shear1@_FillValue
     shear1@long_name = "Sfc to 1km Bulk Shear"
     shear1@standard_name = "Sfc to 1km bulk shear"
     shear1@units = "m s-1"
     shear1@info = "Interpolation of wind speed to 1km for this calculation using a weighted linear interpolation (same as that done in NCL function wrf_interp_3d_z, but addapted for use with NARCCAP pressure level data)."

     shear1 = (/(du1*du1 + dv1*dv1)^0.5/)

     shear1@lat2d = xlat
     shear1@lon2d = xlon


     delete(usvar)
     delete(vsvar)
     delete(u3)
     delete(u1)
     delete(v3)
     delete(v1)
     delete(du3)
     delete(du1)
     delete(dv3)
     delete(dv1)

;CHECK
;
;     if(m.eq.0)then
;       printVarSummary(msp )
;       printVarSummary(mwd )
;       printVarSummary(mshr)
;       printVarSummary(mhgt)
;       printVarSummary(lljcat)
;       printVarSummary(shear3)
 ;      printVarSummary(shear1)
;     end if


opts = res

       plot = gsn_csm_contour_map(wks,msp(m,:,:),opts)
       opts@gsnLeftString = "MAXD"
       plot = gsn_csm_contour_map(wks,mwd(m,:,:),opts)
       opts@gsnLeftString = "MAXSHR"
       plot = gsn_csm_contour_map(wks,mshr(m,:,:),opts)
       opts@gsnLeftString = "MAXHGT"
       plot = gsn_csm_contour_map(wks,mhgt(m,:,:),opts)
       opts@gsnLeftString = "LLJCAT"
       plot = gsn_csm_contour_map(wks,lljcat(m,:,:),opts)
       opts@gsnLeftString = "SHEAR3"
       plot = gsn_csm_contour_map(wks,shear3(m,:,:),opts)
       opts@gsnLeftString = "SHEAR1"
       plot = gsn_csm_contour_map(wks,shear1(m,:,:),opts)

;----------------------------------------------------------------------
;NOW OUTPUT THIS ARRAY - DIFFERENT FILE FOR EACH LEVEL AND MONTH
;----------------------------------------------------------------------
;OUTPUT NEW FILES


     foutname1 = "llj.nc"
     dirout = "/ptmp/acaruthe/facets/"
     fout1 = dirout+foutname1

     foutname2 = "shear.nc"
     fout2 = dirout+foutname2

     system("rm "+fout1)
     system("rm "+fout2)

     ncdf1 = addfile(fout1,"c")
     ncdf2 = addfile(fout2,"c")

;ASSIGN FILE ATTRIBUTES

     fAtt = True
 
      fAtt@source_file = "infile.nc"
      fAtt@creation_date = systemfunc("date") 
      fAtt@Conventions = "None" 
     
     fileattdef(ncdf1,fAtt)
     fileattdef(ncdf2,fAtt)

;PREDEFINE COORDINATE INFO

     dimNames = (/"time","yc","xc"/)
     dimSizes = (/nt,ny,nx/)
     dimUnlim = (/True,False,False/)
    
     filedimdef(ncdf1,dimNames,dimSizes,dimUnlim)
     filedimdef(ncdf2,dimNames,dimSizes,dimUnlim)
    
    
     filevardef    (ncdf1, "time", typeof(time), "time")
     filevarattdef (ncdf1, "time", time)
    
     filevardef    (ncdf1, "xc", typeof(xc), "xc")
     filevarattdef (ncdf1, "xc", xc)
  
     filevardef    (ncdf1, "yc", typeof(yc), "yc")
     filevarattdef (ncdf1, "yc", yc)
  
     filevardef    (ncdf1, "lat2d", typeof(lat2d), (/"yc","xc"/) )
     filevarattdef (ncdf1, "lat2d", lat2d)
    
     filevardef    (ncdf1, "lon2d", typeof(lon2d), (/"yc","xc"/) )
     filevarattdef (ncdf1, "lon2d", lon2d)
    
     filevardef    (ncdf1, "msp", typeof(msp), (/"time","yc","xc"/) )
     filevarattdef (ncdf1, "msp", msp )
  
     filevardef    (ncdf1, "mwd", typeof(mwd), (/"time","yc","xc"/) )
     filevarattdef (ncdf1, "mwd", mwd )
  
     filevardef    (ncdf1, "mshr", typeof(mshr), (/"time","yc","xc"/) )
     filevarattdef (ncdf1, "mshr", mshr )
  
     filevardef    (ncdf1, "mhgt", typeof(mhgt), (/"time","yc","xc"/) )
     filevarattdef (ncdf1, "mhgt", mhgt )
  
     filevardef    (ncdf1, "lljcat", typeof(lljcat), (/"time","yc","xc"/) )
     filevarattdef (ncdf1, "lljcat", lljcat )
  
  
  
     filevardef    (ncdf2, "time", typeof(time), "time")
     filevarattdef (ncdf2, "time", time)
    
     filevardef    (ncdf2, "xc", typeof(xc), "xc")
     filevarattdef (ncdf2, "xc", xc)
  
     filevardef    (ncdf2, "yc", typeof(yc), "yc")
     filevarattdef (ncdf2, "yc", yc)
  
     filevardef    (ncdf2, "lat2d", typeof(lat2d), (/"yc","xc"/) )
     filevarattdef (ncdf2, "lat2d", lat2d)
    
     filevardef    (ncdf2, "lon2d", typeof(lon2d), (/"yc","xc"/) )
     filevarattdef (ncdf2, "lon2d", lon2d)
    
     filevardef    (ncdf2, "shear3", typeof(shear3), (/"time","yc","xc"/) )
     filevarattdef (ncdf2, "shear3", shear3)
  
     filevardef    (ncdf2, "shear1", typeof(shear1), (/"time","yc","xc"/) )
     filevarattdef (ncdf2, "shear1", shear1)
  
  
;WRITE VARS TO PREDEFINED LOCATIONS, ALREADY COPIED ATTS, SO
;TRANSFER VALUES ONLY (/.../)

     ncdf1->time	= (/time/)
     ncdf1->xc		= (/xc/)
     ncdf1->yc		= (/yc/)
     ncdf1->lat2d	= (/lat2d/)
     ncdf1->lon2d	= (/lon2d/)
     ncdf1->$pname$	= proj
     ncdf1->msp		= (/msp/)
     ncdf1->mwd		= (/mwd/)
     ncdf1->mshr	= (/mshr/)
     ncdf1->mhgt	= (/mhgt/)
     ncdf1->lljcat      = (/lljcat/)

     ncdf2->time	= (/time/)
     ncdf2->xc		= (/xc/)
     ncdf2->yc		= (/yc/)
     ncdf2->lat2d	= (/lat2d/)
     ncdf2->lon2d	= (/lon2d/)
     ncdf2->$pname$	= proj
     ncdf2->shear3	= (/shear3/)
     ncdf2->shear1	= (/shear1/)


;----------------------------------------------------------------------
;COUNTERS + 1

; m = m+1
; if(mm.eq.12)then
;   mm = 1
;   yyyy = yyyy+1
; else
;   mm = mm+1
; end if
; yyyymm = floattoint((yyyy*100)+mm)


 delete(fAtt)

 delete(msp)
 delete(mwd)
 delete(mshr)
 delete(mhgt)
 delete(lljcat)

 delete(shear3)
 delete(shear1)
 
 delete(time)
 delete(date)

print("DONE")
end
