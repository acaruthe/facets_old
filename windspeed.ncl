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

 external UTIL17 "/ptmp/acaruthe/facets/speed.so"
;----------------------------------------------------------------------
;read in regcm variables 

f1 = addfile("infile.nc", "r") ;weve pre-defined infile in the command window 

   time = f1->time
   nt = dimsizes(time)   
 
   uvar = f1->ua
   vvar = f1->va
   zvar = f1->sigma

   topo = f1->topo

   xlon = f1->xlon
   xlat = f1->xlat

   lat2d = f1->xlat(:,:)
   lon2d = f1->xlon(:,:)

   topo@lat2d = lat2d
   topo@lon2d = lon2d

 ;  topo!0 = "lat"
 ;  topo!1 = "lon"


   uvar!0      = "time"
   uvar!1        = "lev"
   uvar!1      = "lat"
   uvar!2      = "lon"


   uvar@lat2d = lat2d
   uvar@lon2d = lon2d


   vvar!0      = "time"
   vvar!1      = "level"
   vvar!2      = "lat"
   vvar!3      = "lon"

   vvar@lat2d = lat2d
   vvar@lon2d = lon2d

   nlev = dimsizes(zvar)

   xc = f1->jx
   yc = f1->iy

   nx = dimsizes(xc)
   ny = dimsizes(yc)


   uvar@_FillValue = -999
   vvar@_FillValue = -999
   zvar@_FillValue = -999


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

   yyyymmstart = cd_calendar(time(0),-1)
     ; option 1 is YYYYMM
   yyyymmend   = cd_calendar(time(nt-1),-1)
     ; option 1 is YYYYMM

 
;START LOOP

   m = 0	;counter


;----------------------------------------------------------------------
;COMPUTE SFC-XKM SHEAR & LLJ STUFF


 wks = gsn_open_wks("png","wtest")

 colormap = "BlAqGrYeOrReVi200"
 gsn_define_colormap(wks,colormap)
 
     res                     = True              ; Plot mods desired.
    res@gsnDraw             = True             ; We will panel later.
    res@gsnFrame            = True

;    res@mpProjection      = "LambertConformal"
    res@gsnMaximize         = True              ; Maximize plot
    res@gsnSpreadColors     = True              ; use full range of colormap

;    res@mpMaxLatF           =  60              ; Set accordingly
;    res@mpMinLatF           =  24
;    res@mpMinLonF           = -30
;    res@mpMaxLonF           =  60

    res@cnFillOn            = True              ; color plot desired
    res@cnLinesOn           = False             ; turn off contour lines
    res@cnLineLabelsOn      = False             ; turn off contour lines

;    res@cnLevelSelectionMode =  "ManualLevels"   ; manual levels
;    res@cnMinLevelValF       = 0                 ; min level
;    res@cnMaxLevelValF       = 6000                ; max level
;    res@cnLevelSpacingF      =  250               ; interval

    res@lbLabelBarOn        = True              ; Labelbar will be in panel


 plot =  gsn_csm_contour_map(wks,topo,res)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;LLJ

     msp  = uvar(:,0,:,:)
 
     msp  = msp@_FillValue
     
     msp@standard_name = "Maximum LLJ Speed"

     msp@units = "m s-1"

;floattoint doesn't copy anything, and copy_VarMeta won't work because fillval is float,
;so do stuff here

;calculate wind speed and dir for whole atmos


     windspeed = (uvar*uvar + vvar*vvar)^0.5
printVarSummary(windspeed)


     msp!0 = "time"
     msp!1 = "yc"
     msp!2 = "xc"


     windspeed!0 = "time"
     windspeed!1 = "lev"
     windspeed!2 = "yc"
     windspeed!3 = "xc"
 

     msp@lat2d = xlat
     msp@lon2d = xlon

;LLJ stuff

print("attempting subroutine now!")
;
;for whatever reason, the dimensions of the vars need to be reversed to be read into fortran
;coding is weird. 
;instead of time, elve, yc, xc,
;xc, yc, lev, time

     UTIL17::swellj(windspeed(xc|:,yc|:,lev|:,time|:),msp(xc|:,yc|:,time|:), \
                    nt,nlev,ny,nx,zvar@_FillValue)               
     delete(windspeed)
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

printVarSummary(msp)

opts = res

       plot = gsn_csm_contour_map(wks,msp(m,:,:),opts)
       opts@gsnLeftString = "MAXD"

;----------------------------------------------------------------------
print("DONE")
end
