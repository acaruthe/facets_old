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

begin

;----------------------------------------------------------------------
;read in regcm variables 

dir = "/mnt/nrel/acaruthe/facets/"

do year = 1989,2010

;print(year)

f1 = addfile(dir+"llj_"+year+".nc", "r") ;weve pre-defined infile in the command window 

   time = f1->time
   nt = dimsizes(time)   
 
   msp = f1->msp
   mwd = f1->mwd
   mshr = f1->mshr
   mhgt = f1->mhgt

   xc = f1->xc
   yc = f1->yc

   xlat = f1->xlat
   xlon = f1->xlon
 
   lat2d = f1->xlat(:,:)
   lon2d = f1->xlon(:,:)

   
     msp!0 = "time"
     msp!1 = "yc"
     msp!2 = "xc"

;printVarSummary(msp)

     mwd!0 = "time"
     mwd!1 = "yc"
     mwd!2 = "xc"

     mshr!0 = "time"
     mshr!1 = "yc"
     mshr!2 = "xc"

     mhgt!0 = "time"
     mhgt!1 = "yc"
     mhgt!2 = "xc"
;;;idk why, but this needs to be deleted bc its annoying;;;;;

   delete(msp@lat2d)
   delete(msp@lon2d)

   msp@lat2d = lat2d
   msp@lon2d = lon2d

  
   pname = msp@grid_mapping
       proj = f1->$pname$
       ;print(proj)


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

 

;;;; finding the max at each hour;;;;;;



;----------------------------------------------------------------------
;COMPUTE SFC-XKM SHEAR & LLJ STUFF


wks = gsn_open_wks("png","msp"+year)
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

  res@cnLevelSelectionMode = "ManualLevels"
  res@cnMinLevelValF       = 0.0
  res@cnMaxLevelValF       = 60.0
  res@cnLevelSpacingF      = 3.0
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   opts = res


;do m = 0,3

      mspmax = dim_max_n(msp,0)


printVarSummary(mspmax)
      mspmax@lat2d = xlat
      mspmax@lon2d = xlon

 
        opts@gsnLeftString = "MAXWSP"+year
        ; plot = gsn_csm_contour_map(wks,msp(m,:,:), opts)
        plot = gsn_csm_contour_map(wks,mspmax,opts)
;       opts@gsnLeftString = "MAXD"
;       plot = gsn_csm_contour_map(wks,mwd(m,:,:),opts)
;       opts@gsnLeftString = "MAXSHR"
;       plot = gsn_csm_contour_map(wks,mshr(m,:,:),opts)
;       opts@gsnLeftString = "MAXHGT"
;       plot = gsn_csm_contour_map(wks,mhgt(m,:,:),opts)
;       opts@gsnLeftString = "LLJCAT"
;       plot = gsn_csm_contour_map(wks,lljcat(m,:,:),opts)
;       opts@gsnLeftString = "LLJCAT1"
;       plot = gsn_csm_contour_map(wks,lljcat1_total,opts)
;       opts@gsnLeftString = "LLJCAT2"
;       plot = gsn_csm_contour_map(wks,lljcat2_total,opts)
;       opts@gsnLeftString = "LLJCAT3"
;       plot = gsn_csm_contour_map(wks,lljcat3_total,opts)
;       opts@gsnLeftString = "SHEAR3"
;       plot = gsn_csm_contour_map(wks,shear3(m,:,:),opts)
;       opts@gsnLeftString = "SHEAR1"
;       plot = gsn_csm_contour_map(wks,shear1(m,:,:),opts)


end do 
;end do 
print("DONE")
end
