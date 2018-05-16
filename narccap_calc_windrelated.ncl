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
 load "/glade/u/home/bukovsky/ncl/phd/my_ncl_funct/calc_climate.ncl"
 load "/glade/u/home/bukovsky/ncl/phd/my_ncl_funct/calc_stat.ncl"
 load "/glade/u/home/bukovsky/ncl/phd/my_ncl_funct/my_plot_functions.ncl"
 load "/glade/u/home/bukovsky/ncl/phd/my_ncl_funct/convert.ncl"
 load "/glade/u/home/bukovsky/ncl/phd/WRF/my_wrf_functs/convertWRF.ncl"
 load "/glade/u/home/bukovsky/ncl/phd/WRF/my_wrf_functs/calc_wrf_msb.ncl"
 load "/glade/u/home/bukovsky/ncl/my_narccap_functs/narccap_plotting.ncl"
 load "/glade/u/home/bukovsky/ncl/my_narccap_functs/narccap_functs.ncl"

begin

 external UTIL17 "/glade/u/home/bukovsky/ncl/my_narccap_functs/swellj.so"
 external UTIL18 "/glade/u/home/bukovsky/ncl/my_narccap_functs/interp3dz.so"
;----------------------------------------------------------------------
;             0      1      2      3      4      5
 models = (/"CRCM","ECP2","HRM3","MM5I","RCM3","WRFG"/)
;
;           (0)       (1)             (2)           (3)            (4)
 runs = (/"ncep","cgcm3-current","cgcm3-future","gfdl-current","gfdl-future", \
          "ccsm-current","ccsm-future","hadcm3-current","hadcm3-future"/)
;             (5)            (6)             (7)              (8)
;----------------------------------------------------------------------
;3D DATA DIRECTORY

 dir = "/glade/p/vetssg/data/narccap/DATA/"

;dirh = "/glade/p/image/bukovsky/narccap/"	;dir for geopotential height in case it is different
 dirh = "/glade/p/vetssg/data/narccap/bukovsky/"
;dirh = dir

 model = models(2)

 run = runs(0)

;SO WE KNOW WHICH YEARS/FILES WE ARE PULLING, SPECIFY: "ncep","current","future"

 period = "ncep"
;period = "current"
;period = "future"

;OUTPUT DIRECTORY

 dirout = "/glade/p/image/bukovsky/narccap/"+model+"/"+run+"/"

;----------------------------------------------------------------------
;----------------------------------------------------------------------
 frun = run     ;run in file name
 if(run.eq."ccsm-current".or.run.eq."ccsm-future")then
   frun = "ccsm"
 end if
 if(run.eq."cgcm3-current".or.run.eq."cgcm3-future")then
   frun = "cgcm3"
 end if
 if(run.eq."hadcm3-current".or.run.eq."hadcm3-future")then
   frun = "hadcm3"
 end if
 if(run.eq."gfdl-current".or.run.eq."gfdl-future")then
   frun = "gfdl"
 end if

;----------------------------------------------------------------------
;MUST READ IN ALL FILES FROM ALL LEVELS, SO DO PER FILE TIME CHUNK
;LIST FILE START DATE YEARS

 if(period.eq."ncep")then
   fyrs = (/"1979","1981","1986","1991","1996","2001"/)
 else
   if(period.eq."current")then
     fyrs = (/"1968","1971","1976","1981","1986","1991","1996"/)
   else
     fyrs = (/"2038","2041","2046","2051","2056","2061","2066"/)
   end if
 end if

 nfyrs = dimsizes(fyrs)

 r2d = 45.0/atan(1.0)		;conversion factor (radians to degrees)
;----------------------------------------------------------------------
;in case we want to plot this
 
;wks = gsn_open_wks("pdf","ztest")
 wks = gsn_open_wks("ncgm","wtest")
 colormap = "BlAqGrYeOrReVi200"
 gsn_define_colormap(wks,colormap)
 opts                           = True

 opts@mpLimitMode               = "Corners"
  
 opts@mpFillOn                  = False
 opts@mpOutlineDrawOrder        = "PostDraw"
 opts@mpOutlineBoundarySets     = "GeophysicalAndUSStates"
 opts@mpPerimDrawOrder          = "PostDraw"

 opts@tfDoNDCOverlay            = True
 
 opts@gsnAddCyclic              = False ;for regional
 
 opts@gsnSpreadColors           = True

 opts@cnFillOn                  = True
 opts@cnLinesOn                 = False
 opts@cnLineLabelsOn            = False
 opts@cnFillMode		= "CellFill"

;opts@cnMissingValPerimOn	= True
;opts@cnMissingValPerimColor	= "black"
 opts@cnMissingValFillPattern	= 0
 opts@cnMissingValFillColor	= "black"

 opts@lbLabelBarOn              = True               ;common label bar
 opts@lbLabelAngleF		= -90.

 opts@cnLevelSelectionMode  = "AutomaticLevels"



;DO THIS PER FILE YEAR CHUNK
;HERE

;do n = 0,nfyrs-1
 do n = 1,nfyrs-1
 

;----------------------------------------------------------------------
;----------------------------------------------------------------------
;READ IN THE THE NTH SET OF FILES

;  if(dir.eq."/glade/p/vetssg/data/narccap/DATA/")then
     if(n.eq.0.and.model.eq."WRFG".and.period.eq."ncep")then
       filsu = systemfunc("ls "+dir+model+"/"+run+"/table5/ua_"+model+"_"+frun+"_p*_"+fyrs(n)+"090103.nc")
       filsv = systemfunc("ls "+dir+model+"/"+run+"/table5/va_"+model+"_"+frun+"_p*_"+fyrs(n)+"090103.nc")
       filsus= systemfunc("ls "+dir+model+"/"+run+"/table2/uas_"+model+"_"+frun+"_"+fyrs(n)+"090103.nc")
       filsvs= systemfunc("ls "+dir+model+"/"+run+"/table2/vas_"+model+"_"+frun+"_"+fyrs(n)+"090103.nc")
     else
       filsu = systemfunc("ls "+dir+model+"/"+run+"/table5/ua_"+model+"_"+frun+"_p*_"+fyrs(n)+"010103.nc")
       filsv = systemfunc("ls "+dir+model+"/"+run+"/table5/va_"+model+"_"+frun+"_p*_"+fyrs(n)+"010103.nc")
       filsus= systemfunc("ls "+dir+model+"/"+run+"/table2/uas_"+model+"_"+frun+"_"+fyrs(n)+"010103.nc")
       filsvs= systemfunc("ls "+dir+model+"/"+run+"/table2/vas_"+model+"_"+frun+"_"+fyrs(n)+"010103.nc")
     end if
;  else
;    print("ADD NEW FILE POINTERS USING model AND run!")
;    exit
;  end if

   if(dirh.eq."/glade/p/vetssg/data/narccap/DATA/")then
     if(n.eq.0.and.model.eq."WRFG".and.period.eq."ncep")then
       filsz = systemfunc("ls "+dirh+model+"/"+run+"/table5/zg_"+model+"_"+frun+"_p*_"+fyrs(n)+"090103.nc")
     else
       filsz = systemfunc("ls "+dirh+model+"/"+run+"/table5/zg_"+model+"_"+frun+"_p*_"+fyrs(n)+"010103.nc")
     end if
   else
     if(dirh.eq."/glade/p/image/bukovsky/narccap/".or.dirh.eq."/glade/p/vetssg/data/narccap/bukovsky/")then
       filsz = systemfunc("ls "+dirh+model+"/"+run+"/zg_"+model+"_"+frun+"_p*_"+fyrs(n)+"010103.nc")
     else
       print("ADD FILE POINTER FOR GEOPOTENTIAL HEIGHT")
       exit
     end if
   end if

   fu  = addfiles(filsu,"r")
   fv  = addfiles(filsv,"r")
   fz  = addfiles(filsz,"r")
   fus = addfile(filsus,"r")
   fvs = addfile(filsvs,"r")

   ListSetType(fu,"join")	;join will put levels in different dim than time
   ListSetType(fv,"join")       ;level will be first dim
   ListSetType(fz,"join")


;READ IN VARIABLES
;HERE

;  if(n.eq.0)then		;these shouldn't change, just read them once
   if(n.eq.1)then		;these shouldn't change, just read them once

     yc = fz[0]->yc
     xc = fz[0]->xc
   
     lat = fz[0]->lat
     lon = fz[0]->lon

;HRM3 winds are on different grid, don't want that
     if(model.eq."HRM3")then
       uyc = fu[0]->yc
       uxc = fu[0]->xc
       ulat = fu[0]->lat
       ulon = fu[0]->lon
printVarInfo(ulat,"ulat")
printVarInfo(ulon,"ulon")
printVarInfo(lat,"lat")
printVarInfo(lon,"lon")
     end if

     level = fu[:]->level
     levelz = fz[:]->level	;for some reason z comes in in a different order

     if(level@units.eq."Pa".and.model.ne."MM5I")then    ;MM5 in hPa but labeled Pa
;    if(level@units.eq."Pa")then
       level = level/100.
       level@units = "hPa"      ;needed for hydro function
     end if
     if(model.eq."MM5I")then
       level@units = "hPa"
     end if

;    print(level)

     nlev = dimsizes(level)
;    if(nlev.ne.28)then
;      print("NUMBER OF LEVELS DOES NOT EQUAL 28 - THIS WILL CAUSE PROBLEMS!!!")
;      exit
;    end if

     ny = dimsizes(yc)
     nx = dimsizes(xc)

     opts@mpLeftCornerLatF          = lat(0,0)
     opts@mpLeftCornerLonF          = lon(0,0)
     opts@mpRightCornerLatF         = lat(ny-1,nx-1)
     opts@mpRightCornerLonF         = lon(ny-1,nx-1)

;AT THIS POINT LEVEL (AND EVERYTHING READ IN LATER FROM THE FILES) WILL HAVE MIXED
;LEVELS (NUMERICAL STRING ORDER DOES SOMETHING LIKE 50,1000,100,1025,1050,150...)
;NEED BOTTOM TO TOP ORDER FOR hydro

     ip = dim_pqsort(level,-2) ;permutation vector, sort level in decreasing order
     ipz = dim_pqsort(levelz,-2) ;permutation vector, sort level in decreasing order

printVarInfo(yc,"yc")
printVarInfo(xc,"xc")
printVarInfo(lon,"lon")
printVarInfo(lat,"lat")
printVarInfo(level,"level")
print(level)

   end if

   time = fu[0]->time
   nt = dimsizes(time)

printVarInfo(time,"time")

   date = cd_calendar(time,-3)

   yyyymm_all = cd_calendar(time,-1)

;DOING THIS PER FILE CHUNK AND THEN A DO LOOP OVER ALL TIMES IN IT HAS PROVEN
;TO BE TOO MUCH... SO WE ARE GOING TO DO THIS PER MONTH NOW INSTEAD

   datestart = cd_calendar(time(0),0)
   yyyystart = datestart(0,0)
   mmstart   = datestart(0,1)
  
   dateend = cd_calendar(time(nt-1),0)

   yyyymmstart = cd_calendar(time(0),-1)
   yyyymmend   = cd_calendar(time(nt-1),-1)

 
;START LOOP

   m = 0	;counter 

;  yyyymm = yyyymmstart
;  mm = mmstart
;  yyyy = yyyystart

;HERE

   if(n.eq.1)then
     yyyymm = 198101
     mm = 1.
     yyyy = 1981.
   end if

   do while (yyyymm.le.yyyymmend)

     iget = ind(yyyymm_all.eq.yyyymm)
     niget = dimsizes(iget)

     times = time(iget)
     dates = date(iget)

     if(niget.eq.1)then	;read from file with 1 time will not put in time dimension - add it
       uvar  = fu[:]->ua (:,iget,:,:)
       vvar  = fv[:]->va (:,iget,:,:)
       zvar  = fz[:]->zg (:,iget,:,:)
       usvar = fus->uas(iget,:,:)
       vsvar = fvs->vas(iget,:,:)

       udims = dimsizes(uvar)
       zdims = dimsizes(zvar)	;in case they are different
       tmpu  = new( (/udims(0),1,udims(1),udims(2)/),float)
       tmpv  = new( (/udims(0),1,udims(1),udims(2)/),float)
       tmpz  = new( (/zdims(0),1,zdims(1),zdims(2)/),float)
       tmpus = new( (/1,udims(1),udims(2)/),float)
       tmpvs = new( (/1,udims(1),udims(2)/),float)

       delete(udims)

       printVarInfo(uvar,"uvar")
       printVarInfo(vvar,"vvar")
       printVarInfo(zvar,"zvar")
  
       tmpu(:,0,:,:) = uvar
       tmpv(:,0,:,:) = vvar
       tmpz(:,0,:,:) = zvar
       tmpus(0,:,:)   = usvar
       tmpvs(0,:,:)   = vsvar

       copy_VarAtts(uvar,tmpu)
       copy_VarAtts(vvar,tmpv)
       copy_VarAtts(zvar,tmpz)
       copy_VarAtts(usvar,tmpus)
       copy_VarAtts(vsvar,tmpvs)
       
       tmpu!0 = uvar!0
       tmpu!1 = "time"
       tmpu!2 = uvar!1
       tmpu!3 = uvar!2
       tmpu&time = times
       tmpu&xc = uxc
       tmpu&yc = uyc

       tmpv!0 = vvar!0
       tmpv!1 = "time"
       tmpv!2 = vvar!1
       tmpv!3 = vvar!2
       tmpv&time = times
       tmpv&xc = uxc
       tmpv&yc = uyc

       tmpz!0 = zvar!0
       tmpz!1 = "time"
       tmpz!2 = zvar!1
       tmpz!3 = zvar!2
       tmpz&time = times
       tmpz&xc = xc
       tmpz&yc = yc

       tmpus!0 = "time"
       tmpus!1 = usvar!0
       tmpus!2 = usvar!1
       tmpus&time = times
       tmpus&xc = uxc
       tmpus&yc = uyc

       tmpvs!0 = "time"
       tmpvs!1 = vsvar!0
       tmpvs!2 = vsvar!1
       tmpvs&time = times
       tmpvs&xc = uxc
       tmpvs&yc = uyc

       delete(uvar)
       delete(vvar)
       delete(zvar)
       delete(usvar)
       delete(vsvar)
 
       uvar  = tmpu
       vvar  = tmpv
       zvar  = tmpz
       usvar = tmpus
       vsvar = tmpvs
     else
       uvar = fu[:]->ua (:,iget,:,:)
       vvar = fv[:]->va (:,iget,:,:)
       zvar = fz[:]->zg(:,iget,:,:)

       usvar = fus->uas(iget,:,:)
       vsvar = fvs->vas(iget,:,:)
     end if

     uvar!0 = "lev"
     vvar!0 = "lev"
     zvar!0 = "lev"

     print("DOING "+dates(0)+"-"+dates(niget-1))

;print(zvar(:,1,1,1))

;sort by level (same permutation vector as above)
     uvar = uvar(ip,:,:,:)
     vvar = vvar(ip,:,:,:)
     zvar = zvar(ipz,:,:,:)

;print(zvar(:,1,1,1))

     if(m.eq.0)then
       printVarInfo(uvar,"uvar")
       print("U Num Missing: "+num(ismissing(uvar)))
       printVarInfo(vvar,"vvar")
       print("V Num Missing: "+num(ismissing(vvar)))
       printVarInfo(zvar,"zvar")
       print("Z Num Missing: "+num(ismissing(zvar)))  
       printVarInfo(usvar,"uas")
       print("UAS Num Missing: "+num(ismissing(usvar)))
       printVarInfo(vsvar,"vas")
       print("VAS Num Missing: "+num(ismissing(vsvar)))

       pname = uvar@grid_mapping
       proj = fu[0]->$pname$
       print(proj)
     end if

     uvar  = change_missing_val2(uvar,-999.)
     vvar  = change_missing_val2(vvar,-999.)
     zvar  = change_missing_val2(zvar,-999.)
     usvar = change_missing_val2(usvar,-999.)
     vsvar = change_missing_val2(vsvar,-999.)

     opts = set_map_proj_res(fu[0],uvar@grid_mapping,opts)

;----------------------------------------------------------------------
;if hadley, put winds on same grid as z first

     if(model.eq."HRM3")then
    
       udims = dimsizes(uvar)
    
       lat1d = fspan(min(ulat),max(ulat),udims(2))
       lon1d = fspan(min(ulon),max(ulon),udims(3))

       delete(udims)

       ugrid = rcm2rgrid_Wrap(ulat,ulon,uvar,lat1d,lon1d,0)
       unew  = rgrid2rcm_Wrap(lat1d,lon1d,ugrid,lat,lon,0)
 
       vgrid = rcm2rgrid_Wrap(ulat,ulon,vvar,lat1d,lon1d,0)
       vnew  = rgrid2rcm_Wrap(lat1d,lon1d,vgrid,lat,lon,0)

       usgrid = rcm2rgrid_Wrap(ulat,ulon,usvar,lat1d,lon1d,0)
       usnew  = rgrid2rcm_Wrap(lat1d,lon1d,usgrid,lat,lon,0)
 
       vsgrid = rcm2rgrid_Wrap(ulat,ulon,vsvar,lat1d,lon1d,0)
       vsnew  = rgrid2rcm_Wrap(lat1d,lon1d,vsgrid,lat,lon,0)

       copy_VarAtts(uvar,unew)
       copy_VarAtts(vvar,vnew)
       copy_VarAtts(usvar,usnew)
       copy_VarAtts(vsvar,vsnew)

       unew!0 = uvar!0
       unew!1 = uvar!1
       unew!2 = uvar!2
       unew!3 = uvar!3

       vnew!0 = vvar!0
       vnew!1 = vvar!1
       vnew!2 = vvar!2
       vnew!3 = vvar!3

       usnew!0 = usvar!0
       usnew!1 = usvar!1
       usnew!2 = usvar!2

       vsnew!0 = vsvar!0
       vsnew!1 = vsvar!1
       vsnew!2 = vsvar!2

       delete(uvar)
       delete(vvar)
       delete(usvar)
       delete(vsvar)
       uvar = unew
       vvar = vnew
       usvar = usnew
       vsvar = vsnew
       delete(unew)
       delete(vnew)
       delete(usnew)
       delete(vsnew)

       delete(lat1d)
       delete(lon1d)
       delete(ugrid)
       delete(vgrid)
       delete(usgrid)
       delete(vsgrid)

       printVarInfo(uvar,"new uvar")
       printVarInfo(vvar,"new vvar")
       printVarInfo(usvar,"new usvar")
       printVarInfo(vsvar,"new vsvar")

     end if

;----------------------------------------------------------------------
;COMPUTE SFC-XKM SHEAR & LLJ STUFF

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

     windspeed = (uvar*uvar + vvar*vvar)^0.5
     winddir   = atan2(uvar,vvar) * r2d + 180.

     windspeed!0 = "lev"
     windspeed!1 = "time"
     windspeed!2 = "yc"
     windspeed!3 = "xc"
 
     winddir!0 = "lev"
     winddir!1 = "time"
     winddir!2 = "yc"
     winddir!3 = "xc"
 
;LLJ stuff

     
     if(m.eq.0)then
       printVarInfo(windspeed,"windspeed")
       printVarInfo(winddir,"winddir")
     end if

     UTIL17::swellj(windspeed(lev|:,xc|:,yc|:,time|:),winddir(lev|:,xc|:,yc|:,time|:), \
                    zvar(lev|:,xc|:,yc|:,time|:),msp(xc|:,yc|:,time|:),                \
                    mwd(xc|:,yc|:,time|:),mshr(xc|:,yc|:,time|:),                      \
                    mhgt(xc|:,yc|:,time|:),lljcat(xc|:,yc|:,time|:),                   \
                    niget,nlev,ny,nx,zvar@_FillValue)               
     delete(windspeed)
     delete(winddir)
     

;SHEAR calculation

     ;6-km, 5km, 3km, and 1km wind 

      u6 = usvar
      u5 = usvar
      u3 = usvar
      u1 = usvar

      v6 = usvar
      v5 = usvar
      v3 = usvar
      v1 = usvar

      u6 = usvar@_FillValue
      u5 = usvar@_FillValue
      u3 = usvar@_FillValue
      u1 = usvar@_FillValue

      v6 = usvar@_FillValue
      v5 = usvar@_FillValue
      v3 = usvar@_FillValue
      v1 = usvar@_FillValue

      UTIL18::interp3dz(uvar(lev|:,xc|:,yc|:,time|:),zvar(lev|:,xc|:,yc|:,time|:),6000.,u6(xc|:,yc|:,time|:), \
                        niget,nlev,ny,nx,zvar@_FillValue)
      UTIL18::interp3dz(uvar(lev|:,xc|:,yc|:,time|:),zvar(lev|:,xc|:,yc|:,time|:),5000.,u5(xc|:,yc|:,time|:), \
                        niget,nlev,ny,nx,zvar@_FillValue)
      UTIL18::interp3dz(uvar(lev|:,xc|:,yc|:,time|:),zvar(lev|:,xc|:,yc|:,time|:),3000.,u3(xc|:,yc|:,time|:), \
                        niget,nlev,ny,nx,zvar@_FillValue)
      UTIL18::interp3dz(uvar(lev|:,xc|:,yc|:,time|:),zvar(lev|:,xc|:,yc|:,time|:),1000.,u1(xc|:,yc|:,time|:), \
                        niget,nlev,ny,nx,zvar@_FillValue)
      UTIL18::interp3dz(vvar(lev|:,xc|:,yc|:,time|:),zvar(lev|:,xc|:,yc|:,time|:),6000.,v6(xc|:,yc|:,time|:), \
                        niget,nlev,ny,nx,zvar@_FillValue)
      UTIL18::interp3dz(vvar(lev|:,xc|:,yc|:,time|:),zvar(lev|:,xc|:,yc|:,time|:),5000.,v5(xc|:,yc|:,time|:), \
                        niget,nlev,ny,nx,zvar@_FillValue)
      UTIL18::interp3dz(vvar(lev|:,xc|:,yc|:,time|:),zvar(lev|:,xc|:,yc|:,time|:),3000.,v3(xc|:,yc|:,time|:), \
                        niget,nlev,ny,nx,zvar@_FillValue)
      UTIL18::interp3dz(vvar(lev|:,xc|:,yc|:,time|:),zvar(lev|:,xc|:,yc|:,time|:),1000.,v1(xc|:,yc|:,time|:), \
                        niget,nlev,ny,nx,zvar@_FillValue)

     delete(uvar)
     delete(vvar)
     delete(zvar)

;U and V differences

     du6 = (/u6 - usvar/)
     du5 = (/u5 - usvar/)
     du3 = (/u3 - usvar/)
     du1 = (/u1 - usvar/)
     dv6 = (/v6 - vsvar/)
     dv5 = (/v5 - vsvar/)
     dv3 = (/v3 - vsvar/)
     dv1 = (/v1 - vsvar/)
   
;BULK SHEAR = MAGNITUDE OF DIFFERENCE VECTOR...

     shear6 = usvar
     shear6 = shear6@_FillValue
     shear6@long_name = "Sfc to 6km Bulk Shear"
     shear6@standard_name = "Sfc to 6km bulk shear"
     shear6@units = "m s-1"
     shear6@info = "Interpolation of wind speed to 6km for this calculation using a weighted linear interpolation (same as that done in NCL function wrf_interp_3d_z, but addapted for use with NARCCAP pressure level data)."

     shear6 = (/(du6*du6 + dv6*dv6)^0.5/)

     shear5 = usvar
     shear5 = shear5@_FillValue
     shear5@long_name = "Sfc to 5km Bulk Shear"
     shear5@standard_name = "Sfc to 5km bulk shear"
     shear5@units = "m s-1"
     shear5@info = "Interpolation of wind speed to 5km for this calculation using a weighted linear interpolation (same as that done in NCL function wrf_interp_3d_z, but addapted for use with NARCCAP pressure level data)."

     shear5 = (/(du5*du5 + dv5*dv5)^0.5/)

     shear3 = usvar
     shear3 = shear3@_FillValue
     shear3@long_name = "Sfc to 3km Bulk Shear"
     shear3@standard_name = "Sfc to 3km bulk shear"
     shear3@units = "m s-1"
     shear3@info = "Interpolation of wind speed to 3km for this calculation using a weighted linear interpolation (same as that done in NCL function wrf_interp_3d_z, but addapted for use with NARCCAP pressure level data)."

     shear3 = (/(du3*du3 + dv3*dv3)^0.5/)

     shear1 = usvar
     shear1 = shear1@_FillValue
     shear1@long_name = "Sfc to 1km Bulk Shear"
     shear1@standard_name = "Sfc to 1km bulk shear"
     shear1@units = "m s-1"
     shear1@info = "Interpolation of wind speed to 1km for this calculation using a weighted linear interpolation (same as that done in NCL function wrf_interp_3d_z, but addapted for use with NARCCAP pressure level data)."

     shear1 = (/(du1*du1 + dv1*dv1)^0.5/)

     delete(usvar)
     delete(vsvar)
     delete(u6)
     delete(u5)
     delete(u3)
     delete(u1)
     delete(v6)
     delete(v5)
     delete(v3)
     delete(v1)
     delete(du6)
     delete(du5)
     delete(du3)
     delete(du1)
     delete(dv6)
     delete(dv5)
     delete(dv3)
     delete(dv1)

;CHECK

     if(m.eq.0)then
       printVarInfo(msp ,"msp ")
       printVarInfo(mwd ,"mwd ")
       printVarInfo(mshr  ,"mshr  ")
       printVarInfo(mhgt,"mhgt")
       printVarInfo(lljcat,"lljcat")
       printVarInfo(shear6,"shear6")
       printVarInfo(shear5,"shear5")
       printVarInfo(shear3,"shear3")
       printVarInfo(shear1,"shear1")
     end if

     if(m.eq.0)then
       opts@gsnRightString = yyyymm
       opts@gsnLeftString = "MAXU"
       plot = gsn_csm_contour_map(wks,msp(m,:,:),opts)
       opts@gsnLeftString = "MAXD"
       plot = gsn_csm_contour_map(wks,mwd(m,:,:),opts)
       opts@gsnLeftString = "MAXSHR"
       plot = gsn_csm_contour_map(wks,mshr(m,:,:),opts)
       opts@gsnLeftString = "MAXHGT"
       plot = gsn_csm_contour_map(wks,mhgt(m,:,:),opts)
       opts@gsnLeftString = "LLJCAT"
       plot = gsn_csm_contour_map(wks,lljcat(m,:,:),opts)
       opts@gsnLeftString = "SHEAR5"
       plot = gsn_csm_contour_map(wks,shear5(m,:,:),opts)
       opts@gsnLeftString = "SHEAR3"
       plot = gsn_csm_contour_map(wks,shear3(m,:,:),opts)
       opts@gsnLeftString = "SHEAR1"
       plot = gsn_csm_contour_map(wks,shear1(m,:,:),opts)
     end if

;----------------------------------------------------------------------
;NOW OUTPUT THIS ARRAY - DIFFERENT FILE FOR EACH LEVEL AND MONTH
;----------------------------------------------------------------------
;OUTPUT NEW FILES

     foutname1 = "llj_"+model+"_"+run+"_"+dates(0)+"-"+dates(niget-1)+".nc"
     fout1 = dirout+foutname1

     foutname2 = "shear_"+model+"_"+run+"_"+dates(0)+"-"+dates(niget-1)+".nc"
     fout2 = dirout+foutname2

     system("rm "+fout1)
     system("rm "+fout2)

     ncdf1 = addfile(fout1,"c")
     ncdf2 = addfile(fout2,"c")

;ASSIGN FILE ATTRIBUTES

     fatt = True
  
     fatts = getvaratts(fu[0])
     if(.not.all(ismissing(fatts)))then
       do i = 0,dimsizes(fatts)-1
          ncdf1@$fatts(i)$ = fu[0]@$fatts(i)$	;copy over original file atts
          ncdf2@$fatts(i)$ = fu[0]@$fatts(i)$	;copy over original file atts
       end do
     end if
  
     fatt@other = "Variables created using original 3D NARCCAP output and post-calculated geopotential height on "+systemfunc("date")
  
     fileattdef(ncdf1,fatt)
     fileattdef(ncdf2,fatt)

;PREDEFINE COORDINATE INFO

     dimNames = (/"time","yc","xc"/)
     dimSizes = (/niget,ny,nx/)
     dimUnlim = (/True,False,False/)
    
     filedimdef(ncdf1,dimNames,dimSizes,dimUnlim)
     filedimdef(ncdf2,dimNames,dimSizes,dimUnlim)
    
    
     filevardef    (ncdf1, "time", typeof(time), "time")
     filevarattdef (ncdf1, "time", time)
    
     filevardef    (ncdf1, "xc", typeof(xc), "xc")
     filevarattdef (ncdf1, "xc", xc)
  
     filevardef    (ncdf1, "yc", typeof(yc), "yc")
     filevarattdef (ncdf1, "yc", yc)
  
     filevardef    (ncdf1, "lat", typeof(lat), (/"yc","xc"/) )
     filevarattdef (ncdf1, "lat", lat)
    
     filevardef    (ncdf1, "lon", typeof(lon), (/"yc","xc"/) )
     filevarattdef (ncdf1, "lon", lon)
    
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
  
     filevardef    (ncdf2, "lat", typeof(lat), (/"yc","xc"/) )
     filevarattdef (ncdf2, "lat", lat)
    
     filevardef    (ncdf2, "lon", typeof(lon), (/"yc","xc"/) )
     filevarattdef (ncdf2, "lon", lon)
    
     filevardef    (ncdf2, "shear6", typeof(shear6), (/"time","yc","xc"/) )
     filevarattdef (ncdf2, "shear6", shear6)
  
     filevardef    (ncdf2, "shear5", typeof(shear5), (/"time","yc","xc"/) )
     filevarattdef (ncdf2, "shear5", shear5)
  
     filevardef    (ncdf2, "shear3", typeof(shear3), (/"time","yc","xc"/) )
     filevarattdef (ncdf2, "shear3", shear3)
  
     filevardef    (ncdf2, "shear1", typeof(shear1), (/"time","yc","xc"/) )
     filevarattdef (ncdf2, "shear1", shear1)
  
  
;WRITE VARS TO PREDEFINED LOCATIONS, ALREADY COPIED ATTS, SO
;TRANSFER VALUES ONLY (/.../)

     ncdf1->time	= (/times/)
     ncdf1->xc		= (/xc/)
     ncdf1->yc		= (/yc/)
     ncdf1->lat		= (/lat/)
     ncdf1->lon		= (/lon/)
     ncdf1->$pname$	= proj
     ncdf1->msp		= (/msp/)
     ncdf1->mwd		= (/mwd/)
     ncdf1->mshr	= (/mshr/)
     ncdf1->mhgt	= (/mhgt/)
     ncdf1->lljcat      = (/lljcat/)

     ncdf2->time	= (/times/)
     ncdf2->xc		= (/xc/)
     ncdf2->yc		= (/yc/)
     ncdf2->lat		= (/lat/)
     ncdf2->lon		= (/lon/)
     ncdf2->$pname$	= proj
     ncdf2->shear6	= (/shear6/)
     ncdf2->shear5	= (/shear5/)
     ncdf2->shear3	= (/shear3/)
     ncdf2->shear1	= (/shear1/)


;----------------------------------------------------------------------
;COUNTERS + 1

 m = m+1
 if(mm.eq.12)then
   mm = 1
   yyyy = yyyy+1
 else
   mm = mm+1
 end if
 yyyymm = floattoint((yyyy*100)+mm)


 delete(fatts)

 delete(msp)
 delete(mwd)
 delete(mshr)
 delete(mhgt)
 delete(lljcat)

 delete(shear6)
 delete(shear5)
 delete(shear3)
 delete(shear1)
 
 delete(iget)
 delete(times)
 delete(dates)

;exit	;HERE... for 1 month

end do		;end loop through this month


 delete(filsu)
 delete(filsv)
 delete(filsz)
 delete(filsus)
 delete(filsvs)
 delete(fu)
 delete(fv)
 delete(fz)
 delete(fus)
 delete(fvs)
 delete(time)
 delete(date)
 delete(yyyymm_all)


 end do		;end loop through time chunks


;----------------------------------------------------------------------
;----------------------------------------------------------------------
print("DONE")
end
