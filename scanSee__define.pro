;
; this currently read the version1_1 scanSee data format
;
@u_read.pro
@fit_statistic.pro
; @readScan.pro
@colorbar.pro
@scan2d_roi.pro
@view3d_2d.pro

PRO scanSee_pick3d,file,image_array,pickDet=pickDet,Dump=dump,Group=group
; pickDet - specifies the desired detector # for big 3D scan file
; dump    - dump read info
; file    - input 3D scan file name
; image_array - return image_array

	pick = 1
	if keyword_set(pickDet) then pick = pickDet
	t1 = systime(1)
	r = read_scan(file,Scan,pickDet=pick,dump=dump)
	print,'Time used in read detector # ',strtrim(pick,2),'  ',systime(1)-t1

	if r eq -1 then return
	detname =  'D'+ [strtrim(indgen(9)+1,2),'A','B','C','D','E','F' , $
        	'01','02','03','04','05','06','07','08','09', $
        	strtrim(indgen(61)+10,2)]

	title = strmid(file,rstrpos(file,!os.file_sep)+1,strlen(file))

	image_array = *(*Scan.da)[0]
;	sz = size(image_array)
;	panimage,image_array,indgen(sz(3))+1
	view3d_2d,image_array,title=title+'('+detname(pick-1)+')',Group=group
END


PRO scanSee::Ezfit,detno=detno,row=row,column=column,group=group
view=0
if keyword_set(detno) then view=detno
lineno=0
if keyword_set(row) then lineno = row-1

      self->read,dim=dim,cpt=cpt,labels=labels,id_def=id_def, $
                da1d=da,pa1d=pa,da2d=da2d,pa2d=pa2d, $
		view=view,x=xa,y=ya,im=im

	if self.dim eq 2 then begin
	wd = n_elements(xa)
	ht = n_elements(ya)
	im2 = im(*,0:ht-1)
	if keyword_set(column) then $
        ez_fit,xarray=xa,yarray=ya,im=im2,group=group,inpath=self.path, $
                ipick=column-1 else $
        ez_fit,xarray=xa,yarray=ya,im=im2,group=group,inpath=self.path, $
                jpick=lineno
        end

        if dim eq 1 then begin
	ez_fit,xarray=xa,yarray=ya,im=da,group=group,inpath=self.path
	return
                xa = pa(*,0)
                def = id_def(*,0)
                def = def(4:n_elements(def)-1)
                nd = fix(total(def)+.1)
                ya = make_array(cpt(0),nd)
                id =0
                for i=0,nd-1 do begin
                if def(i) gt 0 then begin
                ya(*,id) = da(*,i)
                id = id + 1
                end
                end
        ez_fit,xarray=xa,yarray=ya,group=group,inpath=self.path
	end

END

PRO scanSee::Statistic,VX,VY,C_MASS=c_mass,X_PEAK=x_peak,Y_PEAK=y_peak, $
	Y_HPEAK=y_hpeak, X_HWDL=x_hwdl,X_HWDR=x_hwdr, FWHM=fwhm,LIST=LIST, $
	row=row,DETECTOR=detector,FIT=FIT,REPORT=REPORT,TITLE=TITLE,GROUP=group
;+
; NAME:
;       scanSee::Statistic
;
; PURPOSE:
;       This method allows the user to calculate peak,fwhm width
;
; CALLING SEQUENCE:
;       Obj->[scanSee::]STATISTIC [,X,Y] ,C_MASS=c_mass,X_PEAK=x_peak, $
;                      Y_PEAK=y_peak,Y_HPEAK=y_hpeak, $
;                      X_HWDL=x_hwdl,X_HWDR=x_hwdr,FWHM=fwhm, $
;                      [,Row=row] [,DETECTOR=detector]
;
; ARGUMENTS:
;    VX      - specifies/returns the independent variable X
;    VY      - specifies/returns the dependent variable Y
;
; KEYWORDS:
;    ROW     - specifies the 1D scan row sequence number valid only if 
;              2D scan file opened 
;    COLUMN  - specifies the column number valid only if 
;              2D scan file opened 
;    DETECTOR- specifies the desired detector number , default to 1
;    C_MASS  - returns the center of mass of the Y curve
;    X_PEAK  - returns the X coordinate corresponding to peak Y value
;    Y_PEAK  - returns the peak Y value
;    Y_HPEAK - returns the Y value at the FWHM width
;    X_FWDL  - returns the left end of X coordinate of the FWHM width 
;    X_FWDR  - returns the right end of X coordinate of the FWHM width 
;    FWHM    - returns the full width of the half peak  
;    FIT     - specifies whether fitting is desired before the statistic calculation  
;    REPORT  - specifies whether rpt.listing button of data is desired  
;    GROUP   - specifies the parent widget ID if plot desired  
;
; EXAMPLE:
;   Example 1  will calucultate the FWHM value for the 6th scan line and
;    2nd detector. The calculated value for center of mass, peak x, peak y,
;    half peak y value, half peak x values, and width are all returned.
;    
;    The object v1 need to be defined only if it is not yet defined.
;
;         v1 = obj_new('scanSee',file='filename.scan')
;         v1->statistic,VX,VY,c_mass=cx,x_peak=xp,y_peak=yp, $
;                   y_hpeak=yhp,x_hwdl=xl,x_hwdr=xr,fwhm=fwhm,ROW=6,DET=2
;
;   Example 2  will calucultate the FWHM value for the know vectors VY versus
;    VX. The calculated value for center of mass, peak x, peak y,
;    half peak y value, half peak x values, and width are all returned.
;
;         v1->statistic,VX,VY,c_mass=cx,x_peak=xp,y_peak=yp, $
;                   y_hpeak=yhp,x_hwdl=xl,x_hwdr=xr,fwhm=fwhm
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Feb. 11, 2000.  
;       xx-xx-xxxx      comment
;-

if n_params() eq 1 then begin
	VY = VX
	VX = indgen(n_elements(VY))
	end

if n_params() eq 0 then begin
	self->read,pa1d=pa,da1d=da,pa2d=pa2d,da2d=da2d
	det=0
	no=1
	if keyword_set(row) then no = row

	if keyword_set(detector) then det=detector-1	
	VX = pa(*,0)
	VY = da(*,det)
	if self.dim eq 2 then begin
	vx = pa2d(*,0,0)
	im = da2d(*,*,det)
	vy = im(*,0)
	if no gt 1 then vy = im(*,no-1)
	if keyword_set(column) then vy = im(column-1,*)
	end
end
	if max(vy) eq min(vy) then begin
		st = ['Not available for Detector '+strtrim(detector,2), $
			'because of constant data']
		r=dialog_message(st,/info)
		return
	end

	if keyword_set(report) then $
	statistic_1d,VX,VY,c_mass,x_peak,y_peak,y_hpeak,fwhm,xl,xr,/plot,list=list,report=report,TITLE=title,GROUP=group else $
	statistic_1d,VX,VY,c_mass,x_peak,y_peak,y_hpeak,fwhm,xl,xr,/plot,list=list,TITLE=title,GROUP=group

	if n_elements(xl) then x_hwdl = xl
	if n_elements(xr) then x_hwdr = xr
END

PRO scanSee::Read,errcode,view=view,dim=dim,num_pts=num_pts,cpt=cpt,pv=pv, $
labels=labels,id_def=id_def,pa1d=pa1d,da1d=da1d,pa2d=pa2d,da2d=da2d, $
pa3d=pa3d,da3d=da3d,scanno=scanno,IP=IP,JP=JP,X=x,Y=y,Z=z,im=im, $
class=class,outpath=outpath
;+
; NAME:
;       scanSee::Read
;
; PURPOSE:
;       This method allows the user to get the scan data as IDL variables 
;       from a defined scanSee object. 
;
; CALLING SEQUENCE:
;       Obj->[scanSee::]Read [,Errcode] [,Dim=dim] [, Num_pts=num_pts] 
;                    [,Cpt=cpt] [,Pv=pv] [,Labels=Labels] [,Id_def=id_def] 
;                    [,Pa1d=pa1d] [,Da1d=da1d] [,Pa2d=pa2d] [,Da2d=da2d] 
;		     [,Pa3d=pa3d] [,Da3d=da3d]
;                    [,View=detno] [,X=x], [,Y=y] [,Im=im]
;                    [,Class=class] [,Outpath=outpath]
;
; ARGUMENTS:
;     ERRCODE - Specifies the return code of this method.
;               It is an optional input, it returns 0 for success, 
;		-1 for failure.
;
; KEYWORDS:
;     SCANNO  - returns the scan # 
;     DIM     - returns the type of scan, 1 for 1D, 2 for 2D
;     NUM_PTS - returns the requested positioner points in 1D/2D scan
;     CPT     - returns the actual positioner points in 1D/2D scan
;     PV      - returns the scan PV names
;     LABELS  - LABELS[57] or LABELS[57,2] returns the string 
;               description arrays of the 1D/2D scan for 4 positioners (X) 
;               and 15 detectors (Y)
;                LABELS[0:3,0]    - x_names 
;                LABELS[4:18,0]   - y_names
;                LABELS[19:22,0]  - x_descs 
;                LABELS[23:37,0]  - y_descs 
;                LABELS[38:41,0]  - x_engus 
;                LABELS[42:56,0]  - y_engus 
;     ID_DEF  - ID_DEF[19,DIM] or  returns the indicator for 
;               defined positoner/detector of the 1D/2D/3D scan, 
;               if value 0 not present, 1 present in scan record
;     PA1D    - PA1D[CPT[DIM-1],4] returns positional array for scan1 record
;               or scan2 record for 1D/2D scan
;     DA1D    - PA1D[CPT[DIM-1],15] returns detector array for scan1 record
;               or scan2 record for 1D/2D scan
;     PA2D    - PA2D[NUM_PTS[DIM-2],NUM_PTS[DIM-1],4] returns positional array 
;		for 2D scan 
;     DA2D    - DA2D[NUM_PTS[DIM-2],NUM_PTS[DIM-1],15] returns detector array 
;               2D scan 
;     PA3D    - PA3D[NUM_PTS[DIM-3],NUM_PTS[DIM-2],NUM_PTS[DIM-1],4] returns 
;		positional array for 3D scan 
;     DA3D    - DA3D[NUM_PTS[DIM-3],NUM_PTS[DIM-2],NUM_PTS[DIM-1],4] returns 
;		detector array for 3D scan 
;     VIEW    - specifies the image of the detector # to be plotted 
;     X       - returns the X vector of Positioner 1 of scan1 record
;     Y       - returns the Y vector of Positioner 1 of scan2 record
;     Z       - returns the Z vector of Positioner 1 of scan3 record
;     IM      - returns the image array for the selected detector of 2D scan 
;     CLASS   - returns the class file name of the file
;     OUTPATH - returns the write permissive output file path
;
; EXAMPLE:
;    Following example reads the IOC created scan file and returns the
;    IDL variables pa2d,pa1d,da2d. 
;    The object v2 need to be defined only if it is not yet defined.
;
;	  file='/home/sricat/CHA/data/rix/cha:_0010.scan'
;         v2 = obj_new('scanSee',file=file)
;         v2->read,da2d=da2d,pa2d=pa2d,pa1d=pa1d
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Jan 19, 2000.
;       03-02-2000      PA1D, DA1D returns actual number of points saved
;-

	errcode = -1
	scanno = -1
	if self.scanno lt 0 or self.dim lt 1 then return
	errcode = 0

	scanno = *(*self.gD).scanno
	dim = *(*self.gD).dim
	num_pts = *(*self.gD).num_pts
	cpt = *(*self.gD).cpt
	pv = *(*self.gD).pv
	labels = *(*self.gD).labels
	id_def = *(*self.gD).id_def

	ndim = n_elements(id_def)/dim
	nd = ndim - 4
	for i=4,ndim-1 do begin
	 if id_def(i) gt 0 then nd=i-4
	end
	self.nd = nd 

	pa1 = *(*self.gD).pa1D
	da1 = *(*self.gD).da1D
	pa3d = *(*self.gD).pa3D
	da3d = *(*self.gD).da3D
	pa2d = *(*self.gD).pa2D
	da2d = *(*self.gD).da2D
	pa1d = *(*self.gD).pa1D
	da1d = *(*self.gD).da1D

	if dim eq 1 then begin
	w = cpt(0)
	if cpt(0) eq 0 then w = num_pts(0)
	pa1D = pa1(0:w-1,*)
	da1D = da1(0:w-1,*)
	x = pa1D(*,0)   ;pa1D(0:w-1,0)
	end

	if dim eq 2 then begin
	h = cpt(DIM-1)
	if h eq 0 then h = num_pts(DIM-1)
	pa1D = pa1(0:h-1,*)   
	da1D = da1(0:h-1,*)  
	x = pa2d(*,0)   ;pa2D(*,0,0)
	y = pa1D(*,0)

	detector=1
	im = da2d(*,*,0)
	if keyword_set(view) then begin
	if view gt 1 then detector = view 
	sz = size(da2d)
	if sz(3) lt detector then detector = sz(3)
	im = da2d(*,*,detector-1)
	dname = self.detname(detector-1)
	scan2Ddata,self.gD,detector,/view,xarr=xarr,yarr=yarr,im=im,dname=dname
	end
	end

	if dim eq 3 then begin
	w = cpt(DIM-3)
	if w eq 0 then w = num_pts(DIM-3)
	x = pa3D(*,0)  ;pa3D(0:w-1,0,0,0)

	h = cpt(DIM-2)
	if h eq 0 then h = num_pts(DIM-2)
	y = pa2D(*,0)  ;pa2D(0:h-1,0,0)

	d = cpt(DIM-1)
	if d eq 0 then d = num_pts(DIM-1)
;	pa1D = pa1(0:d-1,*) 
;	da1D = da1(0:d-1,*) 
	z = pa1D(*,0)  ;pa1D(0:d-1,0)

	self.width = num_pts(0)
	self.height = num_pts(1)
	end

	class = self.name
	outpath = self.outpath

;help,pa3d,da3d,pa2d,da2d,pa1d,da1d,x,y,z
;help,labels,pv,id_def

END

PRO scanSee::view3d_panImage,slice,rank,data,tiff=tiff,reverse=reverse,pict=pict,xdr=xdr,group=group,SEL=SEL
;+
; NAME:
;       scanSee::VIEW3D_PANIMAGE
;
; PURPOSE:
;
; CATEGORY:
;    Widgets.
;
; CALLING SEQUENCE:
;      Obj->[scanSee::]VIEW3D_PANIMAGE, Slice [,Rank] [,Data] [,/TIFF] 
;                  [,/REVERSE] [,/XDR] [,/PICT] [,GROUP=group]
;
; ARGUMENTS:
;      Slice     - Specifies the slice # in the viewing direction 
;      RANK      - Specifies the viewing axis direction, 0-X axis, 1-Y axis,
;                  2-Z axis, default is Z axis
;      DATA      - returns the panimage data array
;
; KEYWORDS:
;       TIFF     - specifies the output tiff filename
;       REVERSE  - indicates the reverse tiff is desired
;       PICT     - specifies the output pict filename
;       XDR      - specifies the output XDR filename
;       GROUP    - specifies the parent widget ID
;       SEL      - provides selection dialog for panimage
;
; RESTRICTION:
;   The scanSee object must be a 3D scan object.
;
; EXAMPLE:
;   Following example examines the various 2D slice for detector number 1
;   for a given 3D scan object v2
;     
;     v2 = obj_new('scanSee',file='/home/sricat/CHA/data/rix/cha:_0049.scan')
;     v2->view3d_panimage
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, April 7, 2000.
;       11-03-2000      Remove GIF file generation
;                       Add panimage_sel support
;                       Add u_read,u_write,u_open XDR data format
;
;-

	if self.dim ne 3 then begin
		r = dialog_message('Error! The object is not a 3D scan',/error)
		return
	end

	if n_elements(rank) eq 0 then rank = 2
	no  = (*(*self.gD).num_pts)[rank]
	kindex = 0 
	if n_elements(slice) ne 0 then kindex = slice
	if kindex ge no then return

	filename = self.file
	
	xyz = *(*self.gD).da3D
	id_def = self.def

	if rank eq 2 then data = reform(xyz(*,*,kindex,*))
	if rank eq 1 then data = reform(xyz(*,kindex,*,*))
	if rank eq 0 then data = reform(xyz(kindex,*,*,*))

	scanno = '.panZ'
	if rank eq 0 then scanno = '.panX'
	if rank eq 1 then scanno = '.panY'
	scanno = scanno + '_'+strtrim(kindex,2)
	title = '3D Scan #'+strtrim(self.scanno,2)+scanno

	class = self.name

	if keyword_set(TIFF) then $
	tiff = self.outpath+'TIFF'+!os.file_sep+class+ $
		scanno+'.tiff'
	if keyword_set(REVERSE) then $
	tiff = self.outpath+'TIFF'+!os.file_sep+class+ $
		scanno+'.rtiff'
	if keyword_set(XDR) then $
	xdr = self.outpath+'XDR'+!os.file_sep+class+ $
		scanno+'.xdr'
	if keyword_set(PICT) then $
	pict = self.outpath+'PICT'+!os.file_sep+class+ $
		scanno+'.pict'

	nw = self.win
	if keyword_set(SEL) then $
	panImage_sel,data,id_def,new_win=nw,title=title else $
	panImage,data,id_def,new_win=nw,tiff=tiff,reverse=reverse,pict=pict,xdr=xdr,title=title
	self.win = nw

END


PRO scanSee::view3d_2d,det,group=group,title=title,slicer3=slicer3
;+
; NAME:
;       scanSee::VIEW3D_2D
;
; PURPOSE:
;       This method allows the user flexiblely examine any 2D slice out 
;       from a defined scanSee 3D scan object. It allows the user view the
;	2D slice as various 1D/2D plots or ASCII output.
;
; CATEGORY:
;    Widgets.
;
; CALLING SEQUENCE:
;      Obj->[scanSee::]VIEW3D_2D, Det [,GROUP=group] [,TITLE=title] 
;		[,SLICER3=slicer3]
;
; ARGUMENTS:
;     Det      - Specifies detecotr #, extracts the 3D array asscocited with 
;                the the detector # 
;
; KEYWORDS:
;   GROUP      - Specifies the parent widget ID
;   TITLE      - Overrides default VIEW3D_2D window title by this specification
;   SLICER3    - Specifies whether view the 3D array by the SLICER3 program
;
; RESTRICTION:
;   The scanSee object must be a 3D scan object.
;
; EXAMPLE:
;   Following example examines the various 2D slice for detector number 1
;   for a given 3D scan object v2
;     
;     v2 = obj_new('scanSee',file='/home/sricat/CHA/data/rix/cha:_0049.scan')
;     v2->view3d_2d,1
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, April 4, 2000.
;       12-01-2000      Add scan # to title
;
;-

	if self.dim ne 3 then begin
		r = dialog_message('Error! The object is not a 3D scan',/error)
		return
	end

	detector = 1
	if n_elements(det) then detector = det 
	if detector le 0 or detector gt 85 then detector = 1
	da3d = *(*self.gD).da3D
	cpt = *(*self.gD).cpt
	npts = *(*self.gD).num_pts

	sz = size(da3d)
	if sz(0) eq 4 then begin
	if cpt(2) lt npts(2) then $
	data = da3d(*,*,0:cpt[2],detector-1) else $
	data = da3d(*,*,*,detector-1)
	endif else data = da3d

	if keyword_set(title) eq 0 then title='3D Scan# '+strtrim(self.scanno,2)+', Detector '+self.detname(detector-1)       ;strtrim(detector,2)
	view3d_2d,data,group=group,title=title,slicer3=slicer3
END

PRO scanSee::Images,image_array,def,vmax,vmin,X=x,Y=y,panimage=panimage
;+
; NAME:
;       scanSee::Images
;
; PURPOSE:
;       This method allows the user to get the complete scan image array
;       from a defined scanSee object. 
;
; CALLING SEQUENCE:
;       Obj->[scanSee::]Images,Image_array [,Def] [,Vmax] [,Vmin] 
;                    [,X=x] [,Y=y]  [,/PANIMAGE]
;
; ARGUMENTS:
;     IMAGE_ARRAY  - returns the complete set of image array for a 2D scan
;                    IMAGE_ARRAY[NUM_PTS[0],NUM_PTS[1],15]
;     DEF[15]      - returns the vector of indicators for detectors 
;                    0 - not defined, 1 - defined
;     VMAX[15]     - returns the maximum of image_array(*,*,i)
;     VMAX[15]     - returns the minimu of image_array(*,*,i)
;
; KEYWORDS:
;     PANIMAGE     - if specified the panimage window will pop up
;     X            - returns the X position vector
;     Y            - returns the Y position vector
;
; EXAMPLE:
;    Following example extracts the image_array from the input file and also
;    plot the 2D panimage.
;    The object v2 need to be defined only if it is not yet defined.
;
;         v2 = obj_new('scanSee',file=file)
;         v2->images,image_array,/panimage
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Jan 19, 2000.
;       xx-xx-xxxx      comment
;-

	if self.dim eq 1 then return
	image_array = *(*self.gD).da2D

	pa1D = *(*self.gD).pa1D
	pa2D = *(*self.gD).pa2D
	x = pa2D(*,0,0)
	y = pa1D(*,0)

	def = self.def
	nd = self.nd

	vmin = make_array(nd+1,/float)
	vmax = make_array(nd+1,/float)

	for i=0,nd do begin
	if def(i) gt 0 then begin
	vmax(i) = max(image_array(*,*,i))
	vmin(i) = min(image_array(*,*,i))
	end
	end

	if keyword_set(panimage) then self->panImage 
END

PRO scanSee::panImage,SEL=SEL,TIFF=TIFF,XDR=XDR,PICT=PICT,REVERSE=REVERSE
;+
; NAME:
;       scanSee::PanImage
;
; PURPOSE:
;       This method allows the user to view detector images in a pop up window.
;
; CALLING SEQUENCE:
;       Obj->[scanSee::]PanImage] [,SEL=SEL] [,TIFF=TIFF] [,REVERSE=REVERSE]
;                  [,PICT=PICT] [,XDR=XDR]
;
; ARGUMENTS:
;     None.
;
; KEYWORDS:
;       TIFF     - specifies the output tiff filename
;       REVERSE  - indicates the reverse tiff is desired 
;       PICT     - specifies the output pict filename
;       XDR      - specifies the output xdr filename
;       GROUP    - specifies the parent widget ID 
;       SEL      - specifies the selection dialog
;
; EXAMPLE:
;    Following example pops up the panimage window for the input scanSee file
;    and also save as a TIFF file in reverse order. 
;    The object v2 need to be defined only if it is not yet defined.
;
;         v2 = obj_new('scanSee',file=file)
;         v2->panImage,tiff='file.tiff',/reverse
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Jan 19, 2000.
;       xx-xx-xxxx      comment
;       11-03-2000      Add the panImage selection option
;-

; pops up pan images

	image_array = *(*self.gD).da2D
	sz = size(image_array)

	seq = self.scanno
	def = self.def(0:sz(3)-1)
	title=self.name+' SCAN # '+strtrim(seq,2)

catch,error_status
if error_status ne 0 then begin
	if !error_state.name eq 'IDL_M_CNTOPNFIL' then begin
	r = dialog_message([!error_state.msg,!error_state.sys_msg,$
		string(!error_state.code)],/error)
	return	
	end
	self.win = -1
end
if self.win ne -1 then wdelete,self.win
self.win = -1

	if keyword_set(SEL) then begin
	nw = self.win 
	panImage_sel,image_array,def,new_win=nw,title=title 
	self.win = !d.window
	return
	end

update:

	width = 60
	height = 60
	old_win = !D.window

	if n_elements(factor) then begin
		if factor lt .1 then factor = 1
		width = width * factor
		height = height * factor
	end

	ND = sz(3) ; 85
	NC = 8
	NR = ND/8 + 1
	NL = NR*NC - 1

	if self.win lt 0 then begin
		window,/free, xsize = NC*width, ysize=NR*height, $
			title=title,retain=2
		for i=0,ND-1 do begin
		ii = NL-i
		xi=(i mod NC)*width+width/2 - 5 
		yi=height/2+ii/NC*height
		xyouts, xi,yi,self.detname(i),/device
		end
	end

new_win = !D.window
self.win = new_win

	wset,new_win
	for sel=0,ND-1 do begin
	if def(sel) gt 0 then begin
	v_max = max(image_array(*,*,sel),min=v_min)
	if v_max eq v_min then begin
		temp = !d.table_size * image_array(*,*,sel) 
		TV,congrid(temp,width,height),sel
	endif else begin
		temp = congrid(image_array(*,*,sel), width, height)
		TVSCL, temp, sel
	end
	end
	end


	for i=1,NR-1 do plots,[0,NC*width],[i*height,i*height],/device
	for i=1,NC-1 do plots,[i*width,i*width],[0,NR*height],/device

	if keyword_set(TIFF) then begin
		tvlct,r,g,b,/get
		tiffname = strtrim(tiff,2)
	 	if keyword_set(reverse) then $
		write_tiff,tiffname,reverse(TVRD(),2),1,red=r,green=g,blue=b $
		else write_tiff,tiffname,TVRD(),red=r,green=g,blue=b
	end

	if keyword_set(XDR) then begin
		xdrname = strtrim(xdr,2)
		u_openw,unit,xdrname,/XDR,error=error
		u_write,unit,image_array
		u_close,unit
	end

	if keyword_set(PICT) then begin
		tvlct,r,g,b,/get
		gifname = strtrim(pict,2)
		write_pict,gifname,TVRD(),r,g,b
	end

	wset,old_win
	if old_win eq 0 then wdelete,0
END

PRO scanSee::Next,seqno,filename,error=error
;+
; NAME:
;       scanSee::Next
;
; PURPOSE:
;       This method points to the next scan # file saved by IOC. 
;
; CALLING SEQUENCE:
;       Obj->[scanSee::]Next [,Seqno]
;
; ARGUMENTS:
;     SEQNO     - jumps to the scan # seqno file if it is specified.
;     FILENAME  - returns the new filename
;
; KEYWORD:
;     ERROR     - returns the error code, non-zero if not found
;
; EXAMPLE:
;    Following examples replace the current object by next scan number.
;    If current file is 'cha:_0010.scan' then the object will be replaced
;    by 'cha:_0011.scan' in Example 1. 
;    Example 2 jumps to the 'cha:_0100.scan' file.
;
;         v2->Next
;         v2->Next,100
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Jan 19, 2000.
;       xx-xx-xxxx      comment
;-

	error=0
	scanno = self.scanno + 1
	if n_elements(seqno) gt 0 then scanno = seqno

	if scanno lt 10000 then begin
	str = '0000'
	st = strtrim(scanno,2)
	len = strlen(st)
	strput,str,st,4-len
	endif else str = strtrim(scanno,2)
	filename = self.prefix+str+self.suffix
	print,filename

	; if next seqno found then replace the current

	found = findfile(filename,count=ct)
	if ct gt 0 then begin
	self->delete
	self = obj_new('scanSee',file=filename)
	endif else begin
	  res = dialog_message(filename+' not found!',/Error)
	  error=-1
	end
	if n_elements(seqno) then seqno = seqno + 1
END

PRO scanSee::Prev,seqno,filename,error=error
;+
; NAME:
;       scanSee::Prev
;
; PURPOSE:
;       This method points to the previous scan # file saved by IOC. 
;
; CALLING SEQUENCE:
;       Obj->[scanSee::]Prev [,Seqno]
;
; ARGUMENTS:
;     SEQNO     - jumps to the scan # seqno if it is specified.
;     FILENAME  - returns the new filename
;
; KEYWORD:
;     ERROR     - returns the error code, non-zero if not found
;
; EXAMPLE:
;    Following example replace the current object by previous scan number.
;    If current file is 'cha:_0010.scan' then the object will switch to
;    'cha:_0009.scan'
;    Example 2 jumps to the 'cha:_0100.scan' file.
;
;         v2->Prev
;         v2->Prev,100
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Jan 19, 2000.
;       xx-xx-xxxx      comment
;-

	error=0
	scanno = self.scanno - 1
	if n_elements(seqno) gt 0 then scanno = seqno

	if scanno lt 10000 then begin
	str = '0000'
	st = strtrim(scanno,2)
	len = strlen(st)
	strput,str,st,4-len
	endif else str = strtrim(scanno,2)
	filename = self.prefix+str+self.suffix
	print,filename

	; if seqno found then replace the current

	found = findfile(filename,count=ct)
	if ct gt 0 then begin
	self->delete
	self = obj_new('scanSee',file=filename)
	endif else begin
	  res = dialog_message(filename+' not found!',/Error)
	  error = -1
	end
	if n_elements(seqno) then seqno = seqno - 1
END

PRO scanSee::First,seqno,filename
;+
; NAME:
;       scanSee::First
;
; PURPOSE:
;       This method opens the first scan # file created by IOC. 
;
; CALLING SEQUENCE:
;       Obj->[scanSee::]First
;
; ARGUMENTS:
;      None.
;
; EXAMPLE:
;    Following example replace the current object by first scan number.
;
;         v2->First
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Jan 19, 2000.
;       03-15-01    bkc Accommondate for W95 use wild search for *.scan
;-

	seqno = 1
	str = '0001'
 	filename = self.prefix+str + self.suffix
	found = findfile(filename,count=ct)
	if ct gt 0 then begin
	self->delete
	self = obj_new('scanSee',file=filename)
	return
	end

	list = findfile(self.path+'*.scan',count=ct)
	if ct gt 0 then begin

	ip = rstrpos(self.prefix,!os.file_sep)+1
	prefix = strmid(self.prefix,ip,strlen(self.prefix)-ip)

	; find the first scan # then goto step1

	is = strlen(prefix)	
	nf = strpos(self.prefix,prefix)
	l2 = rstrpos(list(0),self.suffix)
	str = strmid(list,nf+is,l2-nf-is)
	scanno = fix(str)
	ind = sort(scanno)
	seqno = scanno(ind(0))

step1:
 	filename = list(ind(0)) ; self.prefix+str + self.suffix
print,filename

	; if seqno found then replace the current

	self->delete
	self = obj_new('scanSee',file=filename)
	end
END

PRO scanSee::Last,seqno,filename
;+
; NAME:
;       scanSee::Last
;
; PURPOSE:
;       This method replace the object by the last scan # file created by IOC. 
;
; CALLING SEQUENCE:
;       Obj->[scanSee::]Last [,Seqno] [,Filename] 
;
; ARGUMENTS:
;      SEQNO      -  returns the last scan seqno found
;      Filename   - returns the filename of the last scan
;
; EXAMPLE:
;    Following example replace the current object by the most recent scan 
;    number.
;
;         v2->Last
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Jan 19, 2000.
;       03-15-01    bkc Accommondate for W95 use wild search for *.scan
;-
	found = findfile(self.path+'*.scan',count=ct)
	len = strlen(self.prefix)
	sp = rstrpos(self.prefix,!os.file_sep)
	if sp gt -1 then sp=sp+1
	prefix = strmid(self.prefix,sp,len-sp)

	len1 = strlen(prefix)
	rp = strpos(found(0),prefix)
	rp1 = rstrpos(found(0),self.suffix)
	num = 0
	for i=0,n_elements(found)-1 do begin
	if rp ge 0 and rp1 gt len1 then begin
		ar = strmid(found(i),rp+len1,rp1-rp-len1)
		if fix(ar) gt num then begin
			num= fix(ar)
			ip = i
			end
		end
	end

	filename = found(ip)
print,filename

	seqno = num
	if n_params() lt 2 then return

	self->delete
	self = obj_new('scanSee',file=filename)
END

PRO scanSee::Print,debug=debug
;+
; NAME:
;       scanSee::Print
;
; PURPOSE:
;       This method print the object info.
;
; CALLING SEQUENCE:
;       Obj->[scanSee::]Print [,/DEBUG]
;
; ARGUMENTS:
;      None.
;
; KEYWORD:
;     DEBUG   - if specified more info will be printed.
;
; EXAMPLE:
;    Following example print the object info.
;
;         v2->Print
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Jan 19, 2000.
;       xx-xx-xxxx      comment
;-
	print,'ScanSee File:  ',self.file
	print,'File Path:     ',self.path
	print,'File Name:     ',self.name
	print,'Home Dir:      ',self.home
	print,'Out Path:      ',self.outpath
	print,'Scan No:       ',self.scanno
	print,'Scan Dimension:',self.dim
	print,'id_def:        ',self.def
	print,'Width:         ',self.width
	print,'Height:        ',self.height
	if keyword_set(debug) then scanimage_print,self.gD
END

PRO scanSee::View2D,detno,xarr=xarr,yarr=yarr,im=im,plot=plot,group=group,_extra=e
;+
; NAME:
;       scanSee::View2D
;
; PURPOSE:
;       This method views the 2D image data corresponding to the specified
;       detector number.
;
; CALLING SEQUENCE:
;       Obj->[scanSee::]View2D, DetNo [,Xarr=xarr] [,Yarr=yarr] [,Im=im] [,/PLOT]
;
; ARGUMENTS:
;     DETNO  - specifies the detector number desired, default to 1.
;
; KEYWORDS:
;     XARR   - returns the X vector
;     YARR   - returns the Y vector
;     IM     - returns the image array 
;     PLOT   - calls PLOT2D package
;
; EXAMPLE:
;    Following example let the user view the 10th detector of the 
;    scanSee object v2. 
;
;         v2->View2d,10
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Jan 19, 2000.
;       xx-xx-xxxx      comment
;-
	detector = 1
	if keyword_set(detno) then detector=detno 
	dname = self.detname(detector-1)
	if keyword_set(plot) then $
	scan2Ddata,self.gD,detector,/plot,dname=dname,xarr=xarr,yarr=yarr,im=im,group=group,_extra=e else $
	scan2Ddata,self.gD,detector,/view,dname=dname,xarr=xarr,yarr=yarr,im=im,group=group,_extra=e
END

PRO scanSee::ASCII2D,detno,nowin=nowin,view=view,outfile=outfile,plot=plot,format=format,group=group
;+
; NAME:
;       scanSee::ASCII2D
;
; PURPOSE:
;       This method generates the 2D image ASCII file.
;
; CALLING SEQUENCE:
;       Obj->[scanSee::]ASCII2D [,Detno] [,/NOWIN] [,/VIEW | /PLOT] 
;                  [,Outfile=outfile] [,Format=format]
;
; ARGUMENTS:
;     DETNO   - specifies the desired detector number, defaults to 1
;
; KEYWORDS:
;     NOWIN   - suppress the popup display window
;     VIEW    - if specified view 2D image is allowed 
;     PLOT    - if specified plot2d is called
;     OUTFILE - specifies the outfile name to override the default filename
;     FORMAT  - specifies the outfile format, default 'G18.8'
;
; EXAMPLE:
;    Creates and pops up xdisplay window for 2D image ASCII file for 
;    detector 1, and detecor 10, in example 2 also show the image.
;
;         v2->ASCII2D
;         v2->ASCII2D,10,/view
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Jan 19, 2000.
;       xx-xx-xxxx      comment
;-
	detector = 1
	if keyword_set(detno) then detector=detno 
	dname = self.detname(detector-1)

	if keyword_set(view) then $
	scan2Ddata,self.gD,detector,/view,dname=dname,xarr=px,yarr=py,im=data 
	if keyword_set(plot) then $
	scan2Ddata,self.gD,detector,/plot,dname=dname,xarr=px,yarr=py,im=data ,group=group
	if keyword_set(view) or keyword_set(plot) eq 0 then $
	scan2Ddata,self.gD,detector,xarr=px,yarr=py,im=data,group=group


	t_format = 'G18.8'
	if keyword_set(format) then t_format = format
	fwidth = 'I'+strmid(t_format,1,strpos(t_format,'.')-1)

	dir = self.outpath+'ASCII'+!os.file_sep
	found = findfile(dir,count=ct)
	if ct lt 1 then spawn,!os.mkdir + ' ' +dir

	suf0 = '00'
	suf = strtrim(detector,2)
	ln = strlen(suf)
	strput,suf0,suf,2-ln
	file = self.name +'.im'+suf0
	report = file

	if keyword_set(outfile) then report = strtrim(outfile,2)

	openw,fw,dir+report,/get_lun

	s = size(data)
	dim = s(1:2)
	printf,fw,'; From:',self.file ,',   Detector = ',strtrim(detector,2)
	printf,fw,';   data('+strtrim(dim(0),2)+','+strtrim(dim(1),2)+')'
	printf,fw,'; -------------------------------'

	f0 = '(";              (yvalues)",'+ '5000('+t_format+',:))'
	if n_elements(py) gt 0 then printf,fw,format=f0,py
	if n_elements(py) gt 0 then begin
		f1 = '('+t_format+',I,'+strtrim(dim(1),2)+'('+t_format+'))' 
		f0 = '(";                   \ Y",'+strtrim(dim(1),2)+fwidth+',/,";                  X \",/,";      (xvalues)")'
		endif else begin
		f0 = '(";    \ Y",'+strtrim(dim(1),2)+fwidth+',/,";   X \",/)'
		f1 = '(I,'+strtrim(dim(1),2)+'('+t_format+'))' 
		end
	printf,fw,format=f0,indgen(dim(1))
	newdata = transpose(data)
	d1 = dim(1)
	d2 = dim(0)
	temp = make_array(dim(1))
	for j=0,d2-1 do begin
	temp = newdata(0:d1-1,j)
	if n_elements(px) gt 0 then printf,fw,format=f1,px(j),j,temp else $
		printf,fw,format=f1,j,temp
	end
	free_lun,fw

	if keyword_set(nowin) then return
	xdisplayfile,dir+report,group=group
	
END

PRO scanSee::Plot1D,no,xarr=xarr,yarr=yarr,xsel=xsel,ysel=ysel,title=title,group=group,_extra=e
;+
; NAME:
;       scanSee::Plot1D
;
; PURPOSE:
;       This method plots the detector(s) data for a selected 1D scan
;       number for 1D/2D scan.
;
; CALLING SEQUENCE:
;       Obj->[scanSee::]Plot1D[,No] [,Xarr=xarr] [,Yarr=yarr] 
;               [,Xsel=xsel] [,Ysel=ysel]
;
; ARGUMENTS:
;     NO  -   specifies the 1D scan # to be plotted.
;
; KEYWORD:
;     XARR  - specifies the X axis values
;     YARR  - specifies the Y axis values
;     XSEL  - specifies the positioner index  0 to 3
;     YSEL  - a string to specify a list of detectors to be plotted
;             0 to 14, it defaults to all defined detectors
;     TITLE - specifies the plot title
;
; EXAMPLE:
;    Example 1 plot the 5th 1D scan from a 2D scan for all the detectors.
;    Example 2 plot the 5th 1D scan from a 2D scan only for detectors 3 and 5.
;
;         v2->Plot1d,5
;         v2->Plot1d,5,ysel='2,4'
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Jan 19, 2000.
;       11-27-2000      Trap for zero detector case
;-
; ysel - string list
	scanno = 1
	if keyword_set(no) then scanno = no
	self.xaxis = 0
	if total(self.def) lt 1. then begin
		r = dialog_message('No detector found for scan file: '+self.file,/error)
	return
	end
	if keyword_set(xsel) then self.xaxis = xsel
	if keyword_set(ysel) then $
	scan1Ddata,self.gD,scanno,/plot, xarr=xarr,yarr=yarr, title=title,$
		xsel=xsel,ysel=ysel,_extra=e  else $
        self->ascii1d,scanno,/plot,/nowin,title=title,group=group,_extra=e
END

PRO scanSee::ASCII1DAll,startno,endno,nowin=nowin,format=format,group=group
; default create all, startno < endno
	i1 = 1
	i2 = self.height
	if n_elements(startno) then i1 = startno
	if n_elements(endno) then i2 = endno 
	for i=i1,i2 do begin
	if i lt self.height then $
	self->ascii1D,i,format=format,nowin=nowin,GROUP=group
	end
END


PRO scanSee::ASCII1D,no,nowin=nowin,outfile=outfile,plot=plot,format=format,$
title=title,group=group,all=all,_extra=e
;+
; NAME:
;       scanSee::ASCII1D
;
; PURPOSE:
;       This method generates the ASCII 1D output for a 1D/2D scan.
;
; CALLING SEQUENCE:
;       Obj->[scanSee::]ASCII1D [,No] [,/NOWIN] [,/PLOT] [,Outfile=filename]
;             [,Format=format]
;
; ARGUMENTS:
;     NO  - specifies the 1D scan number for a 2D scan object, otherwise 
;           is not required.
;
; KEYWORD:
;    PLOT     - calls the plot1d program only if it is set, no ascii will be
;               generated
;    NOWIN    - saves the output file only without display the ascii report
;    OUTFILE  - overrides the default outfile name if it is specified 
;    FORMAT   - specifies the format used in ascii report, default 'G18.8'
;    ALL      - automatically generates all 1D scan ascii files for a 2D scan
;
; EXAMPLE:
;    Example 1 generates the tabulated ASCII data for the first line
;    scan of the 2D scan object v2.
;    Example 2 generates the tabulated ASCII data for the 5th line
;    scan of the 2D scan object v2.
;
;         v2->ascii1d
;         v2->ascii1d,5
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Jan 19, 2000.
;       xx-xx-xxxx      comment
;-
; no gt 0 lt num_pts
; outfile   - specify the ascii name

	dir = self.outpath+'ASCII'+!os.file_sep
	found = findfile(dir,count=ct)
	if ct lt 1 then begin
		spawn,!os.mkdir + ' ' +dir
		openw,1,dir+'1'
		close,1
	end

	dim = self.dim
	cpt = *(*self.gD).cpt
	num_pts = *(*self.gD).num_pts
	seqno = 1

	self_width = cpt(0)
	if cpt(0) eq 0 then self_width = num_pts(0)

	if dim eq 1 then begin
	def = *(*self.gD).id_def
	ndim = n_elements(def)
	pa1d = *(*self.gD).pa1d
	da1d = *(*self.gD).da1d

	pa = pa1d(0:self_width-1,*)
	da = da1d(0:self_width-1,*)
	labels = *(*self.gD).labels
	end
	if dim eq 2 then begin
	if n_elements(no) gt 0 then begin
		if no gt 0 and no le cpt(1) then seqno = no else begin
		res = dialog_message('Invalid seq # specified, reset to 1 ',/info)
		end
	end
	id_def = *(*self.gD).id_def
	ndim = n_elements(id_def)/self.dim
	labels = *(*self.gD).labels(*,0)
	pa2d = *(*self.gD).pa2D
	da2d = *(*self.gD).da2d
	sz = size(da2d)
	nd = sz(3)
	ndim = nd + 4
	def = id_def(0:ndim-1)

	pa = make_array(self.width,4,/double)
	da = make_array(self.width,nd)
;	pa[*,*] = pa2d(*,seqno-1,*)   only last vector returned in R2 
	pa = pa2d
	da[*,*] = da2d(*,seqno-1,*)
	end

	pts = self.width ; *(*self.gD).cpt(0)
	np = fix(total(def(0:3)))
	for i=4,ndim-1 do begin
	 if def(i) gt 0 then begin
	   nd = i-4
	   if n_elements(d_list) eq 0 then d_list = nd else d_list=[d_list,nd]
	 end
	end

	x_names = make_array(4,/string,value=string(replicate(32b,30)))
	x_engus = x_names
	x_descs = x_names
	y_names = make_array(n_elements(d_list),/string,value=string(replicate(32b,30)))
	y_engus = y_names
	y_descs = y_names
	num=0
	for i=0,3 do begin
		if def(i) gt 0 then begin
		x_names(num) = labels(i)
		x_descs(num) = labels(i+ndim)
		x_engus(num) = labels(i+ndim*2)
		num = num +1
		end
	end
	for i=0,n_elements(d_list)-1 do begin
		if def(i+4) gt 0 then begin
		y_names(i) = labels(i+4)
		y_descs(i) = labels(i+4+ndim)
		y_engus(i) = labels(i+4+ndim*2)
		end
	end

	seqstr = 'Scan # '+strtrim(self.scanno,2) + '   Req_x_npt='+strtrim(num_pts(0),2)
	if dim eq 2 then seqstr = seqstr + ' (Seq # '+strtrim(seqno,2)+')'

	; plot is requested

	if keyword_set(plot) then begin
	comment = ' Scan # '+string(seqno)
	comment = seqstr
	comment = [comment, 'File : '+ self.file]
	ytitle = y_names(0) 
	if self.xaxis eq 4 then begin
		x = indgen(cpt(0)) 
		xtitle = 'Step #'
	endif else begin
		x = pa(*,self.xaxis)
		if self.xaxis lt np then xtitle = x_names(self.xaxis) else $
		xtitle = 'P'+strtrim(self.xaxis+1,2)
	end
	y = da(*,0:nd)
	plot1d,x,y,title=title,comment=comment, $
		xtitle=xtitle,ytitle=ytitle,group=group,_extra=e
	return
	end

	;  get default ascii report filename

	temp_format = 'G18.8'
	if keyword_set(format) then temp_format=format
	fwidth = fix(strmid(temp_format,1,strpos(temp_format,'.')-1))
	temp_format= '('+temp_format+')'

	ind_width = 5
	suf0 = '0000'
	suf = strtrim(seqno,2)
	ln = strlen(suf)
	strput,suf0,suf,4-ln
	file = self.name +'.'+suf0
	report = file

	if keyword_set(outfile) then report = strtrim(outfile,2)

	st0=string(replicate(32b,ind_width+fwidth*fix(total(def))))

	openw,3,dir+report

	printf,3,'; Source File: '+self.file
	printf,3,'; ASCII  File: '+dir+report
	printf,3,'; ',strtrim(seqstr,1)

st1='; Defined PI: '
for i=0,3 do st1 = st1 + strtrim(def(i),2) + '  '
st1 = st1 + '  DI: '
for i=4,nd+4 do begin
	if (i-4) mod 8 eq 7 then sep = ',  ' else sep='  '
	st1 = st1 + strtrim(def(i),2) + sep
end
printf,3,st1

	st1 = '; Index #   '
	for i=0,np-1 do st1 = st1 + strtrim(x_names(i),2) + "  "
	for i=0,n_elements(d_list)-1 do st1 = st1 + strtrim(y_names(i),2) + "  "
	printf,3,st1

	for k=0,pts(0)-1 do begin
	ip = 0
	st = st0
	st1 = strtrim(k,2)
	len1 = strlen(st1)
	strput,st,st1,ip+ind_width-len1-1
	ip = ip + ind_width
	for i=0,np-1 do begin
		st1 = string(pa(k,i),format=temp_format)
;		st1 = strtrim(pa(k,i),2)
		len1 = strlen(st1)
		strput,st,st1,ip+fwidth-len1-1
		ip = ip + fwidth
		end
	for ii=0,n_elements(d_list)-1 do begin
		i = d_list(ii)
		st1 = string(da(k,i),format=temp_format)
;		st1 = strtrim(da(k,i),2)
		len1 = strlen(st1)
		strput,st,st1,ip+fwidth-len1-1
		ip = ip + fwidth
		end
	printf,3,st
	end
	close,3

	if n_elements(nowin) eq 0 then $
	xdisplayfile,dir+report,group=group,_extra=e

END

PRO scanSee::Pick1d,detno,GROUP=group
	
	title='SCAN # '+ strtrim(scanno,2)+', D'+strtrim(pick1d,2)
	im = image_array(*,*,detno-1)
	calibra_pick1d,im,xa=x,ya=y,title=title,GROUP=group
END

PRO scanSee::Calibration,pick1d=pick1d,GROUP=group,_extra=e
;+
; NAME:
;       scanSee::Calibration
;
; PURPOSE:
;       This method calls the calibration program for a 2D scan object.
;
; CALLING SEQUENCE:
;       Obj->[scanSee::]Calibration
;
; ARGUMENTS:
; KEYWORD:
;     GROUP  - specifies the parent group widget, the destroy of parent
;              group results the destroy of calibration program.
;     PICK1D - specifies the detector number and calls calibra_pick1d 
;              directly without calling calibration program
;
; EXAMPLE:
;    Following example brings up the calibration program.
;
;         v2->calibration
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Jan 19, 2000.
;       xx-xx-xxxx      comment
;-
	
	scanno = self.scanno

	if self.dim eq 1 then begin
	pa = *(*self.gD).pa1D
	da = *(*self.gD).da1D
	labels = *(*self.gD).labels

	x = pa(*,0)
	y = da(*,0)
	nd = ceil(total(self.def))
	sz = size(da)
	im = make_array(sz(1),nd)
	id = 0
	for i=0,sz(2)-1 do begin
		if self.def(i) gt 0 then begin
			im(*,id) = da(*,i)
			id = id+1
		end
	end

	def = self.def
        calibration_factor,im,def,xv=x,yv=y, $
                classname=self.name,inpath=self.path, $ 
                title=':  SCAN # '+strtrim(scanno,2),GROUP=group,_extra=e

	end

	if self.dim eq 2 then begin
	pa1D = *(*self.gD).pa1D
	pa2D = *(*self.gD).pa2D
	x = pa2D(*,0,0)
	y = pa1D(*,0)

	if keyword_set(pick1d) then begin
		self->images,image_array,def
		if pick1d lt 1 or def(pick1d-1) eq 0 then return
		title='SCAN # '+ strtrim(scanno,2)+', D'+strtrim(pick1d,2)
		im = image_array(*,*,pick1d-1)
		calibra_pick1d,im,xa=x,ya=y,title=title,GROUP=group
		return
	end

	self->images,image_array,def,/PANIMAGE
        calibration_factor,image_array,def,xv=x,yv=y, $
                classname=self.name,inpath=self.path, $ 
                title=':  SCAN # '+strtrim(scanno,2),GROUP=group,_extra=e

	end

END


PRO scanSee::ROI,no,debug=debug,header=header,commint=comment,_extra=e
;+
; NAME:
;       scanSee::ROI
;
; PURPOSE:
;       This method calls the 2D image region of interest(ROI) program. 
;
; CALLING SEQUENCE:
;       Obj->[scanSee::]ROI [,No]
;
; ARGUMENTS:
;      NO   - specifies the image number from the 2D object, defaults to 1
;
; EXAMPLE:
;    Following example calls the ROI program with 5th detector's image array
;
;         v2->ROI,5
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Jan 19, 2000.
;       xx-xx-xxxx      comment
;-
	if n_elements(no) eq 0 then no=1
	if no lt 1 or no gt 15 then return

p = self.outpath+'ROI'
found = findfile(p,count=ct)
if ct eq 0 then spawn,!os.mkdir + ' '+ p

	pa1D = *(*self.gD).pa1D
	pa2D = *(*self.gD).pa2D
	x = pa2D(*,0,0)
	y = pa1D(*,0)

	da2d = *(*self.gD).da2D
	im = da2d(*,*,no-1)


	if keyword_set(header) then h_annote = header else $
	begin
	h_annote=[ self.file,'Image Seq # '+strtrim(no,2) + $
		',  2D Scan # '+strtrim(self.scanno,2)+',  Detector # '+ $
			strtrim(no,2)]
	end

	rptfile = p+!os.file_sep+self.name+'_roi.rpt'
	roifile = p+!os.file_sep+self.name+'_roi.xdr'
	if keyword_set(comment) then begin
	if keyword_set(debug) then scan2d_roi,im,x,y, $
		rptfile=rptfile,roifile=roifile,/debug, $
		header=h_annote,comment=comment,_extra=e else $
	scan2d_roi,im,x,y,rptfile=rptfile,roifile=roifile, $
		header=h_annote, comment=comment,_extra=e
	endif else begin
	if keyword_set(debug) then scan2d_roi,im,x,y, $
		rptfile=rptfile,roifile=roifile,/debug, $
		header=h_annote,_extra=e else $
	scan2d_roi,im,x,y,rptfile=rptfile,roifile=roifile, $
		header=h_annote,_extra=e
	end
END

PRO scanSee::Delete
;+
; NAME:
;       scanSee::Delete
;
; PURPOSE:
;       This method cleans up the scanSee object and frees up the allocated 
;       data pointers.
;
; CALLING SEQUENCE:
;       Obj->[scanSee::]Delete
;
; ARGUMENTS:
;     None.
;
; EXAMPLE:
;    Following example deallocate the data pointers for a scanSee object
;
;         v2->Delete
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Jan 19, 2000.
;       xx-xx-xxxx      comment
;-


	scanimage_free,self.gD
	obj_destroy,self

	catch,error_status
	if error_status ne 0 then return
	wdelete,0
END

PRO scanSee::Cleanup
;+
; NAME:
;       scanSee::Cleanup
;
; PURPOSE:
;       This method calls the heap_gc for the unused heap variables.
;
; CALLING SEQUENCE:
;       Obj->[scanSee::]Cleanup
;
; ARGUMENTS:
;       None.
;
; EXAMPLE:
;    Following example frees up all the unused heap variables.
;
;         v2->Cleanup
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Jan 19, 2000.
;       xx-xx-xxxx      comment
;-
;	help,/heap_variables
	heap_gc	
END

FUNCTION scanSee::Init,file=file,help=help
;+
; NAME:
;       scanSee::Init
;
; PURPOSE:
;       This method initializes an object for an input scanSee file name.
;       It is automatically called by the OBJ_NEW function.
;
; CALLING SEQUENCE:
;       Obj->[scanSee::]Init,file=filename
;
; ARGUMENTS:
;     None.
;
; KEYWORD:
;     FILE   - specifies the input scanSee filename
;     HELP   - prints the usage syntax 
;
; EXAMPLE:
;    Following example creates a scanSee object.
;
;         v2 = obj_new('scanSee',file='cha:_0001.scan')
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Jan 19, 2000.
;       xx-xx-xxxx      comment
;-

	if keyword_set(file) then filename = strtrim(file,2) else begin
		return,0
	end
	if keyword_set(help) then begin
	str = ['Usage:','','V = OBJ_NEW("scanSee" , file="filename")', $
		'','filename is a required input. ']
	res = dialog_message(str ,/Error)
	return,0
	end

	found = findfile(filename,count=ct)
	if ct le 0 then begin
	res = dialog_message('File: '+filename + ' not found !',/Error)
	return,0
	end

;	catch,error_status
;	if error_status ne 0 then begin
;	str = ['Error!  Error!','Wrong type of file entered!']
;	res = dialog_message(str,/Error)
;	return,0
;	end

	if ptr_valid(self.gD) then scanimage_free,self.gD
heap_gc
	scanimage_alloc,filename,gD,scanno
	self.gD = gD
	
	self.scanno = *(*gD).scanno 
	if self.scanno lt 0 then return,1
	
	self.dim = *(*gD).dim 
	num_pts = *(*gD).num_pts
	self.width = num_pts(0)
	if self.dim eq 2 then self.height = num_pts(1)

	id_def = *(*gD).id_def
	ndet = n_elements(id_def(*,0))
	self.def = id_def(4:ndet-1,0)

	cd,current=home
	self.home = home
	self.file = filename
	rp = rstrpos(filename,!os.file_sep)
	if rp eq -1 then self.path = '' else $
	self.path = strmid(filename,0,rp+1)
	len = strlen(filename)-rp
	self.name = strmid(filename,rp+1,len)
	rp = rstrpos(self.file,'_')
	self.prefix = strmid(self.file,0,rp+1)
	self.suffix =  '.scan'

	dir = self.path
        CATCH,error_status
        if error_status ne 0 then begin
        if self.path ne '' and self.home ne self.path then $
        dir = self.home+!os.file_sep else $
        dir = getenv('HOME')+!os.file_sep
        end
        openw,1,dir+'.tmp'
        close,1
        self.outpath = dir

	self.detname =  'D'+ [strtrim(indgen(9)+1,2),'A','B','C','D','E','F' , $
        	'01','02','03','04','05','06','07','08','09', $
        	strtrim(indgen(61)+10,2)]
	return,1
END

PRO scanSee__define
	
struct = {scanSee, $
        home    : '', $
        path    : '', $
        outpath : '', $
        name    : '', $
        file    : '', $  ;/home/sricat/CHA/data/rix/cha:_0010.scan',           $
	prefix  : '', $
	suffix  : '', $      ; '.scan'
	scanno  : -1, $
	dim     : -1, $
	xaxis   : 0, $
	def     : make_array(85,/int), $
	detname : make_array(85,/string), $
	nd      : 0, $   ; index of last detector, 0 based
	width   : 0, $
	height  : 0, $
	win     : -1, $
	gD      : ptr_new(/allocate_heap) $
	}
END
