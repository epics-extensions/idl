;
; this currently read the version1_1 scanSee data format
;
@u_read.pro
@fit_statistic.pro
@readScan.pro
@colorbar.pro
@scan2d_roi.pro

PRO scanSee::Ezfit,detno=detno,row=row,column=column,group=group
view=0
if keyword_set(detno) then view=detno
lineno=0
if keyword_set(row) then lineno = row-1

      self->read,dim=dim,cpt=cpt,labels=labels,id_def=id_def, $
                da1d=da,pa1d=pa,da2d=da2d,pa2d=pa2d, $
		view=view,x=xa,y=ya,im=im

	if self.dim eq 2 then begin

	if keyword_set(column) then $
        ez_fit,xarray=xa,yarray=ya,im=im,group=group,inpath=self.path, $
                ipick=column-1 else $
        ez_fit,xarray=xa,yarray=ya,im=im,group=group,inpath=self.path, $
                jpick=lineno
        end

        if dim eq 1 then begin
	VX = pa(*,0)
	VY = da(*,0)
	if keyword_set(detno) then VY=da(*,detno-1)
	ez_fit,xarray=vx,yarray=vy,group=group,inpath=self.path
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
;    VX:       specifies/returns the independent variable X
;    VY:       specifies/returns the dependent variable Y
;
; KEYWORDS:
;    ROW:      specifies the 1D scan row sequence number valid only if 
;              2D scan file opened 
;    COLUMN:   specifies the column number valid only if 
;              2D scan file opened 
;    DETECTOR  specifies the desired detector number , default to 1
;    C_MASS:   returns the center of mass of the Y curve
;    X_PEAK:   returns the X coordinate corresponding to peak Y value
;    Y_PEAK:   returns the peak Y value
;    Y_HPEAK:  returns the Y value at the FWHM width
;    X_FWDL:   returns the left end of X coordinate of the FWHM width 
;    X_FWDR:   returns the right end of X coordinate of the FWHM width 
;    FWHM:     returns the full width of the half peak  
;    FIT:      specifies whether fitting is desired before the statistic calculation  
;    REPORT:   specifies whether rpt.listing button of data is desired  
;    GROUP:    specifies the parent widget ID if plot desired  
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

	if keyword_set(report) then $
	statistic_1d,VX,VY,c_mass,x_peak,y_peak,y_hpeak,fwhm,xl,xr,/plot,list=list,report=report,TITLE=title,GROUP=group else $
	statistic_1d,VX,VY,c_mass,x_peak,y_peak,y_hpeak,fwhm,xl,xr,/plot,list=list,TITLE=title,GROUP=group

	if n_elements(xl) then x_hwdl = xl
	if n_elements(xr) then x_hwdr = xr
END

PRO scanSee::Read,errcode,view=view,dim=dim,num_pts=num_pts,cpt=cpt,pv=pv, $
labels=labels,id_def=id_def,pa1d=pa1d,da1d=da1d,pa2d=pa2d,da2d=da2d, $
scanno=scanno,IP=IP,JP=JP,X=x,Y=y,im=im
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
;                    [,View=detno] [,X=x], [,Y=y] [,Im=im]
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
;     ID_DEF  - ID_DEF[19] or ID_DEF[19,2] returns the indicator for 
;               defined positoner/detector of the 1D/2D scan, 
;               if value 0 not present, 1 present in scan record
;     PA1D    - PA1D[CPT[0],4] returns positional array for scan1 record
;               or scan2 record for 1D/2D scan
;     DA1D    - PA1D[CPT[0],15] returns detector array for scan1 record
;               or scan2 record for 1D/2D scan
;     PA2D    - PA2D[NUM_PTS[0],NUM_PTS[1],4] returns positional array for 
;               2D scan 
;     DA2D    - DA2D[NUM_PTS[0],NUM_PTS[1],15] returns detector array for 
;               2D scan 
;     VIEW    - specifies the image of the detector # to be plotted 
;     X       - returns the X vector of Positioner 1
;     Y       - returns the Y vector of Positioner 1
;     IM      - returns the image array for the selected detector 
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
	if self.scanno lt 0 or self.dim lt 1 then return
	errcode = 0

	scanno = *(*self.gD).scanno
	dim = *(*self.gD).dim
	num_pts = *(*self.gD).num_pts
	cpt = *(*self.gD).cpt
	pv = *(*self.gD).pv
	labels = *(*self.gD).labels
	id_def = *(*self.gD).id_def

	pa1 = *(*self.gD).pa1D
	da1 = *(*self.gD).da1D
	pa1D = pa1(0:cpt(0)-1,*)
	da1D = da1(0:cpt(0)-1,*)

	if dim eq 2 then begin
	da2D = *(*self.gD).da2D
	pa2D = *(*self.gD).pa2D
	x = pa2D(*,0,0)
	y = pa1D(*,0)

	detector=1
	im = da2d(*,*,0)
	if keyword_set(view) then begin
	if view gt 1 then detector = view 
	im = da2d(*,*,detector-1)
	scan2Ddata,self.gD,detector,/view,xarr=xarr,yarr=yarr,im=im
	end
	end

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
	vmin = make_array(15,/float)
	vmax = make_array(15,/float)

	for i=0,14 do begin
	if def(i) gt 0 then begin
	vmax(i) = max(image_array(*,*,i))
	vmin(i) = min(image_array(*,*,i))
	end
	end

	if keyword_set(panimage) then self->panImage 
END

PRO scanSee::panImage,GIF=GIF,TIFF=TIFF,PICT=PICT,REVERSE=REVERSE
;+
; NAME:
;       scanSee::PanImage
;
; PURPOSE:
;       This method allows the user to view detector images in a pop up window.
;
; CALLING SEQUENCE:
;       Obj->[scanSee::]PanImage] [,GIF=GIF] [,TIFF=TIFF] [,REVERSE=REVERSE]
;                  [,PICT=PICT]
;
; ARGUMENTS:
;     None.
;
; KEYWORDS:
;       GIF      - specifies the output gif filename
;       TIFF     - specifies the output tiff filename
;       REVERSE  - indicates the reverse tiff is desired 
;       PICT     - specifies the output pict filename
;       GROUP    - specifies the parent widget ID 
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
;-

; pops up pan images

	def = self.def
	seq = self.scanno
	image_array = *(*self.gD).da2D

update:

	width = 60
	height = 60
	old_win = !D.window

	if n_elements(factor) then begin
		if factor lt .1 then factor = 1
		width = width * factor
		height = height * factor
	end

catch,error_status
if error_status then self.win=-1
if self.win ne -1 then wdelete,self.win
self.win = -1
	if self.win lt 0 then begin
		window,/free, xsize = 8*width, ysize=2*height, $
			title=self.name+' SCAN # '+strtrim(seq,2)
		for i=0,14 do begin
		xi=(i mod 8)*width+width/2 - 5 
		yi=height/2+(15-i)/8*height
		xyouts, xi,yi,'D'+strtrim(i+1,2),/device
		end
	end

new_win = !D.window
self.win = new_win

	wset,new_win
	for sel=0,14 do begin
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


	plots,[0,8*width],[height,height],/device
	for i=1,7 do plots,[i*width,i*width],[0,2*height],/device

	if keyword_set(TIFF) then begin
		tvlct,r,g,b,/get
		tiffname = strtrim(tiff,2)
	 	if keyword_set(reverse) then $
		write_tiff,tiffname,reverse(TVRD(),2),1,red=r,green=g,blue=b $
		else write_tiff,tiffname,TVRD(),red=r,green=g,blue=b
	end

	if keyword_set(GIF) then begin
		tvlct,r,g,b,/get
		gifname = strtrim(gif,2)
		write_gif,gifname,TVRD(),r,g,b
		write_gif,gifname,/close
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
;       xx-xx-xxxx      comment
;-

	seqno = 0
	str = '0000'

	list = findfile(self.path,count=ct)
	if ct gt 0 then begin

	ip = rstrpos(self.prefix,!os.file_sep)+1
	prefix = strmid(self.prefix,ip,strlen(self.prefix)-ip)

	; find the first scan # then goto step1

	is = strlen(prefix)	
	nf = 0
	for i=0,n_elements(list) -1 do begin
	l2 = rstrpos(list(i),self.suffix)
	if l2 ne -1 then begin
		l1 = rstrpos(list(i),prefix)
		if l1 ne -1 then begin
			if nf eq 0 then str = strmid(list(i),is,l2-is) else $
			str = [str, strmid(list(i),is,l2-is)]
			seqno = fix(str)
			goto, step1
		end
	end
	end

step1:
 	filename = self.prefix+str + self.suffix
print,filename

	; if seqno found then replace the current

	found = findfile(filename,count=ct)
	if ct gt 0 then begin
	self->delete
	self = obj_new('scanSee',file=filename)
	endif else begin
	  res = dialog_message(filename+' not found!',/Error)
	end
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
;-
	found = findfile(self.path,count=ct)
	len = strlen(self.prefix)
	sp = rstrpos(self.prefix,!os.file_sep)
	if sp gt -1 then sp=sp+1
	prefix = strmid(self.prefix,sp,len-sp)
	num = 0
	for i=0,n_elements(found)-1 do begin
	len1 = strlen(prefix)
	rp = strpos(found(i),prefix)
	rp1 = rstrpos(found(i),self.suffix)
	if rp eq 0 and rp1 gt len1 then begin
		ar = strmid(found(i),len1,rp1-len1)
		if fix(ar) gt num then begin
			num= fix(ar)
			ip = i
			end
		end
	end

	filename = self.path+found(ip)
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
	if keyword_set(plot) then $
	scan2Ddata,self.gD,detector,/plot,xarr=xarr,yarr=yarr,im=im,group=group,_extra=e else $
	scan2Ddata,self.gD,detector,/view,xarr=xarr,yarr=yarr,im=im,group=group,_extra=e
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

	if keyword_set(view) then $
	scan2Ddata,self.gD,detector,/view,xarr=px,yarr=py,im=data 
	if keyword_set(plot) then $
	scan2Ddata,self.gD,detector,/plot,xarr=px,yarr=py,im=data ,group=group
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
;       xx-xx-xxxx      comment
;-
; ysel - string list
	scanno = 1
	if keyword_set(no) then scanno = no
	self.xaxis = 0
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
	if ct lt 1 then spawn,!os.mkdir + ' ' +dir

	dim = self.dim
	cpt = *(*self.gD).cpt
	num_pts = *(*self.gD).num_pts
	seqno = 1

	if dim eq 1 then begin
	def = *(*self.gD).id_def
	ndim = n_elements(def)
	pa1d = *(*self.gD).pa1d
	da1d = *(*self.gD).da1d
	pa = pa1d(0:cpt(0)-1,*)
	da = da1d(0:cpt(0)-1,*)
	labels = *(*self.gD).labels
	end
	if dim eq 2 then begin
	if n_elements(no) gt 0 then begin
		if no gt 0 and no le cpt(1) then seqno = no else begin
		res = dialog_message('Invalid seq # specified, reset to 1 ',/info)
		end
	end
	id_def = *(*self.gD).id_def
	ndim = n_elements(id_def)/2
	def = id_def(0:ndim-1)
	labels = *(*self.gD).labels(*,0)
	pa2d = *(*self.gD).pa2D
	da2d = *(*self.gD).da2d
	pa = make_array(cpt(0),4,/double)
	da = make_array(cpt(0),ndim-4)
	pa[*,*] = pa2d(*,seqno-1,*)
	da[*,*] = da2d(*,seqno-1,*)
	end

	pts = *(*self.gD).cpt(0)
	np = fix(total(def(0:3)))
	nd = fix(total(def(4:ndim-1)))
	x_names = make_array(np,/string,value=string(replicate(32b,30)))
	x_engus = x_names
	x_descs = x_names
	y_names = make_array(nd,/string,value=string(replicate(32b,30)))
	y_engus = y_names
	y_descs = y_names
	num=0
	for i=0,3 do begin
		if def(i) gt 0 then begin
		x_names(num) = labels(i)
		x_descs(num) = labels(i+19)
		x_engus(num) = labels(i+38)
		num = num +1
		end
	end
	num=0
	for i=0,14 do begin
		if def(i+4) gt 0 then begin
		y_names(num) = labels(i+4)
		y_descs(num) = labels(i+4+19)
		y_engus(num) = labels(i+4+38)
		num = num +1
		end
	end

	seqstr = 'Scan # '+strtrim(self.scanno,2)
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
	y = da(*,0:nd-1)
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

	st0=string(replicate(32b,ind_width+(nd+np)*fwidth))

	openw,3,dir+report

	printf,3,'; Source File: '+self.file
	printf,3,'; ASCII  File: '+dir+report
	printf,3,'; ',seqstr

	st1 = '; '
	for i=0,np-1 do st1 = st1 + x_names(i) + " "
	for i=0,nd-1 do st1 = st1 + y_names(i) + " "
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
	for i=0,nd-1 do begin
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

PRO scanSee::Calibration,GROUP=group,_extra=e
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
	
	if self.dim eq 1 then begin
	pa = *(*self.gD).pa1D
	da = *(*self.gD).da1D
	labels = *(*self.gD).labels

	x = pa(*,0)
	y = da(*,0)
	scanno = self.scanno
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
	self->images,image_array,def,/PANIMAGE
	scanno = self.scanno

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
catch,error_status
if error_status then goto,delstep
	if self.win ne -1 then wdelete,self.win
delstep:
	scanimage_free,self.gD
	obj_destroy,self
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

	catch,error_status
	if error_status ne 0 then begin
	str = ['Error!  Error!','Wrong type of file entered!']
	res = dialog_message(str,/Error)
	return,0
	end

	scanimage_alloc,filename,gD,scanno
	self.gD = gD
	
	self.dim = *(*gD).dim 
	self.scanno = *(*gD).scanno 
	num_pts = *(*gD).num_pts
	self.width = num_pts(0)
	if self.dim eq 2 then self.height = num_pts(1)

	id_def = *(*gD).id_def
	ndim = n_elements(id_def)/2
	self.def = id_def(4:ndim-1,0)

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
	def     : make_array(15,/int), $
	width   : 0, $
	height  : 0, $
	win     : -1, $
	gD      : ptr_new(/allocate_heap) $
	}
END
