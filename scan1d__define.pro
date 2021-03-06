;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
;
; scan1d__define.pro
;

@u_read.pro
@fit_statistic.pro
@fixIndexFile.pro

PRO readfixindex,indexfile,fsize,maxno,array

	found = findfile(indexfile,count=ct)
	if ct eq 0 then return

	t = lonarr(5)
	if !d.name eq 'WIN' then openr,unit1,indexfile,/get_lun,/XDR else $
	openr,unit1,indexfile,/get_lun
	
	point_lun,unit1,0
	readu,unit1,t
	if t(0) eq 0 and t(1) eq 7 then fname=''
	readu,unit1,fname
	readu,unit1,t
	if t(0) eq 0 and t(1) eq 3 then fsize=0L 
	readu,unit1,fsize
	readu,unit1,t
	if t(0) eq 0 and t(1) eq 2 then maxno=0 
	readu,unit1,maxno
	readu,unit1,t
	if t(2) eq 3 then array = make_array(t(1),/long)
	readu,unit1,array
	free_lun,unit1
	close,unit1

END

PRO scan1d::FixIndexFile,nowrite=nowrite,print=print

	self.fptr = make_array(10000,/long)

	WIDGET_CONTROL,/HOURGLASS
	catch1d_newIndexFile,self.file,array,/XDR,nowrite=nowrite,print=print
	maxno = n_elements(array)
	self.fptr = array
	self.maxno = maxno-1

END

PRO scan1d::Calibration,scanno,format=format,GROUP=group

	if n_elements(scanno) then $
	self->read,scanno,def=def,pa=pa,da=da,np=np,nd=nd,/plot, $
	x_names=x_names,x_descs=x_descs,x_engus=x_engus, $
	y_names=y_names,y_descs=y_descs,y_engus=y_engus, $
	title=title,stamp=stamp,file=file ,seqno=seqno else $
	self->read,def=def,pa=pa,da=da,np=np,nd=nd,/plot, $
	x_names=x_names,x_descs=x_descs,x_engus=x_engus, $
	y_names=y_names,y_descs=y_descs,y_engus=y_engus, $
	title=title,stamp=stamp,file=file ,seqno=seqno 
	
       path = self.path
        ln = strlen(self.path) 
        if ln gt 0 and strmid(self.path,ln-1,1) ne !os.file_sep then $
                path = self.path + !os.file_sep

	def = def[4:18]
	calibration_factor,da,def,xv=pa,classname=file,title=title,$
		inpath=path,format=format,GROUP=group
END

PRO scan1d::Statistic,VX,VY,C_MASS=c_mass,X_PEAK=x_peak,Y_PEAK=y_peak, $
		Y_HPEAK=y_hpeak, X_HWDL=x_hwdl,X_HWDR=x_hwdr, FWHM=fwhm, $
		NO=NO,DETECTOR=detector,FIT=FIT,LIST=LIST
;+
; NAME:
;       scan1d::Statistic
;
; PURPOSE:
;       This method allows the user to calculate peak,fwhm width
;
; CALLING SEQUENCE:
;       Obj->[scan1d::]STATISTIC [,X,Y] ,C_MASS=c_mass,X_PEAK=x_peak, $
;                      Y_PEAK=y_peak,Y_HPEAK=y_hpeak, $
;                      X_HWDL=x_hwdl,X_HWDR=x_hwdr,FWHM=fwhm, $
;                      [,NO=no] [,DETECTOR=detector]
;
; ARGUMENTS:
;    VX:        specifies/returns the independent variable X
;    VY:        specifies/returns the dependent variable Y
;
; KEYWORDS:
;    NO:       specifies the input 1D scan sequence number, if specified
;              the new data read in and a new VX and VY will be returned.
;    DETECTOR  specifies the desired detector number , default to 1
;    C_MASS:   returns the center of mass of the Y curve
;    X_PEAK:   returns the X coordinate corresponding to peak Y value
;    Y_PEAK:   returns the peak Y value
;    Y_HPEAK:  returns the Y value at the FWHM width
;    X_FWDL:   returns the left end of X coordinate of the FWHM width 
;    X_FWDR:   returns the right end of X coordinate of the FWHM width 
;    FWHM:     returns the full width of the half peak  
;    LIST:
;
; EXAMPLE:
;   Example 1  will calucultate the FWHM value for the 6th scan and
;    2nd detector. The calculated value for center of mass, peak x, peak y,
;    half peak y value, half peak x values, and width are all returned.
;    
;    The object v1 need to be defined only if it is not yet defined.
;
;         v1 = obj_new('scan1d',file='junk2')
;         v1->statistic,VX,VY,c_mass=cx,x_peak=xp,y_peak=yp, $
;                   y_hpeak=yhp,x_hwdl=xl,x_hwdr=xr,fwhm=fwhm,NO=6,DET=2
;
;   Example 2  will calucultate the FWHM value for the know vectors VY versus
;    VX. The calculated value for center of mass, peak x, peak y,
;    half peak y value, half peak x values, and width are all returned.
;
;         v1->statistic,VX,VY,c_mass=cx,x_peak=xp,y_peak=yp, $
;                   y_hpeak=yhp,x_hwdl=xl,x_hwdr=xr,fwhm=fwhm
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Sept. 27, 1999.
;       xx-xx-xxxx      comment
;-

if n_params() eq 0 then begin
	res = dialog_message('Usage: obj->statistic,Vx,Vy,C_MASS=c_mass,X_PEAK=xpeak,Y_HPEAK=yhpeak,FWHM=fwhm',/info)
	return
	end
if n_params() eq 1 then begin
	VY = VX
	VX = indgen(n_elements(VY))
	end
if keyword_set(NO) then begin
	self->read,no,pa=pa,da=da,seqno=seqno
	det=0
	if keyword_set(detector) then det=detector-1	
	VX = pa(*,0)
	VY = da(*,det)
end

	if keyword_set(list) then $
	statistic_1d,VX,VY,c_mass,x_peak,y_peak,y_hpeak,fwhm,xl,xr,/plot,/LIST,report='fwhm.rpt' else $
	statistic_1d,VX,VY,c_mass,x_peak,y_peak,y_hpeak,fwhm,xl,xr,/plot

	x_hwdl = xl
	x_hwdr = xr
END


PRO parse_num0,instring,ids,sep=sep
Keysepar = '-'
if keyword_set(sep) then Keysepar = sep
res = strpos(instring,keysepar)
if res ne -1 then begin
        str = str_sep(instring,keysepar,/trim)
        no = fix(str(1)) - fix(str(0)) + 1
        ids = indgen(no) + fix(str(0))
endif else begin
        com = strpos(instring,',')
        if com ne -1 then begin
                str = str_sep(instring,',')
                ids = fix(str)
        endif else begin
                str = str_sep(instring,' ')
                ids = fix(str)
        end
end
END

; parse by sep1 first then by sep2
; default sep1=',' sep2='-'
;       instring = '1,2:5,7'
;       instring = '1,2-5,7'
PRO parse_num,instring,res,sep1=sep1,sep2=sep2
        d_sep1 = ','
        d_sep2 = '-'
        if keyword_set(sep1) then d_sep1 = sep1
        if keyword_set(sep2) then d_sep2 = sep2
        str = str_sep(instring,d_sep1,/trim)
        res = fix(str(0))
        for i=0,n_elements(str)-1 do begin
        newstr =  strtrim(str(i),2)
	if strlen(newstr) gt 0 then begin
        parse_num0,newstr,ids,sep=d_sep2
        if i eq 0 then begin
                if n_elements(ids) gt 1 then res = ids
                end
        if i gt 0 then  res = [res,ids]
	end
        end
END

PRO scan1d::ASCII,list,nowin=nowin
;+
; NAME:
;	scan1d::ASCII
;
; PURPOSE:
;       This method allows the user to create a list of ASCII data files
;       based on the user specified list of 1D scan numbers. The name 
;       convention of each ASCII file will be the 1D scan file suffixed 
;       with its 4 digit scan number.
;
; CALLING SEQUENCE:
;       Obj->[scan1d::]ASCII, List, /NOWIN
;
; ARGUMENTS:
;     List:      List is used to specify the sequence of 1D scans to be
;                generated. It can be a list of short integers which 
;                explicitly specify the desired scans or it can be 
;                string variable. If it is a string, it will be parsed
;                into a list of scan number first by this method.
;                The user has to insure that the number entered is valid
;                for normal operation of this method.
;
; KEYWORDS:
;     NOWIN:     If specified, only the ASCII files will be created but the
;                xdisplayfile window will not be shown. 
;
; EXAMPLE:
;     Example 1 creates the ASCII data files for the scan # [10,20,30] from 
;         file 'junk2', and each file will be displayed automatically. 
;         The object v1 needs to be defined only if it is not yet defined.
;
;         v1 = obj_new('scan1d',file='junk2')
;         v1->ASCII,[10,20,30]
;
;     Example 2 creates the ASCII data files for the scan # 10, 20 to 30, and
;         40 from the 'junk2', and no ASCII file will be displayed. 
;
;         v1->ASCII,'10,20-30,40',/NOWIN
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jan 26, 1998.
;	xx-xx-xxxx      comment
;-


if n_elements(list) lt 1 then return

; parse the string list

sz = size(list)
if sz(n_elements(sz)-2) eq 7 then begin
sep1=','
if strpos(list,sep1) lt 0 then sep1=' '
if strpos(list,':') gt 0 then parse_num,string(list),res,sep1=sep1,sep2=':' else $
parse_num,string(list),res,sep1=sep1
print,'scan:',res
list = res
end

; integer list

	for i=0,n_elements(list)-1 do begin
	if list(i) le self.maxno then begin
	self->point_lun,list(i)-1
	if keyword_set(nowin) then self->read,/list,/nowin else $
	self->read,/list
	endif else r=dialog_message(string(list(i))+' exceeds the maxno'+string(self.maxno),/error)
	end

END


; check the binary type and return lun unit and XDR type
PRO scan1d::Open,filename,wid
;+
; NAME:
;	scan1d::Open
;
; PURPOSE:
;       The method opens the input data file according to the binary type.
;       It supports both native binary type and XDR binary type.
;
;       This method is automatically called by the ReadIndex method.
;
; CALLING SEQUENCE:
;       Obj->[scan1d::]Open, Filename [,Wid]
;
; ARGUMENTS:
;  FILENAME:  Specifies the input 1D scan file used (generated by data catcher) 
;  WID:       Optional input, specifies the input droplist widget ID to reflect 
;             the binary TYPE
;
; KEYWORDS:
;     None.
;
; EXAMPLE:
;    Following example show how to explicitly open the 1D scan data file 'junk2'
;    The v1 object needs to be defined only if it is not yet defined.

;         v1 = obj_new('scan1d')
;         v1->open,'junk2'
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jan 26, 1998.
;	xx-xx-xxxx      comment
;-

if !d.name eq 'X' then begin
        type = 1
        U_OPENR,unit,filename,/XDR
        u_read,unit,version,errcode
        if errcode lt 0 then begin
                u_close,unit
                U_OPENR,unit,filename
                type = 0
        end
	if  n_elements(version) eq 0 then begin
	r=dialog_message('Error: wrong type of file entered!',/Error)
	return
	end
end
if !d.name eq 'WIN' then begin
        U_OPENR,unit,filename,/XDR
        type = 1
end
        if n_params() eq 4 then WIDGET_CONTROL,wid,set_droplist_select=type
	self.unit = unit
	self.type = type

	; get path

        self.file = filename
        pos = rstrpos(filename,!os.file_sep)
        if pos gt 0 then begin
                self.path = strmid(filename,0,pos)
                self.file = strmid(filename,pos+1,strlen(filename))
        endif else begin
                self.path = self.home
        end

	self->point_lun,0

END

PRO scan1d::Plot,no,ix=ix,iy=iy
;+
; NAME:
;	scan1d::Plot
;
; PURPOSE:
;       The method reads and plots the next scan record from the opened 
;       logical unit. Scan record supports 4 positioners and 15 detectors.
;       Default option all present detectors will be plotted. If X vector
;       is a constant value then vector index will be used in X axis.
;
; CALLING SEQUENCE:
;       Obj->[scan1d::]Plot [,Seqno] [,IX=#] [,IY='#,...']
;
; ARGUMENTS:
;     SEQNO:       Optional, if specified the specified record will be read
;                  and plotted. Otherwise the next record from the current
;                  file pointer will read and plotted.
;
; KEYWORDS:
;     IX:          Optional, it specifies the positioner number to be used
;                  as X axis. 
;                  If not specified, the positioner one's vector is used. 
;     IY:          Optional, it specifies the list of detector numbers to 
;                  be plotted. 
;                  If not specified, every existing detector will be plotted.
;                  If specified, only the entered detectors will be plotted.
;
; EXAMPLE:
;    Following example plots the first and the 10th record from the file 'junk2'.
;    The object v1 needs to be defined only if it is not yet defined.
;
;         v1 = obj_new('scan1d',file='junk2')
;         v1->Plot
;         v1->Plot,10
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jan 26, 1998.
;	xx-xx-xxxx      comment
;-

	if n_elements(no) then begin
		if no gt 0 and no le self.maxno then $
		self->point_lun,no-1
	end
		self->read,seqno=seqno,pts=pts,np=np,nd=nd,pa=pa,da=da,def=def,stamp=stamp,x_descs=x_descs,y_descs=y_descs,x_names=x_names,y_names=y_names,x_engus=x_engus,y_engus=y_engus
;help,pa,da,seqno,pts,np,nd,def

	p_pick = 0
	if n_elements(ix) then begin
		if ix gt 1 and ix le np then p_pick = ix - 1
	end 
	x = pa(*,p_pick)
	if MIN(x) eq MAX(x) then x = indgen(pts)
	
	if n_elements(iy) then begin
	  list = iy
	  sz = size(iy)
	  if sz(n_elements(sz)-2) eq 7 then begin
	  sep1=','
	  if strpos(list,sep1) lt 0 then sep1=' '
	  if strpos(list,':') gt 0 then  $
	  parse_num,string(list),res,sep1=sep1,sep2=':' else $
	  parse_num,string(list),res,sep1=sep1
	print,'detectors:',res
	  list = res
	  end
	
	  d_pick = n_elements(list)
	  if d_pick lt 15 then begin
		y = make_array(pts,d_pick)
		for i=0,d_pick-1 do begin
		ij = list(i) - 1
		y(*,i) = da(*,ij)
		end
	  end
	endif else y = da

	xtitle = x_descs(p_pick)
	if xtitle eq '' then xtitle=x_names(p_pick)
	comment = ' Scan # '+string(seqno)
	comment = [comment, 'File : '+ self.file, stamp]
	plot1d,x,y,id_tlb,title=title,comment=comment,xtitle=xtitle
	self.win = id_tlb
END

;
; unit known
;
PRO scan1d::Read,no,list=list,nowin=nowin,plot=plot, $
seqno=seqno,scanno_2d=scanno_2d,pts=pts, $
pa=pa,da=da,np=np,nd=nd,title=title,stamp=stamp,file=file, $
x_names=x_names,x_descs=x_descs,x_engus=x_engus, $
y_names=y_names,y_descs=y_descs,y_engus=y_engus,def=def

;+
; NAME:
;	scan1d::Read
;
; PURPOSE:
;       The method reads a scan record from the opened logical unit and
;       returns the user desired data components through the keyword 
;       specification.
;
; CALLING SEQUENCE:
;       Obj->[scan1d::]Read,No,/List,/Plot,pa=pa,da=da,np=np,nd=nd,...
;
; ARGUMENTS:
;     NO:       Optional. If not specified the next record from the current
;               file pointer will be read in. If the scan # is specified,
;               then the file pointer will be moved to the beginning of 
;               the desired scan record before reading in the next set 
;               of scan record. 
;
; KEYWORDS:
;  PLOT:        To view the DA array after reading in the data if specified 
;  LIST:        If /LIST specified then the ASCII PA & DA array will be saved
;               in default file name 'temp'. If LIST='filename' is specified
;               then the 'filename' will be saved. 
;  NOWIN:       If /NOWIN specified, the ASCII files will be created but will
;               not be displayed by the scroll window.
;  NP:          Returns the number of positioner vectors
;  ND:          Returns the number of detector vectors
;  PTS:        Returns the number of data points acquired for the scan
;  PA[NPTS,NP]: Returns the positioner array 
;  DA[NPTS,ND]: Returns the detector array 
;  X_NAMES[NP]: Returns the string names of positioners 
;  X_DESCS[NP]: Returns the string descriptions of positioners 
;  X_ENGUS[NP]: Returns the string engineering units of the positioners 
;  Y_NAMES[ND]: Returns the string names of detectors 
;  Y_DESCS[ND]: Returns the string descriptions of detectors 
;  Y_ENGUS[ND]: Returns the string engineering units of the detectors 
;  SEQNO:       Returns current scan number
;  SCANNO_2D:   Returns corresponding 2D scan number
;  TITLE:       Returns plot title stored
;  STAMP:       Returns time stamp and user ID stored
;  FILE:        Returns the filename 
;  DEF[19]:     Returns 4 positioner and 15 detector presence indicators 
;
; EXAMPLE:
;    Following example read the scan record #10 from the 'junk2'.
;    The object v1 needs to be defined only if it is not yet defined.
;
;         v1 = obj_new('scan1d',file='junk2')
;         v1->point_lun,10
;         v1->read,da=da,pa=pa,pts=pts,np=np,nd=nd
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jan 26, 1998.
;	xx-xx-xxxx      comment
;-

unit = self.unit

if n_params() then begin
	if no gt 0 and no le self.maxno then self->point_lun,no-1
end
	u_read,unit,version
	u_read,unit,pv
	u_read,unit,num_pts
	u_read,unit,id_def
	u_read,unit,x_dpt

; positioner 
np = 0
for i=0,3 do begin
	if id_def(i) gt 0 then np = np + 1
end
PA = make_array(num_pts(0)+1,np,/double)
for i=0,np-1 do begin
        u_read,unit,px
        PA(*,i) = px
end

; detector
nd = 0
for i=0,14 do begin
	if id_def(4+i) gt 0 then nd = nd + 1
end
DA = make_array(num_pts(0)+1,nd)
for i=0,nd-1 do begin
        u_read,unit,px
        DA(*,i) = px
end

	u_read,unit,labels
	u_read,unit,x
	u_read,unit,y
	u_read,unit,n
	if n(0) gt 0 then begin
	u_read,unit,ze
	end

if arg_present(da) or keyword_set(plot) or keyword_set(list) then begin

	pts = num_pts(0) + 1
	seqno = fix(y(0)) + 1
	refno = fix(y(1))
	y_seqno = fix(y(2))
	scanno_2d = fix(y(3))
	y_req = fix(y(5))
	y_act = fix(y(6))
	y_value = y(7)
	title = strtrim(x(0),2)
	stamp = x(4)
	file = strtrim(x(3),2)
	def = id_def

	labels = string(labels)
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

	; list is requested

	if keyword_set(list) then begin

sz = size(list)
if sz(n_elements(sz)-2) ne 7 then begin
suf0 = '0000'
suf = strtrim(seqno,2)
ln = strlen(suf)
strput,suf0,suf,4-ln
file = self.file +'.'+suf0
endif else file = list+'.txt'

	report = file
	pos = rstrpos(file,!os.file_sep)
	report = strmid(file,pos+1,strlen(file)-pos-1)
	sz=size(list)
	if sz(n_elements(sz)-2) eq 7 then report = strtrim(list,2) 

	st0=string(replicate(32b,(nd+np)*18))

	openw,3,self.dir+report

	printf,3,'Scan # ',strtrim(seqno,2)

	st1 = ''
	for i=0,np-1 do st1 = st1 + x_names(i) + " "
	for i=0,nd-1 do st1 = st1 + y_names(i) + " "
	printf,3,st1

	for k=0,pts-1 do begin
	ip = 0
	width = 18
	st = st0
	st1 = strtrim(k,2)
	len1 = strlen(st1)
	strput,st,st1,ip+4-len1-1
	ip = ip + 4
	for i=0,np-1 do begin
		st1 = strtrim(pa(k,i),2)
		len1 = strlen(st1)
		strput,st,st1,ip+width-len1-1
		ip = ip + width
		end
	for i=0,nd-1 do begin
		st1 = strtrim(da(k,i),2)
		len1 = strlen(st1)
		strput,st,st1,ip+width-len1-1
		ip = ip + width
		end
	printf,3,st
	end
	close,3
	if n_elements(nowin) eq 0 then $
	xdisplayfile,self.dir+report
	end


	; plot is requested

	if keyword_set(plot) then begin
	comment = ' Scan # '+string(seqno)
	comment = [comment, 'File : '+ file, stamp]
	plot1d,pa(*,0),da,title=title,comment=comment
	end
end

END

;
; file already opened with u_openr: unit
;
PRO scan1d::scan_read_all,maxno

unit = self.unit

	status = FSTAT(unit)
	self.seqno = 0
	self.size = status.size
	self.file = status.name

	indexFile = status.name + '.index'
	size = status.size

	self_file = status.name

u_rewind,unit
; check whether indexFile exist
found = findfile(indexFile)
if found(0) eq '' then begin

	id = 0
	self.fptr = make_array(10000,/long)
	point_lun,unit,0

	if !d.name eq 'WIN' then begin
	self->FixIndexFile,/print ;,/nowrite
	return
	end

	WHILE NOT  EOF(unit) DO BEGIN
	id = id + 1
		self->read
		point_lun,-unit,pos
		self.fptr(id) = pos
	END
	maxno = id	
	self.maxno = maxno
endif else begin

	readIndex,indexFile,fsize,maxno,array
	self.fptr = array
	self.maxno = maxno
	self.size = fsize

end
END

PRO scan1d::Point_lun,no
;+
; NAME:
;	scan1d::Point_lun
;
; PURPOSE:
;       The method moves the file pointer to the end of the specified scan
;       record and ready to read the next record.
;
; CALLING SEQUENCE:
;       Obj->[scan1d::]Point_lun, SEQNO
;
; ARGUMENTS:
;     SEQNO:   Specifies the zero based scan record number.
;
; KEYWORDS:
;     None.   
;
; EXAMPLE:
;    Following example moves the file pointer to the end of 10th record.
;    of the 1D scan file 'junk2'.
;    The object v1 needs to be defined only if it is not yet defined.
;
;         v1 = obj_new('scan1d',file='junk2')
;         v1->point_lun,10
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jan 26, 1998.
;	xx-xx-xxxx      comment
;-

	if no ge 0 and no le self.maxno then begin
	point_lun,self.unit,self.fptr(no)
	end
END

PRO scan1d::Readindex,filename
;+
; NAME:
;	scan1d::Readindex
;
; PURPOSE:
;       The method reads the index file of a given 1D scan file. It is 
;       assumed that the index file is written in native binary format.
;       If index file does not exist yet, it will be created by this
;       method.
;
; CALLING SEQUENCE:
;       Obj->[scan1d::]Readindex,Filename
;
; ARGUMENTS:
;  FILENAME:  Specifies the input 1D scan file used (generated by catcher) 
;
; KEYWORDS:
;     None.
;
; EXAMPLE:
;    Following examples reads in the index object corresponding to 'junk2'
;    The following two examples are equivalent in scan1d object creation:
;
;    Example 1
;
;         v1 = obj_new('scan1d')
;         v1->readindex,'junk2'
;
;    Example 2
;
;         v1 = obj_new('scan1d',file='junk2')
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jan 26, 1998.
;       05-15-1998  bkc Catch error for nozeo (due to openw )
;-

; check whether filename exists

if n_elements(filename) eq 0 then filename = self.file
fd = findfile(filename)
IF fd(0) NE '' THEN BEGIN
	
	indexfile = filename+'.index'

found = findfile(indexfile)
if found(0) ne '' then begin
	if !d.name eq 'WIN' then $
	readfixindex,indexfile,fsize,maxno,array else begin
		u_openr,unit,indexfile
		u_read,unit,fn
		u_read,unit,fsize
		u_read,unit,maxno
		u_read,unit,array
		u_close,unit
	end
	openr,1,filename
	status = FSTAT(1)
	close,1
	
	if status.size eq fsize(0) then begin
	self.file = filename
	self.size = fsize(0)
	self.maxno = maxno(0)
	self.fptr = array
	end
;	print,'***Read Index File: ',indexfile


	self->open,filename


endif else begin
	self->open,filename
	self->scan_read_all,maxno
	self->writeindex,filename
end

        ; get dir
 
	dir = ''
	if self.path ne '' then dir = self.path+!os.file_sep 
	CATCH,error_status
	if error_status ne 0 then begin
       	 if self.path ne '' and self.home ne self.path then $
       	 dir = self.home+!os.file_sep  else $
       	 dir = getenv('HOME')+!os.file_sep
	end
	openw,fw,dir+'1',/get_lun
	close,fw
	dir = dir + 'ASCII' 

	fd = findfile(dir,count=ct)
	if ct eq 0 then spawn,!os.mkdir + ' ' +dir
	dir = dir + !os.file_sep
	self.dir = dir
ENDIF ELSE BEGIN
	res=WIDGET_MESSAGE('Warning: file "' + filename + '" not found.',/info)
	return
END 

print,'Selected Data File         : ', filename
print,'Total Number of 1D Scans   : ', self.maxno

END

PRO scan1d::Writeindex,filename
;+
; NAME:
;	scan1d::Writeindex
;
; PURPOSE:
;       The method creates the index file for the specified 1D scan file.
;       Normally if the index file is not found for the scan file, it will
;       be automatically called by the Readindex method.
;
; CALLING SEQUENCE:
;       Obj->[scan1d::]Writeindex, filename
;
; ARGUMENTS:
;     FILENAME:   Specifies the input 1D scan file name.
;
; KEYWORDS:
;     None.   
;
; EXAMPLE:
;    Following example creates the scan index file for the file 'junk2'.
;
;         v1->writeindex,'junk2'
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jan 26, 1998.
;	xx-xx-xxxx      comment
;-

; check file existence

; if !d.name eq 'WIN' then return

if n_elements(filename) eq 0 then filename=self.file
found = findfile(filename)
if found(0) eq '' then return
	openr,1,filename
	status = FSTAT(1)
	close,1
	
	if filename eq self.file then begin

	if self.maxno gt 0 then begin

	indexfile = self.file + '.index'

	array = self.fptr(0:self.maxno)
	array(self.maxno) = status.size

	CATCH,error_status
	if error_status lt 0 then return
	if !d.name eq 'WIN' then U_OPENW,unit,ndexfile,/XDR else $
	U_OPENW,unit,indexfile
	u_write,unit,status.name
	u_write,unit,status.size
	u_write,unit,self.maxno
	u_write,unit,array
	u_close,unit

;print,'***File ',indexfile,' updated.'
	end
	end
END


PRO scan1d::Print
;+
; NAME:
;	scan1d::Print
;
; PURPOSE:
;       The method dumps the current contents of the index object.
;
; CALLING SEQUENCE:
;       Obj->[scan1d::]Print
;
; ARGUMENTS:
;     None.
;
; KEYWORDS:
;     None.
;
; EXAMPLE:
;    Lists the index object corresponding to 'junk2'.
;    The object v1 needs to be defined only if it is not yet defined.
;
;         v1 = obj_new('scan1d',file='junk2')
;         v1->Print
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jan 16, 1998.
;	xx-xx-xxxx      comment
;-

	print,'scan1d.home=',self.home
	print,'scan1d.path=',self.path
	print,'scan1d.dir=',self.dir
	print,'scan1d.file=',self.file
	print,'scan1d.type=',self.type
	print,'scan1d.unit=',self.unit
	print,'scan1d.size=',self.size 
	print,'scan1d.maxno=',self.maxno
	print,'scan1d.fptr=',self.fptr(0:self.maxno)
END

PRO scan1d::Delete
        obj_destroy,self
END

PRO scan1d::Cleanup
        if self.unit then free_lun,self.unit
        catch,error_status
        if error_status ne 0 then goto,reset
        if self.win ne -1 then widget_control,self.win,/destroy
reset:
        self.unit=0
        self.win = -1
END

FUNCTION scan1d::Init,file=file
; populate the index object if file is specified
loadct,39
	cd,current=h
	self.home = h
	if keyword_set(file) then self->Readindex,file
	return,1
END

PRO scan1d__define

; init view1d_viewscan
struct = { scan1d, $ 
	file: '', $
	path: '', $
	home: '', $
	dir: '', $
        type: 0, $
        unit: 0, $
        seqno: 0, $
        maxno: 0, $
	win: -1, $
	size: 0L, $
        fptr: make_array(10000,/long) $
        }
END
