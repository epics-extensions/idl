;
; scan2d__define.pro
;

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

PRO scan2d::ASCII,list,nowin=nowin
;+
; NAME:
;	scan2d::ASCII
;
; PURPOSE:
;       This method allows the user to create a list of ASCII data files
;       based on the user specified list of image numbers. The name convention
;       of each ASCII file will be the image file suffixed with its 4 digit 
;       image number.
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]ASCII, List, /NOWIN
;
; ARGUMENTS:
;     List:      List is used to specify the sequence of image files to be
;                generated. It can be a list of short integers which 
;                explicitly specify the desired images or it can be 
;                string variable. If it is a string, it will be parsed
;                into a list of image number first by this method.
;                The user has to insure that the number entered is valid
;                for normal operation of this method.
;
; KEYWORDS:
;     NOWIN:     If specified, only the ASCII files will be created but the
;                xdisplayfile window will not be shown. 
;
; EXAMPLE:
;     Example 1 creates the ASCII data files for the image [10,20,30] from the
;         'junk2.image', and each file will be displayed automatically. 
;         The object v2 need to be defined only if it is not yet defined.
;
;         v2 = obj_new('scan2d',file='junk2.image')
;         v2->ASCII,[10,20,30]
;
;     Example 2 creates the ASCII data files for the images 10, 20 to 30, and
;         40 from the 'junk2.image', and no ASCII file will be displayed. 
;
;         v2->ASCII,'10,20-30,40',/NOWIN
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jan 19, 1998.
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

if self.path eq '' then $
temp = obj_new('scan2d',file=self.name) else $
temp = obj_new('scan2d',file=self.path +'/'+self.name) 

	for i=0,n_elements(list)-1 do begin
	temp->point_lun,list(i)-1
	temp->read
	if keyword_set(nowin) then temp->datatotext,/outfile,/nowin else $
	temp->datatotext,/outfile
	end

obj_destroy,temp
END

PRO scan2d::dataToText,data,px,py,title=title,unit=unit,outfile=outfile,nowin=nowin
;+
; NAME:
;	scan2d::DataToText
;
; PURPOSE:
;       For the current scan2d object, this method creates its tabulated 2D 
;       image data in a disk file and uses the xdisplayfile command to 
;       show the contents of the created ASCII file.
;
;       It will try to create the ASCII file in the following order: 
;       try the data directory first, if failed then try the user starting 
;       directory, if still failed then try the  user home directory.
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]DataToText, Data, Px, Py, OUTFILE='outfile', /NOWIN
;
; ARGUMENTS:
;     Data:      Optional output variable, gives the output image data array. 
;     Px:        Optional output variable, gives the X positioner vector. 
;     Py:        Optional output variable, gives the Y positioner vector. 
;
; KEYWORDS:
;     OUTFILE:   If not specified, the default outfile text file name with
;                'view2d_data.txt' will be used.
;
;                If OUTFILE=1 is specified, then the outfile name will be
;                generated from the image file name suffixed with 4 digit
;                image number.
;
;                If OUTFILE='anyname' then the outfile name suffixed with
;                '.txt' will be used by the text file. 
;
;     NOWIN:     If specified, the xdisplayfile window will not be shown. 
;
; EXAMPLE:
;     Following example reads and plots the 135th image from file 'junk2.image'
;         and generates an ASCII outfile with file name 'junk2.image.0135'.
;         The object v2 need to be defined only if it is not yet defined.
;
;         v2 = obj_new('scan2d',file='junk2.image')
;         v2->View,135
;         v2->datatotext,/OUTFILE
;
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jan 19, 1998.
;	xx-xx-xxxx      comment
;-


w = self.width
h = self.height
data = make_array(w,h)
ln = 1L * w * h
data(0:w-1,0:h-1) = self.image(0:ln-1)
px = self.xarr(0:w-1)
py = self.yarr(0:h-1)
no = self.image_no(self.scanno_current-1) + self.detector
title = '2D Scan # '+string(self.scanno_current) + $
                ',    Image seqno ' + string(no) + ',  Detector ='+ $
		string(self.detector) + ', '

if keyword_set(outfile) then begin
sz = size(outfile)
if sz(n_elements(sz)-2) ne 7 then begin 
suf0 = '0000'
suf = strtrim(no,2)
ln = strlen(suf)
strput,suf0,suf,4-ln
file = self.name+'.'+suf0
endif else file = outfile+'.txt'
end

if n_elements(data) eq 0 then begin
	w_warningtext,'Error: no SDS data available!',60,3
	return
	end


filename = 'view2d_data.txt'
if n_elements(file) ne 0 then filename = file

s = size(data)
no = s(0)
dim = make_array(no)
dim = s(1:no)
type = s(n_elements(s)-2)

T1='' & T2=''
if n_elements(title) ne 0 then T1 = title
if n_elements(unit) ne 0 then T2 = unit 
s1 = '  data('+strtrim(dim(0),2)
for i=1,no-1 do begin
	s1 = s1 + ',' + strtrim(dim(i),2)
end
s1 = s1 + ')'

st = ['; ' + T1 + T2 + s1 ]

; first try data directory
;       then try starting directory
;       then user home directory
;
dir = ''
if self.path ne '' then dir = self.path+'/'
CATCH,error_status
if error_status eq -206 then begin
	if self.path ne '' and self.home ne self.path then $
	dir = self.home+'/' else $
	dir = getenv('HOME')+'/'	
end
openw,fw,dir+filename,/get_lun
printf,fw,st
printf,fw, '; ------------------------------'

;
; BYTE type data
;
	if type eq 1 then begin
	if no eq 1 then begin 
		BytesToStrings,data,outdata,lrecl=80 
		printf,fw,outdata
	endif else begin 
		newdata = string(data)
		for i=0,dim(1)-1 do printf,fw,newdata(i)
	end
	free_lun,fw
	if keyword_set(nowin) then return
	xdisplayfile,dir+filename
;	id = CW_TERM(widget_ids.textdata,filename=filename,/reset)
	return
	end
;
;  other type 
;
if no eq 1 then begin
	f1 = '(I,f17.7)'
	for j=0,dim(0)-1 do begin
	printf,fw,format=f1,j,data(j)
	end
	free_lun,fw
	if keyword_set(nowin) then return
	xdisplayfile,dir+filename
;	id = CW_TERM(widget_ids.textdata,filename=filename,/reset)
	return
end


if no eq 2 then begin
	f0 = '(";              (yvalues)",'+ '5000(f17.7,:))'
	if n_elements(py) gt 0 then printf,fw,format=f0,py
	if n_elements(py) gt 0 then begin
		f1 = '(f17.7,I,'+strtrim(dim(1),2)+'(f17.7))' 
		f0 = '(";                   \ Y",'+strtrim(dim(1),2)+'I17,/,";                  X \",/,";      (xvalues)")'
		endif else begin
		f0 = '(";    \ Y",'+strtrim(dim(1),2)+'I17,/,";   X \",/)'
		f1 = '(I,'+strtrim(dim(1),2)+'(f17.7))' 
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
	xdisplayfile,dir+filename
;	id = CW_TERM(widget_ids.textdata,filename=filename,/reset)
	return
end


if no eq 3 then begin
	f0 = '("J =    ",'+strtrim(dim(1),2)+'I10,/)'
	f1 = '(I,'+strtrim(dim(1),2)+'f17.7)'
	ij=dim(0)*dim(1)
	newdata = make_array(dim(0),dim(1))
	for k=0,dim(2)-1 do begin
	printf,fw,''
	printf,fw,'K = ',strtrim(k+1,2)
	printf,fw,format=f0,indgen(dim(1))
		k1 = ij * k
		k2 = ij - 1 + k1 
	d1=dim(0)-1
	d2=dim(1)-1	
	newdata(0:d1,0:d2)=data(k1:k2)
	new = transpose(newdata)
	d1 = dim(1)
	d2 = dim(0)
	for j=0,d2-1 do begin
	j1=j*d1
	j2 = j1+d1-1	
	x1 = new(j1:j2)
	printf,fw,format=f1,j,x1
	end
	end
	free_lun,fw
	if keyword_set(nowin) then return
	xdisplayfile,dir+filename
;	id = CW_TERM(widget_ids.textdata,filename=filename,/reset)
	return
end

END

PRO scan2d::Open,filename,wid
;+
; NAME:
;	scan2d::Open
;
; PURPOSE:
;       This method opens the 2D image file according to the data binary type.
;       It supports both native binary type and XDR binary type. 
;
;	This method is automatically called by the object creation method.
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]Open, Filename [,Wid]
;
; ARGUMENTS:
;  FILENAME:  Specifies the 2D image file (generated by the data catcher) 
;  WID:       Optional input, specifies the input droplist widget ID to 
;             reflect the binary TYPE in an existing widget program 
;
; KEYWORDS:
;     None.   
;
; EXAMPLE:
;    Following example shows how to explicitly open the 2D image file  with
;    name 'junk2.image', and the file pointer is positioned at the beginning
;    of the file.
;    The object v2 need to be defined only if it is not yet defined.
;
;         v2 = obj_new('scan2d')
;         v2->open, 'junk2.image'
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jan 19, 1998.
;	xx-xx-xxxx      comment
;-

; check for XDR format 

if n_params() eq 0 then begin
	st = ['Usage: obj->Open, Filename','', '  Input image filename is required']
	res = widget_message(st,/Error)
	return
end

if !d.name eq 'X' then begin
	type = 0
	u_openr,unit,filename
	u_read,unit,pvs
	if string(byte(pvs(*,0))) eq '' then begin
	u_close,unit	
	u_openr,unit,filename,/XDR
	type = 1
	end
endif else begin
	u_openr,unit,filename,/XDR
	type = 1
end
	if n_params() eq 2 then $
	WIDGET_CONTROL,wid,set_droplist_select=type

	self.unit = unit
	self.type = type
	self.XDR = self.type
	self.opened = self.unit
END

PRO scan2d::Index,filename
;+
; NAME:
;	scan2d::Index
;
; PURPOSE:
;       This method explicitly reads in the whole 2D image file and creates 
;       the index structure for the image file. It then moves the file pointer 
;       at the beginning of the file. 
; 
;       This method is automatcally called by the obj_new creation if the file
;       keyword is specified on the object creation. 
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]Index, Filename
;
; ARGUMENTS:
;  FILENAME:  Specifies name of the 2D image file (generated by the data catcher) 
;
; KEYWORDS:
;     None.   
;
; EXAMPLE:
;    Following example shows how to open and index the 2D image file  with
;    name 'junk2.image', and the file pointer is positioned at the beginning
;    of the file.
;    The object v2 need to be defined only if it is not yet defined.
;
;         v2 = obj_new('scan2d')
;         v2->index, 'junk2.image'
;
;    The above example is equivalent to the following example.
;
;         v2 = obj_new('scan2d',file='junk2.image')
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jan 19, 1998.
;	xx-xx-xxxx      comment
;-

if n_params() eq 0 then begin
  st = ['Usage: obj->Index, Filename','', '  Input image filename is required']
  res = widget_message(st,/Error)
  return
end
        if self.opened ne 0 then free_lun,self.opened
        self.opened = 0

	; get path
	self.name = filename
	pos = rstrpos(filename,'/')
	if pos gt 0 then begin
		self.path = strmid(filename,0,pos)
		self.name = strmid(filename,pos+1,strlen(filename))
	endif else begin
		self.path = self.home
	end

	self->open,filename
	self->ReadAll
	self->point_lun,0

print,'Selected Image File        : ', filename
print,'Total Number of 2D Scans   : ', self.scanno_2d_last
print,'Total Number of Images     : ', self.maxno

END

PRO scan2d::Read,view=view,width=width,height=height,detector=detector,scanno_2d=scanno_2d,x=x,y=y,im=im
;+
; NAME:
;	scan2d::Read
;
; PURPOSE:
;       This method reads in one set of image record from the current file
;       pointer.
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]Read, /VIEW, WIDTH=width, HEIGHT=height, $
;          DETECTOR=detector, SCANNO_2D=scanno_2d, X=x, Y=y, IM=im
;
; ARGUMENTS:
;     None.
;
; KEYWORDS:
;     VIEW:     If specified, the read in 2D image will also be displayed on the
;               plot window.   
;     X:        If specified, it returns the X vector values.
;     Y:        If specified, it returns the Y vector values.
;     IM:       If specified, it returns the 2D IM array values.
;     WIDTH:    It specified, it returns the X vector size.
;     HEIGHT:   It specified, it returns the Y vector size.
;     SCANNO_2D:If specified, it returns the corresponding 2D scan number.
;     DETECTOR: If specified, it returns the detector number for the image.
;
; EXAMPLE:
;    Following example reads the 136th image from the input image file and also
;    plot the 2D image. 
;    The object v2 need to be defined only if it is not yet defined.
;
;         v2 = obj_new('scan2d',file='junk2.image')
;         v2->point_lun,135
;         v2->read,/view
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jan 19, 1998.
;	xx-xx-xxxx      comment
;-

unit = self.unit
        IF EOF(unit) THEN RETURN
 
        ; read pv names
        u_read,unit,pvs
        pvs = string(pvs)
        self.x_pv = pvs(0)
        self.y_pv = pvs(1)
        self.file_1d = pvs(2)
        if n_elements(pvs) gt 3 then begin
        self.x_desc = pvs(3)
        self.y_desc = pvs(4)
        self.z_desc = pvs(5)
        end
 
        ; read seqno, dims
 
        u_read,unit,x
;       print,x
       width = x(1)
       height= x(2)
       scanno_2d = x(4)
       detector = x(3) + 1
        self.scanno = x(0)
        self.width = x(1)
        self.height = x(2)
        self.detector = x(3) + 1
        self.scanno_current = x(4)
        self.y_req_npts = x(5)
 
        ; read x and y position array
 
        u_read,unit,x
        self.xarr = x
        u_read,unit,y
        self.yarr = y
 
        ; read image
 
        u_read,unit,im
        self.image = im
 
;       newImage = im(0:self.width-1, 0:self.height-1)
        newImage = im
        s = size(newImage)
 
        if s(0) ne 2 then begin
              w_warningtext,'Warning: data is not 2D image ',60,5,'VIEW2D Messaes'
                end
 
	seqno = self.image_no(self.scanno_current-1)+self.detector-1
        if keyword_set(view) then self->View,seqno,/noread
END

PRO scan2d::Print
;+
; NAME:
;	scan2d::Print
;
; PURPOSE:
;       This method prints the current key variables for the object structure. 
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]Print
;
; ARGUMENTS:
;     None.
;
; KEYWORDS:
;     None.
;
; EXAMPLE:
;    Following example shows current key variables of the object v2. 
;
;         v2->print
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jan 19, 1998.
;	xx-xx-xxxx      comment
;-

print,'Starting Directory         : ', self.home
if strlen(self.path) gt 1 then $
print,'Data Directory             : ', self.path
print,'Selected Image File        : ', self.name
print,'Image Binary Type          : ', self.type
print,'Opened Logical Unit        : ', self.unit
print,'Total Number of 2D Scans   : ', self.scanno_2d_last
print,'Total Number of Images     : ', self.maxno
print,'Scan seq image_no of detector 1 : '
print,self.image_no(0:self.scanno_2d_last)
print,'Current 2D Scan #   :',self.scanno_current
print,'Current detector #  :',self.detector
if self.scanno_current gt 0 then $
print,'Current Image seqno :',self.image_no(self.scanno_current-1)+self.detector
END


PRO scan2d::Save,no,file=file,verbose=verbose,object=object
;+
; NAME:
;	scan2d::Save
;
; PURPOSE:
;       This method allows the user read the next image record and save the
;       data to a save file. The saved file can be restored by the restore
;       command. Two type of variables can be saved by this command: 
;       either save the whole object or just save the x,y,im arrays.
;       The data saved will be in XDR format.
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]Save, No, FILE=file, /VERBOSE, /OBJECT
;
; ARGUMENTS:
;     No       Specifies the desired sequence number of the image record 
;              to be read. If not specified then the next record is assumed.
;
; KEYWORDS:
;     FILE:    Specifies the save file name, it defaults to 'idlsave.dat'.
;     VERBOSE: If specified, the verbose mode is assumed, the variables
;              saved will be listed.
;     OBJECT:  If specified, the complete data object is saved. Otherwise
;              only the NO, X, Y, IM variables are saved for the record.
;
; EXAMPLE:
;    Example 1 - Saves the X,Y,IM arrays for the 135th record 
;    in 'junk2.image.s0135', also list the variables saved.
;    The object v2 need to be defined only if it is not yet defined.
;
;         v2 = obj_new('scan2d',file='junk2.image')
;         v2->save,135,file='junk2.image.s0135',/verbose
;
;    Example 2 - Saves all the variables for the 135th record 
;    in 'junk2.image.s0135', also list the variables saved
;
;         v2->save,135,file='junk2.image.s0135',/verbose,/object
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jan 19, 1998.
;	xx-xx-xxxx      comment
;-


	filename = 'idlsave.dat'
	if keyword_set(file) then filename = file

	unit = self.unit
	if n_params() then begin
		self->Valid,no,code=code
		if code then return
		self->point_lun,no-1
	end
	self->Read
	w = self.width
	h = self.height
	im = make_array(w,h)
	ln = 1L * w * h
	im(*,*) = self.image(0:ln-1)
	x = self.xarr(0:w-1)
	y = self.yarr(0:h-1)

;   save the complete self object

	if keyword_set(object) then begin
	save,filename=filename,/XDR,/verbose
	endif else begin

;   save only image no, xarr, yarr, and image array 
		if keyword_set(verbose) then $
		save,no,x,y,im,filename=filename,/XDR,/verbose else $
		save,no,x,y,im,filename=filename,/XDR
	end
END

PRO scan2d::Valid,no,code=code
;+
; NAME:
;	scan2d::Valid
;
; PURPOSE:
;       This method checks whether the input image no is valid.
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]Valid,No,CODE=code
;
; ARGUMENTS:
;     No       Specifies the image record number. 
;
; KEYWORDS:
;     CODE:    Returns the return code, 0 if valid, -1 if invalid.
;
; EXAMPLE:
;     Following example checks whether image number 130 is a valid request
;     for the 'junk2.image' file
;     The object v2 need to be defined only if it is not yet defined.
;
;         v2 = obj_new('scan2d',file='junk2.image')
;         v2->Valid,130,code=code
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jan 19, 1998.
;	xx-xx-xxxx      comment
;-

code = 0
	if no lt 1 or no gt self.maxno then begin
	st = ['Valid Image Range [  1 : '+strtrim(self.maxno,2) +' ]', $
		'',	'Input out of range!!']
	res = widget_message(st,/Info)
	code = -1
	return
	end
END

PRO scan2d::View,no,scanno=scanno,detector=detector,noread=noread
;+
; NAME:
;	scan2d::View
;
; PURPOSE:
;       This method lets the user view any valid 2D image from the 2D 
;       image file.
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]View, No, SCANNO=scanno, DETECTOR=detector, NOREAD=noread
;
; ARGUMENTS:
;     No       Specifies the sequence number of the image record. 
;
; KEYWORDS:
;     SCANNO:    It specifies the 2D scan number. If specified,it ignore the 
;                'No' specification. Valid SCANNO range:
;                [1 - self.scanno_2d_last] 
;     DETECTOR:  It specifies the desired detector number from the 2D scan.
;                If not specified, and SCANNO is given, detector 1 is assumed.
;     NOREAD:    If specified, no reading from the file pointer is done, 
;                only plot the current object is performed.
;
; EXAMPLE:
;     Example 1 reads and plots the 135th image from the 'junk2.image'
;         The object v2 need to be defined only if it is not yet defined.
;
;         v2 = obj_new('scan2d',file='junk2.image')
;         v2->View,135
;
;     Example 2 reads and plots the image of detector 7 of the 21th 2D scan
;         from the 'junk2.image' file
;
;         v2->View,scanno=21,detector=7
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jan 19, 1998.
;	xx-xx-xxxx      comment
;-

if keyword_set(noread) then goto,plotonly 
	unit = self.unit
	print,'Read unit:',unit
	if n_params() then begin
		self->Valid,no,code=code
		if code then return
		self->point_lun,no-1
	end

	if n_elements(scanno) then begin
		if scanno lt 1 or scanno gt self.scanno_2d_last then begin
			res = widget_message('2D scanno out of range',/Error)
			return
		end
		no = self.image_no(scanno-1)
		if keyword_set(detector) then no = no + detector else no = no+1
		self->point_lun,no-1
	end

	self->Read

plotonly:

if self.scanno_current gt 0 then $
no = self.image_no(self.scanno_current-1)+self.detector

	w = self.width
	h = self.height
	im = make_array(w,h)
	ln = 1L * w*h
	im(*,*) = self.image(0:ln-1)

window,0,xsize=500,ysize=500,title='scan2d Object'

	ncolors = !d.table_size
	header_note1 = '2D Scan # '+string(self.scanno_current) + $
		',    Image seqno ' + string(no) 
	header_note = 'Image( '+strtrim(w,2)+' , '+  strtrim(h,2)+') '
          xdis = 0.001 * !d.x_size
          ydis = !d.y_size - 1.2 * !d.y_ch_size
	  xyouts,xdis,ydis,header_note1,/device,color=ncolors-1

          ydis = !d.y_size - 2.2 * !d.y_ch_size
          xyouts,xdis,ydis,header_note,/device,color=ncolors-1
	xrange=[self.xarr(0), self.xarr(w-1)]
	yrange=[self.yarr(0), self.yarr(h-1)]

	TVSCL,congrid(im,!d.x_size -100,!d.y_size-100),50,50

	plot,xrange=xrange,yrange=yrange,[-1,-1],/noerase, $
		pos=[50./!d.x_size, 50./!d.y_size, $
		 (!d.x_size-50.)/!d.x_size, (!d.y_size-50.)/!d.y_size], $
		xstyle=1, ystyle=1, xtitle=self.x_desc, ytitle=self.y_desc, $
		title=self.z_desc + 'D'+ strtrim(self.detector,2)

END

PRO scan2d::ReadAll
; populate the 2D image index

unit = self.unit

	seqno = 0
	id = 0
	self.fptr = make_array(10000,/long)

	point_lun, unit, 0 

	self.seqno = 0
        self.scanno_2d = 0
        self.scanno_2d_last = 0
	WHILE NOT  EOF(unit) DO BEGIN
	id = id + 1
		u_read,unit,pvs
		u_read,unit,nos
		u_read,unit,x
		u_read,unit,y
		u_read,unit,image
		point_lun,-unit,pos
		self.fptr(id) = pos
		scanno_2d = nos(4)
		detector = nos(3) + 1

	;  check for scan # increment

		if self.scanno_2d_last lt scanno_2d then begin
			self.scanno_2d_last = scanno_2d
			self.image_no(scanno_2d-1) = self.seqno 
			end

	; check for scan # out of sync case ???

		if scanno_2d lt self.scanno_2d_last and detector eq 1 then begin 
			self.scanno_2d_last = self.scanno_2d_last + 1 
			self.image_no(self.scanno_2d_last-1) = self.seqno 
			end

		self.seqno = self.seqno + 1 

	if detector gt 1 then self.image_no(self.scanno_2d_last) = self.seqno


	END

	maxno = id
	self.maxno = maxno
	self.seqno = maxno-1
	self.image_no(self.scanno_2d_last) = maxno 

;	point_lun, unit, 0 

END

PRO scan2d::Point_lun,seqno
;+
; NAME:
;	scan2d::Point_lun
;
; PURPOSE:
;       The method moves the 2D image file pointer to the end of the 
;       specified record.
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]Point_lun, SEQNO
;
; ARGUMENTS:
;     SEQNO:   Specifies the zero based image sequence number.
;
; KEYWORDS:
;     None.   
;
; EXAMPLE:
;    Following example moves the file pointer to the end of the 10th record
;    of the 2D image file 'junk2.image'.
;    The object v2 need to be defined only if it is not yet defined.
;
;         v2 = obj_new('scan2d',file='junk2.image')
;         v2->point_lun,10
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jan 19, 1998.
;	xx-xx-xxxx      comment
;-

        if seqno ge 0 and seqno le self.maxno then begin
        point_lun,self.unit,self.fptr(seqno)
        end
END

FUNCTION scan2d::Init,file=file
; populate the index object if file is specified
loadct,39
        if keyword_set(file) then self->Index,file
;        self->point_lun,0
	cd,current=h
	self.home = h
        return,1
END
 
PRO scan2d__define
 
;  catch2d_file = {                $
struct = { scan2d, $
	unit	: -1, $
	type	: 0, $
	XDR     : 0, $  ; default pure binary
	version : '', $
	home    : '', $
	path    : '', $
        name    : 'catch1d.trashcan.image',           $
        opened  : 0,            $
	fptr	: make_array(10000,/long), $
	pan     : 0, $ ; pan all detectors or not
        win     : -1, $; pan window # $
;       mode    : 0,            $   ; 0 - scan, 1 - view
        seqno   : 0, $		; image seq #
        maxno   : 0, $ 		; total # of images found
	x_pv	: '', $
	y_pv	: '', $
	x_desc	: '', $		; 1D positioner desc
	y_desc	: '', $		; 2D posititoner desc
	z_desc	: '', $		; detector desc
	file_1d	: '', $
	scanno_2d : 0, $  	; current readin 2d scan #
	scanno_current : 0, $  	; current viewed 2d scan #
	scanno_2d_last : 0, $  	; last 2d scan # from opened file
	image_no  : make_array(1000,/int), $ ; detector 1 image no for scanno_2d #
	scanno  : 0, $  	; current image at the end 1d scan #
	width   : 100, $  	; current image width 
	height  : 100, $  	; current image height
	detector : 0, $  	; current image detector index (15 detectors)
	x_act_npts :  100, $
	y_act_npts :  100, $
	y_req_npts :  100, $
	x_mag : 1., $ 	; x image mag facctor
	y_mag : 1., $ 	; y image mag facctor
;	xprof :  1, $	; window 1
;	yprof :  3, $	; window 3
	xprof : -1, $   ; xz profile window 
	yprof : -1, $   ; yz profile window
	xzdraw : 0, $   ; xz draw area
	yzdraw : 0, $   ; yz draw area
	xzline_x :  [0.,0.],$
	xzline_z :  [0.,0.],$
	xzline_xo : [0.,0.],$
	xzline_zo : [0.,0.],$
	yzline_y :  [0.,0.],$
	yzline_z :  [0.,0.],$
	yzline_yo : [0.,0.],$
	yzline_zo : [0.,0.],$
	s_wid   : -1, $       ; window id for saved pixmap
	xarr	: make_array(1000), $
	yarr	: make_array(1000), $
	image	: make_array(1000,1000,/float) $
        }
END 
