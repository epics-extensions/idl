;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
;
; scan2d__define.pro
;

@u_read.pro
@scan_colorbar.pro
@scan2d_overlay.pro
@scan2d_convert.pro
@scan2d_roi.pro

PRO scan2d::ROIpick,no,picke

	if n_elements(no) eq 0 then no = self.seqno
	 self->point_lun,no-1
	 self->read,im=im,x=x,y=y,/view
	roifile = self.outpath+self.name+'_'
	multiroi_pick,im,class=roifile
END


PRO fileSeqString,no,suf0
	suf0 = '0000'
	suf = strtrim(no,2)
	ln = strlen(suf)
	strput,suf0,suf,4-ln
	if no gt 9999 then suf0=suf
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




PRO scan2d::NewPos,pos_id, outfile=outfile
;+
; NAME:
;      	scan2d::NEWPOS
;
; PURPOSE:
; 	This method allows the user generate a new 2D image file with
;       new postioner selected instead of default positioner P1
;
; ARGUMENTS:
;    POS_ID:  New positioner # selected (must > 1 and < 5)
;             default is positioner 2.
;
; KEYWORD:
;    OUTFILE: Specifies the output image file, default is the
;             temporary file name 'tmp.image' is used.
;
; RESTRICTION:
;    The positioner id entered must be defined for the scan1 record. 
;
; EXAMPLES
;    In this example the orignal 1D and 2D scan data are stored in 'junk2' 
;    and 'junk2.image'. The purpose is to create a new image file with
;    positioner2 values as the X vector, the new image file will be saved
;    as 'junk2.image_2'. The v3 object is used to varify the new image file.
;
; 	v2 = obj_new('scan2d',file='junk2.image')
;       v2->newPos,2
;
;       v3 = obj_new('scan2d',file='junk2.image_2')
;       v3->view,1
;-

	if n_elements(pos_id) eq 0 then pos_id=2 
	if pos_id lt 2 then return
	
	len = strpos(self.name,'.image')
	if len gt 0 then file1 = strmid(self.name,0,len)
	if self.path ne '' then file1 = self.path+!os.file_sep+ file1

	v1 = obj_new('scan1d',file=file1)

	if obj_valid(v1) eq 0 then begin
	str = ['Usage: v2->newPos, pos_id, outfile="new.image"', $
		'where', $
		'V2 must be an "scan2d" object', $
		'pos_id  specifies position #', $
		'** 1D scan file not found **']
	r = dialog_message(str,/error)

		return
	end

	nfile = 'tmp.image'  ;self.name+'_2'
	if keyword_set(outfile) then nfile=outfile
	
	u_openw,unit2,nfile,/XDR

	unit = self.unit
	self->point_lun,0

	FOR seqno = 1,self.maxno DO BEGIN
	u_read,unit,pvs
	u_read,unit,nos
	u_read,unit,x
	u_read,unit,y
	u_read,unit,im

	scan1d = nos(0)-nos(2)
	v1->read,scan1d,pa=pa,da=da,np=np,nd=nd,x_names=x_names, $
		x_descs=x_descs,x_engus=x_engus

	; output new image file
	sz = size(pa)
	if sz(0) eq 1 then pos_id = 1
	if sz(0) eq 2 and pos_id gt sz(2) then begin
		x = indgen(sz(1))
		xlabel = 'P'+strtrim(pos_id,2)
	endif else begin
		xlabel = x_descs(pos_id-1) +'(' + x_engus(pos_id-1) +')'
		x = pa(*,pos_id-1)
	end
	pvs(*,3) = 0 
	pvs(0,3) = byte(xlabel) 

	u_write,unit2,pvs
	u_write,unit2,nos
	u_write,unit2,x
	u_write,unit2,y
	u_write,unit2,im
	END

	u_close,unit2
	obj_destroy,v1

	u_close,unit
	if keyword_set(outfile) then return

	str = !os.mv + ' ' + nfile +' '+self.path
	spawn,str

END

PRO scan2d::ASCII1D,lineno,endno=endno,all=all,nowin=nowin,format=format,group=group
;+
; NAME:
;	scan2d::ASCII1D
;
; PURPOSE:
;       This method allows the user to extract all detectors data 
;       from a 2D scan. The name convention of ASCII file will be the 
;       image file suffixed with its corresponding 1D scan sequence number.
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]ASCII1D [,Lineno] [,Endno=endno] [,/ALL] $
;                  [,/NOWIN]  [,FORMAT=format] [,GROUP=group]
;
; ARGUMENTS:
;     Lineno:    Specifies the 1D line scan # to be extracted from a 
;                given 2D scan.
;
; KEYWORDS:
;     ENDNO:     If specified, ASCII files from the 1D scan lineno to the
;                endno will be created
;     ALL:       If specified, ASCII files for all 1D scan lines from a 
;                given 2D scan will be created.
;     NOWIN:     If specified, only the ASCII files will be created but the
;                xdisplayfile window will not be shown. 
;     FORMAT:    If specified, override the default output format
;     GROUP:     Specifies the parent group ID, the destroy of parent window 
;                results the destroy of this child window
;
; EXAMPLE:
;     Example 1 extracts the 10th scan line from the current 2D scan. 
;     Example 2 generates all the 1D scan data file without displaying window. 
;     Example 3 extracts the 1D scan 3 to 5 from the current 2D scan.
;
;         v2->ASCII1D,10
;         v2->ASCII1D,/ALL,/NOWIN
;         v2->ASCII1D,3,endno=5
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Feb 18, 2000.
;       xx-xx-xxxx  xxx comment   
;-


scanno = self.scanno_current

	self->Images,scanno,image_array,def,vmax,vmin,zdesc=zdesc,panimage=panimage,print=print

	if n_elements(lineno) eq 0 then lineno=1
	if lineno gt self.height then begin
		r = dialog_message(['v->ascii1d, lineno', $
			'Lineno can not exceeds '+string(self.height)],/error)
		return
	end
w = self.width
h = self.height
px = self.xarr(0:w-1)
py = self.yarr(0:h-1)

	fmt = 'F18.8'
	ifmt = strmid(fmt,1,strpos(fmt,'.')-1)
	wd = fix(ifmt)
	nd = ceil(total(def))
	st0 = string(replicate(32b,(2+nd)*wd))
	
	sthd = st0
	sthd2 = st0
	strput,sthd,';',0
	strput,sthd,'I',6
	strput,sthd,'P1',wd
	strput,sthd2,';',0
	strput,sthd2,self.x_desc,wd
	ip = 10 + wd
	ji = 0
	for j=0,nd-1 do begin
		if def(j) gt 0 then begin
		dip = ip + ji*wd
		st1 = 'D'+strtrim(j+1,2)
		st2 = zdesc(j)
		strput,sthd,st1,dip
		strput,sthd2,st2,dip
		ji = ji+ 1
		end
	end


	dir = self.outpath + 'ASCII' + !os.file_sep

	startno = lineno
	if n_elements(endno) eq 0 then endno = lineno
	if keyword_set(all) then begin
		startno=1
		endno = self.height
	end

	for lineno = startno,endno do begin

	seqno = self.scanno-self.height+lineno
	fileSeqString,seqno,suf0

	outfile = dir+self.name+'.S'+suf0
	openw,unit,outfile,/get_lun
	printf,unit,'; 1D ASCII detector data extracted from the image arrays'
	printf,unit,'; 2D Scan # :',strtrim(scanno,2), $
		',    width='+strtrim(w,2), $
		',    height='+strtrim(h,2), $
		',    1D Scan Line # :',lineno
	printf,unit,'; 1D scan Line #  @ y(',strtrim(lineno-1,2),') = ',py(lineno-1)
	printf,unit,'; Equivalent 1D Scan # :',seqno
	printf,unit,sthd
	printf,unit,sthd2

	for i=0,w-1 do begin
	st = st0
	strput,st,string(i),0
	strput,st,strtrim(px(i),2),10
	ip = 10 + wd
	ji = 0
	for j=0,n_elements(def)-1 do begin
		if def(j) gt 0 then begin
		dip = ip + wd*ji
		st1 = strtrim(image_array(i,lineno-1,j),2)
		strput,st,st1,dip
		ji = ji+1
		end
	end
	printf,unit,st
	end
	free_lun,unit
	close,unit
	
	if keyword_set(nowin) eq 0 then $
	xdisplayfile,outfile,group=group

	end
END

PRO scan2d::DetListString,detno=detno,list
; return the list strin
; if detno specified find from every scan
; if nothing found return ''
;
	i = self.scanno_current
	if i lt 1 then i = 1
	list = ''

	if keyword_set(detno) then begin
	if detno lt 1 then detno = 1
	i1 = 1
	i2 = self.scanno_2d_last
	for i=i1,i2 do begin
		no = self.image_no(i-1)+detno
		if no le self.image_no(i) then begin
			if i eq 1 then list=strtrim(no,2) else $
			list = list+','+strtrim(no,2)
		end
	end
	endif else $
	list = strtrim(self.image_no(i-1)+1,2)+'-'+strtrim(self.image_no(i),2)	

END


PRO scan2d::ASCII2D,detno,pick=pick,all=all,whole=whole,nowin=nowin,format=format,group=group
;+
; NAME:
;	scan2d::ASCII2D
;
; PURPOSE:
;       This method allows the user to create the 2D ASCII data file(s)
;       for the selected detector number. The ASCII file created will be
;       automatically suffixed with its 4 digit image sequence number and
;       '.txt' 
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]ASCII2D, Detno [,/ALL] [,/NOWIN] [,FORMAT=format]
;                             [,/WHOLE] [,GROUP=group]
;
; ARGUMENTS:
;     Detno:     Specify the detector number desired for current opened
;                2D scan, default 1.
;
; KEYWORDS:
;     WHOLE:     If specified, pick all detectors from the current 2D scan #
;     ALL:       If specified, pick all images from the 2D image file
;     PICK:      If specified, pick the same detno out of every 2D scan
;     NOWIN:     If specified, only the ASCII files will be created but the
;                xdisplayfile window will not be shown. 
;     FORMAT:    If specified, override the default output format
;     GROUP:     Specifies the parent group ID, the destroy of parent window 
;                results the destroy of this child window
;
; EXAMPLE:
;     Example 1 generates ASCII report for detector 5 for the current 2D scan #
;
;               v2->ascii2d,5
;
;     Example 2 generates ASCII report for detector 5 for every 2D scan 
;       contained in the image file.
;
;               v2->ascii2d,5,/PICK
;
;-
	begin_no = 1
	end_no = self.scanno_2d_last

	detector = 1
	if n_elements(detno) then detector = detno

	if keyword_set(pick) then begin
		self->detliststring,detno=detector,lists
		if lists eq '' then begin
			r = dialog_message('Detector'+strtrim(detector,2)+' not found!',/Info)
			return
		end
		self->ascii,lists,format=format,nowin=nowin,GROUP=group
		return
	end

	if keyword_set(all) then begin
		for i=begin_no,end_no do begin
		for j=self.image_no(i-1),self.image_no(i)-1 do begin
			list = j+1
			self->ascii,list,format=format,nowin=nowin,GROUP=group
		end
		end
		return
	end

	if keyword_set(whole) then self->detliststring,list 
	self->ascii,list,format=format,nowin=nowin,GROUP=group
	
END


PRO scan2d::ASCII,list,nowin=nowin,format=format,GROUP=group
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
;       Obj->[scan2d::]ASCII, List, /NOWIN  [,FORMAT=format]
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
;     FORMAT:    If specified, override the default output format
;     GROUP:     Specifies the parent group ID, the destroy of parent window 
;                results the destroy of this child window
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
;       01-21-2000      Use current image # of the defined object 
;-


if n_elements(list) lt 1 then  list = self.seqno

; parse the string list

sz = size(list)
if sz(n_elements(sz)-2) eq 7 then begin
sep1=','
if strpos(list,sep1) lt 0 then sep1=' '
if strpos(list,':') gt 0 then parse_num,string(list),res,sep1=sep1,sep2=':' else $
parse_num,string(list),res,sep1=sep1
print,'Image #:',res
list = res
end

; integer list


	for i=0,n_elements(list)-1 do begin
	self->point_lun,list(i)-1
	self->read
	if keyword_set(nowin) then self->datatotext,/outfile,/nowin,Group=group else $
	self->datatotext,/outfile,Group=group
	end

END


;
;   bind two 2D scans into one 2D scan images
;

PRO scan2d::bindImage,file1=file1,file2=file2, outfile=outfile, h1s=h1s, h2s=h2s, h1e=h1e, h2e=h2e, im1=im1, im2=im2, s1=s1,s2=s2
;+
; NAME:
;	scan2d::bindImage
;
; PURPOSE:
;       This method combines images from two different 2D scan object and
;       create a new 2D image and saved the combined images into a new file.
;       The output file inheritates the scan number and detector number 
;       from the FILE1 for the combined images.
;
;       If the 2D image file1 and file2 are same then 2 scans from the same 
;       file is combined into one scan.
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]bindImage,File1=file1, im1=im1, 
;                  File2=file2, im2=im2, outfile='...'
;
; ARGUMENTS:
;    None.
;
; KEYWORDS:
;  OUTFILE:   Specifies the output file for combined 2D images
;  FILE1:     Specifies the 2D image file1 
;  Im1:       Specifies the starting image seq # of the first scan 
;  Im2:       Specifies the starting image seq # of the second scan
;  FILE2:     If specified, the Im2 is from a different 2D image file2 
;  S1:        If specified, it returns the 2D scan # for Im1 
;  S2:        If specified, it returns the 2D scan # for Im2 
;  h1s:       Optional, specifies the y1 start index, defaults 0
;  h1e:       Optional, specifies the y1 end index, defaults height
;  h2s:       Optional, specifies the y2 start index, defaults 0
;  h2e:       Optional, specifies the y2 end index, defaults height
;
; RESTRICTION:
;  The number of detectors must be exactly same in both files.
;  The obj variable names internally used by this routine are
;  cv1,cv2,cv3, a user have to avoid to use these name.
;
; EXAMPLE:
;    Following example binds the images of 2D # 31 with
;    images of 2D scan # 33 from the same file. The starting image # 
;    of scan # 31 is 205. The starting image # of scan # 33 is 217. 
;    We want to bind these two scans into one new 2D scan. The new 2D 
;    scan for each detector will be saved in 'new.image'. 
;    The new scan will inherit the scan description from the 2D scan # 31.
;
;    The object v2 need to be defined only if it is not yet defined.
;
;    filename1='/home/sricat/CHA/user/s2idd/15nov98_data.01.image' 
;
;    v2 = obj_new('scan2d',file=filename1)
;    v2->view,205,/noplot
;    v2->point_lun,204
;    v2->bindimage,file1=filename1, im1=205, im2=217, $
;	 	outfile='new.image'
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, April 16, 1999
;	xx-xx-xxxx      comment
;-

if  keyword_set(file1) * keyword_set(im1) *keyword_set(im2)* keyword_set(outfile) eq 0 then begin
	print,"Usage: Obj->bindImage,file1='...',im1=im1, $"
	print," 	file2='...',im2=im2,outfile='new.image'"
	return
end
 
image1=0
image2=0

if keyword_set(outfile) then newname=strtrim(outfile,2)
if keyword_set(file1) then filename1 = file1
filename2=file1
if keyword_set(file2) then filename2 = file2
if keyword_set(IM1) then image1=im1-1
if keyword_set(IM2) then image2=im2-1
if keyword_set(h1s) then y1s=h1s
if keyword_set(h1e) then y1e=h1e
cv1 = obj_new('scan2d',file=filename1)
cv2 = obj_new('scan2d',file=filename2)

cv1->point_lun,image1
cv1->read,scanno_2d=scan1
cv1->panimage,scan1
ij1 = cv1.image_no(scan1-1)
t_det = cv1.image_no(scan1) - ij1
if t_det gt 0 and t_det lt 15 then numdetector = t_det-1

cv2->point_lun,image2
cv2->read,scanno_2d=scan2
cv2->panimage,scan2
ij2=cv2.image_no(scan2-1)
s1=scan1
s2=scan2

det1=0
det2=numdetector

for i=det1,det2 do begin

	cv1->point_lun,ij1 + i
	cv1->read,width=w1,height=h1,detector=d1,x=x1,y=y1,im=im1 
	cv2->point_lun,ij2 + i
	cv2->read,width=w2,height=h2,detector=d2,x=x2,y=y2,im=im2 

	h11 = 0
	h12 = h1-1
	if keyword_set(y1s) then h11=y1s-1
	if keyword_set(y1e) then h12=y1e-1
	if h11 lt 0 then h11=0
	if h12 ge h1 then h12=h1-1

	h21 = 0
	h22 = h2-1
	if keyword_set(y2s) then h21=y2s-1
	if keyword_set(y2e) then h22=y2e-1
	if h21 lt 0 then h21=0
	if h22 ge h2 then h22=h1-1
	
	th1 = h12-h11+1
	th2 = h22-h21+1
	w3 = w1
	h3 = th1+th2
	im3 = make_array(w1,h3)
	y3 = [y1(h11:h12),y2(h21:h22)] 
	im3(0,0) = im1(*,h11:h12) 
	im3(0,th1) = im2(*,h21:h22) 
	x3 = x1
	;print,total(im3(*,h12)-im1(*,h12))

	self->read

	pvs = make_array(60,6,/byte)
	pvs(0,0) = byte(self.x_pv)
	pvs(0,1) = byte(self.y_pv)
	pvs(0,2) = byte(newname)
	pvs(0,3) = byte(self.x_desc)
	pvs(0,4) = byte(self.y_desc)
	pvs(0,5) = byte(self.z_desc)

	const = make_array(6,/int)
	const(0) = self.scanno
	const(1) = w3
	const(2) = h3
	const(3) = self.detector - 1
	const(4) = self.scanno_current
	const(5) = h3

	self.width = w3
	self.height = h3
	self.y_req_npts = h3
	self.xarr = x3
	self.yarr = y3
	self.image = im3

;	window,1,xsize=500,ysize=500
;	tvscl,congrid(im3,400,400),50,50

	if i eq 0 then u_openw,unit,newname else $
	u_openw,unit,newname,/append
		u_write,unit,pvs
		u_write,unit,const
		u_write,unit,x3
		u_write,unit,y3
		u_write,unit,im3
		u_close,unit
end

free_lun,cv1.unit
free_lun,cv2.unit
obj_destroy,cv1
obj_destroy,cv2

	cv3 = obj_new('scan2d',file=newname)
	cv3->read
	cv3->point_lun,0
	cv3->panimage
	free_lun,cv3.unit
	obj_destroy,cv3
END

PRO scan2d::dataToText,data,px,py,title=title,unit=unit,outfile=outfile,nowin=nowin,format=format,GROUP=group
;+
; NAME:
;	scan2d::DataToText
;
; PURPOSE:
;       For the current scan2d object, this method creates its tabulated 2D 
;       image data in a disk file and uses the xdisplayfile command to 
;       show the contents of the created ASCII file.
;
;       If the outfile is not specified it will try to create the ASCII 
;       file in the following order: try the data directory first, 
;       if failed then try the user starting directory, if still failed 
;       then try the  user home directory.
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
;                image number plus '.txt'.
;
;                If OUTFILE='anyname' then the outfile name suffixed with
;                '.txt' will be used by the text file. 
;
;     NOWIN:     If specified, the xdisplayfile window will not be shown. 
;     FORMAT:    If specified, override the default F17.7 format. 
;     GROUP:     Specifies the parent group widget ID.
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
;      05-15-1998  bkc  Catch error for openw
;      05-11-1998  bkc  Add fileSeqString routine, add suffix '.txt' to seqno
;                       If outfile specified, no directory check will be done.
;      02-17-2000  bkc  Add format keyword, default suffix use scan # and
;                       detector#  '.S#D#' 
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
suf0 = ''
fileSeqString,no,suf0
file = self.name+'.'+ suf0+ '.txt'
endif else file = outfile+ '.txt'
end

if n_elements(data) eq 0 then begin
	res = dialog_message('Error: no SDS data available!',/info)
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
s1 = ';    data('+strtrim(dim(0),2)
for i=1,no-1 do begin
	s1 = s1 + ',' + strtrim(dim(i),2)
end
s1 = s1 + ')'

st = ['; ' + T1 + T2 ]

r = strpos(filename,!os.file_sep)
if r lt 0 then begin

dir = self.outpath + 'ASCII' + !os.file_sep
found = findfile(dir,count=ct)
if ct eq 0 then spawn,!os.mkdir + ' '+dir

report = dir+filename
endif else report = filename

openw,fw,report,/get_lun

printf,fw,st
printf,fw,s1
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

	fmt = 'F17.7'
	if keyword_set(format) then fmt = format
	ifmt = 'I'+strmid(fmt,1,strpos(fmt,'.')-1)

if no eq 1 then begin
	f1 = '(I,'+fmt+')'      ; f1 = '(I,'+fmt+')'
	for j=0,dim(0)-1 do begin
	printf,fw,format=f1,j,data(j)
	end
	free_lun,fw
	if keyword_set(nowin) then return
	xdisplayfile,report    ;dir+filename
;	id = CW_TERM(widget_ids.textdata,filename=filename,/reset)
	return
end


if no eq 2 then begin
	f0 = '(";              (yvalues)",'+ '5000('+fmt+',:))' 
	if n_elements(py) gt 0 then printf,fw,format=f0,py
	if n_elements(py) gt 0 then begin
		f1 = '('+fmt+',I,'+strtrim(dim(1),2)+'('+fmt+'))' 
		f0 = '(";                   \ Y",'+strtrim(dim(1),2)+ifmt+',/,";                  X \",/,";      (xvalues)")'
		endif else begin
		f0 = '(";    \ Y",'+strtrim(dim(1),2)+ifmt+',/,";   X \",/)'
		f1 = '(I,'+strtrim(dim(1),2)+'('+fmt+'))' 
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
	xdisplayfile,report    ;dir+filename
;	id = CW_TERM(widget_ids.textdata,filename=filename,/reset)
	return
end


if no eq 3 then begin
	f0 = '("J =    ",'+strtrim(dim(1),2)+'I10,/)'
	f1 = '(I,'+strtrim(dim(1),2)+''+fmt+')'
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
	xdisplayfile,report    ;dir+filename
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

	; get path
	self.name = filename
	pos = rstrpos(filename, !os.file_sep)
	if pos gt 0 then begin
		self.path = strmid(filename,0,pos)
		self.name = strmid(filename,pos+1,strlen(filename))
	endif else begin
		self.path = self.home
	end
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

	self->open,filename
	self->ReadAll
	self->point_lun,0

print,'Selected Image File        : ', filename
print,'Total Number of 2D Scans   : ', self.scanno_2d_last
print,'Total Number of Images     : ', self.maxno

END

PRO scan2d::Next,seqno,filename,error=error
;+
; NAME:
;       scan2d::Next
;
; PURPOSE:
;       This method points to the next set of 2D scan images or to the 
;       specified 2D scan seqno set of 2D images.
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]Next [,Seqno] [,Filename] [,ERROR=error]
;
; ARGUMENTS:
;     SEQNO     - jumps to the specified 2D scan # if it is specified.
;     FILENAME  - returns the opened filename
;
; KEYWORD:
;     ERROR     - returns the error code, non-zero if error found
;
; EXAMPLE:
;    Example points to the next set of 2D scan images.
;    Example 2 jumps to the 2D scan number 10. 
;
;         v2->Next
;         v2->Next,10
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Feb 19, 2000.
;       xx-xx-xxxx      comment
;-

        error=0
	if n_elements(seqno) eq 0 then seqno = self.scanno_current + 1
	 self->panimage,seqno,error=error
	filename = self.name
END

PRO scan2d::Prev,seqno,filename,error=error
;+
; NAME:
;       scan2d::Prev
;
; PURPOSE:
;       This method points to the prev set of 2D scan images or to the 
;       specified 2D scan seqno set of 2D images.
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]Prev [,Seqno] [,Filename] [,ERROR=error]
;
; ARGUMENTS:
;     SEQNO     - jumps to the specified 2D scan # if it is specified.
;     FILENAME  - returns the opened filename
;
; KEYWORD:
;     ERROR     - returns the error code, non-zero if error found
;
; EXAMPLE:
;    Example points to the previous set of 2D scan images.
;    Example 2 jumps to the 2D scan number 10. 
;
;         v2->Prev
;         v2->Prev,10
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Feb 19, 2000.
;       xx-xx-xxxx      comment
;-

        error=0
	if n_elements(seqno) eq 0 then seqno = self.scanno_current - 1
	 self->panimage,seqno,error=error
	filename = self.name
END

PRO scan2d::First,filename,error=error
;+
; NAME:
;       scan2d::First
;
; PURPOSE:
;       This method points to the first set of 2D scan images 
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]First [,Seqno] [,Filename] [,ERROR=error]
;
; ARGUMENTS:
;     SEQNO     - specifies/returns the 2D scanno
;     FILENAME  - returns the opened filename
;
; KEYWORD:
;     ERROR     - returns the error code, non-zero if error found
;
; EXAMPLE:
;    Example points to the first set of 2D scan images.
;
;         v2->First
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Feb 19, 2000.
;       xx-xx-xxxx      comment
;-

        error=0
	 self->panimage,1,error=error
	filename = self.name
	seqno = 1 
END

PRO scan2d::Last,seqno,filename,error=error
;+
; NAME:
;       scan2d::Last
;
; PURPOSE:
;       This method points to the last set of 2D scan images 
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]Last [,Seqno] [,Filename] [,ERROR=error]
;
; ARGUMENTS:
;     SEQNO     - specifies/returns the 2D scanno
;     FILENAME  - returns the opened filename
;
; KEYWORD:
;     ERROR     - returns the error code, non-zero if error found
;
; EXAMPLE:
;    Example points to the last set of 2D scan images.
;
;         v2->Last
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Feb 19, 2000.
;       xx-xx-xxxx      comment
;-

        error=0
	 self->panimage,self.scanno_2d_last,error=error
	filename = self.name
	seqno = self.scanno_2d_last
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
 
        newImage = im(0:self.width-1, 0:self.height-1)
        s = size(newImage)
        im = newImage
 
        if s(0) ne 2 then begin
		res = dialog_message('Warning: data is not 2D image',/info)
                end
 
	seqno = self.image_no(self.scanno_current-1)+self.detector-1
        if keyword_set(view) then self->View,seqno,/noread
	self.seqno = seqno+1
END

PRO scan2d::Print
;+
; NAME:
;	scan2d::Print
;
; PURPOSE:
;       This method prints the current key variables for the object structure. 
;       It prints the starting and data directory, image filename, total #
;       of 2D scan, totol # of images, current 2D Scan #, detector #, and
;       image seq # at current image pointer, and the image seq array to
;       corresponding to detector 1.
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
print,self.image_no(0:self.scanno_2d_last - 1)
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

PRO scan2d::View2d,detno,plot=plot,group=group
	scanno = self.scanno_current
	imageno = self.image_no(scanno-1) + detno 
	self->point_lun,imageno-1
	self->view,/noplot
END


PRO scan2d::View,no,rscanno,rdetno,scanno=scanno,detector=detector,noread=noread,noplot=noplot,type=type,winId=winId
;+
; NAME:
;	scan2d::View
;
; PURPOSE:
;       This method lets the user view any valid 2D image from the 2D data
;       catcher image file.
; 
;       It also automatically pops up the flexible 2D plot package, it allows
;       the user view 2D image in TV, SURFACE, CONTOUR, and SHADE_SURF and
;       generated the printer copy from the PS file, idl.ps, if desired.
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]View [,No] [,Rscanno] [,Rdetno] 
;               [,SCANNO=scanno] [,DETECTOR=detector] [,NOREAD=noread]
;
; ARGUMENTS:
;     No       Specifies the sequence number of the image record. 
;     Rscanno  Returns the 2D scanno number of the image record. 
;     Rdecno   Returns the detector number of the image record. 
;
; KEYWORDS:
;     SCANNO:    It specifies the 2D scan number. If specified,it ignore the 
;                'No' specification. Valid SCANNO range:
;                [1 - self.scanno_2d_last] 
;     DETECTOR:  It specifies the desired detector number from the 2D scan.
;                If not specified, and SCANNO is given, detector 1 is assumed.
;     NOREAD:    If specified, no reading from the file pointer is done, 
;                only plot the current object is performed.
;     NOPLOT:    If specified, 2D flexible plotting package is not desired.
;     TYPE:      Specifies the string of plot type: CONTOUR,SURFACE,SHADE_SURF
;                the defualt is the TV plot
;     WINID:     If specified, the plot is send to the destination window
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
;	12-16-1998      Add flexible 2D TV, SURFACE, CONTOUR,SHADE_SURF 
;                       plot option
;	05-11-1999      Add plot TYPE keyword for window 0 
;                       Add NOPLOT keyword to bypass plot2d function call
;-

if keyword_set(noread) then goto,plotonly 
	unit = self.unit
;	print,'Read unit:',unit
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

	self->Read,scanno_2d=rscanno,detector=rdetno

plotonly:

if self.scanno_current gt 0 then $
no = self.image_no(self.scanno_current-1)+self.detector

	w = self.width
	h = self.height
	im = make_array(w,h)
	ln = 1L * w*h
	im(*,*) = self.image(0:ln-1)

if keyword_set(winid) then wset,winid else $
 window,0,xsize=500,ysize=500,title='scan2d Object'

	ncolors = !d.table_size
	header_note1 = '2D Scan # '+string(self.scanno_current) + $
		',    Image seqno ' + string(no) 
	header_note = 'Image( '+strtrim(w,2)+' , '+  strtrim(h,2)+') '
	xrange=[self.xarr(0), self.xarr(w-1)]
	yrange=[self.yarr(0), self.yarr(h-1)]

	TVSCL,congrid(im,!d.x_size -160,!d.y_size-160),80,80

        xstyle = 1
        ystyle = 1
        if yrange(0) eq yrange(1) then ystyle = 4
        if xrange(0) eq xrange(1) then xstyle = 4

	plot,xrange=xrange,yrange=yrange,/nodata, $
		[-1+yrange(0),-1+yrange(0)],/noerase, $
		pos=[80./!d.x_size, 80./!d.y_size, $
		 (!d.x_size-80.)/!d.x_size, (!d.y_size-80.)/!d.y_size], $
		xstyle=xstyle, ystyle=ystyle, $
		xtitle=self.x_desc, ytitle=self.y_desc, $
		title=self.z_desc + 'D'+ strtrim(self.detector,2)
	colorbar,[min(im),max(im)]


if keyword_set(TYPE) then begin
	CASE strupcase(TYPE) OF 
	'SURFACE': begin
	    SURFACE,congrid(im,100,100), charsize=1.5, $
		xtitle=self.x_desc, ytitle=self.y_desc, $
		title=self.z_desc + 'D'+ strtrim(self.detector,2)

	end
	'SHADE_SURF': begin
	    SHADE_SURF,im, self.xarr(0:w-1), self.yarr(0:h-1), charsize=1.5, $
		xtitle=self.x_desc, ytitle=self.y_desc, $
		title=self.z_desc + 'D'+ strtrim(self.detector,2)
	end
	'CONTOUR': begin
	zmin=min(im)
	zmax = max(im)
	dz = float(zmax-zmin)/8
	levels = make_array(9,/float)
	colors = make_array(9,/int)
	nc = !d.n_colors  ; !d.table_size 
	dnc = nc/8    ; 16
	for i=0,8 do begin
		levels(i)=zmin+i*dz	
		colors(i) = nc-i*dnc
	end
	 CONTOUR,im, self.xarr(0:w-1), self.yarr(0:h-1), levels=levels, $
		xmargin=[10,5], ymargin=[5,5], $
		xtitle=self.x_desc, ytitle=self.y_desc, $
		title=self.z_desc + 'D'+ strtrim(self.detector,2), $
		c_colors=reverse(colors), c_charsize=1.5,/follow
	end
	ELSE:
	ENDCASE
end

          xdis = 0.001 * !d.x_size
          ydis = !d.y_size - 1.2 * !d.y_ch_size
	  xyouts,xdis,ydis,header_note1,/device,color=ncolors-1

          ydis = !d.y_size - 2.2 * !d.y_ch_size
          xyouts,xdis,ydis,header_note,/device,color=ncolors-1

	if keyword_set(noplot) then return
	fileSeqString,no,suf0
	classname = self.name+'.'+suf0
	plot2d,im, xarr=self.xarr(0:w-1), yarr=self.yarr(0:h-1), $
		comment=[header_note1,header_note], $
		classname = classname, $
		xtitle=self.x_desc, ytitle=self.y_desc, $
                title=self.z_desc + 'D'+ strtrim(self.detector,2)

END

PRO scan2d::ReadAll,maxno,scanno_2d
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
		u_read,unit,pvs
		u_read,unit,nos
;  the EOF(unit) will not work on XDR data, following is add for terminate
;  the read
;        if self.scanno_2d_last gt nos(4) then goto,readfail   
	id = id + 1
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

readfail:
	maxno = id
	self.maxno = maxno
	self.seqno = maxno-1
	self.image_no(self.scanno_2d_last) = maxno 

	point_lun, unit, 0 

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
;      05-15-1998  bkc  Add message dialog if illegal number entered
;-

        if seqno ge 0 and seqno lt self.maxno then begin
        point_lun,self.unit,self.fptr(seqno)
        endif else begin
	res = dialog_message('Seqno must be less than '+string(self.maxno),/Error)
	end
END


PRO scan2d::panImage,scanno,factor,seqno=seqno,new_win=new_win, tiff=tiff,reverse=reverse,gif=gif,pict=pict,error=error
;+
; NAME:
;	scan2d::panImage
;
; PURPOSE:
;       This method pops up a new PanImage window for a given 2D scan #.
;       If the 2D scan # is not specified, then the 2D scanno # will be
;       calculated from the current 2D image sequence number.
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]panImage [,Scanno] [,Factor] [,TIFF='tifname',/reverse]
;                 [,GIF='gifname']
;
; ARGUMENTS:
;  Scanno:  Specifies the 2D scan # 
;  Factor:  Optional input, to specify the panImage window ratio factor 
;
; KEYWORDS:
;     GIF   : specifies the output gif filename. If specified the
;              panImage window will be saved in the gif output file.
;     TIFF   : specifies the output tiff filename. If specified the
;              panImage window will be saved in the tiff output file.
;     REVERSE: specifies whether the reverse tiff should be saved.
;
; EXAMPLE:
;    Following example shows how to get the panImage of all detectors 
;    for 2D scan #2, #3, and # 5 from the 2D image file 'junk2.image'.
;    The object v2 need to be defined only if it is not yet defined.
;
;         v2 = obj_new('scan2d',file='junk2.image')
;         v2->read
;	  v2->panImage,2
;	  v2->panImage,3
;	  v2->panImage,5
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Nov 19, 1998.
;	11-23-99  bkc  add option of saving panImage as TIFF/GIF file 
;-

error=0
unit = self.opened
seq = self.scanno_current
if n_elements(scanno) then seq = scanno 
last = self.scanno_2d_last

if unit le 0 then begin
	res=WIDGET_MESSAGE('Error: no image file loaded in')
	error=-1
	return
end
if seq lt 1 or seq gt self.scanno_2d_last then begin
	res=WIDGET_MESSAGE('Error: outside range 2D scanno entered')
	error=-1
	return
end

	seqno = self.image_no(seq-1)

	point_lun, unit, self.fptr(seqno)
	self->read
xdim = self.width 
ydim = self.y_req_npts

	image_array  = make_array(xdim,ydim,15,/float)
	def = make_array(15,value=0)

	point_lun, unit, self.fptr(seqno)
	scanno_2d = seq
	for i=0,14 do begin
		if EOF(unit) eq 1 then goto,update
		u_read, unit, pvs
		u_read, unit, nos
		if nos(4) ne seq then goto,update
		u_read, unit, x
		u_read, unit, y
		u_read, unit, t_image
		if nos(3) ge i then begin  ; exclude the 2nd set 2b-2c6 
		def(nos(3)) = 1
		image_array(*,*,nos(3)) = t_image
		end
	end

; pops up pan images

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
if error_status ne 0 and !error_state.name eq 'IDL_M_CNTOPNFIL' then begin
        r = dialog_message([!error_state.msg,!error_state.sys_msg,$
                string(!error_state.code)],/error)
        return
end
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
	self.seqno = self.image_no(seq-1)+self.detector
	self->point_lun,self.seqno

;	print,self.seqno,new_win,self.win,old_win

END

PRO scan2d::Images,scanno,image_array,def,vmax,vmin,X=x,Y=y,panimage=panimage,print=print,zdesc=zdesc
;+
; NAME:
;       scan2d::IMAGES
;
; PURPOSE:
;       For a given 2D scan number this method allows the user to extract the 
;       image_array for all the detectors from a 2D catcher file.
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]IMAGES, Scanno, Image_array, Def, Vmax, Vmin,/PAN ,/PRINT
;
; ARGUMENTS:
;       Scanno:        Specifies the 2D scan # to be extracted.
;       Image_array:   Image_array(*,*,15) is used to return all 15 detectors 
;                      image array for a picked 2D scan
;       Def[15]:       If specified, it returns the detector definition array
;                      for a scan record. 1 is defined, 0 is not defined
;       Vmax[15]:      If specified, it returns the maximum value of the 2D
;                      detector image array
;       Vmin[15]:      If specified, it returns the minimum value of the 2D
;                      detector image array
; 
; KEYWORDS:
;	PANIMAGE:      If specified, the panimage window of the 2D scanno
;                      will be displayed.
;       X:             If specified, it returns the X vector
;       Y:             If specified, it returns the Y vector
;       PRINT:         If specified, the Def, Vmax, Vmin vectors will be
;                      printed.
;       ZDESC:         If specified, it returns the detector description array
; EXAMPLE:
;    This example extracts all the detector images for the 2D scan # 15 from
;    the 2D image file 'junk2.image'. The panimage window is desired to
;    show the 2D scanno read in.
;    The object v2 need to be defined only if it is not yet defined.
;
;        v2 = obj_new('scan2d',file='junk2.image')
;        v2->IMAGES, 15, image_arrays, /PAN
;
; MODIFICATION HISTORY:
;        Written by:     Ben-chin Cha, Nov 9, 1999.
;        xx-xx-xxxx      comment
;-
; for a given scanno it returns the
;  image_array(width,height,15)
;  def(15)  - vector for detector defined
;  vmax(15) - vector for image max
;  vmin(15) - vector for image min

unit = self.opened
seq = self.scanno_current
if n_elements(scanno) then seq = scanno 
xdim = self.width 
ydim = self.y_req_npts
last = self.scanno_2d_last

if unit le 0 then begin
	res=WIDGET_MESSAGE('Error: no image file loaded in')
	return
end
if seq lt 1 or seq gt self.scanno_2d_last then begin
	res=WIDGET_MESSAGE('Error: outside range 2D scanno entered')
	return
end

	seqno = self.image_no(seq-1)

	point_lun, unit, self.fptr(seqno)

	image_array  = make_array(xdim,ydim,15,/float)
	def = make_array(15,value=0)

	vmin = make_array(15,/float)
	vmax = make_array(15,/float)
	zdesc = make_array(15,/string)
	scanno_2d = seq
	for i=0,14 do begin
		if EOF(unit) eq 1 then goto,update
		u_read, unit, pvs
		u_read, unit, nos
		if nos(4) ne seq then goto,update
		u_read, unit, x
		u_read, unit, y
		u_read, unit, t_image
		if def(nos(3)) eq 0 then begin
		def(nos(3)) = 1
		image_array(*,*,nos(3)) = t_image
		rmax = max(t_image,min=rmin)
		vmax(nos(3)) = rmax
		vmin(nos(3)) = rmin

        pvs = string(pvs)
        if n_elements(pvs) gt 3 then begin
		zdesc(nos(3)) = strtrim(pvs(5),2)
        end

		end
	end

update:


	if keyword_set(panimage) then self->panimage
	if keyword_set(print) then begin
	print,def
	print,vmax	
	print,vmin
	end
END

PRO scan2d::Calibration,scanno,format=format,GROUP=group
;+
; NAME:
;       scan2d::CALIBRATION
;
; PURPOSE:
;       This method calls the 2D image calibration program for a given
;       2D scanno. It allows the user flexiblely to select the images
;       and define the calibration function, calculate and displaying
;       the resultant image.
;
;       If the 2D scanno # is not specified, then the current 2D scan 
;       sequence number is assumed.
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]CALIBRATION [,Scanno]
;
; ARGUMENTS:
;  Scanno:     Specifies the 2D scan sequence #
;
; KEYWORDS:
;    FORMAT    Specifies the calibration output format.
;    GROUP:    Specifies the parent widget ID..
;
; EXAMPLE:
;     Following example calls the image calibration program for the 
;     2D scanno 10 from the image file 'junk2.image'
;     The object v2 need to be defined only if it is not yet defined.
;
;     	v2 = obj_new('scan2d',file='junk2.image')
;	v2->Calibration,10
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Nov 11, 1999.
;	xx-xx-xxxx      comment
;
;-

	if n_params() eq 0 then scanno = self.scanno_current
	if scanno gt 0 and scanno le self.scanno_2d_last then begin		

	seqno = self.image_no(scanno-1)
	point_lun, self.opened, self.fptr(seqno)
	self->read,x=x,y=y
	self->images,scanno,image_array,def,/PANIMAGE

	path = self.path
	ln = strlen(self.path) 
	if ln gt 0 and strmid(self.path,ln-1,1) ne !os.file_sep then $
		path = self.path + !os.file_sep
	calibration_factor,image_array,def,xv=x,yv=y,format=format, $
		classname=self.name,inpath=path, $ ;scanno=self.scanno, $
		title='  SCAN # '+strtrim(scanno,2),GROUP=group
	endif else print,'Error: scanno out of range'
END

PRO scan2d::ROI,no,debug=debug,header=header,comment=comment,_extra=e
;+
; NAME:
;       scan2d::ROI
;
; PURPOSE:
;       This method calls the 2D image Region of Interest program (scan2d_ROI)
;       and calculates the 2D statistics of the interested ROI.
;
;       If the 2D image # is not specified, then the current 2D image 
;       sequence number is assumed.
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]ROI [,no] [,/DEBUG]
;
; ARGUMENTS:
;  No:      Specifies the 2D image sequence #
;
; KEYWORDS:
;     DEBUG    If specified, the selected region of interest pixel values
;              and the corresponding X,Y index value will be printed.
;    HEADER    If specified, the header will be used in report generation
;              for the specified image
;
; SIDE EFFECTS:
;     The upper bound of rigion of intestest may be off by 1 pixel due to the 
;     rounding off the ratio of pixel/factor.
;
;     All ROI files will be created under the current working directory.
;     The default filename used for rectangle ROI is composed of 
;     'ROI/'+image_filename+'_roi.xdr' 
;     The default filename used for polygon ROI is composed of 
;     'ROI/'+image_filename+'_roi.xdr.poly' 
;     The default filename used for ROI report is composed of 
;     'ROI/'+image_filename+'_roi.rpt' 
;     
; EXAMPLE:
;     Following example calls the 2D ROI program for the 101th image from 
;     file 'junk2.image'
;     The object v2 need to be defined only if it is not yet defined.
;
;     	v2 = obj_new('scan2d',file='junk2.image')
;	v2->ROI,101
;
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, May 21, 1999.
;	xx-xx-xxxx      comment
;
;-

p = self.home+!os.file_sep+'ROI'
found = findfile(p) 
if found(0) eq '' then begin
	spawn,'mkdir p'
end

	if n_elements(no) eq 0 then no = self.seqno
	 self->point_lun,no-1
	 self->read,im=im,x=x,y=y

	if keyword_set(header) then h_annote = header else $
	begin
	h_annote=[ self.path+!os.file_sep+self.name,'Image Seq # '+strtrim(no,2) + $
		',  2D Scan # '+strtrim(self.scanno_current,2)+',  Detector # '+ $
			strtrim(self.detector,2)]
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


PRO scan2d::ROIRpt,scanno,Ref=Ref,roifile=roifile,rptfile=rptfile,header=header,comment=comment,append=append,mode=mode
;+
; NAME:
;	scan2d::ROIRPT
;
; PURPOSE:
;       For a specified 2D scan # and ROI, this method generates the complete
;       2D statistic report for all the detectors defined in a 2D scan.
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]ROIRPT [,Scanno] [,Ref=Ref] [,Roifile=Roifile] $
;	[,Rptfile=Rptfile] [,header=header] [,comment=comment] [,append=append]
;
; ARGUMENTS:
;  Scanno:  Specifies the 2D scan # 
;
; KEYWORDS:
;     Ref:      Specifies reference detector # for image normalization
;     Roifile:  Specifies the filename for ROI
;               which was previously created by the obj->ROI method.
;     Rptfile:  Specifies the filename for ROI report
;     Header:   Specifies the header description to be provided by the user 
;     Comment:  Specifies the comment string to be provided by the user
;     Append:   If specified, the new report will be appended to the file 
;               instead of save as new.
;     Mode:     Specifies the type of ROI file, 0 - RectROI, 2 - PolyROI
;
; RESTRICTION:
;     The 2D scanno, the detector Ref # must be a valid number.
;     If no ROI is found, then whole image area is assumed as ROI.
;
;     All the ROI files will be created under current working directory.
;     The default filename used for rectangle ROI is composed of 
;     'ROI/'+image_filename+'_roi.xdr' 
;     The default filename used for polygon ROI is composed of 
;     'ROI/'+image_filename+'_roi.xdr.poly' 
;     The default filename used for ROI report is composed of 
;     'ROI/'+image_filename+'_roi.rpt' 
;
; EXAMPLE:
;    
;    Following example shows how to get the ROIRPT of all detectors 
;    for 2D scan #2, #3, and # 5 from the 2D image file 'junk2.image'.
;    The scan #5 is normalized agaist the detector # 2.
;    The object v2 need to be defined only if it is not yet defined.
;
;         v2 = obj_new('scan2d',file='junk2.image')
;	  v2->roirpt,2
;	  v2->roirpt,3
;	  v2->roirpt,5,Ref=2
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, June 1, 1999.
;	xx-xx-xxxx      comment
;-

p = self.home+!os.file_sep+'ROI'
found = findfile(p) 
if found(0) eq '' then begin
	spawn,'mkdir p'
end

unit = self.opened
seq = self.scanno_current
if n_elements(scanno) then seq = scanno 

last = self.scanno_2d_last

if unit le 0 then begin
	res=WIDGET_MESSAGE('Error: no image file loaded in')
	return
end
if seq lt 1 or seq gt self.scanno_2d_last then begin
	res=WIDGET_MESSAGE('Error: outside range 2D scanno entered')
	return
end

	seqno = self.image_no(seq-1)

	point_lun, unit, self.fptr(seqno)
	self->read
xdim = self.width 
ydim = self.y_req_npts

	point_lun, unit, self.fptr(seqno)
	image_array  = make_array(xdim,ydim,15,/float)
	def = make_array(15,value=0)

	scanno_2d = seq
	for i=0,14 do begin
		if EOF(unit) eq 1 then goto,update
		u_read, unit, pvs
		u_read, unit, nos
		if nos(4) ne seq then goto,update
		u_read, unit, x
		u_read, unit, y
		u_read, unit, t_image
		def(nos(3)) = 1
		image_array(*,*,nos(3)) = t_image
	end

; pops up roi images

update:
	header_ass = ''
	comment_ass = ''
	if keyword_set(header) then header_ass=header
	if keyword_set(comment) then comment_ass=comment

	nodet = i
	pick = 1

	if keyword_set(ref) then pick=ref	
	if pick gt 0 and pick le nodet then im_ref = image_array(*,*,pick-1) else begin
		res = dialog_message('Invalid reference detector # picked',/error)
		return
		end

	xarr = self.xarr(0:xdim-1)
	yarr = self.yarr(0:ydim-1)

	reportname=p+!os.file_sep+self.name+'_roi.rpt'
	if keyword_set(rptfile) then reportname=rptfile

	; read roi

	xrange=[0,xdim-1]
	yrange=[0,ydim-1]

	filename=p+!os.file_sep+self.name+'_roi.xdr'
	if keyword_set(mode) then filename=filename+'.poly'
	if keyword_set(roifile) then filename=roifile
	found = findfile(filename)
	if found(0) eq '' then begin
		res = dialog_message(['ROI filename',filename, 'not found.', $
			'','The whole range is assumed if RectROI requested.',$
			'Otherwise call obj->roi method to define ROI first.'],/info)
		if keyword_set(mode) then return
	endif else begin

if keyword_set(mode) then begin
	; mode = 2
	u_openr,unit,filename,/XDR
	u_read,unit,xverts
	u_read,unit,yverts
	u_close,unit
	factor = [300/xdim,300/ydim]
	xv = fix(xverts/factor(0))
	yv = fix(yverts/factor(1))
	arr = polyfillv(xv,yv,xdim,ydim)
endif else begin
	; mode =0
	u_openr,unit,filename,/XDR
	u_read,unit,x
	u_close,unit
	xrange=fix([x(0)/x(4),(x(1)-1)/x(4)])
	yrange=fix([x(2)/x(5),(x(3)-1)/x(5)])
end
	end

	if keyword_set(append) then $
        openw,1,reportname,ERROR=err,/append else $
        openw,1,reportname,ERROR=err
        IF (err NE 0) then PRINTF, -2, !ERR_STRING
	printf,1,'===================================================='
	printf,1,'Generated at:  ',systime(0)
        printf,1,'Header: ',header_ass
        printf,1,'Comment: ',comment_ass
        printf,1,'ROIfile: ',filename
	printf,1,""

	for i=0,14 do begin
	if def(i) then begin
	;
        printf,1,'Detector #: ', i+1
		im = image_array(*,*,i)
		if keyword_set(ref) then im = image_array(*,*,i)/im_ref	
	if keyword_set(mode) then begin
		nelem = n_elements(arr)
		temp = make_array(nelem)
		for ij=0,nelem-1 do begin
		j = arr(ij) / xdim
		k = arr(ij) MOD xdim
		temp(ij) = im(k,j)
		end
	endif else begin
		temp = im[xrange(0):xrange(1),yrange(0):yrange(1)]
		nelem = (xrange(1)-xrange(0)+1)*(yrange(1)-yrange(0)+1)
	end
		result = moment(temp,mdev=mdev,sdev=sdev)
		temp_max = max(temp)
		temp_min = min(temp)
		total = total(temp)
		ave = result[0]

	; write  report
if keyword_set(mode) then begin
	printf,1,'ROI defined by polygon'
	printf,1,'Xverts index:',xv
	printf,1,'Yverts index:',yv
endif else begin
        printf,1,'ROI in index: [',strtrim(xrange(0),2),':', $
                strtrim(xrange(1),2),', ', $
                strtrim(yrange(0),2),':', $
                strtrim(yrange(1),2),'] '
        printf,1,'ROI in values: [',strtrim(self.xarr(xrange(0)),2),':', $
                strtrim(self.xarr(xrange(1)),2),', ', $
                strtrim(self.yarr(yrange(0)),2),':', $
                strtrim(self.yarr(yrange(1)),2),'] '
end

        printf,1,'ave = ',ave
        printf,1,'dev = ',sdev
        printf,1,'min = ',temp_min
        printf,1,'max = ',temp_max
        printf,1,'total = ',total
        printf,1,'nelem = ',nelem
        printf,1,''
	end
	end
        close,1

	xdisplayfile,reportname
END

PRO scan2d::Delete
	obj_destroy,self
END

PRO scan2d::Cleanup
	if self.unit then free_lun,self.unit
	catch,error_status
	if error_status ne 0 then goto,reset
	if self.win ne -1 then wdelete,self.win
reset:
	self.unit=0
	self.win = -1
END

FUNCTION scan2d::Init,file=file,print=print
; populate the index object if file is specified
device,decompose=0   ; required for 24 bits
;loadct,39
        if keyword_set(file) then self->Index,file
;        self->point_lun,0
	cd,current=h
	self.home = h
	self.win = -1

; if outfile not specified
;      first try data directory
;       then try starting directory
;       then user home directory
;
	dir = ''
	if self.path ne '' then dir = self.path+!os.file_sep 

	CATCH,error_status
	if error_status ne 0 then begin
	if keyword_set(print) then $
		print,!error_state.name,!error_state.code,!error_state.sys_msg
	if self.path ne '' and self.home ne self.path then $
	dir = self.home+!os.file_sep else $
	dir = getenv('HOME')+!os.file_sep 
	end
	openw,1,dir+'.tmp'
	close,1
	self.outpath = dir
        return,1
END
 
PRO scan2d__define
; increase max image size to 2000x2000
 
struct = { scan2d, $
	unit	: -1, $
	type	: 0, $
	XDR     : 0, $  ; default pure binary
	version : '', $
	home    : '', $
	path    : '', $
	outpath : '', $
        name    : 'catch1d.trashcan.image',           $
        opened  : 0,            $
	fptr	: make_array(10000,/long), $
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
	xarr	: make_array(2001), $
	yarr	: make_array(2001), $
	image	: make_array(2000,2000,/float) $
        }
END 
