;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************


PRO SDSVGname,names
COMMON HDF_QUERY_BLOCK,HDF_Query,HDF_Query_id

if n_params() eq 0 then begin
	str=['Usage:  Obj->[NX::]SDSVGname, names' $
	]
	xdisplayfile,text=str,title='NX::SDSVGname,names'
	return
end

;	fid = HDF_OPEN(self.file)
       fid = HDF_Query.fid

;	sd_id = HDF_SD_START(self.file)
	sd_id = HDF_Query.sd_id	


	vgroup = -1
	sds_vgname = '' 
	for j=0,HDF_Query.numVG-1 do begin

		vgroup = HDF_VG_GETID(fid,vgroup)
		vg_id = HDF_VG_ATTACH(fid,vgroup)	
		HDF_VG_GETINFO,vg_id,class=class,name=name,nentries=num_entries

		if num_entries ge 1 then begin
		HDF_VG_GETTRS,vg_id,tags,refs
		id = -1
		for i = 0,num_entries-1 do begin
			if tags(i) eq 720 then begin
			seq = HDF_SD_REFTOINDEX(sd_id,refs(i))
			sd_ids = HDF_SD_SELECT(sd_id,seq)
			HDF_SD_GETINFO,sd_ids, name=dname
			if sds_vgname(0) eq '' then $
			sds_vgname = name+'=>'+dname else $
			sds_vgname = [sds_vgname, name + '=>' + dname]
			HDF_SD_ENDACCESS,sd_ids
			end
		end
		end
		HDF_VG_DETACH,vg_id
	end
;	HDF_SD_END,sd_id
;	HDF_CLOSE,fid

	names= sds_vgname
	help,sds_vgname
print,names
END

PRO DumpHDFData, filename,startno, view=view, waittime=waittime 

COMMON HDF_QUERY_BLOCK,HDF_Query,HDF_Query_id

	IF N_ELEMENTS(filename) EQ 0 THEN begin
	print,''
	print,'DumpHDFData dumps the SDS contents of a HDF file'
	print,''
 	print,'USAGE: DumpHDFData,filename,startno [,/view]'
	print,'INPUT:'
	print,'      filename - required intput HDF SDS file'
	print,'KEYWORD:'
	print,'      /VIEW    - optional, if specified then the plot of data'
	print,'                 will be displayed too'
	print,''
	return
	end

wtime = 1.0 
if keyword_set(waittime) then begin
	print,waittime
	if waittime gt 0.5 then wtime = waittime
	end

	;	See if there is anything there to read

	HDF_DFSD_GETINFO, filename, NSDS=NumSDS
print,'NumSDS=',NumSDS

	IF NumSDS LT 1 THEN begin
		Message, "No Scientific Data Sets in File"
		return
		end

	;	Find out about the first SDS

 is = startno

	HDF_DFSD_SETINFO, /RESTART

str = string(replicate(32b,80))

if is gt 0 then begin
	for i=0,is-1  do begin
	HDF_DFSD_GETDATA, filename, SData, /GET_DIMS, /GET_TYPE
	end
end

;
;  the new hdf_sd_ function is used instead of old HDF_DFSD 
;

st0=string(replicate(32b,240))
st = [' SD #     TYPE      NATTR     NDIMS      DIMS           NAME                   FORMAT               LABEL       COORDSYS    UNIT          DATA (***First line of print data buffer***)']
	WIDGET_CONTROL,HDF_Query_id.terminal,BAD_ID=bad,SET_VALUE=st

	for i=is, NumSDS-1  do begin

	sds_id = HDF_SD_SELECT(HDF_Query.sd_id,i)
	HDF_SD_GETINFO,sds_id,dims=d,format=fm,label=lb,natts=na,ndims=nd, $
		coordsys=cs, NAME=n, $
		type=ty,unit=unit
	HDF_SD_GETDATA,sds_id,SData


s = size(SData)
no = s(0)
; print,'Data Set #',i,',   LABEL=',lb,',   UNIT=',unit & help,data
if no lt 1 then no = 1
dim = make_array(no)
dim = s(1:no)
type = s(n_elements(s)-2)


st = st0
strput,st,strtrim(i,1),3
strput,st,ty,10
strput,st,strtrim(string(na,/print),1),20
strput,st,strtrim(string(nd,/print),1),30
st1='('
for k=0,nd-1 do begin
	st1 = st1+' '+strtrim(d(k),2)
end
st1=st1+ ' )'
strput,st,strtrim(st1,2),40
if strlen(n) gt 0 then strput,st,n,56
if strlen(fm) gt 0 then strput,st,fm,80
if strlen(lb) gt 0 then strput,st,lb,100
if strlen(cs) gt 0 then strput,st,cs,120
if strlen(unit) gt 0 then strput,st,unit,130

; dump only first six number from the data array

catch,error_status
if error_status ne 0 then begin
	help,!error_state,/st
	help,sdata
	print,sdata
	return
end
if n_elements(sdata) ge 6 then temps=sdata(0:5) else temps=sdata
if HDF_Query.byte eq 0 then tempdata = string(string(temps),/print) else  $
	tempdata = string(temps,/print)
strput,st,strtrim(tempdata(0),2),141

	WIDGET_CONTROL,HDF_Query_id.terminal,BAD_ID=bad,SET_VALUE=strtrim(st,2)


if keyword_set(view) then begin

	erase
	chk_no = n_elements(chk)
	if (chk_no-2) ge 0 then chk_type = chk(chk_no-2)
	if chk_type eq 1 then vdata=string(SData) else $
		vdata = string(SData,/print)

	WIDGET_CONTROL,HDF_Query_id.terminal,BAD_ID=bad,SET_VALUE=vdata

	Print,'Displaying SData ' + unit
;	loadct,2
CASE no OF 
	1: BEGIN
	HDF_DFSD_DIMGET,0, LABEL=xl, UNIT=xu
		y1=min(data)
		y2=max(data)
		dy = 0.1 * (y2-y1)
		if dy eq 0 then dy = 1
		if type ne 1 then begin
		plot, YRANGE=[y1-dy,y2+dy], data, POS=[0.15,0.2,0.8,0.9], $
			xtitle=xl+' '+xu
		endif else begin
;	if type eq 1 then print,string(data)
		plot,[-1,-1],XStyle=4,YStyle=4
		xyouts, 0.2*!d.x_size, 0.25*!d.y_size, string(data),/DEVICE
		end
	END
	2: BEGIN
	HDF_DFSD_DIMGET,0, LABEL=xl, UNIT=xu
	HDF_DFSD_DIMGET,1, LABEL=yl, UNIT=yu
		y1=min(data)
		y2=max(data)*1.5
                !p.multi(0)=!p.multi(1)
                TVSCL, Data
                !p.multi(0)=!p.multi(1)-1
                surface, Data, zrange=[y1,y2]

;	if type eq 1 then begin
;		for j=0,dim(1)-1 do begin
;			str=data(0:dim(0)-1,j)
;			print,string(str)
;			end
;		end
	END
ENDCASE
	XYOUTS, !d.x_size/2, !d.y_size - 20, ALIGNMENT=0.5, /DEVICE, $
		STRING(title)
	XYOUTS, !d.x_size/2, !d.y_size - 40, ALIGNMENT=0.5, /DEVICE, $
		STRING(unit)
;
	HDF_Query.seqno = i
	WIDGET_CONTROL,HDF_Query_id.seqno,SET_VALUE=i
;	print,'Enter <CR> to continue, enter q to stop'
	WIDGET_CONTROL,HDF_Query_id.terminal,BAD_ID=bad, $
		SET_VALUE='Enter <CR> to continue, enter q to stop'

	st = ''
	read,st
	if st eq  'q' then begin
;		print,' Dump stopped!'
		WIDGET_CONTROL,HDF_Query_id.terminal,BAD_ID=bad, $
                SET_VALUE=' Dump stopped !'
		return
	end
end
	end

	WIDGET_CONTROL,HDF_Query_id.terminal,BAD_ID=bad, $
               SET_VALUE='*** End of Dump HDF SD Data!'

WIDGET_CONTROL,HDF_Query_id.terminal,BAD_ID=bad,GET_VALUE=st
openw,unit,'hdf_data.txt',/get_lun
printf,unit,st
free_lun,unit
close,unit

	HDF_SD_ENDACCESS,sds_id

END


PRO hdf_search_unitstring,filename,unitstring,recno,data
COMMON HDF_QUERY_BLOCK,HDF_Query,HDF_Query_id

if strlen(strtrim(unitstring,2)) lt 1 then return

if filename ne HDF_Query.file then begin
	st=['Error: You have to load the HDF file in first']
	HDF_scrolltext,st,60,3
	return
end

	if n_elements(recno) eq 0 then recno = 1

	;	See if there is anything there to read

	IF HDF_Query.numSDS LT 1 THEN begin 
		HDF_scrolltext, "No Scientific Data Sets in File"
		return
		end
	if recno ge HDF_Query.numSDS then begin
		HDF_scrolltext,'Error: there are only'+HDF_Query.numSDS+' sets of SDS data',60,3
		return
	end

	;	We need to fetch the Name of the data 

	found = -1
	i = recno
	while found eq -1 and i lt HDF_Query.numSDS do begin
		sds_id = HDF_SD_SELECT(HDF_Query.sd_id,i)
		HDF_SD_GETINFO, sds_id, LABEL=Title, UNIT=read_unit, NAME=nm
		found = strpos(strupcase(nm),strupcase(unitstring))
		if i ge HDF_Query.numSDS then begin
		   st=[ 'Error: Could not find Search string: ', $
		        '       " '+unitstring + ' "', $
			'        in the HDF file: '+filename]
	  	   HDF_scrolltext,st,60,3
		   recno = -1  ; not found
		   HDF_SD_ENDACCESS,sds_id
		   return
		end
		if found eq -1 then i = i+1
		HDF_SD_ENDACCESS,sds_id
	end	
	recno = i+1 
	HDF_Query.tname= nm
	HDF_Query.seqno = recno
;help,data,read_unit,Title,recno,nm,found
	if found eq -1 then return
	ReadHDFOneRecord,filename,data,/view
END	




PRO ReadHDFOneRecord, filename, data, view=view 

COMMON HDF_QUERY_BLOCK,HDF_Query,HDF_Query_id

WSET,HDF_Query_id.draw1
erase
	IF N_ELEMENTS(filename) EQ 0 THEN begin
 	print,'USAGE: ReadHDFOneRecord, filename, data [,/view]'
	print,'       Read next set of SDS data from the current positon'
	print,'INPUT:    '
	print,'       filename - HDF filename'
	print,'OUTPUT:'
	print,'       data     - the SDS data array returned'
	print,'KEYWORD:'
	print,'       /VIEW    - optional, if it is specified, the data will be displayed'
	print,''
	return
	end

if HDF_Query.seqno gt HDF_Query.maxno then begin
	HDF_Scrolltext,'Error:  End of SDS record reached!',60,3
	HDF_Query.seqno = 0
	return
	end

if HDF_Query.numSDS lt 1 then begin
	HDF_Scrolltext,'Error: no SDS data available!',60,3
	return
	end

WIDGET_CONTROL,/HOURGLASS
title=''
;=====
sds_id = HDF_SD_SELECT(HDF_Query.sd_id,HDF_Query.seqno)
HDF_SD_GETINFO,sds_id, LABEL=Ti, UNIT=read_unit, NAME=n
HDF_SD_GETDATA,SDS_ID,Data
;print,'NAME=',n
HDF_Query.tname= n
HDF_SD_ENDACCESS,sds_id
;=====

;print,'RECNO',HDF_Query.seqno
;help,Data
if keyword_set(attr) then begin
	Title = Ti
	print,'___LABEL= ',Title
	print,'____UNIT= ',read_unit
end
;print,data
;print,'---------'

s = size(Data)
no = s(0)
;help,s
if no gt 0 then begin 
dim = make_array(no)
dim = s(1:no)
end
type = s(n_elements(s)-2)

	erase
;	Print,'Displaying Data ' + read_unit
        xl='' & xu=''
	CASE no OF 
		0: BEGIN
		plot,[-1,-1],XStyle=4,YStyle=4
		xyouts, 0.2*!d.x_size, 0.25*!d.y_size, string(data),/DEVICE
		END 
		1: BEGIN
		old_win = !d.window
		!p.multi = [0,1,0,0,0]
		y1=min(data)
		y2=max(data)
		dy = 0.1 * (y2-y1)
		if dy eq 0 then dy = y1*0.1
		if type ne 1 then begin
		if s(1) eq 1 then begin 
			plot,[-1,-1],XStyle=4,YStyle=4
			xyouts, 0.2*!d.x_size, 0.25*!d.y_size, string(data),/DEVICE
		endif else $
			plot, YRANGE=[y1-dy,y2+dy], data, POS=[0.15,0.2,0.8,0.9] 
		endif else begin
		plot,[-1,-1],XStyle=4,YStyle=4
		xyouts, 0.2*!d.x_size, 0.25*!d.y_size, string(data),/DEVICE
		end
		END
		2: BEGIN
		if type eq 1 then begin        
		plot,[-1,-1],XStyle=4,YStyle=4
		yloc = 0.9*!d.y_size
			for j=0,dim(1)-1 do begin
			yloc = yloc - !d.y_ch_size
			if yloc ge 0 then begin
			str=data(0:dim(0)-1,j)
			xyouts, 0.2*!d.x_size, yloc, string(str),/DEVICE
			end
			end
		endif else begin
		y1=min(data)
		y2=max(data)*1.5
		old_win = !d.window
		!p.multi = [0,2,0,0,0]
		!p.multi(0)=!p.multi(1)
		TVSCL, CONGRID(data,!d.x_size/2, !d.y_size-20)
		!p.multi(0)=!p.multi(1)-1
		surface, Data , zrange=[y1,y2] 
		end
		END
		3: BEGIN
		plot,[-1,-1],XStyle=4,YStyle=4
		y1=min(data)
		y2=max(data)*1.5
		wide = 50
		left = 30
		bott = 50
		space = 10+wide
		newdata = congrid(data,wide,wide,s(3))
		old_win = !d.window
		!p.multi = [0,2,0,0,0]
		!p.multi(0)=!p.multi(1)
;		for i=0,dim(2)-1 do TV, newdata(*,*,i) , i*(wide+10)+50, 50 
; only show first 12 images from the 3D data array
		last = dim(2) - 1
		if last gt 11 then last = 11
		for i=0,last do begin
			id = i/6 
			ip = i mod 6
			TVSCL, newdata(*,*,i) , ip*space+left, id*space+bott
		end
		!p.multi(0)=!p.multi(1)-1
		END
		ELSE: HDF_scrolltext,'No plot supported for'+string(no)+'D data',60,3
	ENDCASE
	str='RECNO ' + strtrim(HDF_Query.seqno,1) + ': ' + HDF_Query.tname
	XYOUTS, !d.x_size/2, !d.y_size-10 , ALIGNMENT=0.5, /DEVICE, $
		STRING(str), charsize=1
	if keyword_set(attr) then begin
	XYOUTS, !d.x_size/2, !d.y_size - 30, ALIGNMENT=0.5, /DEVICE, $
		STRING(title), charsize=2
	XYOUTS, !d.x_size/2, !d.y_size - 40, ALIGNMENT=0.5, /DEVICE, $
		STRING(u), charsize=1
	end

;
; if	/VIEW is requested (separate Plot Window Option)
;
;if keyword_set(view) then begin
	if HDF_Query.view eq 1 then begin 
	old_win = !d.window
	CASE no OF 
	  0: BEGIN
		plot1d,[-1,-1],XStyle=4,YStyle=4,title=title,xtitle=u
		xyouts, 0.2*!d.x_size, 0.25*!d.y_size, string(data),/DEVICE
	     END
	  1: BEGIN
		if type ne 1 and n_elements(data) gt 1 then begin
		plot1d,data
		endif else begin
		plot1d,[-1,-1],XStyle=4,YStyle=4,title=title,xtitle=u
		xyouts, 0.2*!d.x_size, 0.25*!d.y_size, string(data),/DEVICE
		end
	     END
	  2: BEGIN
		plot2d,data,itools=1,wtitle=HDF_Query.file
	     END
	  3: BEGIN
		view3d_2D,data ; ,/slicer3
	     END
		ELSE: HDF_scrolltext,['Sorry, currently there is no separate plot window','supported for this type of data.'],60,3
	ENDCASE
	str='RECNO ' + strtrim(HDF_Query.seqno,1) + ': ' + HDF_Query.tname
	XYOUTS, !d.x_size/2, !d.y_size-10 , ALIGNMENT=0.5, /DEVICE, $
		STRING(str), charsize=1

	WSET,old_win
	end


	
END


;
;  convert byte array to strings
;
PRO BytesToStrings,inbyte,outstring,lrecl=lrecl,print=print
if n_elements(inbyte) eq 0 then begin
	print,''
	print,"BytesToStrings  routine converts a byte array to a string array"
	print,"               with the user specifyable lrecl."
	print,''
	print,"USAGE: BytesToStrings, inbyte, outstring [,lrecl=#,/print]
	print,"INPUT:"
	print,'        inbyte   - input byte array, required'
	print,'OUTPUT:'
	print,'       outstring - output string array'
	print,'KEYWORD:
	print,'       LRECL=#   - specifies the output string length,'
	print,'                   # default to 80 if not specified.'
	print,'       /PRINT    - print the string array generated'
	print,''
	return
	end
len = 80
if n_elements(lrecl) gt 0 then len = lrecl
s = size(inbyte)
no = s(1)/len
if s(1) gt (no*len) then no = no +1
outstring = make_array(no,/string,value=string(replicate(32b,len)))
for i=0,no-1 do begin
	i1 = i*len & i2 = i1 + len - 1
	if i2 gt (s(1)-1) then i2 = s(1)-1
	outstring(i) = string(inbyte(i1:i2))
	if keyword_set(print) then print,outstring(i)
	end
END
;
; hdf_datatotext.pro
;

PRO subarray,data,y1,y2,x1,x2,newdata
if n_elements(data) eq 0 then begin
	print,''
	print,'SUBARRAY   extracts a subarray from a given array'
	print,''
	print,'USAGE: subarray, data, y1, y2, x1, x2, newdata'
	print,''
	print,'INPUT: 
	print,'    data     -  Input array
	print,'    y1       -  Dimension 1 start index
	print,'    y2       -  Dimension 1 end index
	print,'    x1       -  Dimension 2 start index
	print,'    x2       -  Dimension 2 end index
	print,'OUTPUT:'
	print,'    newdata  -  Extracted sub-array
	print,''
	return
	end
dx = x2 - x1 + 1
dy = y2 - y1 + 1
if dx lt 1 or dy lt 1 then begin
	print,'Error: Subarray - invalid index range!'
	return
	end
temp = make_array(dy)
newdata = make_array(dy,dx)
for j=0,dx-1 do begin
        temp = data(y1:y1+dy-1,x1+j)
        newdata(0,j)=temp
        end
END

PRO datatotext,data,title,unit,file,help=help
COMMON HDF_QUERY_BLOCK, HDF_Query,HDF_QUERY_id

if n_elements(data) eq 0 then return

if keyword_set(help) then begin
	print,''
	print,'DataToText  displays the 1D or 2D data in a scrolled window.'
	print,''
	print,'USAGE: DataToText, data, title, unit, file'
	print,''
	print,'INPUT:'
	print,'     data     - Input data array to be displayed'
	print,'     title    - Descriptive title string, defaults to NULL'
	print,'     unit     - Descriptive unit string, defaults to NULL'
	print,'     file     - Text displaying file, defaults to "hdf_data.txt"
	return
	end

s = size(data)
no = s(0)
if no eq 3 then return

        if  HDF_Query.text eq 1 then begin
        WIDGET_CONTROL,HDF_Query_id.textdata,BAD_ID=bad
        if HDF_Query_id.textdata eq 0 or bad ne 0 then $
        HDF_Query_id.textdata = CW_TERM(HDF_Query_id.base, $
		TITLE='HDF SDS Text Window', $
                 XSIZE=80, YSIZE=20, /SCROLL, $
		rename=HDF_Query.dir+'hdf_sds.txt',bg_names='Save As...')
	end


st = ''
filename = 'hdf_data.txt'
if n_elements(file) ne 0 then filename = file
if no gt 0 then begin
    dim = make_array(no)
    dim = s(1:no)

    T1='' & T2=''
    if n_elements(title) ne 0 then T1 = title
    if n_elements(unit) ne 0 then T2 = unit 
    s1 = '  data('+strtrim(dim(0),2)
    for i=1,no-1 do begin
	s1 = s1 + ',' + strtrim(dim(i),2)
    end
    s1 = s1 + ')'

st = [T1+ '  ' + T2 + s1 ]
end
    type = s(n_elements(s)-2)

openw,fw,filename,/get_lun
printf,fw,'RECNO = ',HDF_Query.seqno
printf,fw,'NAME = ',HDF_Query.tname
printf,fw,st
printf,fw, '------------------------------'

;
; scalar type data
;
	if no eq 0 then begin
		newdata = string(data)
		printf,fw,newdata
		free_lun,fw
		id = CW_TERM(HDF_Query_id.textdata,filename=filename,/reset)
		return
	end
;
; BYTE type data : data = 1 as string, data >1  as byte data
;
	if type eq 1 then begin
	if no eq 1 then begin 
		BytesToStrings,data,outdata,lrecl=dim(0)
		printf,fw,outdata
	endif else begin 		; 2 or 3 D data
		if HDF_QUERY.byte eq 0 then newdata = string(data) else $
		newdata = data
		printf,fw,newdata
	end
	free_lun,fw
;	xdisplayfile,filename
	id = CW_TERM(HDF_Query_id.textdata,filename=filename,/reset, $
		bg_names='Save As...')
	return
	end
;
;  other type 
;
if no eq 1 then begin
;print,data
	f1 = '(I,g17.7)'
	for j=0,dim(0)-1 do begin
	printf,fw,format=f1,j,data(j)
	end
	free_lun,fw
;	xdisplayfile,filename
	id = CW_TERM(HDF_Query_id.textdata,filename=filename,/reset, $
		bg_names='Save As...')
	return
end

if no eq 2 then begin
	f0 = '("J=     ",'+strtrim(dim(1),2)+'I17,/)'
	f1 = '(I,'+strtrim(dim(1),2)+'(g17.7))'

	printf,fw,format=f0,indgen(dim(1))
	newdata = transpose(data)
	d1 = dim(1)
	d2 = dim(0)
	temp = make_array(dim(1))
	for j=0,d2-1 do begin
	temp = newdata(0:d1-1,j)
	printf,fw,format=f1,j,temp
	end
	free_lun,fw
;	xdisplayfile,filename
	id = CW_TERM(HDF_Query_id.textdata,filename=filename,/reset, $
		bg_names='Save As...')
	return
end


if no eq 3 then begin
	return
	f0 = '("J =    ",'+strtrim(dim(1),2)+'I17,/)'
	f1 = '(I,'+strtrim(dim(1),2)+'g17.7)'
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
;  show first 100 columns only
	if d2 gt 100 then d2 = 100
	for j=0,d2-1 do begin
	j1=j*d1
	j2 = j1+d1-1	
	x1 = new(j1:j2)
	printf,fw,format=f1,j,x1
	end
	end
	free_lun,fw
;	xdisplayfile,filename
	id = CW_TERM(HDF_Query_id.textdata,filename=filename,/reset, $
		bg_names='Save As...')
	return
end

END


PRO HDF_scrolltext_event,event
COMMON HDF_QUERY_BLOCK, HDF_Query, HDF_Query_id
COMMON HDF_scrolltext_block,HDF_scrolltext_answer
WIDGET_CONTROL, event.id, GET_UVALUE = eventval
CASE eventval OF
        "WARNINGTEXT_GET" : BEGIN
                WIDGET_CONTROL,event.id,GET_VALUE=ans
                HDF_scrolltext_answer = strtrim(strupcase(ans(0)),2)
                END
        "WARNINGTEXT_OK" : BEGIN
                WIDGET_CONTROL,event.top,/DESTROY
		HDF_Query_id.warning = 0L
                END
ENDCASE
END


PRO HDF_scrolltext, str,width,height,title,quest=quest, GROUP = GROUP
COMMON HDF_QUERY_BLOCK, HDF_Query, HDF_Query_id
COMMON HDF_scrolltext_block,HDF_scrolltext_answer

;if XRegistered('HDF_scrolltext') ne 0 then begin
;        WIDGET_CONTROL,HDF_scrolltext,/DESTROY
;        end

if HDF_Query_id.warning ne 0 then begin 
WIDGET_CONTROL,HDF_Query_id.warning,BAD=bad,/DESTROY
	HDF_Query_id.warning = 0L
end

if n_elements(width) eq 0 then width = 80
if n_elements(height) eq 0 then height = 45
if n_elements(title) eq 0 then title=''

HDF_scrolltext_base=WIDGET_BASE(TITLE = 'HDF Data'+ title, /COLUMN)
HDF_scrolltext_title = WIDGET_LABEL(HDF_scrolltext_base,VALUE=title)

list = WIDGET_TEXT(HDF_scrolltext_base,VALUE=str,UVALUE='LIST', $
        XSIZE =width, $
        YSIZE=height,/SCROLL)

HDF_scrolltext_row =WIDGET_BASE(HDF_scrolltext_base, /ROW, /FRAME)
if n_elements(quest) ne 0 then begin
HDF_scrolltext_lab = WIDGET_LABEL(HDF_scrolltext_row,VALUE=string(quest)+' (Y/N) ? ')

HDF_scrolltext_text = WIDGET_TEXT(HDF_scrolltext_row,VALUE='N', $
        EDITABLE=1, UVALUE='WARNINGTEXT_GET', XSIZE=2)
end

close = WIDGET_BUTTON(HDF_scrolltext_base, $
                        VALUE = ' Close ', $
                        UVALUE = 'WARNINGTEXT_OK')

HDF_Query_id.warning = HDF_scrolltext_base

WIDGET_CONTROL, HDF_scrolltext_base,/REALIZE

XMANAGER, 'HDF_scrolltext',HDF_scrolltext_base, GROUP_LEADER = GROUP

END

PRO DumpGROUPAN,filename
COMMON HDF_QUERY_BLOCK,HDF_Query,HDF_Query_id

FI='(A24," ",A8," ",I6," ",I)'
FS='(A24," ",A8," ",I6," ",A)'
FF='(A24," ",A8," ",I6," ",F)'
FD='(A24," ",A8," ",I6," ",D)'
F4='(A24," ",A8," ",I6)'
F5='(A24," ",A8," ",A6,"    ",A)'

	fileid = HDF_OPEN(filename,/read)
	sd_id = HDF_SD_START(filename,/read)
	HDF_SD_FILEINFO,sd_id,nmfsds,nglobatts

	dataS = "FILENAME        : "+HDF_Query.file 
        dataS=[dataS,'=====================================']
        dataS=[dataS,"    # of VGroups = "+string(HDF_Query.numVG)]
        dataS=[dataS,'=====================================']
        dataS=[dataS,"    # of Global Attributes = "+string(nglobatts)]
if nglobatts gt 0 then begin
        if nglobatts gt 0 then begin
         dataS=[dataS,"------------------------------------------------",'']

	str = string('NAME','TYPE','COUNT','VALUE',FORMAT=F5)
         dataS=[dataS,str,'']
         for i=0,nglobatts-1 do begin
                HDF_SD_ATTRINFO,sd_id,i,name=n,type=t,count=c,data=d
;          if (c ne 1) then dataS=[dataS,strtrim(n),t,c,FORMAT=F4 else $
           if (t eq 'STRING' ) then str1 = string(strtrim(n),t,c,d(0,0),FORMAT=FS) else $
           if (t eq 'FLOAT' ) then str1 = string(strtrim(n),t,c,d(0,0),FORMAT=FF) else $
           if (t eq 'DOUBLE' ) then str1 = string(strtrim(n),t,c,d(0,0),FORMAT=FD) else $
                str1 = string(strtrim(n),t,c,d(0,0),FORMAT=FI)
		dataS=[dataS,str1]
         endfor
        endif
end 
	HDF_SD_END,sd_id
	HDF_CLOSE,fileid

	WIDGET_CONTROL,HDF_Query_id.term,BAD_ID=bad,SET_VALUE=dataS
	if bad then HDF_Query_id.term = 0
END

PRO DumpVDATAAN,filename
COMMON HDF_QUERY_BLOCK,HDF_Query,HDF_Query_id

	fileid = HDF_OPEN(filename,/read)

	dataS = "FILENAME        : "+HDF_Query.file 
        dataS=[dataS,'=====================================']
        dataS=[dataS,"    # of Vdata = "+string(HDF_Query.numVD)]
        dataS=[dataS,'=====================================']

	HDF_CLOSE,fileid

	WIDGET_CONTROL,HDF_Query_id.term,BAD_ID=bad,SET_VALUE=dataS
	if bad then HDF_Query_id.term = 0
END


PRO DumpSDSAN,filename, print=print
COMMON HDF_QUERY_BLOCK,HDF_Query,HDF_Query_id

	dataS = "FILENAME        : "+HDF_Query.file 
        HDF_DFSD_GETINFO,filename, NSDS=numsds
        dataS =[dataS,'=====================================']
        dataS =[dataS," # of SDS = "+string(numsds)]
        dataS =[dataS,'=====================================']

if keyword_set(print) and  numsds gt 0 then begin
	fileid=HDF_OPEN(filename,/read)

	sd_id=HDF_SD_START(filename,/read)
	HDF_SD_FILEINFO,sd_id,nmfsds,nglobatts

;       Get the number of MFSDs in the file
        dataS =[dataS,'','=====================================']
        dataS =[dataS,''," # of Global Attribute MFSD = "+string(nmfsds),'']

        if nmfsds gt 0 then begin
         dataS =[dataS,"     Name        Rank      Type     DIMS      Nattrs   Attribute Value"]
	 dataS =[dataS,"---------------------------------------------------------------"]
         FSD='(A14," ",I4,"      ",A8,"     ",I4)'

         for i=0,nmfsds-1 do begin
          sds_id=HDF_SD_SELECT(sd_id,i)
          HDF_SD_GETINFO,sds_id,name=n,ndims=r,type=t,natts=nats,dims=dims
		str0 = string(n,r,t,nats,FORMAT=FSD)
		str1 = string(dims(0:r-1),format='(i5,i5,i5,i5,i5)')
          dataS =[dataS,str0 + '  DIMS=' + str1]

          for j=0,nats-1 do begin
            HDF_SD_ATTRINFO,sds_id,j,name=n,type=t,count=c,data=d
	    if c gt 1 then $
            dataS =[dataS,'                                  NAME='+string(n)+' TYPE='+strtrim(t,2)+' COUNT='+strtrim(c,2) + ' VAL='+strtrim(d(0),2) + ' ...'] $
            else dataS =[dataS,'                                  NAME='+string(n)+' TYPE='+strtrim(t,2)+' COUNT='+strtrim(c,2) + ' VAL='+strtrim(d(0),2) ]
          endfor
          HDF_SD_ENDACCESS,sds_id
         endfor
        endif

	HDF_SD_END,sd_id
	HDF_CLOSE,fileid
end
	WIDGET_CONTROL,HDF_Query_id.term,BAD_ID=bad,SET_VALUE=dataS
	if bad then HDF_Query_id.term = 0

END


PRO DumpHDFAN,filename,tag=tag,ref=ref,desc=desc,dataString=dataString
COMMON HDF_QUERY_BLOCK,HDF_Query,HDF_Query_id

if n_elements(filename) eq 0 then begin
	print,'DumpHDFAN   dump the HDF file annotations and text attributes'
	print,''
	print,'USAGE:    DumpHDFAN, filename [, /DESC | tag=tag ]'
	print,'INPUT:'
	print,'      filename   - required, input HDF file name'
	print,'KEYWORD: '
	print,'      /DESC      - if specified, dumps file id and desc found'
	print,'      tag        -  100 get file id'
	print,'      		-  101 get file description'
	print,'                 -  102 get tag identifier'
	print,'                 -  103 get tag description'
	print,'                 -  104 get data identifier'
	print,'                 -  105 get data description'
	return
	end



fileid=hdf_open(filename)
if fileid eq -1  then begin
	print,'Error: HDF file "',filename,'" not found!'
	return
	end


	view = keyword_set(desc)
	if view eq 1 then begin

        dataString='=================================================='
        dataString=[dataString," *****BEGINNING OF HDF_INFORMATION***** "]
        dataString=[dataString,''," FILENAME       : "+filename]

;       Get and report the number of free palettes.
        dataString=[dataString,'==================================================']
  	numfpals=hdf_dfp_npals(filename)>0
        dataString=[dataString," # of free palettes = " + string(numfpals)]

        dataString=[dataString,'==================================================']

	numfid = hdf_number(fileid,tag=100)
        dataString = [dataString,' # of file identifier = '+string(numfid)]
        if numfid gt 0 then begin
                HDF_DFAN_GETFID,filename,fid,/first
                dataString = [dataString,fid]
                if numfid gt 1 then $
                        for i=1,numfid-1 do begin
                        HDF_DFAN_GETFID,filename,fid
                	dataString = [dataString,fid]
                        end
        end

        dataString=[dataString,'==================================================']
        numdesc = hdf_number(fileid,tag=101)
; help,numfpals,numfid,numdesc
        if numdesc gt 0 then begin
        dataString=[dataString,'',' # of file descriptor =  '+string(numdesc)]
	dataString=[dataString,'','FILE_DESCRIPTOR # '+string(1)]
                HDF_DFAN_GETFDS,filename,descs,/string,/first
                dataString=[dataString,descs]
                if numdesc gt 1 then $
               	 	for i=1,numdesc-1 do begin
                        HDF_DFAN_GETFDS,filename,descs,/string
			dataString=[dataString,'FILE_DESCRIPTOR # '+string(i+1)]
                        dataString=[dataString,descs]
                        end
        end

	WIDGET_CONTROL,HDF_Query_id.term,BAD_ID=bad,SET_VALUE=string(dataString)
	if bad then HDF_Query_id.term = 0

	end

if n_elements(tag) eq 0 then begin
	hdf_close,fileid	
	return
	end

ON_IOERROR, EOD
; FDS tag =100
if tag eq 100 or string(tag) eq 'FID' then begin 
	numdesc = hdf_number(fileid,tag=100)
	dataString = 'N of file identifier = '+string(numdesc)
	if numdesc gt 0 then begin
        	HDF_DFAN_GETFID,filename,fid,/first
		dataString = [dataString,'','File id # 1']
		dataString = [dataString,fid]
		if numdesc gt 1 then $
			for i=1,numdesc-1 do begin
       	 		HDF_DFAN_GETFID,filename,fid
			dataString = [dataString,'','File id # '+strtrim(i,2)]
			dataString = [dataString,fid]
			end
		end
	hdf_close,fileid
	WIDGET_CONTROL,HDF_Query_id.term,BAD_ID=bad,SET_VALUE=string(dataString)
	if bad then HDF_Query_id.term = 0
	return
	end

; FDS tag =101
if tag eq 101 or  string(tag) eq 'FDS' then begin 
	numdesc = hdf_number(fileid,tag=101)
	if numdesc gt 0 then begin
        dataString=[dataString,'',' N of file descriptor =  '+string(numdesc)]
	dataString=[dataString,'','FILE_DESCRIPTOR # '+string(1)]
                HDF_DFAN_GETFDS,filename,descs,/string,/first
                dataString=[dataString,descs]
                if numdesc gt 1 then $
               	 	for i=1,numdesc-1 do begin
                        HDF_DFAN_GETFDS,filename,descs,/string
			dataString=[dataString,'FILE_DESCRIPTOR # '+string(i+1)]
                        dataString=[dataString,descs]
                        end
        end
	hdf_close,fileid
	WIDGET_CONTROL,HDF_Query_id.term,BAD_ID=bad,SET_VALUE=string(dataString)
	if bad then HDF_Query_id.term = 0
	return
        end

; TID tag =102
if (tag eq 102 or string(tag) eq 'TID') and n_elements(ref) gt 0 then begin 
	print,'Get Tag identifier # = ',ref
	ON_IOERROR, EOD
       	HDF_DFAN_GETLABEL,filename,102,ref,tid  & print,tid
	dataString = tid
	hdf_close,fileid
	return
	end

; TD tag =103
if (tag eq 103 or string(tag) eq 'TD') and n_elements(ref) gt 0 then begin 
	print,'Get Tag descriptor # = ',ref
	ON_IOERROR, EOD
       	HDF_DFAN_GETDESC,filename,103,ref,td  & print,string(td)
	dataString = string(td)
	hdf_close,fileid
	return
	end

; DIL tag =104
if (tag eq 104 or string(tag) eq 'DIL') and n_elements(ref) gt 0 then begin 
	print,'Get Data identifier label # = ',ref
	ON_IOERROR, EOD
       	HDF_DFAN_GETLABEL,filename,104,ref,dil 
	print,dil
	dataString = string(dil)
	hdf_close,fileid
	return
	end

; DIA tag =105
if (tag eq 105 or string(tag) eq 'DIA') and n_elements(ref) gt 0 then begin 
	print,'Get Data identifier annotation # = ',ref
	ON_IOERROR, EOD
       	HDF_DFAN_GETLABEL,filename,105,ref,dia 
	print,string(dia)
	dataString = string(dia)
	hdf_close,fileid
	return
	end

EOD: ON_IOERROR, NULL
	hdf_close,fileid
;	MESSAGE, !ERR_STRING,/NONAME

END


PRO makeHDFAN,filename,tag=tag,ref,text

fileid=hdf_open(filename,/write,/read)

ON_IOERROR, EOD

if fileid eq 0L then begin
	print,'Error: HDF file "',filename,'" not found!'
	return
	end

; FDS tag=101  - file descriptor
if string(tag) eq 'FDS' then begin 
	HDF_DFAN_ADDFDS,filename,string(text)
	hdf_close,fileid
	return
	end

; FID tag=100  - file id annotation 
if string(tag) eq 'FID' then begin 
	HDF_DFAN_ADDFID,filename,string(text)
	hdf_close,fileid
	return
	end

; TID tag=102  - tag identifier
if string(tag) eq 'TID' and n_elements(ref) gt 0 then begin 
	ON_IOERROR, EOD
	HDF_DFAN_PUTLABEL,filename,102,ref,text
	hdf_close,fileid
	return
	end

; TD tag=103  - Data descriptor 
if string(tag) eq 'TD' and n_elements(ref) gt 0 then begin 
	ON_IOERROR, EOD
	HDF_DFAN_PUTDESC,filename,103,ref,byte(text)
	hdf_close,fileid
	return
	end

; DIL tag=104  - Data id label
if string(tag) eq 'DIL' and n_elements(ref) gt 0 then begin 
	ON_IOERROR, EOD
	HDF_DFAN_PUTLABEL,filename,104,ref,text
	hdf_close,fileid
	return
	end

; DIA tag=105  - Data id annotation 
if string(tag) eq 'DIA' and n_elements(ref) gt 0 then begin 
        ON_IOERROR, EOD
	HDF_DFAN_PUTLABEL,filename,105,ref,text
	hdf_close,fileid
	return
	end


EOD: ON_IOERROR, NULL
hdf_close,fileid
MESSAGE, !ERR_STRING,/NONAME

END
PRO dumpDFR8Info
print,"USAGE:  num_image = dumpDFR8Info('filename')"
print,"   Input:  filename  - input hdf name"
print,"   Return:  num_image -  return number of 8-bit raster images found"
END

FUNCTION dumpDFR8Info,file
COMMON HDF_QUERY_BLOCK, HDF_Query, HDF_Query_id

	h = HDF_OPEN(file)
	num = HDF_DFR8_NIMAGES(file)

	if num gt 0 then begin

	HDF_DFR8_RESTART
	str = ""
	str =[str,"================== 8-bit raster image information ==============="]
	str = [str, "There are "+strtrim(num,2)+" image(s) in "+ file ]
	str =[str,""]
	
	for i=0,num-1 do begin
		HDF_DFR8_GETINFO,file,width,height,palette
  		str=[str,"    Image reference number: "+strtrim(i,2)]
            	str=[str,"         Dimension size: "+strtrim(width,2)+" X "+strtrim(height,2)]
		if palette eq 1 then $
            	str=[str,"         Has    palette: TRUE"] else $
            	str=[str,"         Has    palette: FALSE"]
		str=[str,""]
	end

	endif else str=['==================================================', $
			'There is no 8-bit image in '+file, '']
	HDF_CLOSE,h
	WIDGET_CONTROL,HDF_Query_id.an_term,BAD_ID=bad,SET_VALUE=str
	return, num
END

PRO drawDFR8Image,file,index,image,palette
COMMON HDF_QUERY_BLOCK, HDF_Query, HDF_Query_id

if n_elements(index) then begin
	h = HDF_OPEN(file)
	num = HDF_DFR8_NIMAGES(file)
	if index lt num then begin
	  HDF_DFR8_RESTART
	  for i=0,index-1 do begin
	  HDF_DFR8_GETINFO,file,width,height,palette
	  end
	  HDF_DFR8_GETIMAGE, file, image, palette
	  erase
;	  TV,image,/ORDER,0
	  res = CONGRID(image,HDF_Query_id.draw_xsize,HDF_Query_id.draw_ysize,3)
	  TV,res,/ORDER,0
	endif else print,"There are only ",num," sets of 8-bit raster image (0-based)!!"
	HDF_CLOSE,h
endif else begin
	print,""
	print,"USAGE:  drawDFR8Image,file,index,[image,palette]
	print,""
	print," Input:
	print,"    file - required, specifies the hdf file
	print,"    index - required, specifies the ref image number, 0-based
	print," Output:
	print,"    image - optional, return image array obtained
	print,"    palette - optional, return image palette obtained
	print,""
end

END

PRO dumpDF24Info
print,"USAGE:  num_image = dumpDF24Info('filename')"
print,"   Input:  filename  - input hdf name"
print,"   Return:  num_image -  return number of 24-bit raster images found"
END

FUNCTION dumpDF24Info,file
COMMON HDF_QUERY_BLOCK, HDF_Query, HDF_Query_id

	h = HDF_OPEN(file)
	num = HDF_DF24_NIMAGES(file)

	if num gt 0 then begin

	HDF_DF24_RESTART
	str = ""
	str =[str,"================== 24-bit raster image information ==============="]
	str =[str, "There are "+ strtrim(num,2) +" image(s) in "+ file ]
	str =[str,""]
	
	for i=0,num-1 do begin
		HDF_DF24_GETINFO,file,width,height,interlace
  		str =[str,"    Image reference number: "+ strtrim(i,2)]
            	str =[str,"         Dimension size: ",strtrim(width,2)," X ",strtrim(height,2)]
            	str =[str,"         Has    interlace method: "+strtrim(interlace,2)]
		str =[str,""]
	end

	endif else str=['==================================================', $
			'There is no 24-bit image in '+file, '']
	HDF_CLOSE,h
	WIDGET_CONTROL,HDF_Query_id.an_term,BAD_ID=bad,SET_VALUE=str
	return, num
END

PRO drawDF24Image,file,index,image,interlace
COMMON HDF_QUERY_BLOCK, HDF_Query, HDF_Query_id

if n_elements(index) then begin
	h = HDF_OPEN(file)
	num = HDF_DF24_NIMAGES(file)
	if index lt num then begin
	  HDF_DF24_RESTART
	  for i=0,index-1 do begin
	  HDF_DF24_GETINFO,file,width,height,interlace
	  end
	  HDF_DF24_GETIMAGE, file, image
	  erase
;	  TV,image,/ORDER,0
	  res = CONGRID(image,HDF_Query_id.draw_xsize,HDF_Query_id.draw_ysize,3)
	  TV,res,/ORDER,0
	endif else print,"There are only ",num," sets of 24-bit raster image (0-based)!!"
	HDF_CLOSE,h
endif else begin
	print,""
	print,"USAGE:  drawDF24Image,file,index,[image,interlace]
	print,""
	print," Input:
	print,"    file - required, specifies the hdf file
	print,"    index - required, specifies the ref image number, 0-based
	print," Output:
	print,"    image - optional, return image array obtained
	print,"    interlace - optional, return image interlace method 
	print,""
end

END



PRO R8IMAGE_id_Event, Event
COMMON R8IMAGE_BLOCK, R8IMAGE_id

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'BUTTON3': BEGIN
      Print, 'Event for DumpR8Header'
      no = dumpDFR8Info(R8IMAGE_id.file)
      END
  'R8_IMAGE_FIRST': BEGIN
	drawDFR8Image,R8IMAGE_id.file,0,image,palette
	R8IMAGE_id.seqno = 0
	  WIDGET_CONTROL,R8IMAGE_id.seqwid,SET_VALUE=strtrim(1,2)
      END
  'R8_IMAGE_NEXT': BEGIN
	R8IMAGE_id.seqno = R8IMAGE_id.seqno+1 
	if R8IMAGE_id.seqno lt R8IMAGE_id.maxno then begin
	  drawDFR8Image,R8IMAGE_id.file,R8IMAGE_id.seqno,image,palette 
	  WIDGET_CONTROL,R8IMAGE_id.seqwid,SET_VALUE=strtrim(R8IMAGE_id.seqno+1,2)
	endif else HDF_scrolltext,'Warning: last record reached!',60,3
      END
  'R8_IMAGE_PREV': BEGIN
	R8IMAGE_id.seqno = R8IMAGE_id.seqno-1 
	if R8IMAGE_id.seqno gt -1 then begin
	  drawDFR8Image,R8IMAGE_id.file,R8IMAGE_id.seqno,image,palette 
	  WIDGET_CONTROL,R8IMAGE_id.seqwid,SET_VALUE=strtrim(R8IMAGE_id.seqno+1,2)
	endif else HDF_scrolltext,'Warning: first record reached!',60,3
      END
  'R8_IMAGE_LAST': BEGIN
	R8IMAGE_id.seqno = R8IMAGE_id.maxno-1
	  drawDFR8Image,R8IMAGE_id.file,R8IMAGE_id.seqno,image,palette
	  WIDGET_CONTROL,R8IMAGE_id.seqwid,SET_VALUE=strtrim(R8IMAGE_id.maxno,2)
      END
  'R8IMAGE_SEQNO': BEGIN
	WIDGET_CONTROL,R8IMAGE_id.seqwid,GET_VALUE=no
	R8IMAGE_id.seqno = fix(no(0))
	if R8IMAGE_id.seqno gt 0  and R8IMAGE_id.seqno le R8IMAGE_id.maxno then begin
	  drawDFR8Image,R8IMAGE_id.file,R8IMAGE_id.seqno-1,image,palette 
	endif else HDF_scrolltext,'Warning: seqno out of range!',60,3
      END
  'R8IMAGE_SLIDER': BEGIN
	WIDGET_CONTROL,R8IMAGE_id.slider,GET_VALUE=seqno
	  WIDGET_CONTROL,R8IMAGE_id.seqwid,SET_VALUE=strtrim(seqno,2)
	  drawDFR8Image,R8IMAGE_id.file,seqno-1,image,palette 
	  R8IMAGE_id.seqno = seqno - 1
      END
  'R8_IMAGE_EXIT': BEGIN
	WIDGET_CONTROL,R8IMAGE_id.base,/DESTROY,BAD_ID=bad
	R8IMAGE_id.base = 0L
      END
  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END MAIN13
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.

PRO DFR8Image_init
COMMON R8IMAGE_BLOCK, R8IMAGE_id
  R8IMAGE_id = {  $
		base : 0L, $
		slider : 0L, $
		seqwid : 0L, $
		file : '', $
		maxno : 0, $
		seqno : 0 $
	}
END


PRO DFR8Image,file,no,GROUP=Group
COMMON R8IMAGE_BLOCK, R8IMAGE_id

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

if n_elements(no) then begin

if no lt 1 then return

  if n_elements(R8IMAGE_id) eq 0 then DFR8Image_init

  if Xregistered('R8IMAGE_id') then $
	WIDGET_CONTROL,R8IMAGE_id.base,/DESTROY,BAD_ID=bad

  R8IMAGE_id.base = 0L 
  R8IMAGE_id.seqwid = 0L 
  R8IMAGE_id.slider = 0L 
  R8IMAGE_id.file = file
  R8IMAGE_id.maxno = no

  MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
	TITLE='Raster 8 bit', $
      ROW=1, $
      MAP=1, $
      UVALUE='R8IMAGE_id')

  BASE2 = WIDGET_BASE(MAIN13, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

;  BUTTON3 = WIDGET_BUTTON( BASE2, $
;      UVALUE='BUTTON3', $
;      VALUE='DumpR8Header')

  BASE4 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE4')

  R8_IMAGE_FIRST = WIDGET_BUTTON( BASE4, $
      UVALUE='R8_IMAGE_FIRST', $
      VALUE='First')

  R8_IMAGE_NEXT = WIDGET_BUTTON( BASE4, $
      UVALUE='R8_IMAGE_NEXT', $
      VALUE='Next')

  R8_IMAGE_PREV = WIDGET_BUTTON( BASE4, $
      UVALUE='R8_IMAGE_PREV', $
      VALUE='Prev')

  R8_IMAGE_LAST = WIDGET_BUTTON( BASE4, $
      UVALUE='R8_IMAGE_LAST', $
      VALUE='Last')

  BASE9 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE9')

  LB0 = WIDGET_LABEL(BASE9,VALUE='Img #') 
  R8IMAGE_SEQNO = WIDGET_TEXT( BASE9,VALUE='0', $
      /EDITABLE, UVALUE='R8IMAGE_SEQNO', $
      XSIZE=4,YSIZE=1)

if no gt 1 then begin 
  SLIDER11 = WIDGET_SLIDER( BASE9, $
	MIN=1, MAX=no, $ 
      UVALUE='R8IMAGE_SLIDER', $
      VALUE=1)
  R8IMAGE_id.slider = SLIDER11
end

  R8_IMAGE_EXIT = WIDGET_BUTTON( BASE2, $
      UVALUE='R8_IMAGE_EXIT', $
      VALUE='Close')

  R8IMAGE_id.base =  MAIN13
  R8IMAGE_id.seqwid  = R8IMAGE_SEQNO 

  WIDGET_CONTROL, MAIN13, /REALIZE

  XMANAGER, 'R8IMAGE_id', MAIN13
end
END



PRO R24IMAGE_id_Event, Event
COMMON R24IMAGE_BLOCK, R24IMAGE_id

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'R24_IMAGE_FIRST': BEGIN
	drawDF24Image,R24IMAGE_id.file,0,image,palette
	R24IMAGE_id.seqno = 0
	  WIDGET_CONTROL,R24IMAGE_id.seqwid,SET_VALUE=strtrim(1,2)
      END
  'R24_IMAGE_NEXT': BEGIN
	R24IMAGE_id.seqno = R24IMAGE_id.seqno+1 
	if R24IMAGE_id.seqno lt R24IMAGE_id.maxno then begin
	  drawDF24Image,R24IMAGE_id.file,R24IMAGE_id.seqno,image,palette 
	  WIDGET_CONTROL,R24IMAGE_id.seqwid,SET_VALUE=strtrim(R24IMAGE_id.seqno+1,2)
	endif else HDF_scrolltext,'Warning: last record reached!',60,3
      END
  'R24_IMAGE_PREV': BEGIN
	R24IMAGE_id.seqno = R24IMAGE_id.seqno-1 
	if R24IMAGE_id.seqno gt -1 then begin
	  drawDF24Image,R24IMAGE_id.file,R24IMAGE_id.seqno,image,palette 
	  WIDGET_CONTROL,R24IMAGE_id.seqwid,SET_VALUE=strtrim(R24IMAGE_id.seqno+1,2)
	endif else HDF_scrolltext,'Warning: first record reached!',60,3
      END
  'R24_IMAGE_LAST': BEGIN
	R24IMAGE_id.seqno = R24IMAGE_id.maxno-1
	  drawDF24Image,R24IMAGE_id.file,R24IMAGE_id.seqno,image,palette
	  WIDGET_CONTROL,R24IMAGE_id.seqwid,SET_VALUE=strtrim(R24IMAGE_id.maxno,2)
      END
  'R24IMAGE_SEQNO': BEGIN
	WIDGET_CONTROL,R24IMAGE_id.seqwid,GET_VALUE=no
	R24IMAGE_id.seqno = fix(no(0))
	if R24IMAGE_id.seqno gt 0  and R24IMAGE_id.seqno le R24IMAGE_id.maxno then begin
	  drawDF24Image,R24IMAGE_id.file,R24IMAGE_id.seqno-1,image,palette 
	endif else HDF_scrolltext,'Warning: seqno out of range!',60,3
      END
  'R24IMAGE_SLIDER': BEGIN
	WIDGET_CONTROL,R24IMAGE_id.slider,GET_VALUE=seqno
	  WIDGET_CONTROL,R24IMAGE_id.seqwid,SET_VALUE=strtrim(seqno,2)
	  drawDF24Image,R24IMAGE_id.file,seqno-1,image,palette 
	  R24IMAGE_id.seqno = seqno - 1
      END
  'R24_IMAGE_EXIT': BEGIN
	WIDGET_CONTROL,R24IMAGE_id.base,/DESTROY,BAD_ID=bad
	R24IMAGE_id.base = 0L
      END
  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END MAIN13
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.

PRO DF24Image_init
COMMON R24IMAGE_BLOCK, R24IMAGE_id
  R24IMAGE_id = {  $
		base : 0L, $
		slider : 0L, $
		seqwid : 0L, $
		file : '', $
		maxno : 0, $
		seqno : 0 $
	}
END


PRO DF24Image,file,no,GROUP=Group
COMMON R24IMAGE_BLOCK, R24IMAGE_id

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

if n_elements(no) then begin

if no lt 1 then return

  if n_elements(R24IMAGE_id) eq 0 then DF24Image_init

  if Xregistered('R24IMAGE_id') then $
	WIDGET_CONTROL,R24IMAGE_id.base,/DESTROY,BAD_ID=bad

  R24IMAGE_id.base = 0L 
  R24IMAGE_id.seqwid = 0L 
  R24IMAGE_id.slider = 0L 
  R24IMAGE_id.file = file
  R24IMAGE_id.maxno = no

  MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
	TITLE='Raster 24 bit', $
      ROW=1, $
      MAP=1, $
      UVALUE='R24IMAGE_id')

  BASE2 = WIDGET_BASE(MAIN13, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE4 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE4')

  R24_IMAGE_FIRST = WIDGET_BUTTON( BASE4, $
      UVALUE='R24_IMAGE_FIRST', $
      VALUE='First')

  R24_IMAGE_NEXT = WIDGET_BUTTON( BASE4, $
      UVALUE='R24_IMAGE_NEXT', $
      VALUE='Next')

  R24_IMAGE_PREV = WIDGET_BUTTON( BASE4, $
      UVALUE='R24_IMAGE_PREV', $
      VALUE='Prev')

  R24_IMAGE_LAST = WIDGET_BUTTON( BASE4, $
      UVALUE='R24_IMAGE_LAST', $
      VALUE='Last')

  BASE9 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE9')

  LB0 = WIDGET_LABEL(BASE9,VALUE='Img #') 
  R24IMAGE_SEQNO = WIDGET_TEXT( BASE9,VALUE='0', $
      /EDITABLE, UVALUE='R24IMAGE_SEQNO', $
      XSIZE=4,YSIZE=1)

if no gt 1 then begin
  SLIDER11 = WIDGET_SLIDER( BASE9, $
	MIN=1, MAX=no, $ 
      UVALUE='R24IMAGE_SLIDER', $
      VALUE=1)
  R24IMAGE_id.slider = SLIDER11
end

  R24_IMAGE_EXIT = WIDGET_BUTTON( BASE2, $
      UVALUE='R24_IMAGE_EXIT', $
      VALUE='Close')

  R24IMAGE_id.base =  MAIN13
  R24IMAGE_id.seqwid  = R24IMAGE_SEQNO 

  WIDGET_CONTROL, MAIN13, /REALIZE

  XMANAGER, 'R24IMAGE_id', MAIN13
end
END
;
;  hdf_vg.pro
;
; Assumption that:
;         String array is stored as byte array
;         Image array can be any other type except the byte type
;            
; VG


PRO READHDFVG,fid,vdata_ids,vgroup_ids
COMMON HDF_QUERY_BLOCK, HDF_Query, HDF_Query_id

id=-1
tmp=(num=0)
while tmp ne -1 do begin
 tmp=HDF_VG_GETID(fid,id)
 if tmp ne -1 then begin
	 num=num+1
	if id eq -1 then vgroup_ids = tmp  else  vgroup_ids = [vgroup_ids,tmp]
	end
 id=tmp
endwhile
HDF_Query.numVG = num


id=-1
tmp=(num=0)
while tmp ne -1 do begin
 tmp=HDF_VD_GETID(fid,id)
 if tmp ne -1 then begin
	 num=num+1
	if id eq -1 then vdata_ids = tmp  else  vdata_ids = [vdata_ids,tmp]
	end
 id=tmp
endwhile
HDF_Query.numVD = num


return

END


;
;	read Vgroup for index vg
; 
PRO GetHDFVG,vg, data=data,entry=entry
COMMON HDF_QUERY_BLOCK, HDF_Query, HDF_Query_id
COMMON HDF_ID_BLOCK,vgroup_ids,vdata_ids,sds_ids

        if HDF_Query.numVG lt 1 then begin
                HDF_scrolltext,'Error: no VGroup available!',60,3
                return
        end

	WIDGET_CONTROL,HDF_Query_id.vg_term,BAD_ID=bad
	if HDF_Query_id.vg_term eq 0 or bad ne 0 then $
	HDF_Query_id.vg_term = CW_TERM(HDF_Query_id.base,TITLE='HDF VG Query', $
                bg_names='Save As...', $
		rename=HDF_Query.dir+HDF_Query.classname+'_vgQuery.txt',$
		XSIZE=80, YSIZE=20, /SCROLL)

str = string(replicate(32b,80))
st = str
strput,st,'VGROUP #',0
strput,st,'NAME',18
strput,st,'CLASS',35
strput,st,'NUM_ENTRIES',50
;print,st

	WIDGET_CONTROL,HDF_Query_id.vg_term,BAD_ID=bad, SET_VALUE=st

fid = HDF_Query.fid

 VGROUP = vgroup_ids(vg)

 VGROUP_ID=HDF_VG_ATTACH(fid,VGROUP)
 HDF_VG_GETINFO,VGROUP_ID,CLASS=CLASS,NAME=GNAME,NENTRIES=NUM_ENTRIES

st = str
strput,st,'VGROUP # = ',0
strput,st,strtrim(vg,2),11
strput,st,GNAME,18
strput,st,CLASS,35
strput,st,string(NUM_ENTRIES,format='(i5)'),50

	WIDGET_CONTROL,HDF_Query_id.vg_term,BAD_ID=bad, SET_VALUE=st

; if /ENTRY set then get and print the Vgroup entries

 if NUM_ENTRIES ge 1  and keyword_set(entry) then begin

HDF_VG_GETTRS,VGROUP_ID,tags,refs
att_names = make_array(NUM_ENTRIES+1,/string,value=string(replicate(32b,30)))
att_names(0) = 'GROUP #'+strtrim(vg,1)+' NAME: ' + GNAME

        id=-1
        for i=0,NUM_ENTRIES-1 do begin
	att_names(i+1) = "ENTRY # " + string(i)
                id=HDF_VG_GETNEXT(VGROUP_ID,id)
                v_string=STRING(i,FORMAT='(40x,"ENTRY # ",i3," is ")')

	if tags(i) ge 700 and tags(i) lt 800 then begin
		CASE tags(i) OF
                	700 : st=string(v_string,'SD Group',/print)
                	701 : st=string(v_string,'SD DimRec',/print)
                	702 : st=string(v_string,'SD ',/print)
                	703 : st=string(v_string,'SD Scales',/print)
                	704 : st=string(v_string,'SD Labels',/print)
                	705 : st=string(v_string,'SD Units',/print)
                	706 : st=string(v_string,'SD Formats',/print)
                	707 : st=string(v_string,'SD Max/Min',/print)
                	708 : st=string(v_string,'SD Coord sys',/print)
                	709 : st=string(v_string,'SD Transpose',/print)
                	720 : st=string(v_string,'SD Numeric Data Group',/print)
                	731 : st=string(v_string,'SD Calibration info',/print)
                	732 : st=string(v_string,'SD Fill Value info',/print)
		ENDCASE
		WIDGET_CONTROL,HDF_Query_id.vg_term,BAD_ID=bad, SET_VALUE=st
		;===== SD data
		IF keyword_set(data) and  tags(i) EQ 720 THEN $
			HDFDumpSDSData,tags,refs,i,[att_names(0),att_names(i+1)]
 
	endif else begin
		; check is it a VDATA entry

                if HDF_VG_ISVD(VGROUP_ID,id) then begin
                  vd_id=HDF_VD_ATTACH(fid,id)
                  HDF_VD_GET,vd_id,NAME=VDNAME
		  att_names(i+1) = VDNAME 
		st = string(v_string,' VDATA -> ' ,VDNAME,/print)
	
	WIDGET_CONTROL,HDF_Query_id.vg_term,BAD_ID=bad, SET_VALUE=st

; if /DATA then get and print the Vdata

if keyword_set(data) then begin
HDF_VD_GET,vd_id,NAME=VDNAME,count=ct,fields=fd,nfields=nf,ref=ref,tag=tag,size=sz

count= HDF_VD_READ(vd_id,vdata)

st=''
st=[st,string('nfields=',nf,/print)]
st=[st,string('fields=',fd,/print)]
st=[st,string('ref=',ref,/print)]
st=[st,string('tag=',tag,/print)]
st=[st,string('size=',sz,/print)]
st=[st,string('count=',count,/print)]
	WIDGET_CONTROL,HDF_Query_id.vg_term,BAD_ID=bad, SET_VALUE=st

		types=make_array(nf,/string)
		for index=0,nf-1 do begin
		  HDF_VD_GETINFO, vd_id, index, NAME=n, TYPE=ty
		  types(index) = ty
		end

	VDATA_lines,ct,nf,types,sz,fd,vdata,HDF_Query_id.vg_term

;chk = size(vdata)
;chk_no = n_elements(chk)
;if (chk_no-2) ge 0 then chk_type = chk(chk_no-2)
;if chk_type eq 1 then st=string('vdata=',string(vdata),/print) else $
;	st=string('vdata=',vdata,/print)
;st = [st,'']

;	WIDGET_CONTROL,HDF_Query_id.vg_term,BAD_ID=bad, SET_VALUE=st
end

                  HDF_VD_DETACH,vd_id

		; check is it a VGROUP entry

                endif else begin

                  if HDF_VG_ISVG(VGROUP_ID,id) then begin
                        vg_id=HDF_VG_ATTACH(fid,id)
			if vg_id ne -1 then begin
                        HDF_VG_GETINFO,vg_id,NAME=NAME
			st = string(v_string,'VGROUP -> ',GNAME,/print)
			WIDGET_CONTROL,HDF_Query_id.vg_term,BAD_ID=bad, SET_VALUE=st
                        HDF_VG_DETACH,vg_id
			att_names(i+1) = name
			endif else begin   ; vg_id = -1
				; tag = 1965 global attributes 
				if tags(i) eq 1965 then begin
				st = string(v_string,'VG ATTRI-> ',/print)
				WIDGET_CONTROL,HDF_Query_id.vg_term,BAD_ID=bad, SET_VALUE=st
				att_names(i+1) = 'Attribute'+string(i)
				HDFDumpSDSData,tags,refs,i,[att_names(0),att_names(i+1)]
				end
			end 
                  endif else begin
		st = string(v_string,'NOT A VDATA or VGROUP ',/print)
		WIDGET_CONTROL,HDF_Query_id.vg_term,BAD_ID=bad, SET_VALUE=st
		  end
                endelse
	end ;    not SD
        endfor
 endif         ; NUM_ENTRIES

 HDF_VG_DETACH,VGROUP_ID

END

PRO HDFDumpSDSData,tags,refs,i,NAME
COMMON VGPDATA_BLOCK, vgroupData_id
COMMON HDF_QUERY_BLOCK, HDF_Query, HDF_Query_id

;===== SD data
;IF keyword_set(data) and  tags(i) EQ 720 THEN BEGIN
sd_id = HDF_Query.sd_id 
index = HDF_SD_REFTOINDEX(sd_id,refs(i))
if index ge 0 then begin
        sds_id=HDF_SD_SELECT(sd_id,index)
        HDF_SD_GETDATA,sds_id,data
        HDF_SD_ENDACCESS,sds_id


s = size(data)
no = s(0)
if no gt 0 then begin
dim = make_array(no)
dim = s(1:no)
type = s(n_elements(s)-2)


CASE no OF 
  1: BEGIN
	if type ne 1 then plot,data,title=name(0) $
	else begin
		plot,[-1,-1],XStyle=4,YStyle=4
		xyouts, 0.2*!d.x_size, 0.25*!d.y_size, string(data),/DEVICE
	     end
	!p.multi = [1,2,0,0,0]			;plot attr names
	plot,yrange=[0,100],[-1,-1],XStyle=4,YStyle=4 
	real_xl = 0.55 * !d.x_size
	real_dy = !d.y_ch_size 
	real_yl = real_dy * (no +10)
	for j=0,n_elements(name)-1 do begin
	xyouts, real_xl, (real_yl - j * real_dy) ,name(j),/device
	end
	; if view plot is desired
	; use plot1d  
	if HDF_Query.view eq 1 then begin
		old_win = !d.window			;plot area
		!p.multi = [0,2,0,0,0]
		if type ne 1 then plot1d,data, title=name(0), comment=name  $
		else begin
		plot1d,[-1,-1],title=name(0),comment=[name,string(data)],XStyle=4,YStyle=4
		WSET,old_win
		!p.multi = [0,2,0,0,0]
	        end

	end
    END
  2: BEGIN
	erase
	y1 = min(data)
	y2 = max(data)*1.5
	!p.multi(0) = !p.multi(1)
	TVSCL,CONGRID(data,!d.x_size/2,!d.y_size-20)
	!p.multi(0) = !p.multi(1) - 1
	SURFACE,data,zrange=[y1,y2]
	; if view plot is desired
	if HDF_Query.view eq 1 then begin
		old_win = !d.window			;plot area
		!p.multi = [0,1,0,0,0]
		plot2d,data,itools=1,wtitle=HDF_Query.file
		WSET,old_win
		!p.multi = [0,2,0,0,0]
	end
     END
  ELSE: Print,'Curretly no plot supported for ',no,'D data'
ENDCASE


; if  data attrbutes is off return

if HDF_Query.attr eq 0 then return
 
T1='' & T2=''
s1 = ' SD:  data('+strtrim(dim(0),2)
for j=1,no-1 do begin
        s1 = s1 + ',' + strtrim(dim(j),2)
end
s1 = s1 + ') = '
s1 = [s1,'']

chk = size(data)
chk_no = n_elements(chk)
if (chk_no-2) ge 0 then chk_type = chk(chk_no-2)
if chk_type eq 1 then st=string(s1,string(data),/print) else $
	st=string(s1,data,/print)
st = [st,'']
	WIDGET_CONTROL,HDF_Query_id.vg_term,BAD_ID=bad, SET_VALUE=st
end  ; no > 0
end  ; index ne -1
END

;
; DumpHDFVG, filename       - dump whole VG
; dumpHDFVG, filename, n1, n2  
;			    - dump groups n1 to n2
;
;   /ENTRY                  - dump entry names if specified
;   /DATA		    - vdata is returned if specified
;
PRO DumpHDFVG,filename,n1,n2,vdata,data=data,entry=entry
COMMON HDF_QUERY_BLOCK, HDF_Query, HDF_Query_id

if HDF_Query.numVG lt 1  then begin
	HDF_scrolltext,'Error: no VGroup available !',60,3
	return
	end
	
fid = HDF_OPEN(filename, /READ)

if HDF_Query.numVG gt 0 then begin
start = 0
VGROUP=-1
num = HDF_Query.numVG

if n_elements(n2) gt 0 and n_elements(n1) gt 0 then begin
	if n2 lt n1 then begin
	HDF_scrolltext,'Error: n2 less tnan n1 ',60,3
	return
	end
	if n1 ge 1 then begin
	for i=0,n1-1 do begin
		VGROUP = HDF_VG_GETID(fid,VGROUP)
		if VGROUP eq -1 then begin
			HDF_scrolltext,'Error: only '+ string(i) +' groups found! ',60,3
			return
			end
		end
	end
	start= n1
	num = n2
	if num gt HDF_Query.numVG then num= HDF_Query.numVG
	end

	WIDGET_CONTROL,HDF_Query_id.vg_dump,BAD_ID=bad
        if HDF_Query_id.vg_dump eq 0 or bad ne 0 then $
        HDF_Query_id.vg_dump = CW_TERM(HDF_Query_id.base, $
                TITLE='HDF VG Dump', bg_names='Save As...',$
		rename=HDF_Query.dir+HDF_Query.classname+'_vgDump.txt',$
                 XSIZE=80, YSIZE=20, /SCROLL)

openw,fw,HDF_Query.textfile,/GET_LUN

printf,fw,'numVG =',HDF_Query.numVG

str = string(replicate(32b,80))
st = str
strput,st,'VGROUP #',0
strput,st,'NAME',15
strput,st,'CLASS',45
strput,st,'NUM_ENTRIES',55
printf,fw,st
for vg=start,num-1  do begin
 VGROUP=HDF_VG_GETID(fid,VGROUP)
 VGROUP_ID=HDF_VG_ATTACH(fid,VGROUP)
 HDF_VG_GETINFO,VGROUP_ID,CLASS=CLASS,NAME=NAME,NENTRIES=NUM_ENTRIES

st = str
strput,st,'VGROUP # = ',0
strput,st,strtrim(vg,2),11
strput,st,NAME,15
strput,st,CLASS,45
strput,st,string(NUM_ENTRIES,format='(i5)'),55
printf,fw,st

; if /ENTRY set then get and print the Vgroup entries

 if NUM_ENTRIES ge 1  and keyword_set(entry) then begin

HDF_VG_GETTRS,VGROUP_ID,tags,refs
        id=-1
        for i=0,NUM_ENTRIES-1 do begin
                id=HDF_VG_GETNEXT(VGROUP_ID,id)
                v_string=STRING(i,FORMAT='(40x,"ENTRY # ",i3," is ")')
	if tags(i) ge 700 and tags(i) lt 800 then begin
		CASE tags(i) OF
                	700 : printf,fw,v_string,'SD Group'
                	701 : printf,fw,v_string,'SD DimRec'
                	702 : printf,fw,v_string,'SD '
                	703 : printf,fw,v_string,'SD Scales'
                	704 : printf,fw,v_string,'SD Labels'
                	705 : printf,fw,v_string,'SD Units'
                	706 : printf,fw,v_string,'SD Formats'
                	707 : printf,fw,v_string,'SD Max/Min'
                	708 : printf,fw,v_string,'SD Coord sys'
                	709 : printf,fw,v_string,'SD Transpose'
                	720 : printf,fw,v_string,'SD Numeric Data Group'
                	731 : printf,fw,v_string,'SD Calibration info'
                	732 : printf,fw,v_string,'SD Fill Value info'
		ENDCASE
	endif else begin
                if HDF_VG_ISVD(VGROUP_ID,id) then begin
                  vd_id=HDF_VD_ATTACH(fid,id)
                  HDF_VD_GET,vd_id,NAME=NAME
                  printf,fw,v_string,' VDATA -> ' ,NAME

; if /DATA then get and print the Vdata

if keyword_set(data) then begin
HDF_VD_GET,vd_id,NAME=NAME,fields=fd,nfields=nf,ref=ref,tag=tag,size=sz

printf,fw,'nfields=',nf
printf,fw,'fields=',fd
printf,fw,'ref=',ref
printf,fw,'tag=',tag
printf,fw,'size=',sz
count= HDF_VD_READ(vd_id,vdata)
printf,fw,'count=',count

chk = size(vdata)
chk_no = n_elements(chk)
if (chk_no-2) ge 0 then chk_type = chk(chk_no-2)
if chk_type eq 1 then begin
	st=string(vdata)
	printf,fw,'vdata=',st
endif else printf,fw,'vdata=',vdata

printf,fw,''
end
                  HDF_VD_DETACH,vd_id
                endif else begin

                  if HDF_VG_ISVG(VGROUP_ID,id) then begin
                        vg_id=HDF_VG_ATTACH(fid,id)
                        HDF_VG_GETINFO,vg_id,NAME=NAME
                        printf,fw,v_string,'VGROUP -> ',NAME
                        HDF_VG_DETACH,vg_id
                   endif else $ 
                        printf,fw,v_string,'NOT A VDATA or VGROUP '
                endelse
	end   ; not SD
        endfor
 endif
 HDF_VG_DETACH,VGROUP_ID
endfor
free_lun,fw

;	xdisplayfile,HDF_Query.textfile,width=110,TITLE='HDF VG Dump'

	vg_dump = CW_TERM(HDF_Query_id.vg_dump,filename=HDF_Query.textfile, $
		/reset)


NO_VGROUPS:

 HDF_CLOSE,fid
end


END

;
; dump vdata from n1_ob to n2_ob
;

PRO DumpHDFVData,filename, n1_ob ,n2_ob,numVD=numVD
COMMON HDF_QUERY_BLOCK, HDF_Query, HDF_Query_id

; INPUT:
; 	fid   	- opened HDF file handler
; 	n1_ob 	- starting vdata record
; 	n2_ob 	- ending vdata record
;
; KEYWORD:
;	/numVD	-  if the keyword numVD is set 
;		   the n1_ob returns the total # of Vdata 
; 		   found in the opened HDF file
;

print,'filename=',filename
	if HDF_Query.fid eq 0 then begin
	fid = HDF_OPEN(filename,/READ)
	HDF_Query.fid = fid
	endif else fid = HDF_Query.fid

	if keyword_set(numVD) then begin
	i=0
	o1 = HDF_VD_GETID(fid,-1)
		while (o1 ne -1) do begin
		i=i+1
		o1 = HDF_VD_GETID(fid,o1)
		end
	n1_ob=i
	print,'numVD=',i
	HDF_Query.numVD = i
	return
	end

		if HDF_Query.numVD le 0 then  begin
		HDF_scrolltext,'Error: no Vdata available !',60,3
		return
		end

	WIDGET_CONTROL,HDF_Query_id.vd_dump,BAD_ID=bad
        if HDF_Query_id.vd_dump eq 0 or bad ne 0 then $
        HDF_Query_id.vd_dump = CW_TERM(HDF_Query_id.base, $
                TITLE='HDF VD Dump', $
                 XSIZE=80, YSIZE=20, /SCROLL)

str = string(replicate(32b,180))
	if n2_ob ge n1_ob  and n1_ob ge 0 then begin
		o1 = HDF_VD_GETID(fid,-1)
		if o1 ne -1 then begin

		for i=0,n1_ob-1 do begin
		print,'o1=',o1
		o1 = HDF_VD_GETID(fid,o1)
		if o1 eq -1 then begin
			HDF_Query.numVD = i
			print,'Last of list of VD encountered: ',i
			return
			end
		end

openw,fw,HDF_Query.textfile,/GET_LUN

printf,fw,'numVD=',HDF_Query.numVD
st=str
strput,st,'Record',0
strput,st,'Name',10
strput,st,'Fields',30
strput,st,'Class',40
strput,st,'Ref',50
strput,st,'Tag',60
strput,st,'Count',70
strput,st,'Size',80
strput,st,'Nfields',90
strput,st,'Data(0)',100
printf,fw,st

		for i=n1_ob,n2_ob do begin
		vd = HDF_VD_ATTACH(fid,o1,/READ)
		HDF_VD_GET,vd, class=cl, count=ct, fields=fd, interlace=il,$
			name=nm, nfields=nf, ref=r, size=sz, tag=tg 

st = str
strput,st,strtrim(string(i),2),0
strput,st,nm,10
strput,st,fd,30
strput,st,cl,40
strput,st,strtrim(string(r),2),50
strput,st,strtrim(string(tg),2),60
strput,st,strtrim(string(ct),2),70
strput,st,strtrim(string(sz),2),80
strput,st,strtrim(string(nf),2),90
		nread = HDF_VD_READ(vd,data,fields=fd)
	d_s = size(data)
	d_t = d_s(n_elements(d_s)-2)
	if HDF_Query.byte eq 0 then tempdata = string(string(data),/print) else $
	tempdata = string(data,/print)
	if d_t eq 7 then strput,st,string(data),100 else $
		strput,st,strtrim(string(tempdata(0)),2),100

printf,fw,strtrim(st,2)

		HDF_VD_DETACH,vd
		o1 = HDF_VD_GETID(fid,o1)
		if o1 eq -1 then begin
			FREE_LUN,fw
;		xdisplayfile,HDF_Query.textfile,width=110,TITLE='HDF VD Dump'

	vd_dump = CW_TERM(HDF_Query_id.vd_dump,filename=HDF_Query.textfile, $
		/reset)
			return
			end
		end
		end
	end

;HDF_CLOSE,fid

END

PRO GetHDFVData,filename,no,data,sz,ct,cl,nm,fd,nf,rf,tg, print=print, view=view
COMMON HDF_QUERY_BLOCK, HDF_Query, HDF_Query_id
COMMON HDF_ID_BLOCK,vgroup_ids,vdata_ids,sds_ids

;
; INPUT:
;	filename-	HDF filename 
;	no	-	Vdata record no
; OUTPUT:
;	data	-       Obtained data array
;	sz	- 	size
;	ct	-	count
;	cl	-	class string
;	nm	-	name string
;	fd	-	fields string
;	nf	-	no of fields
;	rf	-	reference no
;	tg	-	tag no
; KEYWORD:
;  	/PRINT  -       if set, prints the Vdata obtained
;

	if HDF_Query.fid eq 0  or HDF_Query.numVD le 0 then begin
		if HDF_QUERY.numVD le 0 then  begin
		HDF_scrolltext,'Error: no Vdata available !',60,3
		return
		end
	end

	fid = HDF_Query.fid

	if no lt 0 or no ge HDF_QUERY.numVD then begin
		st = ['Error: index exceeds the valid range', $
				'       only '+HDF_QUERY.numVD+' set of VD ']
		HDF_scrolltext,st,60,3
		return
		end

	o1 = vdata_ids(no)
	if o1 ne -1 then begin


	vd = HDF_VD_ATTACH(fid,o1,/READ)
	HDF_VD_GET,vd, class=cl, count=ct, fields=fd, interlace=il,$
		name=nm, nfields=nf, ref=rf, size=sz, tag=tg 

	nread = HDF_VD_READ(vd,data,fields=fd)

; dump to vd_term

	st = [ '', 'RECORD = '+string(no), $
		'class =  '+cl, $
		'fields = '+fd, $
		'name =   '+nm, $
		'nfields ='+string(nf), $
		'ref  =   '+string(rf), $
		'tag  =   '+string(tg), $
		'size =   '+string(sz), $
		'count =  '+string(ct), $
		'vdata =  ' $
		]

	WIDGET_CONTROL,HDF_Query_id.vd_term,BAD_ID=bad
	if HDF_Query_id.vd_term eq 0 or bad ne 0 then $
	HDF_Query_id.vd_term = CW_TERM(HDF_Query_id.base,TITLE='HDF VD Query', $
		XSIZE=80, YSIZE=20, /SCROLL)
	WIDGET_CONTROL,HDF_Query_id.vd_term,BAD_ID=bad, SET_VALUE=st

		types=make_array(nf,/string)
		for index=0,nf-1 do begin
		  HDF_VD_GETINFO, vd, index, NAME=n, TYPE=ty
		  types(index) = ty
		end

	VDATA_lines,ct,nf,types,sz,fd,data,HDF_Query_id.vd_term


; plot the data

	if keyword_set(view) then begin
        erase
	s = size(data)
	CASE s(0) OF
	0 : begin
		erase
	   end
	1: begin
                y1=min(data)
                y2=max(data)
                dy = 0.1 * (y2-y1)
                if dy eq 0 then dy = 1
                plot, YRANGE = [y1-dy,y2+dy], data, POS=[0.15,0.2,0.8,0.9] 
	   end
	2: begin
;               tv,data
		!p.multi(0) = !p.multi(1)
                tvscl,data
		!p.multi(0) = !p.multi(1)-1
                surface,data
	   end
	default:
	ENDCASE
        XYOUTS, !d.x_size/2, !d.y_size - 10, ALIGNMENT=0.5, /DEVICE, $
		STRING('RECNO='+string(no))
	end

	end
	HDF_VD_DETACH,vd
END


PRO VDATA_lines,ct,nf,types,sz,fd,data,vd_term
COMMON HDF_QUERY_BLOCK, HDF_Query, HDF_Query_id

if nf eq 1 then begin

	chk = size(data)
	chk_no = n_elements(chk)
	if (chk_no-2) ge 0 then chk_type = chk(chk_no-2)
 	if chk_type eq 1 then begin
		if HDF_Query.byte eq 0 then begin
		vdata = data  
		endif else begin
		vdata = string(data,format='(80(A))',/print)
		end
	endif else vdata = string(data,/print)

	; the data returned is always index number ???
	WIDGET_CONTROL,vd_term,BAD_ID=bad, SET_VALUE=string(vdata)

endif else begin

		str0 = string(replicate(32b,15))

		fields = str_sep(fd,',')
		st=''		
		for index=0,nf-1 do begin
		str =str0
		strput,str,fields(index), (15-strlen(fields(index)))/2
		st=st+str
		end
		WIDGET_CONTROL,vd_term,BAD_ID=bad, SET_VALUE=st

		for count=0,ct-1 do begin
			off = count*sz 
			st=''	
		for index=0,nf-1 do begin
			str=str0
			CASE types(index) OF
			'DOUBLE': begin
				strput,str,string(double(data,off),format='(g15.10)')
				off=off+8
				end
			'FLOAT': begin
				strput,str,string(float(data,off),format='(g12.7)')
				 off=off+4
				end
			'BYTE': begin
				strput,str,string(byte(data,off),format='(i1)')
				 off=off+1
				end
			'LONG': begin
				strput,str,string(long(data,off),format='(i8)')
				 off=off+4
				end
			'INT': begin 
				strput,str,string(int(data,off),format='(i6)')
				off=off+2
				end
			ELSE: print, 'type uncovered ',types(index)
			ENDCASE
			st=st+str
			end
		WIDGET_CONTROL,vd_term,BAD_ID=bad, SET_VALUE=st
		end
end

END



PRO HDFVD_DATA_Event, Event
COMMON VD_DATA_BLOCK, vdataData_id
COMMON HDF_QUERY_BLOCK, HDF_Query, HDF_Query_id

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF

; VD events

  'HDF_QUERY_VD_DUMP': BEGIN
      Print, 'Event for VD Dump'
	DumpHDFVData,HDF_Query.file,0,HDF_Query.numVD - 1
      END
  'HDF_VD_NEXT': BEGIN
	WIDGET_CONTROL,vdataData_id.seqwid, GET_VALUE= seqno
	recno = fix(seqno(0)) + 1
	if recno le 0 then $
		GetHDFVData, HDF_Query.file, 0 

        if recno lt HDF_Query.numVD then begin
             HDF_Query.vd_seqno = recno
	     if HDF_Query.view gt 0 then $
	     GetHDFVData,HDF_Query.file,recno, data, /view $
	     else GetHDFVData,HDF_Query.file,recno, data
	     WIDGET_CONTROL,vdataData_id.seqwid, $
			SET_VALUE= strtrim(string(recno),2)
	endif else begin
		HDF_scrolltext,'Warning: last record reached!',60,3
        end

      END

  'HDF_VD_PREV': BEGIN
	WIDGET_CONTROL,vdataData_id.seqwid, GET_VALUE= seqno
	recno = fix(seqno(0)) - 1

	if recno ge 0 then begin
		HDF_Query.vd_seqno = recno
		if HDF_Query.view gt 0 then $
		GetHDFVData, HDF_Query.file, recno, data, /view $
		else GetHDFVData, HDF_Query.file, recno, data
		WIDGET_CONTROL,vdataData_id.seqwid, $
			SET_VALUE= strtrim(string(recno),2)
	endif else begin
		HDF_scrolltext,'Warning: first record reached!',60,3
	end
      END

  'HDF_VD_FIRST': BEGIN
	if HDF_Query.view gt 0 then $
	GetHDFVData, HDF_Query.file, 0, data, /view $
	else GetHDFVData, HDF_Query.file, 0, data
	WIDGET_CONTROL,vdataData_id.seqwid, SET_VALUE='0'
	HDF_Query.vd_seqno = 0
      END

  'HDF_VD_LAST': BEGIN
	seqno = HDF_Query.numVD - 1
	WIDGET_CONTROL,vdataData_id.seqwid, $
		SET_VALUE=strtrim(string(seqno),2) 
	HDF_Query.vd_seqno = seqno
	if HDF_Query.view gt 0 then $
	GetHDFVData, HDF_Query.file, seqno ,data,/view $
	else GetHDFVData, HDF_Query.file, seqno ,data
      END

  'HDF_VD_SEQNO': BEGIN
      WIDGET_CONTROL,vdataData_id.seqwid, GET_VALUE=seqno
	seqno = fix(seqno(0))
	if seqno ge 0 and seqno lt HDF_Query.numVD then begin
      HDF_Query.vd_seqno = fix(seqno)
        if HDF_Query.view gt 0 then $
        GetHDFVData, HDF_Query.file, seqno, data, /view $
        else GetHDFVData, HDF_Query.file, seqno, data
	endif else begin
		HDF_scrolltext,'Warning: invalid record entered!',60,3
		WIDGET_CONTROL,vdataData_id.seqwid, SET_VALUE='0'
		HDF_Query.vd_seqno=0
	end
      END

  'HDF_VD_SLIDER': BEGIN
      WIDGET_CONTROL,vdataData_id.slider, GET_VALUE=seqno
      WIDGET_CONTROL,vdataData_id.seqwid, $
		 SET_VALUE=strtrim(string(seqno),2)
      HDF_Query.vd_seqno = seqno
        if HDF_Query.view gt 0 then $
        GetHDFVData, HDF_Query.file, seqno ,data,/view $
        else GetHDFVData, HDF_Query.file, seqno ,data
      END

  'HDF_VD_EXIT': BEGIN
	WIDGET_CONTROL,vdataData_id.base,BAD_ID=bad,/DESTROY
	WIDGET_CONTROL,HDF_Query_id.vd_term,BAD_ID=bad,/DESTROY
	WIDGET_CONTROL,HDF_Query_id.vd_dump,BAD_ID=bad,/DESTROY
	HDF_Query_id.vd_term = 0L
	HDF_Query_id.vd_dump = 0L
     END
  ENDCASE
END

PRO HDFVD_DATA_init
COMMON VD_DATA_BLOCK, vdataData_id
  vdataData_id = {  $
		base : 0L, $
		slider : 0L, $
		seqwid : 0L $
	}
END


PRO HDFVD_DATA,file,no,GROUP=Group
COMMON VD_DATA_BLOCK, vdataData_id
COMMON HDF_QUERY_BLOCK, HDF_Query, HDF_Query_id

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

if n_elements(no) then begin

if no lt 1 then return

  if n_elements(vdataData_id) eq 0 then HDFVD_DATA_init

  if Xregistered('HDFVD_DATA') then $
	WIDGET_CONTROL,vdataData_id.base,/DESTROY,BAD_ID=bad

  vdataData_id.base = 0L 
  vdataData_id.seqwid = 0L 
  vdataData_id.slider = 0L 

  MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
        TITLE='QUERY - VD ', $
      COL=1, $
      MAP=1, $
      UVALUE='vdataData_id')

;
; VDATA Query widgets
;

  BASE25 = WIDGET_BASE(MAIN13, $
      COLUMN=1, FRAME=1, $
      MAP=1, $
      UVALUE='BASE25')

  HDF_QUERY_VD_DUMP = WIDGET_BUTTON( BASE25, $
      UVALUE='HDF_QUERY_VD_DUMP', $
      VALUE='DumpVD')

  BASE25_1 = WIDGET_BASE(BASE25, $
      ROW=1, MAP=1, $
      UVALUE='BASE25_1')
  HDF_VD_FIRST = WIDGET_BUTTON( BASE25_1, $
      UVALUE='HDF_VD_FIRST', $
      VALUE='First')
  HDF_VD_NEXT = WIDGET_BUTTON( BASE25_1, $
      UVALUE='HDF_VD_NEXT', $
      VALUE='Next')
  HDF_VD_PREV = WIDGET_BUTTON( BASE25_1, $
      UVALUE='HDF_VD_PREV', $
      VALUE='Prev')
  HDF_VD_LAST= WIDGET_BUTTON( BASE25_1, $
      UVALUE='HDF_VD_LAST', $
      VALUE='Last')

  BASE25_3 = WIDGET_BASE(BASE25, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE25_3')
 hdf_vd_label=WIDGET_LABEL(BASE25_3,value='VD #')
 HDF_VD_SEQNO = WIDGET_TEXT(BASE25_3,/EDITABLE, /NO_NEWLINE, $
                 XSIZE=7, YSIZE=1, VALUE='0', $
                 UVALUE='HDF_VD_SEQNO')

  HDF_VD_SLIDER = 0L
  if HDF_Query.numVD gt 1 then $
  HDF_VD_SLIDER = WIDGET_SLIDER(BASE25_3,MAX=HDF_Query.numVD-1 , $
	MIN=0,UVALUE='HDF_VD_SLIDER')


  HDF_VD_EXIT= WIDGET_BUTTON( BASE25, $
      UVALUE='HDF_VD_EXIT', $
      VALUE='Close')

  vdataData_id.base =  MAIN13
  vdataData_id.seqwid  = HDF_VD_SEQNO
  vdataData_id.slider = HDF_VD_SLIDER

  WIDGET_CONTROL, MAIN13, /REALIZE

  XMANAGER, 'HDFVD_DATA', MAIN13
end
END
PRO VG,seq,tags,refs,ent_name,ent_type
COMMON HDF_ID_BLOCK,vgroup_ids,vdata_ids,sds_ids
COMMON HDF_QUERY_BLOCK,HDF_Query,HDF_Query_id

	fid = HDF_Query.fid
        vg_hdl = HDF_VG_ATTACH(fid,vgroup_ids(seq))
        if vg_hdl gt 0 then begin
        HDF_VG_GETINFO,vg_hdl,class=class,name=name,NENTRIES=NENTRIES
	if nentries gt 0 then $
        subVg,vg_hdl,NENTRIES,tags,refs,ent_name,ent_type,str
        end
        HDF_VG_DETACH,vg_hdl
END

PRO subVg,vgroup_id,num_en,tags,refs,att_name,att_type,str
COMMON HDF_QUERY_BLOCK,HDF_Query,HDF_Query_id

; att_type = 1 VGroup subgroup
;            2 VData 
;            3 SDS data
	str=''
	att_name = strarr(num_en)
	att_type = intarr(num_en)
	HDF_VG_GETTRS,vgroup_id,tags,refs

str = [str,string('tags:',tags,/print),string('refs:',refs,/print)]
	id = -1
	for i=0,num_en - 1 do begin
;	id = HDF_VG_GETNEXT(vgroup_id,id)
	id = refs(i)
	vd = HDF_VG_ISVD(vgroup_id,id)
	if vd then begin
		nm=''
		st = '     VDATA Attribute Entry # '+strtrim(i,2)
		att_type(i) = 2
		vd_id = HDF_VD_ATTACH(HDF_Query.fid,id)
		if vd_id gt 0 then begin

		HDF_VD_GET,vd_id,NAME=nm,fields=fields,nfields=nfields,ref=ref,tag=tag,size=sz
		count= HDF_VD_READ(vd_id,vdata)
		help,name,fields,nfields,ref,tag,sz,count,vdata,output=st0
		st=[st,st0, string(string(vdata),/print)]
 
		end 
		str = [str,st]
		HDF_VD_DETACH,vd_id
	end
	vg = HDF_VG_ISVG(vgroup_id,id)
	if vg then begin
		vg_id = HDF_VG_ATTACH(HDF_Query.fid,id)
		; subgroup entry
		if vg_id ne -1 then begin
			att_type(i) = 1
			HDF_VG_GETINFO,vg_id,NAME=nm
			if n_elements(str) eq 0 then $
			str = '     Attribute Entry # '+strtrim(i,2) + '     SUBGROUP name:'+ nm  else $
			str = [str,'     Attribute Entry # '+strtrim(i,2) + '     SUBGROUP name:'+ nm ]
		; group attribute entry
		end
		HDF_VG_DETACH,vg_id
	end

	sd_id = HDF_Query.sd_id
        seq = HDF_SD_REFTOINDEX(sd_id,id)
        if seq ge 0 then begin
		att_type(i) = 3
		; check for group attribute
		st = '     SUBGROUP Attribute Entry # '+strtrim(i,2)

                sds_id = HDF_SD_SELECT(sd_id,seq)
                HDF_SD_GETINFO,sds_id,NAME=nm,type=type,Ndims=ndims,dims=dims
;                HDF_SD_GETDATA,sds_id,data
                HDF_SD_ENDACCESS,sds_id
                if n_elements(nm) then st = st + '     name='+nm
                if n_elements(data) then st = st + '     data='+string(data)
		str = [str,st]
        end

	if att_type(i) eq 3 then nm = nm + '(SDS)'
	if att_type(i) eq 1 then nm = nm + '(VGroup)'
	if att_type(i) eq 2 then nm = nm + '(VData)'
	att_name(i) = nm
	end
	str = [str,'']
END

PRO VGROUPLIST_MAIN13_Event, Event
COMMON HDF_QUERY_BLOCK,HDF_Query,HDF_Query_id
COMMON VGROUPLIST_BLOCK,vgtree_id,p_name,p_class,p_ref,p_ent,c_ref,c_name,c_type,g_ref,g_name,g_type
COMMON HDF_ID_BLOCK,vgroup_ids,vdata_ids,sds_ids


  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'PARENT_VGROUP': BEGIN
	VG,Event.index,tags,refs,ent_name,ent_type
	c_name = ent_name
	c_ref = refs
	c_type = ent_type
	vgtree_id.parent_idx = Event.index
	WIDGET_CONTROL,vgtree_id.child,SET_VALUE=ent_name
	WIDGET_CONTROL,vgtree_id.grandchild,SET_VALUE=''
      END
  'CHILD_VGROUP': BEGIN
	WIDGET_CONTROL,vgtree_id.grandchild,SET_VALUE=''
	vgtree_id.child_idx = Event.index
	vgtree_id.child_ref = c_ref(Event.index)
	vgtree_id.child_type = c_type(Event.index)
	if vgtree_id.child_type eq 3 then begin
		HDF_Query.seqno = HDF_SD_REFTOINDEX(HDF_Query.sd_id,vgtree_id.child_ref)
	        ReadHDFOneRecord,HDF_Query.file, data, /view
        	if HDF_Query.text gt 0 then datatotext,data
		end
	if vgtree_id.child_type eq 1 then begin
	vg_id = HDF_VG_ATTACH(HDF_Query.fid,c_ref(Event.index))
		if vg_id gt 0 then begin
		HDF_VG_GETINFO,vg_id,name=name,class=class,NENTRIES=NENTRIES
		if nentries gt 0 then begin
		subVg,vg_id,NENTRIES,tags,refs,ent_name,ent_type,str
		WIDGET_CONTROL,vgtree_id.grandchild,SET_VALUE=ent_name
	g_ref = refs
	g_name = ent_name
	g_type = ent_type
		if HDF_Query.text gt 0 then $
		xdisplayfile,text=str,title='By VGroup List'
		end
		end
	HDF_VG_DETACH,vg_id
	endif
      END
  'GRANDCHILD_VGROUP': BEGIN
	vgtree_id.grandchild_idx = Event.index
	vgtree_id.grandchild_ref = g_ref(Event.index)
	vgtree_id.grandchild_type = g_type(Event.index)
	if vgtree_id.grandchild_type eq 3 then begin
		HDF_Query.seqno = HDF_SD_REFTOINDEX(HDF_Query.sd_id,vgtree_id.grandchild_ref)
	        ReadHDFOneRecord,HDF_Query.file, data, /view
        	if HDF_Query.text gt 0 then datatotext,data
		end
	if vgtree_id.grandchild_type eq 1 then begin
	vg_id = HDF_VG_ATTACH(HDF_Query.fid,g_ref(Event.index))
		if vg_id gt 0 then begin
		HDF_VG_GETINFO,vg_id,name=name,class=class,NENTRIES=NENTRIES
		if nentries gt 0 then begin
		subVg,vg_id,NENTRIES,tags,refs,ent_name,ent_type,str
		if HDF_Query.text gt 0 then $
		xdisplayfile,text=str,title='GrandChild VGroup'
		end
;	WIDGET_CONTROL,vgtree_id.grandchild,SET_VALUE=ent_name
;	WIDGET_CONTROL,vgtree_id.grandchild,SET_VALUE=''
		end
	HDF_VG_DETACH,vg_id
	endif
      END
  'BUTTON16': BEGIN
	WIDGET_CONTROL,Event.Top,/DESTROY
      END
  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END VGROUPLIST_MAIN13
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO VGROUPLIST, GROUP=Group
COMMON HDF_QUERY_BLOCK,HDF_Query,HDF_Query_id
COMMON VGROUPLIST_BLOCK,vgtree_id,p_name,p_class,p_ref,p_ent
COMMON HDF_ID_BLOCK,vgroup_ids,vdata_ids,sds_ids

  if XRegistered('VGROUPLIST_MAIN13') then return

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  VGROUPLIST_MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
	TITLE='** SELECT BY VGROUP LIST **', $
      ROW=1,  MAP=1, $
      UVALUE='VGROUPLIST_MAIN13')

  BASE2 = WIDGET_BASE(VGROUPLIST_MAIN13, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')


  BASE4 = WIDGET_BASE(VGROUPLIST_MAIN13, $
      COLUMN=1, $
      FRAME=2, $
      MAP=1, $
      UVALUE='BASE4')


  vgroup = -1
  p_name = strarr(HDF_Query.numVG)
  p_class = strarr(HDF_Query.numVG)
  p_ref = lonarr(HDF_Query.numVG)
  p_ent = intarr(HDF_Query.numVG)
  for i=0,HDF_Query.numVG-1 do begin
	VGROUP=HDF_VG_GETID(HDF_Query.fid,vgroup)
	VGROUP_ID=HDF_VG_ATTACH(HDF_Query.fid,VGROUP)
	HDF_VG_GETINFO,VGROUP_ID,CLASS=CLASS,NAME=NAME,NENTRIES=NUM_ENTRIES
	p_name(i) = name
	p_class(i) = 'name='+ name + '  class=' + class
	p_ref(i) = vgroup
	p_ent(i) = NUM_ENTRIES
  end

  LIST5 = WIDGET_LIST( BASE4,VALUE=p_class, $
      UVALUE='PARENT_VGROUP', $
      YSIZE=5)

  BASE6 = WIDGET_BASE(BASE4, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE6')

  ListVal437 = ['                                '] 
  LIST7 = WIDGET_LIST( BASE6,VALUE=ListVal437, $
      UVALUE='CHILD_VGROUP', $
      YSIZE=5,XSIZE=20)

  ListVal482 = ['                                '] 
  LIST8 = WIDGET_LIST( BASE6,VALUE=ListVal482, $
      UVALUE='GRANDCHILD_VGROUP', $
      YSIZE=5,XSIZE=20)


  BASE14 = WIDGET_BASE(BASE4, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE14')

  BUTTON16 = WIDGET_BUTTON( BASE14, $
      UVALUE='BUTTON16', $
      VALUE='Close')


  vgtree_id = { $
 	  parent: List5,  $
	  child: List7, $
	  grandchild: List8, $
	  parent_idx : 0, $         ; select index
	  child_idx : 0, $
	  grandchild_idx : 0, $
	  parent_ref : 0, $         ; hdf ref  
	  child_ref : 0, $
	  grandchild_ref : 0, $
	  parent_type : 1, $         ; group type 1,2,3 
	  child_type : 0, $
	  grandchild_type : 0 $
	}

  WIDGET_CONTROL, VGROUPLIST_MAIN13, /REALIZE

  XMANAGER, 'VGROUPLIST_MAIN13', VGROUPLIST_MAIN13
END




PRO HDFVGPDATA_Event, Event
COMMON VGPDATA_BLOCK, vgroupData_id
COMMON HDF_QUERY_BLOCK, HDF_Query, HDF_Query_id

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

; VG events

  CASE Ev OF 

  'HDF_VG_DROPLIST': BEGIN
	HDF_Query.glevel = Event.Index
        CASE Event.Index OF
            0: begin
		print,HDF_Query.glevel
               end
            1: begin
		print,HDF_Query.glevel
               end
            2: begin
		print,HDF_Query.glevel
	       end
	ENDCASE
      END
  'HDF_QUERY_VG_DUMP': BEGIN
	if HDF_Query.glevel eq 0 then DumpHDFVG,HDF_Query.file
	if HDF_Query.glevel eq 1 then DumpHDFVG,HDF_Query.file,/entry
	if HDF_Query.glevel eq 2 then DumpHDFVG,HDF_Query.file,/entry,/data
      END

  'HDF_VG_FIRST': BEGIN
	GetHDFVG, 0, /data, /entry 
	WIDGET_CONTROL,vgroupData_id.seqwid, SET_VALUE='0'
	HDF_Query.vg_seqno = 0
      END

  'HDF_VG_LAST': BEGIN
	seqno = HDF_Query.numVG-1
	WIDGET_CONTROL,vgroupData_id.seqwid, $
		SET_VALUE=strtrim(string(seqno),2) 
	HDF_Query.vg_seqno = seqno 
	GetHDFVG, HDF_Query.vg_seqno,/data,/entry 
      END

  'HDF_VG_NEXT': BEGIN
	WIDGET_CONTROL,vgroupData_id.seqwid, GET_VALUE= seqno
	recno = fix(seqno(0)) + 1
	if recno lt 0 then recno = 0

        if recno lt HDF_Query.numVG then begin
             HDF_Query.vg_seqno = recno
	     GetHDFVG, HDF_Query.vg_seqno, /data, /entry 
	     WIDGET_CONTROL,vgroupData_id.seqwid, $
			SET_VALUE= strtrim(string(recno),2)
	endif else begin
		HDF_scrolltext,'Warning: last record reached!',60,3
        end
      END

  'HDF_VG_PREV': BEGIN
	WIDGET_CONTROL,vgroupData_id.seqwid, GET_VALUE= seqno
	recno = fix(seqno(0)) - 1

	if recno ge 0 then begin
		HDF_Query.vg_seqno = recno
		WIDGET_CONTROL,vgroupData_id.seqwid, $
			SET_VALUE= strtrim(string(recno),2)
		GetHDFVG, recno, /data, /entry
	endif else begin
		HDF_scrolltext,'Warning: first record reached!',60,3
	end
      END


  'HDF_VG_SEQNO': BEGIN
      WIDGET_CONTROL,vgroupData_id.seqwid, GET_VALUE=seqno
	seqno = fix(seqno(0))
	if seqno ge 0 and seqno lt HDF_Query.numVG then begin
      HDF_Query.vg_seqno = fix(seqno)
        GetHDFVG, seqno, /data, /entry 
	endif else begin
		HDF_scrolltext,'Warning: invalid record entered!',60,3
		WIDGET_CONTROL,vgroupData_id.seqwid, SET_VALUE='0'
		HDF_Query.vg_seqno=0
	end
      END

  'HDF_VG_SLIDER': BEGIN
      WIDGET_CONTROL,vgroupData_id.slider, GET_VALUE=seqno
      WIDGET_CONTROL,vgroupData_id.seqwid, $
		 SET_VALUE=strtrim(string(seqno),2)
      HDF_Query.vg_seqno = seqno
        GetHDFVG, seqno, /data, /entry 
      END

  'HDF_VG_LIST': BEGIN
	VGROUPLIST,GROUP=Event.top
	END
  'HDF_VG_EXIT': BEGIN
      WIDGET_CONTROL,vgroupData_id.base,BAD_ID=bad,/DESTROY 
      WIDGET_CONTROL,HDF_Query_id.vg_term,BAD_ID=bad,/DESTROY 
      WIDGET_CONTROL,HDF_Query_id.vg_dump,BAD_ID=bad,/DESTROY 
      HDF_Query_id.vg_term = 0L
      HDF_Query_id.vg_dump = 0L
      END
  ENDCASE
END

PRO HDFVGPDATA_init
COMMON VGPDATA_BLOCK, vgroupData_id
  vgroupData_id = {  $
		base : 0L, $
		slider : 0L, $
		seqwid : 0L $
	}
END


PRO HDFVGPDATA,file,no,GROUP=Group
COMMON VGPDATA_BLOCK, vgroupData_id
COMMON HDF_QUERY_BLOCK, HDF_Query, HDF_Query_id

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

if n_elements(no) then begin

if no lt 1 then return

  if n_elements(vgroupData_id) eq 0 then HDFVGPDATA_init

  if Xregistered('HDFVGPDATA') then $
	WIDGET_CONTROL,vgroupData_id.base,/DESTROY,BAD_ID=bad

  vgroupData_id.base = 0L 
  vgroupData_id.seqwid = 0L 
  vgroupData_id.slider = 0L 

  HDF_Query.glevel = 0
  HDF_Query.vg_seqno = 0

  MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
	TITLE='QUERY - VG ', $
      ROW=1, $
      MAP=1, $
      UVALUE='vgroupData_id')

;
; VGroup Query widgets
;
  BASE45 = WIDGET_BASE(MAIN13, $
      COLUMN=1, FRAME=1, $
      MAP=1, $
      UVALUE='BASE45')

  BASE45_1 = WIDGET_BASE(BASE45, $
      FRAME=1, $
      ROW=1, MAP=1, $
      UVALUE='BASE45_1')

vg_droplist_btns = [ $
	'VG Only', $
	'VG Entries' $
;	'VG Entries + VD' $
	]
HDF_VG_DROPLIST = WIDGET_DROPLIST(BASE45_1, VALUE=vg_droplist_btns, $
	UVALUE='HDF_VG_DROPLIST', TITLE='')
WIDGET_CONTROL, HDF_VG_DROPLIST, SET_DROPLIST_SELECT=HDF_Query.glevel

  HDF_QUERY_VG_DUMP = WIDGET_BUTTON( BASE45_1, $
      UVALUE='HDF_QUERY_VG_DUMP', $
      VALUE='DumpVG')


  BASE45_2 = WIDGET_BASE(BASE45, $
      ROW=1, MAP=1, $
      UVALUE='BASE45_2')
  HDF_VG_FIRST = WIDGET_BUTTON( BASE45_2, $
      UVALUE='HDF_VG_FIRST', $
      VALUE='First')
  HDF_VG_NEXT = WIDGET_BUTTON( BASE45_2, $
      UVALUE='HDF_VG_NEXT', $
      VALUE='Next')
  HDF_VG_PREV = WIDGET_BUTTON( BASE45_2, $
      UVALUE='HDF_VG_PREV', $
      VALUE='Prev')
  HDF_VG_LAST= WIDGET_BUTTON( BASE45_2, $
      UVALUE='HDF_VG_LAST', $
      VALUE='Last')

  BASE45_3 = WIDGET_BASE(BASE45, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE45_3')
 hdf_vg_label=WIDGET_LABEL(BASE45_3,value='VG #')
 HDF_VG_SEQNO = WIDGET_TEXT(BASE45_3,/EDITABLE, /NO_NEWLINE, $
                 XSIZE=7, YSIZE=1, VALUE='0', $
                 UVALUE='HDF_VG_SEQNO')

  if HDF_Query.numVG gt 1 then begin
  HDF_VG_SLIDER = WIDGET_SLIDER(BASE45_3,MAX=HDF_Query.numVG-1 , $
	MIN=0,UVALUE='HDF_VG_SLIDER')
  vgroupData_id.slider = HDF_VG_SLIDER
  end

  HDF_VG_LIST = WIDGET_BUTTON( BASE45, $
      UVALUE='HDF_VG_LIST', $
      VALUE='BY VGROUP LISTS')

  HDF_VG_EXIT= WIDGET_BUTTON( BASE45, $
      UVALUE='HDF_VG_EXIT', $
      VALUE='Close')

  vgroupData_id.base =  MAIN13
  vgroupData_id.seqwid  = HDF_VG_SEQNO

  WIDGET_CONTROL, MAIN13, /REALIZE

  XMANAGER, 'HDFVGPDATA', MAIN13
end
END
;
;   hdf_sd.pro
;
; if file=file is not specified then the hdf_query.file is assumed
;

PRO hdf_create,FILE

if n_params() lt 1 then begin
	print,'Usage: hdf_create,FILE'
	return
	end

found=findfile(FILE)

if found(0) eq '' then begin
	filename=string(FILE)
	file_id = HDF_OPEN(filename,/ALL)
	HDF_CLOSE,file_id

	hdf_sd_open,filename

endif else print,'Warning: file already existed - ',FILE

END

PRO hdf_sd_init,file
	hdf_sd_open,file
END

PRO hdf_sd_close
COMMON HDF_QUERY_SD_BLOCK,HDF_QUERY_SD,HDF_QUERY_SD_id

	HDF_QUERY_SD.numSDS = 0L
	HDF_QUERY_SD.numGAttr = 0L

if HDF_QUERY_SD.sd_id gt 0 then $ 
	HDF_SD_END,HDF_QUERY_SD.sd_id
	HDF_QUERY_SD.sd_id=0

if HDF_QUERY_SD.fid gt 0 then begin
	CATCH,error_status
	if error_status ne 0 then begin
		print,!err_string
		HDF_QUERY_SD.fid = 0
		return
	end
	HDF_CLOSE,HDF_QUERY_SD.fid
	HDF_QUERY_SD.fid=0
end


END


PRO hdf_sd_open,file
COMMON HDF_QUERY_SD_BLOCK,HDF_QUERY_SD,HDF_QUERY_SD_id
;
; open as RDWR mode
;
if n_params() gt 0 then begin
	filename = string(file)

if n_elements(HDF_QUERY_SD) eq 0 then $
HDF_QUERY_SD = { $,
        fid: 0L, $
        sd_id : 0L, $
        file : '', $
        numSDS : 0L, $
        numGAttr : 0L $
        }

if HDF_QUERY_SD.fid gt 0 then hdf_sd_close

	CATCH,error_status
	if error_status ne 0 then begin
		print,'Error: file not found   "',filename,'"'
		return
	end

	fid= HDF_OPEN(filename,/RDWR)

	sd_id = HDF_SD_START(filename,/RDWR)
	HDF_SD_FILEINFO,sd_id,NumSDS,NumGAttr

	HDF_QUERY_SD.file = filename
	HDF_QUERY_SD.fid = fid
	HDF_QUERY_SD.sd_id = sd_id 
	HDF_QUERY_SD.numSDS = NumSDS
	HDF_QUERY_SD.numGAttr = NumGAttr
	print,'NumSDS',NumSDS
	print,'NumGAttr',NumGAttr

end

END

PRO hdf_sd_find,name,data,FILE=FILE,print=print
COMMON HDF_QUERY_SD_BLOCK,HDF_QUERY_SD,HDF_QUERY_SD_id

if n_params() lt 1 then begin
	print,'Usage: hdf_sd_find,name,data [FILE=FILE,/PRINT]
	return
	end

if n_elements(HDF_QUERY_SD) gt 0 then $
filename = HDF_QUERY_SD.file
if keyword_set(FILE) ne 0 then filename=string(FILE)
if n_elements(filename) eq 0 then begin
	print,'Error: need filename!'
	return
	end

	CATCH,error_status
	if error_status ne 0 then begin
		print,'Error: file not found   "',filename,'"'
		return
	end

	sd_id = HDF_SD_START(filename)
	index = HDF_SD_NAMETOINDEX(sd_id,name)

	if index ge 0 then begin
	sds_id=HDF_SD_SELECT(sd_id,index)
	HDF_SD_GETDATA,sds_id,data
	if keyword_set(print) then begin
		print,name
		s = size(data)
		type = s(n_elements(s)-2)
		if type eq 1 then print,string(data) else print,data
		end
	HDF_SD_ENDACCESS,sds_id
	endif else print,'****** ', name,' ******not found!'

	HDF_SD_END,sd_id

END


PRO hdf_sd_dump, FILE=FILE, NumSDS, NumGAttr, sds_id_array, sds_name_array
COMMON HDF_QUERY_SD_BLOCK,HDF_QUERY_SD,HDF_QUERY_SD_id
;
; dump header for all the SD found in FILE
;
;   optional  input: FILE=FILE
;             output:  NumSDS, NumGAttr, sds_id_array
;

if n_elements(HDF_QUERY_SD) gt 0 then $
filename = HDF_QUERY_SD.file
if keyword_set(FILE) ne 0 then filename=string(FILE)
if n_elements(filename) lt 1 then begin
	print,'Error: need keyword FILE="filename"!'
	return
	end

hdf_sd_open,filename

if HDF_QUERY_SD.fid gt 0 then begin

	sd_id = HDF_QUERY_SD.sd_id
	NumSDS = HDF_QUERY_SD.numSDS
	NumGAttr = HDF_QUERY_SD.numGAttr

	if NumSDS eq 0 then begin
		print,"Error:  No SD set found in '",filename,"'"
		return
	end

	sds_id_array = make_array(NumSDS,/LONG)
	sds_name_array = make_array(NumSDS,/string)

    for index=0,NumSDS-1 do begin
	sds_id = HDF_SD_SELECT(sd_id,index)

	if sds_id  gt 0 then begin
	HDF_SD_GETINFO,sds_id,dims=d,format=f,label=l,natts=na, $
		ndims=nd,type=t,unit=u, name=ti

	HDF_SD_GETDATA,sds_id,data

	print,'index=',index,'   sds_id',sds_id
	help,d,f,l,na,nd,t,u
	help,data

	sds_id_array(index) = sds_id
	sds_name_array(index) = ti

	end
	HDF_SD_ENDACCESS,sds_id
    end

end
hdf_sd_close

END

PRO hdf_sd_read, FILE=FILE, no,name,data,nattr 
COMMON HDF_QUERY_SD_BLOCK,HDF_QUERY_SD,HDF_QUERY_SD_id
;
; dump header the no'th  SD found in the FILE
;
;   optional  input: FILE, no
;             output: name,data,nattr 
;

if n_params() lt 1 then begin
	print,'Usage: hdf_sd_read,no [,name, data, nattr],FILE=FILE
	return
	end

if n_elements(HDF_QUERY_SD) gt 0 then $
filename = HDF_QUERY_SD.file
if keyword_set(FILE) ne 0 then filename=string(FILE)
if n_elements(filename) lt 1 then begin
        print,'Error: need filename!'
        return
        end

hdf_sd_open,filename
NumSDS = HDF_QUERY_SD.numSDS
sd_id = HDF_QUERY_SD.sd_id

	if no lt 0 or no gt (NumSDS-1) then begin
		print,'Error: input no out of range!'
		print,'Allowable index number must be less than',NumSDS
		HDF_SD_END, sd_id
		return
	end


	sds_id = HDF_SD_SELECT(sd_id,no)
	print,'index=',no,'   sds_id',sds_id

	if sds_id  gt 0 then begin
	HDF_SD_GETINFO,sds_id,dims=d,format=f,label=l,natts=nattr, $
			ndims=nd,type=t,unit=u, name=name

	HDF_SD_GETDATA,sds_id,data
	help,d,f,l,nattr,nd,t,u
	help,data

	end

	HDF_SD_ENDACCESS,sds_id
hdf_sd_close

END




PRO hdf_sd_write, FILE=FILE, name=name, data, index, ref, field=field, $
	range=range, unit=unit, format=format, coordsys=coordsys, label=label, $
        caldata=caldata, fill=fill, $	
	attr_name=attr_name, attr_data=attr_data
COMMON HDF_QUERY_SD_BLOCK,HDF_QUERY_SD,HDF_QUERY_SD_id
;
; create an SD data set and get the reference number
;
varName = 'Name'
if n_elements(name) eq 0 then begin
usage:
	print,'Usage: hdf_sd_write,FILE=FILE,data,name=name [,index,ref, $
	print,'       fields=fields,attr_name=attr_name, attr_data=attr_data] 
print,'
print,' INPUT:
print,'   data          - data array
print,' OUTPUT:
print,'   ref           - ref number
print,'   index         - SD index number
print,' KEYWORD: 
print,'	  NAME  	- required, string for varName
print,'   FIELD		- string
print,'   RANGE		- [min, max]
print,'   UNIT		- string
print,'   FORMAT	- string
print,'   COORDSYS	- string
print,'   LABEL		- string
print,'   CALDATA	- calibration structure 
print,'   FILL		- value 
print,'   ATTR_NAME	- string
print,'   ATTR_DATA	- string
print,'   FILE		- existing HDF filename 
print,' 
	return 
	end

if keyword_set(FILE) ne 0 then begin
	filename=string(FILE)
	found=findfile(filename)
	if found(0) eq '' then hdf_create,filename $
	else hdf_sd_open,filename
end

if n_elements(HDF_QUERY_SD) gt 0 then $
filename = HDF_QUERY_SD.file else begin
	print,'Error: the HDF file name is unknown!'
	return
	end
	fid =  HDF_QUERY_SD.fid
	sd_id=HDF_SD_START(filename,/RDWR)

;  write data

  hdf_write_data, sd_id, data, index, ref, name=name, attr_name=attr_name, $
    attr_data=attr_data, field=field, range=range, unit=unit, format=format, $
    coordsys=coordsys, label=label, caldata=caldata, fill=fill

	HDF_SD_END,sd_id
	HDF_CLOSE,fid

END

PRO hdf_sd_dump_gattr,gattr_names,FILE=FILE
COMMON HDF_QUERY_SD_BLOCK,HDF_QUERY_SD,HDF_QUERY_SD_id
;
;  dump global attribute gattr_names 
;

if n_elements(HDF_QUERY_SD) gt 0 then $
filename = HDF_QUERY_SD.file
if keyword_set(FILE) ne 0 then filename=string(FILE)
if n_elements(filename) lt 1 then begin
        print,'Error: need filename!'
        return
        end

hdf_sd_open,filename
sd_id = HDF_QUERY_SD.sd_id
numGAttr = HDF_QUERY_SD.numGAttr
	
if numGAttr gt 0 then begin
gattr_names=make_array(numGAttr,/string)

	for gindex=0,numGAttr-1  do begin
	hdf_sd_attrinfo,sd_id,gindex,name=n,type=t,count=c,data=data
	help,gindex,sd_id,n,t,c
	print,data
	gattr_names(gindex)=n
	end

endif else begin
	print,'hdf_sd_dump_gattr: No global attributes found !'
end
hdf_sd_close

END


PRO hdf_sd_find_gattr,gattr_name,gattr_data,FILE=FILE
COMMON HDF_QUERY_SD_BLOCK,HDF_QUERY_SD,HDF_QUERY_SD_id
;
;  find the global attribute data for the given global attribute name 
;     input:   gattr_name
;     output:  gattr_data
;
if n_params() lt 1 then begin
	print,'Usage: hdf_sd_find_gattr, gattr_name, gattr_data [,FILE=FILE]
	return
	end

if n_elements(HDF_QUERY_SD) gt 0 then $
filename = HDF_QUERY_SD.file
if keyword_set(FILE) ne 0 then filename=string(FILE)
if n_elements(filename) lt 1 then begin
        print,'Error: need filename!'
        return
        end

hdf_sd_open,filename
sd_id = HDF_QUERY_SD.sd_id

;CATCH, error_status
;if error_status ne 0 then begin
;	print,!err_string
;	hdf_sd_close
;	retall
;end

if HDF_QUERY_SD.fid gt 0 then begin
	title = string(gattr_name)
	gindex=hdf_sd_attrfind(sd_id,title)
	if gindex ne -1 then begin
	hdf_sd_attrinfo,sd_id,gindex,name=n,type=t,count=c,data=gattr_data
	help,sd_id,gindex,n,t,c
	print,gattr_data
	endif else begin
		print,'hdf_sd_find_gattr: Global attribute not found!'
	end
end
hdf_sd_close

END


PRO hdf_sd_read_gattr,no,gattr_name,gattr_data,FILE=FILE
COMMON HDF_QUERY_SD_BLOCK,HDF_QUERY_SD,HDF_QUERY_SD_id
;
;  read the no'th global attribute 
;
;            Input: no
;            Output: gattr_name, gattr_data
;
;
;  read the no the global attribute and return  attribute name and data
;

if n_params() lt 1 then begin
	print,'Usage: hdf_sd_read_gattr, no, gattr_name, gattr_data [,FILE=FILE]
	return
	end

if n_elements(HDF_QUERY_SD) gt 0 then $
filename = HDF_QUERY_SD.file
if keyword_set(FILE) ne 0 then filename=string(FILE)
if n_elements(filename) lt 1 then begin
        print,'Error: need filename!'
        return
        end

hdf_sd_open,filename
sd_id = HDF_QUERY_SD.sd_id
NumGAttr = HDF_QUERY_SD.numGAttr

if HDF_QUERY_SD.fid gt 0 then begin

	if no lt 0 or no gt (NumGAttr-1) then begin
		print,'Error: input no out of range!'
		print,'       Total number of global attributes found is',NumGAttr
		hdf_sd_close
		return
	end

	gindex=no
	hdf_sd_attrinfo,sd_id,gindex,name=gattr_name,type=t,count=c,data=gattr_data
	help,sd_id,gindex,gattr_name,t,c
	print,gattr_data
end
hdf_sd_close

END


PRO hdf_sd_write_gattr,gattr_name,gattr_data,FILE=FILE, long=long, $
	double=double,float=float,int=int,short=short,byte=byte
COMMON HDF_QUERY_SD_BLOCK,HDF_QUERY_SD,HDF_QUERY_SD_id
;
;  write global attribute name and data
;        input:  gattr_name,  gattr_data
;

if n_params() lt 2 then begin
	print,'Usage: hdf_sd_write_gattr, gattr_name, gattr_data [,FILE=FILE,/TYPE]
	return
	end

if n_elements(HDF_QUERY_SD) gt 0 then $
filename = HDF_QUERY_SD.file
if keyword_set(FILE) ne 0 then filename=string(FILE)
if n_elements(filename) lt 1 then begin
        print,'Error: need filename!'
        return
        end

hdf_sd_open,filename
sd_id = HDF_QUERY_SD.sd_id

if HDF_QUERY_SD.fid gt 0 then begin

	title=string(gattr_name)

	s = size(gattr_data)
	no = s(0) 
	type = s(n_elements(s)-2)

if keyword_set(double) then type=5
if keyword_set(float) then type=4
if keyword_set(long) then type=3
if keyword_set(int) then type=2
if keyword_set(short) then type=2
if keyword_set(byte) then type=1
CASE type OF
	0: begin
	   print,'Undefined should never happened'
	   end
	1: begin
	   hdf_sd_attrset,sd_id,title,gattr_data,/byte
	   end
	2: begin
	   hdf_sd_attrset,sd_id,title,gattr_data,/short
	   end
	3: begin
	   hdf_sd_attrset,sd_id,title,gattr_data,/long
	   end
	4: begin
	   hdf_sd_attrset,sd_id,title,gattr_data,/float
	   end
	5: begin
	   hdf_sd_attrset,sd_id,title,gattr_data,/double
	   end
	7: begin
	   len = strlen(gattr_data)
  	   hdf_sd_attrset,sd_id,title,gattr_data,len
	   end
ENDCASE
end

hdf_sd_close

END

PRO hdf_sd_dump_attr,no,attr_names,FILE=FILE
COMMON HDF_QUERY_SD_BLOCK,HDF_QUERY_SD,HDF_QUERY_SD_id
;
; dump all the attribute names for the no th SD set : 
;               input:   no 
;              output:   attr_names
;

if n_params() lt 1 then begin
	print,'Usage: hdf_sd_dump_attr, no, attr_names [,FILE=FILE]
	return
	end

if n_elements(HDF_QUERY_SD) gt 0 then $
filename = HDF_QUERY_SD.file
if keyword_set(FILE) ne 0 then filename=string(FILE)
if n_elements(filename) lt 1 then begin
        print,'Error: need filename!'
        return
        end

hdf_sd_open,filename
sd_id = HDF_QUERY_SD.sd_id
NumSDS = HDF_QUERY_SD.numSDS

if HDF_QUERY_SD.fid gt 0 then begin

	if no lt 0 or no gt (NumSDS-1) then begin
		print,'Error: input no out of range!'
		print,'Allowable index number must be less than',NumSDS
		hdf_sd_close
		return
	end

	sds_id= HDF_SD_SELECT(sd_id,no)
	hdf_sd_getinfo,sds_id,natts=na,unit=un,type=ty,coordsys=co,label=la, $
		ndims=nd, dims=di, format=fo, noreverse=re, $
		 range=ra, fill=fi, $
		 caldata=ca
print,''
print,'NATTS',na
	
	if na gt 0 then begin
	attr_names=make_array(na,/string)
	for dindex=0,na-1 do begin
	hdf_sd_attrinfo,sds_id,dindex,name=n,type=t,count=c,data=attr_data
	attr_names(dindex)=n
	help,no,sds_id,dindex,n,t,c
	print,attr_data
	end
	endif else print,'No attribute found for this SD set!' 

	HDF_SD_ENDACCESS,sds_id
end

hdf_sd_close

END

PRO hdf_sd_ref_find_attr,sd_id,refno,attr_name,attr_data , INDEX=INDEX
;
; find the named attribute data in the refno SD set : 
;               input:   sd_id, refno, attr_name
;              output:   attr_data
;

if n_params() lt 3 then begin
	print,'Usage: hdf_sd_ref_find_attr, sd_id, refno, attr_name, attr_data [,INDEX=INDEX]
	print,''
	print,'     Input :    sd_id, refno, attr_name
	print,'     Output:    attr_data [, INDEX=INDEX]
	return
	end

index = HDF_SD_REFTOINDEX(sd_id,refno)
sds_id = HDF_SD_SELECT(sd_id,index)

CATCH, error_status
if error_status ne 0 then begin
	print,!err_string
 	HDF_SD_ENDACCESS,sds_id	
	retall
end

dindex = HDF_SD_ATTRFIND(sds_id,string(attr_name))
if dindex ge 0 then begin
HDF_SD_ATTRINFO,sds_id,dindex,name=n,type=t,count=c,data=attr_data
print,attr_data
end
HDF_SD_ENDACCESS,sds_id
END


PRO hdf_sd_find_attr,no,attr_name,attr_data ,FILE=FILE
COMMON HDF_QUERY_SD_BLOCK,HDF_QUERY_SD,HDF_QUERY_SD_id
;
; find the named attribute data in the no th SD set : 
;               input:   no, attr_name
;              output:   attr_data
;

if n_params() lt 2 then begin
	print,'Usage: hdf_sd_find_attr, no, attr_name, attr_data [,FILE=FILE]
	return
	end

if n_elements(HDF_QUERY_SD) gt 0 then $
filename = HDF_QUERY_SD.file
if keyword_set(FILE) ne 0 then filename=string(FILE)
if n_elements(filename) lt 1 then begin
        print,'Error: need filename!'
        return
        end

hdf_sd_open,filename
NumSDS = HDF_QUERY_SD.numSDS
sd_id = HDF_QUERY_SD.sd_id

if HDF_QUERY_SD.fid gt 0 then begin

	if no lt 0 or no gt (NumSDS-1) then begin
		print,'Error: input no out of range!'
		print,'Allowable index number must be less than',NumSDS
		hdf_sd_close
		return
	end

CATCH, error_status
if error_status ne 0 then begin
	print,!err_string
	hdf_sd_close
	retall
end
	sds_id= HDF_SD_SELECT(sd_id,no)

	dindex = HDF_SD_ATTRFIND(sds_id,string(attr_name))
	HDF_SD_ATTRINFO,sds_id,dindex,name=n,type=t,count=c,data=attr_data
	help,sds_id,dindex,n,t,c
	print,attr_data
	HDF_SD_ENDACCESS,sds_id
end

hdf_sd_close

END

PRO hdf_sd_read_attr, no, attr_name, attr_data ,FILE=FILE
if n_params() lt 2 then begin
	print,'Usage: hdf_sd_read_attr, no, attr_name, attr_data [,FILE=FILE]
	return
	end

	hdf_sd_find_attr,no,attr_name,attr_data, FILE=FILE
END

PRO hdf_sd_write_attr, no, attr_name, attr_data, FILE=FILE 
COMMON HDF_QUERY_SD_BLOCK,HDF_QUERY_SD,HDF_QUERY_SD_id
;
; add a data attribute to the n'th HDF SD data : input  no, attr_name, attr_data
;

if n_params() lt 3 then begin
	print,'Usage: hdf_sd_write_attr, no, attr_name, attr_data [,FILE=FILE]
	return
	end


if n_elements(HDF_QUERY_SD) gt 0 then $
filename = HDF_QUERY_SD.file
if keyword_set(FILE) ne 0 then begin
        filename=string(FILE)
        found=findfile(filename)
        if found(0) eq '' then hdf_create,filename
end

hdf_sd_open,filename
sd_id = HDF_QUERY_SD.sd_id
NumSDS = HDF_QUERY_SD.numSDS

if HDF_QUERY_SD.fid gt 0 then begin

	if no lt 0 or no gt (NumSDS-1) then begin
		print,'Error: input no out of range!'
		print,'Allowable index number must be less than',NumSDS
		hdf_sd_close
		return
	end

;CATCH, error_status
;if error_status ne 0 then begin
;	print,!err_string
;	hdf_sd_close
;	retall
;end
	sds_id= HDF_SD_SELECT(sd_id,no)
print,no,sd_id,sds_id
;ref=HDF_SD_IDTOREF(sds_id)
	hdf_sd_write_dattr,sds_id,string(attr_name),attr_data
	HDF_SD_ENDACCESS,sds_id
end

hdf_sd_close

END

PRO hdf_sd_ref_write_dattr,sd_id,refno,attr_name,attr_data, _Extra=extra
;
;  write data attribute to a know ref SD dataset : 
;
;           input  ref,attr_name,attr_data
;
if n_params() lt 4 then begin
	print,'Usage: hdf_sd_ref_write_dattr, sd_id, refno, attr_name, attr_data
	print,''
	print,'       Write  attribute name and data to an opened sd_id with known refno'
	print,''
	return
	end

index = HDF_SD_REFTOINDEX(sd_id,refno)
sds_id = HDF_SD_SELECT(sd_id,index)
hdf_sd_write_dattr,sds_id,attr_name,attr_data, _Extra=extra
HDF_SD_ENDACCESS,sds_id

END

PRO hdf_sd_write_dattr, sds_id,attr_name, attr_data, long=long, $
	double=double,float=float,int=int,short=short,byte=byte
;
;  write data attribute to an epen HDF SD dataset : 
;
;           input  sds_id,attr_name,attr_data
;

if n_params() lt 3 then begin
	print,'Usage: hdf_sd_write_dattr, sds_id, attr_name, attr_data
	print,''
	print,'       Write  attribute name and data to an opened sds_id'
	print,''
	return
	end

if n_elements(attr_name) ne 0 then title = string(attr_name)

	s = size(attr_data)
	no = s(0) 
	type = s(n_elements(s)-2)

if keyword_set(double) then type=5
if keyword_set(float) then type=4
if keyword_set(long) then type=3
if keyword_set(int) then type=2
if keyword_set(short) then type=2
if keyword_set(byte) then type=1
CASE type OF
	0: begin
	   print,'Undefined should never happened'
	   end
	1: begin
	   hdf_sd_attrset,sds_id,title,attr_data,/byte
	   end
	2: begin
	   hdf_sd_attrset,sds_id,title,attr_data,/short
	   end
	3: begin
	   hdf_sd_attrset,sds_id,title,attr_data,/long
	   end
	4: begin
	   hdf_sd_attrset,sds_id,title,attr_data,/float
	   end
	5: begin
	   hdf_sd_attrset,sds_id,title,attr_data,/double
	   end
	7: begin
	   len = strlen(attr_data)
  	   hdf_sd_attrset, sds_id, title, attr_data, len
	   end
ENDCASE

END

PRO HDF_SD
print,'         *******Additional HDF_SD Routines********'
print,'Note:  If non zero of !err returns, error detected'
print,''
print,'HDF_SD_DUMP            Dump the header of all SD data for a HDF file'
print,''
print,'HDF_SD_READ            Read the n th SD data from the HDF file'
print,''
print,'HDF_CREATE             Create a new empty HDF file '
print,'HDF_SD_FIND            Find the named SD data from the HDF file '
print,'HDF_SD_WRITE           Write a new SD data to the HDF file'
print,'HDF_SD_WRITE           Write a new SD data to the HDF file'
print,''
print,'HDF_SD_DUMP_GATTR      Dump all the global attributes for the HDF file'
print,''
print,'HDF_SD_FIND_GATTR      Find the named global attribute from the HDF file'
print,''
print,'HDF_SD_READ_GATTR      Read the n th global attribute from the HDF file'
print,''
print,'HDF_SD_WRITE_GATTR     Add a new global attribute to the HDF file'
print,''
print,'HDF_SD_DUMP_ATTR       Dump all the attributes for the n th SD set'
print,''
print,'HDF_SD_FIND_ATTR       Find the named attribute for the n th SD set'
print,''
print,'HDF_SD_REF_FIND_ATTR   Find the named attribute for the refno SD set'
print,''
print,''
print,'HDF_SD_READ_ATTR       Same as HDF_SD_FIND_ATTR'
print,''
print,'HDF_SD_WRITE_ATTR      Add a new attribute to the n th SD set'
print,'
print,'HDF_SD_REF_WRITE_ATTR  Add a new attribute to the refno SD set'
print,''
print,'HDF_VG_DUMP            Dump top level VGs '
print,''
print,'HDF_WRITE_SCAN	      Example of creating a grouped Scan SDs'
print,''
print,'HDF_VG_FIND            Find the named VGroup from the HDF file'
END

PRO hdf_addVdata,ref,data,type,dim,name=name,class=class,field=field,FILE=FILE
;
;   if we want the SD shown as in VDATA then call this
;   function will create a VDATA entry 
;
;   The VGROUP is comment out if we want to add the VDATA 
;   to the group
;

if n_params() lt 2 then begin
	print,'Usage: hdf_addVdata,ref,data,name=name,class=class,field=field
	return
	end

if n_elements(HDF_QUERY_SD) gt 0 then $
filename = HDF_QUERY_SD.file
if keyword_set(FILE) ne 0 then begin
        filename=string(FILE)
        found=findfile(filename)
        if found(0) eq '' then hdf_create,filename
end

group=0
g_name='Unknown'
g_class='Unknown'
d_field='Unknown'
d_name='Unknown'
d_class='Var0.0'
d_order= dim(0)

if keyword_set(name) then d_name = string(name)
if keyword_set(field) then d_field = string(field)
if keyword_set(class) then d_class = string(class)


file_id=HDF_OPEN(filename,/RDWR)

if file_id gt 0 then begin
sd_tag=720 ; SD

;
; add vdata
;
vd_id = HDF_VD_ATTACH(file_id,-1,/WRITE)
CASE type OF
	1: begin
		HDF_VD_FDEFINE,vd_id,d_field,/byte,order=d_order
	   end
	2: begin
		HDF_VD_FDEFINE,vd_id,d_field,/int
	   end
	3: begin
		HDF_VD_FDEFINE,vd_id,d_field,/long
	   end
	4: begin
		HDF_VD_FDEFINE,vd_id,d_field,/float
	   end
	5: begin
		HDF_VD_FDEFINE,vd_id,d_field,/double
	   end
	else: begin
		print,'HDF_ADDVDATAVGROUP Error: not supported',type
	   end
ENDCASE
	HDF_VD_SETINFO,vd_id,class=d_class,name=d_name,/full
	HDF_VD_WRITE,vd_id,d_field,data
	HDF_VD_DETACH,vd_id


;
; close
;
HDF_CLOSE,file_id
end

END

PRO hdf_vg_dump,ref,FILE=FILE,PRINT=PRINT,HELP=HELP
COMMON HDF_QUERY_SD_BLOCK,HDF_QUERY_SD,HDF_QUERY_SD_id
; dump the top level groups for a given file

if keyword_set(help) then begin
        print,'
        print,'Usage: hdf_vg_dump,gindex,FILE=FILE,PRINT=PRINT,/HELP
        print,'
	print,' OUTPUT:
	print,'    gindex             - array of group indecis 
        print,' KEYWORD:
        print,'    FILE="filename"    - specify the HDF file name'
        print,'    /PRINT             - list the group information
        return
        end

if n_elements(HDF_QUERY_SD) gt 0 then $
filename = HDF_QUERY_SD.file
if keyword_set(FILE) ne 0 then filename=string(FILE)
if n_elements(filename) lt 1 then begin
        print,'Error: need filename!'
        return
        end

fid = HDF_OPEN(filename,/READ)
if fid gt 0 then begin

; find no of vg

numVG=0
vg = HDF_VG_GETID(fid,-1)
ref = vg 
while vg gt 0 do begin
	numVG = numVG + 1
	if numVG gt 1 then ref = [ref,vg]
	vg = HDF_VG_GETID(fid,vg)
	end

;  find vg name and class
if keyword_set(print) ne 0 then begin
   for i=0,numVG-1 do begin
	vg = HDF_VG_GETID(fid,vg)
	vg_id = HDF_VG_ATTACH(fid,vg)

	if vg_id gt 0 then begin

	HDF_VG_GETTRS,vg_id,tags,refs
	HDF_VG_GETINFO,vg_id,class=cl,name=nm,nentries=no
	print,'Name=',nm,',  Class=',cl,',  Index=',strtrim(i+1,2), $
		',  Nent=',strtrim(no,2)
print,'      Tags',tags
print,'      Refs',refs
print,'-----------------------------------------------------------------------------'
	end
	HDF_VG_DETACH,vg_id
   end
	print,'G_REF',ref
endif else begin
	print,'numVG=',numVG
end

HDF_CLOSE,fid
end

END

PRO hdf_write_scan,parent_vg_id,scan_no=scan_no,pvname=pvname,FILE=FILE
COMMON HDF_QUERY_SD_BLOCK,HDF_QUERY_SD,HDF_QUERY_SD_id
; create a new group in file

if keyword_set(scan_no) eq 0 or keyword_set(pvname) eq 0 then begin
	print,'Usage: hdf_write_scan,scan_no=scan_no, pvname=pvname [,FILE=FILE]
	print,''
	print,' INPUT:  
	print,'   parent_vg_id - optional,specify the parent group id 
	print,''
	print,' KEYWORD: 
	print,'    scan_no     - required, specify scan id
	print,'    pvname      - required, specify scan pvname class 
	print,' 
	return
	end


if n_elements(HDF_QUERY_SD) gt 0 then $ filename = HDF_QUERY_SD.file
if keyword_set(FILE) ne 0 then filename=string(FILE)
if n_elements(filename) eq 0 then begin
        print,'Error: need filename!'
	print,filename
        return
        end

MAX_ID=100  ;	max no of SD in a group set
sid = make_array(MAX_ID,/int)

fid = HDF_OPEN(filename,/ALL)
if fid gt 0 then begin

sd_id = HDF_SD_START(filename,/RDWR)

;
; create the scan 'entry#' group
;

entry_name = 'entry'+strtrim(scan_no,2)
entry_class = 'APS_entry'

vg_tag = 1965 ; VG

vg_id = HDF_VG_ATTACH(fid,-1,/WRITE)
HDF_VG_SETINFO,vg_id,name=entry_name,class=entry_class

date = catimestamp('chademoai1')

hdf_write_data,sd_id,name='date',byte(strmid(date,0,12)),index,ref
HDF_VG_ADDTR,vg_id,vg_tag,ref

hdf_write_data,sd_id,name='hour',byte(strmid(date,13,8)),index,ref
HDF_VG_ADDTR,vg_id,vg_tag,ref

hdf_write_data,sd_id,name='user_name',byte(strupcase(getenv('USER'))),index,ref
HDF_VG_ADDTR,vg_id,vg_tag,ref

hdf_write_data,sd_id,name='entry_analysis',byte('NONE'),index,ref
HDF_VG_ADDTR,vg_id,vg_tag,ref

hdf_write_data,sd_id,name='entry_intent',byte('calibration'),index,ref
HDF_VG_ADDTR,vg_id,vg_tag,ref

hdf_write_data,sd_id,name='comment',byte('any comment'),index,ref
HDF_VG_ADDTR,vg_id,vg_tag,ref


HDF_VG_DETACH,vg_id


;
; create the scan 'data1' group
;

data_class = 'APS_scan'
data_name  = 'data1'

vg_id = HDF_VG_ATTACH(fid,-1,/WRITE)
HDF_VG_SETINFO,vg_id,name=data_name,class=data_class

sd_tag = 720
;vg_tag = 1965 ; VG

;  example of writing mono_energy
 
hdf_write_data,sd_id,name='mono_energy',20.99,index,ref, $
	label = 'energy', unit = 'keV', format='%.5', $
	attr_name='axis', attr_data=1
HDF_VG_ADDTR,vg_id,vg_tag,ref

;  example of writing ic1 

ic1 = make_array(501,value=200000)
gain1 = 1.e8

hdf_write_data,sd_id,name='ic1',ic1,index,ref, $
	label = 'monitor', unit = 'photons', format='%.5', $
	attr_name = 'gain', attr_data = gain1, $
	fill = -1
hdf_sd_ref_write_dattr,sd_id,ref,'I_monitor',1
HDF_VG_ADDTR,vg_id,vg_tag,ref

;  example of writing ic2 

ic2 = indgen(501)
ic2 = 100000 + 2 * ic2
gain2 = 1.e8

hdf_write_data,sd_id,name='ic2',ic2,index,ref, $
	label = 'detector', unit = 'photons', format='%.5', $
	fill = -1
hdf_sd_ref_write_dattr,sd_id,ref,'gain',gain2
hdf_sd_ref_write_dattr,sd_id,ref,'signal',1
HDF_VG_ADDTR,vg_id,vg_tag,ref

if keyword_set(scan_no) then sd_name = entry_name 
if keyword_set(pvname) then sd_class = string(pvname)

x = sin(indgen(20)*!pi/10)
y = cos(indgen(20)*!pi/10)

fd1 = 'x'
fd2 = 'y'
name1= sd_name+'_'+fd1
name2= sd_name+'_'+fd2


hdf_write_data, sd_id, x, index, ref, name=name1, $
	 attr_name=sd_name, attr_data=sd_class 
HDF_VG_ADDTR,vg_id,vg_tag,ref

hdf_write_data, sd_id, y, index, ref, name=name2, $
	 attr_name=sd_name, attr_data=sd_class 
HDF_VG_ADDTR,vg_id,vg_tag,ref


HDF_VG_GETTRS,vg_id,tags,refs
print,'TAGS',tags
print,'Refs',refs

if n_elements(parent_vg_id) gt 0 then begin
	HDF_VG_INSERT,parent_vg_id,vg_id
;	add the vg_id to the parent
end

HDF_VG_DETACH,vg_id
HDF_SD_END,sd_id
HDF_CLOSE,fid
end


END

PRO hdf_vg_find,gindex,name=name,class=class,startno=startno,FILE=FILE,ALL=ALL
COMMON HDF_QUERY_SD_BLOCK,HDF_QUERY_SD,HDF_QUERY_SD_id

; find a named group from the HDF file
; gindex = -1 not found

if keyword_set(name) eq 0 then begin
	print,'
	print,'Usage: hdf_vg_find,gindex, name=name, /ALL $'
	print,'              [, class=class, startno=startno, FILE=FILE]
	print,'
	print,' INPUT:
	print,'    NAME=name_string   - specify the search group name
	print,' OUTPUT: 
	print,'    gindex             - gives the index for the found group(s),
	print,'                         if -1 not found
	print,' KEYWORD:
	print,'    FILE="filename"    - specify the HDF file name'
	print,'    CLASS=class_string - specify the search group class
	print,'    STARTNO=n          - specify the search starting group number
	print,'    /ALL               - find all the matched groups
	return
	end

if n_elements(HDF_QUERY_SD) gt 0 then $
filename = HDF_QUERY_SD.file
if keyword_set(FILE) ne 0 then filename=string(FILE)
if n_elements(filename) lt 1 then begin
        print,'Error: need filename!'
        return
        end

fid = HDF_OPEN(filename,/READ)

if fid gt 0 then begin

hdf_vg_dump,ref,file=filename
no = n_elements(ref)

gindex = -1
is = 0
if keyword_set(startno) gt 0 then is = fix(startno)

n_found = 0
for i=is,no-1 do begin
	vg_id = HDF_VG_ATTACH(fid,ref(i))
	HDF_VG_GETINFO,vg_id,name=nm,class=cl
	if keyword_set(name) then found = strpos(nm,name) else found = 0
	if keyword_set(class) then found1 = strpos(cl,class) else found1 = 0
	if found ne -1 and found1 ne -1 then begin
		print,i,vg_id,'name=',nm,'  class=',cl
		n_found = n_found + 1
		if n_found eq 1 then gindex = ref(i) else $
			gindex = [gindex,ref(i)]
	if keyword_set(all) eq 0 then begin
		HDF_VG_DETACH,vg_id
		hdf_close,fid
		gindex = ref(i)
		return
		end
	end
	HDF_VG_DETACH,vg_id
end

hdf_close,fid
end

END


PRO hdf_create_group,vg_id,name=name,class=class,tag=tag,ref=ref,FILE=FILE
COMMON HDF_QUERY_SD_BLOCK,HDF_QUERY_SD,HDF_QUERY_SD_id
; create a new group in file

if keyword_set(name) eq 0 or keyword_set(class) eq 0 then begin
        print,'Usage: hdf_create_group,vg_id, name=name, class=class [,FILE=FILE]
        return
        end

if n_elements(HDF_QUERY_SD) gt 0 then $
filename = HDF_QUERY_SD.file
if keyword_set(FILE) ne 0 then filename=string(FILE)
if n_elements(filename) lt 1 then begin
        print,'Error: need filename!'
        return
        end
fid = HDF_OPEN(filename,/ALL)
if fid gt 0 then begin

	vg_id = HDF_VG_ATTACH(fid,-1,/WRITE)
	HDF_VG_SETINFO,vg_id,name=name,class=class

	;sd_tag = 720
	sd_tag = 1965 ; VG
	sd_ref = 1 ; VG
	if keyword_set(tag) then sd_tag = tag
	if keyword_set(ref) then sd_ref = ref 

	HDF_VG_ADDTR,vg_id,sd_tag,sd_ref

	HDF_VG_DETACH,vg_id

hdf_close,fid
end
END

	
PRO hdf_write_data, sd_id, data, index, ref, name=name, attr_name=attr_name,$
    attr_data=attr_data, field=field, range=range, unit=unit, format=format, $
    coordsys=coordsys, label=label, caldata=caldata, fill=fill

if n_params() lt 1 then begin
	print,'Usage:  hdf_write_data,sd_id,data,index,ref,name=name'
	print,' INPUT:
	print,'   sd_id		- opened for sd access
	print,'   data          - SD data to be written
	print,' OUTPUT:
	print,'   index         - created SD dataset index number 
	print,'   ref           - SD reference number
	print,' KEYWORD:
	print,'    name         - required input,
	print,'                   specify the variable name for SD data
	print,'   attr_name	- string
	print,'   attr_data	- string
	print,'   field         - string
	print,'   range	        - [min,max]
	print,'   unit          - string
	print,'   format        - string
	print,'   coordsys      - string
	print,'   label         - string
	print,'   caldata       - calibration array
	print,'   fill          - fill data
	return
	end

;  write data

varName='Name'
if n_elements(name) ne 0  and strlen(name) gt 0 then varName=string(name)

	s = size(data)
	num = n_elements(s)
	type = s(num-2)

	no = s(0) 
	if no gt 0 then dim = s(1:no) else dim = [s(num-1)]

CASE type OF
	0: begin
	   print,'Undefined should never happened'
		return
	   end
	1: begin
	   sds_id = HDF_SD_CREATE(sd_id,varName,dim,/BYTE)
	   end
	2: begin
	   sds_id = HDF_SD_CREATE(sd_id,varName,dim,/SHORT)
	   end
	3: begin
	   sds_id = HDF_SD_CREATE(sd_id,varName,dim,/LONG)
	   end
	4: begin
	   sds_id = HDF_SD_CREATE(sd_id,varName,dim,/FLOAT)
	   end
	5: begin
	   sds_id = HDF_SD_CREATE(sd_id,varName,dim,/DOUBLE)
	   end
else: begin
	print,'HDF_SD_WRITE Error: type not supported ',type
	return
	end
ENDCASE

	if keyword_set(caldata) ne 0 then $
		HDF_SD_SETINFO,sds_id,caldata=caldata

	if keyword_set(range) ne 0 then $
		HDF_SD_SETINFO,sds_id,range=range 
	
	if keyword_set(label) ne 0 then begin
		if strlen(label) gt 0 then $
		HDF_SD_SETINFO,sds_id,label=label
		end

	if keyword_set(format) ne 0 then begin
		if strlen(format) gt 0 then $
		HDF_SD_SETINFO,sds_id,format=format
		end

	if keyword_set(unit) ne 0 then begin
		if strlen(unit) gt 0 then $
		HDF_SD_SETINFO,sds_id,unit=unit
		end

	if keyword_set(coordsys) ne 0 then begin
		if strlen(coordsys) gt 0 then $
		HDF_SD_SETINFO,sds_id,coordsys=coordsys
		end

	if keyword_set(fill) ne 0 then begin
		if strlen(fill) gt 0 then $
		HDF_SD_SETINFO,sds_id,fill=fill
		end

;
; if attribute data is assigned
;

if keyword_set(attr_name) ne 0 and keyword_set(attr_data) ne 0 then begin
	no1 = n_elements(attr_name)
	no2 = n_elements(attr_data)
	for i=0,no1<no2-1 do begin
	if strlen(attr_name(i)) gt 0 and strlen(attr_data(i)) gt 0 then $
	HDF_SD_ATTRSET,sds_id,string(attr_name(i)), attr_data(i)
	end
end

	HDF_SD_ADDDATA,sds_id,data

	ref = HDF_SD_IDTOREF(sds_id)

	index = HDF_SD_REFTOINDEX(sd_id,ref)

	HDF_SD_ENDACCESS,sds_id
END





PRO HDFSDSDATA_Event, Event
COMMON SDS_DATA_BLOCK, sdsData_id
COMMON HDF_QUERY_BLOCK, HDF_Query, HDF_Query_id

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'SDS_DATA_DUMP': BEGIN
        if HDF_Query.numSDS gt 0 then begin
        WIDGET_CONTROL,HDF_Query_id.terminal,BAD_ID=bad
        if HDF_Query_id.terminal eq 0 or bad ne 0 then $
        HDF_Query_id.terminal = CW_TERM(Event.top,TITLE='HDF SDS Header Dump', $
                 XSIZE=80, YSIZE=20, /SCROLL)
        HDF_Query.pause = 0
        DumpHDFData,HDF_Query.file,0
	WIDGET_CONTROL,HDF_Query_id.terminal,BAD_ID=bad,/DESTROY
        HDF_Query_id.terminal = CW_TERM(Event.top,TITLE='HDF SDS Header Dump', $
                 XSIZE=80, YSIZE=20, /SCROLL, filename='hdf_data.txt', $
		rename=HDF_Query.dir+HDF_Query.classname+'_sdsDump.txt',bg_names='Save As...')
        endif else $
                HDF_scrolltext,'Warning: no SDS data available!',60,3
      END

  'SDS_DATA_FIRST': BEGIN
	HDF_Query.seqno = 0
	ReadHDFOneRecord, HDF_Query.file, data, /view
	if HDF_Query.text gt 0 then datatotext,data
	  WIDGET_CONTROL,sdsData_id.seqwid,SET_VALUE='0'
        WIDGET_CONTROL,sdsData_id.slider, SET_VALUE=HDF_Query.seqno
        WIDGET_CONTROL,sdsData_id.list, SET_LIST_SELECT=HDF_Query.seqno
      END
  'SDS_DATA_NEXT': BEGIN
	WIDGET_CONTROL,sdsData_id.seqwid,GET_VALUE=seqno
	recno = fix(seqno(0))+1

	if recno lt HDF_Query.maxno then begin
		HDF_Query.seqno = recno
		ReadHDFOneRecord,HDF_Query.file, data, /view 
		if HDF_Query.text gt 0 then datatotext,data
		WIDGET_CONTROL,sdsData_id.seqwid, SET_VALUE=strtrim(string(recno),2) 
        WIDGET_CONTROL,sdsData_id.slider, SET_VALUE=HDF_Query.seqno
        WIDGET_CONTROL,sdsData_id.list, SET_LIST_SELECT=HDF_Query.seqno
	endif else begin
		HDF_scrolltext,'Warning: wrap to 1st record !',60,3
		WIDGET_CONTROL,sdsData_id.seqwid, SET_VALUE= '0'
	HDF_Query.seqno = 0
	end
      END
  'SDS_DATA_PREV': BEGIN
        WIDGET_CONTROL,sdsData_id.seqwid, GET_VALUE= seqno
        recno = fix(seqno(0)) - 1
        if recno ge 0 then begin
        HDF_Query.seqno = recno
        ReadHDFOneRecord,HDF_Query.file, data, /view 
        if HDF_Query.text gt 0 then datatotext,data
                WIDGET_CONTROL,sdsData_id.seqwid,SET_VALUE= strtrim(string(recno),2)
        WIDGET_CONTROL,sdsData_id.slider, SET_VALUE=HDF_Query.seqno
        WIDGET_CONTROL,sdsData_id.list, SET_LIST_SELECT=HDF_Query.seqno
        endif else begin
                HDF_scrolltext,'Warning: first record reached!',60,3
                WIDGET_CONTROL,sdsData_id.seqwid, SET_VALUE= '0'
                 HDF_Query.seqno = 0
                end
      END
  'SDS_DATA_LAST': BEGIN
        HDF_Query.seqno = HDF_Query.maxno - 1
        ReadHDFOneRecord,HDF_Query.file, data, /view 
        if HDF_Query.text gt 0 then datatotext,data
        WIDGET_CONTROL,sdsData_id.seqwid, SET_VALUE=strtrim(string(HDF_Query.seqno),2) 
        WIDGET_CONTROL,sdsData_id.slider, SET_VALUE=HDF_Query.seqno
        WIDGET_CONTROL,sdsData_id.list, SET_LIST_SELECT=HDF_Query.seqno
      END
  'SDS_DATA_SEQNO': BEGIN
      WIDGET_CONTROL,sdsData_id.seqwid, GET_VALUE=seqno
        seqno = fix(seqno(0))
        if seqno gt 0 and seqno lt HDF_Query.maxno then begin
        WIDGET_CONTROL,sdsData_id.slider, SET_VALUE=seqno
        WIDGET_CONTROL,sdsData_id.list, SET_LIST_SELECT=seqno
      HDF_Query.seqno = fix(seqno)
        ReadHDFOneRecord,HDF_Query.file, data, /view
        if HDF_Query.text gt 0 then datatotext,data
        end
      END
  'SDS_DATA_SLIDER': BEGIN
        WIDGET_CONTROL,sdsData_id.slider, GET_VALUE=seqno
        WIDGET_CONTROL,sdsData_id.seqwid, SET_VALUE=strtrim(string(seqno),2)
        WIDGET_CONTROL,sdsData_id.slider, SET_VALUE=seqno
        WIDGET_CONTROL,sdsData_id.list, SET_LIST_SELECT=seqno
        HDF_Query.seqno = seqno
        ReadHDFOneRecord,HDF_Query.file, data, /view
        if HDF_Query.text gt 0 then datatotext,data
      END
  'SDS_DATA_NAME': BEGIN
        HDF_Query.seqno = Event.index 
        WIDGET_CONTROL,sdsData_id.seqwid, SET_VALUE= strtrim(Event.index,2)
        WIDGET_CONTROL,sdsData_id.slider, SET_VALUE=HDF_Query.seqno
        ReadHDFOneRecord,HDF_Query.file, data, /view
        if HDF_Query.text gt 0 then datatotext,data
      END
  'SDS_DATA_EXIT': BEGIN
	WIDGET_CONTROL,sdsData_id.base,/DESTROY,BAD_ID=bad
	WIDGET_CONTROL,HDF_Query_id.terminal,/DESTROY,BAD_ID=bad
	WIDGET_CONTROL,HDF_Query_id.textdata,/DESTROY,BAD_ID=bad
	HDF_Query_id.terminal = 0L
	HDF_Query_id.textdata = 0L
	sdsData_id.base = 0L
      END
  ENDCASE
END



PRO HDFSDSDATA_init
COMMON SDS_DATA_BLOCK, sdsData_id
  sdsData_id = {  $
		base : 0L, $
		sdsindex : 0, $
		slider : 0L, $
		list : 0L, $
		seqwid : 0L $
	}

END


PRO HDFSDSDATA,file,no,GROUP=Group
COMMON SDS_DATA_BLOCK, sdsData_id
COMMON HDF_QUERY_BLOCK, HDF_Query, HDF_Query_id

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

if n_elements(no) then begin

if no lt 1 then return

  if n_elements(sdsData_id) eq 0 then HDFSDSDATA_init

        sdsnames = strarr(no)
;	sdsvgname,sdsnames
	for i=0,HDF_Query.numSDS-1 do begin
	sds_id = HDF_SD_SELECT(HDF_Query.sd_id,i)
	HDF_SD_GETINFO,sds_id,NAME=n
	sdsnames(i) = n
	HDF_SD_ENDACCESS,sds_id
	end
	
  if Xregistered('HDFSDSDATA') then $
	WIDGET_CONTROL,sdsData_id.base,/DESTROY,BAD_ID=bad

  sdsData_id.base = 0L 
  sdsData_id.seqwid = 0L 
  sdsData_id.slider = 0L 
  sdsData_id.list = 0L 

  MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
	TITLE='QUERY - SDS ', $
      ROW=1, $
      MAP=1, $
      UVALUE='sdsData_id')

  BASE2 = WIDGET_BASE(MAIN13, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  SDS_DATA_DUMP = WIDGET_BUTTON( BASE2, $
      UVALUE='SDS_DATA_DUMP', $
      VALUE='DumpSDSHeader')

  BASE4 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE4')

  SDS_DATA_FIRST = WIDGET_BUTTON( BASE4, $
      UVALUE='SDS_DATA_FIRST', $
      VALUE='First')

  SDS_DATA_NEXT = WIDGET_BUTTON( BASE4, $
      UVALUE='SDS_DATA_NEXT', $
      VALUE='Next')

  SDS_DATA_PREV = WIDGET_BUTTON( BASE4, $
      UVALUE='SDS_DATA_PREV', $
      VALUE='Prev')

  SDS_DATA_LAST = WIDGET_BUTTON( BASE4, $
      UVALUE='SDS_DATA_LAST', $
      VALUE='Last')

  BASE9 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE9')

  LB0 = WIDGET_LABEL(BASE9,VALUE='SDS #') 
  SDS_DATA_SEQNO = WIDGET_TEXT( BASE9,VALUE='0', $
      /EDITABLE, UVALUE='SDS_DATA_SEQNO', $
      XSIZE=4,YSIZE=1)

if no gt 1 then begin 
  SLIDER11 = WIDGET_SLIDER( BASE9, $
	MIN=0, MAX=no-1, $ 
      UVALUE='SDS_DATA_SLIDER', $
      VALUE=0)
  sdsData_id.slider = SLIDER11
end

  ListVal738 = sdsnames
  LIST4 = WIDGET_LIST( BASE2,VALUE=ListVal738, $
      UVALUE='SDS_DATA_NAME', $
      YSIZE=5)
  if n_elements(sdsnames) gt 1 then $
  WIDGET_CONTROL,LIST4,SET_LIST_TOP=n_elements(sdsnames)-2

  SDS_DATA_EXIT = WIDGET_BUTTON( BASE2, $
      UVALUE='SDS_DATA_EXIT', $
      VALUE='Close')

  sdsData_id.base =  MAIN13
  sdsData_id.seqwid  = SDS_DATA_SEQNO 
  sdsData_id.list =  LIST4

  WIDGET_CONTROL, MAIN13, /REALIZE

  XMANAGER, 'HDFSDSDATA', MAIN13
end
END

; Auto Save File For HDF.pro
;
;  Mon Jul 24 15:10:20 CDT 1995
;


PRO PDMENUSETUP3_Event, Event


  CASE Event.Value OF

  'Setup.Color...': BEGIN
    XLOADCT
    END
  'Setup.Save PVTCT': BEGIN
	TVLCT,red,green,blue,/GET
        save,red,green,blue,file='pvtcolors.dat'
    END
  'Setup.Load PVTCT': BEGIN
	found = findfile('pvtcolors.dat')
        if found(0) eq '' then begin
        str = 'Error: Private color table never been saved before'
        r = dialog_message(str,/error)
        endif else begin
        restore,'pvtcolors.dat'
        TVLCT,red,green,blue
        end
    END
  ENDCASE
END


PRO hdf_checkOutpath,dir

	catch,error_status
	if error_status ne 0 then begin
	cd,current=dir
	dir = dir + !os.file_sep
	end

	openw,unit,dir+'.tmp',/get_lun
	free_lun,unit
	close,unit

;	print,'dir=',dir
END

PRO invoke_HDFSDSDATA,event
COMMON HDF_QUERY_BLOCK, HDF_Query, HDF_Query_id
	WIDGET_CONTROL,HDF_Query_id.an_droplist,SET_DROPLIST_SELECT=3
            DumpSDSAN,HDF_Query.file
            if HDF_Query.maxno gt 0 then $
            HDFSDSDATA, HDF_Query.file, HDF_Query.maxno,GROUP=Event.top
END

PRO HDFSDSDATA_checkfile,F,Event
COMMON HDF_QUERY_BLOCK, HDF_Query, HDF_Query_id

	found = HDF_ISHDF(F)
	
	if found eq 0 then begin
		r = dialog_message(/error,'Error: '+F+ ' is not a HDF file!')
		return
		end

	HDF_Query.file = F

	r = rstrpos(F,!os.file_sep)
	if r gt 0 then begin
	   HDF_Query.classname = strmid(F,r+1,strlen(F)-r) 
	   P = strmid(F,0,r+1)
	endif else begin
	   HDF_Query.classname = F
	   P = ''
	end

	HDF_Query.fpath = P
	dir = p
	hdf_checkOutpath,dir
	HDF_Query.dir = dir

;	print,'File selected=',F
	WIDGET_CONTROL,HDF_Query_id.filename, SET_VALUE=HDF_Query.file

	HDFInitData,file=F
	DumpHDFAN,F,tag=101,dataString=d,/desc
	if n_elements(d) then $
	WIDGET_CONTROL,HDF_Query_id.term,SET_VALUE=d else $
	WIDGET_CONTROL,HDF_Query_id.term,SET_VALUE=''
	WSET,HDF_Query_id.draw1
	erase
	invoke_HDFSDSDATA,Event

	; write config
	openw,1,'hdfb.config'
	printf,1,HDF_Query.file
	close,1
END

PRO HDFSDSDATA_config,file
	file=''
	openr,1,'hdfb.config'
	readf,1,file
	close,1
END

PRO HDFSDSDATA_checkfileSeq,Event,first=first,last=last,next=next,prev=prev
COMMON HDF_QUERY_BLOCK, HDF_Query, HDF_Query_id
	if HDF_Query.fpath eq '' then return 
	r = file_search(HDF_Query.fpath+'*',/mark)
	num = n_elements(r)
	HDF_Query.filenos[1] = num-1 
	if HDF_Query.filenos[0] lt num then HDF_Query.filenos[0] = num 
	for i=0,num-1 do begin
		if r(i) eq HDF_Query.file then goto,seq
	end
seq:
	if keyword_set(next) then begin
		if i lt (num-1) then i = i+1 else i=0
	end
	if keyword_set(prev) then begin
		 if i gt 0 then i = i-1 else i=num-1
	end
	if keyword_set(first) then i = 0
	if keyword_set(last) then i = num-1
	HDF_Query.filenos[1] = i
	HDF_Query.file = r(i)
	HDFSDSDATA_checkfile,r(i),Event
END

PRO PDMENU4_Event, Event
COMMON HDF_QUERY_BLOCK, HDF_Query, HDF_Query_id

  CASE Event.Value OF 


  'File.Open...': BEGIN
	F = DIALOG_PICKFILE(/READ,FILE='4.hdf',PATH=HDF_Query.fpath,GET_PATH=P,FILTER=['*.hdf','*.Nx','*.nx','*.nexus'])
	if strlen(F) lt 1 then begin
		print,'Error: no file selected by you!'
		
		end

	HDFSDSDATA_checkfile,F,Event

    END

  'File.Quit': BEGIN

	WIDGET_CONTROL, event.top, /DESTROY
	exit
    END
  ENDCASE
END


PRO MAIN13_HDF_Event, Event
COMMON HDF_QUERY_BLOCK, HDF_Query, HDF_Query_id

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'HDF_DRAW3': BEGIN
      Print, 'Event for HDF_DRAW3'
      END

  ; Event for PDMENU4
  'PDMENU4': BEGIN
	PDMENU4_Event, Event
	END
  ; Event for PDMENUSETUP3
  'PDMENUSETUP3': BEGIN
	 PDMENUSETUP3_Event, Event
	END

  'HDF_FIRST_FILE': BEGIN
	HDFSDSDATA_checkfileSeq,Event,/first
	END
  'HDF_LAST_FILE': BEGIN
	HDFSDSDATA_checkfileSeq,Event,/last
	END
  'HDF_NEXT_FILE': BEGIN
	HDFSDSDATA_checkfileSeq,Event,/next
	END
  'HDF_PREV_FILE': BEGIN
	HDFSDSDATA_checkfileSeq,Event,/prev
	END
  'HDF_FILENAME': BEGIN
	WIDGET_CONTROL,HDF_Query_id.filename, GET_VALUE=file 
	file = strtrim(file(0),2)

	if strlen(file) gt 1 then begin
	found = findfile(file,count=ct)
	if ct eq 0 then return

	HDFSDSDATA_checkfile,file,Event
	end
      END

  'HDF_SDS_SEQNO': BEGIN
      WIDGET_CONTROL,HDF_Query_id.seqno, GET_VALUE=seqno
	seqno = fix(seqno(0))
	if seqno gt 0 and seqno lt HDF_Query.maxno then begin
      HDF_Query.seqno = fix(seqno)

	ReadHDFOneRecord,HDF_Query.file, data, /view 
	if HDF_Query.text gt 0 then datatotext,data

	end

      END

  'HDF_SDS_SLIDER': BEGIN
      WIDGET_CONTROL,HDF_Query_id.slider, GET_VALUE=seqno
	if seqno eq 0 then seqno=1
      WIDGET_CONTROL,HDF_Query_id.seqno, SET_VALUE=strtrim(string(seqno),2)
      WIDGET_CONTROL,HDF_Query_id.slider, SET_VALUE=seqno
      HDF_Query.seqno = seqno

	ReadHDFOneRecord,HDF_Query.file, data, /view 
	if HDF_Query.text gt 0 then datatotext,data

      Print, 'Event for SDS Slider:',seqno
      END

  'BGROUP3': BEGIN
      IF Event.Select THEN Sel = 1 ELSE Sel = 0 
      CASE Event.Value OF
      0: BEGIN
	HDF_Query.attr = Sel
	HDF_Query.text = Sel
	if Sel eq 0 then  begin
	   WIDGET_CONTROL,HDF_Query_id.textdata,BAD_ID=bad,/DESTROY
	   HDF_Query_id.textdata = 0L
	end
	END
      1: BEGIN
	HDF_Query.view = Sel
	if sel then begin
	  ReadHDFOneRecord,HDF_Query.file, data, /view
	  if HDF_Query.text gt 0 then datatotext,data
	end
	END
      ELSE: Message,'Unknown button pressed'
      ENDCASE
      END

  'BGROUP4': BEGIN
      HDF_Query.byte= Event.Value 
	ReadHDFOneRecord,HDF_Query.file, data, /view 
      if HDF_Query.text gt 0 then datatotext,data
      END

  'HDF_QUERY_PAUSE': BEGIN
	HDF_Query.pause = 1
	print,'Event for pause'
	END
  'HDF_QUERY_RESUME': BEGIN
	HDF_Query.pause = 0
	startno = HDF_Query.seqno
	if HDF_Query.view eq 1 then DumpHDFData,HDF_Query.file,startno,/view $
		 else DumpHDFData,HDF_Query.file,startno
	print,'Event for resume',startno
	END
  'HDF_QUERY_STOP': BEGIN
	HDF_Query.pause = -1
	END

  'ANN_HELP': BEGIN
	st = [ $
		'File Menu    - select HDF/NEXUS file or quit IDL', $
		'Setup Menu   - set the color tables to be used by IDL', $
		'HDF Filename - display HDF/NEXUS files picked', $
		'','File Selection: |>, ->, <-, <|', $
		'  |>    -  Bitmap button for First file',$
		'  ->    -  Bitmap button for Next file',$
		'  <-    -  Bitmap button for Prev file',$
		'  <|    -  Bitmap button for Last file',$
		'', $
		'HDF Info Droplist - select & display HDF info in text area and pops up query dialog', $
		'     Types of popup query dialog: ', $
		'        QUERY - SDS', $
		'        QUERY - VD', $
		'        QUERY - VG', $
		'Help...   - display this help message', $
		'Clear     - clear the scroll text info area', $
		'Text Area - display summary of HDF info for selected HDF Info droplist ', $
		'','Check Options:', $
	      '  Txt Window  - turn on/off popup window for HDF SDS Text ', $
	      '  Plot Window - turn on/off popup window for HDF SDS 1D/2D/3D plot ', $
		'','Byte Array as:', $
	      '  String / Byte - display byte array data as String / Byte', $
		'','Drawing Area - display raw data as 1D/2D/3D graph', $
		'',	'QUERY - SDS Dialog:',$
		'  DumpSDSHeader -  dumps SDS attributes and first 5 elements from the SDS array',$
		'  First - access the first set of SDS',$
		'  Next  - access the next set of SDS',$
		'  Prev  - access the prev set of SDS',$
		'  Last  - access the last set of SDS',$
		'  SDS # - text field display the current seqno of SDS',$
		'  Slider - select the desired seqno of SDS',$
		'  SDS List - select SDS by the attribute list',$
		'(Selected SDS data will be displayed in the drawing area)', $
		'', $
		'For 3D SDS data:',$
		'- Only the first 12 slices (i.e. 3rd rank) will be plotted in the drawing area', $
		'- Up to 100 columns of the first rank can be displayed in text window', $
		'(To access discrete SDS data array, ', $
		'	you may use the itools user interface options, or', $
		'	you may refer the NX object class.)' $
		]
;	r = dialog_message(st,/info,title='HDF Query Help')
xdisplayfile,title='Help on HDFB',text=st,group=Event.top
	END
; ANNOTATION dump

  'ANN_CLEAR': BEGIN
	WIDGET_CONTROL,HDF_Query_id.term,SET_VALUE=''
       END

  'HDF_AN_DROPLIST': BEGIN
	HDF_Query.anlevel = Event.Index
        CASE Event.Index OF
            0: begin
		DumpHDFAN,HDF_Query.file,tag=101,/desc, dataString=d
		if n_elements(d) then $
		WIDGET_CONTROL,HDF_Query_id.term,SET_VALUE=d
               end
            1: begin        ; 8 bit raster image
		no_image=dumpDFR8Info(HDF_Query.file)
		if no_image gt 0 then $
		DFR8Image,HDF_Query.file,no_image,GROUP=Event.top
               end
            2: begin
		no_image=dumpDF24Info(HDF_Query.file)
	       end
            3: begin
		DumpSDSAN,HDF_Query.file
		if HDF_Query.maxno gt 0 then $
		HDFSDSDATA, HDF_Query.file, HDF_Query.maxno,GROUP=Event.top
	       end
            4: begin
		dumpVDATAAN,HDF_Query.file
		if HDF_Query.numVD gt 0 then $
		HDFVD_DATA, HDF_Query.file, HDF_Query.numVD,GROUP=Event.top
	       end
            5: begin
		dumpGROUPAN,HDF_Query.file
		if HDF_Query.numVG gt 0 then $
		HDFVGPDATA, HDF_Query.file, HDF_Query.numVG,GROUP=Event.top
	       end
	ENDCASE
      END



  'HDF_SEARCHUNIT': BEGIN
      Print, 'Event for HDF unit search:'
	WIDGET_CONTROL,HDF_Query_id.search, GET_VALUE=unitstring
	unitstring = strtrim(unitstring(0),2)
	if strlen(unitstring) gt 1 then begin
		HDF_Query.search = unitstring

	; find the record need here
	WIDGET_CONTROL,HDF_Query_id.seqno, GET_VALUE= st 
	recno = fix(st(0))
	if recno lt 1 or recno eq HDF_Query.numSDS then recno = 1
	HDF_Query.seqno = recno

        if recno le HDF_Query.maxno then begin
        HDF_Query.seqno = recno

	hdf_search_unitstring,HDF_Query.file,unitstring,recno,data
	if HDF_Query.text gt 0 then datatotext,data

		if recno eq -1 then return
		WIDGET_CONTROL,HDF_Query_id.seqno, $
			SET_VALUE= strtrim(string(recno),2)
		endif else begin
		HDF_scrolltext,'Warning: last record reached!',60,3
		WIDGET_CONTROL,HDF_Query_id.seqno, SET_VALUE= '0'
        	end
	end
      END

; VD events

  'HDF_QUERY_VD_DUMP': BEGIN
      Print, 'Event for VD Dump'
	DumpHDFVData,HDF_Query.file,0,HDF_Query.numVD - 1
      END
  'HDF_VD_NEXT': BEGIN
	WIDGET_CONTROL,HDF_Query_id.vd_seqno, GET_VALUE= seqno
	recno = fix(seqno(0)) + 1
	if recno le 0 then $
		GetHDFVData, HDF_Query.file, 0 

        if recno lt HDF_Query.numVD then begin
             HDF_Query.vd_seqno = recno
	     if HDF_Query.view gt 0 then $
	     GetHDFVData,HDF_Query.file,recno, data, /view $
	     else GetHDFVData,HDF_Query.file,recno, data
	     WIDGET_CONTROL,HDF_Query_id.vd_seqno, $
			SET_VALUE= strtrim(string(recno),2)
	endif else begin
		HDF_scrolltext,'Warning: last record reached!',60,3
        end

      END

  'HDF_VD_PREV': BEGIN
	WIDGET_CONTROL,HDF_Query_id.vd_seqno, GET_VALUE= seqno
	recno = fix(seqno(0)) - 1

	if recno ge 0 then begin
		HDF_Query.vd_seqno = recno
		if HDF_Query.view gt 0 then $
		GetHDFVData, HDF_Query.file, recno, data, /view $
		else GetHDFVData, HDF_Query.file, recno, data
		WIDGET_CONTROL,HDF_Query_id.vd_seqno, $
			SET_VALUE= strtrim(string(recno),2)
	endif else begin
		HDF_scrolltext,'Warning: first record reached!',60,3
	end
      END

  'HDF_VD_FIRST': BEGIN
	if HDF_Query.view gt 0 then $
	GetHDFVData, HDF_Query.file, 0, data, /view $
	else GetHDFVData, HDF_Query.file, 0, data
	WIDGET_CONTROL,HDF_Query_id.vd_seqno, SET_VALUE='0'
	HDF_Query.vd_seqno = 0
      END

  'HDF_VD_LAST': BEGIN
	seqno = HDF_Query.numVD - 1
	WIDGET_CONTROL,HDF_Query_id.vd_seqno, $
		SET_VALUE=strtrim(string(seqno),2) 
	HDF_Query.vd_seqno = seqno
	if HDF_Query.view gt 0 then $
	GetHDFVData, HDF_Query.file, seqno ,data,/view $
	else GetHDFVData, HDF_Query.file, seqno ,data
      END

  'HDF_VD_SEQNO': BEGIN
      WIDGET_CONTROL,HDF_Query_id.vd_seqno, GET_VALUE=seqno
	seqno = fix(seqno(0))
	if seqno ge 0 and seqno lt HDF_Query.numVD then begin
      HDF_Query.vd_seqno = fix(seqno)
        if HDF_Query.view gt 0 then $
        GetHDFVData, HDF_Query.file, seqno, data, /view $
        else GetHDFVData, HDF_Query.file, seqno, data
	endif else begin
		HDF_scrolltext,'Warning: invalid record entered!',60,3
		WIDGET_CONTROL,HDF_Query_id.vd_seqno, SET_VALUE='0'
		HDF_Query.vd_seqno=0
	end
      END

  'HDF_VD_SLIDER': BEGIN
      WIDGET_CONTROL,HDF_Query_id.vd_slider, GET_VALUE=seqno
      WIDGET_CONTROL,HDF_Query_id.vd_seqno, $
		 SET_VALUE=strtrim(string(seqno),2)
      HDF_Query.vd_seqno = seqno
        if HDF_Query.view gt 0 then $
        GetHDFVData, HDF_Query.file, seqno ,data,/view $
        else GetHDFVData, HDF_Query.file, seqno ,data
      END

  ENDCASE

END


PRO HDFInitData,file=file
COMMON HDF_QUERY_BLOCK, HDF_Query, HDF_Query_id
COMMON HDF_ID_BLOCK,vgroup_ids,vdata_ids,sds_ids

        if HDF_Query.fid ne 0 then begin
                HDF_SD_END, HDF_Query.sd_id
                HDF_CLOSE,HDF_Query.fid
                HDF_Query.sd_id = 0L
                HDF_Query.fid = 0
                end

if keyword_set(file) then begin
;	print,'filename=',file

        fid = HDF_OPEN(file,/READ)
	if fid le 0 then begin
;        HDF_scrolltext,'Error: failed to open '+file+' !',60,3
        return
        end

        HDF_Query.fid = fid
 
	READHDFVG,fid,vdata_ids,vgroup_ids

	HDF_Query.vg_seqno = 0
	HDF_Query.vd_seqno = 0


        HDF_DFSD_GETINFO, file, NSDS=NumSDS

        HDF_Query.numSDS = NumSDS
	HDF_Query.maxno = NumSDS
	HDF_Query.seqno = 0 

	HDF_Query.file = file
        HDF_Query.sd_id = HDF_SD_START(file)
        HDF_Query.numSD = NumSDS
	HDF_Query.attr = 0
	HDF_Query.search = ''
end

END

PRO HDFB, filename, GROUP=Group
COMMON HDF_QUERY_BLOCK, HDF_Query, HDF_Query_id
COMMON HDF_ID_BLOCK,vgroup_ids,vdata_ids,sds_ids

loadct,2

HDF_Query = { $,
        fid :   0L, $
        sd_id : 0L, $
        numVG : 0L, $
        numVD : 0L, $
        numSDS : 0L, $
        numSD : 0L, $
	attr : 0, $
	view : 0, $
	text : 0, $
	byte : 0, $
	filenos : [0,0], $   [# offile,current_seq]
	file : '', $
	fpath : '', $
	dir: '', $
	classname: '', $
	textfile : 'hdf_data.txt', $
	search : '', $
	tname: '', $
	wtime : 0.5, $  ; dump continuously 
	pause : 0, $
	sd_tag : 720, $     ; SD tag
	vg_tag : 1965, $    ; VGroup tag
	vd_tag : 1962, $    ; VData tag
	maxno : 0, $
	vd_seqno : 0, $
	vg_seqno : 0, $
	anlevel: 0, $		; set annotate dump level
	glevel: 0, $		; set group dump level
	seqno : 0 $
	}	

  HDF_Query_id = { $
	base     : 0L, $
	filename : 0L, $
	filebase : 0L, $
	draw1	: 0L, $
	draw_xsize : 0L, $
	draw_ysize : 0L, $
	warning : 0L, $
	term : 0L, $
	vg_term : 0L, $
	vd_term : 0L, $
	an_droplist : 0L, $
	an_term : 0L, $
	terminal: 0L, $
	vg_dump : 0L, $
	vd_dump : 0L, $
	textdata: 0L $
	}

	CD,CURRENT=cur
	HDF_Query.fpath = cur
	HDF_Query.dir = cur

	found = findfile('hdfb.config')
	if found(0) ne '' then begin
	 HDFSDSDATA_config,filename
	r = rstrpos(filename,!os.file_sep)
	if r gt 0 then begin
	   HDF_Query.classname = strmid(filename,r+1,strlen(filename)-r) 
	   P = strmid(filename,0,r+1)
	endif else begin
	   HDF_Query.classname = filename
	   P = ''
	end
	HDF_Query.fpath = P
	end
	
if n_elements(filename) gt 0 then begin	
	found = findfile(filename)
	if found(0) ne '' then begin
	HDFInitData,file=filename

	if HDF_Query.fid gt 0 then begin
	found = HDF_ISHDF(filename)
		if found eq 1 then begin
		HDF_Query.file = filename
		HDF_DFSD_GETINFO, filename, NSDS=NumSDS
		HDF_Query.maxno = NumSDS
		HDF_Query.seqno = 1
		end
	end
	end
end

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  MAIN13_HDF = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
      MAP=1, $
      TITLE='HDF Query', $
      UVALUE='MAIN13_HDF')

  BASE0 = WIDGET_BASE(MAIN13_HDF, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE0')

  BASE0_1 = WIDGET_BASE(BASE0, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE0_1')

  MenuDesc167 = [ $
      { CW_PDMENU_S,       3, 'File' }, $ ;        0
        { CW_PDMENU_S,       0, 'Open...' }, $ ;        1
        { CW_PDMENU_S,       2, 'Quit' } $  ;      2

  ]

  PDMENU4 = CW_PDMENU( BASE0_1, MenuDesc167, /RETURN_FULL_NAME, $
      UVALUE='PDMENU4')

  MenuDescsetup167 = [ $
      { CW_PDMENU_S,       3, 'Setup' }, $ ;        0
        { CW_PDMENU_S,       0, 'Save PVTCT' }, $ ;        1
        { CW_PDMENU_S,       0, 'Load PVTCT' }, $ ;        1
        { CW_PDMENU_S,       0, 'Color...' }, $ ;        1
        { CW_PDMENU_S,       2, '' } $  ;      2
  ]

  PDMENUSETUP3 = CW_PDMENU( BASE0_1, MenuDescsetup167, /RETURN_FULL_NAME, $
      UVALUE='PDMENUSETUP3')

  hdf_filelb1 = WIDGET_LABEL(BASE0_1,VALUE='HDF Filename:')
  HDF_FILENAME = WIDGET_TEXT( BASE0_1,VALUE=HDF_Query.file, $
      YSIZE=1, XSIZE=60, /EDITABLE, $
      UVALUE='HDF_FILENAME')

@vw2d.bm

  BASE_file = WIDGET_BASE(MAIN13_HDF, ROW=1, MAP=1, $
      UVALUE='BASE_FILE')
  hdf_lb1 = WIDGET_LABEL(BASE_file,VALUE='File Selection:')
  BMPBTN14 = WIDGET_BUTTON( BASE_file,VALUE=BMP767, $
      UVALUE='HDF_FIRST_FILE')

  BMPBTN11 = WIDGET_BUTTON( BASE_file,VALUE=BMP686, $
      UVALUE='HDF_PREV_FILE')

  BMPBTN12 = WIDGET_BUTTON( BASE_file,VALUE=BMP688, $
      UVALUE='HDF_NEXT_FILE')

  BMPBTN15 = WIDGET_BUTTON( BASE_file,VALUE=BMP809, $
      UVALUE='HDF_LAST_FILE')

  BASE0 = WIDGET_BASE(MAIN13_HDF, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE0')
; add annotation support

an_droplist_btns = [ $
        'File ID & Desc', $
        'R8 [8-bit Raster Image]', $
        'R24 [24-bit Raster Image]', $
        'Global SDS Attributes', $
        'Global VDATA Attributes', $
        'Global GROUP Attributes' $
        ]
HDF_AN_DROPLIST = WIDGET_DROPLIST(BASE0, VALUE=an_droplist_btns, $
        UVALUE='HDF_AN_DROPLIST', FRAME=2, TITLE='File Annotation / Raster Image / HDF Info:')
WIDGET_CONTROL, HDF_AN_DROPLIST, SET_DROPLIST_SELECT=HDF_Query.anlevel

  ANN_HELP = WIDGET_BUTTON( BASE0, $
      UVALUE='ANN_HELP', $
      VALUE='Help...')
; annotation
  ANN_CLEAR = WIDGET_BUTTON( BASE0, $
      UVALUE='ANN_CLEAR', $
      VALUE='Clear')
  ANN_TEXT = WIDGET_TEXT( MAIN13_HDF,VALUE='', $
      UVALUE='ANN_TEXT', /SCROLL, $
      XSIZE=70, $
      YSIZE=10)

; check options 

  BASE1 = WIDGET_BASE(MAIN13_HDF, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE1')

  Btns266 = [ $
    'Txt Window ', $
    'Plot Window']
  BGROUP3 = CW_BGROUP( BASE1, Btns266, $
      ROW=1, $
      NONEXCLUSIVE=1, $
      LABEL_LEFT='Check Options:', $
      UVALUE='BGROUP3')
;  WIDGET_CONTROL,BGROUP3,SET_VALUE=[0,1]
;  HDF_Query.view = 1

  ByteOString= [ $
    'String', $
    'Byte']
  BGROUP4 = CW_BGROUP( BASE1, ByteOString, $
      ROW=1, /FRAME, $
      EXCLUSIVE=1, $
      LABEL_LEFT='       Byte Array as:', $
      UVALUE='BGROUP4')
	WIDGET_CONTROL,BGROUP4,SET_VALUE=0

  DRAW_BASE2 = WIDGET_BASE(MAIN13_HDF, $
      ROW=1, $
      MAP=1, $
      UVALUE='DRAW_BASE2')

  draw_xsize=400
  draw_ysize=200
  HDF_DRAW3 = WIDGET_DRAW( DRAW_BASE2, $
      RETAIN=1, $
      UVALUE='HDF_DRAW3', $
      XSIZE=draw_xsize, $
      YSIZE=draw_ysize)

  !p.multi = [0,2]


	HDF_Query_id.base = MAIN13_HDF
 	HDF_Query_id.filename = HDF_FILENAME
 	HDF_Query_id.filebase = BASE_FILE
 	HDF_Query_id.draw_xsize  = draw_xsize
 	HDF_Query_id.draw_ysize  = draw_ysize
 	HDF_Query_id.term = ANN_TEXT
 	HDF_Query_id.an_droplist = HDF_AN_DROPLIST
 
  WIDGET_CONTROL, MAIN13_HDF, /REALIZE

  ; Get drawable window index

  COMMON HDF_DRAW3_Comm, HDF_DRAW3_Id
  WIDGET_CONTROL, HDF_DRAW3, GET_VALUE=HDF_DRAW3_Id
 	HDF_Query_id.draw1 = HDF_DRAW3_Id

  XMANAGER, 'MAIN13_HDF', MAIN13_HDF,/no_block
;  XMANAGER, 'MAIN13_HDF', MAIN13_HDF, BLOCK=1
END
