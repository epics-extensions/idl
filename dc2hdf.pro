;
;  To save platform independent data use the /XDR option on file open
;
;  u_openw, unit [,/XDR]  [,'filename']
;  u_openr, unit [,/XDR]  [,'filename']
;  u_write 
;  u_read
;  u_dump
;  u_rewind, unit
;
; Examples: 
;  u_openw, unit, 'filename'
;  u_write, unit, x
;  u_close, unit
;
;
;  u_dump
;
;  u_openr, unit, 'filename'
;  u_read, unit, x  &  print,x
;  u_close, unit
;

;PRO DebugError
;help,/struct,!error_state
;END

FUNCTION u_writePermitted,filename
;
; check for filename write permission
;
; existed
	found = findfile(filename)
	if found(0) ne '' then begin
	ret= dialog_message(['Do you want to overwrite the existed file : ',$
		'','     '+filename], $
			/question)
	if strupcase(ret) eq 'NO' then return,-1
	end
; create new
        CATCH,error_status
;        if !error_state.name eq 'IDL_M_CNTOPNFIL' then begin 
	if error_status eq -215 or error_status eq -206 then begin
                ret=WIDGET_MESSAGE(!err_string + string(!err))
                if n_elements(unit) then u_close,unit
                return,-1
        end
	openw,1,filename
	close,1
	return,0
END

PRO u_rewind,unit
;+
; NAME:
;       U_REWIND
;
; PURPOSE:
;       This routine locates the LUN file pointer at the beginning of the 
;       file.
;
; CALLING SEQUENCE:
;
;       U_REWIND, Unit
;
; INPUTS:
;       Unit:     The LUN number to be rewind.
;
; OUTPUTS:
;       None.
;
; EXAMPLE:
;
;        U_REWIND, unit
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       07-25-97      bkc  Rename routine rewind to u_rewind     
;-
point_lun,unit,0
END

PRO u_openw,unit,filename,append=append,help=help,XDR=XDR,ERRCODE
;+
; NAME:
;       U_OPENW
;
; PURPOSE:
;       This routine assigns a LUN to the opened file for unformatted
;       write only.
;
; CALLING SEQUENCE:
;
;       U_OPENW, Unit, 'filename' [,/Append] [,/XDR] [,/Help]
;
; INPUTS:
;       filename: Specifies the filename to be created or opened for
;                 data recording through using the U_WRITE command.
;
; OUTPUTS:
;       Unit:     The LUN number to be associated with the opened file.
;
; KEYWORD PARAMETERS:
;       APPEND:   This keyword specifies that the file is opened for 
;                 data appending. If not specified, write on the unit
;                 will replace the old file content by the new data.
;       XDR:      This keyword specifies that the file is opened for 
;                 writing data in platform-independent XDR binary form. 
;       HELP:     If this keyword is set, a simple on line help is given.
;
; RESTRICTIONS:
;       The data file should contain only consistant type of binary 
;       objects: either native binary data or platform-independent 
;       XDR data. No mixture type is allowed.
;
; EXAMPLE:
;
;        U_OPENW, unit, 'catch1d.trashcan'
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       xx-xx-xx      iii  comment     
;-
;
if keyword_set(help) then goto, help1
if n_elements(filename) eq 0 then filename='data.dat'
ERRCODE=0
        CATCH,error_status
;        if !error_state.name eq 'IDL_M_CNTOPNFIL' then begin 
	if !err eq -215 or !err eq -206 then begin
                ret=WIDGET_MESSAGE(!err_string + string(!err))
                if n_elements(unit) then u_close,unit
		ERRCODE=-99
                return 
        end

if keyword_set(XDR) then begin
	if keyword_set(append) then $
	openw,/XDR,unit,filename,/GET_LUN,/APPEND else $
	openw,/XDR,unit,filename,/GET_LUN  
endif else begin
	if keyword_set(append) then $
	openw,unit,filename,/GET_LUN,/APPEND else $
	openw,unit,filename,/GET_LUN  
end

if n_params() eq 0 then print,'unit=',unit
return

help1:
	print,''
	print,'Usage:  u_openw, unit, filename'
	print,''
	print,'This routine assigns a LUN to the opened file for write only.'
	print,'        unit     - a LUN is returned by unit'
	print,"        filename - optional, default to 'data.dat'" 
	print,'e.g.'
	print,'       u_openw,unit'
	print,"       u_openw,unit,'data1.dat'"
	print,''
END

PRO u_openr,unit,filename,help=help,XDR=XDR
;+
; NAME:
;       U_OPENR
;
; PURPOSE:
;       This routine assigns a LUN to the opened file for unformatted
;       read only.
;
; CALLING SEQUENCE:
;
;       U_OPENR, Unit, 'filename' [,/XDR] [,/Help]
;
; INPUTS:
;       filename: Specifies the filename to be read by the U_READ 
;                 command.
;
; OUTPUTS:
;       Unit:     The LUN number to be associated with the opened file.
;
; KEYWORD PARAMETERS:
;       XDR:      This keyword specifies that the file is opened for 
;                 reading data in platform-independent XDR binary form. 
;       HELP:     If this keyword is set, a simple on line help is given.
;
; RESTRICTIONS:
;       The data file should contain only consistant type of binary 
;       objects: either native binary data or platform-independent 
;       XDR data. No mixture type is allowed.
;
; EXAMPLE:
;
;        U_OPENR, unit, 'catch1d.trashcan'
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       xx-xx-xx      iii  comment     
;-
if keyword_set(help) then goto, help1
if n_elements(filename) eq 0 then filename='data.dat'

if keyword_set(XDR) then  $
	openr,/XDR,unit,filename,/GET_LUN $
else $
	openr,unit,filename,/GET_LUN

if n_params() eq 0 then print,'unit=',unit
return

help1:
	print,''
	print,'Usage:  u_openr, unit, filename '
	print,''
	print,'This routine assigns a LUN to the opened file for read only.'
	print,'        unit     - a LUN is returned by unit'
	print,"        filename - optional, default to 'data.dat'" 
	print,'e.g.'
	print,'       u_openr,unit'
	print,"       u_openr,unit,'data1.dat'"
	print,''
END

PRO u_close,unit
;+
; NAME:
;       U_CLOSE
;
; PURPOSE:
;       This routine closes a file LUN opened for unformmated I/O.
;
; CALLING SEQUENCE:
;
;       U_CLOSE, Unit
;
; INPUTS:
;       Unit:     The LUN number to be closed.
;
; OUTPUTS:
;       None.
;
; EXAMPLE:
;
;        U_CLOSE, unit
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       xx-xx-xx      iii  comment     
;-
free_lun,unit
END

;
;  u_write, unit, array
;
PRO u_write,unit,array,help=help
;+
; NAME:
;       U_WRITE
;
; PURPOSE:
;       This routine writes an IDL data array to a file unit which is
;       opened for unformatted write. It supports all IDL data type except the
;       complex number and up to 2D array.  
;
; CALLING SEQUENCE:
;
;       U_WRITE, Unit, Var [,/Help]
;
; INPUTS:
;       Unit:   The logic unit number returned by file open for unformatted
;               write.
;
;       Var:    This variable holds the data  array to be written to
;               the opened file, it can be scaler, vector, or 2D array.
;	
; KEYWORD PARAMETERS:
;       HELP:   If this keyword is set, a simple on line help is given.
;
;
; RESTRICTIONS:
;       The data array can not be complex number. In order to make the
;       data to be read by the U_READ routine, all the data saved must 
;       be using this routine. 
;
; EXAMPLE:
;
;       Create the 'test.dat'  and write the variable X to the file
;
;         u_openw,unit,'test.dat'
;         u_write, unit, X
;         u_close,unit
;
;       Create or append X to the 'test.dat'  
;
;         u_openw,unit,'test.dat',/append
;         u_write, unit, X
;         u_close,unit
;
;       Create XDR platform independent data to the 'test.dat'  
;
;         u_openw,unit,/XDR,'test.dat'
;         u_write, unit, X
;         u_close,unit
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       05-30-97	bkc	Support opened file as XDR type data.
;-
;
if keyword_set(help) then goto, help1
if n_params() ne 2 then begin
	print,'Usage: u_write, unit, array'
	return
end
s = size(array)
if (s(0) eq 0) and (s(1) eq 0) then return    	; undefined
no = n_elements(s)
if s(no-2) eq 7 then begin
	len = strlen(array)
	if no eq 4 then s = [s,len(0)] else $		; vector
	if no eq 3 then s = [s,0,len]			; scalor
endif else begin
	if no eq 4 then s = [s,0] else $		; vector
	if no eq 3 then s = [s,0,0]			; scalor
end
writeu,unit,s(0:4),array
return

help1:
	print,''
	print,'Usage:  u_write, unit, array'
	print,''
	print,'This routine writes an array variable to the referenced unit in'
	print,'unformatted form.'
	print,'       unit     - required, a LUN already opened'
	print,"       array    - the array contents to be recorded" 
	print,''
	print,'Note:  For each array, two variables are written:'
	print,'       the long(5) size info array, and the array itself.'
	print,'       For string array each element must be exactly same size'
	print,' e.g.'
	print,'       u_write, unit, findgen(3,5)'
	print,"       u_write, unit, ,['111','222','333']"
	print,''
END


;
; dump unfomatted data ( which was written by u_write)
;
PRO u_dump,norecord,filename,data=data,help=help,XDR=XDR
ON_IOERROR,BAD
if keyword_set(help) then goto, help1
if n_elements(filename) eq 0 then filename='data.dat'

if keyword_set(XDR) then $
	openr,/XDR,unit,filename,/GET_LUN $
else $
	openr,unit,filename,/GET_LUN
	
s = lonarr(5)
norecord = 0
print,''
print,'DUMP FILE : ',filename
WHILE NOT EOF(unit) DO BEGIN
norecord = norecord + 1
u_read_set,unit,s,x
print,''
if keyword_set(data) then print,'<<',norecord,'>>  RECORDED SIZE & DATA:' else print,'<<',norecord,'>>  RECORDED SIZE INFO :'
print,'SIZE=',s
if keyword_set(data) then print,x
ENDWHILE
	goto, done
BAD:
	print,'Error encounted in'
	print,!ERR_STRING
done:
free_lun,unit
return

help1:
print,''
print,'Usage: u_dump, /data, norecord, filename '
print,''
print,'This routine dump the unformatted data from a file which was recored by u_write'
print,"       norecord - optional, if given it returns the number of records to the caller"
print,"       filename - optional, default to 'data.dat'"
print,"       /data    - optional, both size, and data arrays will be printed'"
print,''
print,' e.g.'
print,''
print,'       u_dump'
print,"       u_dump,no,'data1.dat',/data"
print,''
END


PRO u_read_set,unit,s,x,ERRCODE,help=help
if n_params() lt 3 then begin
	print,''
	print,'Usage: u_read_set, unit, s, x'
	print,''
	print,'This routine reads a set of two varaibles: size and array '
	print,'from the LUN unit. Note that the data must be recorded by the'
	print,'u_write routine.'
	print,'     where   S    LONG = Array(5), must be defined before calling this routine'
	print,'             X    returned array'
	print,'       ERRCODE    returned code, 0 for success, -99 for failure'
	return
	end

CATCH,error_status
if error_status  eq -229 or error_status eq -219 or error_status eq -184 then begin 
	str = [ !err_string + string(!err),'', $
		'Error: unable to read data, wrong type of file opened!!' ]
	ret=WIDGET_MESSAGE(str)
	return
	end

readu,unit,s

if (s(0) gt 1L) then begin	; two dim
	type = s(3)
	int = s(4)
endif else if (s(0) eq 1) then begin    ; one dim
	type = s(2)
	int = s(3)
endif else if (s(0) eq 0) then begin     ; scalor
	type = s(1)
	int = s(2)
end
;print,s
;print,'type=',type, '  dim=',int
int2 = int/s(1)
case fix(type) of 
	1: if (s(0) eq 2) then begin		; byte
		x = make_array(s(1),s(2),/byte) 
	   endif else begin
		x = bytarr(fix(int))  	
	   end
	2: if (s(0) eq 2) then begin		; int
		x = make_array(s(1),s(2),/int) 
	   endif else begin
		x = intarr(fix(int))  	
	   end
	3: if (s(0) eq 2) then begin		; long 
		x = make_array(s(1),s(2),/long) 
	   endif else begin
		x = lonarr(fix(int))  	
	   end
	4: if (s(0) eq 2) then begin		; float
		x = make_array(s(1),s(2),/float) 
	   endif else begin
		x = fltarr(fix(int))  	
	   end
	5: if (s(0) eq 2) then begin		; double 
		x = make_array(s(1),s(2),/double) 
	   endif else begin
		x = make_array(fix(int),/double)  	
	   end
	6: if (s(0) eq 2) then begin		; complex
		x = make_array(s(1),s(2),/complex) 
	   endif else begin
		x = make_array(fix(int),/complex)  	
	   end
	7: if (s(0) eq 2) then begin		; string
		print,'Error u_write/u_read can only support single string array'
		print,'size=',s
		return
	   endif else begin
		x = make_array(fix(int),/string,value=string(replicate(32b,s(4))))  	
	   end
else: begin
	print,'type=',type
;		ret=WIDGET_MESSAGE('Error: wrong type of data read in!!')
;		retall
		return
	end
endcase

	readu,unit,x
ERRCODE = 0
END


PRO u_read,unit,x,ERRCODE,help=help
;+
; NAME:
;       U_READ
;
; PURPOSE:
;       This routine reads an unformatted data entity from a file unit which is
;       opened for unformatted read. It supports all IDL data type except the
;       complex number and up to 2D array.  
;
; CALLING SEQUENCE:
;
;       U_READ, Unit, Var [,ERRCODE ,/Help]
;
; INPUTS:
;       Unit:   The logic unit number returned by file open for unformatted
;               read.
;	
; KEYWORD PARAMETERS:
;       HELP:   If this keyword is set, a simple on line help is given.
;
; OUTPUTS:
;       Var:    This variable holds the right type of data obtained from 
;               the opened file, it can be either 1D vector, or 2D array.
;   ERRCODE:    This variable holds the error code for the u_read. It
;               returns 0 if succeeded, returns -99 if failed.
;
; RESTRICTIONS:
;       All the data must be created by the U_WRITE routine in order to be 
;       read by this routine.
;
; EXAMPLE:
;
;       Read the first data entity from the 'test.dat' which was previously
;       created by the U_WRITE routine.
;
;         u_openr,unit,'test.dat'
;         u_read, unit, X
;         u_close,unit
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       05-30-97	bkc	Support opened file as XDR type data.
;       10-13-97	bkc	Add the ERRCODE to indicate success or failure.
;-

ERRCODE = -99
if keyword_set(help) then goto, help1
if n_params() lt 2 then begin
	print,'Usage: u_read, unit, array'
	return
	end
s = lonarr(5)
IF NOT EOF(unit) THEN  u_read_set,unit,s,x,ERRCODE ;ELSE print,'EOF on unit ',unit
return

help1:
	print,''
	print,'Usage: u_read, unit, x'
	print,''
	print,'This routine reads an array from the LUN unit.'
	print,'Note that the data must be recorded by the u_write routine.'
	print,'       X    returned array'

END


PRO u_bi2xdr,filename,help=help
;+
; NAME:
;       U_BI2XDR
;
; PURPOSE:
;       This IDL routine converts native binary data into platform-independent
;       XDR binary data. 
;
;       The input file should contain only pure native binary data.
;       The output filename uses the input filename suffixed with '.xdr'.
;
; CALLING SEQUENCE:
;
;       U_BI2XDR, 'filename' [,/Help]
;
; INPUTS:
;       filename:   The data file should contain pure binary data objects.
;
; OUTPUTS:
;       filename.xdr:   Output file. 
;                   It contains the converted XDR binary data objects.
;
; KEYWORD PARAMETERS:
;       HELP:   If this keyword is set, a simple on line help is given.
;
; RESTRICTIONS:
;       The input data file should contain pure binary data objects.
;
; EXAMPLE:
;
;        U_BI2XDR,'catch1d.trashcan'
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 05-30-97.
;
;       xx-xx-xx      iii  comment     
;-
;

if n_elements(filename) eq 0 or keyword_set(help) then goto,help1

	found = findfile(filename)
	if found(0) eq '' then return

	OK_WRITE = u_writePermitted(filename+'.xdr')
	if OK_WRITE lt 0 then return

        id=0
        u_openr,unit,filename
        u_openw,unit2,filename+'.xdr',/XDR

        WHILE NOT  EOF(unit) DO BEGIN
        id = id + 1
        u_read,unit,x
	u_write,unit2,x
        END
        maxno = id
        u_close,unit
        u_close,unit2
        ret=WIDGET_MESSAGE(string(maxno)+' sets of binary objects saved in "'+ $
		filename+'.xdr"')

	return

help1:

	print,''
	print,'Usage: U_BI2XDR,"filename"'
	print,''
	print,'This program converts the pure binary data objects into XDR binary format.'
	print,'The file "filename.xdr" created will be IDL platform independent.'
	print,''
END


; $Id: dc2hdf.pro,v 1.2 1998/05/26 21:56:40 cha Exp $

; Copyright (c) 1991-1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
PRO XDispFile_evt, event


WIDGET_CONTROL, event.top, GET_UVALUE = state
WIDGET_CONTROL, event.id, GET_UVALUE=Ev

CASE Ev OF 
'EXIT': WIDGET_CONTROL, event.top, /DESTROY
'FILE_PRINT': begin
	WIDGET_CONTROL,state.text_area,GET_VALUE=str
	openw,unit,'tmp',/GET_LUN
	for i=0,n_elements(str)-1 do printf,unit,str(i)
	FREE_LUN,unit
	PS_print,'tmp'
	end
ENDCASE
END


PRO XDisplayFile, FILENAME, TITLE = TITLE, GROUP = GROUP, WIDTH = WIDTH, $
		HEIGHT = HEIGHT, TEXT = TEXT, FONT = font
;+
; NAME: 
;	XDISPLAYFILE
;
; PURPOSE:
;	Display an ASCII text file using widgets and the widget manager.
;
; CATEGORY:
;	Widgets.
;
; CALLING SEQUENCE:
;	XDISPLAYFILE, Filename
;
; INPUTS:
;     Filename:	A scalar string that contains the filename of the file
;		to display.  The filename can include a path to that file.
;
; KEYWORD PARAMETERS:
;	FONT:   The name of the font to use.  If omitted use the default
;		font.
;	GROUP:	The widget ID of the group leader of the widget.  If this 
;		keyword is specified, the death of the group leader results in
;		the death of XDISPLAYFILE.
;
;	HEIGHT:	The number of text lines that the widget should display at one
;		time.  If this keyword is not specified, 24 lines is the 
;		default.
;
;	TEXT:	A string or string array to be displayed in the widget
;		instead of the contents of a file.  This keyword supercedes
;		the FILENAME input parameter.
;
;	TITLE:	A string to use as the widget title rather than the file name 
;		or "XDisplayFile".
;
;	WIDTH:	The number of characters wide the widget should be.  If this
;		keyword is not specified, 80 characters is the default.
;
; OUTPUTS:
;	No explicit outputs.  A file viewing widget is created.
;
; SIDE EFFECTS:
;	Triggers the XMANAGER if it is not already in use.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	Open a file and create a widget to display its contents.
;
; MODIFICATION HISTORY:
;	Written By Steve Richards, December 1990
;	Graceful error recovery, DMS, Feb, 1992.
;       12 Jan. 1994  - KDB
;               If file was empty, program would crash. Fixed.
;       4 Oct. 1994     MLR Fixed bug if /TEXT was present and /TITLE was not.
;      14 Jul. 1995     BKC Increased the max line to variable size.
;      16 Jun. 1997     BKC Max dispalyable line is 10000 for non-unix OS system.
;      28 Aug. 1997     BKC Add the printer button, file name label, it uses the
;                       PS_print,file to print.
;-
COMMON SYSTEM_BLOCK,OS_SYSTEM
                                                        ;use the defaults if
IF(NOT(KEYWORD_SET(HEIGHT))) THEN HEIGHT = 24		;the keywords were not
IF(NOT(KEYWORD_SET(WIDTH))) THEN WIDTH = 80		;passed in

IF(NOT(KEYWORD_SET(TEXT))) THEN BEGIN
  IF(NOT(KEYWORD_SET(TITLE))) THEN TITLE = FILENAME     
  OPENR, unit, FILENAME, /GET_LUN, ERROR=i		;open the file and then
  if i lt 0 then begin		;OK?
	a = [ !err_string, ' Can not display ' + filename]  ;No
  endif else begin

    y=10000
    if OS_SYSTEM.os_family eq 'unix' then begin
	spawn,[OS_SYSTEM.wc,'-l',FILENAME],y,/noshell

	lines=long(y(0))
	if lines eq 0 then begin
	res=WIDGET_MESSAGE('Unable to display '+FILENAME)
	return
	end
    end

	  a = strarr(y(0))				;Maximum # of lines
	  i = 0L
	  c = ''
	  while not eof(unit) do begin
		readf,unit,c
		a(i) = c
		i = i + 1
		if i ge y(0) then goto,stopread
		endwhile
	  stopread:
	  a = a(0:(i-1)>0)  ;Added empty file check -KDB
	  FREE_LUN, unit				;free the file unit.
  endelse
ENDIF ELSE BEGIN
    IF(NOT(KEYWORD_SET(TITLE))) THEN TITLE = 'XDisplayFile'
    a = TEXT
ENDELSE

filebase = WIDGET_BASE(TITLE = TITLE, $			;create the base
		/COLUMN ) 

label=WIDGET_LABEL(filebase,value=TITLE)
rowbtn = WIDGET_BASE(filebase,/ROW,TITLE='ROWBTN')
fileprint = WIDGET_BUTTON(rowbtn, $			;create a Print Button
		VALUE = "Print", $
		UVALUE = "FILE_PRINT")

filequit = WIDGET_BUTTON(rowbtn, $			;create a Done Button
		VALUE = "Done", $
		UVALUE = "EXIT")

IF n_elements(font) gt 0 then $
 filetext = WIDGET_TEXT(filebase, $			;create a text widget
		XSIZE = WIDTH, $			;to display the file's
		YSIZE = HEIGHT, $			;contents
		/SCROLL, FONT = font, $
		VALUE = a) $
ELSE filetext = WIDGET_TEXT(filebase, $			;create a text widget
		XSIZE = WIDTH, $			;to display the file's
		YSIZE = HEIGHT, $			;contents
		/SCROLL, $
		VALUE = a)

state = { $
	 base: filebase, $
	 text_area: filetext, $
	 file: filename $
	 }
WIDGET_CONTROL,filebase,SET_UVALUE=state

WIDGET_CONTROL, filebase, /REALIZE			;instantiate the widget

Xmanager, "XDisplayFile", $				;register it with the
		filebase, $				;widget manager
		GROUP_LEADER = GROUP, $
		EVENT_HANDLER = "XDispFile_evt" 

END  ;--------------------- procedure XDisplayFile ----------------------------

PRO scan_read_record,unit,version,pv,num_pts,FA,x,y,n,ze

	u_read,unit,version
	u_read,unit,pv
	u_read,unit,num_pts
	u_read,unit,id_def
	u_read,unit,x_dpt

num_po = 0
for i=0,18 do begin
       if id_def(i) gt 0 then num_po = num_po + 1
end
FA = make_array(num_pts(0)+1,num_po)
for i=0,num_po-1 do begin
        u_read,unit,px
	FA(*,i) = px
end

	u_read,unit,labels
	u_read,unit,x
	u_read,unit,y
	u_read,unit,n
	if n(0) gt 0 then begin
	u_read,unit,ze
	end
END


;
; if seq_no + 1 = id will plot
;
PRO scan_read,unit,seq_no,id

COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids
COMMON env_field_block,env_field
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus

if n_params() lt 2 then begin
        w_warningtext,'usage:   scan_read, unit, seq_no, id'
        return
        end

if EOF(unit) eq 1 then begin
	w_warningtext,'end of file reached ! '
	POINT_LUN,w_viewscan_id.unit,0
	id = 0
	return
	end

; 
next_seq_no = seq_no + 1
	u_read,unit,version,ERRCODE
	if ERRCODE eq -99 or version(0) ne scanData.version then begin
		if scanData.XDR then $
                s0 = 'Error: data version is in native binary form.' else $
                s0 = 'Error: data version is in XDR binary form.'
	 
		if scanData.XDR then $
                s1 = '       Current setting is in XDR binary form' else $
                s1 = '       Current setting is in native binary form' 
                st = ['Error: wrong binary form selected.',s0,s1]
                w_warningtext,st
                return
                end

	u_read,unit,pv
	u_read,unit,num_pts
	u_read,unit,id_def
	u_read,unit,x_dpt

	realtime_id.def = id_def
	scanData.pa = make_array(4000,4,/double)
	scanData.da = FLTARR(4000,15)

	num_pts = fix(num_pts(0))

for i=0,3 do begin
        if realtime_id.def(i) gt 0 then begin
	u_read,unit,px
	scanData.pa(0:num_pts,i) = px
        end
end

for i=0,14 do begin
        if realtime_id.def(4+i) gt 0 then begin
	u_read,unit,px
	scanData.da(0:num_pts,i) = px
        end
end


; read 1D-plot labels

	labels = make_array(30,57,/byte)
	u_read,unit,labels
	read_desc_engu,labels 
	
x = make_array(6,/string,value=string(replicate(32b,60)))

y = make_array(8,/float)

	u_read,unit,x
	u_read,unit,oy
	u_read,unit,n
y=oy
no = n_elements(oy)
if n_elements(y) gt 1 then begin
        scanData.refno = fix(y(1))
        scanData.y_seqno = fix(y(2))
        scanData.scanno_2d = fix(y(3))
	if no ge 5 then scanData.y_scan = fix(y(4))
	if no eq 8 then begin
		scanData.y_value = y(7)
		scanData.dataversion = scanData.release
		endif else scanData.dataversion = ''
        endif else scanData.refno = fix(y(0)) + 1

w_plotspec_id.seqno = fix(y(0))
w_viewscan_id.seqno = fix(y(0))


env_field.exist = 0 
if n(0) gt 0 then begin 
no=n(0)
env_field.exist = no
env_field.no = no        ; actual no of env written on data file
ze = make_array(no,/string,value=string(replicate(32b,110)))

	u_read,unit,ze

	for i=0,no-1 do begin
	env_field.pvnames(i)=strmid(ze(i),0,30)
	env_field.values(i)=strmid(ze(i),30,40)
	env_field.descs(i)=strmid(ze(i),70,40)
	end

	env_field.pvnames = strtrim(env_field.pvnames,2)

end

;
; external file name is used for display the data
; the internal file name may be not consistant with the external file name
; due to the user copy the trashcan to a new file without use file->copy
;
temp_name = w_plotspec_array(3)
s1 = n_elements(x)
for i=0,s1-1  do w_plotspec_array(i)=x(i) 
w_plotspec_array(3) = temp_name

;field_value = make_array(field_max,/string,value=string(replicate(32b,40)))
;for i=0,field_max-1 do field_value(i)=strtrim(ze(i+env_field.noenv))

   ;populate read data into global arrays

scanData.pv = string(pv(0))
scanData.act_npts = num_pts + 1

if id eq next_seq_no or next_seq_no le 0 then begin
	if scanData.debug eq 1 then $
	print,'Scan # ',seq_no, ' accessed.'
	scanData.scanno = seq_no
;	setDefaultLabels
	setPlotLabels
	UPDATE_PLOT, scanData.lastPlot
	id = next_seq_no
	end

scanData.readin_npts=scanData.act_npts

END

; add the support of index file this should improve the
; perfomance of data catcher
;

PRO catch1d_readFileIndex,filename,errcode
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

; check whether filename exists

errcode=-99
fd = findfile(filename)
IF fd(0) NE '' THEN BEGIN
	
	indexfile = filename+'.index'

found = findfile(indexfile)
if found(0) ne '' then begin
errcode = 0
	u_openr,unit,indexfile
	u_read,unit,name
	u_read,unit,fsize
	u_read,unit,maxno
	u_read,unit,array
	u_close,unit

	openr,1,filename
	status = FSTAT(1)
	close,1
	
	if status.size eq fsize(0) then begin
	w_viewscan_id.size = fsize(0)
	w_viewscan_id.maxno = maxno(0)
;	w_viewscan_id.fptr(0:w_viewscan_id.maxno) = array
	w_viewscan_id.fptr = array
	end
;	print,'***Read Index File: ',indexfile
end
ENDIF ELSE BEGIN
	w_warningtext,'Warning: file "' + filename + '" not found, it will be created.'
END 
END


PRO catch1d_writeFileIndex,filename
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

; check file existence

found = findfile(filename)
if found(0) eq '' then return
	openr,1,filename
	status = FSTAT(1)
	close,1
	
	if filename eq w_viewscan_id.file then begin

	if w_viewscan_id.maxno gt 0 then begin

	indexfile = w_viewscan_id.file + '.index'
	CATCH,error_status
	if error_status lt 0 then return
	openw,unit,indexfile,/GET_LUN

	array = w_viewscan_id.fptr(0:w_viewscan_id.maxno)
	array(w_viewscan_id.maxno) = status.size

	u_write,unit,status.name
	u_write,unit,status.size
	u_write,unit,w_viewscan_id.maxno
	u_write,unit,array
	u_close,unit

;print,'***File ',indexfile,' updated.'
	end
	end
END

PRO  getStatisticDeviation_1d,id1,y,mean,sdev,mdev,st
	mean=0.
	sdev=0.
	mdev=0.
	no = n_elements(y)
	if no eq 0 then return 
	mean = total(y)/no
	if no eq 1 then return
	index = where(y gt mean, count)      ; check for constant function 
	mean = [mean,0.,0.,0.]
	if count gt 0 then mean = MOMENT(y,mdev=mdev,sdev=sdev)

st = [' Detector '+strtrim(id1+1,1)]
st= [st+' ']
        st = [st, '   Mean         = '+string(mean(0))]
        st = [st, '   Standard Dev = '+string(sdev)]
        st = [st, '   Mean Abs Dev = '+string(mdev)]
        st = [st, '   Variance     = '+string(mean(1))]
        st = [st, '   Skewness     = '+string(mean(2))]
        st = [st, '   Kurtosis     = '+string(mean(3))]
END

PRO  getStatistic_1d,id1,p1,d1,c_mass,xpeak,ypeak,y_hpeak,FWHM,st

; call statistic_1d

        statistic_1d,p1,d1,c_mass,x_peak,y_peak,y_hpeak,FWHM

st = [' Detector '+strtrim(id1+1,1)]
st= [st+' ']
        st = [st, '   Peak  X='+strtrim(x_peak,1)+'  Y='+strtrim(y_peak,1)]
;       st = [st, '   H-Peak  Y='+strtrim(y_hpeak)]
        st = [st, '   Centroid  '+ strtrim(c_mass,1)]
        st = [st, '   FWHM      '+strtrim(FWHM,1)]

if n_elements(x_peak) gt 0 then begin
	largest = max(y_peak)
	i_largest = 0
	for i=0,n_elements(x_peak)-1 do begin
		if y_peak(i) ge largest then begin 
		i_largest = i
		goto, write_peak
		end
		end
	write_peak:
	xpeak = x_peak(i_largest)
	ypeak = y_peak(i_largest)
	end

END





;
; find  fwh_max, c_mass, peak for a given x,y array
;
PRO statistic_1d,x,y,c_mass,x_peak,y_peak,y_hpeak,fwhm,fwhm_xl,fwhm_wd, $
	FIT=FIT,XINDEX=XINDEX,LIST=LIST

xindex = keyword_set(XINDEX)
list = keyword_set(LIST)

nx = n_elements(x)
a=make_array(nx,/float)
da=make_array(nx,/float)
ny=make_array(nx,/float)
slopey=make_array(nx,/float)

ymin = min(y)
ymax = max(y)
ny = y - ymin

peak = ymax
hpeak = 0.5 * max(ny)
y_hpeak= hpeak + ymin

; area = int_tabulated(x,ny)
; harea = 0.5 * area

d0=0
for i=1,nx-1 do begin
	dx = x(i) - x(i-1)
	if dx ne 0. then begin
	da(i) = 0.5 *(ny(i)+ny(i-1)) * dx
	d0 = d0 + da(i)
	a(i) = d0
	slopey(i)= (ny(i)-ny(i-1))/dx
	if list then print,strtrim(i,1),x(i),y(i),da(i),a(i),slopey(i),ny(i)
	end
end

area = d0
harea = 0.5 * area

; Find c_mass

newtons_method,x,a,harea,c_mass
if list then print,'===='
if list then print,'C_mass',harea,c_mass


; Find half peaks

if list then print,'===='
nohwdl=0
nohwdr=0
x_hwdl=0
x_hwdr=0
for i=1,nx-1 do begin
	yl = ny(i-1) - hpeak
	yr = ny(i) - hpeak
       if yl*yr lt 0. and yl lt 0. then begin
		nohwdl = [nohwdl, i-1]
;		print,i-1,y(i-1)
		newtons_method,[x(i-1),x(i)],[yl,yr],0.,x_sol,notfound
		x_hwdl= [x_hwdl,x_sol]
		end
       if yl*yr lt 0. and yl gt 0. then begin
		nohwdr = [nohwdr, i-1]
;		print,i-1,y(i-1)
		newtons_method,[x(i-1),x(i)],[yl,yr],0.,x_sol,notfound
		x_hwdr= [x_hwdr,x_sol]
		end
end
;print,'nohwdl',nohwdl, x_hwdl
;print,'nohwdr',nohwdr, x_hwdr
	lo=0
	fwhm = 0.
if n_elements(nohwdl) gt 1 then begin 
	x_hwd = x_hwdl(1:n_elements(nohwdl)-1)
	nohw = n_elements(x_hwd)
if n_elements(nohwdr) gt 1 then begin
	x_hwde = x_hwdr(1:n_elements(nohwdr)-1)
	nohwe = n_elements(x_hwde)
	fwhm = make_array(nohw,/float)
	for i=0,nohw-1 do begin
		x1 = x_hwd(i)
	for j=0,nohwe-1 do begin
		if x_hwde(j) ne x1 then begin
			fwhm(i) = abs(x_hwde(j) - x1)
			lo=lo+1
;			print,'FWHM',lo,fwhm(i)
			goto,outer
			end
		end
	outer:
	end
	end
	FWHM = max(fwhm)
end

;if n_elements(nohwdr) gt 1 then begin
;	if n_elements(x_hwd) gt 0 then $
;	x_hwd = [x_hwd, x_hwdr(1:n_elements(nohwdr)-1)] else $
;	x_hwd = [x_hwdr(1:n_elements(nohwdr)-1)]
;	end
;if n_elements(x_hwd) gt 0 then begin
;	x_HPeak = x_hwd(sort(x_hwd))
;	if list then print,'hpeak,y_hpeak',hpeak,y_hpeak
;	if list then print,'HPeak pts:',x_HPeak
;end

; Find peaks

if keyword_set(FIT) then begin
nopeaks=0
if list then print,'===='
for i=1,nx-1 do begin
       if slopey(i-1) gt 0 and slopey(i-1)*slopey(i) lt 0. then begin
;		print,i,slopey(i-1),slopey(i)
		nopeaks = [nopeaks, i]
		end
end
;print,'nopeaks',nopeaks
no = n_elements(nopeaks)-1
if no gt 0 then begin
x_peak = make_array(no,/float)
y_peak = make_array(no,/float)
for i=1,no do begin
	i2= nopeaks(i)
	i1= i2-1
	newtons_method,[x(i1),x(i2)],[slopey(i1),slopey(i2)],0.,x_sol,notfound
	if notfound eq 0 then begin
if list then 	print,'Peak #',i,x_sol,y(i1)
		x_peak(i-1)= x_sol
		y_peak(i-1) = y(i1)
		end
end
endif else begin
	y_peak = ymax
	if y(0) gt y(nx-1) then x_peak = x(0) else x_peak = x(nx-1)
if list then 	print,'Ymax at pt ',y_peak,x_peak
end
endif else begin

	for i=0,nx -1 do begin
		if y(i) eq peak then begin
		x_peak = x(i)
		y_peak = peak
		return
		end
	end
end

END

PRO find_hpeak,x,nx,xindex=xindex
print,'===='
fwh_max= make_array(4,/float)
ix = nx / 4
x_index = indgen(nx)
for m=0,3 do begin
i1 = ix *m 
i2 = i1+ix-1
newx = x(i1:i2)
newy = ny(i1:i2)

xindex = keyword_set(XINDEX)
	if xindex then begin
	newx = x_index(i1:i2)
	newtons_method_norm,newx,newy,hpeak,n1,x_sol,notfound
	fwh_max_x1 = x(n1) + x_sol * (x(n1+1) - x(n1))
	endif else begin
	newtons_method,newx,newy,hpeak,fwh_max_x1,notfound
	end

	if notfound then print,'HPeak RANGE #',m+1,'     ENCOUNTERED NOT FOUND PROBLEM' 
	fwh_max(m)=fwh_max_x1
	print,'HPeak RANGE #',m+1,fwh_max(m)
end
END


PRO newtons_method,x,y,y_sol,x_sol,notfound
notfound = 0
nx = n_elements(y)
n1 = 0 
n2 = nx-1 
RETEST:
;print,'N1,N2',n1,n2,y(n1),y(n2)
if (n2-n1) le 1 then begin
	if (y_sol - y(n2)) * (y_sol - y(n1)) gt 0 then begin
		x_sol= x(n1)
		notfound = 1
		return
		end
	if (x(n2)-x(n1)) eq 0. then begin
		x_sol = x(n1)
		return
	end
	x_sol = x(n1)+ (y_sol - y(n1)) /(y(n2)-y(n1)) *(x(n2)-x(n1))
	 return
	end
 
nm = (n2-n1)/ 2 + n1
fm = y (nm)
;print,nm,fm,y_sol
if abs(fm-y_sol) le 1.e-5 then begin
	x_sol = x(nm)
;	print,'Stop at NM,x_sol',nm,x_sol
	return
endif else begin
	if (fm-y_sol) *(y(n2) - y_sol) gt 0 then begin
		n2 = nm
	endif else  begin
		n1 = nm
	end
	goto,RETEST
	end
END

;
; using index and factor instead of real value for x array
;
PRO newtons_method_norm,x,y,y_sol,n1,x_sol,notfound
	rx = float(x)
	newtons_method,rx,y,y_sol,x_sol,notfound
	n1 = fix(x_sol)
	x_sol = x_sol-float(n1)
END


;
; DC2HDFUser.pro
;



PRO DC2HDFUser_Event, Event
COMMON DC2HDFUser_BLOCK, DC2HDFUser_wid 
COMMON HDF_QUERY_SD_BLOCK, HDF_QUERY_SD, HDF_QUERY_WID, HDF_QUERY_DC

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'USER_NAME': BEGIN
      END
  'USER_MAIL': BEGIN
      END
  'USER_EMAIL': BEGIN
      END
  'USER_PHONE': BEGIN
      END
  'USER_FAX': BEGIN
      END
  'FIELD8': BEGIN
      END
  'USER_HISTORY': BEGIN
      END
  'USER_DESC': BEGIN
      END
  'USER_ACCEPT': BEGIN
	WIDGET_CONTROL,DC2HDFUser_wid.user_name,GET_VALUE=st
	HDF_QUERY_SD.user_name = st(0)
	WIDGET_CONTROL,DC2HDFUser_wid.user_mail,GET_VALUE=st
	HDF_QUERY_SD.user_mail = st(0)
	WIDGET_CONTROL,DC2HDFUser_wid.user_email,GET_VALUE=st
	HDF_QUERY_SD.user_email = st(0)
	WIDGET_CONTROL,DC2HDFUser_wid.user_phone,GET_VALUE=st
	HDF_QUERY_SD.user_phone = st(0)
	WIDGET_CONTROL,DC2HDFUser_wid.user_fax,GET_VALUE=st
	HDF_QUERY_SD.user_fax = st(0)
	WIDGET_CONTROL,DC2HDFUser_wid.user_history,GET_VALUE=st
	HDF_QUERY_SD.user_history = st(0)
	WIDGET_CONTROL,DC2HDFUser_wid.user_desc,GET_VALUE=st
	nline = n_elements(st)
	if nline gt 100 then nline = 100
	HDF_QUERY_SD.desc_nline = nline
	HDF_QUERY_SD.user_desc = st(0:nline-1)

	if HDF_QUERY_SD.file ne '' then $
	update_global_attribute, HDF_QUERY_SD.file 

      WIDGET_CONTROL,DC2HDFUser_wid.base,/DESTROY
      DC2HDFUser_wid.base=0L
      END
  'USER_CANCEL': BEGIN
      WIDGET_CONTROL,DC2HDFUser_wid.base,/DESTROY
      DC2HDFUser_wid.base=0L
      END
  'USER_CLEAR': BEGIN
      WIDGET_CONTROL,DC2HDFUser_wid.user_desc,SET_VALUE=''
      END
  'USER_UPDATE': BEGIN
	st = HDF_QUERY_SD.user_desc(0:HDF_QUERY_SD.desc_nline-1)
      WIDGET_CONTROL,DC2HDFUser_wid.user_desc,SET_VALUE=st
      END
  ENDCASE
END




PRO DC2HDFUser,GROUP=Group 
COMMON DC2HDFUser_BLOCK, DC2HDFUser_wid 
COMMON HDF_QUERY_SD_BLOCK, HDF_QUERY_SD, HDF_QUERY_WID, HDF_QUERY_DC

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  DC2HDFUser= WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
      UVALUE='DC2HDFUser')

  BASE2 = WIDGET_BASE(DC2HDFUser, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  LABEL11 = WIDGET_LABEL( BASE2, $
      FONT='-adobe-helvetica-bold-r-normal--18-180-75-75-p-103-iso8859-1', $
      UVALUE='LABEL11', $
      VALUE='HDF Global Attributes')

  LABEL11 = WIDGET_LABEL( BASE2, $
      FONT='-adobe-helvetica-bold-r-normal--14-140-75-75-p-103-iso8859-1', $
      UVALUE='LABEL11', $
      VALUE='(NEXUS 1.24)')

  FieldVal167 = [ $
    '' ]
  USER_NAME = CW_FIELD( BASE2,VALUE=FieldVal167, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=0, $
      TITLE='Owner_name', $
      UVALUE='USER_NAME', $
      XSIZE=30)

  FieldVal232 = [ $
    '' ]
  USER_MAIL = CW_FIELD( BASE2,VALUE=FieldVal232, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=0, $
      TITLE='Owner_mail', $
      UVALUE='USER_MAIL', $
      XSIZE=30)

  FieldVal297 = [ $
    '' ]
  USER_EMAIL = CW_FIELD( BASE2,VALUE=FieldVal297, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=0, $
      TITLE='Owner_email', $
      UVALUE='USER_EMAIL', $
      XSIZE=30)

  BASE2_1 = WIDGET_BASE(BASE2, $
      COLUMN=2, $
      MAP=1, $
      UVALUE='BASE2_1')

  FieldVal362 = [ $
    '' ]
  USER_PHONE = CW_FIELD( BASE2_1,VALUE=FieldVal362, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=0, $
      TITLE='Owner_phone', $
      UVALUE='USER_PHONE', $
      XSIZE=20)

  FieldVal427 = [ $
    '' ]
  USER_FAX = CW_FIELD( BASE2_1,VALUE=FieldVal427, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=0, $
      TITLE='Owner_fax', $
      UVALUE='USER_FAX', $
      XSIZE=20)


  FieldVal557 = [ $
    '' ]
  USER_HISTORY = CW_FIELD( BASE2,VALUE=FieldVal557, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=0, $
      TITLE='File_history', $
      UVALUE='USER_HISTORY', $
      XSIZE=60)

  BASE2_3 = WIDGET_BASE(BASE2,/ROW, MAP=1)

  LABEL12 = WIDGET_LABEL( BASE2_3, $
      UVALUE='LABEL12', $
      VALUE='Experimental Description')
  USER_CLEAR = WIDGET_BUTTON( BASE2_3,VALUE='Clear', $
	UVALUE='USER_CLEAR')
  USER_UPDATE = WIDGET_BUTTON( BASE2_3,VALUE='Update', $
	UVALUE='USER_UPDATE')

  TextVal645 = [ $
    '' ]
  USER_DESC = WIDGET_TEXT( BASE2,VALUE=TextVal645, $
      EDITABLE=1, $
      UVALUE='USER_DESC', $
      scr_XSIZE=60, YSIZE=6, /scroll )

  BASE2_4 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2_4')
  USER_ACCEPT = WIDGET_BUTTON( BASE2_4,VALUE='Accept', $
	UVALUE='USER_ACCEPT')
  USER_CANCEL = WIDGET_BUTTON( BASE2_4,VALUE='Cancel', $
	UVALUE='USER_CANCEL')

  DC2HDFUser_wid = { $
	base : DC2HDFUser, $
	user_name : USER_NAME, $
	user_mail : USER_MAIL, $
	user_email : USER_EMAIL, $
	user_phone : USER_PHONE, $
	user_fax : USER_FAX, $
	user_history : USER_HISTORY, $
	user_desc : USER_DESC $
	}

if HDF_QUERY_SD.user_name eq '' then begin
	user,u_id,u_nm,u_em
	WIDGET_CONTROL,DC2HDFUser_wid.user_name,SET_VALUE=u_nm
	WIDGET_CONTROL,DC2HDFUser_wid.user_email,SET_VALUE=u_em
endif else begin
	WIDGET_CONTROL,DC2HDFUser_wid.user_name,SET_VALUE=HDF_QUERY_SD.user_name
	WIDGET_CONTROL,DC2HDFUser_wid.user_email,SET_VALUE=HDF_QUERY_SD.user_email
	WIDGET_CONTROL,DC2HDFUser_wid.user_mail,SET_VALUE=HDF_QUERY_SD.user_mail
	WIDGET_CONTROL,DC2HDFUser_wid.user_phone,SET_VALUE=HDF_QUERY_SD.user_phone
	WIDGET_CONTROL,DC2HDFUser_wid.user_fax,SET_VALUE=HDF_QUERY_SD.user_fax
	WIDGET_CONTROL,DC2HDFUser_wid.user_fax,SET_VALUE=HDF_QUERY_SD.user_fax
	WIDGET_CONTROL,DC2HDFUser_wid.user_history,SET_VALUE=HDF_QUERY_SD.user_history
	WIDGET_CONTROL,DC2HDFUser_wid.user_desc,SET_VALUE=HDF_QUERY_SD.user_desc(0:HDF_QUERY_SD.desc_nline-1)
end

  WIDGET_CONTROL, DC2HDFUser, /REALIZE

  XMANAGER, 'DC2HDFUser', DC2HDFUser
END

PRO user,user_id,user_name,user_email
	user_name = ''
	user_email = ''

	user_id =  getenv('USER') + '@'
	CATCH, error_status
	if error_status lt 0 then return 

	spawn,['malias',user_id],/NOSHELL, listing
	ml = listing(2)

	l2 = strpos(ml,user_id)
	len = strlen(ml)
	user_name = strtrim(strmid(ml,0,29),2)
	user_email = strmid(ml,l2(0),len(0)-l2(0))

END

PRO open_hdf,filename, gname=gname, gclass=gclass, $
	user_location = user_location, $
	user_name = user_name, $
	user_email = user_email, $
	user_phone = user_phone, $
	program_name = program_name
COMMON HDF_QUERY_SD_BLOCK, HDF_QUERY_SD, HDF_QUERY_WID, HDF_QUERY_DC


	if keyword_set(program_name) eq 0 then $
		program_name = HDF_QUERY_DC.version 

; this section only hold true for aps
;

	if keyword_set(gname) then default_entry = gname else $
		default_entry='entry_default'
	if keyword_set(gclass) then default_class = gclass else $
		default_class='NXdefault'

	if keyword_set(user_location) then location = user_location else $
		location = 'APS'

default:
time0 = systime(1)

HDF_QUERY_SD.file = filename

found = findfile(filename)

if found(0) eq '' then begin

	fid = HDF_OPEN(filename,/ALL)
	HDF_CLOSE,fid

	fid = HDF_OPEN(filename,/RDWR)
        HDF_QUERY_SD.fid = fid

; Write global attributes only once

	if HDF_QUERY_SD.user_name eq '' then begin
	user,u_id,u_nm,u_em
	HDF_QUERY_SD.user_name = u_nm
	HDF_QUERY_SD.user_email = u_em
	end

;	update_global_attribute,filename

	sd_id = HDF_SD_START(filename,/RDWR)

	HDF_SD_FILEINFO,sd_id,nmfsds,nglobatts
	if nglobatts eq 0 then begin
	HDF_SD_ATTRSET,sd_id,'file_name',filename
	HDF_SD_ATTRSET,sd_id,'version',HDF_QUERY_DC.version
	end

; Write the default entry & class

	vg_id = HDF_VG_ATTACH(fid,-1,/WRITE)
	HDF_VG_SETINFO,vg_id,name=default_entry,class=default_class

;	vg_tag = 1965
	vg_tag = HDF_QUERY_SD.vg_tag
	hdf_write_data,sd_id,name='location',byte(location),index,ref
	HDF_VG_ADDTR,vg_id,vg_tag,ref

	hdf_write_data,sd_id,name='program_name',byte(program_name),index,ref
	HDF_VG_ADDTR,vg_id,vg_tag,ref

	HDF_VG_DETACH,vg_id

	HDF_SD_END,sd_id

endif else begin

; print,'filename=',filename ,' already exists!' 

	if HDF_QUERY_SD.fid gt 0 then close_hdf
	fid  = HDF_OPEN(filename,/RDWR)
	HDF_QUERY_SD.fid = fid

	if HDF_QUERY_SD.debug then begin
	time1 = systime(1)
	print,'HDF open used time = ',time1-time0, ' seconds.'
	end
end


END

PRO update_global_attribute,filename
COMMON HDF_QUERY_SD_BLOCK, HDF_QUERY_SD, HDF_QUERY_WID

found = findfile(filename) 
if found(0) eq '' then return

	sd_id = HDF_SD_START(filename,/RDWR)

	if HDF_QUERY_SD.user_name ne '' then $
	HDF_SD_ATTRSET,sd_id,'owner_name',HDF_QUERY_SD.user_name
	if HDF_QUERY_SD.user_mail ne '' then $
	HDF_SD_ATTRSET,sd_id,'owner_mail',HDF_QUERY_SD.user_mail
	if HDF_QUERY_SD.user_email ne '' then $
	HDF_SD_ATTRSET,sd_id,'owner_email',HDF_QUERY_SD.user_email
	if HDF_QUERY_SD.user_phone ne '' then $
	HDF_SD_ATTRSET,sd_id,'owner_phone',HDF_QUERY_SD.user_phone
	if HDF_QUERY_SD.user_fax ne '' then $
	HDF_SD_ATTRSET,sd_id,'owner_fax',HDF_QUERY_SD.user_fax
	if HDF_QUERY_SD.user_history ne '' then $
	HDF_SD_ATTRSET,sd_id,'file_history',HDF_QUERY_SD.user_history
;	if HDF_QUERY_SD.user_desc ne '' then $
;	HDF_SD_ATTRSET,sd_id,'experiment_description',HDF_QUERY_SD.user_desc

;HDF_SD_FILEINFO,sd_id,NumSDS,NumGAttr
;help,NumSDS,NumGAttr
	HDF_SD_END,sd_id

; write file desc
	sz = size(byte(HDF_QUERY_SD.user_desc))
	desc = byte(replicate(32b,sz(n_elements(sz)-1)))
	len = 0L
	str = 'Owner Name: '+ HDF_QUERY_SD.user_name
	dlen = strlen(str)
	desc(len) = byte(str)
	desc(len+dlen) = 10b
	len = len+dlen + 1

	str = 'Owner Mail: '+ HDF_QUERY_SD.user_mail
	dlen = strlen(str)
	desc(len) = byte(str)
	desc(len+dlen) = 10b
	len = len+dlen + 1

	str = 'Owner_email: ' + HDF_QUERY_SD.user_email
	dlen = strlen(str)
	desc(len) = byte(str)
	desc(len+dlen) = 10b
	len = len+dlen + 1

	str = 'Owner_phone: ' + HDF_QUERY_SD.user_phone
	dlen = strlen(str)
	desc(len) = byte(str)
	desc(len+dlen) = 10b
	len = len+dlen + 1

	str = 'Owner_fax:   ' + HDF_QUERY_SD.user_fax
	dlen = strlen(str)
	desc(len) = byte(str)
	desc(len+dlen) = 10b
	len = len+dlen + 1

	str = 'File_history :' + HDF_QUERY_SD.user_history
	dlen = strlen(str)
	desc(len) = byte(str)
	desc(len+dlen) = 10b
	len = len+dlen + 1

	str = 'Experiment_description: ' 
	dlen = strlen(str)
	desc(len) = byte(str)
	desc(len+dlen) = 10b
	len = len+dlen + 1

	for i=0,HDF_QUERY_SD.desc_nline - 1 do begin
		str = strtrim(HDF_QUERY_SD.user_desc(i))
		dlen = strlen(str)
		desc(len) = byte(str)
		desc(len+dlen) = 10b
		len = len+dlen + 1
	end

	HDF_DFAN_ADDFDS,filename, desc(0:len)


END



PRO close_hdf
COMMON HDF_QUERY_SD_BLOCK, HDF_QUERY_SD

	time2 = systime(1)

	;HDF_SD_END,HDF_QUERY_SD.sd_id
	if HDF_QUERY_SD.fid gt 0 then hdf_close,HDF_QUERY_SD.fid
	HDF_QUERY_SD.fid = -1 

	if HDF_QUERY_SD.debug then begin
	time3 = systime(1)
	print,'HDF close used time = ',time3-time2, ' seconds.'
	end
END


PRO hdf_write_data, sd_id, data, index, ref, name=name, $
    range=range, unit=unit, format=format, $
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


	HDF_SD_ADDDATA,sds_id,data

	ref = HDF_SD_IDTOREF(sds_id)

	index = HDF_SD_REFTOINDEX(sd_id,ref)

	HDF_SD_ENDACCESS,sds_id

END


;
;  use the APS HDF standards proposed by J. Tischler
;

PRO scan_write_hdf3,parent_vg_id,scan_no=scan_no,pvname=pvname,FILE=FILE
COMMON HDF_QUERY_SD_BLOCK, HDF_QUERY_SD, HDF_QUERY_WID,  HDF_QUERY_DC

; read in a Data catcher file

	scan_read_record_2,HDF_QUERY_DC.unit,version,pv,num_pts,id_def,x_dpt,FA,FB,labels,x,y,n,ze

	HDF_QUERY_DC.version = version(0) + ' --> DC2HDF_V1'
	HDF_QUERY_DC.pv = pv(0)
	HDF_QUERY_DC.num_pts = num_pts(0) 
	HDF_QUERY_DC.act_npts = num_pts(0) + 1 
	HDF_QUERY_DC.req_npts = num_pts(0) + 1  ; this is not currently recorded
	HDF_QUERY_DC.id_def = id_def
	HDF_QUERY_DC.x_dpt = x_dpt

;if HDF_QUERY_SD.debug then begin
;help,version,pv,num_pts,id_def,x_dpt,FA,FB,labels,x,y,n,ze
;	print,'num_pts',HDF_QUERY_DC.num_pts
;	print,'id_def',HDF_QUERY_DC.id_def
;	print,'x_dpt',HDF_QUERY_DC.x_dpt
;end

	n_pos = 0
	for i=0,3 do begin
	if id_def(i) gt 0 then begin
		HDF_QUERY_DC.pa(0:num_pts(0),i) = FA(0:num_pts(0),n_pos)
		n_pos = n_pos + 1
		end
	end
	n_det = 0
	for i=4,18 do begin
	if id_def(i) gt 0 then begin
		HDF_QUERY_DC.da(0:num_pts(0),i-4) = FB(0:num_pts(0),n_det)
		n_det = n_det + 1
		end
	end

	for i=0,3 do begin
	HDF_QUERY_DC.x_name(i) = string(labels(0:29,i))
	HDF_QUERY_DC.x_desc(i) = string(labels(0:29,i+19))
	HDF_QUERY_DC.x_engu(i) = string(labels(0:29,i+38))
	end

	for i=4,18 do begin
	HDF_QUERY_DC.y_name(i-4) = string(labels(0:29,i))
	HDF_QUERY_DC.y_desc(i-4) = string(labels(0:29,i+19))
	HDF_QUERY_DC.y_engu(i-4) = string(labels(0:29,i+38))
	end

	for i=0,5 do begin
	HDF_QUERY_DC.plot_labels(i) = x(i)
	end

	HDF_QUERY_DC.seqno = fix(y(0)) 
	HDF_QUERY_DC.scanno = fix(y(0)) + 1
	HDF_QUERY_DC.refno = fix(y(1))
	HDF_QUERY_DC.y_seqno = fix(y(2))
	HDF_QUERY_DC.scanno_2d = fix(y(3))
	HDF_QUERY_DC.y_scan = fix(y(4))

	sz = size(y) 
	if sz(1) eq 6 then begin		; R1.5.4 and later
	HDF_QUERY_DC.y_req_npts = fix(y(5))
	HDF_QUERY_DC.y_act_npts = fix(y(6))
	HDF_QUERY_DC.y_value = y(7)
	end

	HDF_QUERY_DC.no_env = n(0)
	if n_elements(ze) gt 0 then $
	HDF_QUERY_DC.ze = byte(ze)


; create a new group in file

if n_elements(HDF_QUERY_SD) gt 0 then filename = HDF_QUERY_SD.file
if keyword_set(FILE) ne 0 then filename=string(FILE)
if n_elements(filename) eq 0 then begin
        print,'Error: need filename!'
	print,filename
        return
        end

scan_no = HDF_QUERY_DC.scanno
print,'scan_no :',scan_no, HDF_QUERY_DC.scanno

pvname = HDF_QUERY_DC.pv

MAX_ID=100  ;	max no of SD in a group set
sid = make_array(MAX_ID,/int)

time0 = systime(1)

; fid = HDF_OPEN(filename,/RDWR)
fid = HDF_QUERY_SD.fid

if fid gt 0 then begin

sd_id = HDF_SD_START(filename,/RDWR)

;
; create the scan 'entry#' group
;

suffix_string,scan_no,sufix

entry_name = 'entry'+sufix
entry_class = 'NXentry'

; vg_tag = 1965 ; VG
vg_tag = HDF_QUERY_SD.vg_tag

vg_id = HDF_VG_ATTACH(fid,-1,/WRITE)
HDF_VG_SETINFO,vg_id,name=entry_name,class=entry_class

;  create the header SDS byte array for the scan

	scan_write_hdf_header, sd_id, index, ref
	HDF_VG_ADDTR, vg_id, vg_tag, ref

;
;  write scan header attibutes 
;
start_time='not recorded yet'
date = x(4)
	
sds_id = HDF_SD_SELECT(sd_id,index)
HDF_SD_ATTRSET, sds_id, 'start_time#'+sufix, byte(start_time),/BYTE 

HDF_SD_ATTRSET, sds_id, 'end_time#'+sufix, byte(strmid(x(4),0,22)),/BYTE 

HDF_SD_ATTRSET, sds_id, 'title#'+sufix, byte(x(0)),/BYTE 

HDF_SD_ATTRSET, sds_id, 'comment#'+sufix, byte(x(5)),/BYTE 


; pack scan # parameters
;  define and write the  val attributes to the SDS header  data

val = make_array(19,3,/int)
val(0,0) = [ HDF_QUERY_DC.seqno, $
	HDF_QUERY_DC.req_npts, $
	HDF_QUERY_DC.act_npts, $
	HDF_QUERY_DC.refno, $
	HDF_QUERY_DC.y_seqno, $
	HDF_QUERY_DC.scanno_2d, $
	HDF_QUERY_DC.y_scan, $
	HDF_QUERY_DC.no_env $
	]

val(0,1) = HDF_QUERY_DC.x_dpt          ; x_dpt  detector dimensions for pv1
val(0,2) = HDF_QUERY_DC.id_def         ; realtime_id.def


HDF_SD_ATTRSET, sds_id, 'scan_params#'+sufix, val
HDF_SD_ENDACCESS,sds_id

;
;	hdf_write_data, sd_id, val, index,ref,name='Scan_parms#'+sufix
;
; create the scan 'data1' group
;

data_class = 'NXdata'
data_name  = 'data1'

vd_id = HDF_VG_ATTACH(fid,-1,/WRITE)
HDF_VG_SETINFO,vd_id,name=data_name,class=data_class
HDF_VG_INSERT,vg_id,vd_id


;  create the PiDi SDS data array for the scan

	scan_write_hdf_pidi, sd_id, vd_id


HDF_VG_GETTRS,vd_id,tags,refs

HDF_VG_DETACH,vd_id

if n_elements(parent_vg_id) gt 0 then begin
	HDF_VG_INSERT,parent_vg_id,vg_id
;	add the vg_id to the parent
end


HDF_VG_DETACH,vg_id

HDF_SD_END,sd_id
; HDF_CLOSE,fid
end

	if HDF_QUERY_SD.debug then begin
	time1 = systime(1)
	print,'HDF write a scan used time =                   ',time1-time0, ' seconds.'
	end

END


PRO scan_write_hdf_header, sd_id, index, ref
COMMON HDF_QUERY_SD_BLOCK, HDF_QUERY_SD, HDF_QUERY_WID,  HDF_QUERY_DC

st_group = HDF_QUERY_DC.pv+'(#'+ strtrim(HDF_QUERY_DC.scanno,2)+')'
st_scan = HDF_QUERY_DC.pv

; help,HDF_QUERY_DC,/struct


s0 = string(replicate(32b,110))

no = HDF_QUERY_DC.no_env

data = make_array(110,21+no,/byte)

st = s0
strput,st,'Appl_name :',0
strput,st,HDF_QUERY_DC.version,30
data(0,0) = byte(st)

st = s0
strput,st,'Release   :',0
strput,st,HDF_QUERY_DC.release,30
data(0,1) = byte(st)

st = s0
strput,st,'1D SCAN # :',0
strput,st,strtrim(HDF_QUERY_DC.scanno,1),30
data(0,2) = byte(st)

st = s0
strput,st,'2D SCAN # :',0
strput,st,strtrim(HDF_QUERY_DC.scanno_2d,1),30
data(0,3) = byte(st)

st = s0
strput,st,'Y INDEX # :',0
strput,st,strtrim(HDF_QUERY_DC.y_seqno,1),30
data(0,4) = byte(st)

st = s0
strput,st,'SCAN Record Name:',0
strput,st,HDF_QUERY_DC.pv +' '+HDF_QUERY_DC.y_pv,20
data(0,5) = byte(st)

st = s0
strput,st,'Requested NPTS :',0
strput,st,strtrim(HDF_QUERY_DC.req_npts,1),30
data(0,6) = byte(st)

st = s0
strput,st,'Acquired NPTS :',0
strput,st,strtrim(HDF_QUERY_DC.act_npts,1),30
data(0,7) = byte(st)

st = s0
strput,st,'HDF VGroup_name :',0
strput,st,st_group,30
data(0,8) = byte(st)

st = s0
strput,st,'HDF VData_name :',0
strput,st,st_scan,30
data(0,9) = byte(st)

; plot title, comment

st = s0
strput,st,'Plot title :',0
strput,st, HDF_QUERY_DC.plot_labels(0), 30
data(0,10) = byte(st)

st = s0
strput,st,'X-label :',0
strput,st, HDF_QUERY_DC.plot_labels(1), 30
data(0,11) = byte(st)

st = s0
strput,st,'Y-label :',0
strput,st, HDF_QUERY_DC.plot_labels(2), 30
data(0,12) = byte(st)

st = s0
strput,st,'Saved ID Name :',0
strput,st, HDF_QUERY_DC.plot_labels(3), 30
data(0,13) = byte(st)

st = s0
strput,st,'Time Stamp & User ID :',0
strput,st, HDF_QUERY_DC.plot_labels(4), 30
data(0,14) = byte(st)

st = s0
strput,st,'Comment :',0
strput,st, HDF_QUERY_DC.plot_labels(5), 30
data(0,15) = byte(st)

; pack environment fields into data array
;  no

if no gt 0 then begin ;======

	st = s0
	strput,st,'env_field.no :',0
	strput,st,strtrim(no,1),30
	data(*,20)= byte(st)

        for i=0,no-1  do begin
        st = s0
	env = string(HDF_QUERY_DC.ze(0:109,i))
        strput,st,env,0
        len = strlen(st)
        data(*,21+i) = byte(st)
        end

end


suffix_string,HDF_QUERY_DC.scanno,hdrno

hdf_write_data, sd_id, name='header#'+hdrno, data, index, ref

END


PRO scan_write_hdf_pidi, sd_id, vg_id
COMMON HDF_QUERY_SD_BLOCK, HDF_QUERY_SD, HDF_QUERY_WID,  HDF_QUERY_DC

; sd_tag = 720
; vg_tag = 1965 ; VG
sd_tag = HDF_QUERY_SD.sd_tag
vg_tag = HDF_QUERY_SD.vg_tag

;
; determine the position array to be saved in FA

num_pts = HDF_QUERY_DC.num_pts

suffix_string,HDF_QUERY_DC.scanno,sufix

for i=0,3 do begin
        if HDF_QUERY_DC.id_def(i) gt 0 then begin

        data = HDF_QUERY_DC.pa(0:num_pts,i)

	; name, label, unit

	name = HDF_QUERY_DC.x_name(i)+'#'+sufix
	label = 'P'+strtrim(string(i+1),2) 
	if strtrim(HDF_QUERY_DC.x_desc(i),2) ne '' then $
		label = HDF_QUERY_DC.x_desc(i)
	unit =  HDF_QUERY_DC.x_engu(i)

	hdf_write_data, sd_id, data, index, ref, $
		name=name, label=label, unit=unit
	HDF_VG_ADDTR, vg_id, vg_tag, ref

	; axis
	if i eq 0 then begin
        p1 = HDF_QUERY_DC.pa(0:num_pts,0)
	a_name = 'axis'
	a_data = 1 
	sds_id = HDF_SD_SELECT(sd_id,index)
	HDF_SD_ATTRSET, sds_id, a_name, a_data 
	HDF_SD_ENDACCESS,sds_id
	end


        end
end

; determine the detector array to be saved in FB


for i=0,14 do begin
        if HDF_QUERY_DC.id_def(4+i) gt 0 then begin

        data = HDF_QUERY_DC.da(0:num_pts,i)

	name = HDF_QUERY_DC.y_name(i)+'#'+sufix
	label = 'D'+strtrim(string(i+1),2) 
	if strtrim(HDF_QUERY_DC.y_desc(i),2) ne '' then $
		label = HDF_QUERY_DC.y_desc(i)
	unit =  HDF_QUERY_DC.y_engu(i)

	hdf_write_data, sd_id, data, index, ref, $
		name=name, label=label, unit=unit
	HDF_VG_ADDTR, vg_id, vg_tag, ref

	sds_id = HDF_SD_SELECT(sd_id,index)

	; primary signal
	if i eq 0 then begin
	a_name = 'signal'
	a_data = 1 
	HDF_SD_ATTRSET, sds_id, a_name, a_data
	end

;
; need write the voigt attribute for data values FWHM, etc
;
; mean,variance,skewness,kurtosis,sdev,mdev,xpeak,ypeak,yhpeak,centroid,FWHM

	a_name = 'voigt'
	getStatisticDeviation_1d,i,data,mean,sdev,mdev,st
	getStatistic_1d,i,p1,data,c_mass,x_peak,y_peak,y_hpeak,FWHM,st
	a_data = [mean,sdev,mdev,x_peak,y_peak,y_hpeak,c_mass,FWHM]
	; double is returned due to positioner precision
	HDF_SD_ATTRSET, sds_id, a_name, a_data 

	HDF_SD_ENDACCESS,sds_id

        end
end


END

;
; read hdf file  and try to display same as dc
;
PRO scan_all_entry_groups,file=file,vg_ids, vd_ids, dump=dump
COMMON HDF_QUERY_SD_BLOCK, HDF_QUERY_SD

;  vg_ids  - gives  ref number of all entry# found
;  vd_ids  - gives  the corresponding ref number for the 'data1' group

if keyword_set(file) eq 0 then begin
	print,"error: need file='filename'"
	return
	end
fid = HDF_QUERY_SD.fid

sd_id = HDF_SD_START(file)

HDF_SD_FILEINFO, sd_id, no_datasets, no_gattributes
help,file,no_datasets,no_gattributes

HDF_QUERY_SD.numSDS = no_datasets
HDF_QUERY_SD.numGAttr = no_gattributes

id=-1
tmp=(num=0)
while tmp ne -1 do begin
 tmp=HDF_VG_GETID(fid,id)
 if tmp ne -1 then begin
	vg_id = HDF_VG_ATTACH(fid,tmp)
	if vg_id gt -1 then begin
	HDF_VG_GETINFO,vg_id,class=cl,name=nm,nentries=no
	HDF_VG_GETTRS,vg_id,tags,refs
print,tmp,cl,no,nm
if n_elements(tags) then print,'refs=',refs
	HDF_VG_DETACH,vg_id
	end   ; end of vg_id
  end   ; end tmp
 id=tmp
endwhile ; end while
HDF_QUERY_SD.numVG = num


HDF_SD_END,sd_id
END

;
; read hdf file  and try to display same as dc
;
PRO scan_all_entry_groups,file=file,vg_ids, vd_ids, dump=dump
COMMON HDF_QUERY_SD_BLOCK, HDF_QUERY_SD

;  vg_ids  - gives  ref number of all entry# found
;  vd_ids  - gives  the corresponding ref number for the 'data1' group

if keyword_set(file) eq 0 then begin
	print,"error: need file='filename'"
	return
	end
fid = HDF_QUERY_SD.fid

sd_id = HDF_SD_START(file)

HDF_SD_FILEINFO, sd_id, no_datasets, no_gattributes
help,file,no_datasets,no_gattributes

HDF_QUERY_SD.numSDS = no_datasets
HDF_QUERY_SD.numGAttr = no_gattributes

id=-1
tmp=(num=0)
while tmp ne -1 do begin
 tmp=HDF_VG_GETID(fid,id)
 if tmp ne -1 then begin
	vg_id = HDF_VG_ATTACH(fid,tmp)
	if vg_id gt -1 then begin
	HDF_VG_GETINFO,vg_id,class=cl,name=nm,nentries=no
	HDF_VG_GETTRS,vg_id,tags,refs
print,tmp,cl,no,nm
if n_elements(tags) then print,'refs=',refs
	HDF_VG_DETACH,vg_id
	end   ; end of vg_id
  end   ; end tmp
 id=tmp
endwhile ; end while
HDF_QUERY_SD.numVG = num


HDF_SD_END,sd_id
END

PRO scan_dump_hdf,keyword,ind_array,FILE=FILE,no_found
COMMON HDF_QUERY_SD_BLOCK, HDF_QUERY_SD, HDF_QUERY_WID, HDF_QUERY_DC

filename =  HDF_QUERY_SD.file
if n_params() eq 0 then begin
        print,'Usage: scan_dump_hdf, keyword, ind_array, file=file'
        print,''
        print,'  keyword    -  #000i specifies the scan number 
        print,'  ind_array  -  This parameter returns the found SDS index array.
        print,'  no_found   -  This parameters returns the total number of names
        print,'                found which contains the search keyword
        return
        end

st_scan = string(keyword)
if keyword_set(FILE) then filename = FILE

found = findfile(filename)
if found(0) eq '' then open_hdf,filename
sd_id = hdf_sd_start(filename)

hdf_sd_fileinfo, sd_id, no_datasets, no_gattributes
no_found = 0
ind_array = [no_found]

for i=0,no_datasets-1 do begin
        sds_id = hdf_sd_select(sd_id,i)
	HDF_SD_GETINFO,sds_id,name=nm,ndims=nd,natts=na,type=t,dims=d
	found = strpos(nm,st_scan)
	if found ge 0 then begin
        if no_found eq 0 then ind_array(0) = i  else $
                ind_array  = [ind_array, i]
        no_found = no_found+1
        if keyword_set(debug) then begin
                hdf_sd_getdata,sds_id,data
;                help,nd,na,t,d,data
        end
        end
        hdf_sd_endaccess,sds_id
end

	if no_found gt 0 then begin
	HDF_QUERY_SD.fptr = ind_array(0:no_found-1)
	;
	; populate data structure 
	;

	populate_scanData,sd_id,st_scan,ind_array
	
	end

hdf_sd_end,sd_id
END


PRO populate_scanData,sd_id,st_scan,ind_array,debug=debug
COMMON HDF_QUERY_SD_BLOCK, HDF_QUERY_SD, HDF_QUERY_WID, HDF_QUERY_DC
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block,w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved

COMMON env_field_block,env_field

if n_elements(w_plotspec_array) eq 0 then $
w_plotspec_array = make_array(6,/string,value=string(replicate(32b,60)))

if n_elements(env_field) eq 0 then $
env_field = { $
        exist   : 0, $
        no      : 0, $
        noenv   : 0, $
        numkey  : 0, $
        keys    : make_array(scanData.maxenv,/int), $
        pvnames : make_array(scanData.maxenv,/string,value=string(replicate(32b,30))), $
        descs   : make_array(scanData.maxenv,/string,value=string(replicate(32b,40))), $
        values  : make_array(scanData.maxenv,/string,value=string(replicate(32b,40))), $
        oldvalues : make_array(scanData.maxenv,/string,value=string(replicate(32b,40))) $
        }

if HDF_QUERY_SD.debug then print,'Scan',st_scan,ind_array

; header ind_array(0)
	
	sds_id = HDF_SD_SELECT(sd_id,ind_array(0))

	HDF_SD_GETINFO,sds_id,ndims=nd,dims=d
	HDF_SD_GETDATA,sds_id,data
	HDF_SD_ENDACCESS,sds_id

	no_env = d(1)-21 

	header_data = data
	header_lines = d(1)

	temp = data(20:80,5)
	pvs = str_sep(string(temp),' ')
	scanData.pv = pvs(0)	
	scanData.y_pv = pvs(1)	
if HDF_QUERY_SD.debug then print,scanData.pv, scanData.y_pv

	w_plotspec_array(0) = strtrim(string(data(30:100,10)),2)
	w_plotspec_array(1) = strtrim(string(data(30:100,11)),2)
	w_plotspec_array(2) = strtrim(string(data(30:100,12)),2)
	w_plotspec_array(3) = strtrim(string(data(30:100,13)),2)
	w_plotspec_array(4) = strtrim(string(data(30:100,14)),2)
	w_plotspec_array(5) = strtrim(string(data(30:100,15)),2)
	
if HDF_QUERY_SD.debug then begin
for i=0,4 do begin
print,i, '  ',w_plotspec_array(i)
end
end

	no = fix(string(data(30:50,20)))
	if no gt 0 then begin
	env_field.exist = no
	env_field.no = no
	ze = string(replicate(32b,110))

		for i=0,no-1 do begin
		ze = string(data(*,21+i))
		env_field.pvnames(i)=strtrim(strmid(ze,0,30),2)
		env_field.values(i)=strtrim(strmid(ze,30,40),2)
		env_field.descs(i)=strtrim(strmid(ze,70,40),2)
if HDF_QUERY_SD.debug then $ 
print,env_field.pvnames(i),env_field.values(i),env_field.descs(i)
		end
		env_field.pvnames = strtrim(env_field.pvnames,2)
	end	

; Scan_parms : ind_array(1)

;	sds_id = HDF_SD_SELECT(sd_id,ind_array(1))
;	HDF_SD_GETINFO,sds_id,ndims=nd,dims=d
;	HDF_SD_GETDATA,sds_id,data
;	HDF_SD_ENDACCESS,sds_id

	; find scan_params attibutes

	sds_id = HDF_SD_SELECT(sd_id,ind_array(0))
	pindex = HDF_SD_ATTRFIND(sds_id,'scan_params'+st_scan)
	if pindex gt -1 then $
	HDF_SD_ATTRINFO,sds_id,pindex,name=nm,type=ty,count=ct,data=da
	HDF_SD_ENDACCESS,sds_id
	data = make_array(19,3,/int)
	data(0:18,0) = da(0:18)
	data(0:18,1) = da(19:37)
	data(0:18,2) = da(38:56)

	scanData.scanno = data(0,0)
	scanData.req_npts = data(1,0)
	scanData.act_npts = data(2,0)
	scanData.refno = data(3,0)
	scanData.y_seqno = data(4,0)
	scanData.scanno_2d = data(5,0)

if HDF_QUERY_SD.debug then $ 
print,scanData.scanno, scanData.req_npts, scanData.act_npts, scanData.refno, scanData.y_seqno, scanData.scanno_2d

	scanData.x_dpt = data(0:14,1)
	HDF_QUERY_DC.id_def = data(0:18,2)

; Get defined positioner : ind_array(1:*)

	id = 1
	for i=0,3 do begin
		if HDF_QUERY_DC.id_def(i) gt 0 then begin
       	 	sds_id = HDF_SD_SELECT(sd_id,ind_array(id))
       		HDF_SD_GETINFO,sds_id,ndims=nd,dims=d,name=nm,label=lb,unit=un
       	 	HDF_SD_GETDATA,sds_id,data
		HDF_SD_ENDACCESS,sds_id
		id=id+1	
		scanData.pa(0:d-1,i) = data
if HDF_QUERY_SD.debug then print,i,nm,'&',lb,'&',un
		len = strpos(nm,st_scan)
		name = strmid(nm,0,len)
		HDF_QUERY_DC.x_name(i)=name
		HDF_QUERY_DC.x_desc(i)=lb
		HDF_QUERY_DC.x_engu(i)=un
		end
	end
	
; Get Detector names, descs, engus

	num_id = n_elements(ind_array)
	for i=0,14 do begin
		if HDF_QUERY_DC.id_def(4+i) gt 0 and id lt num_id  then begin
       	 	sds_id = HDF_SD_SELECT(sd_id,ind_array(id))

		; find  voigt  attibutes
		vname= 'voigt'
		pindex = HDF_SD_ATTRFIND(sds_id,vname)
		if pindex gt -1 then begin
		HDF_SD_ATTRINFO,sds_id,pindex,name=nm,type=ty,count=ct,data=voigt
		scanData.voigt(0:n_elements(voigt)-1, i) = voigt
		end

       		HDF_SD_GETINFO,sds_id,ndims=nd,dims=d,name=nm,label=lb,unit=un
       	 	HDF_SD_GETDATA,sds_id,data
		HDF_SD_ENDACCESS,sds_id
		scanData.da(0:d-1,i) = data
if HDF_QUERY_SD.debug then print,i,nm,'&',lb,'&',un
		len = strpos(nm,st_scan)
		name = strmid(nm,0,len)
		HDF_QUERY_DC.y_name(i)=name
		HDF_QUERY_DC.y_desc(i)=lb
		HDF_QUERY_DC.y_engu(i)=un

		id = id+1
		end
	end
	

; generate report here


	save_hdf_scan_dump_curr,HDF_QUERY_SD.file+st_scan

	xdisplayfile,HDF_QUERY_SD.file+st_scan,Group=HDF_QUERY_WID.base

END


PRO suffix_string,seqno,str,length=length
; input : seqno
; output : str in 0000
; keyword : length of digits

        len0 = 4
	if keyword_set(length) gt 0 then len0 = fix(length)
	str=string(replicate(48b,len0))
        st = strtrim(seqno,2)
        len = strlen(st)
        strput,str,st,len0-len
END



;
; save ascii file of curr scan record
;
PRO save_hdf_scan_dump_curr,filename
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits , w_plotspec_saved

;filename = strcompress(w_plotspec_array(3),/remove_all)+'.tmp'
openw,unit,filename,/get_lun

shortreport_hdf_data_dump,unit
free_lun, unit

END


; 
; save ascii file of curr scan record 
; 
PRO shortreport_hdf_data_dump, unit
COMMON HDF_QUERY_SD_BLOCK, HDF_QUERY_SD, HDF_QUERY_WID, HDF_QUERY_DC
COMMON CATCH1D_COM, widget_ids, scanData 
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits , w_plotspec_saved
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids
COMMON env_field_block,env_field

printf,unit,"; VERSION: ",scanData.version,' ',scanData.release
if scanData.y_seqno gt 0 or scanData.y_scan gt 0 then begin
printf,unit,"; 2D SCAN #: ",scanData.scanno_2d,",      Y INDEX #:",scanData.y_seqno
        end

printf,unit,"; 1D SCAN #: ",scanData.scanno + 1
printf,unit,"; SCAN Record Name: ",scanData.pv

no = env_field.numkey 
if no gt 0 then begin
printf,unit,'; '
printf,unit,';  KEY PV names got from the catch1d.env'
printf,unit,'; '
;s0 = string(replicate(32b,340))
twd = 18*total(HDF_QUERY_DC.id_def) + 10
s0 = string(replicate(32b,twd))
st = s0
strput,st,'; ',0
strput,st,'PVNAME',2
strput,st,'VALUE',30
strput,st,'DESCRIPTION',60
printf,unit,st

; handle the read in env same as the key env for summary report
noenv = env_field.noenv
if noenv ge no then begin

	for i=0,no-1 do begin
	st = s0
	strput,st,'; ',0
	strput,st,env_field.pvnames(i),2
	strput,st,env_field.values(i),30
	strput,st,env_field.descs(i),60
	printf,unit,st
	end
	
endif else begin

	for i=0,no-1 do begin
	st = s0
	strput,st,'; ',0
	strput,st,env_field.pvnames(env_field.keys(i)),2
	strput,st,env_field.values(env_field.keys(i)),30
	strput,st,env_field.descs(env_field.keys(i)),60
	printf,unit,st
	end
end
end


printf,unit,'; '
printf,unit,"; Saved in:   ",scanData.path+w_plotspec_array(3)
printf,unit,"; Time Stamp: ",w_plotspec_array(4)
printf,unit,"; Comment:    ",w_plotspec_array(5)
printf,unit,'; '
printf,unit,'; SCAN Data:'
printf,unit,'; '

save_data_subset_dump, unit 

printf,unit,' '

END

PRO save_data_subset_dump, unit
COMMON HDF_QUERY_SD_BLOCK, HDF_QUERY_SD, HDF_QUERY_WID, HDF_QUERY_DC
COMMON CATCH1D_COM, widget_ids, scanData 
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved

; get desc & eng units

	names = [HDF_QUERY_DC.x_name,HDF_QUERY_DC.y_name]
	descs = [HDF_QUERY_DC.x_desc,HDF_QUERY_DC.y_desc]
	engus = [HDF_QUERY_DC.x_engu,HDF_QUERY_DC.y_engu]

	no = n_elements(names)
	st = '; Detector           '
        for i=0,no-1 do begin
        if HDF_QUERY_DC.id_def(i) ne 0 then begin
                st = st+ ' '+names(i)
                end
        end

temp_format = '('+scanData.code+scanData.format+')'
temp_digit = fix(scanData.format)

twd = strlen(st) > 18*total(HDF_QUERY_DC.id_def) + 20
s0 = string(replicate(32b,twd))

; print voigt

printf,unit,''
	st = '; Detector           '
        for i=0,14 do begin
        if HDF_QUERY_DC.id_def(4+i) ne 0 then st = st+ ' '+names(4+i)
        end
printf,unit,strtrim(st)
st = s0
strput,st,';  (Desc)',0  &  ij = 30 
        for j=0,14 do begin
        if HDF_QUERY_DC.id_def(4+j) ne 0 then begin
                strput,st,descs(4+j),ij
                ij = ij + temp_digit
                end
        end
printf,unit,strtrim(st)
printf,unit,''

for i=0,10 do begin

	case (i) of 
	0: ti = 'Mean:'
	1: ti = 'Variance:'
	2: ti = 'Skewness:'
	3: ti = 'Kurtosis:'
	4: ti = 'Standard  Deviation:'
	5: ti = 'Mean Abs. Deviation:'
	6: ti = 'Peak at x position:'
	7: ti = 'Peak at y value:'
	8: ti = 'Half peak y value:'
	9: ti = 'Centrod x position:'
	10: ti = 'FWHM:'
	endcase
st = s0
strput,st,ti,0  &  ij = 20
	for j = 0,14 do begin
		if HDF_QUERY_DC.id_def(4+j) ne 0 then begin
		strput,st,string(scanData.voigt(i,j),format=temp_format),ij  
		ij = ij + temp_digit
		end
	end
printf,unit,strtrim(st)
end
printf,unit,''
printf,unit,''

;
; tabulate data array
;

	no = n_elements(names)
	st = ';    I   '
        for i=0,no-1 do begin
        if HDF_QUERY_DC.id_def(i) ne 0 then begin
                st = st+ ' '+names(i)
                end
        end
printf,unit,strtrim(st)

st = s0
strput,st,';  (Desc)',0  &  ij = 17 
        for i=0,no-1 do begin
        if HDF_QUERY_DC.id_def(i) ne 0 then begin
                strput,st,descs(i),ij
                ij = ij + temp_digit
                end
        end
printf,unit,strtrim(st)

st = s0
strput,st,'; (Units)',0  &  ij = 17 
        for i=0,no-1 do begin
        if HDF_QUERY_DC.id_def(i) ne 0 then begin
                strput,st,engus(i),ij
                ij = ij + temp_digit
                end
        end
printf,unit,strtrim(st)

num_npts = scanData.readin_npts
num_npts = scanData.act_npts

for i=0,num_npts-1 do begin
st = s0
strput,st,i,0  &  ij = 10
	for j = 0,3 do begin
		if HDF_QUERY_DC.id_def(j) ne 0 then begin
		strput,st,string(scanData.pa(i,j),format=temp_format),ij  
		ij = ij + temp_digit
		end
	end
	for j = 0,14 do begin
		if HDF_QUERY_DC.id_def(4+j) ne 0 then begin
		strput,st,string(scanData.da(i,j),format=temp_format),ij  
		ij = ij + temp_digit
		end
	end
printf,unit,strtrim(st)
end

END


PRO hdf_write_image, sd_id, data, index, ref, xarr=xarr, yarr=yarr, name=name, $
    range=range, unit=unit, format=format, $
    coordsys=coordsys, label=label, caldata=caldata, fill=fill

if n_params() lt 1 then begin
	print,'Usage:  hdf_write_image,sd_id,data,index,ref,name=name'
	print,' INPUT:
	print,'   sd_id		- opened for sd access
	print,'   data          - SD data to be written
	print,' OUTPUT:
	print,'   index         - created SD dataset index number 
	print,'   ref           - SD reference number
	print,' KEYWORD:
	print,'    name         - required input,
	print,'                   specify the variable name for SD data
	print,'    xarr         - input x array if specified,
	print,'    yarr         - input y array if specified,
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


	HDF_SD_ADDDATA,sds_id,data
	if keyword_set(xarr) then HDF_SD_ATTRSET,sds_id,'xarr',xarr
	if keyword_set(yarr) then HDF_SD_ATTRSET,sds_id,'yarr',yarr

;	ref = HDF_SD_IDTOREF(sds_id)

;	index = HDF_SD_REFTOINDEX(sd_id,ref)

	HDF_SD_ENDACCESS,sds_id

END


PRO image2hdf,filename,unit,maxno,XDR=XDR
COMMON HDF_QUERY_SD_BLOCK, HDF_QUERY_SD,HDF_QUERY_WID , HDF_QUERY_DC
;
; the keyword XDR must be set if the filename is in  XDR binary format 
;

if HDF_QUERY_DC.D2_unit gt 0 then begin
	u_close,HDF_QUERY_DC.D2_unit
	HDF_QUERY_DC.D2_unit = -1
end

testfile = findfile(filename)
if testfile(0) ne '' then begin

	CATCH,error_status
	if error_status ne 0 then begin
		res = dialog_message(!err_string + string(!error),/Info)
		return
	end
	if keyword_set(XDR) then u_openr,unit,filename,/XDR else $
	u_openr,unit,filename
	HDF_QUERY_DC.D2_unit = unit

	; open hdf file  for sd I/O

	hdfname =  filename + '.hdf'
	res = HDF_ISHDF(hdfname)
	if res eq 1 then begin
		ans = dialog_message('Append data to the old file!', $
			title='image2hdf',/question,/default_no)
		if ans eq 'No' then return
		end

	open_hdf,hdfname
	sd_id = HDF_SD_START(hdfname,/RDWR)

	seqno = 0
	id = 0
	HDF_QUERY_DC.D2_fptr = make_array(1000,/long)

	point_lun, unit, 0 

	HDF_QUERY_DC.D2_seqno = 0
        HDF_QUERY_DC.D2_scanno = 0
        HDF_QUERY_DC.D2_scanno_last = 0
	WHILE NOT  EOF(unit) DO BEGIN
	id = id + 1
		u_read,unit,pvs
		if strlen(pvs(0)) eq 0 then begin
			HDF_SD_END,sd_id
			close_hdf
			res = dialog_message('Error: bad input file!',/Error)
			return
			end
		u_read,unit,nos
		u_read,unit,x
		u_read,unit,y
		u_read,unit,image
		point_lun,-unit,pos
		HDF_QUERY_DC.D2_fptr(id) = pos
		scanno_2d = nos(4)
		detector = nos(3) + 1

	;  check for scan # increment

		if HDF_QUERY_DC.D2_scanno_last lt scanno_2d then begin
			HDF_QUERY_DC.D2_scanno_last = scanno_2d
			HDF_QUERY_DC.D2_image_no(scanno_2d-1) = HDF_QUERY_DC.D2_seqno 
			end

	; check for scan # out of sync case ???

		if scanno_2d lt HDF_QUERY_DC.D2_scanno_last and detector eq 1 then begin 
			HDF_QUERY_DC.D2_scanno_last = HDF_QUERY_DC.D2_scanno_last + 1 
			HDF_QUERY_DC.D2_image_no(HDF_QUERY_DC.D2_scanno_last-1) = HDF_QUERY_DC.D2_seqno 
			end

		HDF_QUERY_DC.D2_seqno = HDF_QUERY_DC.D2_seqno + 1 

	if detector gt 1 then HDF_QUERY_DC.D2_image_no(HDF_QUERY_DC.D2_scanno_last) = HDF_QUERY_DC.D2_seqno

		; write HDF image data 

		HDF_SD_ATTRSET,sd_id,'file_name', hdfname
		tname = 'image_'+strtrim(string(HDF_QUERY_DC.D2_seqno),2)
		tunit='detector_'+strtrim(string(detector),2)
		tlabel='scann2d_'+strtrim(string(scanno_2d),2)
		hdf_write_image,sd_id,xarr=x,yarr=y,image,index,ref, $
			name=tname,unit=tunit,label=tlabel
		print,'image2hdf - scan2d #:',scanno_2d, $
			'   image #:',HDF_QUERY_DC.D2_seqno, $
			'   detector #:',detector
	END

	maxno = id
	HDF_QUERY_DC.D2_maxno = maxno
	HDF_QUERY_DC.D2_seqno = maxno-1
	HDF_QUERY_DC.D2_image_no(HDF_QUERY_DC.D2_scanno_last) = maxno 

	HDF_SD_END,sd_id
	close_hdf
	return
endif else begin
	res = dialog_message(['File:',filename,'', 'not found!'],/Error)
end
	
END



;
; Auto Save File For catcher2hdf.pro
;
;  Thu Nov 14 12:11:47 CST 1996
;

PRO scan_read_all_2,filename
COMMON HDF_QUERY_SD_BLOCK, HDF_QUERY_SD,HDF_QUERY_WID , HDF_QUERY_DC

testfile = findfile(filename)
if testfile(0) ne '' then begin

	HDF_QUERY_DC.file = filename

	u_openr,unit,filename
	u_read,unit,ver,errcode
	if errcode ne 0 then begin
	res = widget_message("Error: wrong type of file entered!!");
	return
	end
	if string(ver(0)) ne 'CATCHER_V1' then begin
	u_close,unit
	u_openr,unit,filename,/XDR
	end
	POINT_LUN,unit,0

        status = FSTAT(unit)
        HDF_QUERY_DC.size = status.size
        HDF_QUERY_DC.unit = unit 

        indexFile = status.name + '.index'


; check whether indexFile exist

found = findfile(indexFile)
if found(0) eq '' then begin

        id = 0
        HDF_QUERY_DC.fptr = make_array(10000,/long)

        WHILE NOT  EOF(unit) DO BEGIN
        id = id + 1
                scan_read_record_2,unit
                point_lun,-unit,pos
                HDF_QUERY_DC.fptr(id) = pos
        END
        maxno = id
        HDF_QUERY_DC.maxno = maxno

endif else begin
        u_openr,unit2,indexfile
        u_read,unit2,name
        u_read,unit2,fsize
        u_read,unit2,maxno
        u_read,unit2,array
        u_close,unit2

        if status.size eq fsize(0) then begin
        HDF_QUERY_DC.size = fsize(0)
        HDF_QUERY_DC.maxno = maxno(0)
        HDF_QUERY_DC.fptr = array
        end
;       print,'***Read Index File: ',indexfile

end
end

; create the dummy 'hdf.config' if not exists

found = findfile('hdf.config')
if found(0) eq '' then begin
	openw,unit,'hdf.config',/GET_LUN
	printf,unit,'; '
	close,unit
end

END

PRO scan_read_record_2,unit,version,pv,num_pts,id_def,x_dpt,FA,FB,labels,x,y,n,ze

        u_read,unit,version
        u_read,unit,pv
        u_read,unit,num_pts
        u_read,unit,id_def
        u_read,unit,x_dpt

num_po = 0
for i=0,3 do begin
       if id_def(i) gt 0 then num_po = num_po + 1
end
FA = make_array(num_pts(0)+1,num_po,/double)
for i=0,num_po-1 do begin
        u_read,unit,px
        FA(*,i) = px
end

num_po = 0
for i=4,18 do begin
       if id_def(i) gt 0 then num_po = num_po + 1
end
FB = make_array(num_pts(0)+1,num_po)
for i=0,num_po-1 do begin
        u_read,unit,px
        FB(*,i) = px
end

        u_read,unit,labels
        u_read,unit,x
        u_read,unit,y
        u_read,unit,n
        if n(0) gt 0 then begin
        u_read,unit,ze
        end
END


PRO PDMENU_VWDATA_Event, Event
COMMON HDF_QUERY_SD_BLOCK, HDF_QUERY_SD, HDF_QUERY_WID, HDF_QUERY_DC

if HDF_QUERY_DC.maxno eq 0 then begin
	res=WIDGET_MESSAGE('File '+HDF_QUERY_DC.file+' not found!')
	return
end

  CASE Event.Value OF

  'ViewData.View1D ...': BEGIN
;	str="xterm -e catcher -v -d "+HDF_QUERY_DC.file+" -c hdf.config &"
;	spawn,str
	view1d,data=HDF_QUERY_DC.file
	END
  'ViewData.View2D ...': BEGIN
	view2d,file=HDF_QUERY_DC.file+'.image'
	END
  ENDCASE
END

PRO PDMENU3_Event, Event
COMMON HDF_QUERY_SD_BLOCK, HDF_QUERY_SD, HDF_QUERY_WID, HDF_QUERY_DC

  CASE Event.Value OF 


  'File.Input DC File ...': BEGIN
	F = PICKFILE(TITLE='Pick Data Catcher File ...', $
		GET_PATH=P, PATH=HDF_QUERY_DC.path, FILE='catch1d.trashcan') 

	if strpos(F,P) lt 0 then F = P+F
	; strip tmp_mnt away

	x = F
        first = strpos(x,'/home')
        if first gt 0 then begin
                F = strmid(x,first,strlen(x))
        end

	HDF_QUERY_DC.path = P
	HDF_QUERY_SD.file = F+'.hdf'
	WIDGET_CONTROL,HDF_QUERY_WID.infile,SET_VALUE=F
	WIDGET_CONTROL,HDF_QUERY_WID.outfile,SET_VALUE=HDF_QUERY_SD.file
	scan_read_all_2,F
	WIDGET_CONTROL,HDF_QUERY_WID.ranges, $
		SET_VALUE='Scan # [1-'+strtrim(HDF_QUERY_DC.maxno,2) +']    '

	catcher2hdf_slider
    END
  'File.Output Filename ...': BEGIN
    PRINT, 'Event for File.Output Filename ...'
	F = PICKFILE(TITLE='Output Filename ...',FILE='') 
print,F
	HDF_QUERY_SD.file = F
	WIDGET_CONTROL,HDF_QUERY_WID.outfile,SET_VALUE=F
    END
  'File.Quit': BEGIN
    PRINT, 'Event for File.Quit'
	WIDGET_CONTROL,HDF_QUERY_WID.base,/DESTROY,BAD_ID=bad	
	spawn,'rm catch1d.config.tmp'
;	exit
    END
  ENDCASE
END


PRO catcher2hdf_slider
COMMON HDF_QUERY_SD_BLOCK, HDF_QUERY_SD,HDF_QUERY_WID, HDF_QUERY_DC

	WIDGET_CONTROL,HDF_QUERY_WID.slider,/destroy,bad=badid

	if HDF_QUERY_DC.maxno gt 1 then begin
	SLIDER3 = WIDGET_SLIDER( HDF_QUERY_WID.scanrow, $
      	MAXIMUM=HDF_QUERY_DC.maxno, $
      	MINIMUM=1, $
     	 UVALUE='SLIDER3', $
      	VALUE=1)
	HDF_QUERY_WID.slider = SLIDER3
	WIDGET_CONTROL,HDF_QUERY_WID.begin_no,SET_VALUE=1 
	end
END


PRO CATCHER2HDF_Event, Event
COMMON HDF_QUERY_SD_BLOCK, HDF_QUERY_SD,HDF_QUERY_WID, HDF_QUERY_DC

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  ; Event for PDMENU3
  'PDMENU3': PDMENU3_Event, Event
  'PDMENU_VWDATA': PDMENU_VWDATA_Event, Event
  'GLB_ATTR': BEGIN
	DC2HDFUser,GROUP=Event.top	
	END
  'FIELD17': BEGIN
	WIDGET_CONTROL,HDF_QUERY_WID.infile,GET_VALUE=F
	HDF_QUERY_SD.file = F(0)+'.hdf'
	WIDGET_CONTROL,HDF_QUERY_WID.outfile,SET_VALUE=HDF_QUERY_SD.file
	scan_read_all_2,F(0)
	WIDGET_CONTROL,HDF_QUERY_WID.ranges, $
		SET_VALUE='Scan # [1-'+strtrim(HDF_QUERY_DC.maxno,2) +']    '
	catcher2hdf_slider

      END
  'BEGIN_NUM': BEGIN
	WIDGET_CONTROL,HDF_QUERY_WID.begin_no,GET_VALUE=F
	HDF_QUERY_SD.seqno = fix(F(0))
      Print, 'begin seqno ',HDF_QUERY_SD.seqno
      point_lun, HDF_QUERY_DC.unit, HDF_QUERY_DC.fptr(HDF_QUERY_SD.seqno)
      END
  'END_NUM': BEGIN
	WIDGET_CONTROL,HDF_QUERY_WID.end_no,GET_VALUE=F
	endno = fix(F(0))
	if HDF_QUERY_SD.endno gt HDF_QUERY_DC.maxno then $
	WIDGET_CONTROL,HDF_QUERY_WID.end_no,SET_VALUE=HDF_QUERY_DC.maxno
      Print, 'end seqno ',HDF_QUERY_SD.endno
	HDF_QUERY_SD.endno = endno
      END
  'SLIDER3': BEGIN
	WIDGET_CONTROL,HDF_QUERY_WID.slider,GET_VALUE=val
	if val eq 0 then val=1
	WIDGET_CONTROL,HDF_QUERY_WID.begin_no,SET_VALUE=Val
      END

  'FIELD23': BEGIN
	WIDGET_CONTROL,HDF_QUERY_WID.outfile,GET_VALUE=F
	HDF_QUERY_SD.file = F(0)
      Print, 'output ', HDF_QUERY_SD.file
      END

  'HDF_ACCEPT': BEGIN
	WIDGET_CONTROL,HDF_QUERY_WID.infile,GET_VALUE=F
	HDF_QUERY_DC.file = F(0)
	WIDGET_CONTROL,HDF_QUERY_WID.outfile,GET_VALUE=F
	HDF_QUERY_SD.file = F(0)

	WIDGET_CONTROL,HDF_QUERY_WID.begin_no,GET_VALUE=F
	seqno_1 = fix(F(0))
        if seqno_1 lt 1 or seqno_1 gt HDF_QUERY_DC.maxno then begin
                res=WIDGET_MESSAGE("The scan # out of range !",/INFO, $
			DIALOG_PARENT=Event.top)
		return 
	end
	
	WIDGET_CONTROL,HDF_QUERY_WID.end_no,GET_VALUE=F
	seqno_2 = fix(F(0))
	if seqno_2 lt seqno_1 then seqno_2 = seqno_1
	if seqno_2 gt HDF_QUERY_DC.maxno then seqno_2 = HDF_QUERY_DC.maxno

open_hdf,HDF_QUERY_SD.file
for seqno1 = seqno_1,seqno_2 do begin

	if seqno1 gt 0 then begin

		suffix_string,seqno1,sufix
		st_search = '#'+ sufix 

		scan_dump_hdf,st_search,sds_array,num_found,file=HDF_QUERY_SD.file
		if num_found gt 4 then begin 
                res=WIDGET_MESSAGE("Scan "+st_search+" already found in '"+HDF_QUERY_SD.file+"'",/INFO, $
			DIALOG_PARENT=Event.top)
		goto,nextseq
		end
	end

        point_lun, HDF_QUERY_DC.unit, HDF_QUERY_DC.fptr(seqno1 - 1)
;	open_hdf,HDF_QUERY_SD.file
	scan_write_hdf3,scan_no=seqno1
;	close_hdf
	seq_no = HDF_QUERY_DC.seqno
nextseq:
end
close_hdf
	END

  'HDF_ALL': BEGIN
	WIDGET_CONTROL,HDF_QUERY_WID.infile,GET_VALUE=F
	HDF_QUERY_DC.file = F(0)
	WIDGET_CONTROL,HDF_QUERY_WID.outfile,GET_VALUE=F
	HDF_QUERY_SD.file = F(0)

	WIDGET_CONTROL,HDF_QUERY_WID.begin_no,GET_VALUE=F
	seqno1 = fix(F(0))
        if seqno1 lt 1 or seqno1 gt HDF_QUERY_DC.maxno then begin
                res=WIDGET_MESSAGE("The scan # out of range !",/INFO, $
			DIALOG_PARENT=Event.top)
		return 
	end
	
open_hdf,HDF_QUERY_SD.file
for seqno1=1, HDF_QUERY_DC.maxno do begin
	if seqno1 gt 0 then begin

		suffix_string,seqno1,sufix
		st_search = '#'+ sufix 

		scan_dump_hdf,st_search,sds_array,num_found,file=HDF_QUERY_SD.file
		if num_found gt 4 then begin 
                res=WIDGET_MESSAGE("Scan "+st_search+" already found in '"+HDF_QUERY_SD.file+"'",/INFO, $
			DIALOG_PARENT=Event.top)
		return
		end
	end

        point_lun, HDF_QUERY_DC.unit, HDF_QUERY_DC.fptr(seqno1 - 1)
;	open_hdf,HDF_QUERY_SD.file
	scan_write_hdf3,scan_no=seqno1
;	close_hdf
end
close_hdf
	END

  'HDF_CANCEL': BEGIN
	WIDGET_CONTROL,HDF_QUERY_WID.base,/DESTROY,BAD_ID=bad
	close_hdf
	u_close,HDF_QUERY_DC.unit
	HDF_QUERY_DC.unit = -1
	END
  'HDF_DUMP': BEGIN
	WIDGET_CONTROL,HDF_QUERY_WID.outfile,GET_VALUE=F
	HDF_QUERY_SD.file = F(0)
	WIDGET_CONTROL,HDF_QUERY_WID.begin_no,GET_VALUE=F
	seqno1 = fix(F(0))
	if seqno1 gt 0 then begin

		suffix_string,seqno1,sufix
		st_search = '#'+ sufix 

		scan_dump_hdf,st_search,sds_array,num_found,file=HDF_QUERY_SD.file
		if num_found eq 0 then $ 
                res=WIDGET_MESSAGE(" Scan "+st_search+" not found in file '" + HDF_QUERY_SD.file +"'",/INFO, $
			DIALOG_PARENT=Event.top)

	end
	END
  'HDF_DEBUG': BEGIN
	HDF_QUERY_SD.debug = Event.Select
      END
  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END CATCHER2HDF
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO catcher2hdf, GROUP=Group, DEBUG=DEBUG
COMMON HDF_QUERY_SD_BLOCK, HDF_QUERY_SD, HDF_QUERY_WID , HDF_QUERY_DC
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  IF XRegistered('CATCHER2HDF') THEN RETURN

if n_elements(w_plotspec_id) eq 0 then $
w_plotspec_id = { $
	color : 1, $     ;  0 no color curve, 1 color curve 
        type:   0, $
	solid: 1, $ 		; 1 solid line only
        log:    0, $
        grid:   0, $
        mode:   1, $		; default is viewing mode
        x_axis_u :   0, $
        autosave:       0, $
	scan: 0,$
        errbars: 0, $
	opened: 0, $
	realtime: 1, $
	itime: 1, $
	dtime: 0.2, $
        xcord: 0, $
	seqno: 0, $
	statistic: 0, $
	xticklen: 0.04, $
	yticklen: 0.02, $
	gridstyle: 0, $
	colorI: make_array(19,/INT,value=!d.n_colors-1) $ 
        }

	if w_plotspec_id.color eq 1 then begin
        LOADCT, 39
        dcl = !d.n_colors - 1
        colorlevel = !d.n_colors / 4 
        for i=0,18 do begin
	ii = i / 4
	im = i mod 4
	w_plotspec_id.colorI(i) = dcl - ii - im * colorlevel
	end
	end

   scanData  =  {                 $
debug : 0,    $   ;  0 normal runing mode, 1 debug mode
option : 0, $    ; 0 ->view mode only, 1 ->scan/view both allowed
nosave: 0, $   ; 0 autosave on, 1 no automatic save used by command line
      version     : 'CATCHER_V1', $
      release     : '(R2.2.2)', $
      pv          : '',           $
      pvconfig    : '',           $
      pvwait      : '',           $
      pvbusy      : '',           $
      pvfound     : -1, 	  $  indicator for pv found or not found
      num_pos	  : 4, 		  $  max no of pos allowed   
      num_det     : 15, 	  $  max no of detectors allowed 
      home	  : '', $
      envfile	  : 'catch1d.env', $
      maxenv 	  : 500, $
      config      : 'catch1d.config', $
      path  	  : '', $
      trashcan    : 'catch1d.trashcan', $
      code        : 'G', $
      format      : '18.8', $
      req_npts    : 100,          $
      act_npts    : 0,          $
      readin_npts    : 0,          $
      	px         : FLTARR(4000), $
      	dx         : make_array(4000,/double), $
	pa : Make_array(4000,4,/double), $
	da : FLTARR(4000,15), $	
	voigt : make_array(11,15,/double), $
      lastPlot    : -1,   $   ; <0 not plot data, 0 userscale, 1 autoscale 
      scanno	  : 0,            $   ; current plot no 
      startno	  : 0,            $   ; start no when catch1d is brought up
      refno       : 0,            $   ; environments ref no saved in data   
      showlist	  : 0,            $   ; list life scan data ?
      readpv      : 0, 		    $     ; if 1 read from scan_pvnames 
      pvfile	  : 'scan_pvnames', $     ; list of monitor pvnames 
      nonames	  : 0, $
      plotspec    : w_plotspec_id, $
      realtime    : 0, 	  	  $   ; realtime scan monitor indicator 
; add 2D scan parameters here
      scanno_2d	  : 0,            $   ; current 2D scan no 
      y_handshake      : '',           $ ; 2d handshake pvname
      image     : 0, $		; default to first detector image 
      y_pv	: '', $
      y_scan 	: 0, $		; if 1 2D scan is goring on
      y_req_npts    : 1,          $
      y_act_npts    : 0,          $
      x_dpt 	: make_array(15,/INT,value=1), $ ; data dim for X detectors
      y_dpt 	: make_array(15,/INT,value=1), $ ; data dim for Y detectors
      x_dtype 	: make_array(15,/INT,value=4), $ ; float for X detectors 
      y_dtype 	: make_array(15,/INT,value=4), $ ; float for Y detectors
      y_cur_pt  : 0, 	$  ; y index no from IOC
      y_seqno   : 0 	$  ; y dim scan seqno
   }

if n_elements(HDF_QUERY_DC) eq 0 then $ 
    HDF_QUERY_DC = { $
	file : scanData.trashcan+'.hdf', $
	path : '', $
	unit : -1, $
	maxno : 0, $
	seqno : 0, $
        size: 0L, $
	fptr : make_array(10000,/long), $
	pv: '',$
	y_pv:'',$
	scanno: 0, $
	refno: 0, $
	y_seqno: 0, $
	scanno_2d: 0, $
	y_scan: 0, $
	version: scanData.version + ' --> DC2HDF_V1' ,$
	release: scanData.release + ' --> NEXUS 1.24',$
	req_npts: 0,$
	act_npts: 0,$
	num_pts: '',$
	y_req_npts: 0,$
	y_act_npts: 0,$
	id_def: make_array(19,/int), $
	x_dpt: make_array(15,/int), $
	pa: make_array(4000,4,/double), $
	da: make_array(4000,15,/float), $
	x_name: make_array(4,/string,value=string(replicate(32b,30))), $
	x_desc: make_array(4,/string,value=string(replicate(32b,30))), $
	x_engu: make_array(4,/string,value=string(replicate(32b,30))), $
	y_name: make_array(15,/string,value=string(replicate(32b,30))), $
	y_desc: make_array(15,/string,value=string(replicate(32b,30))), $
	y_engu: make_array(15,/string,value=string(replicate(32b,30))), $
	plot_labels: make_array(6,/string,value=string(replicate(32b,60))), $
	no_env: 0, $
	ze : make_array(110,1000,/byte), $
; 2D image variable
	D2_unit : -1, $       ;  opened unit for image file
	D2_fptr : make_array(1000,/long), $   ; image data pointer
	D2_seqno : 0, $         ; 2D image seqno
	D2_maxno : 0, $		; total # of 2D images
	D2_scanno : 0, $	; 2D scan no
	D2_scanno_last : 0, $	; current scanno of the image
	D2_image_no : make_array(1000,/int) $	; image no at change of 2D scanno
	}	
	

if n_elements(HDF_QUERY_SD) eq 0 then $
   HDF_QUERY_SD = { $
      file    : 'catch1d.trashcan.hdf', $
      fid     : 0L, $
      sd_id   : 0L, $
      key_id  : 'new.dat', $
      debug   : 0, $
      numSDS  : 0L, $
      numVG   : 0L, $
      numVD   : 0L, $
     numGAttr : 0L, $
      sds_id  : 0L, $
      seqno   : 0, $
	endno : 0, $
      maxno   : 0, $
      dptr: make_array(10000,/long), $
      fptr: make_array(10000,/long), $
	vg_tag : 1965, $
	sd_tag : 720, $
	user_name : '',$
	user_mail : '',$
	user_email : '',$
	user_phone : '',$
	user_fax : '',$
	user_history : '',$
	desc_nline : 1, $
	user_desc : strarr(100) $   ; 100 lines of desciptions per desc entry
        }

  CD,current=p
  HDF_QUERY_DC.path = p

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  CATCHER2HDF = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, TITLE='Catcher2HDF', $
      MAP=1, $
      UVALUE='CATCHER2HDF')

  BASE2 = WIDGET_BASE(CATCHER2HDF, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2')

  MenuDesc167 = [ $
      { CW_PDMENU_S,       3, 'File' }, $ ;        0
        { CW_PDMENU_S,       0, 'Input DC File ...' }, $ ;        1
;        { CW_PDMENU_S,       0, 'Output Filename ...' }, $ ;        2
        { CW_PDMENU_S,       2, 'Quit' } $  ;      3
  ]

  PDMENU3 = CW_PDMENU( BASE2, MenuDesc167, /RETURN_FULL_NAME, $
      UVALUE='PDMENU3')

  MenuDesc177 = [ $
      { CW_PDMENU_S,       3, 'ViewData' }, $ ;        0
        { CW_PDMENU_S,       0, 'View1D ...' }, $ ;        1
        { CW_PDMENU_S,       2, 'View2D ...' } $  ;      3
  ]

  PDMENU_VWDATA = CW_PDMENU( BASE2, MenuDesc177, /RETURN_FULL_NAME, $
      UVALUE='PDMENU_VWDATA')

  LABEL14 = WIDGET_LABEL( CATCHER2HDF, $
      FONT='-adobe-helvetica-bold-r-normal--18-180-75-75-p-103-iso8859-1', $
      UVALUE='LABEL14', $
      VALUE='Port Catcher Data to HDF')

  FieldVal1015 = [ $
    '' ]
  FIELD17 = CW_FIELD( CATCHER2HDF,VALUE=FieldVal1015, $
      ROW=1, RETURN_EVENTS=1,$
      STRING=1, $
      TITLE='Input Data Filename', $
      UVALUE='FIELD17', $
      XSIZE=70)

  BASE19 = WIDGET_BASE(CATCHER2HDF, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE19')

  LABEL24 = WIDGET_LABEL( BASE19, $
      UVALUE='LABEL24', /DYNAMIC_RESIZE, $
      VALUE='Scan # [0-0]  ')

  btn1 = WIDGET_BUTTON(BASE19,VALUE='Write HDF Scan',UVALUE='HDF_ACCEPT')
  FieldVal1322 = [ $
    '1' ]
  BEGIN_NUM = CW_FIELD( BASE19,VALUE=FieldVal1322, $
      ROW=1, RETURN_EVENTS=1,$
      STRING=1, XSIZE=6, $
      TITLE='Begin #', $
      UVALUE='BEGIN_NUM')

  FieldVal1387 = [ $
    '1' ]
  END_NUM = CW_FIELD( BASE19,VALUE=FieldVal1387, $
      ROW=1, RETURN_EVENTS=1,$
      STRING=1, XSIZE=6, $
      TITLE='End #', $
      UVALUE='END_NUM')

  BASE30 = WIDGET_BASE(CATCHER2HDF, $
	FRAME=1, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE30')

  FieldVal1470 = [ $
    'catch1d.hdf' ]
  FIELD23 = CW_FIELD( BASE30,VALUE=FieldVal1470, $
      ROW=1, /NOEDIT, $
      STRING=1, $
      TITLE='Output HDF Filename', $
      UVALUE='FIELD23', $
      XSIZE=75)

  BASE31 = WIDGET_BASE(BASE30, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE31')

  GLB_ATTR = WIDGET_BUTTON(BASE31,VALUE='GLB_Attr ...', $
      UVALUE='GLB_ATTR')

  btn1 = WIDGET_BUTTON(BASE31,VALUE='HDF_Report #',UVALUE='HDF_DUMP')

if keyword_set(DEBUG) then begin
  Btns167 = [ $
    '' ]
  HDF_DEBUG = CW_BGROUP( BASE2, Btns167, $
      ROW=1, $
      NONEXCLUSIVE=1, $
      LABEL_LEFT='Debug On/Off', $
      UVALUE='HDF_DEBUG')

  btn2 = WIDGET_BUTTON(BASE2,VALUE='Cancel',UVALUE='HDF_CANCEL')
  btn3 = WIDGET_BUTTON(BASE2,VALUE='DC2HDF Timing',UVALUE='HDF_ALL')
end

  WIDGET_CONTROL, CATCHER2HDF, /REALIZE

   HDF_QUERY_WID = { $
	base: CATCHER2HDF, $
	scanrow: BASE19, $
	ranges: LABEL24, $
	slider: 0L, $
	infile: FIELD17, $
	begin_no: BEGIN_NUM, $
	end_no: END_NUM, $
	outfile: FIELD23 $
	}

	found = findfile(scanData.trashcan)

	WIDGET_CONTROL,HDF_QUERY_WID.infile,SET_VALUE= scanData.trashcan
if strlen(found(0)) gt 1 then begin
	scan_read_all_2,scanData.trashcan
	WIDGET_CONTROL,HDF_QUERY_WID.outfile,SET_VALUE=scanData.trashcan+'.hdf' 
	WIDGET_CONTROL,HDF_QUERY_WID.ranges, $
		SET_VALUE=' Scan # [1-'+strtrim(HDF_QUERY_DC.maxno,2) +']    '

	catcher2hdf_slider
endif else begin
	res = widget_message('File : '+scanData.trashcan+' not found!',/Info, $
		dialog_parent=HDF_QUERY_WID.base)
end

  XMANAGER, 'CATCHER2HDF', CATCHER2HDF, NO_BLOCK=1  ; R5.0
;  XMANAGER, 'CATCHER2HDF', CATCHER2HDF  ; R4.01
;  XMANAGER,CATCH=0

END
