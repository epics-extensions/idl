;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
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

FUNCTION u_writePermitted,filename,VT=VT
;
; check for filename write permission
;
; existed
	found = findfile(filename)
	if found(0) ne '' then begin
	ret=''
	if keyword_set(VT) then $ 
	read,ret,prompt='Overwrite the existed file - '+filename+' (Yes/No) ?' else $
	ret= widget_message(['Do you want to overwrite the existed file : ',$
		'','     '+filename], $
			/question)
	if strupcase(ret) eq 'NO' then return,-1
	end
; create new
        CATCH,error_status
;        if !error_state.name eq 'IDL_M_CNTOPNFIL' then begin 
	if error_status eq -215 or error_status eq -206 then begin
		if keyword_set(VT) then $
		read,ret,prompt=!err_string+string(!err) else $
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
;		exit
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
;       07-27-00    bkc Add readu and rewind to take care the read error 
;                       for WIN system 
;-
if keyword_set(help) then goto, help1
if n_elements(filename) eq 0 then filename='data.dat'

if keyword_set(XDR) then  $
	openr,/XDR,unit,filename,/GET_LUN $
else $
	openr,unit,filename,/GET_LUN
	if !d.name eq 'WIN' then begin
		readu,unit,s
		u_rewind,unit
	end

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
close,unit
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
if error_status  then begin 
	str = [ !err_string + string(!err),'', $
		'Error: unable to read data, wrong type of file opened!!' ]
	ret=WIDGET_MESSAGE(str)
	return
	end

IF EOF(unit) THEN begin
	print,'Error! Error! Error!'
	print,'Error: wrong type or bad data encountered'
	return 
END
s = lonarr(5)
readu,unit,s

if (s(0) gt 1L) then begin	; two dim
	type = s(3)
	int = s(4)
endif else if (s(0) eq 1L) then begin    ; one dim
	type = s(2)
	int = s(3)
endif else if (s(0) eq 0) then begin     ; scalor
	type = s(1)
	int = s(2)
end
;print,s
;print,'type=',type, '  dim=',int
case fix(type) of 
	1: if (s(0) eq 2) then begin		; byte
		x = make_array(s(1),s(2),/byte) 
	   endif else begin
		x = bytarr(int)  	
	   end
	2: if (s(0) eq 2) then begin		; int
		x = make_array(s(1),s(2),/int) 
	   endif else begin
		x = intarr(int)  	
	   end
	3: if (s(0) eq 2) then begin		; long 
		x = make_array(s(1),s(2),/long) 
	   endif else begin
		x = lonarr(int)  	
	   end
	4: if (s(0) eq 2) then begin		; float
		x = make_array(s(1),s(2),/float) 
	   endif else begin
		x = fltarr(int)  	
	   end
	5: if (s(0) eq 2) then begin		; double 
		x = make_array(s(1),s(2),/double) 
	   endif else begin
		x = make_array(int,/double)  	
	   end
	6: if (s(0) eq 2) then begin		; complex
		x = make_array(s(1),s(2),/complex) 
	   endif else begin
		x = make_array(int,/complex)  	
	   end
	7: if (s(0) eq 2) then begin		; string
		print,'Error u_write/u_read can only support single string array'
		print,'size=',s
		return
	   endif else begin
		x = make_array(int,/string,value=string(replicate(32b,s(4))))  	
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
IF NOT EOF(unit) THEN u_read_set,unit,s,x,ERRCODE 
return

help1:
	print,''
	print,'Usage: u_read, unit, x'
	print,''
	print,'This routine reads an array from the LUN unit.'
	print,'Note that the data must be recorded by the u_write routine.'
	print,'       X    returned array'

END


PRO u_bi2xdr,filename,help=help,VT=VT
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
;       U_BI2XDR, 'filename' [,/VT] [,/Help]
;
; INPUTS:
;       filename:   The data file should contain pure binary data objects.
;
; OUTPUTS:
;       filename.xdr:   Output file. 
;                   It contains the converted XDR binary data objects.
;
; KEYWORD PARAMETERS:
;       VT:     If a dumb terminal without X window server is used, 
;               this option must be set, e.g. a telnet session.
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
	if found(0) eq '' then begin
		print,'Error: '+filename+' not found!'
		return
		end
	if keyword_set(VT) then $
	OK_WRITE = u_writePermitted(filename+'.xdr',/VT) else $
	OK_WRITE = u_writePermitted(filename+'.xdr')
	if OK_WRITE lt 0 then return

        id=0
        u_openr,unit,filename
;        u_openw,unit2,filename+'.xdr',/XDR
	openw,/XDR,unit2,filename+'.xdr',/GET_LUN  

        WHILE NOT  EOF(unit) DO BEGIN
        id = id + 1
        u_read,unit,x
	u_write,unit2,x
        END
        maxno = id
        u_close,unit
        u_close,unit2
	if keyword_set(VT) then $
	print,string(maxno)+' sets of binary objects saved in "'+ filename+'.xdr"' else $
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

PRO u_xdr2bi,filename,help=help,VT=VT
;+
; NAME:
;       U_XDR2BI
;
; PURPOSE:
;       This IDL routine converts platform-independent XDR data into
;       native binary data. 
;
;       The output filename uses the input filename suffixed with '.2bi'.
;
; CALLING SEQUENCE:
;
;       U_XDR2BI, 'filename' [,/VT] [,/Help]
;
; INPUTS:
;       filename:   The data file should contain XDR binary data objects.
;
; OUTPUTS:
;       filename.2bi:   Output file. 
;                   It contains the converted native binary data objects.
;
; KEYWORD PARAMETERS:
;       VT:     If a dumb terminal without X window server is used, 
;               this option must be set, e.g. a telnet session.
;       HELP:   If this keyword is set, a simple on line help is given.
;
; RESTRICTIONS:
;       The XDR input file should be created by the u_write command.
;
; EXAMPLE:
;
;        U_XDR2BI,'catch1d.trashcan.xdr'
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 08-10-98.
;
;       xx-xx-xx      iii  comment     
;-
;

if n_elements(filename) eq 0 or keyword_set(help) then goto,help1

	found = findfile(filename)
	if found(0) eq '' then begin
		print,'Error: '+filename+' not found!'
		return
		end
	if keyword_set(VT) then $
	OK_WRITE = u_writePermitted(filename+'.2bi',/VT) else $
	OK_WRITE = u_writePermitted(filename+'.2bi')
	if OK_WRITE lt 0 then return

        id=0
        u_openr,unit,filename,/XDR
        u_openw,unit2,filename+'.2bi'

        WHILE NOT  EOF(unit) DO BEGIN
        id = id + 1
        u_read,unit,x
	u_write,unit2,x
        END
        maxno = id
        u_close,unit
        u_close,unit2
	if keyword_set(VT) then $
	print,string(maxno)+' sets of binary objects saved in "'+ filename+'.2bi"' else $
        ret=WIDGET_MESSAGE(string(maxno)+' sets of binary objects saved in "'+ $
		filename+'.2bi"')

	return

help1:

	print,''
	print,'Usage: U_XDR2BI,"filename"'
	print,''
	print,'This program converts the xdr data objects into native binary format.'
	print,'A new file "filename.2bi" will be created.'
	print,''
END

