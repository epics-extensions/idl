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
if error_status  eq -229 or error_status eq -219 or error_status eq -184 then begin 
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
;
; this routine is requied for generate the runtime executable
;
PRO os_init
;+
; NAME:
;	OS_INIT
;
; PURPOSE:
;       Defines the structure of main operating system variables. All the 
;       operating system dependent varibles used in data catcher are 
;       assembled in this routine. 
;
; CATEGORY:
;       Global System Variables !os.
;
; CALLING SEQUENCE:
;       OS_INIT
;
; PARAMETER FIELDS:
;       ARCH:           IDL detected operating system architecture
;       OS:             IDL detected operating system 
;       OS_FAMILY:      IDL detected operating system family
;       RELEASE:        IDL release number
;       FONT:           Bold font used in highlight label in dialog
;       DEVICE:         Default output device
;       FILE_SEP:       Operating sytem file separator, '/' for unix '\' for W95
;       CHMOD:          Command change file permission mode, 'chmod'
;       MV:             Command rename file, 'mv' for unix, 'rename' for W95
;       CP:             Command copy file, 'cp' for unix, 'copy' for W95
;       RM:             Command remove file, 'rm' for unix, 'del' for W95
;       LPR:            Command print PS file, 'lpr' for unix, 'print' for W95
;       PRT:            Command print text file, 'enscript' for unix, 'print' for W95
;       PRINTER:        Default printer name, '' 
;       WC:             Command return line count, 'wc' for unix
;
; COMMON BLOCKS:
;       COMMON SYSTEM_BLOCK
;
; SIDE EFFECTS:
;       This routine defines the OS_SYSTEM structure and the global 
;       system variable !os. All the system dependent varialbes used in
;       data catcher and data viewer are kept in this routine.
;
; RESTRICTIONS:
;       Current version works for Unix and W95 operating system. 
;
; PROCEDURE:
;       Porting to other operating system, the corresponding
;       fields in 'os.init' may need to be modified accordingly.
;
; EXAMPLE:
;
;               OS_INIT
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 6-01-97.
;       xx-xx-xx iii  - comment
;-
@os.init
END

;
; PS_open,'name.ps'               name defalut to idl
;
PRO PS_init
COMMON PRINTER_BLOCK,printer_info
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

if n_elements (printer_info) eq 0 then $
  printer_info = { $
	name: '', $
	color: 1, $
	reverse: 0, $
	base: 0L, $
	ptr_field:0L }
; inherit from the parent process
;if n_elements(r_curr) eq 0 then begin
;	LOADCT,39  
;	end
END

PRO PS_open,psfile,TV=TV
;+
; NAME:
;       PS_OPEN
;
; PURPOSE:
;       This routine sets the current graphics device to PostScript.
;       and saves plot in a user specified PostScript file.
;
; CALLING SEQUENCE:
;       PS_OPEN, 'myfile.ps' [,/TV]
;
; INPUTS:
;       myfile.ps:  Specifies the PostScript filename to be saved.
;
; OUTPUTS:
;       The PostScript graphic output is saved which can be sent to
;       any PostScript printer or viewer. 
;
; KEYWORD PARAMETERS:
;       TV:       Specifies whether reverse color video to be used in PS. 
;
; COMMON BLOCKS:
;       COMMON PRINTER_BLOCK
;       COMMON COLORS 
;
; RESTRICTIONS:
;       The program 'PS_open.pro' must be loaded into IDL first before 
;       calling this routine.
;
; EXAMPLE:
;
;        PS_OPEN, 'myfile.ps'
;        tvscl,scan
;        PS_CLOSE
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       07-28-97   bkc  Add the support for color PostScript
;                       Add the support for reverse video
;                       Add handling capability for different operating system
;       05-15-98   bkc  Change the reverse video to reverse legend color for
;                       2D TV plot, to get reverse video use the xloadct's
;                       option, reverse feature  
;-

COMMON PRINTER_BLOCK,printer_info
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

	PS_init
	set_plot,'PS'
	!P.FONT=0
	if (n_elements(psfile) ne 0) then begin

	if keyword_set(TV) then begin 

	; use xloadct reverse video, reverse legend only  


	    if printer_info.color gt 0 then $
		device,filename=psfile,/color,bits=8, $
			/Courier,/Bold, $
			 yoffset=7, xsize=15, ysize=15  else  $
		device,filename=psfile,/Courier,/Bold
	endif else begin
	    if printer_info.color gt 0 then $
		device,filename=psfile,/color,bits=8, $
			/Courier,/Bold, $
			yoffset=7, xsize=17.78, ysize=12.7  else $
		device,filename=psfile,/Courier,/Bold
	end

	end
END

PRO PS_close
;+
; NAME:
;       PS_CLOSE
;
; PURPOSE:
;       This routine closes the PostScript output device and resets the
;       the original system graphic device as the output plot device.
;
; CALLING SEQUENCE:
;       PS_CLOSE
;
; INPUTS:
;       None.
;
; OUTPUTS:
;       None.
;
; KEYWORD PARAMETERS:
;       None.
;
; COMMON BLOCKS:
;       COMMON SYSTEM_BLOCK
;       COMMON COLORS 
;
; RESTRICTIONS:
;       The program 'PS_open.pro' must be loaded into IDL prior calling
;       this routine. 
;
; EXAMPLE:
;
;        PS_OPEN, 'myfile.ps'
;        tvscl,scan
;        PS_CLOSE
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       07-28-97   bkc  Add the support for reverse PostScript color scheme. 
;                       Add handling capability for different operating system
;-

COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

	if !d.name eq 'PS' then begin
	!P.FONT=-1
	device,/close

	r_curr = r_orig
	g_curr = g_orig
	b_curr = b_orig
	TVLCT,r_orig,g_orig,b_orig

	set_plot,OS_SYSTEM.device
	end
END

PRO PS_enscript,fileName
;+
; NAME:
;       PS_ENSCRIPT
;
; PURPOSE:
;       This routine uses the system printing command to print
;       an ASCII text file. On unix operating system the command
;       'enscript -r' is used. 
;
; CALLING SEQUENCE:
;       PS_ENSCRIPT, 'filename'
;
; INPUTS:
;       filename : Specifies the ASCII filename to be printed.
;
; OUTPUTS:
;       A copy of the specified file is sent to the user selected  
;       printer.
;
; KEYWORD PARAMETERS:
;
; COMMON BLOCKS:
;       COMMON SYSTEM_BLOCK
;
; RESTRICTIONS:
;       The program 'PS_open.pro' must be loaded into IDL prior calling
;       this routine. 
;
; EXAMPLE:
;
;        PS_ENSCRIPT, 'myfile'
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       07-28-97   bkc  Add handling capability for different operating system
;-
COMMON SYSTEM_BLOCK,OS_SYSTEM

	if n_elements(fileName) eq 0 then begin
		print,'Usage: PS_enscript, <fileName>'
		return
	end
	if strtrim(fileName,2) eq '' then begin
		print,'Usage: PS_enscript, <fileName>'
		return
	end
	if OS_SYSTEM.os_family eq 'unix' then $
	spawn,[OS_SYSTEM.prt, OS_SYSTEM.printer ,  '-r', fileName], /noshell else $
	spawn,[OS_SYSTEM.prt, fileName, OS_SYSTEM.printer]
END

PRO PS_print,psfile
;+
; NAME:
;       PS_PRINT
;
; PURPOSE:
;       This routine uses the system printing command to print
;       a PostScript or ASCII text file. On the unix operating 
;       system the command 'lpr' is used. 
;
; CALLING SEQUENCE:
;       PS_PRINT, 'myfile.ps'
;
; INPUTS:
;       myfile:    Specifies either the PostScript or ASCII text filename 
;                  to be printed.
;
; OUTPUTS:
;       A copy of the specified file is sent to the user selected  
;       printer.
;
; KEYWORD PARAMETERS:
;
; COMMON BLOCKS:
;       COMMON SYSTEM_BLOCK
;
; RESTRICTIONS:
;       The program 'PS_open.pro' must be loaded into IDL prior calling
;       this routine. 
;
; EXAMPLE:
;
;        PS_PRINT, 'myfile.ps'
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       07-28-97   bkc  Add handling capability for different operating system
;       05-14-98   bkc  Add the checking for unreadable color on the PS plot
;                       On unix if the color is too light use the gv to preview 
;			pops up setup printer and info dialog
;-
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON PRINTER_BLOCK,printer_info
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

	if (n_elements(psfile) ne 0) then begin 
		if strtrim(psfile,2) eq '' then begin
			print,'Usage: PS_print, <filename>'
			return
		end
	end else psfile = 'idl.ps'

	if OS_SYSTEM.os_family eq 'unix' then begin
        	str =  OS_SYSTEM.lpr + ' ' + OS_SYSTEM.printer +  psfile 
		color = r_curr(0) + g_curr(0)*256L + b_curr(0)*256L ^2
		if color ge 16777200 then begin 
			temp = ['Warning:','',$
			 'There may be problem of unreadable title or legend on PS plot.',$
			'The ghostview is brought up for you to preview the PS plot.', $
			'If the PS plot looks fine you may use the ghostview to send the',$
			'print job and then close the ghostview and Printer Setup Dialog.',$
			'','If you can not see the title and legend, please close', $
			'the ghostview program first, try different color table or set the ',$
			'Reverse_Legend_Color to "Y" in Printer Setup Dialog first then', $
			'try Print again'] 
			res=dialog_message(temp,/info,title='PS legend problem')
			PS_printer
			spawn,'gv '+psfile + ' &'
		endif else spawn,str
	endif else begin
		str = OS_SYSTEM.lpr + ' ' + psfile + ' ' + OS_SYSTEM.printer
	        spawn,str
	end
	print,str
END


PRO PS_printer_Event, Event
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON PRINTER_BLOCK,printer_info

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'PS_REVERSE': BEGIN
	printer_info.reverse = Event.Index
      END

  'BGROUP3': BEGIN
      CASE Event.Value OF
      0: begin
		Print,'Button B/W Pressed'
		printer_info.color = 0
	 end
      1: begin
		Print,'Button Color Pressed'
		printer_info.color = 1
	 end
      ELSE: Message,'Unknown button pressed'
      ENDCASE
      END

  'FIELD5': BEGIN
      	WIDGET_CONTROL, printer_info.ptr_field, GET_VALUE=str
	printer_info.name = strtrim(str(0),2)
      END

  'BGROUP7': BEGIN
      CASE Event.Value OF
      0: begin
      		WIDGET_CONTROL, printer_info.ptr_field, GET_VALUE=str
		printer_info.name = strtrim(str(0),2)
      		WIDGET_CONTROL, printer_info.base, /DESTROY, BAD_ID=bad
	 end
      1: begin
      		WIDGET_CONTROL, printer_info.base, /DESTROY, BAD_ID=bad
	 end
      ELSE: Message,'Unknown button pressed'
      ENDCASE
      END
  ENDCASE

if printer_info.name ne '' then begin
	if OS_SYSTEM.os_family eq 'unix' then $
	OS_SYSTEM.printer = '-P'+printer_info.name + ' ' else $
	OS_SYSTEM.printer = printer_info.name 
end

END



PRO PS_printer, GROUP=Group
;+
; NAME:
;	PS_PRINTER
;
; PURPOSE:
;       This widget dialog allows the user to set up PostScript printer
;       and printer name to be used by the IDL session. Default setting
;       is color PS using default printer.
;
; CATEGORY:
;       Widgets.
;
; CALLING SEQUENCE:
;       PS_PRINTER [,GROUP=Group]
;
; INPUTS:
;       None.
;	
; KEYWORD PARAMETERS:
;       GROUP:  The widget ID of the group leader of the widget. If this 
;               keyword is specified, the death of the group leader results in
;               the death of PS_PRINTER.
;
; OUTPUTS:
;
; COMMON BLOCKS:
;       COMMON PRINTER_BLOCK
;
; SIDE EFFECTS:
;       Initially the system printer is set to the user's default 
;       printer. If a null printer name is specified, whatever the 
;       system printer was previously set will be used. 
;
; RESTRICTIONS:
;       The program 'PS_open.pro' must be loaded into IDL prior calling
;       this routine. 
;       
; PROCEDURE:
;      
; EXAMPLE:
;
;               PS_PRINTER
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 6-01-97.
;       10-15-97 bkc  - Add droplist Y/N for reverse color option
;                       Now it defaults to non reverse color option.
;       05-14-98 bkc  - Remove the B/W option, use the xloadct to select B/W
;                       Change reverse video to reverse legeng color if legend
;                       is in white color 
;-

COMMON PRINTER_BLOCK,printer_info

if XRegistered('PS_printer') then return

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  PS_init

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  PS_printer_base = WIDGET_BASE(GROUP_LEADER=Group, $
      TITLE='Setup Printer', $
      ROW=1, $
      MAP=1, $
      UVALUE='PS_PRINTER')
  printer_info.base = PS_printer_base 

  BASE2 = WIDGET_BASE(PS_printer_base, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  LABEL3 = WIDGET_LABEL( BASE2, $
;	FONT=!os.font, $
      UVALUE='LABEL3', $
      VALUE='Setup PS Printer')

;  Btns167 = [ $
;    'B/W', $
;    'Color' ]
;  BGROUP3 = CW_BGROUP( BASE2, Btns167, $
;      ROW=1, $
;      EXCLUSIVE=1, $
;      LABEL_LEFT='Output PS', $
;      UVALUE='BGROUP3')
;  WIDGET_CONTROL,BGROUP3,SET_VALUE= printer_info.color

  Btn168 = ['N','Y']
  ps_reverse = WIDGET_DROPLIST(BASE2, VALUE=Btn168, $
        UVALUE='PS_REVERSE',TITLE='Reverse Legend Color')
  WIDGET_CONTROL,ps_reverse,SET_DROPLIST_SELECT=printer_info.reverse

  FieldVal269 = [ $
    '' ]
  FIELD5 = CW_FIELD( BASE2,VALUE=FieldVal269, $
      ROW=1, RETURN_EVENTS=1, $
      STRING=1, $
      TITLE='Printer Name', $
      UVALUE='FIELD5', $
      XSIZE=10)
  printer_info.ptr_field = FIELD5  
  if strtrim(printer_info.name,2) ne '' then $
  WIDGET_CONTROL,FIELD5,SET_VALUE=printer_info.name

  Btns342 = [ 'Accept', 'Cancel' ] 
  BGROUP7 = CW_BGROUP( BASE2, Btns342, $
      ROW=1, $
      UVALUE='BGROUP7')

  WIDGET_CONTROL, PS_printer_base, /REALIZE

  XMANAGER, 'PS_printer', PS_printer_base
END

PRO rix2BenChin, Scan
ON_ERROR,1
  if(*Scan.dim EQ 1) then begin
    BenChin= { $
	scanno	: Scan.scanno, $
	dim	: Scan.dim, $
	num_pts : Scan.npts, $
	cpt	: Scan.cpt, $
	id_def	: Scan.id_def, $
	pv	: Scan.pv, $
	labels	: Scan.labels, $
	pa1D	: (*Scan.pa)[0], $
	da1D	: (*Scan.da)[0], $
	pa2D	: ptr_new(/ALLOCATE_HEAP), $
	da2D	: ptr_new(/ALLOCATE_HEAP) $
	}
  endif else begin
    BenChin= { $
	scanno	: Scan.scanno, $
	dim	: Scan.dim, $
	num_pts : Scan.npts, $
	cpt	: Scan.cpt, $
	id_def	: Scan.id_def, $
	pv	: Scan.pv, $
	labels	: Scan.labels, $
	pa1D	: (*Scan.pa)[1], $
	da1D	: (*Scan.da)[1], $
	pa2D	: (*Scan.pa)[0], $
	da2D	: (*Scan.da)[0] $
	}
  endelse

  ptr_free,Scan.pa
  ptr_free,Scan.da
  Scan=BenChin
END

PRO rix2DC,Scan,gData
ON_ERROR,1
 
        *gData.scanno  = *Scan.scanno
        *gData.dim     = *Scan.dim
        *gData.num_pts = *Scan.npts
        *gData.cpt     = *Scan.cpt
        *gData.id_def  = *Scan.id_def
        *gData.pv      = *Scan.pv
        *gData.labels  = *Scan.labels

	if *Scan.dim eq 1 then begin
          *gData.pa1D  = *(*Scan.pa)[0]
          *gData.da1D  = *(*Scan.da)[0]
	  *gData.pa2D  = ptr_new(/ALLOCATE_HEAP)
	  *gData.da2D  = ptr_new(/ALLOCATE_HEAP)
	end
	if *Scan.dim eq 2 then begin
          *gData.pa1D    = *(*Scan.pa)[1]
          *gData.da1D    = *(*Scan.da)[1]
          *gData.pa2D  = *(*Scan.pa)[0]
          *gData.da2D  = *(*Scan.da)[0]
        end
 
  ptr_free,Scan.scanno
  ptr_free,Scan.dim
  ptr_free,Scan.npts
  ptr_free,Scan.cpt
  ptr_free,Scan.id_def
  ptr_free,Scan.pv
  ptr_free,Scan.labels
  ptr_free,Scan.pa
  ptr_free,Scan.da


END
	
FUNCTION nbElem,dim,vector
  res=1L
  for i=0,dim-1 do begin
     res= res*vector[i]
  end
  return, res
END

FUNCTION read_scan_rest,lun,Scan,dim,offset
ON_IOERROR, BAD	

  rank=0
  npts=0
  cpt=0
  readu,lun,rank,npts,cpt

  nb_pts=cpt
  if(nb_pts EQ npts) then nb_pts=nb_pts-1

  if(rank GT 1) then begin $
    sub_scan_ptr=lonarr(npts)
    readu,lun,sub_scan_ptr
  endif

  (*Scan.cpt)[rank-1]=cpt;

  ; read the pvname
  name=''
  time=''
  readu,lun,name,time
  
  nb_pos=0
  nb_det=0
  nb_trg=0
  readu,lun,nb_pos,nb_det,nb_trg

  if(nb_pos NE 0) then begin $
    pos_num=intarr(nb_pos)
  endif
  pos_info= { $
     pxpv:'', $
     pxds:'', $
     pxsm:'', $
     pxeu:'', $
     rxpv:'', $
     rxds:'', $
     rxeu:'' }
  if(nb_det NE 0) then begin
    det_num=intarr(nb_det)
  endif

  det_info= { $
     dxpv:'', $
     dxds:'', $
     dxeu:'' }
  if(nb_trg NE 0) then trg_num=intarr(nb_trg)
  trg_info= { $
     txpv:'', $
     txcd:'' }

  num=0
  for i=0,nb_pos-1 do begin
     readu,lun, num
     pos_num[i]=num
     readu,lun,pos_info
  end

  for i=0,nb_det-1 do begin
     readu,lun,num
     det_num[i]=num
     readu,lun,det_info
  end

  for i=0,nb_trg-1 do begin
     readu,lun,num
     trg_num[i]=num
     readu,lun,trg_info
  end
  
  tmp=dblarr(npts)
  for i=0,nb_pos-1 do begin
     readu,lun,tmp
     if(cpt NE 0) then $
       (*(*Scan.pa)[rank-1])[offset:offset+cpt-1,pos_num[i]]=tmp[0:cpt-1]
  end

  tmp=fltarr(npts)
  for i=0,nb_det-1 do begin
     readu,lun,tmp
     if(cpt NE 0) then $
       (*(*Scan.da)[rank-1])[offset:offset+cpt-1,det_num[i]]=tmp[0:cpt-1]
  end

  if(rank GT 1) then begin
    sub_offset=offset
    nb_sub= cpt
    if(cpt NE npts) then nb_sub=nb_sub+1
    for i=0,nb_sub do begin
      res= read_scan_rest(lun,Scan,dim+1,sub_offset)
      if(res NE 1) then goto,BAD
    end
  end

  offset=offset+(*Scan.npts)[rank+dim-2]

  return, 1

BAD:
  return, 0
end  



FUNCTION read_scan_first,lun,Scan,dim
ON_IOERROR, BAD	

  rank=0
  npts=0
  cpt=0
  readu,lun,rank,npts,cpt

  if(rank GT 1) then begin $
    sub_scan_ptr=lonarr(npts)
    readu,lun,sub_scan_ptr
  endif

  (*Scan.cpt)[rank-1]=cpt;

  ; read the pvname
  name=''
  time=''
  readu,lun,name,time
  (*Scan.pv)[rank-1]=name
  
  nb_pos=0
  nb_det=0
  nb_trg=0
  readu,lun,nb_pos,nb_det,nb_trg

  dims=(*Scan.npts)[rank-1:rank+dim-1]
  size= nbElem(dim+1, dims)
  if(nb_pos NE 0) then pos_num= intarr(nb_pos)

  (*Scan.pa)[rank-1]= ptr_new(dblarr(size,4), /NO_COPY)
;  (*(*Scan.pa)[rank-1])[*]= !VALUES.D_NAN  

  pos_info= { $
     pxpv:'', $
     pxds:'', $
     pxsm:'', $
     pxeu:'', $
     rxpv:'', $
     rxds:'', $
     rxeu:'' }


  if(nb_det NE 0) then det_num=intarr(nb_det)

  (*Scan.da)[rank-1]= ptr_new(fltarr(size,15), /NO_COPY)
;  (*(*Scan.da)[rank-1])[*]= !VALUES.F_NAN

  det_info= { $
     dxpv:'', $
     dxds:'', $
     dxeu:'' }

  if(nb_trg NE 0) then trg_num=intarr(nb_trg)
  trg_info= { $
     txpv:'', $
     txcd:'' }

  num=0
  for i=0,nb_pos-1 do begin
     readu,lun, num
     pos_num[i]=num
     readu,lun,pos_info
     (*Scan.id_def)[num,rank-1]=1
     if(pos_info.rxpv NE '') then begin
	(*Scan.labels)[num,rank-1]= pos_info.rxpv
	(*Scan.labels)[19+num,rank-1]= pos_info.rxds
	(*Scan.labels)[38+num,rank-1]= pos_info.rxeu
     endif else begin
	(*Scan.labels)[num,rank-1]= pos_info.pxpv
	(*Scan.labels)[19+num,rank-1]= pos_info.pxds
	(*Scan.labels)[38+num,rank-1]= pos_info.pxeu
     endelse
  end

  for i=0,nb_det-1 do begin
     readu,lun,num
     det_num[i]=num
     readu,lun,det_info
     (*Scan.id_def)[4+num,rank-1]=1
     (*Scan.labels)[4+num,rank-1]= det_info.dxpv
     (*Scan.labels)[23+num,rank-1]= det_info.dxds
     (*Scan.labels)[42+num,rank-1]= det_info.dxeu
  end

  for i=0,nb_trg-1 do begin
     readu,lun,num
     trg_num[i]=num
     readu,lun,trg_info
  end
  
  tmp=dblarr(npts)
  for i=0,nb_pos-1 do begin
     readu,lun,tmp
     if(cpt NE 0) then (*(*Scan.pa)[rank-1])[0:cpt-1,pos_num[i]]=tmp[0:cpt-1]
  end

  tmp=fltarr(npts)
  for i=0,nb_det-1 do begin
     readu,lun,tmp
     if(cpt NE 0) then (*(*Scan.da)[rank-1])[0:cpt-1,det_num[i]]=tmp[0:cpt-1]
  end

  if(rank GT 1) then begin
    res=0
    res= read_scan_first(lun,Scan,dim+1)
    if(res NE 1) then goto,BAD
    offset= LONG((*Scan.npts)[rank+dim-2])
    nb_sub= cpt-1
    if(cpt NE npts) then nb_sub=nb_sub+1
    for i=1,nb_sub do begin
      res= read_scan_rest(lun,Scan,dim+1,offset)
      if(res NE 1) then goto,BAD
    end
  end

  return, 1

BAD:
  return, 0
end  


FUNCTION read_scan,filename, Scan

  ON_ERROR,1
  ON_IOERROR,BAD

  res=0

  Scan = { $
	scanno	: ptr_new(/allocate_heap), $  ;0L, $
	dim	: ptr_new(/allocate_heap), $  ;0, $
	npts	: ptr_new(/allocate_heap), $  ;[0,0], $
	cpt	: ptr_new(/allocate_heap), $  ;[0,0], $
	id_def	: ptr_new(/allocate_heap), $  ;intarr(19,2), $
	pv	: ptr_new(/allocate_heap), $  ;['',''], $
	labels	: ptr_new(/allocate_heap), $  ;strarr(57,2), $
	pa	: ptr_new(/allocate_heap), $
	da	: ptr_new(/allocate_heap) $
	}

  get_lun, lun
  openr, /XDR, lun, filename      ;Open the file for input.

  tmp= {$
     version: 0.0, $
     scanno: 0L, $
     rank: 0L }

  readu,lun, tmp

  npts= intarr(tmp.rank)
  readu,lun, npts
  readu,lun, isRegular
  readu,lun, env_fptr

  *Scan.scanno=tmp.scanno
  *Scan.dim= tmp.rank
  *Scan.npts= reverse(npts)
  *Scan.cpt = intarr(tmp.rank)
  *Scan.id_def= intarr(19,tmp.rank)
  *Scan.pv= strarr(tmp.rank)
  *Scan.labels= strarr(57,tmp.rank)
  *Scan.pa= ptrarr(tmp.rank)
  *Scan.da= ptrarr(tmp.rank)

  if(read_scan_first(lun, Scan, 0) NE 1) then goto,BAD

  for i=0,tmp.rank-1 do begin
    dims=(*Scan.npts)[i:tmp.rank-1]
    *(*Scan.pa)[i]= reform(*(*Scan.pa)[i], [dims,4])
    *(*Scan.da)[i]= reform(*(*Scan.da)[i], [dims,15])
  end

  res= *Scan.scanno

  goto,DONE
BAD:
  res= -1
  print, !ERR_STRING

DONE:
  free_lun, lun

  return, res
END

@PS_open.pro


PRO catch1d_get_pvtcolor,i,color
COMMON COLORS, R_ORIG, G_ORIG, B_ORIG, R_CURR, G_CURR, B_CURR
; 24 bits
	if n_elements(R_ORIG) eq 0 then $
	catch1d_get_pvtct
	color = R_ORIG(i) + G_ORIG(i)*256L + B_ORIG(i)*256L ^2
;	plot,indgen(10),color=color
END

PRO catch1d_save_pvtct
	tvlct,red,green,blue,/get
	save,red,green,blue,file='catch1d.tbl'
END

PRO catch1d_get_pvtct
COMMON COLORS, R_ORIG, G_ORIG, B_ORIG, R_CURR, G_CURR, B_CURR

; 8 bit visual

	if  !d.n_colors lt 16777216 then begin
		tvlct,red,green,blue,/get
	endif else begin

; 24 bit visual
	file = 'catch1d.tbl'
	found = findfile(file)
	if found(0) eq '' then begin
		file =getenv('EPICS_EXTENSIONS_PVT')+'/bin/'+getenv('HOST_ARCH')+'/catch1d.tbl'
		found1 = findfile(file)
		if found1(0) eq '' then $
		file =getenv('EPICS_EXTENSIONS')+'/bin/'+getenv('HOST_ARCH')+'/catch1d.tbl'
		end
	restore,file
	tvlct,red,green,blue
	end

; set ORIG color 

	R_ORIG = red
	G_ORIG = green
	B_ORIG = blue

	LOADCT,39
END


PRO plot1d_replot,state
COMMON Colors,r_orig,g_orig,b_orig,r_curr,g_curr,b_curr

	state.xsize = !d.x_size
	state.ysize = !d.y_size
	cl = state.color
	psym = state.symbol
	thick = state.thick
	line = state.linestyle
	y = state.y
	s = size(y)

; set xy margin

	xmgin = state.xmargin
	ymgin = state.ymargin 

; one dim array 

if !d.name ne 'PS' then WSET,state.winDraw
!p.multi = [0,1,0,0,0]
erase

if !d.name eq 'PS' then cl = 0

if state.yrange(0) eq state.yrange(1) then begin
	dy =  state.yrange(0)/5
	state.yrange(0) =  state.yrange(0) - 2*dy
	state.yrange(1)=  state.yrange(1) + 3*dy
end
color = cl 
if !d.n_colors eq 16777216 then catch1d_get_pvtcolor,cl,color
if s(0) eq 1 then $
	PLOT,state.x,state.y, COLOR=color, $
		xrange=state.xrange, yrange = state.yrange, $
		ylog=state.ylog, xlog=state.xlog, psym=psym, $
		thick=thick, xthick=thick, ythick=thick,$
		xstyle = state.xstyle, ystyle = state.ystyle, $
		linestyle=line, $
		xmargin=xmgin, ymargin=ymgin, $
		title=state.title, xtitle=state.xtitle, ytitle=state.ytitle

; two dim array - multiple curves

if s(0) eq 2 and s(2) gt 1 then begin
in_color = make_array(s(2),/int,value=cl)
in_line = make_array(s(2),/int)
in_symbol = make_array(s(2),/int)
in_color(0)= cl
in_line(0)= line
in_symbol(0)=psym
	PLOT,state.x,state.y,COLOR=color, $
		xrange=state.xrange, yrange = state.yrange, $
		ylog=state.ylog, xlog=state.xlog, $
		thick=thick, xthick=thick, ythick=thick,$
		linestyle=line, psym=psym, $
		xmargin=xmgin, ymargin=ymgin, $
		title=state.title, xtitle=state.xtitle, ytitle=state.ytitle
	
;	dcl = state.color / 16
	dcl = !d.table_size-2
	ncv = 4  ;7
	colorlevel = dcl / ncv
	for i= 1 ,s(2) - 1 do begin
		if state.autocolor eq 1 then begin
		ii = i / ncv
		im = i MOD ncv
		cl = dcl -ii -im*colorlevel
		in_color(i) = cl
		color = cl
		; if 24 bits use cl_val
        	if !d.n_colors eq 16777216 then begin
                	catch1d_get_pvtcolor,cl,t_color
                	color = t_color
                	end

		end
		if psym gt 0 then psym = psym+1
		if psym lt 0 then psym = psym-1

; the symbol 2 is too light, skip it
if i eq 2 and psym lt 0 then psym=psym-1
if i eq 2 and psym gt 0 then psym=psym+1

		if line gt 0 then line = line+1
		in_line(i) = line
		in_symbol(i) = psym 
		z = y(0:s(1)-1,i)

		if state.curvfit then begin
			psym=7      ; if curve fitting is true
		end
		OPLOT,state.x,z,COLOR=color,linestyle=line, psym=psym mod 7, $
			thick=thick 
		psym = in_symbol(i)
; print,i,psym,cl,line
	end
end

; draw footnote comment

if state.footnote ne 0 then begin

	real_xl = 0.01*state.xsize
	real_dy = !d.y_ch_size     ; character pixel height
	real_yl = (state.footnote+1)*real_dy
	for i=0,state.footnote -1 do begin
	xyouts,real_xl,(real_yl-i*real_dy), state.comment(i), /DEVICE
	end
end

; draw stamp comment
	
if state.stamp ne 0 then begin
	st = systime(0)
	xyouts,0.01*state.xsize, 1, st, /device
	xyouts,0.75*state.xsize, 1, $
		 'User Name:  '+getenv('USER'), /device
end

; draw legend

if s(0) eq 2 and state.legendon gt 0 then begin

	real_x1 = state.xylegend(0)*(!x.crange(1)-!x.crange(0)) + $
		!x.crange(0)
	real_y1 = state.xylegend(1)*(!y.crange(1)-!y.crange(0)) + $
		!y.crange(0)
	real_dy = 0.1 * (!y.crange(1)-!y.crange(0))

	real_xl = real_x1 + 0.16*(!x.crange(1)-!x.crange(0))
	real_yl = real_y1 - 0.5*real_dy

	xyouts,real_x1,real_y1,'LEGEND'
	oplot,[real_x1,real_xl],[real_yl,real_yl],thick=2

	real_xl = real_x1 + 0.075*(!x.crange(1)-!x.crange(0))
	real_xr = real_x1 + 0.1*(!x.crange(1)-!x.crange(0))

	for i=0,n_elements(state.legend)-1 do begin

	real_yl = real_y1 - (i*0.5+1)*real_dy

	x=[real_x1,real_xl]
	y=[real_yl,real_yl]
	color = in_color(i)
        ; if 24 bits use cl_val
	if !d.n_colors eq 16777216 then begin
	catch1d_get_pvtcolor,in_color(i),t_color
	color = t_color
	end

	if psym ne 0 then $
	oplot,x,y,linestyle=in_line(i),color=color,thick=2
	oplot,x,y,linestyle=in_line(i),color=color,psym=in_symbol(i),thick=2

	xyouts,real_xr,real_yl, state.legend(i)
	end
end

END

PRO plot1d_event,ev 

; resize event

;WIDGET_CONTROL, ev.top, GET_UVALUE = state, /NO_COPY
WIDGET_CONTROL, ev.top, GET_UVALUE = state

IF (ev.id EQ ev.top) then begin
; plot1d the draw widget and redraw its plot;
	WIDGET_CONTROL,state.id_draw, SCR_XSIZE=ev.x, SCR_YSIZE=ev.y

	; if device is X
	if !d.name ne 'PS' then  WSET,state.winDraw

	plot1d_replot, state

	WIDGET_CONTROL,ev.top,SET_UVALUE=state,/NO_COPY
	return
ENDIF

WIDGET_CONTROL,ev.Id,GET_UVALUE=B_ev
CASE B_ev OF
'PLOT1D_PRINT': Begin
	PS_open, 'idl.ps'
	linestyle = state.linestyle
	state.autocolor = 0
	state.linestyle = 1
	plot1d_replot, state
	PS_close
	PS_print, 'idl.ps'
	state.autocolor = 1
	state.linestyle = linestyle 
	end
'PLOT1D_CLOSE': begin
	WIDGET_CONTROL,ev.top,BAD=bad,/DESTROY
	end
ENDCASE
END

PRO plot1d, x, y, id_tlb, windraw, $
	title=title,xtitle=xtitle,ytitle=ytitle,color=color, $
	symbol=symbol, thick=thick, linestyle=linestyle, $
        xrange=xrange, yrange=yrange, xlog=xlog, ylog=ylog, $
	xmargin=xmargin, ymargin=ymargin, stamp=stamp,$
	legend=legend, xylegend=xylegend, $
	width=width, height=height, $
	comment=comment, cleanup=cleanup, $
	curvfit=curvfit, $
	xstyle=xstyle, ystyle=ystyle, $
	wtitle=wtitle, button=button, GROUP=GROUP
;+
; NAME:
;       PLOT1D
;
; PURPOSE:
;       This routine provides a general purpose flexible cartesion plot
;       package.  It provides simple to use automatic feature of labels,
;       legend, comment, line style, symbols, and color options on plot.
;
;       The window generated by this routine will be resizable by the 
;       window manager. 
;
;       Depress the 'Print' button will generate a postscript copy of the
;       graph.
;
;       Normally it accepts two parameters X and Y. If the first parameter
;       is not used then the data array is plotted on the ordinate versus 
;       the point number on the abscissa.  Multiple curves (or variables) 
;       can be stacked into the second parameter as a two dimensional array, 
;       the first dimension gives the number of data points in each curve,
;       the second dimension gives the number of curves to be plotted.
;  
;
; CATEGORY:
;       Widgets.
;
; CALLING SEQUENCE:
;
;       PLOT1D, [X,] Y [,ID_TLB]  [,ID_DRAW]
;
; INPUTS:
;       X:      The vector array for X abscissa.
;
;       Y:      The Y data array for curve plot. The Y array can contain
;               more than one curve, the first dimension gives the number
;               of data to be plotted for the curve, the second dimension
;               gives the number of curves to be plotted.
;	
; KEYWORD PARAMETERS:
;       TITLE:  Set this keyword to specify the plot title string.
;
;       XTITLE: Set this keyword to specify the xtitle string.
;
;       YTITLE: Set this keyword to specify the ytitle string.
;
;       COLOR:  Set this keyword to specify the color number used
;               in the plot routine.
;
;      CURVFIT: Set this keyword if two curves are plotted, first curve
;               is the fitted curve, the second curve is data to be fitted. 
;
;       SYMBOL: Set this keyword to specify data plotted as symbol, set to -1
;               data plot as symbol and connected with line.
;
;       XLOG:   Set this keyword to specify a logrithmic X axis.
;
;       YLOG:   Set this keyword to specify a logrithmic Y axis.
;
;       XRANGE: Set this keyword to specify the desired data range for 
;               the X axis.
;
;       YRANGE: Set this keyword to specify the desired data range for 
;               the Y axis.
;
;       XMARGIN: Set this keyword to specify the left and right margin, 
;                default xmargin=[10,3]
;
;       YMARGIN: Set this keyword to specify the bottom and top margin 
;                default ymargin=[5,3]
;
;       THICK:  Set this keyword to specify the line thickness for the
;               axes and the line plot. 
;
;       LINESTYLE:  Set this keyword to turn on different line style used. 
;
;       XSTYLE:  Set this keyword to control x axis in IDL plot routine. 
;
;       YSTYLE:  Set this keyword to control y axis in IDL plot routine. 
;
;       LEGEND:  Set the legend strings corresponding to curves drawn.
;
;       XYLEGEND: Set the x,y location of the legend strings, % from the
;                 lower left corner from the graph window, default 
;                 xylegend=[0.75, 0.35].
;
;       COMMENT:  Set this keyword to write any footnotes on the graph.
;
;       STAMP:  Set this keyword to put the time stamp and user ID on the page. 
;
;       WTITLE: Set this keyword to specify the window title string,
;               default to 'Plot1d'.
;
;       WIDTH:  The initial window width at the creation time, which 
;               default to 350 pixel.
;  
;       HEIGHT: The initial window height at the creation time, which 
;               default to 350 pixel.
;
;       GROUP:  The widget ID of the group leader of the widget. If this
;               keyword is specified, the death of the group leader results 
;               in the death of PLOT1D.
;
;       BUTTON: Set this keyword if no print and close buttons are desired
;               for the PLOT1D widget.
;
;       CLEANUP: Set this keyword if the created window can no be closed by the
;                window manager is desired.
;
; OPTIONAL_OUTPUTS:
;       ID_TLB: The widget ID of the top level base returned by the PLOT1D. 
;
;       ID_DRAW: The widget ID of the drawing area used by the PLOT1D. 
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       If more than one curves are stored in the Y array, it automatically
;       uses different color for each curve. If the color keyword is set
;       by the user, then the specified color will be used for the whole plot. 
;
; RESTRICTIONS:
;       It is assumed that the X position array is the same for multiple
;       curve plot. 
;
; EXAMPLES:
;       Create a resizable line plot without any title or label 
;       specification.
;
;           x = !pi * indgen(100)/25
;           PLOT1D, x, sin(x)
;
;       Create a resizable line plot with title specifications. 
;
;           PLOT1D, x, sin(x), title='title', xtitle='xtitle', ytitle='ytitle'
;
;       Plot two curves with different linestyle and legend at default location.
;
;           x=indgen(100)
;           y=make_array(100,2)
;           y(0,0)=sin(x * !pi / 50)
;           y(0,1)=cos(x * !pi / 50)
;           PLOT1D,x,y,legend=['line1','line2'],/linestyle
;
;       Same as the above example plus symbol and a specified legend location.
;
;           x=indgen(100)
;           y=make_array(100,2)
;           y(0,0)=sin(x * !pi / 50)
;           y(0,1)=cos(x * !pi / 50)
;           PLOT1D,x,y,/linestyle,symbol=-1, $
;              legend=['line1','line2'], xylegend=[0.5,0.9]
;
;       Plot x,y array plus two lines of comment and a time stamp on the graph.
;     
;           PLOT1D,x,y,comment=['Comment line1','Comment line2'],/stamp
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, Mar. 7, 1996.
;
;       04-26-96 bkc   Add the window cleanup keyword 
;       10-28-96 bkc   Add the xstyle and ystyle keywords 
;       07-01-97 bkc   Comment out LOADCT,39 inherit color from parent process 
;       08-11-97 bkc   Add the curvfit support, only two curves allowed 
;       12-22-97 bkc   Add the 24 bit color visual device support 
;       09-04-98 bkc   Fix the plot problem due to ymax eq ymin
;-

;LOADCT,39

; check any data provided

n1 = n_elements(x)
if n1 lt 2 then begin
	print,'Error: No data specified.'
	return
	end
xa = x

if n_elements(y) eq 0 then begin
	ya = xa
	s = size(ya)
	n1 = s(1)
	xa = indgen(n1)
endif else ya = y

; check for input labels
xsize=350
ysize=350
xl = ''
yl =''
ti = ''
wti='Plot1d'
cl = !d.table_size - 1
leg =['']
footnote = ''
add_line=0
if keyword_set(title) then ti = string(title)
if keyword_set(xtitle) then xl = string(xtitle)
if keyword_set(ytitle) then yl = string(ytitle)
if keyword_set(wtitle) then wti = string(wtitle)
if keyword_set(legend) then leg = string(legend)
if keyword_set(comment) then footnote = string(comment)

state = { $
	id_draw:0L, $
	winDraw:0L,$
	autocolor: 1, $ 	; automatic use different color for each curve
	color:cl, $
	symbol: 0, $
	curvfit: 0, $		; whether data is from curve fitting
	xtitle:xl, $
	ytitle:yl, $
	title:ti, $
	xstyle:0,$
	ystyle:0,$
	xsize:0,$
	ysize:0,$
	xlog: 0, $
	ylog: 0, $
	xmargin: [10,3], $
	ymargin: [5,3], $
	stamp: 0, $
	footnote: 0, $
	comment: footnote, $
        xrange: [min(xa),max(xa)], $
        yrange: [min(ya),max(ya)], $
	legendon: 0, $
	legend: leg, $
	xylegend: [.75,0.35], $
	thick: 2, $
	linestyle: 0, $
	x: xa, $
	y: ya $
	}

if keyword_set(xstyle) then state.xstyle = xstyle 
if keyword_set(ystyle) then state.ystyle = ystyle 

if keyword_set(color) then begin
	cl = long(color)
	state.color = cl
	state.autocolor = 0   ; use fixed color
	end
if keyword_set(symbol) then begin
	state.symbol = symbol
	end
	psym = state.symbol
if keyword_set(xlog) then state.xlog=1
if keyword_set(ylog) then state.ylog=1
if keyword_set(thick) then state.thick= thick
if keyword_set(linestyle) then state.linestyle=1 
if keyword_set(stamp) then state.stamp=1 
if keyword_set(legend) then state.legendon = 1
if keyword_set(xylegend) then begin 
	if n_elements(xylegend) eq 2 then state.xylegend=xylegend
	end
if keyword_set(xrange) then begin 
	if n_elements(xrange) eq 2 then state.xrange=xrange
	end
if keyword_set(yrange) then begin 
	if n_elements(yrange) eq 2 then state.yrange=yrange
	end
if keyword_set(xmargin) then begin 
	if n_elements(xmargin) eq 2 then state.xmargin=xmargin
	end
if keyword_set(comment) then begin
	add_line = n_elements(comment)
	state.footnote= add_line
	ymargin=[5+add_line, 3]
	end
if keyword_set(ymargin) then begin 
	if n_elements(ymargin) eq 2 then state.ymargin=ymargin
	end

if keyword_set(curvfit) then state.curvfit = 1

if keyword_set(width) then xsize=width
if xsize lt 350 then xsize=350
if keyword_set(height) then ysize=height
if ysize lt 350 then ysize=350

; drawing with data 

if keyword_set(CLEANUP) then $
id_tlb=WIDGET_BASE(Title=wti,/COLUMN, /TLB_SIZE_EVENTS, TLB_FRAME_ATTR=8) $
else $
id_tlb=WIDGET_BASE(Title=wti,/COLUMN, /TLB_SIZE_EVENTS)
;WIDGET_CONTROL,id_tlb,default_font='-*-Helvetica-Bold-R-Normal--*-120-*75-*'
id_draw=WIDGET_DRAW(id_tlb,xsize=xsize,ysize=ysize, RETAIN=2)

if keyword_set(button) eq 0 then begin
id_tlb_row=WIDGET_BASE(id_tlb,/ROW)
id_tlb_print = WIDGET_BUTTON(id_tlb_row,VALUE='Print',UVALUE='PLOT1D_PRINT')
id_tlb_close = WIDGET_BUTTON(id_tlb_row,VALUE='Close',UVALUE='PLOT1D_CLOSE')
end

g_tlb=WIDGET_INFO(id_tlb,/geometry)

WIDGET_CONTROL,id_tlb, /realize
WIDGET_CONTROL,id_draw,get_value=windraw
	state.winDraw = windraw
	state.id_draw = id_draw
	state.xsize = g_tlb.scr_xsize
	state.ysize = g_tlb.scr_ysize
	if !d.name ne 'PS' then WSET,windraw
	
	plot1d_replot, state

WIDGET_CONTROL,id_tlb,set_uvalue=state,/no_copy
xmanager,'plot1d',id_tlb,  GROUP_LEADER=GROUP, $
	EVENT_HANDLER="plot1d_event"

END
; $Id: vw2d.pro,v 1.1 1998/12/22 19:56:24 cha Exp $

; Copyright (c) 1991-1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
;+
; NAME:
;	XSURFACE
;
; PURPOSE:
;	This routine provides a graphical interface to the SURFACE and
;	SHADE_SURFACE commands.  Different controls are provided to change 
;	the viewing angle and other plot parameters.  The command used to 
;	generate the resulting surface plot is shown in a text window.
;
; CATEGORY:
;	Widgets.
;
; CALLING SEQUENCE:
;	XSURFACE, Data
;
; INPUT PARAMETERS:
;	Data:	The two-dimensional array to display as a wire-mesh or
;		shaded surface.
;
; KEYWORD PARAMETERS:
;	GROUP:	The widget ID of the widget that calls XSURFACE.  When this
;		keyword is specified, the death of the caller results in the
;		death of XSURFACE.
;
; SIDE EFFECTS:
;	The XMANAGER is initiated if it is not already running.
;
; RESTRICTIONS:
;	XSURFACE does not accept any of the keywords that the IDL command 
;	SURFACE does.
;
; PROCEDURE:
;	Create and register the widget with the XMANAGER and then exit.
;
; MODIFICATION HISTORY:
;	Created from a template written by: Steve Richards, January, 1991.
;       02-12-96   BKC  Modify the Xsurface, returned the base widget ID,
;			which provides a handle for the calling program
;			and such that can be managed by the calling program
;-

;------------------------------------------------------------------------------
;	procedure XSurface_draw
;------------------------------------------------------------------------------

PRO XSurface_draw

COMMON orientation, zrot, thedata, xrot, skirt, shade, axes, thedraw, $
		xmargin, ymargin, upper, commandid

WSET, thedraw

IF(shade EQ 0) THEN BEGIN
  IF(skirt EQ 0) THEN $
    SURFACE, thedata, $
		XSTYLE = axes, $
		YSTYLE = axes, $
		ZSTYLE = axes, $
		UPPER_ONLY = upper, $
		XMARGIN = xmargin, $
		YMARGIN = ymargin, $
		AZ = zrot, $
		AX = xrot $
  ELSE SURFACE, thedata, $
		XSTYLE = axes, $
		YSTYLE = axes, $
		ZSTYLE = axes, $
		UPPER_ONLY = upper, $
		XMARGIN = xmargin, $
		YMARGIN = ymargin, $
		AZ = zrot, $
		AX = xrot, $
		SKIRT = MIN(thedata)
ENDIF ELSE BEGIN
  IF(skirt EQ 0) THEN $
    SHADE_SURF, thedata, $
		XSTYLE = axes, $
		YSTYLE = axes, $
		ZSTYLE = axes, $
		UPPER_ONLY = upper, $
		XMARGIN = xmargin, $
		YMARGIN = ymargin, $
		AZ = zrot, $
		AX = xrot $
    ELSE SHADE_SURF, thedata, $
		XSTYLE = axes, $
		YSTYLE = axes, $
		ZSTYLE = axes, $
		UPPER_ONLY = upper, $
		XMARGIN = xmargin, $
		YMARGIN = ymargin, $
		AZ = zrot, $
		AX = xrot, $
		SKIRT = MIN(thedata)
ENDELSE

IF(shade EQ 0) THEN command = "SURFACE, data" $
ELSE command = "SHADE_SURF, data"
IF(xrot NE 30.0) THEN command = command + STRING(xrot, $
					FORMAT = '(", AX = ",I3.3)')
IF(zrot NE 30.0) THEN command = command + STRING(zrot, $
					FORMAT = '(", AZ = ",I3.3)')
IF(skirt NE 0) THEN command = command + ", /SKIRT"
IF(xmargin(0) NE 10.0) THEN $
	command = command + STRING(xmargin, $
	FORMAT = '(", XMARGIN = [",F4.1,", ",F4.1,"]")')
IF(ymargin(0) NE 4.0) THEN $
	command = command + STRING(ymargin, $
	FORMAT = '(", YMARGIN = [",F4.1,", ",F4.1,"]")')
IF(upper NE 0) THEN command = command + ", /UPPER_ONLY"
IF(axes NE 0) THEN command = command + $
	", XSTYLE = 4, YSTYLE = 4, ZSTYLE = 4"

WIDGET_CONTROL, commandid, SET_VALUE = command

END


;------------------------------------------------------------------------------
;	procedure XSurface_ev
;------------------------------------------------------------------------------

PRO XSurface_ev, event

COMMON orientation, zrot, thedata, xrot, skirt, shade, axes, thedraw, $
		xmargin, ymargin, upper, commandid

WIDGET_CONTROL, event.id, GET_UVALUE = eventval		;find the user value
							;of the widget where
							;the event occured
CASE eventval OF

  "       0": BEGIN
		zrot = (zrot + 15) mod 360
		IF(zrot LT 0) THEN zrot = 360 + zrot
		XSurface_draw
		WIDGET_CONTROL, event.id, SET_BUTTON = 0
	      END

  "       1": BEGIN
		zrot = (zrot - 15) mod 360
		IF(zrot LT 0) THEN zrot = 360 + zrot
		XSurface_draw
		WIDGET_CONTROL, event.id, SET_BUTTON = 0
	      END

  "       2": BEGIN
		xrot = (xrot - 15) mod 360
		IF(xrot LT 0) THEN xrot = 360 + xrot
		XSurface_draw
		WIDGET_CONTROL, event.id, SET_BUTTON = 0
	      END

  "       3": BEGIN
		xrot = (xrot + 15) mod 360
		IF(xrot LT 0) THEN xrot = 360 + xrot
		XSurface_draw
		WIDGET_CONTROL, event.id, SET_BUTTON = 0
	      END

  "       4": BEGIN	;shrink
		xmargin = xmargin * 1.2
		ymargin = ymargin * 1.2
		XSurface_draw
		WIDGET_CONTROL, event.id, SET_BUTTON = 0
	      END

  "       5": BEGIN	;grow
		xmargin = xmargin * 0.8
		ymargin = ymargin * 0.8
		XSurface_draw
		WIDGET_CONTROL, event.id, SET_BUTTON = 0
	      END

  "SKIRTON": IF(event.select EQ 1) THEN BEGIN
		skirt = 1
		XSurface_draw
	     ENDIF

  "SKIRTOFF":  IF(event.select EQ 1) THEN BEGIN
		skirt = 0
		XSurface_draw
	      ENDIF

  "SHADEOFF":  IF(event.select EQ 1) THEN BEGIN
		shade = 0
		XSurface_draw
	      ENDIF

  "SHADEON":  IF(event.select EQ 1) THEN BEGIN
		shade = 1
		XSurface_draw
	      ENDIF

  "AXESOFF":  IF(event.select EQ 1) THEN BEGIN
		AXES = 4
		XSurface_draw
	      ENDIF

  "AXESON":  IF(event.select EQ 1) THEN BEGIN
		AXES = 0
		XSurface_draw
	      ENDIF

  "UPPERON": IF(event.select EQ 1) THEN BEGIN
		upper = 0
		XSurface_draw
	      ENDIF

  "UPPEROFF": IF(event.select EQ 1) THEN BEGIN
		upper = 1
		XSurface_draw
	      ENDIF

  "XLOADCT": XLoadct, GROUP = event.top

  "XPALETTE": XPalette, GROUP = event.top

  "XMANTOOL": XMTool, GROUP = event.top

  "EXIT": WIDGET_CONTROL, event.top, /DESTROY

  ELSE:; MESSAGE, "Event User Value Not Found"

ENDCASE

END ;============= end of XSurface event handling routine task =============



;------------------------------------------------------------------------------
;	procedure XSurface
;------------------------------------------------------------------------------

PRO XSurface, DATA,XSurfacebase, GROUP = GROUP

COMMON orientation, zrot, thedata, xrot, skirt, shade, axes, thedraw, $
		xmargin, ymargin, upper, commandid

IF(XRegistered("XSurface")) THEN RETURN		;only one instance of
							;the XSurface widget
							;is allowed.  If it is
							;already managed, do
							;nothing and return

thesize = SIZE(DATA)
zrot = 30.
xrot = 30.
skirt = 0
shade = 0
axes = 0
xmargin = [10.0, 3.0]
ymargin = [4.0, 2.0]
upper = 0
commandid = 0L

XSurfacebase = WIDGET_BASE( TITLE = "XSurface", $
;	TLB_FRAME_ATTR = 2, $
		/COLUMN)

XPdMenu, [	'"Done"				EXIT',		$
		'"Tools"	{',				$
				'"XLoadct"	XLOADCT',	$
				'"XPalette"	XPALETTE',	$
				'"XManagerTool"	XMANTOOL',	$
				'}'],				$
	 XSurfacebase

thebase = WIDGET_BASE(XSurfacebase, /ROW)

ver	= widget_info(/version)
case ver.style OF
'OPEN LOOK': BEGIN
	  XSurfacepalette = WIDGET_BASE(thebase, $
				/COLUMN, $
				/FRAME, $
				/EXCLUSIVE)
	END
ELSE:	    BEGIN
	  XSurfacepalette = WIDGET_BASE(thebase, $
				/COLUMN, $
				/FRAME)
	END
ENDCASE

controls = [							$
		[						$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 248B, 255B, 063B, 000B],			$
		[128B, 007B, 000B, 224B, 001B],			$
		[112B, 000B, 000B, 000B, 014B],			$
		[136B, 000B, 016B, 000B, 016B],			$
		[052B, 000B, 048B, 000B, 056B],			$
		[172B, 000B, 080B, 000B, 032B],			$
		[124B, 000B, 144B, 000B, 040B],			$
		[164B, 007B, 016B, 001B, 048B],			$
		[012B, 248B, 031B, 062B, 056B],			$
		[036B, 000B, 000B, 228B, 033B],			$
		[004B, 000B, 000B, 008B, 062B],			$
		[012B, 000B, 000B, 016B, 048B],			$
		[036B, 000B, 000B, 016B, 032B],			$
		[008B, 000B, 000B, 008B, 000B],			$
		[112B, 000B, 000B, 004B, 000B],			$
		[128B, 007B, 000B, 002B, 000B],			$
		[000B, 248B, 031B, 001B, 000B],			$
		[000B, 000B, 144B, 000B, 000B],			$
		[000B, 000B, 080B, 000B, 000B],			$
		[000B, 000B, 048B, 000B, 000B],			$
		[000B, 000B, 016B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B]			$
		],						$
;		dnz.bmdef
		[						$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 252B, 255B, 031B, 000B],			$
		[128B, 007B, 000B, 224B, 001B],			$
		[112B, 000B, 000B, 000B, 014B],			$
		[008B, 000B, 008B, 000B, 017B],			$
		[028B, 000B, 012B, 000B, 044B],			$
		[004B, 000B, 010B, 000B, 053B],			$
		[020B, 000B, 009B, 000B, 062B],			$
		[012B, 128B, 008B, 224B, 037B],			$
		[028B, 124B, 248B, 031B, 048B],			$
		[132B, 039B, 000B, 000B, 036B],			$
		[124B, 016B, 000B, 000B, 032B],			$
		[012B, 008B, 000B, 000B, 048B],			$
		[004B, 008B, 000B, 000B, 036B],			$
		[000B, 016B, 000B, 000B, 016B],			$
		[000B, 032B, 000B, 000B, 014B],			$
		[000B, 064B, 000B, 224B, 001B],			$
		[000B, 128B, 248B, 031B, 000B],			$
		[000B, 000B, 009B, 000B, 000B],			$
		[000B, 000B, 010B, 000B, 000B],			$
		[000B, 000B, 012B, 000B, 000B],			$
		[000B, 000B, 008B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B]			$
		],						$
		;upz.bm
		[						$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 128B, 255B, 001B, 000B],			$
		[000B, 064B, 169B, 002B, 000B],			$
		[000B, 160B, 254B, 007B, 000B],			$
		[000B, 160B, 068B, 000B, 000B],			$
		[000B, 016B, 032B, 000B, 000B],			$
		[000B, 016B, 032B, 000B, 000B],			$
		[000B, 016B, 032B, 000B, 000B],			$
		[000B, 016B, 032B, 000B, 000B],			$
		[000B, 008B, 016B, 000B, 000B],			$
		[000B, 008B, 016B, 000B, 000B],			$
		[000B, 008B, 144B, 001B, 000B],			$
		[000B, 008B, 112B, 006B, 000B],			$
		[000B, 008B, 016B, 008B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 006B, 096B, 000B],			$
		[000B, 008B, 001B, 128B, 000B],			$
		[000B, 136B, 000B, 000B, 001B],			$
		[000B, 200B, 015B, 240B, 003B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 016B, 004B, 008B, 000B],			$
		[000B, 016B, 004B, 008B, 000B],			$
		[000B, 016B, 005B, 008B, 000B],			$
		[000B, 080B, 004B, 008B, 000B],			$
		[000B, 032B, 147B, 004B, 000B],			$
		[000B, 160B, 042B, 005B, 000B],			$
		[000B, 064B, 149B, 002B, 000B],			$
		[000B, 128B, 255B, 001B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B]			$
		],						$
		;dnx.bm
		[						$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 128B, 255B, 001B, 000B],			$
		[000B, 064B, 149B, 002B, 000B],			$
		[000B, 160B, 042B, 005B, 000B],			$
		[000B, 032B, 147B, 004B, 000B],			$
		[000B, 080B, 004B, 008B, 000B],			$
		[000B, 016B, 005B, 008B, 000B],			$
		[000B, 016B, 004B, 008B, 000B],			$
		[000B, 016B, 004B, 008B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 200B, 015B, 240B, 003B],			$
		[000B, 136B, 000B, 000B, 001B],			$
		[000B, 008B, 001B, 128B, 000B],			$
		[000B, 008B, 006B, 096B, 000B],			$
		[000B, 008B, 008B, 016B, 000B],			$
		[000B, 008B, 016B, 008B, 000B],			$
		[000B, 008B, 112B, 006B, 000B],			$
		[000B, 008B, 144B, 001B, 000B],			$
		[000B, 008B, 016B, 000B, 000B],			$
		[000B, 008B, 016B, 000B, 000B],			$
		[000B, 016B, 032B, 000B, 000B],			$
		[000B, 016B, 032B, 000B, 000B],			$
		[000B, 016B, 032B, 000B, 000B],			$
		[000B, 016B, 032B, 000B, 000B],			$
		[000B, 160B, 068B, 000B, 000B],			$
		[000B, 160B, 254B, 007B, 000B],			$
		[000B, 064B, 169B, 002B, 000B],			$
		[000B, 128B, 255B, 001B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B]			$
		],						$
		;shrink.bm
		[						$
		[000B, 000B, 008B, 000B, 000B],			$
		[000B, 000B, 008B, 000B, 000B],			$
		[000B, 000B, 073B, 000B, 000B],			$
		[000B, 000B, 042B, 000B, 000B],			$
		[000B, 000B, 028B, 000B, 000B],			$
		[000B, 000B, 008B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[128B, 255B, 255B, 255B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[132B, 000B, 000B, 000B, 033B],			$
		[136B, 000B, 000B, 000B, 017B],			$
		[144B, 000B, 000B, 000B, 009B],			$
		[191B, 000B, 000B, 000B, 253B],			$
		[144B, 000B, 000B, 000B, 009B],			$
		[136B, 000B, 000B, 000B, 017B],			$
		[132B, 000B, 000B, 000B, 033B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 000B, 000B, 000B, 001B],			$
		[128B, 255B, 255B, 255B, 001B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 008B, 000B, 000B],			$
		[000B, 000B, 028B, 000B, 000B],			$
		[000B, 000B, 042B, 000B, 000B],			$
		[000B, 000B, 073B, 000B, 000B],			$
		[000B, 000B, 008B, 000B, 000B],			$
		[000B, 000B, 008B, 000B, 000B]			$
		],						$
		;grow.bm
		[						$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[248B, 255B, 255B, 255B, 031B],			$
		[008B, 000B, 000B, 000B, 016B],			$
		[008B, 000B, 008B, 000B, 016B],			$
		[008B, 000B, 028B, 000B, 016B],			$
		[008B, 000B, 042B, 000B, 016B],			$
		[008B, 000B, 073B, 000B, 016B],			$
		[008B, 000B, 008B, 000B, 016B],			$
		[008B, 000B, 008B, 000B, 016B],			$
		[008B, 000B, 008B, 000B, 016B],			$
		[008B, 000B, 008B, 000B, 016B],			$
		[008B, 000B, 008B, 000B, 016B],			$
		[008B, 000B, 000B, 000B, 016B],			$
		[008B, 000B, 000B, 000B, 016B],			$
		[008B, 001B, 000B, 000B, 016B],			$
		[136B, 000B, 000B, 128B, 016B],			$
		[072B, 000B, 000B, 000B, 017B],			$
		[232B, 063B, 000B, 000B, 018B],			$
		[072B, 000B, 000B, 252B, 023B],			$
		[136B, 000B, 000B, 000B, 018B],			$
		[008B, 001B, 000B, 000B, 017B],			$
		[008B, 000B, 000B, 128B, 016B],			$
		[008B, 000B, 000B, 000B, 016B],			$
		[008B, 000B, 000B, 000B, 016B],			$
		[008B, 000B, 000B, 000B, 016B],			$
		[008B, 000B, 016B, 000B, 016B],			$
		[008B, 000B, 016B, 000B, 016B],			$
		[008B, 000B, 016B, 000B, 016B],			$
		[008B, 000B, 016B, 000B, 016B],			$
		[008B, 000B, 146B, 000B, 016B],			$
		[008B, 000B, 084B, 000B, 016B],			$
		[008B, 000B, 056B, 000B, 016B],			$
		[008B, 000B, 016B, 000B, 016B],			$
		[008B, 000B, 000B, 000B, 016B],			$
		[248B, 255B, 255B, 255B, 031B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B],			$
		[000B, 000B, 000B, 000B, 000B]			$
		]						$
	   ]

FOR i = 0,N_ELEMENTS(controls(0,0,*))-1 DO $
  toss = WIDGET_BUTTON(XSurfacepalette, $
		VALUE = controls(*,*,i), $
		UVALUE = STRING(i))

XSurfacedisplay = WIDGET_DRAW(thebase, $
		XSIZE = 375, $
		YSIZE = 300, $
		RETAIN = 2)

XSurfacecontrols = WIDGET_BASE(XSurfacebase, $
		/ROW)

skirtbase = WIDGET_BASE(XSurfacecontrols, $
		/COLUMN, $
		/EXCLUSIVE, $
		/FRAME)

skirtoff = WIDGET_BUTTON(skirtbase, $
		VALUE = "No Skirt", $
		UVALUE = "SKIRTOFF")

skirton = WIDGET_BUTTON(skirtbase, $
		VALUE = "Skirt", $
		UVALUE = "SKIRTON")

shadebase = WIDGET_BASE(XSurfacecontrols, $
		/COLUMN, $
		/EXCLUSIVE, $
		/FRAME)

shadeoff = WIDGET_BUTTON(shadebase, $
		VALUE = "Wire Frame", $
		UVALUE = "SHADEOFF")

shadeon = WIDGET_BUTTON(shadebase, $
		VALUE = "Shaded Surface", $
		UVALUE = "SHADEON")


axesbase = WIDGET_BASE(XSurfacecontrols, $
		/COLUMN, $
		/EXCLUSIVE, $
		/FRAME)

axeson = WIDGET_BUTTON(axesbase, $
		VALUE = "Show Axes", $
		UVALUE = "AXESON")

axesoff = WIDGET_BUTTON(axesbase, $
		VALUE = "Hide Axes", $
		UVALUE = "AXESOFF")

upperbase = WIDGET_BASE(XSurfacecontrols, $
		/COLUMN, $
		/EXCLUSIVE, $
		/FRAME)

upperon = WIDGET_BUTTON(upperbase, $
		VALUE = "Show Top and Bottom", $
		UVALUE = "UPPERON")

upperoff = WIDGET_BUTTON(upperbase, $
		VALUE = "Only Show Top", $
		UVALUE = "UPPEROFF")

commandbase = WIDGET_BASE(XSurfacebase, $
		/FRAME, $
		/COLUMN)

commandlabel = WIDGET_LABEL(commandbase, $
		VALUE = "IDL Commmand To Produce Above Output:")

case ver.style of
'OPEN LOOK':  commandid = WIDGET_LABEL(commandbase, VALUE = "SURFACE, data")
ELSE:	      commandid = WIDGET_TEXT(commandbase, $
				VALUE = "SURFACE, data", $
				/SCROLL, $
				YSIZE = 1)
ENDCASE

WIDGET_CONTROL, XSurfacebase, /REALIZE			;create the widgets
							;that is defined

WIDGET_CONTROL, skirtoff, /SET_BUTTON
WIDGET_CONTROL, shadeoff, /SET_BUTTON
WIDGET_CONTROL, axeson, /SET_BUTTON
WIDGET_CONTROL, upperon, /SET_BUTTON
WIDGET_CONTROL, XSurfacedisplay, GET_VALUE = temp & thedraw = temp

IF(N_PARAMS() gt 0) THEN BEGIN
	thedata = DATA
	XSurface_draw
END

XManager, "XSurface", XSurfacebase, $			;register the widgets
		EVENT_HANDLER = "XSurface_ev", $	;with the XManager
		GROUP_LEADER = GROUP

END ;================ end of XSurface background task =====================



; $Id: vw2d.pro,v 1.1 1998/12/22 19:56:24 cha Exp $

pro my_box_cursor, x0, y0, nx, ny, INIT = init, FIXED_SIZE = fixed_size, $
	MESSAGE = message
;+
; NAME:
;	BOX_CURSOR
;
; PURPOSE:
;	Emulate the operation of a variable-sized box cursor (also known as
;	a "marquee" selector).
;
; CATEGORY:
;	Interactive graphics.
;
; CALLING SEQUENCE:
;	BOX_CURSOR, x0, y0, nx, ny [, INIT = init] [, FIXED_SIZE = fixed_size]
;
; INPUTS:
;	No required input parameters.
;
; OPTIONAL INPUT PARAMETERS:
;	x0, y0, nx, and ny give the initial location (x0, y0) and 
;	size (nx, ny) of the box if the keyword INIT is set.  Otherwise, the 
;	box is initially drawn in the center of the screen.
;
; KEYWORD PARAMETERS:
;	INIT:  If this keyword is set, x0, y0, nx, and ny contain the initial
;	parameters for the box.
;
;	FIXED_SIZE:  If this keyword is set, nx and ny contain the initial
;	size of the box.  This size may not be changed by the user.
;
;	MESSAGE:  If this keyword is set, print a short message describing
;	operation of the cursor.
;
; OUTPUTS:
;	x0:  X value of lower left corner of box.
;	y0:  Y value of lower left corner of box.
;	nx:  width of box in pixels.
;	ny:  height of box in pixels. 
;
;	The box is also constrained to lie entirely within the window.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	A box is drawn in the currently active window.  It is erased
;	on exit.
;
; RESTRICTIONS:
;	Works only with window system drivers.
;
; PROCEDURE:
;	The graphics function is set to 6 for eXclusive OR.  This
;	allows the box to be drawn and erased without disturbing the
;	contents of the window.
;
;	Operation is as follows:
;	Left mouse button:   Move the box by dragging.
;	Middle mouse button: Resize the box by dragging.  The corner
;		nearest the initial mouse position is moved.
;	Right mouse button:  Exit this procedure, returning the 
;			     current box parameters.
;
; MODIFICATION HISTORY:
;	DMS, April, 1990.
;	DMS, April, 1992.  Made dragging more intutitive.
;	June, 1993 - Bill Thompson
;			prevented the box from having a negative size.
;       04-18-96   bkc  Made the box color more visible.
;       05-28-98   bkc  Reset bounding box color 
;-

device, get_graphics = old, set_graphics = 6  ;Set xor
col = !d.n_colors - 2

if keyword_set(message) then begin
	st = [$,
	"Drag Left button to move box.",$
	"Drag Middle button near a corner to resize box.",$
	"Right button when done."]
	res=WIDGET_MESSAGE(st)
	endif

if keyword_set(init) eq 0 then begin  ;Supply default values for box:
	if keyword_set(fixed_size) eq 0 then begin
		nx = !d.x_size/8   ;no fixed size.
		ny = !d.x_size/8
		endif
	x0 = !d.x_size/2 - nx/2
	y0 = !d.y_size/2 - ny/2
	endif

button = 0
goto, middle

while 1 do begin
	old_button = button
	cursor, x, y, 2, /dev	;Wait for a button
	button = !err
	if (old_button eq 0) and (button ne 0) then begin
		mx0 = x		;For dragging, mouse locn...
		my0 = y		
		x00 = x0	;Orig start of ll corner
		y00 = y0
		endif
	if !err eq 1 then begin  ;Drag entire box?
		x0 = x00 + x - mx0
		y0 = y00 + y - my0
		endif
	if (!err eq 2) and (keyword_set(fixed_size) eq 0) then begin ;New size?
		if old_button eq 0 then begin	;Find closest corner
			mind = 1e6
			for i=0,3 do begin
				d = float(px(i)-x)^2 + float(py(i)-y)^2
				if d lt mind then begin
					mind = d
					corner = i
					endif
			   endfor
			nx0 = nx	;Save sizes.
		   	ny0 = ny
			endif
		dx = x - mx0 & dy = y - my0	;Distance dragged...
		case corner of
		0: begin x0 = x00 + dx & y0 = y00 + dy
			nx = nx0 -dx & ny = ny0 - dy & endcase
		1: begin y0 = y00 + dy
			nx = nx0 + dx & ny = ny0 - dy & endcase
		2: begin nx = nx0 + dx & ny = ny0 + dy & endcase
		3: begin x0 = x00 + dx
			nx = nx0 -  dx & ny = ny0 + dy & endcase
		endcase
		endif
	plots, px, py, col=col, /dev, thick=3, lines=0	;Erase previous box
	empty				;Decwindow bug

	if !err eq 4 then begin  ;Quitting?
		device,set_graphics = old
		return
		endif
middle:

	if nx lt 0 then begin
		x0 = x0 + nx
		nx = -nx
	endif
	if ny lt 0 then begin
		y0 = y0 + ny
		ny = -ny
	endif

	x0 = x0 > 0
	y0 = y0 > 0
	x0 = x0 < (!d.x_size-1 - nx)	;Never outside window
	y0 = y0 < (!d.y_size-1 - ny)

	px = [x0, x0 + nx, x0 + nx, x0, x0] ;X points
	py = [y0, y0, y0 + ny, y0 + ny, y0] ;Y values

	plots,px, py, col=col, /dev, thick=3, lines=0  ;Draw the box
	wait, .1		;Dont hog it all
	endwhile
end
;+
; NAME:
;	cw_term
;
; PURPOSE:
;      writtable text window widget
;
; CATEGORY:
;	Compound widgets.
;
; CALLING SEQUENCE:
;	widget_id = CW_TERM(parent)
;
; INPUTS:
;       PARENT - The ID of the parent widget.
;
; KEYWORD PARAMETERS:
;	BG_NAMES:	An array of strings to be associated with
;			each button and returned in the event structure as VALUE.
;	BGEVENT_FUNCT:	The name of an user-supplied event function 
;			for the buttons. This function is called with the return
;			value structure whenever a button is pressed, and 
;			follows the conventions for user-written event
;			functions.
;	FONT:		The name of the font to be used for the text output 
;			If this keyword is not specified, the default
;			font is used.
;	FRAME:		Specifies the width of the frame to be drawn around
;			the base.
;       FILENAME:       Copy contents of file into widget
;       RESET:          Clear existing widget contents and write new value/file.
;                       The parent widget is the existing widget id. 
;	MAP:		If set, the base will be mapped when the widget
;			is realized (the default).
;	SCROLL:		If set, the base will include scroll bars to allow
;			viewing a large text area through a smaller viewport.
;	SET_VALUE:	The initial value of the text widget. This is equivalent
;			to the later statement:
;
;			WIDGET_CONTROL, widget, set_value=value
;
;       TITLE:          New Window title
;	UVALUE:         The user value for the compound widget
;
;	XSIZE:		The width of the text widget
;	YSIZE:		The height of the text widget
;
; OUTPUTS:
;       The ID of the created widget is returned.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;
; PROCEDURE:
;	WIDGET_CONTROL, id, SET_VALUE=value can be used to change the
;		current value displayed by the widget.
;
;	WIDGET_CONTROL, id, GET_VALUE=var can be used to obtain the current
;		value displayed by the widget.
;
; MODIFICATION HISTORY:
;  01  8-9-95  jps  	modified from idl's cw_tmpl.pro
;-



PRO cwterm_Save_Event, Event
COMMON SYSTEM_BLOCK,OS_SYSTEM

  WIDGET_CONTROL,Event.Top,GET_UVALUE=info
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'CWTERM_SAVEFILE': BEGIN
      END
  'CWTERM_SAVEACCEPT': BEGIN
	WIDGET_CONTROL,info.newname, GET_VALUE=filename
	if strtrim(filename(0),2) ne '' then begin
	found = findfile(filename(0))
	if found(0) ne '' then begin
		WIDGET_CONTROL,info.base,/DESTROY
		st = [ 'File: '+filename(0),' already existed!', $
			'ASCII data saved in ',info.oldname]
		res = widget_message(st,/info)
		return
	end
	spawn,[OS_SYSTEM.cp, info.oldname, filename(0)],/noshell
	WIDGET_CONTROL,info.base,/DESTROY
;	res=widget_message('File: "'+filename(0)+'" saved',/info)
	end
      END
  'CWTERM_SAVECANCEL': BEGIN
	WIDGET_CONTROL,info.base,/DESTROY
      END
  ENDCASE
END

;
; if filename specifies the default file name used by the cw_term, 
;     it will be override by the textfield entered by the user
;
PRO cwterm_save_dialog, GROUP=Group,oldname=oldname, rename=rename

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  cwterm_Save = WIDGET_BASE(GROUP_LEADER=Group, $
	TITLE='CW_TERM Save File', $
      ROW=1, $
      MAP=1, $
      UVALUE='cwterm_Save')

  BASE2 = WIDGET_BASE(cwterm_Save, $
      COLUMN=1, TITLE='CW_TERM SaveFile', $
      MAP=1, $
      UVALUE='BASE2')

  FieldVal288 = [ $
    '' ]
  if n_elements(rename) then FieldVal288 = strtrim(rename,2)
  FIELD3 = CW_FIELD( BASE2,VALUE=FieldVal288, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='File:', $
      UVALUE='CWTERM_SAVEFILE', $
      XSIZE=60)

  BASE4 = WIDGET_BASE(BASE2, $
      COLUMN=2, $
      MAP=1, $
      UVALUE='BASE4')

  CWTERM_SAVE_BUTTON5 = WIDGET_BUTTON( BASE4, $
      UVALUE='CWTERM_SAVEACCEPT', $
      VALUE='Accept')

  CWTERM_SAVE_BUTTON6 = WIDGET_BUTTON( BASE4, $
      UVALUE='CWTERM_SAVECANCEL', $
      VALUE='Cancel')

  info = {  $
	base : cwterm_Save, $
	oldname: oldname, $
	newname: FIELD3 $
	}

  WIDGET_CONTROL, cwterm_Save, SET_UVALUE=info
  WIDGET_CONTROL, cwterm_Save, /REALIZE

  XMANAGER, 'cwterm_Save', cwterm_Save
END

PRO term_set_value, id, value

	; This routine is used by WIDGET_CONTROL to set the value for
	; your compound widget.  It accepts one variable.  
	; You can organize the variable as you would like.  If you have
	; more than one setting, you may want to use a structure that
	; the user would need to build and then pass in using 
	; WIDGET_CONTROL, compoundid, SET_VALUE = structure.

	; Return to caller.
  ON_ERROR, 2

	; Retrieve the state.
   stash = WIDGET_INFO(id, /CHILD)
   WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY, BAD_ID=bad_id

    IF (N_ELEMENTS(value) NE 0) THEN BEGIN
	   WIDGET_CONTROL, state.text_id, $
				SET_VALUE=value, $
				/APPEND, $
				BAD_ID=bad_id, $
				/NO_COPY
    ENDIF
   
   WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY, BAD_ID=bad_id

END



FUNCTION term_get_value, id, value

	; This routine is by WIDGET_CONTROL to get the value from 
	; your compound widget.  As with the set_value equivalent,
	; you can only pass one value here so you may need to load
	; the value by using a structure or array.

	; Return to caller.
  ON_ERROR, 2

	; Retrieve the structure from the child that contains the sub ids.
  stash = WIDGET_INFO(id, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY, BAD_ID=bad_id

	; Get the value here
  WIDGET_CONTROL, state.text_id, GET_VALUE=ret, BAD_ID=bad_id

  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY, BAD_ID=bad_id
  
        ; Return the value here.
  RETURN,ret
END

;-----------------------------------------------------------------------------

FUNCTION term_event, event
COMMON SYSTEM_BLOCK,OS_SYSTEM

  parent=event.handler

  WIDGET_CONTROL, event.id, GET_UVALUE=Ev

		; Retrieve the structure from the child that contains the sub ids.
  stash = WIDGET_INFO(parent, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY,  BAD_ID=bad_id
  fileName = state.win_file

  CASE Ev OF 

  'MAIN': BEGIN
      END
  'BGROUP':BEGIN
	CASE event.value OF
	  'Save...': BEGIN
		if XRegistered('cwterm_Save') eq 0 then $
		cwterm_save_dialog,GROUP=Event.id, $
			rename=state.rename,oldname=fileName
	      END
	  'Close': BEGIN
	      WIDGET_CONTROL, parent, DESTROY=1, BAD_ID=bad_id
	      END
	  'Clear': BEGIN
		  WIDGET_CONTROL, state.text_id, SET_VALUE='', BAD_ID=bad_id
	      END
	  'Print': BEGIN
		  ANS = WIDGET_MESSAGE('Are you sure ?',/QUESTION, $
			/DEFAULT_NO, DIALOG_PARENT=Event.top)
		  IF ANS EQ 'Yes' THEN BEGIN
		  WIDGET_CONTROL, state.text_id, GET_VALUE=value, BAD_ID=bad_id
			; open the scratch file for printing
		  fileName = state.win_file
		  OPENW, unit, fileName, /GET_LUN, ERROR=error	;
	    	  IF error LT 0 THEN BEGIN		;OK?
		     print, [ !err_string, ' Can not display ' + filename]  ;No
		  ENDIF ELSE BEGIN	
		     printf,unit, FORMAT='(A)',value
	     	     FREE_LUN, unit			;free the file unit.
		     if OS_SYSTEM.os_family eq 'unix' then begin
		     spawn,[OS_SYSTEM.prt, OS_SYSTEM.printer, '-r', fileName], /noshell
		     spawn,[OS_SYSTEM.rm, '-f', fileName], /noshell
		     endif else begin
		     spawn,[OS_SYSTEM.prt, fileName]
		     spawn,[OS_SYSTEM.rm, fileName]
		     end
		  ENDELSE
		  END
	      END
	   ELSE: 
	ENDCASE
      END
  'TEXT': BEGIN
      End
  ENDCASE

  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY,  BAD_ID=bad_id

  RETURN, { ID:parent, TOP:event.top, HANDLER:0L }
END

;-----------------------------------------------------------------------------

FUNCTION cw_term, parent, SET_VALUE=value, $
	COLUMN=column, TITLE=title, $
	FILENAME=filename, $
	RENAME = rename, $
        RESET=reset, $
	BG_NAMES = bg_names, BGEVENT_FUNCT = bg_efun, $
	FONT=font, FRAME=frame, $
	MAP=map, SENSITIVE=sense, $
	ROW=row, SCROLL=scroll, SPACE=space, UVALUE=uvalue, $
	XSIZE=xsize, YSIZE=ysize

COMMON SYSTEM_BLOCK,OS_SYSTEM

  IF (N_PARAMS() LT 1) THEN MESSAGE, 'Must specify a parent for cw_term.'

  ON_ERROR, 2					;return to caller

	; Defaults for keywords
  version = WIDGET_INFO(/version)
  if (version.toolkit eq 'OLIT') then def_space_pad = 4 else def_space_pad = 3
  IF NOT (KEYWORD_SET(append))  THEN append = 0
  IF NOT (KEYWORD_SET(xsize)) THEN xsize = 80
  IF NOT (KEYWORD_SET(ysize)) THEN ysize = 24
  IF NOT (KEYWORD_SET(reset)) THEN reset = 0

;  IF (N_ELEMENTS(value) eq 0) 	then value = ''
  IF (N_ELEMENTS(Title) eq 0) 	 	then Title = ''
  IF (N_ELEMENTS(column) eq 0) 		then column = 0
  IF (N_ELEMENTS(frame) eq 0)		then frame = 0
  IF (N_ELEMENTS(map) eq 0)		then map=1
  IF (N_ELEMENTS(row) eq 0)		then row = 0
  IF (N_ELEMENTS(scroll) eq 0)		then scroll = 0
  IF (N_ELEMENTS(sense) eq 0)		then sense = 1
  IF (N_ELEMENTS(uvalue) eq 0)		then uvalue = 0



; File read section copied from XDISPLAYFILE utility
;	Written By Steve Richards, December 1990
;	Graceful error recovery, DMS, Feb, 1992.
;       12 Jan. 1994  - KDB
;               If file was empty, program would crash. Fixed.
;       4 Oct. 1994     MLR Fixed bug if /TEXT was present and /TITLE was not.
;      14 Jul. 1995     BKC Increased the max line to variable size.
;      16 Jun. 1997     BKC Max line set to 10000, os system check.
;      18 Dec. 1997     BKC add the save file event.

  IF(KEYWORD_SET(filename)) THEN BEGIN

    IF(NOT(KEYWORD_SET(TITLE))) THEN TITLE = filename     
    OPENR, unit, filename, /GET_LUN, ERROR=i		;open the file and then
    IF i LT 0 THEN BEGIN		;OK?
	text = [ !err_string, ' Can not display ' + filename]  ;No
    ENDIF ELSE BEGIN

    y=10000
    if OS_SYSTEM.os_family eq 'unix' then  spawn,[OS_SYSTEM.wc,'-l',FILENAME],y,/noshell

	text = strarr(y(0))				;Maximum # of lines
	i = 0L
	c = ''
	WHILE not eof(unit) do BEGIN
		READF,unit,c
		text(i) = c
		i = i + 1
		if i ge y(0) then goto,stopread
	ENDWHILE
    stopread:
	value = text(0:(i-1)>0)  ;Added empty file check -KDB
	FREE_LUN, unit			;free the file unit.
    ENDELSE
  ENDIF ELSE BEGIN
    IF(NOT(KEYWORD_SET(TITLE))) THEN TITLE = 'Term'
  ENDELSE

  winFile = ''
  if n_elements(filename) then winFile=filename
  winTitle = title
 
 IF reset EQ 0 THEN BEGIN

  if n_elements(rename) then $
	  state = { main_id:0L, group_leader:0L, $
			rename : rename, $
		    bgroup_id:0L, text_id:0L, win_file:winFile } else $
	  state = { main_id:0L, group_leader:0L, $
		    bgroup_id:0L, text_id:0L, win_file:winFile }

	  MAIN = WIDGET_BASE( $
			    GROUP_LEADER=parent, $
			    UVALUE = uvalue, $
			    TITLE=winTitle, $
			    MAP=map, $
			    EVENT_FUNC = "term_event", $
			    FUNC_GET_VALUE = "term_get_value", $
			    PRO_SET_VALUE = "term_set_value", $
			    /COLUMN)
		
	  state.main_id = MAIN
	  state.group_leader = parent

	  ; Create text widget
	  IF (N_ELEMENTS(font) EQ 0) THEN BEGIN
	      state.text_id = WIDGET_TEXT( MAIN, $
	      XSIZE=xsize, $
	      YSIZE=ysize, $
	      /NO_COPY, $
	      SCROLL=scroll)
	  ENDIF ELSE BEGIN
	      state.text_id = WIDGET_TEXT( MAIN, $
	      XSIZE=xsize, $
	      YSIZE=ysize, $
	      /NO_COPY, $
	      SCROLL=scroll, $
	      FONT=font)
	  ENDELSE


	  IF (N_ELEMENTS(value) NE 0) THEN $
		WIDGET_CONTROL, state.text_id, SET_VALUE=value

	  ; Standard control buttons
	  N_BUTTONS = 3
	  buttons = STRARR(N_BUTTONS+N_ELEMENTS(bg_names))
	  buttons(0:N_BUTTONS-1) = ['Print','Clear','Close']

	
	  ; User control buttons
	  IF N_ELEMENTS(bg_names) NE 0 THEN BEGIN
 	   buttons(N_BUTTONS:N_BUTTONS+N_ELEMENTS(bg_names)-1) = bg_names(*)
	  ENDIF

	  ; Create control buttons
	  state.bgroup_id = CW_BGROUP( MAIN, buttons, $
				      /ROW, $
				      /RETURN_NAME, $
				      EVENT_FUNCT=bg_efun, $
				      FRAME=frame, $
				      UVALUE='BGROUP')

	  ; Save out the initial state structure into the first childs UVALUE.
	  WIDGET_CONTROL, WIDGET_INFO(MAIN, /CHILD), SET_UVALUE=state, /NO_COPY

	  WIDGET_CONTROL, MAIN, /REALIZE 

  ENDIF ELSE BEGIN
		; Retrieve the structure from the child that contains the sub ids.
	  IF  WIDGET_INFO(parent, /VALID_ID) THEN BEGIN
	      stash = WIDGET_INFO(parent, /CHILD)
	      WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY, BAD_ID=bad_id
              state.win_file= winFile

	      IF (N_ELEMENTS(value) eq 0) 	then value = ''	  

	      WIDGET_CONTROL, state.text_id, SET_VALUE=value, BAD_ID=bad_id

	      WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY, BAD_ID=bad_id
	 ENDIF

         MAIN = parent
   ENDELSE

	; value is all the user will know about the internal structure
	; of your widget.
  RETURN, MAIN

END




PRO show_cross,x,y,d_id,s_id
if n_params() lt 4 then begin
	print,'Usage: show_cross,x,y,d_wid,s_wid
	print,'       x, y - specify cross hair coordinate
	print,'       d_win  - specify tv image window
	print,'       s_win  - saved virtual image window        
	return
	end
CATCH,error_status
if error_status eq -324 then begin
	print,!err_string
	print,'Invalid window id : ', s_id
	return
	end
	WSET,s_id
	width = !d.x_size
	height = !d.y_size
	WSET,d_id
	xa = [0,width-1]
	ya = [y,y]
	plots,xa,ya,/device
	xa = [x,x]
	ya = [0,height-1]
	plots,xa,ya,/device
END

PRO hide_cross,x,y,d_id,s_id
if n_params() lt 4 then begin
	print,'Usage: hide_cros,x,y,d_wid,s_wid
	print,'       x, y - specify cross hair coordinate
	print,'       d_win  - specify tv image window
	print,'       s_win  - saved virtual image window        
	return
	end
CATCH,error_status
if error_status eq -324 then begin
	print,!err_string
	print,'Invalid window id : ', s_id
	return
	end
	WSET,s_id
	width = !d.x_size
	height = !d.y_size
	WSET,d_id
if x ge 0 and x lt width then $
 	device,copy=[x,0,1,height,x,0,s_id]
if y ge 0 and y lt height then $
 	device,copy=[0,y,width,1,0,y,s_id]
END

PRO update_pixmap,wid
	o_wid = !d.window
	if !d.n_colors eq 16777216 then	channel=1 else channel=0
	data = TVRD(TRUE=channel)
	WSET,wid
	TV,data,TRUE=channel
	WSET,o_wid
END

PRO create_pixmap,wid,data=data,xp=xp,yp=yp,width=width,height=height
if n_params() lt 1 then begin
	print,'Usage: create_pixmap,wid 
	print,'       output - wid , saved virtual image window id
	print,'       keyword - xp,yp, width,height
	print,'Save the whole TV window to a new virtual window
	print,'         if keyword is used all four of them must be specified  
	return
	end


	if !d.n_colors eq 16777216 then	data = TVRD(TRUE=1) else $
	data = TVRD(TRUE=0)

	if keyword_set(xp) and keyword_set(yp) and keyword_set(width) $
		 and keyword_set(height) then begin
		if !d.n_colors eq 16777216 then $ 
		newdata = data(0:2, xp:xp+width-1, yp:yp+height-1) else $
		newdata = data(xp:xp+width-1, yp:yp+height-1)
		data = newdata
		end

	ss = size(data)
	if ss(0) eq 2 then begin
		xs = ss(1)
		ys = ss(2)
		channel = 0
		end
	if ss(0) eq 3 and ss(1) eq 3 then begin
		xs = ss(2)
		ys = ss(3)
		channel = 1
		end
	if !d.n_colors eq 16777216 then	$
	print,'CREATE PIXMAP: Array(3,',strtrim(xs,2),',',strtrim(ys,2),')'  else $
	print,'CREATE PIXMAP: Array(',strtrim(xs,2),',',strtrim(ys,2),')'

	window,/free,/pixmap, xsize=xs, ysize=ys
	wid= !d.window
	TV,data,TRUE=channel

END

PRO w_warningtext_quest
COMMON w_warningtext_block,w_warningtext_ids

	WIDGET_CONTROL,w_warningtext_ids.text,GET_VALUE=ans
	w_warningtext_ids.answer = strtrim(strupcase(ans(0)),2)
	WIDGET_CONTROL,w_warningtext_ids.base,BAD_ID=bad,/DESTROY
	if w_warningtext_ids.answer eq 'Y' then begin
		if w_warningtext_ids.quest eq 'GoTo' then $
			xycoord_setmotor_confirmed
		if w_warningtext_ids.quest eq 'Get Scan Data and Save' then begin
			catch1dReadScanRecordAppendFile 
			end
	endif else begin   ; 'N'
		if w_warningtext_ids.quest eq 'APPEND' then $
			catch1d_append
	end
END

PRO w_warningtext_event,event
COMMON w_warningtext_block,w_warningtext_ids

WIDGET_CONTROL, event.id, GET_UVALUE = eventval
CASE eventval OF
        "WARNINGTEXT_GET" : BEGIN
		WIDGET_CONTROL,event.id,GET_VALUE=ans
		w_warningtext_ids.answer = strtrim(strupcase(ans(0)),2)
		w_warningtext_quest
		END
        "WARNINGTEXT_Y" : BEGIN
                WIDGET_CONTROL,w_warningtext_ids.text,SET_VALUE='Y'
                END
        "WARNINGTEXT_N" : BEGIN
                WIDGET_CONTROL,w_warningtext_ids.text,SET_VALUE='N'
                END
        "WARNINGTEXT_OK" : BEGIN
		w_warningtext_quest
		END
        "WARNINGTEXT_CLOSE" : BEGIN
                WIDGET_CONTROL,event.top,BAD_ID=bad,/DESTROY
                END
ENDCASE
END


PRO w_warningtext, str,width,height,heading,title=title,quest=quest,xloc=xloc,yloc=yloc, GROUP = GROUP
COMMON w_warningtext_block,w_warningtext_ids

if XRegistered('w_warningtext') ne 0 then begin
	WIDGET_CONTROL,w_warningtext_ids.base,/DESTROY
	end
wtitle = 'scanSee Messages'
dtitle = ''
if n_elements(width) eq 0 then width = 80
if n_elements(height) eq 0 then height = 5 
if n_elements(heading) gt 0 then dtitle=string(heading)
if n_elements(title) gt 0 then wtitle=string(title)

w_warningtext_ids = { $
	base : 0L, $
	text : 0L, $
	quest : '', $
	answer : 'Y' $
	}

w_warningtext_base=WIDGET_BASE(TITLE = wtitle, $
	TLB_FRAME_ATTR = 2, $
	/COLUMN)
w_warningtext_ids.base = w_warningtext_base
w_warningtext_title = WIDGET_LABEL(w_warningtext_base,VALUE=dtitle)

list = WIDGET_TEXT(w_warningtext_base,VALUE=str,UVALUE='LIST', $
	XSIZE =width, $
	YSIZE=height,/SCROLL)

if n_elements(quest) ne 0 then begin
w_warningtext_ids.quest = string(quest)
w_warningtext_row =WIDGET_BASE(w_warningtext_base, /ROW, /FRAME)
w_warningtext_lab = WIDGET_LABEL(w_warningtext_row,VALUE=string(quest)+' (Y/N) ?')
w_warningtext_text = WIDGET_TEXT(w_warningtext_row,VALUE='Y', $
	EDITABLE=1, UVALUE='WARNINGTEXT_GET', XSIZE=2)
w_warningtext_ids.text = w_warningtext_text 

w_warningtext_y = WIDGET_BUTTON(w_warningtext_row,VALUE='Y', $
	UVALUE='WARNINGTEXT_Y')
w_warningtext_n = WIDGET_BUTTON(w_warningtext_row,VALUE='N', $
	UVALUE='WARNINGTEXT_N')

w_warningtext_actrow =WIDGET_BASE(w_warningtext_base, /ROW)
w_warningtext_ok = WIDGET_BUTTON(w_warningtext_actrow,VALUE=' Accept ', $
	UVALUE='WARNINGTEXT_OK')
close = WIDGET_BUTTON(w_warningtext_actrow, $
                        VALUE = ' Cancel ', $
                        UVALUE = 'WARNINGTEXT_CLOSE')

endif else begin
close = WIDGET_BUTTON(w_warningtext_base, $
                        VALUE = ' Close ', $
                        UVALUE = 'WARNINGTEXT_CLOSE')
end

if keyword_set(xloc) then begin
	if n_elements(yloc) eq 0 then yloc = 300
	WIDGET_CONTROL, w_warningtext_base,/REALIZE, $
	TLB_SET_XOFFSET= xloc, TLB_SET_YOFFSET= yloc 
endif else $
	WIDGET_CONTROL, w_warningtext_base,/REALIZE


XMANAGER,'w_warningtext',w_warningtext_base, GROUP_LEADER = GROUP


END

;
; view2d_datatotext.pro
;

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

PRO dataToText,data,px,py,title=title,unit=unit,file=file,help=help
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file

if n_elements(data) eq 0 then begin
	w_warningtext,'Error: no SDS data available!',60,3
	return
	end

if keyword_set(help) then begin
	print,''
	print,'DataToText  displays the 1D or 2D data in a scrolled window.'
	print,''
	print,'USAGE: DataToText, data, title, unit, file'
	print,''
	print,'INPUT:'
	print,'     data     - Input data array to be displayed'
	print,'     px       - Input px vector to be displayed'
	print,'     py       - Input py vector to be displayed'
	print,'KEYWORD:'
	print,'     title    - Descriptive title string, defaults to NULL'
	print,'     unit     - Descriptive unit string, defaults to NULL'
	print,'     file     - Text displaying file, defaults to "view2d_data.txt"
	return
	end

filename = 'view2d_data.txt'
if n_elements(file) ne 0 then filename = file

dir = ''
if catch2d_file.path ne '' then dir = catch2d_file.path
no_dir = 1
CATCH,error_status
if error_status ne 0 then begin  ; eq -206 then begin
	if no_dir eq 1 then dir = catch2d_file.home+OS_SYSTEM.file_sep ; '/'
	if no_dir eq 2 then dir = getenv('HOME')+OS_SYSTEM.file_sep ; '/'
	no_dir = no_dir + 1 
	if no_dir gt 3 then begin
	 res = widget_message([!err_string,dir+filename],/INFO)
	 return
	end
end

openw,fw,dir+filename,/get_lun
printf,fw,';'
close,fw
;
; rename filename
;
       ino = catch2d_file.image_no(catch2d_file.scanno_current-1) $
                + catch2d_file.detector
        suf0 = '0000'
        suf = strtrim(ino,2)
        ln = strlen(suf)
        strput,suf0,suf,4-ln
        rename = catch2d_file.name+'.'+suf0

        WIDGET_CONTROL,widget_ids.textdata,BAD_ID=bad ,/DESTROY
;        if widget_ids.textdata eq 0 or bad ne 0 then $
        widget_ids.textdata = CW_TERM(widget_ids.base, $
		TITLE='VIEW2D SDS Text Window', BG_NAMES='Save...', $
		FILENAME=dir+filename, RENAME=dir+rename, $
                 XSIZE=80, YSIZE=20, /SCROLL)

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

se = catch2d_file.image_no(catch2d_file.scanno_current-1) $
	+ catch2d_file.detector
 

openw,fw,dir+filename,/get_lun

printf,fw,'; 2D SCAN #  ',strtrim(catch2d_file.scanno_current,2) + $
                ',    Image seqno = ' + strtrim(se,2) + ',    Detector = '+ $
                strtrim(catch2d_file.detector,2)
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
;	xdisplayfile,filename
	id = CW_TERM(widget_ids.textdata,filename=dir+filename,rename=rename,/reset)
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
;	xdisplayfile,filename
	id = CW_TERM(widget_ids.textdata,filename=dir+filename,rename=rename,/reset)
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
;	xdisplayfile,filename
	id = CW_TERM(widget_ids.textdata,filename=dir+filename,rename=rename,/reset)
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

	id = CW_TERM(widget_ids.textdata,filename=dir+filename,rename=rename,/reset)
	return
end

END


PRO view2d_datatotext,filename=filename
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref

; check for user range
                xdim = catch2d_file.x_act_npts
                ydim = catch2d_file.y_act_npts
                xdim = catch2d_file.width
                ydim = catch2d_file.height

                x_min=0
                x_max=xdim-1
		y_min=0
                y_max=ydim-1

        if view_option.user eq 1 then begin
                if view_option.x_min gt x_min and view_option.x_min lt x_max then x_min = view_option.x_min
                if view_option.x_max lt x_max and view_option.x_max gt x_min then x_max = view_option.x_max
                if view_option.y_min gt y_min and view_option.y_min lt y_max then y_min = view_option.y_min
                if view_option.y_max lt y_max and view_option.y_max gt y_min then y_max = view_option.y_max

                newimage = image(x_min:x_max,y_min:y_max)
        endif else begin
                newimage = image
                end

                x = catch2d_file.xarr(0:catch2d_file.width-1)
                y = catch2d_file.yarr(0:catch2d_file.height-1)
                ix = n_elements(x)
                iy = n_elements(y)

                        if x_max lt ix then ix=x_max
                        if y_max lt iy then iy=y_max
                        newim = image(x_min:ix,y_min:iy)
                        nx=x(x_min:ix)
                        ny=y(y_min:iy)
		if n_elements(filename) eq 0 then $
			dataToText,newim,nx,ny else $
			dataToText,newim,nx,ny,file=filename

	
END




;
; plot y distributions vs values
;
;    xin: the input index number associated with the TV area
;
PRO catch2d_ydist,xin, Event
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file

if catch2d_file.x_act_npts lt 1 or catch2d_file.y_act_npts lt 1 then return
x_size = 3 > catch2d_file.x_act_npts
y_size = 3 > catch2d_file.y_act_npts

	x = xin

if x lt 0 or x ge x_size then begin
	st = ['Error:  X index out of range for image data.', $
		'        Valid X index range : [0 , '+strtrim(x_size-1,2)+']' $
		]
	w_warningtext,st,60,5,'VW2D Messages' 
	return
end

	WIDGET_CONTROL,widget_ids.y_min,GET_VALUE=y_min
	WIDGET_CONTROL,widget_ids.y_max,GET_VALUE=y_max
	WIDGET_CONTROL,widget_ids.x_min,GET_VALUE=x_min
	WIDGET_CONTROL,widget_ids.x_max,GET_VALUE=x_max

	if x_max gt x_size then x_max = x_size - 1

	if x lt x_min or x gt x_max then begin
		st = ['Error:  x index out of range for TV ydist.', $
			'        Valid x index range : ['+ strtrim(x_min,1) $
			+' , '+strtrim(x_max,2)+']' $
			]
		w_warningtext,st,60,5,'VW2D Messages' 
		return
	end

	y_min = fix(y_min)
	y_max = fix(y_max) - 1

	if y_min lt 0 then y_min = 0
	if y_max ge y_size then y_max = y_size - 1

	y_vec = image(x, y_min:y_max)
	xv = catch2d_file.xarr(x)
	title = 'At X(' + strtrim(x,2) + ') = ' + strtrim(xv,2)

	ay = catch2d_file.yarr(y_min:y_max)

; call plot1d resizable window 

	WIDGET_CONTROL,catch2d_file.yprof,BAD=bad,/DESTROY
	no = n_elements(y_vec)
	plot1d,ay,transpose(y_vec),id_tlb,windraw,GROUP=Event.top, $
		/cleanup, $
		wtitle='YZ Profile',xtitle='Y (Values)', ytitle='Z - VAL', $
		title=title
	catch2d_file.yprof =id_tlb 
	catch2d_file.yzdraw = windraw
	WIDGET_CONTROL,catch2d_file.yprof, $
		TLB_SET_XOFFSET= 10, TLB_SET_YOFFSET= 450

widget_ids.x2 = !x
widget_ids.y2 = !y

if !d.name eq OS_SYSTEM.device then WSET,widget_ids.plot2d_area

END

;
; plot x distributions vs values
;
;    yin: the input y index number associated with the TV area
;
PRO catch2d_xdist,yin, Event
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file

if catch2d_file.x_act_npts lt 1 or catch2d_file.y_act_npts lt 1 then return
x_size = 3 > catch2d_file.x_act_npts
y_size = 3 > catch2d_file.y_act_npts

	y = yin

if y lt 0 or y ge y_size then begin
	st = ['Error:  Y index out of range for image data.', $
		'        Valid Y index range : [0 , '+strtrim(y_size-1,2)+']' $
		]
	w_warningtext,st,60,5,'VW2D Messages' 
	return
end

	WIDGET_CONTROL,widget_ids.y_min,GET_VALUE=y_min
	WIDGET_CONTROL,widget_ids.y_max,GET_VALUE=y_max
	WIDGET_CONTROL,widget_ids.x_min,GET_VALUE=x_min
	WIDGET_CONTROL,widget_ids.x_max,GET_VALUE=x_max

	if y_max gt y_size then y_max = y_size - 1

	if y lt y_min or y gt y_max then begin
		st = ['Error:  y index out of range for TV ydist.', $
			'        Valid y index range : ['+ strtrim(y_min,1) $
			+','+strtrim(y_max,2)+']' $
			]
		w_warningtext,st,60,5,'VW2D Messages' 
		return
	end

	x_min = fix(x_min)
	x_max = fix(x_max) - 1

	if x_min lt 0 then y_min = 0
	if x_max ge x_size then x_max = x_size - 1

	x_vec = image( x_min:x_max,y)
	yv = catch2d_file.yarr(y)
	title = 'At Y(' + strtrim(y,2) + ') = ' + strtrim(yv,2)

	ax = catch2d_file.xarr(x_min:x_max)

; call plot1d resizable window 

	WIDGET_CONTROL,catch2d_file.xprof,BAD=bad,/DESTROY
	no = n_elements(x_vec)
	plot1d,ax,x_vec,id_tlb,windraw, GROUP=Event.top, $
		/cleanup, $
		wtitle='XZ Profile', xtitle='X (Values)', ytitle='Z - VAL', $
		title=title
	catch2d_file.xprof = id_tlb 
	catch2d_file.xzdraw = windraw
	WIDGET_CONTROL,catch2d_file.xprof, $
		TLB_SET_XOFFSET= 10, TLB_SET_YOFFSET= 50
	
widget_ids.x1 = !x
widget_ids.y1 = !y

if !d.name eq OS_SYSTEM.device then WSET,widget_ids.plot2d_area
END

;
; plot x,y distributions
;
PRO catch2d_xydist, Event
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file

;wdelete,catch2d_file.xprof,catch2d_file.yprof

x_size = 3 > catch2d_file.x_act_npts
y_size = 3 > catch2d_file.y_act_npts
;print,'x_size, y size ',x_size,y_size
;print,'x_mag, y_mag ',catch2d_file.x_mag, catch2d_file.y_mag

WSET,widget_ids.plot2d_area
hide_cross,view_option.x,view_option.y,view_option.d_wid,view_option.s_wid
;cursor,x,y,0,/device
x = Event.x
y = Event.y
if x lt 0 or y lt 0 then return

; save cursor location
view_option.x = x
view_option.y = y
show_cross,x,y,view_option.d_wid,view_option.s_wid

;TVCRS,x,y


	; get x plot range

	WIDGET_CONTROL,widget_ids.x_min,GET_VALUE=x_min
	WIDGET_CONTROL,widget_ids.x_max,GET_VALUE=x_max
	x_min = fix(x_min) 
	x_max = fix(x_max) - 1
	if x_min lt 0 then x_min = 0
	if x_max ge x_size then x_max = x_size - 1

	; get y plot range

	WIDGET_CONTROL,widget_ids.y_min,GET_VALUE=y_min
	WIDGET_CONTROL,widget_ids.y_max,GET_VALUE=y_max
	y_min = fix(y_min)  
	y_max = fix(y_max) - 1 
	if y_min lt 0 then y_min = 0
	if y_max ge y_size then y_max = y_size - 1

	; real mag factor

	rx_mag = float(!d.x_size) / (x_max-x_min+1)
	ry_mag = float(!d.y_size) / (y_max-y_min+1)

	x = round( float(x) / rx_mag)
	y = round( float(y) / ry_mag)

if x ge catch2d_file.width or y ge catch2d_file.height then begin
	w_warningtext,'Cursor outside the image range',60,5,'VW2D Messages'
	return
	end

;  find vectior values

zv = image(x+x_min,y+y_min)
if view_option.versus then begin
	xv = catch2d_file.xarr(x+x_min)
	yv = catch2d_file.yarr(y+y_min)
	ax = catch2d_file.xarr(x_min:x_max) 
	ay = catch2d_file.yarr(y_min:y_max)
	xtitle = ' (Values)'
endif else begin
	xv = x+x_min
	yv = y+y_min
	ax = indgen(x_max-x_min+1) + x_min
	ay = indgen(y_max-y_min+1) + y_min
	xtitle = ' (Step #)'
end

if y ge 0 and y lt y_size then begin

	x_vec = image(x_min:x_max,y + y_min) 
	y_vec = image(x + x_min, y_min:y_max)

; call plot1d resizaable window

	WIDGET_CONTROL,catch2d_file.xprof,BAD=bad,/DESTROY
	plot1d,ax,x_vec,id_tlb,windraw, GROUP=Event.top, $
		/cleanup, $
		wtitle='XZ Profile', xtitle='X '+xtitle, ytitle='Z - VAL', $
		title='At Y = '+ strtrim(yv,2)  + xtitle
	catch2d_file.xprof = id_tlb 
	catch2d_file.xzdraw = windraw
	WIDGET_CONTROL,catch2d_file.xprof, $
		TLB_SET_XOFFSET= 10, TLB_SET_YOFFSET= 50
	end
widget_ids.x1 = !x
widget_ids.y1 = !y

if x ge 0 and (x+x_min) lt x_size then begin


; call plot1d resizable window

	WIDGET_CONTROL,catch2d_file.yprof,BAD=bad,/DESTROY
	plot1d,ay,transpose(y_vec),id_tlb,windraw,GROUP=Event.top, $
		/cleanup, $
		wtitle='YZ Profile', xtitle='Y '+xtitle, ytitle='Z - VAL', $
		title='At X = '+ strtrim(xv,2) + xtitle
	catch2d_file.yprof =id_tlb 
	catch2d_file.yzdraw = windraw
	WIDGET_CONTROL,catch2d_file.yprof, $
		TLB_SET_XOFFSET= 10, TLB_SET_YOFFSET= 450

widget_ids.x2 = !x
widget_ids.y2 = !y

WIDGET_CONTROL,widget_ids.x_cursor,SET_VALUE=strtrim(xv,2)
WIDGET_CONTROL,widget_ids.y_cursor,SET_VALUE=strtrim(yv,2)
WIDGET_CONTROL,widget_ids.z_cursor,SET_VALUE=strtrim(zv)

	end

if !d.name eq OS_SYSTEM.device then WSET,widget_ids.plot2d_area
END

; 
; get cursor coordinates
;
PRO catch2d_xycoord, x, y, Event
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file

hide_cross,view_option.x,view_option.y,view_option.d_wid,view_option.s_wid
;cursor,x,y,0,/device
x = Event.x
y = Event.y
view_option.x = x
view_option.y = y
show_cross,x,y,view_option.d_wid,view_option.s_wid

	x = x / catch2d_file.x_mag
	y = y / catch2d_file.y_mag


; if user coordinate mode is set

if view_option.user eq 1 then begin
	WIDGET_CONTROL,widget_ids.y_min,GET_VALUE=y_min
	if y_min gt 0 then y = fix( y + y_min)
	WIDGET_CONTROL,widget_ids.x_min,GET_VALUE=x_min
	if x_min gt 0 then x = fix( x + x_min)
	end

if x lt catch2d_file.width and y lt catch2d_file.height then begin
;print,'x,y,zval',x,y, image(x,y)

	zv = image(x,y)
	if view_option.versus then begin
		xv = catch2d_file.xarr(x)
		yv = catch2d_file.yarr(y)
	endif else begin
		xv = x
		yv = y
	end
WIDGET_CONTROL,widget_ids.x_cursor,SET_VALUE=strtrim(xv,2) + '(*)'
WIDGET_CONTROL,widget_ids.y_cursor,SET_VALUE=strtrim(yv,2) + '(*)'
WIDGET_CONTROL,widget_ids.z_cursor,SET_VALUE=strtrim(zv,2) + '(*)'
endif else begin
	w_warningtext,'Cursor outside the image range',60,5,'VW2D Messages'
	end

END

;
; xdistribution cursor
;
PRO catch2d_xycoord1,st
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON w_warningtext_block,w_warningtext_ids

!x = widget_ids.x1
!y = widget_ids.y1
CATCH,error_status
if error_status ne 0 then begin
	st = ['Click MMB in the 2D image area first!','Before press the XZ button.']
	w_warningtext,st,60,5,'VW2D Messages',xloc=500
	return
	end

;WSET,catch2d_file.xprof
WSET,catch2d_file.xzdraw
wshow,catch2d_file.xzdraw

!ERR = 1
dline = (!y.crange(1)-!y.crange(0)) *.2
hline = (!x.crange(1)-!x.crange(0)) *.1
clr1 = 0
clr2 = !d.n_colors - 1

while !ERR eq 1 do begin
cursor,x,y,1,/normal

x = (x - !x.window(0)) / (!x.window(1)-!x.window(0)) * $
        (!x.crange(1)-!x.crange(0)) + !x.crange(0)
 
y = (y - !y.window(0)) / (!y.window(1)-!y.window(0)) * $
        (!y.crange(1)-!y.crange(0)) + !y.crange(0)

oplot,catch2d_file.xzline_x, catch2d_file.xzline_z, color=clr1
oplot,catch2d_file.xzline_xo, catch2d_file.xzline_zo, color=clr1

catch2d_file.xzline_x = [x,x]
catch2d_file.xzline_z = [y-dline,y+dline]
catch2d_file.xzline_xo = [x-hline,x+hline]
catch2d_file.xzline_zo = [y,y]
oplot,catch2d_file.xzline_x, catch2d_file.xzline_z, color=clr2
oplot,catch2d_file.xzline_xo, catch2d_file.xzline_zo, color=clr2

st = 'X='+strtrim(x,2)+', Z='+strtrim(y,2)
WIDGET_CONTROL,widget_ids.xzl,SET_VALUE=st
endwhile
oplot,catch2d_file.xzline_x, catch2d_file.xzline_z, color=clr1
oplot,catch2d_file.xzline_xo, catch2d_file.xzline_zo, color=clr1
WIDGET_CONTROL,w_warningtext_ids.base,/DESTROY

if !d.name eq OS_SYSTEM.device then WSET,widget_ids.plot2d_area
END

;
; ydistribution cursor
;
PRO catch2d_xycoord2,st
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON w_warningtext_block,w_warningtext_ids

!x = widget_ids.x2
!y = widget_ids.y2
CATCH,error_status
if error_status ne 0 then begin
;if error_status eq -324 then begin
	st = ['Click MMB in the 2D image area first!','Before press the YZ button.']
	w_warningtext,st,60,5,'VW2D Messages',xloc=500
	return
	end
;WSET,catch2d_file.yprof
WSET,catch2d_file.yzdraw
wshow,catch2d_file.yzdraw

!ERR = 1
dline = (!y.crange(1)-!y.crange(0)) *.2
hline = (!x.crange(1)-!x.crange(0)) *.1
clr1 = 0
clr2 = !d.n_colors - 1
while !ERR eq 1 do begin
cursor,x,y,1,/normal

x = (x - !x.window(0)) / (!x.window(1)-!x.window(0)) * $
        (!x.crange(1)-!x.crange(0)) + !x.crange(0)
 
y = (y - !y.window(0)) / (!y.window(1)-!y.window(0)) * $
        (!y.crange(1)-!y.crange(0)) + !y.crange(0)

oplot,catch2d_file.yzline_y, catch2d_file.yzline_z, color=clr1
oplot,catch2d_file.yzline_yo, catch2d_file.yzline_zo, color=clr1

catch2d_file.yzline_y = [x,x]
catch2d_file.yzline_z = [y-dline,y+dline]
catch2d_file.yzline_yo = [x-hline,x+hline]
catch2d_file.yzline_zo = [y,y]
oplot,catch2d_file.yzline_y, catch2d_file.yzline_z, color=clr2
oplot,catch2d_file.yzline_yo, catch2d_file.yzline_zo, color=clr2

st = 'Y='+strtrim(x,2)+', Z='+strtrim(y,2)
WIDGET_CONTROL,widget_ids.yzl,SET_VALUE=st
endwhile
oplot,catch2d_file.yzline_y, catch2d_file.yzline_z, color=clr1
oplot,catch2d_file.yzline_yo, catch2d_file.yzline_zo, color=clr1
WIDGET_CONTROL,w_warningtext_ids.base,/DESTROY

if !d.name eq OS_SYSTEM.device then WSET,widget_ids.plot2d_area
END


;
; plot x,y distributions
;
PRO catch2d_xydist2, Event
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file

;wdelete,catch2d_file.xprof,catch2d_file.yprof

x_size = 3 > catch2d_file.x_act_npts
y_size = 3 > catch2d_file.y_act_npts

WSET,widget_ids.plot2d_area
hide_cross,view_option.x,view_option.y,view_option.d_wid,view_option.s_wid
;cursor,x,y,0,/device
x = Event.x
y = Event.y
if x lt 0 or y lt 0 then return

; save cursor location
view_option.x = x
view_option.y = y
show_cross,x,y,view_option.d_wid,view_option.s_wid

;TVCRS,x,y


	; get x plot range

	WIDGET_CONTROL,widget_ids.x_min,GET_VALUE=x_min
	WIDGET_CONTROL,widget_ids.x_max,GET_VALUE=x_max
	x_min = fix(x_min) 
	x_max = fix(x_max) - 1
	if x_min lt 0 then x_min = 0
	if x_max ge x_size then x_max = x_size - 1

	; get y plot range

	WIDGET_CONTROL,widget_ids.y_min,GET_VALUE=y_min
	WIDGET_CONTROL,widget_ids.y_max,GET_VALUE=y_max
	y_min = fix(y_min)  
	y_max = fix(y_max) - 1 
	if y_min lt 0 then y_min = 0
	if y_max ge y_size then y_max = y_size - 1

	; real mag factor

	rx_mag = catch2d_file.x_mag
	ry_mag = catch2d_file.y_mag

	x = fix( float(x-view_option.margin_l) / rx_mag)
	y = fix( float(y-view_option.margin_b) / ry_mag)

if x ge catch2d_file.width or y ge catch2d_file.height then begin
	w_warningtext,'Cursor outside the image range',60,5,'VW2D Messages'
	return
	end

;  find vectior values

zv = image(x+x_min,y+y_min)
xv = catch2d_file.xarr(x+x_min)
yv = catch2d_file.yarr(y+y_min)
if view_option.versus then begin
	ax = catch2d_file.xarr(x_min:x_max) 
	ay = catch2d_file.yarr(y_min:y_max)
	xtitle = ' (Values)'
endif else begin
	ax = indgen(x_max-x_min+1) + x_min
	ay = indgen(y_max-y_min+1) + y_min
	xtitle = ' (Step #)'
end

if y ge 0 and y lt y_size then begin

	x_vec = image(x_min:x_max,y + y_min) 
	y_vec = image(x + x_min, y_min:y_max)

; call plot1d resizaable window

	WIDGET_CONTROL,catch2d_file.xprof,BAD=bad,/DESTROY
	plot1d,ax,x_vec,id_tlb,windraw, GROUP=Event.top, $
		/cleanup, $
		wtitle='XZ Profile', xtitle='X '+xtitle, ytitle='Z - VAL', $
		title='At Y('+strtrim(y+y_min,2)+') = '+ strtrim(yv,2)  + xtitle
	catch2d_file.xprof = id_tlb 
	catch2d_file.xzdraw = windraw
	WIDGET_CONTROL,catch2d_file.xprof, $
		TLB_SET_XOFFSET= 10, TLB_SET_YOFFSET= 50
	end
widget_ids.x1 = !x
widget_ids.y1 = !y

if x ge 0 and (x+x_min) lt x_size then begin


; call plot1d resizable window

	WIDGET_CONTROL,catch2d_file.yprof,BAD=bad,/DESTROY
	plot1d,ay,transpose(y_vec),id_tlb,windraw,GROUP=Event.top, $
		/cleanup, $
		wtitle='YZ Profile', xtitle='Y '+xtitle, ytitle='Z - VAL', $
		title='At X('+strtrim(x+x_min,2)+') = '+ strtrim(xv,2) + xtitle
	catch2d_file.yprof =id_tlb 
	catch2d_file.yzdraw = windraw
	WIDGET_CONTROL,catch2d_file.yprof, $
		TLB_SET_XOFFSET= 10, TLB_SET_YOFFSET= 450

widget_ids.x2 = !x
widget_ids.y2 = !y

WIDGET_CONTROL,widget_ids.x_cursor,SET_VALUE=strtrim(xv,2)
WIDGET_CONTROL,widget_ids.y_cursor,SET_VALUE=strtrim(yv,2)
WIDGET_CONTROL,widget_ids.z_cursor,SET_VALUE=strtrim(zv)

	end

if !d.name eq OS_SYSTEM.device then WSET,widget_ids.plot2d_area
END

; 
; get cursor coordinates
;
PRO catch2d_xycoord_TV, x, y, Event
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file

hide_cross,view_option.x,view_option.y,view_option.d_wid,view_option.s_wid
;cursor,x,y,0,/device
x = Event.x
y = Event.y
view_option.x = x
view_option.y = y
show_cross,x,y,view_option.d_wid,view_option.s_wid

        ; real mag factor

        rx_mag = catch2d_file.x_mag
        ry_mag = catch2d_file.y_mag

        x = fix( float(x-view_option.margin_l) / rx_mag)
        y = fix( float(y-view_option.margin_b) / ry_mag)



; if user coordinate mode is set

if view_option.user eq 1 then begin
	WIDGET_CONTROL,widget_ids.y_min,GET_VALUE=y_min
	if y_min gt 0 then y = fix( y + y_min)
	WIDGET_CONTROL,widget_ids.x_min,GET_VALUE=x_min
	if x_min gt 0 then x = fix( x + x_min)
	end

if x lt catch2d_file.width and x ge 0 and y ge 0 and  y lt catch2d_file.height then begin
;print,'x,y,zval',x,y, image(x,y)

	zv = image(x,y)
	if view_option.versus then begin
		xv = catch2d_file.xarr(x)
		yv = catch2d_file.yarr(y)
	endif else begin
		xv = x
		yv = y
	end
WIDGET_CONTROL,widget_ids.x_cursor,SET_VALUE=strtrim(xv,2) + '(*)'
WIDGET_CONTROL,widget_ids.y_cursor,SET_VALUE=strtrim(yv,2) + '(*)'
WIDGET_CONTROL,widget_ids.z_cursor,SET_VALUE=strtrim(zv,2) + '(*)'
endif else begin
	w_warningtext,'Cursor outside the image range',60,5,'VW2D Messages'
	end

END
PRO scanimage_alloc,filename,gD,scanno

gData = { $
	scanno	: ptr_new(/allocate_heap), $  ;0L, $
	dim	: ptr_new(/allocate_heap), $  ;0, $
	num_pts	: ptr_new(/allocate_heap), $  ;[0,0], $
	cpt	: ptr_new(/allocate_heap), $  ;[0,0], $
	id_def	: ptr_new(/allocate_heap), $  ;intarr(19,2), $
	pv	: ptr_new(/allocate_heap), $  ;['',''], $
	labels	: ptr_new(/allocate_heap), $  ;strarr(57,2), $
	pa1D	: ptr_new(/allocate_heap), $
	da1D	: ptr_new(/allocate_heap), $
	pa2D	: ptr_new(/allocate_heap), $
	da2D	: ptr_new(/allocate_heap) $
	}
	gD = ptr_new(/allocate_heap)
	*gD = gData

;	scanno = read_scan(filename,dim,num_pts,cpt,pv,labels,id_def,pa1D,da1D,pa2D,da2D)

; help,scanno,dim,num_pts,cpt,pv,labels,id_def,pa1d,pa2d,da1d,da2d

	scanno = read_scan(filename, Scan)
	*gData.scanno = scanno

	if scanno lt 0 then return
	rix2DC, Scan, gData
;	*gData.dim = dim
;	*gData.num_pts = num_pts
;	*gData.cpt = cpt
;	*gData.pv = pv
;	if n_elements(labels) then $
;	*gData.labels = labels
;	if n_elements(id_def) then $
;	*gData.id_def = id_def
;	*gData.pa1D = pa1D
;	*gData.da1D = da1D
;	if dim eq 2 then begin
;	*gData.pa2D = pa2D
;	*gData.da2D = da2D
;	end

;	scanimage_print,gD

END

PRO scanimage_print,gD,test=test
	gData = *gD
	print,'scanno  : ',*gData.scanno
	print,'dim     : ',*gData.dim
	print,'num_pts : ',*gData.num_pts
	print,'cpt     : ',*gData.cpt
	print,'id_def  : ',*gData.id_def
	print,'pvname  : ',*gData.pv
	print,'labels  : ',*gData.labels
	help,*gData.pa1D
	help,*gData.da1D
	if *gDdata.dim eq 2 then begin
	help,*gData.pa2D
	help,*gData.da2D
	end
	
	if keyword_set(test) then begin
	num_pts = *gData.num_pts
	width = num_pts(0)
	height = num_pts(1)
	help,width,height
	da2D = *gData.da2D
	im = da2d(*,*,1)
	help,im
	tvscl, congrid(im,400,400),/NAN  ; IDL 5.1
	end

END

PRO scanimage_free,gD
	gData = *gD
	if ptr_valid(gData.scanno) then	ptr_free,gData.scanno
	if ptr_valid(gData.dim) then	ptr_free,gData.dim
	if ptr_valid(gData.num_pts) then	ptr_free,gData.num_pts
	if ptr_valid(gData.cpt) then	ptr_free,gData.cpt
	if ptr_valid(gData.id_def) then	ptr_free,gData.id_def
	if ptr_valid(gData.pv) then	ptr_free,gData.pv
	if ptr_valid(gData.labels) then	ptr_free,gData.labels
	if ptr_valid(gData.pa1D) then	ptr_free,gData.pa1D
	if ptr_valid(gData.da1D) then	ptr_free,gData.da1D
	if ptr_valid(gData.pa2D) then	ptr_free,gData.pa2D
	if ptr_valid(gData.da2D) then	ptr_free,gData.da2D
	if ptr_valid(gD) then ptr_free,gD
END

PRO scanimage_cleanup
	help,/heap_variables
	heap_gc
END


PRO rix2BenChin, Scan
ON_ERROR,1
  if(*Scan.dim EQ 1) then begin
    BenChin= { $
	scanno	: Scan.scanno, $
	dim	: Scan.dim, $
	num_pts : Scan.npts, $
	cpt	: Scan.cpt, $
	id_def	: Scan.id_def, $
	pv	: Scan.pv, $
	labels	: Scan.labels, $
	pa1D	: (*Scan.pa)[0], $
	da1D	: (*Scan.da)[0], $
	pa2D	: ptr_new(/ALLOCATE_HEAP), $
	da2D	: ptr_new(/ALLOCATE_HEAP) $
	}
  endif else begin
    BenChin= { $
	scanno	: Scan.scanno, $
	dim	: Scan.dim, $
	num_pts : Scan.npts, $
	cpt	: Scan.cpt, $
	id_def	: Scan.id_def, $
	pv	: Scan.pv, $
	labels	: Scan.labels, $
	pa1D	: (*Scan.pa)[1], $
	da1D	: (*Scan.da)[1], $
	pa2D	: (*Scan.pa)[0], $
	da2D	: (*Scan.da)[0] $
	}
  endelse

  ptr_free,Scan.pa
  ptr_free,Scan.da
  Scan=BenChin
END

PRO rix2DC,Scan,gData
ON_ERROR,1
 
        *gData.scanno  = *Scan.scanno
        *gData.dim     = *Scan.dim
        *gData.num_pts = *Scan.npts
        *gData.cpt     = *Scan.cpt
        *gData.id_def  = *Scan.id_def
        *gData.pv      = *Scan.pv
        *gData.labels  = *Scan.labels

	if *Scan.dim eq 1 then begin
          *gData.pa1D  = *(*Scan.pa)[0]
          *gData.da1D  = *(*Scan.da)[0]
	  *gData.pa2D  = ptr_new(/ALLOCATE_HEAP)
	  *gData.da2D  = ptr_new(/ALLOCATE_HEAP)
	end
	if *Scan.dim eq 2 then begin
          *gData.pa1D    = *(*Scan.pa)[1]
          *gData.da1D    = *(*Scan.da)[1]
          *gData.pa2D  = *(*Scan.pa)[0]
          *gData.da2D  = *(*Scan.da)[0]
        end
 
  ptr_free,Scan.scanno
  ptr_free,Scan.dim
  ptr_free,Scan.npts
  ptr_free,Scan.cpt
  ptr_free,Scan.id_def
  ptr_free,Scan.pv
  ptr_free,Scan.labels
  ptr_free,Scan.pa
  ptr_free,Scan.da


END
	
FUNCTION nbElem,dim,vector
  res=1L
  for i=0,dim-1 do begin
     res= res*vector[i]
  end
  return, res
END

FUNCTION read_scan_rest,lun,Scan,dim,offset
ON_IOERROR, BAD	

  rank=0
  npts=0
  cpt=0
  readu,lun,rank,npts,cpt

  nb_pts=cpt
  if(nb_pts EQ npts) then nb_pts=nb_pts-1

  if(rank GT 1) then begin $
    sub_scan_ptr=lonarr(npts)
    readu,lun,sub_scan_ptr
  endif

  (*Scan.cpt)[rank-1]=cpt;

  ; read the pvname
  name=''
  time=''
  readu,lun,name,time
  
  nb_pos=0
  nb_det=0
  nb_trg=0
  readu,lun,nb_pos,nb_det,nb_trg

  if(nb_pos NE 0) then begin $
    pos_num=intarr(nb_pos)
  endif
  pos_info= { $
     pxpv:'', $
     pxds:'', $
     pxsm:'', $
     pxeu:'', $
     rxpv:'', $
     rxds:'', $
     rxeu:'' }
  if(nb_det NE 0) then begin
    det_num=intarr(nb_det)
  endif

  det_info= { $
     dxpv:'', $
     dxds:'', $
     dxeu:'' }
  if(nb_trg NE 0) then trg_num=intarr(nb_trg)
  trg_info= { $
     txpv:'', $
     txcd:'' }

  num=0
  for i=0,nb_pos-1 do begin
     readu,lun, num
     pos_num[i]=num
     readu,lun,pos_info
  end

  for i=0,nb_det-1 do begin
     readu,lun,num
     det_num[i]=num
     readu,lun,det_info
  end

  for i=0,nb_trg-1 do begin
     readu,lun,num
     trg_num[i]=num
     readu,lun,trg_info
  end
  
  tmp=dblarr(npts)
  for i=0,nb_pos-1 do begin
     readu,lun,tmp
     if(cpt NE 0) then $
       (*(*Scan.pa)[rank-1])[offset:offset+cpt-1,pos_num[i]]=tmp[0:cpt-1]
  end

  tmp=fltarr(npts)
  for i=0,nb_det-1 do begin
     readu,lun,tmp
     if(cpt NE 0) then $
       (*(*Scan.da)[rank-1])[offset:offset+cpt-1,det_num[i]]=tmp[0:cpt-1]
  end

  if(rank GT 1) then begin
    sub_offset=offset
    nb_sub= cpt
    if(cpt NE npts) then nb_sub=nb_sub+1
    for i=0,nb_sub do begin
      res= read_scan_rest(lun,Scan,dim+1,sub_offset)
      if(res NE 1) then goto,BAD
    end
  end

  offset=offset+(*Scan.npts)[rank+dim-2]

  return, 1

BAD:
  return, 0
end  



FUNCTION read_scan_first,lun,Scan,dim
ON_IOERROR, BAD	

  rank=0
  npts=0
  cpt=0
  readu,lun,rank,npts,cpt

  if(rank GT 1) then begin $
    sub_scan_ptr=lonarr(npts)
    readu,lun,sub_scan_ptr
  endif

  (*Scan.cpt)[rank-1]=cpt;

  ; read the pvname
  name=''
  time=''
  readu,lun,name,time
  (*Scan.pv)[rank-1]=name
  
  nb_pos=0
  nb_det=0
  nb_trg=0
  readu,lun,nb_pos,nb_det,nb_trg

  dims=(*Scan.npts)[rank-1:rank+dim-1]
  size= nbElem(dim+1, dims)
  if(nb_pos NE 0) then pos_num= intarr(nb_pos)

  (*Scan.pa)[rank-1]= ptr_new(dblarr(size,4), /NO_COPY)
;  (*(*Scan.pa)[rank-1])[*]= !VALUES.D_NAN  

  pos_info= { $
     pxpv:'', $
     pxds:'', $
     pxsm:'', $
     pxeu:'', $
     rxpv:'', $
     rxds:'', $
     rxeu:'' }


  if(nb_det NE 0) then det_num=intarr(nb_det)

  (*Scan.da)[rank-1]= ptr_new(fltarr(size,15), /NO_COPY)
;  (*(*Scan.da)[rank-1])[*]= !VALUES.F_NAN

  det_info= { $
     dxpv:'', $
     dxds:'', $
     dxeu:'' }

  if(nb_trg NE 0) then trg_num=intarr(nb_trg)
  trg_info= { $
     txpv:'', $
     txcd:'' }

  num=0
  for i=0,nb_pos-1 do begin
     readu,lun, num
     pos_num[i]=num
     readu,lun,pos_info
     (*Scan.id_def)[num,rank-1]=1
     if(pos_info.rxpv NE '') then begin
	(*Scan.labels)[num,rank-1]= pos_info.rxpv
	(*Scan.labels)[19+num,rank-1]= pos_info.rxds
	(*Scan.labels)[38+num,rank-1]= pos_info.rxeu
     endif else begin
	(*Scan.labels)[num,rank-1]= pos_info.pxpv
	(*Scan.labels)[19+num,rank-1]= pos_info.pxds
	(*Scan.labels)[38+num,rank-1]= pos_info.pxeu
     endelse
  end

  for i=0,nb_det-1 do begin
     readu,lun,num
     det_num[i]=num
     readu,lun,det_info
     (*Scan.id_def)[4+num,rank-1]=1
     (*Scan.labels)[4+num,rank-1]= det_info.dxpv
     (*Scan.labels)[23+num,rank-1]= det_info.dxds
     (*Scan.labels)[42+num,rank-1]= det_info.dxeu
  end

  for i=0,nb_trg-1 do begin
     readu,lun,num
     trg_num[i]=num
     readu,lun,trg_info
  end
  
  tmp=dblarr(npts)
  for i=0,nb_pos-1 do begin
     readu,lun,tmp
     if(cpt NE 0) then (*(*Scan.pa)[rank-1])[0:cpt-1,pos_num[i]]=tmp[0:cpt-1]
  end

  tmp=fltarr(npts)
  for i=0,nb_det-1 do begin
     readu,lun,tmp
     if(cpt NE 0) then (*(*Scan.da)[rank-1])[0:cpt-1,det_num[i]]=tmp[0:cpt-1]
  end

  if(rank GT 1) then begin
    res=0
    res= read_scan_first(lun,Scan,dim+1)
    if(res NE 1) then goto,BAD
    offset= LONG((*Scan.npts)[rank+dim-2])
    nb_sub= cpt-1
    if(cpt NE npts) then nb_sub=nb_sub+1
    for i=1,nb_sub do begin
      res= read_scan_rest(lun,Scan,dim+1,offset)
      if(res NE 1) then goto,BAD
    end
  end

  return, 1

BAD:
  return, 0
end  


FUNCTION read_scan,filename, Scan

  ON_ERROR,1
  ON_IOERROR,BAD

  res=0

  Scan = { $
	scanno	: ptr_new(/allocate_heap), $  ;0L, $
	dim	: ptr_new(/allocate_heap), $  ;0, $
	npts	: ptr_new(/allocate_heap), $  ;[0,0], $
	cpt	: ptr_new(/allocate_heap), $  ;[0,0], $
	id_def	: ptr_new(/allocate_heap), $  ;intarr(19,2), $
	pv	: ptr_new(/allocate_heap), $  ;['',''], $
	labels	: ptr_new(/allocate_heap), $  ;strarr(57,2), $
	pa	: ptr_new(/allocate_heap), $
	da	: ptr_new(/allocate_heap) $
	}

  get_lun, lun
  openr, /XDR, lun, filename      ;Open the file for input.

  tmp= {$
     version: 0.0, $
     scanno: 0L, $
     rank: 0L }

  readu,lun, tmp

  npts= intarr(tmp.rank)
  readu,lun, npts
  readu,lun, isRegular
  readu,lun, env_fptr

  *Scan.scanno=tmp.scanno
  *Scan.dim= tmp.rank
  *Scan.npts= reverse(npts)
  *Scan.cpt = intarr(tmp.rank)
  *Scan.id_def= intarr(19,tmp.rank)
  *Scan.pv= strarr(tmp.rank)
  *Scan.labels= strarr(57,tmp.rank)
  *Scan.pa= ptrarr(tmp.rank)
  *Scan.da= ptrarr(tmp.rank)

  if(read_scan_first(lun, Scan, 0) NE 1) then goto,BAD

  for i=0,tmp.rank-1 do begin
    dims=(*Scan.npts)[i:tmp.rank-1]
    *(*Scan.pa)[i]= reform(*(*Scan.pa)[i], [dims,4])
    *(*Scan.da)[i]= reform(*(*Scan.da)[i], [dims,15])
  end

  res= *Scan.scanno

  goto,DONE
BAD:
  res= -1
  print, !ERR_STRING

DONE:
  free_lun, lun

  return, res
END

PRO scanimage_readall,filename,maxno,gD
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus

	catch2d_file.seqno = 0
        catch2d_file.scanno_2d_last = 0 

	scanimage_alloc,filename,gD

	scanno = *(*gD).scanno
	dim = *(*gD).dim
	if scanno lt 0  or dim ne 2 then return
	num_pts = *(*gD).num_pts
	cpt = *(*gD).cpt
	pv = *(*gD).pv
	labels = *(*gD).labels
	id_def = *(*gD).id_def
	pa1D = *(*gD).pa1D
	da1D = *(*gD).da1D
	pa2D = *(*gD).pa2D
	da2D = *(*gD).da2D

	catch2d_file.scanno_2d = scanno

	max_pidi = n_elements(id_def) / 2
	pv1_desc = labels(max_pidi,0)
	if pv1_desc eq '' then pv1_desc = labels(0,0)
	pv2_desc = labels(max_pidi,1)
	if pv2_desc eq '' then pv2_desc = labels(0,1)
	pvs0 = [pv(0:1),filename,pv1_desc,pv2_desc]

	seqno = 0
	id = 0
	pvs = pvs0
	FOR I=4,max_pidi-1 DO BEGIN
	if id_def(i,0) ne 0 then begin
	detector = i - 4
	y_name = labels(i+max_pidi,0)
	if y_name eq '' then y_name = labels(i,0)
	pvs = [ pvs,y_name]
	nos = [cpt(0),num_pts(0),cpt(1),detector,scanno,num_pts(1)]
	x = pa2D(*,0,0)
	y = pa1D(*,0)
	image = da2D(*,*,i-4)

		scanno_2d = nos(4)
		detector = nos(3) + 1

	id = id + 1
	end
	END

readfail:
if scanno eq 0 then scanno = 1
	maxno = id
	catch2d_file.maxno = maxno
	catch2d_file.seqno = maxno-1
	catch2d_file.scanno = scanno	
	if catch2d_file.scanno_2d_last le 0 then $
		catch2d_file.scanno_2d_last = scanno	
	catch2d_file.image_no(catch2d_file.scanno_2d_last) = maxno 
	catch2d_file.image_no(catch2d_file.scanno_2d_last - 1) = 0


END


PRO scanimage_readRecord,seqno,gD,view=view
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus

IF ptr_valid((*gD).da2D) THEN BEGIN
	scanno = *(*gD).scanno
	dim = *(*gD).dim
	num_pts = *(*gD).num_pts
	cpt = *(*gD).cpt
	pv = *(*gD).pv
	labels = *(*gD).labels
	id_def = *(*gD).id_def
	pa1D = *(*gD).pa1D
	da1D = *(*gD).da1D
	pa2D = *(*gD).pa2D
	da2D = *(*gD).da2D

	catch2d_file.scanno_2d = scanno

	max_pidi = n_elements(id_def) / 2
	pv1_desc = labels(max_pidi,0)
	if pv1_desc eq '' then pv1_desc = labels(0,0)
	pv2_desc = labels(max_pidi,1)
	if pv2_desc eq '' then pv2_desc = labels(0,1)
	filename = catch2d_file.name
	pvs0 = [pv(0:1),filename,pv1_desc,pv2_desc]

	pvs = pvs0
	I = seqno + 4 
	IF I ge 0 and I lt max_pidi-1 THEN BEGIN
	if id_def(i,0) ne 0 then begin
	detector = seqno
	y_name = labels(i+max_pidi,0)
	if y_name eq '' then y_name = labels(i,0)
	pvs = [ pvs,y_name]
	nos = [cpt(0),num_pts(0),cpt(1),detector,scanno,num_pts(1)]
	x = pa2D(*,0,0)
	y = pa1D(*,0)
	image = da2D(*,*,seqno)

		scanno_2d = nos(4)
		detector = nos(3) + 1
	
	catch2d_file.x_pv = pvs(0)
	catch2d_file.y_pv = pvs(1)
	catch2d_file.file_1d = pvs(2)
	catch2d_file.x_desc = pvs(3)
	catch2d_file.y_desc = pvs(4)
	catch2d_file.scanno = scanno
	catch2d_file.width = num_pts(0)
	catch2d_file.height = cpt(1)
	catch2d_file.detector = detector
	catch2d_file.scanno_current = scanno
	if scanno le 0 then catch2d_file.scanno_current = 1
	catch2d_file.y_req_npts = num_pts(1)
	catch2d_file.xarr = x
	catch2d_file.yarr = y
	catch2d_file.image = image

        newImage = image
        s = size(newImage)

        ; find the max only for 2D or 1D

        if s(0) eq 2 then totalno = s(4) - 1
        if s(0) eq 1 then totalno = s(3) - 1
        view_option.z_max = newImage(0)
        view_option.i_max = 0
        view_option.j_max = 0
        view_option.z_max = MAX(newImage,imax)
        view_option.j_max = imax / s(1)
        view_option.i_max = imax mod s(1)
        view_option.z_min = MIN(newImage,imax)
        view_option.j_min = imax / s(1)
        view_option.i_min = imax mod s(1)
        if view_option.fullcolor eq 0 then begin
                view_option.k_max = view_option.z_max
                view_option.k_min = view_option.z_min
        end

        if s(0) ne 2 then begin
                w_warningtext,'Warning: data is not 2D image ',60,5,'VW2D Messages'
                end

	if keyword_set(view) then $
        REPLOT

	end
	END



END
END


PRO view2d_pan_images_on
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH1D_2D_COM,data_2d, gD

seq = catch2d_file.scanno_current
last = catch2d_file.scanno_2d_last

if catch2d_file.maxno le 0 then begin
	res=WIDGET_MESSAGE('Error: no image file loaded in')
	return
end
if seq lt 1 or seq ne catch2d_file.scanno_2d_last then begin
	res=WIDGET_MESSAGE('Error: outside range seq entered')
	return
end

	seqno = catch2d_file.image_no(seq-1)

	da2D = *(*gD).da2D
	id_def = *(*gD).id_def

	def = make_array(15,value=0)

	xdim = catch2d_file.width 
	ydim = catch2d_file.y_req_npts
	image_array  = make_array(xdim,ydim,15,/float)

	scanno_2d = seq
	for i=0,14 do begin
		if id_def(i+4) gt 0 then begin
		t_image = da2D(*,*,i)
		image_array(*,*,i) = t_image
		end
	end

; update the image plot

update:

	width = 60
	height = 60
	old_win = !D.window
	if catch2d_file.win lt 0 then begin
		window, xsize = 8*width, ysize=2*height, $
			title='PanImages 2D SCAN # '+strtrim(catch2d_file.scanno_current,2)
		for i=0,14 do begin
		xi=(i mod 8)*width+width/2 - 5 
		yi=height/2+(15-i)/8*height
		xyouts, xi,yi,'D'+strtrim(i+1,2),/device
		end
	end

	new_win = !D.window
	wset,new_win
	for sel=0,14 do begin
	if id_def(sel+4) gt 0 then begin
	v_max = max(image_array(*,*,sel),min=v_min)
	if v_max eq v_min then begin 
		temp = view_option.ncolors * image_array(*,*,sel) 
		TV,congrid(temp,width,height),sel
	endif else begin
		temp = congrid(image_array(*,*,sel), width, height)
		TVSCL, temp, sel
	end
	end
	end


	plots,[0,8*width],[height,height],/device
	for i=1,7 do plots,[i*width,i*width],[0,2*height],/device

	wset,old_win
;	viewscanimage_current

END

PRO viewscanimage_init,file
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH1D_2D_COM,data_2d,gD

scanimage_readall,file,maxno,gD
;scanimage_print,gD

if n_elements(maxno) lt 1 then begin
	res = dialog_message([file, '',' is not a 2D scan file !'],/Info)
	return
end

print,'Selected Image File        : ', file 
print,'Total Number of 2D Scans   : ', catch2d_file.scanno_2d_last
print,'Total Number of Images     : ', catch2d_file.maxno

view_option.fullcolor = 0 ; initialize to auto scaled image

	catch2d_file.seqno = maxno - 1
	if catch2d_file.scanno_2d_last gt 0 then viewscanimage_current


END

PRO view2d_normalize_accept,pick_i
COMMON CATCH2D_FILE_BLOCK, catch2d_file
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON w_warningtext_block,w_warningtext_ids
COMMON CATCH1D_2D_COM,data_2d, gD

	view_option.pick_ref = pick_i
	view_option.fullcolor = 2

	save_seqno = catch2d_file.seqno
        scanno = catch2d_file.scanno_current
        if scanno le 0 then begin
                st = 'You have to load the scan # in first'
                w_warningtext,st, 60,3,title='VIEW2D Messages'
                return
                end
        begin_seqno = catch2d_file.image_no(scanno-1)
        end_seqno = catch2d_file.image_no(scanno)
        seqno = begin_seqno + view_option.pick_ref - 1
        if seqno lt end_seqno and seqno ge begin_seqno then begin
                catch2d_file.seqno = seqno
                if XRegistered('w_warningtext') then $
                WIDGET_CONTROL,w_warningtext_ids.base,BAD=bad,/DESTROY

		seqno = catch2d_file.seqno
		scanimage_readRecord,seqno,gD

		view_option.r_k_max = MAX(image)
		view_option.r_k_min = MIN(image)

		image_ref = image

        endif else begin
                st = [ $
                'No more image for SCAN #' + string(scanno), $
                'Total number of images for this scan is ' + $
                string(end_seqno - begin_seqno) ]
                w_warningtext,st, 60,5,title='VIEW2D Messages'
		return
        end
	REPLOT
	catch2d_file.seqno = save_seqno
	scanimage_readRecord,save_seqno,gD,/view
END

PRO view2d_normalize_Event, Event
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON VIEW2D_NORM_BLOCK, norm_ids

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'NORM_PICKED': BEGIN
	WIDGET_CONTROL,Event.id,GET_VALUE=i
	WIDGET_CONTROL,norm_ids.norm,SET_VALUE=2
	view2d_normalize_accept, i
      END
  'NORM_COLOR_SCHEME': BEGIN
	view_option.fullcolor = Event.value
	if view_option.fullcolor lt 2 then begin
	   zmin = view_option.z_min
   	   zmax = view_option.z_max
	   if view_option.fullcolor eq 1 then begin
		zmin = view_option.u_k_min
		zmax = view_option.u_k_max
	   end
           WIDGET_CONTROL,widget_ids.z_min,SET_VALUE=zmin
           WIDGET_CONTROL,widget_ids.z_max,SET_VALUE=zmax
	   REPLOT
	endif else begin
   	   WIDGET_CONTROL,norm_ids.pick,GET_VALUE=i
	   view2d_normalize_accept, i
	end
      END
  'NORM_HELP': BEGIN
	st=['Currently, there are three modes of image color scheme available:', $
	'', '    Auto_Scaled - Automatically scaled by the z-max, z-min of the image data.', $
	'', '    User_Scaled - User settable interest range of z-min, z-max.', $
	'', '    Normalized - Value normalized against the value of the selected detector.', $
	'', $
	'It defaults to the Auto Scaled scheme. The title label reflects current scheme', $
	'is used for the TV image.', '', $
	'If the User Scaled color scheme is selected, then a user can modify the Zmin and Zmax', $
	'fields of the main window.', '', $ 
	'If the detector number field is entered with a <CR>, it automatically', $
	'turns on the normalized color scheme mode. The entered # will be the reference detector.', $
	'The normalized value of the reference detector itself will be a uniform 1.', $
	'', $
	 'The z-min and z-max corresponding to different schemes are all shown in this dialog window.', $
	'' $
	 ] 
	res=WIDGET_MESSAGE(st)
	END
  'NORM_CANCEL': BEGIN
	WIDGET_CONTROL,widget_ids.norm_base,/DESTROY
	widget_ids.norm_base = 0L
      END
  ENDCASE
END

PRO view2d_normalize_setvalue
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON VIEW2D_NORM_BLOCK, norm_ids

      mode = 'Normalized Against ...'
   if view_option.fullcolor eq 0 then mode = 'Auto Scaled'
   if view_option.fullcolor eq 1 then mode = 'User Scaled'

      st1='Auto Scaled (Max  Value) :' + string(view_option.z_max)
      st2='Auto Scaled (Min  Value) :' + string(view_option.z_min)
      st3='User Scaled (Upper Bound) :' + string(view_option.u_k_max)
      st4='User Scaled (Lower Bound) :' + string(view_option.u_k_min)
      st5='Ref Detector '+strtrim(view_option.pick_ref,2) + $
		' (Max Value) :' + string(view_option.r_k_max)
      st6='Ref Detector '+strtrim(view_option.pick_ref,2) + $
		' (Min Value) :' + string(view_option.r_k_min)

	WIDGET_CONTROL,norm_ids.mode,SET_VALUE=mode
	WIDGET_CONTROL,norm_ids.lb1,SET_VALUE=st1
	WIDGET_CONTROL,norm_ids.lb2,SET_VALUE=st2
	WIDGET_CONTROL,norm_ids.lb3,SET_VALUE=st3
	WIDGET_CONTROL,norm_ids.lb4,SET_VALUE=st4
	WIDGET_CONTROL,norm_ids.lb5,SET_VALUE=st5
	WIDGET_CONTROL,norm_ids.lb6,SET_VALUE=st6
END

PRO view2d_normalize, GROUP=Group
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON VIEW2D_NORM_BLOCK, norm_ids

if XRegistered('view2d_normalize') then return

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  view2d_normalize = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
      TITLE='VIEW2D (Image Color Scheme)', $
      UVALUE='view2d_normalize')

  BASE2 = WIDGET_BASE(view2d_normalize, $
      COL=1, $
      MAP=1, $
      UVALUE='BASE2')

  LABEL3 = WIDGET_LABEL( BASE2, $
      FONT='-dt-application-bold-r-normal-sans-20-140-100-100-p-105-iso8859-1', $
      UVALUE='LABEL3', /DYNAMIC_RESIZE, $
      VALUE='Auto Scaled')

  color_schemes = [ $
	'AutoScaled', $
	'UserScaled', $
	'Normalized' $
	]
  NORM_COLOR_SCHEME = CW_BGROUP(BASE2, color_schemes, $
	ROW=1, /EXCLUSIVE, /NO_RELEASE, $
;	LABEL_LEFT='Color Scheme', $
	UVALUE='NORM_COLOR_SCHEME')

  norm_min = WIDGET_LABEL( BASE2, $
      UVALUE='NORM_MIN', /ALIGN_LEFT,$
      VALUE='Auto Scaled (Min Value) :' + string(view_option.z_min))

  norm_max = WIDGET_LABEL( BASE2, $
      UVALUE='NORM_MAX', /ALIGN_LEFT,$
      VALUE='Auto Scaled (Max  value) :' + string(view_option.z_max))

  BASE5 = WIDGET_BASE( BASE2, COL=1)   ;/FRAME)
  norm_lower = WIDGET_LABEL( BASE5, $
      UVALUE='NORM_LOWER', /ALIGN_LEFT,$
      VALUE='User Scaled (Lower Bound) :' + string(view_option.u_k_min))

  norm_upper = WIDGET_LABEL( BASE5, $
      UVALUE='NORM_UPPER', /ALIGN_LEFT,$
      VALUE='User Scaled (Upper Bound) :' + string(view_option.u_k_max))

  BASE3 = WIDGET_BASE( BASE2, COLUMN=1)  ;/FRAME)
  FieldVal1198 = [ $
    '1' ]
  norm_picked = CW_FIELD( BASE3,VALUE=FieldVal1198, $
      ROW=1, $
      INTEGER=1, /return_events, $
      TITLE='Normalize Against Detector:', XSIZE=2, $
      UVALUE='NORM_PICKED')

  norm_ref_lower = WIDGET_LABEL( BASE3, $
      UVALUE='NORM_LOWER', /ALIGN_LEFT,$
      VALUE='Reference (Min Value) :' + string(view_option.r_k_min))

  norm_ref_upper = WIDGET_LABEL( BASE3, $
      UVALUE='NORM_UPPER', /ALIGN_LEFT,$
      VALUE='Reference (Max Value)) :' + string(view_option.r_k_max))

  BASE4 = WIDGET_BASE( BASE2, ROW=1)

  NORM_HELP = WIDGET_BUTTON( BASE4, $
      UVALUE='NORM_HELP', $
      VALUE='Help')

  NORM_CANCEL = WIDGET_BUTTON( BASE4, $
      UVALUE='NORM_CANCEL', $
      VALUE='Done')

  norm_ids = { $
	mode : LABEL3, $
	lb1 : norm_max, $
	lb2 : norm_min, $
	lb3 : norm_upper, $
	lb4 : norm_lower, $
	lb5 : norm_ref_upper, $
	lb6 : norm_ref_lower, $
	norm : norm_color_scheme, $
	pick : norm_picked $
	}

  WIDGET_CONTROL, norm_ids.norm, SET_VALUE=0
  widget_ids.norm_base = view2d_normalize
  view2d_normalize_setvalue

  WIDGET_CONTROL, view2d_normalize, /REALIZE

  XMANAGER, 'view2d_normalize', view2d_normalize
END
;
; catch2d.pro
;

;@DCV2D_read.pro

PRO REPLOT
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON PRINTER_BLOCK,printer_info
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

if catch2d_file.scanno_current lt 0 then return

; update the info block

xtitle='2D SCAN # '+strtrim(catch2d_file.scanno_current,2)+ $
;	',  D'+strtrim(catch2d_file.detector,2)+ $
	', '+catch2d_file.x_desc +  $
	', FILE:'+catch2d_file.name + $ 
	',  USER:'+strupcase(getenv('USER')) 

str = ['Selected Image File        : '+ catch2d_file.name] 
str = [str,'Total Number of 2D Scans   : '+ string(catch2d_file.scanno_2d_last)]
str = [str,'Total Number of Images     : '+ string(catch2d_file.maxno)]
	str = [ str,'2D SCAN # ='+strtrim(catch2d_file.scanno_current)+ $
		',   DETECTOR='+strtrim(catch2d_file.detector,2) + $
		',   IMAGE # ='+strtrim(catch2d_file.seqno+1,2) + $
	', ('+catch2d_file.x_desc +', '+ catch2d_file.y_desc+', '+ $
		catch2d_file.z_desc + ')']
	str = [str, '1D scan #=(0-'$
		+ strtrim(catch2d_file.height-1,2)+ ')'+$
		',   width='+strtrim(catch2d_file.width,2)+ $
		',   height='+strtrim(catch2d_file.height,2) ] 
	str = [str, 'x_pv = '+catch2d_file.x_pv+',   y_pv = '+catch2d_file.y_pv]
	str = [str, 'catch1d filename = '+ catch2d_file.file_1d]
	WIDGET_CONTROL, widget_ids.info, SET_VALUE= str
	WIDGET_CONTROL, widget_ids.sel_image, SET_VALUE= catch2d_file.detector-1
	str = strtrim(view_option.z_min,2) + ' @ (' + $
		strtrim(view_option.i_min,2) + ',' + $
		strtrim(view_option.j_min,2) + ')'
	WIDGET_CONTROL, widget_ids.zmin, SET_VALUE= str
	str = strtrim(view_option.z_max,2) + ' @ (' + $
		strtrim(view_option.i_max,2) + ',' + $
		strtrim(view_option.j_max,2) + ')'
	WIDGET_CONTROL, widget_ids.zmax, SET_VALUE= str

if view_option.fullcolor eq 0 then begin
	WIDGET_CONTROL,widget_ids.z_min,SET_VALUE=strtrim(view_option.z_min,2)
	WIDGET_CONTROL,widget_ids.z_max,SET_VALUE=strtrim(view_option.z_max,2)
end
if view_option.fullcolor eq 1 then begin
	WIDGET_CONTROL,widget_ids.z_min,SET_VALUE=strtrim(view_option.u_k_min,2)
	WIDGET_CONTROL,widget_ids.z_max,SET_VALUE=strtrim(view_option.u_k_max,2)
end

; check for user range
		xdim = catch2d_file.width
		ydim = catch2d_file.height
		catch2d_file.x_act_npts = xdim
		catch2d_file.y_act_npts = ydim
		x_min=0
		x_max=xdim-1
		y_min=0
		y_max=ydim-1

	if view_option.fullcolor eq 2 then begin
	 	image = image/ image_ref	
	end
		

	if view_option.user eq 1 then begin
		if view_option.x_min gt x_min and view_option.x_min lt x_max then x_min = view_option.x_min
		if view_option.x_max lt x_max and view_option.x_max gt x_min then x_max = view_option.x_max
		if view_option.y_min gt y_min and view_option.y_min lt y_max then y_min = view_option.y_min
		if view_option.y_max lt y_max and view_option.y_max gt y_min then y_max = view_option.y_max

		newimage = image(x_min:x_max,y_min:y_max)
	endif else begin
		newimage = image
		end
;      
; set plot area for 2D view
;


if !d.name eq OS_SYSTEM.device then WSET,widget_ids.plot2d_area

		x = catch2d_file.xarr(0:catch2d_file.width - 1)
		y = catch2d_file.yarr(0:catch2d_file.height - 1)
		ix = n_elements(x)
		iy = n_elements(y)

;  draw 2D data as data image
   if view_option.user eq 0 then begin
	erase 
	; expand data to drawing area
		catch2d_file.x_mag = 1
		catch2d_file.y_mag = 1
		newimage2 = newimage

	v_max = max(newimage2)
	v_min = min(newimage2)
	ncolors = view_option.ncolors

	
	if v_max eq v_min then begin       ;(all same value)
;		dv = v_max - v_min
;		if dv eq 0 then fact = ncolors  else fact = ncolors / dv
		fact=ncolors
		TV,newimage2*fact
	endif else begin
		newimage2 = bytscl(newimage2,top=ncolors,min=v_min,max=v_max)
		TV,newimage2
	end
	return
   end

	CASE view_option.surface OF
	2: begin
		if view_option.versus then begin
			if x_max lt ix then ix=x_max
			if y_max lt iy then iy=y_max 
			newim = image(x_min:ix,y_min:iy)
			nx=x(x_min:ix)
			ny=y(y_min:iy)
			SHADE_SURF, newim,nx,ny  
		endif else SHADE_SURF, newimage
	   end
	3: begin
			if x_max lt ix then ix=x_max
			if y_max lt iy then iy=y_max 
			newim = image(x_min:ix-1,y_min:iy-1)
			nc = view_option.ncolors
			labels=[1,1,1,1,1,1,1,1,1,1,1]
;			colors = [31,28,25,22,19,16,13,10]
			zmax = max(newim)
			zmin = min(newim)
			dz= (zmax-zmin)/ 9.
			dc = nc / 10 
			colors = nc 
			levels = zmin
			for i=1,9 do begin
			levels = [levels, zmin + dz*i]
			colors = [colors, nc - dc*i ]
			end
			if !d.n_colors eq 16777216 then begin
				catch1d_get_pvtcolor,colors(0),lcolor
				for i=1,9 do begin
				catch1d_get_pvtcolor,colors(i),tcolor
				lcolor =[lcolor,tcolor]
				end
			colors = lcolor
			end

		if view_option.versus then begin       ; versus values
			nx=x(x_min:ix-1)
			ny=y(y_min:iy-1)
		endif else begin       			; versus step # 
			temp = indgen(ix)
			nx=temp(x_min:ix-1)
			temp = indgen(iy)
			ny=temp(y_min:iy-1)
		end
		CONTOUR, newim,nx,ny, $
			levels = levels, $
			c_colors=reverse(colors), c_labels=labels, $
			 c_charsize=1.5,/follow
	   end
	4: begin
		if view_option.versus then begin
			if x_max lt ix then ix=x_max
			if y_max lt iy then iy=y_max 
			newim = image(x_min:ix-1,y_min:iy-1)
			nx=x(x_min:ix-1)
			ny=y(y_min:iy-1)
			SHOW3, newim 
		endif else SHOW3, newimage
;		SHOW3, newimage, sscale=2
	   end
	5: begin
		XSURFACE, newimage, id
		if n_elements(id) eq 0 then begin
		w_warningtext,['Error: First close the old XSurface window',$
			'       then select TV before select new image and XSURFACE'], $
			60,5,title='VW2D Messages'
		return
		endif else $
		widget_ids.xsurface = id
	   end
	0: begin
	   erase
		; expand data to drawing area
		catch2d_file.x_mag = 1
		catch2d_file.y_mag = 1

	 	    xratio = 1.
		    yratio = float(y_max-y_min+1)/(x_max-x_min+1) 
		    if yratio gt 1. then begin
			xratio = 1. / yratio
			yratio = 1.
		    end 
		width = (x_max - x_min + 1) / xratio
		height = (y_max - y_min + 1)/ yratio

		if view_option.user eq 1 and !d.name eq OS_SYSTEM.device then begin
		width = !d.x_size - view_option.margin_l - view_option.margin_r
		height = !d.y_size - view_option.margin_t - view_option.margin_b
		catch2d_file.x_mag = float(width)/(x_max-x_min +1)
		catch2d_file.y_mag = float(height)/(y_max-y_min +1)
		end
; help,image,newimage,width,height
		newimage2 = CONGRID(newimage,width,height)
		ncolors = view_option.ncolors

	if view_option.fullcolor eq 0 then begin
		v_max = max(newimage2)
		v_min = min(newimage2)
	end
	if view_option.fullcolor eq 1 then begin
		v_max = view_option.u_k_max
		v_min = view_option.u_k_min
	end

	if view_option.fullcolor eq 2 then begin

		v_max = max(newimage2)
		v_min = min(newimage2)
	WIDGET_CONTROL,widget_ids.z_min,SET_VALUE=strtrim(v_min,2)
	WIDGET_CONTROL,widget_ids.z_max,SET_VALUE=strtrim(v_max,2)
	end

;		if !d.name ne OS_SYSTEM.device then ncolors = ncolors - 1

		if v_max eq v_min then begin       ;(all same value)
			dv = v_max - v_min
			if dv eq 0 then fact = ncolors  else fact = ncolors / dv
			newimage2 = newimage2*fact
		endif else begin
		if view_option.fullcolor lt 2 then $
		newimage2 = bytscl(newimage2,top=ncolors,min=v_min,max=v_max) 
		if view_option.fullcolor eq 2 then $
		newimage2 = bytscl(newimage2,top=ncolors,min=v_min,max=v_max)
;		newimage2 = bytscl(newimage2,top=fact,min=v_min,max=v_max)
		end

		xrange = [x_min,x_max]
		yrange = [y_min,y_max]
		title = 'vs X,Y Step #'
		if view_option.versus then begin
		xrange = [ catch2d_file.xarr(x_min), catch2d_file.xarr(x_max)]
		yrange = [ catch2d_file.yarr(y_min), catch2d_file.yarr(y_max)]
		title = 'vs X,Y Values'


		; for PS  get aspect ratio, outward tick marks 

	; draw headers

        header_note1='MAX: ' + strtrim(view_option.z_max,2) + ' @ ('+ $
		strtrim(catch2d_file.xarr(view_option.i_max),2) + ', ' + $
		strtrim(catch2d_file.yarr(view_option.j_max),2) + ')' 

        header_note='MIN: ' + strtrim(view_option.z_min,2) + ' @ ('+ $
		strtrim(catch2d_file.xarr(view_option.i_min),2) + ', ' + $
		strtrim(catch2d_file.yarr(view_option.j_min),2) + ')' 

	if !d.name ne 'PS' then begin
          xdis = 0.01 * !d.x_size
          ydis = !d.y_size - 1.2 * !d.y_ch_size
          xyouts,xdis,ydis,header_note1,/device
          ydis = !d.y_size - 2.2 * !d.y_ch_size
          xyouts,xdis,ydis,header_note,/device
	endif else begin
	  if printer_info.reverse then t_color = ncolors-1 else t_color = 0
          xdis = 0.001 * !d.x_size
          ydis = !d.y_size - 1.2 * !d.y_ch_size
          xyouts,xdis,ydis,header_note1,/device, color=t_color
          ydis = !d.y_size - 2.2 * !d.y_ch_size
          xyouts,xdis,ydis,header_note,/device, color=t_color
	end

	ytitle = catch2d_file.y_desc
	if strtrim(catch2d_file.z_desc,2) ne '' then $
	title = catch2d_file.z_desc + ' - ' + title else $
	title = 'D'+strtrim(catch2d_file.detector,2) + ' - '+title

		if !d.name eq 'PS' then begin
		    xo = !d.x_size * view_option.ps_l
		    yo = !d.y_size * view_option.ps_b
		    xw = !d.x_size * (view_option.ps_r - view_option.ps_l)
		    yw = !d.y_size * (view_option.ps_t - view_option.ps_b)

		    pos = [view_option.ps_l, view_option.ps_b, $
			view_option.ps_r, view_option.ps_t]

		    TV,newimage2,xo,yo,xsize=xw,ysize=yw

		    plot,/noerase,/nodata, pos=pos, [-1,-1], $
			xrange=xrange, yrange=yrange, $
			xticklen= -!p.ticklen, yticklen=-!p.ticklen, $
			title=title, xtitle=xtitle, $
			ytitle=ytitle, $
			xstyle = 1, ystyle=1 ,color=t_color

		endif else begin

		    TV,newimage2, view_option.margin_l, view_option.margin_b

		    p1 = [float(view_option.margin_l)/ !d.x_size, $
			float(view_option.margin_b)/!d.y_size, $
			float(!d.x_size - view_option.margin_r) / !d.x_size, $
			float(!d.y_size - view_option.margin_t) / !d.y_size $
			]

		    plot,/noerase,/nodata, pos=p1 ,[-1,-1], $
			xrange=xrange, yrange=yrange, $
			xtitle= catch2d_file.x_desc, $
			ytitle=ytitle, $
			title=title, xstyle = 1, ystyle=1

			end
		end


                ; save pixmap
                if !d.name ne OS_SYSTEM.device then return
                view_option.d_wid = !d.window
                if view_option.s_wid ge 0 then begin
                        wid = view_option.s_wid
                        update_pixmap,wid
                endif else begin
                        create_pixmap,wid
                        view_option.s_wid = wid
                end

	if widget_ids.norm_base ne 0 then view2d_normalize_setvalue

	   end
	1: begin
	   erase
		; equal aspect ratio  
		catch2d_file.x_mag = 1
		catch2d_file.y_mag = 1
		width = x_max - x_min + 1
		height = y_max - y_min + 1

	 	    xratio = 1.
		    yratio = float(y_max-y_min+1)/(x_max-x_min+1) 
		    if yratio gt 1. then begin
			xratio = 1. / yratio
			yratio = 1.
		    end 

		if view_option.user eq 1 and !d.name eq OS_SYSTEM.device then begin
		width = !d.x_size - view_option.margin_l - view_option.margin_r
		height = !d.y_size - view_option.margin_t - view_option.margin_b
		catch2d_file.x_mag = floor(width/(x_max-x_min + 1))
		catch2d_file.y_mag = floor(height/(y_max-y_min + 1))
		width = width * xratio
		height = height * yratio
		end

		newimage2 = CONGRID(newimage,width,height)

		v_max = max(newimage2)
		v_min = min(newimage2)
		ncolors = view_option.ncolors

		if v_max eq v_min then begin       ;(all same value)
			dv = v_max - v_min
			if dv eq 0 then fact = ncolors  else fact = ncolors / dv
			newimage2 = newimage2*fact
		endif else begin
		newimage2 = bytscl(newimage2,top=ncolors,min=v_min,max=v_max)
		end

		xrange = [x_min,x_max]
		yrange = [y_min,y_max]
		title = 'vs X,Y Step #'
		if view_option.versus then begin
		xrange = [ catch2d_file.xarr(x_min), catch2d_file.xarr(x_max)]
		yrange = [ catch2d_file.yarr(y_min), catch2d_file.yarr(y_max)]
		title = 'vs X,Y Values'


		; for PS  get aspect ratio, outward tick marks 

	; draw headers

        header_note1='MAX: ' + strtrim(view_option.z_max,2) + ' @ ('+ $
		strtrim(catch2d_file.xarr(view_option.i_max),2) + ', ' + $
		strtrim(catch2d_file.yarr(view_option.j_max),2) + ')' 

        header_note='MIN: ' + strtrim(view_option.z_min,2) + ' @ ('+ $
		strtrim(catch2d_file.xarr(view_option.i_min),2) + ', ' + $
		strtrim(catch2d_file.yarr(view_option.j_min),2) + ')' 

	if !d.name ne 'PS' then begin
          xdis = 0.01 * !d.x_size
          ydis = !d.y_size - 1.2 * !d.y_ch_size
          xyouts,xdis,ydis,header_note1,/device
          ydis = !d.y_size - 2.2 * !d.y_ch_size
          xyouts,xdis,ydis,header_note,/device
	endif else begin
	  if printer_info.reverse then t_color = ncolors-1 else t_color = 0
          xdis = 0.001 * !d.x_size
          ydis = !d.y_size - 1.2 * !d.y_ch_size
          xyouts,xdis,ydis,header_note1,/device,color=t_color
          ydis = !d.y_size - 2.2 * !d.y_ch_size
          xyouts,xdis,ydis,header_note,/device,color=t_color
	end

	ytitle = catch2d_file.y_desc
	if strtrim(catch2d_file.z_desc,2) ne '' then $
	title = catch2d_file.z_desc + ' - ' + title else $
	title = 'D'+strtrim(catch2d_file.detector,2) + ' - '+title

		if !d.name eq 'PS' then begin
		    xo = !d.x_size * view_option.ps_l
		    yo = !d.y_size * view_option.ps_b
		    xw = !d.x_size * xratio *(view_option.ps_r - view_option.ps_l)
		    yw = !d.y_size * yratio *(view_option.ps_t - view_option.ps_b)

		    TV,newimage2,xo,yo,xsize=xw,ysize=yw

		    pos = [view_option.ps_l, view_option.ps_b, $
			xw / !d.x_size + view_option.ps_l,  $
			yw / !d.y_size + view_option.ps_b ]

		    plot,/noerase,/nodata, pos=pos, [-1,-1], $
			xrange=xrange, yrange=yrange, $
			xticklen= -!p.ticklen, yticklen=-!p.ticklen, $
			title=title, xtitle=xtitle, ytitle=ytitle, $
			xstyle = 1, ystyle=1, color=t_color

		endif else begin
		    TV,newimage2, view_option.margin_l, view_option.margin_b

		    p1 = [float(view_option.margin_l)/ !d.x_size, $
			float(view_option.margin_b)/!d.y_size, $
			float(!d.x_size - view_option.margin_r) / !d.x_size, $
			(float(view_option.margin_b) + height)/!d.y_size $
			]

		    plot,/noerase,/nodata, pos=p1 ,[-1,-1], $
			xrange=xrange, yrange=yrange,title=title, $
			xtitle=xtitle, ytitle=ytitle, $
			xstyle = 1, ystyle=1
			end
		end

	   end
	ELSE: print,'Unknow case entered'
	ENDCASE

END

PRO scanimage_read_record,unit, view=view 
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file

	IF EOF(unit) THEN RETURN

	if (widget_ids.xsurface) then begin
		WIDGET_CONTROL,widget_ids.xsurface,/DESTROY,BAD=bad
		widget_ids.xsurface = 0L
		end

	; read pv names
	u_read,unit,pvs
	pvs = string(pvs)
	catch2d_file.x_pv = pvs(0)
	catch2d_file.y_pv = pvs(1) 
	catch2d_file.file_1d = pvs(2) 
	if n_elements(pvs) gt 3 then begin
	catch2d_file.x_desc = pvs(3)
	catch2d_file.y_desc = pvs(4) 
	catch2d_file.z_desc = pvs(5) 
	end
;	print,string(pvs)
	

	; read seqno, dims

	u_read,unit,x
;	print,x
	scanno_2d = x(4)

;	width = x(1)
;	height= x(2)
	catch2d_file.scanno = x(0)
	catch2d_file.width = x(1)
	catch2d_file.height = x(2)
	catch2d_file.detector = x(3) + 1
	catch2d_file.scanno_current = x(4) 
	if x(4) eq 0 then catch2d_file.scanno_current = 1    ; fix 0 2d # 
	catch2d_file.y_req_npts = x(5)

	; read x and y position array

	u_read,unit,x
	catch2d_file.xarr = x
	u_read,unit,x
	catch2d_file.yarr = x

	; read image

	u_read,unit,image
	catch2d_file.image = image

;	newImage = image(0:catch2d_file.width-1, 0:catch2d_file.height-1)
	newImage = image
	s = size(newImage)

	; find the max only for 2D or 1D

	if s(0) eq 2 then totalno = s(4) - 1
	if s(0) eq 1 then totalno = s(3) - 1
        view_option.z_max = newImage(0)
	view_option.i_max = 0
	view_option.j_max = 0
	view_option.z_max = MAX(newImage,imax)
	view_option.j_max = imax / s(1)
	view_option.i_max = imax mod s(1) 
	view_option.z_min = MIN(newImage,imax)
	view_option.j_min = imax / s(1)
	view_option.i_min = imax mod s(1) 
	if view_option.fullcolor eq 0 then begin
		view_option.k_max = view_option.z_max 
		view_option.k_min = view_option.z_min 
	end

	if s(0) ne 2 then begin
		w_warningtext,'Warning: data is not 2D image ',60,5,'VW2D Messaes'
		end 

	if keyword_set(view) then REPLOT
END

PRO viewscanimage_current
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH1D_2D_COM, data_2d, gD

	seqno = catch2d_file.seqno
	scanimage_readRecord,seqno,gD,/view

END


PRO dc2aim,infile
common com_file,jfile
savefile = 'dc2aim.sav'
if n_elements(infile) then savefile=infile
found = findfile(savefile)
if found(0) eq '' then begin
        res = widget_message(savefile + ' not found!',/Error)
        return
end
restore,file=savefile
jfile.nxin=ncol
jfile.nyin=nrow
retall
END

PRO PDMENU189_Event, Event
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file

  CASE Event.Value OF 

  'File.Open ...': BEGIN
    PRINT, 'Event for File.Open ...'

        FNAME = '*scan*'
        F = PICKFILE(/READ,FILE='',GET_PATH=P,PATH=catch2d_file.path,FILTER=FNAME)
        IF F eq '' THEN begin
                F = ''
		return
                end
        found=findfile(F)

        catch2d_file.name = F

        if found(0) ne '' then viewscanimage_init, F else begin
		w_warningtext,'Error:  file not found - '+ F, 60,5, $
			title='VW2D Messages'
		return
		end
                
	catch2d_file.path = P
	pos = rstrpos(F,OS_SYSTEM.file_sep) ;'/'
	if pos gt 0 then catch2d_file.name = strmid(F,pos+1,strlen(F))

    END

  'File.Save Image for AIM': BEGIN
        ncol = catch2d_file.width
        nrow = catch2d_file.height
        xarr = catch2d_file.xarr(0:ncol-1)
        yarr = catch2d_file.yarr(0:nrow-1)
        imarr = image
        save,filename='dc2aim.sav',/XDR,ncol,nrow,xarr,yarr,imarr
    END

  'File.Printer ...': BEGIN
    PS_printer,GROUP=Event.Top
    END

  'File.Print': BEGIN
    PS_open,'view2d.ps',/TV
    REPLOT
    PS_close
    PS_print,'view2d.ps'
    END

  'File.PS_close': BEGIN
    PS_close
    END

  'File.Quit': BEGIN
    PRINT, 'Event for File.Quit'
    WIDGET_CONTROL, event.top, /DESTROY
    if widget_ids.xsurface then begin 
	WIDGET_CONTROL, widget_ids.xsurface,BAD=bad, /DESTROY
	widget_ids.xsurface = 0L
	end
;    WDELETE,catch2d_file.xprof,catch2d_file.yprof
    if catch2d_file.opened ne 0 then free_lun,catch2d_file.opened
    catch2d_file.opened=0
;    LOADCT,39
    END
  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END PDMENU189
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.


; CODE MODIFICATIONS MADE ABOVE THIS COMMENT WILL BE LOST.
; DO NOT REMOVE THIS COMMENT: BEGIN PDMENU188




PRO PDMENU188_Event, Event
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref

  CASE Event.Value OF 

  'Color.Save Private Color Table': BEGIN
	TVLCT,red,green,blue,/Get
	Save,red,green,blue,file='pvtcolors.dat'
    END
  'Color.Load Private Color Table': BEGIN
	found = findfile('pvtcolors.dat')
	if found(0) eq ''  then begin
		st = ['Error: Private color table never been saved before. ', $
	      '       You have to save the private color first, before', $
	      '       you can load it into the view2d program.'] 
		w_warningtext,st,60,5,title='VW2D Messages'
	endif else begin
		restore,'pvtcolors.dat'
		TVLCT,red,green,blue
	end
    END

  'Color.Change Color Table ...': BEGIN
    PRINT, 'Event for Color.Change Color Table'
    XLOADCT
    END
  'Color.Image Color Scheme ...': BEGIN
	view2d_normalize,GROUP=Event.top
	END
  ENDCASE
END


PRO PDMENU189_help_Event, Event

  if getenv('EPICS_EXTENSIONS') eq '' then begin
	res=WIDGET_MESSAGE('EPICS_EXTTENSIONS not defined.')
	return
	end
  CASE Event.Value OF 

  'Help.Help ...': BEGIN
    str = getenv('EPICS_EXTENSIONS')
    new = getenv('EPICS_EXTENSIONS_PVT')
    if strlen(new) gt 3 then str = new 
	str = str+'/doc/vw2d_help.txt'
    xdisplayfile, str, GROUP=Event.top 
    END
  ENDCASE
END

PRO PDMENU2D_FITTING_event, Event
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON w_viewscanimage_block, w_viewscanimage_ids
COMMON w_warningtext_block,w_warningtext_ids

if n_elements(image) eq 0  then begin
	w_warningtext,['No image data found','','You have to load the 2D data first']
	return
end
x = catch2d_file.xarr(0:catch2d_file.width-1)
y = catch2d_file.yarr(0:catch2d_file.height-1)
im = make_array(catch2d_file.width, catch2d_file.height)
im(*,*) = image

  CASE Event.Value OF

  'Fitting.Ez_Fit ...': BEGIN
        ez_fit,xarray=x,yarray=y,im=im,GROUP=Event.Top
        END
  'Fitting.2D Binary': BEGIN
        u_openw,unit,'fitting.bin',/XDR
        u_write,unit,x
        u_write,unit,y
	u_write,unit,image
        u_close,unit
        st = '2D binary data save in "fitting.bin"'
        w_warningtext,st
        END
  ENDCASE
END


PRO main13_2_Event, Event

COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON w_viewscanimage_block, w_viewscanimage_ids
COMMON w_warningtext_block,w_warningtext_ids


  ; The next CASE statement is from the Widget Builder.
  ; It uses the User_value of a widget to identify itself.



  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  ; Event for FILE_MENU
  'PDMENU189': PDMENU189_Event, Event

  ; Event for Color_MENU
  'PDMENU188': PDMENU188_Event, Event

  'PDMENU189_help': PDMENU189_help_Event, Event
  'PDMENU2D_FITTING': PDMENU2D_FITTING_Event, Event

  'SURFACE_PLOT': BEGIN
	view_option.surface = Event.Index
	REPLOT
	END
  'PLOTVERSUS': BEGIN
	view_option.versus = Event.Index
	REPLOT
	END
  'BGROUP184': BEGIN
	view_option.user = Event.Index
	CASE view_option.user OF
	0: begin
		WIDGET_CONTROL,widget_ids.x_min,SET_VALUE=0
		WIDGET_CONTROL,widget_ids.x_max,SET_VALUE=view_option.width
		WIDGET_CONTROL,widget_ids.y_min,SET_VALUE=0
		WIDGET_CONTROL,widget_ids.y_max,SET_VALUE=view_option.height
	end
	1: begin
		WIDGET_CONTROL,widget_ids.x_min,SET_VALUE=view_option.x_min
		WIDGET_CONTROL,widget_ids.x_max,SET_VALUE=view_option.x_max
		WIDGET_CONTROL,widget_ids.y_min,SET_VALUE=view_option.y_min
		WIDGET_CONTROL,widget_ids.y_max,SET_VALUE=view_option.y_max
	end
	ELSE:
	ENDCASE
	REPLOT
      END
; Event for PDMENU226
;  'PDMENU226': PDMENU226_Event, Event

  'DRAW62': BEGIN

	view = view_option.surface

      IF ((Event.PRESS EQ 1) AND (view EQ 0)) THEN BEGIN
	catch2d_xycoord_TV, x, y, Event
	END

      IF ((Event.PRESS EQ 2) AND (view EQ 0)) THEN BEGIN
	catch2d_xydist2, Event
	return
	END

      IF ((Event.PRESS EQ 4) AND (view EQ 0)) THEN BEGIN
	hide_cross,view_option.x,view_option.y,view_option.d_wid,view_option.s_wid
	nx = 50 
	ny = 50 
	x0 = Event.x - 25
	y0 = Event.y - 25
	
	my_box_cursor, x0, y0, nx,ny, /INIT

	if view_option.user eq 1 then begin
		x_min = (x0-view_option.margin_b) / $
			catch2d_file.x_mag + view_option.x_min
		x_max = (x0+nx-view_option.margin_b) / $
			catch2d_file.x_mag + view_option.x_min
		y_min = (y0-view_option.margin_l) / $
			catch2d_file.y_mag + view_option.y_min
		y_max = (y0+ny-view_option.margin_l) / $
			catch2d_file.y_mag + view_option.y_min

	WIDGET_CONTROL,widget_ids.plot_wid,/CLEAR_EVENTS
		WIDGET_CONTROL,widget_ids.x_min, $
			SET_VALUE = strtrim( fix(x_min),2)
		WIDGET_CONTROL,widget_ids.x_max, $
			SET_VALUE = strtrim( ceil(x_max),2)
		WIDGET_CONTROL,widget_ids.y_min, $
			SET_VALUE = strtrim( fix(y_min),2)
		WIDGET_CONTROL,widget_ids.y_max, $
			SET_VALUE = strtrim( ceil(y_max),2)

		view_option.x_min = fix(x_min)
		view_option.y_min = fix(y_min)
		view_option.x_max = ceil(x_max)
		view_option.y_max = ceil(y_max)
		REPLOT
	end
	return
	END
      END

  'REFRESH_DATA': BEGIN
        WIDGET_CONTROL, widget_ids.x_min, SET_VALUE = 0 
        WIDGET_CONTROL, widget_ids.x_max, SET_VALUE = view_option.width
        WIDGET_CONTROL, widget_ids.y_min, SET_VALUE = 0
        WIDGET_CONTROL, widget_ids.y_max, SET_VALUE = view_option.height
		view_option.x_min = 0
		view_option.y_min = 0
		view_option.x_max = view_option.width
		view_option.y_max = view_option.height
	REPLOT
	END
  'FIELD246': BEGIN
      Print, 'Event for Ymax entry'
         WIDGET_CONTROL, widget_ids.y_max, GET_VALUE = y_max  
	if fix(y_max) gt view_option.y_min then begin
	view_option.y_max = fix(y_max)
        if view_option.user eq 1 then REPLOT
	end
      END
  'FIELD157': BEGIN
      Print, 'Event for Ymin entry'
         WIDGET_CONTROL, widget_ids.y_min, GET_VALUE = y_min
	if fix(y_min) lt view_option.y_max then begin
	view_option.y_min = fix(y_min)
        if view_option.user eq 1 then REPLOT
	end
      END
  'FIELD159': BEGIN
      Print, 'Event for Xmax entry'
         WIDGET_CONTROL, widget_ids.x_max, GET_VALUE = x_max
	if fix(x_max) gt view_option.x_min then begin
	view_option.x_max = fix(x_max)
        if view_option.user eq 1 then REPLOT
	end
      END
  'FIELD161': BEGIN
      Print, 'Event for Xmin entry'
         WIDGET_CONTROL, widget_ids.x_min, GET_VALUE = x_min
	if fix(x_min) lt view_option.x_max then begin
	view_option.x_min = fix(x_min)
        if view_option.user eq 1 then REPLOT
	end
      END
  'VIEW2D_ZMAX': BEGIN
         WIDGET_CONTROL, widget_ids.z_max, GET_VALUE = z_max
	view_option.u_k_max = z_max
	view_option.k_max = z_max
        if view_option.user eq 1 then REPLOT
      END
  'VIEW2D_ZMIN': BEGIN
         WIDGET_CONTROL, widget_ids.z_min, GET_VALUE = z_min
	view_option.u_k_min = z_min
	view_option.k_min = z_min
        if view_option.user eq 1 then REPLOT
      END
  'IMAGE_PAN': BEGIN
	view2d_pan_images_on
      END
  'IMAGE186': BEGIN
	scanno = catch2d_file.scanno_current
	if scanno le 0 then begin
		st = 'You have to load the scan # in first'
		w_warningtext,st, 60,3,title='VW2D Messages'
		return
		end
	begin_seqno = catch2d_file.image_no(scanno-1)	
	end_seqno = catch2d_file.image_no(scanno)	
	seqno = begin_seqno + event.value

	if seqno lt end_seqno then begin
		catch2d_file.seqno = seqno
		if XRegistered('w_warningtext') then $
		WIDGET_CONTROL,w_warningtext_ids.base,BAD=bad,/DESTROY
		viewscanimage_current
	endif else begin
		st = [ $
		'No more image for SCAN #' + string(scanno), $
		'Total number of images for this scan is ' + $
		string(end_seqno - begin_seqno) ]
		w_warningtext,st, 60,5,title='VW2D Messages'
	end
	END
  'CURSOR62_X': BEGIN
	WIDGET_CONTROL,Event.id,GET_VALUE=x
	WIDGET_CONTROL,widget_ids.y_cursor,GET_VALUE=y
	catch2d_ydist,fix(x(0))	, Event
	catch2d_zcursor,x(0),y(0)
	END
  'CURSOR62_Y': BEGIN
	WIDGET_CONTROL,Event.id,GET_VALUE=y
	WIDGET_CONTROL,widget_ids.x_cursor,GET_VALUE=x
	catch2d_xdist,fix(y(0))	, Event
	catch2d_zcursor,x(0),y(0)
	END
  'CURSOR62_XZ': BEGIN
	if catch2d_file.xzdraw eq 0 then begin
        st = ['Click MMB in the 2D image area first!','Before press the XZ button.']
	w_warningtext, st,60,5,title='VW2D Messages',xloc=500
        return
        end

	w_warningtext,['Query X,Z value : Left Mouse Button', $
                       '     Stop Query : Other Buttons'],60,5, $
			title='VW2D Messages',xloc=500
	catch2d_xycoord1,st
	END
  'CURSOR62_YZ': BEGIN
	if catch2d_file.yzdraw eq 0 then begin
        st = ['Click MMB in the 2D image area first!','Before press the YZ button.']
	w_warningtext, st,60,5,title='VW2D Messages',xloc=500
        return
        end

	w_warningtext,['Query Y,Z value : Left Mouse Button', $
                       '     Stop Query : Other Buttons'],60,5, $
			title='VW2D Messages',xloc=500
	catch2d_xycoord2,st
	END
  'ASCII_DATA': BEGIN
	view2d_datatotext
	END
  'TEXT133': BEGIN
      Print, 'Event for Information Block'
      END
  ELSE:     ;don't stop of no matches
  ENDCASE
END

PRO catch2d_zcursor,x,y
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref
COMMON CATCH2D_FILE_BLOCK,catch2d_file

z=''
if x gt 0 and x lt catch2d_file.x_act_npts and y gt 0 and y lt catch2d_file.y_act_npts then z = string(catch2d_file.image(x,y))
WIDGET_CONTROL,widget_ids.z_cursor,SET_VALUE=strtrim(z,2)
END

; DO NOT REMOVE THIS COMMENT: END main13_2
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO VW2D, GROUP=Group, file=file
;
;+
; NAME:
;       VW2D	
;
; PURPOSE:
;       This program provides the EPICS user a convenient IDL 2D scan data 
;       display tool.  Its input image file is automatically saved by the  
;       data catcher program CATCHER_V1. 
;
;       Currently, this program provides TV, SURFACE, CONTOUR, SHOW3, and 
;       XSURFACE plot. It also provides simple xz, yz line plot and data
;       value query information.
;
; CATEGORY:
;	Widgets. 
;
; CALLING SEQUENCE:
;	VW2D
;
; INPUTS:
;       None.	
;
; KEYWORD PARAMETERS:
;     GROUP:    The widget ID of the group leader of the widget.  If this 
;               keyword is specified, the death of the group leader results in
;               the death of VW2D.
;
;     FILE:    The input image file name.  If this keyword is specified, the
;              file should contain the image data must be in the data catcher
;              created format. 
;
; OUTPUTS:
;       It provides option of postscript plot of drawing area.
;
; COMMON BLOCKS:
;       None.
;
; RESTRICTIONS:
;	Drawing area is 460 x 400 pixels.
;
; PROCEDURE:
;       This program is available as an epics/extensions tool. It can be
;       directly accessed from the view data menu of the scanSee - DC.  
; 
; EXAMPLE:
;       VW2D
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 02-27-96.
;       10-19-98 bkc   R3.13.1 new XDR save format
;       12-04-98 bkc   R1.2
;                      Fix the 2D image width problem due to aborted 2D scan
;-
;
@os.init

if XRegistered('main13_2') ne 0  then return

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  version = 'VW2D (R1.2)'

  main13_2 = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $; SCR_XSIZE=750, SCR_YSIZE=820, /SCROLL, $
      MAP=1, $
      TITLE= version, $
      UVALUE='main13_2')

  BASE68 = WIDGET_BASE(main13_2, $
      COLUMN=2, $
      MAP=1, $
      TITLE='Top Menu Line', $
      UVALUE='BASE68')

  BASE190 = WIDGET_BASE(BASE68, $
      COLUMN=3, $
;      FRAME=2, $
      MAP=1, $
      TITLE='menu base', $
      UVALUE='BASE190')

  MenuDesc907 = [ $
      { CW_PDMENU_S,       3, 'File' }, $ ;        0
        { CW_PDMENU_S,       0, 'Open ...' }, $ ;        1
;        { CW_PDMENU_S,       0, 'Save as ...' }, $ ;        2
        { CW_PDMENU_S,       0, 'Save Image for AIM' }, $ ;        2
        { CW_PDMENU_S,       0, 'Printer ...' }, $ ;        2
        { CW_PDMENU_S,       0, 'Print' }, $ ;        2
        { CW_PDMENU_S,       0, 'PS_close' }, $ ;        2
        { CW_PDMENU_S,       2, 'Quit' } $  ;      6
  ]


  PDMENU189 = CW_PDMENU( BASE190, MenuDesc907, /RETURN_FULL_NAME, $
      UVALUE='PDMENU189')

  MenuDesc909 = [ $
      { CW_PDMENU_S,       3, 'Color' }, $ ;        0
        { CW_PDMENU_S,       0, 'Save Private Color Table' }, $  ;      1
        { CW_PDMENU_S,       0, 'Load Private Color Table' }, $  ;      1
        { CW_PDMENU_S,       0, 'Change Color Table ...' }, $  ;      1
        { CW_PDMENU_S,       0, 'Image Color Scheme ...' } $  ;      1
  ]

  PDMENU188 = CW_PDMENU( BASE190, MenuDesc909, /RETURN_FULL_NAME, $
      UVALUE='PDMENU188')

  MenuDesc911 = [ $
      { CW_PDMENU_S,       3, 'Help' }, $ ;        0
        { CW_PDMENU_S,       2, 'Help ...' } $ ;        1
	]
  PDMENU189_help = CW_PDMENU( BASE190, MenuDesc911, /RETURN_FULL_NAME, $
      UVALUE='PDMENU189_help')


  BASE177 = WIDGET_BASE(BASE68, $
      ROW=1, $
;      FRAME=1, $
      MAP=1, $
      TITLE='image/surf select base', $
      UVALUE='BASE177')


  Btns912 = ['TV','Eq.TV.AspRt','SURFACE','CONTOUR','SHOW3','XSURFACE']
;  Btns912 = ['TV','SURFACE','CONTOUR','SHOW3','XUSRFACE']
  surface_plot = WIDGET_DROPLIST(BASE177, VALUE=BTNS912, $
	UVALUE='SURFACE_PLOT',TITLE='View as')

  Btns915 = ['By Image', 'By User']
  BGROUP184 = WIDGET_DROPLIST(BASE177, VALUE=Btns915, $
	UVALUE='BGROUP184', TITLE='Pixel')

  Btns918 = ['Step #', 'Values']
  plot_versus = WIDGET_DROPLIST(BASE177, VALUE=Btns918, $
	UVALUE='PLOTVERSUS', TITLE='Plot vs')


; add the view mode widgets

  BASE185 = WIDGET_BASE(main13_2, $
      ROW=1, MAP=1, $
;	FRAME=1, $
      TITLE='View btns', $
      UVALUE='BASE185')

  ascii_data = WIDGET_BUTTON( BASE185, VALUE='ASCII ...', $
      UVALUE='ASCII_DATA')

  refresh_data = WIDGET_BUTTON( BASE185, VALUE='ReNew', $
      UVALUE='REFRESH_DATA')


; add detectors

  BASE186 = WIDGET_BASE(main13_2, $
      ROW=1, $
      MAP=1, $
      TITLE='Detector btns', $
      UVALUE='BASE186')
  Btns_detector = [ $
    '1', $
    '2', $
    '3', $
    '4', $
    '5', $
    '6', $
    '7', $
    '8', $
    '9', $
    '10', $
    '11', $
    '12', $
    '13', $
    '14', $
    '15' $
         ]
  IMAGE186 = CW_BGROUP( BASE186, Btns_detector, $
      ROW=1, EXCLUSIVE=1, LABEL_LEFT='Images', /NO_RELEASE, $
      UVALUE='IMAGE186')

  BASE62 = WIDGET_BASE(main13_2, $
      COLUMN=2, $
      MAP=1, $
      TITLE='Plot Area', $
      UVALUE='BASE62')

  BASE62 = WIDGET_BASE(main13_2, $
      COLUMN=2, $
      MAP=1, $
      TITLE='Plot Area', $
      UVALUE='BASE62')
 
  PLOT62 = WIDGET_BASE( BASE62, /COLUMN)
  LABEL60 = WIDGET_LABEL( PLOT62, $
      UVALUE='LABEL60', $
      VALUE='3 Mouse Buttons:  LMB -Values, MMB - Line plots, RMB - Zoom_In')
 
  DRAW62 = WIDGET_DRAW( PLOT62, $
      BUTTON_EVENTS=1, $
      RETAIN=2, $
      UVALUE='DRAW62', $
      XSIZE=460, $
      YSIZE=400)


  IMAGE62 = WIDGET_BASE( BASE62, /COLUMN )
  IMAGE62_L0 = WIDGET_LABEL( IMAGE62, $
      VALUE='--IMAGE--')
  IMAGE62_L1 = WIDGET_LABEL( IMAGE62, $
      VALUE='MIN Value:')

  str='                        '
  CURSOR62_ZMIN = WIDGET_LABEL( IMAGE62, /DYNAMIC_RESIZE, $
      VALUE=str, UVALUE='CURSOR62_ZMIN')

  CURSOR62 = WIDGET_BASE( IMAGE62, /COLUMN )
  CURSOR62_L1 = WIDGET_LABEL( CURSOR62, $
      VALUE='MAX Value:')
  CURSOR62_ZMAX = WIDGET_LABEL( CURSOR62, /DYNAMIC_RESIZE, $
      VALUE=str, UVALUE='CURSOR62_ZMAX')

  CURSOR62_B1 = WIDGET_BASE( IMAGE62, /COLUMN)
  CURSOR62_XL = WIDGET_LABEL( CURSOR62_B1, $
      VALUE='Cursor @ X')
  CURSOR62_X = WIDGET_TEXT( CURSOR62_B1, VALUE='', $
	/EDITABLE, /NO_NEWLINE, $
      UVALUE='CURSOR62_X', $
	XSIZE=20, YSIZE=1)
	
  CURSOR62_B2 = WIDGET_BASE( IMAGE62, /COLUMN)
  CURSOR62_YL = WIDGET_LABEL( CURSOR62_B2, $
      VALUE='Cursor @ Y')
  CURSOR62_Y = WIDGET_TEXT( CURSOR62_B2, VALUE='', $
	/EDITABLE, /NO_NEWLINE, $
      UVALUE='CURSOR62_Y', $
	XSIZE=20, YSIZE=1)
	
  CURSOR62_B3 = WIDGET_BASE( IMAGE62, /COLUMN)
  CURSOR62_ZL = WIDGET_LABEL( CURSOR62_B3, $
      VALUE='Z Value:')
  CURSOR62_Z = WIDGET_LABEL( CURSOR62_B3, VALUE=' ', XSIZE=150, $
      UVALUE='CURSOR62_Z')

  CURSOR62_B4 = WIDGET_BASE( IMAGE62, /COLUMN)
  CURSOR62_PL = WIDGET_LABEL( CURSOR62_B4, $
      VALUE='PROBE:')
  CURSOR62_XZ = WIDGET_BUTTON( CURSOR62_B4, $
      UVALUE='CURSOR62_XZ', VALUE='XZ')
  CURSOR62_XZL = WIDGET_LABEL( CURSOR62_B4, VALUE=' ', XSIZE=150)
  CURSOR62_YZ = WIDGET_BUTTON( CURSOR62_B4, $
      UVALUE='CURSOR62_YZ', VALUE='YZ')
  CURSOR62_YZL = WIDGET_LABEL( CURSOR62_B4, VALUE=' ', XSIZE=150)


  BASE151 = WIDGET_BASE(main13_2, $
      COLUMN=1, $
;      FRAME=2, $
      MAP=1, $
      TITLE='Plot Limits', $
      UVALUE='BASE151')

;  LABEL152 = WIDGET_LABEL( BASE151, $
;      UVALUE='LABEL152', $
;      VALUE='User Entered Indices of Plot Range')

  BASE153 = WIDGET_BASE(BASE151, $
      ROW=1, MAP=1, $
      TITLE='user entered fields', $
      UVALUE='BASE153')

  BASE154 = WIDGET_BASE(BASE153, $
      ROW=1, FRAME=1, MAP=1, $
      TITLE='user entered fields', $
      UVALUE='BASE154')

  FieldVal947 = [ $
    '' ]
  FIELD161 = CW_FIELD( BASE154,VALUE=FieldVal947, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Xmin', $
      UVALUE='FIELD161', $
      XSIZE=4)

  FieldVal945 = [ $
    '' ]
  FIELD159 = CW_FIELD( BASE154,VALUE=FieldVal945, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Xmax', $
      UVALUE='FIELD159', $
      XSIZE=4)

  FieldVal943 = [ $
    '' ]
  FIELD157 = CW_FIELD( BASE154,VALUE=FieldVal943, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Ymin', $
      UVALUE='FIELD157', $
      XSIZE=4)

  FieldVal941 = [ $
    '' ]
  FIELD246 = CW_FIELD( BASE154,VALUE=FieldVal941, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Ymax', $
      UVALUE='FIELD246', $
      XSIZE=4)

  VIEW2D_ZMIN = CW_FIELD( BASE154,VALUE=0., $
      ROW=1, $
      FLOATING=1, $
      RETURN_EVENTS=1, $
      TITLE='Zmin', $
      UVALUE='VIEW2D_ZMIN', $
      XSIZE=8)

  VIEW2D_ZMAX = CW_FIELD( BASE154,VALUE=0., $
      ROW=1, $
      FLOATING=1, $
      RETURN_EVENTS=1, $
      TITLE='Zmax', $
      UVALUE='VIEW2D_ZMAX', $
      XSIZE=8)

  BASE129 = WIDGET_BASE(main13_2, $
      ROW=1, FRAME=2, MAP=1, TITLE='Info Block', $
      UVALUE='BASE129')

  BASE129_1 = WIDGET_BASE(BASE129, $
      COL=1, MAP=1)

  image_pan = WIDGET_BUTTOn(BASE129_1, VALUE='PanImages', $
        UVALUE='IMAGE_PAN')

;  LABEL131 = WIDGET_LABEL( BASE129_1, $
;      UVALUE='LABEL131', $
;      VALUE='Images Strip')

  MenuFitting = [ $
      { CW_PDMENU_S,       3, 'Fitting' }, $ ;        0
        { CW_PDMENU_S,       0, 'Ez_Fit ...' }, $ ;        1
        { CW_PDMENU_S,       2, '2D Binary' } $ ;        1
        ]
  PDMENU2D_fitting = CW_PDMENU( BASE129_1, MenuFitting, /RETURN_FULL_NAME, $
      UVALUE='PDMENU2D_FITTING')

  TextVal952 = [ $
    '' ]
  TEXT133 = WIDGET_TEXT( BASE129,VALUE=TextVal952, $
;      EDITABLE=1, $
      UVALUE='TEXT133', /SCROLL, $
      XSIZE=70, $
      YSIZE=5)

  WIDGET_CONTROL, main13_2, /REALIZE

  ; Get drawable window index

  COMMON DRAW62_Comm, DRAW62_Id
  WIDGET_CONTROL, DRAW62, GET_VALUE=DRAW62_Id

@vw2d.init

  WIDGET_CONTROL, surface_plot, SET_DROPLIST_SELECT=view_option.surface
  WIDGET_CONTROL, BGROUP184, SET_DROPLIST_SELECT=view_option.user
  WIDGET_CONTROL, plot_versus, SET_DROPLIST_SELECT=view_option.versus

catch2d_file.version = version

; get path if file defined
  if keyword_set(file) then begin
	catch2d_file.name = file

    if catch2d_file.name ne '' then begin
	 found=findfile(catch2d_file.name)
	 if found(0) ne '' then begin
		viewscanimage_init,catch2d_file.name
	 endif else begin
		w_warningtext,'Error: file not found - '+catch2d_file.name, $
			60,5,title='VW2D Messages'
	end
    end
	pos = rstrpos(file,OS_SYSTEM.file_sep)   ;'/'
	if pos gt 0 then begin
		catch2d_file.path = strmid(file,0,pos+1)
		catch2d_file.name = strmid(file,pos+1,strlen(file))
	endif else begin
		catch2d_file.path = catch2d_file.home
	end
  end

  XMANAGER, 'main13_2', MAIN13_2

END


