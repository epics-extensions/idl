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

FUNCTION u_writePermitted,filename
;
; check for filename write permission
;
        CATCH,error_status
        if error_status eq -171 then begin
                ret=WIDGET_MESSAGE(!err_string)
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

PRO u_openw,unit,filename,append=append,help=help,XDR=XDR
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


PRO u_read_set,unit,s,x
if n_params() ne 3 then begin
	print,''
	print,'Usage: u_read_set, unit, s, x'
	print,''
	print,'This routine reads a set of two varaibles: size and array '
	print,'from the LUN unit. Note that the data must be recorded by the'
	print,'u_write routine.'
	print,'     where   S    LONG = Array(5), must be defined before calling this routine'
	print,'             X    returned array'
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
		ret=WIDGET_MESSAGE('Error: wrong type of data read in!!')
		retall
	end
endcase

	CATCH,error_status
	if error_status eq -184 then begin
		ret=WIDGET_MESSAGE('Error: unable to read data, wrong type of file opened!!')
		retall
	end
	readu,unit,x
END

PRO u_read,unit,x,help=help
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
;       U_READ, Unit, Var [,/Help]
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
;-

if keyword_set(help) then goto, help1
if n_params() ne 2 then begin
	print,'Usage: u_read, unit, array'
	return
	end
s = lonarr(5)
IF NOT EOF(unit) THEN  u_read_set,unit,s,x  ELSE print,'EOF on unit ',unit
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

	OK_WRITE = u_writePermitted(filename+'.xdr')
	if OK_WRITE lt 0 then return

        id=0
        CATCH,error_status
        if error_status eq -206 then begin

                ret=WIDGET_MESSAGE(!err_string)
                if n_elements(unit) then u_close,unit
                 retall
        end

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
        ret=WIDGET_MESSAGE(string(maxno)+' sets of binary objects saved in "'+filename+'.xdr"')

	return

help1:

	print,''
	print,'Usage: U_BI2XDR,"filename"'
	print,''
	print,'This program converts the pure binary data objects into XDR binary format.'
	print,'The file "filename.xdr" created will be IDL platform independent.'
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
	base: 0L, $
	ptr_field:0L }
if n_elements(r_curr) eq 0 then begin
	LOADCT,39
	end
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
;-

COMMON PRINTER_BLOCK,printer_info
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

	PS_init
	set_plot,'PS'
	!P.FONT=0
	if (n_elements(psfile) ne 0) then begin

	if keyword_set(TV) then begin 

	; change to reverse video  for TV image

	r_curr = reverse(r_orig)
	g_curr = reverse(g_orig)
	b_curr = reverse(b_orig)
	TVLCT,r_curr,g_curr,b_curr

	    if printer_info.color gt 0 then $
		device,filename=psfile,/color,bits=8, $
			/Courier,/Bold, $
			 yoffset=7, xsize=15, ysize=15  else $
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
	!P.FONT=1
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
;-
COMMON SYSTEM_BLOCK,OS_SYSTEM

	if (n_elements(psfile) ne 0) then begin 
		if strtrim(psfile,2) eq '' then begin
			print,'Usage: PS_print, <filename>'
			return
		end
	end else psfile = 'idl.ps'

	if OS_SYSTEM.os_family eq 'unix' then $
        str =  OS_SYSTEM.lpr + ' ' + OS_SYSTEM.printer +  psfile $
	else str = OS_SYSTEM.lpr + ' ' + psfile + ' ' + OS_SYSTEM.printer
        spawn,str
	print,str
END


PRO PS_printer_Event, Event
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON PRINTER_BLOCK,printer_info

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

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

if printer_info.name ne '' and OS_SYSTEM.os_family eq 'unix' then $
	OS_SYSTEM.printer = '-P'+printer_info.name + ' ' else $
	OS_SYSTEM.printer = ''

END



PRO PS_printer,  GROUP=Group
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
;       xx-xx-xx bkc  - comment
;-

COMMON PRINTER_BLOCK,printer_info

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
      FONT='-adobe-times-medium-r-normal--20-140-100-100-p-96-iso8859-1', $
      UVALUE='LABEL3', $
      VALUE='Setup PS Printer')

  Btns167 = [ $
    'B/W', $
    'Color' ]
  BGROUP3 = CW_BGROUP( BASE2, Btns167, $
      ROW=1, $
      EXCLUSIVE=1, $
      LABEL_LEFT='Output PS', $
      UVALUE='BGROUP3')
  WIDGET_CONTROL,BGROUP3,SET_VALUE= printer_info.color

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

  Btns342 = [ $
    'Accept', $
    'Cancel' ]
  BGROUP7 = CW_BGROUP( BASE2, Btns342, $
      ROW=1, $
      UVALUE='BGROUP7')

  WIDGET_CONTROL, PS_printer_base, /REALIZE

  XMANAGER, 'PS_printer', PS_printer_base
END

; $Id: view1d.pro,v 1.1 1997/08/18 22:25:04 cha Exp $

; Copyright (c) 1991-1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
PRO XDispFile_evt, event

WIDGET_CONTROL, GET_UVALUE = retval, event.id

IF(retval EQ "EXIT") THEN WIDGET_CONTROL, event.top, /DESTROY

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
		/COLUMN, $
		SPACE = 20, $
		XPAD = 20, $
		YPAD = 20)

filequit = WIDGET_BUTTON(filebase, $			;create a Done Button
		VALUE = "Done with " + TITLE, $
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


WIDGET_CONTROL, filebase, /REALIZE			;instantiate the widget

Xmanager, "XDisplayFile", $				;register it with the
		filebase, $				;widget manager
		GROUP_LEADER = GROUP, $
		EVENT_HANDLER = "XDispFile_evt" 

END  ;--------------------- procedure XDisplayFile ----------------------------

; $Id: view1d.pro,v 1.1 1997/08/18 22:25:04 cha Exp $

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
;-

device, get_graphics = old, set_graphics = 6  ;Set xor
col = !d.n_colors -1
col = 2

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
;	BGROUP_NAMES:	An array of strings to be associated with
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

  CASE Ev OF 

  'MAIN': BEGIN
      END
  'BGROUP':BEGIN
	CASE event.value OF
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
        RESET=reset, $
	BGROUP_NAMES = bg_names, BGEVENT_FUNCT = bg_efun, $
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

  winFile = title
  winTitle = title
 
 IF reset EQ 0 THEN BEGIN

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
		buttons(N_BUTTONS:1+N_ELEMENTS(bg_names)) = bg_names(*)
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



;
;  figure out the ~ file name
; only work for unix system and HOME is defined
;
PRO filename_expand,F
        h = getenv('HOME')
        u = strupcase(getenv('USER'))
        p0 = strpos(h,u,0)
        s0 = strmid(h,0,p0)
        sp = strpos(F,!os.file_sep)
        len = strlen(F)-sp
        if STRMID(F,1,1) ne !os.file_sep  then begin
                s1 = strupcase(strmid(F,1,sp-1))
                F=s0+s1+strmid(F,sp,len)
        endif else F=h+strmid(F,sp,len)
END


;
; extract the filename & filepath from the input filename,
;     return P as file path
;
PRO filenamepath,filename,F,P
if n_elements(filename) eq 0 then return
	len = strlen(filename)
	F=filename
	if n_elements(P) eq 0 then  CD,CURRENT=P
	if strpos(filename,!os.file_sep) eq -1 then return 

	x=byte(filename)
	P=''
	for i=0,len-1 do begin
	is = len-1 -i
	if string(x(is)) eq !os.file_sep then begin
		P = strmid(filename,0,is+1)
		F = strmid(filename,is+1,len-is)
		return
		end
	end
END



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


PRO  getStatisticDeviation_1d,id1,y,mean,sdev,mdev,st
	mean=0.
	sdev=0.
	mdev=0.
	no = n_elements(y)
	if no eq 0 then return 
	mean = y(0)
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
;       st = [st, '   H-Peak  Y='+strtrim(y_hpeak)]
        st = [st, '   Peak Y='+strtrim(y_peak,1)]
	st = [st, '      @ X='+strtrim(x_peak,1)]
        st = [st, '   Centroid  @ X='+strtrim(c_mass,1)]
        st = [st, '   FWHM          '+strtrim(FWHM,1)]

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



PRO scan_read_extract,data,startno=startno,endno=endno,max=max,positioner=positioner,detector=detector,header=header,infile=infile,outfile=outfile,help=help
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON view1d_warningtext_block,view1d_warningtext_ids

 in = keyword_set(infile)
 out = keyword_set(outfile)
 ht1 = keyword_set(startno)
 ht2 = keyword_set(endno)
 no = keyword_set(max)
 position = keyword_set(positioner) 
if position eq 0 then positioner = 0
 detect = keyword_set(detector) 
if detect eq 0 then detector = 0
 header = keyword_set(header)

if no le 0 then begin
	print,'ERROR: MAX=n must be specified!'
	goto,help 
	end

if ht1 and ht2 then begin
numstep = endno-startno+1

if numstep le 0 then begin
	print,'ERROR: illegal startno, endno  range entered!'
	goto,help
	end
end

data = make_array(max, numstep, /float)

if in le 0 then filename = 'catch1d.trashcan' else $
	filename = string(infile)
	unit2=-99

if out then begin
	st = 'Extracting subset data from the " '+filename+' ".' 
	st = [st,' Extracted data will be writen to " '+outfile+' ".']
	st = [st,' Enter Y/N, default to Yes.']
	if header eq 0 then st = [st,'','No header, only 2D data image saved.']
	st2 = 'Ok to write extracted data to '+outfile	
	view1d_warningtext_ids.answer = 'Y'
	view1d_warningtext,st,quest=st2
	if view1d_warningtext_ids.answer eq 'Y' then begin
		if V1D_scanData.XDR eq 1 then U_OPENW,unit2,outfile,/XDR else $ 
		U_OPENW,unit2,outfile 
		end
	end

; position record to the startno

	id = startno
	if V1D_scanData.XDR eq 1 then U_OPENR,unit,filename,/XDR else $
	U_OPENR,unit,filename
	for i=1,id-1 do begin
	if EOF(unit) then begin
		print,'EOF!  Last record is',i-1
		return
		end
	scan_read_record,unit
	end
	
; extract read loop through startno - endno

	WHILE id le endno and NOT  EOF(unit) DO BEGIN
                scan_read_record,unit,version,pv,num_pts,FA,x,y,n,ze
;print,id,num_pts(0)+1
;help,FA

; if header is set then write the subset to outfile

	if header and unit2 gt 0 then begin
		y(11) = y(11) - startno
		y(12) = 1
		x(3) = outfile

	nx = n_elements(x)
	for i=0,nx-1 do begin
	len = strlen(x(i))
	if len lt 60 then x(i)=  x(i) + string(replicate(32b,60-len))
	end

	nx = n_elements(ze)
	for i=0,nx-1 do begin
	len = strlen(ze(i))
	if len lt 110 then ze(i)=  ze(i) + string(replicate(32b,110-len))
	end

; scan_mode_write,unit2

        u_write,unit2,version(0)
        u_write,unit2,pv(0)
        u_write,unit2,num_pts(0)
	s = size(FA)
	for i=0,s(2)-1 do begin
		px = FA(*,i)
        	u_write,unit2,px
	end
        u_write,unit2,x
        u_write,unit2,y
        u_write,unit2,n(0)
        if n(0) gt 0 then u_write,unit2,ze

	end

; subset is limited by max

subset = num_pts(0)
if subset ge max then subset = max-1
 
; default to detector 1

	i1 = 0
	if position and positioner gt 0 then begin
		i1 = positioner - 1 
		data(0:subset, id-startno) = FA(0:subset, i1)
	end
	if detect and detector gt 0     then begin
		i1 = detector - 1
		data(0:subset, id-startno) = FB(0:subset, i1)
	end

        id = id + 1
        END

	if header eq 0 and unit2 gt 0 then begin
		dims = [max,numstep]
		u_write,unit2,dims
		u_write,unit2,data
		end

	u_close,unit
	if unit2 gt 0 then u_close,unit2
	return

; on line help

help: 
	begin
	print,''
	print,'This routine extracts scan data for a given detector/positioner '
	print,'from the "catch1d.trashcan" or from a user specified input file.'
	print,'It defaults to extract scan data for detector #1. '
	print,''
	print,'Usage: scan_read_extract,data,startno=s,endno=e,max=n,positioner=i,detector=j,$'
	print,'          infile="in",outfile="out",/header'
	print,''
	print,'   DATA          -  output 2D data array'
	print,'   STARTNO=s     -  Start record no'
	print,'   ENDNO=e       -  End record no, endno > startno'
	print,'   MAX=n         -  Maximum dimension'
	print,'   POSITIONER=i  -  Extract i-th positoner data'
	print,'   DETECTOR=j    -  Extract j-th detector data'
	print,'   INFILE="in"   -  Input 1D data file, default "catch1d.trashcan"'
	print,'   OUTFILE="out" -  If specified, the extracted data will be saved as "out".'
	print,'   /HEADER       -  If specified the complete data will be saved.'
;	print,'                    If not specified, the 2D "data" array is saved.'
	return
	end
END



;
; find  fwh_max, c_mass, peak for a given x,y array
;
PRO statistic_1d,x,y,c_mass,x_peak,y_peak,y_hpeak,fwhm, $
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

PRO find_hpeak,x,nx
print,'===='
fwh_max= make_array(4,/float)
ix = nx / 4
x_index = indgen(nx)
for m=0,3 do begin
i1 = ix *m 
i2 = i1+ix-1
newx = x(i1:i2)
newy = ny(i1:i2)

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


PRO center,xindex=xindex

x = indgen(11) 
y = x + 10

scan_read_extract,x,startno=2,endno=2,max=11,positioner=1
scan_read_extract,y,startno=2,endno=2,max=11,detector=1

;print,x
;print,y

scan_read_extract,x,startno=4,endno=11,max=100,positioner=1
scan_read_extract,y,startno=2,endno=11,max=100,detector=1

x = x(20:45)
y = y(20:45)

xindex = keyword_set(xindex)
if xindex then begin
	statistic_1d,x,y,c_mass,peak,hpeak,/xindex,/list
	print,'index x used'
endif else begin
	statistic_1d,x,y,c_mass,peak,hpeak
	print,'real x used'
end

END





FUNCTION view1d_check_data_version,filename
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
;
; check for save data version consistancy
;  return -1 if not consistant, 0 if consistant
;
        F = strcompress(filename,/remove_all)
        found=findfile(F)

        if found(0) ne '' then begin

if !os.os_family eq 'unix' then begin
spawn,[!os.wc,'-l',F],y,/noshell
if y(0) eq 0 then return, -3
end

	if V1D_scanData.XDR eq 1 then U_OPENR,unit,F,/XDR else $
                U_OPENR,unit,F
                u_read,unit,version
                free_lun,unit
                if string(version(0)) ne V1D_scanData.version then begin
                st = ['Error: data version inconsistant!!!', $
		      '       Data Version: '+string(version(0)), $
                      '       Code Version: '+V1D_scanData.version, $
                      '       Wrong type of data file entered by user !!' ]
                view1d_warningtext,st
		return,-2

                if version(0) gt V1D_scanData.version then begin
			st = [ '      No conversion available! ', $
				'      Use the newer Version of CATCHER !']
                	view1d_warningtext,st
                        return,-2 
                        end
                if strlen(version(0)) gt strlen(V1D_scanData.version) then begin
			st = [ '      No conversion available! ', $
				'      Use the newer Version of CATCHER !']
                	view1d_warningtext,st
                        return,-2 
                        end
 
;                w_catch1dconvert
		return,-1
                end
        end
	return,0	
END

PRO view1d_fileOpened,FNAME
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array , view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved
COMMON view1d_viewscan_block, view1d_viewscan_ids, view1d_viewscan_id

	if view1d_viewscan_id.unit gt 0 then free_lun,view1d_viewscan_id.unit
	maxno = 0

	if V1D_scanData.XDR eq 1 then U_OPENR,unit,FNAME,/XDR else $
                U_OPENR,unit,FNAME

	found = findfile(FNAME+'.index')
	if found(0) eq '' then view1d_scan_read_all,unit,maxno else $
	view1d_readFileIndex,FNAME

		maxno = view1d_viewscan_id.maxno
		if maxno gt 0 then begin          ; update screen
		point_lun,unit,view1d_viewscan_id.fptr(maxno-1)
		view1d_scan_read,unit, maxno, maxno+1

		; open a new view file handle 
		view1d_viewscan_id.unit = unit 
		WIDGET_CONTROL,view1d_widget_ids.slider,SET_SLIDER_MAX=maxno
		WIDGET_CONTROL, view1d_widget_ids.label, $
			SET_VALUE = 'Scan # [1-'+strtrim(maxno,1)+']'
		view1d_update_summary_setup,Event
		end

                view1d_viewscan_id.seqno = maxno
                view1d_plotspec_id.seqno = maxno
	
	if view1d_plotspec_id.opened ne 0 then free_lun,view1d_plotspec_id.opened
	view1d_plotspec_id.opened = 0 

        view1d_plotspec_id.mode = 0

;print,'unit=',unit
END


PRO catcher_view1d_filename,Event
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array , view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved
COMMON view1d_viewscan_block, view1d_viewscan_ids, view1d_viewscan_id


	WIDGET_CONTROL,view1d_widget_ids.filename,GET_VALUE=F

	FNAME = strtrim(F(0),2)
	if STRMID(FNAME,0,1) eq '~' then filename_expand,FNAME 

	found = findfile(FNAME)
        if found(0) ne '' then begin

	id = view1d_check_data_version(FNAME)

	if id ne 0 then begin
		res=WIDGET_MESSAGE(FNAME + ' is a invalid data file')
		return
	end

	filenamepath,FNAME,F,P

	V1D_scanData.path = P 
	V1D_scanData.trashcan = FNAME
        view1d_plotspec_array(3) = FNAME
	
	view1d_fileOpened,FNAME

	endif else begin
		res=WIDGET_MESSAGE(FNAME + ' is a invalid data file')
		return
	end

END


PRO catcher_view1d_open,Event
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array , view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved
COMMON view1d_viewscan_block, view1d_viewscan_ids, view1d_viewscan_id

;
; destroy the old view1d_plotspec window
;
;	view1d_close_plotspec

filenamepath,V1D_scanData.trashcan,old_file,old_path

        FNAME = ''
	if V1D_scanData.XDR eq 1 then FNAME='*.xdr'
	filename = 'catch1d.trashcan'

        if view1d_plotspec_id.opened ne 0 then free_lun,view1d_plotspec_id.opened
        view1d_plotspec_id.opened = 0

; check for bad directory -296

        CATCH, error_status

        if error_status eq -296 then begin
        error = !err
	res=WIDGET_MESSAGE('Error: bad directory path for the data file ', $
		dialog_parent=Event.id)
        return
        end


        F = PICKFILE(TITLE='Open ...',/READ,FILE=filename,PATH=old_path,GET_PATH=P,FILTER=FNAME)
        IF F eq '' THEN return

	if STRMID(F,0,1) eq '~' then filename_expand,F 

        found=findfile(F)

        IF (STRPOS(F,P,0) ne -1) THEN FNAME=F else $
        IF (STRMID(F,0,1) EQ !os.file_sep) THEN $
                FNAME = F $
        ELSE $
                FNAME = P+F

        id = view1d_check_data_version(FNAME)
        if id ne 0 then begin
		res=WIDGET_MESSAGE(FNAME+' invalid data file')
		return
	end

filenamepath,FNAME,F,P
V1D_scanData.path = P 

if strlen(P) gt 1 then begin

	CATCH,error_status
	if error_status lt 0 then begin
	res=WIDGET_MESSAGE('Error: '+!err_string,/information, $
		dialog_parent=Event.id)
	return
	end

	if V1D_scanData.debug eq 1 then $
	print,'CURRENT_DIR: ',P
	cl = strlen(P)
	if strmid(P,cl-1,1) eq !os.file_sep then D = strmid(P,0,cl-1)
end

; check for bad D

	if n_elements(D) eq 0 then begin
;	view1d_warningtext,'Error:  bad directory path for the data file '
	res=WIDGET_MESSAGE('Error: bad directory path for the data file ', $
		dialog_parent=Event.id)
	return
	end

; check file for current path

        view1d_plotspec_array(3) = F
	V1D_scanData.trashcan = FNAME

	found = findfile(FNAME)
        if found(0) ne '' then begin
		WIDGET_CONTROL,view1d_widget_ids.filename,SET_VALUE=FNAME
		view1d_fileOpened,FNAME

	fd = findfile(V1D_scanData.trashcan+'.index')
	if fd(0) eq '' then $
	view1d_writeFileIndex,V1D_scanData.trashcan

        endif else begin
                view1d_viewscan_id.seqno = 0
                view1d_viewscan_id.maxno = 0 
                view1d_viewscan_id.size = 0 
                view1d_plotspec_id.seqno = 0 
	end
	

END


PRO user_scale_event,event
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON user_scale_block,user_scale_ids
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array, view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved
COMMON V1D_realtime_block, view1d_realtime_id, realtime_retval, realtime_pvnames

WIDGET_CONTROL, event.id, GET_UVALUE = eventval
CASE eventval OF
	"USER_SCALE_SLDR1" : BEGIN
		WIDGET_CONTROL,user_scale_ids.slider1,GET_VALUE=s1
		val = 10.D^s1
		WIDGET_CONTROL,user_scale_ids.xmin, $
			SET_VALUE=strtrim(string(val,format='(g20.10)'),2)
		view1d_plotspec_limits(0) = val
		END
	"USER_SCALE_SLDR2" : BEGIN
		WIDGET_CONTROL,user_scale_ids.slider2,GET_VALUE=s1
		val = 10.D^s1
		WIDGET_CONTROL,user_scale_ids.xmax, $
			SET_VALUE=strtrim(string(val,format='(g20.10)'),2)
		view1d_plotspec_limits(1) = val
		END
	"USER_SCALE_SLDR3" : BEGIN
		WIDGET_CONTROL,user_scale_ids.slider3,GET_VALUE=s1
		val = 10.D^s1
		WIDGET_CONTROL,user_scale_ids.ymin, $
			SET_VALUE=strtrim(string(val,format='(g20.10)'),2)
		view1d_plotspec_limits(2) = val
		END
	"USER_SCALE_SLDR4" : BEGIN
		WIDGET_CONTROL,user_scale_ids.slider4,GET_VALUE=s1
		val = 10.D^s1
		WIDGET_CONTROL,user_scale_ids.ymax, $
			SET_VALUE=strtrim(string(val,format='(g20.10)'),2)
		view1d_plotspec_limits(3) = val
		END
        "USER_SCALE_XMIN" : BEGIN
		WIDGET_CONTROL,user_scale_ids.xmin,GET_VALUE=s1
		val = float(s1)
		view1d_plotspec_limits(0) = val 
;       	 	view1d_UPDATE_PLOT,V1D_scanData.lastPlot
		END
        "USER_SCALE_XMAX" : BEGIN
		WIDGET_CONTROL,user_scale_ids.xmax,GET_VALUE=s1
		val = float(s1)
		view1d_plotspec_limits(1) = val 
;       	 	view1d_UPDATE_PLOT,V1D_scanData.lastPlot
		END
        "USER_SCALE_YMIN" : BEGIN
		WIDGET_CONTROL,user_scale_ids.ymin,GET_VALUE=s1
		val = float(s1)
		view1d_plotspec_limits(2) = val 
;       	 	view1d_UPDATE_PLOT,V1D_scanData.lastPlot
		END
        "USER_SCALE_YMAX" : BEGIN
		WIDGET_CONTROL,user_scale_ids.ymax,GET_VALUE=s1
		val = float(s1)
		view1d_plotspec_limits(3) = val 
;       	 	view1d_UPDATE_PLOT,V1D_scanData.lastPlot
		END
        "USER_SCALE_REFRESH" : BEGIN
		V1D_scanData.lastPlot = 1
		if view1d_realtime_id.ind eq 1 then begin
			view1d_realtime_id.ymin =0.
			view1d_realtime_id.ymax =0.
			view1d_realtime_id.axis = 1 
		endif else begin
       		 	view1d_UPDATE_PLOT,1
		end
		END
        "USER_SCALE_OK" : BEGIN
        	WIDGET_CONTROL,user_scale_ids.xmin,GET_VALUE=temp
	        view1d_plotspec_limits(0) = float(strcompress(temp(0),/remove_all))
       		WIDGET_CONTROL,user_scale_ids.xmax,GET_VALUE=temp
       		view1d_plotspec_limits(1) = float(strcompress(temp(0),/remove_all))
       	 	WIDGET_CONTROL,user_scale_ids.ymin,GET_VALUE=temp
       	 	view1d_plotspec_limits(2) = float(strcompress(temp(0),/remove_all))
       	 	WIDGET_CONTROL,user_scale_ids.ymax,GET_VALUE=temp
       	 	view1d_plotspec_limits(3) = float(strcompress(temp(0),/remove_all))
		V1D_scanData.lastPlot = 0
		if view1d_realtime_id.ind eq 1 then begin
			view1d_realtime_id.axis = 1
		endif else begin
       		 	view1d_UPDATE_PLOT,0
;			V1D_scanData.lastPlot = 1
		end
		END
        "USER_SCALE_CLOSE" : BEGIN
                WIDGET_CONTROL,event.top,/DESTROY
                END
ENDCASE
END


PRO user_scale, GROUP = GROUP
COMMON user_scale_block,user_scale_ids
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array, view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved

if XRegistered('user_scale') ne 0 then begin
	WIDGET_CONTROL,user_scale_ids.base,/DESTROY
	end

user_scale_base=WIDGET_BASE(TITLE = 'Plot Ranges ... ', /COLUMN)
label0 = WIDGET_LABEL(user_scale_base,value='User Scale Plot Ranges')

row1 = WIDGET_BASE(user_scale_base, /ROW)
label1 = WIDGET_LABEL(row1,value='XMIN')
user_scale_xmin = WIDGET_TEXT(row1,VALUE=strtrim(view1d_plotspec_limits(0),2), $
	EDITABLE=1, UVALUE='USER_SCALE_XMIN', XSIZE=20)
slider1 = WIDGET_SLIDER(row1,MIN=-17, MAX=17, SUPPRESS_VALUE=0, $
	UVALUE='USER_SCALE_SLDR1', VALUE=0)

row2 = WIDGET_BASE(user_scale_base, /ROW)
label2 = WIDGET_LABEL(row2,value='XMAX')
user_scale_xmax = WIDGET_TEXT(row2,VALUE=strtrim(view1d_plotspec_limits(1),2), $
	EDITABLE=1, UVALUE='USER_SCALE_XMAX', XSIZE=20)
slider2 = WIDGET_SLIDER(row2,MIN=-17, MAX=17, SUPPRESS_VALUE=0, $
	UVALUE='USER_SCALE_SLDR2', VALUE=0)

row3 = WIDGET_BASE(user_scale_base, /ROW)
label3 = WIDGET_LABEL(row3,value='YMIN')
user_scale_ymin = WIDGET_TEXT(row3,VALUE=strtrim(view1d_plotspec_limits(2),2), $
	EDITABLE=1, UVALUE='USER_SCALE_YMIN', XSIZE=20)
slider3 = WIDGET_SLIDER(row3,MIN=-17, MAX=17, SUPPRESS_VALUE=0, $
	UVALUE='USER_SCALE_SLDR3', VALUE=0)

row4 = WIDGET_BASE(user_scale_base, /ROW)
label4 = WIDGET_LABEL(row4,value='YMAX')
user_scale_ymax = WIDGET_TEXT(row4,VALUE=strtrim(view1d_plotspec_limits(3),2), $
	EDITABLE=1, UVALUE='USER_SCALE_YMAX', XSIZE=20)
slider4 = WIDGET_SLIDER(row4,MIN=-17, MAX=17, SUPPRESS_VALUE=0, $
	UVALUE='USER_SCALE_SLDR4', VALUE=0)

row5 = WIDGET_BASE(user_scale_base, /ROW)
ok = WIDGET_BUTTON(row5, $
                        VALUE = ' User Scale ', $
                        UVALUE = 'USER_SCALE_OK')

refresh = WIDGET_BUTTON(row5, $
                        VALUE = ' Auto Scale ', $
                        UVALUE = 'USER_SCALE_REFRESH')

close = WIDGET_BUTTON(row5, $
                        VALUE = ' Done ', $
                        UVALUE = 'USER_SCALE_CLOSE')


user_scale_ids = { $
	base : user_scale_base, $
	xmin : user_scale_xmin, $
	xmax : user_scale_xmax, $
	ymin : user_scale_ymin, $
	ymax : user_scale_ymax, $
	slider1 : slider1, $
	slider2 : slider2, $
	slider3 : slider3, $
	slider4 : slider4 $
	}
	

WIDGET_CONTROL, user_scale_base,/REALIZE

XMANAGER, 'user_scale',user_scale_base, GROUP_LEADER = GROUP


END


; PLOTSPEC


PRO view1d_close_plotspec
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array, view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved
if XRegistered('view1d_plotspec') ne 0 then $
	WIDGET_CONTROL,view1d_plotspec_ids.base,/DESTROY
END


PRO view1d_plotspec_saveTitle
COMMON CATCH1D_COM, widget_ids, scanData
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array, view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved 
COMMON view1d_viewscan_block, view1d_viewscan_ids, view1d_viewscan_id
COMMON SAVED_BLOCK, view1d_viewscan_id_saved, view1d_plotspec_id_saved

		view1d_plotspec_saved(0) = view1d_plotspec_array(0)
		view1d_plotspec_saved(1) = view1d_plotspec_array(1)
		view1d_plotspec_saved(2) = view1d_plotspec_array(2)
		view1d_plotspec_saved(3) = view1d_plotspec_array(3)
		view1d_plotspec_saved(4) = view1d_plotspec_array(4)
		view1d_plotspec_saved(5) = view1d_plotspec_array(5)

	view1d_plotspec_id_saved = view1d_plotspec_id
	view1d_viewscan_id_saved = view1d_viewscan_id
	widget_ids.view1d_base = view1d_widget_ids.base
print,'path=',scanData.path
print,'data=',scanData.trashcan
print,'home=',scanData.home
print,view1d_plotspec_limits
END

PRO view1d_plotspec_restoreTitle
COMMON CATCH1D_COM, widget_ids, scanData
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array, view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved 
COMMON view1d_viewscan_block, view1d_viewscan_ids, view1d_viewscan_id
COMMON SAVED_BLOCK, view1d_viewscan_id_saved, view1d_plotspec_id_saved

		view1d_plotspec_array(0) = view1d_plotspec_saved(0)
		view1d_plotspec_array(1) = view1d_plotspec_saved(1)
		view1d_plotspec_array(2) = view1d_plotspec_saved(2)
		view1d_plotspec_array(3) = view1d_plotspec_saved(3)
		view1d_plotspec_array(4) = view1d_plotspec_saved(4)
		view1d_plotspec_array(5) = view1d_plotspec_saved(5)

	view1d_plotspec_id = view1d_plotspec_id_saved
	view1d_viewscan_id = view1d_viewscan_id_saved

print,'path=',scanData.path
print,'data=',scanData.trashcan
print,'home=',scanData.home
CD,scanData.path
print,view1d_plotspec_limits
END


PRO view1d_setDefaultLabels
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array , view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus

if n_elements(x_names) eq 0 then begin
        x_names=make_array(4,/string,value=string(replicate(32b,30)))
        y_names=make_array(15,/string,value=string(replicate(32b,30)))
        x_descs=make_array(4,/string,value=string(replicate(32b,30)))
        y_descs=make_array(15,/string,value=string(replicate(32b,30)))
        x_engus=make_array(4,/string,value=string(replicate(32b,30)))
        y_engus=make_array(15,/string,value=string(replicate(32b,30)))
	end

END

PRO view1d_setPlotLabels
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array , view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus

; if view1d_plotspec is open

; if view1d_plotspec_id.mode eq 1 then return
if XRegistered('view1d_plotspec') ne 0 then return
	
        title = view1d_plotspec_array(0)
        ix = view1d_plotspec_id.xcord
	t_xlabel = 'P'+strtrim(ix+1,2)

; get title
	st = ''
	title = string(replicate(32b,60))
        title = st +' ('+ V1D_scanData.pv +')'
        if V1D_scanData.y_scan then begin

		if V1D_scanData.dataversion ne '' then $
		title = st + ' @ y('+strtrim(V1D_scanData.y_seqno,2) + ')=' + $
			strtrim(V1D_scanData.y_value,2) $
		else $
		title = st + ' @ y('+strtrim(V1D_scanData.y_seqno,2) + ')'
	end

; get xlabel
	xlabel = string(replicate(32b,60))
     if ix lt 4 then begin
	len = strlen(strtrim(x_descs(ix),2))
        if len gt 1 then strput,xlabel,x_descs(ix) else $
        	strput,xlabel,'P'+strtrim(ix+1,2)
	if len lt 1 then len = 2
	l2 = strlen(x_engus(ix))
        if l2 gt 1 then begin
		len = len + 2
		strput,xlabel,'(',len
		len = len + 2
		strput,xlabel,strtrim(x_engus(ix)),len
		len = len + l2 + 1
		strput,xlabel,')',len
		end
     end
     if ix ge 4 then begin   ; if detector for x axis
	ixx = ix - 4
	len = strlen(y_descs(ixx))
        if len gt 1 then strput,xlabel,y_descs(ixx) else $
        	strput,xlabel,'D'+strtrim(ixx+1,2)
	if len lt 1 then len = 2
	l2 = strlen(y_engus(ixx))
        if l2 gt 1 then begin
		len = len + 2
		strput,xlabel,'(',len
		len = len + 2
		strput,xlabel,strtrim(y_engus(ixx)),len
		len = len + l2 + 1
		strput,xlabel,')',len
		end
     end

	if strtrim(title,2) ne '' then view1d_plotspec_array(0) = title
	if strtrim(xlabel,2) ne '' then view1d_plotspec_array(1) = xlabel else $
		view1d_plotspec_array(1) = t_xlabel
;print,'TITLE:',title
;print,'XLABEL:',xlabel

END




PRO view1d_plotspec_event,event
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array, view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved
COMMON PLOTMENU_OPTION_BLOCK,ids,r_names        ; update plotoption menu
COMMON V1D_realtime_block, view1d_realtime_id, realtime_retval, realtime_pvname


WIDGET_CONTROL, event.id, GET_UVALUE = eventval

if view1d_plotspec_id.scan eq 0 and view1d_realtime_id.ind eq -1 then $
  V1D_scanData.act_npts = V1D_scanData.readin_npts 

CASE eventval OF
	"PLOT_TITLE" : BEGIN
                WIDGET_CONTROL,view1d_plotspec_ids.title,GET_VALUE=temp
                view1d_plotspec_array(0) = strcompress(temp(0))
		END
	"PLOT_XTITLE" : BEGIN
                WIDGET_CONTROL,view1d_plotspec_ids.xtitle,GET_VALUE=temp
                view1d_plotspec_array(1) = strcompress(temp(0))
		END
	"PLOT_YTITLE" : BEGIN
                WIDGET_CONTROL,view1d_plotspec_ids.ytitle,GET_VALUE=temp
                view1d_plotspec_array(2) = strcompress(temp(0))
		END
	"PLOT_SAVENAME" : BEGIN
                WIDGET_CONTROL,view1d_plotspec_ids.savename,GET_VALUE=temp
                view1d_plotspec_array(3) = strcompress(temp(0),/remove_all)
		END
	"PLOT_FOOTER" : BEGIN
                WIDGET_CONTROL,view1d_plotspec_ids.footer,GET_VALUE=temp
                view1d_plotspec_array(5) = strcompress(temp(0))
		END
;	"PLOT_RANGES" : BEGIN
;		user_scale,GROUP=event.top
;		END
        "PLOTSPEC_OK" : BEGIN
                WIDGET_CONTROL,view1d_plotspec_ids.title,GET_VALUE=temp
                view1d_plotspec_array(0) = strcompress(temp(0))
                WIDGET_CONTROL,view1d_plotspec_ids.xtitle,GET_VALUE=temp
                view1d_plotspec_array(1) = strcompress(temp(0))
                WIDGET_CONTROL,view1d_plotspec_ids.ytitle,GET_VALUE=temp
                view1d_plotspec_array(2) = strcompress(temp(0))
                WIDGET_CONTROL,view1d_plotspec_ids.savename,GET_VALUE=temp
                view1d_plotspec_array(3) = strcompress(temp(0),/remove_all)
                WIDGET_CONTROL,view1d_plotspec_ids.footer,GET_VALUE=temp
                view1d_plotspec_array(5) = strcompress(temp(0))
                END
	"PLOTSPEC_DONE" : BEGIN
		WIDGET_CONTROL,view1d_plotspec_ids.title,GET_VALUE=temp
		view1d_plotspec_array(0) = strcompress(temp(0))
		WIDGET_CONTROL,view1d_plotspec_ids.xtitle,GET_VALUE=temp
		view1d_plotspec_array(1) = strcompress(temp(0))
		WIDGET_CONTROL,view1d_plotspec_ids.ytitle,GET_VALUE=temp
		view1d_plotspec_array(2) = strcompress(temp(0))
		WIDGET_CONTROL,view1d_plotspec_ids.savename,GET_VALUE=temp
		view1d_plotspec_array(3) = strcompress(temp(0),/remove_all)
                WIDGET_CONTROL,view1d_plotspec_ids.footer,GET_VALUE=temp
                view1d_plotspec_array(5) = strcompress(temp(0))
		WIDGET_CONTROL, event.top, /DESTROY
		return
                END
	"PLOTSPEC_CANCEL" : BEGIN
		WIDGET_CONTROL, event.top, /DESTROY
		return
		END
ENDCASE

if view1d_realtime_id.ind eq 1 then begin
	view1d_realtime_id.axis = 1
endif else $
	view1d_UPDATE_PLOT,V1D_scanData.lastPlot

END

PRO view1d_plotspec, GROUP = GROUP, help=help
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array, view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved 
COMMON view1d_viewscan_block, view1d_viewscan_ids, view1d_viewscan_id

if XRegistered('view1d_plotspec') ne 0 then return

if n_elements(view1d_plotspec_array) eq 0 then $
view1d_plotspec_array = make_array(6,/string,value=string(replicate(32b,60)))
	
if n_elements(view1d_plotspec_limits) eq 0 then begin
	view1d_plotspec_limits = make_array(4,/float)
	view1d_plotspec_limits = [0., 100., 0., 100.]
	end


if strlen(strcompress(view1d_plotspec_array(3),/remove_all)) lt 1 then $
	view1d_plotspec_array(3) = 'catch1d.trashcan'

view1d_plotspec_base=WIDGET_BASE(TITLE = 'Plot Labels ... ', /COLUMN)     

row0 = WIDGET_BASE(view1d_plotspec_base, /ROW)

seqno_lb = WIDGET_LABEL(row0, VALUE='Scan #: ' + $
	 strcompress(view1d_plotspec_id.seqno + 1))

;limits_lb = WIDGET_BUTTON(row0, VALUE='Set User Scale ...', $
;		UVALUE= 'PLOT_RANGES')


row1 = WIDGET_BASE(view1d_plotspec_base, /ROW)
title_lb = WIDGET_LABEL(row1, VALUE='Title  :')
view1d_plotspec_title = WIDGET_TEXT(row1, /EDITABLE,  /NO_NEWLINE, $
	SCR_XSIZE = 300, $
        XSIZE=60, YSIZE=1, VALUE=strtrim(view1d_plotspec_array(0)), UVALUE='PLOT_TITLE')

row2 = WIDGET_BASE(view1d_plotspec_base, /ROW)
xtitle_lb = WIDGET_LABEL(row2, VALUE='X Label:')
view1d_plotspec_xtitle = WIDGET_TEXT(row2, /EDITABLE,  /NO_NEWLINE, $
	SCR_XSIZE = 300, $
        XSIZE=60, YSIZE=1, VALUE=strtrim(view1d_plotspec_array(1)), UVALUE='PLOT_XTITLE')

row3 = WIDGET_BASE(view1d_plotspec_base, /ROW)
ytitle_lb = WIDGET_LABEL(row3, VALUE='Y Label:')
view1d_plotspec_ytitle = WIDGET_TEXT(row3, /EDITABLE,  /NO_NEWLINE, $
	SCR_XSIZE = 300, $
        XSIZE=60, YSIZE=1, VALUE=strtrim(view1d_plotspec_array(2)), UVALUE='PLOT_YTITLE')

row4 = WIDGET_BASE(view1d_plotspec_base, /ROW)
savefile_lb = WIDGET_LABEL(row4, VALUE='Scan Data Saved in:   ')
view1d_plotspec_savename = WIDGET_LABEL(row4, VALUE=strtrim(view1d_plotspec_array(3)) )

row4_1 = WIDGET_BASE(view1d_plotspec_base, /ROW)
savefile_lb = WIDGET_LABEL(row4_1, VALUE='Comment:')
view1d_plotspec_footer = WIDGET_TEXT(row4_1, /EDITABLE,  /NO_NEWLINE, $
	SCR_XSIZE = 300, $
        XSIZE=60, YSIZE=1, VALUE=strtrim(view1d_plotspec_array(5)), UVALUE='PLOT_FOOTER')


lastrow = WIDGET_BASE(view1d_plotspec_base, /ROW)

view1d_plotspec_ok = WIDGET_BUTTON(lastrow, $
                        VALUE = ' Apply ', $
                        UVALUE = 'PLOTSPEC_OK')
view1d_plotspec_cancel = WIDGET_BUTTON(lastrow, $
                        VALUE = 'Cancel', $
                        UVALUE = 'PLOTSPEC_CANCEL')
view1d_plotspec_done = WIDGET_BUTTON(lastrow, $
                        VALUE = ' Done ', $
                        UVALUE = 'PLOTSPEC_DONE')

; set widget ids :
view1d_plotspec_ids = { $
	base:	view1d_plotspec_base, $
	title:  view1d_plotspec_title, $
	xtitle:  view1d_plotspec_xtitle, $
	ytitle:  view1d_plotspec_ytitle, $
	savename:  view1d_plotspec_savename, $
	footer:  view1d_plotspec_footer $
	 }

; Realize the widgets:
WIDGET_CONTROL, view1d_plotspec_base, /REALIZE
if view1d_plotspec_id.realtime eq 0 then $
WIDGET_CONTROL, view1d_plotspec_dtime,SENSITIVE=0 

; Hand off to the XMANAGER:
XMANAGER, 'view1d_plotspec', view1d_plotspec_base, GROUP_LEADER = GROUP

END


; HELP MENU

PRO HELPMENU_Event, Event
  COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData

  CASE Event.Value OF 
  'Help.Version ...': BEGIN
	st = ['          VIEW1D Version  : (R1.1)', $
	        '','This program allows the IDL users to view the', $
		'1D data saved by the data catcher developed at APS/XFD.', $
		'','This IDL program supports different platform. The system', $
		'operation dependent parameters are defined in the os.init', $
		'which can be tailored to fit different platform.',$ 
		'', 'This program is written in IDL 4.01b and is IDL 5.0', $
		'compatiable.']	
	res = WIDGET_MESSAGE(st,/information,dialog_parent=Event.Top)
 	END

  'Help.BIN/XDR ...': BEGIN
	st = [ 'VIEW1D acccepts the 1D data saved by the data catcher which can',$
		'be either in pure BINary or XDR form.  If the data is in XDR', $
		'form which is platform independent.','', $
		'The type droplist indicates the input filename is either BIN or XDR.', $
		'The default option is BIN type. The correct type of option BIN/XDR', $
		'must be set, before selecting the desired input files. The filename', $
		'of XDR type of data is assumed ended with a ".xdr"' , '',$
		'The pure binary data can be converted to XDR form by the following,', $
		'routine defined in the view1d.pro program,  e.g. ', '',$
		'       U_BI2XDR,"filename" ','', $
		'For input 1D data "filename", a XDR binary data file "filename.xdr"', $
		'will be created by the above command.','']	
	res = WIDGET_MESSAGE(st,/information,dialog_parent=Event.Top)
 	END
  ENDCASE
END

; WARNINGTEXT window

PRO view1d_warningtext_quest
COMMON view1d_warningtext_block,view1d_warningtext_ids

	WIDGET_CONTROL,view1d_warningtext_ids.text,GET_VALUE=ans
	view1d_warningtext_ids.answer = strtrim(strupcase(ans(0)),2)
	WIDGET_CONTROL,view1d_warningtext_ids.base,BAD_ID=bad,/DESTROY
END

PRO view1d_warningtext_event,event
COMMON view1d_warningtext_block,view1d_warningtext_ids

WIDGET_CONTROL, event.id, GET_UVALUE = eventval
CASE eventval OF
        "WARNINGTEXT_GET" : BEGIN
		WIDGET_CONTROL,event.id,GET_VALUE=ans
		view1d_warningtext_ids.answer = strtrim(strupcase(ans(0)),2)
		view1d_warningtext_quest
		END
        "WARNINGTEXT_Y" : BEGIN
                WIDGET_CONTROL,view1d_warningtext_ids.text,SET_VALUE='Y'
                END
        "WARNINGTEXT_N" : BEGIN
                WIDGET_CONTROL,view1d_warningtext_ids.text,SET_VALUE='N'
                END
        "WARNINGTEXT_OK" : BEGIN
		view1d_warningtext_quest
		END
        "WARNINGTEXT_CLOSE" : BEGIN
                WIDGET_CONTROL,event.top,BAD_ID=bad,/DESTROY
                END
ENDCASE
END


PRO view1d_warningtext, str,width,height,heading,title=title,quest=quest, GROUP = GROUP
COMMON view1d_warningtext_block,view1d_warningtext_ids

if XRegistered('view1d_warningtext') ne 0 then begin
	WIDGET_CONTROL,view1d_warningtext_ids.base,/DESTROY
	end
wtitle = 'View1d Messages'
dtitle = ''
if n_elements(width) eq 0 then width = 80
if n_elements(height) eq 0 then height = 5 
if n_elements(heading) gt 0 then dtitle=string(heading)
if n_elements(title) gt 0 then wtitle=string(title)

view1d_warningtext_ids = { $
	base : 0L, $
	text : 0L, $
	quest : '', $
	answer : 'Y' $
	}

view1d_warningtext_base=WIDGET_BASE(TITLE = wtitle, /COLUMN)
view1d_warningtext_ids.base = view1d_warningtext_base
view1d_warningtext_title = WIDGET_LABEL(view1d_warningtext_base,VALUE=dtitle)

list = WIDGET_TEXT(view1d_warningtext_base,VALUE=str,UVALUE='LIST', $
	XSIZE =width, $
	YSIZE=height,/SCROLL)

if n_elements(quest) ne 0 then begin
view1d_warningtext_ids.quest = string(quest)
view1d_warningtext_row =WIDGET_BASE(view1d_warningtext_base, /ROW, /FRAME)
view1d_warningtext_lab = WIDGET_LABEL(view1d_warningtext_row,VALUE=string(quest)+' (Y/N) ?')
view1d_warningtext_text = WIDGET_TEXT(view1d_warningtext_row,VALUE='Y', $
	EDITABLE=1, UVALUE='WARNINGTEXT_GET', XSIZE=2)
view1d_warningtext_ids.text = view1d_warningtext_text 

view1d_warningtext_y = WIDGET_BUTTON(view1d_warningtext_row,VALUE='Y', $
	UVALUE='WARNINGTEXT_Y')
view1d_warningtext_n = WIDGET_BUTTON(view1d_warningtext_row,VALUE='N', $
	UVALUE='WARNINGTEXT_N')

view1d_warningtext_actrow =WIDGET_BASE(view1d_warningtext_base, /ROW)
view1d_warningtext_ok = WIDGET_BUTTON(view1d_warningtext_actrow,VALUE=' Accept ', $
	UVALUE='WARNINGTEXT_OK')
close = WIDGET_BUTTON(view1d_warningtext_actrow, $
                        VALUE = ' Cancel ', $
                        UVALUE = 'WARNINGTEXT_CLOSE')

endif else begin
close = WIDGET_BUTTON(view1d_warningtext_base, $
                        VALUE = ' Close ', $
                        UVALUE = 'WARNINGTEXT_CLOSE')
end

WIDGET_CONTROL, view1d_warningtext_base,/REALIZE

XMANAGER,'view1d_warningtext',view1d_warningtext_base, GROUP_LEADER = GROUP


END


; ZOOM BOX 


PRO zoom_to_box
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array , view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved
COMMON view1d_warningtext_block,view1d_warningtext_ids

tx = ['Mouse buttons :', $
	'    Left :   drag box ', $
	'    Middle:  resize box ', $
	'    Right:   zoom to box']
view1d_warningtext,tx,40,5,'Zoom to box'

	win_state = WIDGET_INFO(view1d_widget_ids.base,/GEOMETRY)
	if win_state.xoffset gt 300 then $
	WIDGET_CONTROL,view1d_warningtext_ids.base, $
		TLB_SET_XOFFSET=10, $
		TLB_SET_YOFFSET=300 $
	else $
	WIDGET_CONTROL,view1d_warningtext_ids.base, $
		TLB_SET_XOFFSET=10+win_state.xoffset+win_state.xsize, $
		TLB_SET_YOFFSET=300 


IF V1D_scanData.lastPlot eq -1 then return 
;        WIDGET_CONTROL, view1d_widget_ids.plot_area, SENSITIVE = 0

save_limits = make_array(4,/float)

save_limits(0) = view1d_plotspec_limits(0)
save_limits(1) = view1d_plotspec_limits(1)
save_limits(2) = view1d_plotspec_limits(2)
save_limits(3) = view1d_plotspec_limits(3)

WSET, view1d_widget_ids.plot_area

        MY_BOX_CURSOR,x,y,xs,ys
        WIDGET_CONTROL,view1d_widget_ids.plot_wid,/CLEAR_EVENTS 
        WIDGET_CONTROL,view1d_warningtext_ids.base,/DESTROY
d=convert_coord([x,x+xs],[y,y+ys],/DEVICE,/TO_DATA)
view1d_plotspec_limits(0) = d(0,0)
view1d_plotspec_limits(1) = d(0,1)
view1d_plotspec_limits(2) = d(1,0)
view1d_plotspec_limits(3) = d(1,1)
        WAIT, .2
;        WIDGET_CONTROL,view1d_widget_ids.plot_area , SENSITIVE = 1
        view1d_UPDATE_PLOT, 0

view1d_plotspec_limits(0) = save_limits(0)
view1d_plotspec_limits(1) = save_limits(1)
view1d_plotspec_limits(2) = save_limits(2)
view1d_plotspec_limits(3) = save_limits(3)

END

PRO zoom_box,x1,y1,x2,y2
  COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array , view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved

IF V1D_scanData.lastPlot eq -1 then return 

save_limits = make_array(4,/float)

save_limits(0) = view1d_plotspec_limits(0)
save_limits(1) = view1d_plotspec_limits(1)
save_limits(2) = view1d_plotspec_limits(2)
save_limits(3) = view1d_plotspec_limits(3)

WSET, view1d_widget_ids.plot_area

view1d_plotspec_limits(0) = x1
view1d_plotspec_limits(1) = x2
view1d_plotspec_limits(2) = y1
view1d_plotspec_limits(3) = y2
        WAIT, .2
;        WIDGET_CONTROL,view1d_widget_ids.plot_area , SENSITIVE = 1
        view1d_UPDATE_PLOT, 0

view1d_plotspec_limits(0) = save_limits(0)
view1d_plotspec_limits(1) = save_limits(1)
view1d_plotspec_limits(2) = save_limits(2)
view1d_plotspec_limits(3) = save_limits(3)
END


PRO zoom_in_out
  COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON view1d_warningtext_block,view1d_warningtext_ids
tx = ['Mouse buttons :', $
	'    Left :   zoom in ', $
	'    Middle:  zoom out ', $
	'    Right:   quit zoom in/out mode']
view1d_warningtext,tx,40,5,'Zoom In/Out'

	win_state = WIDGET_INFO(view1d_widget_ids.base,/GEOMETRY)
	if win_state.xoffset gt 300 then $
	WIDGET_CONTROL,view1d_warningtext_ids.base, $
		TLB_SET_XOFFSET=10, $
		TLB_SET_YOFFSET=300 $
	else $
	WIDGET_CONTROL,view1d_warningtext_ids.base, $
		TLB_SET_XOFFSET=10+win_state.xoffset+win_state.xsize, $
		TLB_SET_YOFFSET=300 

WSET, view1d_widget_ids.plot_area

WHILE 1 do begin
;cursor,x,y,1,/normal
cursor,x,y,0,/normal

if !err eq 2 then begin            ; zoom out
;	view1d_UPDATE_PLOT,1
x = (x - !x.window(0)) / (!x.window(1)-!x.window(0)) * $
        (!x.crange(1)-!x.crange(0)) + !x.crange(0)

y = (y - !y.window(0)) / (!y.window(1)-!y.window(0)) * $
        (!y.crange(1)-!y.crange(0)) + !y.crange(0)
;st = 'Cursor X Y value @ button 1 pressed: '+string(x) +string(y)
;print,st

dx = 1. * (!x.crange(1)-!x.crange(0))
dy = 1. * (!y.crange(1)-!y.crange(0))
x1 = x - dx
x2 = x + dx
y1 = y - dy
y2 = y + dy
zoom_box,x1,y1,x2,y2
end

if !err eq 1 then begin            ; zoom in 
x = (x - !x.window(0)) / (!x.window(1)-!x.window(0)) * $
        (!x.crange(1)-!x.crange(0)) + !x.crange(0)

y = (y - !y.window(0)) / (!y.window(1)-!y.window(0)) * $
        (!y.crange(1)-!y.crange(0)) + !y.crange(0)
;st = 'Cursor X Y value @ button 1 pressed: '+string(x) +string(y)
;print,st

dx = 0.25 * (!x.crange(1)-!x.crange(0))
dy = 0.25 * (!y.crange(1)-!y.crange(0))
x1 = x - dx 
x2 = x + dx 
y1 = y - dy 
y2 = y + dy 
;if x2 gt !x.crange(1) then x2 = !x.crange(1)
;if x1 lt !x.crange(0) then x1 = !x.crange(0)
;if y2 gt !y.crange(1) then y2 = !y.crange(1)
;if y1 lt !y.crange(0) then y1 = !y.crange(0)
zoom_box,x1,y1,x2,y2
end

if !err eq 4 then begin 		; stop zoom in/out
	WIDGET_CONTROL,view1d_warningtext_ids.base,BAD=bad,/DESTROY
	WIDGET_CONTROL,view1d_widget_ids.plot_wid,/CLEAR_EVENTS
	return
	end

end
END


PRO draw_dragLine,clean=clean,x,y,slope
  COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON view1d_warningtext_block,view1d_warningtext_ids
COMMON w_statistic_block,w_statistic_ids

tx = ['Mouse buttons :', $
	'    Left :   pick start point ', $
	'    Middle:  pick end point ', $
	'    Right:   quit slope calc mode']
view1d_warningtext,tx,40,5,'Pick Slope Line'

	win_state = WIDGET_INFO(view1d_widget_ids.base,/GEOMETRY)
	if win_state.xoffset gt 300 then $
	WIDGET_CONTROL,view1d_warningtext_ids.base, $
		TLB_SET_XOFFSET=10,TLB_SET_YOFFSET=300 $
	else $
	WIDGET_CONTROL,view1d_warningtext_ids.base, $
		TLB_SET_XOFFSET=10+win_state.xoffset+win_state.xsize, $
		TLB_SET_YOFFSET=300


!Err = 0
if keyword_set(clean) then begin
        if n_params() lt 2 then begin
                print,'Usage: draw_dragLine,x,y,[/clean,slope]
                return
                end
        if n_elements(x) eq n_elements(y) and n_elements(x) gt 1 then $
        oplot,x,y,color=0
        return
        end

; need to be drawing area

WSET, view1d_widget_ids.plot_area

LOOP0:
cursor,x1,y1,/down
x2=x1 & y2=y1
LOOP:
while (!err ne 2) do begin
        oplot,[x1,x2],[y1,y2], color=0
        cursor,x2,y2,/nowait
        oplot,[x1,x2],[y1,y2], color = !d.n_colors - 2
wait,0.001
endwhile
	
	slope=0
	x=[x1,x2]
	y=[y1,y2]
	if  x(1) ne x(0) then slope = (y(1)-y(0)) /(x(1) -x(0))
	st = ''
	st = [st,'X1 = '+string(x1)]
	st = [st,'Y1 = '+string(y1)]
	st = [st,'X2 = '+string(x2)]
	st = [st,'Y2 = '+string(y2)]
	st = [st,'','Slope = '+string(slope)]	

	if !err eq 2 then begin 		; whether stop the mode 
	WIDGET_CONTROL,view1d_widget_ids.plot_wid,/CLEAR_EVENTS

	w_statistic,st,25,10,'Slope Calc'
	win_state = WIDGET_INFO(view1d_widget_ids.base,/GEOMETRY)
	if win_state.xoffset gt 300 then $
	WIDGET_CONTROL,w_statistic_ids.base, $
		TLB_SET_XOFFSET=10, $
		TLB_SET_YOFFSET=500 $
	else $
	WIDGET_CONTROL,w_statistic_ids.base, $
		TLB_SET_XOFFSET=10+win_state.xoffset+win_state.xsize, $
		TLB_SET_YOFFSET=500


	cursor,x1,y1,/down

	if !err eq 4 then begin
		oplot,x,y,color=0
		oplot,x,y
		WIDGET_CONTROL,view1d_warningtext_ids.base,BAD=bad,/DESTROY
		WIDGET_CONTROL,w_statistic_ids.base,BAD=bad,/DESTROY
		view1d_UPDATE_PLOT,V1D_scanData.lastPlot
		return
		end

	if !err eq 1 then begin
		oplot,x,y,color=0
		goto,LOOP
		end
	end

	if !err eq 2 then begin
		oplot,x,y,color=0
		goto,LOOP0
	end
END

; PRINTMENU

PRO PRINTMENU_Event, Event
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON view1d_viewscan_block, view1d_viewscan_ids, view1d_viewscan_id

; COMMON font_block, text_font, graf_font, ps_font

  CASE Event.Value OF 

  'Print.ASCII (tmp)': BEGIN

	if demo_mode() eq 1 then begin
		res=WIDGET_MESSAGE('Not available in demo mode !')
		return
	end

	filename = V1D_scanData.trashcan
	outfile = filename+'.tmp'

        CATCH,error_status
        if error_status eq -171 then begin
                report_path = V1D_scandata.home + !os.file_sep
                outfile = report_path+ '.tmp'
                goto, RESET_TMPNAME
        end
        openw,unit,outfile,/get_lun
        u_close,unit
	RESET_TMPNAME:

	id = view1d_viewscan_id.seqno+1 
	if id gt view1d_viewscan_id.maxno then id = view1d_viewscan_id.maxno
	header = 0
	view1d_summary_report_dump,filename,outfile,id,id,header  

	WIDGET_CONTROL,view1d_widget_ids.terminal,BAD_ID=bad
	if bad ne 0 or view1d_widget_ids.terminal eq 0 then $
	view1d_widget_ids.terminal = CW_TERM(view1d_widget_ids.base, $
		TITLE='VIEW1D (ASCII .tmp file)', XSIZE=100,ysize=20,/SCROLL)
	id = CW_TERM(view1d_widget_ids.terminal,filename=outfile,/reset)

	END
  'Print.Plot': BEGIN
	if V1D_scanData.lastPlot lt 0 then return
	V1D_scanData.act_npts = V1D_scanData.readin_npts
    	PS_open,'view1d.ps'
;    graf_font = ps_font
    	view1d_UPDATE_PLOT, V1D_scanData.lastPlot
    	PS_close
    	PS_print,'view1d.ps'
;    graf_font = text_font
 	END
  'Print.Report ...': BEGIN
	if demo_mode() eq 1 then begin
		res=WIDGET_MESSAGE('Not available in demo mode !')
		return
	end

	view1d_summary_setup,GROUP=Event.top  		; pick the range

 	END
  ENDCASE
END

; STATISTICMENU

PRO STATISTICMENU_Event, Event

COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array , view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved
COMMON w_statistic_block,w_statistic_ids

  CASE Event.Value OF

  'Statistic.None': BEGIN
        view1d_plotspec_id.statistic = 0
        view1d_UPDATE_PLOT,1
	if XRegistered('w_statistic') ne 0 then begin
	WIDGET_CONTROL,w_statistic_ids.base,BAD=bad,/DESTROY
	view1d_widget_ids.statistic = 0
	end
    END

  'Statistic.Peak/Centroid/FWHM on plot': BEGIN
        view1d_plotspec_id.statistic = 1
        view1d_UPDATE_PLOT,1,st
	if XRegistered('w_statistic') ne 0 then begin
	WIDGET_CONTROL,w_statistic_ids.base,BAD=bad,/DESTROY
	view1d_widget_ids.statistic = 0
	end
    END

  'Statistic.Peak/Centroid/FWHM ...': BEGIN
        view1d_plotspec_id.statistic = 2
        view1d_UPDATE_PLOT,1,st
        if n_elements(st) gt 0 then begin
        if view1d_widget_ids.statistic eq 0 then $
                w_statistic,st,34,25,'Statistic',GROUP=Event.top $
          else WIDGET_CONTROL,view1d_widget_ids.statistic,SET_VALUE=st
        end
    END

  'Statistic.Average/Deviation ...': BEGIN
        view1d_plotspec_id.statistic = 3
        view1d_UPDATE_PLOT,1,st
        if n_elements(st) gt 0 then begin
        if view1d_widget_ids.statistic eq 0 then $
        	w_statistic,st,34,25,'Statistic',GROUP=Event.top $ 
	else WIDGET_CONTROL,view1d_widget_ids.statistic,SET_VALUE=st 
	end
    END

  ENDCASE

END

;
; CROSS-HAIRS XY_COORD
;


PRO view1d_xycoord, clean=clean
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON XY_COORD_BLOCK, xy_id, xy_wid
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array , view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved

; wipe out the old value
if xy_id.plot eq 1 then begin
    if view1d_plotspec_id.log ne 1 then begin
	oplot,[xy_id.x0,xy_id.x0],!y.crange, line = 1, color=0
	oplot,!x.crange,[xy_id.y0,xy_id.y0], line = 1, color=0
    endif else begin    ; YLOG
	oplot,[xy_id.x0,xy_id.x0],10^!y.crange, line = 1, color=0
	oplot,!x.crange,[xy_id.y0,xy_id.y0], line = 1, color=0
    end
end
if keyword_set(clean) then return

cursor,x,y,0,/normal
x = (x - !x.window(0)) / (!x.window(1)-!x.window(0)) * $
	(!x.crange(1)-!x.crange(0)) + !x.crange(0)

y = (y - !y.window(0)) / (!y.window(1)-!y.window(0)) * $
	(!y.crange(1)-!y.crange(0)) + !y.crange(0)

if view1d_plotspec_id.log eq 1 then begin    ; YLOG
	y = 10^y
	oplot,[x,x],10^!y.crange, line = 1
	oplot,!x.crange,[y,y], line = 1
endif else begin
	oplot,[x,x],!y.crange, line = 1
	oplot,!x.crange,[y,y], line = 1
end

st = 'Cursor X Y value @ button 1 pressed: '+string(x) +string(y)
if V1D_scanData.debug gt 0 then print,st

xy_id.x0 = x
xy_id.y0 = y
xy_id.x1 = x
xy_id.y1 = y
xy_id.plot = 1
;xy_id.st = st

if xy_wid.base ne 0 then begin
WIDGET_CONTROL,	xy_wid.x, SET_VALUE = strtrim(xy_id.x0,2)
WIDGET_CONTROL,	xy_wid.y, SET_VALUE = strtrim(xy_id.y1,2) 
WIDGET_CONTROL,	xy_wid.motor, SET_VALUE =  'Ref Positioner # '+strtrim(view1d_plotspec_id.xcord+1,2)
end

END


PRO XYCOORD_BASE_Event, Event
COMMON XY_COORD_BLOCK, xy_id, xy_wid

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'XY_COORD_FIELD4': BEGIN
	WIDGET_CONTROL,xy_wid.x,GET_VALUE=xvalue
      END
  'XY_COORD_FIELD7': BEGIN
;	WIDGET_CONTROL,xy_wid.y,GET_VALUE=yvalue
      END
  'BUTTON10': BEGIN
	WIDGET_CONTROL,Event.top,/DESTROY
	xy_wid.base = 0L
	view1d_xycoord,/CLEAN
;	view1d_UPDATE_PLOT,1
      END
  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END XYCOORD_BASE
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO view1d_xy_coord, GROUP=Group
COMMON XY_COORD_BLOCK, xy_id, xy_wid
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array , view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved

if XRegistered('XYCOORD_BASE') NE 0 then return

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

if n_elements(xy_id) eq 0 then begin
xy_id = { $
        plot : 0, $
        x0 : 0., $
        x1 : 0., $
        y0 : 0., $
        y1 : 0., $
        st : '' $
        }
        end

  XYCOORD_BASE = WIDGET_BASE(GROUP_LEADER=Group, $
	/COLUMN, MAP=1, $
      TITLE='XY-COORD', $
      UVALUE='XYCOORD_BASE')

  XY_COORD_MOTOR = WIDGET_LABEL(XYCOORD_BASE, $
	VALUE='Ref Positioner # '+strtrim(view1d_plotspec_id.xcord+1,2))

; x value 
  BASE3 = WIDGET_BASE(XYCOORD_BASE, ROW=1, MAP=1, UVALUE='BASE3')

  label_p1 = WIDGET_LABEL(BASE3,VALUE='X: ')
  XY_COORD_FIELD4 = WIDGET_TEXT( BASE3,VALUE='', $
      EDITABLE=1, UVALUE='XY_COORD_FIELD4', XSIZE=20)

; y value 
  BASE4 = WIDGET_BASE(XYCOORD_BASE, ROW=1, MAP=1, UVALUE='BASE4')
  label_d1 = WIDGET_LABEL(BASE4,VALUE='Y: ')
  XY_COORD_FIELD7 = WIDGET_TEXT( BASE4,VALUE='', $
      EDITABLE=1, UVALUE='XY_COORD_FIELD7', XSIZE=20)

; close button

  BUTTON10 = WIDGET_BUTTON( XYCOORD_BASE, $
      UVALUE='BUTTON10', $
      VALUE='Close')


xy_wid = { $
	base : XYCOORD_BASE, $ 
	motor : XY_COORD_MOTOR, $
	x : XY_COORD_FIELD4, $
	y : XY_COORD_FIELD7 $
	}

DEVICE,GET_SCREEN_SIZE=ssize

  WIDGET_CONTROL, XYCOORD_BASE, /REALIZE, $
	TLB_SET_XOFFSET= ssize(0)-200, TLB_SET_YOFFSET= 100

  XMANAGER, 'XYCOORD_BASE', XYCOORD_BASE
END
;
; this routine does an auto-scaled plot of the selected waveforms
;
PRO view1d_UPDATE_PLOT, auto, st

COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON V1D_realtime_block, view1d_realtime_id, realtime_retval, realtime_pvnames
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array , view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved
COMMON font_block, text_font, graf_font, ps_font
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus
COMMON w_statistic_block,w_statistic_ids


   WIDGET_CONTROL, view1d_widget_ids.wf_select, GET_VALUE = wf_sel
   x_axis = view1d_plotspec_id.x_axis_u

win_state = WIDGET_INFO(view1d_widget_ids.plot_wid, /GEOMETRY)

;   plotSubTitle = strtrim(view1d_plotspec_array(4))
   plotSubTitle = ''
   plotTitle=''
   plotYTitle=''
   plotXTitle =''


   num_pts = 1 > (V1D_scanData.act_npts-1)


   ;extract valid data from global arrays

   view1d_check_xaxis,num_pts,p1,xmin,xmax

; check any data available 
   if V1D_scanData.lastPlot lt 0 then begin 
	if xmax eq xmin then return 
	auto = 1
	end
 
if view1d_plotspec_id.log eq 0 and auto ne 0 then auto=1   
if view1d_plotspec_id.log eq 2 and auto ne 0 then auto=2   ;  Y > 0

   view1d_setPlotLabels                
   V1D_scanData.lastPlot = auto
   y_zero = 0 
   if auto eq 2 then y_zero = 1    ; exclude zero for auto scale

   IF (auto gt 0) THEN BEGIN     ;  auto scale
!y.style = 1
!x.style = 1

     ;if autoscale, determine appropriate Y values
     ;depending on which waveforms will be plotted
     ymin = 1.e20
     ymax = -1.e20
     err_dy = 0


for i=0,14 do begin

     IF (wf_sel(i) EQ 1) THEN  BEGIN

	d1 = V1D_scanData.da(0:num_pts,i)

         IF (MIN(d1) LT ymin) THEN ymin = MIN(d1)
         IF (MAX(d1) GT ymax) THEN ymax = MAX(d1)
	if sqrt(abs(ymax)) gt err_dy then err_dy = sqrt(abs(ymax))
	if sqrt(abs(ymin)) gt err_dy then err_dy = sqrt(abs(ymin))
     END

end

; add the support postioner as Y
for i=15,18 do begin
     IF (wf_sel(i) EQ 1) THEN  BEGIN
	d1 = V1D_scanData.pa(0:num_pts,i-15)
         IF (MIN(d1) LT ymin) THEN ymin = MIN(d1)
         IF (MAX(d1) GT ymax) THEN ymax = MAX(d1)
	if sqrt(abs(ymax)) gt err_dy then err_dy = sqrt(abs(ymax))
	if sqrt(abs(ymin)) gt err_dy then err_dy = sqrt(abs(ymin))
     END
end

; if error bar is on ajust ymin,ymax accordingly

	if view1d_plotspec_id.errbars  eq 1 then begin
		ymax = ymax + err_dy
		ymin = ymin - err_dy
		end

;
;  increase the xmin,xmax by +5%
;

	if auto gt 0 then view1d_adjust_ranges,xmin,xmax


; user scale auto=0
   ; if not autoscale, get limits from entry widgets. 

   ENDIF ELSE BEGIN
!y.style = 1
!x.style = 1
xmin = view1d_plotspec_limits(0)
xmax = view1d_plotspec_limits(1)
ymin = view1d_plotspec_limits(2)
ymax = view1d_plotspec_limits(3)
ENDELSE
     
     ;now determine xmin and xmax depending on x-axis selection

     IF (x_axis EQ 0) THEN BEGIN
       plotXTitle = strtrim(view1d_plotspec_array(1))
     ENDIF ELSE BEGIN
       xmin = 0
	xmax=num_pts
	if auto eq 1 then view1d_adjust_ranges,xmin,xmax
       plotXTitle = 'Step #'        
     ENDELSE

     if total(wf_sel) eq 0 then  plotXTitle = 'Nothing Selected' 

     if n_elements(view1d_plotspec_array) ne 0 then begin 
	if strlen(strtrim(view1d_plotspec_array(0))) gt 1 then $
	plotTitle = strtrim( view1d_plotspec_array(0))
	if strlen(view1d_plotspec_array(2)) gt 1 then $
	plotYTitle = strtrim(view1d_plotspec_array(2))
	end

   ;Now draw the axis and plot the selected waveforms


if !d.name eq !os.device then WSET, view1d_widget_ids.plot_area
;   ERASE

   ;fake out PLOT to plot an empty axis
   junk = ['5','6']

   ; If plotting before p1 was read ...
;   IF ((STRLEN(V1D_scanData.pv) EQ 0) OR  $    gives problem when no config
    IF (  (ymax LE ymin)             OR  $
       ((MIN(p1) EQ 0) AND (MAX(p1) EQ 0))) THEN  BEGIN
	p1=indgen(num_pts+1)
	xmin=0
	xmax=num_pts
	if auto eq 1 then view1d_adjust_ranges,xmin,xmax
   ENDIF

   ;Plot the axis w/o any waveforms

	POS=[0.15,0.2,0.78,0.85] 
	xticklen = view1d_plotspec_id.xticklen
	yticklen = view1d_plotspec_id.yticklen
	gridstyle = view1d_plotspec_id.gridstyle

;if V1D_scanData.act_npts ge V1D_scanData.req_npts then begin
; 
; linear plot
;
if view1d_plotspec_id.log ne 1 then begin


; 10 % margin

	if auto gt 0 then begin
        dy = 0.1 *(ymax-ymin)
        if dy eq 0 then begin
                if ymax eq 0 then  dy = 10 else dy = 0.05 * ymax
                end
        ymax = ymax + dy
        ymin = ymin - dy
	end

; auto scale but only plot y> 0 case

	if auto gt 1 then ymin = 0.

   PLOT, XRANGE = [xmin,xmax],             $
         YRANGE = [ymin,ymax],             $
         XTITLE = plotXTitle,               $
         YTITLE = plotYTitle,               $
	YNOZERO = y_zero, $
	XTICKLEN = xticklen, $
	YTICKLEN = yticklen, $
	XGRIDSTYLE = gridstyle, YGRIDSTYLE= gridstyle, $
	XMINOR = 10, $
        YMINOR = 10, $
         TITLE = plotTitle,               $
         SUBTITLE = plotSubTitle,               $
	POS=pos, $
;	FONT = graf_font, $
         MAX_VALUE = 0, junk
end

;
; log plot
;
if view1d_plotspec_id.log eq 1 then begin
if ymax le 0. then begin
	view1d_warningtext,'Data not suitable for YLOG plot.'
	view1d_plotoptionsmenu_set_string,18,19
	view1d_plotspec_id.log = 0
	return
	end

	if auto gt 0 then begin
		dy = .5 * (ymax-ymin)
		ymax = ymax + dy 
		ymin = ymin - dy
		end

	if ymin le 0. and ymax gt 1. then ymin = 1.

	if ymin eq ymax and ymax gt 0. then begin
		ymin = 0.1 * ymax
		ymax = 10. * ymax
		end

   PLOT, XRANGE = [xmin,xmax],             $
         YRANGE = [ymin,ymax],             $
         XTITLE = plotXTitle,               $
         YTITLE = plotYTitle,               $
	XTICKLEN = xticklen, $
	YTICKLEN = yticklen, $
	XGRIDSTYLE = gridstyle, YGRIDSTYLE= gridstyle, $
         TITLE = plotTitle,               $
         SUBTITLE = plotSubTitle,               $
	 XMINOR = 10, $
;	YMINOR=9,$
	YTYPE=1,$
	POS=pos, $
;	FONT = graf_font, $
         MAX_VALUE = 0, junk
end

y_descs = strtrim(y_descs,2)

st='Scan #: ' + strtrim(V1D_scanData.scanno)

is = 0
for i=0,14 do begin
   IF (wf_sel(i) EQ 1 and view1d_realtime_id.def(4+i) gt 0) THEN begin
	d1 = V1D_scanData.da(0:num_pts,i)
if view1d_plotspec_id.statistic eq 3 then begin
	getStatisticDeviation_1d,i,d1,moments,sdev,mdev,st1
        st = [st, st1]
	statis_value = [sdev,mdev,moments(0),moments(1)]
endif else if view1d_plotspec_id.statistic gt 0 then begin
	getStatistic_1d,i,p1,d1,c_mass,xpeak,ypeak,y_hpeak,FWHM,st1
        st = [st, st1]
	statis_value = [xpeak,c_mass,FWHM,ypeak]
end

       view1d_legends,pos,is,i,p1,d1,num_pts,x_axis,statis_value
	is = is + 1
        end
end

for i=15,18 do begin
   IF (wf_sel(i) EQ 1 and view1d_realtime_id.def(i-15) gt 0) THEN begin
        d1 = V1D_scanData.pa(0:num_pts,i-15)
if view1d_plotspec_id.statistic eq 3 then begin
        getStatisticDeviation_1d,i,d1,moments,sdev,mdev,st1
        st = [st, st1]
        statis_value = [sdev,mdev,moments(0),moments(1)]
endif else if view1d_plotspec_id.statistic gt 0 then begin
        getStatistic_1d,i,p1,d1,c_mass,xpeak,ypeak,y_hpeak,FWHM,st1
        st = [st, st1]
        statis_value = [xpeak,c_mass,FWHM,ypeak]
end
       view1d_legends,pos,is,i,p1,d1,num_pts,x_axis,statis_value
        is = is + 1
        end
end

if auto eq 1 and n_elements(st) gt 0  and view1d_widget_ids.statistic gt 1 then begin
	WIDGET_CONTROL,view1d_widget_ids.statistic,SET_VALUE=st,BAD_ID=bad_id,/NO_COPY
	if bad_id ne 0 then view1d_widget_ids.statistic = 0L
        end


;
; plot scan number + filename
;
	filenamepath,V1D_scanData.trashcan,F,P

	header_note='data file: ' + F

	xdis = 0.01 * !d.x_size
	ydis = !d.y_size - 1.2 * !d.y_ch_size
	xyouts,xdis,ydis,header_note,/device

	if V1D_scanData.y_scan gt 0 then begin
	xdis = 0.35 * !d.x_size
	ydis = !d.y_size - 1.2 * !d.y_ch_size
	if V1D_scanData.dataversion ne '' then $
	header_note = '2D SCAN # '+strtrim(V1D_scanData.scanno_2d,2)+' @ y('+strtrim(V1D_scanData.y_seqno,2)+')='+ $
		strtrim(V1D_scanData.y_value,2) $
	else $
	header_note = '2D scan # '+strtrim(V1D_scanData.scanno_2d,2)+' @ y('+strtrim(V1D_scanData.y_seqno,2)+')'
	xyouts,xdis,ydis,header_note,/device
	end

;	header_note =  'scan #: ' + strtrim(view1d_plotspec_id.seqno+1) 
	header_note =  'scan #: ' + strtrim(V1D_scanData.scanno) 
	xdis = 0.75 * !d.x_size
	ydis = !d.y_size - 1.2 * !d.y_ch_size
	xyouts,xdis,ydis,header_note,/device


	footer_note = strmid(strtrim(view1d_plotspec_array(4)),0,29)
	xdis = 0.01 * !d.x_size
	ydis = 1.2*!d.y_ch_size
	xyouts,xdis,ydis,footer_note,/device


	len = strlen( strtrim(view1d_plotspec_array(4)))
	footer_note = strmid(strtrim(view1d_plotspec_array(4)),30,len-30)
	xdis = 0.7 * !d.x_size
	ydis = 1.2*!d.y_ch_size
	xyouts,xdis,ydis,footer_note,/device


footer_note= 'comment: ' + strtrim(view1d_plotspec_array(5))
	view1d_ydist,(.01-pos(1)),ydis	
	xdis = 0.01 * !d.x_size
	ydis = 0.1*!d.y_ch_size
	xyouts,xdis,ydis,footer_note,/device


END


PRO view1d_check_xaxis,num_pts,p1,xmin,xmax
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array , view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved

; select the x-axis for plot

	i = view1d_plotspec_id.xcord
	if view1d_plotspec_id.xcord lt 4 then $
	   p1 = V1D_scanData.pa(0:num_pts,i) else $
	   p1 = V1D_scanData.da(0:num_pts,i-4)
	   xmin = MIN(p1)
	   xmax = MAX(p1)
	   if xmin eq xmax then $ 
		res=WIDGET_MESSAGE('Maybe no value available for P'+strtrim(i+1,2)+ ' x-axis vector',/info)

END


PRO view1d_xticks,xmin,xmax,XVAL

  XVAL = make_array(!X.TICKS+1,/float)
  DXVAL = (xmax - xmin)/ !X.TICKS 
  for i=0,!X.TICKS do begin
  XVAL(i) = xmin + i*DXVAL
  end
END
 
PRO view1d_yticks,ymin,ymax,YVAL
  YVAL = make_array(!Y.TICKS+1,/float)
  DYVAL = (ymax - ymin)/ !Y.TICKS 
  for i=0,!Y.TICKS do begin
  YVAL(i) = ymin + i*DYVAL
  end
END

PRO view1d_xdist,fact,xval
	dx = !x.window(1) - !x.window(0)
	if fact gt (1.-!x.window(0)) then begin
		print,'Error: ',-!x.window(0),' < fact < ',1 -!x.window(0)
		return
		end
	xval = !x.crange(0) + fact/dx * (!x.crange(1) - !x.crange(0)) 
END

PRO view1d_ydist,fact,yval
	dy = !y.window(1) - !y.window(0)
	if fact gt (1.-!y.window(0)) then begin
		print,'Error: ',-!y.window(0),' < fact < ',1 -!y.window(0)
		return
		end
	yval = !y.crange(0) + fact/dy * (!y.crange(1) - !y.crange(0)) 
END

PRO view1d_set_range,xmin,xmax,no
print,xmin,xmax,no
dx = (xmax-xmin)/no
xmin = xmin - 0.5 * dx
xmax = xmax + 0.5 * dx
i1 = fix(xmin/dx) - 1
i2 = fix(xmax/dx) + 1
xmin = i1*dx
xmax = i2*dx
print,xmin,xmax,no
END

PRO view1d_adjust_ranges,xmin,xmax
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array , view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved
 


;
;  increase the xmin,xmax by +5%
;
	if xmax eq xmin then dx = 1. else $
        dx = 0.05 *(xmax-xmin)
        xmax = xmax + dx
        xmin = xmin - dx

	view1d_round_xrange,xmin,xmax

END

;
; round the xrange to integer if total width > 5
;
PRO view1d_round_xrange,xmin,xmax
if (xmax - xmin) le 5. then return 
v = fix(xmax)
if (xmax - v) gt 0 then v = v+1
xmax=v
v = fix(xmin)
if xmin lt 0 then v = v - 1
xmin = v
END

PRO view1d_power10_max,x,newx,no
newx = x
if x lt 2. then return
v = fix(x)
;if (x-v) gt 0 then v = v+1

in1 = v / 10 + 1
ir1 = v - in1 *10
p = 1
if ir1 eq 0 then begin
	newx = v
	return
	end

if abs(in1) lt 10 then begin
	newx = in1 * 10^p + (1+ir1) *10^(p-1)
	no = p
	return
	end

REP:
        in2 = in1 /10
	ir2 = in1 - in2 *10
	p = p + 1
	if abs(in2) lt 10 then begin
		newx = in2 * 10^p + (1+ir2) *10^(p-1)
		no = p
		return
		end
	in1 = in2
	ir1 = ir2	
	goto, REP

END

PRO view1d_legends,pos,id1,id,p1,d1,num_pts,x_axis,statis_value

COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array , view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus

	view1d_xdist,(0.01 + pos(2)-pos(0)),xdis	
	view1d_xdist,(0.075+pos(2)-pos(0)),xdis2	

	ino = 5*id1
	ch_ratio = float(!d.y_ch_size) / !d.y_size
	view1d_ydist,(pos(3)-pos(1)-5*id1*ch_ratio),lydis	

	color = view1d_plotspec_id.colorI(id1)
	if !d.name eq 'PS' then color = 0

;if V1D_scanData.debug eq 1 then $
; print,"color,!d.n_colors:", color, !d.n_colors

	line = id1
	if view1d_plotspec_id.solid eq 1 and !d.name ne 'PS' then line = 0

if view1d_plotspec_id.type eq 0 then begin
      IF (x_axis EQ 0) THEN OPLOT, p1, d1, color=color, LINE = line , THICK=2 $
      ELSE OPLOT, d1, color=color, LINE = line, THICK=2
	; write legend 
	if view1d_plotspec_id.log ne 1 then $ 
	oplot,[xdis,xdis2],[lydis,lydis],color=color,LINE=line,/noclip else $
	oplot,[xdis,xdis2],[10^lydis,10^lydis],color=color,LINE=line,/noclip
	xdis = 0.8*!d.x_size
	xdis2 = 0.85*!d.x_size
	ydis = pos(3) * !d.y_size - 5 *id1*!d.y_ch_size
	if id lt 15 then begin
	   if strlen(y_descs(id)) gt 1 then $
		xyouts,xdis2,ydis,'  '+y_descs(id), /device else $
		xyouts,xdis2,ydis,'  Detector '+strtrim(id+1,1), /device
	endif else begin
	   idd = id-15
	   if strlen(x_descs(idd)) gt 1 then $
		xyouts,xdis2,ydis,'  '+x_descs(idd), /device else $
		xyouts,xdis2,ydis,'  Encoder P'+strtrim(idd+1,1), /device
	end
endif else begin
	sym = id1+1
	if view1d_plotspec_id.type eq 2 then sym = -(id1+1)
		IF (x_axis EQ 0) THEN OPLOT, p1, d1,color=color, PSYM = sym else $
		OPLOT, d1,color=color, PSYM = sym

	if view1d_plotspec_id.log ne 1 then $ 
		oplot,[xdis,xdis],[lydis,lydis],color=color,PSYM=sym,/noclip else $
		oplot,[xdis,xdis],[10^lydis,10^lydis],color=color,PSYM=sym,/noclip
	; write legend
	xdis = 0.8*!d.x_size
	xdis2 = 0.85*!d.x_size
	ydis = pos(3) * !d.y_size - 5 *id1*!d.y_ch_size
	if id lt 15 then begin
	   if strlen(y_descs(id)) gt 1 then  $
		xyouts,xdis2,ydis,'  '+y_descs(id),/device  else $
		xyouts,xdis2,ydis,'  Detector '+strtrim(id+1,1),/device
	endif else begin
	   idd = id-15
	   if strlen(x_descs(idd)) gt 1 then $
		xyouts,xdis2,ydis,'  '+x_descs(idd), /device else $
		xyouts,xdis2,ydis,'  Encoder P'+strtrim(idd+1,1), /device
	end
end

if view1d_plotspec_id.errbars eq 1 then begin 
	d_err = sqrt(abs(d1))
	for i=0, num_pts do begin
	x2 = p1(i)
	ny1 = d1(i) - d_err(i)
	ny2 = d1(i) + d_err(i)
      IF (x_axis EQ 0) THEN $
	OPLOT,color=color, [x2,x2],[ny1,ny2] else OPLOT,color=color,[i,i], [ny1,ny2] 
	end
end


if view1d_plotspec_id.statistic gt 0 then begin

	xpeak = statis_value(0)
	c_mass = statis_value(1)
	FWHM = statis_value(2)
	peak = statis_value(3)

desc_legend = make_array(4,/string)
if view1d_plotspec_id.statistic eq 3 then begin
	desc_legend(0) = 'Std Dev '
	desc_legend(1) = 'Ave Dev '
	desc_legend(2) = '  Mean  '
	desc_legend(3) = '  Vari  '
endif else begin
	desc_legend(0) = '  Peak @'
	desc_legend(1) = '  Cntr @'
	desc_legend(2) = '  FWHM '
	desc_legend(3) = '  Peak '
end

if n_elements(xpeak) gt 0 then begin
	ydis = pos(3) * !d.y_size - (ino+1)*!d.y_ch_size
	xyouts,xdis,ydis,desc_legend(0)+strtrim(xpeak,1),/device
	end

if n_elements(c_mass) gt 0 then begin
	ydis = pos(3) * !d.y_size - (ino+2)*!d.y_ch_size
	xyouts,xdis,ydis,desc_legend(1)+strtrim(c_mass,1) ,/device 
	end

if n_elements(FWHM) gt 0 then begin
	ydis = pos(3) * !d.y_size - (ino+3)*!d.y_ch_size
	xyouts,xdis,ydis,desc_legend(2)+strtrim(FWHM,1) ,/device
	end

if n_elements(peak) gt 0 then begin
	ydis = pos(3) * !d.y_size - (ino+4)*!d.y_ch_size
	xyouts,xdis,ydis,desc_legend(3)+strtrim(peak,1) ,/device
	end
end

END



PRO w_statistic_event,event
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON w_statistic_block,w_statistic_ids
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array , view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved

WIDGET_CONTROL, event.id, GET_UVALUE = eventval
CASE eventval OF
        "STATISTIC_CLOSE" : BEGIN
                WIDGET_CONTROL,event.top,/DESTROY
		view1d_widget_ids.statistic = 0L
		view1d_plotspec_id.statistic = 0
                END
ENDCASE
END


PRO w_statistic, str,width,height,title,quest=quest, GROUP = GROUP
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON w_statistic_block,w_statistic_ids

if n_elements(str) eq 0 then return
if XRegistered('w_statistic') ne 0 then begin
	WIDGET_CONTROL,w_statistic_ids.base,/DESTROY
	end
if n_elements(width) eq 0 then width = 80
if n_elements(height) eq 0 then height = 5 
if n_elements(title) eq 0 then title=''

w_statistic_base=WIDGET_BASE(TITLE = 'Catch1d '+ title, $
	TLB_FRAME_ATTR = 2, $
	 /COLUMN)
w_statistic_title = WIDGET_LABEL(w_statistic_base,VALUE=title)

list = WIDGET_TEXT(w_statistic_base,VALUE=str,UVALUE='LIST', $
	XSIZE =width, $
	YSIZE=height,/SCROLL)

close = WIDGET_BUTTON(w_statistic_base, $
                        VALUE = ' Close ', $
                        UVALUE = 'STATISTIC_CLOSE')

WIDGET_CONTROL, w_statistic_base,/REALIZE, $
	 TLB_SET_XOFFSET= 10, TLB_SET_YOFFSET= 400

view1d_widget_ids.statistic = list 
XMANAGER, 'w_statistic',w_statistic_base, GROUP_LEADER = GROUP

w_statistic_ids = { base : w_statistic_base }

END

PRO view1d_save_data_subset_dump,unit 
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData 
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array, view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved
COMMON V1D_realtime_block, view1d_realtime_id, realtime_retval, realtime_pvnames
COMMON LABEL_BLOCK,x_names,y_names,x_descs,y_descs,x_engus,y_engus

; get desc & eng units

	names = [x_names,y_names]
	descs = [x_descs,y_descs]
	engus = [x_engus,y_engus]

	no = n_elements(names)
	st = ';    I   '
        for i=0,no-1 do begin
        if view1d_realtime_id.def(i) ne 0 then begin
                st = st+ ' '+names(i)
                end
        end
	printf,unit,st

;s0 = string(replicate(32b,340))
twd = strlen(st) > 18*total(view1d_realtime_id.def) + 10
s0 = string(replicate(32b,twd))
st = s0
strput,st,';  (Desc)',0  &  ij = 17 
        for i=0,no-1 do begin
        if view1d_realtime_id.def(i) ne 0 then begin
                strput,st,descs(i),ij
                ij = ij + 18
                end
        end
printf,unit,st

st = s0
strput,st,'; (Units)',0  &  ij = 17 
        for i=0,no-1 do begin
        if view1d_realtime_id.def(i) ne 0 then begin
                strput,st,engus(i),ij
                ij = ij + 18
                end
        end
printf,unit,st

num_npts = V1D_scanData.readin_npts

temp_format = '('+V1D_scanData.code+V1D_scanData.format+')'
temp_digit = fix(V1D_scanData.format)

for i=0,num_npts-1 do begin
st = s0
strput,st,i,0  &  ij = 10
	for j = 0,3 do begin
		if view1d_realtime_id.def(j) ne 0 then begin
		strput,st,string(V1D_scanData.pa(i,j),format=temp_format),ij  
		ij = ij + temp_digit
		end
	end
	for j = 0,14 do begin
		if view1d_realtime_id.def(4+j) ne 0 then begin
		strput,st,string(V1D_scanData.da(i,j),format=temp_format),ij  
		ij = ij + temp_digit
		end
	end
printf,unit,st
end

END

PRO view1d_scan_read_all,unit,maxno
COMMON view1d_viewscan_block, view1d_viewscan_ids, view1d_viewscan_id

	status = FSTAT(unit)

	indexFile = status.name + '.index'
	size = status.size

	view1d_viewscan_id.file = status.name

; check whether indexFile exist
found = findfile(indexFile)
if found(0) eq '' then begin
	id = 0
	view1d_viewscan_id.fptr = make_array(10000,/long)

	WHILE NOT  EOF(unit) DO BEGIN
	id = id + 1
		scan_read_record,unit
		point_lun,-unit,pos
		view1d_viewscan_id.fptr(id) = pos
	END
	maxno = id	
	view1d_viewscan_id.maxno = maxno
endif else begin

; check file size change

if view1d_viewscan_id.file ne status.name then begin

	id = 0
	view1d_viewscan_id.fptr = make_array(10000,/long)

	WHILE NOT  EOF(unit) DO BEGIN
	id = id + 1
		scan_read_record,unit
		point_lun,-unit,pos
		view1d_viewscan_id.fptr(id) = pos
	END
	maxno = id	
	view1d_viewscan_id.maxno = maxno

endif else begin

	if size gt view1d_viewscan_id.size then begin

	id = view1d_viewscan_id.maxno
	point_lun,unit,view1d_viewscan_id.size
        view1d_viewscan_id.fptr(id) = view1d_viewscan_id.size 

	WHILE NOT  EOF(unit) DO BEGIN
	id = id + 1
		scan_read_record,unit
		point_lun,-unit,pos
		view1d_viewscan_id.fptr(id) = pos
	END
	maxno = id	
	view1d_viewscan_id.maxno = maxno
	endif else maxno = view1d_viewscan_id.maxno 
end
end
	view1d_viewscan_id.seqno = 0
	view1d_viewscan_id.size = status.size
	view1d_viewscan_id.file = status.name

END
; 
; save data only for curr scan record 
; 
PRO view1d_mere_data_dump, unit
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData 
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array, view1d_plotspec_id, view1d_plotspec_limits , view1d_plotspec_saved
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids
COMMON env_field_block,env_field

if V1D_scanData.y_seqno gt 0 or V1D_scanData.y_scan gt 0 then begin
printf,unit,"; 2D SCAN #: ",V1D_scanData.scanno_2d,",      Y INDEX #:",V1D_scanData.y_seqno
        end

printf,unit,"; 1D SCAN #: ",V1D_scanData.scanno
view1d_save_data_subset_dump, unit 
printf,unit,' '

END
; 
; save ascii file of curr scan record 
; 
PRO view1d_shortreport_data_dump, unit
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData 
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array, view1d_plotspec_id, view1d_plotspec_limits , view1d_plotspec_saved
COMMON V1D_realtime_block, view1d_realtime_id, realtime_retval, realtime_pvnames
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids
COMMON env_field_block,env_field

printf,unit,"; VERSION: ",V1D_scanData.version,' ',V1D_scanData.release
if V1D_scanData.y_seqno gt 0 or V1D_scanData.y_scan gt 0 then begin
printf,unit,"; 2D SCAN #: ",V1D_scanData.scanno_2d,",      Y INDEX #:",V1D_scanData.y_seqno
        end

printf,unit,"; 1D SCAN #: ",V1D_scanData.scanno
printf,unit,"; SCAN Record Name: ",V1D_scanData.pv

no = env_field.numkey 
if no gt 0 then begin
printf,unit,'; '
printf,unit,';  KEY PV names got from the catch1d.env'
printf,unit,'; '
;s0 = string(replicate(32b,340))
twd = 18*total(view1d_realtime_id.def) + 10
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
printf,unit,"; Saved in:   ",V1D_scanData.path+view1d_plotspec_array(3)
printf,unit,"; Time Stamp: ",view1d_plotspec_array(4)
printf,unit,"; Comment:    ",view1d_plotspec_array(5)
printf,unit,'; '
printf,unit,'; SCAN Data:'
printf,unit,'; '

view1d_save_data_subset_dump, unit 

printf,unit,' '

END

; 
; save ascii file of a scan record 
; 
PRO view1d_summary_report_dump,filename,outfile,start,stop,header 
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array, view1d_plotspec_id, view1d_plotspec_limits , view1d_plotspec_saved
COMMON view1d_viewscan_block, view1d_viewscan_ids, view1d_viewscan_id

tempname=outfile

; position record to the startno
	if V1D_scanData.XDR eq 1 then U_OPENR,unit1,filename,/XDR else $
	U_OPENR,unit1,filename

	if start gt 0 then point_lun,unit1,view1d_viewscan_id.fptr(start-1)

end_unit1:
if EOF(unit1) then begin
                print,'EOF! Last record is',i-1
                u_close, unit1
                return
	end

openw,unit2,tempname,/get_lun 

for i=start, stop do begin
	if not EOF(unit1) then begin
	view1d_scan_read,unit1,i,i	
	V1D_scanData.scanno = i
	if header eq 0 then view1d_report_data_dump,unit2
	if header eq 1 then view1d_shortreport_data_dump,unit2
	if header eq 2 then view1d_mere_data_dump,unit2
	endif else begin
		print,'EOF!  Last scan # is',i-1
		goto, end_loop 
	end
end

end_loop:
	u_close,unit2
	u_close,unit1

END


PRO view1d_report_data_dump,unit
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData 
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array, view1d_plotspec_id, view1d_plotspec_limits , view1d_plotspec_saved
COMMON V1D_realtime_block, view1d_realtime_id, realtime_retval, realtime_pvnames
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids
COMMON env_field_block,env_field


printf,unit,"; VERSION: ",V1D_scanData.version,' ',V1D_scanData.release
if V1D_scanData.y_seqno gt 0 or V1D_scanData.y_scan gt 0 then begin
printf,unit,"; 2D SCAN #: ",V1D_scanData.scanno_2d,",      Y INDEX #:",V1D_scanData.y_seqno
        end

printf,unit,"; 1D SCAN #: ",view1d_plotspec_id.seqno + 1 
printf,unit,"; SCAN Record Name: ",V1D_scanData.pv


no = env_field.no             
if env_field.exist gt 0 then begin
; in summary report generation the env_field.no is the actual 
; environment values written for the case

if no gt 0 then begin
printf,unit,'; '
if V1D_scanData.scanno gt V1D_scanData.refno then $
printf,unit,';  REFERENCE ENVIRONMENT VARIABLES SAVED IN SCAN #',V1D_scanData.refno
printf,unit,';  ENVIRONMENT VARIABLES SAVED FOR SCAN #:', V1D_scanData.scanno
printf,unit,'; '
;s0 = string(replicate(32b,340))
twd = 18*total(view1d_realtime_id.def) + 10
s0 = string(replicate(32b,twd))
st = s0
strput,st,'; ',0
strput,st,'PVNAME',2
strput,st,'VALUE',30
strput,st,'DESCRIPTION',60
printf,unit,st
printf,unit,';'
	for i=0,no-1 do begin
	if i eq env_field.noenv then printf,unit,';'
	st = s0
	strput,st,'; ',0
	strput,st,env_field.pvnames(i),2
	strput,st,env_field.values(i),30
	strput,st,env_field.descs(i),60
	printf,unit,st
	end
end
endif else begin
if V1D_scanData.scanno gt V1D_scanData.refno then begin
printf,unit,'; '
printf,unit,';  ENVIRONMENT VARIABLES ARE SAME AS THE PREVIOUS SCAN, OR '
printf,unit,';  THE ENVIRONMENT FILE  catch1d.env DOES NOT EXIST.'
printf,unit,'; '
	end
end

if V1D_scanData.scanno eq V1D_scanData.refno then begin
printf,unit,'; '
	for i=no,env_field.no-1 do begin
	st = s0
	strput,st,'; ',0
	strput,st,env_field.pvnames(i),2
	strput,st,env_field.values(i),30
	strput,st,env_field.descs(i),60
	printf,unit,st
	end
end

printf,unit,'; '
printf,unit,"; PLOT SPECIFICATIONS"
printf,unit,'; '
printf,unit,"; Title:      ",view1d_plotspec_array(0)
printf,unit,"; X Label:    ",view1d_plotspec_array(1)
printf,unit,"; Y Label:    ",view1d_plotspec_array(2)
printf,unit,"; Saved in:   ",V1D_scanData.path+view1d_plotspec_array(3)
printf,unit,"; Time Stamp: ",view1d_plotspec_array(4)
printf,unit,"; Comment:    ",view1d_plotspec_array(5)

if view1d_plotspec_id.type eq 0 then printf,unit,"; Type:      Line"
if view1d_plotspec_id.type eq 1 then printf,unit,"; Type:      Point"
if view1d_plotspec_id.type eq 2 then printf,unit,"; Type:      Line/Point"
if view1d_plotspec_id.log  eq 0 then printf,unit,"; Y Scale:   Linear"
if view1d_plotspec_id.log  eq 1 then printf,unit,"; Y Scale:   Log"
if view1d_plotspec_id.errbars eq 0 then printf,unit,"; Errbars:   Off"
if view1d_plotspec_id.errbars eq 1 then printf,unit,"; Errbars:   On"
printf,unit,'; Realtime: itime=',view1d_plotspec_id.itime, ',  dtime=',view1d_plotspec_id.dtime
printf,unit,'; Plot Vs Position Array # ',view1d_plotspec_id.xcord + 1

printf,unit,'; '
printf,unit,'; SCAN Data:'
printf,unit,'; '

 view1d_save_data_subset_dump, unit

printf,unit,' '

END


PRO view1d_summary_setup_Event, GROUP=GROUP, Event
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON view1d_viewscan_block, view1d_viewscan_ids, view1d_viewscan_id
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array, view1d_plotspec_id, view1d_plotspec_limits , view1d_plotspec_saved
COMMON view1d_summary_block, view1d_summary_ids, view1d_summary_id

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'summary_header': BEGIN
      CASE Event.Value OF
      0: view1d_summary_id.header = 0
      1: view1d_summary_id.header = 1
      2: view1d_summary_id.header = 2
      ELSE: Message,'Unknown button pressed'
      ENDCASE
      END
  'summary_format': BEGIN
      Print, 'Event for output format'
	WIDGET_CONTROL, view1d_summary_ids.format, GET_VALUE=format
	format = strcompress(format(0),/remove_all)
	V1D_scanData.code = strmid(format,0,1)
	V1D_scanData.format = strmid(format,1,10)
        ret = strpos('defgDEFG',V1D_scanData.code)
       if ret eq -1 then view1d_warningtext,'Error:   illegal format entered !!!'
	END
  'summary_file': BEGIN
      Print, 'Event for Filename'
	WIDGET_CONTROL, view1d_summary_ids.file, GET_VALUE=file 
	filename=strcompress(file(0),/remove_all)
	found = findfile(filename)
	if found(0) ne '' then begin	
	view1d_summary_id.file = filename

if V1D_scanData.XDR eq 1 then U_OPENR,unit,view1d_summary_id.file,/XDR else $
U_OPENR,unit,view1d_summary_id.file
view1d_scan_read_all,unit,maxno
free_lun,unit
view1d_summary_id.start = maxno
view1d_summary_id.stop = maxno

	WIDGET_CONTROL,view1d_summary_ids.start, $ 
		SET_VALUE=strtrim(view1d_summary_id.start,2)
	WIDGET_CONTROL,view1d_summary_ids.stop, $
		SET_VALUE=strtrim(view1d_summary_id.stop,2)
	view1d_report_setup
	outfile = view1d_summary_id.outfile
	WIDGET_CONTROL, view1d_summary_ids.outfile, SET_VALUE= outfile
	end
      END
  'summary_start': BEGIN
	WIDGET_CONTROL,view1d_summary_ids.start, GET_VALUE=start
	if start gt 0 and start le view1d_viewscan_id.maxno then begin
	view1d_summary_id.start = start
	view1d_report_setup
	WIDGET_CONTROL, view1d_summary_ids.outfile, SET_VALUE= view1d_summary_id.outfile
	endif else view1d_warningtext,['Error: can not exceed '+ string(view1d_viewscan_id.maxno) ]
      END
  'summary_end': BEGIN
	WIDGET_CONTROL,view1d_summary_ids.stop, GET_VALUE=stop
	if stop gt 0 and stop le view1d_viewscan_id.maxno then begin
	view1d_summary_id.stop = stop
	view1d_report_setup
	WIDGET_CONTROL, view1d_summary_ids.outfile, SET_VALUE= view1d_summary_id.outfile
	endif else begin
	  view1d_warningtext,['Error: can not exceed '+ string(view1d_viewscan_id.maxno),$
		'       Reset to '+string(view1d_viewscan_id.maxno) ]
          WIDGET_CONTROL,view1d_summary_ids.stop, SET_VALUE=view1d_viewscan_id.maxno
	end
      END
  'summary_separate': BEGIN
	view1d_summary_ids.separate = Event.Index
	END
  'summary_ok': BEGIN
	WIDGET_CONTROL, view1d_summary_ids.format, GET_VALUE=format
	format = strcompress(format(0),/remove_all)
	V1D_scanData.code = strmid(format,0,1)
	V1D_scanData.format = strmid(format,1,10)

	WIDGET_CONTROL, view1d_summary_ids.view, SENSITIVE = 0 
	WIDGET_CONTROL, view1d_summary_ids.print, SENSITIVE = 0 
	WIDGET_CONTROL,view1d_summary_ids.start, GET_VALUE=start
	WIDGET_CONTROL,view1d_summary_ids.stop, GET_VALUE=stop
	if stop lt start then stop=start
	view1d_summary_id.start = start
	view1d_summary_id.stop = stop

;	WIDGET_CONTROL,view1d_summary_ids.file, GET_VALUE=file
;	filename=strcompress(file(0),/remove_all)
	filename = view1d_summary_id.file
	found = findfile(filename)
if found(0) ne '' then  begin	
	view1d_summary_id.file = filename
	WIDGET_CONTROL,view1d_summary_ids.outfile, GET_VALUE=file
	view1d_summary_id.outfile=strcompress(file(0),/remove_all)

	;  use the data directory first if failed then home directory

	report_path = V1D_scandata.path
	save_outfile = V1D_scandata.path+view1d_summary_id.outfile
	CATCH,error_status
	if error_status eq -171 then begin
		report_path = V1D_scandata.home + !os.file_sep 
		save_outfile = report_path+view1d_summary_id.outfile
		goto, RESETSENSE
	end
	openw,unit,save_outfile,/get_lun
	u_close,unit
RESETSENSE:

; save as one big file
    if view1d_summary_ids.separate eq 0 then begin
		view1d_plotspec_id.mode = 1
		view1d_summary_report_dump,filename,save_outfile,start,stop,view1d_summary_id.header
		view1d_plotspec_id.mode = 0

	if V1D_scanData.debug eq 1 then $
		print, 'Report file: ', save_outfile,' created!'
; save as separate files
     endif else begin
	str = '0000'
	len0 = 4 
	view1d_plotspec_id.mode = 1
	for i=start,stop do begin
	sss = str
	st = strtrim(i,2)
	len = strlen(st)
	strput,sss,st,len0-len
		save_outfile=report_path+view1d_plotspec_array(3)+'.'+sss
		view1d_summary_report_dump,filename,save_outfile,i,i,view1d_summary_id.header
	if V1D_scanData.debug eq 1 then print,save_outfile
	view1d_plotspec_id.mode = 0
	end
     end

endif else view1d_warningtext,'Error:  Data file " '+filename+' " not found!'
;	WIDGET_CONTROL, view1d_summary_ids.base , /DESTROY
	WIDGET_CONTROL, view1d_summary_ids.view, SENSITIVE = 1 
	WIDGET_CONTROL, view1d_summary_ids.print, SENSITIVE = 1 

      END
  'summary_view': BEGIN
	WIDGET_CONTROL,view1d_summary_ids.outfile, GET_VALUE=file
	filename=strcompress(file(0),/remove_all)

	; check the data directory first

	view1d_summary_id.outfile = V1D_scanData.path + filename
	found = findfile(view1d_summary_id.outfile)
	if found(0) ne '' then begin
	  xdisplayfile,view1d_summary_id.outfile,width=110,GROUP=event.top 
  		return
 	end

	; check startup directory

	found = findfile(filename)
	if found(0) ne '' then 	$
        xdisplayfile,filename,width=110,GROUP=event.top else $
	view1d_warningtext,['Error:','    '+filename+ '  not found!']
	END

  'summary_print': BEGIN

	WIDGET_CONTROL,view1d_summary_ids.outfile, GET_VALUE=file
	filename=strcompress(file(0),/remove_all)

	; check the data directory first

	view1d_summary_id.outfile = V1D_scanData.path + filename
	found = findfile(view1d_summary_id.outfile)
	if found(0) ne '' then begin
	 if !os.os_family eq 'unix' then $
       	 spawn,[!os.prt, !os.printer, '-r', view1d_summary_id.outfile], /noshell else $
	 spawn,[!os.prt,view1d_summary_id.outfile]
		return
	end

	; check startup directory

	found = findfile(filename)
	if found(0) ne '' then 	begin 
	 if !os.os_family eq 'unix' then $
         spawn,[!os.prt, !os.printer, '-r', filename], /noshell else $
	 spawn,[!os.prt,filename]
	endif else $
	view1d_warningtext,['Error:','    '+filename+ '  not found!']
	END
  'summary_cancel': BEGIN
	WIDGET_CONTROL, view1d_summary_ids.base , /DESTROY
      END
  ENDCASE
END

PRO view1d_report_setup
COMMON view1d_summary_block, view1d_summary_ids, view1d_summary_id

str = '0000'
len0 = strlen(str)
sss = str
st = strtrim(view1d_summary_id.start,2)
len = strlen(st)
strput,sss,st,len0-len

filenamepath, view1d_summary_id.file, file, path
view1d_summary_id.outfile = file+'.'+sss

if view1d_summary_id.stop gt view1d_summary_id.start then begin
eee = str
st = strtrim(view1d_summary_id.stop,2)
len = strlen(st)
strput,eee,st,len0-len
view1d_summary_id.outfile = view1d_summary_id.outfile+ '_'+eee
end

END

PRO view1d_update_summary_setup,Event
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData 
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array , view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved

if XRegistered('w_plotspec') ne 0 then begin
	WIDGET_CONTROL,view1d_plotspec_ids.base,/DESTROY
	end

if XRegistered('view1d_summary_setup') eq 0 then return 

        WIDGET_CONTROL,view1d_widget_ids.summary,/DESTROY,BAD_ID=b    
        view1d_widget_ids.summary = 0L
	
	view1d_summary_setup,GROUP=Event.top
END

PRO view1d_summary_setup, GROUP=Group
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData 
COMMON view1d_summary_block, view1d_summary_ids, view1d_summary_id
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array , view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved
COMMON view1d_viewscan_block, view1d_viewscan_ids, view1d_viewscan_id


if XRegistered('view1d_summary_setup') ne 0 then return

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

view1d_summary_id = { $
	start: 1, $
	stop: 1, $
	header: 0, $
	outfile: view1d_plotspec_array(3)+'.rep',  $
	file: V1D_scanData.trashcan  $
	}

seqno = view1d_viewscan_id.seqno + 1
if seqno gt view1d_viewscan_id.maxno then seqno = view1d_viewscan_id.maxno
view1d_summary_id.start = seqno
view1d_summary_id.stop = seqno

  view1d_report_setup

  view1d_summary_base = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
 TITLE = 'Report ...', $
      UVALUE='view1d_summary_base')

  BASE2 = WIDGET_BASE(view1d_summary_base, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  LABEL3 = WIDGET_LABEL( BASE2, $
      UVALUE='LABEL3', $
      VALUE='SUMMARY ASCII REPORT')

Btns220 = [ 'Full', 'Abbreviated', 'None' ]
  summary_header = CW_BGROUP( BASE2, Btns220, $
      ROW=1, $
      EXCLUSIVE=1, $
      LABEL_LEFT='Header options:', $
      UVALUE='summary_header')
WIDGET_CONTROL,summary_header,SET_VALUE=0

  summary_format = CW_FIELD( BASE2,VALUE=V1D_scanData.code+V1D_scanData.format, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS= 1, $
      TITLE='Ouput Data Format: ', $
      XSIZE=8, $
      UVALUE='summary_format')

  summary_file = WIDGET_LABEL( BASE2,/ALIGN_LEFT, $
		VALUE='Source: '+view1d_summary_id.file)

;  summary_file = CW_FIELD( BASE2,VALUE=view1d_summary_id.file, $
;      ROW=1, $
;      STRING=1, $
;      NOEDIT=1, $
;      RETURN_EVENTS= 1, $
;      TITLE='Data file name: ', $
;      XSIZE=60, $
;      UVALUE='summary_file')

  FieldVal388 = strtrim(view1d_summary_id.start,2)
  summary_start = CW_FIELD( BASE2,VALUE=FieldVal388, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS= 1, $
      TITLE='Start scan #    ', $
      UVALUE='summary_start')

  FieldVal465 = strtrim(view1d_summary_id.stop,2)
  summary_end = CW_FIELD( BASE2,VALUE=FieldVal465, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS= 1, $
      TITLE='End scan #      ', $
      UVALUE='summary_end')

  summary_separate = WIDGET_DROPLIST(BASE2, VALUE=['No', 'Yes'], $
        UVALUE='summary_separate',TITLE='Save Selected Scans as Separate ASCII Files')
  WIDGET_CONTROL,summary_separate,set_droplist_select = 0

  BASE112 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE112')

;  label = WIDGET_LABEL(BASE112,VALUE='Out Report File: ')
;  summary_outfile = WIDGET_LABEL(BASE112,VALUE=view1d_summary_id.outfile)

  summary_outfile = CW_FIELD( BASE2,VALUE=view1d_summary_id.outfile, $
      ROW=1, XSIZE=60, $
      TITLE='Output file name: ', $
      UVALUE='summary_outfile')

  BASE12 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE12')

  summary_ok = WIDGET_BUTTON( BASE12, $
      UVALUE='summary_ok', $
      VALUE='Generate Report')

  summary_view = WIDGET_BUTTON( BASE12, $
      UVALUE='summary_view', $
      VALUE='View Report')

  summary_print = WIDGET_BUTTON( BASE12, $
      UVALUE='summary_print', $
      VALUE='Print Report')

  summary_cancel = WIDGET_BUTTON( BASE12, $
      UVALUE='summary_cancel', $
      VALUE='Done')


; set widget ids:

view1d_summary_ids = { $
	base: view1d_summary_base, $
	format: summary_format, $
;	file: summary_file, $
	outfile: summary_outfile, $
	view: summary_view, $
	print: summary_print, $
	separate: 0, $
	start: summary_start, $
	stop: summary_end $
	}

  view1d_widget_ids.summary = view1d_summary_base

  WIDGET_CONTROL, view1d_summary_base, /REALIZE

  XMANAGER, 'view1d_summary_setup', view1d_summary_base, GROUP_LEADER = GROUP 
END

;
; view1d_drv.pro
;
 
; @u_read.pro
; @PS_open.pro
; @my_box_cursor.pro
; @cw_term.pro
; @xdisplayfile.pro
; @view1d_util.pro
; @view1d_eventLib.pro
; @view1d_plot.pro
; @view1d_summary.pro


PRO read_desc_engu,labels
COMMON LABEL_BLOCK,x_names,y_names,x_descs,y_descs,x_engus,y_engus

if n_elements(x_names) lt 4 then begin
	x_names = make_array(4,/string,value=string(replicate(32b,30)))
	x_descs = x_names
	x_engus = x_names
	y_names = make_array(15,/string,value=string(replicate(32b,30)))
	y_descs = y_names
	y_engus = y_names
	end
labels = string(labels)
for i=0,3 do begin 
	x_names(i) = labels(i)
	x_descs(i) = labels(i+19)
	x_engus(i) = labels(i+38)
end
for i=0,14 do begin 
	y_names(i) = labels(i+4)
	y_descs(i) = labels(i+4+19)
	y_engus(i) = labels(i+4+38)
end

END


;
; if seq_no + 1 = id will plot
;
PRO view1d_scan_read,unit,seq_no,id
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON V1D_realtime_block, view1d_realtime_id, realtime_retval, realtime_pvnames
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array, view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved
COMMON view1d_viewscan_block, view1d_viewscan_ids, view1d_viewscan_id
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids
COMMON env_field_block,env_field
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus

if n_params() lt 2 then begin
        view1d_warningtext,'usage:   view1d_scan_read, unit, seq_no, id'
        return
        end

if EOF(unit) eq 1 then begin
	res=WIDGET_MESSAGE('end of file reached ! ',/info)
	u_rewind,view1d_viewscan_id.unit
	id = 0
	return
	end

; 
next_seq_no = seq_no + 1
	u_read,unit,version
	if version(0) ne V1D_scanData.version then begin
                s0 = 'Error: data version is '+ version(0)
                s1 = '       Current version is ' + V1D_scanData.version
                st = [s0,s1]
                view1d_warningtext,st
                return
                end

	u_read,unit,pv
	u_read,unit,num_pts
	u_read,unit,id_def
	u_read,unit,x_dpt

	view1d_realtime_id.def = id_def
	V1D_scanData.pa = make_array(4000,4,/double)
	V1D_scanData.da = FLTARR(4000,15)

	num_pts = fix(num_pts(0))

for i=0,3 do begin
        if view1d_realtime_id.def(i) gt 0 then begin
	u_read,unit,px
	V1D_scanData.pa(0:num_pts,i) = px
        end
end

for i=0,14 do begin
        if view1d_realtime_id.def(4+i) gt 0 then begin
	u_read,unit,px
	V1D_scanData.da(0:num_pts,i) = px
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
        V1D_scanData.refno = fix(y(1))
        V1D_scanData.y_seqno = fix(y(2))
        V1D_scanData.scanno_2d = fix(y(3))
	if no ge 5 then V1D_scanData.y_scan = fix(y(4))
	if no eq 8 then begin
		V1D_scanData.y_value = y(7)
		V1D_scanData.dataversion = V1D_scanData.release
		endif else V1D_scanData.dataversion = ''
        endif else V1D_scanData.refno = fix(y(0)) + 1

view1d_plotspec_id.seqno = fix(y(0))
view1d_viewscan_id.seqno = fix(y(0))


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
temp_name = view1d_plotspec_array(3)
s1 = n_elements(x)
for i=0,s1-1  do view1d_plotspec_array(i)=x(i) 
view1d_plotspec_array(3) = temp_name

;field_value = make_array(field_max,/string,value=string(replicate(32b,40)))
;for i=0,field_max-1 do field_value(i)=strtrim(ze(i+env_field.noenv))

   ;populate read data into global arrays

V1D_scanData.pv = string(pv(0))
V1D_scanData.act_npts = num_pts + 1

if id eq next_seq_no or next_seq_no le 0 then begin
	if V1D_scanData.debug eq 1 then $
	print,'Scan # ',seq_no, ' accessed.'
	V1D_scanData.scanno = seq_no
;	view1d_setDefaultLabels
	view1d_UPDATE_PLOT, V1D_scanData.lastPlot
	id = next_seq_no
	end

V1D_scanData.readin_npts=V1D_scanData.act_npts

END



; add the support of index file this should improve the
; perfomance of data catcher
;

PRO view1d_readFileIndex,filename
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON view1d_viewscan_block, view1d_viewscan_ids, view1d_viewscan_id

; check whether filename exists

fd = findfile(filename)
IF fd(0) NE '' THEN BEGIN
	
	indexfile = filename+'.index'

found = findfile(indexfile)
if found(0) ne '' then begin
	if V1D_scanData.XDR eq 1 then U_OPENR,unit,indexfile,/XDR  else $
	U_OPENR,unit,indexfile
	u_read,unit,name
	u_read,unit,fsize
	u_read,unit,maxno
	u_read,unit,array
	u_close,unit

	openr,1,filename
	status = FSTAT(1)
	close,1
	
	if status.size eq fsize(0) then begin
	view1d_viewscan_id.size = fsize(0)
	view1d_viewscan_id.maxno = maxno(0)
;	view1d_viewscan_id.fptr(0:view1d_viewscan_id.maxno) = array
	view1d_viewscan_id.fptr = array
	end
;	print,'***Read Index File: ',indexfile

endif else begin
	view1d_writeFileIndex,filename
end

ENDIF ELSE BEGIN
	res=WIDGET_MESSAGE('Warning: file "' + filename + '" not found.',/info)
END 
END


PRO view1d_writeFileIndex,filename
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON view1d_viewscan_block, view1d_viewscan_ids, view1d_viewscan_id

; check file existence

found = findfile(filename)
if found(0) eq '' then return
	openr,1,filename
	status = FSTAT(1)
	close,1
	
	if filename eq view1d_viewscan_id.file then begin

	if view1d_viewscan_id.maxno gt 0 then begin

	indexfile = view1d_viewscan_id.file + '.index'
	CATCH,error_status
	if error_status lt 0 then return
	if V1D_scanData.XDR eq 1 then U_OPENW,unit,indexfile,/XDR else $
	U_OPENW,unit,indexfile

	array = view1d_viewscan_id.fptr(0:view1d_viewscan_id.maxno)
	array(view1d_viewscan_id.maxno) = status.size

	u_write,unit,status.name
	u_write,unit,status.size
	u_write,unit,view1d_viewscan_id.maxno
	u_write,unit,array
	u_close,unit

;print,'***File ',indexfile,' updated.'
	end
	end
END

PRO view1d_read_config,filename
COMMON VIEW1D_COM,  view1d_widget_ids, V1D_scanData
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array, view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved

file = V1D_scanData.config
if n_elements(filename) gt 0 then file = string(filename)

found = findfile(file)
if found(0) eq '' then return
openr,unit,file,/get_lun 

st=''
while(not eof(unit)) do begin
readf,unit,st
;print,st
	key_st=''
	new_st=''
	; get key variable name defined

	ln1 = strpos(st,"=")
	if ln1 gt -1 then begin
		key_st = strmid(st,0,ln1)

	; get string specification

	ln1 = strpos(st,"'")
	if ln1 gt -1 then begin
		new_st = strmid(st,ln1,strlen(st) - ln1)
		ln2 = strpos(new_st,"'",1)
		if ln2 eq -1 then $ 
		new_st = strmid(new_st,1,strlen(new_st)-1) $
		else if ln2 eq 1 then new_st = '' else $
		new_st = strmid(new_st,1,ln2-1)
	end

	CASE key_st OF 
	'scanData.pv' : V1D_scanData.pv = new_st
	'scanData.y_pv' : V1D_scanData.y_pv = new_st
	'scanData.pvwait' : V1D_scanData.pvwait = new_st
	'scanData.pvbusy' : V1D_scanData.pvbusy = new_st
	'scanData.y_handshake' : V1D_scanData.y_handshake = new_st
	'scanData.path' : V1D_scanData.path = new_st
	'scanData.trashcan' : view1d_plotspec_array(3) = new_st
	'scanData.envfile' : V1D_scanData.envfile = new_st
	'scanData.config' : V1D_scanData.config = new_st
	'scanData.option' : V1D_scanData.option = new_st
	'scanData.nosave' : V1D_scanData.nosave = new_st
	'scanData.debug' : V1D_scanData.debug = new_st
	ELSE :
	ENDCASE
	end

end
free_lun,unit

END

PRO catcher_view1d_Init

COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON V1D_realtime_block, view1d_realtime_id, realtime_retval, realtime_pvnames
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array, view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved
COMMON view1d_viewscan_block, view1d_viewscan_ids, view1d_viewscan_id
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids
COMMON env_field_block,env_field
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus

; check for demo mode

	if demo_mode() eq 1 then begin
	str = ['Warning: Not able to save file in Demo mode !!!' ]
	res=WIDGET_MESSAGE(str,/info)
	end

; read in configuration file if XDR eq 0 

; read in go_catcher2 for runtime version

if V1D_scanData.XDR eq 0 then begin
found = findfile('go_catcher2')
if found(0) ne '' then view1d_read_config,'go_catcher2'

found = findfile('catch1d.config.tmp')
if found(0) ne '' then view1d_read_config,'catch1d.config.tmp' else view1d_read_config
end

V1D_scanData.pvconfig = V1D_scanData.pv
if V1D_scanData.debug eq 1 then begin
print,V1D_scanData.home
print,V1D_scanData.path
end

; override the data file on command line by the setting in 
; the configuration file

	if strlen(view1d_plotspec_array(3)) then $
	V1D_scanData.trashcan = V1D_scanData.path + view1d_plotspec_array(3)
	if strlen(V1D_scanData.trashcan) then begin
	view1d_readFileIndex,V1D_scanData.trashcan
	end

view1d_readFileIndex,V1D_scanData.trashcan

WIDGET_CONTROL,/HOURGLASS

; set plot menu options

	view1d_plotspec_id.x_axis_u = 0
	view1d_plotspec_id.type = 0
	view1d_plotspec_id.log = 0
	view1d_plotspec_id.grid = 0
	view1d_plotspec_id.errbars = 0
	view1d_plotspec_id.xcord = 0


; plot the very last scan at the startup time

view1d_plotspec_id.seqno = view1d_viewscan_id.maxno

view1d_plotspec_id.mode = 1
if view1d_plotspec_id.seqno gt 0 then begin
	if V1D_scanData.XDR eq 1 then $
	U_OPENR,unit,V1D_scanData.trashcan,/XDR else $
	U_OPENR,unit,V1D_scanData.trashcan
	i1= view1d_plotspec_id.seqno
	point_lun, unit, view1d_viewscan_id.fptr(i1-1)
	i2 = i1+1
	view1d_scan_read, unit, i1, i2
;	set the view file handle here
	view1d_viewscan_id.file = V1D_scanData.trashcan
	view1d_viewscan_id.unit = unit
	view1d_plotspec_id.seqno = i1
	WIDGET_CONTROL,view1d_widget_ids.filename,SET_VALUE=view1d_viewscan_id.file
	WIDGET_CONTROL,view1d_widget_ids.slider,SET_SLIDER_MAX=view1d_viewscan_id.maxno
	WIDGET_CONTROL, view1d_widget_ids.label, $
		SET_VALUE = 'Scan # [1-'+strtrim(view1d_viewscan_id.maxno,1)+']' 
end

END



PRO view1d_plotoptionsmenu_sensitive,i,on_off
COMMON view1d_PLOTMENU_OPTION_BLOCK,ids,r_names
        WIDGET_CONTROL,ids(i),SENSITIVE=on_off
END

PRO view1d_plotoptionsmenu_set_string,i,j,k,l,m
COMMON view1d_PLOTMENU_OPTION_BLOCK,ids,r_names
        WIDGET_CONTROL,ids(i),SET_VALUE=r_names(i)
        len = strlen(r_names(j))-1
        WIDGET_CONTROL,ids(j),SET_VALUE=' '+strmid(r_names(j),1,len)
if n_params() eq 2 then return
        len = strlen(r_names(k))-1
        WIDGET_CONTROL,ids(k),SET_VALUE=' '+strmid(r_names(k),1,len)
if n_params() eq 3 then return
        len = strlen(r_names(l))-1
        WIDGET_CONTROL,ids(l),SET_VALUE=' '+strmid(r_names(l),1,len)
if n_params() eq 4 then return
        len = strlen(r_names(m))-1
        WIDGET_CONTROL,ids(m),SET_VALUE=' '+strmid(r_names(m),1,len)
END

PRO plotoption_setcolor
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array , view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved

  if view1d_plotspec_id.color eq 1 then begin
        LOADCT, 39
        dcl = float(!d.n_colors - 1)
	ncv=10
        colorlevel = dcl / ncv 
        for i=0,18 do begin
        ii = i / ncv
        im = i mod ncv
        view1d_plotspec_id.colorI(i) = colorlevel * (ncv - im - ii*0.5) 
        end
  end

END


PRO view1d_PLOTOPTIONSMENU_EVENT, Event
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array , view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved
COMMON view1d_PLOTMENU_OPTION_BLOCK,ids,r_names

  WIDGET_CONTROL,EVENT.Id,GET_UVALUE=Ev

   V1D_scanData.act_npts = V1D_scanData.readin_npts

  CASE Event.Value OF
; Color Curve 
    2: begin
	view1d_plotoptionsmenu_set_string,2,3
	view1d_plotspec_id.color = 1
	plotoption_setcolor
	end
    3: begin
	view1d_plotoptionsmenu_set_string,3,2
	view1d_plotspec_id.color = 0
	for i=0,18 do begin
       	view1d_plotspec_id.colorI(i) = !d.n_colors - 1 
	end
	end
; solid / dotted/ dashed
      5: begin
	view1d_plotoptionsmenu_set_string,5,6
	view1d_plotspec_id.solid = 0
	end
      6: begin
	view1d_plotoptionsmenu_set_string,6,5
	view1d_plotspec_id.solid = 1
	end
; plot style line,point,both
      8: begin
	view1d_plotoptionsmenu_set_string,8,9,10
	view1d_plotspec_id.type = 0
	end
      9: begin
	view1d_plotoptionsmenu_set_string,9,10,8
	view1d_plotspec_id.type = 1
	end
      10: begin
	view1d_plotoptionsmenu_set_string,10,8,9
	view1d_plotspec_id.type = 2
	end
; Grid off/on
     12: begin
	view1d_plotoptionsmenu_set_string,12,13
	view1d_plotspec_id.xticklen = 0.04
	view1d_plotspec_id.yticklen = 0.02
	view1d_plotspec_id.gridstyle= 0
	view1d_plotspec_id.grid = 0
	end
     13: begin
	view1d_plotoptionsmenu_set_string,13,12
	view1d_plotspec_id.xticklen = 0.5
	view1d_plotspec_id.yticklen = 0.5
	view1d_plotspec_id.gridstyle= 1
	view1d_plotspec_id.grid = 1
	end
; Errbar off/on
     15: begin
	view1d_plotoptionsmenu_set_string,15,16
	view1d_plotspec_id.errbars = 0
	end
     16: begin
	view1d_plotoptionsmenu_set_string,16,15
	view1d_plotspec_id.errbars = 1
	end
; Y scale linear, Y > 0, log
      18: begin
	view1d_plotoptionsmenu_set_string,18,19,20
	view1d_plotspec_id.log = 0
	end
     19: begin
	view1d_plotoptionsmenu_set_string,19,18,20
	view1d_plotspec_id.log = 2
	end
     20: begin
	view1d_plotoptionsmenu_set_string,20,18,19
	view1d_plotspec_id.log = 1
	end
; Plot ranges 
     21: begin
        user_scale, GROUP= event.top
        return
	end
; Plot labels 
     22: begin
        view1d_plotspec, GROUP= event.top
        return
	end
  ELSE:
  ENDCASE

   view1d_UPDATE_PLOT,V1D_scanData.lastPlot

END

FUNCTION view1d_plotOptions,parent,UVALUE=uvalue
COMMON view1d_PLOTMENU_OPTION_BLOCK,ids,r_names
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array , view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved

  junk   = { CW_PDMENU_S, flags:0, name:'' }

; replot menu
  MenuOptions = [ $
      { CW_PDMENU_S,       3, 'Plot Options' }, $ ;        0
        { CW_PDMENU_S,       1, 'Colors' }, $ ;        1
          { CW_PDMENU_S,       0, '* Colors' }, $ ;        2
          { CW_PDMENU_S,       2, '  Black&White' }, $ ;        3
        { CW_PDMENU_S,       1, 'Lines' }, $ ;        4
          { CW_PDMENU_S,       0, '  Solid/Dotted/etc ' }, $ ;        5
          { CW_PDMENU_S,       2, '* Solid Only' }, $ ;        6
        { CW_PDMENU_S,       1, 'Symbols' }, $ ;        7
          { CW_PDMENU_S,       0, '* Line Only' }, $ ;        8
          { CW_PDMENU_S,       0, '  Symbol Only' }, $ ;        9
          { CW_PDMENU_S,       2, '  Both' }, $ ;        10
        { CW_PDMENU_S,       1, 'Grid' }, $ ;        11
          { CW_PDMENU_S,       0, '* Off' }, $ ;       12
          { CW_PDMENU_S,       2, '  On' }, $ ;        13
        { CW_PDMENU_S,       1, 'Err Bars' }, $ ;        14
          { CW_PDMENU_S,       0, '* Off' }, $ ;        15
          { CW_PDMENU_S,       2, '  On' }, $ ;       16 
        { CW_PDMENU_S,       1, 'Y Scale' }, $ ;        17
          { CW_PDMENU_S,       0, '* Linear' }, $ ;       18 
          { CW_PDMENU_S,       0, '  Linear (Y>0)' }, $ ;       19 
          { CW_PDMENU_S,       2, '  Log' }, $ ;       20 
      { CW_PDMENU_S,       0, 'Ranges ...' }, $ ;        21
      { CW_PDMENU_S,       0, 'Labels ...' } $ ;       22 
  ]

ids = make_array(23,value=0L)
r_names = [ '', $
	'', '* Colors', '* Black&White', $
	'', '* Solid/Dotted/etc', '* Solid Only', $
	'', '* Line Only','* Symbol Only','* Both', $
	'', '* Off', '* On', $
	'', '* Off', '* On', $
	'', '* Linear',  '* Linear (Y>0)', '* Log', $
	'', '']


  view1d_PLOTOPTIONSMENU = CW_PDMENU( parent, MenuOptions, $
	IDS=ids, $
 	RETURN_ID = r_id, $
	RETURN_NAME = r_name, $
      UVALUE=uvalue)

	return, view1d_PLOTOPTIONSMENU
END




PRO view1d_PDMENU127_EVENT,Event
COMMON view1d_viewscan_block, view1d_viewscan_ids, view1d_viewscan_id

CASE Event.Value OF

'File.Open ...': begin
	catcher_view1d_open,Event
	end
'File.Printer ...': begin
	PS_printer,GROUP=Event.Top
	end
'File.Quit': begin
	WIDGET_CONTROL,Event.Top,/DESTROY
	if view1d_viewscan_id.unit ne 0 then u_close,view1d_viewscan_id.unit
	view1d_viewscan_id.unit = 0
	end
ENDCASE
END


PRO view1d_base_close,wid
	if XRegistered('MAIN13_1') then begin 
;		view1d_plotspec_restoreTitle
;		catch1d_scanInitSetup
		end
END

PRO view1d_PDMENU128_EVENT,Event
CASE Event.Value OF
'Setup.Color ...': begin
	XLOADCT,GROUP=Event.top
	end

ENDCASE
END


PRO ZOOMMENU_Event, Event
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON user_scale_block,user_scale_ids

if V1D_scanData.lastPlot ge 0 then begin 

if XRegistered('user_scale') ne 0 then begin
        WIDGET_CONTROL,user_scale_ids.base,/DESTROY
        end

V1D_scanData.act_npts = V1D_scanData.readin_npts

  CASE Event.Value OF 

  'Zoom.Zoom To Box': BEGIN
	zoom_to_box
 	END
  'Zoom.Zoom In/Out': BEGIN
	zoom_in_out
 	END
  'Zoom.Calc Slopes': BEGIN
	draw_dragLine
 	END
  'Zoom.Auto Scale (Refresh)': BEGIN
	V1D_scanData.lastPlot = 1
	view1d_UPDATE_PLOT, 1
 	END
  'Zoom.User Scale ...': BEGIN
	V1D_scanData.lastPlot = 0
	user_scale,GROUP=event.top
;	view1d_UPDATE_PLOT, 0
 	END
  ENDCASE
end
END



PRO VIEW1D_1_EVENT,Event
COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array, view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved
COMMON view1d_viewscan_block, view1d_viewscan_ids, view1d_viewscan_id


  WIDGET_CONTROL, Event.id, GET_UVALUE=Ev

  CASE Ev OF
  'view1d_PDMENU127': view1d_PDMENU127_EVENT, Event
  'view1d_PDMENU128': view1d_PDMENU128_EVENT, Event
  'view1d_PLOTOPTIONSMENU': view1d_PLOTOPTIONSMENU_EVENT, Event
  'HELPMENU': HELPMENU_EVENT, Event
  'PRINTMENU': PRINTMENU_EVENT, Event
  'ZOOMMENU': ZOOMMENU_EVENT, Event
  'STATISTICMENU': STATISTICMENU_EVENT, Event
  'FIELD141': catcher_view1d_filename, Event 

  'VIEWSPEC_SLIDER': begin
		WIDGET_CONTROL,view1d_widget_ids.slider,GET_VALUE=seqno
		point_lun,view1d_viewscan_id.unit,view1d_viewscan_id.fptr(seqno-1)
		view1d_scan_read,view1d_viewscan_id.unit,seqno,seqno+1 
		s1 = strtrim(string(seqno),2)
		WIDGET_CONTROL,view1d_widget_ids.seqno,SET_VALUE=s1
		view1d_update_summary_setup,Event
	end
  'VIEWSPEC_SEQNO': begin
		WIDGET_CONTROL,view1d_widget_ids.seqno,GET_VALUE=seqno
		i1 = fix(seqno(0))
	if i1 lt 1  or i1 gt view1d_viewscan_id.maxno then begin
	res= WIDGET_MESSAGE('Input out of range !!',/info, $
		dialog_parent=Event.id)
	return
	end
		if i1 ge 1 then begin 
		point_lun, view1d_viewscan_id.unit, view1d_viewscan_id.fptr(i1-1)
		i2 = i1 + 1
		view1d_scan_read,view1d_viewscan_id.unit, i1, i2
		end
		view1d_update_summary_setup,Event
	end
  'VIEWSPEC_FIRST': begin
		if view1d_viewscan_id.maxno eq 0 then begin
			res=WIDGET_MESSAGE('Error: no data available!',/info, $
				dialog_parent=Event.id)
			return
			end
		point_lun,view1d_viewscan_id.unit,view1d_viewscan_id.fptr(0)
		view1d_scan_read,view1d_viewscan_id.unit, 1, 2
		WIDGET_CONTROL,view1d_widget_ids.seqno,SET_VALUE='1'
		view1d_update_summary_setup,Event
	end
  'VIEWSPEC_NEXT': begin
		WIDGET_CONTROL,view1d_widget_ids.seqno,GET_VALUE=seqno
		i1 = fix(seqno(0)) + 1
		if i1 ge (view1d_viewscan_id.maxno+1) then i1 = 1 
		i2 = i1 + 1
		point_lun,view1d_viewscan_id.unit,view1d_viewscan_id.fptr(i1-1)
		view1d_scan_read,view1d_viewscan_id.unit, i1, i2 
		seqno = strtrim(string(i1),2)
		WIDGET_CONTROL,view1d_widget_ids.seqno,SET_VALUE=seqno
		view1d_update_summary_setup,Event
	end
  'VIEWSPEC_PREV': begin
		WIDGET_CONTROL,view1d_widget_ids.seqno,GET_VALUE=seqno
		i1 = fix(seqno(0)) - 1
		if i1 lt 1 then i1 = view1d_viewscan_id.maxno 
		i2 = i1 + 1
		point_lun,view1d_viewscan_id.unit,view1d_viewscan_id.fptr(i1-1)
		view1d_scan_read,view1d_viewscan_id.unit, i1, i2
		seqno = strtrim(string(i1),2)
		WIDGET_CONTROL,view1d_widget_ids.seqno,SET_VALUE=seqno
		view1d_update_summary_setup,Event
	end
  'VIEWSPEC_LAST': begin
		if view1d_viewscan_id.maxno eq 0 then begin
			res=WIDGET_MESSAGE('Error: no data available!',/info, $
				dialog_parent=Event.id)
			return
			end
		i1 = view1d_viewscan_id.maxno-1
		i2 = i1+1
		point_lun,view1d_viewscan_id.unit,view1d_viewscan_id.fptr(i1)
		view1d_scan_read,view1d_viewscan_id.unit, i2, i2+1
		WIDGET_CONTROL,view1d_widget_ids.seqno,SET_VALUE=strtrim(i2,2)
		view1d_update_summary_setup,Event
	end
  'PICK_XAXIS': BEGIN
	view1d_plotspec_id.x_axis_u = 0
	view1d_plotspec_id.xcord = 0
	if Event.Index eq 0 then view1d_plotspec_id.x_axis_u = 1 else $
	view1d_plotspec_id.xcord = Event.Index - 1
	view1d_UPDATE_PLOT, V1D_scanData.lastPlot
	END
  'PICK_XDR': BEGIN
	V1D_scanData.XDR = Event.Index
	END
  'VIEW1D_DRAW61': BEGIN
;print,'Event.PRESS',event.press
	if (!x.window(1) - !x.window(0)) eq 0 then begin
		view1d_warningtext,'Error: Plot data not established yet.'
		return
		end
; cross-hairs
      IF (Event.PRESS EQ 1) THEN BEGIN
	WSET, view1d_widget_ids.plot_area
	view1d_xy_coord, GROUP=Event.top
	view1d_xycoord
	END
      END

  'BGROUP145': BEGIN
	view1d_UPDATE_PLOT, V1D_scanData.lastPlot 
      END


  ELSE:   ;don't stop of no matches
  ENDCASE
END



PRO VIEW1D, config=config, data=data, debug=debug, XDR=XDR, GROUP=Group
;
;+
; NAME:
;       VIEW1D	
;
; PURPOSE:
;
;       This program is specially written for the data catcher. It
;       provides the IDL user a convenient post data acquisition
;       1D data display program. It shares the common 1D data file
;       with data catcher without interrupting the process of data
;       scanning of data catcher.
;
; CATEGORY:
;	Widgets. 
;
; CALLING SEQUENCE:
;	VIEW1D [,Data='test.dat'] [,Config='test.config'] [,/XDR] [GROUP=Group] 
;
; INPUTS:
;	None
;
; KEYWORD PARAMETERS:
;     CONFIG:   Specifies whether the configuration file to be used in the 
;               startup of the VIEW1D.
;     DATA:     Specifies the 1D file name to be used for displaying 
;               captured 1D scan data. If not specified, the default 
;               data file name 'catch1d.trashcan' is assumed.
;     XDR:      Indicates whether the data file entered on the command line is
;               in XDR format.
;     GROUP:    The widget ID of the group leader of the widget.  If this 
;               keyword is specified, the death of the group leader results in
;               the death of VIEW1D.
;
; OUTPUTS:
;       It provides various post acquistion 1D scan displaying features. 
;       Various levels of report files can be generated by this program.  
;       All the file generated will be prefixed by the DATA file name.
;
; COMMON BLOCKS:
;       VIEW1D_COM
;       W_PLOTSPEC_BLOCK
;       W_VIEWSCAN_BLOCK
;
; SIDE EFFECTS:
;       New scan data may be appended at the end of data file if the
;       data catcher is runing at the same time and new data 
;       is detected by the data catcher. Reload the data file will
;       let you access the newly captured scan data from the file.
;
; RESTRICTIONS:
;
; PROCEDURE:
;       The 'os.init' must be loaded into IDL before invoking the 
;       VIEW1D application.
;
; EXAMPLE:
;       Use default setting for view1d 
;
;       	VIEW1D
;
;       Override the default setting by specifying config and data file
;       on the command line
;
;       	VIEW1D, CONFIG='test.config', DATA='test.dat'
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 05-27-97.
;
;       08-01-97  bkc   Add the 2D scan # in the header 
;-

COMMON VIEW1D_COM, view1d_widget_ids, V1D_scanData
COMMON V1D_realtime_block, view1d_realtime_id, realtime_retval, realtime_pvnames
COMMON view1d_plotspec_block, view1d_plotspec_ids, view1d_plotspec_array, view1d_plotspec_id, view1d_plotspec_limits, view1d_plotspec_saved
COMMON view1d_viewscan_block, view1d_viewscan_ids, view1d_viewscan_id
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids
COMMON env_field_block,env_field
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus

  if XRegistered('VIEW1D_1') NE 0 then return

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

@view1d.init

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  VIEW1D_1 = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
      MAP=1, /TLB_SIZE_EVENTS, $
;      TLB_FRAME_ATTR = 8, $
      TITLE='VIEW1D (R1.1)', $
      UVALUE='VIEW1D_1')

  BASE68 = WIDGET_BASE(VIEW1D_1, $
	/ROW, $
      MAP=1, $
      UVALUE='BASE68')

V1D_scanData.XDR = 0
if keyword_set(XDR) then V1D_scanData.XDR = 1
if keyword_set(DEBUG) then V1D_scanData.debug = 1
	
if keyword_set(data) then begin
  MenuDesc1981 = [ $
      { CW_PDMENU_S,       3, 'File' }, $ ;        0
        { CW_PDMENU_S,       0, 'Printer ...' }, $ ;        2
        { CW_PDMENU_S,       2, 'Quit' } $  ;      4
	] 
endif else $
  MenuDesc1981 = [ $
      { CW_PDMENU_S,       3, 'File' }, $ ;        0
        { CW_PDMENU_S,       0, 'Open ...' }, $ ;        1
        { CW_PDMENU_S,       0, 'Printer ...' }, $ ;        2
        { CW_PDMENU_S,       2, 'Quit' } $  ;      4
  ]

  view1d_PDMENU127 = CW_PDMENU( BASE68, MenuDesc1981, /RETURN_FULL_NAME, $
      UVALUE='view1d_PDMENU127')

  MenuSetup = [ $
      { CW_PDMENU_S,       3, 'Setup' }, $ ;        0
        { CW_PDMENU_S,       0, 'Color ...' }  $ ;        1
  ]
  view1d_PDMENU128 = CW_PDMENU( BASE68, MenuSetup, /RETURN_FULL_NAME, $
      UVALUE='view1d_PDMENU128')


  view1d_PLOTOPTIONSMENU = view1d_plotOptions( BASE68, UVALUE='view1d_PLOTOPTIONSMENU')

  MenuPrint = [ $
      { CW_PDMENU_S,       3, 'Print' }, $ ;        0
        { CW_PDMENU_S,       0, 'ASCII (tmp)' }, $ ;        1
        { CW_PDMENU_S,       0, 'Plot' }, $ ;        1
        { CW_PDMENU_S,       2, 'Report ...' } $ ;        4
  ]
  PDMENU_print = CW_PDMENU( BASE68, MenuPrint, /RETURN_FULL_NAME, $
      UVALUE='PRINTMENU')

  MenuZoom = [ $
      { CW_PDMENU_S,       3, 'Zoom' }, $ ;        0
        { CW_PDMENU_S,       0, 'Zoom To Box' }, $ ;    1   
        { CW_PDMENU_S,       0, 'Zoom In/Out' }, $ ;        2
        { CW_PDMENU_S,       0, 'Calc Slopes' }, $ ;       3
        { CW_PDMENU_S,       0, 'Auto Scale (Refresh)' }, $ ;       4
        { CW_PDMENU_S,       2, 'User Scale ...' } $ ;       5
  ]
  PDMENU_zoom = CW_PDMENU( BASE68, MenuZoom, /RETURN_FULL_NAME, $
      UVALUE='ZOOMMENU')

; statistic menu

  MenuStatistic = [ $
      { CW_PDMENU_S,       3, 'Statistic' }, $ ;        0
        { CW_PDMENU_S,       0, 'None' }, $ ;        1
        { CW_PDMENU_S,       0, 'Peak/Centroid/FWHM on plot' }, $ ;        1
        { CW_PDMENU_S,       0, 'Peak/Centroid/FWHM ...' }, $ ;        1
        { CW_PDMENU_S,       2, 'Average/Deviation ...' } $ ;        1
  ]

  PDMENU_statistic = CW_PDMENU( BASE68, MenuStatistic, /RETURN_FULL_NAME, $
      UVALUE='STATISTICMENU')


  MenuHelp = [ $
      { CW_PDMENU_S,       3, 'Help' }, $ ;        0
        { CW_PDMENU_S,       0, 'Version ...' }, $ ;        1
        { CW_PDMENU_S,       0, 'BIN/XDR ...' } $ ;        1
	]

  PDMENU_help = CW_PDMENU( BASE68, MenuHelp, /RETURN_FULL_NAME, $
      UVALUE='HELPMENU')

;  SAVE_LABEL = WIDGET_LABEL( BASE68, $
;      FONT='-bitstream-charter-bold-i-normal--25-240-75-75-p-154-iso8859-1', $
;	xsize=150, VALUE='          ')

  BASE140 = WIDGET_BASE(VIEW1D_1, $
      ROW=1, $
;      FRAME=2, $
      MAP=1, $
      TITLE='', $
      UVALUE='BASE140')

  Btns915 = ['BIN','XDR']
  pick_xdr = WIDGET_DROPLIST(BASE140, VALUE=BTNS915, $
        UVALUE='PICK_XDR',TITLE='Type')
if V1D_scanData.XDR eq 1 then begin
  WIDGET_CONTROL,pick_xdr,set_droplist_select = 1
end

  FieldVal1988 = [ $
    'catch1d.trashcan' ]
  FIELD141 = CW_FIELD( BASE140,VALUE=FieldVal1988, $
      ROW=1, $
      STRING=1, $
;      NOEDIT=1, $
	RETURN_EVENTS=1, $
      TITLE='Filename', $
      UVALUE='FIELD141', $
      XSIZE=65)


; view scan contorls

  row1 = WIDGET_BASE(VIEW1D_1, $
      ROW=1, MAP=1, TITLE='Scan')


view1d_viewscan_first = WIDGET_BUTTON(row1, $
                        VALUE = ' First ', $
                        UVALUE = 'VIEWSPEC_FIRST')
view1d_viewscan_next = WIDGET_BUTTON(row1, $
                        VALUE = ' Next ', $
                        UVALUE = 'VIEWSPEC_NEXT')
view1d_viewscan_next = WIDGET_BUTTON(row1, $
                        VALUE = ' Prev ', $
                        UVALUE = 'VIEWSPEC_PREV')
view1d_viewscan_last = WIDGET_BUTTON(row1, $
                        VALUE = ' Last ', $
                        UVALUE = 'VIEWSPEC_LAST')


	; ?? read file find the maxno

	maxno = 2

if keyword_set(data) then begin
found = findfile(data)  
if found(0) ne '' then begin
view1d_plotspec_array(3) = data 
if V1D_scanData.XDR eq 1 then $
U_OPENR,unit,view1d_plotspec_array(3),/XDR else $
U_OPENR,unit,view1d_plotspec_array(3)
view1d_scan_read_all,unit,maxno
free_lun,unit
end
V1D_scanData.trashcan = view1d_plotspec_array(3)
WIDGET_CONTROL,FIELD141,SET_VALUE=V1D_scanData.trashcan
end

        str = 'Scan # [ 1 -'+ strcompress(string(maxno)) +' ] '
	view1d_viewscan_label = WIDGET_LABEL(row1,VALUE=str)
	view1d_viewscan_seqno = WIDGET_TEXT(row1,VALUE='0', $
                EDITABLE=1, $
                UVALUE='VIEWSPEC_SEQNO', XSIZE = 5)

	; add seqno slider here

	if maxno gt 1 then begin
        view1d_viewscan_slider = WIDGET_SLIDER(row1, $
                MAX=maxno, $
                MIN=1,UVALUE='VIEWSPEC_SLIDER')
        end



  BASE61 = WIDGET_BASE(VIEW1D_1, $
      COLUMN=1, $
      MAP=1, $
      TITLE='Plot Area', $
      UVALUE='BASE61')


  VIEW1D_DRAW61 = WIDGET_DRAW( BASE61, $
      BUTTON_EVENTS=1, $
      RETAIN=2,  $
      UVALUE='VIEW1D_DRAW61', $
      XSIZE=400, $
      YSIZE=300)


  BASE144 = WIDGET_BASE(VIEW1D_1, $
      ROW=1, $
      MAP=1, $
      TITLE='WF Selector', $
      UVALUE='BASE144')

  BASE144_1 = WIDGET_BASE(BASE144, $
      COL=1, $
      FRAME=2, $
      UVALUE='BASE144_1')


  Btns913 = ['#','P1','P2','P3','P4', $
	     'D1','D2','D3','D4','D5','D6','D7','D8', $
	     'D9','D10','D11','D12','D13','D14','D15']

; if detector for X axis is desired just comment out the following line
  Btns913 = ['#','P1','P2','P3','P4']

  pick_xaxis = WIDGET_DROPLIST(BASE144_1, VALUE=BTNS913, $
        UVALUE='PICK_XAXIS',TITLE='Xaxis')
  WIDGET_CONTROL,pick_xaxis,set_droplist_select = 1

  Btns1994 = [ $
    'D1', $
    'D2', $
    'D3', $
    'D4', $
    'D5', $
    'D6', $
    'D7', $
    'D8', $
    'D9', $
    'D10', $
    'D11', $
    'D12', $
    'D13', $
    'D14', $
    'D15', $
    'P1', $
    'P2', $
    'P3', $
    'P4' $
	 ]
  BGROUP145 = CW_BGROUP( BASE144, Btns1994, $
      ROW=2, $
      FRAME=2, $
      NONEXCLUSIVE=1, $
      LABEL_LEFT='Y', $
      UVALUE='BGROUP145')

;  user_before = WIDGET_BUTTON( BASE144, $
;      UVALUE='USER_BEFORE', $
;      VALUE='Before_Scan')

;  user_after = WIDGET_BUTTON( BASE144, $
;      UVALUE='USER_AFTER', $
;      VALUE='After_Scan')


; set drawing area as wide as window width
win_state = WIDGET_INFO(VIEW1D_1, /GEOMETRY)
WIDGET_CONTROL, VIEW1D_DRAW61, DRAW_XSIZE=win_state.scr_xsize 

  WIDGET_CONTROL, VIEW1D_1, /REALIZE


  ; Get drawable window index

  COMMON VIEW1D_DRAW61_Comm, VIEW1D_DRAW61_Id
  WIDGET_CONTROL, VIEW1D_DRAW61, GET_VALUE=VIEW1D_DRAW61_Id

  view1d_widget_ids = {                  $ 
      base        : VIEW1D_1,     $
      menubar_base: BASE68,       $
      menuplot_base: view1d_PLOTOPTIONSMENU, $
      filename    : FIELD141,     $
      label       : view1d_viewscan_label,  $
      seqno       : view1d_viewscan_seqno,  $
      slider      : view1d_viewscan_slider, $
      wf_select   : BGROUP145,    $
      x_axis      : pick_xaxis,   $
      plot_wid    : VIEW1D_DRAW61,       $
      plot_area   : VIEW1D_DRAW61_Id,    $
      summary     : 0L,    $
      terminal    : 0L,    $
      statistic   : 0L    $
  }

   WIDGET_CONTROL, view1d_widget_ids.wf_select, SET_VALUE = [1,1,0,0]

catcher_view1d_Init


; get start home work directory

  CD,'.',CURRENT=old_path
  V1D_scanData.home=old_path

;  default is view only 
; check for input file names

if keyword_set(config) then V1D_scanData.config = config

if keyword_set(data) then view1d_plotspec_array(3) = data 

if keyword_set(envfile) then V1D_scanData.envfile=envfile

  XMANAGER, 'VIEW1D_1', VIEW1D_1,CLEANUP='view1d_base_close'
;  XMANAGER, 'VIEW1D_1', VIEW1D_1, CLEANUP='catcher_close',NO_BLOCK=0

END
