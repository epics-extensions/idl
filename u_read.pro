;  u_openw, unit
;  u_openr
;  u_write 
;  u_read
;  u_dump
;  rewind, unit
;
; Examples: 
;  u_openw, unit
;  u_write, unit, x
;  u_close, unit
;
;  u_dump
;
;  u_openr, unit
;  u_read, unit, x  &  print,x
;  u_close, unit
;


PRO rewind,unit
point_lun,unit,0
END

PRO u_openw,unit,filename,append=append,help=help
if keyword_set(help) then goto, help1
if n_elements(filename) eq 0 then filename='data.dat'
if keyword_set(append) then openw,unit,filename,/GET_LUN,/APPEND else $
openw,unit,filename,/GET_LUN  
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

PRO u_openr,unit,filename,help=help
if keyword_set(help) then goto, help1
if n_elements(filename) eq 0 then filename='data.dat'
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
;       U_WRITE, Unit, Var, /Help
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
; MODIFICATION HISTORY:
;       Written by:	Ben-chin Cha, 03-23-95.
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
PRO u_dump,norecord,filename,data=data,help=help
ON_IOERROR,BAD
if keyword_set(help) then goto, help1
if n_elements(filename) eq 0 then filename='data.dat'
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
		x = btyarr(fix(int))  	
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
else: print,'type=',type
endcase
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
;       U_READ, Unit, Var, /Help
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
;       Written by:	Ben-chin Cha, 03-23-95.
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
