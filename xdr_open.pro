;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************

PRO xdr_write,unit,x, error=error
;+
; NAME:
;       XDR_WRITE
;
; PURPOSE:
;       This routine writes the input variable data on an opened XDR unit. 
;       It supports all IDL data type except the complex number and up 
;       to 4D array.  
;
; CALLING SEQUENCE:
;
;       XDR_WRITE, Unit, Var [,ERROR=Error]
;
; INPUTS:
;       Unit:   The logic unit number returned by file open for unformatted
;               XDR write.
;       Var:    The varible contains the data array to be written to the unit.
;
; KEYWORD PARAMETERS:
;      ERROR:   Returns the error code of this routine. It returns 0 if
;               succeeded, returns negative value if failed.
;
; RESTRICTIONS:
;       The file unit must be already opened for unformatted XDR write.
;
; EXAMPLE:
;
;         xdr_open,unit,'test.xdr',/write
;         xdr_write,unit,x
;         xdr_close,unit
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 11-11-00.
;       xx-xx-xxxx      comment
;-

; the unit is already opened

	error = 0
	catch,error_status
	if error_status ne 0 then begin
		print,!error_state.msg + string(!error_state.code)
		error = error_status
		return 
	end

	sz = size(x)
	num = n_elements(sz)
	type = sz(num-2)

	writeu,unit,sz,x

END


PRO xdr_read,unit,y, error=error
;+
; NAME:
;       XDR_READ
;
; PURPOSE:
;       This routine reads a variable data from an opened XDR unit. 
;       It supports all IDL data type except the complex number and up 
;       to 4D array.  
;
; CALLING SEQUENCE:
;
;       XDR_READ, Unit, Var [,ERROR=Error]
;
; INPUTS:
;       Unit:   The logic unit number returned by file open for unformatted
;               XDR read.
;       Var:    The varible returns the data array.
;
; KEYWORD PARAMETERS:
;      ERROR:   Returns the error code of this routine. It returns 0 if
;               succeeded, returns negative value if failed.
;
; RESTRICTIONS:
;       The file unit must be already opened for unformatted XDR read.
;       The data must be previously written on the file by the XDR_WRITE
;       routine.
;
; EXAMPLE:
;
;         xdr_open,unit,'test.xdr'
;         xdr_read,unit,x
;         xdr_close,unit
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 11-11-00.
;       xx-xx-xxxx      comment
;-  

	error = 0
	catch,error_status
	if error_status ne 0 then begin
		print,!error_state.msg + string(!error_state.code)
		error = error_status
		return 
	end

	dim = 0L
	readu,unit,dim
	
	szn = lonarr(dim+2)
	readu,unit,szn

	type = szn(dim) 	; 1-byte, 2-int,3-long,4-float,5-double
	szn = [dim,szn]

	CASE dim OF
	0: begin
		y = make_array(1,TYPE=type)
		readu,unit,y
	   end
	1: begin
		y = make_array(szn(1),TYPE=type)
		readu,unit,y
	   end
	2: begin
		y = make_array(szn(1),szn(2),TYPE=type)
		readu,unit,y
	   end
	3: begin
		y = make_array(szn(1),szn(2),szn(3),TYPE=type)
		readu,unit,y
	   end
	4: begin
		y = make_array(szn(1),szn(2),szn(3),szn(4),TYPE=type)
		readu,unit,y
	   end
	ENDCASE

END

PRO xdr_setFP,unit,pos
; set file pointer
	point_lun,unit,pos
END

PRO xdr_getFP,unit,pos
; get file pointer
	point_lun,-unit,pos
END

PRO xdr_rewind,unit
; rewind 
	point_lun,unit,0
END

PRO xdr_close,unit
;+
; NAME:
;       XDR_CLOSE
;
; PURPOSE:
;       This routine closes the opened XDR file.
;
; CALLING SEQUENCE:
;       XDR_CLOSE, Unit
;
; PARAMETERS:
;       Unit:        The free logic unit number obtained by XDR_OPEN.
;
; EXAMPLE:
;	XDR_CLOSE,unit
;
;-

	free_lun,unit
END

PRO xdr_open,unit,filename,read=read,write=write,append=append,error=error
;+
; NAME:
;       XDR_OPEN
;
; PURPOSE:
;       This routine opens the file for XDR read or write depends on the 
;       keyword specified by the user. Default is open for read.
;
; CALLING SEQUENCE:
;
;       XDR_OPEN, Unit, Filename [,WRITE=Write] [,APPEND=Append] [,ERROR=Error]
;
; PARAMETERS:
;       Filename:    The varible specifies the input XDR file name.
;       Unit:        The free logic unit number obtained for opened file.
;
; KEYWORD PARAMETERS:
;      WRITE:   Overwrite the file for writing with new data.
;      APPEND:  Append the data at the end of file.
;      ERROR:   Returns the error code of this routine. It returns 0 if
;               succeeded, returns negative value if failed.
;
; RESTRICTIONS:
;       The file must contain unformatted XDR data.
;
; EXAMPLE:
;
;      Example 1 - open for read
;         xdr_open,unit,'test.xdr'
;
;      Example 2 - open for write
;         xdr_open,unit,'test.xdr',/write
;
;      Example 3 - open for append
;         xdr_open,unit,'test.xdr',/append
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 11-11-00.
;       xx-xx-xxxx      comment
;-
	error = 0
	catch,error_status
	if error_status ne 0 then begin
		print,!error_state.msg + string(!error_state.code)
		error = error_status
		return 
	end
	if keyword_set(append) then begin
		openw,unit,/XDR,/GET_LUN,filename,/append
		return
	end
	if keyword_set(write) then openw,unit,/XDR,/GET_LUN,filename else $
	openr,unit,/XDR,/GET_LUN,filename
END
