;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************

@u_read.pro

PRO catch1d_newIndexFile,file,array,XDR=XDR,print=print,TV=TV,header=header,nowrite=nowrite
;
; this will fix the index file if there is problem with index data 
;
; Get 1D data index
;
;      catch1d_newindexfile,file1,array,/xdr,/print
;
; Get 2D image file data index, the keyword TV must be set
;
; x1 = '2idd:scan1'
; y = make_array(60,/byte)
; y(0,0) = byte(x1)
;
;   catch1d_newindexfile,file,array,/xdr,/print,/tv,header=y
;

	filename = '/home/sricat/CHA/user/s2bmb/align2.xdr.bk'
	if n_elements(file) eq 0 then begin
		print,'Usage: catch1d_newIndexFile,File [,/XDR] [,/PRINT] [,HEADER=header]'
		return
	end
	filename = file

	buff1 = byte( 'CATCHER_V1')
	if keyword_set(header) then buff1 = byte(header)

	openr,unit,filename,/get_lun
	st = fstat(unit)
	buff = make_array(st.size,/byte)
	readu,unit,buff
	close,unit
	no=0
	array=0L
	xdr_offset = 28L
	sz = size(buff1)
	shift = sz(n_elements(sz)-1)  
	if keyword_set(TV) then begin
		xdr_offset = 24L
		end
	if keyword_set(print) then begin
		print,buff(0:50)
		print,st.size,shift,xdr_offset
	end

	for i=xdr_offset,st.size-shift do begin
		buff2 = buff(i:i+shift-1)
		diff = total(buff1-buff2) 
		if diff eq 0. then begin
		no = no + 1
		if keyword_set(XDR) then begin
			if keyword_set(print) then print,no,i-xdr_offset,buff(i:i+shift)
			if (i-xdr_offset) gt 0 then array = [array,i-xdr_offset]
			i = i + xdr_offset + shift+1
			endif else begin
			if keyword_set(print) then print,no,i-20,buff(i:i+shift)
			if (i-20) gt 0 then array = [array,i-20]
			i = i + 20 + shift+1
			end
		end
	end
	array = [array,st.size]

	if keyword_set(print) then begin
		print,st.name
		print,st.size
		print,no
		print,array
	end
	if keyword_set(nowrite) or keyword_set(TV) then return

	if !d.name eq 'WIN' then openw,unit,filename+'.index',/get_lun,/XDR else $
	openw,unit,filename+'.index',/get_lun
	u_write,unit,st.name
	u_write,unit,st.size
	u_write,unit,no
	u_write,unit,array
	close,unit
	u_close,unit
	print,'***New Index File generated***'
        print,filename+'.index'

END


PRO readfixindex,indexfile,fsize,maxno,array
; The fixed index file on WIN system will be save in XDR format
; this routine especially written for readin the fixed index file for WIN system
;
	if !d.name ne 'WIN' then return

	found = findfile(indexfile,count=ct)
	if ct eq 0 then return

	t = lonarr(5)
	openr,unit1,indexfile,/get_lun,/XDR 
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
