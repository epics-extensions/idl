
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
