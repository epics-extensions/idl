PRO panImage,image_array,id_def,factor,title=title,new_win=new_win, tiff=tiff,reverse=reverse,gif=gif,pict=pict,error=error
;+
; NAME:
;	panImage
;
; PURPOSE:
;       This method pops up a new PanImage window for a given 2D scan 
;       image_array.
;
; CALLING SEQUENCE:
;       panImage, Image_array [,Factor]  [,TIFF='tifname',/reverse]
;		  [,TITLE='description']
;                 [,GIF='gifname'] [,PICT='pictname'] [,ERROR=error]
;
; ARGUMENTS:
;  Image_array:  Image_array(Width,Height,Ndets) specifies the 2D 
;                image_array for all detectors, where each image has
;                dimension of WidthxHeight, Ndets is the number of detectors
;  id_def:       Id_def(Ndets) defines the vector of indicators for detector 
;                image presence, 0 not present , 1 present 
;  Factor:       Specifies the multiplication factor for TV image, default
;                TV image size is 60x60
;
; KEYWORDS:
;     TITLE:   Specifies the title of the panImage window
;     GIF:     Specifies the output gif filename. If specified the
;              panImage window will be saved in the gif output file.
;     PICT:    Specifies the output pict filename. If specified the
;              panImage window will be saved in the pict output file.
;     TIFF:    Specifies the output tiff filename. If specified the
;              panImage window will be saved in the tiff output file.
;     REVERSE: Specifies whether the reverse tiff should be saved.
;     NEW_WIN: Returns the new window number of the panImages
;
; EXAMPLE:
;     
;     panImage, image_array
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Mar 29, 2000.
;	xx-xx-xxxx bkc  comment
;-

error=0
	if n_elements(image_array) eq 0 then begin
		st = "Usage: panImage,image_array,title='description',tif=..."
		r = dialog_message(st,/Error)
		error = -1
		return
	end

	sz = size(image_array)
	if n_elements(sz) ne 6 then begin
		st = "Dimension error!!  Image_array(W,H,N)"
		r = dialog_message(st,/Error)
		error = -1
		return
	end

	if n_elements(id_def) eq 0 then id_def = make_array(sz(3),value=1)

; pops up pan images

update:

	NC = 8
	ND = sz(3)
	NR = ND / NC +1
	NL = NR*NC -1

	width = 60
	height = 60
	old_win = !D.window

	if n_elements(factor) then begin
		if factor lt .1 then factor = 1
		width = width * factor
		height = height * factor
	end

	o_win = -1
	if n_elements(new_win) then o_win = new_win
catch,error_status
if error_status ne 0 and !error_state.name eq 'IDL_M_CNTOPNFIL' then begin
        r = dialog_message([!error_state.msg,!error_state.sys_msg,$
                string(!error_state.code)],/error)
        return
end
if error_status then o_win=-1
if o_win ne -1 then wdelete,o_win
o_win = -1
	if o_win lt 0 then begin
		window,/free, xsize = NC*width, ysize=NR*height, $
			title=title
		for i=0,ND-1 do begin
		ii = NL-i
		xi=(i mod NC)*width+width/2 - 5 
		yi=height/2+ii/NC*height
		xyouts, xi,yi,'D'+strtrim(i+1,2),/device
		end
	end

new_win = !D.window

	wset,new_win
	for sel=0,ND-1 do begin
	if id_def(sel) gt 0 then begin
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


	for i=1,NR-1 do plots,[0,NC*width],[i*height,i*height],/device
	for i=1,NC-1 do plots,[i*width,i*width],[0,NR*height],/device

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

END
