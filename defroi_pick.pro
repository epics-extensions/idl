;
; defroi_pick.pro
;

PRO defroi_zoombox,im,xl,yl,wd,ht,zoom_box,picke,background=background,region=region,delete=delete,oldpicke=oldpicke
if n_params() lt 5 then begin
	str = ['  Useing the zoombox to fine tune the ROIs. Marks the elements to be added or deleted.','', $
	'  Drag Left button to move zoom box. ',$
	'  Drag Middle button near a corner to resize box.',$
	'  Right button when done.' , '', $

	'USAGE:  defroi_zoombox, im, xl, yl, wd, ht, zoom_box=zoombox,...', '',$
	'INPUT:',$
	'    im         - image array corresponding to the TV image', $
	'    xl, yl     - lower left corner of TV image', $
	'    wd, ht     - total pixel width and height of image area', $
	'OUTPUT:',$
	'    zoom_box   - returns the [IL,JL,IR,JR] vector of selected region', $
	'KEYWORD:',$
	'    BACKGROUNG - background color', $
	'    OLDPICKE   - initial picked elements, required for window system', $
	'    REGION     - specifies the desired region for add list', $
	'    DELETE     - mark the list to be deleted from ROI' $
	]
	r = dialog_message(str,/info)
	return
end

	if n_elements(im) lt 4 then begin
	r = dialog_message('Input error in Im',/Error)	
	return
	end

	region_no = 1
	if keyword_set(region) then region_no = region

	otv = tvrd(xl,yl,wd,ht)
;help,otv

	bg=0
	if keyword_set(background) then bg=background
	erase,bg
	TVSCL,congrid(im,wd,ht),xl,yl

	sz = size(im)
	factor = [wd/sz(1), ht/sz(2)]

;	catch,error_status
;	if error_state ne 0 then print,!error_state.msg,!error_state.code

	x0 = xl+wd/4
	y0 = yl+ht/4
	nx = wd/2
	ny = ht/2
	box_cursor,x0,y0,nx,ny,/INIT,/MESSAGE

	ixl = 0 > (x0-xl)/factor(0) < (sz(1)-1)
	iyl = 0 > (y0-yl)/factor(1)< (sz(2)-1)
	ixr =  (ixl + nx/factor(0)) < (sz(1)-1)
	iyr =  (iyl + ny/factor(1)) < (sz(2)-1)
	if iyl eq iyr and iyr gt 0 then iyl = iyr-1
	if ixl eq ixr and ixr gt 0 then ixl = ixr-1
	zoom_box = [ixl,iyl,ixr,iyr]
;print,ixl,iyl, ixr,iyr

	im_sub = im(ixl:ixr,iyl:iyr)
	vmax = max(im)
	vmin = min(im)
	top = (!d.table_size-1) * (max(im_sub)-vmin)/(vmax-vmin)
	
	if !d.name ne 'X' then begin
	oldpicke = reform(oldpicke,sz(1),sz(2))
	oldpicke = oldpicke(ixl:ixr,iyl:iyr)
	multiplier = [wd/(ixr-ixl+1),ht/(iyr-iyl+1)]
	for jj=iyl,iyr do begin
	j = jj - iyl
	for ii=ixl,ixr do begin
	i = ii - ixl
	c_color = (!d.table_size-1) * (im_sub(i,j)-vmin)/(vmax-vmin)
	xv = xl + multiplier(0)*[i,i+1,i+1,i,i]
	yv = yl + multiplier(1)*[j,j,j+1,j+1,j]
	polyfill,xv,yv,color = c_color,/device
	end
	end
	for k=1,max(oldpicke) do begin
	defroi_listregion,im_sub,oldpicke,region=k,/reverse 
	end

	endif else begin
	; zoombox TV region

	x1 = factor(0)*[ixl,ixr+1]
	y1 = factor(1)*[iyl,iyr+1]
	sub_otv = otv(x1(0):x1(1)-1,y1(0):y1(1)-1)
	tv,congrid(sub_otv,wd,ht),xl,yl,top=top
	end

	; call pick

;print,'oldpicke=',oldpicke
	defroi_pick,im_sub,picke,region=region_no
;print,'   picke=',picke

	dx = ixr-ixl+1
	
	ct = 0
	for ij=0,n_elements(picke)-1 do begin
	if picke(ij) gt 0 then begin
		i = ixl + ij MOD dx
		j = iyl + ij / dx
		nij = i + sz(1)* j
		if ct eq 0 then elist = nij else $
			elist = [elist, nij]
		ct = ct + 1
	end
	end

	; delete  picked elements 
	if keyword_set(delete) then begin
	if n_elements(elist) gt 0 then begin
		defroi_listall,im,picke,minuslist=elist,charsize=1
	endif else begin
		defroi_listall,im,picke,charsize=1
	end
	return
	end

	; add picked elements 

	if n_elements(elist) gt 0 then begin
		elist = [elist, region_no]
		defroi_listall,im,picke,addlist=elist,charsize=1
	endif else begin
		defroi_listall,im,picke,charsize=1
	end

END

PRO defroi_addlist,picke,elist
	nel = n_elements(elist) - 1
	region_no = elist(nel)
	for i=0,nel-1 do begin
		ij = elist(i)
;		if picke(ij) eq 0 then picke(ij) = region_no
		if picke(ij) ne region_no then picke(ij) = region_no
	end
END

PRO defroi_minuslist,picke,elist,region=region
; delete a selected region or marked elements
	; 1 - delete selected region
	if keyword_set(region) then begin
		region_no = region
		for i=0,n_elements(picke)-1 do begin
			if picke(i) eq region_no then picke(i)=0
		end
	return
	end
	; 2 - delete the marked elements
	nel = n_elements(elist)
	for i=0,nel-1 do begin
		ij = elist(i)
		if picke(ij) gt 0 then picke(ij) = 0
	end
END

PRO defroi_drawcolors,xl,yl,wd,ht,color=color

	erase
	scl = !d.table_size - 1
	c_color=100L+100*256L+100*256L*256L
	if keyword_set(color) then c_color = color*(1L+256L+256L*256L)
print,c_color
	i=0
	j=0
	xv = xl + wd*[i,i+1,i+1,i,i]
	yv = yl + ht*[j,j,j+1,j+1,j]
	polyfill,xv,yv,color = c_color,/device
END

PRO defroi_refresh,im,width=width,height=height,xpos=xpos,ypos=ypos,print=print,charsize=charsize

	xl=80
	yl=80
	wd=340
	ht=340
	TVSCL,congrid(im,wd,ht),xl,yl
	
END

PRO defroi_listregion,im,picke,region_data,region=region,refresh=refresh,width=width,height=height,xpos=xpos,ypos=ypos,print=print,charsize=charsize,reverse=reverse
;+
; INPUT: 
;   Im          - image array
;   Picke       - vector of selected ROI numbers in byte
;   Region_data - returns the data structure for a selected region
;
;	region_data = { nelem: sel_no, $  number of elements in region
;		el_list: el_list, $   elements indices in region
;		total:  sel_total, $    Total
;		ave:    ave, $		Average
;		var:    var, $          Variance
;		dev:    dev, $          Standard deviation
;		min:    temp_min, $     Minimum in region    
;		max:    temp_max  $     Maximum in region  
; KEYWORD:
;     REGION    - specifies the region number, default 1
;     CHARSIZE  - if specified, mark the region with the region number
;     PRINT     - list the values of the region element
;-

	xl=80
	yl=80
	wd=340
	ht=340
	if keyword_set(refresh) then TVSCL,congrid(im,wd,ht),xl,yl

	sz = size(im)
	multiplier = [float(wd)/sz(1),float(ht)/sz(2)]
 	region_no = region 
	nregion = max(picke)
	if region_no gt nregion then begin
		str = 'Error: ROI '+strtrim(nregion,2) + ' not found!'
		r = dialog_message(str,/error)
		return
	end
	nelem = n_elements(picke)
	csize=1
	if keyword_set(charsize) then csize=charsize

if  keyword_set(print) then $
print,'      IJ           I           J      IM(I,J)      ROI#'
xtr = strtrim(region_no,2)

	orient = 45
	if keyword_set(reverse) then orient = -45
	device,set_graphics_function=6
	for ij=0,nelem-1 do begin
		if picke(ij) eq region_no then begin
		i = ij mod sz(1)
		j = ij / sz(1)
		xv = xl + multiplier(0)*[i,i+1,i+1,i,i]
		yv = yl + multiplier(1)*[j,j,j+1,j+1,j]
		polyfill,xv,yv,line_fill=1,orientation=orient,/device

		if csize gt 0 then $
		xyouts,xv(0),yv(0),xtr,/device,charsize=csize

if keyword_set(print) then print,ij,i,j,im(i,j),region_no
		if n_elements(sel_list) eq 0 then begin
			sel_list = im(i,j) 
			el_list = ij
		end else begin
			sel_list = [sel_list, im(i,j)]
			el_list = [el_list,ij]
		end
		end
	end
	device,set_graphics_function=3

	if n_elements(sel_list) gt 1 then begin
		sel_total = total(sel_list)
		sel_no = n_elements(sel_list)
		res = moment(sel_list,sdev=dev,mdev=mdev)
		ave = res(0)
		var = res(1)
		temp_min = min(sel_list,jmin)
		temp_max = max(sel_list,jmax)
		min_i = el_list(jmin) MOD sz(1)
		min_j = el_list(jmin) / sz(1)
		max_i = el_list(jmax) MOD sz(1)
		max_j = el_list(jmax) / sz(1)
	end

	if n_elements(sel_no) then $
	region_data = { nelem: sel_no, $
		el_list: el_list, $
		total:  sel_total, $
		ave:    ave, $
		var:    var, $
		dev:    dev, $
		min_i:	min_i, $
		min_j:	min_j, $
		max_i:	max_i, $
		max_j:	max_j, $
		min:    temp_min, $
		max:    temp_max  $
	}	

END

PRO defroi_listall,im,picke,width=width,height=height,xpos=xpos,ypos=ypos,file=file,print=print,charsize=charsize,addlist=addlist,minuslist=minuslist
;+
;     CHARSIZE  - if specified, mark the pixel with region number
;     PRINT     - list the values of the region element
;     file      - override the default filename 'roi.pick' if file is specified 
;-

x0=80
y0=80
wd=340
ht=340
csize = 0.
if keyword_set(charsize) then csize=charsize
if keyword_set(width) then wd = width
if keyword_set(height) then ht = height
if keyword_set(xpos) then x0 = xpos
if keyword_set(ypos) then y0 = ypos
TVSCL,congrid(im,wd,ht),x0,y0

sz = size(im)
multiplier = [wd/sz(1),ht/sz(2)]
filename = 'roi.pick'
if keyword_set(file) then filename = file

nelem = sz(1)*sz(2)
picke = make_array(nelem,/byte)

	found = findfile(filename,count=ct)
	if ct gt 0 then begin
	u_openr,unit,filename,/XDR
	u_read,unit,picke
	u_close,unit
	end
	; redefine the region of interest
	if nelem lt n_elements(picke) then begin
		addlist = 1 
		picke = make_array(nelem,/byte)
	end

	if nelem gt n_elements(picke) then begin
		addlist = 1
		picke = make_array(nelem,/byte)
	end

	if keyword_set(addlist) then begin
		defroi_addlist,picke,addlist
		u_openw,unit,filename,/XDR
		u_write,unit,picke
		u_close,unit
		end

	if keyword_set(minuslist) then begin
		if minuslist(0) lt 0 then $
		defroi_minuslist,picke,region=-minuslist(0) else $
		defroi_minuslist,picke,minuslist
		u_openw,unit,filename,/XDR
		u_write,unit,picke
		u_close,unit
		end

found = findfile(filename,count=ct)
if ct eq 0 then begin
	er = dialog_message(filename+' not found',/Error)
	return
end

	max_region = max(picke)

	; marked the readin  picked elements

  for k=1,max_region do begin
	defroi_listregion,im,picke,region_data,region=k,charsize=csize ;,/print
  end

END


PRO defroi_pick,im,picke,width=width,height=height,xpos=xpos,ypos=ypos,listall=listall,clear=clear,new=new,modify=modify,print=print,region=region,ave=ave,dev=dev,var=var,readonly=readonly,charsize=charsize
;+
;    LMB - toggle the pixel selection, the selected pixel is marked by lines
;    MMB - print the data value at the mouse cursor position
;    RMB - terminate the selection and update the saved ROI, default filename
;          'roi.pick'
; INPUT
;    Im     - input image array
;    Picke  - returns the vector indicator 1 - picked, 0 - not picked
; KEYWORD
;    XPOS   - image lower left pixel position
;    YPOS   - image lower bottom pixel position
;    WIDTH  - total width of pixels used for displaying im
;    HEIGHT - total height of pixels used for displaying im
;    CLEAR  - refresh the TV 
;    NEW    - saving as new roi.pick file
;    MODIFY - read in previous roi.pick and let the user modify the ROI by
;             toggling the desired pixel
;    PRINT  - if specified, print the array result
;    READONLY - only read the region data no modification allowed
;    CHARSIZE - charsize mark the picked pixel
;    REGION - specified the region number to be examined, default 0
;    EL_LIST - returns the selected element list for the specified region
;    AVE     - returns the average value for the specified region
;    DEV     - returns the standard deviation value for the specified region
;    VAR     - returns the variance for the specified region
;-

region_no = 1
if keyword_set(region) then region_no = region

csize = 1
if keyword_set(charsize) then csize=charsize

x0=80
y0=80
wd=340
ht=340
if keyword_set(width) then wd = width
if keyword_set(height) then ht = height
if keyword_set(xpos) then x0 = xpos
if keyword_set(ypos) then y0 = ypos
; TVSCL,congrid(im,wd,ht),x0,y0

sz = size(im)
multiplier = [wd/sz(1),ht/sz(2)]
filename = 'roi.pick'
if keyword_set(modify) then begin
	 if strlen(strtrim(modify,2)) gt 1 then filename = modify
end
vmax = max(im)
vmin = min(im)

nelem = sz(1)*sz(2)
picke = make_array(nelem,/byte)

found = findfile(filename,count=ct)
if ct gt 0 then begin
	if keyword_set(modify) then begin
		u_openr,unit,filename,/XDR
		u_read,unit,picke
		u_close,unit


	; marked the readin  picked elements

	for ij=0,nelem-1 do begin
		if picke(ij) eq region_no then begin
		i = ij mod sz(1)
		j = ij / sz(1)
;		xv = x0 + multiplier(0)*[i,i+1,i+1,i,i]
;		yv = y0 + multiplier(1)*[j,j,j+1,j+1,j]
;		polyfill,xv,yv,line_fill=1,orientation=45,/device
if keyword_set(print) then print,ij,i,j,im(i,j),region_no
		if n_elements(sel_list) eq 0 then begin
			sel_list = im(i,j) 
			el_list = ij
		end else begin
			sel_list = [sel_list, im(i,j)]
			el_list = [el_list,ij]
		end
		end
	end

		if n_elements(sel_list) gt 1 then begin
		sel_total = total(sel_list)
		sel_no = n_elements(sel_list)
		res = moment(sel_list,sdev=dev,mdev=mdev)
		ave = res(0)
		var = res(1)
		temp_min = min(sel_list,jmin)
		temp_max = max(sel_list,jmax)
		min_i = jmin MOD sz(1)
		min_j = jmin / sz(1)
		max_i = jmax MOD sz(1)
		max_j = jmax / sz(1)
		end

	if keyword_set(readonly) then begin
	device,set_graphics_function=3
	return
	end
	end
end ;  if else new=1

	device,set_graphics_function=6

	str = ['You are in ROI discrete pixel selection mode', $
		'for region # ='+strtrim(region_no)]
r = dialog_message(str,/info)
cursor,x,y,/device,/down

 while (!mouse.button ne 4) do begin
	i = (x-x0)/multiplier(0)
	j = (y-y0)/multiplier(1)
	ij = j*sz(1)+i
	if !mouse.button eq 1 then begin
	if picke(ij) eq 0 then picke(ij) = region_no else picke(ij)=0
 
	; mark/unmark  the picked elements

	xv = x0 + multiplier(0)*[i,i+1,i+1,i,i]
	yv = y0 + multiplier(1)*[j,j,j+1,j+1,j]
	polyfill,xv,yv,line_fill=1,orientation=45,/device 
	
 if keyword_set(print) then print,ij,i,j,im(i,j),region_no
	end
		 
 cursor,x,y,/device,/DOWN
 end

device,set_graphics_function=3

	if !mouse.button eq 4 then begin
		device,set_graphics_function=3
		if keyword_set(modify) or keyword_set(new) then begin
		u_openw,unit,filename,/XDR
		u_write,unit,picke
		u_close,unit
		end
;	defroi_listall,im,charsize=csize
	return
	end
END
