;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
;
; multiroi_pick.pro
;

PRO graph_region,xl,yl,wd,ht,xsize,ysize
	xl =30
	yl =30
	wd =300
	ht =300
	xsize = wd + 2*xl
	ysize = ht + 2*yl
END

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
	factor = [float(wd)/sz(1), float(ht)/sz(2)]

;	catch,error_status
;	if error_state ne 0 then print,!error_state.msg,!error_state.code

	x0 = xl+wd/4
	y0 = yl+ht/4
	nx = wd/2
	ny = ht/2
	box_cursor,x0,y0,nx,ny,/INIT,/MESSAGE

	ixl = 0 > fix((x0-xl)/factor(0)) < (sz(1)-1)
	iyl = 0 > fix((y0-yl)/factor(1))< (sz(2)-1)
	ixr =  (ixl + ceil(nx/factor(0))) < (sz(1)-1)
	iyr =  (iyl + ceil(ny/factor(1))) < (sz(2)-1)
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
	multiplier = [float(wd)/(ixr-ixl+1),float(ht)/(iyr-iyl+1)]
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

	x1 = fix(factor(0)*[ixl,ixr+1])
	y1 = fix(factor(1)*[iyl,iyr+1])
	sub_otv = otv(x1(0):x1(1)-1,y1(0):y1(1)-1)
	tv,congrid(sub_otv,wd,ht),xl,yl,top=top
	end

	; call pick

;print,'oldpicke=',oldpicke
	defroi_pick,im_sub,picke,region=region_no
;print,'   picke=',picke

	dx = ixr-ixl+1
	ct = 0
	for ij=0L,n_elements(picke)-1 do begin
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
	for i=0L,nel-1 do begin
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
		for i=0L,n_elements(picke)-1 do begin
			if picke(i) eq region_no then picke(i)=0
		end
	return
	end
	; 2 - delete the marked elements
	nel = n_elements(elist)
	for i=0L,nel-1 do begin
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

;	xl=80
;	yl=80
;	wd=340
;	ht=340
	graph_region,xl,yl,wd,ht
	TVSCL,congrid(im,wd,ht),xl,yl
	
END

PRO defroi_listregion,im,picke,region_data,region=region,refresh=refresh,width=width,height=height,xpos=xpos,ypos=ypos,print=print,charsize=charsize,reverse=reverse
;
; NAME:
;   DEFROI_LISTREGION
;
; PURPOSE:
;   List region of interest
;
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
;

;	xl=80
;	yl=80
;	wd=340
;	ht=340
	graph_region,xl,yl,wd,ht
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


el_list = where(picke eq region_no)
if n_elements(el_list) gt 1 then begin
sel_list = im(el_list)

	for ij=0L,n_elements(el_list) -1 do begin
		i = el_list(ij) mod sz(1)
		j = el_list(ij) / sz(1)
		xv = xl + multiplier(0)*[i,i+1,i+1,i,i]
		yv = yl + multiplier(1)*[j,j,j+1,j+1,j]
		polyfill,xv,yv,line_fill=1,orientation=orient,/device

		if csize gt 0 then $
		xyouts,xv(0),yv(0),xtr,/device,charsize=csize

if keyword_set(print) then print,ij,i,j,im(i,j),region_no
	end
	device,set_graphics_function=3

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
;
;     CHARSIZE  - if specified, mark the pixel with region number
;     PRINT     - list the values of the region element
;     file      - override the default filename 'roi.pick' if file is specified 
;

;x0=80
;y0=80
;wd=340
;ht=340
	graph_region,x0,y0,wd,ht
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
picke = make_array(nelem,/long)

	found = findfile(filename,count=ct)
	if ct gt 0 then begin
	xdr_open,unit,filename
	xdr_read,unit,picke
	xdr_close,unit
	end
	; redefine the region of interest
	if nelem lt n_elements(picke) then begin
		addlist = 1 
		picke = make_array(nelem,/long)
	end

	if nelem gt n_elements(picke) then begin
		addlist = 1
		picke = make_array(nelem,/long)
	end

	if keyword_set(addlist) then begin
		defroi_addlist,picke,addlist
		xdr_open,unit,filename,/write
		xdr_write,unit,picke
		xdr_close,unit
		end

	if keyword_set(minuslist) then begin
		if minuslist(0) lt 0 then $
		defroi_minuslist,picke,region=-minuslist(0) else $
		defroi_minuslist,picke,minuslist
		xdr_open,unit,filename,/write
		xdr_write,unit,picke
		xdr_close,unit
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
;    REGION - specified the region number to be examined, default 1
;    EL_LIST - returns the selected element list for the specified region
;    AVE     - returns the average value for the specified region
;    DEV     - returns the standard deviation value for the specified region
;    VAR     - returns the variance for the specified region

region_no = 1
if keyword_set(region) then region_no = region

csize = 1
if keyword_set(charsize) then csize=charsize

;x0=80
;y0=80
;wd=340
;ht=340
	graph_region,x0,y0,wd,ht
if keyword_set(width) then wd = width
if keyword_set(height) then ht = height
if keyword_set(xpos) then x0 = xpos
if keyword_set(ypos) then y0 = ypos
; TVSCL,congrid(im,wd,ht),x0,y0

sz = size(im)
multiplier = [float(wd)/sz(1),float(ht)/sz(2)]
filename = 'roi.pick'
if keyword_set(modify) then begin
	 if strlen(strtrim(modify,2)) gt 1 then filename = modify
end
vmax = max(im)
vmin = min(im)

nelem = sz(1)*sz(2)
picke = make_array(nelem,/long)

found = findfile(filename,count=ct)
if ct gt 0 then begin
	if keyword_set(modify) then begin
		xdr_open,unit,filename
		xdr_read,unit,picke
		xdr_close,unit

	; marked the readin  picked elements

	for ij=0L,nelem-1 do begin
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
		'for region # ='+strtrim(region_no), $
		'LMB to pick/unpick the pixel, RMB to stop ']
r = dialog_message(str,/info)
cursor,x,y,/device,/down

 while (!mouse.button ne 4) do begin
	i = fix((x-x0)/multiplier(0))
	j = fix((y-y0)/multiplier(1))
	ij = j*sz(1)+i
	if !mouse.button eq 1 then begin
	if ij lt nelem then begin
	if picke(ij) eq 0 then picke(ij) = region_no else picke(ij)=0
 
	; mark/unmark  the picked elements

	xv = x0 + multiplier(0)*[i,i+1,i+1,i,i]
	yv = y0 + multiplier(1)*[j,j,j+1,j+1,j]
	polyfill,xv,yv,line_fill=1,orientation=-45,/device 
	
 if keyword_set(print) then print,ij,i,j,im(i,j),region_no
	end
	end
		 
 cursor,x,y,/device,/DOWN
 end

device,set_graphics_function=3

	if !mouse.button eq 4 then begin
		device,set_graphics_function=3
		if keyword_set(modify) or keyword_set(new) then begin
		xdr_open,unit,filename,/write
		xdr_write,unit,picke
		xdr_close,unit
		end
;	defroi_listall,im,charsize=csize
	return
	end
END

PRO multiroi_help

str = [ 'For detail information, please refer','', $
	'   http://www.aps.anl.gov/~cha/multiROI.html', '', $
	'MULTIROI_PICK allows the user flexiblly redefine the ROIs by', $
	'free-hand drawing and picking the pixel elements.', $ 
	'Element selected is marked with hash line and associated ROI number', $
	'','The function of each button are listed below:','', $
	'Drawing Area- Draw the marked image area', $
	'Msg Info    - Hint about mouse operation below the drawing area', $ 
	'Help...     - Show this on-line help', $
	'Read ROIs...- Read a ROIs definition file', $
	'Save ROIs...- Save / update the  ROIs definition file', $
	'Tiff/Png/Pict   - Save the ROIs to a TIFF file', $
	'Color...    - Change the IDL window color map ', $
	'Refresh     - Refresh TV image', $
	'ShowAll ROIs - Redraw and mark all the selected elements', $
	'ROI #:      - Show list of existing ROIs defined for the image', $
	'              Select the number will re-display the ROI statistic', $
	'              in the scroll area', $
	'Draw PolyROI - Redraw the PolyROI for a selected ROI #', $ 
	'Modify ROI  - Select/unselect pixels for a selected ROI #', $ 
	'               (LMB to pick/unpick pixel, RMB to stop) ', $
	'Add ROI     - Add an additional ROI to the ROI # list', $
	'               (LMB to pick/unpick pixel, RMB to stop) ', $
	'Del ROI     - Delete the selected ROI', $
	'Zoom Add    - Zoom a box region, pick pixel elements to be added to the ROI', $
	'Zoom Del    - Zoom a box region, pick pixel elements to be deleted from the ROI', $
	'Query Image - Query image mode, RMB to stop', $
	'Offset Val: - Offset value for the ROI statistic calculation', $
	'Charsize:   - Specify the charsize used in marking the pixel', $
	'Scroll Area - Display the ROI statistics if new ROI# is selected', $
	'Save As ... - Save the statistic scroll window content to a disk file', $
	'Print       - Print the statistic scroll window content to printer', $
	'Clear       - Clear the statistic scroll window', $
	'ROIsTIFF    - Show TIFF filename corresponding to ROIs filename ', $
	'Done        - Close the multi-roi selection program' $
	]
	r = dialog_message(str,/info)
END

PRO multiroi_pickregion,defroi_pickinfo,region
	im = defroi_pickinfo.im0
	picke = defroi_pickinfo.picke

	nelem = total(picke eq region)
	if nelem gt 0 then begin

	if defroi_pickinfo.offset ne 0. then im = im - defroi_pickinfo.offset

	defroi_refresh,im
	defroi_listregion,im,picke,region_data,region=region
	str = [ '===========================', $
		'Report generated at: ' + systime(0), $
		defroi_pickinfo.comment, $
		'===========================', $
                'REGION OF INTEREST : '+strtrim(region,2)]
	if n_elements(region_data) eq 0 then $
	str = [str, $
		 "ERROR: no statistic for region " + string(region), $
		'        at 2 pixels required for statistics ', $
		"***************************",'']
	if n_elements(region_data) then begin

	str = [str, $
                'MINIMUM:        '+strtrim(region_data.min,2)+'  I='+strtrim(region_data.min_i,2)+'  J='+strtrim(region_data.min_j,2), $
                'MAXIMUM:        '+strtrim(region_data.max,2)+'  I='+strtrim(region_data.max_i,2)+'  J='+strtrim(region_data.max_j,2), $
                'TOTAL:          '+strtrim(region_data.total,2), $
                'AVERAGE:        '+strtrim(region_data.ave,2), $
                'VARIANCE:       '+strtrim(region_data.var,2), $
                'DEVIATION:      '+strtrim(region_data.dev,2), $
                'OFFSET:         '+strtrim(defroi_pickinfo.offset,2), $
                'NELEM:          '+strtrim(region_data.nelem,2), $
                'N           I           J  IM(I,J)-Offset' $
        ]

		st = strarr(region_data.nelem)

		sz = size(im)
		for ij=0L,region_data.nelem-1 do begin
		i = region_data.el_list(ij) MOD sz(1)	
		j = region_data.el_list(ij) / sz(1)	
		st(ij) = strtrim(ij,2)+string(i)+string(j)+string(im(i,j))
		end

		str = [str,st]
	end

	WIDGET_CONTROL,defroi_pickinfo.text,SET_VALUE=str,/append
        end
END

PRO multiroi_picklistall,defroi_pickinfo

	im = defroi_pickinfo.im0
	picke = defroi_pickinfo.picke
	nregion = max(picke)

	FOR region=1,nregion DO BEGIN
	multiroi_pickregion,defroi_pickinfo,region
	END
END


PRO multiroi_pick_Event, Event


  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev
  WIDGET_CONTROL, Event.Top, GET_UVALUE=defroi_pickinfo 

  xsize = defroi_pickinfo.xsize
  ysize = defroi_pickinfo.ysize
  
  CASE Ev OF 

  'DEFROIPICK_DRAW': BEGIN
      END
  'DEFROIPICK_DONE': BEGIN
	WIDGET_CONTROL,Event.Top,/DESTROY
	return
      END
  'DEFROIPICK_REFRESH': BEGIN
	defroi_refresh,defroi_pickinfo.im0
      END
  'DEFROIPICK_HELP': BEGIN
	multiroi_help	
      END
  'DEFROIPICK_COLOR': BEGIN
	xloadct,GROUP=Event.top	
      END
  'DEFROIPICK_ALL': BEGIN
	WIDGET_CONTROL,defroi_pickinfo.text,SET_VALUE=''
	multiroi_picklistall,defroi_pickinfo
	im = defroi_pickinfo.im0
WIDGET_CONTROL,/HOURGLASS
	defroi_listall,im,picke,charsize=defroi_pickinfo.csize
	defroi_pickinfo.picke = picke
      END
  'DEFROIPICK_LISTREGION': BEGIN
	im = defroi_pickinfo.im0
	picke = defroi_pickinfo.picke
	region = defroi_pickinfo.region_id
	defroi_refresh,im
	defroi_listregion,im,picke,region_data,region=region
	END
  'DEFROIPICK_LIST': BEGIN
	WIDGET_CONTROL,defroi_pickinfo.text,SET_VALUE=''
	r = widget_info(Event.Id,/list_select)
	defroi_pickinfo.region_id = r+1
	im = defroi_pickinfo.im0
	picke = defroi_pickinfo.picke
	region = defroi_pickinfo.region_id
	multiroi_pickregion,defroi_pickinfo,region
      END
  'DEFROIPICK_DEL': BEGIN
	picke = defroi_pickinfo.picke
	region = defroi_pickinfo.region_id
	polyfill,[0,xsize,xsize,0],[ysize-30,ysize-30,ysize,ysize], $
		color=defroi_pickinfo.bg,/device
	xyouts,1,ysize-19,'***Del ROI '+strtrim(region,2)+' ***',/device
	im = defroi_pickinfo.im0
	defroi_listall,im,picke,charsize=defroi_pickinfo.csize,minuslist=-region
	defroi_pickinfo.picke = picke
	polyfill,[0,xsize,xsize,0],[ysize-30,ysize-30,ysize,ysize], $
		color=defroi_pickinfo.bg,/device
	END
  'DEFROIPICK_ADD': BEGIN
	picke = defroi_pickinfo.picke
	region = max(picke) + 1
	polyfill,[0,xsize,xsize,0],[ysize-30,ysize-30,ysize,ysize], $
		color=defroi_pickinfo.bg,/device
	xyouts,1,ysize-19,'***Add ROI '+strtrim(region,2)+': LMB pick element, RMB stop***',/device
	defroi_pickinfo.region_max = region  
	str = string( defroi_pickinfo.list(1:defroi_pickinfo.region_max))
	WIDGET_CONTROL,defroi_pickinfo.listwid,set_value=str
  	WIDGET_CONTROL, defroi_pickinfo.listwid, SET_LIST_SELECT=defroi_pickinfo.region_max-1
	im = defroi_pickinfo.im0
	defroi_listall,im,picke,charsize=defroi_pickinfo.csize
	; add the new region selection
	defroi_pickinfo.region_id = defroi_pickinfo.region_max
	region = defroi_pickinfo.region_id
;	defroi_pick,im,picke,region=region,/modify
	defroi_pick,im,picke,region=region,modify='roi.pick'
	defroi_pickinfo.picke = picke
	defroi_listall,im,picke,charsize=defroi_pickinfo.csize
	polyfill,[0,xsize,xsize,0],[ysize-30,ysize-30,ysize,ysize], $
		color=defroi_pickinfo.bg,/device
      END
  'DEFROIPICK_QUERY': BEGIN
	if defroi_pickinfo.help then $
	str="Use Right Mouse Button to stop query."
	WIDGET_CONTROL,defroi_pickinfo.msg,SET_VALUE=str
;	r = dialog_message("Use Right Mouse Button to stop query.",/info)
	polyfill,[0,xsize,xsize,0],[ysize-30,ysize-30,ysize,ysize], $
		color=defroi_pickinfo.bg,/device
	xyouts,1,ysize-19,defroi_pickinfo.cursor_val,/device
	cursor,x,y,/device,/change
	while (!mouse.button ne 4) do begin
	x = !mouse.x
	y = !mouse.y
	im = defroi_pickinfo.im0
	sz = size(im)
	zoom = [float(defroi_pickinfo.wd)/sz(1),float(defroi_pickinfo.ht)/sz(2)]
	i = fix((x - defroi_pickinfo.xl)/zoom(0))
	j = fix((y - defroi_pickinfo.yl)/zoom(1))
	if i lt 0 then i = 0
	if j lt 0 then j = 0
	if i ge sz(1) then i = sz(1) - 1
	if j ge sz(2) then j = sz(2) - 1
	str = '***IMAGE('+strtrim(i,2)+','+strtrim(j,2)+')='+strtrim(im(i,j),2)
	cursor,x,y,/device,/change
	polyfill,[0,xsize,xsize,0],[ysize-30,ysize-30,ysize,ysize], $
		color=defroi_pickinfo.bg,/device
	xyouts,1,ysize-19,str,/device,charsize=1.5 ;2
	end
      END
  'DEFROIPICK_DRAWROI': BEGIN
	polyfill,[0,xsize,xsize,0],[ysize-30,ysize-30,ysize,ysize], $
		color=defroi_pickinfo.bg,/device
	xyouts,1,ysize-19,'***Draw PolyROI',/device
        im = defroi_pickinfo.im0
	sz = size(im)
        defroi_refresh,im
        xw = defroi_pickinfo.wd
        yw = defroi_pickinfo.ht
        xl = defroi_pickinfo.xl
        yl = defroi_pickinfo.yl
	zoom = [float(xw)/sz(1),float(yw)/sz(2)]
        r = defroi(xw,yw,xverts,yverts,x0=xl,y0=yl)
	xverts = float(xverts) /zoom(0)
	yverts = float(yverts) /zoom(1)
	xv = fix(xverts+0.5)
	yv = fix(yverts+0.5)
        arr = polyfillv(xv,yv,sz(1),sz(2))
        picke = defroi_pickinfo.picke
 
        defroi_listall,im,picke,addlist=[arr,defroi_pickinfo.region_id],charsize=defroi_pickinfo.csize
        defroi_pickinfo.picke = picke
	polyfill,[0,xsize,xsize,0],[ysize-30,ysize-30,ysize,ysize], $
		color=defroi_pickinfo.bg,/device
      END
  'DEFROIPICK_MOD': BEGIN
	region = defroi_pickinfo.region_id
	polyfill,[0,xsize,xsize,0],[ysize-30,ysize-30,ysize,ysize], $
		color=defroi_pickinfo.bg,/device
	xyouts,1,ysize-19,'***Modify ROI '+strtrim(region,2)+': LMB pick element, RMB stop***',/device
	im = defroi_pickinfo.im0
	defroi_listall,im,picke,charsize=defroi_pickinfo.csize
	defroi_pick,im,picke,region=region,modify='roi.pick'
	defroi_pickinfo.picke = picke
	defroi_listall,im,picke,charsize=defroi_pickinfo.csize
	polyfill,[0,xsize,xsize,0],[ysize-30,ysize-30,ysize,ysize], $
		color=defroi_pickinfo.bg,/device
      END
  'DEFROIPICK_ZOOMDEL': BEGIN
	polyfill,[0,xsize,xsize,0],[ysize-30,ysize-30,ysize,ysize], $
		color=defroi_pickinfo.bg,/device
	xyouts,1,ysize-19,'***Zoom Del: LMB pick element, RMB stop***',/device
	if defroi_pickinfo.help then begin
	str =['Zoom Del Mode:  resize zoom box, delete pixels', $
		'Zoom Box Control: ',$
		'    Drag LMB - Reposition box',$
		'    Drag RMB - Resize box', $
		'    Click RMB - Accept box region', $ 
		'Select Pixels:', $
		'    LMB - Pickup the pixels to be deleted', $
		'    RMB - Done with selection ']
	WIDGET_CONTROL,defroi_pickinfo.msg,SET_VALUE=str
	end
	im = defroi_pickinfo.im0
	xl = defroi_pickinfo.xl
	yl = defroi_pickinfo.yl
	wd = defroi_pickinfo.wd
	ht = defroi_pickinfo.ht
	picke = defroi_pickinfo.picke
	region = defroi_pickinfo.region_id
	defroi_zoombox,im,xl,yl,wd,ht,zoom_box,picke,oldpicke=picke,region=region,/delete
	defroi_pickinfo.picke = picke
	polyfill,[0,xsize,xsize,0],[ysize-30,ysize-30,ysize,ysize], $
		color=defroi_pickinfo.bg,/device
      END
  'DEFROIPICK_ZOOMADD': BEGIN
	region = defroi_pickinfo.region_id
	polyfill,[0,xsize,xsize,0],[ysize-30,ysize-30,ysize,ysize], $
		color=defroi_pickinfo.bg,/device
	xyouts,1,ysize-19,'***Zoom Add '+strtrim(region,2)+': LMB pick element, RMB stop***',/device
	if defroi_pickinfo.help then begin
	str =['Zoom Add Mode:  resize box, pick pixel', $
		'Zoom Box Control: ',$
		'    Drag LMB - Reposition box',$
		'    Drag RMB - Resize box', $
		'    Click RMB - Accept box region', $ 
		'Select Pixels:', $
		'    LMB - Pickup the pixel', $
		'    RMB - Done with selection']
	WIDGET_CONTROL,defroi_pickinfo.msg,SET_VALUE=str
	end
	im = defroi_pickinfo.im0
	xl = defroi_pickinfo.xl
	yl = defroi_pickinfo.yl
	wd = defroi_pickinfo.wd
	ht = defroi_pickinfo.ht
	picke = defroi_pickinfo.picke
	defroi_zoombox,im,xl,yl,wd,ht,zoom_box,picke,oldpicke=picke,region=defroi_pickinfo.region_id
	defroi_pickinfo.picke = picke
	polyfill,[0,xsize,xsize,0],[ysize-30,ysize-30,ysize,ysize], $
		color=defroi_pickinfo.bg,/device
      END
  'DEFROIPICK_ROIREAD': BEGIN
	path=defroi_pickinfo.path
	filename = dialog_pickfile(filter='*roi.pick*', $
		path=defroi_pickinfo.path, Title='Read ROIs File',$
		GET_PATH=gp,/READ,/MUST_EXIST)

	if filename eq '' then return

	defroi_pickinfo.roifile = filename

	WIDGET_CONTROL,defroi_pickinfo.text,SET_VALUE=filename 
	p1 = strpos(filename,!os.file_sep,/reverse_search)
	name = strmid(filename,p1+1,strlen(filename)-p1)
	defroi_pickinfo.tiffname = filename+'.tiff'
	WIDGET_CONTROL,defroi_pickinfo.tiffwid,SET_VALUE=name+'.tiff'

	xdr_open,unit,filename
	xdr_read,unit,picke,ERROR=er
	if er ne 0 then begin
		xdr_close,unit
		r = dialog_message('Error: wrong type of data picked',/error)
		return
	end
	xdr_close,unit

	xdr_open,unit2,'roi.pick',/write
	xdr_write,unit2,picke
	xdr_close,unit2

	im = defroi_pickinfo.im0
	defroi_listall,im,picke,charsize=defroi_pickinfo.csize
	defroi_pickinfo.picke = picke
	nregion = max(picke)
	if nregion gt 1 then begin
	defroi_pickinfo.region_max =  nregion 
	str = string( defroi_pickinfo.list(1:defroi_pickinfo.region_max))
	WIDGET_CONTROL,defroi_pickinfo.listwid,set_value=str
	end

      END
  'DEFROIPICK_ROISAVE': BEGIN
	file0 = 'roi.pick'
	path=defroi_pickinfo.path

	file1 = defroi_pickinfo.roifile
	p = strpos(file1,!os.file_sep,/reverse_search)
	file = strmid(file1,p+1,strlen(file1)-p-1)

	filename = dialog_pickfile(filter='*roi.pick*', $
		path=defroi_pickinfo.path, Title='Save ROIs File',$
		file=file1, $
		GET_PATH=gp,/WRITE)

	if filename eq '' then return

	WIDGET_CONTROL,defroi_pickinfo.text,SET_VALUE=filename 
	p1 = strpos(filename,!os.file_sep,/reverse_search)
	name = strmid(filename,p1+1,strlen(filename)-p1)
	defroi_pickinfo.roifile = filename
	defroi_pickinfo.tiffname = filename+'.tiff'
	WIDGET_CONTROL,defroi_pickinfo.tiffwid,SET_VALUE=name+'.tiff'

	xdr_open,unit,'roi.pick'
	xdr_read,unit,picke
	xdr_close,unit

	xdr_open,unit2,filename,/write
	xdr_write,unit2,picke
	xdr_close,unit2
      END
  'DEFROIPICK_ROITIFF': BEGIN
      pngname = defroi_pickinfo.tiffname
	type = defroi_pickinfo.tifftype
      tvlct,r,g,b,/get
      case type of 
	0: write_tiff,pngname,reverse(TVRD(),2),red=r,green=g,blue=b 
	1: write_png,pngname,TVRD(),r,g,b
        2: write_pict,pngname,TVRD(),r,g,b
      endcase
; print,'pngname=',pngname
      END
  'DEFROIPICK_TEXT': BEGIN
      Print, 'Event for DEFROIPICK_TEXT'
      END

  'DEFROIPICK_SAVEIMG': BEGIN
        type = WIDGET_INFO(Event.ID,/DROPLIST_SELECT)
	name = defroi_pickinfo.roifile 
        case type of
        0: newname = name + '.tiff'
        1: newname = name + '.png'
        2: newname = name + '.pict'
        endcase
	p1 = strpos(newname,!os.file_sep,/reverse_search)
	name = strmid(newname,p1+1,strlen(newname)-p1)
        WIDGET_CONTROL,defroi_pickinfo.tiffwid,SET_VALUE = name
	defroi_pickinfo.tiffname = newname
	defroi_pickinfo.tifftype = type
;print,'newname=',newname
        END
  'DEFROIPICK_TIFFNAME': BEGIN
	WIDGET_CONTROL,Ev.Id,GET_VALUE=st
	defroi_pickinfo.tiffname = st(0)
      END

  'DEFROIPICK_TEXTSAVE': BEGIN
	filename = dialog_pickfile(filter='*rois.rpt*', $
		path=defroi_pickinfo.path, $
		file = defroi_pickinfo.class+'rois.rpt', $
		get_path=gpath,/write,title='Save rois.rpt')
	if filename eq gpath then begin
	r = dialog_message('Error: the file name end with "rois.rpt" is required',/error)
	return
	end
	found = findfile(filename,count=ct)
	if ct gt 0 then begin
		r = dialog_message(['Overwite the existing file:',filename],/question)
		if r eq 'No' then return  
	end
	WIDGET_CONTROL,defroi_pickinfo.text,GET_VALUE=str
	openw,1,filename
	for i=0,n_elements(str)-1 do printf,1,str(i)
	close,1
      END
  'DEFROIPICK_TEXTPRINT': BEGIN
	WIDGET_CONTROL,defroi_pickinfo.text,GET_VALUE=str 
	openw,1,'rois.rpt'
	for i=0,n_elements(str)-1 do printf,1,str(i)
	close,1
	PS_print,'rois.rpt'
      END
  'DEFROIPICK_TEXTCLEAR': BEGIN
	WIDGET_CONTROL,defroi_pickinfo.text,SET_VALUE=''
      END
  'DEFROIPICK_BACKGROUND': BEGIN
	WIDGET_CONTROL,defroi_pickinfo.slider,GET_VALUE=bg
	defroi_pickinfo.bg = bg
	erase,bg
	defroi_refresh,defroi_pickinfo.im0
	picke = defroi_pickinfo.picke
	im = defroi_pickinfo.im0
	device,set_graphics_function=3
	defroi_listall,im,picke,charsize=1
	device,set_graphics_function=6
      END
  'DEFROIPICK_OFFSET': BEGIN
	WIDGET_CONTROL,defroi_pickinfo.offwid,GET_VALUE=str
	defroi_pickinfo.offset = str
      END
  'DEFROIPICK_CSIZE': BEGIN
	WIDGET_CONTROL,defroi_pickinfo.csizewid,GET_VALUE=str
	defroi_pickinfo.csize = str
	print,defroi_pickinfo.csize
      END
  ENDCASE

  WIDGET_CONTROL,defroi_pickinfo.msg,SET_VALUE=''
  WIDGET_CONTROL, defroi_pickinfo.base, SET_UVALUE=defroi_pickinfo 

END



PRO multiroi_pick,im, GROUP=Group,CLASS=Class,bg=bg,comment=comment,header=header
;+
; NAME: 
;    MULTIROI_PICK
;
; PURPOSE:
;    This routine provide a flexible 2D image Region Of Interest statistic
;    program. It supports multiple ROIs by allowing the user dynamically
;    to select the ROIs and modify the ROIs by toggling the pixels.  
;
;    It is a complete mouse driven program. It let user easily generated
;    the statistic ROI report for an arbitrary input 2D image. 
;
; CATEGORY:
;    Widgets.
;
; CALLING SEQUENCE:
;    
;    MULTIROI_PICK, IM
;
; INPUTS:
;    IM:     Input 2D image array
;
; KEYWORD PARAMETERS:
;    GROUP:  Specifies the group leader of the widget, the death of the group
;            leader results in the death of MULTIROI_PICK.
;
;    CLASS:  Specifies the file name where the image was originally extracted.
;            If this keyword is specified, it tells the MULTIROI_PICK where
;            to store the statistic output file.
;
;   COMMENT: Specifies the comment about the image data
;
;    BG:     Specifies the background color, default is black
;    HEADER: Specifies the header to be displayed 
;
; MODIFICATION HISTORY:
;      Written by:     Ben-chin Cha, Aug 4, 2000.
;      01-24-2001  bkc R1.1
;                      Replace the xdr read/write by the new method from the
;                      xdr_open.pro    
;		       Improve the efficiency for handling very large image data
;      05-14-2002  bkc Add the option of read/save the multiple roi.pick file 
;                      Add the option of saving Tiff/Png/Pict image file
;-

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


graph_region,xl,yl,wd,ht,xsize,ysize

  multiroi_pickBase = WIDGET_BASE(GROUP_LEADER=Group, $
	title='MULTIROI_PICK R1.1 ', $
      ROW=1, $
      MAP=1, $
      UVALUE='multiroi_pickBase')

  BASE2 = WIDGET_BASE(multiroi_pickBase, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE3 = WIDGET_BASE(BASE2, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE3')

  DRAW23 = WIDGET_DRAW( BASE3, $
      BUTTON_EVENTS=1, $
  ;    MOTION_EVENTS=1, $
      RETAIN=2, $
      UVALUE='DEFROIPICK_DRAW', $
      XSIZE=xsize, $
      YSIZE=ysize)

  TEXTMSG = WIDGET_TEXT( BASE3,VALUE='', $
      EDITABLE=0, /SCROLL, $
      UVALUE='DEFROIPICK_MESSAGE', $
      XSIZE=40, $
      YSIZE=10)


  BASE4 = WIDGET_BASE(BASE2, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE4')

  BUTTON24 = WIDGET_BUTTON( BASE4, $
      UVALUE='DEFROIPICK_HELP', $
      VALUE='Help...')

  BUTTON35 = WIDGET_BUTTON( BASE4, $
      UVALUE='DEFROIPICK_ROIREAD', $
      VALUE='Read ROIs...')

  BUTTON36 = WIDGET_BUTTON( BASE4, $
      UVALUE='DEFROIPICK_ROISAVE', $
      VALUE='Save ROIs...')

  BUTTON36 = WIDGET_BUTTON( BASE4, $
      UVALUE='DEFROIPICK_ROITIFF', $
      VALUE='Tiff/Png/Pict')

  BUTTON24 = WIDGET_BUTTON( BASE4, $
      UVALUE='DEFROIPICK_COLOR', $
      VALUE='Color...')

  BUTTON26 = WIDGET_BUTTON( BASE4, $
      UVALUE='DEFROIPICK_REFRESH', $
      VALUE='Refresh')

  BUTTON27 = WIDGET_BUTTON( BASE4, $
      UVALUE='DEFROIPICK_ALL', $
      VALUE='ShowAll ROIs')

;  BUTTON28 = WIDGET_BUTTON( BASE4, $
;      UVALUE='DEFROIPICK_LISTREGION', $
;      VALUE='ShowPick ROI')

  BASE36 = WIDGET_BASE(BASE4, $
      ROW=1, $
      FRAME=1, $
      MAP=1, $
      UVALUE='BASE36')

  LABEL37 = WIDGET_LABEL( BASE36, $
      UVALUE='LABEL37', $
      VALUE='ROI # :')

  roilist = indgen(32) 
  str = string(roilist(1))
  LIST41 = WIDGET_LIST( BASE36,VALUE=str, $
      UVALUE='DEFROIPICK_LIST', $
      YSIZE=5)

  BUTTON39 = WIDGET_BUTTON( BASE4, $
      UVALUE='DEFROIPICK_DRAWROI', $
      VALUE='Draw PolyROI')

  BUTTON32 = WIDGET_BUTTON( BASE4, $
      UVALUE='DEFROIPICK_MOD', $
      VALUE='Modify ROI')

  BUTTON30 = WIDGET_BUTTON( BASE4, $
      UVALUE='DEFROIPICK_ADD', $
      VALUE='Add ROI')

  BUTTON35 = WIDGET_BUTTON( BASE4, $
      UVALUE='DEFROIPICK_DEL', $
      VALUE='Del ROI')

  BUTTON33 = WIDGET_BUTTON( BASE4, $
      UVALUE='DEFROIPICK_ZOOMADD', $
      VALUE='Zoom Add')

  BUTTON34 = WIDGET_BUTTON( BASE4, $
      UVALUE='DEFROIPICK_ZOOMDEL', $
      VALUE='Zoom Del')

  BUTTON31 = WIDGET_BUTTON( BASE4, $
      UVALUE='DEFROIPICK_QUERY', $
      VALUE='Query Image')

  BASE5 = WIDGET_BASE(BASE2, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE5')

  if keyword_set(header) then $
  label1 = WIDGET_LABEL(BASE5,VALUE=header)

  BASE5_0 = WIDGET_BASE(BASE5, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE5_0')

  offset_field = CW_FIELD( BASE5_0,VALUE=0., $
      ROW=1, FLOAT=1, RETURN_EVENTS= 1, $
      TITLE='Offset Val: ', $
      XSIZE=8, $
      UVALUE='DEFROIPICK_OFFSET')

  csize_field = CW_FIELD( BASE5_0,VALUE=1., $
      ROW=1, FLOAT=1, RETURN_EVENTS= 1, $
      TITLE='Charsize: ', $
      XSIZE=8, $
      UVALUE='DEFROIPICK_CSIZE')


  TEXT6 = WIDGET_TEXT( BASE5,VALUE='', $
      EDITABLE=1, /SCROLL, $
      UVALUE='DEFROIPICK_TEXT', $
      XSIZE=30, $
      YSIZE=20)

  BASE7 = WIDGET_BASE(BASE5, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE7')

  BUTTON8 = WIDGET_BUTTON( BASE7, $
      UVALUE='DEFROIPICK_TEXTSAVE', $
      VALUE='Save As...')

  BUTTON9 = WIDGET_BUTTON( BASE7, $
      UVALUE='DEFROIPICK_TEXTPRINT', $
      VALUE='Print')

  BUTTON10 = WIDGET_BUTTON( BASE7, $
      UVALUE='DEFROIPICK_TEXTCLEAR', $
      VALUE='Clear')

  Slider = WIDGET_SLIDER( BASE7, MAX=!d.table_size, MIN=0, $
      UVALUE='DEFROIPICK_BACKGROUND')

  BASE8 = WIDGET_BASE(BASE5, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE8')

  img_output = WIDGET_DROPLIST( BASE8,VALUE=['TIFF','PNG','PICT'], $
      UVALUE = "DEFROIPICK_SAVEIMG",TITLE='ROIs')

  TIFFFIELD = CW_FIELD( BASE8,VALUE='', $
      ROW=1, /NOEDIT, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE=':', XSIZE=50, $
      UVALUE='DEFROIPICK_TIFFNAME')

  BUTTON25 = WIDGET_BUTTON( BASE5, $
      UVALUE='DEFROIPICK_DONE', $
      VALUE='Done')


  WIDGET_CONTROL, multiroi_pickBase, /REALIZE

  ; Get drawable window index

  COMMON DRAW23_Comm, DRAW23_Id
  WIDGET_CONTROL, DRAW23, GET_VALUE=DRAW23_Id

	sz = size(im)
  	cursor_val = '*** Query cursor image value, RMB to stop ***'

defroi_pickinfo = { $
	roifile : 'roi.pick', $      ; multiple roi.pick defined file
	path : 'ROI'+ !os.file_sep, $
	class : '', $                ; file class 
	comment: '', $
	help : 1, $
	base : multiroi_pickBase, $
	text : TEXT6, $
	msg : TEXTMSG, $
	wid : DRAW23_Id, $
	listwid : LIST41, $
	list: roilist, $
	csizewid : csize_field, $
	csize : 1., $
	offwid : offset_field, $
	offset : 0., $
	tiffwid : TIFFFIELD, $
	tiffname : '', $            ; output roi.pick tiff file
	tifftype : 0, $
	cursor_val : cursor_val, $
	xsize : xsize, $
	ysize : ysize, $
	xl : xl, $
	yl : yl, $
	wd : wd, $
	ht : ht, $
	scale : [wd/sz(1),ht/sz(2)], $
	bg : 0, $
	slider : slider, $
	region_id : 1, $
	region_max : 1, $
	picke: make_array(n_elements(im),/long), $
	im0 : im $
	}

	if keyword_set(comment) then defroi_pickinfo.comment = comment
	if keyword_set(bg) then defroi_pickinfo.bg = bg
	if keyword_set(class) then begin
		 defroi_pickinfo.class = class
		 len = strpos(class,!os.file_sep,/reverse_search)
		 if len ge 0 then $ 
		 defroi_pickinfo.path = strmid(class,0,len+1)
	end

; tiff and roi.pick setup

	defroi_pickinfo.roifile = defroi_pickinfo.class+'roi.pick'
	defroi_pickinfo.tiffname = defroi_pickinfo.class+'roi.pick.tiff'
	len = strpos(defroi_pickinfo.tiffname,!os.file_sep,/reverse_search)
	name = strmid(defroi_pickinfo.tiffname,len+1,strlen(defroi_pickinfo.tiffname)-len)
	WIDGET_CONTROL,TIFFFIELD,SET_VALUE=name

	defroi_listall,im,picke,charsize=defroi_pickinfo.csize
	defroi_pickinfo.picke = picke
	nregion = max(picke)
	if nregion gt 1 then begin
	defroi_pickinfo.region_max =  nregion 
	str = string( defroi_pickinfo.list(1:defroi_pickinfo.region_max))
	WIDGET_CONTROL,defroi_pickinfo.listwid,set_value=str
	end
  	WIDGET_CONTROL, LIST41, SET_LIST_SELECT=0

  WIDGET_CONTROL, multiroi_pickBase, SET_UVALUE=defroi_pickinfo 
	
  XMANAGER, 'multiroi_pick', multiroi_pickBase
END
