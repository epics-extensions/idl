;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
@PS_open.pro

PRO defroi_congrid,statistic_2dids,arr,xverts,yverts,xs=xs,ys=ys,x0=x0,y0=y0
; define a polygon region
;

	im = *statistic_2dids.im0

	wset,statistic_2dids.wid

	xl = statistic_2dids.margin_l
	yl = statistic_2dids.margin_b
	xw = statistic_2dids.xsize
	yw = statistic_2dids.ysize

	if keyword_set(x0) then xl = x0
	if keyword_set(y0) then yl = y0
	if keyword_set(xs) then xw = xs
	if keyword_set(ys) then yw = ys

	sz = size(*statistic_2dids.im0)
	width = statistic_2dids.width ; sz(1)
	height = statistic_2dids.height ; sz(2)

	zoom = [xw/sz(1),yw/sz(2)]
	tvscl,congrid(im,xw,yw),xl,yl
	r = defroi(xw+1,yw+1,xv,yv,x0=xl,y0=yl)  
	xverts = xv / statistic_2dids.factor(0)
	yverts = yv / statistic_2dids.factor(1)
	xverts = fix(xverts+.5)
	yverts = fix(yverts+.5)
	arr = polyfillv(xverts,yverts,statistic_2dids.width,statistic_2dids.height)
	if n_elements(arr) eq 1 then begin
		res = dialog_message('At least two elements must be picked for polygon region defined !!',/Error)
		return
	endif else begin
		res = dialog_message('Do you want to over-write the old polygon ROI ?',/question)
		if res eq 'No' then return
	end
	
	statistic_2dids.roi_nelem = n_elements(arr)
	statistic_2dids.roi_elist = 0
	statistic_2dids.roi_elist = arr

;	statistic_2dWritePolyROI,statistic_2dids,xverts,yverts,r_index
	statistic_2dWritePolyROI,statistic_2dids,xv,yv,arr
END

PRO statistic_2dPlot, statistic_2dids

	wset,statistic_2dids.wid
	erase

	im = *statistic_2dids.im0

;  if lower level threshhold value is given

	if statistic_2dids.back eq 1 then begin
	;	im = im > statistic_2dids.backave
		tv,bytscl(congrid(im,300,300),min=statistic_2dids.backave, $
			max=statistic_2dids.backave2), $
		statistic_2dids.margin_l,statistic_2dids.margin_b, $
		xsize=statistic_2dids.xsize,ysize=statistic_2dids.ysize
	endif else $

; whole image region

	tvscl,congrid(im,300,300), $
		statistic_2dids.margin_l,statistic_2dids.margin_b, $
		xsize=statistic_2dids.xsize,ysize=statistic_2dids.ysize

; if const image is found
	if statistic_2dids.max eq statistic_2dids.min then begin
		tv,congrid(im*(!d.table_size-1),300,300), $
		statistic_2dids.margin_l,statistic_2dids.margin_b, $
		xsize=statistic_2dids.xsize,ysize=statistic_2dids.ysize
		end

	p1 = [float(statistic_2dids.margin_l)/ !d.x_size, $
	  float(statistic_2dids.margin_b)/ !d.y_size, $
	  float(!d.x_size - statistic_2dids.margin_r)/ !d.x_size, $
	  float(!d.y_size - statistic_2dids.margin_t)/ !d.y_size $
	  ]

	width = n_elements(statistic_2dids.x)
	height = n_elements(statistic_2dids.y)
	
	xrange = [0,width]
	yrange = [0,height]
	if statistic_2dids.versus eq 1 then begin
	xrange = [statistic_2dids.x(0),statistic_2dids.x(width-1)]
	yrange = [statistic_2dids.y(0),statistic_2dids.y(height-1)]
	end

	plot,/noerase,/nodata,pos=p1, [-1.,-1.], $
		title=' ',$
		xrange=xrange,yrange=yrange, xstyle=1,ystyle=1
	xyouts,!d.x_ch_size,!d.y_size -!d.y_ch_size,statistic_2dids.header(1),/device

	if statistic_2dids.back eq 0 then scan2d_ROI_LMB ,statistic_2dids
	if statistic_2dids.back eq 1 then $
	statistic_2dRange,statistic_2dids,im,statistic_2dids.backave,statistic_2dids.backave2
	if statistic_2dids.back eq 2 then $
	statistic_2dPickedPolygon,statistic_2dids,im

	statistic_2dids.cross = 0
END

PRO statistic_2dReadPolyROI,statistic_2dids,xverts,yverts,xv,yv,arr,r_index

	xdr_open,unit,statistic_2dids.picked
	xdr_read,unit,xv
	xdr_read,unit,yv
	xdr_close,unit

	statistic_2dids.roi_elist = 0

	xverts = fix(xv / statistic_2dids.factor(0)+.5)
	yverts = fix(yv / statistic_2dids.factor(1)+.5)
	arr = polyfillv(xverts,yverts,statistic_2dids.width,statistic_2dids.height)
	statistic_2dids.roi_nelem = n_elements(arr)
	statistic_2dids.roi_elist = arr

END

PRO statistic_2dWritePolyROI,statistic_2dids,xverts,yverts,r_index

	xdr_open,unit,statistic_2dids.picked,/write
	xdr_write,unit,xverts
	xdr_write,unit,yverts
	xdr_close,unit
END

PRO statistic_2dPickedPolygon,statistic_2dids,im
;
	found = findfile(statistic_2dids.picked)
	if found(0) eq '' then return
	
;  xverts,yverts in data index
;  xv,yv in  pixels
	statistic_2dReadPolyROI,statistic_2dids,xverts,yverts,xv,yv,arr,r_index

device,get_graphics_function=oldGraphFunc
device,set_graphics_function=6
	polyfill,xv+statistic_2dids.margin_l, $
		yv+statistic_2dids.margin_b, $
		/dev,color = !d.table_size-1,/noclip
device,set_graphics_function=oldGraphFunc

	if n_elements(arr) eq 1 then begin
		res = dialog_message('The polygon ROI not suitable for this image',/Info)
	return
	end
	temp_ind = arr
	
	sz = size(im)
	width = sz(1)
	height = sz(2)
	nelem = n_elements(temp_ind)
	case sz(3) of
	1: temp = make_array(nelem,/byte)
	2: temp = make_array(nelem,/int)
	3: temp = make_array(nelem,/long)
	4: temp = make_array(nelem,/float)
	5: temp = make_array(nelem,/double)
	else:
	endcase

	statistic_2dids.roi_nelem = nelem
	for ij=0L,nelem-1 do begin
		j = temp_ind(ij) / width 
		i = temp_ind(ij) MOD width
		temp(ij) = im(i,j)
	end 

	result = moment(temp,mdev=mdev,sdev=sdev)

	temp_min = min(temp,jmin)
	temp_max = max(temp,jmax)
	jmin = temp_ind(jmin)
	jmax = temp_ind(jmax)
	min_i = jmin MOD width
	min_j = jmin / width
	max_i = jmax MOD width
	max_j = jmax / width
	

	scan2d_ROI_field,statistic_2dids,result(0),sdev,temp_min,temp_max,min_i,min_j,max_i,max_j

	statistic_2dids.roi_total = total(temp)
	statistic_2dids.roi_nelem = n_elements(temp)
	statistic_2dids.roi_max = temp_max
	statistic_2dids.roi_min = temp_min
	statistic_2dids.roi_ave = result(0)
	statistic_2dids.roi_dev = sdev

	; update  ROI label 
	; region defined by defroi routine

  	st='ROI: defined by PolyROI '+ $
			',  Nelem='+strtrim(statistic_2dids.roi_nelem,2)+ $
			',  Total='+strtrim(statistic_2dids.roi_total,2)
	WIDGET_CONTROL,statistic_2dids.roiid,SET_VALUE=st


END

PRO statistic_2dWriteFilter,statistic_2dids,lower_b,upper_b

	im = *statistic_2dids.im0
	bine = (im ge lower_b) and (im le upper_b)

        len = strpos(statistic_2dids.file,"_roi")
        if len ge 0 then class = strmid(statistic_2dids.file,0,len)
	xdr_open,unit,class+'_roi.pickfltr',/write
	xdr_write,unit,bine
	xdr_write,unit,lower_b
	xdr_write,unit,upper_b
	xdr_close,unit
END

PRO statistic_2dReadFilter,statistic_2dids,lower_b,upper_b
        len = strpos(statistic_2dids.file,"_roi")
        if len ge 0 then class = strmid(statistic_2dids.file,0,len)
	xdr_open,unit,class+'_roi.pickfltr'
	xdr_read,unit,bine
	xdr_read,unit,lower_b
	xdr_read,unit,upper_b
	xdr_close,unit

;print,lower_b,upper_b
;print,bine
END

PRO statistic_2dRange,statistic_2dids,im,lower_b,upper_b
; 
; extract the elements fall in between the selected range
;    statistic_2dids.backave <    < statistic_2dids.backave2
;

	sz = size(im)
	width = sz(1)
	height = sz(2)

	xrange = [0,width-1]
	yrange = [0,height-1]

	bine = (im ge lower_b) and (im le upper_b)

	nelem = long(total(bine))
	if nelem gt 1 then begin
		temp = make_array(nelem)
		temp_ind = make_array(nelem,/long)
	ij = 0L
	for j=0,height-1 do begin
	for i=0,width-1 do begin
	if im(i,j) ge lower_b and im(i,j) le upper_b then begin
		temp(ij) = im(i,j)
		temp_ind(ij) = i+ 1L*j*width
		ij=ij+1
		end
	end
	end
	statistic_2dids.roi_nelem = nelem
	statistic_2dids.roi_elist = 0
	statistic_2dids.roi_elist = temp_ind
	endif else begin
		res = dialog_message('ROI too small, at least 2 elements has to be selected',/Info)
		temp=im * 0 
		return
	end
	if statistic_2dids.back eq 0 then  temp = im

	result = moment(temp,mdev=mdev,sdev=sdev)

	temp_min = min(temp,jmin)
	temp_max = max(temp,jmax)
	if statistic_2dids.back eq 1 then begin
		jmin = temp_ind(jmin)
		jmax = temp_ind(jmax)
	end
	min_i = jmin MOD width
	min_j = jmin /width
	max_i = jmax MOD width
	max_j = jmax / width
	

	scan2d_ROI_field,statistic_2dids,result(0),sdev,temp_min,temp_max,min_i,min_j,max_i,max_j

	statistic_2dids.roi_total = total(temp)
	statistic_2dids.roi_nelem = n_elements(temp)
	statistic_2dids.roi_max = temp_max
	statistic_2dids.roi_min = temp_min
	statistic_2dids.roi_ave = result(0)
	statistic_2dids.roi_dev = sdev

	; update  ROI label 

	; if backgrund threshold value is given
  	st='ROI: IM['+ strtrim(xrange(0),2) +':'+ strtrim(xrange(1),2)+', '+ $
			strtrim(yrange(0),2)+':'+ strtrim(yrange(1),2)+']'+ $
			',  Nelem='+strtrim(statistic_2dids.roi_nelem,2)+ $
			',  Total='+strtrim(statistic_2dids.roi_total,2)
	WIDGET_CONTROL,statistic_2dids.roiid,SET_VALUE=st

END

PRO boxregion,statistic_2dids,Event

;	device,get_graphics_function=gmode
;	if gmode eq 6 then begin
;	xbox = [statistic_2dids.x1,statistic_2dids.x2,statistic_2dids.x2]
;	ybox = [statistic_2dids.y1,statistic_2dids.y1,statistic_2dids.y2]
;	xbox = xbox + statistic_2dids.margin_l
;	ybox = ybox + statistic_2dids.margin_b
;	plots,xbox,ybox,/device,thick=2
;	xbox = [statistic_2dids.x1,statistic_2dids.x1,statistic_2dids.x2]
;	ybox = [statistic_2dids.y1,statistic_2dids.y2,statistic_2dids.y2]
;	xbox = xbox + statistic_2dids.margin_l
;	ybox = ybox + statistic_2dids.margin_b
;	plots,xbox,ybox,/device,thick=2
;	endif else device,set_graphics_function=6
st=['You are in the define rectangular ROI mode', $
	'Press and move LMB to select the ROI', $
	'Release and move LMB to show the ROI box']
WIDGET_CONTROL,statistic_2dids.message,SET_VALUE=st
	wset,statistic_2dids.wid
	tv,statistic_2dids.pixmap,statistic_2dids.margin_l,statistic_2dids.margin_b

device,set_graphics_function=6
	wset,statistic_2dids.wid
	cursor,x1,y1,/device,/change ; /nowait

while(!err eq 1) do begin

	cursor,x2,y2,/device,/change,/nowait   ; draw continuously 

	plots,[x1,x2,x2],[y1,y1,y2],/device,thick=2
	plots,[x1,x1,x2],[y1,y2,y2],/device,thick=2

	plots,[x1,x2,x2],[y1,y1,y2],/device,thick=2
	plots,[x1,x1,x2],[y1,y2,y2],/device,thick=2
endwhile
if  n_elements(x2) eq 0 then begin
	device,set_graphics_function=3
	return
end
statistic_2dids.x1 = x1 - statistic_2dids.margin_l
statistic_2dids.x2 = x2 - statistic_2dids.margin_l
if x2 lt x1 then begin 
	statistic_2dids.x1 = x2 - statistic_2dids.margin_l
	statistic_2dids.x2 = x1 - statistic_2dids.margin_l
end
statistic_2dids.y1 = y1 - statistic_2dids.margin_b
statistic_2dids.y2 = y2 - statistic_2dids.margin_b
if y2 lt y1 then begin 
	statistic_2dids.y1 = y2 - statistic_2dids.margin_b
	statistic_2dids.y2 = y1 - statistic_2dids.margin_b
end
	plots,[x1,x2,x2],[y1,y1,y2],/device,thick=2
	plots,[x1,x1,x2],[y1,y2,y2],/device,thick=2

device,set_graphics_function=3

WIDGET_CONTROL,statistic_2dids.message,SET_VALUE="**Press MMB to Query Pixel Values**"
END


PRO scan2d_ROI_MMB,statistic_2dids

	wset,statistic_2dids.wid
	cursor,px2,py2,0,/device

	device,set_graphics_function=6
	if statistic_2dids.cross then begin
	plots,[statistic_2dids.cursor_x,statistic_2dids.cursor_x], $
	  [statistic_2dids.margin_b,!d.y_size-statistic_2dids.margin_t],/device
	plots,[statistic_2dids.margin_l,!d.x_size-statistic_2dids.margin_r], $
	  [statistic_2dids.cursor_y,statistic_2dids.cursor_y],/device
	end
	statistic_2dids.cross = 1
	statistic_2dids.cursor_x=px2
	statistic_2dids.cursor_y=py2
	plots,[px2,px2],[statistic_2dids.margin_b,!d.y_size-statistic_2dids.margin_t],/device
	plots,[statistic_2dids.margin_l,!d.x_size-statistic_2dids.margin_r],[py2,py2],/device
	device,set_graphics_function=3

	px2 = px2-statistic_2dids.margin_l
	py2 = py2-statistic_2dids.margin_b
	x2 = fix(px2/statistic_2dids.factor[0])
	y2 = fix(py2/statistic_2dids.factor[1]) ;+.5)
	if x2 ge statistic_2dids.width or $
		y2 ge statistic_2dids.height then begin
		res = dialog_message('Out of image area!',/error)
		return
		end
	if statistic_2dids.versus eq 1 then begin
	st = 'CURSOR: X='+strtrim(statistic_2dids.x(x2),2)+', Y=' $
		+strtrim(statistic_2dids.y(y2),2)+',' 
	endif else begin
	st = 'CURSOR: Ix='+strtrim(x2,2)+', Jy='+strtrim((y2),2)+','
	end
	im0 = *statistic_2dids.im0
	stv= '     Pixel Value ='+strtrim(im0(x2,y2),2)
	WIDGET_CONTROL,statistic_2dids.cursor,SET_VALUE=st
	WIDGET_CONTROL,statistic_2dids.cursorv,SET_VALUE=stv
	if statistic_2dids.debug then print,'cursor:i,j=',px2,py2,x2,y2
	statistic_2dids.cursor_i = x2
	statistic_2dids.cursor_j = y2

END

PRO scan2d_ROI_LMB,statistic_2dids

	xbox = [statistic_2dids.x1,statistic_2dids.x2,statistic_2dids.x2]
	ybox = [statistic_2dids.y1,statistic_2dids.y1,statistic_2dids.y2]
	xbox = xbox + statistic_2dids.margin_l
	ybox = ybox + statistic_2dids.margin_b
	plots,xbox,ybox,/device,thick=2
	xbox = [statistic_2dids.x1,statistic_2dids.x1,statistic_2dids.x2]
	ybox = [statistic_2dids.y1,statistic_2dids.y2,statistic_2dids.y2]
	xbox = xbox + statistic_2dids.margin_l
	ybox = ybox + statistic_2dids.margin_b
	plots,xbox,ybox,/device,thick=2

statistic_2dids.xrange[0] = fix(statistic_2dids.x1/statistic_2dids.factor[0])
statistic_2dids.yrange[0] = fix(statistic_2dids.y1/statistic_2dids.factor[1])
statistic_2dids.xrange[1] = fix(statistic_2dids.x2/statistic_2dids.factor[0])
statistic_2dids.yrange[1] = fix(statistic_2dids.y2/statistic_2dids.factor[1]) ;+.5)

	if statistic_2dids.xrange[1] ge n_elements(statistic_2dids.x) then statistic_2dids.xrange[1] = n_elements(statistic_2dids.x) - 1
	if statistic_2dids.yrange[1] ge n_elements(statistic_2dids.y) then statistic_2dids.yrange[1] = n_elements(statistic_2dids.y) - 1

	if statistic_2dids.debug then begin
	print,'x1,y1',statistic_2dids.x1,statistic_2dids.y1
	print,'x2,y2',statistic_2dids.x2,statistic_2dids.y2
	print,'factor',statistic_2dids.factor
	print,'xrange',statistic_2dids.xrange
	print,'yrange',statistic_2dids.yrange
	end

	xrange = statistic_2dids.xrange
	if xrange(1) lt xrange(0) then begin
		statistic_2dids.xrange(0) = xrange(1)
		statistic_2dids.xrange(1) = xrange(0)
		end
	yrange = statistic_2dids.yrange
	if yrange(1) lt yrange(0) then begin
		statistic_2dids.yrange(0) = yrange(1)
		statistic_2dids.yrange(1) = yrange(0)
		end
	im0 = *statistic_2dids.im0
	sz = size(im0)
	if sz(0) eq 2 then begin
	  if statistic_2dids.xrange(1) ge sz(1) then statistic_2dids.xrange(1) = sz(1)-1
	  if statistic_2dids.yrange(1) ge sz(2) then statistic_2dids.yrange(1) = sz(2)-1
	end
	temp2 = im0(statistic_2dids.xrange(0):statistic_2dids.xrange(1),$
		statistic_2dids.yrange(0):statistic_2dids.yrange(1))
	if n_elements(temp2) lt 2 then begin
	res = dialog_message('ROI is too small! Try again.',/Error)
	return
	end

	dj = statistic_2dids.yrange(1) - statistic_2dids.yrange(0) + 1
	di = statistic_2dids.xrange(1) - statistic_2dids.xrange(0) + 1

	for j=0L,dj-1 do begin
		ij = (j+statistic_2dids.yrange(0)) * statistic_2dids.width $
			+ statistic_2dids.xrange(0)
		ll = indgen(di) + ij
		if n_elements(elist) eq 0 then elist = ll else $
			elist = [elist,ll]
	i = di+statistic_2dids.xrange(0)
	end
	statistic_2dids.roi_nelem = n_elements(elist)
	statistic_2dids.roi_elist = 0
	statistic_2dids.roi_elist = elist

	result = moment(temp2,mdev=mdev,sdev=sdev)
	temp_max = max(temp2,jmax)
	max_i = statistic_2dids.xrange[0] + $
	  jmax MOD (statistic_2dids.xrange[1]-statistic_2dids.xrange[0]+1) 
	max_j = statistic_2dids.yrange[0] + $
	  jmax / (statistic_2dids.xrange[1]-statistic_2dids.xrange[0]+1)
	temp_min = min(temp2,jmin)
	min_i = statistic_2dids.xrange[0] + $
	  jmin MOD (statistic_2dids.xrange[1]-statistic_2dids.xrange[0]+1)
	min_j = statistic_2dids.yrange[0] + $
	  jmin / (statistic_2dids.xrange[1]-statistic_2dids.xrange[0]+1)

	statistic_2dids.min_i = min_i
	statistic_2dids.min_j = min_j
	statistic_2dids.max_i = max_i
	statistic_2dids.max_j = max_j
	statistic_2dids.roi_total = total(temp2)
	statistic_2dids.roi_nelem = n_elements(temp2)
	statistic_2dids.roi_max = temp_max
	statistic_2dids.roi_min = temp_min
	statistic_2dids.roi_ave = result(0)
	statistic_2dids.roi_dev = sdev

	scan2d_ROI_field,statistic_2dids,result(0),sdev,temp_min,temp_max,min_i,min_j,max_i,max_j

	; ROI
  	st='ROI: IM['+strtrim(statistic_2dids.xrange(0),2)+':'+ $
			strtrim(statistic_2dids.xrange(1),2)+', '+ $
			strtrim(statistic_2dids.yrange(0),2)+':'+ $
			strtrim(statistic_2dids.yrange(1),2)+']'+ $
			',  Nelem='+strtrim(statistic_2dids.roi_nelem,2)+ $
			',  Total='+strtrim(statistic_2dids.roi_total,2)
	WIDGET_CONTROL,statistic_2dids.roiid,SET_VALUE=st
END

PRO scan2d_ROI_field,statistic_2dids,ave,dev,tmin,tmax,min_i,min_j,max_i,max_j

	WIDGET_CONTROL,statistic_2dids.aveid,SET_VALUE=ave
	WIDGET_CONTROL,statistic_2dids.devid,SET_VALUE=dev
	WIDGET_CONTROL,statistic_2dids.minid,SET_VALUE=tmin
	WIDGET_CONTROL,statistic_2dids.maxid,SET_VALUE=tmax

	if statistic_2dids.versus eq 0 then begin
	WIDGET_CONTROL,statistic_2dids.minxid,SET_VALUE=min_i
	WIDGET_CONTROL,statistic_2dids.minyid,SET_VALUE=min_j
	WIDGET_CONTROL,statistic_2dids.maxxid,SET_VALUE=max_i
	WIDGET_CONTROL,statistic_2dids.maxyid,SET_VALUE=max_j
	endif else begin
	WIDGET_CONTROL,statistic_2dids.minxid,SET_VALUE=statistic_2dids.x(min_i)
	WIDGET_CONTROL,statistic_2dids.minyid,SET_VALUE=statistic_2dids.y(min_j)
	WIDGET_CONTROL,statistic_2dids.maxxid,SET_VALUE=statistic_2dids.x(max_i)
	WIDGET_CONTROL,statistic_2dids.maxyid,SET_VALUE=statistic_2dids.y(max_j)
	end

END

PRO PDMENU13_Event,statistic_2dids, Event

  CASE Event.Value OF

  'ROIFile.New...': BEGIN
	filter='*roi.xdr*'
	if statistic_2dids.back eq 2 then filter='*roi*poly*'
	f = dialog_pickfile(path=statistic_2dids.roipath,filter=filter, $
		title='New ROI File',get_path=p,/WRITE)
	found = findfile(f)
	if found(0) ne '' then begin
		res=dialog_message('File already exists!!',/Error)
		return
	end
	if strtrim(f,2) ne '' then begin
	WIDGET_CONTROL,statistic_2dids.fileid,SET_VALUE=f
	res = strpos(f,'.poly')
	if res gt 0 then f= strmid(f,0,res)
	statistic_2dids.file = f
	statistic_2dids.roipath = p
	statistic_2dids.picked = f+'.poly'
	end
    END
  'ROIFile.Open...': BEGIN
	filter='*roi.xdr*'
	if statistic_2dids.back eq 2 then filter='*roi*poly*'
	f = dialog_pickfile( path=statistic_2dids.roipath,filter=filter, $
		title='Pick ROI File',/MUST_EXIST,get_path=p,/WRITE)
	if strtrim(f,2) ne '' then begin
	WIDGET_CONTROL,statistic_2dids.fileid,SET_VALUE=f
	res = strpos(f,'.poly')
	if res gt 0 then f= strmid(f,0,res)
	statistic_2dids.file = f
	statistic_2dids.roipath = p
	statistic_2dids.picked = f+'.poly'
	scan2d_ROI_readroi,statistic_2dids
	end
    END
  ENDCASE
END


PRO ROI2D_PDMENU3_Event,statistic_2dids, Event

  CASE Event.Value OF

  'RptFile.New...': BEGIN
    PRINT, 'Event for File.New...'
	f = dialog_pickfile( path=statistic_2dids.rptpath,filter='*roi.rpt*', $
		get_path=p,title='ROI Report.New...',/WRITE)
	found = findfile(f)
	if found(0) ne '' then begin
		res=dialog_message('File already exists!!',/Error)
		return
	end
	if strtrim(f,2) ne '' then begin
	WIDGET_CONTROL,statistic_2dids.rptid,SET_VALUE=f
	statistic_2dids.rpt = f
	statistic_2dids.rptpath = p
	end
    END
  'RptFile.Open...': BEGIN
    PRINT, 'Event for File.Open...'
	f = dialog_pickfile(path=statistic_2dids.rptpath,filter='*roi.rpt*', $
		get_path=p,title='ROI Report.Open...',/MUST_EXIST,/WRITE)
	if strtrim(f,2) ne '' then begin
	WIDGET_CONTROL,statistic_2dids.rptid,SET_VALUE=f
	statistic_2dids.rpt = f
	statistic_2dids.rptpath = p
	end
    END
  ENDCASE
END

PRO scan2d_ROI_default, statistic_2dids

	statistic_2dids.x1=0
	statistic_2dids.y1=0
	statistic_2dids.x2=300
	statistic_2dids.y2=300
	statistic_2dPlot,statistic_2dids

	statistic_2dids.refresh = 1
END

PRO scan2d_ROI_Event, Event

  WIDGET_CONTROL,Event.Top,GET_UVALUE=statistic_2dids
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'STATISTIC_2DDRAW': BEGIN
	if (Event.PRESS eq 2) then begin
	scan2d_ROI_MMB,statistic_2dids
	goto,set_return
	end

	if (Event.press eq 4) then begin
	scan2d_ROI_MMB,statistic_2dids
	statistic_2dPlot,statistic_2dids
	end

      if (Event.PRESS eq 1) then begin
	boxregion,statistic_2dids,Event
	scan2d_ROI_LMB,statistic_2dids
	statistic_2dids.refresh = 1
	goto,set_return
	end

      END
  'STATISTIC_2DCLEAN': BEGIN
	scan2d_ROI_default, statistic_2dids
      END
  'STATISTIC_2DFILE2': BEGIN
	WIDGET_CONTROL,statistic_2dids.rptid,GET_VALUE=f
	statistic_2dids.rpt = f
	END
  'STATISTIC_2DFILE': BEGIN
	WIDGET_CONTROL,statistic_2dids.fileid,GET_VALUE=f
	res = strpos(f(0),'.poly')
	if res gt 0 then f = strmid(f(0),0,res)
	statistic_2dids.file = f
	statistic_2dids.picked = f+'.poly'
	scan2d_ROI_readroi,statistic_2dids
	END
  'STATISTIC_2DBACKFLD': BEGIN
	WIDGET_CONTROL,statistic_2dids.backid,GET_VALUE=ave
	statistic_2dids.backave = ave
	statistic_2dPlot,statistic_2dids
	END
  'STATISTIC_2DHIGHFLD': BEGIN
	WIDGET_CONTROL,statistic_2dids.highid,GET_VALUE=ave
	if ave lt statistic_2dids.backave then begin
		res = dialog_message('High value must be greater than Low value.',/error)
		goto,set_return
	end
	statistic_2dids.backave2 = ave
	statistic_2dPlot,statistic_2dids
	END
  'STATISTIC_2DCOMMENT': BEGIN
	WIDGET_CONTROL,statistic_2dids.commentid,GET_VALUE=f
	statistic_2dids.comment = f
	END
  'STATISTIC_2DMULTIROI': BEGIN
	len = strpos(statistic_2dids.picked,"_roi",/reverse_search)
	if len ge 0 then class = strmid(statistic_2dids.file,0,len+1)
	im0 = *statistic_2dids.im0
	WIDGET_CONTROL,Event.top,/DESTROY
	multiroi_pick,im0,class=class,comment=statistic_2dids.header(1), $
		header=statistic_2dids.header(0)  ;,Group=Event.top
	return
	END
  'STATISTIC_2DBACKMODE': BEGIN
	statistic_2dids.back = Event.Index

	if Event.Index eq 2 then $
	WIDGET_CONTROL,statistic_2dids.fileid,SET_VALUE=statistic_2dids.picked else $
	WIDGET_CONTROL,statistic_2dids.fileid,SET_VALUE=statistic_2dids.file

	if statistic_2dids.back eq 0 then $
		WIDGET_CONTROL,statistic_2dids.rectbase,SENSITIVE=1 else $
  		WIDGET_CONTROL,statistic_2dids.rectbase,SENSITIVE=0
	if statistic_2dids.back eq 2 then $
		WIDGET_CONTROL,statistic_2dids.polybase,SENSITIVE=1 else $
		WIDGET_CONTROL,statistic_2dids.polybase,SENSITIVE=0
	if statistic_2dids.back eq 1 then $ 
		WIDGET_CONTROL,statistic_2dids.backbase,SENSITIVE=1 else $
		WIDGET_CONTROL,statistic_2dids.backbase,SENSITIVE=0

		statistic_2dPlot,statistic_2dids
		st0='**Press MMB to Query Pixel Values**'
		if statistic_2dids.back eq 0 then $ 
		st =[st0,  $
			'Press & move LMB in image area to define new ROI', $
			'Use SaveRecROI button to save new RectROI ', $
			'Use Refresh button to use whole image ROI', $
			'Use ReadRectROI button to load old RectROI']
		if statistic_2dids.back eq 1 then $ 
		st =[st0, 'You are in the FilterROI mode', $
			'Use Filter Low/High/SLIDERS to define ROI']
		if statistic_2dids.back eq 2 then $ 
		st =[st0, 'You are in the PolyROI mode', $
			'Use DefPolyROI&Save to start new defROI funtion', $
			'Press LMB in image area to define vertices',$
			'Use ReadPolyROI load old polygon ROI']
		WIDGET_CONTROL,statistic_2dids.message,SET_VALUE=st
	END
  'STATISTIC_2DBACKSLDR': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=v
	WIDGET_CONTROL,statistic_2dids.backid,SET_VALUE=v
	if v ge statistic_2dids.backave2 then begin
		res = dialog_message('Low value must be less than High value.',/error)
		WIDGET_CONTROL,statistic_2dids.backid,SET_VALUE=statistic_2dids.backave
		WIDGET_CONTROL,Event.id,SET_VALUE=statistic_2dids.backave
		goto,set_return
	end
        statistic_2dids.backave=v
	statistic_2dPlot,statistic_2dids
      END
  'STATISTIC_2DHIGHSLDR': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=v
	WIDGET_CONTROL,statistic_2dids.highid,SET_VALUE=v
	if v lt statistic_2dids.backave then begin
		res = dialog_message('High value must be greater than Low value.',/error)
		WIDGET_CONTROL,statistic_2dids.highid,SET_VALUE=statistic_2dids.backave2
		WIDGET_CONTROL,Event.ID,SET_VALUE=statistic_2dids.backave2
		return
	end
        statistic_2dids.backave2=v
	statistic_2dPlot,statistic_2dids
      END
  'STATISTIC_2DVERSUS': BEGIN
	statistic_2dids.versus = Event.Index
if statistic_2dids.versus eq 0 then begin
WIDGET_CONTROL,statistic_2dids.minxid,SET_VALUE=statistic_2dids.min_i
WIDGET_CONTROL,statistic_2dids.minyid,SET_VALUE=statistic_2dids.min_j
WIDGET_CONTROL,statistic_2dids.maxxid,SET_VALUE=statistic_2dids.max_i
WIDGET_CONTROL,statistic_2dids.maxyid,SET_VALUE=statistic_2dids.max_j
	x2 = statistic_2dids.cursor_i
	y2 = statistic_2dids.cursor_j
	st = 'CURSOR: Ix='+strtrim(x2,2)+', Jy='+ strtrim(y2,2)+','
	im0 = *statistic_2dids.im0
	stv= '     Pixel Value ='+strtrim(im0(x2,y2),2)
	WIDGET_CONTROL,statistic_2dids.cursor,SET_VALUE=st
	WIDGET_CONTROL,statistic_2dids.cursorv,SET_VALUE=stv
endif else begin
WIDGET_CONTROL,statistic_2dids.minxid,SET_VALUE=statistic_2dids.x(statistic_2dids.min_i)
WIDGET_CONTROL,statistic_2dids.minyid,SET_VALUE=statistic_2dids.y(statistic_2dids.min_j)
WIDGET_CONTROL,statistic_2dids.maxxid,SET_VALUE=statistic_2dids.x(statistic_2dids.max_i)
WIDGET_CONTROL,statistic_2dids.maxyid,SET_VALUE=statistic_2dids.y(statistic_2dids.max_j)
	x2 = statistic_2dids.cursor_i
	y2 = statistic_2dids.cursor_j
	st = 'CURSOR: X='+strtrim(statistic_2dids.x(x2),2)+', Y='+ $
	   	strtrim(statistic_2dids.y(y2),2)+','
	stv= '     Pixel Value ='+strtrim(im0(x2,y2),2)
	WIDGET_CONTROL,statistic_2dids.cursor,SET_VALUE=st
	WIDGET_CONTROL,statistic_2dids.cursorv,SET_VALUE=stv
end
	statistic_2dPlot,statistic_2dids

      END
  'STATISTIC_2DDONE': BEGIN
;	  device,set_graphics_function=3
	  WIDGET_CONTROL,Event.Top,/DESTROY
	  return
      END
  'STATISTIC_2DCOLOR': BEGIN
	XLOADCT,GROUP=Event.Top
      END
  'STATISTIC_2DREADROI': BEGIN
	if statistic_2dids.back eq 0 then scan2d_ROI_readroi ,statistic_2dids
      END
  'STATISTIC_2DREADFLTR': BEGIN
	statistic_2dReadFilter,statistic_2dids,lower_b,upper_b
	WIDGET_CONTROL,statistic_2dids.backid,SET_VALUE=lower_b
	WIDGET_CONTROL,statistic_2dids.highid,SET_VALUE=upper_b
	statistic_2dids.backave = lower_b
	statistic_2dids.backave2 = upper_b
	statistic_2dPlot,statistic_2dids
      END
  'STATISTIC_2DWRITEFLTR': BEGIN
	WIDGET_CONTROL,statistic_2dids.backid,GET_VALUE=lower_b
	WIDGET_CONTROL,statistic_2dids.highid,GET_VALUE=upper_b
	statistic_2dWriteFilter,statistic_2dids,lower_b,upper_b
      END
  'STATISTIC_2DREADPOLYROI': BEGIN
	statistic_2dPlot,statistic_2dids
      END
  'STATISTIC_2DDEFROI': BEGIN
	text=['You are in the defining PolyROI mode:', $
		'Left button to mark point', $
		'Middle button to erase previous point', $
		'Right button to close polygon region']
	WIDGET_CONTROL,statistic_2dids.message,SET_VALUE=text
		defroi_congrid,statistic_2dids,arr,xverts,yverts
scan2d_ROI_default, statistic_2dids
	statistic_2dPlot,statistic_2dids
	WIDGET_CONTROL,statistic_2dids.message,SET_VALUE='**Press MMB to Query Pixel Values**'
	END
  'STATISTIC_2DSAVEROI': BEGIN
	found = findfile(statistic_2dids.file)
	if found(0) ne '' then begin
		st = ['File: '+statistic_2dids.file , 'already exists !!', $
			'','Do you want to override the ROI file?']
		res = dialog_message( st,/question)
		if res eq 'No' then return
	end
	x = [statistic_2dids.x1,statistic_2dids.x2,statistic_2dids.y1, $
		statistic_2dids.y2,statistic_2dids.factor]
	xdr_open,unit,statistic_2dids.file,/write
	xdr_write,unit,x
	xdr_close,unit
      END
  'STATISTIC_2DREPORTVIEW': BEGIN
	f = dialog_pickfile( path=statistic_2dids.rptpath,filter='*rpt*', $
		title='View Rpt File',/READ)
	if f eq '' then return
	found = findfile(f) ; statistic_2dids.rpt)
	if found(0) eq '' then begin
		res = dialog_message(['ROI file : ','',f, $
			'',' not found!'],/error)
		return
	end
	xdisplayfile,f,GROUP=Event.Top
;	res = cw_term(statistic_2dids.base,filename=f,/scroll)
      END
  'STATISTIC_2DRENAME': BEGIN
        fn = statistic_2dids.rpt 
        res = rstrpos(statistic_2dids.rpt,!os.file_sep)
        if res gt -1 then rpath = strmid(statistic_2dids.rpt,0,res)
        rename_dialog,rpath,fn,'',GROUP=Event.top
	END
  'STATISTIC_2DREPLACE': BEGIN
	scan2d_ROI_writeReport,statistic_2dids,/new
	xdisplayfile,statistic_2dids.rpt,GROUP=Event.Top
	END
  'STATISTIC_2DREPORT': BEGIN
	scan2d_ROI_writeReport,statistic_2dids
	xdisplayfile,statistic_2dids.rpt,GROUP=Event.Top
;	res = cw_term(statistic_2dids.base,filename=statistic_2dids.rpt,/scroll)
	END
  'STATISTIC_2DHELP': BEGIN
     st = [$
	'This program supports 4 types of ROI: rectangular region, low and high', $
	'value filter, polygonal region, and multiple-ROI selections. The initial', $
	'default mode is the rectangle ROI','',$
	'It is assumed that the ROI file named ended with file type "roi.xdr"',$
	'and the ROI report file ended with file type "roi.rpt". For accessing',$
	'files not follow this name rule a user can modify the Filter field', $
	'in the File Selection Dialog.','', $
	'For easy of file management, it is recommanded that all the files ', $
	'related to 2D-ROI are stored or grouped in a subdirectory "ROI"', $
	'below the scan data directory.','', $
	'For multiROIs picking mode, the ROI is re-defined every time the new image ',$
	'is loading in. The default suffix file name roi.pick and rois.rpt are', $
	'used for multiROIs.', $
	'        *** Brief User Interface Functions Given Below ***','', $
	'Drawing Area Mouse Button Functions:',$
	'    RectROI mode',$
	'         Left Button  - Press and move LMB to select the ROI, and ', $
	'                        release and move LMB to show the ROI box',  $
	'                        At least two elements must be selected by ROI.', $
	'       Middle Button  - Get CURSOR values in index#/values', $
	'        Right Button  - Refresh the drawing area and ROI', $
	'    PolyROI mode',$
	'         Left button to mark point',$
	'         Middle button to erase previous point',$
	'         Right button to close region',$
	'ROI Mode:    ',$
	'           RectROI    - Rectangle ROI mode', $
	'           FilterROI  - Filter ROI based on Low and High values mode', $
	'           PolyROI    - Polygon ROI mode', $
	'MultiROIs... Button   - Multiple ROIs selection program', $
	'Help... Button        - Display this help window',$
	'Color... Button       - Run XLOADCT program to select different color tables',$
	'Done    Button        - Close this program', $
	'','Rectangle  ROI:     ', $
	'   SaveRectROI Button - Save new rectangle ROI in ROIFile',$
	'   Refresh Button     - Refresh the drawing area and recalculate the', $
	'                        ROI statistics based on the whole image area', $
	'   ReadRectROI Button - Read ROI for either rectangle mode',$
	'','Filter ROI: ', $
	'           SaveFltr   - Save region defined by the low & high filters', $
	'           ReadFltr   - Read region defined by the low & high filters', $
	'           Low:       - Field to set low value',$
	'           Slider     - Controls the low field value', $
	'           High:      - Field to set high value',$
	'           Slider     - Controls the high field value', $
	'','Polygon ROI:      ', $
	'    DefPolyROI&Save   - Define and Save new polygon ROI in tmproi.xdr.poly',$
	'                        statistics based on the read in ROI', $
	'    ReadPolyROI       - Read polygon ROI from tmproi.xdr.poly',$
	'', $
	'Data:IM[wd x ht]      - Shows the raw image dim and area total', $
	'ROI: Im[I1:I2, J1:J2] - ROI dim and area total', $ 
	'   ROI Arerage:       - Shows the mean average of ROI',$
	'   ROI Deviation:     - Shows the standard deviation of ROI', $
	'   ROI Min:           - Shows the minimum value in ROI',$
	'       @x:            - Shows X index#/value of local Min', $
	'       @y:            - Shows Y index#/value of local Min', $
	'   ROI Max:           - Shows the maximum value in ROI', $
	'       @x:            - Shows X index#/value of local Max', $
	'       @y:            - Shows Y index#/value of local Max', $
	'   vs Index#/Value    - Index#/Value option of x,y value', $
	'Comment Field         - Optional comment to ROI report', $
	'', $
	'ROIFile Menu          - Select the desired ROIFile to be used ', $
	'        Field         - Enter/show the ROIFile used ', $
	'', $
	'RptFile Menu          - Select the desired RptFile to be used ', $
	'        Field         - Enter/show the RptFile used ', $
	'   RenameRpt...       - Rename the RptFile to a new name',$
	'   AppendRpt...       - Append 2D statistic report data to RptFile',$
	'   ViewRpt...         - View the 2D statistic RptFile',$
	'', $
	'CURSOR:...            - MMB/RMB querys the X,Y,Z values at cursor',$
	'', $
	'Note:  All the editable field must be entered with <CR>' $
	]
  	xdisplayfile,'',title='Help... 2D ROI',text=st, GROUP=Event.Top
      END
  ; Event for PDMENU3
  'ROI2D_PDMENU3': ROI2D_PDMENU3_Event,statistic_2dids, Event
  'PDMENU13': PDMENU13_Event,statistic_2dids, Event
  ENDCASE

  set_return:
  WIDGET_CONTROL,Event.Top,SET_UVALUE=statistic_2dids
END

PRO scan2d_ROI_writeReport,statistic_2dids,new=new,debug=debug

	if keyword_set(new) then openw,1,statistic_2dids.rpt,ERROR=err else $
	openw,1,statistic_2dids.rpt,ERROR=err,/append
	IF (err NE 0) then begin
		PRINTF, -2, !ERR_STRING
		close,1
		return
	end

	xrange = statistic_2dids.xrange
	yrange = statistic_2dids.yrange

	if statistic_2dids.back then begin
		xrange = [0,statistic_2dids.width-1]
		yrange = [0,statistic_2dids.height-1]
	end

	printf,1,'===================================================='
	printf,1,'Generated at:  ',systime(0)
	printf,1,'Header: ',statistic_2dids.header
	printf,1,'Comment: ',statistic_2dids.comment
	if statistic_2dids.back eq 2 then begin
		printf,1,'ROI: Polygon region defined in ',statistic_2dids.picked

		statistic_2dReadPolyROI,statistic_2dids,xverts,yverts,xv,yv,arr,r_index

if keyword_set(debug) then begin
print,xverts
print,yverts
print,xv
print,yv
print,arr
end

	printf,1,'Xverts index:',xverts
	printf,1,'Yverts index:',yverts
		if n_elements(arr) eq 1 then begin
			printf,1,'The polygon ROI is not suitable for this image.'
			close,1
			return
		end
	endif else begin
	if statistic_2dids.back eq 0 then  begin 
		printf,1,'ROI: Rectangle region defined in ',statistic_2dids.file
	printf,1,'ROI in index: [',strtrim(xrange(0),2),':', $
		strtrim(xrange(1),2),', ', $
		strtrim(yrange(0),2),':', $
		strtrim(yrange(1),2),'] ' 
	printf,1,'ROI in values: [',strtrim(statistic_2dids.x(xrange(0)),2),':', $
		strtrim(statistic_2dids.x(xrange(1)),2),', ', $
		strtrim(statistic_2dids.y(yrange(0)),2),':', $
		strtrim(statistic_2dids.y(yrange(1)),2),'] ' 

		di = xrange(1)-xrange(0) + 1
		xlist = indgen(di)+xrange(0)
		for j=yrange(0),yrange(1) do begin
		ij = j*statistic_2dids.width+xlist
		if n_elements(arr) eq 0 then arr = ij else arr= [arr,ij]
		end
	end
	printf,1,''
	if statistic_2dids.back eq 1 then begin 
	im0 = *statistic_2dids.im0
	arr = where(im0 ge statistic_2dids.backave and im0 le statistic_2dids.backave2)
	printf,1,'Filter Low and High Values: ['+strtrim(statistic_2dids.backave,2)+','+ $
		strtrim(statistic_2dids.backave2,2)+']'
	end
	end
	printf,1,'ave = ',statistic_2dids.roi_ave
	printf,1,'dev = ',statistic_2dids.roi_dev
	printf,1,'min = ',statistic_2dids.roi_min
	printf,1,'max = ',statistic_2dids.roi_max
	printf,1,'total = ',statistic_2dids.roi_total
	printf,1,'nelem = ',statistic_2dids.roi_nelem
	printf,1,''
	printf,1,'            N         I            J    IM(I,J)    IM(I,J)-ave   ROI'
	printf,1,'         ------------------------------------------------------------'

	im0 = *statistic_2dids.im0
	ty = "    RECT"
	if statistic_2dids.back eq 1 then ty = "    FILTER"
	if statistic_2dids.back eq 2 then ty = "    POLY"

	str = strarr(statistic_2dids.roi_nelem)
	for ij=0L,statistic_2dids.roi_nelem-1 do begin
	   i = arr(ij) MOD statistic_2dids.width
	   j = arr(ij) / statistic_2dids.width
 	   str(ij) = string(ij,i,j,im0(i,j),im0(i,j)-statistic_2dids.roi_ave)
	end

	printf,1,str+ty
	close,1

END

PRO scan2d_ROI_readroi,statistic_2dids

	found = findfile(statistic_2dids.file)
	if found(0) ne '' then begin

	xdr_open,unit,statistic_2dids.file
	xdr_read,unit,x
	xdr_close,unit

	statistic_2dids.x1=x(0)
	statistic_2dids.x2=x(1)
	statistic_2dids.y1=x(2)
	statistic_2dids.y2=x(3)

	statistic_2dPlot,statistic_2dids
	x = [statistic_2dids.x1,statistic_2dids.x2,statistic_2dids.y1, $
		statistic_2dids.y2,statistic_2dids.factor]
	xdr_open,unit,statistic_2dids.file,/write
	xdr_write,unit,x
	xdr_close,unit
	end

	statistic_2dids.refresh = 0
END

PRO scan2d_ROI,im,x,y,debug=debug,header=header,roifile=roifile,rptfile=rptfile,mode=mode,comment=comment, GROUP=Group,roi_data=statistic_2dids
;
;+
; NAME:
;       SCAN2D_ROI
;
; PURPOSE:
;       This program let the user interactively define the ROI for a given 
;       2D image array, calculate the 2D-ROI statistics, and generate ROI
;       report.
;
;       It supports 3 types of ROI: rectangle, polygon, and pixel value
;       range filter.
;
; CATEGORY:
;       Widgets.
;
; CALLING SEQUENCE:
;
;       SCAN2D_ROI, Im, X, Y [,HEADER=header] [,ROIFILE=roifile] $
;            [,RPTFILE=rptfile] [,MODE=mode] [,COMMENT=comment] [GROUP=group]
;
; INPUTS:
;       IM:     Input 2D image array
;       X:      X data vector
;       Y:      Y data vector
;	
; KEYWORD PARAMETERS:
;       HEADER:     A string list which holds two strings.  If specified it
;                   will be plotted at the top left corner of the TV screen.
;       COMMENT:    A string used to annotate the special ROI. If specified
;                   it will be put in the statistic report. 
;       ROIFILE:    A string to specifies the default RectROI filename used.
;       RPTFILE:    A string to specifies the default ROI report filename used. 
;       MODE:       Starts the scan2d_roi with desired ROI mode, default is
;                   RectROI mode.
;
; SIDE EFFECTS:
;       A directory ROI will be created at the current directory. All the
;       ROI related files will be saved under this directory. 
;       If the keyword rptfile is defined on the command line, then the
;       location of ROI directory will be determined from the rptfile.
;
;       The default filenames used for ROI report, rectangle ROI, polygon 
;       ROI are 'tmproi.rpt', 'tmproi.xdr', 'tmproi.xdr.poly' respectively.
;
; RESTRICTIONS:
;       The ROI filename entered in the file selection dialog is used by the
;       rectangle ROI. The ROI filename is automatically suffixed by '.poly'
;       for the polygon ROI.
;
; EXAMPLE:
;       For a given 2D image array IM corresponding to  X and Y vectors,
;       to run the scan2d_roi program to get various 2D-ROI statistic 
;       report:
;
;       scan2d_roi,im,x,y
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, June 23, 1999.
;       01-24-2001  bkc R1.1
;                       Handle large image array, improve the efficiency
;       12-05-2003  bkc Fix the problem of inverse order of ROI
;-

;
; optional ROI[x1,x2,y1,y2] specify initial ROI of pixels
;
  if n_params() lt 3 then begin
	sz = size(im)
	if sz(0) ne 2 then return
	x = indgen(sz(1))
	y = indgen(sz(2))
  end

  if XRegistered('scan2d_ROI') then return 

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  result = moment(im,mdev=mdev,sdev=sdev)

  width = n_elements(x)
  height= n_elements(y)

  im = temporary( im(0:width-1,0:height-1) )

  im_max = max(im,jmax)
  im_min = min(im,jmin)

xsize = 300
ysize = 300

  cd,current=p
  p = p + !os.file_sep + 'ROI' + !os.file_sep

  if keyword_set(rptfile) then begin
	len = rstrpos(rptfile,!os.file_sep)
	p = strmid(rptfile,0,len+1)
  end
  found = findfile(p)
  if found(0) eq '' then begin
	spawn,'mkdir '+p
  end

;  if n_elements(statistic_2dids) eq 0 then $
  statistic_2dids = { $
	header: ['',''],$
	roipath: p, $
	rptpath: p, $
	file: p+'tmproi.xdr', $
	fileid: 0L, $
	rpt: p+'tmproi.rpt', $
	picked:  p+'tmproi.xdr.poly', $
	rptid: 0L, $
	comment: '', $
	commentid: 0L, $
	base: 0L, $
	rectbase: 0L, $
	polybase: 0L, $
	backbase: 0L, $
	rptbase: 0L, $
	wid : -1, $
	message : 0L, $
	cursor: 0L, $
	cursorv: 0L, $
	cursor_x: 0., $
	cursor_y: 0., $
	cursor_i: 0, $
	cursor_j: 0, $
	cross: 0, $
	roiid : 0L, $
	drawid : 0L, $
	x1: 0,$
	x2: xsize,$
	y1: 0,$
	y2: ysize,$
	maxid : 0L, $
	maxxid : 0L, $
	maxyid : 0L, $
	minid : 0L, $ 
	minxid : 0L, $
	minyid : 0L, $
	aveid : 0L, $
	devid : 0L, $
	back: 0, $ 		;0-none,1-ave,2-userset
	backid: 0L, $
	backave: 0., $
	backave2: im_max, $
	backslid: 0L, $
	highid: 0L, $
	highslid: 0L, $
	im0: ptr_new(/allocate_heap), $  ;im,$ 
	x: x, $
	y: y, $
	im: congrid(im,xsize,ysize), $   ;400,400), $
	xsize : xsize, $
	ysize : ysize, $
	margin_l : 50, $
	margin_r : 50, $
	margin_b : 50, $
	margin_t : 50, $
	pixmap : make_array(xsize,ysize,/byte), $
	factor: [float(xsize)/width,float(ysize)/height], $
	xrange:[0,width-1], $
	yrange:[0,height-1], $
	width: width, $
	height: height, $
	refresh: 0, $
	versus : 0,$
	debug: 0, $
	report: 0, $
	max : im_max, $
	min : im_min, $
	jmax : jmax, $
	jmin : jmin, $
	max_i: jmax MOD width, $
	max_j: jmax / width, $
	min_i: jmin MOD width, $
	min_j: jmin / width, $
	roi_nelem: width*height, $
	roi_elist: make_array(width*height,/long), $    ;/byte), $
	roi_min: im_min, $
	roi_max: im_max, $
	roi_total: total(im), $
	roi_ave : result[0], $
	roi_dev : sdev, $
	total: total(im), $
	ave : result[0], $
	dev : sdev $
	}

  *statistic_2dids.im0 = im

  if keyword_set(roifile) then begin
	statistic_2dids.file = roifile
	statistic_2dids.picked = roifile+'.poly'
  end
  if keyword_set(rptfile) then statistic_2dids.rpt = rptfile
  if keyword_set(header) then statistic_2dids.header = header
  if keyword_set(comment) then statistic_2dids.comment = comment

  if keyword_set(debug) then statistic_2dids.debug=1

  scan2d_ROI = WIDGET_BASE(GROUP_LEADER=Group, $
      TITLE='2D Statistic ROI (R1.1)', $
      COLUMN=1, $
      MAP=1, $
      UVALUE='scan2d_ROI')

  BASE2 = WIDGET_BASE(scan2d_ROI, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE3 = WIDGET_BASE(BASE2, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE3')

  DRAW3 = WIDGET_DRAW( BASE3, $
      BUTTON_EVENTS=1, $
      MOTION_EVENTS=1, $
      RETAIN=2, $
      UVALUE='STATISTIC_2DDRAW', $
      XSIZE=400, $
      YSIZE=400)

  BASE4 = WIDGET_BASE(BASE2, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE4')

  BASE5 = WIDGET_BASE(BASE4, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE5')

Btns928 = ['RectROI', 'FilterROI', 'PolyROI']   ;,'MultiROIs']
back_versus = WIDGET_DROPLIST(BASE5, VALUE=Btns928, $
        UVALUE='STATISTIC_2DBACKMODE', TITLE='ROI Mode:')

  BUTTON5 = WIDGET_BUTTON( BASE5, $
      UVALUE='STATISTIC_2DMULTIROI', $
      VALUE='MultiROIs...')

  BUTTON6 = WIDGET_BUTTON( BASE5, $
      UVALUE='STATISTIC_2DHELP', $
      VALUE='Help...')

  BUTTON8 = WIDGET_BUTTON( BASE5, $
      UVALUE='STATISTIC_2DCOLOR', $
      VALUE='Color...')

  BUTTON7 = WIDGET_BUTTON( BASE5, $
      UVALUE='STATISTIC_2DDONE', $
      VALUE='Done')

  BASE4_RECTBASE = WIDGET_BASE(BASE4, $
      ROW=1, $ ;/frame, $
      MAP=1, $
      UVALUE='BASE4_RECTBASE')

  rect_label = WIDGET_LABEL(BASE4_RECTBASE,value='Rectangle ROI:')

  BUTTON10 = WIDGET_BUTTON( BASE4_RECTBASE, $
      UVALUE='STATISTIC_2DSAVEROI', $
      VALUE='SaveRectROI')

  BUTTON9 = WIDGET_BUTTON( BASE4_RECTBASE, $
      UVALUE='STATISTIC_2DCLEAN', $
      VALUE='Refresh')

  BUTTON11 = WIDGET_BUTTON( BASE4_RECTBASE, $
      UVALUE='STATISTIC_2DREADROI', $
      VALUE='ReadRectROI')


  BASE4_back = WIDGET_BASE(BASE4, $
      ROW=1, $ ;/frame, $
      MAP=1, $
      UVALUE='BASE4_back')

  BUTTON18 = WIDGET_BUTTON( BASE4_back, $
      UVALUE='STATISTIC_2DWRITEFLTR', $
      VALUE='SaveFltr')
  BUTTON19 = WIDGET_BUTTON( BASE4_back, $
      UVALUE='STATISTIC_2DREADFLTR', $
      VALUE='ReadFltr')

  FIELD27_back = CW_FIELD( BASE4_back,VALUE=statistic_2dids.backave, $
      ROW=1, $
      FLOAT=1, /RETURN_EVENTS, $
      TITLE='Low:', XSIZE=10, $
      UVALUE='STATISTIC_2DBACKFLD')

  if statistic_2dids.max gt statistic_2dids.min then begin
  FSLID3 = CW_FSLIDER( BASE4_back, $
      MAXIMUM=statistic_2dids.max, $
      MINIMUM=statistic_2dids.min, $
      SUPPRESS=1, $
      UVALUE='STATISTIC_2DBACKSLDR', $
      VALUE=statistic_2dids.min)

      statistic_2dids.backslid = FSLID3 
	end
  FIELD27_high = CW_FIELD( BASE4_back,VALUE=statistic_2dids.max, $
      ROW=1, $
      FLOAT=1, /RETURN_EVENTS, $
      TITLE='High:', XSIZE=10, $
      UVALUE='STATISTIC_2DHIGHFLD')
  if statistic_2dids.max gt statistic_2dids.min then begin
  FSLID33 = CW_FSLIDER( BASE4_back, $
      MAXIMUM=statistic_2dids.max, $
      MINIMUM=statistic_2dids.min, $
      SUPPRESS=1, $
      UVALUE='STATISTIC_2DHIGHSLDR', $
      VALUE=statistic_2dids.max)

      statistic_2dids.highslid = FSLID33 
	end

  BASE4_POLYBASE = WIDGET_BASE(BASE4, $
      ROW=1, $ ;/frame, $
      MAP=1, $
      UVALUE='BASE4_POLYBASE')
  poly_label = WIDGET_LABEL(BASE4_POLYBASE,value='Polygon ROI:')
  BUTTON12 = WIDGET_BUTTON( BASE4_POLYBASE, $
      UVALUE='STATISTIC_2DDEFROI', $
      VALUE='DefPolyROI&Save')
  BUTTON13 = WIDGET_BUTTON( BASE4_POLYBASE, $
      UVALUE='STATISTIC_2DREADPOLYROI', $
      VALUE='ReadPolyROI')

  st='Data: IM['+strtrim(width,2)+' x '+strtrim(height,2)+']'
  st=st + ', Total='+strtrim(statistic_2dids.total,2)
  Label1 = WIDGET_LABEL(BASE4,VALUE=st)

  st='Initial ROI: IM[0:'+strtrim(statistic_2dids.xrange[1],2)+ $
	', 0:'+strtrim(statistic_2dids.yrange[1],2)+']'+ $
	',  Nelem='+strtrim(statistic_2dids.roi_nelem,2) + $
	',  Total='+strtrim(statistic_2dids.total)
  Label2 = WIDGET_LABEL(BASE4,VALUE=st)

  BASE4_values = WIDGET_BASE(BASE4, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE4_values')

  FIELD27 = CW_FIELD( BASE4_values,VALUE=statistic_2dids.ave, $
      ROW=1, $
      FLOAT=1, /NOEDIT, $
      TITLE='ROI Ave:', $
      UVALUE='STATISTIC_2DAVE')

  FIELD28 = CW_FIELD( BASE4_values,VALUE=statistic_2dids.dev, $
      ROW=1, $
      FLOAT=1, /NOEDIT, $
      TITLE='ROI Dev:', $
      UVALUE='STATISTIC_2DDEV')

  BASE4_min = WIDGET_BASE(BASE4, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE4_min')

  FIELD16 = CW_FIELD( BASE4_min,VALUE=statistic_2dids.min, $
      ROW=1, $
      FLOAT=1, /NOEDIT, $
      TITLE='ROI Min:', $
      UVALUE='STATISTIC_2DMIN')

  FIELD17 = CW_FIELD( BASE4_min,VALUE=statistic_2dids.min_i, $
      ROW=1, $
      STRING=1, /NOEDIT, Xsize=6,$
      TITLE='@x', $
      UVALUE='STATISTIC_2DXMIN')

  FIELD18 = CW_FIELD( BASE4_min,VALUE=statistic_2dids.min_j, $
      ROW=1, $
      STRING=1, /NOEDIT, xsize=6,$
      TITLE='@y', $
      UVALUE='STATISTIC_2DYMIN')

Btns918 = ['Index #', 'Values']
position_versus = WIDGET_DROPLIST(BASE4_min, VALUE=Btns918, $
        UVALUE='STATISTIC_2DVERSUS', TITLE='vs')
WIDGET_CONTROL, position_versus, SET_DROPLIST_SELECT=statistic_2dids.versus


  BASE4_max = WIDGET_BASE(BASE4, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE4_max')

  FIELD23 = CW_FIELD( BASE4_max,VALUE=statistic_2dids.max, $
      ROW=1, $
      FLOAT=1, /NOEDIT, $
      TITLE='ROI Max:', $
      UVALUE='STATISTIC_2DMAX')


  FIELD24 = CW_FIELD( BASE4_max,VALUE=statistic_2dids.max_i, $
      ROW=1, $
      STRING=1, /NOEDIT, xsize=6, $
      TITLE='@x', $
      UVALUE='STATISTIC_2DXMAX')

  FIELD25 = CW_FIELD( BASE4_max,VALUE=statistic_2dids.max_j, $
      ROW=1, $
      STRING=1, /NOEDIT, xsize=6, $
      TITLE='@y', $
      UVALUE='STATISTIC_2DYMAX')

  statistic_comment = CW_FIELD(BASE4,value=statistic_2dids.comment, $
	TITLE='Comment:', /RETURN_EVENTS, $
	UVALUE='STATISTIC_2DCOMMENT', XSIZE=60)
  if keyword_set(comment) then statistic_2dids.comment=comment

  BASE4_file = WIDGET_BASE(BASE4, $
      ROW=1, /frame, $
      MAP=1, $
      UVALUE='BASE4_file')

MenuDesc165 = [ $
      { CW_PDMENU_S,       3, 'ROIFile' }, $ ;        0
        { CW_PDMENU_S,       0, 'New...' }, $ ;        1
        { CW_PDMENU_S,       0, 'Open...' } $ ;        2
  ]
  PDMENU13 = CW_PDMENU( BASE4_file, MenuDesc165, /RETURN_FULL_NAME, $
      UVALUE='PDMENU13')

statistic_filename = WIDGET_TEXT(BASE4_file,VALUE=statistic_2dids.file, $
		XSIZE=60, /EDITABLE, UVALUE='STATISTIC_2DFILE')
  statistic_2dids.fileid = statistic_filename


  BASE4_file20 = WIDGET_BASE(BASE4, $
      COLUMN=1, /frame, $
      MAP=1, $
      UVALUE='BASE4_file2')

  BASE4_file2 = WIDGET_BASE(BASE4_file20, $
      ROW=1, /frame, $
      MAP=1, $
      UVALUE='BASE4_file2')

MenuDesc167 = [ $
      { CW_PDMENU_S,       3, 'RptFile' }, $ ;        0
        { CW_PDMENU_S,       0, 'New...' }, $ ;        1
        { CW_PDMENU_S,       0, 'Open...' } $ ;        2
  ]
  ROI2D_PDMENU3 = CW_PDMENU( BASE4_file2, MenuDesc167, /RETURN_FULL_NAME, $
      UVALUE='ROI2D_PDMENU3')

statistic_filename2 = WIDGET_TEXT(BASE4_file2,VALUE=statistic_2dids.rpt, $
		XSIZE=60, /EDITABLE, UVALUE='STATISTIC_2DFILE2')
  statistic_2dids.rptid = statistic_filename2

  BASE4_file21 = WIDGET_BASE(BASE4_file20, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE4_file2')

;  ReplaceRpt_btn = WIDGET_BUTTON( BASE4_file21, $
;      UVALUE='STATISTIC_2DREPLACE', $
;      VALUE='ReplaceRpt')

  RenameRpt_btn = WIDGET_BUTTON( BASE4_file21, $
      UVALUE='STATISTIC_2DRENAME', $
      VALUE='RenameRpt...')

  AppendRpt_btn = WIDGET_BUTTON( BASE4_file21, $
      UVALUE='STATISTIC_2DREPORT', $
      VALUE='AppendRpt...')

  BUTTON18 = WIDGET_BUTTON( BASE4_file21, $
      UVALUE='STATISTIC_2DREPORTVIEW', $
      VALUE='ViewRpt...')

  st='**Press MMB to Query Pixel Values**'
  TEXT4 = WIDGET_TEXT( BASE3,VALUE=st, /scroll, $
      UVALUE='SCAN2D_ROIMESSAGE', XSIZE=40, YSIZE=4)

  st='CURSOR:...  '
  Label3 = WIDGET_LABEL(BASE3,VALUE=st,XSIZE=400,/align_left)
  Label5 = WIDGET_LABEL(BASE3,VALUE='         ',XSIZE=400,/align_left)
  
  statistic_2dids.base = scan2d_ROI
  statistic_2dids.rectbase = BASE4_RECTBASE 
  statistic_2dids.polybase = BASE4_POLYBASE 
  statistic_2dids.rptbase =  BASE4_file20
  statistic_2dids.backbase =  BASE4_back
  statistic_2dids.cursor = Label3 
  statistic_2dids.cursorv = Label5 
  statistic_2dids.message = TEXT4 
  statistic_2dids.roiid = Label2 
  statistic_2dids.drawid = DRAW3
  statistic_2dids.minid = FIELD16
  statistic_2dids.minxid = FIELD17
  statistic_2dids.minyid = FIELD18
  statistic_2dids.maxid = FIELD23
  statistic_2dids.maxxid = FIELD24
  statistic_2dids.maxyid = FIELD25
  statistic_2dids.aveid = FIELD27
  statistic_2dids.devid = FIELD28
  statistic_2dids.backid = FIELD27_back
  statistic_2dids.highid = FIELD27_high
  statistic_2dids.commentid = statistic_comment

  WIDGET_CONTROL, scan2d_ROI, /REALIZE

  ; Get drawable window index

  COMMON DRAW3_Comm, DRAW3_Id
  WIDGET_CONTROL, DRAW3, GET_VALUE=DRAW3_Id

	statistic_2dPlot,statistic_2dids
	statistic_2dids.pixmap = tvrd(statistic_2dids.margin_l,statistic_2dids.margin_b,statistic_2dids.xsize,statistic_2dids.ysize)

  if keyword_set(mode) then statistic_2dids.back = mode
  WIDGET_CONTROL,BASE4_RECTBASE,SENSITIVE=0
  WIDGET_CONTROL,BASE4_back,SENSITIVE=0
  WIDGET_CONTROL,BASE4_POLYBASE,SENSITIVE=0
  if statistic_2dids.back eq 0 then WIDGET_CONTROL,BASE4_RECTBASE,SENSITIVE=1
  if statistic_2dids.back eq 1 then WIDGET_CONTROL,BASE4_back,SENSITIVE=1
  if statistic_2dids.back eq 2 then WIDGET_CONTROL,BASE4_POLYBASE,SENSITIVE=1
  WIDGET_CONTROL, back_versus, SET_DROPLIST_SELECT=statistic_2dids.back

 	statistic_2dids.wid = !d.window

  	scan2d_ROI_readroi,statistic_2dids

  WIDGET_CONTROL, scan2d_ROI, SET_UVALUE=statistic_2dids
  XMANAGER, 'scan2d_ROI', scan2d_ROI
END
