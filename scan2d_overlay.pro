PRO scan2d::Overlay,scanno,row=row,col=col,pixels=pixels,selects=selects,discrete=discrete
;
;+
; NAME:
;	scan2d::Overlay
;
; PURPOSE:
;       Using an overlay composite image reveals information about the 
;       superposition of the images of selected detectors. It provides 
;       another way of data interpretation.
;
;       This method constructs a composite image based on user selected
;       detectors for a given 2D scanno. The composite image is composed
;       of the basic composite element area. Each composite element area 
;       consists of ColxRow of small squares. Each colored filled small 
;       square area represents a data point from the selected image (or 
;       detector). So each composite area is composed of one data point
;       from each selected detectors and they are arranged in row order. 
;
;       The resultant composite image with width of ColxWidth squares,
;       and height of RowxHeight squares. (each Detector dimension is 
;       Width x Height)
;
;       In the basic element area all selected detectors are filled in row
;       order until the area is full then it re-starts from the first row 
;       again, i.e. if more detectors than the available squares are 
;       available then the overlay of colored squares may be resulted.
;
;       Default 2x2 squares are used for basic element area which can hold 
;       4 detects without overlapping. Each square has width of 2 pixels.
;
;       Each detector has a fixed color associated with it, it is linearly
;       devided into 16 levels. The detector value is linearly interpreted 
;       by 16 levels (see restriction).
;
; CATEGORY:
;       Widgets.
;
; CALLING SEQUENCE:
;       Obj->[scan2d::]Overlay [,Scanno] [,Row=row] [,Col=col] [,Pixels=pixels]
;                 [,Selects=selects] [,Discrete=discrete]
;
; ARGUMENTS:
;  Scanno:   Optional, specifies the corresponding 2D scan seq #, normally
;            it is internally determined by the [scan2d::View] method.
;
; KEYWORDS:
;  Col:      Specifies the number of squares in the composite area , default 2
;  Row:      Specifies the number of squares in the composite area , default 2
;  Pixels:   Specifies the number of pixels used for each square, default 2
;  Selects:  Specifies the list of selected detectors, default 0,1 
;  Discrete: Plots selected detecor image seperately with info of min and max
;
; RESTRICTION:
;  The image array size may varies from the scan to scan. In order to 
;  make sure this method works properly, the scan2d::View method has to be 
;  called first to establish the proper image array size for the desired 
;  2D scanno. 
;  
;  16 colors are used and they are shaded with gray
;
;       Detector 1            Red 
;       Detector 2            Green 
;       Detector 3            Blue 
;       Detector 4            Yellow 
;       Detector 5            Cyne 
;       Detector 6            Magenta 
;       Detector 7            Gray 
;       Detector 8            Orange 
;       Detector 9            Light Green 
;       Detector 10           Purple 
;       Detector 11           Gold 
;       Detector 12           Light Orange 
;       Detector 13           Light Cyne 
;       Detector 14           Light purple 
;       Detector 15           Dark Gray 
;       Detector 16           Dark Yellow 
;
;  The 'Color ...' button let user access various color tables comes with IDL. 
;  The 'myColr' button let user switch back to overlay image color table.
; 
; EXAMPLE:
;    The 2D image file is '/home/oxygen/LEGNINI/data/root/plla.june97.image'
;    The scanno 29 consists of 10 detectors, with 2D image # 202 to 211.
;
;    Example 1 gives the default overlay of detectors 1 and 2 image for 
;    scanno 29 from this file. The panImage method shows all detectors
;    images for the 2D scan.
;    
;    The object v2 need to be defined only if it is not yet defined.
;
;    filename='/home/oxygen/LEGNINI/data/root/plla.june97.image' 
;    v2 = obj_new('scan2d',file=filename)
;    v2->view,202
;    v2->panimage
;    v2->overlay
;
;    Example 2 uses 2x2 composite area with 4 detectors selected, number
;    of pixels used for each square is 8.
;
;    v2->overlay,row=2,col=2,pixels=8,selects=[0,1,8,9]
;
;    Toggle the 'myColor' and 'Color ...' buttons from the Overlay window
;    to access various color map.
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jan 19, 1998.
;	xx-xx-xxxx      comment
;-
; pixels   - no of pixels used for each data point, default 2 
; detectors overlay matrix  (col x row)
; col      - no of detectors in col used for overlay, default 2
; row      - no of detectors in row used for overlay, default 2
; selects   - list of selected detectors, default to first two detectors:[0,1]
; discrete  - if defined each detector has its own image,
; Color table is devided into 15 color schemes. 
;

unit = self.opened
seq = self.scanno_current
if n_elements(scanno) then seq = scanno 
xdim = self.width 
ydim = self.y_req_npts
last = self.scanno_2d_last

if unit le 0 then begin
	res=WIDGET_MESSAGE('Error: no image file loaded in')
	return
end
if seq lt 1 or seq gt self.scanno_2d_last then begin
	res=WIDGET_MESSAGE('Error: outside range 2D scanno entered')
	return
end

	seqno = self.image_no(seq-1)

	point_lun, unit, self.fptr(seqno)

	image_array  = make_array(xdim,ydim,15,/float)
	def = make_array(15,value=0)

	vmin = make_array(15,/float)
	vmax = make_array(15,/float)
	scanno_2d = seq
	for i=0,14 do begin
		if EOF(unit) eq 1 then goto,update
		u_read, unit, pvs
		u_read, unit, nos
		if nos(4) ne seq then goto,update
		u_read, unit, x
		u_read, unit, y
		u_read, unit, t_image
		def(nos(3)) = 1
		image_array(*,*,nos(3)) = t_image
		rmax = max(t_image,min=rmin)
		vmax(nos(3)) = rmax
		vmin(nos(3)) = rmin
	end

update:
	overlayInitState,overlay_state,image_array,def,vmax,vmin,col=col,row=row,pixels=pixels,selects=selects,discrete=discrete

	SCAN2D_OVERLAYIMAGE,overlay_state
END


PRO box,x0,y0,x1,y1,color
	polyfill,[x0,x0,x1,x1], [y0,y1,y1,y0],col=color,/device
END


PRO scan2d_setOverlayColorTbl,base,rat

r = make_array(256,/int)
g = make_array(256,/int)
b = make_array(256,/int)

if n_elements(base) eq 0 then $
base = 128; base color: 0 is black, 128 dark gray
if n_elements(rat) eq 0 then $
rat = 8     ; for black use 16, for dark gray use 8 

for j=0,15 do begin
ij = j*16
CASE j OF
   15:BEGIN ; red
	for i=1,16 do begin
		r(i-1+ij) = i*rat-1
		g(i-1+ij) = 0
		b(i-1+ij) = 0
	end
     END
   14:BEGIN ; green
	for i=1,16 do begin
		r(i-1+ij) = 0
		g(i-1+ij) = i*rat-1
		b(i-1+ij) = 0
	end
     END
   13:BEGIN ; blue 
	for i=1,16 do begin
		r(i-1+ij) = 0
		g(i-1+ij) = 0 
		b(i-1+ij) = i*rat-1
	end
     END
   12:BEGIN ; yellow 
	for i=1,16 do begin
		r(i-1+ij) = i*rat-1
		g(i-1+ij) = i*rat-1
		b(i-1+ij) = 0
	end
     END
   11:BEGIN ; cyan 
	for i=1,16 do begin
		r(i-1+ij) = 0
		b(i-1+ij) = i*rat-1
		g(i-1+ij) = i*rat-1
	end
     END
   10:BEGIN ; magenta 
	for i=1,16 do begin
		r(i-1+ij) = i*rat-1
		g(i-1+ij) = 0
		b(i-1+ij) = i*rat-1
	end
     END
   9:BEGIN ; gray 
	for i=1,16 do begin
		r(i-1+ij) = i*rat-1
		g(i-1+ij) = i*rat-1
		b(i-1+ij) = i*rat-1
	end
     END
   8:BEGIN ; orange 
	for i=1,16 do begin
		r(i-1+ij) = i*rat-1
		g(i-1+ij) = i*rat/2-1
		b(i-1+ij) = 0
	end
     END
   7:BEGIN ; light green 
	for i=1,16 do begin
		r(i-1+ij) = i*rat/2-1
		g(i-1+ij) = i*rat-1
		b(i-1+ij) = i*rat/2-1
	end
     END
   6:BEGIN ; purple 
	for i=1,16 do begin
		r(i-1+ij) = i*rat/2-1
		g(i-1+ij) = 0
		b(i-1+ij) = i*rat-1
	end
     END
   5:BEGIN ; gold
	for i=1,16 do begin
		r(i-1+ij) = i*rat-1
		g(i-1+ij) = i*rat-1
		b(i-1+ij) = i*rat/2-1
	end
     END
   4:BEGIN ; light orange
	for i=1,16 do begin
		r(i-1+ij) = i*rat-1
		g(i-1+ij) = i*rat/2-1
		b(i-1+ij) = i*rat/2-1
	end
     END
   3:BEGIN ; light cyne 
	for i=1,16 do begin
		r(i-1+ij) = i*rat/2-1
		g(i-1+ij) = i*rat-1
		b(i-1+ij) = i*rat-1
	end
     END
   2:BEGIN ; light purple
	for i=1,16 do begin
		r(i-1+ij) = i*rat/2-1
		g(i-1+ij) = i*rat/2-1
		b(i-1+ij) = i*rat-1
	end
     END
   1:BEGIN ; dark gray
	for i=1,16 do begin
		r(i-1+ij) = i*rat/2-1
		g(i-1+ij) = i*rat/2-1
		b(i-1+ij) = i*rat/2-1
	end
     END
   0:BEGIN ; dark yellow
	for i=1,16 do begin
		r(i-1+ij) = i*rat/2-1
		g(i-1+ij) = i*rat/2-1
		b(i-1+ij) = 0
	end
     END
   ELSE:
ENDCASE
end
	tvlct,r+base,g+base,b+base
;	xpalette
	scan2d_saveOverlayColorTbl
END

PRO scan2d_saveOverlayColorTbl
	tvlct,red,green,blue,/get
	save,red,green,blue,file='overlay.tbl'
END

PRO scan2d_getOverlayColorTbl
	restore,'overlay.tbl'
	tvlct,red,green,blue
END

PRO SCAN2D_OVERLAY_Event, Event

  WIDGET_CONTROL,Event.Top,GET_UVALUE=overlay_state
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'scan2d_overlay': BEGIN
      Print, 'Event for scan2d_overlay'
      END
  'BUTTON6': BEGIN
	WIDGET_CONTROL,Event.Top,/DESTROY
      END
  'BUTTON7': BEGIN
	XLOADCT
      END
  'BUTTON8': BEGIN
	SCAN2D_GETOVERLAYCOLORTBL
      END

  'OVERLAY_NX': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ncol
	old_ncol = overlay_state.ncol
	overlay_state.ncol = ncol
;	overlay_state.nrow = 16 / overlay_state.ncol
	WIDGET_CONTROL,Event.ID,SET_VALUE=old_ncol

	overlay_replot,overlay_state

      END

  'OVERLAY_NY': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=nrow
	old_nrow = overlay_state.nrow
	overlay_state.nrow = nrow
;	overlay_state.ncol = 16 / overlay_state.nrow
	WIDGET_CONTROL,Event.ID,SET_VALUE=old_nrow

	overlay_replot,overlay_state

      END
  'OVERLAY_SELECTS': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=detectors
	selects = str_sep(detectors(0),',')
	selects = fix(selects)
	if max(selects) ge 15 then begin
		res=dialog_message('0 based detector value can not exceed 14',/Error)
		return
	end
	pick = make_array(15,/int)
	if n_elements(selects) gt 0 then begin
		for i=0,n_elements(selects)-1 do begin
		pick(selects(i)) = 1
		end
	end
	
	old_string = overlay_state.pick_string
	pick_string=strcompress(detectors(0),/remove_all)
	overlay_state.pick_string = pick_string
	overlay_state.pick = pick
	overlay_state.selects = total(pick)
	WIDGET_CONTROL,Event.ID,SET_VALUE=old_string

	overlay_replot,overlay_state

      END

  'OVERLAY_PIXELS': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=pixels
	old_pixels = overlay_state.pixels
	overlay_state.pixels = pixels
	WIDGET_CONTROL,Event.ID,SET_VALUE=old_pixels

	overlay_replot,overlay_state

      END
  ENDCASE
END

PRO overlay_replot,overlay_state

	overlay_state.width = overlay_state.pixels*overlay_state.ncol*overlay_state.xdim+250
	if overlay_state.discrete eq 0 then $
	overlay_state.height = overlay_state.pixels*overlay_state.ydim*overlay_state.nrow else $
	overlay_state.height = overlay_state.pixels*overlay_state.ydim*overlay_state.selects

	scan2d_overlayimage,overlay_state
END


PRO overlayInitState,overlay_state,image_array,def,vmax,vmin,col=col,row=row,pixels=pixels,selects=selects,discrete=discrete
	
s = size(image_array)
xdim=s(1)
ydim=s(2)
	vcol = make_array(15,/int)
;	dcol = !d.n_colors/15
	dcol = !d.n_colors/16
	found = findfile('overlay.tbl')
	if found(0) eq '' then SCAN2D_SETOVERLAYCOLORTBL,128,8
	SCAN2D_GETOVERLAYCOLORTBL

	for i=0,14 do begin
		vcol(i) = !d.n_colors - i*dcol - 1 
;	print,i,vcol(i),vmin(i),vmax(i)
	end

	magnifyfactor = 2
	if keyword_set(pixels) then magnifyfactor=pixels

	nrow=2
	ncol=2
	if keyword_set(row) then nrow=row
	if keyword_set(col) then ncol=col
	if nrow*ncol gt 16 then nrow = 16/ncol+1

	pick_string=''
	pick = make_array(15,/int)
	if n_elements(selects) gt 0 then begin
		for i=0,n_elements(selects)-1 do begin
		pick(selects(i)) = 1
		pick_string=pick_string+strtrim(selects(i),2)+','
		end
	endif else begin
	pick_string='0,1'
	pick(0:1)=1
	end

; construct overlay array

overlay_state = { $
	base : 0L, $
	win : 0, $
	width: magnifyfactor*ncol*xdim +250,$
	height: magnifyfactor*ydim*total(pick),$
	xsize : 300, $
	ysize : 400, $
	xdim : xdim, $
	ydim : ydim, $
	def : def, $
	images : image_array, $
	vmin: vmin, $
	vmax: vmax, $
	ncol: ncol, $
	nrow: nrow, $
	pixels: magnifyfactor, $
	pick_string: pick_string, $
	pick: pick, $
	selects : total(pick), $
	discrete : 0, $
	dcol: dcol, $
	vcol: vcol $
	}

if keyword_set(discrete) then overlay_state.discrete=1  ; plot discrete images
if overlay_state.discrete eq 0 then overlay_state.height=magnifyfactor*ydim*nrow

END

PRO overlayImage,overlay_state

	xdim = overlay_state.xdim
	ydim = overlay_state.ydim
	selects = overlay_state.selects
	def = overlay_state.def
	vmax = overlay_state.vmax
	vmin = overlay_state.vmin
	image_array = overlay_state.images
	ncol = overlay_state.ncol
	nrow = overlay_state.nrow
	magnifyfactor = overlay_state.pixels
	vcol = overlay_state.vcol
	dcol = overlay_state.dcol
	pick = overlay_state.pick
	discrete = overlay_state.discrete

	WSET,overlay_state.win	
	erase

	if discrete eq 0 then begin
	  kk=0
	  for k=0,14 do begin
	  IF def(k) EQ 1 and pick(k) gt 0 THEN BEGIN
		for i=0,xdim-1 do begin
		ii = i * ncol

		for j=0,ydim-1 do begin
		jj = j * nrow

		factor = 0. ; 1.
		if vmax(k) gt vmin(k) then $
			factor = (image_array(i,j,k) - vmin(k)) / (vmax(k)-vmin(k)) 
		kcolor = vcol(k) - fix(dcol*(1 - factor))
		if factor le 0. then kcolor = vcol(k+1)+1 

		ki = kk mod ncol
		kj = kk / ncol	
		x0=(ii+ki)*magnifyfactor
		x1=(ii+ki+1)*magnifyfactor
		y0=(jj+kj)*magnifyfactor
		y1=(jj+kj+1)*magnifyfactor
		box,x0,y0,x1,y1,kcolor
		end
		end
		kk=kk+1
	  END
	  end
	endif else begin
;
; plot selected detectors with color scheme
;
	numj=0 
	ncol = 1
	for k=0,14 do begin
	IF pick(k) EQ 1 THEN BEGIN
		ki = k mod ncol
		kj1 = overlay_state.height - numj*ydim*magnifyfactor
		kj0 = kj1 - ydim*magnifyfactor
		for i=0,xdim-1 do begin
		ii = i * ncol

		for j=0,ydim-1 do begin
		jj = j 

		factor = 0. ; 1.
		if vmax(k) gt vmin(k) then $
			factor = (image_array(i,j,k) - vmin(k)) / (vmax(k)-vmin(k)) 
		kcolor = vcol(k) + dcol*factor
	
		x0=(ii+ki)*magnifyfactor
		x1=(ii+ki+1)*magnifyfactor
		y0= kj0+jj*magnifyfactor
		y1= y0 + magnifyfactor
		box,x0,y0,x1,y1,kcolor
		end
		end
		numj = numj+1
	;  write the detector info here min,max,detector
		str =  ' D'+strtrim(k+1,2) + ', Max='+strtrim(vmax(k),2)+ $
			', Min='+strtrim(vmin(k),2)
		xyouts,x1,kj0+ydim/2*magnifyfactor,str,/device
		END
	end
	end
END



; DO NOT REMOVE THIS COMMENT: END SCAN2D_OVERLAY
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO SCAN2D_OVERLAYIMAGE,overlay_state, GROUP=Group

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }
  xsize =overlay_state.width
  ysize =overlay_state.height

  SCAN2D_OVERLAY = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, TITLE='Overlay 2D Image', $
      MAP=1, $
      UVALUE='SCAN2D_OVERLAY')

  BASE2 = WIDGET_BASE(SCAN2D_OVERLAY, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE2_2 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2_2')

  BASE2_1 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2_1')

  DRAW3 = WIDGET_DRAW( BASE2, $
      RETAIN=1, $
      UVALUE='scan2d_overlay', $
      XSIZE=xsize, $
      X_SCROLL_SIZE=300, $
      YSIZE=ysize, $
      Y_SCROLL_SIZE=400)

  BASE5 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE5')

  BUTTON6 = WIDGET_BUTTON( BASE5, $
      UVALUE='BUTTON6', $
      VALUE='Done')

  BUTTON7 = WIDGET_BUTTON( BASE5, $
      UVALUE='BUTTON7', $
      VALUE='Color ...')

  BUTTON8 = WIDGET_BUTTON( BASE5, $
      UVALUE='BUTTON8', $
      VALUE='myColor')

  pixels_FIELD2 = CW_FIELD( BASE2_1,VALUE=overlay_state.pixels, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Pixels', $
      UVALUE='OVERLAY_PIXELS', $
      XSIZE=2)

if overlay_state.discrete eq 0 then begin
  Ncol_FIELD3 = CW_FIELD( BASE2_1,VALUE=overlay_state.ncol, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Col', $
      UVALUE='OVERLAY_NX', $
      XSIZE=2)

  Nrow_FIELD4 = CW_FIELD( BASE2_1,VALUE=overlay_state.nrow, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Row', $
      UVALUE='OVERLAY_NY', $
      XSIZE=2)
end
  selects_FIELD3 = CW_FIELD( BASE2_2,VALUE=overlay_state.pick_string, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Select Detectors', $
      UVALUE='OVERLAY_SELECTS', $
      XSIZE=25)

  g_tlb = WIDGET_INFO(SCAN2D_OVERLAY,/geometry)

  WIDGET_CONTROL, SCAN2D_OVERLAY, /REALIZE
  WIDGET_CONTROL,SCAN2D_OVERLAY,SET_UVALUE=overlay_state

  ; Get drawable window index

  COMMON DRAW3_Comm, DRAW3_overlayId
  WIDGET_CONTROL, DRAW3, GET_VALUE=DRAW3_overlayId

	overlay_state.base = SCAN2D_OVERLAY
	overlay_state.win = DRAW3_overlayId
	overlay_state.xsize = g_tlb.scr_xsize
	overlay_state.ysize = g_tlb.scr_ysize

  overlayimage,overlay_state


  XMANAGER, 'SCAN2D_OVERLAY', SCAN2D_OVERLAY,/NO_BLOCK
END
