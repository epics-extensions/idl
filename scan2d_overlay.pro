;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
; supoort scan2d, scanSee and any image_array > 15 detectors
; overlay.tbl      - default save file for overlay color table
; overlay_pvt.tbl  - private save file for overlay color table  

@PS_open.pro

PRO scan2d::Overlay,scanno,row=row,col=col,pixels=pixels,selects=selects,discrete=discrete
;
;+
; NAME:
;	scan2d::Overlay
;
; PURPOSE:
;       Using an overlay composite image reveals information about the 
;       superposition of the images of selected detectors. It provides 
;       another way of data interpretation of catcher generated images.
;
;       This method constructs a composite image based on user selected
;       detectors for a given 2D scanno. The composite image is composed
;       of the basic composite element area. Each composite element area 
;       consists of ColxRow of small squares. Each colored filled small 
;       square area represents a data point from the selected image (or 
;       detector). So each composite area is composed of one data point
;       from each selected detectors and they are arranged in row order. 
;
;       The resultant composite image with width of ColxWidth,
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
;       Each detector has a fixed color table associated with it, it is linearly
;       divided into 16 levels. The detector value is linearly interpreted 
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
;  Selects:  Specifies the list of selected detectors, default 1,2 
;  Discrete: Plots selected detecor image seperately with info of min and max
;
; RESTRICTION:
;  The postscript "Print" button read the TV sceen and generate the PS output.
;  The PS output only good for the screen size within  750x750 pixels.
;
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
; EXAMPLES:
;    The 2D image file is '/home/oxygen/LEGNINI/data/root/plla.june97.image'
;    The scanno 29 consists of 10 detectors, with 2D image # 202 to 211.
;
;    Example 1 - Use the default overlay of detectors 1 and 2 image for 
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
;    Example 2 - Use 2x2 composite area with 4 detectors selected, number
;    of pixels used for each square is 8.
;
;    v2->overlay,row=2,col=2,pixels=8,selects=[1,8,9,10]
;
;    Toggle the 'Default' and 'Color ...' buttons from the Overlay window
;    to access various color map.
;
;    Example 3 - Plot detector 9's discrete image (pixel scaled by 4) 
;
;	v2->overlay,pixels=4,selects=9,/discrete
;
;    Example 3 - Plot discrete images of detectors 8,9,10 (pixel scaled by 4) 
;
;	v2->overlay,pixels=4,selects=[8,9,10],/discrete
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jan 19, 1998.
;	01-08-02      Add Print, Printer... button
;                     Add slider control on Color Table used
;                     Default Color will set Bg slider to bg=128, Fg ratio=8
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

;	panImage,image_array,def,detnm=overlay_state.dname

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
	catch,error_status
	if error_status ne 0 then return
	restore,'overlay.tbl'
	tvlct,red,green,blue
END

PRO scan2d_overlay_help,Event
  str = ["                 Help on Overlay 2D Image",'', $
  'Pane Images  :   Show discrete images ', $
  'List of names:   Show name of images selected', $
  'Plot Type    :   Overlay / Discrete / Composite', $
  'XDR		:   Save the composite as XDR data', $
  'Contrast     :   On or Off', $
  'List of DIs  :   List of initially picked images', $
  'Image seq #  :   Enter images sequence numbers seperated by comma', $
  'Pixel        :   Number of pixels used for each value point, default 2', $
  'Column       :   Columns used in overlay/splice selected image, default 2', $
  'Row          :   Rows used in overlay/splice selected image, default 2', $
  'Image Area   :   Drawing area for displaying overlaid/discrete images', $
  'Color Control:',$
  '     "Bg     " - Control the Background Color level ', $
  '     "Fg     " - Control the number of Foreground Color number', $
  '     "Default" - Set Bg to 128, and Fg to 8', $
  'Save PvtColorT : Save current color table in overlay.tbl', $
  'Load PvtColorT : Load in the private overlay color table ', $
  'Color ...    :   Access of default IDL color tables, default pepermint', $
  'Print        :   Dump the Image area to PS printer', $
  'Printer...   :   Override the default PS printer', $
  'Help...      :   Display this help page', $
  'Done         :   Exit 2D overlay program' $
	]
  xdisplayfile,text=str,GROUP=Event.top
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
	return
      END
  'BUTTON7': BEGIN
;	loadct,31   ;pepermint  waves=37
	XLOADCT
	overlayImage,overlay_state
  WIDGET_CONTROL,overlay_state.tyWID,SET_VALUE=overlay_state.discrete
      END
  'OVERLAY_SAVE_PVTCT': BEGIN
	tvlct,R,G,B,/get
	save,R,G,B,file='overlay_pvt.tbl'
      END
  'OVERLAY_LOAD_PVTCT': BEGIN
	catch,error_status
	if error_status eq 0 then begin
	restore,'overlay_pvt.tbl'
	tvlct,R,G,B
	end
	overlayImage,overlay_state
      END
  'BUTTON8': BEGIN
	widget_control,overlay_state.bg_sdr,SET_VALUE=128,bad=bad
	if bad then return
	widget_control,overlay_state.ratio_sdr,SET_VALUE=8
	scan2d_setOverlayColorTbl,128,8
	SCAN2D_GETOVERLAYCOLORTBL
	overlayImage,overlay_state
      END
  'BUTTON9': BEGIN
	PS_TVRD,file='ovl.ps',wid=overlay_state.win,scale=2
      END
  'BUTTON10': BEGIN
	PS_printer
      END
  'BUTTON11': BEGIN
	scan2d_overlay_help,Event
      END

  'OVERLAY_NX': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ncol
	if ncol le 0 then begin
		widget_control,Event.ID,set_value=1
		ncol = 1
		end
	overlay_state.ncol = ncol

	overlayImage_draw2,overlay_state

      END

  'OVERLAY_NY': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=nrow
	if nrow le 0 then begin
		widget_control,Event.ID,set_value=1
		nrow = 1
		end
	overlay_state.nrow = nrow

	overlayImage_draw2,overlay_state

      END
  'OVERLAY_SELECTS': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=detectors
	selects = str_sep(detectors(0),',')
	selects = fix(selects) - 1
	if max(selects) ge 15 then begin
		res=dialog_message('Image # value can not exceed 15',/Error)
		return
	end
	pick = make_array(15,/int)
	if n_elements(selects) gt 0 then begin
		for i=0,n_elements(selects)-1 do begin
		if selects(i) ge 0 then pick(selects(i)) = 1
		end
	end
	
;	old_string = overlay_state.pick_string
	pick_string=strcompress(detectors(0),/remove_all)
	overlay_state.pick_string = pick_string
	overlay_state.pick = pick
	overlay_state.selects = fix(total(pick))

  pick = where(overlay_state.pick gt 0)
  str ='SELECTED:   '
  for i=0,N_ELEMENTS(pick)-1 do begin
  str = str + overlay_state.dname(pick(i))
  if (i+1) lt N_ELEMENTS(pick) then str=str+','
  end
	WIDGET_CONTROL,overlay_state.labelWID,SET_VALUE=str

	overlayImage_draw2,overlay_state

      END
  'OVERLAYTABLE_BG': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=bg
	overlay_state.tbl_bg = bg
	scan2d_setOverlayColorTbl,overlay_state.tbl_bg,overlay_state.tbl_ratio
	overlayImage,overlay_state
      END
  'OVERLAYTABLE_RATIO': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=v
	overlay_state.tbl_ratio = v
	scan2d_setOverlayColorTbl,overlay_state.tbl_bg,overlay_state.tbl_ratio
	overlayImage,overlay_state
      END
  'OVERLAY_PIXELS': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=pixels
	if pixels le 0 then begin
		widget_control,Event.ID,set_value=1
		pixels = 1
		end
	overlay_state.pixels = pixels

	overlayImage_draw2,overlay_state

      END
  'OVERLAY_XDR': BEGIN
	overlay_state.xdr = Event.Value
      END
  'OVERLAY_TYPE': BEGIN
	overlay_state.discrete = Event.Value
	overlayImage_draw1,overlay_state
	overlayImage_draw2,overlay_state
      END
  'OVERLAY_SHOWMINPT': BEGIN
	overlay_state.minpt = Event.Value
	overlayImage_draw2,overlay_state
      END

  ENDCASE

  WIDGET_CONTROL,Event.Top,SET_UVALUE=overlay_state

END



PRO overlayInitState,overlay_state,image_array,def,vmax,vmin,col=col,row=row,pixels=pixels,selects=selects,discrete=discrete,fdname=fdname
	
s = size(image_array)
xdim=s(1)
ydim=s(2)
	vcol = make_array(15,/int)
	dcol = 16
	found = findfile('overlay.tbl')
	if found(0) eq '' then SCAN2D_SETOVERLAYCOLORTBL,128,8
	SCAN2D_GETOVERLAYCOLORTBL

	for i=0,14 do begin
		vcol(i) = 256 - i * dcol -1
	end

	magnifyfactor = 2
	if keyword_set(pixels) then magnifyfactor=pixels

	nrow=2
	ncol=2
	if keyword_set(row) then nrow=row
	if keyword_set(col) then ncol=col
	if keyword_set(discrete) then begin
		ncol = 1
		nrow = 1
	end
	if nrow*ncol gt 16 then nrow = 16/ncol+1

	pick_string=''
	pick = make_array(15,/int)
	if n_elements(selects) gt 0 then begin
		for i=0,n_elements(selects)-1 do begin
		pick(selects(i) - 1) = 1
		pick_string=pick_string+strtrim(selects(i),2)+','
		end
	endif else begin
	pick_string='1,2'  ;'0,1'
	pick(0:1)=1
	end

; construct overlay array

overlay_state = { $
	base : 0L, $
	bg_sdr: 0L, $
	ratio_sdr: 0L, $
	labelWID : 0L, $
	ncolWID : 0L, $
	nrowWID : 0L, $
	panWID : 0L, $
	WID : 0L, $
	tyWID: 0L, $
	panWin : 0L, $
	win : 0L, $
	width: magnifyfactor*ncol*xdim +250,$
	height: magnifyfactor*nrow*ydim,$
	xdr: 0, $
	xsize : 400, $
	ysize : 600, $
	xdim : xdim, $
	ydim : ydim, $
	def : def, $
	images : image_array, $
	vmin: vmin, $
	vmax: vmax, $
	ncol: ncol, $
	nrow: nrow, $
	minpt: 0, $  ; 0 - show min pt, 1 - not show min, use next color number
	pixels: magnifyfactor, $
	pick_string: pick_string, $
	pick: pick, $
	selects : total(pick), $
	discrete : 0, $
	dcol: dcol, $
	vcol: vcol, $
	tbl_bg:128, $
	tbl_ratio:8, $
	dname: ['D1','D2','D3','D4','D5','D6','D7','D8','D9','DA','DB','DC','DD','DE','DF'] $
	}

if keyword_set(discrete) then begin
	 overlay_state.discrete=discrete ;1-discrete 2-composite 
;	overlay_state.height=magnifyfactor*ydim*10 ; overlay_state.selects
	end
if keyword_set(fdname) then begin
	overlay_state.dname = '???'
	for i=0,n_elements(fdname)-1 do begin
		overlay_state.dname(i)=fdname(i)
	end
end

END


PRO overlayImage,overlay_state
	overlayImage_draw1,overlay_state
	overlayImage_draw2,overlay_state
END


PRO overlayImage_draw1,overlay_state

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
	minpt= overlay_state.minpt

	if !d.name eq 'X' then WSET,overlay_state.panWin
	erase
	for k=0,14 do begin
	ki = k mod 8
	kj = k / 8
	if def(k) eq 1 then begin
		if vmax(k) gt vmin(k) then begin
		factor = (image_array(*,*,k) - vmin(k))/(vmax(k)-vmin(k)) 
;		kcolor = vcol(k)-dcol*factor + 1
		kcolor = vcol(k+1)+ceil(dcol*factor)
		if minpt then begin
		pt = where(factor lt 0.0001) 
		kcolor(pt)=kcolor(pt)+1
		end
		endif else begin
		kcolor = make_array(xdim,ydim,value=vcol(k))
		end
		x0 = ki * 61
		y0 = (1-kj) * 61 
		tv,congrid(kcolor,61,61),x0,y0
	end
	end
	
	if !d.name eq 'X' then WSET,overlay_state.win	

END





PRO overlayImage_draw2,overlay_state
widget_control,/hourglass
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
	minpt= overlay_state.minpt

	; reset ncol for overlay mode
	if discrete eq 0 and ncol le 1 then begin
		overlay_state.ncol=2
		overlay_state.nrow=2
		widget_control,overlay_state.ncolWID,set_value=2
		widget_control,overlay_state.nrowWID,set_value=2
		ncol = 2
		nrow = 2
	end

	; resize the drawing area

	CASE discrete OF
	0: begin
	  wd = overlay_state.ncol * overlay_state.xdim *overlay_state.pixels
	  ht = overlay_state.nrow * overlay_state.ydim *overlay_state.pixels
	end
	1: begin
	  wd = overlay_state.pixels*overlay_state.ncol*overlay_state.xdim+250 
	  ht = overlay_state.pixels*overlay_state.ydim*overlay_state.selects
	end
	2: begin
	  wd = overlay_state.xdim *overlay_state.pixels + 250
	  ht = overlay_state.ydim *overlay_state.pixels
	end
	ENDCASE

	overlay_state.height = fix(ht+1)
	overlay_state.width = fix(wd+1)
	WIDGET_CONTROL,overlay_state.WID,draw_xsize=overlay_state.width, $
		draw_ysize=overlay_state.height

	if discrete lt 2 then $
	im = make_array(overlay_state.width,overlay_state.height)

	if !d.name eq 'X' then WSET,overlay_state.win	
	erase

	if discrete eq 0 then begin
	  xyouts,5,overlay_state.height-50,charsize=2, $
		'GENERATING OVERLAY IMAGE  ...',/device
	  kk=0
	  for k=0,14 do begin
	  IF def(k) EQ 1 and pick(k) gt 0 THEN BEGIN
		for j=0,ydim-1 do begin
		jj = j * nrow

		for i=0,xdim-1 do begin
		ii = i * ncol

		factor = 0. ; 1.
		if vmax(k) gt vmin(k) then begin
			factor = (float(image_array(i,j,k)) - vmin(k)) / (vmax(k)-vmin(k)) 
;			kcolor = vcol(k) - fix(dcol*(1 - factor))
		kcolor = vcol(k+1)+ceil(dcol*factor)
		if minpt then begin
			if factor le 0. then kcolor = vcol(k+1)+1 
			end
		endif else begin
			kcolor = vcol(k)
		end

		ki = kk mod ncol
		kj = kk / ncol	
		x0=(ii+ki)*magnifyfactor
		x1=(ii+ki+1)*magnifyfactor
		y0=(jj+kj)*magnifyfactor
		y1=(jj+kj+1)*magnifyfactor
;		box,x0,y0,x1,y1,kcolor
catch,error_status
if error_status ne 0 then begin
	print,kk,i,j,k,ki,kj,x0,x1,y0,y1,magnifyfactor
	help,im
return
end
		im(x0:x1-1,y0:y1-1) = kcolor
		end
		end
		kk=kk+1
	  END
	  end
	  tv,im
	end
;
; plot discrete selected detectors with color scheme
;

	if discrete eq 1 then begin
	xyouts,5,overlay_state.height-50,charsize=2, $
		'GENERATING DISCRETE IMAGES  ...',/device
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

		factor = 0 
		if vmax(k) gt vmin(k) then begin 
			factor = (image_array(i,j,k) - vmin(k)) / (vmax(k)-vmin(k)) 
; 		kcolor = vcol(k) - dcol*factor
		kcolor = vcol(k+1)+ceil(dcol*factor)
		if minpt then begin
			if factor le 0. then kcolor = vcol(k+1)+1 
			end
		endif else begin
			kcolor = vcol(k)
		end
		x0=(ii+ki)*magnifyfactor
		x1=(ii+ki+1)*magnifyfactor
		y0= kj0+jj*magnifyfactor
		y1= y0 + magnifyfactor
;		box,x0,y0,x1,y1,kcolor
		im(x0:x1-1,y0:y1-1) = kcolor
		end
		end
		numj = numj+1
		END
	end
	tv,im
	;  write the detector info here min,max,detector
	for k=0,14 do begin
	if pick(k) gt 0 then begin
		kj0 = overlay_state.height - k*ydim*magnifyfactor
		str = overlay_state.dname(k) + ', Max='+strtrim(vmax(k),2) 
		xyouts,x1,kj0-ydim/2*magnifyfactor,str,charsize=1.5,/device
		str = '    , Min='+strtrim(vmin(k),2)
		xyouts,x1,kj0-15-ydim/2*magnifyfactor,str,charsize=1.5,/device
		end
	end
	end

	if discrete eq 2 then begin
	t_im = make_array(xdim,ydim)
	for k=0,14 do begin
	if pick(k) eq 1 then begin
	   for j=0,ydim-1 do begin
	     for i=0,xdim-1 do begin
		t_im(i,j) = t_im(i,j)+ image_array(i,j,k) 
	     end
	   end
	end
	end
	col_range = overlay_state.dcol * overlay_state.selects
	col_start = overlay_state.vcol(0)
	factor = col_range * (max(t_im)-t_im)/(max(t_im)-min(t_im)) 
	factor = col_start - fix(factor)
	x1 = xdim*magnifyfactor
	y1 = ydim*magnifyfactor
	factor = congrid(factor,x1,y1)
	tv,factor
		str = 'Sum: '+ ', Max='+strtrim(max(t_im),2) 
		xyouts,x1,y1/2,str,charsize=1.5,/device
		str = '      , Min='+strtrim(min(t_im),2)
		xyouts,x1,y1/2-15,str,charsize=1.5,/device
; write xdr output
	if overlay_state.xdr then begin
		xdr_open,unit,'ovl.xdr',/write
		xdr_write,unit,t_im
		xdr_write,unit,[0,xdim-1,0,ydim-1,min(t_im),max(t_im)]
		xdr_close,unit
	end
	end
END







PRO SCAN2D_OVERLAYIMAGE,overlay_state, GROUP=Group

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }
  xsize =overlay_state.width
  ysize =overlay_state.height

  SCAN2D_OVERLAY = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, TITLE='Overlay 2D Image (R1.0)', $
      MAP=1, $
      UVALUE='SCAN2D_OVERLAY')

  BASE2 = WIDGET_BASE(SCAN2D_OVERLAY, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  DRAW1 = WIDGET_DRAW( BASE2, $
      RETAIN=1, $
      UVALUE='scan2d_overlay_panimage', $
      XSIZE=61*8, $ ;15, $
      YSIZE=61*2 ) ;, $
;      X_SCROLL_SIZE=400, $
;      Y_SCROLL_SIZE=400)

  BASE2_2 = WIDGET_BASE(BASE2, $
      COLUMN=1, /frame, $
      MAP=1, $
      UVALUE='BASE2_2')
  str =''
  for i=0,N_ELEMENTS(overlay_state.dname)-1 do begin
  str = str + overlay_state.dname(i) 
  if (i+1) lt N_ELEMENTS(overlay_state.dname) then str=str+','
  end
  label2 = WIDGET_LABEL(BASE2_2,VALUE=str)

  BASE2_3 = WIDGET_BASE(BASE2_2, $
      ROW=1, /frame, $
      MAP=1, $
      UVALUE='BASE2_3')
  BGROUP4 = CW_BGROUP( BASE2_3, ['Overlay','Discrete','Composite'], $
      ROW=1, /frame, $ 
      EXCLUSIVE=1, /NO_RELEASE, $
      LABEL_LEFT='Type:', $
      UVALUE='OVERLAY_TYPE')
  WIDGET_CONTROL,BGROUP4,SET_VALUE=overlay_state.discrete

  BGROUP43 = CW_BGROUP( BASE2_3, ['No','Yes'], $
      ROW=1, /frame, $
      EXCLUSIVE=1, /NO_RELEASE, $
      LABEL_LEFT='Xdr:', $
      UVALUE='OVERLAY_XDR')
  WIDGET_CONTROL,BGROUP43,SET_VALUE=overlay_state.xdr

  BGROUP44 = CW_BGROUP( BASE2_3, ['On','Off'], $
      ROW=1, /frame, $
      EXCLUSIVE=1, /NO_RELEASE, $
      LABEL_LEFT='Contrast:', $
      UVALUE='OVERLAY_SHOWMINPT')
  WIDGET_CONTROL,BGROUP44,SET_VALUE=overlay_state.minpt

  selects_FIELD3 = CW_FIELD( BASE2_2,VALUE=overlay_state.pick_string, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Image Seq #', $
      UVALUE='OVERLAY_SELECTS', $
      XSIZE=50)

  pick = where(overlay_state.pick gt 0)
  str ='SELECTED:   '
  for i=0,N_ELEMENTS(pick)-1 do begin
  str = str + overlay_state.dname(pick(i)) 
  if (i+1) lt N_ELEMENTS(pick) then str=str+','
  end
  label3 = WIDGET_LABEL(BASE2_2,VALUE=str,/align_left)

  BASE2_1 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2_1')

  BASE3  = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE3')

  DRAW3 = WIDGET_DRAW( BASE3, $
      RETAIN=1, $
      UVALUE='scan2d_overlay', $
      XSIZE=xsize, $
      X_SCROLL_SIZE=400, $
      YSIZE=ysize, $
      Y_SCROLL_SIZE=400)

  pixels_FIELD2 = CW_FIELD( BASE2_1,VALUE=overlay_state.pixels, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Pixel Sz', $
      UVALUE='OVERLAY_PIXELS', $
      XSIZE=2)

;if overlay_state.discrete eq 0 then begin
  Ncol_FIELD3 = CW_FIELD( BASE2_1,VALUE=overlay_state.ncol, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Col', $
      UVALUE='OVERLAY_NX', $
      XSIZE=2)
  overlay_state.ncolWID = Ncol_FIELD3

  Nrow_FIELD4 = CW_FIELD( BASE2_1,VALUE=overlay_state.nrow, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Row', $
      UVALUE='OVERLAY_NY', $
      XSIZE=2)
  overlay_state.nrowWID = Nrow_FIELD4
;end

  BASE4 = WIDGET_BASE(BASE2_1, $
      ROW=1, /Frame, $
      MAP=1, $
      UVALUE='BASE4')

  label1 = WIDGET_LABEL(BASE4,VALUE='Color Bg:')
  mycolor_bg = WIDGET_SLIDER( BASE4, $
      MINIMUM=0,MAXIMUM=256,UVALUE='OVERLAYTABLE_BG', $
      XSIZE=50,VALUE=overlay_state.tbl_bg,/scroll)
  label1 = WIDGET_LABEL(BASE4,VALUE='Fg:')
  mycolor_ratio = WIDGET_SLIDER( BASE4, $
      MINIMUM=2,MAXIMUM=64,UVALUE='OVERLAYTABLE_RATIO', $
      XSIZE=50,VALUE=overlay_state.tbl_ratio,/scroll)
  BUTTON8 = WIDGET_BUTTON( BASE4, $
      UVALUE='BUTTON8', $
      VALUE='Default')
  overlay_state.bg_sdr = mycolor_bg
  overlay_state.ratio_sdr = mycolor_ratio

  BASE5 = WIDGET_BASE(BASE3, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE5')


  BUTTON71 = WIDGET_BUTTON( BASE5, $
      UVALUE='OVERLAY_SAVE_PVTCT', $
      VALUE='Save PvtColorT')

  BUTTON72 = WIDGET_BUTTON( BASE5, $
      UVALUE='OVERLAY_LOAD_PVTCT', $
      VALUE='Load PvtColorT')

  BUTTON7 = WIDGET_BUTTON( BASE5, $
      UVALUE='BUTTON7', $
      VALUE='Color ...')

  BUTTON9 = WIDGET_BUTTON( BASE5, $
      UVALUE='BUTTON9', $
      VALUE='Print')
  BUTTON10 = WIDGET_BUTTON( BASE5, $
      UVALUE='BUTTON10', $
      VALUE='Printer...')
  BUTTON11 = WIDGET_BUTTON( BASE5, $
      UVALUE='BUTTON11', $
      VALUE='Help...')

  BUTTON6 = WIDGET_BUTTON( BASE5, $
      UVALUE='BUTTON6', $
      VALUE='Done')


  g_tlb = WIDGET_INFO(SCAN2D_OVERLAY,/geometry)

  WIDGET_CONTROL, SCAN2D_OVERLAY, /REALIZE

  ; Get drawable window index

;  COMMON DRAW1_Comm, DRAW1_overlayId
  WIDGET_CONTROL, DRAW1, GET_VALUE=DRAW1_overlayId
;  COMMON DRAW3_Comm, DRAW3_overlayId
  WIDGET_CONTROL, DRAW3, GET_VALUE=DRAW3_overlayId

	overlay_state.base = SCAN2D_OVERLAY
	overlay_state.panWID = DRAW1
	overlay_state.WID = DRAW3
	overlay_state.labelWID = label3
	overlay_state.tyWID = BGROUP4
	overlay_state.panWin = DRAW1_overlayId
	overlay_state.win = DRAW3_overlayId
	overlay_state.xsize = g_tlb.scr_xsize
	overlay_state.ysize = g_tlb.scr_ysize

  overlayImage,overlay_state

  WIDGET_CONTROL,SCAN2D_OVERLAY,SET_UVALUE=overlay_state
  XMANAGER, 'SCAN2D_OVERLAY', SCAN2D_OVERLAY,/NO_BLOCK
END


PRO scanSee::Overlay2D,scanno,row=row,col=col,pixels=pixels,selects=selects,discrete=discrete
;+
; NAME:
;	scanSee::Overlay2D
;
; PURPOSE:
;       Using an overlay composite image reveals information about the 
;       superposition of the selected images of detectors. It provides 
;       another way of data displaying.
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
;       Each detector has a fixed color table associated with it, it is linearly
;       divided into 16 levels. The detector value is linearly interpreted 
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
;  COL:      Specifies the number of squares in the composite area , default 2
;  ROW:      Specifies the number of squares in the composite area , default 2
;  PIXELS:   Specifies the number of pixels used for each square, default 2
;  SELECTS:  Specifies the list of selected detectors, default [1,2]
;  DISCRETE: Draw each selected image seperately with min and max info
;
; RESTRICTION:
;  The postscript "Print" button read the TV sceen and generate the PS output.
;  The PS output only good for the screen size within  750x750 pixels.
;
;  Color table is devided into 15 sub color table schemes. 
;
;  Only 15 images can be passed into the scan2d_overlayImage program, a user
;  has to select the desired sub-list images from the 85 detector list first.
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
;  The 'Reset' button let user switch back to default overlay image color table.
;  There are two sliders which allows the user to control the myColor table.
;  The saved 'overlay.tbl' file is used for storing the current myColor table.
; 
; EXAMPLES:
;    The 2D image file is '/home/beams/CHA/data/xxx/cha_0001.mda'
;
;    Example 1 - Read in the MDA 2D files generated by the IOC, and call
;    the 2D overlay2D method for the file. Initailly user has to select
;    the interested detectors (among the 85 detectors) at most 15 detectors 
;    can be picked from the scroll list. 
;    Then the 2D image overlay program pops up. 
;    The panImage method shows all detectors images for the 2D scan.
;    
;    The object v2 need to be defined only if it is not yet defined.
;
;       filename='/home/beams/CHA/data/xxx/cha_0001.mda' 
;       v2 = obj_new('scanSee',file=filename)
;       v2->panimage,/sel
;       v2->overlay2d
;
;    Example 2 - Instead of overlaying the image discrete images are
;    displaied.
;
;       v2->overlay2d,/discrete
;
;    Toggle the 'myColor' and 'Color ...' buttons from the Overlay window
;    to access various color map.
;
;    Example 3 - Plot detector 9's discrete image (pixel scaled by 4) 
;
;	v2->overlay2d,pixels=4,selects=9,/discrete
;
;    Example 4 - Plot discrete images of detectors 8,9,10 (pixel scaled by 4) 
;
;	v2->overlay2d,pixels=4,selects=[8,9,10],/discrete
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jan 19, 1998.
;	01-08-02      Add Print, Printer... button
;                     Add slider control on myColor table used
;                     Reset myColor will set myColor slider to bg=128, ratio=8
;-

seq = self.scanno
if n_elements(scanno) then seq = scanno 

if self.dim le 1 then begin
	res=WIDGET_MESSAGE('Error: no image file loaded in')
	return
end
	; extract read mda data in

        da2d = *(*self.gD).da2D
        id_def= *(*self.gD).id_def
	if self.dim eq 3 then def = id_def(4:88,1) else $
		def = id_def(4:88,0)	

	self->images,/panimage
;	panimage,da2d,def,detnm=self.detname

  	dname = self.detname
	overlayInitSelect,dname

	found = findfile('selects.dat',count=ct)
	if ct then begin
	xdr_open,unit,'selects.dat'
	xdr_read,unit,init_selects
 	xdr_close,unit
	if n_elements(init_selects) gt 15 then init_selects = init_selects(0:14)
	endif else begin
		init_selects = indgen(15)+15
	end

fdname = self.detname(init_selects)
;print,'init_selects=',init_selects
;print,fdname
;print,init_selects
;help,id_def,da2d
xdim = 60 
ydim = 60 

	image_array  = make_array(xdim,ydim,15,/float)
	def = make_array(15,value=0)

	vmin = make_array(15,/float)
	vmax = make_array(15,/float)
	for i=0,n_elements(init_selects)-1 do begin
		def(i) = 1
		if self.dim eq 3 then di = id_def(init_selects(i)+4,1) else $
		di = id_def(init_selects(i)+4,0)
		if di gt 0 then begin
		t_image = da2d(*,*,init_selects(i))
		image_array(*,*,i) = congrid(t_image,xdim,ydim)
		rmax = max(t_image,min=rmin)
		vmax(i) = rmax
		vmin(i) = rmin
		end
	end

;print,'def:', def
;print,'vmin:',vmin
;print,'vmax:',vmax
	
update:
	
	overlayInitState,overlay_state,image_array,def,vmax,vmin,col=col,row=row,pixels=pixels,selects=selects,discrete=discrete  ,fdname=fdname

	SCAN2D_OVERLAYIMAGE,overlay_state
END

PRO scanSee::Dnames,detname
	detname = self.detname
END

PRO overlay2DImages,image_array,def,vmax,vmin,col=col,row=row,pixels=pixels,selects=selects,detnm=detnm,discrete=discrete
;+
; NAME:
;	OVERLAY2DIMAGES	
;
; PURPOSE:
;       This program overlays a selected list of images from an 
;       input image_array and displays them as an expanded composite image 
;       representation. At most 15 images can be selected from the 
;       input image_array. 
;
; CALLING SEQUENCE:
;       Overlay2DImages,Image_array [,Def] [,Vmax,Vmin] [,COL=col] [,Row=row] $
;		[,Pixels=pixels] [,Selects=selects] [,Detnm=detnm] $
;		[,Discrete=discrete]
;
; INPUT: 
; 	IMAGE_ARRAY(width,height,nd)  
;                -  'nd' the number of images (>15) 
;                    Initial 2D image array up to 85 2D images
;                    Each image contains 'width x height' values
; 	DEF(nd)  -  Initial Image presence indicators for image_array 
;                    0 not defined, 1 defined, default all defined
;
; OUTPUT:
;	VMAX(15)  -  Maximum value of selected detector images
;	VMIN(15)  -  Minimum value of selected detector images
;
; KEYWORD:
; 	COL         -  Columns of dectors in overlay image composition
; 	ROW         -  Rows of detectors in overlay image compositon
; 	PIXELS      -  Pixel width and height in a unit element of image 
; 	SELECTS(15) -  List of sequence number of selected detectors
;                      At most 15 detectors can be selected
;		       defaults [16,17,...,31]
;	DETNM(15)   -  List of selected Detector names assigned
;                      At most 15 detectors can be selected
;                      defaults ['D01,'D02',...,'D15']
; 	DISCRETE    -  Discrete images instead of overlaying images
; 
; RESTRICTIONS
;   The resultant composite image with new pixel dimension width and height
;   as given  
;              (60 x pixels x col , 60 x pixels x row)
;
; EXAMPLE:
;     file='/home/spare/sector2/vx2id/2xfm/data01Q4/MarineBio/mda/2xfm_0249.mda'
;     v1 = obj_new('scanSee',file=file)
;     v1->images,image_array,def
;	overlay2DImages,image_array,def,selects=[16,17,18,19,20]
;-

	sz = size(image_array)
	if sz(0) ne 3 then return
	if n_elements(def) eq 0 then def=make_array(sz(3),value=1)

	detname =  'D'+ [strtrim(indgen(9)+1,2),'A','B','C','D','E','F' , $
                '01','02','03','04','05','06','07','08','09', $
                strtrim(indgen(61)+10,2)]
	if keyword_set(selects) eq 0 then selects=indgen(15)+16 
	t_detnm = detname(selects-1)

	if keyword_set(detnm) then begin
		for i=0,n_elements(detnm)-1 do begin
		t_detnm(i) = detnm(i)
		end
	end
	detnm = t_detnm

	; at most 15 detectors will be selected
	xwid=60
	ywid=60
	t_image = make_array(xwid,ywid,15)
	id_def = make_array(15,/int)
	vmax = make_array(15)
	vmin = make_array(15)
	dname = make_array(15,value='???')
	ip=0

	for k=0,n_elements(selects)-1 do begin
 	i = selects(k) - 1 
	  if def(i) gt 0 then begin
		image = image_array(*,*,i)
		vmax(ip) = max(image)
		vmin(ip) = min(image)
		t_image(*,*,ip) = congrid(image,xwid,ywid)
		id_def(ip) = 1
		dname(ip) = detnm(ip)
		ip = ip+1
		if ip ge 15 then goto,overlay2
	  end
	;if k eq 0 then n_selects=1 else n_selects=[n_selects,k+1]
	end

overlay2:
	overlayInitState,overlay_state,t_image,id_def,vmax,vmin,col=col,row=row,pixels=pixels,discrete=discrete,fdname=dname

;	panImage,t_image,id_def,detnm=dname

	SCAN2D_OVERLAYIMAGE,overlay_state

END


PRO overlayInitSelect_Event, Event


  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 
  'INIT_CLOSE': BEGIN
        WIDGET_CONTROL, Event.top, GET_UVALUE= overlayInitSelect_state
	res = WIDGET_INFO(overlayInitSelect_state.listWID,/LIST_SELECT)
	xdr_open,unit,'selects.dat',/write
	xdr_write,unit,res
	xdr_close,unit
	WIDGET_CONTROL,Event.top,/destroy
      END

  'INIT_HELP': BEGIN
  str = ['All selected items are highlighted. Pick items from the scroll list by ', $
	'locate the desired item and click the left mouse button (LMB).', $
	'Only the first 15 items from the user selected list will be accepted', $
	'by the 2D images overlay program.', '', $
	'CNTL + LMB    - Add/remove the item from the selected list', $
	'SHIFT + LMB   - Select items between the previous and current clicks', $
	'Accept Button - Accept all selections and call Image Overlay program']
	res = dialog_message(str,/info)
      END

  'OVERLAY_INITLIST': BEGIN
      END
  ENDCASE
END





PRO overlayInitSelect,dname
if n_elements(dname) eq 0 then begin
	r = dialog_message('The initial detector name list is required!!',/error)
	return
end

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

	found = findfile('selects.dat',count=ct)
	if ct then begin
	xdr_open,unit,'selects.dat'
	xdr_read,unit,init_selects
 	xdr_close,unit
	if n_elements(init_selects) gt 15 then init_selects = init_selects(0:14)
	endif else begin
		init_selects = indgen(15)+15
	end

  overlayInitSelect = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1,   $
      TITLE='Overlay2D ', $
      UVALUE='overlayInitSelect')

  LABEL2 = WIDGET_LABEL( overlayInitSelect, $
      UVALUE='LABEL2', $
      VALUE='Image Selections ')

  LIST3 = WIDGET_LIST( overlayInitSelect,VALUE=dname, $
      UVALUE='OVERLAY_INITLIST', /multiple, $
      YSIZE=10)
  WIDGET_CONTROL,LIST3,SET_LIST_TOP=15
  WIDGET_CONTROL,LIST3,SET_LIST_SELECT=init_selects 

  BASE2 = WIDGET_BASE(overlayInitSelect,/ROW)
  button = WIDGET_BUTTON(BASE2,value="Help...",UVALUE='INIT_HELP')
  button = WIDGET_BUTTON(BASE2,value="Accept",UVALUE='INIT_CLOSE')

  overlayInitSelect_state = {  listWID: LIST3 }

  WIDGET_CONTROL, overlayInitSelect, SET_UVALUE= overlayInitSelect_state
  WIDGET_CONTROL, overlayInitSelect, /REALIZE

  XMANAGER, 'overlayInitSelect', overlayInitSelect
END

PRO scan2d_overlay_selectFiles,xdrfile_array,nfile=nfile,path=path
;+
; PROGRAM:
;	SCAN2D_OVERLAY_SELECTFILES
;
; PURPOSE:
;	Use file selection dialog to select a set of XDR files which should
;	contain XDR 2D image, X,Y,Z ragnes data which are saved by the scanSee 
;	subprogram image2d 
;
; CATEGORY:
;	Widgets.
;
; CALL SEQUENCE:
;	SCAN2D_OVERLAY_SELECTFILES, XDRFILE_ARRAY [,PATH=path] [,NFILE=nfile] 
;
; OUTPUT:
;  XDRFILE_ARRAY - return an array of XDR image filenames selected from the 
;		   2D overlay file selection dialog, at least two file must be  
;		   selected
;
; KEYWORD:
; PATH         - specify the directory where xdr files exists
;                At least two xdr files must be selected for image overlaying
; NFILE        - Number of files selected from the directory 
;
; EXAMPLE:
;	SCAN2D_OVERLAY_SELECTFILES,xdrfile_array
;-

cd,current=opath
        ; read overlay.config
        fd = findfile('overlay.config',count=ct)
        if ct then begin
        openr,1,'overlay.config'
        readf,1,opath
        close,1
        catch,error_status
        if error_status ne 0 then cd,current=opath
        end

if keyword_set(path) then opath=path

xdrpick:
	xdrfile_array = dialog_pickfile(title='2D Overlay File Pick', $
		path = opath,Filter='*.xdr*', $
		get_path=p,/multiple,/must_exist,/read)
	if xdrfile_array(0) eq '' then return

	nfile = n_elements(xdrfile_array)
	if nfile le 1 then begin
		r = dialog_message('At least two XDR image files must be selected!!!',/Error)
		opath=p
		goto,xdrpick
	end
	path = p

       ; write overlay.config
        openw,1,'overlay.config'
        printf,1,path
        close,1
END



PRO scan2d_overlay_calc,xdrfile_array,image_array,readonly=readonly,Group=group
; read two xdr img & ranges and call overlay plot
; INPUT:
;  xdrfile_array  - a list of input xdr file name array < 15
; OUTPUT:
; IMAGE_ARRAY  - return the resultant image array (wd,ht,15) 
;
 
 nfile= n_elements(xdrfile_array)

 overlay_data = { name: strarr(15), $
	im: ptrarr(15,/allocate_heap), $
	ranges_def: intarr(15), $
	ranges: dblarr(6,15) $
	}


for i=0,nfile-1 do begin
	xdr_open,unit,xdrfile_array(i)
	xdr_read,unit,im1
	xdr_read,unit,ranges1,error=err
	xdr_close,unit
;	help,im1,ranges1,err
	overlay_data.name(i) = 'IM'+strtrim(i+1,2) 
	*overlay_data.im(i) = im1
	if err eq 0 then begin
	overlay_data.ranges_def(i) = 1
	overlay_data.ranges(0:5,i) = ranges1(*)
	end
end
if keyword_set(readonly) then begin
	nfile = n_elements(xdrfile_array)
	str = ['   ** INFO About XDR Files **','']
	for i=0,nfile-1 do begin
	im = *overlay_data.im(i)
	sz = size(im)
	str1 = string('Image Dim: ',strtrim(sz(1),2),' x ',strtrim(sz(2),2),/print)
	ranges = string('Ranges: ',overlay_data.ranges(*,i),/print)
	str = [str,'Filename:  '+xdrfile_array(i),str1,ranges,'']
	end
	xdisplayfile,text=str,Group=group,title='XDR Files Info'
	return
end

	im1 = *overlay_data.im(0)
	sz1 = size(im1)

	;  calculate dx, dy 

;	if n_elements(ranges1) eq 0 then ranges1 = [0.,sz1(1)-1.,0.,sz1(2)-1.]
	if overlay_data.ranges_def(0) eq 0 then $
	 ranges1 = [-0.5*(sz1(1)-1.), 0.5*(sz1(1)-1.), $
			-0.5*(sz1(2)-1.), 0.5*(sz1(2)-1.)] else $
	ranges = overlay_data.ranges(*,0)
	dx = (ranges1(1)-ranges1(0))/(sz1(1)-1)
	dy = (ranges1(3)-ranges1(2))/(sz1(2)-1)

	for i=0,nfile-1 do begin
	im2 = *overlay_data.im(i)
	sz2 = size(im2)
	
;	if n_elements(ranges2) eq 0 then ranges2 = [0.,sz2(1)-1.,0.,sz2(2)-1.]
	if overlay_data.ranges_def(i) eq 0 then $
	 ranges2 = [-0.5*(sz2(1)-1.), 0.5*(sz2(1)-1.), $
			-0.5*(sz2(2)-1.), 0.5*(sz2(2)-1.)] else $
	ranges2 = overlay_data.ranges(*,i)
	dx2 = (ranges2(1)-ranges2(0))/(sz2(1)-1)
	dy2 = (ranges2(3)-ranges2(2))/(sz2(2)-1)


	; change to same mesh size

	if dx2 lt dx then dx = dx2
	if dy2 lt dy then dy = dy2

	if ranges2(0) lt ranges(0) then ranges(0) = ranges2(0)
	if ranges2(1) gt ranges(1) then ranges(1) = ranges2(1)
	if ranges2(2) lt ranges(2) then ranges(2) = ranges2(2)
	if ranges2(3) gt ranges(3) then ranges(3) = ranges2(3)

	end

	wd = fix((ranges(1)-ranges(0))/dx) + 1
	ht = fix((ranges(3)-ranges(2))/dy) + 1
	num_image = nfile 
	image_array = make_array(wd,ht,15)

	for i=0,nfile-1 do begin
	im1 = *overlay_data.im(i)
	ranges1 = overlay_data.ranges(*,i)
	nx1 = fix((ranges1(1)-ranges1(0))/dx) + 1
	ny1 = fix((ranges1(3)-ranges1(2))/dy) + 1
	im1 = congrid(im1,nx1,ny1)

	sz1 = size(im1)
	nx1 = sz1(1)
	ny1 = sz1(2)

	if ranges1(0) eq ranges(0) and ranges1(2) eq ranges(2) then begin
		image_array(0:nx1-1,0:ny1-1,i) = im1(*,*)
	end
	if ranges1(0) gt ranges(0) and ranges1(2) gt ranges(2) then begin
		ixs = fix((ranges1(0)-ranges(0))/dx)
		iys = fix((ranges1(2)-ranges(2))/dy)
		image_array(ixs:ixs+nx1-1,iys:iys+ny1-1,i) = im1(*,*)
	end
	if ranges1(0) eq ranges(0) and ranges1(2) gt ranges(2) then begin
		iys = fix((ranges1(2)-ranges(2))/dy)
		image_array(0:nx1-1,iys:iys+ny1-1,i) = im1(*,*)
	end
	if ranges1(0) gt ranges(0) and ranges1(2) eq ranges(2) then begin
		ixs = fix((ranges1(0)-ranges(0))/dx)
		image_array(ixs:ixs+nx1-1,0:ny1-1,i) = im1(*,*)
	end
	end

	vmax = make_array(15)
	vmin = make_array(15)
	def = intarr(15)
	selects = indgen(nfile)+1 

	for i=0,num_image-1 do begin
		vmax(i)= max(image_array(*,*,i))
		vmin(i)= min(image_array(*,*,i))
		def(i)=1
	end


	; call overlayinit state

	if wd gt 200 or ht gt 200 then  $
		image_array = congrid(image_array,201,201,15)

	fdname=overlay_data.name(0:nfile-1)
	overlayInitState,overlay_state,image_array,def,vmax,vmin, $
		pixels=1,discrete=2,selects=selects,fdname=fdname

	for i=0,nfile-1 do begin
	if ptr_valid(overlay_data.im(i)) then $
	ptr_free,overlay_data.im(i)
	end 
	heap_gc,/ptr

	scan2d_overlayImage,overlay_state


END


;-----------------------------------------------------------------
pro scan2d_overlay_onRecall, Event

	wWidget =  Event.top
	textWID = Widget_Info(wWidget, FIND_BY_UNAME='W_TEXT_11')
	widget_control,textWID,get_value=xdrfile_array

	if xdrfile_array(0) eq '' then begin
		r = dialog_message('You have to load the xdr files in first.',/error)
		return 
	end
	scan2d_overlay_calc,xdrfile_array,image_array
end

;-----------------------------------------------------------------
pro scan2d_Overlay_onOpen, Event

	widget_control,Event.top,get_uvalue=overlay_path
	scan2d_overlay_selectFiles,xdrfile_array,nfile=nfile,path=overlay_path.path
	if xdrfile_array(0) eq '' then return 

	wWidget =  Event.top
	textWID = Widget_Info(wWidget, FIND_BY_UNAME='W_TEXT_11')
	widget_control,textWID,set_value=xdrfile_array

	scan2d_overlay_calc,xdrfile_array,image_array
end

;-----------------------------------------------------------------
pro scan2d_overlay_onClose, Event
	widget_control,Event.top,/destroy
end
;-----------------------------------------------------------------
pro scan2d_overlay_onHelp, Event
	str = ['The multiple file selection is accomplished by clicking the',$
	'file with the Left Mouse Button (LMB) in the file selection box:','', $
	'       LMB       - select only the picked file', $
	'       CNTL-LMB  - add the picked file to the selection list', $
	'       SHIFT-LMB - add all the files between the last two click files', $
	'                   to the selection list', $
	'Press the OK or Open button accept all the fiels selected', $
	'']
	r = dialog_message(str,/info)
end
;-----------------------------------------------------------------
pro scan2d_overlay_onXDRFile, Event
	wWidget =  Event.top
	textWID = Widget_Info(wWidget, FIND_BY_UNAME='W_TEXT_11')
	widget_control,textWID,get_value=xdrfile_array

	if xdrfile_array(0) eq '' then begin
		r = dialog_message('You have to load the xdr files in first.',/error)
		return 
	end
	scan2d_overlay_calc,xdrfile_array,image_array,/readonly,Group=Event.top
end
;-----------------------------------------------------------------

pro SCAN2D_OVERLAY_BASE_1_event, Event
if !d.name eq 'X' then device,decomposed=0

  wWidget =  Event.top

  case Event.id of

    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_3'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        scan2d_Overlay_onOpen, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_4'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        scan2d_overlay_onClose, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_5'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        scan2d_overlay_onHelp, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_6'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        scan2d_overlay_onXDRFile, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_21'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        scan2d_overlay_onRecall, Event
    end
    else:
  endcase

end

pro SCAN2D_OVERLAY_BASE_1,path=path, GROUP=wGroup, _EXTRA=_VWBExtra_

  SCAN2D_OVERLAY_BASE_1 = Widget_Base( GROUP_LEADER=wGroup, UNAME='SCAN2D_OVERLAY_BASE_1'  $
	,TITLE='IMAGE_OVERLAY' $
      ,XOFFSET=5 ,YOFFSET=5  $ ;,SCR_XSIZE=300 ,SCR_YSIZE=219  $
      ,SPACE=3 ,XPAD=3 ,YPAD=3 ,MBAR=SCAN2D_OVERLAY_BASE_1_MBAR)

  
  W_MENU_0 = Widget_Button(SCAN2D_OVERLAY_BASE_1_MBAR, UNAME='W_MENU_0' ,/MENU  $
      ,VALUE='File')

  
  W_MENU_3 = Widget_Button(W_MENU_0, UNAME='W_MENU_3'  $
      ,VALUE='Open...')

  W_MENU_21 = Widget_Button(W_MENU_0, UNAME='W_MENU_21' , /separator, $
      VALUE='Re-run Overlay...')

  
  W_MENU_4 = Widget_Button(W_MENU_0, UNAME='W_MENU_4' ,/separator, VALUE='Close')
  
  W_MENU_1 = Widget_Button(SCAN2D_OVERLAY_BASE_1_MBAR, UNAME='W_MENU_1' ,/MENU  $
      ,VALUE='Help')

  
  W_MENU_5 = Widget_Button(W_MENU_1, UNAME='W_MENU_5'  $
      ,VALUE='Help...')

  
  W_MENU_6 = Widget_Button(W_MENU_1, UNAME='W_MENU_6'  $
      ,VALUE='xdrFile...')

  W_TEXT_11 = widget_text(SCAN2D_OVERLAY_BASE_1,value=strarr(15), $
	UNAME='W_TEXT_11',xsize=50,ysize=10,/scroll)

  overlay_path = { path:path}
  widget_control,SCAN2D_OVERLAY_BASE_1,set_uvalue=overlay_path

  Widget_Control, /REALIZE, SCAN2D_OVERLAY_BASE_1

  XManager, 'SCAN2D_OVERLAY_BASE_1', SCAN2D_OVERLAY_BASE_1, /NO_BLOCK  

end
; 
; Empty stub procedure used for autoloading.
; 
pro scan2d_overlay,path=path, GROUP=Group, _EXTRA=_VWBExtra_
;+
; NAME:
;	SCAN2D_OVERLAY
;
; PURPOSE:
; 	Using an overlay composite image reveals information about the
;	superposition of the selected images of XDR 2D image files.
;
;	The region of each 2D image may be different sizes.
;
;	It allows the user to select multiple files from a list of 2D XDR 
;	files each contains a 2D image and the corresponding X,Y,Value ranges, 
;	then convert the selected images into same geometric scale, and  then
;	costruct the display image as according to overlay, discrete, or 
;	superpose composite.
;
; CATEGORY:
;	Widgets.
;
; CALL SEQUENCE:
;	SCAN2D_OVERLAY [,PATH=path] [,GROUP=Group] 
;
; KEYWORDS:
;  PATH:         Specify the directory where XDR image files are located
;  GROUP:        Specify the parent widget, the destroy of parent
;                widget results the exiting of this program
;
; RESTRICTION:
;	The XDR file must be created by XDR image generation program such as
;	VIEW2D, VW2D, PLOT2D, IMAGE2D. Each image file consists of image 
;	data array and corresponding X,Y,Value ranges which is written by 
;	the XDR_WRITE routine (see xdr_open.pro)
; 
;	All the XDR files must be located in the same data directory.
;
;	At most 15 image files can be selected at one time. The color table 
;	spectrum is devided into 16 sub-tables. Each discrete image uses
;	its sub color table. A user can use the slider bars and color table
;	to manipulate the resultant image color display.
;
;	For WIN system, a user always has to redraw the iamge to reflect 
;	the new color table used.
;
; EXAMPLES:
;	SCAN2D_OVERLAY,path='/home/beams/CHA/data/xxx/XDR'
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, July 17, 2002
;	07-23-2004  bkc update drawing area when it is true color device
;-
  cd, current=p
  if keyword_set(path) then p=path
  SCAN2D_OVERLAY_BASE_1,path=p, GROUP=Group, _EXTRA=_VWBExtra_
end
