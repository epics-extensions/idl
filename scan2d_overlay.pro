


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
;    Toggle the 'myColor' and 'Color ...' buttons from the Overlay window
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
;                     Add slider control on myColor table used
;                     Reset myColor will set myColor slider to bg=128, ratio=8
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

	panImage,image_array,def,detnm=overlay_state.dname

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
;	save,red,green,blue,file='overlay.tbl'
	xdr_open,unit,'overlay.tbl',/write
	xdr_write,unit,red
	xdr_write,unit,green
	xdr_write,unit,blue
	xdr_close,unit
END

PRO scan2d_getOverlayColorTbl
;	restore,'overlay.tbl'
	xdr_open,unit,'overlay.tbl'
	xdr_read,unit,red
	xdr_read,unit,green
	xdr_read,unit,blue
	xdr_close,unit
	tvlct,red,green,blue
END

PRO scan2d_overlay_help,Event
  str = ["                 Help on Overlay 2D Image",'', $
  'List of DIs  :   List of initially picked detector images', $
  'Image seq #  :   Enter images sequence numbers seperated by comma', $
  'Pixel        :   Number of pixels used for each value point, default 2', $
  'Column       :   Columns used in overlay/splice selected image, default 2', $
  'Row          :   Rows used in overlay/splice selected image, default 2', $
  'Image Area   :   Drawing area for displaying overlaid/discrete images', $
  'myColor Control:',$
  '     "Slider1" - Control the Background Color level ', $
  '     "Slider2" - Control the number of Foreground Color number', $
  '     "Reset"   - Set slider1 to 128, and slider2 to 8', $
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
	loadct,31   ;pepermint  waves=37
	XLOADCT
      END
  'BUTTON8': BEGIN
	widget_control,overlay_state.bg_sdr,SET_VALUE=128,bad=bad
	if bad then return
	widget_control,overlay_state.ratio_sdr,SET_VALUE=8
	scan2d_setOverlayColorTbl,128,8
	SCAN2D_GETOVERLAYCOLORTBL
      END
  'BUTTON9': BEGIN
	arr = TVRD()
        sz = size(arr)
        xs = sz(1)
        ys = sz(2)
        width = float(xs)/40
        height = float(ys)/40
	scale = 1.1
	
	; if width or height exceed 18.3 gives problem

        set_plot,'PS'
        device,filename='idl.ps',/color,bits=8, $
                /Courier,/Bold, scale_factor=scale, $
                xsize=width,ysize=height
        TV,arr
        PS_close
        PS_print, 'idl.ps'
	if !d.name eq 'X' then spawn,"gv idl.ps" 
;	WIDGET_CONTROL,Event.id,SENSITIVE=0
      END
  'BUTTON10': BEGIN
	PS_printer
      END
  'BUTTON11': BEGIN
	scan2d_overlay_help,Event
      END

  'OVERLAY_NX': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ncol
	old_ncol = overlay_state.ncol
	overlay_state.ncol = ncol
;	overlay_state.nrow = 16 / overlay_state.ncol
	WIDGET_CONTROL,Event.ID,SET_VALUE=old_ncol

	overlay_replot,overlay_state
;	scan2d_overlayImage,overlay_state

      END

  'OVERLAY_NY': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=nrow
	old_nrow = overlay_state.nrow
	overlay_state.nrow = nrow
;	overlay_state.ncol = 16 / overlay_state.nrow
	WIDGET_CONTROL,Event.ID,SET_VALUE=old_nrow

	overlay_replot,overlay_state
;	scan2d_overlayImage,overlay_state

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
	
	old_string = overlay_state.pick_string
	pick_string=strcompress(detectors(0),/remove_all)
	overlay_state.pick_string = pick_string
	overlay_state.pick = pick
	overlay_state.selects = fix(total(pick))
	WIDGET_CONTROL,Event.ID,SET_VALUE=old_string

  WIDGET_CONTROL,Event.Top,SET_UVALUE=overlay_state
	overlay_replot,overlay_state
;	scan2d_overlayImage,overlay_state

      END
  'OVERLAYTABLE_BG': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=bg
	overlay_state.tbl_bg = bg
	scan2d_setOverlayColorTbl,overlay_state.tbl_bg,overlay_state.tbl_ratio
      END
  'OVERLAYTABLE_RATIO': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=v
	overlay_state.tbl_ratio = v
	scan2d_setOverlayColorTbl,overlay_state.tbl_bg,overlay_state.tbl_ratio
      END
  'OVERLAY_PIXELS': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=pixels
	old_pixels = overlay_state.pixels
	overlay_state.pixels = pixels
	WIDGET_CONTROL,Event.ID,SET_VALUE=old_pixels

	overlay_replot,overlay_state
;	scan2d_overlayImage,overlay_state

      END
  'OVERLAY_TYPE': BEGIN
	overlay_state.discrete = Event.Value
	if Event.Value eq 0 and overlay_state.ncol eq 1 then overlay_state.ncol=2
      END

  ENDCASE

  WIDGET_CONTROL,Event.Top,SET_UVALUE=overlay_state

END

PRO overlay_replot,overlay_state

	overlay_state.width = overlay_state.pixels*overlay_state.ncol*overlay_state.xdim+250 
	if overlay_state.discrete eq 0 then $
	overlay_state.height = overlay_state.pixels*overlay_state.ydim*overlay_state.nrow else $
	overlay_state.height = overlay_state.pixels*overlay_state.ydim*overlay_state.selects

	scan2d_overlayimage,overlay_state

END


PRO overlayInitState,overlay_state,image_array,def,vmax,vmin,col=col,row=row,pixels=pixels,selects=selects,discrete=discrete,fdname=fdname
	
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
	if keyword_set(discrete) then ncol = 1
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
	win : 0, $
	width: magnifyfactor*ncol*xdim +250,$
	height: magnifyfactor*nrow*ydim,$
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
	overlay_state.discrete=1  ; plot discrete images
	overlay_state.height=magnifyfactor*ydim*10 ; overlay_state.selects
	end	
if keyword_set(fdname) then begin
	overlay_state.dname = '???'
	for i=0,n_elements(fdname)-1 do begin
		overlay_state.dname(i)=fdname(i)
	end
end

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
; plot discrete selected detectors with color scheme
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
		kcolor = vcol(k) - dcol*factor
		x0=(ii+ki)*magnifyfactor
		x1=(ii+ki+1)*magnifyfactor
		y0= kj0+jj*magnifyfactor
		y1= y0 + magnifyfactor
		box,x0,y0,x1,y1,kcolor
		end
		end
		numj = numj+1
	;  write the detector info here min,max,detector
		str = overlay_state.dname(k) + ', Max='+strtrim(vmax(k),2)+ $
			', Min='+strtrim(vmin(k),2)
		xyouts,x1,kj0+ydim/2*magnifyfactor,str,/device
		END
	end
	end
END





PRO SCAN2D_OVERLAYIMAGE,overlay_state, GROUP=Group

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }
  xsize =overlay_state.width
  ysize =overlay_state.height

  SCAN2D_OVERLAY = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, TITLE='Overlay 2D Image', $
      MAP=1, $
      UVALUE='SCAN2D_OVERLAY')

  BASE2 = WIDGET_BASE(SCAN2D_OVERLAY, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

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

  BGROUP4 = CW_BGROUP( BASE2_2, ['Overlay','Discrete'], $
      ROW=1, $
      EXCLUSIVE=1, /NO_RELEASE, $
      LABEL_LEFT='Composite Type: ', $
      UVALUE='OVERLAY_TYPE')
  WIDGET_CONTROL,BGROUP4,SET_VALUE=overlay_state.discrete

  selects_FIELD3 = CW_FIELD( BASE2_2,VALUE=overlay_state.pick_string, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Image Seq #', $
      UVALUE='OVERLAY_SELECTS', $
      XSIZE=25)

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

  DRAW3 = WIDGET_DRAW( BASE2, $
      RETAIN=1, $
      UVALUE='scan2d_overlay', $
      XSIZE=xsize, $
      X_SCROLL_SIZE=400, $
      YSIZE=ysize, $
      Y_SCROLL_SIZE=400)

  BASE4 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE4')

  label1 = WIDGET_LABEL(BASE4,VALUE='myColor:')
  mycolor_bg = WIDGET_SLIDER( BASE4, $
      MINIMUM=0,MAXIMUM=256,UVALUE='OVERLAYTABLE_BG', $
      VALUE=overlay_state.tbl_bg)
  mycolor_ratio = WIDGET_SLIDER( BASE4, $
      MINIMUM=2,MAXIMUM=64,UVALUE='OVERLAYTABLE_RATIO', $
      VALUE=overlay_state.tbl_ratio)
  BUTTON8 = WIDGET_BUTTON( BASE4, $
      UVALUE='BUTTON8', $
      VALUE='Reset')
  overlay_state.bg_sdr = mycolor_bg
  overlay_state.ratio_sdr = mycolor_ratio

  BASE5 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE5')

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
;                    Each image contains 'widthxheight' pixels
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

	panImage,t_image,id_def,detnm=dname

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
