
PRO view3d_2Dslice,da3D,rank,kindex,xv,yv,zv,SLICER3=SLICER3,title=title,group=group,data=data
;+
; NAME:
;       VIEW3D_2DSLICE 
;
; PURPOSE:
;      This routine cut out a cartesian 2D slice from an arbitrary
;      input 3d array. It allows the user view the 2D slice as 
;      a 2D image with user specified rank and slice index number.
;
; CATEGORY:
;    Widgets.
;
; CALLING SEQUENCE:
;	VIEW3D_2DSLICE, Da3D [,Kindex] [,Rank] [,SLICER3=slicer3] [,TITLE=title]
;                  [,GROUP=group] [,DATA=data]
;
; ARGUMENTS:
;    Da3D:    Input 3D array to be examined
;    Kindex:  Specifies the index number of the slice, zero based number, 
;             default 0 
;    Rank:    Specifies the viewing direction rank number, 0-X axis, 
;             1-Y axis, 2-Z axis, default 2 
;
; KEYWORDS:
;  SLICER3:   Specifies whether the IDL slicer3 program will be called 
;  TITLE:     Specifies the tile for 2D plot
;  GROUP:     Specifies the widget ID of the parent group
;  DATA:      Returns the cut out 2D data array slice
;
; RESTRICTIONS:
;    The environment variables must be set by source in 
;    /usr/local/epics/extensions/bin/solaris/setup_ezcaIDL for IDL 5.1
;    /usr/local/epics/extensions/bin/solaris/ezcaidl_setup for IDL 5.3
;    All required programs will be automatically loaded into IDL by this
;    setup.
;-
	if n_elements(kindex) eq 0 then kindex = 0
	if n_elements(rank) eq 0 then rank = 2

;	shade_surf,da3d(*,*,kindex)

	if rank eq 2 then begin
		data = da3D(*,*,kindex) 
		xa = xv
		ya = yv
	end
	if rank eq 1 then begin
		data = reform(da3D(*,kindex,*)) 
		xa = xv
		ya = zv
	end
	if rank eq 0 then begin
		data = reform(da3D(kindex,*,*))
		xa = yv
		ya = zv
	end

	plot2d,data,xarr=xa,yarr=ya,title=title,group=group
	
	if keyword_set(slicer3) then begin
	slicer = ptr_new(/allocate_heap)
	*slicer = da3D(*,*,*) 
	slicer3,slicer,/MODAL
	if ptr_valid(slicer) then ptr_free,slicer
	end

END

PRO view3d_2Dpick1D,da3D,rank,k,xa=xa,ya=ya,group=group
;
	xyz = da3D
	sz = size(da3D)
	if sz(0) ne 3 then begin
		r=dialog_message('3D array required',/error)
		return
	end
	if n_elements(rank) eq 0 then rank=2
	if n_elements(k) eq 0 then k=0

	ipos = 0
	cpt = sz(1:3)

	CASE rank OF 
	0: BEGIN    ; x axis picked
	if n_elements(xa) eq 0 then xa = dindgen(sz(2))
	if n_elements(ya) eq 0 then ya = dindgen(sz(3))
	za = reform(xyz(k,*,*))
	title = ' X slice @: '+strtrim(k,2)
	END
	1: BEGIN    ; y axis picked
	if n_elements(xa) eq 0 then xa = dindgen(sz(1))
	if n_elements(ya) eq 0 then ya = dindgen(sz(3))
	za = reform(xyz(*,k,*))
	title = ' Y slice @: '+strtrim(k,2)
	END
	2: BEGIN    ; z axis picked
	if n_elements(xa) eq 0 then xa = dindgen(sz(1))
	if n_elements(ya) eq 0 then ya = dindgen(sz(2))
	za = xyz(*,*,k)
	title = ' Z slice @: '+strtrim(k,2)
	END
	ENDCASE

	if max(xa) eq min(xa) then xa = indgen(n_elements(xa))
	if max(ya) eq min(ya) then ya = indgen(n_elements(ya))
	calibra_pick1d,za,xa=xa,ya=ya,title=title,Group=group

END

PRO view3d_2Dascii,data,rank,kindex,report,px=px,py=py,title=title,format=format,group=group,outfile=outfile

	if n_elements(rank) eq 0 then rank = 2
	if rank gt 2 then rank = 2
	if n_elements(kindex) eq 0 then kindex = 0
	if kindex lt 0 then kindex = 0

	t_format = 'G18.8'
	if keyword_set(format) then t_format = format
	fwidth = 'I'+strmid(t_format,1,strpos(t_format,'.')-1)

	suf0 = 'Z_Slice'
	if rank eq 0 then suf0 = 'X_Slice'
	if rank eq 1 then suf0 = 'Y_Slice'
	file = suf0+'.slc'+strtrim(kindex,2)
	report = file

	if keyword_set(outfile) then report = strtrim(outfile,2)

	openw,fw,report,/get_lun

	s = size(data)
	dim = s(1:2)
	st ='; Pick Rank # = '+strtrim(rank,2)
	if rank eq 0 then st = st+ ' (ie X axis) '
	if rank eq 1 then st = st+ ' (ie Y axis) '
	if rank eq 2 then st = st+ ' (ie Z axis) '
	st = st +',   Slicer # ='+strtrim(kindex,2)
	printf,fw,st
	printf,fw,';   data('+strtrim(dim(0),2)+','+strtrim(dim(1),2)+')'
	printf,fw,'; -------------------------------'
		f2 = '(";         Yvalues       "'
		f0 = '(";      \ Y              #",'+strtrim(dim(1),2)+fwidth+',/,";     X \               #",/,";         Xvalues")'

		if rank eq 0 then begin
		f2 = '(";         Zvalues       "'
		f0 = '(";      \ Z              #",'+strtrim(dim(1),2)+fwidth+',/,";     Y \               #",/,";         Yvalues")'
		end
		if rank eq 1 then begin
		f2 = '(";         Zvalues       "'
$
		f0 = '(";      \ Z              #",'+strtrim(dim(1),2)+fwidth+',/,";     X \               #",/,";         Xvalues")'

		end
		f2 = f2+strtrim(dim(1),2)+'('+t_format+'))' 
		f1 = '(I,'+strtrim(dim(1),2)+'('+t_format+'))' 
		if n_elements(px) gt 0 then  $
		f1='('+t_format+',I,'+strtrim(dim(1),2)+'('+t_format+'))'


	if n_elements(py) gt 0 then  printf,fw,format=f2,py
	printf,fw,format=f0,indgen(dim(1))
	newdata = transpose(data)

	d1 = dim(1)
	d2 = dim(0)
	temp = make_array(dim(1))
	for j=0,d2-1 do begin
	temp = newdata(0:d1-1,j)
	if n_elements(px) gt 0 then printf,fw,format=f1,px(j),j,temp else $
		printf,fw,format=f1,j,temp
	end
	free_lun,fw

	if keyword_set(nowin) then return
	xdisplayfile,report,group=group
	
END

PRO view3d_2Dredisplay,state,Event

	data = *state.data
	rank = state.rank
	kindex = state.kindex
	slicer3 = state.slicer3
	title = state.title
	xv = *(state.x)
	yv = *(state.y)
	zv = *(state.z)

		suf0 = 'Z_Slice'
		if rank eq 0 then suf0 = 'X_Slice'
		if rank eq 1 then suf0 = 'Y_Slice'
		title = title +' (' + suf0+'.slc'+strtrim(kindex,2) +')'

	   sz = size(data)
		if rank eq 0 and kindex lt sz(1) then begin
			im = data(kindex,*,*)
			x = yv
			y = zv
		end
		if rank eq 1 and kindex lt sz(2) then begin
			im = data(*,kindex,*)
			x = xv
			y = zv
		end
		if rank eq 2 and kindex lt sz(3) then begin
			im = data(*,*,kindex)
			x = xv
			y = yv
		end
		im = reform(im)

; update the preview of 2D-slice
	wset,state.drawWID
	erase
	temp = im
	if max(im) eq min(im) then temp = !d.table_size*im
	TVSCL, congrid(temp,100,100)
	if state.display lt 0 then return

	CASE state.display OF 
	0: begin
		plot2d,im,xarr=x,yarr=y,title=title,group=state.base
	   end
	1: begin
	view3d_2Dascii,im,rank,kindex,report,px=x,py=y,title=title,group=Event.top
	outpath = state.outpath + 'ASCII'
	rename_dialog,outpath,report,state.class+'_'+report,Group=group

	   end
	2: begin
	xdr_open,unit,'scan2d_roi.im',/write
	xdr_write,unit,x
	xdr_write,unit,y
	xdr_write,unit,im
	xdr_close,unit
		scan2d_roi,im,x,y,group=Event.top,header=title,comment=title
	   end
	3: begin
;		if max(x) eq min(x) then x = indgen(n_elements(x))
		calibra_pick1d,im,xa=x,ya=y,title=title,group=state.base
	   end
	ENDCASE

END

PRO VIEWDRV3D_Event, Event


  WIDGET_CONTROL, Event.top, GET_UVALUE=state
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'VIEW3D_DISPLAY': BEGIN
	state.display = Event.Value
	view3d_2Dredisplay,state,Event
      END
  'VIEW3D_AXIS': BEGIN
	state.rank = Event.Value
	if state.kindex ge state.dim(state.rank) then  $
		state.kindex = state.dim(state.rank)-1
	list = state.list(0:state.dim(state.rank)-1)
	WIDGET_CONTROL,state.listWid,SET_VALUE=list
	WIDGET_CONTROL,state.listWid,SET_LIST_SELECT=state.kindex
	WIDGET_CONTROL,state.sldrWid,SET_SLIDER_MAX=state.dim(state.rank)-1
	view3d_2Dredisplay,state,Event
      END
  'VIEW3D_INDEX': BEGIN
	r =  WIDGET_INFO(Event.ID,/LIST_SELECT)
	state.kindex = r
	view3d_2Dredisplay,state,Event
	WIDGET_CONTROL,state.sldrWid,SET_VALUE=r
      END
  'VIEW3D_IMAGE_SLIDER': BEGIN
	state.kindex = Event.value  ; r
	odisplay = state.display
	state.display = -1
	view3d_2Dredisplay,state,Event
	state.display = odisplay
      END
  'VIEW3D_IMAGE_SLICE': BEGIN
      END
  'VIEW3D_ACCEPT': BEGIN
	view3d_2Dredisplay,state,Event
      END
  'VIEW3D_2DSUM': BEGIN
	data = *state.data
	rank = state.rank
	view3d_2dSum,data,rank,class=state.title,group=state.base
      END
  'VIEW3D_CLOSE': BEGIN
      WIDGET_CONTROL,Event.Top,/DESTROY
	if ptr_valid(state.data) then ptr_free,state.data
	return
      END
  ENDCASE

  WIDGET_CONTROL, state.base, SET_UVALUE=state

END




PRO view3d_2D, data, rank, xv,yv,zv,GROUP=Group,title=title,slicer3=slicer3,outpath=outpath,class=class
;+
; NAME:
;       VIEW3D_2D 
;
; PURPOSE:
;      This program let the user flexiblely examine any 2D slice from an 
;      input 3d array. It allows the user view the 2D slice as 
;      various 1D/2D plots or ASCII output.
;
; CATEGORY:
;    Widgets.
;
; CALLING SEQUENCE:
;       VIEW3D_2D, Data, [,GROUP=group]
;
; ARGUMENTS:
;    Data:    Input 3D array to be examined
;    Rank:    Specifies the rank of axis picked, 0 - x, 1 -y , 2 -z
;    XV:      Specifies the input vector of x coordinates
;    YV:      Specifies the input vector of y coordinates
;    ZV:      Specifies the input vector of z coordinates
;
; KEYWORDS:
;  GROUP:     Specifies the widget ID of the parent group
;  TITLE:     Specifies the window title
;  SLICER3:   Calls the slicers if it is non-zero
;  OUTPATH:   Specifies the output directory path
;  CLASS:     Specifies the source file class name 
;
; RESTRICTIONS:
;    The environment variables must be set by source in 
;    /usr/local/epics/extensions/bin/solaris/setup_ezcaIDL for IDL 5.1
;    /usr/local/epics/extensions/bin/solaris/ezcaidl_setup for IDL 5.3
;    All required programs will be automatically loaded into IDL by this
;    setup.
;
; EXAMPLE:
;
;    VIEW3D_2D, data
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Dec 16, 1999.
;       02-01-2001  bkc Add the xv,yv,zv parameters.
;                       If xv,yv,zv are given it will try to display data
;                       against the input coordinates. If invalid coordinates
;                       are entered, it will try to display data against index #.
;       04-24-2001  bkc Add Outpath, Class keywords on the command line
;       02-22-2002  bkc Add a slider and a preview image of 2D slice
;-


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  sz = size(data)
  if sz(0) ne 3 then begin
	r = dialog_message('3D type of data required',/error)
	return
  end
  if n_elements(rank) eq 0 then rank = 0

  ListVal919 = strtrim(indgen(max(sz(1:3))),2)

  if n_elements(title) eq 0 then title='VIEW3D_2D_SLICER'
  VIEWDRV3D = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, title=title, $
      MAP=1, $
      UVALUE='VIEWDRV3D')

  BASE1 = WIDGET_BASE(VIEWDRV3D, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE1')

  BASE2 = WIDGET_BASE(BASE1, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE3 = WIDGET_BASE(BASE2, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE3')

  Btns234 = [ $
    'PLOT2D', $
    'ASCII2D', $
    'ROI2D', $
    'PICK1D' ]
  BGROUP13 = CW_BGROUP( BASE3, Btns234, $
      COLUMN=1, /FRAME, /NO_RELEASE, $
      EXCLUSIVE=1, $
      LABEL_TOP='Display By', $
      UVALUE='VIEW3D_DISPLAY')


  BASE8 = WIDGET_BASE(BASE2, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE8')

  Btns429 = [ $
    'X Axis', $
    'Y Axis', $
    'Z Axis' ]
  BGROUP11 = CW_BGROUP( BASE8, Btns429, $
      COLUMN=1, /FRAME, /NO_RELEASE, $
      EXCLUSIVE=1, $
      LABEL_TOP='Pick Rank', $
      UVALUE='VIEW3D_AXIS')


  BASE14 = WIDGET_BASE(BASE2, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE14')

  LABEL19 = WIDGET_LABEL( BASE14, $
      UVALUE='LABEL19', $
      VALUE='Slice #')

  LIST17 = WIDGET_LIST( BASE14,VALUE=ListVal919, $
      UVALUE='VIEW3D_INDEX', $
      YSIZE=5)
  WIDGET_CONTROL,LIST17,SET_VALUE=listVal919(0:sz(rank+1)-1)

  BASE19 = WIDGET_BASE(BASE2, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE19')
  draw18 = WIDGET_DRAW(BASE19,xsize=100,ysize=100, $
	RETAIN=2, UVALUE='VIEW3D_IMAGE_SLICE')
  slider = WIDGET_SLIDER(BASE19,MAXIMUM=sz(rank+1)-1,MINIMUM=0,VALUE=0, $
		UVALUE='VIEW3D_IMAGE_SLIDER')

  BASE20 = WIDGET_BASE(BASE1, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE20')

  BUTTON21 = WIDGET_BUTTON( BASE20, $
      UVALUE='VIEW3D_ACCEPT', $
      VALUE='Accept')

  BUTTON22 = WIDGET_BUTTON( BASE20, $
      UVALUE='VIEW3D_2DSUM', $
      VALUE='View3d_2d Sum Slices...')

  BUTTON23 = WIDGET_BUTTON( BASE20, $
      UVALUE='VIEW3D_CLOSE', $
      VALUE='Close')

  if n_elements(xv) eq 0 then xv = indgen(sz(1))
  if n_elements(yv) eq 0 then yv = indgen(sz(2))
  if n_elements(zv) eq 0 then zv = indgen(sz(3))

  cd,current=p
  p = p + !os.file_sep

  view3drv_state = { $
	outpath: p, $
	class: '', $
	base:VIEWDRV3D, $
	drawWID: 0L, $
	display:0, $
	rank: rank, $
	dim: sz(1:3), $
	title: title, $
	list: ListVal919, $
	listWid: list17, $
	sldrWid: slider, $
	kindex : 0 ,$
	slicer3 : 0,$
	x: ptr_new(/allocate_heap), $
	y: ptr_new(/allocate_heap), $
	z: ptr_new(/allocate_heap), $
	data: ptr_new(/allocate_heap) $
	}
  if keyword_set(slicer3) then view3drv_state.slicer3 = 1
  if keyword_set(outpath) then view3drv_state.outpath = outpath
  if keyword_set(class) then view3drv_state.class = class
		
  *view3drv_state.data = data
  *view3drv_state.x = xv
  *view3drv_state.y = yv
  *view3drv_state.z = zv

  WIDGET_CONTROL, VIEWDRV3D, /REALIZE

  COMMON DRAW18_COMM, DRAWView3d_2d_Id
  WIDGET_CONTROL,DRAW18, GET_VALUE=DRAWView3d_2d_Id
  view3drv_state.drawWID = DRAWView3d_2d_Id

  WIDGET_CONTROL, LIST17, SET_LIST_SELECT=view3drv_state.kindex
  WIDGET_CONTROL, BGROUP11, SET_VALUE=view3drv_state.rank
  WIDGET_CONTROL, BGROUP13, SET_VALUE=view3drv_state.display
  WIDGET_CONTROL, VIEWDRV3D, SET_UVALUE=view3drv_state
 
  XMANAGER, 'VIEWDRV3D', VIEWDRV3D

END


