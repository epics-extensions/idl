
PRO view3d_2Dslice,da3D,rank,kindex,SLICER3=SLICER3,title=title,group=group,data=data
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

	if rank eq 2 then data = da3D(*,*,kindex) 
	if rank eq 1 then data = reform(da3D(*,kindex,*)) 
	if rank eq 0 then data = reform(da3D(kindex,*,*))

	plot2d,data,title=title,group=group
	
	if keyword_set(slicer3) then begin
	slicer = ptr_new(/allocate_heap)
	*slicer = da3D(*,*,*) 
	slicer3,slicer,/MODAL
	if ptr_valid(slicer) then ptr_free,slicer
	end

END

PRO view3d_2Dpick1D,da3D,rank,k,xa=xa,ya=ya,group=group

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

	calibra_pick1d,za,xa=xa,ya=ya,title=title,Group=group

END

PRO view3d_2Dascii,da3D,rank,kindex,title=title,format=format,group=group,outfile=outfile

	if n_elements(rank) eq 0 then rank = 2
	if rank gt 2 then rank = 2
	if n_elements(kindex) eq 0 then kindex = 0
	if kindex lt 0 then kindex = 0

	sz = size(da3D)
	if sz(0) ne 3 then begin
		r = dialog_message('Wrong type of data entered.',/error)
		return
	end
	catch,error_status
	if error_status ne 0 then begin
		r = dialog_message([!error_state.name+string(!error_state.code),$
			!error_state.msg,'Kindex out of range'+string(kindex)],/error)
		return
	end

	if rank eq 2 then data = reform(da3D(*,*,kindex))
	if rank eq 1 then data = reform(da3D(*,kindex,*))
	if rank eq 0 then data = reform(da3D(kindex,*,*))

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

		f0 = '(";    \ Y",'+strtrim(dim(1),2)+fwidth+',/,";   X \",/)'

		if rank eq 0 then $
		f0 = '(";    \ Z",'+strtrim(dim(1),2)+fwidth+',/,";   Y \",/)'
		if rank eq 1 then $
		f0 = '(";    \ Z",'+strtrim(dim(1),2)+fwidth+',/,";   X \",/)'

		f1 = '(I,'+strtrim(dim(1),2)+'('+t_format+'))' 

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

	CASE state.display OF 
	0: begin
		suf0 = 'Z_Slice'
		if rank eq 0 then suf0 = 'X_Slice'
		if rank eq 1 then suf0 = 'Y_Slice'
		title = title +' (' + suf0+'.slc'+strtrim(kindex,2) +')'

		view3d_2Dslice,data,rank,kindex,title=title, $
			slicer3=slicer3,group=Event.Top
	   end
	1: begin
		view3d_2Dascii,data,rank,kindex,group=Event.top
	   end
	2: begin
		view3d_2Dpick1d,data,rank,kindex,group=Event.top
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
	view3d_2Dredisplay,state,Event
      END
  'VIEW3D_INDEX': BEGIN
	r =  WIDGET_INFO(Event.ID,/LIST_SELECT)
	state.kindex = r
	view3d_2Dredisplay,state,Event
      END
  'VIEW3D_ACCEPT': BEGIN
	view3d_2Dredisplay,state,Event
      END
  'VIEW3D_CLOSE': BEGIN
      WIDGET_CONTROL,Event.Top,/DESTROY
	if ptr_valid(state.data) then ptr_free,state.data
	return
      END
  ENDCASE

  WIDGET_CONTROL, state.base, SET_UVALUE=state

END

PRO view3d_2D, data, GROUP=Group,title=title,slicer3=slicer3
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
;
; KEYWORDS:
;  GROUP:     Specifies the widget ID of the parent group
;  TITLE:     Specifies the window title
;  SLICER3:   Calls the slicers if it is non-zero
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
;-


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  sz = size(data)
  if sz(0) ne 3 then begin
	r = dialog_message('3D type of data required',/error)
	return
  end

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
  WIDGET_CONTROL,LIST17,SET_VALUE=listVal919(0:sz(3)-1)

  BASE20 = WIDGET_BASE(BASE1, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE20')

  BUTTON21 = WIDGET_BUTTON( BASE20, $
      UVALUE='VIEW3D_ACCEPT', $
      VALUE='Accept')

  BUTTON22 = WIDGET_BUTTON( BASE20, $
      UVALUE='VIEW3D_CLOSE', $
      VALUE='Close')

  view3drv_state = { $
	base:VIEWDRV3D, $
	display:0, $
	rank: 2, $
	dim: sz(1:3), $
	title: title, $
	list: ListVal919, $
	listWid: list17, $
	kindex : 0 ,$
	slicer3 : 0,$
	data: ptr_new(/allocate_heap) $
	}
  if keyword_set(slicer3) then view3drv_state.slicer3 = 1
		
  *view3drv_state.data = data

  WIDGET_CONTROL, VIEWDRV3D, /REALIZE

  WIDGET_CONTROL, LIST17, SET_LIST_SELECT=view3drv_state.kindex
  WIDGET_CONTROL, BGROUP11, SET_VALUE=view3drv_state.rank
  WIDGET_CONTROL, BGROUP13, SET_VALUE=view3drv_state.display
  WIDGET_CONTROL, VIEWDRV3D, SET_UVALUE=view3drv_state
 
  XMANAGER, 'VIEWDRV3D', VIEWDRV3D

END


