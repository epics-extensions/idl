PRO view3d_2dsum_result,state
	data = state.data
	rank = state.rank

        suf0 = 'Z_Slice'
        if rank eq 0 then suf0 = 'X_Slice'
        if rank eq 1 then suf0 = 'Y_Slice'
        file= suf0+'.slc'+strtrim(state.k1_pick,2)+'_'+strtrim(state.k2_pick,2)

	CASE rank OF
	0: begin
	image = make_array(state.imax,state.jmax)
	for k=state.k1_pick,state.k2_pick do begin
		temp = data(k,*,*)
		image(*,*) = image(*,*)+temp
	   end
	end
	1: begin
	image = make_array(state.imax,state.jmax)
	for k=state.k1_pick,state.k2_pick do begin
		temp = data(*,k,*)
		image(*,*) = image(*,*)+temp
	   end
	end
	2: begin
	image = make_array(state.imax,state.jmax)
	for k=state.k1_pick,state.k2_pick do begin
		temp = data(*,*,k)
		image(*,*) = image(*,*)+temp
	   end
	end
	ENDCASE

;	state.image = image

      CASE state.dpy_type OF
      0: begin
	plot2d,image,title=state.class+file
	end
      1: begin
	view3d_sum_ascii,image,rank,title=state.class,outfile=file,group=state.base
	end
      2: begin
	comment = state.class+file
	scan2d_roi,image,group=state.base,comment=comment   ;x,y
	end
      3: begin
	Print,'Button PICK1D Pressed'
	calibra_pick1d,image,group=state.base,title=state.class+file  ;xa=xa,ya=ya
	end
      ELSE: Message,'Unknown button pressed'
      ENDCASE
END

PRO view3d_sum_ascii,image,rank,title=title,outfile=outfile,group=group

	t_format = 'G18.8'
	if keyword_set(format) then t_format = format
        fwidth = 'I'+strmid(t_format,1,strpos(t_format,'.')-1)

	file = 'Slices_sum'
        report = file 
        if keyword_set(outfile) then report = strtrim(outfile,2)

        st ='; Pick Rank # = '+strtrim(rank,2) 
        if rank eq 0 then st = st+ ' (ie X axis) '
        if rank eq 1 then st = st+ ' (ie Y axis) '
        if rank eq 2 then st = st+ ' (ie Z axis) '

	if keyword_set(title) then st = st+title
	st = st + report	

        openw,fw,report,/get_lun

        s = size(image)
        dim = s(1:2)
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
        newdata = transpose(image)

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


PRO view3d_2dsum_plot,state
	rank = state.rank
	CASE rank OF
	0: begin
		v = state.data(*,state.i_pick,state.j_pick)
	end
	1: begin
		v = state.data(state.i_pick,*,state.j_pick)
	end
	2: begin
		v = state.data(state.i_pick,state.j_pick,*)
	end
	ELSE:
	ENDCASE

	title='Rank='+strtrim(state.rank,2)+', I='+strtrim(state.i_pick,2)+', J='+strtrim(state.j_pick,2)+'
	xtitle = 'K1='+strtrim(state.k1_pick,2)+', K2='+strtrim(state.k2_pick,2)

	if state.PS eq 2 then begin
	  openw,1,'idl.ps'
	  printf,1,title+',  '+xtitle
	  for i=0,n_elements(v)-1 do printf,1,i,v(i)
	  close,1
	  xdisplayfile,'idl.ps',group=state.base
	  state.PS = 0
	  return
	end
	if state.PS eq 1 then begin
	  PS_open,'idl.ps'
	  plot,v,title=title,xtitle=xtitle
  	  yr = [state.min,state.max]
	  oplot,[state.k1_pick,state.k1_pick],yr,thick=3,color=100
	  oplot,[state.k2_pick,state.k2_pick],yr,thick=3,color=100
	  PS_close
	  PS_print,'idl.ps'
	  state.PS = 0
	  return
	endif else begin
	  wid = !d.window
  	  wset,state.drawWid
	  plot,v,title=title,xtitle=xtitle    ;,/ylog
	  yr = [state.min,state.max]
	  oplot,[state.k1_pick,state.k1_pick],yr,thick=3,color=100
	  oplot,[state.k2_pick,state.k2_pick],yr,thick=3,color=100
	  if wid ge 0 then wset,wid
	end
END


PRO VIEW3D_2DSUM_Event, Event

  WIDGET_CONTROL, Event.Top, GET_UVALUE=state
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'VIEW3D_SUM_I': BEGIN
	widget_control,Event.Id,get_value=id
	state.i_pick = id
	view3d_2dsum_plot,state
      END
  'VIEW3D_SUM_J': BEGIN
	widget_control,Event.Id,get_value=id
	state.j_pick = id
	view3d_2dsum_plot,state
      END
  'VIEW3D_SUM_K1': BEGIN
	widget_control,Event.Id,get_value=id
	state.k1_pick = id
	view3d_2dsum_plot,state
      END
  'VIEW3D_SUM_K2': BEGIN
	widget_control,Event.Id,get_value=id
	state.k2_pick = id
	view3d_2dsum_plot,state
      END
  'VIEW3D_SUM_PS': BEGIN
	state.PS = 1
	view3d_2dsum_plot,state	
      END
  'VIEW3D_SUM_PSARRAY': BEGIN
	state.PS = 2
	view3d_2dsum_plot,state	
      END
  'VIEW3D_SUM_PLOT2D': BEGIN
	state.dpy_type = 0
	view3d_2dsum_result,state	
      END
  'VIEW3D_SUM_ASCII2D': BEGIN
	state.dpy_type = 1
	view3d_2dsum_result,state	
      END
  'VIEW3D_SUM_ROI2D': BEGIN
	state.dpy_type = 2
	view3d_2dsum_result,state	
      END
  'VIEW3D_SUM_PICK1D': BEGIN
	state.dpy_type = 3
	view3d_2dsum_result,state	
      END
  'VIEW3D_SUM_DONE': BEGIN
      Print, 'Event for Done'
	widget_control,Event.top,/DESTROY
	return
      END
  'VIEW3D_SUM_DRAW': BEGIN
	if Event.PRESS eq 1 or Event.PRESS eq 2 then begin
	wid = !d.window
	wset,state.drawWID
	cursor,x,y,/data,/NOWAIT
	if wid ne -1 then wset,wid
	if fix(x) lt 0 then x = 0
	if fix(x) gt state.kmax then x = state.kmax
	CASE !mouse.button OF
	1: begin
		state.k1_pick = fix(x)
		widget_control,state.k1_slider,set_value=state.k1_pick
	end
	2: begin
		state.k2_pick = fix(x)
		widget_control,state.k2_slider,set_value=state.k2_pick
	end
	4: begin
		return
	end
	ELSE:
	ENDCASE
	view3d_2dsum_plot,state
	end
      END
  ENDCASE

  WIDGET_CONTROL, Event.top, SET_UVALUE=state

END

PRO view3d_2dsum_init,data,rank,view3d_2dsumState
	sz = size(data)
   
     CASE rank OF
	0: begin
	kmax = sz(1)
	imax = sz(2)
	jmax = sz(3)
	end
	1: begin
	kmax = sz(2)
	imax = sz(1)
	jmax = sz(3)
	end
	2: begin
	kmax = sz(3)
	imax = sz(1)
	jmax = sz(2)
	end
     ENDCASE

	v2 = max(data,id,min=v1)
; i = id mod sz(1)
; j = id / sz(1) mod sz(2) 
; k = id / (sz(1)*sz(2))
; print,v1,v2,i,j,k, data(i,j,k)

	view3d_2dsumState = {$
		class: '', $
		base : 0L, $
		k1_slider : 0L, $
		k2_slider : 0L, $
		drawWid : 0L, $
		PS : 0, $       ; post script ind
		rank : rank, $
		imax : imax, $
		jmax : jmax, $
		kmax : kmax, $
		dpy_type: 0, $	 ; plot2d,ascii,roi2d,pick2d
		i_pick: imax/2, $
		j_pick: jmax/2, $
		k1_pick: 0, $
		k2_pick: kmax-1, $
		min : v1, $
		max : v2, $
		image: make_array(imax,jmax), $
		data: data $
		}


END

PRO view3d_2dsum,data,rank,class=class, GROUP=Group
;+
; NAME:
;       VIEW3D_2DSUM
;
; PURPOSE:
;       This program let the user interactively extract the ROI for a given
;       3D data array for a user sepecified axial rank. It calculates the 
;       axial sum of 2D-ROI data and generated a resultant final 2D image, 
;       and provide various 2D image viewing function PLOT2D, ASCII2D, 
;       ROI2D, PICK1d on the resultant 2D image.
;
; CATEGORY:
;       Widgets.
;
; CALLING SEQUENCE:
;
;       VIEW3D_2DSUM, Data [,Rank] [,CLASS=Class] [,GROUP=Group] 
;
; INPUTS:
;       DATA:   Input 3D data array
;       RANK:   Specifies the summation axial direction, default to 0
;               0 - X axis, 1 - Y axis, 2 - Z axis
;
; KEYWORD PARAMETERS:
;       CLASS:      A string used to annotate the special ROI. If specified
;                   it will be put in the statistic report.
;       GROUP:      Specifies the parent window ID, the close of parent window
;                   will result the close of this program.
;
; SIDE EFFECTS:
;       This program allows the user to access any 2D image viewing
;       program with the newly generated sum 2D image data.
;
; RESTRICTIONS:
;       Input data array must be a 3D data array.
;
; EXAMPLE:
;      Extract the 2D image along the X axis with the given 3D data array 
;
;       view3d_2dsum, data, 0
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Jan 31, 2001.
;       xx-xx-xxxx  bkc comment
;-

sz = size(data)
if sz(0) ne 3 then begin
	r = dialog_message('3D array required',/Error)
	return
end

; if Xregistered('VIEW3D_2DSUM') then return 

if n_elements(rank) eq 0 then rank=0

	view3d_2dsum_init,data,rank,view3d_2dsumState

  if keyword_set(class) then view3d_2dsumState.class = class

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  VIEW3D_2DSUM_BASE = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, MAP=1, $
	title='VIEW3D_SUM_2DROI (R1.0)', $
      UVALUE='VIEW3D_2DSUM_BASE')
  view3d_2dsumState.base = VIEW3D_2DSUM_BASE


  BASE1 = WIDGET_BASE(VIEW3D_2DSUM_BASE, $
      ROW=1, $
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

  SLIDER4 = WIDGET_SLIDER( BASE3, $
      MAXIMUM=view3d_2dsumState.imax-1, $
      MINIMUM=0, /scroll, $
      TITLE='I', /vertical, ysize=80, $ 
      UVALUE='VIEW3D_SUM_I', $
      VALUE=view3d_2dsumState.i_pick)

  SLIDER5 = WIDGET_SLIDER( BASE3, $
      MAXIMUM=view3d_2dsumState.jmax-1, $
      MINIMUM=0, /scroll, $
      TITLE='J', /vertical, ysize=80, $
      UVALUE='VIEW3D_SUM_J', $
      VALUE=view3d_2dsumState.j_pick)


  BASE5 = WIDGET_BASE(BASE2, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE5')

  if keyword_set(class) then tbl_title= widget_label(BASE5, $
	value=class)
  DRAW20 = WIDGET_DRAW( BASE5, $
      RETAIN=1, /BUTTON_EVENTS, $
      UVALUE='VIEW3D_SUM_DRAW', $
      XSIZE=300, $
      YSIZE=150)

  LABEL23 = WIDGET_LABEL( BASE5, $
      UVALUE='LABEL23', $
      VALUE='Axial ROI Range Indices ')

  BASE51 = WIDGET_BASE(BASE5, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE51')

  SLIDER6 = WIDGET_SLIDER( BASE51, $
      MAXIMUM=view3d_2dsumState.kmax-1, MINIMUM=0, $
      title='',xsize=150, /scroll, $
      UVALUE='VIEW3D_SUM_K1', $
      VALUE=0)
  view3d_2dsumState.k1_slider = SLIDER6

  SLIDER7 = WIDGET_SLIDER( BASE51, $
      MAXIMUM=view3d_2dsumState.kmax-1, MINIMUM=0, $
      TITLE='', xsize=150, /scroll, $
      UVALUE='VIEW3D_SUM_K2', $
      VALUE=view3d_2dsumState.kmax-1)
  view3d_2dsumState.k2_slider = SLIDER7

  BASE6 = WIDGET_BASE(BASE2, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE6')

  plot2d_b = WIDGET_BUTTON( BASE6, $
      UVALUE='VIEW3D_SUM_PLOT2D', $
      VALUE='PLOT2D')

  ascii2d_b = WIDGET_BUTTON( BASE6, $
      UVALUE='VIEW3D_SUM_ASCII2D', $
      VALUE='ASCII2D')

  roi2d_b = WIDGET_BUTTON( BASE6, $
      UVALUE='VIEW3D_SUM_ROI2D', $
      VALUE='ROI2D')

  pick1d_b = WIDGET_BUTTON( BASE6, $
      UVALUE='VIEW3D_SUM_PICK1D', $
      VALUE='PICK1D')

  viewdata_b = WIDGET_BUTTON( BASE6, $
      UVALUE='VIEW3D_SUM_PSARRAY', $
      VALUE='Screen Data')

  postscpt_b = WIDGET_BUTTON( BASE6, $
      UVALUE='VIEW3D_SUM_PS', $
      VALUE='PS Plot')

  BUTTON24 = WIDGET_BUTTON( BASE6, $
      UVALUE='VIEW3D_SUM_DONE', $
      VALUE='Done')

  WIDGET_CONTROL, VIEW3D_2DSUM_BASE, /REALIZE

  ; Get drawable window index

  COMMON DRAW20_Comm, DRAW20_Id
  WIDGET_CONTROL, DRAW20, GET_VALUE=DRAW20_Id
  view3d_2dsumState.drawWid = DRAW20_Id
  view3d_2dsum_plot,view3d_2dsumState

  WIDGET_CONTROL, VIEW3D_2DSUM_BASE, SET_UVALUE=view3d_2dsumState
  XMANAGER, 'VIEW3D_2DSUM', VIEW3D_2DSUM_BASE
END