;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
@colorbar.pro
@fit_statistic.pro
@PS_open.pro
@iplot1d.pro

PRO plot1d_help
    str = ['Select Curves: Multiple Selection List', $
	'Toggle All button select all curves or just one', $
	'Click any item from scroll list just select one curve',$
	'Click while hold down CNTL adding the item to the list', $ 
	'Click while hold down SHIFT adding the items between 2 last clicks' $ 
	]
   res = dialog_message(str,title='plot1d_help',/info)
END

PRO PLOT1D_PDMENU3_Event, Event
WIDGET_CONTROL,Event.top,GET_UVALUE=state

  CASE Event.Value OF
  'Create.TIFF': BEGIN
	tvlct,R,G,B,/get
	cd,current=p
	p = p + !os.file_sep +'TIFF'+!os.file_sep
	found = findfile(p,count=ct)
	if ct eq 0 then spawn,!os.mkdir + ' ' +p
	file = 'plot1d.tiff'
	fn = dialog_pickfile(filter='*tiff',path=p,file=file,/WRITE, $
	title='Save plot1d TIFF Image')
	if fn ne '' then begin
	old_win = !d.window
	WSET,state.winDraw
	if !d.n_colors gt !d.table_size then $
	WRITE_TIFF,fn,reverse(TVRD(/true),3) else $
	WRITE_TIFF,fn,reverse(TVRD(),2),1,red=R,green=G,blue=B
	WSET,old_win
	end
    END
  'Create.PNG': BEGIN
	tvlct,R,G,B,/get
	cd,current=p
	p = p + !os.file_sep +'PNG'+!os.file_sep
	found = findfile(p,count=ct)
	if ct eq 0 then spawn,!os.mkdir + ' ' +p
	file = 'plot1d.png'
	fn = dialog_pickfile(filter='*png',path=p,file=file,/WRITE, $
	title='Save plot1D PNG Image')
	if fn ne '' then begin
	old_win = !d.window
	WSET,state.winDraw
	if !d.n_colors gt !d.table_size then $
	WRITE_PNG,fn,TVRD(/true) else $
	WRITE_PNG,fn,TVRD(),R,G,B
	WSET,old_win
	end
    END
  'Create.PICT': BEGIN
       tvlct,R,G,B,/get
	cd,current=p
	p = p + !os.file_sep +'PICT' +!os.file_sep
	found = findfile(p,count=ct)
	if ct eq 0 then spawn,!os.mkdir + ' ' +p
	file = 'plot1d.pict'
	fn = dialog_pickfile(filter='*pict',path=p,file=file,/WRITE, $
		title='Save plot1d PICT Image')
	if fn ne '' then begin
	old_win = !d.window
	if !d.n_colors gt !d.table_size then begin
		t_arr = TVRD(/true)
		arr = color_quan(t_arr,1,red,green,blue)
		tvlct,red,green,blue
		WSET,state.winDraw
		WRITE_PICT,fn,arr,Red,Green,Blue
		WSET,old_win
		tvlct,R,G,B	
	endif else begin
	WSET,state.winDraw
	WRITE_PICT,fn,TVRD(),R,G,B
	WSET,old_win
	end
	end
    END
  ENDCASE
END

PRO plot1d_dialogs_Event, Event

  WIDGET_CONTROL,Event.top,get_uvalue=state

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'plot1d_xsize': BEGIN
	widget_control,event.id,get_value=x
        state.xsize = x 
	widget_control,state.id_draw,scr_xsize=state.xsize,scr_ysize=state.ysize
      END
  'plot1d_ysize': BEGIN
	widget_control,event.id,get_value=x
        state.ysize = x 
	widget_control,state.id_draw,scr_xsize=state.xsize,scr_ysize=state.ysize
      END
  'plot1d_xrange1': BEGIN
	widget_control,Event.id,get_value=x
	state.xmin = x
      END
  'plot1d_xrange2': BEGIN
	widget_control,Event.id,get_value=x
	state.xmax = x
      END
  'plot1d_yrange1': BEGIN
	widget_control,Event.id,get_value=x
	state.ymin = x
      END
  'plot1d_yrange2': BEGIN
	widget_control,Event.id,get_value=x
	state.ymax = x
      END
  'plot1d_userscale': BEGIN
	state.userscale = Event.Index 
  	widget_control,state.rangebase,SENSITIVE = state.userscale
      END
  'plot1d_charsize': BEGIN
	state.charsize = Event.Index + 1
      END
  'plot1d_linestyle': BEGIN
	state.linestyle = Event.Index
      END
  'plot1d_grid': BEGIN
	state.grid = Event.Index
      END
  'plot1d_stamp': BEGIN
	state.stamp = Event.Index
      END
  'plot1d_yexpand': BEGIN
	state.yexpand = Event.Index
      END
  'plot1d_bgreverse': BEGIN
	state.bgrevs = Event.Index
      END
  'plot1d_xstyle': BEGIN
	state.xstyle = 2^Event.Index
      END
  'plot1d_ystyle': BEGIN
	state.ystyle = 2^Event.Index
      END
  'plot1d_all': BEGIN
	state.selection = Event.Select 
;	WIDGET_CONTROL,state.selectID,SET_VALUE=state.selection
	; set main_list
	if Event.Select then begin
		sel = indgen(n_elements(state.selection))
		WIDGET_CONTROL,state.main_list,SET_LIST_SELECT=sel
		str = state.legend(sel)
	endif else begin
		WIDGET_CONTROL,state.main_list,SET_LIST_SELECT=Event.Select
		state.selection(0) = 1
		str = state.legend(0)
	end
	WIDGET_CONTROL,state.legendStrWid,set_value=str
      END
  'plot1d_even': BEGIN
		state.selection= 0 
                state.list_sel = 0
		WIDGET_CONTROL,state.plotallWID,SET_VALUE=0
                for i=0,n_elements(state.selection)-1,2 do begin
			if i eq 0 then sel = 0 else $
			sel = [sel,i]
			state.selection(i) = 1
                end
		
                state.list_sel = sel 
	for i=0,n_elements(sel)-1 do begin
		if i eq 0 then str = state.legend(sel(0)) else $
		str = [str,state.legend(sel(i))]
	end
	WIDGET_CONTROL,state.legendStrWid,set_value=str
	WIDGET_CONTROL,state.main_list,set_list_select=sel
      END
  'plot1d_odd': BEGIN
		state.selection= 0 
                state.list_sel = 0
  		WIDGET_CONTROL,state.plotallWID,SET_VALUE=0
                for i=1,n_elements(state.selection)-1,2 do begin
			if i eq 1 then sel = 1 else $
			sel = [sel,i]
			state.selection(i) = 1
                end
		
                state.list_sel = sel 
	for i=0,n_elements(sel)-1 do begin
		if i eq 0 then str = state.legend(sel(0)) else $
		str = [str,state.legend(sel(i))]
	end
	WIDGET_CONTROL,state.legendStrWid,set_value=str
	WIDGET_CONTROL,state.main_list,set_list_select=sel
      END
  'plot1d_FWHM_Y': BEGIN
	if state.main_list gt 0 then begin
	; only plot first 10 curves to avoid too many windows pop up
	sel = widget_info(state.main_list,/list_select)
	nn = n_elements(sel)
	if nn gt 10 then nn = 10
	for i=0,nn-1 do begin
	namelabel = state.legend(sel(i))
	p1 = state.x(*,0)
	d1 = state.y(*,sel(i))
	if i eq 0 then $
	Statistic_1d,p1,d1,c_mass,xpeak,ypeak,y_hpeak,FWHM,xl,xr,/plot, $
		report='fwhm.rpt',title=state.title+namelabel+'_FWHM_Y',Group=Event.top else $
	Statistic_1d,p1,d1,c_mass,xpeak,ypeak,y_hpeak,FWHM,xl,xr,/plot, $
		title=state.title+namelabel+'_FWHM_Y',Group=Event.top
	end
	endif else begin
	namelabel = state.title
	p1 = state.x(*,0)
	d1 = state.y(*)
	Statistic_1d,p1,d1,c_mass,xpeak,ypeak,y_hpeak,FWHM,xl,xr,/plot, $
		report='fwhm.rpt',title=state.title+'_FWHM_Y',Group=Event.top
	end
      END
  'plot1d_FWHM_DY': BEGIN
	if state.main_list gt 0 then begin
	; only plot first 10 curves to avoid too many windows pop up
	sel = widget_info(state.main_list,/list_select)
	nn = n_elements(sel)
	if nn gt 10 then nn = 10
	for i=0,nn-1 do begin
	namelabel = state.legend(sel(i))
	p1 = state.x(*,0)
	d1 = state.y(*,sel(i))
	d1 = slope(p1,d1)
	if i eq 0 then $
	Statistic_1d,p1,d1,c_mass,xpeak,ypeak,y_hpeak,FWHM,xl,xr,/plot, $
		/fit, $
		report='fwhm.rpt',title=state.title+namelabel+'_FWHM_DY',Group=Event.top else $
	Statistic_1d,p1,d1,c_mass,xpeak,ypeak,y_hpeak,FWHM,xl,xr,/plot, $
		/fit, $
		title=state.title+namelabel+'_FWHM_DY',Group=Event.top
	end
	endif else begin
	namelabel = state.title
	p1 = float(state.x(*,0))
	d1 = state.y(*,0)
	d1 = slope(p1,d1)
	Statistic_1d,p1,d1,c_mass,xpeak,ypeak,y_hpeak,FWHM,xl,xr,/plot, $
		/fit, $
		report='fwhm.rpt',title=state.title+'_FWHM_DY',Group=Event.top
	end
      END
  'plot1d_FITTING': BEGIN
        xa = state.x(*,0)
        ya = state.y(*,*)
	ez_fit,xarray=xa,yarray=ya,group=Event.top
      END
  'plot1d_STATISTIC': BEGIN
	if state.main_list gt 0 then begin
	sel = widget_info(state.main_list,/list_select)
	for i=0,n_elements(sel)-1 do begin
	namelabel = state.legend(sel(i))
	p1 = state.x(*,0)
	d1 = state.y(*,sel(i))
	get_statistic_1d,namelabel,p1,d1,c_mass,xpeak,ypeak,y_hpeak,FWHM,st
		if i eq 0 then str = st else $
		str = [str,st]
	getStatisticDeviation_1d,namelabel,d1,mean,sdev,mdev,st
	str = [str,st,'']
	end
	endif else begin
	namelabel = state.title
	p1 = state.x(*,0)
	d1 = state.y(*)
	get_statistic_1d,namelabel,p1,d1,c_mass,xpeak,ypeak,y_hpeak,FWHM,str
	getStatisticDeviation_1d,namelabel,d1,mean,sdev,mdev,st
	str = [str,st,'']
	end
	xdisplayfile,text=str,title=state.title+'_STATISTIC',Group=Event.top
      END
  'plot1d_main_list': BEGIN
		state.selection= 0 
  		WIDGET_CONTROL,state.plotallWID,SET_VALUE=0
		sel = WIDGET_INFO(state.main_list, /list_select)
                if sel(0) ne -1 then begin
                for i=0,n_elements(sel)-1 do begin
			state.selection(sel(i)) = 1
                end
                end
                state.list_sel = sel 
	for i=0,n_elements(sel)-1 do begin
		if i eq 0 then str = state.legend(sel(0)) else $
		str = [str,state.legend(sel(i))]
	end
	WIDGET_CONTROL,state.legendStrWid,set_value=str
      END
  'plot1d_title': BEGIN
	WIDGET_CONTROL,Event.Id,GET_VALUE=val
	state.title = val(0)
      END
  'plot1d_xtitle': BEGIN
	WIDGET_CONTROL,Event.Id,GET_VALUE=val
	state.xtitle = val(0)
      END
  'plot1d_ytitle': BEGIN
	WIDGET_CONTROL,Event.Id,GET_VALUE=val
	state.ytitle = val(0)
      END
  'plot1d_help': BEGIN
	plot1d_help
      END
  'plot1d_npt': BEGIN
	WIDGET_CONTROL,state.CPT_WID0,GET_VALUE=cpt0
	WIDGET_CONTROL,state.CPT_WID,GET_VALUE=cpt
	if cpt lt cpt0 then begin
		cpt = cpt0+1
		WIDGET_CONTROL,state.CPT_WID,SET_VALUE=cpt
	end
	state.cpt0 = cpt0
	state.cpt = cpt
  	plot1d_replot,state
      END
  'plot1d_comment': BEGIN
	WIDGET_CONTROL,Event.Id,GET_VALUE=val
	n = n_elements(state.comment)
	state.comment = val(0:n-1)
	for i=0,n-1 do begin
	if strlen(strtrim(state.comment(i),2)) gt 1 then ln=i
	end
	state.footnote = ln+1
      END
  'plot1d_footnote': BEGIN
	state.footnote = Event.Index
	if Event.Index then begin
		for i=0,n_elements(state.comment)-1 do begin	
		 if strlen(strtrim(state.comment(i),2)) gt 1 then ln = i
		end
		state.footnote = ln + 1
	end
      END
  'plot1d_Xlogon': BEGIN
	state.xlog = Event.Index
      END
  'plot1d_Ylogon': BEGIN
	state.ylog = Event.Index
      END
  'plot1d_legendon': BEGIN
	state.legendon = Event.Index
	widget_control,state.legendWid,MAP=Event.Index
      END
  'plot1d_legend': BEGIN
	WIDGET_CONTROL,Event.Id,GET_VALUE=val
	ln=0
	for i=0,n_elements(state.legend)-1 do begin
	state.legend(i) = ''
	if strlen(strtrim(val[i],2)) gt 1 then begin
		state.legend(ln)= val(i)
		ln = ln+1
		end
	end
	WIDGET_CONTROL,Event.Id,SET_VALUE=state.legend
      END
  'plot1d_legendx': BEGIN
	WIDGET_CONTROL,Event.Id,GET_VALUE=val
	state.xylegend[0] = val
      END
  'plot1d_legendy': BEGIN
	WIDGET_CONTROL,Event.Id,GET_VALUE=val
	state.xylegend[1] = val
      END
  'plot1d_thickness': BEGIN
	WIDGET_CONTROL,Event.Id,GET_VALUE=val
	state.thick = val
      END
  'plot1d_zoomin': BEGIN
	state.zoom = state.zoom * 1.25
      END
  'plot1d_zoomout': BEGIN
	state.zoom = state.zoom / 1.25
      END
  'plot1d_symbol': BEGIN
	state.symbol = Event.Index
      END
  'plot1d_xysize_cancel': BEGIN
	WIDGET_CONTROL,state.dialogsWid,/DESTROY
	state.dialogsWid = 0L
	state.legendWid = 0L
  	WIDGET_CONTROL,state.base,set_uvalue=state
	return	
      END
  ENDCASE

  plot1d_replot,state
  WIDGET_CONTROL,Event.top,set_uvalue=state

END



PRO plot1d_dialogs, GROUP=Group ,state

if XRegistered('plot1d_dialogs') then return
state.list_sel = indgen(n_elements(state.selection))

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  plot1d_xysize = WIDGET_BASE(GROUP_LEADER=Group, $
	TITLE=state.wtitle+'Setup', $
      ROW=1, $
      MAP=1, $
      UVALUE='plot1d_xysize')

  BASE2 = WIDGET_BASE(plot1d_xysize, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  LABEL3 = WIDGET_LABEL( BASE2, $
      UVALUE='LABEL3', $
      VALUE='Setup PLOT1D Keywords')

  BASE2_1 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2')
  FieldVal1757 = [ $
    '350' ]
  FIELD4 = CW_FIELD( BASE2_1,VALUE=FieldVal1757, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='X Size:', $
      UVALUE='plot1d_xsize', $
      XSIZE=4, $
      YSIZE=1)

  FieldVal1822 = [ $
    '350' ]
  FIELD5 = CW_FIELD( BASE2_1,VALUE=FieldVal1822, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Y Size:', $
      UVALUE='plot1d_ysize', $
      XSIZE=4, $
      YSIZE=1)

  value1=['1','2','4','8','16']
  xstyle = WIDGET_DROPLIST(BASE2_1,title='Xstyle:',value=value1, $
		UVALUE='plot1d_xstyle')
  widget_control,xstyle,set_droplist_select=0

  ystyle = WIDGET_DROPLIST(BASE2_1,title='Ystyle:',value=value1, $
		UVALUE='plot1d_ystyle')
  widget_control,ystyle,set_droplist_select=0

  charsize = WIDGET_DROPLIST(BASE2_1,title='CharSize:',value=['1','2'], $
		UVALUE='plot1d_charsize')
  widget_control,charsize,set_droplist_select=0

  userscale = WIDGET_DROPLIST(BASE2_1,value=['AutoScl','UserScl'], $
		UVALUE='plot1d_userscale')
  widget_control,userscale,set_droplist_select=0

  bgrever = WIDGET_DROPLIST(BASE2_1,title='Bg:',value=['Blk','Wht'], $
		UVALUE='plot1d_bgreverse')
  widget_control,bgrever,set_droplist_select=state.bgrevs

  BASE2_2 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2_2')
  BASE2_21 = WIDGET_BASE(BASE2_2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2_21')

  BMP5267 = [ $
    [ 128b, 1b ], $
    [ 144b, 9b ], $
    [ 160b, 5b ], $
    [ 192b, 3b ], $
    [ 130b, 65b ], $
    [ 4b, 32b ], $
    [ 8b, 16b ], $
    [ 31b, 248b ], $
    [ 31b, 248b ], $
    [ 8b, 16b ], $
    [ 4b, 32b ], $
    [ 130b, 65b ], $
    [ 192b, 3b ], $
    [ 160b, 5b ], $
    [ 144b, 9b ], $
    [ 128b, 1b ]  $
  ]
  BMP5269 = [ $
    [ 128b, 1b ], $
    [ 192b, 3b ], $
    [ 160b, 5b ], $
    [ 128b, 1b ], $
    [ 136b, 17b ], $
    [ 4b, 32b ], $
    [ 2b, 64b ], $
    [ 31b, 248b ], $
    [ 31b, 248b ], $
    [ 2b, 64b ], $
    [ 4b, 32b ], $
    [ 136b, 17b ], $
    [ 128b, 1b ], $
    [ 160b, 5b ], $
    [ 192b, 3b ], $
    [ 128b, 1b ]  $
  ]

  zooml = WIDGET_LABEL(BASE2_21,value='Margins:')
  plot1d_setupZoomin = WIDGET_BUTTON( BASE2_21,VALUE=BMP5267, $
	UVALUE='plot1d_zoomin')
  plot1d_setupZoomout = WIDGET_BUTTON( BASE2_21,VALUE=BMP5269, $
	UVALUE='plot1d_zoomout')

  slider3 = widget_slider(BASE2_2,title='Thickness',value=state.thick, $
		max=10,min=1, xsize=60, $
		UVALUE='plot1d_thickness')

  linestyle = WIDGET_DROPLIST(BASE2_2,title='Linestyle:',value=['Off','On'], $
		UVALUE='plot1d_linestyle')
  widget_control,linestyle,set_droplist_select=0

  gridline = WIDGET_DROPLIST(BASE2_2,title='Grid:',value=['Off','On'], $
		UVALUE='plot1d_grid')
  widget_control,linestyle,set_droplist_select=0

  stamp = WIDGET_DROPLIST(BASE2_2,title='Stamp:',value=['Off','On'], $
		UVALUE='plot1d_stamp')
  widget_control,stamp,set_droplist_select=state.stamp

  yexpand = WIDGET_DROPLIST(BASE2_2,title='Y+:',value=['Off','On'], $
		UVALUE='plot1d_yexpand')
  widget_control,yexpand,set_droplist_select=state.yexpand

  BASE2_411 = WIDGET_BASE(BASE2, $
      /ROW, $
      MAP=1, $
      UVALUE='BASE2_411')
  labeluser = WIDGET_LABEL(BASE2_411,VALUE='UserScl Ranges:')
  xrange = state.xrange
  yrange = state.yrange
  xrange1 = CW_FIELD(BASE2_411,VALUE=xrange(0),/return_events,/float, $
		title='XMin:',XSIZE=10,UVALUE='plot1d_xrange1')
  xrange2 = CW_FIELD(BASE2_411,VALUE=xrange(1),/return_events,/float, $
		title='XMax:',XSIZE=10,UVALUE='plot1d_xrange2')
  yrange1 = CW_FIELD(BASE2_411,VALUE=yrange(0),/return_events,/float, $
		title='YMin:',XSIZE=10,UVALUE='plot1d_yrange1')
  yrange2 = CW_FIELD(BASE2_411,VALUE=yrange(1),/return_events,/float, $
		title='YMax:',XSIZE=10,UVALUE='plot1d_yrange2')
  state.rangebase = BASE2_411
  widget_control,state.rangebase,SENSITIVE = 0

  BASE2_6 = WIDGET_BASE(BASE2, $
      /ROW, $
      MAP=1, $
      UVALUE='BASE2_6')

  BASE2_61 = WIDGET_BASE(BASE2_6, $
      /COLUMN, $
      MAP=1, $
      UVALUE='BASE2_61')
  titleFIELD5 = CW_FIELD( BASE2_61,VALUE=state.title, $
      ROW=1, $
      RETURN_EVENTS=1, $
      TITLE='Title:', $
      UVALUE='plot1d_title', $
      XSIZE=40, $
      YSIZE=1)

  xtitleFIELD5 = CW_FIELD( BASE2_61,VALUE=state.xtitle, $
      ROW=1, $
      RETURN_EVENTS=1, $
      TITLE='XTitle:', $
      UVALUE='plot1d_xtitle', $
      XSIZE=40, $
      YSIZE=1)

  ytitleFIELD5 = CW_FIELD( BASE2_61,VALUE=state.ytitle, $
      ROW=1, $
      RETURN_EVENTS=1, $
      TITLE='YTitle:', $
      UVALUE='plot1d_ytitle', $
      XSIZE=40, $
      YSIZE=1)

if n_elements(state.selection) gt 1 then begin
  BASE2_62 = WIDGET_BASE(BASE2_6, $
      /ROW, /FRAME, $
      MAP=1, $
      UVALUE='BASE2_62')

  BASE2_622 = WIDGET_BASE(BASE2_62, $
      /COLUMN,  $
      MAP=1, $
      UVALUE='BASE2_622')
  lebel2 = widget_label(BASE2_622,value='SelectCurves:')
  BGROUP14 = CW_BGROUP( BASE2_622, ['All'], $
      COLUMN=1, $
      NONEXCLUSIVE=1, $
      UVALUE='plot1d_all')
  WIDGET_CONTROL,BGROUP14,SET_VALUE=1  
  state.plotallWID = BGROUP14
  help_list = WIDGET_BUTTON(BASE2_622,VALUE='Help...',UVALUE='plot1d_help')
  even_list = WIDGET_BUTTON(BASE2_622,VALUE='Even #',UVALUE='plot1d_even')
  odd_list = WIDGET_BUTTON(BASE2_622,VALUE='Odd #',UVALUE='plot1d_odd')

   ;Hold down cntrl to select only those items that are chicked on.
   ; Shift select items between last two clicks

   main_list = WIDGET_LIST(BASE2_62, VALUE=state.curves, /MULTIPLE, $
          UVALUE='plot1d_main_list', YSIZE=6)
   state.main_list = main_list
   WIDGET_CONTROL,main_list,set_list_select=state.list_sel
  
end

  ; statistic button

  BASE2_623 = WIDGET_BASE(BASE2_6, $
      /COLUMN, /frame, $
      MAP=1, $
      UVALUE='BASE2_623')
  label_tool = WIDGET_LABEL(BASE2_623,VALUE='Analysis Tools')
  statistic = WIDGET_BUTTON(BASE2_623,VALUE='STATISTIC...', $
	UVALUE='plot1d_STATISTIC')
  fwhm_y = WIDGET_BUTTON(BASE2_623,VALUE='FWHM...', $
	UVALUE='plot1d_FWHM_Y')
  fwhm_dy = WIDGET_BUTTON(BASE2_623,VALUE='FWHM DY...', $
	UVALUE='plot1d_FWHM_DY')
  fitting = WIDGET_BUTTON(BASE2_623,VALUE='FITTING...', $
	UVALUE='plot1d_FITTING')

  BASE2_4 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2_4')
  BASE2_41 = WIDGET_BASE(BASE2_4, $
      /ROW, $
      MAP=1, $
      UVALUE='BASE2_41')
  lebel1 = widget_label(BASE2_41,value='At most 10 comment lines')

  commentId = WIDGET_TEXT( BASE2_41,VALUE=state.comment, $
	/editable, /scroll, $
      UVALUE='plot1d_comment', $
      XSIZE=45, $
      YSIZE=2)

  npt_slidet0 = WIDGET_SLIDER(BASE2_41,value=1, $
	title='Start NPT:', $
	maximum=state.npt,minimum=1,UVALUE='plot1d_npt')
  npt_slidet = WIDGET_SLIDER(BASE2_41,value=state.npt, $
	title='End NPT:', $
	maximum=state.npt,minimum=1,UVALUE='plot1d_npt')
  state.cpt_wid0 = npt_slidet0
  state.cpt_wid = npt_slidet

  BASE6 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE6')

  footnote = WIDGET_DROPLIST(BASE6,title='Comment:',value=['Off','On'], $
		UVALUE='plot1d_footnote')
  if strlen(state.comment[0]) gt 1 then $
  widget_control,footnote,set_droplist_select=1 else $
  widget_control,footnote,set_droplist_select=0

  legendon = WIDGET_DROPLIST(BASE6,title='Legend:',value=['Off','On'], $
		UVALUE='plot1d_legendon')
  widget_control,legendon,set_droplist_select= state.legendon

  xlogon = WIDGET_DROPLIST(BASE6,title='Xlog:',value=['Off','On'], $
		UVALUE='plot1d_Xlogon')
  widget_control,xlogon,set_droplist_select=0
  ylogon = WIDGET_DROPLIST(BASE6,title='Ylog:',value=['Off','On'], $
		UVALUE='plot1d_Ylogon')
  widget_control,ylogon,set_droplist_select=0

  symbol = WIDGET_DROPLIST(BASE6,title='Symb:',value=['Off','On'], $
		UVALUE='plot1d_symbol')
  widget_control,symbol,set_droplist_select=0

  BUTTON8 = WIDGET_BUTTON( BASE2, $
      UVALUE='plot1d_xysize_cancel', $
      VALUE='Close')

  BASE2_3 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=0, $
      UVALUE='BASE2_3')
  legendx = CW_FSLIDER(BASE2_3,title='XLegend',value=0.75,max=1.,min=0, $
		UVALUE='plot1d_legendx',format='(F8.5)',/edit)
  legendy = CW_FSLIDER(BASE2_3,title='YLegend',value=0.35,max=1.,min=0, $
		UVALUE='plot1d_legendy',format='(F8.5)',/edit)

  legend = CW_FIELD( BASE2_3,VALUE=state.legend, $
      ROW=1, STRING=1, RETURN_EVENTS=1, TITLE='LegendStr:', $
      UVALUE='plot1d_legend', $
      XSIZE=15, $
      YSIZE=4)

  state.legendWid = BASE2_3
  state.legendStrWid = legend

  state.dialogsWid = plot1d_xysize

  WIDGET_CONTROL, plot1d_xysize, /REALIZE

   WIDGET_CONTROL,plot1d_xysize,set_uvalue=state

  XMANAGER, 'plot1d_dialogs', plot1d_xysize
END


PRO plot1d_replot,state
;COMMON Colors,r_orig,g_orig,b_orig,r_curr,g_curr,b_curr
;if n_elements(r_orig) eq 0 then loadct,39
 
; if 24 bits use color table need set deomposed=0
if !d.n_colors gt !d.table_size  then device,decomposed=1 

	state.xsize = !d.x_size
	state.ysize = !d.y_size
	cl = state.color
	psym = state.symbol
	thick = state.thick
	line = state.linestyle
	colorI = state.colorI
	if !d.name eq 'PS'  then getLineColors,colorI

	y = state.y
	s = size(y)

; check for selection 

	no = fix(total(state.selection))

	if no eq 0 then begin
		res=dialog_message('Plot1d error, nothing selected!',/Error)
		return
	end

	if s(0) eq 1 then y = y * state.factor
	x = state.x(*,0)
	
	if s(0) eq 2 then begin
 	y = make_array(s(1),no)
	if state.scatter then x =y
	il = 0
	for i=0,s(2)-1 do begin
		if state.selection(i) then begin
		y(0,il) = state.y(*,i) * state.factor(i)	
		if state.scatter then x(0,il) = state.x(*,i) 
		il = il + 1
		if n_elements(pick) eq 0 then pick = i else $
		pick = [pick,i]
		end
	end 
	s = size(y) 
	end

	cpt0 = state.cpt0-1 
	cpt = state.cpt-1 
	y = y(cpt0:cpt,*)
	s = size(y)
	x = x(cpt0:cpt,*)   

	if state.userscale eq 0 then begin
;		state.xrange = [min(x),max(x)]
		state.xrange = [x(cpt0),x(cpt)]
		state.yrange = [min(y),max(y)]
	endif else begin
		state.xrange = [state.xmin,state.xmax]
		state.yrange = [state.ymin,state.ymax]
	end

; set xy margin

	x_ch_size = !d.x_ch_size * state.charsize
	y_ch_size = !d.y_ch_size * state.charsize
	xchar_no = !d.x_size / x_ch_size *.8
	ychar_no = !d.y_size / y_ch_size *.8
	xmgin = state.xmargin * state.zoom *state.charsize
	ymgin = state.ymargin * state.zoom *state.charsize
	if xmgin(0) gt fix(xchar_no) then xmgin = xchar_no
	if ymgin(0) gt fix(ychar_no) then ymgin = ychar_no

; one dim array 

if !d.name ne 'PS' then WSET,state.winDraw
!p.multi = [0,1,0,0,0]
if state.bgrevs then begin
	state.tcolor = 0
	state.bgcolor = !d.n_colors-1 
endif else begin
	state.tcolor = !d.n_colors-1 
	state.bgcolor = 0
end
erase,122  ;,state.bgcolor

if !d.name eq 'PS' then begin
	state.tcolor=0
	thick = thick*3
	cl = 0
end

if state.yrange(0) eq state.yrange(1) then begin
	dy =  state.yrange(0)/5
	state.yrange(0) =  state.yrange(0) - 2*dy
	state.yrange(1)=  state.yrange(1) + 3*dy
end

; expand the yrange
yrange = state.yrange
if state.userscale eq 0 then begin 
	exdy = state.yexpand *(state.yrange(1)-state.yrange(0))
	yrange = yrange+0.1*[-exdy,exdy]
end

	!x.style = state.xstyle
	!y.style = state.ystyle
	if state.grid then !p.ticklen=1 else !p.ticklen=0.02
color = cl 

z = y(*,0)

	PLOT,x,z,/nodata, COLOR=state.tcolor, $
		background=state.bgcolor, $
		xrange=state.xrange, yrange = yrange, $
		ylog=state.ylog, xlog=state.xlog, psym=psym, $
		xgridstyle=state.grid, $
		ygridstyle=state.grid, $
		thick=thick,  xthick=2, ythick=2,$
		linestyle=line, $
		xmargin=xmgin, ymargin=ymgin, $
		title=state.title, xtitle=state.xtitle, ytitle=state.ytitle

if s(0) eq 1 then begin 
	OPLOT,x,z, COLOR=colorI(0), thick=thick, psym=psym, linestyle=line
end

; two dim array - multiple curves

if s(0) eq 2 and s(2) gt 1 then begin
in_line = make_array(s(2),/int)
in_symbol = make_array(s(2),/int)
in_line(0)= line
in_symbol(0)=psym
;x = state.x
;if state.scatter then x = state.x(*,0)
xt = x
if state.scatter then xt = x(*,0)
	; plot 1st line
	OPLOT,xt,z,COLOR=colorI(0), thick=thick, linestyle=line, psym=psym, $
		symsize=state.charsize 
	for i= 1 ,s(2) - 1 do begin
		if psym gt 0 then psym = psym+1
		if psym lt 0 then psym = psym-1

; the symbol 2 is too light, skip it
if i eq 2 and psym lt 0 then psym=psym-1
if i eq 2 and psym gt 0 then psym=psym+1

		if line gt 0 then line = line+1
		in_line(i) = line mod 6
		in_symbol(i) = psym 
		z = y(0:s(1)-1,i)
		if state.scatter then xt = x(*,i)

		if state.curvfit then begin
			psym=7      ; if curve fitting is true use line plot
		end
		ii = i mod 16
		OPLOT,xt,z,COLOR=colorI(ii),linestyle=line mod 6, psym=psym mod 7, $
			thick=thick 
		psym = in_symbol(i)
; print,i,psym,cl,line
	end
end

; draw footnote comment

if state.footnote ne 0 then begin
	real_xl = 0.01*!d.x_size
	real_dy = y_ch_size       ; character pixel height
	real_yl = state.footnote*real_dy +!d.y_ch_size
	for i=0,state.footnote -1 do begin
	xyouts,real_xl,(real_yl-i*real_dy), state.comment(i), /DEVICE , $
		color=state.tcolor,charsize=state.charsize
	end
end

; draw stamp comment
	
if state.stamp ne 0 then begin
	if state.timestamp eq '' then begin
	st = systime(0)
	xyouts,0.01*state.xsize, 1, st, /device, color=state.tcolor
	xyouts,0.75*state.xsize, 1, $
		 'User Name:  '+getenv('USER'), /device, color=state.tcolor
	endif else begin
	xyouts,0.01*state.xsize, 1, state.timestamp, /device, color=state.tcolor
	xyouts,0.75*state.xsize, 1, $
		 'User Name:  '+getenv('USER'), /device, color=state.tcolor
	end
end

; draw legend

if s(0) eq 2 and state.legendon gt 0 then begin

	real_x1 = state.xylegend(0)*(!x.crange(1)-!x.crange(0)) + $
		!x.crange(0)
	real_y1 = state.xylegend(1)*(!y.crange(1)-!y.crange(0)) + $
		!y.crange(0)
	real_dy = 0.1 * (!y.crange(1)-!y.crange(0))

	real_xl = real_x1 + 0.16*(!x.crange(1)-!x.crange(0))
	real_yl = real_y1 - 0.5*real_dy

	xyouts,real_x1,real_y1,'LEGEND', color=state.tcolor
	oplot,[real_x1,real_xl],[real_yl,real_yl],thick=thick

	real_xl = real_x1 + 0.075*(!x.crange(1)-!x.crange(0))
	real_xr = real_x1 + 0.1*(!x.crange(1)-!x.crange(0))

	for i=0,n_elements(pick)-1 do begin
	real_yl = real_y1 - (i*0.5+1)*real_dy

	x=[real_x1,real_xl]
	y=[real_yl,real_yl]
	ii = i mod 16
	color = state.colorI(ii)

	if psym ne 0 then $
	oplot,x,y,linestyle=line mod 6,color=color,thick=thick
	oplot,x,y,linestyle=line mod 6,color=color,psym=in_symbol(i),thick=thick

	xyouts,real_xr,real_yl, state.legend(pick(i)), color=state.tcolor
	end
end

if !d.n_colors gt !d.table_size then device,decomposed=0

END

PRO plot1d_event,ev 

WIDGET_CONTROL,ev.Top,GET_UVALUE=state

; resize event

IF (ev.id EQ ev.top) then begin
; plot1d the draw widget and redraw its plot;
	WIDGET_CONTROL,state.id_draw, SCR_XSIZE=ev.x, SCR_YSIZE=ev.y

	; if device is X
	if !d.name ne 'PS' then  WSET,state.winDraw

	plot1d_replot, state

	WIDGET_CONTROL,ev.Top,GET_UVALUE=state
	return
ENDIF

WIDGET_CONTROL,ev.Id,GET_UVALUE=B_ev

CASE B_ev OF
'PLOT1D_SAVEMENU': PLOT1D_PDMENU3_Event,ev
'PLOT1D_DATA': begin
	sz = size(state.y)
;	nel = sz(1)
	nel = state.cpt
	f1 = '('+ strtrim(nel+1,2)+'G17.7)'

	openw,1,'plot1d.txt'
	if state.scatter then  $
		printf,1,'; Scattering data column vectors: Xi,Yi, ...' else $
		printf,1,'; Data column vectors: X,Y1,Y2,Y3, ...'

	if sz(0) eq 1 then $
	for i=0,nel-1 do printf,1,state.x(i),state.y(i) 
	if sz(0) eq 2 then begin
	str = ';YIndex          '
	for j=0L,sz(2)-1 do str = str + string(j) + '  '
	printf,1,str
	end

	if sz(0) eq 2 then $
	for i=0,nel-1 do begin
	y = reform(state.y(i,*))
;	print,format=f1,state.x(i),y

	if state.scatter then begin
		str =  ''
		for j=1,n_elements(y) do begin
		str = str + ''+string(state.x(i,j-1))+ ' ' + string(state.y(i,j-1))
		end
	endif else begin
		str =  ''
		str = string(state.x(i))
		for j=1,n_elements(y) do begin
		str = str + ' ' + string(y(j-1))
		end
	end
	printf,1,str
	end

	close,1

	xdisplayfile,'plot1d.txt',title='Listing of plot1d.txt',Group=Ev.top
	cd,current=cpath
	if xregistered('plot1d') then $
	rename_dialog,cpath,'plot1d.txt','',Group=Ev.top
	end
'PLOT1D_REPORT': begin
	widget_control,ev.Id,GET_VALUE=st
	xdisplayfile,state.report,title='Listing of '+ state.report
 	end
'PLOT1D_REVERSE': begin
	if state.bgrevs then state.bgrevs = 0 else state.bgrevs = 1
	plot1d_replot, state
 	end
'PLOT1D_PRINTER': begin
	PS_printer,Group=Ev.top
	end
'PLOT1D_PSPLOT': begin
	PS_open, 'idl.ps'
	linestyle = state.linestyle
	bgrevs = state.bgrevs
	autocolor = state.autocolor
;	state.autocolor = 0
	state.linestyle = 1
	state.bgrevs = 1
	plot1d_replot, state
	PS_close
	state.linestyle = linestyle 
	state.bgrevs = bgrevs 
	state.autocolor = autocolor 
	PS_print, 'idl.ps'
	end
'PLOT1D_PRINT': begin
	PS_TVRD,wid=state.winDraw
	end
'PLOT1D_OPTIONS': begin
	plot1d_dialogs, state, Group=ev.top
	end
'PLOT1D_IPLOT': begin
	x = state.x
	y = state.y
	iplot1d_drv,x,y,title=state.title,group=ev.top
	end
'PLOT1D_CLOSE': begin
	WIDGET_CONTROL,ev.top,BAD=bad,/DESTROY
	state.winDraw=0L
	return
	end
ENDCASE

	catch,error_status
	if error_status then return
	WIDGET_CONTROL,ev.Top,SET_UVALUE=state,BAD_ID=bad

END

PRO plot1d, x, y, id_tlb, windraw, factor=factor, $
	title=title,xtitle=xtitle,ytitle=ytitle,color=color, $
	symbol=symbol, charsize=charsize, thick=thick, linestyle=linestyle, $
        xrange=xrange, yrange=yrange, xlog=xlog, ylog=ylog, $
	xmargin=xmargin, ymargin=ymargin, stamp=stamp,$
	legend=legend, xylegend=xylegend, $
	width=width, height=height, $
	comment=comment, cleanup=cleanup, $
	curvfit=curvfit, bgrevs=bgrevs, $
	xstyle=xstyle, ystyle=ystyle, $
	wtitle=wtitle, report=report, data=data, button=button, GROUP=GROUP
;+
; NAME:
;       PLOT1D
;
; PURPOSE:
;       This routine provides a general purpose flexible cartesion plot
;       package.  It provides simple to use automatic feature of labels,
;       legend, comment, line style, symbols, and color options on plot.
;
;       The window generated by this routine will be resizable by the 
;       window manager. 
;
;       Depress the 'Print' button will generate a postscript copy of the
;       graph.
;
;       Normally it accepts two parameters X and Y. If the first parameter
;       is not used then the data array is plotted on the ordinate versus 
;       the point number on the abscissa.  Multiple curves (or variables) 
;       can be stacked into the second parameter as a two dimensional array, 
;       the first dimension gives the number of data points in each curve,
;       the second dimension gives the number of curves to be plotted.
;  
;
; CATEGORY:
;       Widgets.
;
; CALLING SEQUENCE:
;
;       PLOT1D, [X,] Y [,ID_TLB]  [,ID_DRAW]
;
; INPUTS:
;       X:      The vector array for X abscissa.
;
;       Y:      The Y data array for curve plot. The Y array can contain
;               more than one curve, the first dimension gives the number
;               of data to be plotted for the curve, the second dimension
;               gives the number of curves to be plotted.
;	
; KEYWORD PARAMETERS:
;       TITLE:  Set this keyword to specify the plot title string.
;
;       XTITLE: Set this keyword to specify the xtitle string.
;
;       YTITLE: Set this keyword to specify the ytitle string.
;
;       COLOR:  Set this keyword to specify the color number used
;               in the plot routine.
;
;       FACTOR: Set the curve multiplication factor for the Y vector, default 1.
;
;      CURVFIT: Set this keyword if two curves are plotted, first curve
;               is the fitted curve, the second curve is data to be fitted. 
;
;       SYMBOL: Set this keyword to specify data plotted as symbol, set to -1
;               data plot as symbol and connected with line.
;
;       XLOG:   Set this keyword to specify a logrithmic X axis.
;
;       YLOG:   Set this keyword to specify a logrithmic Y axis.
;
;       XRANGE: Set this keyword to specify the desired data range for 
;               the X axis.
;
;       YRANGE: Set this keyword to specify the desired data range for 
;               the Y axis.
;
;       XMARGIN: Set this keyword to specify the left and right margin, 
;                default xmargin=[10,3]
;
;       YMARGIN: Set this keyword to specify the bottom and top margin 
;                default ymargin=[5,3]
;
;       CHARSIZE:Set this keyword to specify the charsize
;
;       THICK:   Set this keyword to specify the line thickness for the
;                axes and the line plot. 
;
;       LINESTYLE:  Set this keyword to turn on different line style used. 
;
;       XSTYLE:  Set this keyword to control x axis in IDL plot routine. 
;
;       YSTYLE:  Set this keyword to control y axis in IDL plot routine. 
;
;       LEGEND:  Set the legend strings corresponding to curves drawn.
;
;       XYLEGEND: Set the x,y location of the legend strings, % from the
;                 lower left corner from the graph window, default 
;                 xylegend=[0.75, 0.35].
;
;       COMMENT:  Set this keyword to write any footnotes on the graph.
;
;       STAMP:  Set this keyword to put the time stamp and user ID on the page. 
;
;       WTITLE: Set this keyword to specify the window title string,
;               default to 'Plot1d'.
;
;       WIDTH:  The initial window width at the creation time, which 
;               default to 350 pixel.
;  
;       HEIGHT: The initial window height at the creation time, which 
;               default to 350 pixel.
;
;       GROUP:  The widget ID of the group leader of the widget. If this
;               keyword is specified, the death of the group leader results 
;               in the death of PLOT1D.
;
;       DATA:   Set this keyword if an xdisplayfile data button is desired
;       REPORT: Set this keyword if an xdisplayfile report button is desired
;               It specifies the report file name to be displayed.
;
;       BUTTON: Set this keyword if no print and close buttons are desired
;               for the PLOT1D widget.
;
;       CLEANUP: Set this keyword if the created window can no be closed by the
;                window manager is desired.
;
;       BGREVS:  Reverse background color 
;
; OPTIONAL_OUTPUTS:
;       ID_TLB: The widget ID of the top level base returned by the PLOT1D. 
;
;       ID_DRAW: The widget ID of the drawing area used by the PLOT1D. 
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       If more than one curves are stored in the Y array, it automatically
;       uses different color for each curve. If the color keyword is set
;       by the user, then the specified color will be used for the whole plot. 
;
; RESTRICTIONS:
;       It is assumed that the X position array is the same for multiple
;       curve plot. 
;
; EXAMPLES:
;       Create a resizable line plot without any title or label 
;       specification.
;
;           x = !pi * indgen(100)/25
;           PLOT1D, x, sin(x)
;
;       Create a resizable line plot with title specifications. 
;
;           PLOT1D, x, sin(x), title='title', xtitle='xtitle', ytitle='ytitle'
;
;       Plot two curves with different linestyle and legend at default location.
;
;           x=indgen(100)
;           y=make_array(100,2)
;           y(0,0)=sin(x * !pi / 50)
;           y(0,1)=cos(x * !pi / 50)
;           PLOT1D,x,y,legend=['line1','line2'],/linestyle
;
;       Same as the above example plus symbol and a specified legend location.
;
;           x=indgen(100)
;           y=make_array(100,2)
;           y(0,0)=sin(x * !pi / 50)
;           y(0,1)=cos(x * !pi / 50)
;           PLOT1D,x,y,/linestyle,symbol=-1, $
;              legend=['line1','line2'], xylegend=[0.5,0.9]
;
;       Plot x,y array plus two lines of comment and a time stamp on the graph.
;     
;           PLOT1D,x,y,comment=['Comment line1','Comment line2'],/stamp
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, Mar. 7, 1996.
;
;       04-26-96 bkc   Add the window cleanup keyword 
;       10-28-96 bkc   Add the xstyle and ystyle keywords 
;       07-01-97 bkc   Comment out LOADCT,39 inherit color from parent process 
;       08-11-97 bkc   Add the curvfit support, only two curves allowed 
;       12-22-97 bkc   Add the 24 bit color visual device support 
;       09-04-98 bkc   Fix the plot problem due to ymax eq ymin
;       09-19-99 bkc   Support plot1d various plot options
;                      Support the multiple scatter plot
;                      Add the support of report, factor, charsize keywords
;       11-19-99 bkc   Support auto-scaled/user-specified X,Y plot ranges
;                      Add multiple list selection of curves
;       02-12-01 bkc   Support 'plot1d.txt' data report option
;       03-09-01 bkc   Default white backgrund color
;                      Print button dum the screen plot by using TVRD
;                      PS Plot button generates PS plot output 
;       02-18-02 bkc   Add option of NPT slider to specify the last of the 
;                      data points to be plotted for each curve
;       05-01-02 bkc   Add FWHM_Y, FWHM_DY, Fitting, Statistic buttons
;                      Check for 24 bit display
;                      Add bgrevs keyword
;                      Check for incomplete color table size case 
;                      Check for scattering data 1D plot 
;       04-01-04 bkc   Support both PseudoColor and TrueColor devices
;                      Add ITOOL... dialog for falling iplot
;-

COMMON Colors,r_orig,g_orig,b_orig,r_curr,g_curr,b_curr
if n_elements(r_orig) eq 0 then LOADCT,39

; check any data provided

n1 = n_elements(x)
if n1 lt 2 then begin
	print,'Error: No data specified.'
	return
	end
xa = x

if n_elements(y) eq 0 then begin
	ya = xa
	s = size(ya)
	n1 = s(1)
	xa = indgen(n1)
endif else ya = y

leg =['']
sz = size(ya)
if sz(0) eq 1 then begin
	leg = '' 
	curves = '1'
	selection = 1
	list_sel = 0
	rfactor = 1.
end
if sz(0) eq 2 then begin
	leg = make_array(sz(2),/string) 
	curves = 'Curve '+strtrim(indgen(sz(2)),2)
if keyword_set(legend) then curves = legend(0:sz(2)-1)
	selection = make_array(sz(2),/int,value=1)
	list_sel = indgen(sz(2))
	rfactor = make_array(sz(2),value=1.)
end
if keyword_set(factor) then rfactor = factor

	; find the actual table size
	table_size = !d.table_size
	if table_size eq 256 then begin
        tvlct,r,g,b,/get
        t_size = where(r eq 0 and g eq 0 and b eq 0)
        table_size = 256 - n_elements(t_size)+1
	end

	colorI = lonarr(19)
	getLineColors,colorI

; check for input labels
xsize=350
ysize=350
xl = ''
yl =''
ti = ''
wti='Plot1d'
cl = table_size - 1
footnote = ''
add_line=0
if keyword_set(title) then ti = string(title)
if keyword_set(xtitle) then xl = string(xtitle)
if keyword_set(ytitle) then yl = string(ytitle)
if keyword_set(wtitle) then wti = string(wtitle)
if keyword_set(legend) then leg = string(legend) else leg = curves
if keyword_set(comment) then footnote = string(comment)
timestamp = ''
if keyword_set(stamp) then begin
	if strlen(stamp) gt 1 then timestamp=stamp
end

state = { $
	base:0L, $
	rangebase:0L, $
	id_draw:0L, $
	winDraw:0L,$
	dialogsWid:0L,$
	legendWid:0L,$
	legendStrWid:0L,$
	plotallWid:0L,$
	main_list: 0L, $	; multi-list
	list_sel: list_sel, $   ; initially all selected
	report:'',$
	table_size : table_size, $   ; actual table size
	autocolor: 1, $ 	; automatic use different color for each curve
	bgrevs : 1, $          ; 1-reverse background 
	tcolor : 0, $
	bgcolor : table_size-1, $
	color:cl-2, $ 
	symbol: 0, $
	curvfit: 0, $		; whether data is from curve fitting
	wtitle:wti, $
	xtitle:xl, $
	ytitle:yl, $
	title:ti, $
	xstyle:0,$
	ystyle:0,$
	grid:0, $
	xsize:0,$
	ysize:0,$
	xlog: 0, $
	ylog: 0, $
	xmargin: [10,3], $
	ymargin: [5,3], $
	zoom: 1.0, $
	stamp: 0, $
	timestamp: timestamp, $
	yexpand: 1, $
	footnote: 0, $
	comment: strarr(20), $  ; footnote, $
        xrange: [min(xa),max(xa)], $
        yrange: [min(ya),max(ya)], $
	xmin: 0., $
	xmax: 0., $
	ymin: 0., $
	ymax: 0., $
	legendon: 0, $
	xylegend: [.75,0.35], $
	legend: leg, $
	curves: curves, $
	selection: selection, $
        selectID: 0L, $
	thick: 2, $
	linestyle: 0, $
	charsize: 1, $
	userscale: 0, $
	scatter: 0, $       ; if 1 scatter plot
	factor: rfactor, $
	NPT: sz(1), $   ; requested NPT 
	CPT: sz(1), $   ; plotted CPT
	CPT0: 1, $
	CPT_WID0:0L, $
	CPT_WID:0L, $
	colorI: colorI, $
	x: xa, $
	y: ya $
	}

state.xmin = state.xrange(0)
state.xmax = state.xrange(1)
state.ymin = state.yrange(0)
state.ymax = state.yrange(1)

xsz = size(xa)
if (total(xsz) - total(sz)) eq 0. then state.scatter = 1

state.comment = footnote
if keyword_set(xstyle) then state.xstyle = xstyle 
if keyword_set(ystyle) then state.ystyle = ystyle 

if keyword_set(bgrevs) then state.bgrevs = bgrevs 

if keyword_set(charsize) then begin
	if charsize gt 1 then state.charsize = charsize
	end
if keyword_set(color) then begin
	cl = long(color)
	state.color = cl
	state.autocolor = 0   ; use fixed color
	end
if keyword_set(symbol) then begin
	state.symbol = symbol
	end
	psym = state.symbol
if keyword_set(xlog) then state.xlog=1
if keyword_set(ylog) then state.ylog=1
if keyword_set(thick) then state.thick= thick
if keyword_set(linestyle) then state.linestyle=1 
if keyword_set(stamp) then state.stamp=1 
if keyword_set(legend) then state.legendon = 1
if keyword_set(xylegend) then begin 
	if n_elements(xylegend) eq 2 then state.xylegend=xylegend
	end
if keyword_set(xrange) then begin 
	if n_elements(xrange) eq 2 then state.xrange=xrange
	end
if keyword_set(yrange) then begin 
	if n_elements(yrange) eq 2 then state.yrange=yrange
	end
if keyword_set(xmargin) then begin 
	if n_elements(xmargin) eq 2 then state.xmargin=xmargin
	end
if keyword_set(comment) then begin
	state.footnote= n_elements(comment) 
	ymargin=[5+state.footnote, 3]
	end
if keyword_set(ymargin) then begin 
	if n_elements(ymargin) eq 2 then state.ymargin=ymargin
	end

if keyword_set(curvfit) then state.curvfit = 1

if keyword_set(width) then xsize=width
if xsize lt 350 then xsize=350
if keyword_set(height) then ysize=height
if ysize lt 350 then ysize=350

; drawing with data 

if keyword_set(CLEANUP) then $
id_tlb=WIDGET_BASE(Title=wti,/COLUMN, /TLB_SIZE_EVENTS, TLB_FRAME_ATTR=8) $
else $
id_tlb=WIDGET_BASE(Title=wti,/COLUMN, /TLB_SIZE_EVENTS)

if keyword_set(button) eq 0 then begin
id_tlb_row=WIDGET_BASE(id_tlb,/ROW)

if keyword_set(data) then begin
 id_data_report = WIDGET_BUTTON(id_tlb_row,VALUE='Data...',UVALUE='PLOT1D_DATA')
end

if keyword_set(report) then begin
	state.report = report
 id_tlb_report = WIDGET_BUTTON(id_tlb_row,VALUE=report+'...',UVALUE='PLOT1D_REPORT')
end

id_tlb_colors = WIDGET_BUTTON(id_tlb_row,VALUE='Rvs Bkg',UVALUE='PLOT1D_REVERSE')
id_tlb_options = WIDGET_BUTTON(id_tlb_row,VALUE='Options...',UVALUE='PLOT1D_OPTIONS')
id_tlb_printer = WIDGET_BUTTON(id_tlb_row,VALUE='Printer...',UVALUE='PLOT1D_PRINTER')
id_tlb_print = WIDGET_BUTTON(id_tlb_row,VALUE='Print',UVALUE='PLOT1D_PRINT')
id_tlb_psplot = WIDGET_BUTTON(id_tlb_row,VALUE='PS Plot',UVALUE='PLOT1D_PSPLOT')

junk   = { CW_PDMENU_S, flags:0, name:'' }
  MenuDesc256 = [ $
      { CW_PDMENU_S,       3, 'Create' }, $ ;        0
        { CW_PDMENU_S,       0, 'TIFF' }, $ ;        1
        { CW_PDMENU_S,       0, 'PNG' }, $ ;        2
        { CW_PDMENU_S,       2, 'PICT' } $  ;      3
  ]
  PLOT1D_PDMENU3 = CW_PDMENU( id_tlb_row, MenuDesc256, /RETURN_FULL_NAME, $
      UVALUE='PLOT1D_SAVEMENU')

id_tlb_iplot = WIDGET_BUTTON(id_tlb_row,VALUE='IPLOT',UVALUE='PLOT1D_IPLOT')
id_tlb_close = WIDGET_BUTTON(id_tlb_row,VALUE='Close',UVALUE='PLOT1D_CLOSE')
end

id_draw=WIDGET_DRAW(id_tlb,xsize=xsize,ysize=ysize, RETAIN=2)

state.base = id_tlb

g_tlb=WIDGET_INFO(id_tlb,/geometry)

WIDGET_CONTROL,id_tlb, /realize
WIDGET_CONTROL,id_draw,get_value=windraw
	state.winDraw = windraw
	state.id_draw = id_draw
	state.xsize = g_tlb.scr_xsize
	state.ysize = g_tlb.scr_ysize
	if !d.name ne 'PS' then WSET,windraw
	
	plot1d_replot, state

	WIDGET_CONTROL,id_tlb,SET_UVALUE=state

xmanager,'plot1d',id_tlb,  GROUP_LEADER=GROUP,/NO_BLOCK, $
	EVENT_HANDLER="plot1d_event"

END
