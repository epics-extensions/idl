;
; Auto Save File For plot2d.pro
;
;  Wed Oct 23 10:43:38 CDT 1996
;  a=findgen(40)
;  a=sin(a/5) / exp(a/50)
;  data = a#a
;
@PS_open.pro
@colorbar.pro

PRO plot2d_tablesize,ncolors
        tvlct,r,g,b,/get
        t_size = where(r eq 0 and g eq 0 and b eq 0)
        ncolors= 256 - n_elements(t_size)+1
;        print,t_size
;        help,ncolors
END

PRO plot2d_setupMargins,plot2d_state,parent

  BASE27_0 = WIDGET_BASE(parent, $
      COLUMN=1, FRAME=1, $
      MAP=1, $
      UVALUE='BASE27_0')

  plot2d_DONE_MARGINS = WIDGET_BUTTON(BASE27_0,VALUE='DonePlotMargins',$
	UVALUE='plot2d_DONE_MARGINS')

  BASE27 = WIDGET_BASE(BASE27_0, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE27')

  FieldVal4942 = plot2d_state.xmargin1
  plot2d_setupXleft = CW_FIELD( BASE27,VALUE=FieldVal4942, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Margins: Left', $
      UVALUE='plot2d_setupXleft', $
      XSIZE=2)

  FieldVal4944 = plot2d_state.xmargin2
  plot2d_setupXright = CW_FIELD( BASE27,VALUE=FieldVal4944, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Right', $
      UVALUE='plot2d_setupXright', $
      XSIZE=2)

  FieldVal4946 = plot2d_state.ymargin1
  plot2d_setupYbottom = CW_FIELD( BASE27,VALUE=FieldVal4946, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Bottom', $
      UVALUE='plot2d_setupYbottom', $
      XSIZE=2)

  FieldVal4948 = plot2d_state.ymargin2
  plot2d_setupYTop = CW_FIELD( BASE27,VALUE=FieldVal4948, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Top', $
      UVALUE='plot2d_setupYTop', $
      XSIZE=2)

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

  plot2d_setupZoomin = WIDGET_BUTTON( BASE27,VALUE=BMP5267, $
	UVALUE='plot2d_setupZoomin')
  plot2d_setupZoomout = WIDGET_BUTTON( BASE27,VALUE=BMP5269, $
	UVALUE='plot2d_setupZoomout')

  plot2d_state.marginBase = BASE27_0
END


PRO plot2d_setupLabels,plot2d_state,parent


  BASE41 = WIDGET_BASE(parent, $
      COLUMN=1, FRAME=1, $
      MAP=1, $
      UVALUE='BASE41')

  plot2d_done_labels=WIDGET_BUTTON(BASE41,VALUE='DoneLabels', $
		UVALUE='plot2d_DONE_LABELS')

  FieldVal4956 = plot2d_state.title
  plot2d_setupTitle = CW_FIELD( BASE41,VALUE=FieldVal4956, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Title', $
      UVALUE='plot2d_setupTitle', $
      XSIZE=40)

  BASE36 = WIDGET_BASE(BASE41, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE36')

  FieldVal4951 = plot2d_state.xtitle
  plot2d_setupXtitle = CW_FIELD( BASE36,VALUE=FieldVal4951, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Xtitle', $
      UVALUE='plot2d_setupXtitle', $
      XSIZE=20)

  FieldVal4953 = plot2d_state.ytitle
  plot2d_setupYtitle = CW_FIELD( BASE36,VALUE=FieldVal4953, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Ytitle', $
      UVALUE='plot2d_setupYtitle', $
      XSIZE=20)


  BASE42 = WIDGET_BASE(BASE41, $
      COLUMN=1, FRAME=1, $
      MAP=1, $
      UVALUE='BASE42')

  cmt = plot2d_state.comment(0:plot2d_state.footnote-1)
  plot2d_setupComment = CW_FIELD( BASE42,VALUE=cmt, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Legend', $
      UVALUE='plot2d_setupComment', $
      XSIZE=40,YSIZE=5)

  BASE42_2 = WIDGET_BASE(BASE42, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE42_2')

  plot2d_setupLocX = CW_FIELD( BASE42_2,VALUE=plot2d_state.xloc, $
      ROW=1, $
      FLOAT=1, $
      RETURN_EVENTS=1, $
      TITLE='Relative Placing:  rXLoc', $
      UVALUE='plot2d_setupLocX', $
      XSIZE=8)
  plot2d_setupLocY = CW_FIELD( BASE42_2,VALUE=plot2d_state.yloc, $
      ROW=1, $
      FLOAT=1, $
      RETURN_EVENTS=1, $
      TITLE='rYLoc', $
      UVALUE='plot2d_setupLocY', $
      XSIZE=8)

  plot2d_state.labelBase = BASE41

END

PRO plot2d_tvprocess_Event, Event
  WIDGET_CONTROL,Event.top,get_uvalue=plot2d_state
  WIDGET_CONTROL,plot2d_state.base,get_uvalue=plot2d_stateInit

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 
  'plot2d_setThreshold': BEGIN
	WIDGET_CONTROL,Event.id,GET_VALUE=val
	plot2d_state.threshValue=val(0)
	plot2d_state.thresh = fix((val(0)-plot2d_state.min) /(plot2d_state.max-plot2d_state.min)*(plot2d_state.table_size-1))
	WIDGET_CONTROL,plot2d_state.threshID,SET_VALUE=plot2d_state.thresh
	plot2d_replot,plot2d_state
      END
  'SLIDER7': BEGIN
	WIDGET_CONTROL,Event.id,GET_VALUE=thresh
	plot2d_state.thresh = thresh(0)
	plot2d_state.threshValue = plot2d_state.min+(plot2d_state.max-plot2d_state.min)* thresh(0)/(plot2d_state.table_size-1) 
	WIDGET_CONTROL,plot2d_state.threshVID,SET_VALUE=plot2d_state.threshValue
	plot2d_replot,plot2d_state
      END
  'SLIDER3': BEGIN
	WIDGET_CONTROL,Event.id,GET_VALUE=pixel_min
	plot2d_state.pixel_min = pixel_min(0)
	if plot2d_state.pixel_min gt plot2d_state.pixel_max then $
		plot2d_state.pixel_min = plot2d_state.pixel_max-1
	WIDGET_CONTROL,Event.id,SET_VALUE=plot2d_state.pixel_min
	plot2d_state.tvoption = 8
	plot2d_replot,plot2d_state
      END
  'SLIDER5': BEGIN
	WIDGET_CONTROL,Event.id,GET_VALUE=pixel_max
	plot2d_state.pixel_max = pixel_max(0)
	if plot2d_state.pixel_min gt plot2d_state.pixel_max then $
		plot2d_state.pixel_max = plot2d_state.pixel_min+1
	WIDGET_CONTROL,Event.id,SET_VALUE=plot2d_state.pixel_max
	plot2d_state.tvoption = 8
	plot2d_replot,plot2d_state
      END
  'FIELD127': BEGIN
	WIDGET_CONTROL,Event.id,GET_VALUE=pts
	npts = pts(0)
	dim = n_elements(plot2d_state.xarr)
	if n_elements(plot2d_state.yarr) lt dim then dim = n_elements(plot2d_state.yarr) 
	if npts gt dim then npts = dim
	id = npts MOD 2
	if id eq 0 then npts = npts-1
	if npts lt 3 then npts = 3
	plot2d_state.npts = npts 
	WIDGET_CONTROL,Event.id,SET_VALUE=plot2d_state.npts
	plot2d_replot,plot2d_state
      END
  'BUTTON131': BEGIN
	plot2d_state.tvoption = 1
	plot2d_replot,plot2d_state
      END
  'BUTTON132': BEGIN
	plot2d_state.tvoption = 2
	plot2d_replot,plot2d_state
      END
  'BUTTON133': BEGIN
	plot2d_state.tvoption = 3
	plot2d_replot,plot2d_state
      END
  'BUTTON134': BEGIN
	plot2d_state.tvoption = 4
	plot2d_replot,plot2d_state
      END
  'BUTTON135': BEGIN
	plot2d_state.tvoption = 5
	plot2d_replot,plot2d_state
      END
  'BUTTON136': BEGIN
	plot2d_state.tvoption = 6
	plot2d_replot,plot2d_state
	END
  'BUTTON138': BEGIN
	plot2d_state.tvoption = 7
	plot2d_replot,plot2d_state
	END
  'plot2d_tvhelp': BEGIN
	str = ['       Procedure to Query the Pixel Value', '',$
		'- Expose the PLOT2D window completely by moving away the', $
		'    Plot2d_TV_options window', '', $
		'- Press the "Rdpix" button to start the Query Value Mode', $
		'    LMB  - click the left mouse within the PLOT2D  image ', $
		'           updates the new value beside the "Rdpix" button', $
		'', $
		'- Click the RMB (right mouse button) within the PLOT2D drawing', $
                '    area to stop the Query Value Mode']
	xdisplayfile,text=str,title="HELP on Plot2d_TV_Rdpix"
        END
  'plot2d_tvrdpix': BEGIN
	WSET,plot2d_state.win
	sz = size(plot2d_state.data)
	dx = float(plot2d_state.range(1)-plot2d_state.range(0))/sz(1)
	dy = float(plot2d_state.range(3)-plot2d_state.range(2))/sz(2)
	cursor,x,y,0,/data ;/device
	while (!mouse.button ne 4)  DO begin     ; 2 - MMB 4 - RMB
	cursor,x,y,3,/data ;/device
;	help,!mouse,/st
	i = fix(float(x-plot2d_state.range(0))/dx)
	j = fix(float(y-plot2d_state.range(2))/dy)
	if i ge 0 and i lt sz(1) and j ge 0 and j lt sz(2) then begin
	val = plot2d_state.data(i,j)
	WIDGET_CONTROL,plot2d_state.rdpix, $
	   SET_VALUE='Z('+strtrim(i,2)+','+strtrim(j,2)+')='+strtrim(val,2)
	end
	end
	widget_control,Event.top,/clear_events
	plot2d_replot,plot2d_state
	END
  'BUTTON137': BEGIN
	plot2d_state.tvoption = 0
	plot2d_state = plot2d_stateInit
	plot2d_replot,plot2d_state
	END
  'BUTTON_REALV': BEGIN
	plot2d_state.versus = 0 
	plot2d_replot,plot2d_state
	END
  'BUTTON_STEP': BEGIN
	plot2d_state.versus = 1
	plot2d_replot,plot2d_state
	END
  'BUTTON140': BEGIN
	WIDGET_CONTROL,Event.top,/DESTROY
	plot2d_state.tvoption = 0
	plot2d_state.tvprocess = 0L
	return
      END
  ENDCASE


  WIDGET_CONTROL,Event.top,set_uvalue=plot2d_state

END




PRO plot2d_tvprocess,plot2d_state, GROUP=Group

if plot2d_state.max eq plot2d_state.min then begin
	res = dialog_message('Sorry, not available for constant image',/INFO)
	 return
end
if XRegistered('plot2d_tvprocess') then return

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  plot2d_state.plottype = 0
  plot2d_replot,plot2d_state

  plot2d_tvprocess = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, TITLE='Plot2d_TV_options', $
      MAP=1, $
      UVALUE='plot2d_tvprocess')

  BASE129 = WIDGET_BASE(plot2d_tvprocess, $
      ROw=1, $
      MAP=1, $
      UVALUE='BASE129')

  BASE75 = WIDGET_BASE(BASE129, $
      COLUMN=1, $
      FRAME=1, $
      MAP=1, $
      UVALUE='BASE75')

  BASE108 = WIDGET_BASE(BASE75, $
      ROW=1, $
      FRAME=1, $
      MAP=1, $
      UVALUE='BASE108')

  BUTTON137 = WIDGET_BUTTON( BASE108, $
      UVALUE='BUTTON137', $
      VALUE='Default')

  BUTTON138 = WIDGET_BUTTON( BASE108, $
      UVALUE='BUTTON138', $
      VALUE='Hist_Equal')

  BUTTON140 = WIDGET_BUTTON( BASE108, $
      UVALUE='BUTTON140', $
      VALUE=' Done ')

  BASE109 = WIDGET_BASE(BASE75, $
      ROW=1, $
      FRAME=1, $
      MAP=1, $
      UVALUE='BASE109')
  BUTTON109_1 = WIDGET_BUTTON( BASE109, $
      UVALUE='plot2d_tvhelp', $
      VALUE='H...')
  BUTTON109_2 = WIDGET_BUTTON( BASE109, $
      UVALUE='plot2d_tvrdpix', $
      VALUE='Rdpix')

  RDPIXLABEL = WIDGET_LABEL( BASE109,VALUE='Query Value', $
      UVALUE='RDPIXLABEL',/dynamic_resize)
  plot2d_state.rdpix = RDPIXLABEL

  BASE88 = WIDGET_BASE(BASE75, $
      ROW=1, $
      FRAME=1, $
      MAP=1, $
      UVALUE='BASE88')
  label1 = WIDGET_LABEL(BASE88,VALUE='Axes:')
  BUTTON_STEP = WIDGET_BUTTON( BASE88, $
      UVALUE='BUTTON_STEP', $
      VALUE='vs Step #')
  BUTTON_REALV = WIDGET_BUTTON( BASE88, $
      UVALUE='BUTTON_REALV', $
      VALUE='vs Value')

  BASE90 = WIDGET_BASE(BASE75, $
      ROW=1, $
      FRAME=1, $
      MAP=1, $
      UVALUE='BASE90')

  BASE94 = WIDGET_BASE(BASE90, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE94')

  SLIDER5 = CW_FSLIDER( BASE94, $
      MAXIMUM=plot2d_state.max, $
      MINIMUM=plot2d_state.min, /EDIT, $
      TITLE='Scaling Pixels < ',$ 
      UVALUE='SLIDER5', $
      VALUE=plot2d_state.max)

  SLIDER3 = CW_FSLIDER( BASE94, $
      MAXIMUM=plot2d_state.max, $
      MINIMUM=plot2d_state.min, /EDIT, $
      TITLE='Scaling Pixels > ', $
      UVALUE='SLIDER3', $
      VALUE=plot2d_state.min)

  BASE77 = WIDGET_BASE(BASE75, $
      COLUMN=1, $
      FRAME=1, $
      MAP=1, $
      UVALUE='BASE77')

  FieldVal7575 = [ $
    '7' ]
  FIELD127 = CW_FIELD( BASE77,VALUE=FieldVal7575, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='SMOOTH Width (odd)', $
      UVALUE='FIELD127', $
      XSIZE=2)
  BUTTON133 = WIDGET_BUTTON( BASE77, $
      UVALUE='BUTTON133', $
      VALUE='Smooth')
  BUTTON134 = WIDGET_BUTTON( BASE77, $
      UVALUE='BUTTON134', $
      VALUE='Unsharp Mask')

  BASE71 = WIDGET_BASE(BASE75, $
      COLUMN=1, $
      FRAME=1, $
      MAP=1, $
      UVALUE='BASE71')
  LABEL135 = WIDGET_LABEL( BASE71, $
	VALUE='Image Sharpening Methods')
  BUTTON135 = WIDGET_BUTTON( BASE71, $
      UVALUE='BUTTON135', $
      VALUE='Roberts')
  BUTTON136 = WIDGET_BUTTON( BASE71, $
      UVALUE='BUTTON136', $
      VALUE='Sobel')

  BASE76 = WIDGET_BASE(BASE75, $
      COLUMN=1, $
      FRAME=1, $
      MAP=1, $
      UVALUE='BASE76')
 label7 = WIDGET_LABEL(BASE76,VALUE='>Threshold <')

  BASE78 = WIDGET_BASE(BASE76, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE78')
  BUTTON131 = WIDGET_BUTTON( BASE78, $
      UVALUE='BUTTON131', $
      VALUE='Threshold <')
  BUTTON132 = WIDGET_BUTTON( BASE78, $
      UVALUE='BUTTON132', $
      VALUE='Threshold >')

  threshVID = CW_FIELD( BASE76,VALUE=plot2d_state.threshValue, $
      ROW=1, $
      Float=1, $
      RETURN_EVENTS=1, $
      TITLE=' ', $
      UVALUE='plot2d_setThreshold')

 SLIDER7 = WIDGET_SLIDER( BASE76, $
      MAXIMUM=plot2d_state.table_size-1, $
      MINIMUM=0, $
	/SCROLL, /SUPPRESS_VALUE, $
;      TITLE='Threshold Value', $
      UVALUE='SLIDER7', $
      VALUE=plot2d_state.thresh)
  plot2d_state.tvprocess = plot2d_tvprocess
  plot2d_state.threshID = SLIDER7
  plot2d_state.threshVID = threshVID

;  plot2d_stateInit = plot2d_state

  WIDGET_CONTROL, plot2d_tvprocess, /REALIZE
  WIDGET_CONTROL,plot2d_tvprocess,SET_UVALUE=plot2d_state

  XMANAGER, 'plot2d_tvprocess', plot2d_tvprocess, /NO_BLOCK
END

PRO plot2d_ContourLevels,plot2d_state,nlevels
; 12 number of curves allowed

	plot2d_state.nlevels = nlevels
	dlevels=(plot2d_state.max-plot2d_state.min)/(nlevels - 1)
	plot2d_state.levels = 0.
	levels = make_array(nlevels,/float)
	for i=0,nlevels-1 do begin
	levels(i) = plot2d_state.min + dlevels * i
	end
	plot2d_state.levels = levels
END


PRO plot2d_setupContourLevels_Event, Event

  WIDGET_CONTROL,Event.top,GET_UVALUE=info
  WIDGET_CONTROL,info.parent,GET_UVALUE=plot2d_state
  c_levels = plot2d_state.levels

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'TEXT16': BEGIN
      END
  'NLEVELS_CONTOUR': BEGIN
	WIDGET_CONTROL,Event.id,GET_VALUE=nlevels
	if nlevels gt n_elements(c_levels) or nlevels lt 2 then begin
		WIDGET_CONTROL,Event.id,SET_VALUE=plot2d_state.nlevels
		return
	end
	plot2d_ContourLevels,plot2d_state,nlevels
	WIDGET_CONTROL,info.textID,SET_VALUE=strtrim(plot2d_state.levels(0:plot2d_state.nlevels-1),2)
      END
  'BUTTON18': BEGIN
	print,'Level0 = ',info.levels
	WIDGET_CONTROL,info.textID,get_value=st
	nlevels = n_elements(st)
	if nlevels gt n_elements(c_levels) then begin
		res = dialog_message('Error: only 12 entry allowed!',/Error)
		return
	end
	is=0
	info.levels=make_array(12,/string,value='')
	for i=0,n_elements(st)-1 do begin
		if strtrim(st(i),2) ne '' then begin 
		info.levels(is)=st(i)
	print,'Level',is, ' = ',st(i)
		if i eq 0 then levels = float(st(i)) else $
		levels= [levels,float(st(i))]
		is = is+1
		end
	end
	info.nlevels = is
	plot2d_state.nlevels=is
	plot2d_state.levels = levels
      END
  'BUTTON19': BEGIN
	WIDGET_CONTROL,Event.top, /DESTROY
  	WIDGET_CONTROL,info.parent,SET_UVALUE=plot2d_state
	return
      END
  ENDCASE

	plot2d_replot,plot2d_state

  WIDGET_CONTROL,Event.top,SET_UVALUE=info
  WIDGET_CONTROL,info.parent,SET_UVALUE=plot2d_state
	
END




PRO plot2d_setupContourLevels, plot2d_state, GROUP=Group

   c_levels = plot2d_state.levels

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  plot2d_setupContourLevels = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, title='plot2d_levels',$
      MAP=1, $
      UVALUE='plot2d_setupContourLevels')

  BASE14 = WIDGET_BASE(plot2d_setupContourLevels, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE14')

  LABEL15 = WIDGET_LABEL( BASE14, $
      UVALUE='LABEL15', $
      VALUE='Contour Levels')

  nlevels = CW_FIELD( BASE14,VALUE=plot2d_state.nlevels, $
      /RETURN_EVENTS, /INTEGER, TITLE='# Levels:', $
      UVALUE='NLEVELS_CONTOUR', XSIZE=2)
 
	nlevels = n_elements(c_levels) 
	levels = make_array(nlevels,/string,value='')
	if plot2d_state.nlevels gt 0 then begin
	for i=0,plot2d_state.nlevels-1 do begin
		levels(i) = strtrim(c_levels(i),2)
		end
	end
  TEXT16 = WIDGET_TEXT( BASE14,VALUE=levels(0:n_elements(c_levels)-1), $
      EDITABLE=1, /scroll, $
      UVALUE='TEXT16', $
      XSIZE=12, $
      YSIZE=12)

  BASE17 = WIDGET_BASE(BASE14, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE17')

  BUTTON18 = WIDGET_BUTTON( BASE17, $
      UVALUE='BUTTON18', $
      VALUE='Accept')

  BUTTON19 = WIDGET_BUTTON( BASE17, $
      UVALUE='BUTTON19', $
      VALUE='Done')

  info = { $
	parent: plot2d_state.base, $
	base:plot2d_setupContourLevels, $
	textID:TEXT16, $
	nlevels: nlevels, $
	levels: levels $
	}

  WIDGET_CONTROL, plot2d_setupContourLevels, set_uvalue=info 
  WIDGET_CONTROL, plot2d_setupContourLevels, /REALIZE

  XMANAGER, 'plot2d_setupContourLevels', plot2d_setupContourLevels, Event_Handler='plot2d_setupContourLevels_event',/NO_BLOCK

END


PRO plot2d_setupMain13_Event, Event
COMMON COLORBAR,colorbar_data

  WIDGET_CONTROL,Event.top,GET_UVALUE=setup_info
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  WIDGET_CONTROL,setup_info.parent,GET_UVALUE=plot2d_state

  CASE Ev OF 

  'BGROUP19': BEGIN
      IF Event.Select THEN Sel = 'On' ELSE Sel = 'Off'
      CASE Event.Value OF
      0: plot2d_state.xlog = Event.Select  
      1: plot2d_state.ylog = Event.Select 
      2: plot2d_state.zlog = Event.Select
      3: plot2d_state.lego = Event.Select 
      4: plot2d_state.shade = Event.Select 
      5: plot2d_state.bar = Event.Select 
      6: plot2d_state.stamp = Event.Select 
      7: plot2d_state.bgrevs = Event.Select 
      ELSE: Message,'Unknown button pressed'
      ENDCASE
      END
  'plot2d_zoomin': BEGIN
	st = ['Mouse functions for zoom in the PLOT2D area:','', $
	'Set zoom center : Left mouse button', $
	'Set zoom factor : Middle mouse button', $
	'Exit zoom mode  : Right mouse button']
	r = dialog_message(st, title='plot2d_zoom_info',/info)
	zoom,fact=2,xsize=400,ysize=400,/KEEP ;/continuous
      END
  'plot2d_colorbar': BEGIN
	colorbar_config,GROUP=Event.top
      END
  'plot2d_datahdr': BEGIN
	plot2d_state.ascii = Event.index
	plot2d_asciiReport,plot2d_state,Event
      END
  'plot2d_setupLevels': BEGIN
	plot2d_setupContourLevels, plot2d_state, GROUP=Event.Top 
      END
  'plot2d_setupCharsize': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	plot2d_state.charsize = ch(0) 
      END
  'plot2d_setupContourCsize': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	plot2d_state.c_charsize = ch(0) 
      END
  'plot2d_setupAx': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	WIDGET_CONTROL,setup_info.Axslider,SET_VALUE=ch
	plot2d_state.Ax = ch(0) 
      END
  'plot2d_setupSLIDER3': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	WIDGET_CONTROL,setup_info.Ax,SET_VALUE=ch
	plot2d_state.Ax =  float(ch)
      END
  'plot2d_setupAz': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	WIDGET_CONTROL,setup_info.Azslider,SET_VALUE=ch
	plot2d_state.Az = ch(0) 
      END
  'plot2d_asciifmt': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	plot2d_state.format = ch(0) 
        plot2d_asciiReport,plot2d_state,Event
      END
  'plot2d_setupSLIDER4': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	WIDGET_CONTROL,setup_info.Az,SET_VALUE=ch
	plot2d_state.Az =  float(ch)
      END
  'plot2d_setupXleft': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	plot2d_state.xmargin1 = ch(0) 
      END
  'plot2d_setupXright': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	plot2d_state.xmargin2 = ch(0) 
      END
  'plot2d_setupYbottom': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	plot2d_state.ymargin1 = ch(0) 
      END
  'plot2d_setupYTop': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	plot2d_state.ymargin2 = ch(0) 
      END
  'plot2d_setupZoomin': BEGIN
	plot2d_state.zoom = plot2d_state.zoom * 1.25
      END
  'plot2d_setupZoomout': BEGIN
	plot2d_state.zoom = plot2d_state.zoom / 1.25 
      END
  'plot2d_setupXtitle': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	plot2d_state.xtitle = ch(0) 
      END
  'plot2d_setupYtitle': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	plot2d_state.ytitle = ch(0) 
      END
  'plot2d_setupTitle': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	plot2d_state.title = ch(0) 
      END
  'plot2d_setupLocX': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	plot2d_state.xloc= ch(0) 
      END
  'plot2d_setupLocY': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	plot2d_state.yloc= ch(0) 
      END
  'plot2d_DONE_LABELS': BEGIN
	WIDGET_CONTROL,plot2d_state.labelBase,/Destroy
      END
  'plot2d_DONE_MARGINS': BEGIN
	WIDGET_CONTROL,plot2d_state.marginBase,/Destroy
      END
  'plot2d_DONE_VIEWANGLE': BEGIN
	WIDGET_CONTROL,plot2d_state.viewangleBase,/Destroy
      END
  'plot2d_setupComment': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	nline = n_elements(ch)
	if nline gt 5 then nline = 5
	plot2d_state.footnote = nline
	plot2d_state.comment = ch (0:nline-1)
      END
  'plot2d_setupCurLevels': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=ch
	plot2d_state.levels = ch 
      END
  'BUTTON50': BEGIN
	WIDGET_CONTROL,Event.Top,/DESTROY
      END
  'PLOT2D_RTIFF': BEGIN
       tvlct,R,G,B,/get
	cd,current=p
	p = p + !os.file_sep +'TIFF'+!os.file_sep
	found = findfile(p,count=ct)
	if ct eq 0 then spawn,!os.mkdir + ' ' +p
	file = plot2d_state.class+'.rtiff'
	fn = dialog_pickfile(filter='*tiff',path=p,file=file,/WRITE, $
		title='Save R-TIFF Image')
	if fn ne '' then $
        WRITE_TIFF,fn,reverse(TVRD(),2),1,red=R,green=G,blue=B
      END
  'PLOT2D_TIFF': BEGIN
       tvlct,R,G,B,/get
	cd,current=p
	p = p + !os.file_sep +'TIFF'+!os.file_sep
	found = findfile(p,count=ct)
	if ct eq 0 then spawn,!os.mkdir + ' ' +p
	file = plot2d_state.class+'.tiff'
	fn = dialog_pickfile(filter='*tiff',path=p,file=file,/WRITE, $
		title='Save TIFF Image')
	if fn ne '' then $
        WRITE_TIFF,fn,TVRD(),red=R,green=G,blue=B
      END
  'PLOT2D_PICT': BEGIN
       tvlct,R,G,B,/get
	cd,current=p
	p = p + !os.file_sep +'PICT' +!os.file_sep
	found = findfile(p,count=ct)
	if ct eq 0 then spawn,!os.mkdir + ' ' +p
	file = plot2d_state.class+'.pict'
	fn = dialog_pickfile(filter='*pict',path=p,file=file,/WRITE, $
		title='Save PICT Image')
	if fn ne '' then $
        WRITE_PICT,fn,TVRD(),R,G,B
      END
  'PLOT2D_XDR': BEGIN
	cd,current=p
	p = p + !os.file_sep +'XDR'+!os.file_sep
	found = findfile(p,count=ct)
	if ct eq 0 then spawn,!os.mkdir + ' ' +p
	file = plot2d_state.class+'.xdr'
	fn = dialog_pickfile(filter='*xdr',path=p,file=file,/WRITE, $
		title='Save TIFF Image')
	if fn ne '' then begin
	xdr_open,unit,fn,/write,error=error
	xdr_write,unit,plot2d_state.data
	xdr_close,unit
	end
      END
  'BUTTON51': BEGIN
	plot2d_setupLabels,plot2d_state,Event.Top
      END
  'BUTTON52': BEGIN
	plot2d_setupMargins,plot2d_state,Event.Top
      END
  ENDCASE
	plot2d_replot, plot2d_state

  	WIDGET_CONTROL,setup_info.parent,SET_UVALUE=plot2d_state
END




PRO plot2d_setup,plot2d_state, GROUP=Group
COMMON COLORBAR, colorbar_data

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  plot2d_setupMain13 = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
      TITLE='plot2d_options', $
      MAP=1, $
      UVALUE='plot2d_setupMain13')

  BASE20 = WIDGET_BASE(plot2d_setupMain13, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE20')

  Btns4929 = [ $
    'Xlog', $
    'Ylog', $
    'Zlog', $
    'Lego', $
    'Shade', $
    'ColorBar', $
    'Stamp', $
    'Bg Reverse' $
     ]
  BGROUP19 = CW_BGROUP( BASE20, Btns4929, $
      ROW=1, $
      NONEXCLUSIVE=1, $
      LABEL_LEFT='Options:', $
      UVALUE='BGROUP19')

vals = [plot2d_state.xlog, plot2d_state.ylog, plot2d_state.zlog, $
	plot2d_state.lego, plot2d_state.shade, plot2d_state.bar, $
	plot2d_state.stamp, plot2d_state.bgrevs]
WIDGET_CONTROL,BGROUP19,set_value=vals

  BASE21 = WIDGET_BASE(plot2d_setupMain13, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE21')

  FieldVal4932 = plot2d_state.charsize
  plot2d_setupCharsize = CW_FIELD( BASE21,VALUE=FieldVal4932, $
      ROW=1, $
      FLOAT=1, $
      RETURN_EVENTS=1, $
      TITLE='Charsize', $
      UVALUE='plot2d_setupCharsize', $
      XSIZE=5)

  FieldVal4934 = plot2d_state.c_charsize
  plot2d_setupContourCsize = CW_FIELD( BASE21,VALUE=FieldVal4934, $
      ROW=1, $
      FLOAT=1, $
      RETURN_EVENTS=1, $
      TITLE='Contour-charsize', $
      UVALUE='plot2d_setupContourCsize', $
      XSIZE=5)

  plot2d_setupLevels = WIDGET_BUTTON(BASE21,VALUE='Contour_Levels ...', $
	UVALUE='plot2d_setupLevels')

  plot2d_zoomin = WIDGET_BUTTON(BASE21,VALUE='Zoom...', $
	UVALUE='plot2d_zoomin')

  plot2d_colorbar = WIDGET_BUTTON(BASE21,VALUE='ColorBar...', $
	UVALUE='plot2d_colorbar')

  BASE44 = WIDGET_BASE(plot2d_setupMain13, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE44')

;  FieldVal4959 = [ $
;    '' ]
;  plot2d_setupCurLevels = CW_FIELD( BASE44,VALUE=FieldVal4959, $
;      ROW=1, $
;      STRING=1, $
;      RETURN_EVENTS=1, $
;      TITLE='Contour-Levels', $
;      UVALUE='plot2d_setupCurLevels', $
;      XSIZE=70)


  BASE24 = WIDGET_BASE(plot2d_setupMain13, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE24')

  FieldVal4937 = plot2d_state.ax
  plot2d_setupAx = CW_FIELD( BASE24,VALUE=FieldVal4937, $
      ROW=1, $
      FLOAT=1, $
      RETURN_EVENTS=1, $
      TITLE='Rotation: Ax', $
      UVALUE='plot2d_setupAx', $
      XSIZE=5)

  plot2d_setupSLIDER3 = WIDGET_SLIDER( BASE24, $
      MAXIMUM=180, $
      MINIMUM=-180, $
      UVALUE='plot2d_setupSLIDER3', $
      VALUE=plot2d_state.ax, $
      XSIZE=80,SCROLL=1)

  FieldVal4939 = plot2d_state.az
  plot2d_setupAz = CW_FIELD( BASE24,VALUE=FieldVal4939, $
      ROW=1, $
      FLOAT=1, $
      RETURN_EVENTS=1, $
      TITLE='Az', $
      UVALUE='plot2d_setupAz', $
      XSIZE=5)

  plot2d_setupSLIDER4 = WIDGET_SLIDER( BASE24, $
      MAXIMUM=180, $
      MINIMUM=-180, $
      UVALUE='plot2d_setupSLIDER4', $
      VALUE=plot2d_state.az, $
      XSIZE=80,SCROLL=1)

  BASE24_1 = WIDGET_BASE(BASE24, $
      ROW=1, /frame, $
      MAP=1, $
      UVALUE='BASE24_1')

  plot2d_format = CW_FIELD( BASE24_1,VALUE=plot2d_state.format, $
      ROW=1, RETURN_EVENTS=1, TITLE='ASCII:', $
      UVALUE='plot2d_asciifmt', $
      XSIZE=6)

  plot2d_data_hdr = WIDGET_DROPLIST(BASE24_1,value=['Data only','W/X Value','Full Header'], $
	UVALUE='plot2d_datahdr')

  BASE47 = WIDGET_BASE(plot2d_setupMain13, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE47')

  BUTTON52 = WIDGET_BUTTON( BASE47, $
      UVALUE='BUTTON52', $
      VALUE='SetPlotMargins')
  BUTTON51 = WIDGET_BUTTON( BASE47, $
      UVALUE='BUTTON51', $
      VALUE='SetPlotLabels')
  PLOT2D_RTIFF = WIDGET_BUTTON( BASE47, $
      UVALUE='PLOT2D_RTIFF', $
      VALUE='Save RTIFF...')
  PLOT2D_TIFF = WIDGET_BUTTON( BASE47, $
      UVALUE='PLOT2D_TIFF', $
      VALUE='Save TIFF...')
  PLOT2D_PICT = WIDGET_BUTTON( BASE47, $
      UVALUE='PLOT2D_PICT', $
      VALUE='Save PICT...')
  PLOT2D_XDR = WIDGET_BUTTON( BASE47, $
      UVALUE='PLOT2D_XDR', $
      VALUE='Save XDR...')
  BUTTON50 = WIDGET_BUTTON( BASE47, $
      UVALUE='BUTTON50', $
      VALUE='   Close   ')

  setup_info = { $
	parent:plot2d_state.base, $
	Ax: plot2d_setupAx, $
	Az: plot2d_setupAz, $
	Axslider: plot2d_setupSLIDER3, $
	Azslider: plot2d_setupSLIDER4 $
	}

  WIDGET_CONTROL, plot2d_setupMAIN13, set_uvalue=setup_info
  WIDGET_CONTROL, plot2d_setupMain13, /REALIZE

  XMANAGER, 'plot2d_setupMain13', plot2d_setupMain13, /NO_BLOCK
END

PRO plot2d_replot, plot2d_state
COMMON COLORBAR, colorbar_data

if plot2d_state.bgrevs then begin
	plot2d_state.tcolor = 0
	plot2d_state.bgcolor = plot2d_state.table_size ;-1
endif else begin
	plot2d_state.tcolor = plot2d_state.table_size ;-1
	plot2d_state.bgcolor = 0
end
if !d.name eq 'PS' then plot2d_state.tcolor = 0
if !d.name ne 'PS' then WSET,plot2d_state.win
!p.multi = [0,1,0,0,0]
erase,plot2d_state.bgcolor


s  = size(plot2d_state.data)
no = s(0)
if no gt 0 then begin
dim = make_array(no)
dim = s(1:no)
end
type = s(n_elements(s)-2)

		; byte type treat as string

                if type eq 1 then begin
                plot,[-1,-1],XStyle=4,YStyle=4
                yloc = 0.9*!d.y_size
                        for j=0,dim(1)-1 do begin
                        yloc = yloc - !d.y_ch_size
                        if yloc ge 0 then begin
                        str=plot2d_state.data(0:dim(0)-1,j)
                        xyouts, 0.2*!d.x_size, yloc, string(str),/DEVICE, $
			color=plot2d_state.tcolor
                        end
                        end
		return
		end

maxvl = plot2d_state.max
minvl = plot2d_state.min
xmargin = plot2d_state.zoom *[plot2d_state.xmargin1,plot2d_state.xmargin2]
ymargin = plot2d_state.zoom *[plot2d_state.ymargin1,plot2d_state.ymargin2]

	left=!d.x_size * 0.1 * plot2d_state.zoom
	right=!d.x_size * 0.3 * plot2d_state.zoom
	bottom=!d.y_size *0.125 * plot2d_state.zoom
	top=!d.y_size *0.125 * plot2d_state.zoom

if left gt 0.4*!d.x_size then left = !d.x_size *.4
if right gt 0.4*!d.x_size then right = !d.x_size *.4
if bottom gt 0.4*!d.y_size then bottom = !d.y_size *.4
if top gt 0.4*!d.y_size then top = !d.y_size *.4
	
	plot2d_state.region = [left,bottom,!d.x_size-right,!d.y_size-top]

	width=!d.x_size-left-right
	height=!d.y_size-top-bottom

CASE plot2d_state.plottype OF
    0: begin

;        xrange=[plot2d_state.xarr(0), plot2d_state.xarr(dim(0)-1)]
;        yrange=[plot2d_state.yarr(0), plot2d_state.yarr(dim(1)-1)]
	xrange = [min(plot2d_state.xarr),max(plot2d_state.xarr)]
	yrange = [min(plot2d_state.yarr),max(plot2d_state.yarr)]
 
	if plot2d_state.versus eq 1 then begin
        xrange=[0, dim(0)]
        yrange=[0, dim(1)]
	end

	plot2d_state.range = [xrange,yrange]

	data = plot2d_state.data
	if !d.name ne 'PS' then data = CONGRID(plot2d_state.data,width,height)

	newdata = (data - plot2d_state.min)/(plot2d_state.max - plot2d_state.min)*plot2d_state.table_size
newdata = fix(newdata)

if plot2d_state.tvoption gt 0 then begin
case plot2d_state.tvoption of
1: begin 
   newdata = newdata LT plot2d_state.thresh
   TVSCL,newdata,left,bottom,xsize=width,ysize=height,top=plot2d_state.table_size
   end
2: begin
   newdata = newdata GT plot2d_state.thresh
   TVSCL,newdata,left,bottom,xsize=width,ysize=height,top=plot2d_state.table_size
   end
3: begin
   newdata = smooth(newdata,plot2d_state.npts) 
   TVSCL,newdata,left,bottom,xsize=width,ysize=height,top=plot2d_state.table_size
   end
4: begin
   newdata = fix(newdata - smooth(newdata,plot2d_state.npts)) 
   TVSCL,newdata,left,bottom,xsize=width,ysize=height,top=plot2d_state.table_size
   end
5: begin
   newdata = roberts(newdata) 
   TVSCL,newdata,left,bottom,xsize=width,ysize=height,top=plot2d_state.table_size
   end
6: begin
   newdata = sobel(newdata) 
   TVSCL,newdata,left,bottom,xsize=width,ysize=height,top=plot2d_state.table_size
   end
7: begin
   newdata = hist_equal(newdata) 
   TV,newdata,left,bottom,xsize=width,ysize=height
   end
8: begin
   TVSCL, data>plot2d_state.pixel_min<plot2d_state.pixel_max,left,bottom,xsize=width,ysize=height
   end
endcase
endif else begin
	if max(data) eq min(data) then begin
		temp = plot2d_state.table_size*data
		TV,congrid(temp,width,height),left,bottom
	endif else $
	TVSCL,data,left,bottom,xsize=width,ysize=height,top=plot2d_state.table_size
end
        xstyle = 1
        ystyle = 1
        if yrange(0) eq yrange(1) then ystyle = 4
        if xrange(0) eq xrange(1) then xstyle = 4

        plot,xrange=xrange,yrange=yrange,/nodata,[-1,-1],/noerase, $
                pos=[float(left)/!d.x_size, float(bottom)/!d.y_size, $
                 (!d.x_size-float(right))/!d.x_size, $
		 (!d.y_size-float(top))/!d.y_size], $
                xstyle=xstyle, ystyle=ystyle, xtitle=plot2d_state.xtitle, $
		ytitle=plot2d_state.ytitle, ticklen=-0.02, $
                title=plot2d_state.title, color=plot2d_state.tcolor

	plot2d_notes,plot2d_state

	; draw colorbar
	if plot2d_state.bar ne 0 then begin
colorbar_data.min = plot2d_state.pixel_min
colorbar_data.max = plot2d_state.pixel_max

; readjust if vertical colorbar assumed
if colorbar_data.horiz eq 0 then begin
	colorbar_data.x = !d.x_size-right*0.75
;	colorbar_data.width = 20
;	colorbar_data.height = (!d.y_size-top-bottom) / 2
	colorbar_data.y = (!d.y_size - colorbar_data.height)/2
end

if !d.name eq 'PS' then colorbar_data.width = colorbar_data.width*30

		colorbar,[plot2d_state.pixel_min,plot2d_state.pixel_max], $
			colorbar_data.width,colorbar_data.height, $
			 x=colorbar_data.x,y=colorbar_data.y, $
			horizontal=colorbar_data.horiz, $
			PSfact=30, reverse=plot2d_state.bgrevs, $
			format=colorbar_data.format, $
			ncolors=plot2d_state.table_size+1, $
			ncap=colorbar_data.nlabel
		end

	end
    3: begin
	if plot2d_state.shade eq 1 then begin
	shades = (plot2d_state.data-minvl)/(maxvl-minvl)*plot2d_state.table_size
	shade_surf,plot2d_state.data, plot2d_state.xarr, plot2d_state.yarr, $
		xlog=plot2d_state.xlog,ylog=plot2d_state.ylog, $
		charsize=plot2d_state.charsize, $
		ax = plot2d_state.ax, az=plot2d_state.az, $
		shades=shades, $
		xmargin=xmargin, ymargin=ymargin, $
                title=plot2d_state.title, xtitle=plot2d_state.xtitle, $
		ytitle=plot2d_state.ytitle
	endif else begin   ; default light shade assumed
	shade_surf,plot2d_state.data, plot2d_state.xarr, plot2d_state.yarr, $
		xlog=plot2d_state.xlog,ylog=plot2d_state.ylog, $
		charsize=plot2d_state.charsize, $
		ax = plot2d_state.ax, az=plot2d_state.az, $
		xmargin=xmargin, ymargin=ymargin, $
                title=plot2d_state.title, xtitle=plot2d_state.xtitle, $
		ytitle=plot2d_state.ytitle
	end
	plot2d_notes,plot2d_state
	end
    1: begin
	if plot2d_state.shade then begin 
	shades = (plot2d_state.data-minvl)/(maxvl-minvl)*plot2d_state.table_size
	surface,plot2d_state.data, plot2d_state.xarr, plot2d_state.yarr, $
		background=plot2d_state.bgcolor, color=plot2d_state.tcolor, $
		xlog=plot2d_state.xlog,ylog=plot2d_state.ylog, $
		zlog=plot2d_state.zlog, $
		charsize=plot2d_state.charsize, $
		zcharsize=plot2d_state.charsize, $
		lego=plot2d_state.lego, $
		ax = plot2d_state.ax, az=plot2d_state.az, $
		shades=shades, $
		xmargin=xmargin, ymargin=ymargin, $
                title=plot2d_state.title, xtitle=plot2d_state.xtitle, $
		ytitle=plot2d_state.ytitle
	endif else begin
	surface,plot2d_state.data, plot2d_state.xarr, plot2d_state.yarr, $
		background=plot2d_state.bgcolor, color=plot2d_state.tcolor, $
		xlog=plot2d_state.xlog,ylog=plot2d_state.ylog, $
		zlog=plot2d_state.zlog, $
		charsize=plot2d_state.charsize, $
		lego=plot2d_state.lego, $
		ax = plot2d_state.ax, az=plot2d_state.az, $
		xmargin=xmargin, ymargin=ymargin, $
                title=plot2d_state.title, xtitle=plot2d_state.xtitle, $
		ytitle=plot2d_state.ytitle
	end
	plot2d_notes,plot2d_state
	end
    2: begin
	levels = make_array(plot2d_state.nlevels,value=1,/int)
	contour,plot2d_state.data, plot2d_state.xarr, plot2d_state.yarr, $
		title=plot2d_state.title,xtitle=plot2d_state.xtitle, $
		ytitle=plot2d_state.ytitle, $
                pos=[float(left)/!d.x_size, float(bottom)/!d.y_size, $
                 (!d.x_size-float(right))/!d.x_size, $
		 (!d.y_size-float(top))/!d.y_size], $
		xmargin=xmargin, ymargin=ymargin, $
		max_value= maxvl, min_value=minvl, $
		charsize=plot2d_state.charsize, $
		c_colors=reverse(plot2d_state.colorI),$
		c_charsize=plot2d_state.c_charsize, $
		c_labels=levels, background=plot2d_state.bgcolor, $
		color=plot2d_state.tcolor, $
		levels=plot2d_state.levels(0:plot2d_state.nlevels-1), $
		NLevels=plot2d_state.nlevels, /Follow
	plot2d_notes,plot2d_state
	end
    ELSE: print,'No plot supported for this case'
ENDCASE
END

PRO plot2d_notes,plot2d_state
; draw footnote comment

if plot2d_state.footnote ne 0 then begin

        real_xl = plot2d_state.xloc * !d.x_size
        real_dy = !d.y_ch_size     ; character pixel height
	real_yl = plot2d_state.yloc * !d.y_size
        for i=0,plot2d_state.footnote-1 do begin
        xyouts,real_xl,(real_yl-i*real_dy), plot2d_state.comment(i), /DEVICE, $
		color=plot2d_state.tcolor
	end
end
; draw stamp comment

if plot2d_state.stamp ne 0 then begin
        if strtrim(plot2d_state.timestamp,2) lt 2 then st = systime(0) else $
		st = plot2d_state.timestamp
        xyouts,0.01*!d.x_size, 1, st, /device, $
		color=plot2d_state.tcolor
        xyouts,0.75*!d.x_size, 1, $
                 'User Name:  '+getenv('USER'), /device, $
		color=plot2d_state.tcolor
end

END

PRO plot2d_asciiReport,plot2d_state,Event

	im = plot2d_state.data
	x = plot2d_state.xarr
	y = plot2d_state.yarr
	nx = n_elements(x)
	ny = n_elements(y)

	f1 = '('+plot2d_state.format+',I4,'+ strtrim(nx,2)+plot2d_state.format+')'
	f0 = '('+strtrim(nx+1,2)+plot2d_state.format+')'
	f0_0 = '('+strtrim(nx,2)+plot2d_state.format+')'
	openw,1,'plot2d.txt'
	if plot2d_state.ascii eq 2 then begin
	ip = strpos(plot2d_state.format,'.')
	ff = strmid(plot2d_state.format,1,ip-1)
	f2 = '(";        YIndex:    "'+ strtrim(ny,2)+'(I'+ff+'))'
	f3 = '(";        Yvalues:   "'+ strtrim(ny,2)+'('+plot2d_state.format+'))'
	printf,1,'; ',plot2d_state.title
	printf,1,format=f3,y
	printf,1,format=f2,indgen(ny)
	printf,1,';        Xvalues  XIndex     Yvalues ...'
	end
	  for i=0,nx-1 do begin
	  lineA = reform(im(i,*))
	  if plot2d_state.ascii eq 2 then printf,1,format=f1,x(i),i,lineA 
	  if plot2d_state.ascii eq 1 then printf,1,format=f0,x(i),lineA
	  if plot2d_state.ascii eq 0 then printf,1,format=f0_0,lineA
	  end
	close,1
	xdisplayfile,'plot2d.txt',Group=Event.top
	cd,current=cpath
	rename_dialog,cpath,'plot2d.txt','',Group=Event.top
END


;
;
PRO Plot2dMAIN13_Event, Event
WIDGET_CONTROL,Event.top,GET_UVALUE=plot2d_state

; if top resize  event plot2d the base widget and redraw its plot;
IF (Event.id EQ Event.top) THEN BEGIN
        WIDGET_CONTROL,plot2d_state.id_draw, SCR_XSIZE=Event.x, SCR_YSIZE=Event.y
	plot2d_state.xsize = Event.x
	plot2d_state.ysize = Event.y

        ; if device is X
        if !d.name ne 'PS' then  WSET,plot2d_state.win

        plot2d_replot, plot2d_state

        WIDGET_CONTROL,Event.top,SET_UVALUE=plot2d_state
        return
ENDIF

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'DRAW3': BEGIN
	WSET,plot2d_state.win
if event.PRESS eq 2 then begin
	plot2d_replot, plot2d_state
	cursor,x,y,0,/data ;,/device

	data = plot2d_state.data
	xarr = plot2d_state.xarr
	yarr = plot2d_state.yarr
	vmax = plot2d_state.max
	vmin = plot2d_state.min
	
	sz = size(data)
	nn = sz(1)
	if sz(2) gt nn then nn = sz(2)
	xa = make_array(nn,2,value=!values.f_nan)
	ya = make_array(nn,2,value=!values.f_nan)

	dx = xarr(sz(1)-1) - xarr(0)
	dy = yarr(sz(2)-1) - yarr(0)
	xn = (x-xarr(0))/dx*sz(1)
	yn = (y-yarr(0))/dy*sz(2)
	xn = fix(xn)
	yn = fix(yn)

	x1 = (data(xn,*)-vmin) /(vmax-vmin)
	x1 = transpose(x1)
	y1 = (yarr - yarr(0))/dy
	xa(0:sz(2)-1,0) = x1(*)
	ya(0:sz(2)-1,0) = y1(*)

	x2 = (xarr - xarr(0))/dx
	y2 = (data(*,yn)-vmin) / (vmax - vmin)
	xa(0:sz(1)-1,1) = x2(*)
	ya(0:sz(1)-1,1) = y2(*)

	title = 'Normalized (V-Vmin)/(Vmax-Vmin)'
	xtitle = 'Normalized VY'
	ytitle = 'Normalized VX'
	comment = ['','Cross-hair @ X='+strtrim(x,2)+', Y='+strtrim(y,2)+')', $
		'Vmax = '+strtrim(vmax,2)+',  Vmin = '+strtrim(vmin,2)]
	plot1d, xa,ya,/data,title=title,xtitle=xtitle,ytitle=ytitle, $
		xrange=[0.,1.], yrange=[0,1],/color, /bgrevs, $
		comment=comment, /linestyle, $
		legend=['@ X='+strtrim(x,2),'@ Y='+strtrim(y,2)], $
		xylegend=[0.65,.92], $
		Group=Event.top

	x1 = data(xn,*)
	x1 = transpose(x1)
	plot1d,yarr,x1,/data,title='Profile @ X='+strtrim(x,2), $
		ytitle='Data Values',/color, /bgrevs, $
		xtitle='Y Values',Group=Event.top

	wait,.1
	plot1d,xarr,data(*,yn),/data,title='Profile @ Y='+strtrim(y,2), $
		ytitle='Data Values',/color, /bgrevs,  $
		xtitle='X Values',Group=Event.top
end
      END
  'BGROUP2': BEGIN
;      plot2d_state = plot2d_stateInit
      CASE Event.Value OF
      0: plot2d_state.plottype= 0  	;tv
      1: plot2d_state.plottype= 1  	;surface
      2: plot2d_state.plottype= 2  	;contour
      3: plot2d_state.plottype= 3  	;shade_surf
      4: plot2d_asciiReport,plot2d_state,Event
      5: begin
	calibra_pick1d,plot2d_state.data, $
		xa=plot2d_state.xarr,ya=plot2d_state.yarr, $
		title=plot2d_state.title,GROUP=Event.top
	end
      6: begin
	scan2d_roi,plot2d_state.data, $
		plot2d_state.xarr,plot2d_state.yarr, $
		GROUP=Event.top
	end
      7: begin
	st = ['You may find a HTML document about plot2d at :', $
		'',$
		'   http://www.aps.anl.gov/~cha/plot2d.html'$
		]
	xdisplayfile,'',text=st,title='Help ...PLOT2D'
	return
	end
	ELSE: print,'error'
      ENDCASE
      plot2d_replot, plot2d_state
      END
  'BGROUP6': BEGIN
      CASE Event.Value OF
      0: begin
	WSET,plot2d_state.win
	arr = TVRD()
	sz = size(arr)
	xs = sz(1)
	ys = sz(2)
	width = float(xs)/40
	height = float(ys)/40
;	PS_open,'idl.ps',/TV
	set_plot,'PS'
	device,filename='idl.ps',/color,bits=8, $
		/Courier,/Bold, scale_factor=1.1, $
		xsize=width,ysize=height
	TV,arr
	PS_close
	PS_print, 'idl.ps'
	end
      1: begin
	PS_printer
	end
      2: begin
	xloadct, GROUP=Event.top
	end
      3: begin
	plot2d_setup,plot2d_state, GROUP=Event.top
	end
      4: begin
	plot2d_tvprocess, plot2d_state, GROUP=Event.top
	end
      5: begin
	WIDGET_CONTROL,Event.top,BAD=bad,/DESTROY
	catch,error_status
	if error_status eq 0 then $
	WSET,plot2d_state.old_win
	plot2d_state.win = plot2d_state.old_win
	return
	end
      ELSE: Message,'Unknown button pressed'
      ENDCASE
      END
  ENDCASE

        WIDGET_CONTROL,Event.top,SET_UVALUE=plot2d_state
END




PRO plot2d,data,tlb,win, width=width, height=height, $
	charsize=charsize, lego=lego, ax=ax, az=az, shade=shade, $
	title=title, xtitle=xtitle, ytitle=ytitle, ztitle=ztitle, $
	xarr=xarr,yarr=yarr, NCOLORS=NCOLORS, $
	rxloc=rxloc, ryloc=ryloc, comment=comment, classname=classname, $
	stamp=stamp, wTitle=wTitle, GROUP=Group

;+
; NAME:
;       PLOT2D
;
; PURPOSE:
;       This routine provides a general purpose, flexible generic 2D plot
;       package.  It provides 2D TV, SURFACE, CONTOUR, and SHADE_SURF plot.
;       It is very simple to use and provides various features of 
;       adjusting 2D plot area, size, style, title, comment, etc.  
;
;       The window generated by this routine will be resizable by the
;       window manager.
;
;       Depress the 'Print' button will generate a postscript copy of the
;       graph.
;
; CATEGORY:
;       Widgets.
;
; CALLING SEQUENCE:
;
;       PLOT2D, DATA [,TLB] [,WIN]
;
; INPUTS:
;       DATA:   The 2D array to be plotted.
;
; KEYWORD PARAMETERS:
;       XARR:   Set this keyword to specify the corresponding x vector values.
;
;       YARR:   Set this keyword to specify the corresponding y vector values.
;
;       TITLE:  Set this keyword to specify the plot title string.
;
;       XTITLE: Set this keyword to specify the xtitle string.
;
;       YTITLE: Set this keyword to specify the ytitle string.
;
;       ZTITLE: Set this keyword to specify the ztitle string.
;
;       CHARSIZE: Set this keyword to specify the plot charsize, default 1.
;
;       CLASSNAME:  Set classname used in output file construction.
;
;       COMMENT:  Set this keyword to write any notes on the graph.
;
;       RXLOC:  Set notes X ratio to plot device width, default 0.01.
;
;       RYLOC:  Set notes Y ratio to plot device height, default 0.98.
;
;       STAMP:  Print the time stamp on the graph.
;
;       WTITLE: Set this keyword to specify the window title string,
;               default to 'Plot2d'.
;
;       WIDTH:  The initial window width at the creation time, which
;               default to 500 pixel.
; 
;       HEIGHT: The initial window height at the creation time, which
;               default to 450 pixel.
;
;       AX:     This keyword specifies the rotated angle about the x-axis.
;               AX>0 toward the viewer, AX<0 away from the viewer, default
;               +30 degree (Surface plot)
;
;       AZ:     This keyword specifies the rotated angle about the z-axis,
;               default 30 degree. (Surface plot)
;
;       LEGO:   This keyword specifies the z value as stacked histogram
;               style plot. (Surface plot)
;
;       SHADE:  This keyword specifies the color shade for the surface plot. 
;
;       GROUP:  The widget ID of the group leader of the widget. If this
;               keyword is specified, the death of the group leader results
;               in the death of PLOT2D.
;
;       NCOLORS: If this keyword is specified, it overrides the actual size 
;                of the color table used in plot2d.
;
; OPTIONAL_OUTPUTS:
;       TLB: The widget ID of the top level base returned by the PLOT2D.
;
;       WIN: The window ID of the drawing area used by the PLOT2D.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       The max and min value will be shown as the default comment.
;
; RESTRICTIONS:
;       For contour plot only 12 levels are allowed.
;
; EXAMPLES:
;    Example 1 - Create a resizable 2D plot without any title or label
;       specification.
;
;	     PLOT2D, Data
;
;    Example 2 - Create a resizable 2D plot with real X,Y values, title, stamp,
;       Xtitle and Ytitle specification. 
;
;	     PLOT2D,Data,Xarr=X,Yarr=y,Title=ti,Xtitle=xt,Ytitle=yt,/Stamp
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Dec 16, 1998.
;       12-22-1998      Add zoom in/out button to control X, Y margins
;       01-15-1999      Allow 5 comment lines on plot
;                       Replace base widget MAIN13 by Plot2dMAIN13
;       03-05-1999      Add Plot Options support to let user set the plot
;                       margins, title, labels, color table, various
;                       plot style, etc.
;       03-17-1999      Add TV image options
;       05-14-1999      Add colorbar, save TIFF and GIF options
;       11-23-1999      Save images in TIFF/GIF destination directory
;                       Add option of N contour levels field 
;                       Remove the common block definition
;       01-24-2000      Add TV step # or axis options
;       06-01-2000      Add colorbar_config dialog, and toggle the colorbar to
;                       get the colorbar redraw correctly
;                       Add classname for automatically generate output file
;       12-05-2000      Replace save 2D data as GIF by XDR option
;       02-15-2001      Add Data button to support 'plot2d.txt' report option
;       03-09-2001      Add xsize,ysize keyword to plot2d
;                       Default backgrand color change to white
;                       Initial colorbar use relative location
;                       PS use the TVRD exactly as shown on screen
;       05-29-2001      Add ascii format control on data output
;                       Allow the Color bar width, height ajustment
;       09-21-2001      Post script plot same as window size
;       01-18-2002      Add the PICK1D button to access plot1d, and ezfit
;       03-26-2002      Add drawing normalized X,Y profile event
;       05-01-2002      Add ncolors keyword, MMB event, check for max colors
;-
COMMON COLORBAR, colorbar_data

;if XRegistered('Plot2dMAIN13') then WIDGET_CONTROL,plot2d_state.base,BAD=bad,/DESTROY

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

xsize=500 
ysize=400 
xl = ''
yl =''
zl =''
ti = ''			; plot title
wti='PLOT2D (R1.0b)'		; window title
table_size = !d.table_size
if table_size eq 256 then plot2d_tablesize,table_size   ; actual table size
if keyword_set(NCOLORS) then table_size = NCOLORS
cl = table_size
timestamp=''
xloc = 0.01
yloc = 0.97

if keyword_set(width) then xsize=width
if xsize lt 50 then xsize=350
if keyword_set(height) then ysize=height
if ysize lt 50 then ysize=350

if keyword_set(rxloc) then xloc = float(rxloc)
if keyword_set(ryloc) then yloc = float(ryloc)
if keyword_set(title) then ti = string(title)
if keyword_set(xtitle) then xl = string(xtitle)
if keyword_set(ytitle) then yl = string(ytitle)
if keyword_set(wtitle) then wti = string(wtitle)
if keyword_set(comment) then footnote = string(comment)
if keyword_set(stamp) then timestamp = strtrim(stamp,2)

; set x,y array 
sz = size(data)
if sz(n_elements(sz)-2) eq 1 then data = fix(data)
xdim = sz(1)
ydim = sz(2)
xarray = indgen(xdim)
yarray = indgen(ydim)
if keyword_set(xarr) then xarray = xarr
if keyword_set(yarr) then yarray = yarr
class = 'plot2d'
if keyword_set(classname) then class = classname

  plot2d_state = { $
	data:data, $
	rdpix : 0L, $
	max:max(data), $
	min:min(data), $
	region: [50,50,350,350], $          ; image origin
	range: [min(xarray),max(xarray),min(yarray),max(yarray)], $ 
	x:1, $
	y:1, $
	xarr:xarray, $
	yarr:yarray, $
	ascii:0, $        ; 0,1,2  header off,index,on
	class: class, $
	versus: 0, $  ; real-value or 1 for step #
	plottype:0, $     	; 0 - TV 1-surface
	charsize:1, $
	c_charsize:1, $
	xlog:0, $
	ylog:0, $
	zlog:0, $
	ax:30, $
	az:30, $
	lego:0, $
	shade:0, $
	title:ti, $
	xtitle:xl, $
	ytitle:yl, $
	ztitle:zl, $
	footnote: 1, $
	comment: make_array(5,/string), $ 
	format: 'G17.7', $
	xloc:xloc, $
	yloc:yloc, $
	zoom:1.0, $
	xmargin1:10, $
	xmargin2:5, $
	ymargin1:5, $
	ymargin2:5, $
	bar: 1, $
	stamp: 0, $
	bgrevs: 1, $
	bgcolor: 0, $		   ; background color
	tcolor: table_size-1, $     ; text color
	table_size : table_size - 1, $
	timestamp: timestamp, $
	nlevels: 12, $
	levels: make_array(12,/float), $
	colorI: indgen(12), $
	base:0L, $
	id_draw:0L, $
	win:0, $
	old_win: !d.window, $
	marginBase: 0L, $
	labelBase: 0L, $
	tvprocess: 0L, $         ; tvprocess base widget
	threshID: 0L, $   ; threshold slider
	threshVID: 0L, $  ; threshold value
	tvoption: 0, $         ; tvprocess
	thresh: 140, $          ; threshold index value
	threshValue: 140., $      ; threshold value
	pixel_min:0., $
	pixel_max: table_size - 1., $
	npts:7, $		; smooth by 7x7 points
	xsize: xsize, $
	ysize: ysize $
	}

if n_elements(colorbar_data) eq 0 then colorbar_init,colorbar_data

plot2d_state.thresh =  plot2d_state.table_size/2
plot2d_state.threshValue = plot2d_state.min+(plot2d_state.max-plot2d_state.min)*plot2d_state.thresh/(plot2d_state.table_size-1) 

plot2d_state.pixel_min = plot2d_state.min
plot2d_state.pixel_max = plot2d_state.max

	if n_elements(footnote) gt 0 then plot2d_state.comment = footnote

	maxvl = max(data,min=minvl)
	plot2d_state.max = maxvl
	plot2d_state.min = minvl

; 12 number of curves allowed
	nlevels = plot2d_state.nlevels
	plot2d_ContourLevels,plot2d_state,nlevels

; set colors of line
        dcl = plot2d_state.table_size - 2
        ncv = 12; 4
        colorlevel = dcl / ncv
        for i=0,ncv-1 do begin
        ii = i / ncv
        im = i mod ncv
        plot2d_state.colorI(i) = dcl - ii - im * colorlevel
        end

	xmaxvl = max(data,min=xminvl)
	ymaxvl = max(data,min=yminvl)
if keyword_set(xlog) and xminvl gt 0  then plot2d_state.xlog=xlog
if keyword_set(ylog) and yminvl gt 0  then plot2d_state.ylog=ylog
if keyword_set(zlog) and minvl gt 0  then plot2d_state.zlog=zlog
if keyword_set(charsize) then plot2d_state.charsize=charsize
if keyword_set(az) then plot2d_state.az=az
if keyword_set(ax) then plot2d_state.ax=ax
if keyword_set(lego) then plot2d_state.lego=lego
if keyword_set(comment) then begin
        add_line = n_elements(comment)
        plot2d_state.footnote= add_line
        end
        plot2d_state.comment(0) = plot2d_state.comment(0)+ $
		' (Max='+strtrim(maxvl,2) + ', Min='+strtrim(minvl,2)+')'

  Plot2dMAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
      /TLB_SIZE_EVENTS, $	; resizable window
      /COLUMN, $
      MAP=1, $
      TITLE=wti, $
      UVALUE='Plot2dMAIN13')

  BASE1 = WIDGET_BASE(Plot2dMAIN13, /ROW)
  Btns111 = [ $
    'TV', $
    'SURFACE', $
    'CONTOUR', $
    'SHADE_SURF',$
    'DATA...',$
    'PICK1D...',$
    'ROI2D...',$
	'HELP ...']
  BGROUP2 = CW_BGROUP( BASE1, Btns111, $
      ROW=1, UVALUE= 'BGROUP2') 

  DRAW3 = WIDGET_DRAW( Plot2dMAIN13, XSIZE=xsize, YSIZE=ysize, RETAIN=2, $
		BUTTON_EVENTS=1,UVALUE='DRAW3')

  BASE2 = WIDGET_BASE(Plot2dMAIN13, /ROW)

  Btns361 = [ $
    'Print', $
    'Printer ...', $
    'Colors ...', $
    'Plot Options ...', $ 
    'TV Options ...', $ 
    'Done' ]
  BGROUP6 = CW_BGROUP( BASE2, Btns361, $
      ROW=1, $
      UVALUE='BGROUP6')

  g_tlb = WIDGET_INFO(Plot2dMAIN13,/geometry)

  WIDGET_CONTROL, Plot2dMAIN13, /REALIZE

  ; Get drawable window index

  WIDGET_CONTROL, DRAW3, GET_VALUE=drawWin
  ; WSET, drawWin

  ;surface, data

  tlb = Plot2dMAIN13
  win = DRAW3
	
	plot2d_state.base = tlb 
	plot2d_state.id_draw = DRAW3
	plot2d_state.win = drawWin 
	plot2d_state.xsize = g_tlb.scr_xsize
	plot2d_state.ysize = g_tlb.scr_ysize

;	plot2d_stateInit = plot2d_state
  	plot2d_replot, plot2d_state

  WIDGET_CONTROL, Plot2dMAIN13, SET_UVALUE=plot2d_state

  XMANAGER, 'Plot2dMAIN13', Plot2dMAIN13 ; ,/NO_BLOCK
END
