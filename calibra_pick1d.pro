;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
@u_read.pro

PRO check_window,info_win
info_win0 = info_win(0)
info_win1 = info_win(1)
if info_win1 ne -1 then begin
	catch,error_status
	if error_status ne 0 then goto,clean1
	WSET,info_win1
	widget_control,info_win0,/destroy
end
clean1:	info_win1 = -1
	info_win = [0L,-1]
END

PRO CALIBRA_PICK1D_Event, Event

  WIDGET_CONTROL,Event.top,GET_UVALUE=info
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'CALIBRA_FORMAT': BEGIN
	WIDGET_CONTROL,Event.id, GET_VALUE=ft
	WIDGET_CONTROL,info.table2,FORMAT=ft(0)
      END
  'CALIBRA_TABLE2': BEGIN
	case event.type of
	4: begin
		info.pickx = event.sel_top
		info.picky = event.sel_left
	     if event.sel_left gt -1 then begin
	if event.sel_left eq event.sel_right then begin
	d = event.sel_left
	col = info.values(*,d)
	check_window,info.win2
	plot1d,info.xa,col,id_tlb,windraw,Xtitle='COLUMN '+strtrim(d,2),GROUP=Event.Top, $
		Wtitle='PICK1D', /data, /bgrevs, $
		Ytitle='Values',TITLE=info.title + ' @ Y='+strtrim(info.ya(d),2)
	info.win2 = [id_tlb,windraw]
	end
		info.select = [event.sel_left,event.sel_top, $
			event.sel_right,event.sel_bottom]
;		print,'Column=',strtrim(info.select(0),2), $
;			'   Row=',strtrim(info.select(1),2)
	if event.sel_top eq event.sel_bottom then begin
	d = event.sel_top
	row = transpose(info.values(d,*))
	check_window,info.win1
	plot1d,info.ya,row,id_tlb,windraw,Xtitle='ROW '+strtrim(d,2),GROUP=Event.Top, $
		Wtitle='PICK1D', /data, /bgrevs, $
		Ytitle='Values',TITLE=info.title + ' @ X='+strtrim(info.xa(d),2)
	info.win1= [id_tlb,windraw]
	end
	     end
	end
	else : begin
	end
	endcase
      END

  'CALIBRA_PLOTROWS': BEGIN
	check_window,info.win1
	plot1d,info.ya,transpose(info.values),id_tlb,windraw,GROUP=Event.Top,Xtitle='Y values', $
		Wtitle='PICK1DROWS', /data, /bgrevs, $
		Ytitle='Values',TITLE=info.title
	info.win1= [id_tlb,windraw]
      END
  'CALIBRA_PLOTCOLUMNS': BEGIN
	check_window,info.win2
	plot1d,info.xa,info.values,id_tlb,windraw,GROUP=Event.Top,Xtitle='X values', $
		Wtitle='PICK1DCOLUMNS', /data, /bgrevs, $
		Ytitle='Z values',TITLE=info.title
	info.win2= [id_tlb,windraw]
      END
  'CALIBRA_FITTING': BEGIN
	u_openw,unit,'fitting.bin',/XDR
        u_write,unit,info.xa
        u_write,unit,info.ya
        u_write,unit,info.values
        u_close,unit
	if info.picky ne 0 then $
	ez_fit,xarray=info.xa,yarray=info.ya,im=info.values,jpick=info.picky,GROUP=Event.Top else $
	ez_fit,xarray=info.xa,yarray=info.ya,im=info.values,ipick=info.pickx,GROUP=Event.Top 
      END
  'CALIBRA_CLOSE2': BEGIN
      WIDGET_CONTROL,Event.top,/DESTROY
	return
      END
  'CALIBRA_HELP2': BEGIN
	str = [$
	'PICK1D dialog tabulates the calibrated 2D image versus with X and Y index.',$
	'It allows the user to generate various 1D line plot from the table.',$
	'',$
	'Help...             - display this help message', $
	'Format Field        - control the format used by the table', $
        '                        it must be enclosed in parentheses (  )',$
	'Plot-Rows...        - access multiple rows of data for plot', $ 
	'Plot-Columns...     - access multiple columns of data for plot', $ 
	'Fitting...          - load the 2D image into the ez_fit package', $ 
	'Close               - close this Pick1D dialog', $
	'TABLE Events : ', $
	'  Click Header Xi    - plot the Xi row vector', $
	'  Click Header Yj    - plot the Yj column vector', $
	'  Click Xi,Yj Cell   - plot both the Xi row vector, and Yj column vector', $
	'' $
	]
	res = DIALOG_MESSAGE(str,/info)
        return
      END
  ENDCASE

      WIDGET_CONTROL,Event.top,SET_UVALUE=info
END




PRO calibra_pick1D, val, xa=xa, ya=ya,title=title, GROUP=Group

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  wti = 'CALIBRA_PICK1D'
  if keyword_set(title) then wti=wti+'_'+title

  CALIBRA_PICK1D = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, TITLE=wti, $
      MAP=1, $
      UVALUE='CALIBRA_PICK1D')

  BASE2 = WIDGET_BASE(CALIBRA_PICK1D, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE3 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE3')


	if n_params() eq 0 then val = findgen(20,10)
  	sz = size(val)

	; transpose X,Y in table widget
	tempval = transpose(val)
	width = sz(2)
	height = sz(1)

	if n_elements(xa) eq 0 then xa = indgen(height)
	if n_elements(ya) eq 0 then ya = indgen(width)

	row_labels = make_array(sz(1),/string)
  	for i=0,height -1 do begin
  	row_labels(i) = 'Row' + strtrim(i,2)
	end
	if sz(0) eq 2 then begin
	column_labels = make_array(sz(2),/string)
  	for i=0,width -1 do begin
  	column_labels(i) = 'Col' + strtrim(i,2)
	end
	end

  BUTTON9 = WIDGET_BUTTON( BASE3, $
      UVALUE='CALIBRA_HELP2', $
      VALUE='Help...')

  table2_format = CW_FIELD(BASE3,value='(G12.8)', XSIZE=10, $
		TITLE='Format:',/RETURN_EVENT,UVALUE='CALIBRA_FORMAT')

  BUTTON7 = WIDGET_BUTTON( BASE3, $
      UVALUE='CALIBRA_PLOTROWS', $
      VALUE='Plot-Rows...')

  BUTTON8 = WIDGET_BUTTON( BASE3, $
      UVALUE='CALIBRA_PLOTCOLUMNS', $
      VALUE='Plot-Columns...')

  BUTTON10 = WIDGET_BUTTON( BASE3, $
      UVALUE='CALIBRA_FITTING', $
      VALUE='Fitting...')


  BUTTON9 = WIDGET_BUTTON( BASE3, $
      UVALUE='CALIBRA_CLOSE2', $
      VALUE='Close')

  calibra_table2 = WIDGET_TABLE(BASE2,VALUE=tempval, $
	UVALUE='CALIBRA_TABLE2', /ALL_EVENTS, $
	ALIGNMENT=2, $   ; right justified
	COLUMN_LABELS=column_labels, ROW_LABELS=row_labels, $
	/RESIZEABLE_COLUMNS,/RESIZEABLE_ROWS,XSIZE=width,YSIZE=height, $
	X_SCROLL_SIZE=5, Y_SCROLL_SIZE=10)

	WIDGET_CONTROL,calibra_table2, COLUMN_WIDTHS=150

  if n_elements(title) eq 0 then title='Calibration_Pick1D'
  info = { $
	   title : title, $
	   table2: calibra_table2, $
	   values: val, $        ; transpose of 2D image
	   xa: xa, $
	   ya: ya, $
	   height: height, $
	   width: width, $
plot1d_xlog: 0, $
plot1d_ylog: 0, $
plot1d_xrange: [min(xa),max(xa)], $
plot1d_yrange: [min(ya),max(ya)], $
plot1d_width: 350, $
plot1d_height:350, $
plot1d_charsize: 1, $
plot1d_thick: 1, $
plot1d_xmargin: [10,3], $
plot1d_ymargin: [5,3], $
	   select: [-1,-1,-1,-1], $
	   win1: [0L,-1], $
	   win2: [0L,-1], $
	   pickx: 0L, $
	   picky: 0L $
	}

  WIDGET_CONTROL, CALIBRA_PICK1D, SET_UVALUE=info
  WIDGET_CONTROL, CALIBRA_PICK1D, /REALIZE

  XMANAGER, 'CALIBRA_PICK1D', CALIBRA_PICK1D
END
