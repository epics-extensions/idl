;@sscan.pro
;@os.init

PRO overlay_1d_calc,mdafile_array,pick,debug=debug,Event
	nf = n_elements(mdafile_array)
	print,'pick det=',pick

; set y,y buff
	nmax = 2000
	x_data = make_array(nmax,nf) ;,value=!values.f_NaN)
	y_data = make_array(nmax,nf) ;,value=!values.f_NaN)
	max_npt=0
	nfound = 0

	for i=0,nf-1 do begin
	file = mdafile_array(i)

	sn = read_scan(file,SSD,pickDet=15)
;	if n_elements(SSD.rank) le 2 then begin
	dim = SSD.rank
	id_def = SSD.id_def
	print,strtrim(i,2),'  ',file,'  dim=',dim

if keyword_set(debug) then begin
print,SSD.rank,SSD.npts,SSD.cpt
for j=0,dim-1 do begin
help,*SSD.pa[j]
help,*SSD.da[j]
end
end
	pa = *SSD.pa[dim-1]
	da = *SSD.da[dim-1]
	sz = size(da)
	if sz(1) gt max_npt then max_npt=sz(1)
	if sz(0) eq 2 then begin
	  if pick lt sz(2) then begin
		x_data(0:sz(1)-1,i) = pa(0:sz(1)-1,0)
		y_data(0:sz(1)-1,i) = da(0:sz(1)-1,pick)
		; fill remaining array
		x_data(sz(1):nmax-1,i) = pa(sz(1)-1,0)
		y_data(sz(1):nmax-1,i) = da(sz(1)-1,pick)
		nfound=nfound+1
	  endif else begin
	    print,'   Error: last valid det seq # is : '+strtrim(sz(2)-1,2)
	  end
	endif else begin
	  print,'   Error: No det defined in 1D array !!'
	end
;	end
	free_lun,SSD.lun
	close,SSD.lun,/all
	end
	x_data= x_data(0:max_npt-1,0:nf-1)
	y_data= y_data(0:max_npt-1,0:nf-1)

if keyword_set(debug) then begin
help,x_data,y_data
print,x_data
print,y_data
end

	scanSee_free,SSD

	if nfound gt 0 then $
	plot1d,x_data,y_data,title='DETECTOR '+strtrim(pick,2),group=Event.top else $
	begin
		r = dialog_message('No vector found for seq # '+strtrim(pick,2),/Error)
	end

END

PRO overlay_1d_selectFiles,mdafile_array,nfile=nfile,path=path
; KEYWORD:
; PATH         - specify the directory where xdr files exists
;                At least two xdr files must be selected for image overlaying
; NFILE        - Number of file selected from the directory

cd,current=opath

if keyword_set(path) then opath=path

mdapick:
	mdafile_array = dialog_pickfile(title='Overlay File Pick', $
		path = opath,Filter='*.mda*', $
		get_path=p,/multiple,/must_exist,/read)
	if mdafile_array(0) eq '' then return

	nfile = n_elements(mdafile_array)
	if nfile le 1 then begin
		r = dialog_message('At least two MDA image files must be selected!!!',/Error)
		opath=p
		goto,mdapick
	end
	path = p(0)

       ; write overlay_1d.config
        openw,1,'overlay_1d.config'
        printf,1,path
        close,1
END

PRO overlay_1d_onOpen,Event
	widget_control,Event.top,get_uvalue=overlay_1d_state
	path = overlay_1d_state.path
	pick = overlay_1d_state.pick

	overlay_1d_selectFiles,mdafile_array,nfile=nfile,path=path
	overlay_1d_state.path = path
	widget_control,Event.top,set_uvalue=overlay_1d_state

	if mdafile_array(0) eq '' then return
	wWidget =  Event.top
	textWID = Widget_Info(wWidget, FIND_BY_UNAME='W_TEXT_11')
	widget_control,textWID,set_value=mdafile_array
;	overlay_1d_calc,mdafile_array,pick

END

PRO overlay_1d_onDebug, Event

	widget_control,Event.top,get_uvalue=overlay_1d_state
	if overlay_1d_state.debug then overlay_1d_state.debug=0 else $
	overlay_1d_state.debug=1
	widget_control,Event.top,set_uvalue=overlay_1d_state
END

PRO overlay_1d_onMDAFile, Event
	str = ['The multiple file selection is accomplished by clicking the',$
	'file with the Left Mouse Button (LMB) in the file selection box:','', $
	'       LMB       - select only the picked file', $
	'       CNTL-LMB  - add the picked file to the selection list', $
	'       SHIFT-LMB - add all the files between the last two click files', $
	'                   to the selection list', $
	'Press the OK or Open button accept all the files selected', $
	'']
	r = dialog_message(str,/info)
END

PRO overlay_1d_onHelp, Event
	str = [ $
	'              HELP ON 1D OVERLAY','', $
	'File->Open       - dialog for multiple mda files selection', $
	'File->Debug(On/Off)  - display on/off of picked X,Y vectors', $
	'File->Close      - close the 1D_OVERAY dialog', $
	'Help->Help...    - pop up this help dialog', $
	'Help->mdaFile... - help on multiple file selection', $
	'Curve/Di Seq #   - enter valid curve seq # from 1D data array',$
	'                   e.g. 15 picks the 16th detector from files', $
	'                   At the <CR> invokes the 1D overlay plot', $
	'Mda List Area    - display the selected mda files', $	
	'']
	r = dialog_message(str,/info)
END

PRO overlay_1d_pick, Event
	widget_control,Event.top,get_uvalue=overlay_1d_state
	debug = overlay_1d_state.debug
	widget_control,Event.id,get_value=pick
	overlay_1d_state.pick = pick
	widget_control,Event.top,set_uvalue=overlay_1d_state

	wWidget =  Event.top
	textWID = Widget_Info(wWidget, FIND_BY_UNAME='W_TEXT_11')
	widget_control,textWID,get_value=mdafile_array

	if mdafile_array(0) eq '' then begin
		r = dialog_message('You have to load the mda files in first.',/error)
		return 
	end
	overlay_1d_calc,mdafile_array,pick,debug=debug,Event
END

PRO overlay_1d_run, Event
	widget_control,Event.top,get_uvalue=overlay_1d_state
	debug = overlay_1d_state.debug
	widget_control,overlay_1d_state.seqWID,get_value=pick
	overlay_1d_state.pick = pick
	widget_control,Event.top,set_uvalue=overlay_1d_state

	wWidget =  Event.top
	textWID = Widget_Info(wWidget, FIND_BY_UNAME='W_TEXT_11')
	widget_control,textWID,get_value=mdafile_array

	if mdafile_array(0) eq '' then begin
		r = dialog_message('You have to load the mda files in first.',/error)
		return 
	end
	overlay_1d_calc,mdafile_array,pick,debug=debug,Event
END

PRO overlay_1d_onClose, Event
	widget_control,Event.top,/destroy,bad=bad
END

;-----------------------------------------------------------------

pro OVERLAY_1D_event, Event

  wWidget =  Event.top

  case Event.id of

    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_3'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        overlay_1d_onOpen, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_4'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        overlay_1d_onClose, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_5'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        overlay_1d_onHelp, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_6'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        overlay_1d_onMDAFile, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_MENU_21'): begin
      if( Tag_Names(Event, /STRUCTURE_NAME) eq 'WIDGET_BUTTON' )then $
        overlay_1d_onDebug, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_PICK_13'): begin
        overlay_1d_pick, Event
    end
    Widget_Info(wWidget, FIND_BY_UNAME='W_RUN_1'): begin
        overlay_1d_run, Event
    end
    else:
  endcase

end

pro OVERLAY_1D,path=path, GROUP=wGroup, _EXTRA=_VWBExtra_
        ; read overlay_1d.config
	cd,current=opath
        fd = findfile('overlay_1d.config',count=ct)
        if ct then begin
        openr,1,'overlay_1d.config'
        readf,1,opath
        close,1
        end

	if keyword_set(path) eq 0 then  path=opath

  OVERLAY_1D = Widget_Base( GROUP_LEADER=wGroup, UNAME='OVERLAY_1D'  $
	,/column, TITLE='1D_OVERLAY' $
      ,XOFFSET=5 ,YOFFSET=5  $ ;,SCR_XSIZE=300 ,SCR_YSIZE=219  $
      ,SPACE=3 ,XPAD=3 ,YPAD=3 ,MBAR=OVERLAY_1D_MBAR)

  
  W_MENU_0 = Widget_Button(OVERLAY_1D_MBAR, UNAME='W_MENU_0' ,/MENU  $
      ,VALUE='File')

  
  W_MENU_3 = Widget_Button(W_MENU_0, UNAME='W_MENU_3'  $
      ,VALUE='Open...')

  W_MENU_21 = Widget_Button(W_MENU_0, UNAME='W_MENU_21' , /separator, $
      VALUE='Debug (On/Off)')

  
  W_MENU_4 = Widget_Button(W_MENU_0, UNAME='W_MENU_4' ,/separator, VALUE='Close')
  
  W_MENU_1 = Widget_Button(OVERLAY_1D_MBAR, UNAME='W_MENU_1' ,/MENU  $
      ,VALUE='Help')

  
  W_MENU_5 = Widget_Button(W_MENU_1, UNAME='W_MENU_5'  $
      ,VALUE='Help...')

  
  W_MENU_6 = Widget_Button(W_MENU_1, UNAME='W_MENU_6'  $
      ,VALUE='mdaFile...')

  base3 = widget_base(OVERLAY_1D,/row)
  FieldVal100 = [ $
    '15' ]
  FIELD2 = CW_FIELD( BASE3,VALUE=FieldVal100, $
      ROW=1, $
      INTEGER=1, xsize=6, $
      RETURN_EVENTS=1, $
      TITLE='Curve/Di Seq # (0 based) :', $
	UNAME='W_PICK_13')
  W_RUN_1 = Widget_Button(BASE3, UNAME='W_RUN_1',  $
      VALUE='Run...')

  W_TEXT_11 = widget_text(OVERLAY_1D,value=strarr(15), $
	UNAME='W_TEXT_11',xsize=50,ysize=10,/scroll)

  overlay_1d_state = { path:path, pick:15, debug:0, $
	 	seqWID: FIELD2 }
  widget_control,OVERLAY_1D,set_uvalue=overlay_1d_state

  Widget_Control, /REALIZE, OVERLAY_1D

  XManager, 'OVERLAY_1D', OVERLAY_1D, /NO_BLOCK  

end
