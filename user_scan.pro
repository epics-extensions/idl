
PRO user_scan_getData,st
COMMON USERSCAN,user_scan_state

	sz = size(user_scan_state) 
	if sz(0) eq 0 then return

	widget_control,st.pvWID(0),get_value=pv1
	widget_control,st.pvWID(1),get_value=pv2
	st.pv = [pv1,pv2]
	data = make_array(4,st.nscan)
	for i=0,st.nscan-1 do begin
	widget_control,st.fields(0,i),get_value=xl
	widget_control,st.fields(1,i),get_value=xr
	widget_control,st.fields(2,i),get_value=yl
	widget_control,st.fields(3,i),get_value=yr
	data(0,i) = xl
	data(1,i) = xr
	data(2,i) = yl
	data(3,i) = yr
	end

	user_scan_state.pv = st.pv
	user_scan_state.num = st.nscan
	*user_scan_state.data = data

print,*user_scan_state.data
print,user_scan_state
END

PRO after_sys_scan,file=file,onoff=onoff
COMMON USERSCAN,user_scan_state

	sz = size(user_scan_state)
	if sz(0) eq 0 then return

	; get autoscan # and xl,xr,yl,yr scan ranges

	if keyword_set(onoff) then user_scan_state.onoff=onoff
	if user_scan_state.onoff eq 1 then begin
	user_scan_data = *user_scan_state.data
	line = user_scan_state.num
	user_scan_no = user_scan_state.id

	if user_scan_state.id ge user_scan_state.num then begin
		print,'No more after_sys_scan'
		user_scan_state.onoff = 0
		return
	end
	   	x = user_scan_data(*,user_scan_state.id)
	   	print,user_scan_state.id,x
		pv = user_scan_state.pv

		; check whether 2D scan is going on 

		r = cagetarray(pv(1)+'.EXSC',pd)
		if r ne 0 then begin
		 res = dialog_message(['Invalid SCAN PV names:' ,'',pv],/error)
		 return
		end
		if pd(0) eq 1 then return

		; 2D scan is not going on 

		user_scan_autoscan,pv=pv,x
	   	user_scan_state.id = user_scan_state.id + 1

	end

	
END

PRO user_scan_stopscan
COMMON USERSCAN,user_scan_state
	sz = size(user_scan_state)
	if sz(0) eq 0 then return

	user_scan_state.onoff = 0

	pv = user_scan_state.pv
	st = 'caput ' + pv(1)+'.EXSC  0'
	spawn,st,result
	print,result

END

PRO user_scan_autoscan,x,pv=pv
	if (x(0) eq x(1)) and (x(2) eq x(3)) then begin
		print,'xranges: ',x(0),x(1)
		print,'yranges: ',x(2),x(3)
		return
	end
	if n_elements(pv) eq 2 then begin

	; set scan ranges  x=[xl,xr,yl,yr]
	v = double(transpose(x))
	names = [pv(0)+'.P1SP', pv(0)+'.P1EP', pv(1)+'.P1SP', pv(1)+'.P1EP']
	r = caputarray(names,v)
	r = cagetarray(names,v1)


;	names = pv(0)+'.P1SP' + ',' + pv(0)+'.P1EP' + ',' $
;		+ pv(1)+'.P1SP' + ',' + pv(1)+'.P1EP' 
;	v = strtrim(x,2)
;	values = v(0) + ',' + v(1) + ',' +v(2) + ',' +  v(3)
;	st = 'caput '+ names +' ' + values
;	print,st
;	spawn,st,result
;	print,'result=',result

;	wait,10

	; invoke 2D scans
;	r = caputarray(pv(1)+'.EXSC', double(1))
;	r = cagetarray(pv(1)+'.EXSC',pd)
;	print,'pd=',pd

	st = 'caput ' + pv(1)+'.EXSC  1'
	spawn,st,result
	print,result

	end
END

PRO user_scan_readData,file=file,error=error
COMMON USERSCAN,user_scan_state

filename = 'user_scan.dat'
if keyword_set(file) then filename=file

	sz = size(user_scan_state)
if sz(0) eq 0 then begin
user_scan_state = { $
	num:	 0, $
	pv: ['',''], $
	data:	 ptr_new(/allocate_heap), $
	id:	 0, $
	onoff :	 0, $
	file: 	 filename }
endif else begin
	user_scan_state.num = 0 
	user_scan_state.id = 0
	user_scan_state.onoff = 0
	*user_scan_state.data = 0
	user_scan_state.pv = ['',''] 
	user_scan_state.file = filename
end


	found = findfile(filename,count=ct)

	if ct eq 0 then begin
		error = -1	
		return
	endif else  begin

	line=0
	openr,1,filename
	s = ''
	while not eof(1) do begin
	readf,1,s
	str = strsplit(s,' ',/extract)
	if n_elements(str) eq 4 then begin
	x = float(str)
	if line eq 0 then user_scan_data = x else $
	user_scan_data = [user_scan_data,x]
	line=line+1
	endif else pv = str 
	end
	close,1
	user_scan_no = 0
	user_scan_data = reform(user_scan_data,4,line)
	end

	user_scan_state.pv = pv 
	user_scan_state.num = line
	*user_scan_state.data = user_scan_data
END

FUNCTION user_scan_fields,parent,nscan,file,fileWID,pv=pv
  nm = ['uid:scan1','uid:scan2']
  if n_elements(pv) eq 2 then nm = pv

  if n_elements(nscan) eq 0 then nscan = 1

  fieldWID = make_array(4,nscan,value=0L)

  st = {file: file, $
	pv: nm, $
	nscan:nscan, $
	parent:parent, $
	pvWID: [0L,0L], $
	fileWID : fileWID, $
	base: 0L, $
	fields:fieldWID}

  BASE3 = WIDGET_BASE(parent, $
      COLUMN=1, y_scroll_size=500, /scroll, $
      MAP=1, x_scroll_size=500, $
      UVALUE='BASE3')
  BASE3_1 = WIDGET_BASE(BASE3, $
      ROW=1, /frame, $
      MAP=1, $
      UVALUE='BASE3_1')
  pv1 = CW_FIELD( BASE3_1,VALUE=st.pv(0), $
      TITLE='Scan1 PV:', /return_events, $
      UVALUE='USER_SCAN_PV1')
  pv2 = CW_FIELD( BASE3_1,VALUE=st.pv(1), $
      TITLE='Scan2 PV:', /return_events, $
      UVALUE='USER_SCAN_PV2')

  for i=0,nscan-1 do begin

  BASE4 = WIDGET_BASE(BASE3, $
      COLUMN=1, /frame, $
      MAP=1, $
      UVALUE='BASE4')
  label_i = widget_label(BASE4,value='Auto Scan #' +string(i+1),/align_left)

  BASE4_1 = WIDGET_BASE(BASE4, $
      /ROW, MAP=1, UVALUE='BASE4_1')
  BASE4_2 = WIDGET_BASE(BASE4, $
      /ROW, MAP=1, UVALUE='BASE4_2')
  fieldWID(0,i) = CW_FIELD( BASE4_1,VALUE='', $
      ROW=1, $
      FLOAT=1, $
      TITLE='Xmin', $
      UVALUE='USER_SCAN_XL')

  fieldWID(1,i) = CW_FIELD( BASE4_1,VALUE='', $
      ROW=1, $
      FLOAT=1, $
      TITLE='Xmax', $
      UVALUE='USER_SCAN_XR')

  fieldWID(2,i) = CW_FIELD( BASE4_2,VALUE='', $
      ROW=1, $
      FLOAT=1, $
      TITLE='Ymin', $
      UVALUE='USER_SCAN_YL')

  fieldWID(3,i) = CW_FIELD( BASE4_2,VALUE='', $
      ROW=1, $
      FLOAT=1, $
      TITLE='Ymax', $
      UVALUE='USER_SCAN_YR')

;  FieldVal1117 = [ $
;    '' ]
;  fieldWID(4,i) = CW_FIELD( BASE4,VALUE=FieldVal1117, $
;      ROW=1, $
;      FLOAT=1, $
;      TITLE='ZL', $
;      UVALUE='USER_SCAN_ZL')

;  FieldVal1182 = [ $
;    '' ]
;  fieldWID(5,i) = CW_FIELD( BASE4,VALUE=FieldVal1182, $
;      ROW=1, $
;      FLOAT=1, $
;      TITLE='ZR', $
;      UVALUE='USER_SCAN_YR')
  end

  st.pvWID = [pv1,pv2]
  st.base = base3
  st.nscan = nscan
  st.fields = fieldWID
  return, st
END

PRO user_scan_populate,st

	found = findfile(st.file,count=ct)
	if ct eq 0 then begin
	r = dialog_message(['File is not found:', st.file, $
		'You need first enter new values and then ', $
		'press the "SaveFile" button'],/error)
	return
	end

	line=0
	openr,1,st.file  ;'user_scan.dat'
	s = ''
	readf,1,s
	str = strsplit(s,' ',/extract)
	if n_elements(str) eq 2 then begin
	widget_control,st.pvWID(0),set_value=str(0)
	widget_control,st.pvWID(1),set_value=str(1)
	end

	while not eof(1) do begin
	readf,1,s
	str = strsplit(s,' ',/extract)
	x = float(str)
	if line lt st.nscan then begin
	widget_control,st.fields(0,line),set_value=x(0)
	widget_control,st.fields(1,line),set_value=x(1)
	widget_control,st.fields(2,line),set_value=x(2)
	widget_control,st.fields(3,line),set_value=x(3)
	line=line+1
	end
	end

	close,1
END

PRO user_scan_clearfld,st
	widget_control,st.pvWID(0),set_value=''
	widget_control,st.pvWID(1),set_value=''
	for i=0,st.nscan-1 do begin
	widget_control,st.fields(0,i),set_value=0
	widget_control,st.fields(1,i),set_value=0
	widget_control,st.fields(2,i),set_value=0
	widget_control,st.fields(3,i),set_value=0
	end
END

PRO user_scan_writeData,st
	openw,1,st.file  ;'user_scan.dat'
	widget_control,st.pvWID(0),get_value=pv1
	widget_control,st.pvWID(1),get_value=pv2
	st.pv = [pv1,pv2]
;	print,st.pv
	printf,1,st.pv
	for i=0,st.nscan-1 do begin
	widget_control,st.fields(0,i),get_value=xl
	widget_control,st.fields(1,i),get_value=xr
	widget_control,st.fields(2,i),get_value=yl
	widget_control,st.fields(3,i),get_value=yr
;	print,i,xl,xr,yl,yr
	x=[xl,xr,yl,yr]
	printf,1,x
	end
	close,1
END

PRO user_scan_help
	str = ['This dialog let user define a set of automatic 2D scan ranges.', $
	'', $
	'Usage: USER_SCAN [,NSCAN=#] [,FILE=file] ', $
	'   where', $
	'       NSCAN - specifies the total number of subsequent 2D scans', $
	'               default to 5', $
	'       FILE  - specifies the filename to be used for storing the', $
	'               2D scan ranges, defualt is "user_scan.dat"', $
	'', $
	'If the autoScan option is set in the scanSee it allows the user to ', $
	'access the automatic 2D scan dialog to define the scan # and ranges', $
	'to start or stop the automatic 2D scans', $
	'','User Interface:', $

	' File...   - pick 2D scan ranges file through file selection dialog ', $
	' filename  - display field for current file picked', $
	' Help...   - shows this help page', $
	' Clear     - clears the current Xmin,Xmax,Ymin,Ymax values', $
	' LoadFile  - this button load 2D scan ranges from file', $
	' SaveFile  - save new 2D ranges the file', $
	' Start AutoScan - start/continue the AutoScan loop', $	
	'                  SaveFile must pressed before Start AutoScan', $
	' Stop AutoScan - stop the AutoScan loop', $	
	' Done      - closes the dialog', $
	'']
	r = dialog_message(str,/info)
END

PRO USER_SCAN_MAIN13_Event, Event
COMMON USERSCAN,user_scan_data

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev
  WIDGET_CONTROL,Event.Top,GET_UVALUE=st,/no_copy
	fieldsWID = st.fields
	nscan = st.nscan
  sz = size(user_scan_data)

  CASE Ev OF 

  'USER_SCAN_PV1': BEGIN
	widget_control,Event.Id,get_value=pv1
	st.pv(0) = pv1
	END
  'USER_SCAN_PV2': BEGIN
	widget_control,Event.Id,get_value=pv1
	st.pv(1) = pv1
	END
  'USER_SCAN_FILEPICK': BEGIN
	f = dialog_pickfile(title='AutoScan Pick File', $
		file='user_scan.dat',filter='*.dat*',get_path=p)
	st.file = f
	widget_control,st.fileWID,set_value=f
	user_scan_populate,st
	END
  'USER_SCAN_FILENAME': BEGIN
	widget_control,st.fileWID,get_value=f
	st.file = f
	user_scan_populate,st
	END
  'USER_SCAN_CLEAR': BEGIN
	user_scan_clearfld,st
 	END
  'USER_SCAN_LOADFILE': BEGIN
	user_scan_populate,st
  	END

  'USER_SCAN_SAVEFILE': BEGIN
	r = dialog_message('Are you sure?',/question)
	if r eq 'Yes' then begin
	user_scan_writeData,st
;	user_scan_readData
	end
      END
  'USER_SCAN_SIMULATE': BEGIN
	user_scan_getData,st
	if sz(0) gt 0 then begin
	r = dialog_message(['It is about to START the automatic 2D scan.', $
	'','Are you sure?'],/question)
	if r eq 'Yes' then begin
	after_sys_scan,onoff=1
	end
	endif else user_scan_populate,st
      END
  'USER_SCAN_STOPSCAN': BEGIN
	if sz(0) gt 0 then begin
	r = dialog_message(['It is about to STOP the automatic 2D scan.', $
	'','Are you sure?'],/question)
	if r eq 'Yes' then begin
	user_scan_stopscan
	end
	end
      END
  'USER_SCAN_HELP': BEGIN
	user_scan_help
      END
  'USER_SCAN_CANCEL': BEGIN
	widget_control,Event.Top,/destroy
	return
      END
  ENDCASE

  WIDGET_CONTROL,Event.Top,SET_UVALUE=st,/no_copy

END



PRO user_scan, pv=pv, nscan=nscan, GROUP=Group,file=file


if n_elements(nscan) eq 0 then nscan = 5
if n_elements(file) eq 0 then file='user_scan.dat'
user_scan_readData,file=file,error=error

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  USER_SCAN_MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
      TITLE='AUTO_USER_SCAN_SETUP', $
      UVALUE='USER_SCAN_MAIN13')

  BASE2 = WIDGET_BASE(USER_SCAN_MAIN13, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE1 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE1')
  BUTTON1 = WIDGET_BUTTON( BASE1, $
      UVALUE='USER_SCAN_FILEPICK', $
      VALUE='File...')
  FIELD_file = CW_FIELD( BASE1,VALUE=file, $
      ROW=1, TITLE=' ', /return_events, xsize=50, $
      UVALUE='USER_SCAN_FILENAME')

  BASE44 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE44')

  BUTTON43 = WIDGET_BUTTON( BASE44, $
      UVALUE='USER_SCAN_HELP', $
      VALUE='Help...')

  BUTTON45 = WIDGET_BUTTON( BASE44, $
      UVALUE='USER_SCAN_CLEAR', $
      VALUE='Clear')

  BUTTON44 = WIDGET_BUTTON( BASE44, $
      UVALUE='USER_SCAN_LOADFILE', $
      VALUE='LoadFile')

  BUTTON41 = WIDGET_BUTTON( BASE44, $
      UVALUE='USER_SCAN_SAVEFILE', $
      VALUE='SaveFile')

  BUTTON45 = WIDGET_BUTTON( BASE44, $
      UVALUE='USER_SCAN_SIMULATE', $
      VALUE='Start AutoScan')

  BUTTON46 = WIDGET_BUTTON( BASE44, $
      UVALUE='USER_SCAN_STOPSCAN', $
      VALUE='Stop AutoScan')

  BUTTON42 = WIDGET_BUTTON( BASE44, $
      UVALUE='USER_SCAN_CANCEL', $
      VALUE='Done')

	label = widget_label(BASE2,value="TOTAL # OF AUTOMATIC SCANs"+string(nscan))

     fields = user_scan_fields(BASE2,nscan,file,FIELD_file,pv=pv)

  WIDGET_CONTROL, USER_SCAN_MAIN13, /REALIZE,set_uvalue=fields

  XMANAGER, 'USER_SCAN_MAIN13', USER_SCAN_MAIN13
END



PRO user_scan_init_Event, Event

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev
  WIDGET_CONTROL,Event.top,GET_UVALUE=state

  CASE Ev OF 

  'USER_SCAN_NSCAN': BEGIN
      END
  'USER_SCAN_ACCEPT': BEGIN
	widget_control,state.fldWID,get_value=n
	user_scan,nscan=n,group=state.parent
	widget_control,state.base,/destroy,bad=bad
      END
  ENDCASE
END



PRO user_scan_init, GROUP=Group


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  user_scan_init = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
      MAP=1, $
      TITLE='USER_SCAN', $
      UVALUE='user_scan_init')

  BASE2 = WIDGET_BASE(user_scan_init, $
      COLUMN=1, /frame, $
      MAP=1, $
      UVALUE='BASE2')

  LABEL6 = WIDGET_LABEL( BASE2, $
      UVALUE='LABEL6', $
      VALUE='ENTER TOTAL # OF AUTOMATIC 2D SCANS')

  FieldVal167 = [ $
    '5' ]
  FIELD3 = CW_FIELD( BASE2,VALUE=FieldVal167, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='NSCAN: ', $
      UVALUE='USER_SCAN_NSCAN')

  BUTTON5 = WIDGET_BUTTON( user_scan_init, $
      UVALUE='USER_SCAN_ACCEPT', $
      VALUE='Accept...')

  state = { base:user_scan_init, $
	parent:group, $
	fldWID: FIELD3 $
	}
  WIDGET_CONTROL, user_scan_init, /REALIZE, set_uvalue=state

  XMANAGER, 'user_scan_init', user_scan_init
END
