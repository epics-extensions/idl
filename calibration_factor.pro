
PRO CALIBRAPDMENU3_Event, Event
  WIDGET_CONTROL,Event.Top,GET_UVALUE=info

  CASE Event.Value OF


  'File.Open...': BEGIN
     file = dialog_pickfile(get_path=p,GROUP=Event.top,/must_exist,$
		path=info.path, $
		title='Read Calibration File')
	if file eq '' then return
	found = findfile(file)
	if found(0) ne '' then begin
		u_openr,unit,file,/XDR
		u_read,unit,values
		u_close,unit
		if n_elements(values) eq 0 then begin
			res = dialog_message('Wrong type of file picked!!!',/Error)
			return
		end
		info.vector = values
		WIDGET_CONTROL,info.table,set_value=transpose(values)
		info.path = p
		info.file = file
  		WIDGET_CONTROL, info.base, SET_UVALUE=info
	end
    END

  'File.Done': BEGIN
	WIDGET_CONTROL,info.base,/DESTROY
	return
    END
  'File.Save as...': BEGIN
     file = dialog_pickfile(get_path=p,GROUP=Event.top, $
		path=info.path, $
		title='Write Calibration File')
	if file ne '' then begin
	found = findfile(file)
	if found(0) ne '' then begin
		str = ['It is going to over-write the', file, 'Are you sure?']
		res = dialog_message(str,/question)
		if res eq 'No' then return 
	end
		WIDGET_CONTROL,info.table,GET_VALUE=v
		info.vector = transpose(v)
		u_openw,unit,file,/XDR
		u_write,unit,info.vector
		u_close,unit
		info.path = p
		info.file = file
  		WIDGET_CONTROL, info.base, SET_UVALUE=info
	end
    END
  'File.Save': BEGIN
	if info.file ne '' then begin
	found = findfile(info.file)
	if found(0) ne '' then begin
		WIDGET_CONTROL,info.table,GET_VALUE=v
		info.vector = transpose(v)
		u_openw,unit,info.file,/xdr
		u_write,unit,info.vector
		u_close,unit
  		WIDGET_CONTROL, info.base, SET_UVALUE=info
	end
	endif else begin
		res = dialog_message('Error: new calibration file name is required',/Error)
		return
	end
    END
  ENDCASE

END

PRO CALIBRA_deleteEqu,Event
  WIDGET_CONTROL,Event.Top,GET_UVALUE=info
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

	WIDGET_CONTROL,info.base3,/DESTROY
	info.oper = 0
	info.method = 0
	info.oper_ids = 0L
	info.meth_ids = 0L
	info.base3 = 0L
  	WIDGET_CONTROL, info.base, SET_UVALUE=info
END

PRO CALIBRA_createEqu,Event
  WIDGET_CONTROL,Event.Top,GET_UVALUE=info
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  BASE3 = WIDGET_BASE(info.base, $
      ROW=1, /scroll, x_scroll_size=500, $
      MAP=1, $
      UVALUE='BASE3')
  info.oper_ids(0) = WIDGET_DROPLIST(BASE3,value=info.factors, $
		UVALUE='CALIBRA_OPER0')

	for i=1,info.no do begin
  info.meth_ids(i) = WIDGET_DROPLIST(BASE3,value=info.methods, $
		UVALUE=info.meth_uvalue(i))
  info.oper_ids(i) = WIDGET_DROPLIST(BASE3,value=info.values, $
		UVALUE=info.oper_uvalue(i))
	end
	info.base3 = BASE3
	info.oper = 0
	info.method = 0
  	WIDGET_CONTROL, info.base, SET_UVALUE=info

	CALIBRA_printEqu,Event
END

PRO CALIBRA_printEqu,Event
WIDGET_CONTROL,Event.Top,GET_UVALUE=info

	; print operator method ids
	str = info.factors(info.oper(0))
	for i=1,info.no do begin
		str = str + info.methods(info.method(i)) + info.values(info.oper(i))
	end

	info.equa_str = str
	WIDGET_CONTROL,info.equation,SET_VALUE=str
	WIDGET_CONTROL,Event.Top,SET_UVALUE=info
;print,'operant id:',info.oper(0:info.no)
;print,'method  id : ',info.method(0:info.no)
END

PRO CALIBRA_asciiReport,Event,xvector=xvector,yvector=yvector
  WIDGET_CONTROL,Event.Top,GET_UVALUE=info

filename = 'calib.ascii'
if strlen(info.classname) gt 1 then filename = info.classname+'.calib.ascii'
sz = size(info.image_final)

xdim = sz(1)
xa = indgen(xdim)
if keyword_set(xvector) then xa = xvector

format = info.format 

f2 = str_sep(strmid(format,1,strlen(format)-1),'.')
f2 = 'I'+f2(0)

; 1D scan
if sz(0) eq 1 then begin
	openw,unit, filename ,/get_lun
	printf,unit,'; Title: '+info.title +'  From: '+info.inpath + info.classname
	printf,unit,'; Calibration  Equation : '+info.equa_str
	printf,unit,'; ---------' +info.factors(info.oper(0))+' = '+ strtrim(info.vector(info.oper(0)),2)
	printf,unit,';  I         Xvalues         Yvalues'
	for i=0,xdim-1 do begin	
	printf,unit,i,xa(i),info.image_final(i)
	end
	close,unit
end

; 2D image
if sz(0) eq 2 then begin

	ydim = sz(2)

	ya = indgen(ydim)
	if keyword_set(yvector) then ya = yvector

	format_1 = '('+format+',I5,'+strtrim(ydim,2)+'('+format+'))'
	openw,unit, filename ,/get_lun
	printf,unit,'; Title: '+info.title +'  From: '+info.inpath + info.classname 
	printf,unit,'; Calibration  data ('+strtrim(xdim,2) +','+strtrim(ydim,2)+') = '+info.equa_str
	printf,unit,'; ---------' +info.factors(info.oper(0))+' = '+ strtrim(info.vector(info.oper(0)),2)
        format_0 = '(";          (yvalues)",'+ '5000('+format+',:))'
	printf,unit,format=format_0,ya
	format_0 = '(";               \ Y",'+strtrim(ydim,2)+'('+f2+'),/,";              X \" )'	
	printf,unit,format=format_0,indgen(ydim)
	printf,unit,';     (xvalues)'
	for i=0,xdim-1 do begin	
	printf,unit,format=format_1,xa(i),i,info.image_final(i,0:ydim-1)
	end
	close,unit
end

;    move to Calib directory	

	textdata = CW_TERM(info.base, $
                TITLE='Calibration Text Window', BG_NAMES='Save As...', $
                FILENAME=filename, RENAME=info.path+filename, $
                 XSIZE=80, YSIZE=20, /SCROLL)

	
END


PRO CALIBRA_Event, Event
  WIDGET_CONTROL,Event.Top,GET_UVALUE=info
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  ; Event for CALIBRAPDMENU3
  'CALIBRAPDMENU3': CALIBRAPDMENU3_Event, Event

  'CALIBRA_FIELD3': BEGIN
  	WIDGET_CONTROL, info.accept, SENSITIVE=1
  	WIDGET_CONTROL, info.calibroi, SENSITIVE=0
  	WIDGET_CONTROL, info.pick1d, SENSITIVE=0
  	WIDGET_CONTROL, info.text, SENSITIVE=0
	WIDGET_CONTROL,Event.Id,GET_VALUE=no
	; at most 5 terms allowed
	if no gt 0 and no lt 15 and no ne info.no then info.no = no
  	WIDGET_CONTROL, info.base, SET_UVALUE=info
	if info.base3 ne 0L then CALIBRA_deleteEqu,Event
	CALIBRA_createEqu,Event
      END

  'CALIBRA_EDIT': BEGIN
      END

  'CALIBRA_GETVEC': BEGIN
	if info.dim eq 2 then begin
  	WIDGET_CONTROL, info.calibroi, SENSITIVE=1
  	WIDGET_CONTROL, info.pick1d, SENSITIVE=1
	end
  	WIDGET_CONTROL, info.text, SENSITIVE=1
  	WIDGET_CONTROL, info.base3, SENSITIVE=1
	WIDGET_CONTROL,info.equation, get_value=str
	if str(0) eq ' ....' then begin
		res=dialog_message(['You need to enter the # of Opers: field',$
		'first and followed with CR to activate the',$
		'droplist area first.'],/info)
		return
	end
	info.equa_str = str(0)
	WIDGET_CONTROL,info.table, get_value=v
	info.vector = transpose(v) 
  	WIDGET_CONTROL, info.base, SET_UVALUE=info

	CALIBRA_printEqu, Event

; check for missing detector data

	if info.oper(1) ge info.height then begin
	r = dialog_message('Error: no data defined for D'+strtrim(info.oper(1)+1,2),/error)
	return
	end

	factor = info.vector(info.oper(0))
	if info.dim eq 2 then $
	temp = info.image_array(*,*,info.oper(1))

	if info.dim eq 1 then $ 
	temp = info.image_array(*,info.oper(1))

	; 1st operation

	if info.method(1) eq 0 then $ 		;'*'
	temp = factor * temp 
		
	if info.method(1) eq 1 then $ 		;'/'
	temp = factor / temp 

	if info.method(1) eq 2 then $ 		;'+'
	temp = factor + temp 

	if info.method(1) eq 3 then $ 		;'-'
	temp = factor - temp 

	if info.no gt 1 then begin
	for i=2,info.no do begin
	if info.oper(i) ge info.height then begin
	r = dialog_message('Error: no data defined for D'+strtrim(info.oper(i)+1,2),/error)
	return
	end
		if info.dim eq 1 then begin
		if info.method(i) eq 0 then $	 	;'*'
		temp = temp * info.image_array(*,info.oper(i)) 
		
		if info.method(i) eq 1 then $	 	;'/'
		temp = temp / info.image_array(*,info.oper(i)) 
		
		if info.method(i) eq 2 then $	 	;'+'
		temp = temp + info.image_array(*,info.oper(i)) 
		
		if info.method(i) eq 3 then $	 	;'-'
		temp = temp - info.image_array(*,info.oper(i)) 
		end
		if info.dim eq 2 then begin
		if info.method(i) eq 0 then $	 	;'*'
		temp = temp * info.image_array(*,*,info.oper(i)) 
		
		if info.method(i) eq 1 then $	 	;'/'
		temp = temp / info.image_array(*,*,info.oper(i)) 
		
		if info.method(i) eq 2 then $	 	;'+'
		temp = temp + info.image_array(*,*,info.oper(i)) 
		
		if info.method(i) eq 3 then $	 	;'-'
		temp = temp - info.image_array(*,*,info.oper(i)) 
		end
	end
	end
	
	if info.dim eq 1 then begin
	xarr = info.xv
	plot1d,xarr,temp,title=info.equa_str
	end
	if info.dim eq 2 then begin
	xarr = info.xv
	yarr = info.yv
	plot2d,temp,xarr=xarr,yarr=yarr,title=info.equa_str
	end
	info.image_final = temp
  	WIDGET_CONTROL, info.base, SET_UVALUE=info
	
      END
  'CALIBRA_DEFAULT': BEGIN
	vector = make_array(15,value=1.)
	info.vector = vector
	info.file = ''
	WIDGET_CONTROL,info.table, set_value=transpose(vector)
  	WIDGET_CONTROL, info.base, SET_UVALUE=info
      END
  'CALIBRA_CLOSE': BEGIN
	WIDGET_CONTROL,Event.top,/DESTROY
      END
  'CALIBRA_HELP': BEGIN
	calibration_help1
      END
  'CALIBRA_ASCII': BEGIN
	CALIBRA_asciiReport,Event,xvector=info.xv,yvector=info.yv
      END
  'CALIBRA_ROI2D': BEGIN
	rptfile = info.roipath + info.classname + '_roi.rpt'
	roifile = info.roipath + info.classname + '_roi.xdr'

	h_annote = ['SCANSEE FILE: '+info.inpath + info.classname + info.title,$
		'        CALIBRATION EQU: '+ info.equa_str+ $
		'   where '+ info.factors(info.oper(0))+' = '+ $
		strtrim(info.vector[info.oper(0)],2)]
		  
	scan2d_roi,info.image_final,info.xv,info.yv,rptfile=rptfile,roifile=roifile, $
		header=h_annote,group=Event.top,_extra=e	
      END
  'CALIBRA_PICK1D': BEGIN
	CALIBRA_pick1d,info.image_final,xa=info.xv,ya=info.yv, $
		TITLE=info.equa_str, GROUP=Event.top
      END

  ELSE: BEGIN
	CALIBRA_findID,EV,'CALIBRA_OPER',id
	if id ge 0 then begin
           info.oper(id) = Event.index
	   if id eq 0 then begin
;		res = WIDGET_INFO(info.oper_ids(1))
		WIDGET_CONTROL,info.oper_ids(1),SET_DROPLIST_SELECT=Event.index
		info.oper(1) = Event.index
	   end
  	   WIDGET_CONTROL, info.base, SET_UVALUE=info
	   CALIBRA_printEqu,Event
	end
	CALIBRA_findID,EV,'CALIBRA_METHOD',id
	if id ge 0 then begin
           info.method(id) = Event.index
  	   WIDGET_CONTROL, info.base, SET_UVALUE=info
	   CALIBRA_printEqu,Event
	end
	END
  ENDCASE

END

PRO calibration_help1
	str = [  $
	'PROCEDURE  STEPS  For IMAGE CALIBRATION ', $
	'       Load in & modify the calibration factor file',$
	'       Type in the desired # of math Opers , then press the CR ', $
	'       Use the droplists to define the calibration equation', $
	'       Press the "Accept & ReCalc..." button to get new result', $
	'       Press the "ASCII..." button to view the ASCII file', $
	'       Press the "Pick1D..." button to get various row/column 1D plots',$
	'----------------------------------------------------', $
	'Table Area    -  15 factors are modifiable by the user', $
	'                 Calibration factors can be loaded in or saved ',$
	'                 thru File  menu', $
	'# of Opers:   -  specifies the number of math opeations desired', $
	'                 Entered desired number (<15)  followed with CR,', $
	'                 ACTIVATES the droplist area.',$
	' ....         -  displays the resultant calibration function',$
	'Droplist Area -  specifies the mathmatical operations on various',$
	'                 detector images and updates the resultant function', $
	'                 The mathmatical operations is performed sequentially',$
	'Done          -  closes the calibration dialog',$
	'Help...       -  pops up this help message', $
	'Default       -  resets all the factors to 1 ', $
	'Accept & ReCalc  - accepts the function and performs the calibration',$
	'                   calculations, and pops up the resultant 2D image',$
	'ASCII...      -  pops up the textual window for calibrated results', $
	'                 and provides the option of save results in a permanent file', $	
	'ROI2D...      -  pops up the 2D ROI window for calibrated results', $
	'Pick1D...     -  pops up the Pick1D window for calibrated results', $
	'                 and provides various row/column 1D plots'$	
	]
	res = dialog_message(str,/info,title='HELP on CALIBRATION')
END

PRO CALIBRA_findID,EV,substring,id
; eg substring='CALIBRA_OPER' or 'CALIBRA_METHOD'
	id = -1
	len1 = strlen(EV)
	len0 = strlen(substring)
	if len1 lt len0 then return
	oper_id = strpos(EV,substring)
	if oper_id ge 0 then begin
		id = fix(strmid(EV,len0,len1-len0))
	end 
END


PRO calibration_factor,image_array,id_def,dvalues=dvalues, no_field=no_field, GROUP=Group,title=title,xv=xv,yv=yv,inpath=inpath,classname=classname,format=format 
;+
;
; NAME:
;       CALIBRATION_FACTOR
;
; PURPOSE:
;       This programs allows the user to flexibly define the calibration
;       function and perform 2D image calibration for a given set of
;       2D scan images. 
;
; CALLING SEQUENCE:
;       CALIBRATION_FACTOR,Image_array,Id_def [,Xv=xv] [,Yv=yv] 
;		[,Dvalues=dvalues] [, No_field=no_field] 
;                [,Inpath=inpath] [,Classname=classname] 
;                [,Title=title] [, GROUP=Group] 
;
; ARGUMENTS:
;   IMAGE_ARRAY  -  Specify the 2D image array
;                   IMAGE_ARRAY[WIDTH,HEIGHT,15]
;   ID_DEF[15]   -  Specify the vector of detector indicator definition
;                   0 not defined, 1 defined
;
; KEYWORD:
;   DVALUES[15]  - Specify the vector of multiplication factors for detectors
;                  default to 1.
;   XV[WIDTH]    - Specify the vector of real X position values
;                  default to index array
;   YV[HEIGHT]   - Specify the vector of real Y position values
;                  default to index array
;   NO_FIELD     - Specify the desired number of sequential image operations
;                  in the calibration function, default to 1
;   TITLE        - Specify the calibration window title description 
;                  default to 'Calibration'
;   CLASSNAME    - Specify the prefix classname for calibration file to be saved
;                  default to ''
;                  For uniqueness use the raw (or source) data file name 
;                  without path as the classname
;   INPATH       - Specify the input file path directory
;                  default to ''
;   GROUP        - Specify the parent Group widget ID, destroy of the parent
;                  widget resulting the destroy of this calibration program 
;
; EXAMPLE:
;    Following example calls the calibration program with known image_array,
;    id_def, xv, yv 
;
;         Calibration_factor,image_array,id_def,xv=xv,yv=yv
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Jan 19, 1999.
;       xx-xx-xxxx      comment
;-

if XRegistered('CALIBRA') then return 

if n_params() eq 0 then begin
	str = [ 'Usage: calibration_factor, Image_array, id_def', $
		' ',$
		' Image_array(width,height,15) - required input image array',$
		'   width         - image X dimension', $
		'   height        - image Y dimension', $
		' Id_def(15)      - the detector image presence indicator', $
		'                   0 not defined, 1 defined',$
		'' $
	]
	res = dialog_message(str,/ERROR)
	return
end

if n_elements(image_array) eq 0 then begin
	ret = dialog_message('No image array found !!',/ERROR)
	return
end

  if keyword_set(classname) eq 0 then classname ='' 
  if keyword_set(inpath) eq 0 then inpath ='' 

  path = inpath
   len = strlen(path)
   if len gt 1 and strmid(path,len-1,1) ne !os.file_sep then path=path+!os.file_sep
	catch,error_status
	if error_status ne 0 then begin
		cd,current=p	
		path = p+!os.file_sep
	roipath = path + 'ROI'+!os.file_sep
	calpath = path + 'CALIB'+!os.file_sep
	goto,step2
	end

	fn = path + '.tmp'
	openw,1,fn
	close,1

	roipath = path + 'ROI'+!os.file_sep
	calpath = path + 'CALIB'+!os.file_sep

  step2:

  titledesc = 'Calibration'
  if keyword_set(title) then titledesc = title

  fmt = 'G15.8'
  if keyword_set(format) then fmt = format

  sz = size(image_array)
  if sz(0) eq 3 then begin
	image_final = 0*image_array(*,*,0)
	dim=2
	end
  if sz(0) eq 2 then begin
	image_final = 0*image_array(*,0)
	dim=1
	end

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  CALIBRA = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, title='Calibration - '+titledesc, $
      MAP=1, $
      UVALUE='CALIBRA')

  BASE2 = WIDGET_BASE(CALIBRA, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE2_0 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2_0')

  MenuDesc829 = [ $
      { CW_PDMENU_S,       3, 'File' }, $ ;        0
        { CW_PDMENU_S,       0, 'Open...' }, $ ;        1
        { CW_PDMENU_S,       0, 'Save as...' }, $ ;        2
        { CW_PDMENU_S,       0, 'Save' }, $  ;      3
        { CW_PDMENU_S,       2, 'Done' } $  ;      3

  ]
  CALIBRAPDMENU3 = CW_PDMENU( BASE2_0, MenuDesc829, /RETURN_FULL_NAME, $
      UVALUE='CALIBRAPDMENU3')
  help = WIDGET_BUTTON(BASE2_0,value='Help...',UVALUE='CALIBRA_HELP')
  close = WIDGET_BUTTON(BASE2_0,value='Done',UVALUE='CALIBRA_CLOSE')

  LABEL3 = WIDGET_LABEL( BASE2, $
      UVALUE='LABEL3', $
      VALUE='Calibration Factor')

  rlabels = make_array(15,/string)
  rvalues = make_array(15,/float,value=1.)

  if keyword_set(dvalues) then rvalues(0:n_elements(dvalues)-1) = dvalues

  for i=0,14 do begin
  	rlabels(i) = '  F'+ strtrim(i+1,2)
  end
  table3 = widget_table(BASE2,value=transpose(rvalues), $
		row_labels=rlabels,row_heights=15, $
		column_labels='', $
		/editable,UVALUE='CALIBRA_EDIT')

  BASE4 = WIDGET_BASE(CALIBRA, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE4')

  default = WIDGET_BUTTON(BASE4,value='Default',UVALUE='CALIBRA_DEFAULT')
  accept = WIDGET_BUTTON(BASE4,value='Accept & ReCalc...',UVALUE='CALIBRA_GETVEC')
  ASCII = WIDGET_BUTTON(BASE4,value='ASCII...',UVALUE='CALIBRA_ASCII')
  calibroi = WIDGET_BUTTON(BASE4,value='ROI2D...',UVALUE='CALIBRA_ROI2D')
  pick1d = WIDGET_BUTTON(BASE4,value='Pick1D...',UVALUE='CALIBRA_PICK1D')

  BASE5 = WIDGET_BASE(CALIBRA, $
      ROW=1, MAP=1, $
      UVALUE='BASE5')

 no = 1
 if keyword_set(no_field) then no = no_field
 FIELD3 = CW_FIELD( BASE5,VALUE=no, $
      ROW=1, $
      INTEGER=1, xsize=2, $
      RETURN_EVENTS=1, $
      TITLE='# of Opers:', $
      UVALUE='CALIBRA_FIELD3')

 equation = WIDGET_LABEL(BASE5,value=' .... ', /dynamic_resize, $
	 UVALUE='CALIBRA_EQU')

  methods=['*','/','+','-']
  values = ['D1','D2','D3','D4','D5','D6','D7','D8', $
        'D9','D10','D11','D12','D13','D14','D15']
  factors = ['F1','F2','F3','F4','F5','F6','F7','F8', $
        'F9','F10','F11','F12','F13','F14','F15']

  oper_ids = make_array(15,/long,value=0L)
  meth_ids = make_array(15,/long,value=0L)

  sz = size(image_array)

  if n_elements(xv) eq 0 then xv = indgen(sz(1))
  if n_elements(yv) eq 0 then yv = indgen(sz(2))

  info = { base: CALIBRA, $
	   table: table3, $
	   base3: 0L, $
	   accept: accept, $
	   calibroi: calibroi, $
	   pick1d: pick1d, $
	   text: ASCII, $
	   field_id : FIELD3, $
	   equation : equation, $
	   equa_str : '', $
	   title : titledesc, $
	   no : no, $
	   inpath:inpath, $        ; data dir
	   classname:classname, $  ; data file name
	   roipath: roipath, $	   ; ROI file directory
	   path: calpath, $	   ; calibration file directory
	   file:'', $		   ; calibration factor file name
	   format: fmt, $
	   factors : factors, $
	   values : values, $
	   methods : methods, $
	   meth_uvalue : make_array(15,/string), $
	   oper_uvalue : make_array(15,/string), $
	   oper_ids : make_array(15,value=0L), $   ; oper_ids, $
	   meth_ids : make_array(15,value=0L), $   ; meth_ids, $
	   oper: make_array(15,/int,value=0), $
	   method: make_array(15,/int,value=0), $
	   vector: rvalues, $
	   id_def: id_def, $
	   xv: xv, $
	   yv: yv, $
	   dim:dim, $
	   width: sz(1), $
	   height: sz(2), $
	   image_final: image_final, $
	   image_array: image_array $
	}

  for i=0,14 do begin
	info.meth_uvalue(i) = 'CALIBRA_METHOD'+ strtrim(i,2)
	info.oper_uvalue(i) = 'CALIBRA_OPER'+ strtrim(i,2)
  end

  BASE3 = WIDGET_BASE(CALIBRA, $
      ROW=1, /scroll, x_scroll_size=500, $
      MAP=1, $
      UVALUE='BASE3')
   info.base3 = base3

  info.oper_ids(0)= WIDGET_DROPLIST(BASE3,value=factors, $
		UVALUE=info.oper_uvalue(0))
  for i=1,no do begin
	info.meth_ids(i) = WIDGET_DROPLIST(BASE3,value=methods, $
			UVALUE=info.meth_uvalue(i))
	info.oper_ids(i) = WIDGET_DROPLIST(BASE3,value=values, $
			UVALUE=info.oper_uvalue(i))
  end
  WIDGET_CONTROL, info.accept, SENSITIVE=0
  WIDGET_CONTROL, info.text, SENSITIVE=0
  WIDGET_CONTROL, info.calibroi, SENSITIVE=0
  WIDGET_CONTROL, info.pick1d, SENSITIVE=0
  WIDGET_CONTROL, info.base3, SENSITIVE=0

  WIDGET_CONTROL, CALIBRA, SET_UVALUE=info
  WIDGET_CONTROL, CALIBRA, /REALIZE


	found = findfile(roipath,count=ct)
	if ct eq 0 then spawn,!os.mkdir+ ' '+roipath
	found = findfile(calpath,count=ct)
	if ct eq 0 then spawn,!os.mkdir+ ' '+calpath
  res = dialog_message([ $
	'First type in desired # of Opers in Calibrations window,', $
	'then followed with CR to activate calibration buttons.'],title='Calibration-info',/info)
	
  XMANAGER, 'CALIBRA', CALIBRA
END