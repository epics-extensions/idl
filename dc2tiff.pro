
PRO HELPMAIN13_Event, Event


  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'TIFF_HELPTEXT': BEGIN
      END
  'TIFF_HELPDONE': BEGIN
	WIDGET_CONTROL,Event.top,/DESTROY
      END
  ENDCASE
END


PRO w_help, text, GROUP=Group


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  HELPMAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, title='TIFF_HELP', $
      MAP=1, $
      UVALUE='HELPMAIN13')

  BASE2 = WIDGET_BASE(HELPMAIN13, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  LABEL3 = WIDGET_LABEL( BASE2, $
      UVALUE='LABEL3', $
      VALUE='Help On dc2tiff')

  if n_elements(text) eq 0 then text=''
  TEXT6 = WIDGET_TEXT( BASE2,VALUE=Text, $
;     FONT='-misc-fixed-medium-r-normal--20-140*',$
	/SCROLL, $
      UVALUE='TIFF_HELPTEXT', $
      XSIZE=60, $
      YSIZE=30)

  BUTTON10 = WIDGET_BUTTON( BASE2, $
      UVALUE='TIFF_HELPDONE', $
      VALUE='Done')


  WIDGET_CONTROL, HELPMAIN13, /REALIZE

  XMANAGER, 'HELPMAIN13', HELPMAIN13
END


PRO tiff_init
COMMON TIFF_BLOCK,tiff_ids,tiff_v2

	ret = obj_valid(tiff_v2)
	if ret then tiff_v2->delete
	obj_clean,tiff_v2
	tiff_v2=0
;	obj_clean,/find
	
	tiff_v2 = obj_new('scan2d')
	tiff_v2->open,tiff_ids.filename
	tiff_v2->readAll,maxno,scanno_last
	tiff_ids.maxno = maxno
	tiff_ids.imageno = 1
	tiff_ids.scanno_last = scanno_last

	tiff_ids.classname = tiff_ids.filename
	pos = rstrpos(tiff_ids.filename,!os.file_sep)
	len = strlen(tiff_ids.filename)
	if pos ge 0 then $
	tiff_ids.classname = strmid(tiff_ids.filename,pos+1,len-pos)

	cd,current=dir
	tiff_ids.tiffpath = dir

END

PRO update_tiffplot
COMMON TIFF_BLOCK,tiff_ids,tiff_v2

	CASE tiff_ids.plottype OF
	0: tiff_ids.viewtype='TV'
	1: tiff_ids.viewtype='SURFACE'
	2: tiff_ids.viewtype='CONTOUR'
	3: tiff_ids.viewtype='SHADE_SURF'
	ELSE:
	ENDCASE

	tiff_v2->view,tiff_ids.imageno,/noplot,type=tiff_ids.viewtype

END

PRO MAIN13_Event, Event
COMMON TIFF_BLOCK,tiff_ids,tiff_v2

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'TIFF_PDMENU127':  TIFF_PDMENU127_Event,Event

  'TIFF_FILE1': BEGIN
	F=dialog_pickfile(filter='*.image',GET_PATH=p,GROUP=Event.Top,/MUST_EXIST,$
		PATH=tiff_ids.path,TITLE='Select Image File',/READ)
	if F eq '' then return
	tiff_ids.path = p
	tiff_ids.filename = f
	WIDGET_CONTROL,tiff_ids.filename_id,SET_VALUE=f
	tiff_init
	WIDGET_CONTROL,tiff_ids.slider_id,SET_SLIDER_MAX=tiff_ids.maxno
	WIDGET_CONTROL,tiff_ids.list_id,SET_VALUE=''
	WIDGET_CONTROL,tiff_ids.tiffpath_id,SET_VALUE=tiff_ids.tiffpath
	WIDGET_CONTROL,tiff_ids.classname_id,SET_VALUE=tiff_ids.classname
      END
  'TIFF_FILE2': BEGIN
	WIDGET_CONTROL,tiff_ids.filename_id,GET_VALUE=filename
	tiff_ids.filename = filename(0)
	tiff_init
	WIDGET_CONTROL,tiff_ids.slider_id,SET_SLIDER_MAX=tiff_ids.maxno
	WIDGET_CONTROL,tiff_ids.list_id,SET_VALUE=''
	WIDGET_CONTROL,tiff_ids.tiffpath_id,SET_VALUE=tiff_ids.tiffpath
	WIDGET_CONTROL,tiff_ids.classname_id,SET_VALUE=tiff_ids.classname
      END

  'TIFF_PLOT': BEGIN
	tiff_ids.plottype = Event.Index
	if tiff_ids.filename eq '' then return
	update_tiffplot
      END

  'TIFF_TYPE': BEGIN
	tiff_ids.type = Event.Value
      END

  'TIFF_LIST': BEGIN
      END
  'SCAN_NUMBER': BEGIN
	WIDGET_CONTROL,tiff_ids.scanno_id,GET_VALUE=scanno
	if tiff_ids.panimagewin ne -1 then wdelete,tiff_ids.panimagewin
	tiff_v2->panimage,scanno,new_win=new_win
	tiff_ids.panimagewin = new_win
	tiff_ids.scanno_current = scanno
      END
  'SCAN_FIRST': BEGIN
	tiff_ids.scanno_current=1
	if tiff_ids.panimagewin ne -1 then wdelete,tiff_ids.panimagewin
	tiff_v2->panimage,1,new_win=new_win
	tiff_ids.panimagewin = new_win
	WIDGET_CONTROL,tiff_ids.scanno_id,SET_VALUE=tiff_ids.scanno_current
      END
  'SCAN_NEXT': BEGIN
	tiff_ids.scanno_current=1 + tiff_ids.scanno_current
	if tiff_ids.panimagewin ne -1 then wdelete,tiff_ids.panimagewin
	tiff_v2->panimage,tiff_ids.scanno_current,new_win=new_win
	tiff_ids.panimagewin = new_win
	WIDGET_CONTROL,tiff_ids.scanno_id,SET_VALUE=tiff_ids.scanno_current
      END
  'SCAN_PREV': BEGIN
	tiff_ids.scanno_current = tiff_ids.scanno_current - 1
	if tiff_ids.panimagewin ne -1 then wdelete,tiff_ids.panimagewin
	tiff_v2->panimage,tiff_ids.scanno_current,new_win=new_win
	tiff_ids.panimagewin = new_win
	WIDGET_CONTROL,tiff_ids.scanno_id,SET_VALUE=tiff_ids.scanno_current
      END
  'SCAN_LAST': BEGIN
	tiff_ids.scanno_current = tiff_ids.scanno_last 
	if tiff_ids.panimagewin ne -1 then wdelete,tiff_ids.panimagewin
	tiff_v2->panimage,tiff_ids.scanno_last,new_win=new_win
	tiff_ids.panimagewin = new_win
	WIDGET_CONTROL,tiff_ids.scanno_id,SET_VALUE=tiff_ids.scanno_current
      END
  'TIFF_FIRST': BEGIN
	if tiff_ids.filename eq '' then return
	tiff_ids.imageno= 1
	WIDGET_CONTROL,tiff_ids.slider_id,SET_VALUE=tiff_ids.imageno
	update_tiffplot
      END
  'TIFF_LAST': BEGIN
	if tiff_ids.filename eq '' then return
	tiff_ids.imageno= tiff_ids.maxno
	WIDGET_CONTROL,tiff_ids.slider_id,SET_VALUE=tiff_ids.imageno
	update_tiffplot
      END
  'TIFF_PREV': BEGIN
	if tiff_ids.imageno le 1 then return
	tiff_ids.imageno=tiff_ids.imageno - 1
	WIDGET_CONTROL,tiff_ids.slider_id,SET_VALUE=tiff_ids.imageno
	update_tiffplot
      END
  'TIFF_NEXT': BEGIN
	if tiff_ids.imageno ge tiff_ids.maxno then return
	tiff_ids.imageno=tiff_ids.imageno + 1
	WIDGET_CONTROL,tiff_ids.slider_id,SET_VALUE=tiff_ids.imageno
	update_tiffplot
      END
  'TIFF_SLIDER': BEGIN
	WIDGET_CONTROL,tiff_ids.slider_id,GET_VALUE=no
	if no eq 0 then return
	tiff_ids.imageno = no
	update_tiffplot
      END
  'TIFF_PANIMAGES': BEGIN
	if tiff_ids.panimagewin ne -1 then wdelete,tiff_ids.panimagewin
	tiff_v2->panimage,new_win=new_win
	tiff_ids.panimagewin = new_win
      END
  'TIFF_LISTAPPEND': BEGIN
	WIDGET_CONTROL,tiff_ids.slider_id,GET_VALUE=no
	if no eq 0 then return
	WIDGET_CONTROL,tiff_ids.list_id,GET_VALUE=lista
	if strtrim(lista(0),2) eq '' then lista=strtrim(no,2) else $
	lista = lista+','+strtrim(no,2)
	WIDGET_CONTROL,tiff_ids.list_id,SET_VALUE=lista
	END
  'TIFF_LISTCLR': BEGIN
	WIDGET_CONTROL,tiff_ids.list_id,SET_VALUE=''
      END
  'TIFF_LISTWHOLE': BEGIN
	if tiff_ids.maxno lt 2 then return
	st = '1-'+strtrim(tiff_ids.maxno,2)
	WIDGET_CONTROL,tiff_ids.list_id,SET_VALUE=st
      END
  'TIFF_DIR': BEGIN
	WIDGET_CONTROL,tiff_ids.tiffpath_id,GET_VALUE=dir
	tiff_ids.tiffpath = dir(0)	
      END
  'TIFF_CLASSNAME': BEGIN
	WIDGET_CONTROL,tiff_ids.classname_id,GET_VALUE=filename
	tiff_ids.classname = filename(0)	
      END
  'TIFF_DONE': BEGIN
      WIDGET_CONTROL,tiff_ids.base,/DESTROY
      wdelete,0
      END
  'TIFF_ASCII': BEGIN
	WIDGET_CONTROL,tiff_ids.list_id,GET_VALUE=lista
	lista = strtrim(lista(0),2)
	if lista ne '' then begin
	parse_num,lista,res,sep1=','
	no = n_elements(res)
	if no le 5 then begin
		for i=0,no-1 do begin
		fileSeqString,res(i),suf0
		texfile = tiff_ids.tiffpath+!os.file_sep+ $
		   tiff_ids.classname+'.'+suf0+'.txt'
		found = findfile(texfile)
		if found(0) ne '' then begin
		xdisplayfile,texfile
		endif else res=dialog_message(['File not fond:',texfile],/error)
		end
	endif else begin
		F=dialog_pickfile(filter='*.image.*txt',GET_PATH=p,GROUP=Event.Top,/MUST_EXIST,$
		PATH=tiff_ids.tiffpath,TITLE='Select Image ASCII File',/READ)
		if F eq '' then return
		xdisplayfile,F	
	end
	end
      END
  'TIFF_ACCEPT': BEGIN
	WIDGET_CONTROL,tiff_ids.list_id,GET_VALUE=lista
	lista = strtrim(lista(0),2)

	if lista ne '' then begin

	parse_num,lista,res,sep1=','
	no = n_elements(res)

	if tiff_ids.type eq 1 then begin	
		for i=0,no-1 do begin
		fileSeqString,res(i),suf0
		tiffile = tiff_ids.tiffpath+!os.file_sep+ $
		   tiff_ids.classname+'.'+suf0+'.tiff'
		tiff_v2->write_tiff,res(i),tiff=tiffile,/TOP2BOTTOM, $
			type=tiff_ids.viewtype
		end
	end
	if tiff_ids.type eq 0 then begin	
		for i=0,no-1 do begin
		fileSeqString,res(i),suf0
		tiffile = tiff_ids.tiffpath+!os.file_sep+ $
		   tiff_ids.classname+'.'+suf0+'.tiff'
		tiff_v2->write_tiff,res(i),tiff=tiffile, $
			type=tiff_ids.viewtype
		end
	end
	if tiff_ids.type eq 2 then begin	
		for i=0,no-1 do begin
		fileSeqString,res(i),suf0
		giffile = tiff_ids.tiffpath+!os.file_sep+ $
		   tiff_ids.classname+'.'+suf0+'.gif'
		tiff_v2->write_gif,res(i),gif=giffile, $
			type=tiff_ids.viewtype
		end
	end
	if tiff_ids.type eq 3 then begin	
	WIDGET_CONTROL,/HOURGLASS
		for i=0,no-1 do begin
		fileSeqString,res(i),suf0
		txtfile = tiff_ids.tiffpath+!os.file_sep+ $
			tiff_ids.classname+'.'+suf0
		tiff_v2->point_lun,res(i)-1
		tiff_v2->read
		tiff_v2->datatotext,outfile=txtfile,/nowin
		end
	end
	endif else begin
		res=dialog_message(['Empty List found !', $
		'You have to enter the Image # list desired, ', $
		'or you can use the "AddList" button ', $
		'to add the Image # to the List first'],/error)
	end
      END
  'TIFF_COLOR': BEGIN
	xloadct, GROUP=Event.top
      END
  'TIFF_HELP': BEGIN
	st=['     Create CATCHER Images in TIFF/GIF/ASCII Form', '', $
	'1. use the File... to pick the image file to be converted', $
	'   Following fields will be updated.', $
	'     Catcher Image File: shows the selected source image file',$
	'     Output Dir/Path:    Output file directory which can be ', $
	'                         changed to a user desired directory', $
	'     Output Classname:   same as the image filename', $
	'', $	
	'2. Select the Image Type TIFF/R-TIFF/GIF/ASCII to be saved', $
	'   Note:  output file will be saved in the output Dir/Path, the', $
	'          filename is constructed from image classname, #, and type',$
	'       --Type-----------Output Filename-----------',$
	'         TIFF/R-TIFF  classname+.#+.tiff   (bottom-top/reverse order)',$
	'         GIF          classname+.#+.gif',$
	'         ASCII        classname+.#+.txt','',$
	'3. Use <<,->,<-,>> buttons to preview the first,next,prev,last Image #', $
 
	'   Drag Image# slider to position the 2D scan # of the image',$
	'   Press the D1...D15 button to view all detectors of the image#',$
	'',$
	'4. List field accepts the comma separated image# to be saved', $
	'   Note: AddList button adds the slider image# to List field', $
	'         Whole_List button sets to complete range of image#','',$
	'5. Use the Accept_List button to generate the output files', $
	'', $
	'6. Use the View ASCII... button to view the ASCII files', $
	'   Note: ASCII file must be generated first. Each image has its', $
	'         own window. If the list exceeds 5 images, the file ',$
	'         selection box will popup.',$
	'', $
	'Each Image # is saved as a seperate file.','', $
	'On solaris system, a user can use Image Viewer to view or print the',$
	'TIFF/GIF... files created by this program.' ]

	w_help,st,GROUP=Event.top
;      res=dialog_message(st,/info)
      END
  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END MAIN13
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO dc2tiff, filename, GROUP=Group
COMMON TIFF_BLOCK,tiff_ids,tiff_v2

device,retain=2,decomposed=0
loadct,39

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  tiff_ids = { base: 0L, $
	filename_id: 0L, $
	filename: '', $
	path: '',$
	list_id:     0L, $
	list: '', $
	tiffpath_id: 0L, $
	tiffpath: '',$
	classname_id:  0L, $
	classname: '', $
	slider_id:   0L, $
	scanno_id:   0L, $
	scanno_last :0, $
	scanno_current :0, $
	maxno: 1, $
	slider: 0, $
	imageno: 1, $
	plottype: 0, $  ; TV,SURFACE,CONTOUR,SHADE_SURF
	viewtype: '', $  ; TV,SURFACE,CONTOUR,SHADE_SURF
	type: 0, $  ; tiff,gif,jpeg
	panimagewin : -1, $
	outfile:'' $
	}

	if n_elements(filename) then begin
		tiff_ids.filename=string(filename)
		tiff_init
	end

  MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
      UVALUE='MAIN13')

  BASE2 = WIDGET_BASE(MAIN13, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  LABEL3 = WIDGET_LABEL( BASE2, $
      UVALUE='LABEL3', $
      VALUE='Create TIFF/GIF/ASCII Image Files')

  BASE4 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE4')

  BUTTON5 = WIDGET_BUTTON( BASE4, $
      UVALUE='TIFF_FILE1', $
      VALUE='File...')

  BUTTON21 = WIDGET_BUTTON( BASE4, $
      UVALUE='TIFF_COLOR', $
      VALUE='Color...')

  BUTTON31 = WIDGET_BUTTON( BASE4, $
      UVALUE='TIFF_HELP', $
      VALUE='Help...')

  TIFF_FILE2 = CW_FIELD( BASE2,VALUE=tiff_ids.filename, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Catcher Image File:', $
      UVALUE='TIFF_FILE2', $
      XSIZE=60)

  TIFF_DIR = CW_FIELD( BASE2,VALUE=tiff_ids.tiffpath, $ 
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Output Dir/Path:', $
      UVALUE='TIFF_DIR', $
      XSIZE=60)

  TIFF_CLASSNAME = CW_FIELD( BASE2,VALUE=tiff_ids.classname, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Output Classname:', $
      UVALUE='TIFF_CLASSNAME', $
      XSIZE=60)

  BASE5 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE5')

  Btns912 = ['TV','SURFACE','CONTOUR','SHADE_SURF']
  surface_plot = WIDGET_DROPLIST(BASE5, VALUE=BTNS912, $
        UVALUE='TIFF_PLOT',TITLE='View as')

  Btns800 = [ $
    'TIFF', $
    'R-TIFF', $
    'GIF', $
    'ASCII' ]
  BGROUP3 = CW_BGROUP( BASE5, Btns800, $
      ROW=1, $
      EXCLUSIVE=1, $
      LABEL_LEFT='Save As', SET_VALUE=0,  $
      UVALUE='TIFF_TYPE')

  BASE2_0 = WIDGET_BASE(BASE2, $
      COLUMN=1, $
      MAP=1, FRAME=1, $
      UVALUE='BASE2_0')

  BASE20 = WIDGET_BASE(BASE2_0, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE20')

  SCAN_NUMBER = CW_FIELD( BASE20,VALUE='', $
      ROW=1, $
      INTEGER=1, XSIZE=6, $
      RETURN_EVENTS=1, $
      TITLE='PanImages for Scan 2D # :', $
      UVALUE='SCAN_NUMBER')

  SCAN_FIRST = WIDGET_BUTTON( BASE20, $
      UVALUE='SCAN_FIRST', $
      VALUE='First')
  SCAN_NEXT = WIDGET_BUTTON( BASE20, $
      UVALUE='SCAN_NEXT', $
      VALUE='Next')
  SCAN_PREV = WIDGET_BUTTON( BASE20, $
      UVALUE='SCAN_PREV', $
      VALUE='Prev')
  SCAN_LAST = WIDGET_BUTTON( BASE20, $
      UVALUE='SCAN_LAST', $
      VALUE='Last')


  BASE25 = WIDGET_BASE(BASE2_0, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE25')

  TIFF_FIRST = WIDGET_BUTTON( BASE25, $
      UVALUE='TIFF_FIRST', $
      VALUE='<<')

  BMP2446 = [ $
    [ 128b, 1b ], $
    [ 0b, 3b ], $
    [ 0b, 6b ], $
    [ 0b, 12b ], $
    [ 0b, 24b ], $
    [ 0b, 48b ], $
    [ 0b, 96b ], $
    [ 240b, 255b ], $
    [ 240b, 255b ], $
    [ 0b, 96b ], $
    [ 0b, 48b ], $
    [ 0b, 24b ], $
    [ 0b, 12b ], $
    [ 0b, 6b ], $
    [ 0b, 3b ], $
    [ 128b, 1b ]  $
  ]
  BMPBTN8 = WIDGET_BUTTON( BASE25,VALUE=BMP2446, $
      UVALUE='TIFF_NEXT')

  BMP2404 = [ $
    [ 128b, 1b ], $
    [ 192b, 0b ], $
    [ 96b, 0b ], $
    [ 48b, 0b ], $
    [ 24b, 0b ], $
    [ 12b, 0b ], $
    [ 6b, 0b ], $
    [ 255b, 15b ], $
    [ 255b, 15b ], $
    [ 6b, 0b ], $
    [ 12b, 0b ], $
    [ 24b, 0b ], $
    [ 48b, 0b ], $
    [ 96b, 0b ], $
    [ 192b, 0b ], $
    [ 128b, 1b ]  $
  ]
  BMPBTN7 = WIDGET_BUTTON( BASE25,VALUE=BMP2404, $
      UVALUE='TIFF_PREV')

  TIFF_LAST = WIDGET_BUTTON( BASE25, $
      UVALUE='TIFF_LAST', $
      VALUE='>>')

  TIFF_SLIDER = WIDGET_SLIDER( BASE25, $
      TITLE='Image #', $
	MAXIMUM=tiff_ids.maxno, drag=0, tracking_events=0, $
      UVALUE='TIFF_SLIDER', $
      VALUE=0)

BMP210 = [ $
    [ 0b, 0b, 0b, 0b ], $
    [ 124b, 16b, 0b, 0b ], $
    [ 132b, 24b, 0b, 0b ], $
    [ 4b, 17b, 0b, 0b ], $
    [ 4b, 17b, 0b, 0b ], $
    [ 4b, 17b, 206b, 57b ], $
    [ 4b, 17b, 206b, 57b ], $
    [ 4b, 17b, 0b, 0b ], $
    [ 4b, 17b, 0b, 0b ], $
    [ 132b, 16b, 0b, 0b ], $
    [ 124b, 56b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 124b, 16b, 31b, 0b ], $
    [ 132b, 24b, 1b, 0b ], $
    [ 4b, 17b, 1b, 0b ], $
    [ 4b, 17b, 1b, 0b ], $
    [ 4b, 17b, 31b, 0b ], $
    [ 4b, 17b, 32b, 0b ], $
    [ 4b, 17b, 32b, 0b ], $
    [ 4b, 17b, 32b, 0b ], $
    [ 132b, 16b, 32b, 0b ], $
    [ 124b, 56b, 31b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ], $
    [ 0b, 0b, 0b, 0b ]  $
  ]
  BMPBTN3 = WIDGET_BUTTON( BASE25,VALUE=BMP210, $
      UVALUE='TIFF_PANIMAGES')

  APPEND_BUTTON5 = WIDGET_BUTTON( BASE25, $
      UVALUE='TIFF_LISTAPPEND', $
      VALUE='AddList')

  TIFF_LIST = CW_FIELD( BASE25,VALUE='', $
      ROW=1, $
      STRING=1, XSIZE=40, $
      RETURN_EVENTS=1, $
      TITLE='List:', $
      UVALUE='TIFF_LIST')

  BASE25_1 = WIDGET_BASE(BASE2_0, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE25_1')

  BUTTON21 = WIDGET_BUTTON( BASE25_1, $
      UVALUE='TIFF_ASCII', $
      VALUE='View ASCII...')

  CLR_BUTTON5 = WIDGET_BUTTON( BASE25_1, $
      UVALUE='TIFF_LISTCLR', $
      VALUE='Clear_List')

  CLR_BUTTON5 = WIDGET_BUTTON( BASE25_1, $
      UVALUE='TIFF_LISTWHOLE', $
      VALUE='Whole_List')

  BUTTON20 = WIDGET_BUTTON( BASE25_1, $
      UVALUE='TIFF_ACCEPT', $
      VALUE='Accept_List')

  BASE18 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE18')

  BUTTON19 = WIDGET_BUTTON( BASE18, $
      UVALUE='TIFF_DONE', $
      VALUE='Done')

	tiff_ids.base = MAIN13 
	tiff_ids.filename_id = TIFF_FILE2
	tiff_ids.scanno_id = SCAN_NUMBER
	tiff_ids.list_id = TIFF_LIST
	tiff_ids.tiffpath_id = TIFF_DIR
	tiff_ids.classname_id = TIFF_CLASSNAME
	tiff_ids.slider_id = TIFF_SLIDER
	

  WIDGET_CONTROL, MAIN13, /REALIZE

  XMANAGER, 'MAIN13', MAIN13
END
