
PRO view3d_test,state
;print,read_scan('/home/sricat/CHA/data/rix/cha:_0049.scan',Scan)
;filename='cha:_0049.scan'

   CASE state.method OF
   0: begin
	idet = state.det_listid
	k = state.k_listid
	rank = state.rank
	xyz = *(*state.Scan.da)[0]
	title = state.class+'  (D'+strtrim(idet+1,2)+')'
	if rank eq 0 then title=title+'  (Slice X_'+strtrim(k,2)+')'
	if rank eq 1 then title=title+'  (Slice Y_'+strtrim(k,2)+')'
	if rank eq 2 then title=title+'  (Slice Z_'+strtrim(k,2)+')'

	xyz = xyz(*,*,*,idet)
	view3d,xyz,k,rank,title=title,group=state.base
   end
   1: begin
	view3d_asciiRep,state,group=state.base
   end
   2: begin
	view3d_pick1d,state,group=state.base
   end
   ENDCASE
END

PRO PDMENUV3D_PANIMAGE_Event, Event, state

;	k = state.k_listid
;	rank = state.rank
;	scanno = '.panZ'
;	if rank eq 0 then scanno = '.panX'
;	if rank eq 1 then scanno = '.panY'
;	scanno = scanno + '_'+strtrim(k,2)
;	title = 'View3d'+scanno

  CASE Event.Value OF
  'PanImage.PanImages.PanImages...': begin
        view3d_panImage,state,Group=state.base
        end
  'PanImage.PanImages.PanImages+TIFF': begin
        view3d_panImage,state,tiff=tiff,Group=state.base
        end
  'PanImage.PanImages.PanImages+RTIFF': begin
        view3d_panImage,state,tiff=tiff,/reverse,Group=state.base
        end
  'PanImage.PanImages.PanImages+GIF': begin
        view3d_panImage,state,gif=gif,Group=state.base
        end
  'PanImage.PanImages.PanImages+PICT': begin
        view3d_panImage,state,pict=pict,Group=state.base
        end
  'PanImage.Calibration...': begin
	view3d_pick1d,state,/calibra,group=state.base
        end
  ENDCASE
END

PRO view3d_panImage,state,tiff=tiff,reverse=reverse,gif=gif,pict=pict,group=group
	filename = state.file
	rank = state.rank
	kindex = state.k_listid
	xyz = *(*state.Scan.da)[0]
	id_def = state.id_def

	if rank eq 2 then data = reform(xyz(*,*,kindex,*))
	if rank eq 1 then data = reform(xyz(*,kindex,*,*))
	if rank eq 0 then data = reform(xyz(kindex,*,*,*))

	scanno = '.panZ'
	if rank eq 0 then scanno = '.panX'
	if rank eq 1 then scanno = '.panY'
	scanno = scanno + '_'+strtrim(kindex,2)
	title = '3D Scan #'+strtrim(*(state.Scan.scanno),2)+scanno

	if keyword_set(TIFF) then $
	tiff = state.outpath+'TIFF'+!os.file_sep+state.class+ $
		scanno+'.tiff'
	if keyword_set(REVERSE) then $
	tiff = state.outpath+'TIFF'+!os.file_sep+state.class+ $
		scanno+'.rtiff'
	if keyword_set(GIF) then $
	gif = state.outpath+'GIF'+!os.file_sep+state.class+ $
		scanno+'.gif'
	if keyword_set(PICT) then $
	pict = state.outpath+'PICT'+!os.file_sep+state.class+ $
		scanno+'.pict'

	nw = state.panwin
	panImage,data,id_def,new_win=nw,tiff=tiff,reverse=reverse,gif=gif,pict=pict,title=title
	state.panwin = nw

END

PRO view3d_pick1d,state,calibra=calibra,group=group

	filename = state.file
	idet = state.det_listid
	k = state.k_listid
	rank = state.rank
	xyz = *(*state.Scan.da)[0]
	sz = size(xyz)

	ipos = 0
	cpt = sz(1:3)

	CASE rank OF 
	0: BEGIN    ; x axis picked
	xa = *(*state.Scan.pa)[1]
	xa = xa(*,0,ipos)
	ya = *(*state.Scan.pa)[2]
	ya = ya(*,ipos)
	za = reform(xyz(k,*,*,idet))
	im_array = reform(xyz(k,*,*,*))
	title =  '3D_2D PanImages:  X slice @: '+strtrim(k,2)
	END
	1: BEGIN    ; y axis picked
	xa = *(*state.Scan.pa)[0]
	xa = xa(*,0,0,ipos)
	ya = *(*state.Scan.pa)[2]
	ya = ya(*,ipos)
	za = reform(xyz(*,k,*,idet))
	im_array = reform(xyz(*,k,*,*))
	title = '3D_2D PanImages:  Y slice @: '+strtrim(k,2)
	END
	2: BEGIN    ; z axis picked
	xa = *(*state.Scan.pa)[0]
	xa = xa(*,0,0,ipos)
	ya = *(*state.Scan.pa)[1]
	ya = ya(*,0,ipos)
	za = xyz(*,*,k,idet)
	im_array = reform(xyz(*,*,k,*))
	title = '3D_2D PanImages:  Z slice @: '+strtrim(k,2)
	END
	ENDCASE

	if keyword_set(calibra) then begin
        calibration_factor,im_array,state.id_def,title=title, $
                inpath=state.path,classname=state.class, $
                xv=xv,yv=yv,GROUP=group
	return
	end

	if min(xa) eq max(xa) then xa = indgen(n_elements(xa))
	if min(ya) eq max(xa) then ya = indgen(n_elements(xa))
	calibra_pick1d,za,xa=xa,ya=ya,title=title,Group=group

END

PRO view3d,da3D,kindex,rank,SLICER3=SLICER3,title=title,group=group,data=data
;+
; NAME:
;       VIEW3D 
;
; PURPOSE:
;      This routine cut out a cartesian 2D slice from an arbitrary
;      input 3d array. It allows the user view the 2D slice as 
;      various 1D/2D plots or ASCII output.
;
; CATEGORY:
;    Widgets.
;
; CALLING SEQUENCE:
;	VIEW3D, Da3D [,Kindex] [,Rank] [,SLICER3=slicer3] [,TITLE=title]
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
;  SLICER3:   Specifies whether slicer3 is called 
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


PRO view3d_asciiRep,state,group=group

	filename = state.file
	idet = state.det_listid
	kindex = state.k_listid
	rank = state.rank
	xyz = *(*state.Scan.da)[0]

	if rank eq 2 then data = reform(xyz(*,*,kindex,idet))
	if rank eq 1 then data = reform(xyz(*,kindex,*,idet))
	if rank eq 0 then data = reform(xyz(kindex,*,*,idet))

	detector = idet+1

	t_format = 'G18.8'
	if keyword_set(format) then t_format = format
	fwidth = 'I'+strmid(t_format,1,strpos(t_format,'.')-1)

	dir = state.outpath+'ASCII'+!os.file_sep
	found = findfile(dir,count=ct)
	if ct lt 1 then spawn,!os.mkdir + ' ' +dir

	suf0 = '00'
	suf = strtrim(detector,2)
	ln = strlen(suf)
	strput,suf0,suf,2-ln
	file = state.class +'.im'+suf0+'.slc'+strtrim(kindex,2)
	report = file

	if keyword_set(outfile) then report = strtrim(outfile,2)

	openw,fw,dir+report,/get_lun

	s = size(data)
	dim = s(1:2)
	printf,fw,'; From:',state.file ,',   Detector = ',strtrim(detector,2)
	st ='; Pick Rank # = '+strtrim(rank,2)
	if rank eq 0 then st = st+ ' (ie X axis) '
	if rank eq 1 then st = st+ ' (ie Y axis) '
	if rank eq 2 then st = st+ ' (ie Z axis) '
	st = st +',   Slicer # ='+strtrim(kindex,2)
	printf,fw,st
	printf,fw,';   data('+strtrim(dim(0),2)+','+strtrim(dim(1),2)+')'
	printf,fw,'; -------------------------------'

;	f0 = '(";              (yvalues)",'+ '5000('+t_format+',:))'
;	if n_elements(py) gt 0 then printf,fw,format=f0,py
;	if n_elements(py) gt 0 then begin
;		f1 = '('+t_format+',I,'+strtrim(dim(1),2)+'('+t_format+'))' 
;		f0 = '(";                   \ Y",'+strtrim(dim(1),2)+fwidth+',/,";                  X \",/,";      (xvalues)")'
;		endif else begin
		f0 = '(";    \ Y",'+strtrim(dim(1),2)+fwidth+',/,";   X \",/)'

		if rank eq 0 then $
		f0 = '(";    \ Z",'+strtrim(dim(1),2)+fwidth+',/,";   Y \",/)'
		if rank eq 1 then $
		f0 = '(";    \ Z",'+strtrim(dim(1),2)+fwidth+',/,";   X \",/)'

		f1 = '(I,'+strtrim(dim(1),2)+'('+t_format+'))' 
;		end
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
	xdisplayfile,dir+report,group=group
	
END



PRO view3d_free,Scan 
  if ptr_valid(Scan.scanno) then ptr_free,Scan.scanno
  if ptr_valid(Scan.dim) then ptr_free,Scan.dim
  if ptr_valid(Scan.npts) then ptr_free,Scan.npts
  if ptr_valid(Scan.cpt) then ptr_free,Scan.cpt
  if ptr_valid(Scan.id_def) then ptr_free,Scan.id_def
  if ptr_valid(Scan.pv) then ptr_free,Scan.pv
  if ptr_valid(Scan.labels) then ptr_free,Scan.labels
  if ptr_valid(Scan.pa) then ptr_free,Scan.pa
  if ptr_valid(Scan.da) then ptr_free,Scan.da
  if n_elements(Scan) then Scan = 0
END

PRO view3d_init,state,file

;	outpath = state.home+!os.file_sep 
	r = rstrpos(file,!os.file_sep)
	if r ge 0 then path = strmid(file,0,r+1)
	class = strmid(file,r+1,strlen(file)-r)

	state.path = path
	rp = rstrpos(class,'_')
        state.prefix = strmid(class,0,rp+1)

	error = 0
	scanno = read_scan(file,Scan)
	state.scanno = *Scan.scanno

	if scanno ne -1 and  *Scan.dim ne 3 then begin
		str = ['Non 3D scan file selected:',file, '', $
			'Please first close the VIEW3D_SLICER window, and then', $
			'access any other scan file by either ', $
			'Method 1:', $
			'    ViewData->1D/2D/3D Browser... menu ', $
			'  or  ', $
			'Method 2:', $
			'    File->Open',$
			'    ViewData->1D/2D... menu ' $
		]
		r = dialog_message(str,/error)
		error = -1
		return
	end 

	*(state.Scan).scanno = *Scan.scanno
	*(state.Scan).dim = *Scan.dim
	*(state.Scan).npts = *Scan.npts
	*(state.Scan).cpt = *Scan.cpt
	*(state.Scan).id_def = *Scan.id_def
	*(state.Scan).pv = *Scan.pv
	*(state.Scan).labels = *Scan.labels
	*(state.Scan).pa = *Scan.pa
	*(state.Scan).da = *Scan.da

	view3d_free,Scan

	; initialize variables

	state.file = file
	state.class = class
	WIDGET_CONTROL,state.filenamewid,SET_VALUE=file
	WIDGET_CONTROL,state.prevnext,SENSITIVE=1
	WIDGET_CONTROL,state.basecntl,SENSITIVE=1

	id_def = *(state.Scan).id_def
	ndet = n_elements(state.id_def)
	state.id_def = id_def(4:4+ndet-1,0)
	cpt = *(state.Scan).cpt
	list0 = strtrim(indgen(cpt(state.rank)),2)
	WIDGET_CONTROL,state.k_listwid,set_value=list0
  	WIDGET_CONTROL,state.k_listwid,SET_LIST_SELECT=0

;	view3d_test,state
	view3d_panImage,state,group=state.base

END   


PRO view3d_help
str = ['', $
'This program let user view 2D slices of 3D scanSee data file.', $
'It supports three display options: plot2d, ascii2d, and pick1d ',$
'for the selected detector #, rank #, and slice #', '', $
'File->Open...      - selects and open 3D scanSee file', $
'File->Close        - exits the view3d_slicer program', $
'Color...           - calls the xloadct program', $
'Help...            - shows this help info', $
'Close              - exits the view3d_slicer program', $
'File name          - shows the name of currently opened scanSee file', $
'Display Options    - calls PLOT2D/ASCII2D/PICK1D programs', $
'PanImage Menu      - displays all detectors for selected rank # & slice # ', $
'     PanImages+TIFF  - saves pan image as TIFF file', $
'     PanImages+RTIFF - saves pan image as reverse order TIFF file', $
'     PanImages+GIF   - saves pan image as GIF file', $
'     PanImages+PICT  - saves pan image as PICT file', $
'     Calibration...  - runs calibration program ', $
'Detector List      - selects the detecor #, default D1', $
'Rank Options       - picks the viewing axis of 2D slices, default Z axis', $
'Slice List         - selects the slice # of the picked axis, default 0' $
]
r = dialog_message(str,/info,title='HELP ON VIEW3D_SLICER')
END

PRO PDMENU5_Event, Event, state


  CASE Event.Value OF 


  'File.Open...': BEGIN
    PRINT, 'Event for File.Open...'
	old_path = state.path
	F = PICKFILE(TITLE='Open ...',/READ,FILE=filename,PATH=old_path,GET_PATH=P,FILTER='*.scan')
	if F eq '' then return
	found = findfile(F,count=ct)
	if ct eq 0 then return

	state.path = P

	outpath = p
	catch,error_status
	if error_status ne 0 then begin
		outpath = state.home+!os.file_sep
		goto, step_mkdir
	end
	openw,1,outpath+'.tmp'
	close,1

step_mkdir:

	found = findfile(state.outpath+'ASCII',count=ct)
	if ct gt 0 then goto,step_init

	r = dialog_message(['Please check the existence of ASCII,TIFF,GIF,', $
		'PICT,ROI,CALIB  subdirectory'],/error)
	catch,error_status
	if error_status ne 0 then goto,step_init
	spawn, !os.mkdir + ' ' + state.outpath+'ASCII' + ' ' + $
		state.outpath+'TIFF' + ' ' +$
		state.outpath+'GIF' + ' ' +$
		state.outpath+'PICT' + ' ' +$
		state.outpath+'ROI' + ' ' +$
		state.outpath+'CALIB'

step_init:

	state.outpath = outpath
	view3d_init,state,F
	
    END
  'File.Close': BEGIN
	view3d_free,state.Scan
      WIDGET_CONTROL,Event.top,BAD=bad,/DESTROY
	heap_gc
	state.base = 0L
    END
  ENDCASE
END





PRO VIEW3D_Event, Event
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev
  WIDGET_CONTROL,Event.top,GET_UVALUE=state

  CASE Ev OF 

  ; Event for view3d_file
  'VIEW3D_FILEMENU': PDMENU5_Event, Event, state
  'PDMENUV3D_PANIMAGE': PDMENUV3D_PANIMAGE_Event, Event, state
  'VIEW3D_COLOR': BEGIN
	xloadct,group=Event.Top
      END
  'VIEW3D_HELP': BEGIN
       view3d_help
      END
  'VIEW3D_NEXT': BEGIN
	no = strtrim(state.scanno+1,2)
	if no ge 0 then begin
	st = '0000'
	strput,st,no,4-strlen(no)
	file = state.path+state.prefix+st+state.suffix
	found = findfile(file,count=ct)
	if ct eq 0 then return
	view3d_init,state,file
	end
      END
  'VIEW3D_PREV': BEGIN
	no = strtrim(state.scanno-1,2)
	if no ge 0 then begin
	st = '0000'
	strput,st,no,4-strlen(no)
	file = state.path+state.prefix+st+state.suffix
	found = findfile(file,count=ct)
	if ct eq 0 then return
	view3d_init,state,file
	end
      END
  'VIEW3D_CLOSE': BEGIN
	view3d_free,state.Scan
      WIDGET_CONTROL,Event.top,BAD=bad,/DESTROY
	heap_gc
	state.base = 0L
	return
      END
  'VIEW3D_DETECTOR': BEGIN
	r=WIDGET_INFO(Event.ID,/LIST_SELECT)
	if state.id_def(r) eq 0 then begin
	print,'Detector: ',state.dets(r)
		re = dialog_message(state.dets(r)+' not defined',/Info)
		return
	end
	state.det_listid = r
	view3d_test,state 
      END
  'VIEW3D_INDEX': BEGIN
	r=WIDGET_INFO(Event.ID,/LIST_SELECT)
	state.k_listid = r
	view3d_test,state
      END
  'VIEW3D_RANK': BEGIN
	state.rank = Event.Value
	view3d_test,state
      END
  'VIEW3D_PANIMAGE': BEGIN
	view3d_panImage,state,group=state.base
      END
  'VIEW3D_ASCII': BEGIN
	view3d_asciiRep,state,group=state.base
      END
  'VIEW3D_PICK1D': BEGIN
	view3d_pick1d,/table,state,group=group
      END
  'VIEW3D_METHOD': BEGIN
	state.method = Event.Value
	view3d_test,state
      END
  ENDCASE

  if state.base gt 0 then $
  WIDGET_CONTROL,Event.top,SET_UVALUE=state

END




PRO VIEW3D_SLICER, file=file, DMAX=DMAX, GROUP=Group
;+
; NAME: 
;    VIEW3D_SLICER
; 
; PURPOSE:
;    This routine provides a flexible 2D slicer for the 3D scan data 
;    automatically saved by the IOC scan software. 
;    It provides the option of generating various 2D image plots, 2D
;    ASCII report, and a table driven 1D plots of various columns and
;    rows of the 2D data.
;
; CATEGORY
;    Widgets.
;
; CALLING SEQUENCE:
;    
;    VIEW3D_SLICER, File='filename', GROUP=group
;
; KEYWORD PARAMETERS:
;    FILE:       Specify the input scan filename saved by the IOC scan 
;                software, the file name is sequenced by the scanno and
;                prefixed with uid and suffixed with '.scan', e.g.
;                'uid:_nnnn.scan'
;
;    DMAX:       Specify the maximum number of detectors, 15 for scan 4.1
;                85 for newer
;
;    GROUP:      Specify the widget ID of the group leader of the widget.
;                The death of the group leader results in the death of the
;                VIEW3D_SLICER.
;
; RESTRICTIONS:
;    The input filename must be a 3D scan saved by the IOC.
;
; EXAMPLES:
;
;       VIEW3D_SLICER, File='cha:_0049.scan'
;
;-

  heap_gc
  if Xregistered('VIEW3D') then return

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  ND = 85
  if keyword_set(DMAX) then ND = DMAX
  ndet = ND

  ListVal1283 = 'D'+strtrim(indgen(ND)+1,2) 

  cd,current=cr
  home = cr 

Scan = { $
        scanno  : ptr_new(/allocate_heap), $  ;0L, $
        dim     : ptr_new(/allocate_heap), $  ;0, $
        npts    : ptr_new(/allocate_heap), $  ;[0,0], $
        cpt     : ptr_new(/allocate_heap), $  ;[0,0], $
        id_def  : ptr_new(/allocate_heap), $  ;intarr(ND+4,2), $
        pv      : ptr_new(/allocate_heap), $  ;['',''], $
        labels  : ptr_new(/allocate_heap), $  ;strarr(ND*3,2), $
        pa      : ptr_new(/allocate_heap), $
        da      : ptr_new(/allocate_heap) $
        }

  view3d_state = { base: 0L, $
	prevnext: 0L, $
	basecntl: 0L, $
	filenamewid: 0L, $
	det_listwid: 0L, $
	k_listwid: 0L, $
	det_listid: 0, $
	k_listid: 0, $
	panwin: -1, $
	dets: ListVal1283, $
	id_def: intarr(ndet), $   ; detectors
	rank: 2, $     ; 0-x, 1-y, 2-z
	method: 0, $     ; 0-plot2d, 1-ascii2d, 2-pick1d
	scanno:-1, $
	prefix: '', $
	suffix: '.scan', $
	class: '', $
	file: '', $
	home: home, $
	path: home + !os.file_sep, $
	outpath: home+!os.file_sep, $
	Scan: Scan $
	 }

  if keyword_set(file) then view3d_state.file = file

  VIEW3D = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
	TITLE='VIEW3D_SLICER (1.0)', $
      UVALUE='VIEW3D')

  BASE2 = WIDGET_BASE(VIEW3D, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE3 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE3')

  MenuDesc1027 = [ $
      { CW_PDMENU_S,       3, 'File' }, $ ;        0
        { CW_PDMENU_S,       0, 'Open...' }, $ ;        1
        { CW_PDMENU_S,       2, 'Close' } $  ;      2

  ]


  PDMENU5 = CW_PDMENU( BASE3, MenuDesc1027, /RETURN_FULL_NAME, $
      UVALUE='VIEW3D_FILEMENU')

  BUTTON7 = WIDGET_BUTTON( BASE3, $
      UVALUE='VIEW3D_COLOR', $
      VALUE='Color...')

  BUTTON8 = WIDGET_BUTTON( BASE3, $
      UVALUE='VIEW3D_HELP', $
      VALUE='Help...')

  BUTTON9 = WIDGET_BUTTON( BASE3, $
      UVALUE='VIEW3D_CLOSE', $
      VALUE='Close')

  BASE2_1 = WIDGET_BASE(BASE2, $
      COLUMN=1, /FRAME, $
      MAP=1, $
      UVALUE='BASE2_1')

  LABELFILENAME = WIDGET_LABEL( BASE2_1, $
      UVALUE='VIEW3D_FILENAME', $
      VALUE='File: ',/DYNAMIC)

  BASE3_1 = WIDGET_BASE(BASE2_1, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE3_1')

  BUTTON2 = WIDGET_BUTTON( BASE3_1, $
      UVALUE='VIEW3D_PREV', $
      VALUE='Prev Scan File')
  BUTTON3 = WIDGET_BUTTON( BASE3_1, $
      UVALUE='VIEW3D_NEXT', $
      VALUE='Next Scan File')

  BASE4 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE4')

  BASE4_3 = WIDGET_BASE(BASE4, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE4_3')

  BASE4_0 = WIDGET_BASE(BASE4, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE4_0')

  LABEL10 = WIDGET_LABEL( BASE4_0, $
      UVALUE='LABEL10', $
      VALUE='Detector #')

  LIST11 = WIDGET_LIST( BASE4_0,VALUE=view3d_state.dets, $
      UVALUE='VIEW3D_DETECTOR', $
      YSIZE=5)
  WIDGET_CONTROL,LIST11,SET_LIST_SELECT=0

  BASE4_1 = WIDGET_BASE(BASE4, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE4_1')

  Btns1399 = [ $
    'X', $
    'Y', $
    'Z' ]
  BGROUP13 = CW_BGROUP( BASE4_1, Btns1399, $
      COLUMN=1, $
      EXCLUSIVE=1, /NO_RELEASE, $
      FRAME=1, SET_VALUE=view3d_state.rank, $
      LABEL_TOP='Pick Rank', $
      UVALUE='VIEW3D_RANK')

  BASE4_2 = WIDGET_BASE(BASE4, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE4_2')

  LABEL20 = WIDGET_LABEL( BASE4_2, $
      UVALUE='LABEL20', $
      VALUE='Slice #')

  ListVal1288 = strtrim(indgen(10),2) 
  LIST21 = WIDGET_LIST( BASE4_2,VALUE=ListVal1288, $
      UVALUE='VIEW3D_INDEX', $
      YSIZE=7)
  WIDGET_CONTROL,LIST21,SET_LIST_SELECT=0

  Btns1499 = [ $
    'PLOT2D',$
    'ASCII2D',$
    'PICK1D']
  BGROUP14 = CW_BGROUP( BASE4_3, Btns1499, $
      COLUMN=1, $
      EXCLUSIVE=1, /NO_RELEASE, $
      FRAME=1, SET_VALUE=view3d_state.method, $
      LABEL_TOP='Display By', $
      UVALUE='VIEW3D_METHOD')

  MenuV3DPANImage = [ $
      { CW_PDMENU_S,       3, 'PanImage' }, $ ;        0
      { CW_PDMENU_S,       1, 'PanImages' }, $ ;        0
        { CW_PDMENU_S,       0, 'PanImages...' }, $ ;        1
        { CW_PDMENU_S,       0, 'PanImages+TIFF' }, $ ;        1
        { CW_PDMENU_S,       0, 'PanImages+RTIFF' }, $ ;        1
        { CW_PDMENU_S,       0, 'PanImages+GIF' }, $ ;        1
        { CW_PDMENU_S,       2, 'PanImages+PICT' }, $ ;        1
      { CW_PDMENU_S,       2, 'Calibration...' } $ ;        0
        ]
  PDMENUV3D_panimage = CW_PDMENU( BASE4_3, MenuV3DPANImage, /RETURN_FULL_NAME, $
      UVALUE='PDMENUV3D_PANIMAGE')

;  SLICER_PANIMAGE = WIDGET_BUTTON(BASE4_3,VALUE='PanImages...', $
;	UVALUE='VIEW3D_PANIMAGE')

;  SLICER_PICK1D = WIDGET_BUTTON(BASE4_3,VALUE='Pick_1D...', $
;	UVALUE='VIEW3D_PICK1D')

  view3d_state.base = VIEW3d
  view3d_state.prevnext = BASE3_1
  view3d_state.basecntl = BASE4 
  view3d_state.filenamewid = LABELFILENAME 
  view3d_state.det_listwid = LIST11
  view3d_state.k_listwid = LIST21

  WIDGET_CONTROL, BASE3_1, SENSITIVE=0
  WIDGET_CONTROL, BASE4, SENSITIVE=0
  WIDGET_CONTROL, VIEW3D, /REALIZE

  if n_elements(file) then  begin
	view3d_init,view3d_state,file
	WIDGET_CONTROL,view3d_state.filenamewid,SET_VALUE=file
  end

  WIDGET_CONTROL,VIEW3D,SET_UVALUE=view3d_state

  XMANAGER, 'VIEW3D', VIEW3D
END
