;@scanSee__define
@panimage.pro

PRO DC_view_writeConfig,state
	openw,unit,'scanSee.config',/get_lun
	printf,unit,state.filename
	printf,unit,state.path
	printf,unit,''
	free_lun,unit
END

PRO DC_view_readConfig,filename,path
	filename=''
	path=''
	openr,unit,'scanSee.config',/get_lun
	readf,unit,filename
	readf,unit,path
	free_lun,unit
	 
END


PRO DC_view_init, filename , DC_view_ids, print=print

WIDGET_CONTROL,/HOURGLASS
	ret = obj_valid(DC_view_ids.v)
	if ret then obj_destroy,DC_view_ids.v
;	if ret eq 0 then begin
	v = obj_new('scanSee',file=filename)
	DC_view_ids.v = v
;	endif else v = DC_view_ids.v
	

	DC_view_ids.toobig = 0
	v->read,dim=dim,num_pts=num_pts,cpt=cpt,pv=pv,labels=labels,$
		scanno=scanno,id_def=id_def, $
		pa1d=pa1d,da1d=da1d,pa2d=pa2d,da2d=da2d,pa3d=pa3d,da3d=da3d, $
		x=x,y=y,z=z,class=class,outpath=outpath

	if dim eq 3 then begin
	if cpt(0) ge 500 or cpt(1) ge 500 or cpt(2) ge 500 then begin
		DC_view_ids.toobig = 1
		DC_view_ids.detno = 16
	WIDGET_CONTROL,DC_view_ids.list2DWID,SET_LIST_SELECT=DC_view_ids.detno-1

		end
	end

	if scanno lt 0 then begin
		str = ['Error encountered in reading the file:',filename,'', $
			'You have to use File... selection dialog','' , $
			'to select a new file.']
		r=dialog_message(str, /Error)
		return
	end

	if keyword_set(print) then $
	help,scanno,dim,num_pts,cpt,pv,labels,id_def,pa1d,da1d,pa2d,da2d, $
		pa3d,da3d,x,y,z

;	DC_view_ids.startno = 1
;	DC_view_ids.detno = 16              ; default to D01
	DC_view_ids.endno = 1
	DC_view_ids.fileno = scanno
        DC_view_ids.filename = filename
        DC_view_ids.dim = dim
        DC_view_ids.width = num_pts(0)
        DC_view_ids.maxno = 1
        DC_view_ids.def = id_def
        DC_view_ids.outpath = outpath
        DC_view_ids.class = class

	if dim eq 2 then begin
        DC_view_ids.height = num_pts(1)
	h = cpt(1)
	if h eq 0 then h = 1
        DC_view_ids.maxno = h
	if h eq 0 then begin
		r=dialog_message('Error in read file: '+ DC_view_ids.filename,/Error)
	end
	str1 = indgen(h,/string)  
  	WIDGET_CONTROL, DC_view_ids.base2DWID,SENSITIVE=1
	WIDGET_CONTROL,DC_view_ids.list1dWID,SET_VALUE=str1
	endif else WIDGET_CONTROL, DC_view_ids.base2DWID,SENSITIVE=0

	id_def1 = id_def(*,0)
	ndet = n_elements(id_def1)-4
	str2 = DC_view_ids.list(1:ndet)
	str2 = DC_view_ids.detname(0:ndet-1)
	WIDGET_CONTROL,DC_view_ids.list2DWID,SET_VALUE=str2
	WIDGET_CONTROL,DC_view_ids.list2DWID,SET_LIST_SELECT=DC_view_ids.detno-1
	WIDGET_CONTROL,DC_view_ids.list1dWID,SET_LIST_SELECT=DC_view_ids.list_sel(0:DC_view_ids.sel_no-1)

  	WIDGET_CONTROL, DC_view_ids.base1DWID1,SENSITIVE=1
  	WIDGET_CONTROL, DC_view_ids.base1DWID2,SENSITIVE=1

	if dim eq 3 then begin
  	WIDGET_CONTROL, DC_view_ids.base1DWID1,SENSITIVE=0
  	WIDGET_CONTROL, DC_view_ids.base1DWID2,SENSITIVE=0
        DC_view_ids.height = cpt(1)
	if cpt(1) le 0 then DC_view_ids.height = num_pts(1)
        DC_view_ids.depth = num_pts(2)
	if cpt(2) lt num_pts(2) then DC_view_ids.depth = cpt(2)+1
	num = DC_view_ids.depth
	if DC_view_ids.rank eq 0 then num = DC_view_ids.width
	if DC_view_ids.rank eq 1 then num = DC_view_ids.height
	
	str2= strtrim(indgen(num),2)
	WIDGET_CONTROL,DC_view_ids.list3dWID,SET_VALUE=str2
	DC_view_ids.slice = 0
	WIDGET_CONTROL,DC_view_ids.list3dWID,SET_LIST_SELECT=DC_view_ids.slice

	end

	type = ' 3D Data '
	if dim eq 1 then begin
		v->plot1d,/data,group=DC_view_ids.base
		type = ' 1D Data '
	end
	if dim eq 2 then begin
		v->view2d,DC_view_ids.detno,group=DC_view_ids.base
		type = ' 2D Data '
	end

	if dim eq 3 then WIDGET_CONTROL,DC_view_ids.base3dWID,SENSITIVE=1 else $
		WIDGET_CONTROL,DC_view_ids.base3dWID,SENSITIVE=0

	if DC_view_ids.toobig eq 1 then $
		WIDGET_CONTROL,DC_view_ids.base3dWID2,SENSITIVE=0 else $
		WIDGET_CONTROL,DC_view_ids.base3dWID2,SENSITIVE=1 

	WIDGET_CONTROL,DC_view_ids.filetypeWID,set_value=type
	WIDGET_CONTROL,DC_view_ids.filenoWID,set_value=DC_view_ids.fileno
	
	if dim eq 3 then begin 
;		v->view3d_2d,DC_view_ids.detno,group=DC_view_ids.base
	sz = size(da2D)
	def = id_def(4:4+sz(3)-1,1)  ;make_array(sz(3),/int,value=1)
	new_win = DC_view_ids.wid2d
	panimage,da2D,def,numd=10,new_win=new_win, $
	     title='3D SCAN # '+strtrim(DC_view_ids.fileno,2)+' (All 2D Di)'
	DC_view_ids.wid2d=new_win
	end

END

PRO DC_view_cleanup,state
	v = state.v
	ret = obj_valid(v)
	if ret then v->delete
	v = 0
END

PRO VIEWSPEC_3D_2DMENU_event, Event, state

  title=''
  v = state.v
	v->read,view=state.detno,dim=dim,cpt=cpt, $
		da1d=da1d,pa1d=pa1d,da2d=da2d,pa2d=pa2d, $
		da3d=da3d,pa3d=pa3d, $
		x=xa,y=ya,im=im,labels=labels,id_def=id_def
	x = pa2d(*,0) 
	y = pa1d(*,0)

  CASE Event.Value OF
  '2D Menu.Pick 2D...': BEGIN
	pick2d,da2d,x,y,GROUP=Event.top,class=state.class,path=state.outpath
     END
  '2D Menu.PanImages...': BEGIN
	sz = size(da2d)
	def = state.def(4:4+sz(3)-1,1)
;	def = demake_array(sz(3),value=1,/int)
	panimage_sel,da2d,def,title='3D_2D ( All 2D Di)',Group=Event.top
     END
  '2D Menu.Calibration...': BEGIN
	sz = size(da2d)
	def = make_array(sz(3),value=1,/int)
	new_win = state.wid2d
	panimage,da2d,def,new_win=new_win,title='3D_2D ( All 2D Di)'
	state.wid2d=new_win
;       calibration_factor,da2d,def,   $
;                title=title,Group=Event.top else $
       calibration_factor,da2d,def,   $
                title=title,Group=Event.top, $
                xv = x, yv=y
    END
  '2D Menu.CALIB1D...': BEGIN
	x = y
	sz = size(da1d)
	def = make_array(sz(2),value=1,/int)
help,x,da1d
	plot1d,x,da1d,Group=Event.top,/data
        calibration_factor,da1d,def,xv=x,title='3D_2D ( All 1D Di)', $
		Group=Event.top
    END
  ENDCASE
END

PRO DC_viewOutputFilename,state,subclass,type,filename
; type is a string can be 'TIFF','XDR','PICT'
; subclass is a subclass string, e.g. '.pan.'

;	lp = rstrpos(state.filename,!os.file_sep)+1
;	classname = strmid(state.filename,lp,strlen(state.filename)-lp)
	classname = state.class
	outpath = state.outpath+type+!os.file_sep
	found = findfile(outpath,count=ct)
	if ct lt 1 then spawn,!os.mkdir + ' '+outpath
	filename = outpath+classname+subclass+ strlowcase(type)
;print,'DC_viewOutputFilename:',filename
END


PRO ALLASCII1DSETUP_Event, Event

  WIDGET_CONTROL,Event.top,GET_UVALUE=state
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev
  v = state.v

  CASE Ev OF 

  'VIEWSPEC_STARTNO': BEGIN
	WIDGET_CONTROL,Event.Id,GET_VALUE=n1
	state.lineno = n1
      END
  'VIEWSPEC_ENDNO': BEGIN
	WIDGET_CONTROL,Event.Id,GET_VALUE=n2
	state.endno = n2
      END
  'VIEWSPEC_ALLASCII1DCREATE': BEGIN
	v->ascii1dAll,state.lineno,state.endno,/nowin
      END
  'VIEWSPEC_ALLASCII1DFIRST': BEGIN
	v->ascii1d,state.lineno,GROUP=Event.top
      END
  'VIEWSPEC_ALLASCII1DLAST': BEGIN
	v->ascii1d,state.endno,GROUP=Event.top
      END
  'VIEWSPEC_ALLASCII1DVIEW': BEGIN
	v->ascii1dAll,state.lineno,state.endno,GROUP=Event.top
      END
  'VIEWSPEC_ALLASCII1DDONE': BEGIN
	WIDGET_CONTROL,Event.top,/DESTROY
	return
      END
  ENDCASE

   WIDGET_CONTROL,Event.top,SET_UVALUE=state

END



PRO AllASCII1DSetup, state, GROUP=Group


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  ALLASCII1DSETUP = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, MAP=1, TITLE='ASCII1D-SETUP', $
      UVALUE='ALLASCII1DSETUP')

  BASE2 = WIDGET_BASE(ALLASCII1DSETUP, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  LABEL3 = WIDGET_LABEL( BASE2, $
      UVALUE='LABEL3', $
      VALUE='MULTIPLE 1D ASCII Reports')

  LABEL4 = WIDGET_LABEL( BASE2, $
      UVALUE='LABEL4', $
      VALUE='(Max 1D Line # '+strtrim(state.maxno,2)+')')

  FIELD4 = CW_FIELD( BASE2,VALUE='1', $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Start 1D Line #', $
      UVALUE='VIEWSPEC_STARTNO', $
      XSIZE=5)

  FIELD5 = CW_FIELD( BASE2,VALUE='1', $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='End 1D Line #', $
      UVALUE='VIEWSPEC_ENDNO', $
      XSIZE=5)

  BASE20 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE20')
  BUTTON9 = WIDGET_BUTTON( BASE20, $
      UVALUE='VIEWSPEC_ALLASCII1DCREATE', $
      VALUE='Generate')

  BUTTON11 = WIDGET_BUTTON( BASE20, $
      UVALUE='VIEWSPEC_ALLASCII1DFIRST', $
      VALUE='ViewFirst')

  BUTTON12 = WIDGET_BUTTON( BASE20, $
      UVALUE='VIEWSPEC_ALLASCII1DLAST', $
      VALUE='ViewLast')

  BUTTON10 = WIDGET_BUTTON( BASE20, $
      UVALUE='VIEWSPEC_ALLASCII1DVIEW', $
      VALUE='ViewAll')

  BUTTON11 = WIDGET_BUTTON( BASE20, $
      UVALUE='VIEWSPEC_ALLASCII1DDONE', $
      VALUE='Done')


  WIDGET_CONTROL, ALLASCII1DSETUP, /REALIZE

  WIDGET_CONTROL, ALLASCII1DSETUP, SET_UVALUE=state
  
  XMANAGER, 'ALLASCII1DSETUP', ALLASCII1DSETUP

END
PRO PDMENU_ASCII1D_Event,state,Event

  v = state.v

  CASE Event.Value OF
  'ASCII1D.1D SCAN...': begin
	if state.dim eq 1 then $
	v->ascii1d,state.lineno,GROUP=Event.top
    end
  'ASCII1D.2D SCAN...': begin
	if state.dim eq 2 then $
	ALLASCII1DSETUP,state,GROUP=Event.top
    end
  ENDCASE
END


PRO scanSee_PDMENU2D_PanImage_Event,state,Event

v = state.v

  CASE Event.Value OF
  'PanImage.Options...': begin
	v->panImage,/sel
	end
  'PanImage.Default': begin
        v->panImage
        end
  ENDCASE
END

PRO scanSee_PDMENU3D_PanImage_Event,state,Event

v = state.v
rank = state.rank 
slice = state.slice

  CASE Event.Value OF
  '3D PanImage.Calibration...': begin
	v->read,dim=dim,x=x,y=y,z=z, $
		pa1d=pa1d,pa2d=pa2d,pa3d=pa3d, $
		da1d=da1d,da2d=da2d,da3d=da3d
        v->view3d_panImage,slice,rank,image_array
	title = ' Axis:3D_2D PanImage Slice # '+strtrim(slice,2)
	if rank eq 0 then begin 
		title='X'+title
		xv = y
		yv = z
		end
	if rank eq 1 then  begin
		title='Y'+title
		xv = x
		yv = z
		end
	if rank eq 2 then begin
		title='Z'+title
		xv = x
		yv = y
		end
        calibration_factor,image_array,state.def,xv=xv,yv=yv, $
                classname=state.class,inpath=state.path, $
                title=title,GROUP=Event.top

	end
  '3D PanImage.PanImages+TIFF': begin
	DC_viewOutputFilename,state,'.pan.','TIFF',filename
        v->view3d_panImage,slice,rank,tiff=filename,/reverse
        end
  '3D PanImage.PanImages+PICT': begin
	DC_viewOutputFilename,state,'.pan.','PICT',filename
        v->view3d_panImage,slice,rank,pict=filename
        end
  '3D PanImage.PanImages+XDR': begin
	DC_viewOutputFilename,state,'.pan.','XDR',filename
        v->view3d_panImage,slice,rank,xdr=filename
        end
  '3D PanImage.PanImages...': begin
	DC_viewOutputFilename,state,'.pan.','TIFF',filename
        v->view3d_panImage,slice,rank,tiff=filename,/sel
        end
  ENDCASE
END


PRO SS_VIEWSPEC_Event, Event

  WIDGET_CONTROL,Event.top,GET_UVALUE=state
  v = state.v

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'VIEWSPEC_FILE_OPEN': BEGIN
     F = dialog_pickfile(filter='*.mda*',GET_PATH=p,GROUP=Event.Top,/MUST_EXIST,$
                PATH=state.path,TITLE='Select scanSee File',/READ)
        if F eq '' then return
        state.path = p
        state.filename = F
	nv = obj_new('scanSee',file=F)
	if obj_valid(nv) eq 0 then return
	if obj_valid(v) then v->delete
	state.v = nv 
;help,obj_valid()
	
        WIDGET_CONTROL,state.filenameWID,SET_VALUE=F
	DC_view_init,F,state
        WIDGET_CONTROL,state.filenoWID,SET_VALUE=state.fileno
  	WIDGET_CONTROL, state.basefileWID,SENSITIVE=1
    END

  'VIEWSPEC_LOADCT': BEGIN
	xloadct,GROUP=Event.top
    END
  'VIEWSPEC_HELP': BEGIN
    str = [ $
	'This program can display and extract scan data for 1D/2D/3D scan file.','', $
	'The file name is automatically created by the scan saveData file', $
	'system.  The "scanSee.config" file is used for re-start configuration.',$
	'At the normal completion of scanBrowser this configuration file is updated.',$
	'', $
	'File...     - uses the file selection dialog picking the initial file',$
	'Color...    - calls the xloadct routine', $
	'Help...     - show this help page', $
	'DONE        - close the scanBrowser program',$
	'','                      WORKED ON FILENAME',$
	'File Seq #: - enters the desired scan number saved by IOC', $
	'First       - opens the first scan file', $
	'Next        - opens the next scan file', $
	'Prev        - opens the prev scan file', $
	'Last        - opens the last scan file', $
	'Slider      - picks the desired scan file number', $
	'Filename    - reflects the name of the opened scan file ', $
	'','                      WORKED ON 1D/2D SCAN DATA',$
	'Format      - specifies the column format used in ASCII reports', $
	'PLOT1D...   - calls plot1d to access all detectors of a scan line #', $
	'vs P1       - plots versus the selected positioner #', $
	'ASCII1D...  - generates 1D ASCII report for single/multiple 1D line #',$
	'Ez_fit...   - runs ez_fit for the selected detector # and 1D line # ',$
	'Statistic...- calculates FWHM for selected detector # and 1D line # ',$
	'Run Calibra...  - run calibration program for all detectors ', $
	'','                      WORKED ON 3D SCAN DATA ONLY',$
	'3D_Slicer...- runs view3d_2d program to get 2D slice from the 3D data', $
	'3D PanImage - panImage for 3D scan with option of save as TIFF/GIFF/PICT', $
	'X/Y/Z GBTN  - pick the 3D panImage view axis of the 2D slice plane', $
	'Index #     - pick the 3D panImage slice # of the picked axis', $
	'','                      VALID ON 1D/2D/3D SCAN DATA',$
	'Detector # List - selects the detector number, defaults to 1', $
	'','                      WORKED ON 2D SCAN DATA ONLY',$
	'PLOT2D...       - access various plot2d features of 2D image', $
	'ASCII2D...      - saves and displays the ASCII report of 2D image', $
	'PanImage...     - panImage with option of save TIFF/XDR/PICT file', $
;	'Overlay Plot... - overlays plot of multiple 1D scan lines ',$ 
;	'Help 1D Line #...- hints on selecting multiple lines of a detector', $
;	'                   defaults to line 1 ', $
;	'1D Line # List  - multiple slections of 2D line list' $
	'Pick1D...       - picks columns or rows from a widget table' $

	]
	xdisplayfile,text=str,title='Help on scanBrowser'
	return
    END
  'VIEWSPEC_CANCEL': BEGIN
    DC_view_writeConfig,state
    DC_view_cleanup,state
      WIDGET_CONTROL,event.top,/DESTROY
	return
      END

  'VIEWSPEC_FILE_SEQNO': BEGIN
      WIDGET_CONTROL,event.ID,GET_VALUE=n
	seqno = n
	state.fileno = seqno
	v->next,seqno,filename,error=er
	if er eq 0 then begin
	state.v = v
	DC_view_init,filename,state
	WIDGET_CONTROL,state.filenameWID,SET_VALUE=filename
	end
      END
  'VIEWSPEC_SLIDER': BEGIN
	WIDGET_CONTROL,event.id,GET_VALUE=seqno
	state.fileno = seqno
	WIDGET_CONTROL,state.filenoWID,SET_VALUE=seqno
	v->next,seqno,filename,error=er
	state.v = v
	if er eq 0 then begin
	DC_view_init,filename,state
	WIDGET_CONTROL,state.filenameWID,SET_VALUE=filename
	end
      END
  'VIEWSPEC_FIRST_FILE': BEGIN
	seqno = 0
	state.fileno = seqno
	v->first,seqno,filename
	state.v = v
	DC_view_init,filename,state
	WIDGET_CONTROL,state.filenoWID,SET_VALUE=seqno
	WIDGET_CONTROL,state.filenameWID,SET_VALUE=filename
      END
  'VIEWSPEC_NEXT_FILE': BEGIN
	seqno = state.fileno+1
	state.fileno = seqno
	WIDGET_CONTROL,state.filenoWID,SET_VALUE=seqno
	v->next,seqno,filename,error=er
	state.v = v
	if er eq 0 then begin
	DC_view_init,filename,state
	WIDGET_CONTROL,state.filenameWID,SET_VALUE=filename
	end
      END
  'VIEWSCAN_PREV_FILE': BEGIN
	seqno = state.fileno-1
	state.fileno = seqno
	WIDGET_CONTROL,state.filenoWID,SET_VALUE=seqno
	v->prev,seqno,filename,error=er
	state.v = v
	if er eq 0 then begin
	DC_view_init,filename,state
	WIDGET_CONTROL,state.filenameWID,SET_VALUE=filename
	end
      END
  'VIEWSPEC_LAST_FILE': BEGIN
	v->last,seqno,filename
  	state.lastno = seqno
	state.v = v
	state.fileno = seqno
	WIDGET_CONTROL,state.filenoWID,SET_VALUE=seqno
	DC_view_init,filename,state
	WIDGET_CONTROL,state.filenameWID,SET_VALUE=filename
      END
  'VIEWSPEC_LINE_SEQNO': BEGIN
      WIDGET_CONTROL,event.ID,GET_VALUE=n
	if n(0) lt 1 or n(0) gt state.maxno then begin
		str = ['Invalid 1D Line seq # entered', $ 
			'Valid range: [1-'+strtrim(state.maxno,2)+']', $ 
			'Reset to 1']
		ret=dialog_message(str,/Info)
		n = 1
		WIDGET_CONTROL,event.ID,SET_VALUE=n
	end
	state.lineno = n(0)
      END
  'VIEWSPEC_PLOT1D': BEGIN
	if state.dim eq 1 then $
	v->plot1d,state.lineno,/data,group=Event.top
	return
      END
  'VIEWSPEC_PICKXAXIS': BEGIN
	if state.dim eq 1 then $
	v->plot1d,state.lineno,group=Event.top,xsel=Event.Index  ;.xaxis
      END
  'VIEWSPEC_STATISTIC': BEGIN
	title = '1D Line # '+strtrim(state.lineno,2) + $
		',    Detector D'+strtrim(state.detno,2)
	v->statistic,detector=state.detno,row=state.lineno,group=Event.top,$
		Title=title,report='statistic.rpt'
	return
      END
  'VIEWSPEC_EZFIT': BEGIN
	v->ezfit,row=state.lineno,detno=state.detno
	return
	v->read,view=state.detno,dim=dim,cpt=cpt, $
		da1d=da1d,pa1d=pa1d,da2d=da2d,pa2d=pa2d, $
		x=xa,y=ya,im=im,labels=labels,id_def=id_def
	if dim eq 2 then begin
	ez_fit,xarray=xa,yarray=ya,im=im,group=Event.top,inpath=state.path, $
		ipick=state.lineno-1
	end
	if dim eq 1 then begin
		xa = pa1d(*,0)
		def = id_def(*,0)
		def = def(4:n_elements(def)-1)
		nd = fix(total(def)+.1)
		ya = make_array(cpt(0),nd)
		id =0
		for i=0,nd-1 do begin
		if def(i) gt 0 then begin
		ya(*,id) = da1d(*,i)
		id = id + 1
		end
		end
	ez_fit,xarray=xa,yarray=ya,group=Event.top,inpath=state.path, $
		jpick=state.detno
	end
      END
  'VIEWSPEC_ASCII1D': BEGIN
	if state.dim eq 1 then $
	v->ascii1d,state.lineno,format=state.format,group=Event.top
      END
  'VIEWSPEC_VIEW3D_2D': BEGIN
	v->view3d_2d,state.detno,group=Event.top
      END
  'VIEWSPEC_STARTNO': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=no
	state.lineno = no
      END
  'VIEWSPEC_ENDNO': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=no
	state.endno = no
      END
  'VIEWSPEC_ASCII2D': BEGIN
	v->ascii2d,state.detno,format=state.format,group=Event.top
      END
  'VIEWSPEC_PLOT2D': BEGIN
	v->view2d,state.detno,/plot,group=Event.top
      END
  'VIEWSPEC_FORMAT': BEGIN
      WIDGET_CONTROL,event.ID,GET_VALUE=fm
	state.format = fm(0)
      END
  'VIEWSPEC_HELPMULTI': BEGIN
	str = [ 'Multiple Selection:', $
		'Select 1D scan # for Overlay plot from 2D image ', $
		'Click any item from scroll list selecting the line scan', $
		'Click while hold down CNTL adding the item to the list', $
		'Click while hold down SHIFT adding the items between last two clicks', $
		'Press Overlay Plot... button plot the selected lines']
	res = dialog_message(str,/info,title='Help on Multiple Selection')
	return
      END
  'VIEWSPEC_OVERLAY': BEGIN
	v->read,view=state.detno,dim=dim,cpt=cpt, $
		x=x,y=y,im=im,labels=labels,id_def=id_def
	res = state.list_sel(0:state.sel_no-1)
	ndim = n_elements(res)
	sz = size(im)
	newy = make_array(sz(1),ndim)
	for i=0,n_elements(res)-1 do begin
		newy(*,i) = im(*,res(i))
		if i eq 0 then legend = strtrim(res(i)+1,2) else $
		legend = [legend,strtrim(res(i)+1,2)]
	end
	title = 'Detector D'+strtrim(state.detno,2) + ' Overlay Plot'
	xtitle=labels(0)
	ytitle=labels(4,0)
	wtitle = '1D Overlay Plot'
	comment=['','File: '+state.filename]
	plot1d,x,newy,xtitle=xtitle,ytitle=ytitle,wtitle=wtitle, $
		legend=legend, /data, $
		title=title,comment=comment,group=Event.top
      END
  'VIEWSPEC_1DSELECT': BEGIN
        res = widget_info(Event.ID,/LIST_SELECT)
        state.lineno = res(0) + 1
	if n_elements(res) gt 1 then begin
	state.sel_no = n_elements(res)
	state.list_sel = res(0:state.sel_no-1)
	end
      END
  'VIEWSPEC_2DSELECT': BEGIN
        res = widget_info(Event.ID,/LIST_SELECT)
	if state.def(res+4) eq 0 then begin
		r = dialog_message('Error: detector not defined!!',/error)
		return
	end
        state.detno = res + 1
        WIDGET_CONTROL,event.top,SET_UVALUE=state

	if state.dim eq 2 then $
	v->view2d,state.detno,group=Event.top

	if state.dim eq 3 then begin
	if state.toobig then $
		WIDGET_CONTROL,state.base3dWID2,SENSITIVE=0
	WIDGET_CONTROL,/HOURGLASS
	  if state.toobig eq 0 then $
		v->view3d_2d,state.detno,group=Event.top else $
	  scanSee_pick3d,state.filename,pickDet=state.detno
	end
	return
      END
  'VIEWSPEC_3DIMAXIS': BEGIN
	state.rank = Event.Value
	num = state.depth
	if state.rank eq 1 then num = state.height
	if state.rank eq 0 then num = state.width
	str2= strtrim(indgen(num),2)
	WIDGET_CONTROL,state.list3dWID,SET_VALUE=str2
	state.slice = 0
	WIDGET_CONTROL,state.list3dWID,SET_LIST_SELECT=state.slice
      END
  'VIEWSPEC_3DSELECT': BEGIN
        res = widget_info(Event.ID,/LIST_SELECT)
        state.slice = res(0) 
      END
  'VIEWSPEC_3D_2DMENU': BEGIN
	VIEWSPEC_3D_2DMENU_event, Event, state
      END
  'VIEWSPEC_CALIB2D': BEGIN
	v->calibration,GROUP=Event.top
      END
  'VIEWSPEC_PICK1D': BEGIN
	v->calibration,pick1d=state.detno,GROUP=Event.top
      END
  'VIEWSPEC_ROI2D': BEGIN
	v->ROI,state.detno,GROUP=Event.top
      END
  'PDMENU2D_PANIMAGE_SCANSEE': scanSee_PDMENU2D_PanImage_Event,state,Event
  'PDMENU3D_PANIMAGE_SCANSEE': scanSee_PDMENU3D_PanImage_Event,state,Event
  'PDMENU_ASCII1D': PDMENU_ASCII1D_Event,state,Event
  ENDCASE

      WIDGET_CONTROL,event.top,SET_UVALUE=state
END


; DO NOT REMOVE THIS COMMENT: END SS_VIEWSPEC
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO scanSee, GROUP=Group, fileno=fileno, format=format, filename=filename 
;+
; MODIFICATION HISTORY:
;   02-02-2001   bkc  R2.2 support big 3D scan data set
;                     Add hourglass for initialization
;                     If 3D data is too big the 3D data can be read by selection;                     only
;-

if XRegistered('SS_VIEWSPEC') then return

loadct,39

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

 detname = 'D'+ [strtrim(indgen(9)+1,2),'A','B','C','D','E','F' , $
   '01','02','03','04','05','06','07','08','09', $
   strtrim(indgen(61)+10,2)]

  DC_view_ids = { $
	base:0L,$
	basefileWID: 0L,$
	base2DWID: 0L,$
	base1DWID1:0L, $
	base1DWID2:0L, $
	filetypeWID : 0L, $
	filenameWID : 0L, $
	filenoWID : 0L, $
	sliderWID: 0L, $
	formatWID : 0L, $
	rangeWID : 0L, $
	list1dWID: 0L, $
	list2dWID: 0L, $
	list3dWID: 0L, $
	base3dWID: 0L, $
	base3dWID2: 0L, $
	wid2D: -1, $        ; pan image win
	slice: 0, $     ; axial slice #
	rank: 2, $      ; z axis
	filename : '/home/sricat/CHA/data/rix/cha:_0001.scan', $
	format : 'G18.8', $
	fileno : 0, $
	lastno : 1, $    ; last fileno
	firstno : 0, $
	list_sel : intarr(2000), $
	sel_no: 1, $
	lineno : 1, $         ; start 1D line #
	endno : 1, $          ; end 1D line #
	def : make_array(85+4,3,/int), $
	detno : 16, $
	maxno : 1, $
	home : '', $
	path : '', $
	outpath : '', $
	class : '', $
	dim: 0, $
	width: 0, $
	height: 0, $
	depth: 0, $
	toobig: 0, $
	list : strtrim(indgen(100),2), $
	detname: detname, $
	v : obj_new('scanSee') $
	}

  if keyword_set(fileno) then DC_view_ids.fileno = fileno
  if keyword_set(format) then DC_view_ids.format = format
  if keyword_set(filename) then DC_view_ids.filename = filename

  SS_VIEWSPEC = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, title='scanBrowser R2.2', $
      MAP=1, $
      UVALUE='SS_VIEWSPEC')
  DC_view_ids.base = SS_VIEWSPEC

  BASE1 = WIDGET_BASE(SS_VIEWSPEC, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE1')
  BUTTON2 = WIDGET_BUTTON( BASE1, $
      UVALUE='VIEWSPEC_FILE_OPEN', $
      VALUE='File...')

  BUTTON4 = WIDGET_BUTTON( BASE1, $
      UVALUE='VIEWSPEC_LOADCT', $
      VALUE='Color...')

  BUTTON3 = WIDGET_BUTTON( BASE1, $
      UVALUE='VIEWSPEC_HELP', $
      VALUE='Help...')

  BUTTON10 = WIDGET_BUTTON( BASE1, $
      UVALUE='VIEWSPEC_CANCEL', $
      VALUE='DONE')

  BASE2 = WIDGET_BASE(SS_VIEWSPEC, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')
  DC_view_ids.basefileWID = BASE2

  BASE3 = WIDGET_BASE(BASE2, $
      ROW=1, $
      FRAME=1, $
      MAP=1, $
      UVALUE='BASE3')

  FIELD5 = CW_FIELD( BASE3,VALUE=DC_view_ids.fileno, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='File Seq #:', $
      UVALUE='VIEWSPEC_FILE_SEQNO', $
      XSIZE=5)
  DC_view_ids.filenoWID = FIELD5

  BUTTON6 = WIDGET_BUTTON( BASE3, $
      UVALUE='VIEWSPEC_FIRST_FILE', $
      VALUE='First')

  BUTTON8 = WIDGET_BUTTON( BASE3, $
      UVALUE='VIEWSCAN_PREV_FILE', $
      VALUE='Prev')

  BUTTON7 = WIDGET_BUTTON( BASE3, $
      UVALUE='VIEWSPEC_NEXT_FILE', $
      VALUE='Next')

  BUTTON9 = WIDGET_BUTTON( BASE3, $
      UVALUE='VIEWSPEC_LAST_FILE', $
      VALUE='Last')


  BASE4 = WIDGET_BASE(BASE2, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE4')

  LABEL11 = WIDGET_LABEL( BASE4, $
      UVALUE='VIEWSPEC_FILE_NAME', /DYNAMIC_RESIZE, $
      VALUE=DC_view_ids.filename)
  DC_view_ids.filenameWID = LABEL11

filetype = WIDGET_LABEL( BASE4, $
;      FONT='-dt-application-bold-i-normal-serif-34-240-100-100-p-170-iso8859-1', $
      FONT='-dt-application-bold-i-normal-serif-34-240-100-*', $
      VALUE=' 3D Data ')
  DC_view_ids.filetypeWID = filetype


  BASE12 = WIDGET_BASE(BASE4, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE12')
  BASE12_1 = WIDGET_BASE(BASE12, $
      ROW=1, $
      FRAME=1, $
      MAP=1, $
      UVALUE='BASE12_1')

  BUTTON13 = WIDGET_BUTTON( BASE12_1, $
      UVALUE='VIEWSPEC_PLOT1D', $
      VALUE='PLOT1D...')
  BUTTON19 = WIDGET_DROPLIST( BASE12_1, $
      UVALUE='VIEWSPEC_PICKXAXIS', $
      VALUE=['P1','P2','P3','P4','Step #'],TITLE='vs')
  WIDGET_CONTROL,BUTTON19,SET_DROPLIST_SELECT=0

;  BUTTON16 = WIDGET_BUTTON( BASE12, $
;      UVALUE='VIEWSPEC_ASCII1D', $
;      VALUE='ASCII1D...')
  MenuASCII1D = [ $
      { CW_PDMENU_S,       1, 'ASCII1D' }, $ ;        0
        { CW_PDMENU_S,       0, '1D SCAN...' }, $ ;        1
        { CW_PDMENU_S,       2, '2D SCAN...' } $ ;        1
        ]
  PDMENU_ASCII1D= CW_PDMENU( BASE12, MenuASCII1D, /RETURN_FULL_NAME, $
      UVALUE='PDMENU_ASCII1D')

  FIELD19 = CW_FIELD( BASE12,VALUE=DC_view_ids.format, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Format:', $
      UVALUE='VIEWSPEC_FORMAT', $
      XSIZE=8)
  DC_view_ids.formatWID = FIELD19


  BASE13 = WIDGET_BASE(BASE4, $
      ROW=1, $
      FRAME=1, $
      MAP=1, $
      UVALUE='BASE13')

  DC_view_ids.base1DWID1 = BASE12
  DC_view_ids.base1DWID2 = BASE13

  BUTTON17 = WIDGET_BUTTON( BASE13, $
      UVALUE='VIEWSPEC_EZFIT', $
      VALUE='Ez_Fit...')

  BUTTON18 = WIDGET_BUTTON( BASE13, $
      UVALUE='VIEWSPEC_STATISTIC', $
      VALUE='Statistic...')

  BUTTON35 = WIDGET_BUTTON( BASE13, $
      UVALUE='VIEWSPEC_CALIB2D', $
      VALUE='Run Calibra... ')

  BASE18 = WIDGET_BASE(BASE4, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE18')

  BASE28 = WIDGET_BASE(BASE4, $
      ROW=1, $
      UVALUE='BASE28')

  BASE28_6 = WIDGET_BASE(BASE28, /FRAME, $
      COLUMN=1, UVALUE='BASE28_6')

  MenuDesc496 = [ $
      { CW_PDMENU_S,       3, '2D Menu' }, $ ;        0
        { CW_PDMENU_S,       0, 'PanImages...' }, $ ;        1
        { CW_PDMENU_S,       0, 'Pick 2D...' }, $ ;        1
        { CW_PDMENU_S,       2, 'Calibration...' }, $ ;        1
        { CW_PDMENU_S,       2, 'CALIB1D...' } $ ;        2
  ]
  DC_view3d_2dmenu = CW_PDMENU( BASE28_6, MenuDesc496, /RETURN_FULL_NAME, $
      UVALUE='VIEWSPEC_3D_2DMENU')

  BUTTON25 = WIDGET_BUTTON( BASE28_6, $
      UVALUE='VIEWSPEC_VIEW3D_2D', $
      VALUE='3D_Slicer...')

  BASE28_61 = WIDGET_BASE(BASE28_6, /FRAME, $
      COLUMN=1, UVALUE='BASE28_61')

  Menu3DPANImage = [ $
      { CW_PDMENU_S,       1, '3D PanImage' }, $ ;        0
        { CW_PDMENU_S,       0, 'Calibration...' }, $ ;        1
        { CW_PDMENU_S,       0, 'PanImages...' }, $ ;        1
        { CW_PDMENU_S,       0, 'PanImages+TIFF' }, $ ;        1
        { CW_PDMENU_S,       0, 'PanImages+PICT' }, $ ;        1
        { CW_PDMENU_S,       2, 'PanImages+XDR' } $ ;        1
        ]
  PDMENU3D_panimage = CW_PDMENU( BASE28_61, Menu3DPANImage, /RETURN_FULL_NAME, $
      UVALUE='PDMENU3D_PANIMAGE_SCANSEE')


  DC_view_ids.base3dWID = BASE28_6  
  DC_view_ids.base3dWID2 = BASE28_61  

  BGROUP15 = CW_BGROUP( BASE28_61,['X','Y','Z'],/ROW,/NO_RELEASE, $
		EXCLUSIVE=1, UVALUE='VIEWSPEC_3DIMAXIS')
  WIDGET_CONTROL,BGROUP15,SET_VALUE = DC_view_ids.rank

  LABEL90 = WIDGET_LABEL( BASE28_61, $
      UVALUE='LABEL90', /align_left, $
      VALUE='Slice Index #')
  LIST15 = WIDGET_LIST( BASE28_61,VALUE=DC_view_ids.list(1:15), $
      UVALUE='VIEWSPEC_3DSELECT', $
      YSIZE=3)
  DC_view_ids.list3dWID = LIST15  
  WIDGET_CONTROL,LIST15,SET_LIST_SELECT= DC_view_ids.slice

  BASE28_2 = WIDGET_BASE(BASE28, /FRAME, $
      COLUMN=1, UVALUE='BASE28_2')
  LABEL9 = WIDGET_LABEL( BASE28_2, $
      UVALUE='LABEL9', $
      VALUE='Detector #')

  LIST12 = WIDGET_LIST( BASE28_2,VALUE=DC_view_ids.detname, $
      UVALUE='VIEWSPEC_2DSELECT', $
      YSIZE=8)
  DC_view_ids.list2DWID = LIST12
  widget_control,LIST12,SET_LIST_SELECT=DC_view_ids.detno-1

  BASE28_0 = WIDGET_BASE(BASE28, $
      ROW=1, FRAME=1, $
      UVALUE='BASE28')
  DC_view_ids.base2DWID = BASE28_0

  BASE28_1 = WIDGET_BASE(BASE28_0, $
      COLUMN=1, UVALUE='BASE28_1')
  BUTTON25 = WIDGET_BUTTON( BASE28_1, $
      UVALUE='VIEWSPEC_PLOT2D', $
      VALUE='PLOT2D...')
  BUTTON15 = WIDGET_BUTTON( BASE28_1, $
      UVALUE='VIEWSPEC_ASCII2D', $
      VALUE='ASCII2D...')

  BUTTON36 = WIDGET_BUTTON( BASE28_1, $
      UVALUE='VIEWSPEC_PICK1D', $
      VALUE='2D_PICK1D... ')


  MenuPANImage = [ $
      { CW_PDMENU_S,       1, 'PanImage' }, $ ;        0
        { CW_PDMENU_S,       0, 'Options...' }, $ ;        1
        { CW_PDMENU_S,       2, 'Default' } $ ;        1
        ]
  PDMENU2D_panimage = CW_PDMENU( BASE28_1, MenuPANImage, /RETURN_FULL_NAME, $
      UVALUE='PDMENU2D_PANIMAGE_SCANSEE')

  BASE28_3 = WIDGET_BASE(BASE28_0, /FRAME, $
      COLUMN=1, UVALUE='BASE28_3')
  BUTTON33 = WIDGET_BUTTON( BASE28_3, $
      UVALUE='VIEWSPEC_HELPMULTI', $
      VALUE='Help 1D Line #...')

  LIST4 = WIDGET_LIST( BASE28_3,VALUE=DC_view_ids.list(1:10), $
      UVALUE='VIEWSPEC_1DSELECT', /MULTIPLE, $
      YSIZE=5)
  DC_view_ids.list1dWID = LIST4

; overlay plot
  BUTTON35 = WIDGET_BUTTON( BASE28_3, $
      UVALUE='VIEWSPEC_OVERLAY', $
      VALUE='Overlay Plot...')

if keyword_set(filename) eq 0 then begin
  found = findfile('scanSee.config',count=ct)
  if ct gt 0 then begin
  DC_view_readConfig,filename,path
  if filename ne '' then DC_view_ids.filename = filename
  if path ne '' then DC_view_ids.path = path
  end
endif else begin
  lp = rstrpos(filename,!os.file_sep)+1
  if lp gt 1 then begin
  classname = strmid(filename,lp,strlen(filename)-lp)
	DC_view_ids.path=strmid(filename,0,lp)
	print,DC_view_ids.path
	print,DC_view_ids.filename
  end
end

  WIDGET_CONTROL,DC_view_ids.filenameWID,SET_VALUE=DC_view_ids.filename
  WIDGET_CONTROL, SS_VIEWSPEC, /REALIZE

  if n_elements(filename) eq 0 then filename='.scan' 
  found = findfile(filename,count=ct)
  if ct gt 0 then begin 
;  WIDGET_CONTROL,/HOURGLASS
  DC_view_init,filename,DC_view_ids
  DC_view_ids.v->last,seqno
  DC_view_ids.lastno = seqno

  if seqno gt 1 then begin
  slider = WIDGET_SLIDER(BASE3,VALUE=seqno,MAX=seqno,UVALUE='VIEWSPEC_SLIDER')
  DC_view_ids.sliderWID = slider 
  end
 
  end

  if obj_valid(DC_view_ids.v) eq 0 then $
  WIDGET_CONTROL, DC_view_ids.basefileWID,SENSITIVE=0 else $
  WIDGET_CONTROL,DC_view_ids.filenameWID,SET_VALUE=filename

  WIDGET_CONTROL,SS_VIEWSPEC,SET_UVALUE=DC_view_ids

;  WIDGET_CONTROL, SS_VIEWSPEC, /REALIZE

  XMANAGER, 'SS_VIEWSPEC', SS_VIEWSPEC
END
