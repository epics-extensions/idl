;@os.init
;@scanSee__define.pro
;@xdr_open.pro
;@volume_animator.pro
;@volviewer_eventcb.pro
;@volume_animator.pro

PRO free_scanalloc,scan
	rank = *scan.dim
	ptr_free,scan.scanno
	ptr_free,scan.dim
	ptr_free,scan.npts
	ptr_free,scan.cpt
	ptr_free,scan.id_def
	ptr_free,scan.pv
	ptr_free,scan.labels
	for i=0,rank-1 do begin
	ptr_free,(*scan.pa)[i]
	ptr_free,(*scan.da)[i]
	end
	ptr_free,scan.pa
	ptr_free,scan.da
scan=0
;heap_gc
;print,ptr_valid()
END

PRO mda2vol_message
	st = 'Error: You have to load the image_array in first!!'
	r = dialog_message(st,/error)
END

PRO mda2vol_getLabels,labels,id_def,xlabel,ylabel,zlabel,det_def
	rank = 1
	sz = size(labels)
	if sz(0) eq 2 then rank = sz(2) 
	sz1 = 89 
	if rank le 2 then begin
		det_def = id_def(4:88,0)
		xlabel = labels(sz1:sz1+3,0)
		for i=0,3 do begin
		if strtrim(xlabel(i),2) eq '' then xlabel(i) = labels(i,0)
		end
		zlabel = labels(sz1+4:sz1+88,0)
		for i=0,84 do begin
		if strtrim(zlabel(i),2) eq '' then zlabel(i) = labels(4+i,0)
		end
	end
	if rank eq 2 then begin
		ylabel = labels(sz1:sz1+3,1)
		for i=0,3 do begin
		if strtrim(ylabel(i),2) eq '' then ylabel(i) = labels(i,1)
		end
	end
	if rank eq 3 then begin
		det_def = id_def(4:88,1)
		xlabel = labels(sz1,1)
		if strtrim(xlabel,2) eq '' then xlabel = labels(0,1)
		zlabel = labels(sz1+4:sz1+88,1)
		for i=0,84 do begin
		if strtrim(zlabel(i),2) eq '' then zlabel(i) = labels(4+i,1)
		end
		ylabel = labels(sz1:sz1+3,2)
		for i=0,3 do begin
		if strtrim(ylabel(i),2) eq '' then ylabel(i) = labels(i,2)
		end
	end

END

PRO mda2vol_read,state,pickDet=pickDet

	file = state.path+state.file
	found = findfile(file,count=ct)
	if ct eq 0 then begin
	r = dialog_message([state.path,state.file,'','File not found!'],/error)
	return
	end
	print,file
t1 = systime(1)
	r = read_scan(file,scan,pickDet=pickDet)
t2 = systime(1)
if *scan.dim lt 2 then begin
	r = dialog_message([state.path,state.file,'','Not a 2D/3D file!'],/error)
	return
end
print,'system used  in read_scan (sec) = ',t2-t1

	state.scanno = *scan.scanno
	rank = *scan.dim
	pvs = *scan.pv
	labels = *scan.labels
	state.labels = labels
	id_def = *scan.id_def
	
        mda2vol_getLabels,labels,id_def,xlabel,ylabel,zlabel,det_def

	state.xlabel = xlabel
	state.ylabel = ylabel
	state.zlabel = zlabel
	state.det_def = det_def
	state.pvs = pvs


	if rank eq 2 and keyword_set(pickDet) then begin
		r=dialog_message(/error,['It is not a 3D file!!!', $
		'You have to set File Scan Type to 2D.'])
		return
	end

	da1d = *(*scan.da)[rank-1]
	if rank ge 2 then da2d = *(*scan.da)[rank-2]
	pa1d = *(*scan.pa)[rank-1]
	if rank ge 2 then pa2d = *(*scan.pa)[rank-2]

	sz = size(da2d)
	z = indgen(sz(3))
	x = pa2d(*,0) 
	y = pa1d(*,0)


t3 = systime(1)
if sz(0) eq 3 then  begin
	xdr_open,unit,'vol_'+state.file+'.xdr',/write
	xdr_write,unit,da2d
	xdr_write,unit,x
	xdr_write,unit,y
	xdr_write,unit,id_def   
	xdr_write,unit,pvs
	xdr_write,unit,labels
	xdr_write,unit,da1d
	xdr_close,unit
end
t4 = systime(1)
	*state.da1d = da1d
	*state.data = da2d
	state.det_def = det_def
	*state.x = x 
	*state.y = y 

print,'system used  in write XDR (sec) = ',t4-t3
if state.debug then begin
help,rank
help,pa1d,pa2d,pa3d
help,da1d,da2d
help,x,y,z
print,min(x),max(x)
print,min(y),max(y)
print,min(z),max(z)
end

	free_scanalloc,scan
END




PRO MDA2VOL_MAIN_Event, Event


  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'mda2vol_XDRfile': BEGIN
  WIDGET_CONTROL,Event.top,GET_UVALUE=state,/no_copy
  cd,current=cp
	file = dialog_pickfile(filter='vol*.mda.xdr',/read,get_path=p,$
		path=cp, title='Pick XDR image_array files')
	if file eq '' or p eq file then begin
  		WIDGET_CONTROL,Event.top,SET_UVALUE=state,/no_copy
		return
	end

	state.det_def = intarr(85)
	state.xlabel = strarr(4)
	state.ylabel = strarr(4)
	state.zlabel = strarr(85)
	state.labels = strarr(267,3)
widget_control,/hourglass
t1=systime(1)
	xdr_open,unit,file
	xdr_read,unit,vol,error=error
	xdr_read,unit,x,error=error
	xdr_read,unit,y,error=error
	xdr_read,unit,id_def,error=error
	xdr_read,unit,pvs,error=error
	xdr_read,unit,labels,error=error
	xdr_read,unit,da1d,error=error
	if error ne 0 then begin
		xdr_close,unit
  		WIDGET_CONTROL,Event.top,SET_UVALUE=state,/no_copy
		r = dialog_message(!error_state.msg,/error)
		return
	end
	xdr_close,unit
t2=systime(1)
print,'time used in read XDR file=',t2-t1
	widget_control,state.fileWID,set_value=file

        mda2vol_getLabels,labels,id_def,xlabel,ylabel,zlabel,det_def 

	*state.data = vol
	*state.x = x 
	*state.y = y 
	state.det_def = det_def
	state.pvs = pvs
	state.labels =labels
	state.xlabel = xlabel
	state.ylabel = ylabel
	state.zlabel = zlabel
if state.debug  then begin
help,vol,x,y,det_def
print,det_def
print,file
end

	l = strpos(file,!os.file_sep,/reverse_search)
	name = strmid(file,l+1,strlen(file)-l)
	state.file = name
	detnm = state.detnm
  WIDGET_CONTROL,Event.top,SET_UVALUE=state,/no_copy
	panimage_sel,vol,det_def,detnm=detnm,group=Event.top,title=name
	END

  'mda2vol_MDAfile': BEGIN
  WIDGET_CONTROL,Event.top,GET_UVALUE=state,/no_copy
	file = dialog_pickfile(filter='*.mda',/read,get_path=p,$
		path=state.path, title='Pick 2D/3D MDA files')
	if file eq '' or p eq file then begin
  		WIDGET_CONTROL,Event.top,SET_UVALUE=state,/no_copy
		 return
	end
	state.path = p
	detnm = state.detnm

	r = read_scan(file,scan,/header)
	if r lt 0 then begin
  		WIDGET_CONTROL,Event.top,SET_UVALUE=state,/no_copy
		return
	end

	l = strpos(file,!os.file_sep,/reverse_search)
	name = strmid(file,l+1,strlen(file)-l)
	state.file = name

	widget_control,state.fileWID,set_value=file
	rank = *scan.dim
	npts = *scan.npts
	labels = *scan.labels

        free_scanalloc,scan

	if rank eq 1 then begin
  		WIDGET_CONTROL,Event.top,SET_UVALUE=state,/no_copy
		r = dialog_message('Error: It is not a 2D/3D file',/error)
		return
	end

	if rank eq 3 then pickDet = -1 else pickDet=0

widget_control,/hourglass
	if state.pickDet eq 1 then $
		mda2vol_read,state,pickDet=pickDet else $
      		mda2vol_read,state

	det_def = state.det_def
  if n_elements(*state.data) gt 3 then vol = *state.data else mda2vol_message
  WIDGET_CONTROL,Event.top,SET_UVALUE=state,/no_copy
	if n_elements(vol) gt 3 then begin
	panimage_sel,vol,det_def,detnm=detnm,group=Event.top,title=name
	end
      END

  'mda2vol_images2d': BEGIN
  WIDGET_CONTROL,Event.top,GET_UVALUE=state,/no_copy
	det_def = state.det_def
	name = state.file
	xdescs = state.xlabel
	ydescs = state.ylabel
	zdescs = state.zlabel
	if n_elements(*state.x) gt 1 then xarr = *state.x
	if n_elements(*state.y) gt 1 then yarr = *state.y
  if n_elements(*state.data) gt 3 then vol = *state.data else mda2vol_message
  WIDGET_CONTROL,Event.top,SET_UVALUE=state,/no_copy
	if n_elements(vol) gt 3 then begin
	sz = size(vol)
	id_def = det_def(0:sz(3)-1)
	zdescs = zdescs(0:sz(3)-1)
	ip = strpos(name,'0')
	sc = strmid(name,ip,4)
	scanno = fix(sc)
	image2d,vol,xarr,yarr,id_def=id_def,Group=Event.top,title=name, $
		xdescs=xdescs,ydescs=ydescs,zdescs=zdescs,scanno=scanno
	end
      END

  'mda2vol_view3d_2d': BEGIN
  WIDGET_CONTROL,Event.top,GET_UVALUE=state,/no_copy
	if state.scanno ge 0 then begin
	x = *state.x
	y = *state.y
  if n_elements(*state.data) gt 3 then vol = *state.data else mda2vol_message
	endif else begin
	mda2vol_message
	end
  WIDGET_CONTROL,Event.top,SET_UVALUE=state,/no_copy
	if n_elements(vol) gt 3 then $
	view3d_2d,vol,2,x,y,group=Event.top
      END

  'mda2vol_volviewer': BEGIN
  WIDGET_CONTROL,Event.top,GET_UVALUE=state,/no_copy
  if n_elements(*state.data) gt 3 then vol = *state.data else mda2vol_message
  WIDGET_CONTROL,Event.top,SET_UVALUE=state,/no_copy
	if n_elements(vol) gt 3 then $
	volviewer,vol, group=Event.top
      END

  'mda2vol_help': BEGIN
	st = ['Load MDA File... - Pick 2D/3D MDA scan files, and save the',$
	'                      2D image arrays as XDR vol_file.xdr file', $
	'Load XDR File...  - Load the post-processor vol_file.xdr file',$
	'PanImage...       - Run pan Images with loaded in image_array',$
	'View3d_2d...      - Run view3d_2d with loaded in image_array',$
	'Help...           - Show this page','',$
;	'Run Volviewer...  - Run volviewer with loaded in image_array',$
	'Read 2D Array only - No/Yes toggle button', $
	'                   No - 3D data will be read too', $
	'Debug             - Off/On toggle button', $
	'Filename          - show file name loaded in', $
	'Done              - Close this program','']
 	r = dialog_message(/info,st)
      END

  'mda2vol_debug': BEGIN
  WIDGET_CONTROL,Event.top,GET_UVALUE=state,/no_copy
	state.debug = event.value
  WIDGET_CONTROL,Event.top,SET_UVALUE=state,/no_copy
      END

  'mda2vol_pickDet': BEGIN
  WIDGET_CONTROL,Event.top,GET_UVALUE=state,/no_copy
	state.pickDet = event.value
  WIDGET_CONTROL,Event.top,SET_UVALUE=state,/no_copy
      END

  'mda2vol_done': BEGIN
	widget_control,Event.top,/destroy
	return
      END

  'mda2vol_filename': BEGIN
      Print, 'Event for Filename:'
      END
  ENDCASE
END



PRO mda2vol, filename, GROUP=Group
device,decomposed=0
loadct,39

  detnm = [strtrim(indgen(9)+1,2),'A','B','C','D','E','F', $
	'01','02','03','04','05','06','07','08','09', $
	strtrim(indgen(61)+10,2)]
  detnm = 'D'+detnm

  cd,current=p
  state = { base: 0L, $
	fileWID: 0L, $
	file: '', $
	path: p+!os.file_sep, $
	da1d : ptr_new(/allocate_heap), $
	data : ptr_new(/allocate_heap), $
	x : ptr_new(/allocate_heap), $
	y : ptr_new(/allocate_heap), $
	pvs: ['','',''], $
	labels: strarr(267,3), $
	xlabel: strarr(4), $
	ylabel: strarr(4), $
	zlabel: strarr(85), $
	det_def: intarr(85), $
	detnm : detnm, $
	debug:0, $
	scanno: -1, $
	pickDet :1 }

if n_elements(filename) gt 0 then begin
	l = strpos(filename,!os.file_sep,/reverse_search)
	if l ge 0 then begin
	name = strmid(filename,l+1,strlen(filename)-1)
	state.path = strmid(filename,0,l+1)
	state.file =name
	endif else begin
	state.file = filename
	end
	mda2vol_read,state,pickDet=-1
	return
end

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  MDA2VOL_MAIN = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
      MAP=1, title='MDA2VOL-VIEWER', $
      UVALUE='MDA2VOL_MAIN')

  BASE2 = WIDGET_BASE(MDA2VOL_MAIN, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2')

  BUTTON3 = WIDGET_BUTTON( BASE2, $
      UVALUE='mda2vol_MDAfile', $
      VALUE='Load MDA File...')

  BUTTON3 = WIDGET_BUTTON( BASE2, $
      UVALUE='mda2vol_XDRfile', $
      VALUE='Load XDR File...')

  BUTTON5 = WIDGET_BUTTON( BASE2, $
      UVALUE='mda2vol_images2d', $
      VALUE='Images 2D...')

  BUTTON6 = WIDGET_BUTTON( BASE2, $
      UVALUE='mda2vol_view3d_2d', $
      VALUE='View3d_2d...')

  BUTTON8 = WIDGET_BUTTON( BASE2, $
      UVALUE='mda2vol_help', $
      VALUE='Help...')

  BASE3 = WIDGET_BASE(MDA2VOL_MAIN, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE3')
  Btns212 = [ $
    'No ', $
    'Yes ' ]
  BGROUP4 = CW_BGROUP( BASE3, Btns212, $
      ROW=1, $
      EXCLUSIVE=1, $
      LABEL_LEFT='Read 2D Array only: ', $
      UVALUE='mda2vol_pickDet')
  widget_control,BGROUP4,set_value=1
  Btns215 = [ $
    'Off', $
    'On' ]
  BGROUP5 = CW_BGROUP( MDA2VOL_MAIN, Btns215, $
      ROW=1, $
      EXCLUSIVE=1, $
      LABEL_LEFT='Debug', $
      UVALUE='mda2vol_debug')
  widget_control,BGROUP5,set_value=0

  FIELD2 = CW_FIELD( MDA2VOL_MAIN,VALUE='', $
      ROW=1, $
      STRING=1, $
;      RETURN_EVENTS=1, $
      TITLE='Filename:', $
      UVALUE='mda2vol_filename', $
      XSIZE=70)

;  BUTTON17 = WIDGET_BUTTON( BASE2, $
;      UVALUE='mda2vol_volviewer', $
;      VALUE='Run Volviewer...')

  BUTTON7 = WIDGET_BUTTON( MDA2VOL_MAIN, $
      UVALUE='mda2vol_done', $
      VALUE='Done')

  state.base = MDA2VOL_MAIN
  state.fileWID = FIELD2

  WIDGET_CONTROL, MDA2VOL_MAIN, /REALIZE,set_uvalue=state

  XMANAGER, 'MDA2VOL_MAIN', MDA2VOL_MAIN,/no_block
END
