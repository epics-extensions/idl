;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
; @read_scan.pro.R2.4
@view3d_2d.pro
@panimage.pro

PRO pick3d_writeConfig,state
        openw,unit,'pick3d.config',/get_lun
        printf,unit,state.filename
        printf,unit,state.path
        printf,unit,''
        free_lun,unit
END

PRO pick3d_readConfig,filename,path
        filename=''
        path=''
        openr,unit,'pick3d.config',/get_lun
        readf,unit,filename
        readf,unit,path
        free_lun,unit

END

PRO pick3dToxdr,pick3d_state,xdrname,append=append

	data = *(pick3d_state.data)
	xdr_open,unit,xdrname ,/write, append=append
	xdr_write,unit,data
	xdr_close,unit
END

PRO scan3d_Writexdr,pick3d_state,xdrname

	det = where(pick3d_state.id_def,0)
	for i=0,n_elements(det)-1 do begin
	pick3d_state.pickDet = det(i)+1
	view3d_pickDet,pick3d_state,data
	data = *(pick3d_state.data)

	if i gt 0 then xdr_open,unit,xdrname ,/write,/append else $
	xdr_open,unit,xdrname ,/write
	xdr_write,unit,data
	xdr_close,unit
	end

END

PRO scan3d_Readxdr,pick3d_state,xdrname,Event

	if pick3d_state.dim ne 3 then begin
		r = dialog_message('Load 3D scan file in first',/error)
		return
	end

	det = where(pick3d_state.id_def,0)
	dnames = pick3d_state.detname(det)

; print,pick3d_state.npts, pick3d_state.pickZind, pick3d_state.panAxis

	npts = pick3d_state.npts
	ndet = n_elements(det)

	title='3D Pick '
	if pick3d_state.panAxis eq 0 then begin
		 image_array = make_array(npts(1),npts(2),ndet)
		 title = 'X Axis : '
		end
	if pick3d_state.panAxis eq 1 then begin
		image_array = make_array(npts(0),npts(2),ndet)
		 title = 'Y Axis : '
		end
	if pick3d_state.panAxis eq 2 then begin
		image_array = make_array(npts(0),npts(1),ndet)
		 title = 'Z Axis : '
		end
	title = title+'Slice #'+string(pick3d_state.pickZind)


	xdr_open,unit,xdrname
	point_lun,unit,0	
	for i=0,ndet-1 do begin
   t1=systime(1)
	xdr_read,unit,data
   print,'Read time =',systime(1)-t1, '  for Detector : ',Det(i), i

	if pick3d_state.panAxis eq 0 then begin
		image_array(*,*,i) = data(pick3d_state.pickZind,*,*)
		xv = *pick3d_state.y
		yv = *pick3d_state.z
		end
	if pick3d_state.panAxis eq 1 then begin
		image_array(*,*,i) = data(*,pick3d_state.pickZind,*)
		xv = *pick3d_state.x
		yv = *pick3d_state.z
		end
	if pick3d_state.panAxis eq 2 then begin
		image_array(*,*,i) = data(*,*,pick3d_state.pickZind)
		xv = *pick3d_state.x
		yv = *pick3d_state.y
		end
	end
	xdr_close,unit

;help,image_array

	def = make_array(ndet,value=1,/int)
	panimage,image_array,def,title= title+' (All Di)'

	if pick3d_state.realaxis eq 0 then $
	calibration_factor,image_array,def,dnames=dnames, $
		title=title,Group=Event.top else $
	calibration_factor,image_array,def,dnames=dnames, $
		title=title,Group=Event.top, $
		xv = xv, yv=yv

END

PRO	view3d_pickDet,pick3d_state, data, Event    ;file,det,data

WIDGET_CONTROL,/HOURGLASS

;if n_elements(Scan3D) ne 0 then begin
;  rank = *Scan3D.dim
;  ptr_free,Scan3D.scanno
;  ptr_free,Scan3D.dim
;  ptr_free,Scan3D.npts
;  ptr_free,Scan3D.cpt
;  ptr_free,Scan3D.id_def
;  ptr_free,Scan3D.pv
;  ptr_free,Scan3D.labels
;  for i=0,rank-1 do begin
;    ptr_free,(*Scan3D.pa)[i]
;    ptr_free,(*Scan3D.da)[i]
;  end
;  ptr_free,Scan3D.pa
;  ptr_free,Scan3D.da
;  ptr_free,Scan3D
;	heap_gc
;end

	file = pick3d_state.filename
	det = pick3d_state.pickDet

t1=systime(1)
	r = read_scan(file,Scan3D,pickDet=det,dump=pick3d_state.debug)
print,'Time used in read_scan = ',systime(1)-t1,'  Detector',det


	if r lt 0 then begin
		ret = dialog_message('Read failed on: '+pick3d_state.filename,/error)
		 return
	end

	pick3d_state.dim = *Scan3D.dim
	if pick3d_state.dim ne 3 then begin
		ret = dialog_message('Sorry!  '+pick3d_state.filename+'  is not a 3D scan file.',/error)
		return
	end

	def = *Scan3D.id_def
	pick3d_state.id_def =  def(4:89-1,*)
	pick3d_state.labels =  *Scan3D.labels
	pick3d_state.pv = *Scan3D.pv
	pick3d_state.npts = *Scan3D.npts
	pick3d_state.pickDet = det
	pick3d_state.scanno = *Scan3D.scanno ;r 

	data = *(*Scan3D.da)[0]

	*(pick3d_state.da1D) =  *(*Scan3D.da)[2]
	*(pick3d_state.da2D) =  *(*Scan3D.da)[1]
	*(pick3d_state.data) = data

;help,data

	z =  *(*Scan3D.pa)[2]
	*(pick3d_state.z) = z(0:pick3d_state.npts(2)-1)
	y = *(*Scan3D.pa)[1]
	*(pick3d_state.y) = y(0:pick3d_state.npts(1)-1)
	x= *(*Scan3D.pa)[0]
	*(pick3d_state.x) = x(0:pick3d_state.npts(0)-1)

	free_scanAlloc,Scan3D
	if ptr_valid(Scan3D) then ptr_free,Scan3D

	pick3d_sensitive_on,pick3d_state

	if n_elements(Event) then begin
	if pick3d_state.id_def(pick3d_state.pickDet-1,0) eq 0 then begin
		ex = where(pick3d_state.id_def(*,0))
		xst = 'Defined detector : '+pick3d_state.detname(ex)
		xs = ['Wrong detector picked : '+pick3d_state.detname(pick3d_state.pickDet-1)]
		xst = [xs,'',xst,'']
		xst = [xst,'Please select the correct detector #']
		res = dialog_message(xst,/info)
;	WIDGET_CONTROL,pick3d_state.listWID,SET_LIST_SELECT=ex(0)	
;	pick3d_state.pickDet = ex(0)+1

	return
	end

	rank = pick3d_state.panAxis
	xv = *(pick3d_state.x)
	yv = *(pick3d_state.y)
	zv = *(pick3d_state.z)

	str = '(vs Values)'
	if pick3d_state.realaxis eq 0 then begin
		xv = indgen(pick3d_state.npts(0))
		yv = indgen(pick3d_state.npts(1))
		zv = indgen(pick3d_state.npts(2))
		str = '(vs Index #)'
	end
	title=pick3d_state.class+':'+pick3d_state.detname(pick3d_state.pickDet-1) + str
	view3d_2D,data,rank,xv,yv,zv, title=title, Group=Event.top
	end
END


PRO pick3d_sensitive_off,pick3d_state

	WIDGET_CONTROL,pick3d_state.dataWID,SENSITIVE= 0
;	WIDGET_CONTROL,pick3d_state.allZlistWID,SENSITIVE= 0
;	WIDGET_CONTROL,pick3d_state.calibWID,SENSITIVE= 0
END

PRO pick3d_sensitive_on,pick3d_state
	WIDGET_CONTROL,pick3d_state.dataWID,SENSITIVE= 1 
;	WIDGET_CONTROL,pick3d_state.calibWID,SENSITIVE= 1
;	WIDGET_CONTROL,pick3d_state.allZlistWID, SENSITIVE=1, $
		SET_VALUE=indgen(pick3d_state.npts(pick3d_state.panAxis),/string)
END

PRO PICK3D_2DMENU_Event, Event,pick3d_state
	if n_elements(*(pick3d_state.da2D)) eq 0 then return
	da2d = *(pick3d_state.da2D)
	x = *(pick3d_state.y)
	y = *(pick3d_state.z)
	title=pick3d_state.class+':'+pick3d_state.detname(pick3d_state.pickDet-1) + ' (3D_2D Result)'

  CASE Event.Value OF
  '3D_2D Menu.Pick 2D...': BEGIN
	pick2d,da2d,x,y,GROUP=Event.top,class=pick3d_state.class, $
		path=pick3d_state.outpath
	END
  '3D_2D Menu.PanImages...': BEGIN
	sz = size(da2d)
	def = pick3d_state.id_def(0:sz(3)-1,1)
	panimage_sel,da2d,def,title= title+' (All Di)'

	id_def = pick3d_state.id_def
	det_def = id_def(*,1)
	nd = max(where(det_def > 0))
	labels = pick3d_state.labels
	x_lbl =  reform(labels(*,1),89,3)
	y_lbl =  reform(labels(*,2),89,3)
	xdescs = x_lbl(0,0)
        zdescs = x_lbl(4:4+sz(3)-1,1)
	if x_lbl(0,1) ne '' then xdescs = xdescs+ ' '+ x_lbl(0,1)
	ydescs = y_lbl(0,0)
	if y_lbl(0,1) ne '' then ydescs = ydescs+ ' '+ y_lbl(0,1)
	image2d,da2d,x,y,title=title,xdescs=xdescs,ydescs=ydescs, $
		zdescs=zdescs,group=Event.top
	END

  '3D_2D Menu.CALIB2D...': BEGIN
	sz = size(da2d)
	def = pick3d_state.id_def(0:sz(3)-1,1)
	panimage,da2d,def,title= title+' (All Di)'
	if pick3d_state.realaxis eq 0 then $
	calibration_factor,da2d,def,   $ 
		title=title,Group=Event.top else $
	calibration_factor,da2d,def,   $ 
		title=title,Group=Event.top, $
		xv = x, yv=y

    END
  '3D_2D Menu.Plot/Pick 1D...': BEGIN
	x = *(pick3d_state.z)
	sz = size(da1d)
	plot1d,x,da1d,Group=Event.top,/data
    END
  '3D_2D Menu.CALIB1D...': BEGIN
	da1d = *(pick3d_state.da1D)
	sz = size(da1d)
	def = make_array(sz(2),value=1,/int)
	x = *(pick3d_state.z)
help,da1d,x,def
;	if pick3d_state.realaxis eq 0 then $
;	calibration_factor,da1d,def,   $ 
;		title=title,Group=Event.top else $
	calibration_factor,da1d,def,   $ 
		title='3D_2D (All 1D Di)',Group=Event.top, $
		xv = x
       END
  ENDCASE
END


PRO PICK3D_Event, Event

  WIDGET_CONTROL, Event.top, GET_UVALUE=pick3d_state
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'PICK3D_2DMENU': BEGIN
	PICK3D_2DMENU_Event, Event, pick3d_state
	END
  'PICK3D_NEXT': BEGIN
	pick3d_sensitive_off,pick3d_state
	no = pick3d_state.scanno +1
	pick3d_state.scanno = no
	seqno = strtrim(no,2)
	st = '0000'
	strput,st,seqno,4-strlen(seqno)
	pick3d_state.class = pick3d_state.prefix+st+pick3d_state.suffix
	if pick3d_state.last lt no then pick3d_state.last = no
	file = pick3d_state.path+pick3d_state.class
	found = findfile(file,count=ct)
	if ct le 0 then begin
		res=dialog_message(file+' not found!',/error)
	endif else WIDGET_CONTROL,pick3d_state.fileWID,set_value=file
	pick3d_state.filename = file
	det = pick3d_state.pickDet
	view3d_pickDet, pick3d_state, data, Event
	END
  'PICK3D_PREV': BEGIN
	pick3d_sensitive_off,pick3d_state
	no = pick3d_state.scanno -1
	if no lt 1 then return  
	pick3d_state.scanno = no
	seqno = strtrim(no,2)
	st = '0000'
	strput,st,seqno,4-strlen(seqno)
	pick3d_state.class = pick3d_state.prefix+st+pick3d_state.suffix
	file = pick3d_state.path+pick3d_state.class
	found = findfile(file,count=ct)
	if ct  le 0 then begin
		res=dialog_message(file+' not found!',/error)
		return
	endif else WIDGET_CONTROL,pick3d_state.fileWID,set_value=file
	pick3d_state.filename = file
	det = pick3d_state.pickDet
	view3d_pickDet, pick3d_state, data, Event
	END

  'PICK3D_PICKFILE': BEGIN
	pick3d_sensitive_off,pick3d_state
	flt = '*'+pick3d_state.suffix+'*'
	file = dialog_pickfile(get_path=p,/read,/must_exist,path=pick3d_state.path, $
		filter=flt,title='Select 3D Scan File Only')
	if file eq '' then return
	pick3d_state.path = p 
	pick3d_state.filename = file

	id = strpos(file,!os.file_sep,/reverse_search)
	pick3d_state.class = strmid(file,id+1,strlen(file)-id)
	ip = strpos(pick3d_state.class,'_',/reverse_search)
	pick3d_state.prefix = strmid(pick3d_state.class,0,ip+1)
	ip = strpos(file,'.',/reverse_search)
	pick3d_state.suffix = strmid(file,ip,strlen(file)-ip)

	WIDGET_CONTROL,pick3d_state.fileWID,SET_VALUE=file
	; set hardware type

	det = pick3d_state.pickDet
	view3d_pickDet, pick3d_state, data, Event
      END
  'PICK3D_LOADCT': BEGIN
	XLOADCT,group=Event.top
      END
  'PICK3D_FILENAME': BEGIN
	pick3d_sensitive_off,pick3d_state
	WIDGET_CONTROL,pick3d_state.fileWID,GET_VALUE=file
	pick3d_state.filename = file

	id = strpos(file,!os.file_sep,/reverse_search)
	pick3d_state.class = strmid(file,id+1,strlen(file)-id)
	ip = strpos(pick3d_state.class,'_',/reverse_search)
	pick3d_state.prefix = strmid(pick3d_state.class,0,ip+1)

	det = pick3d_state.pickDet
	view3d_pickDet, pick3d_state, data, Event
      END
  'PICK3D_READALLXDR': BEGIN
	xdrname = pick3d_state.xdrname
	scan3d_Readxdr,pick3d_state,xdrname,Event
	END
  'PICK3D_ALLXDR': BEGIN
	if pick3d_state.dim ne 3 then begin 
		r = dialog_message('Load 3D scan file in first',/error)
		return
	end
	xdrname = pick3d_state.xdrname
	scan3d_Writexdr,pick3d_state,xdrname
	END
  'PICK3D_PANZINDEX': BEGIN
	r = WIDGET_INFO(Event.Id,/LIST_SELECT)
	pick3d_state.pickZind = r
	END

  'PICK3D_PICKDET': BEGIN
	if pick3d_state.filename eq '' then begin
		res = dialog_message('Please select scan file first!',/error)
		return
	end
	r = WIDGET_INFO(Event.Id,/LIST_SELECT)
	if r ge 0 then pick3d_state.pickDet = r + 1
	if r ge 0 and pick3d_state.id_def(r,0) eq 0 then begin
		res = dialog_message('Detector not defined in this scan',/error)
		goto,resetuv
	end
	view3d_pickDet, pick3d_state, data, Event
	if pick3d_state.dim ne 3 then return
	WIDGET_CONTROL,pick3d_state.dataWID,SENSITIVE= 1 
      END
  'PICK3D_2D': BEGIN
	if n_elements(*(pick3d_state.data)) gt 3 then begin
	data = *(pick3d_state.data)
	rank = pick3d_state.panAxis
	xv = *(pick3d_state.x)
	yv = *(pick3d_state.y)
	zv = *(pick3d_state.z)
	str = '(vs Values)'
	if pick3d_state.realaxis eq 0 then begin
		xv = indgen(pick3d_state.npts(0))
		yv = indgen(pick3d_state.npts(1))
		zv = indgen(pick3d_state.npts(2))
		str = '(vs Index #)'
	end
	title=pick3d_state.class+':'+pick3d_state.detname(pick3d_state.pickDet-1) + str
	view3d_2D,data,rank,xv,yv,zv, title=title, Group=Event.top
	end
      END
  'PICK3D_XDR': BEGIN
	if pick3d_state.dim ne 3 then return
	xdrname = pick3d_state.detname(pick3d_state.pickDet-1)+'.xdr'
	pick3dToxdr,pick3d_state,xdrname
	dir = pick3d_state.outpath+'XDR'
	outname=pick3d_state.class+'.'+xdrname
	rename_dialog,dir,xdrname,outname,GROUP=Event.top
      END
  'PICK3D_DEBUG': BEGIN
      IF Event.Select THEN Sel = 'On' ELSE Sel = 'Off'
	pick3d_state.debug = Event.Select
      CASE Event.Value OF
      0: Print,'Button Debug Turned ', Sel
      ELSE: Message,'Unknown button pressed'
      ENDCASE
      END
  'PICK3D_DONE': BEGIN
	pick3d_writeConfig,pick3d_state
	WIDGET_CONTROL,Event.Top,/DESTROY
	ptr_free,pick3d_state.x
	ptr_free,pick3d_state.y
	ptr_free,pick3d_state.z
	ptr_free,pick3d_state.da1d
	ptr_free,pick3d_state.da2d
	ptr_free,pick3d_state.data
	heap_gc
	exit
      END
  'PICK3D_REALAXIS': BEGIN
	pick3d_state.realaxis = Event.Value
      END
  'PICK3D_PANAXIS': BEGIN
	pick3d_state.panAxis = Event.Value
	nl = pick3d_state.npts(pick3d_state.panAxis)
	if pick3d_state.pickZind ge nl then pick3d_state.pickZind=0
;	WIDGET_CONTROL,pick3d_state.allZlistWID, $
;		SET_VALUE=indgen(nl,/string), $
;		SET_LIST_SELECT=pick3d_state.pickZind
	goto,resetuv
	END
  'PICK3D_PANIMAGE': BEGIN
	if pick3d_state.dim eq 3 then begin
	pick3d_panimage,pick3d_state,Event
	return
	end
      END
  'PICK3D_HELP': BEGIN
	str = getenv('EPICS_EXTENSIONS')
	new = getenv('EPICS_EXTENSIONS_PVT')
	if strlen(new) gt 3 then str = new
	str = str +!os.file_sep+'doc'+!os.file_sep+'pick3d_help.txt'
	xdisplayfile,str,GROUP=Event.top
	return
	END
  ENDCASE

  resetuv:
      WIDGET_CONTROL,Event.Top,SET_UVALUE=pick3d_state
END

PRO pick3d_panimage,pick3d_state,Event
      data = *pick3d_state.data
	x = *pick3d_state.x
	y = *pick3d_state.y
	z = *pick3d_state.z
        npts = pick3d_state.npts
	scanno = pick3d_state.scanno
	pv = pick3d_state.pv
	labels = pick3d_state.labels
	id_def = pick3d_state.id_def
	xv = indgen(npts(0))
	yv = indgen(npts(1))
	zv = indgen(npts(2))
	if pick3d_state.realaxis then begin
		xv = x
		yv = y
		zv = z
		if max(xv) eq min(xv) then xv = indgen(npts(0))
		if max(yv) eq min(yv) then yv = indgen(npts(1))
		if max(zv) eq min(zv) then zv = indgen(npts(2))
	end

	sz = size(data)
      title = pick3d_state.title + pick3d_state.detname(pick3d_state.pickDet-1)
      CASE pick3d_state.panAxis OF
      0: begin
	id_def = make_array(npts(0),value=1,/int)
	  image_data = reform(data,npts(0),npts(1)*npts(2))
	  image_data = transpose(image_data)
	  image_data = reform(image_data,npts(1),npts(2),npts(0))
	title = title+' (All X slices)'
	xdescs = pv[1]
        ydescs = pv[2]
        zdescs = 'S#'+ strtrim(indgen(sz(1))+1,2)
	image2d,image_data,yv,zv,title=title,scanno=scanno,Group=Event.top, $
		xdescs=xdescs,ydescs=ydescs,zdescs=zdescs,/seqnm
;	wid = pick3d_state.panwin(0)
;	panimage,image_data,id_def,title=title,new_win=wid
;	pick3d_state.panwin(0) = wid
	end
      1: begin
	id_def = make_array(npts(1),value=1,/int)
	image_data = make_array(npts(0),npts(2),npts(1))
	
	for k=0,npts(1)-1 do begin
	da = reform(data(*,k,*))
	image_data(*,*,k) = da(*,*)
	end
	title = title+' (All Y slices)'
	xdescs = pv[0]
        ydescs = pv[2]
        zdescs = 'S#'+ strtrim(indgen(sz(2))+1,2)
	image2d,image_data,xv,zv,title=title,scanno=scanno,Group=Event.top, $
		xdescs=xdescs,ydescs=ydescs,zdescs=zdescs,/seqnm
;	wid = pick3d_state.panwin(1)
;	panimage,image_data,id_def,title=title,new_win=wid
;	pick3d_state.panwin(1) = wid
	end
      2: begin
	id_def = make_array(npts(2),value=1,/int)
	title = title+' (All Z slices)'
	image_data = data
	xdescs = pv[0]
        ydescs = pv[1]
        zdescs = 'S#'+ strtrim(indgen(sz(3))+1,2)
	image2d,image_data,xv,yv,title=title,scanno=scanno,Group=Event.top, $
		xdescs=xdescs,ydescs=ydescs,zdescs=zdescs,/seqnm
;	wid = pick3d_state.panwin(2)
;	panimage,image_data,id_def,title=title,new_win=wid
;	pick3d_state.panwin(2) = wid
	end
      ELSE: Message,'Unknown button pressed'
      ENDCASE

END

PRO pick3d_closeReset,wid
   widget_control,wid,get_uvalue=state
   if state.parent then $
   widget_control,state.parent,sensitive=1
END


PRO pick3d,file=file,path=path,debug=debug,pickDet=pickDet,Group=group
;+
; NAME: 
;   pick3d
;
; PURPOSE: 
;       This dialog allows the user dynamically picks the desired 
;       detector numbers out of a 3D scan file. A user can freely
;       view the 3D data of the selected detector by the view3d_2D
;       program.
;
; CALLING SEQUENCE:
;       pick3d [,File=file] [,PickDet=pickDet] [,Path=path] 
;                               [,Debug=debug] [,Group=group]
;
; KEYWORDS:
;  PickDet:    The default selected detector is detector number 16 which is
;              detector D01. If different detector is desired it must be
;              defined in database and falls in (1,85).  
;  File:       Specifies the initial 3D scan file on command line
;  Path:       Full path specifies the starting file directory where the scan 
;              files are stored.
;  Group:      Specifies the parent widget ID. If specified, the destroy of
;              parent window resulted the destroy of Pick3d window.
;  Debug:      If specified, dump the scan read information
;
; RESTRICTIONS:
;    The scan file must be automatically generated by the IOC scan software
;    by version 1.2 format. The file selected must be a 3D scan.
;
; EXAMPLE:
;    PICK3D,path='/home/beams/CHA/data/xxx'
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, Feb 1, 2001.
;       02-15-2001      Add 3D_2D button to view 2D image data
;-

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  PICK3D = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, KILL_NOTIFY='pick3d_closeReset', $
      MAP=1, title='PICK3D (R1.0)', $
      UVALUE='PICK3D')

  BASE2 = WIDGET_BASE(PICK3D, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE3 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE3')

  BUTTON4 = WIDGET_BUTTON( BASE3, $
      UVALUE='PICK3D_PICKFILE', $
      VALUE='File...')

  BUTTON5 = WIDGET_BUTTON( BASE3, $
      UVALUE='PICK3D_LOADCT', $
      VALUE='Color...')

  BUTTON29 = WIDGET_BUTTON( BASE3, $
      UVALUE='PICK3D_HELP', $
      VALUE='Help')

  TextVal1038 = [ $
    '' ]
  TEXT5 = WIDGET_TEXT( BASE2,VALUE=TextVal1038, $
      EDITABLE=1, $
      UVALUE='PICK3D_FILENAME', XSIZE=55, $
      YSIZE=1)

  BASE4 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE3')

  BUTTON18 = WIDGET_BUTTON( BASE4, $
      UVALUE='PICK3D_PREV', $
      VALUE='PrevFile')

  BUTTON19 = WIDGET_BUTTON( BASE4, $
      UVALUE='PICK3D_NEXT', $
      VALUE='NextFile')

;  writeallXDR = WIDGET_BUTTON( BASE4, $
;      UVALUE='PICK3D_ALLXDR', $
;      VALUE='Generate Calib Array')

  if keyword_set(debug) then begin
  Btns1249 = [ $
    'Debug' ]
  BGROUP14 = CW_BGROUP( BASE4, Btns1249, $
      ROW=1, $
      NONEXCLUSIVE=1, $
      UVALUE='PICK3D_DEBUG')
  end

  BASE5 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE5')

  BASE5_1 = WIDGET_BASE(BASE5, $
      COLUMN=1, FRAME=1, $
      MAP=1, $
      UVALUE='BASE5_1')

  pick3d_2d = WIDGET_BUTTON( BASE5_1, $
      UVALUE='PICK3D_2D', $
      VALUE='3D Slicer...') 

  label5_1 = WIDGET_LABEL(BASE5_1,VALUE='Pick Detector #')
  detname =  'D'+[ $
        strtrim(indgen(9)+1,2),'A','B','C','D','E',+'F', $
	'01','02', '03','04', '05','06', '07','08', '09', $
        strtrim(indgen(61)+10,2) $
	]

  LIST10 = WIDGET_LIST( BASE5_1,VALUE=detname, $
      UVALUE='PICK3D_PICKDET', $
      YSIZE=6)
  WIDGET_CONTROL,LIST10,SET_LIST_SELECT=15
  WIDGET_CONTROL,LIST10,SET_LIST_TOP=15

  MenuDesc496 = [ $
      { CW_PDMENU_S,       3, '3D_2D Menu' }, $ ;        0
        { CW_PDMENU_S,       0, 'PanImages...' }, $ ;        1
        { CW_PDMENU_S,       0, 'Pick 2D...' }, $ ;        2
        { CW_PDMENU_S,       0, 'CALIB2D...' }, $ ;        1
        { CW_PDMENU_S,       0, 'Plot/Pick 1D...' }, $ ;        1
        { CW_PDMENU_S,       2, 'CALIB1D...' } $ ;        1

  ]


  pick3d_2dmenu = CW_PDMENU( BASE5_1, MenuDesc496, /RETURN_FULL_NAME, $
      UVALUE='PICK3D_2DMENU')


  BASE5_2 = WIDGET_BASE(BASE5, $
      COLUMN=1, FRAME=1, $
      MAP=1, $
      UVALUE='BASE5_2')

  panImage_3d = WIDGET_BUTTON( BASE5_2, $
      UVALUE='PICK3D_PANIMAGE', $
      VALUE='PanImages...')

  Btns1422 = [ $
    'X', $
    'Y', $
    'Z' ]
  BGROUP17 = CW_BGROUP( BASE5_2, Btns1422, $
      ROW=1, EXCLUSIVE=1, FRAME=1, $
      LABEL_LEFT='Pick 3D Axis', $
      UVALUE='PICK3D_PANAXIS')
  WIDGET_CONTROL,BGROUP17,SET_VALUE=0

  realaxis = CW_BGROUP( BASE5_2, ['Index','Value'], $
	ROW=1, EXCLUSIVE=1, FRAME=1, $
	LABEL_LEFT='Axial Values', $
      UVALUE='PICK3D_REALAXIS')
  WIDGET_CONTROL,realaxis,SET_VALUE=0

;  BUTTON12 = WIDGET_BUTTON( BASE5_2, $
;      UVALUE='PICK3D_XDR', $
;      VALUE='Output XDR Array...') 

;  BASE5_3 = WIDGET_BASE(BASE5, $
;      COLUMN=1, FRAME=2, $
;      MAP=1, $
;      UVALUE='BASE5_3')

;  readallXDR = WIDGET_BUTTON(BASE5_3, $
;      UVALUE='PICK3D_READALLXDR', $
;      VALUE='Calib (All Di)')

;  label5_3 = WIDGET_LABEL(BASE5_3,VALUE='Axial Slice #')
;  allXDR_LIST10 = WIDGET_LIST( BASE5_3,VALUE=indgen(2000,/string), $
;      UVALUE='PICK3D_PANZINDEX', $
;      YSIZE=6)
;  WIDGET_CONTROL,allXDR_LIST10,SET_LIST_SELECT=0

  WIDGET_CONTROL,BASE5_2,SENSITIVE=0
;  WIDGET_CONTROL,BASE5_3,SENSITIVE=0

  BUTTON16 = WIDGET_BUTTON( BASE2, $
      UVALUE='PICK3D_DONE', $
      VALUE='Done')

  cd,current=p
  if keyword_set(path) then p=path

  LOADCT,39

  pick3d_state = { $
	parent: Group, $
	fileWID: TEXT5, $
	listWID: LIST10, $
;	allZlistWID: allXDR_LIST10, $
	dataWID: BASE5_2, $
;	calibWID: BASE5_3, $
	debug: 0, $
	panAxis: 0, $ ;  0 x-axis 1 - y axis, 2 - z axis
	realaxis: 0, $ ;  0-index #  , 1-real value
	panwin: [-1,-1,-1], $
	pickDet: 16, $		; init det number picked
	pickZind: 0, $
	detname: detname, $
	filename: '', $
	path: p+!os.file_sep, $
	outpath: p+!os.file_sep, $
	title: '3D Pick Detector:', $
	xdrname: '3dimage.xdr', $
	class: '', $
	prefix: '', $
	suffix: '.mda', $  ;.scan', $
	scanno: -1, $
	last: -1, $
	dim: -1, $
	pv: strarr(3), $	
	npts: intarr(3), $
	labels: strarr(267,3), $
	id_def: intarr(85,3), $
	x: ptr_new(/ALLOCATE_HEAP), $
	y: ptr_new(/ALLOCATE_HEAP), $
	z: ptr_new(/ALLOCATE_HEAP), $
	da1D: ptr_new(/ALLOCATE_HEAP), $
	da2D: ptr_new(/ALLOCATE_HEAP), $
	data: ptr_new(/ALLOCATE_HEAP) $
	}


  ; read config file if it exists
  found = findfile('pick3d.config',count=ct)
  if ct gt 0 then begin
  	pick3d_readConfig,filename,path
	po = strpos(filename,'.',/reverse_search)
	pick3d_state.suffix = strmid(filename,po,strlen(filename)-po)

  	if filename ne '' then pick3d_state.filename = filename
  	if path ne '' then pick3d_state.path = path
	out= pick3d_state.path + '.tmp'
	openw,1,out,error=error
	if error eq 0 then pick3d_state.outpath = pick3d_state.path
	close,1
	
  end

  if keyword_set(file) then begin
	pick3d_state.filename = file
	id = strpos(file,!os.file_sep,/reverse_search)
	if id gt 0 then pick3d_state.path = strmid(file,0,id+1)
	WIDGET_CONTROL,pick3d_state.fileWID,SET_VALUE=file
	view3d_pickDet, pick3d_state, data, Event

        id = strpos(file,!os.file_sep,/reverse_search)
	if id gt 0 then $
        pick3d_state.class = strmid(file,id+1,strlen(file)-id) else $
	pick3d_state.class = file
        ip = strpos(pick3d_state.class,'_',/reverse_search)
        pick3d_state.prefix = strmid(pick3d_state.class,0,ip+1)

  end

  if keyword_set(pickDet) then begin
	WIDGET_CONTROL,LIST10,SET_LIST_SELECT=pickDet-1 
	pick3d_state.pickDet = pickDet
  end
  WIDGET_CONTROL, PICK3D, SET_UVALUE=pick3d_state
  WIDGET_CONTROL, PICK3D, /REALIZE

  XMANAGER, 'PICK3D', PICK3D
END
