;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************


@image2d.pro
@panimage.pro

PRO rix2DC,Scan,gData
; ON_ERROR,0 ;,1
 
  *gData.scanno  = Scan.scanno
  *gData.dim     = Scan.rank
  *gData.num_pts = Scan.npts
  *gData.cpt     = Scan.cpt
  *gData.id_def  = Scan.id_def
  *gData.pv      = Scan.pv
  *gData.labels  = Scan.labels

	rank = Scan.rank
	IF rank EQ 1 THEN BEGIN
    *gData.pa1D  = *Scan.pa[0]
    *gData.da1D  = *Scan.da[0]
	  *gData.pa2D  = 0	;ptr_new(/ALLOCATE_HEAP)
	  *gData.da2D  = 0
	  *gData.pa3D  = 0
	  *gData.da3D  = 0
	ENDIF
	IF rank EQ 2 THEN BEGIN
    *gData.pa1D  = *Scan.pa[1]
    *gData.da1D  = *Scan.da[1]
    *gData.pa2D  = *Scan.pa[0] 
    *gData.da2D  = *Scan.da[0] 
	  *gData.pa3D  = 0
	  *gData.da3D  = 0
  ENDIF
	IF rank EQ 3 THEN BEGIN
    *gData.pa1D  = *Scan.pa[2]
    *gData.da1D  = *Scan.da[2]
    *gData.pa2D  = *Scan.pa[1]
    *gData.da2D  = *Scan.da[1]
    *gData.pa3D  = *Scan.pa[0]
;if ptr_valid(gData.da3D) eq 0 then *gData.da3D  = ptr_new(/ALLOCATE_HEAP)
if n_elements(*Scan.da[0]) then *gData.da3D  = *Scan.da[0]
	ENDIF
 
	free_lun,Scan.lun
	scanSee_free,Scan

END



FUNCTION read_scan,filename, Scan, dump=dump, lastDet=lastDet,pickDet=pickDet,header=header
; Normallly if lastDet is specified only detectOR 1 to lastDet is extracted
; But if pickDet>=1  if specified only the specified detector is extracted 
;             it is target for big 3D scan 
;     if pickDet <0  then no 3D array is returned for 3D scan
;
;+
; NAME:
;       FUNCTION READ_SCAN,Filename,Scan,Dump=Dump,LastDet=LastDet,PickDet=PickDet,Header=Header
;
; PURPOSE:
;       This function read any 1D/2D/3D scan file and returns a scan pointer 
;       which consists of few heap pointers to point to the data extracted
;       from the XDR  scan file.
; 
;       If succeed it returns the scan number, otherwise it returns -1.
;
; CATEGORY:
;       Function.
;
; CALLING SEQUENCE:
;
;       READ_SCAN(Filename,Scan, ...)
;
; INPUTS:
;       Filename:    Input XDR scan filename
; 
; KEYWORD PARAMETERS:
;       DUMP :  Set this keyword to specify the plot title string.
;
;       PICKDET: Specify the detector # , if specified only the 3D array
;                for the specified detector is returned 
;                If -1 is specified, no 3D data array is returned for
;                the 3D scan
;       LASTDET: [1,1,1] set the initial temp detector numbers for
;                3D scan record, it returns the last detector # 
;                defined in each scan record
;                If pickDet is defined, then the lastDet[0]=1 will be 
;                returned for 3D scan
;       HEADER: Set this keyword to specify the xtitle string.
;
; OUTPUTS:
;       SCAN: The scan data structure composed of heap data pointers.
;             
;             scanno   -  integer pointer of scan number
;             dim      -  integer pointer of scan dimension 
;             npts     -  pointer of requested data point vector (dim)
;             cpt      -  pointer of current data point vector (dim)
;             id_def   -  pointer of defined Pi & Di integer array (85,dim)
;             pv       -  pointer of PV names string array  (85,dim)
;             labels   -  pointer to PV labels string array (85*3,dim)
;             pa       -  pointer to positioner array pointer  (dim)
;             da       -  pointer to detecor array pointer  (dim)
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
; RESTRICTIONS: Required scan filename which is automatically saved
;               by the scan record by IOC. The filename follows the
;               special sequential rule which is ended with '.scan' type.
;
; EXAMPLES:
;         filename = '/home/beams/CHA/data/xxx/cha:_0001.scan'
;         scanno = read_scan(filename,SCAN,SSD)
;          
; MODIFICATION HISTORY:
;       Written by:     Originally written by Eric Boucher 
;                       Modify and extended by Ben-chin K. Cha, Mar. 7, 2001.
;
;-


  debug = 0
  IF keyword_set(dump) THEN debug = dump
  ON_ERROR,0 ;,1
  ON_IOERROR,BAD

  res=0
  ndet = 85 ; 15
  ntot = ndet+4

; SSD->Scan
reread:
  sscan_read,Scan,file=filename,/header,error=error
   if n_elements(Scan) eq 0 then goto,BAD
   if error eq -1 then begin
   scanSee_free,Scan
   goto,BAD  ;reread
   end

  npts = Scan.npts

  if keyword_set(pickDet) then begin
        if Scan.rank lt 3 then pickDet=0
  end

  IF Scan.rank EQ 3  THEN BEGIN
  	IF n_elements(pickDet) EQ 0 THEN BEGIN 
        IF npts(0) GE 1000 or npts(1) GE 500 OR npts(2) GE 500 THEN pickDet = 16
	  ENDIF
 	nel =1L *npts(0)*npts(1)*npts(2)
	if nel gt 500000000L then begin
	msg = ['Warning! 3D scan array dimension kind of big', string(npts), $
		'Only one detector returned : ',string(pickDet)]
	print,msg
	end
  ENDIF

  IF keyword_set(header) THEN GOTO,DONE

  if Scan.rank eq 3 and n_elements(pickDet) then begin
	dpick = where(Scan.id_def(4:88,0) gt 0)
;	if pickDet lt 0 then pick3d=0 else $
	pick3d = where((dpick-pickDet+1) eq 0)
	if pick3d ge -1 then begin
	Scan.pick3d = 1 
	sscan_read3DPick,Scan,da2D,da3D,idet=pick3d
	if Scan.im_filled(2) eq 0 then scanSee_fillVector,Scan,da1D  ;,/echo
	endif else begin
	msg = ['Error: pick3D Detector # '+string(pickDet) + ' not defined!']
	r = dialog_message(msg,/error)
	return,-1
	end
  endif else sscan_read,Scan,file=filename,/data

  ; extract the first DetMax detectORs only
  DetMax = Scan.detMax
  IF keyword_set(lastDet) THEN DetMax = lastDet 

; fill pos array for Scan structure 

  for i=0,Scan.rank-1 do begin
	if npts(0) le 0 then goto,BAD
	p = dblarr(npts(i),4)
	v = *Scan.pa(i)
	np = Scan.nb_pos(i)
	if np gt 0 then begin
	jj = 0
	for j=0,3 do begin
	  if Scan.id_def(j,i) then begin
	  p(*,j) = v(jj:jj+npts(i)-1)
  	  jj = jj + npts(i)
	  end
	end	
	end
	*scan.pa[i] = p
  end

; fill det array for Scan structure 

;print,detMax
;print,Scan.nb_det

  IF Scan.rank EQ 3 THEN BEGIN
  IF n_elements(pickDet) THEN BEGIN
	v = *Scan.da(0)
  ENDIF ELSE BEGIN
  if detMax(0) gt SSD.nb_det(0) then begin
	i = 0
	d = fltarr(npts(0),npts(1),npts(2),detMax(i))
	v = *Scan.da(i)
	np = Scan.nb_det(i)
	if np gt 0 then begin
	jj = 0
	for j=0,detMax(i)-1 do begin
	  if Scan.id_def(4+j,i) then begin
	  d(*,*,*,j) = v(*,*,*,jj) 
  	  jj = jj+1
	  end
	end	
	end
	*(*scan.da)[0] = d
  end
  END
  END

  res= Scan.scanno
  lastDet = DetMax

if debug then begin
print,'pickDet=',pickDet
help,*Scan.da[0],*Scan.da[1],*Scan.da[2]
help,*Scan.pa[0],*Scan.pa[1],*Scan.pa[2]
end

  GOTO,DONE
BAD:
  res= -1
  print, !ERR_STRING
  scanSee_free,Scan
DONE:
  RETURN, res
END

PRO sscan_readConfig,fn
	fn =''
	catch,error_status
	if error_status ne 0 then begin
		close,1
		return
	end
	openr,1,'scanSee.config'
	readf,1,fn
	close,1
END

PRO scanSee_writeConfig,SSD
	catch,error_status
	if error_status ne 0 then begin
		close,1
		return
	end
	openw,1,'scanSee.config'
	printf,1,SSD.file
	printf,1,SSD.path
	close,1
END

PRO scanSee_image2d,SSD,axis
	if SSD.rank eq 1 then return
        da2d = *SSD.da(SSD.rank-2)
	xarr = *SSD.pa(SSD.rank-2)
	yarr = *SSD.pa(SSD.rank-1)
	id_def = SSD.id_def(*,SSD.rank-2)
	id_def = id_def(4:4+SSD.detMax(SSD.rank-2)-1)
	if axis ne 0 then image2d,da2d,id_def=id_def else $
        image2d,da2d,xarr,yarr,id_def=id_def
END

PRO scanSee_plot1d,SSD,xaxis
; plot against the xaxis picked
	if n_elements(*SSD.da(SSD.rank-1)) eq 0 then return
	da1d = *SSD.da(SSD.rank-1)
	pa1d = *SSD.pa(SSD.rank-1)
	if xaxis lt SSD.nb_pos(SSD.rank-1) then x = pa1d(*,xaxis) else $
	xaxis = 4
	title='1D Array from '+strtrim(SSD.rank,2)+'D Scan # '+strtrim(SSD.scanno,2)
	if xaxis eq 4 then plot1d,da1d,/data,title=title else $
	plot1d,x,da1d,/data,title=title
END

PRO scanSee_pickXaxis,Event
	axis = widget_info(Event.id,/droplist_select)
  	widget_control,Event.top,get_uvalue=scanSee_data,/no_copy
	scanSee_data.pick1d = axis
	SSD = *scanSee_data.SSD
 	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
  	scanSee_plot1d,SSD,axis
END

PRO scanSee_pickYaxis,Event
	axis = widget_info(Event.id,/droplist_select)
  	widget_control,Event.top,get_uvalue=scanSee_data,/no_copy
	scanSee_data.pick2d = axis
	SSD = *scanSee_data.SSD
  	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy

	scanSee_image2d,SSD,axis
END

PRO scanSee_pick3d_det,Event
	idet = widget_info(Event.id,/droplist_select)
  	widget_control,Event.top,get_uvalue=scanSee_data,/no_copy
	scanSee_data.pick3d = idet
	if n_elements(*scanSee_data.SSD) then SSD = *scanSee_data.SSD
  	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
	if n_elements(SSD) then $
	sscan_read3DPick,SSD,da2D,pda3D,idet=idet
	nd = where(SSD.id_def(4:88,0) gt 0)
	title=SSD.class+' : 3D Seq # :D_'+strtrim(nd(idet)+1,2)
	view3d_2d,pda3D,title=title,group=Event.top
END

PRO scanSee_field,Event
  widget_control,Event.top,get_uvalue=scanSee_data,/no_copy
  widget_control,Event.id,get_value=file
	catch,error_status
	if error_status ne 0 then begin
	  close,1	
	  r = dialog_message('File: "'+file+  '"  not found!')
	endif else begin
	openr,1,file
	close,1
  	sscan_read,SSD,file=file,/echo
  	*scanSee_data.SSD = SSD
  	WIDGET_CONTROL, scanSee_data.btns_wid, SENSITIVE=1
  	widget_control,scanSee_data.p3d_wid,set_value='Pick3D Seq # '+strtrim(indgen(SSD.nb_det(0))+1,2)
	if SSD.rank eq 3 then WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=1 $
	else WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=0
  	WIDGET_CONTROL, scanSee_data.type_wid, set_value= strtrim(SSD.rank)+'D'
	end
  widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
END

PRO scanSee_first,Event
  widget_control,Event.top,get_uvalue=scanSee_data,/no_copy
  SSD = *scanSee_data.SSD 
  path = SSD.path
  ll = findfile(path+!os.file_sep+'*.mda',count=ct)
  if ct gt 1 then begin
	file= ll(0)
	catch,error_status
	if error_status ne 0 then begin
	  close,1	
	  r = dialog_message('File: "'+file+  '"  not found!')
	endif else begin
	openr,1,file
	close,1
  	sscan_read,SSD,file=file,/echo
  	*scanSee_data.SSD = SSD
  	widget_control,scanSee_data.file_wid,set_value=SSD.file
  	widget_control,scanSee_data.p3d_wid,set_value='Pick3D Seq # '+strtrim(indgen(SSD.nb_det(0))+1,2)
	if SSD.rank eq 3 then WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=1 $
	else WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=0
  	WIDGET_CONTROL, scanSee_data.type_wid, set_value= strtrim(SSD.rank)+'D'
	end
  end  
  widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
END

PRO scanSee_next,Event
  widget_control,Event.top,get_uvalue=scanSee_data,/no_copy
  SSD = *scanSee_data.SSD 
  path = SSD.path
  ll = findfile(path+!os.file_sep+'*.mda',count=ct)
  if ct gt 1 then begin
	id = where(ll eq SSD.file)
	file= ll(id+1)
	catch,error_status
	if error_status ne 0 then begin
	  close,1	
	  r = dialog_message('File: "'+file+  '"  not found!')
	endif else begin
	openr,1,file
	close,1
  	sscan_read,SSD,file=file,/echo
  	*scanSee_data.SSD = SSD
  	widget_control,scanSee_data.file_wid,set_value=SSD.file
  	widget_control,scanSee_data.p3d_wid,set_value='Pick3D Seq # '+strtrim(indgen(SSD.nb_det(0))+1,2)
	if SSD.rank eq 3 then WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=1 $
	else WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=0
  	WIDGET_CONTROL, scanSee_data.type_wid, set_value= strtrim(SSD.rank)+'D'
	end
  end  
  widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
END

PRO scanSee_prev,Event
  widget_control,Event.top,get_uvalue=scanSee_data,/no_copy
  SSD = *scanSee_data.SSD 
  path = SSD.path
  ll = findfile(path+!os.file_sep+'*.mda',count=ct)
  if ct gt 1 then begin
	id = where(ll eq SSD.file)
	file= ll(id-1)
	catch,error_status
	if error_status ne 0 then begin
	  close,1	
	  r = dialog_message('File: "'+file+  '"  not found!')
	endif else begin
	openr,1,file
	close,1
  	sscan_read,SSD,file=file,/echo
  	*scanSee_data.SSD = SSD
  	widget_control,scanSee_data.file_wid,set_value=SSD.file
  	widget_control,scanSee_data.p3d_wid,set_value='Pick3D Seq # '+strtrim(indgen(SSD.nb_det(0))+1,2)
	if SSD.rank eq 3 then WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=1 $
	else WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=0
  	WIDGET_CONTROL, scanSee_data.type_wid, set_value= strtrim(SSD.rank)+'D'
	end
  end  
  widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
END

PRO scanSee_last,Event
  widget_control,Event.top,get_uvalue=scanSee_data,/no_copy
  SSD = *scanSee_data.SSD 
  path = SSD.path
  ll = findfile(path+!os.file_sep+'*.mda',count=ct)
  if ct gt 1 then begin
	id = where(ll eq SSD.file)
	file= ll(ct-1)
	catch,error_status
	if error_status ne 0 then begin
	  close,1	
	  r = dialog_message('File: "'+file+  '"  not found!')
	endif else begin
	openr,1,file
	close,1
  	sscan_read,SSD,file=file,/echo
  	*scanSee_data.SSD = SSD
  	widget_control,scanSee_data.file_wid,set_value=SSD.file
  	widget_control,scanSee_data.p3d_wid,set_value='Pick3D Seq # '+strtrim(indgen(SSD.nb_det(0))+1,2)
	if SSD.rank eq 3 then WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=1 $
	else WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=0
  	WIDGET_CONTROL, scanSee_data.type_wid, set_value= strtrim(SSD.rank)+'D'
	end
  end  
  widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
END

PRO SCANSEE_READ_PICK3D_Event, Event


  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'SCANSEE_READ_PICK3D': BEGIN
	i = widget_info(Event.Id,/list_select)
  	widget_control,Event.Top,get_uvalue=da3d_state
	da3d = da3d_state.da3d
	detID = da3d_state.detID
  	widget_control,Event.Top,set_uvalue=da3d_state
	da = da3d(*,*,*,i)
	view3d_2d,da,group=Event.top,title='3D Array Seq # '+': D_'+strtrim(detID(i)+1,2)
      END
  'SCANSEE_READ_DONE': BEGIN
	widget_control,event.top,/destroy
      END
  ENDCASE
END




PRO sscan_read_pick3d,da3D,detID=detID,last=last,GROUP=Group


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  SCANSEE_READ = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
      MAP=1, $
      TITLE='Pick 3D Det', $
      UVALUE='SCANSEE_READ')

  BASE2 = WIDGET_BASE(SCANSEE_READ, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  ListVal1026 = '3D Array Seq # '+ strtrim(indgen(85)+1,2)
  sz = size(da3d)
  ListVal1026= ListVal1026(0:sz(4)-1)

  ListVal1026= '3D Array Seq # D_'+ strtrim(detID+1,2) 


  LIST3 = WIDGET_LIST( BASE2,VALUE=ListVal1026, $
      FRAME=5, $
      UVALUE='SCANSEE_READ_PICK3D', $
      YSIZE=15)

  BUTTON4 = WIDGET_BUTTON( BASE2, $
      UVALUE='SCANSEE_READ_DONE', $
      VALUE='Close')


  WIDGET_CONTROL, SCANSEE_READ, /REALIZE
  da3d_state = { pick: 0, da3d:da3d, detID:detID }
  widget_control,SCANSEE_READ,set_uvalue=da3d_state

  XMANAGER, 'SCANSEE_READ_PICK3D', SCANSEE_READ
END

PRO scanSee_free,SSD

if n_elements(SSD) gt 0 then begin
  if n_elements(SSD) gt 1 then begin
	free_lun,SSD.lun
	close,SSD.lun

	for i=0,2 do begin
	ptr_free,SSD.da(i)
	ptr_free,SSD.pa(i)
	end
	for i=0,1 do begin
	ptr_free,SSD.sub_scan_ptr(i)
	end
	ptr_free,SSD.EH
  end
end
SSD = 0	
heap_gc
; print,ptr_valid()
END

PRO sscan_readHeader,file=file,SSD,echo=echo,error=error
;  this routine closes the old lun and frees the pointer
;  then open the file set create the data pointer and
;  leave the file lun open until next call of this routine
;  

error=0
catch,error_status
if error_status ne 0 then begin
	scanSee_free,SSD
	error=-1
	return
end

   scanSee_free,SSD

; i need to check 106.mda

  pos_info= { $
    pxpv:'', $
    pxds:'', $
    pxsm:'', $
    pxeu:'', $
    rxpv:'', $
    rxds:'', $
    rxeu:'' }
  det_info= { $
    dxpv:'', $
    dxds:'', $
    dxeu:'' }
  trg_info= { $
    txpv:'', $
    txcd: 1.0 }

ntot = 85+4

HD_P = make_array(4,3,value=pos_info)
HD_D = make_array(85,3,value=det_info)
HD_T = make_array(4,3,value=trg_info)

  DetMax = [0,0,0]
  id_def = intarr(ntot,3)
  cd,current=p
  SSD = { file  : '', $
	class	: '', $
	path : p, $
	lun : -1, $
	rank  : 0, $
	scanno : 0, $
	npts	: [0,0,0], $
	cpt	: [0,0,0], $
	nb_pos  : [0,0,0],$
	nb_det  : [0,0,0],$
	nb_trg  : [0,0,0],$
	id_def : id_def, $
	HD_P	: HD_P, $
	HD_D	: HD_D, $
	HD_T	: HD_T, $
	lrecl	: 0L, $ 	1D scan lrecl
	offset	: 0L, $ 	1D scan offset for detector data
	dlen	: 0L, $ 	1D scan detector data length
	pick3d  : 0, $		1 - pick 3D array, 0 - whole 3D array
	labels  : strarr(3*ntot,3), $
	detMax  : [0,0,0], $
	pv	: ['','',''], $
	ts1	: '', $
	ts2	: '', $
	im_filled : [0,0,0], $
	noenv  : 0, $
	envPtr : 0L, $
	EH	: ptr_new(/allocate_heap), $
	da	: ptrarr(3,/allocate_heap), $
	pa	: ptrarr(3,/allocate_heap), $
	sub_scan_ptr : ptrarr(2,/aLLOCATE_HEAP) $	
	}

SSD.file = file
len = strpos(SSD.file,!os.file_sep,/reverse_search)
if len gt 0 then SSD.path = strmid(SSD.file,0,len)
SSD.class = strmid(SSD.file,len+1,strlen(file)-len)

	labels = SSD.labels

get_lun,lun
openr,/XDR,lun,file
SSD.lun = lun

  tmp= {$
    version: 0.0, $
    scanno : 0L , $
    rank   : 0L }

  readu,lun, tmp
  SSD.rank = tmp.rank
  SSD.scanno = tmp.scanno

  npts= intarr(tmp.rank)
  readu,lun, npts
  dim = reverse(npts)

  isRegular=0
  readu,lun, isRegular
  env_fptr=0L
  readu,lun, env_fptr
  SSD.envPtr = env_fptr

  for ir=1,tmp.rank do begin

  rank=0
  npts=0
  cpt=0
  readu,lun,rank,npts,cpt
  SSD.npts(rank-1) = npts
  SSD.cpt(rank-1) = cpt

  IF(rank GT 1) THEN BEGIN
    sub_scan_ptr=lonarr(npts)
    readu,lun,sub_scan_ptr
    *SSD.sub_scan_ptr[ir-1] = sub_scan_ptr  ; exist only rank > 1
    if keyword_set(echo) then print,*SSD.sub_scan_ptr[ir-1]
  ENDIF


  ; read the pvname
  name=''
  time=''
  readu,lun,name,time
  SSD.pv(rank-1) = name
  SSD.ts1 = time

  nb_pos=0
  nb_det=0
  nb_trg=0
  readu,lun,nb_pos,nb_det,nb_trg
  SSD.nb_pos(rank-1) = nb_pos 
  SSD.nb_det(rank-1) = nb_det 
  SSD.nb_trg(rank-1) = nb_trg 
  if keyword_set(echo) then print,nb_pos,nb_det,nb_trg


  if tmp.rank eq 3 then begin
    if ir eq 1 then begin
	nar = 1L*dim(0)*dim(1)*dim(2)
	if dim(0) ge 1000 or nar gt 2601000 then SSD.pick3d = 1
	if nb_det gt 0 then $
	  *SSD.da(2) = make_array(dim(2),nb_det) else $
	  *SSD.da(2) = make_array(dim(2),1) 
    end
    if ir eq 2 then begin
	if nb_det gt 0 then $
	  *SSD.da(1) = make_array(dim(1),dim(2),nb_det) else $
	  *SSD.da(1) = make_array(dim(1),dim(2),1) 
    end
    if ir eq 3 then begin
	if SSD.pick3d eq 0 and nb_det gt 0 then $
	  *SSD.da(0) = make_array(dim(0),dim(1),dim(2),nb_det) else $
	  *SSD.da(0) = make_array(dim(0),dim(1),dim(2),1) 
    end
  end

  if tmp.rank eq 2 then begin
     if ir eq 1 then begin
	if nb_det gt 0 then $
	   *SSD.da(1) = make_array(dim(1),nb_det) 
     end
     if ir eq 2 then begin
	if nb_det gt 0 then $
	   *SSD.da(0) = make_array(dim(0),dim(1),nb_det)
     end
  end

  IF(nb_pos NE 0) THEN pos_num= intarr(nb_pos)
  IF(nb_det NE 0) THEN det_num=intarr(nb_det)
  IF(nb_trg NE 0) THEN trg_num=intarr(nb_trg)

; read header

  num=0
  FOR i=0,nb_pos-1 DO BEGIN
    readu,lun, num
    readu,lun,pos_info
    HD_P(num,rank-1) = pos_info
    id_def(num,rank-1) = 1
    if keyword_set(echo) then print,'position:',i,rank,num,pos_info
        if (pos_info.rxpv ne '') then begin
          labels(num,rank-1) = pos_info.rxpv
          labels(ntot+num,rank-1) = pos_info.rxds
          labels(ntot*2+num,rank-1) = pos_info.rxeu
        endif else begin
          labels(num,rank-1) = pos_info.pxpv
          labels(ntot+num,rank-1) = pos_info.pxds
          labels(ntot*2+num,rank-1) = pos_info.pxeu
        end
  END

  FOR i=0,nb_det-1 DO BEGIN
    readu,lun,num
    readu,lun,det_info
    HD_D(num,rank-1) = det_info
    id_def(4+num,rank-1) = 1
    DetMax(rank-1) = num + 1       ; 1 based
   if keyword_set(echo) then print,'detector:',i,rank,num,det_info
          labels(num+4,rank-1) = det_info.dxpv
          labels(ntot+num+4,rank-1) = det_info.dxds
          labels(ntot*2+num+4,rank-1) = det_info.dxeu
  END

  FOR i=0,nb_trg-1 DO BEGIN
    readu,lun,num
    readu,lun,trg_info
    HD_T(num,rank-1) = trg_info
    if keyword_set(echo) then print,'trigger:',i,rank,num,trg_info
  ENDFOR

; read 1st data set

	if nb_pos gt 0 then begin
		pa = make_array(npts,nb_pos,/double)
		readu,lun,pa
	endif else begin
		pa = dblarr(npts)
	end
	*SSD.pa(rank-1) = pa
;help,pa
	if nb_det gt 0 then begin
		da = make_array(npts,nb_det,/float)
		readu,lun,da
	endif else da = fltarr(npts,1)
	if ir eq 1 then *SSD.da(rank-1) = da
  end

;  allocate data array 

  SSD.labels = labels
  SSD.HD_P = HD_P
  SSD.HD_D = HD_D
  SSD.HD_T = HD_T
  SSD.id_def = id_def
  SSD.detMax = detMax


; actual data array from mda file

  nb_det = SSD.nb_det 

  if keyword_set(echo) then begin
	help,*SSD.pa(0),*SSD.pa(1),*SSD.pa(2)
	help,*SSD.da(0),*SSD.da(1),*SSD.da(2)
  end
END

PRO sscan_readENV,SSD,echo=echo
; read the env variable saved with the scan data
;
	pos = SSD.envPtr
	noenv = 0L
	
	lun = SSD.lun
	point_lun,lun,pos

	env_header = { type:0, count:0, name:'',desc:'', unit:'',vl:''}
	noenv=0
	readu,lun,noenv
	SSD.noenv = noenv
	if noenv gt 0 then begin
	EH = make_array(noenv,value=env_header)
	for i=0,noenv-1 do begin
	count=0L
	type=0L
	name=''
	desc=''
	unit=''
	vl=''
	readu,lun,name
	readu,lun,desc
	readu,lun,type
	if type ne 0 then readu,lun,count
	readu,lun,unit
;print,i,' ',name,' ',desc,type,count,' ',unit
	if count gt 0 then begin
	case type of
	29: begin
		v1 = intarr(count)
		readu,lun,v1
	    end
	30: begin
		v1 = fltarr(count)
		readu,lun,v1
	    end
	31: begin
		v1 = intarr(count)
		readu,lun,v1
	    end
	32: begin
		v1 = bytarr(count)
		readu,lun,v1
	    end
	33: begin
		v1 = lonarr(count)
		readu,lun,v1
	    end
	34: begin
		v1 = make_array(count,/double)
		readu,lun,v1
	    end
	endcase
	end
		EH(i).type = type
		EH(i).count = count
		EH(i).name = name
		EH(i).desc = desc
		EH(i).unit = unit
		EH(i).vl = string(vl)
	end
	end

	*SSD.EH = EH
if keyword_set(echo) then begin
print,SSD.file
print,EH
end
END

PRO scanSee_fillVector,SSD,rim_array,echo=echo
; map to real defined detector vector array

  if SSD.nb_det(SSD.rank-1) eq 0 then return
  im_array = *SSD.da(SSD.rank-1)
	x = *SSD.pa(SSD.rank-1)
  sz = size(im_array)
  detMax = SSD.detMax(SSD.rank-1)

if detMax gt sz(2) then begin
  if SSD.im_filled(SSD.rank-1) eq 0 then begin
	rim_array = make_array(sz(1),detMax)
	id_def = SSD.id_def(4:88,SSD.rank-1)
	jj=0
	for ij=0,detMax-1 do begin
		if id_def(ij) then begin
		rim_array(*,ij) = im_array(*,jj)
		jj = jj+1
		end
	end

	*SSD.da(SSD.rank-1) = rim_array
	SSD.im_filled(SSD.rank-1) = 1
  endif else begin
		rim_array = im_array
  end
endif else begin
	rim_array = im_array
end
	if keyword_set(echo) then begin
	title = strtrim(SSD.rank,2)+"D SCAN #"+strtrim(SSD.scanno,2)
	plot1d,x,rim_array,title=title,/data
	end
END


PRO scanSee_fillImage,SSD,rim_array,echo=echo
; map to real defined detector image array

  if SSD.rank lt 2 then return
  im_array = *SSD.da(SSD.rank-2)
  sz = size(im_array)
  detMax = SSD.detMax(SSD.rank-2)

if detMax gt sz(3) then begin
  if SSD.im_filled(SSD.rank-2) eq 0 then begin
	rim_array = make_array(sz(1),sz(2),detMax)
	id_def = SSD.id_def(4:88,SSD.rank-2)
	jj=0
	for ij=0,detMax-1 do begin
		if id_def(ij) then begin
		rim_array(*,*,ij) = im_array(*,*,jj)
		jj = jj+1
		end
	end

	*SSD.da(SSD.rank-2) = rim_array
	SSD.im_filled(SSD.rank-2) = 1
  endif else begin
	rim_array = im_array
  end
endif else begin
	rim_array = im_array
end
	if keyword_set(echo) then begin
	title = strtrim(SSD.rank,2)+"D SCAN #"+strtrim(SSD.scanno,2)
	panimage,rim_array,id_def,numd=10,title=title
	end
END

PRO sscan_read2D,SSD,da2D
; read 2D scan
   if SSD.rank ne 2 then return

	ptr3D = *SSD.sub_scan_ptr(0)
	if n_elements(ptr3D) eq 0 then return

	cpt = SSD.cpt
	da2D = *SSD.da(0)
	for k=0,cpt(1)-1 do begin
	  pos = ptr3D(k)
	  sscan_read1D,SSD,k,level=0,pa=p1,da=d1 
	  if n_elements(d1) gt 1 then $
	  da2D(*,k,*) = d1(*,*)
	  if k eq 0 then pa2D = p1
	end
	*SSD.da(0) = da2D
END

PRO scanSee_pick3D,SSD,da2D,da3D,idet=idet,file=file
	if keyword_set(file) eq 0 then file=SSD.file
	sscan_readHeader,SSD,file=file
	sscan_read3DPick,SSD,da2D,pda3D,idet=idet
	view3d_2d,pda3D,title=SSD.class+' : 3D Seq # '+strtrim(idet+1,2)
END

PRO sscan_read3DPick,SSD,da2D,pda3D,idet=idet
; read 3D scan
   if SSD.rank lt 3 then return
	ptr3D = *SSD.sub_scan_ptr(0)
	if n_elements(ptr3D) eq 0 then return
	if keyword_set(idet) eq 0 then idet=0
	if idet ge SSD.nb_det(0) then return
widget_control,/hourglass
;t1=systime(1)
	da2D = *SSD.da(1)
	pda3D = make_array(SSD.npts(0),SSD.npts(1),SSD.npts(2),1)
	cpt = SSD.cpt
	d = make_array(SSD.npts(0))	; alloc data vector
tp = 0L
	for k=0,cpt(2)-1 do begin
	  pos = ptr3D(k)
	    sscan_read1D,SSD,k,level=0,pa=p1,da=d1 
	  if n_elements(d1) gt 1 and SSD.im_filled(1) eq 0 then $
		 da2D(*,k,*) = d1(*,*)

	if idet ge 0 then begin
 	  point_lun,-SSD.lun,tp1
	  for j=0,cpt(1)-1 do begin
		if j eq 0 then begin
		sscan_read1D,SSD,j,level=1,pa=p2,da=d2 
		  point_lun,-SSD.lun,tp
	  	  SSD.lrecl = tp-tp1
		  dl = size(d2)
		  SSD.dlen = dl(dl(0)+2)*4L
		  SSD.offset = SSD.lrecl-SSD.dlen + dl(1)*idet*4L
		endif else begin
		  rpos =tp+SSD.offset 
			point_lun,SSD.lun,rpos
			readu,SSD.lun,d
		  tp = tp+SSD.lrecl
		end
		if n_elements(d2) gt 1 then $
		pda3D(0,j,k,0) = d
		if j eq 0 then pa3D = p2
	  end
	end

	end
;print,'time used=',systime(1)-t1

	if idet ge 0 then *SSD.da[0] = pda3D
	
	if SSD.im_filled(1) eq 0 then begin
		*SSD.da[1] = da2D
		scanSee_fillImage,SSD,da2D  ;,/echo
	end
widget_control,/clear_events
;	if idet ge 0 then view3d_2d,pda3D,title=SSD.class+' : 3D Seq # '+strtrim(idet+1,2)
END

PRO sscan_read3D,SSD,da2D,da3D
; read 3D scan
   if SSD.rank lt 3 then return

	ptr3D = *SSD.sub_scan_ptr(0)
	if n_elements(ptr3D) eq 0 then return
t1=systime(1)
	da3D = *SSD.da(0)
	da2D = *SSD.da(1)
	cpt = SSD.cpt
	for k=0,cpt(2)-1 do begin
	  pos = ptr3D(k)
	  sscan_read1D,SSD,k,level=0,pa=p1,da=d1 
 	  point_lun,-SSD.lun,tp1
	  if n_elements(d1) gt 1 then da2D(*,k,*) = d1(*,*)
	  if k eq 0 then pa2D = p1
	  for j=0,cpt(1)-1 do begin
;		sscan_read1D,SSD,j,level=1,pa=p2,da=d2 
		if j eq 0 then begin
		sscan_read1D,SSD,j,level=1,pa=p2,da=d2 
		  point_lun,-SSD.lun,tp
	  	  SSD.lrecl = tp-tp1
		  dl = size(d2)
		  SSD.dlen = dl(dl(0)+2)*4L
		  SSD.offset = SSD.lrecl-SSD.dlen
		endif else begin
		  rpos =tp+SSD.offset 
			point_lun,SSD.lun,rpos
			readu,SSD.lun,d2
		end
		point_lun,-SSD.lun,tp
		if n_elements(d2) gt 1 then $
		da3D(*,j,k,*) = d2(*,*)
		if j eq 0 then pa3D = p2
	  end
	end

	*SSD.da(0) = da3D
	*SSD.da(1) = da2D
print,'time used=',systime(1)-t1
END

PRO sscan_read1D,SSD,seqno,level=level,echo=echo,pa=pa,da=da 
; extract 1D data from the multi 1D scan 
; level=1   default from scanH 
; level=0   from scan1 
;
  file = SSD.file
  if n_elements(level) eq 0 then level=1
  fptr = *SSD.sub_scan_ptr(level)	
  pos_info = SSD.HD_P(0,0)
  det_info = SSD.HD_D(0,0)
  trg_info = SSD.HD_T(0,0)

if n_elements(seqno) eq 0 then seqno=0

  lun = SSD.lun

  pos = fptr(seqno)
  point_lun,lun,pos
  if keyword_set(echo) then print,'seqno,pos:',seqno,pos
  rank=0
  npts=0
  cpt=0
  readu,lun,rank,npts,cpt
if keyword_set(echo) then print,'rank,npts,cpt:',rank,npts,cpt

  IF(rank GT 1) THEN BEGIN
    sub_scan_ptr=lonarr(npts)
    readu,lun,sub_scan_ptr
if keyword_set(echo) then print,sub_scan_ptr
	*SSD.sub_scan_ptr(level+1) = sub_scan_ptr
  ENDIF


  ; read the pvname
  name=''
  time=''
  readu,lun,name,time
  SSD.pv(rank-1) = name
  SSD.ts2 = time

  nb_pos=0
  nb_det=0
  nb_trg=0
  readu,lun,nb_pos,nb_det,nb_trg
  if keyword_set(echo) then print,nb_pos,nb_det,nb_trg

  IF(nb_pos NE 0) THEN pos_num= intarr(nb_pos)
  IF(nb_det NE 0) THEN det_num=intarr(nb_det)
  IF(nb_trg NE 0) THEN trg_num=intarr(nb_trg)

; read header

  num=0
  if nb_pos gt 0 then begin
  FOR i=0,nb_pos-1 DO BEGIN
    readu,lun, num,pos_info
  END
  end

  if nb_det gt 0 then begin
  FOR i=0,nb_det-1 DO BEGIN
    readu,lun,num,det_info
  END
  end

  FOR i=0,nb_trg-1 DO BEGIN
    readu,lun,num,trg_info
  ENDFOR

; read data set

	if nb_pos gt 0 then begin
		pa = make_array(npts,nb_pos,/double)
		readu,lun,pa
	endif else begin
		pa = dblarr(npts) + indgen(npts)
	end
	if nb_det gt 0 then begin
		da = make_array(npts,nb_det,/float)
		readu,lun,da
	end



END

PRO sscan_read,SSD,file=file,path=path,echo=echo,pick3d=pick3d,header=header,data=data,error=error
; mda file reader with dialog_pickfile
; if echo=0 read only
;    echo=1 plot window pops up

	error=-1
;	loadct,39

;	CD, current=p
	if n_elements(SSD) then p = SSD.path
	if keyword_set(path) then p = path

	if keyword_set(file) eq 0 then begin
	file = dialog_pickfile(get_path=path,filter='*.mda',/must_exist, $
		path=p,/read,title='Pick MDA file')
	if file(0) eq '' then return
	p = path
	end
;print,file
	f = findfile(file,count=ct)
	if ct eq 0 then begin
		error=-1
		return
	end

	error = strpos(file,'.mda')
	if  error eq -1  then return

	if keyword_set(data) then goto,dataonly
	sscan_readHeader,SSD,file=file,error=error
	if keyword_set(header) then return

dataonly:
WIDGET_CONTROL,/HOURGLASS
	if SSD.rank eq 2 then sscan_read2D,SSD,da2D

	if SSD.rank eq 3 then begin
	if SSD.pick3d then sscan_read3DPick,SSD,da2D,da3D,idet=pick3d else $
	sscan_read3D,SSD,da2D,da3D
	end

  scanSee_fillImage,SSD,rim_array
  scanSee_fillVector,SSD,rim_array

	if keyword_set(echo) eq 0 then return

	if SSD.rank eq 1 then begin
		da1D = *SSD.da(0)
		if keyword_set(echo) then plot1d,da1D,title='SSCAN: Scan # '+strtrim(SSD.scanno,2)
	end
	if SSD.rank eq 2 then begin
		da2d = *SSD.da(0)
		if SSD.nb_det(1) then da1D = *SSD.da(1)
		panimage,da2D,numd=10,title='SSCAN: 2D scan #'+strtrim(SSD.scanno,2)
	end

	if SSD.rank eq 3 then begin 
		da3D = *SSD.da(0)
		da2D = *SSD.da(1)
		da1D = *SSD.da(2)
		title='SSCAN: 3D Scan #'+strtrim(SSD.scanno,2)
		if SSD.nb_det(1) then panimage,da2D,numd=10,title=title else $
		begin
		sz = size(da3d)
		if sz(0) eq 4 then begin
		da = da3d(*,*,0,*)       ; 1st scan 
		da = da3d(*,*,1,*)       ; 2nd scan
		da = reform(da,SSD.npts(0),SSD.npts(1),SSD.nb_det(0))
		title='SSCAN: 3D Scan #'+strtrim(SSD.scanno,2)+' slicer'
		panimage,da,numd=10,title=title
		end
		end
	end

WIDGET_CONTROL,/clear_events
END



PRO scanSee_axisInfo,SSD,Event,axis=axis
	j = 0
	if keyword_set(axis) then j=axis

	openw,1,'1.txt'

	printf,1,''
	printf,1,'NB_POS=',(SSD.NB_POS)[j]
	printf,1,'SSD.HD_P(*,j)'
	help,SSD.HD_P(*,j),output=out
	printf,1,out
	for i=0,(SSD.NB_POS)[j]-1 do begin
		printf,1,i,'  ',SSD.HD_P(i,j)
	end
	printf,1,''
	printf,1,'NB_DET=',(SSD.NB_DET)[j]
	printf,1,'SSD.HD_D(*,j)'
	help,SSD.HD_D(*,j),output=out
	printf,1,out
	for i=0,(SSD.NB_DET)[j]-1 do begin
		printf,1,i,'  ',SSD.HD_D(i,j)
	end
	printf,1,''
	printf,1,'NB_DET=',(SSD.NB_TRG)[j]
	printf,1,'SSD.HD_T(*,j)'
	help,SSD.HD_T(*,j),output=out
	printf,1,out
	for i=0,(SSD.NB_TRG)[j]-1 do begin
		printf,1,i,'  ',SSD.HD_T(i,j)
	end
	close,1
        xdisplayfile,'1.txt',title=SSD.file+' : Axis RANK'+strtrim(j+1,2)+'_INFO',group=Event.top

END

PRO PDMENU2_Event, Event

  widget_control,Event.top,get_uvalue=scanSee_data,/no_copy
  if n_elements(*scanSee_data.SSD) then SSD = *scanSee_data.SSD

  CASE Event.Value OF 


  'File.Open...': BEGIN
;  	if n_elements(*scanSee_data.SSD) then SSD = *scanSee_data.SSD
	sscan_read,SSD,/echo
	if n_elements(SSD) eq 0 then begin
        widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
	return
	end
	*scanSee_data.SSD = SSD
print,SSD.path
  	WIDGET_CONTROL, scanSee_data.file_wid, set_value=SSD.file 
  	WIDGET_CONTROL, scanSee_data.btns_wid, SENSITIVE=1
  	widget_control,scanSee_data.p3d_wid,set_value='Pick3D Seq # '+strtrim(indgen(SSD.nb_det(0))+1,2)
	if SSD.rank eq 3 then WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=1 $
	else WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=0
  	WIDGET_CONTROL, scanSee_data.type_wid, set_value= strtrim(SSD.rank)+'D'
        widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
	return
    END
  'File.Exit': BEGIN
	scanSee_writeConfig,SSD
	widget_control,Event.top,/destroy
    END

  'Setup.Color...': BEGIN
  	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
	xloadct,group=Event.top
    END
  'ViewData.1D Array...': BEGIN
	xaxis = scanSee_data.pick1d
  	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
	scanSee_plot1d,SSD,xaxis
    END
  'ViewData.2D Array...': BEGIN
	axis = scanSee_data.pick2d
  	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
	if SSD.detMax(SSD.rank-2) eq 0 then return
	if n_elements(*SSD.da(SSD.rank-2)) eq 0 then return
	scanSee_image2d,SSD,axis
    END
  'ViewData.3D Array...': BEGIN
	if SSD.pick3d then begin
  	widget_control,scanSee_data.p3d_wid,set_value='Pick3D Seq # '+strtrim(indgen(SSD.nb_det(0))+1,2)
	if SSD.rank eq 3 then WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=1 $
	else WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=0
	end
  	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
	if SSD.rank ne 3 then return
	if n_elements(*SSD.da(0)) eq 0 then return
	detID = where(SSD.id_def(4:88,0))
	if SSD.pick3d eq 0 then $
	sscan_read_pick3d,*SSD.da(0),detID=detID,group=Event.top else begin
	r = dialog_message(title='3D Array...', $
		['Please use "Pick3D Seq # . " droplist', $
		' to select the other desired 3D data array.'],/info)
	title=SSD.class+' : 3D Seq # :D_'+strtrim(detID(0)+1,2)
	view3d_2d,*SSD.da(0),title=title
	end
    END
  'Scan Info.Axes Info...': BEGIN
  	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
	for i=0,SSD.rank-1 do begin
	  scanSee_axisInfo,SSD,Event,axis=i
	end
    END
  'Scan Info.Env Vars...': BEGIN
  	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
  	if n_elements(SSD) then begin
	sscan_readENV,SSD
	openw,1,'1.txt'
	printf,1,'{    Type  Count     Name    Desc    Unit    Value }'
	for i=0,SSD.noenv-1 do begin
		EH = (*SSD.EH)(i)
		printf,1,EH
	end
	close,1
	xdisplayfile,'1.txt',title=SSD.file+' :Scan Info: ',group=Event.top
	end
    END
  'Scan Info.SSD,/struct...': BEGIN
  	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
  	if n_elements(SSD) then begin
	openw,1,'1.txt'
	printf,1,'RANK',SSD.rank	
	printf,1,'NPTS',SSD.NPTS	
	printf,1,'CPT',SSD.CPT	
	printf,1,'NB_POS',SSD.NB_POS	
	printf,1,'NB_DET',SSD.NB_DET	
	printf,1,'NB_TRG',SSD.NB_TRG	
	help,SSD.ID_DEF,output=out
	printf,1,out
	printf,1,'ID_DEF: seq#  Pi#  Rank1   Rank2   Rank3'
	for i=0,3 do begin
	printf,1,i,i,SSD.ID_DEF(i,0),SSD.ID_DEF(i,1),SSD.ID_DEF(i,2)
	end
	printf,1,'ID_DEF: seq#  Di#  Rank1   Rank2   Rank3'
	for i=4,88 do begin
	printf,1,i,i-4,SSD.ID_DEF(i,0),SSD.ID_DEF(i,1),SSD.ID_DEF(i,2)
	end

	help,SSD.LABELS,output=out
	printf,1,out
;	printf,1,'LABELS',SSD.LABELS	
for j=0,2 do begin
	printf,1,'Axial Rank = ',j+1
	printf,1,'Positioner:'
    for i=0,3 do begin
	if SSD.id_def(i,j) then $
	printf,1,format='(i3,2A30,A15)',i,SSD.labels(i,j),SSD.labels(i+89,j),SSD.labels(i+89*2,j)
    end
	printf,1,'Detector:'
    for i=4,88 do begin
	if SSD.id_def(i,j) then $
	printf,1,format='(2I3,2A30,A15)',i,i-4,SSD.labels(i,j),SSD.labels(i+89,j),SSD.labels(i+89*2,j)
    end
    printf,1,''
end
	printf,1,'DETMAX',SSD.DETMAX	
	printf,1,'PV',SSD.PV	
	printf,1,'TS1',SSD.TS1	
	printf,1,'TS2',SSD.TS2	
	printf,1,'IM_FILLED',SSD.IM_FILLED	
	printf,1,'PICK3D',SSD.PICK3D	
	help,*SSD.PA(0),output=out
	printf,1,'PA(0): ',out
	help,*SSD.PA(1),output=out
	printf,1,'PA(1): ',out
	help,*SSD.PA(2),output=out
	printf,1,'PA(2): ',out
	help,*SSD.DA(0),output=out
	printf,1,'DA(0): ',out
	help,*SSD.DA(1),output=out
	printf,1,'DA(1): ',out
	help,*SSD.DA(2),output=out
	printf,1,'DA(2): ',out
	close,1
        xdisplayfile,'1.txt',title=SSD.file+' :SSD INFO: ',group=Event.top
	end
    END
  'Help.SSD,/struct...': BEGIN
  	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
  	if n_elements(SSD) then begin
	help,SSD,/st,output=out
	xdisplayfile,text=out,title='help,SSD,/st',group=Event.top
	end
    END
  'Help.Help': BEGIN
  	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
    PRINT, 'Event for Help.Help'
    END
  ENDCASE
END



PRO SSCAN_MAIN13_Event, Event


  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  ; Event for PDMENU2
  'PDMENU2': PDMENU2_Event, Event

  'FIELD3': BEGIN
	scanSee_field,Event
      END
  'SSCAN_PICKAX': BEGIN
	scanSee_pickXaxis,Event
      END
  'SSCAN_PICKAY': BEGIN
	scanSee_pickYaxis,Event
      END
  'SSCAN_PICK3D': BEGIN
	scanSee_pick3d_det,Event
      END
  'SSCAN_FIRST': BEGIN
	scanSee_first,Event
      END
  'SSCAN_NEXT': BEGIN
	scanSee_next,Event
      END
  'SSCAN_PREV': BEGIN
	scanSee_prev,Event
      END
  'SSCAN_LAST': BEGIN
	scanSee_last,Event
      END
  'SSCAN_DONE': BEGIN
  	widget_control,Event.top,get_uvalue=scanSee_data,/no_copy
	SSD = *scanSee_data.SSD
  	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
	scanSee_writeConfig,SSD
	widget_control,Event.top,/destroy,bad_id=bad
      END

  ENDCASE

END



PRO sscan,file=file,GROUP=Group

if XRegistered('SSCAN_MAIN13') then return
;loadct,39
;@os.init

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  SSCAN_MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
	title='sscan Reader', $
      UVALUE='SSCAN_MAIN13')

  BASE2 = WIDGET_BASE(SSCAN_MAIN13, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  MenuDesc1123 = [ $
      { CW_PDMENU_S,       1, 'File' }, $ ;        0
        { CW_PDMENU_S,       0, 'Open...' }, $ ;        1
        { CW_PDMENU_S,       2, 'Exit' }, $ ;        2
      { CW_PDMENU_S,       1, 'Setup' }, $ ;        3
        { CW_PDMENU_S,       2, 'Color...' }, $ ;        2
      { CW_PDMENU_S,       1, 'ViewData' }, $ ;        3
        { CW_PDMENU_S,       0, '1D Array...' }, $ ;        4
        { CW_PDMENU_S,       0, '2D Array...' }, $ ;        5
        { CW_PDMENU_S,       2, '3D Array...' }, $ ;        6
      { CW_PDMENU_S,       1, 'Scan Info' }, $ ;        7
        { CW_PDMENU_S,       0, 'Axes Info...' }, $ ;       10
        { CW_PDMENU_S,       0, 'Env Vars...' }, $ ;       11
        { CW_PDMENU_S,       2, 'SSD,/struct...' }, $ ;       12
      { CW_PDMENU_S,       3, 'Help' }, $ ;       13
        { CW_PDMENU_S,       0, 'SSD,/struct...' }, $ ;       14
        { CW_PDMENU_S,       2, 'Help' } $  ;     15

  ]


  PDMENU2 = CW_PDMENU( BASE2, MenuDesc1123, /RETURN_FULL_NAME, $
      UVALUE='PDMENU2')

  BASE4 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE4')


  LABEL3 = WIDGET_LABEL( BASE4, $
     UVALUE='LABEL3', $
      VALUE='Scan Type:')

  LABEL6 = WIDGET_LABEL( BASE4, $
      UVALUE='LABEL6', $
      VALUE='          ')

  pick3d = widget_droplist(BASE4,Value='Pick3D Seq # '+ strtrim(indgen(10)+1,2), $
	/frame, UVALUE='SSCAN_PICK3D',Title='')
  widget_control,pick3d,sensitive=0

  pickAx = widget_droplist(BASE4,Value=['P1','P2','P3','P4','Step #'], $
	/frame, UVALUE='SSCAN_PICKAX',Title='Xaxis:')
  
  pickAy = widget_droplist(BASE4,Value=['P1','P2','P3','P4','Step #'], $
	/frame, UVALUE='SSCAN_PICKAY',Title='Yaxis:')
  
  FieldVal854 = '/home/beams/CHA/data/xxx/cha_0001.mda'
  if keyword_set(file) eq 0 then begin
  r = findfile('scanSee.config',count=ct)
  if ct then begin
  	sscan_readConfig,fn
	file = fn
  end
  end
  if keyword_set(file) then FieldVal854 = file
  FIELD3 = CW_FIELD( BASE2,VALUE=FieldVal854, $
      ROW=1, XSIZE=70, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='File:', $
      UVALUE='FIELD3')

  BASE5 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE5')

  BUTTON6 = WIDGET_BUTTON( BASE5, $
      UVALUE='SSCAN_FIRST', $
      VALUE='First')


  BUTTON7 = WIDGET_BUTTON( BASE5, $
      UVALUE='SSCAN_NEXT', $
      VALUE='Next')

  BUTTON8 = WIDGET_BUTTON( BASE5, $
      UVALUE='SSCAN_PREV', $
      VALUE='Prev')

  BUTTON9 = WIDGET_BUTTON( BASE5, $
      UVALUE='SSCAN_LAST', $
      VALUE='Last')

  BUTTON10 = WIDGET_BUTTON( BASE5, $
      UVALUE='SSCAN_DONE', $
      VALUE='Done')


  WIDGET_CONTROL, BASE5, SENSITIVE=0
  WIDGET_CONTROL, SSCAN_MAIN13, /REALIZE
  
  scanSee_data = { base: SSCAN_MAIN13, $
	file_wid : FIELD3, $
	btns_wid : BASE5, $
	p3d_wid : pick3d, $
	type_wid : LABEL6, $
	SSD : ptr_new(/allocate_heap), $
	pick1d : 0, $
	pick2d : 0, $
	pick3d : 0 $
	}

  if keyword_set(file) then begin
	sscan_read,SSD,file=file,/echo,error=error
	if error eq 0 then begin
		*scanSee_data.SSD = SSD
	end
  end
  widget_control,SSCAN_MAIN13,set_uvalue=scanSee_data,/no_copy

  XMANAGER, 'SSCAN_MAIN13', SSCAN_MAIN13,/no_block
END
