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
@sb2rpt.pro
@view4d.pro

PRO rix2DC,Scan,gData
; ON_ERROR,0 ;,1
 
  *gData.scanno  = Scan.scanno
  *gData.dim     = Scan.rank
  *gData.num_pts = Scan.npts
  *gData.cpt     = Scan.cpt
  *gData.id_def  = Scan.id_def
  *gData.pv      = Scan.pv
  *gData.labels  = Scan.labels
  if Scan.rank gt 1 then *gData.ts = *Scan.ts2 else *gData.ts = Scan.ts1 
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
 
	IF rank  EQ 4 THEN BEGIN
    *gData.pa1D  = *Scan.pa[3]
    *gData.pa2D  = *Scan.pa[2]
    *gData.pa3D  = *Scan.pa[1]
    *gData.pa4D  = *Scan.pa[0]

    *gData.da1D  = *Scan.da[3]
    *gData.da2D  = *Scan.da[2]
    *gData.da3D  = *Scan.da[1]
    *gData.da4D  = *Scan.da[0]
;;if ptr_valid(gData.da3D) eq 0 then *gData.da3D  = ptr_new(/ALLOCATE_HEAP)
;if n_elements(*Scan.da[0]) then *gData.da4D  = *Scan.da[0]
;if ptr_valid(gData.da4D) eq 0 then *gData.da4D  = ptr_new(/ALLOCATE_HEAP)
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
;       READ_SCAN
;
; PURPOSE:
;       This function reads any 1D/2D/3D scan file and returns a scan pointer 
;       paramenter which consists of few heap pointers to point to the data 
;	extracted from the MDA scan file.
; 
;       If succeeds, it returns the scan number, otherwise it returns -1.
;
; CATEGORY:
;       Function.
;
; CALLING SEQUENCE:
;
;       scanno = READ_SCAN(Filename,Scan, ...)
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
;         scanno = read_scan(filename,Scan)
;          
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, July 27, 2004.
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
  sscan_read,Scan,file=filename,/header,error=error,pick3d=pickDet
   if n_elements(Scan) eq 0 then goto,BAD
   if error eq -1 then begin
;   scanSee_free,Scan
   goto,BAD  ;reread
   end

  npts = Scan.npts

  if keyword_set(pickDet) then begin
        if Scan.rank lt 3 then pickDet=0
  end

  IF keyword_set(header) THEN GOTO,DONE

  case Scan.rank of
  3: begin
  	if n_elements(pickDet) then begin
	if pickDet lt 0 then begin
	  Scan.pick3d = 0
	  sscan_read3D,Scan,da2D,/only2d
	  if Scan.im_filled(Scan.rank-2) eq 0 then  $
		scanSee_fillImage,Scan,da2D  ;,/echo
	endif else begin
	  dpick = where(Scan.id_def(4:88,0) gt 0)
	  pick3d = where((dpick-pickDet+1) eq 0)
	  if pick3d ge -1 then begin
	  Scan.pick3d = 1 
	  sscan_read3DPick,Scan,da2D,idet=pick3d
	  if Scan.im_filled(Scan.rank-2) eq 0 then  $
		scanSee_fillImage,Scan,da2D  ;,/echo
	  end
	end
	endif else begin
	msg = ['Error: pick3D Detector # '+string(pickDet) + ' not defined!']
	r = dialog_message(msg,/error)
	return,-1
	end
     end
   1: sscan_read,Scan,file=filename,/data
   2: sscan_read,Scan,file=filename,/data
  4: begin
 	if  n_elements(pickDet) then begin
	sscan_read4dPick,Scan,pda4d ;,idet=0
	end
     end
  endcase

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

PRO scanSee_image2d,SSD,axis1,axis2,vers=vers

	if SSD.rank eq 1 then return
	xarr = *SSD.pa(SSD.rank-2)
	yarr = *SSD.pa(SSD.rank-1)

	sscan_getDescs,SSD,xdescs,ydescs,zdescs

sz = size(xarr)
if axis1 lt 4 then xd = xdescs(axis1) else xd = 'step #'
if sz(0) eq 2 then begin
  if axis1 lt sz(2) then xarr = xarr(*,axis1) $
  else xarr = indgen(sz(1))
end
if sz(0) eq 1 and axis1 gt 0 then xarr = indgen(sz(1))

sz = size(yarr)
if axis2 lt 4 then yd = ydescs(axis2) else yd = 'step #'
if sz(0) eq 2 then begin
  if axis2 lt sz(2) then yarr = yarr(*,axis2) $
  else yarr = indgen(sz(1))
end
if sz(0) eq 1 and axis2 gt 0 then yarr = indgen(sz(1))

	id_def = SSD.id_def(*,SSD.rank-2)
	id_def = id_def(4:4+SSD.detMax(SSD.rank-2)-1)

;        da2d = *SSD.da(SSD.rank-2)
;	if axis2 ge 4 or axis1 ge 4 then image2d,da2d,id_def=id_def else $
	title=SSD.file
        image2d,*SSD.da(SSD.rank-2),xarr,yarr,id_def=id_def,title=title, $
	xdescs=xd,ydescs=yd,zdescs=zdescs,vers=vers
END

PRO sscan_getDescs,SSD,xdescs,ydescs,zdescs
; for rank 1 only xdescs,ydescs returned
	if SSD.rank eq 1 then begin
	HD_P = SSD.HD_P
	xd = HD_P(*,0)
	xdescs = xd.PXDS
	for i=0,3 do begin
	if strtrim(xd(i).RXEU,2) ne ''  then xdescs(i) = xdescs(i) + ' ('+xd(i).RXEU+')'
	end
	HD_D = SSD.HD_D
	yd = HD_D(0:SSD.detMax(0)-1,0)
	ydescs = yd.DXDS
	return
	end
; for rank 2 or 3
; x desc
	HD_P = SSD.HD_P
	xd = HD_P(*,SSD.rank-2)
	xdescs = xd.PXDS
	for i=0,3 do begin
	if strtrim(xd(i).RXEU,2) ne ''  then xdescs(i) = xdescs(i) + ' ('+xd(i).RXEU+')'
	end
; y desc
	yd = HD_P(*,SSD.rank-1)
	ydescs = yd.PXDS
	for i=0,3 do begin
	if strtrim(yd(i).RXEU,2) ne ''  then ydescs(i) = ydescs(i) + ' ('+yd(i).RXEU+')'
	end
; detector desc
	HD_D = SSD.HD_D
	zd = HD_D(0:SSD.detMax(SSD.rank-2)-1,SSD.rank-2)
	zdescs = zd.DXDS
	pvs = zd.DXPV
	for i=0,n_elements(zdescs)-1 do begin
	if strtrim(zdescs(i),2) eq '' then zdescs(i) = pvs(i)
	end
END

PRO scanSee_plot1d,SSD,xaxis,x=x,da=da

	 rank = SSD.rank

; plot against the xaxis picked
	if n_elements(*SSD.da(rank-1)) eq 0 then return
	pa1d = *SSD.pa(rank-1)
	if xaxis lt SSD.nb_pos(rank-1) then x = pa1d(*,xaxis) else $
	xaxis = 4  ; step #
	title='1D Array from '+strtrim(rank,2)+'D Scan # '+strtrim(SSD.scanno,2)

	HD_P = SSD.HD_P
	xd = HD_P(*,rank-1)
	xdescs = xd.PXDS
	for i=0,3 do begin
	if strtrim(xd(i).RXEU,2) ne ''  then xdescs(i) = xdescs(i) + ' ('+xd(i).RXEU+')'
	end

; detector desc
	id_def = SSD.id_def(4:88,rank-1)
	HD_D = SSD.HD_D
	zd = HD_D(0:SSD.detMax(rank-1)-1,rank-1)
	pvs = zd.DXPV
	zdescs = pvs(where (id_def > 0))

	da1d = *SSD.da(rank-1)
	sz = size(da1d)
	no = n_elements(zdescs)
	da = make_array(sz(1),no)
	ii=0
	for i=0,sz(2)-1 do begin
	if id_def(i) gt 0 then begin
	da(*,ii) = da1d(*,i)
	ii = ii+1
	end
	end

	if xaxis eq 4 then plot1d,da,/data,title=title else $
	plot1d,x,da,/data,title=title,xtitle=xdescs(xaxis),legend=zdescs
END

PRO scanSee_pickXaxis,Event
	axis = widget_info(Event.id,/droplist_select)
  	widget_control,Event.top,get_uvalue=scanSee_data,/no_copy
	scanSee_data.pick1d = axis
	SSD = *scanSee_data.SSD
 	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
	if ssd.nb_det(ssd.rank-1) eq 0 then return
  	scanSee_plot1d,SSD,axis
END

PRO scanSee_pickYaxis,Event
	axis = widget_info(Event.id,/droplist_select)
  	widget_control,Event.top,get_uvalue=scanSee_data,/no_copy
	vers = scanSee_data.vers
	scanSee_data.pick2d = axis
	axis1 = scanSee_data.pick1d
	SSD = *scanSee_data.SSD
	SSD.vers = vers
  	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
	if ssd.nb_det(ssd.rank-2) eq 0 then return
	scanSee_image2d,SSD,axis1,axis,vers=vers
END

PRO scanSee_version,Event
	vs = widget_info(Event.id,/droplist_select)
  	widget_control,Event.top,get_uvalue=scanSee_data,/no_copy
	scanSee_data.vers = vs
  	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
END

PRO SSCAN_DLISTPICK4D_Event, Event

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'SSCAN_DLISTPICK4D': BEGIN
	idet = widget_info(Event.Id,/list_select)
  	widget_control,Event.Top,get_uvalue=da4d_state
	SSD = da4d_state.SSD
  	widget_control,Event.Top,set_uvalue=da4d_state

	sscan_read4dPick,SSD,pda4d,idet=idet
	view4d,pda4d,group=Event.top,SSD=SSD ,title='4D: Detector # '+strtrim(idet,2)
      END
  'SSCAN_DLISTDONE': BEGIN
	widget_control,event.top,/destroy
      END
  ENDCASE
END


PRO sscan_dlistpick4d,GROUP=Group,SSD=SSD
 
  if SSD.rank lt 4 then return

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  SSCAN_DLIST = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
      MAP=1, $
      TITLE='Pick 4D Det', $
      UVALUE='SSCAN_DLIST')

  BASE2 = WIDGET_BASE(SSCAN_DLIST, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')


  lastDet = SSD.nb_det(SSD.rank-4)

  ListVal1026 = '4D Array Seq # '+ strtrim(indgen(lastDet)+1,2)

  LIST3 = WIDGET_LIST( BASE2,VALUE=ListVal1026, $
      FRAME=5, $
      UVALUE='SSCAN_DLISTPICK4D', $
      YSIZE=15)

  BUTTON4 = WIDGET_BUTTON( BASE2, $
      UVALUE='SSCAN_DLISTDONE', $
      VALUE='Close')


  WIDGET_CONTROL, SSCAN_DLIST, /REALIZE
  da4d_state = { SSD:SSD }
  widget_control,SSCAN_DLIST,set_uvalue=da4d_state

  XMANAGER, 'SSCAN_DLISTPICK4D', SSCAN_DLIST
END


PRO scanSee_pick4d_det,Event
	idet = widget_info(Event.id,/droplist_select)
  	widget_control,Event.top,get_uvalue=scanSee_data,/no_copy
	scanSee_data.pick4d = idet
	if n_elements(*scanSee_data.SSD) then SSD = *scanSee_data.SSD
  	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
	
	sscan_read4dPick,ssd,pda4d,idet=idet

	view4d,pda4d,group=Event.top,SSD=SSD ,title='4D: Detector # '+strtrim(idet,2)
END

PRO scanSee_pick3d_det,Event
	idet = widget_info(Event.id,/droplist_select)
  	widget_control,Event.top,get_uvalue=scanSee_data,/no_copy
	scanSee_data.pick3d = idet
	if n_elements(*scanSee_data.SSD) then SSD = *scanSee_data.SSD
  	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
	if ssd.rank eq 3 then begin
	sscan_read3DPick,SSD,da2D,idet=idet
	nd = where(SSD.id_def(4:88,0) gt 0)
	title=SSD.class+' : 3D Seq # :D_'+strtrim(nd(idet)+1,2)
	xv = *ssd.pa[ssd.rank-3]
	yv = *ssd.pa[ssd.rank-2]
	zv = *ssd.pa[ssd.rank-1]
;	if n_elements(ssd.cal_array) eq 0 then  $
	catch,error_status
	if error_status ne 0 then begin
		print,!error_state
		print,'***Incomplete 3D file***'
		goto,bypass
	end
	sscan_readENV,ssd
	cal = *ssd.cal_array
	for i=0,ssd.npts(0)-1 do begin
	xv(i) = cal(idet).cal_offset+cal(idet).cal_slope*i+cal(idet).cal_quad*i*i
	end
bypass:
	view3d_2d,*SSD.da[0],0,xv,yv,zv,kmax=SSD.cpt(ssd.rank-1),title=title,group=Event.top,class=SSD.class
	end
	if ssd.rank eq 4 then begin
;	pda3D = *ssd.da(1)
	nd = where(SSD.id_def(4:88,1) gt 0)
	title=SSD.class+' : 3D Seq # :D_'+strtrim(nd(idet)+1,2)
	view3d_2d,(*ssd.da(1))(*,*,*,idet),kmax=SSD.cpt(ssd.rank-1),title=title,group=Event.top,class=SSD.class
	end
END

PRO scanSee_checkrank,scanSee_data
  	SSD = *scanSee_data.SSD
  	WIDGET_CONTROL, scanSee_data.type_wid, set_value= strtrim(SSD.rank,2)+'D'

	if SSD.rank lt 3 then begin
	    WIDGET_CONTROL,scanSee_data.p4d_wid,SENSITIVE=0 
	    WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=0 
	    return
	end
	if SSD.rank eq 3 then begin
	    WIDGET_CONTROL,scanSee_data.p4d_wid,SENSITIVE=0 
	    WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=1 
	    widget_control,scanSee_data.p3d_wid,set_value='Pick3D Seq # '+strtrim(indgen(SSD.nb_det(0))+1,2)
	    return
	end
	if SSD.rank eq 4 then begin
	  if ssd.nb_det(1) gt 0 then begin
	    WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=1
	    widget_control,scanSee_data.p3d_wid,set_value='Pick3D Seq # '+strtrim(indgen(SSD.nb_det(1))+1,2)
	  endif else WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=0
	  if ssd.nb_det(0) gt 0 then begin
  	    widget_control,scanSee_data.p4d_wid,set_value='Pick4D Seq # '+strtrim(indgen(SSD.nb_det(0))+1,2)
	    WIDGET_CONTROL,scanSee_data.p4d_wid,SENSITIVE=1 
	  endif else WIDGET_CONTROL,scanSee_data.p4d_wid,SENSITIVE=0
	end

END
	
PRO scanSee_field,Event
  widget_control,Event.id,get_value=file
  found = findfile(file,count=ct)
  if ct eq 0 then begin
	r = dialog_message('File: "'+file+  '"  not found!')
	return
  end
  widget_control,Event.top,get_uvalue=scanSee_data,/no_copy
	catch,error_status
	if error_status ne 0 then begin
		print,!error_state
	endif else begin
  	sscan_read,SSD,file=file,/echo,error=error,pick3d=-1
	if error eq 0 then begin
	 scanSee_writeConfig,SSD
  	*scanSee_data.SSD = SSD
  	WIDGET_CONTROL, scanSee_data.btns_wid, SENSITIVE=1
  	widget_control,scanSee_data.p3d_wid,set_value='Pick3D Seq # '+strtrim(indgen(SSD.nb_det(0))+1,2)
	if SSD.rank eq 3 then WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=1 $
	else WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=0
	scanSee_checkrank,scanSee_data
	end
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
	  r = dialog_message('File: "'+file+  '"  read failed!')
	endif else begin
	openr,1,file
	close,1
  	sscan_read,SSD,file=file,/echo,error=error,pick3d=-1
	if error eq 0 then begin
	 scanSee_writeConfig,SSD
  	*scanSee_data.SSD = SSD
  	widget_control,scanSee_data.file_wid,set_value=SSD.file
  	widget_control,scanSee_data.p3d_wid,set_value='Pick3D Seq # '+strtrim(indgen(SSD.nb_det(0))+1,2)
	if SSD.rank eq 3 then WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=1 $
	else WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=0

	scanSee_checkrank,scanSee_data

	end
	end
  end  
  widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
END

PRO scanSee_next,Event
  widget_control,Event.top,get_uvalue=scanSee_data,/no_copy
  if size(scanSee_data,/type) eq 8 then begin
  SSD = *scanSee_data.SSD 
  if size(SSD,/type) eq 8 then begin
  SSD = *scanSee_data.SSD 
  path = SSD.path
  ll = findfile(path+!os.file_sep+'*.mda',count=ct)
  if ct gt 1 then begin
	id = where(ll eq SSD.file)
	file= ll(id+1)
	catch,error_status
	if error_status ne 0 then begin
	  close,1	
	  r = dialog_message('File: "'+file+  '"  read failed!')
	endif else begin
	openr,1,file
	close,1
  	sscan_read,SSD,file=file,/echo,error=error,pick3d=-1
	if error eq 0 then begin
	scanSee_writeConfig,SSD
  	*scanSee_data.SSD = SSD
  	widget_control,scanSee_data.file_wid,set_value=SSD.file
  	widget_control,scanSee_data.p3d_wid,set_value='Pick3D Seq # '+strtrim(indgen(SSD.nb_det(0))+1,2)
	if SSD.rank eq 3 then WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=1 $
	else WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=0

	scanSee_checkrank,scanSee_data

	end
	end
  end  
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
	  r = dialog_message('File: "'+file+  '"  read failed!')
	endif else begin
	openr,1,file
	close,1
  	sscan_read,SSD,file=file,/echo,error=error,pick3d=-1
	if error eq 0 then begin
	 scanSee_writeConfig,SSD
  	*scanSee_data.SSD = SSD
  	widget_control,scanSee_data.file_wid,set_value=SSD.file
  	widget_control,scanSee_data.p3d_wid,set_value='Pick3D Seq # '+strtrim(indgen(SSD.nb_det(0))+1,2)
	if SSD.rank eq 3 then WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=1 $
	else WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=0

	scanSee_checkrank,scanSee_data

	end
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
	  r = dialog_message('File: "'+file+  '"  read failed!')
	endif else begin
	openr,1,file
	close,1
  	sscan_read,SSD,file=file,/echo,error=error,pick3d=-1
	if error eq 0 then begin
	 scanSee_writeConfig,SSD
  	*scanSee_data.SSD = SSD
  	widget_control,scanSee_data.file_wid,set_value=SSD.file
  	widget_control,scanSee_data.p3d_wid,set_value='Pick3D Seq # '+strtrim(indgen(SSD.nb_det(0))+1,2)
	if SSD.rank eq 3 then WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=1 $
	else WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=0

	scanSee_checkrank,scanSee_data

	end
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
;	da = da3d(*,*,*,i)
	view3d_2d,da3d(*,*,*,i),kmax=SSD.cpt(ssd.rank-1),group=Event.top,title='3D Array Seq # '+': D_'+strtrim(i+1,2),class=SSD.class
      END
  'SCANSEE_READ_DONE': BEGIN
	widget_control,event.top,/destroy
      END
  ENDCASE
END




PRO sscan_read_pick3d,da3D,detID=detID,GROUP=Group


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

  ListVal1026= '3D Array Seq # D_'+ strtrim(indgen(detID),2) 

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
	close,SSD.lun,/all

	for i=0,2 do begin
	ptr_free,SSD.da(i)
	ptr_free,SSD.pa(i)
	end
	for i=0,1 do begin
	ptr_free,SSD.sub_scan_ptr(i)
	end
	ptr_free,SSD.EH
	if ptr_valid(SSD.ts2) then ptr_free,SSD.ts2
  end
end
SSD = 0	
heap_gc
; print,ptr_valid()
END

PRO sscan_readHeader,file=file,SSD,echo=echo,error=error,pick3d=pick3d
;  this routine closes the old lun and frees the pointer
;  then open the file set create the data pointer and
;  leave the file lun open until next call of this routine
;  

if n_elements(pick3d) eq 0 then pick3d=-1
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
dflt = [0,0,0,0,0]
mdim = 5

HD_P = make_array(4,mdim,value=pos_info)
HD_D = make_array(85,mdim,value=det_info)
HD_T = make_array(4,mdim,value=trg_info)

  DetMax = dflt 
  id_def = intarr(ntot,mdim)
  cd,current=p
  SSD = { file  : '', $
	class	: '', $
	path : p, $
	lun : -1, $
	vers	: 0, $   ; 0 - 85 detecors, 1 - 70 detectors
	mdim : mdim, $
	rank  : 0, $
	scanno : 0, $
	npts	: dflt, $
	cpt	: dflt, $
	nb_pos  : dflt, $
	nb_det  : dflt, $ 
	nb_trg  : dflt, $
	id_def : id_def, $
	HD_P	: HD_P, $
	HD_D	: HD_D, $
	HD_T	: HD_T, $
	lrecl	: 0L, $ 	1D scan lrecl
	offset	: 0L, $ 	1D scan offset for detector data
	dlen	: 0L, $ 	1D scan detector data length
	pick3d  : 0, $		1 - pick 3D array, 0 - whole 3D array
	pick4d  : 0, $		1 - pick 4D array, 0 - whole 4D array
	labels  : strarr(3*ntot,mdim), $
	detMax  : dflt, $
	pv	: ['','','','',''], $
	ts1	: '', $
	im_filled : dflt, $
	noenv  : 0, $
	envPtr : 0L, $
	ts2	: ptr_new(/allocate_heap), $
	cal_array :  ptr_new(/allocate_heap), $
	EH	: ptr_new(/allocate_heap), $
	da	: ptrarr(mdim,/allocate_heap), $
	pa	: ptrarr(mdim,/allocate_heap), $
;	da0	: ptrarr(mdim,/allocate_heap), $
	sub_scan_ptr : ptrarr(mdim-1,/aLLOCATE_HEAP) $	
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
    if keyword_set(echo) then print,'sub_scan_ptr:',*SSD.sub_scan_ptr[ir-1]
  ENDIF


if keyword_set(echo) then print,'rank,npts,cpt:',rank,npts,cpt
if keyword_set(echo) then print,'ssd.npts=',ssd.npts

  ; read the pvname
  name=''
  time=''
  readu,lun,name
if keyword_set(echo) then print,'pvname=',name

if strtrim(name,2) eq '' then begin
	error = -1
	print,'Bad file...',file
	return
end
  readu,lun,time
if strlen(time) eq 0 or  strlen(time) gt 31 then begin
	error = -1
	print,'Bad file...',file
	return	
end
if keyword_set(echo) then print,'time=',time

  SSD.pv(rank-1) = name
  SSD.ts1 = time

if rank gt 1 and rank eq tmp.rank then begin
ts2 = make_array(SSD.npts(rank-1)+1,value='                               ',/string)
ts2(SSD.npts(rank-1)) = SSD.ts1
*SSD.ts2 = ts2
end

  nb_pos=0
  nb_det=0
  nb_trg=0
  readu,lun,nb_pos,nb_det,nb_trg
  SSD.nb_pos(rank-1) = nb_pos 
  SSD.nb_det(rank-1) = nb_det 
  SSD.nb_trg(rank-1) = nb_trg 
  if keyword_set(echo) then print,nb_pos,nb_det,nb_trg


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

  if tmp.rank eq 4 then begin
    if ir eq 1 then begin
	if nb_det gt 0 then $
	  *SSD.da(3) = make_array(dim(3),nb_det,/float) else $
	  *SSD.da(3) = make_array(dim(3),1,/float) 
    end
    if ir eq 2 then begin
	if nb_det gt 0 then $
	  *SSD.da(2) = make_array(dim(2),dim(3),nb_det,/float) else $
	  *SSD.da(2) = make_array(dim(2),dim(3),1,/float) 
    end
    if ir eq 3 then begin
	if SSD.pick3d eq 0 and nb_det gt 0 then $
	  *SSD.da(1) = make_array(dim(1),dim(2),dim(3),nb_det,/float) else $
	  *SSD.da(1) = make_array(dim(1),dim(2),dim(3),1,/float) 
    end
    if ir eq 4 then begin
	nar = 1L*dim(0)*dim(1)*dim(2)*dim(3)
	if dim(0) ge 1000 or nar gt 2601000 then SSD.pick4d = 1
	if SSD.pick4d eq 0 and nb_det gt 0 then $
	  *SSD.da(0) = make_array(dim(0),dim(1),dim(2),dim(3),nb_det,/float) else $
	  *SSD.da(0) = make_array(dim(0),dim(1),dim(2),dim(3),1,/float) 
    end
  end

  if tmp.rank eq 3 then begin
    if ir eq 1 then begin
	if nb_det gt 0 then $
	  *SSD.da(2) = make_array(dim(2),nb_det,/float) else $
	  *SSD.da(2) = make_array(dim(2),1,/float) 
    end
    if ir eq 2 then begin
	if nb_det gt 0 then $
	  *SSD.da(1) = make_array(dim(1),dim(2),nb_det,/float) else $
	  *SSD.da(1) = make_array(dim(1),dim(2),1,/float) 
    end
    if ir eq 3 and pick3d ge 0 then begin
	nar = 1L*dim(0)*dim(1)*dim(2)
	if dim(0) ge 1000 or nar gt 2601000 then SSD.pick3d = 1
	if SSD.pick3d eq 0 and nb_det gt 0 then $
	  *SSD.da(0) = make_array(dim(0),dim(1),dim(2),nb_det,/float) else $
	  *SSD.da(0) = make_array(dim(0),dim(1),dim(2),1,/float) 
    end
  end

  if tmp.rank eq 2 then begin
     if ir eq 1 then begin
	if nb_det gt 0 then $
	   *SSD.da(1) = make_array(dim(1),nb_det,/float) 
     end
     if ir eq 2 then begin
	if nb_det gt 0 then $
	   *SSD.da(0) = make_array(dim(0),dim(1),nb_det,/float)
     end
  end


; read 1st data set

	if nb_pos gt 0 then begin
		pa = make_array(npts,nb_pos,/double)
		readu,lun,pa
	endif else begin
		pa = dblarr(npts)
	end
	*SSD.pa(rank-1) = pa
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
	help,*SSD.pa(0),*SSD.pa(1),*SSD.pa(2),*SSD.pa(3),*SSD.pa(4)
	help,*SSD.da(0),*SSD.da(1),*SSD.da(2),*SSD.da(3),*SSD.da(4)
  end
END

PRO sscan_readENV,SSD,echo=echo
; read the env variable saved with the scan data
; if cal specified returns cal_array
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
	vl = string(v1)
	end
		EH(i).type = type
		EH(i).count = count
		EH(i).name = name
		EH(i).desc = desc
		EH(i).unit = unit
		EH(i).vl = vl
	end
	end

	*SSD.EH = EH
if keyword_set(echo) then begin
print,SSD.file
print,EH
end

; get calibration array

found = make_array(ssd.nb_det(0),/int)
id = 0
for i=0,ssd.noenv-1 do begin
	if strpos(EH(i).desc,'CAL_OFFSET') ge 0 then begin
	found(id) = i
	id = id + 1
	i = i+2
	end	
end
if id eq 0 then return
cal_d = { cal_offset:0., cal_slope:0., cal_quad: 0.}
cal_array = make_array(id,value=cal_d)
for j=0,id-1 do begin
	if found(j) gt 0 then begin
	i = found(j)
	cal_array(j).cal_offset = EH(i).vl
	cal_array(j).cal_slope = EH(i+1).vl
	cal_array(j).cal_quad = EH(i+2).vl
	print,j,i,EH(i).vl,EH(i+1).vl,EH(i+2).vl
	end
end
*ssd.cal_array = cal_array
END

PRO scanSee_fillVector,SSD,im_array,echo=echo
; map to real defined detector vector array

  if SSD.nb_det(SSD.rank-1) eq 0 then return
  if SSD.im_filled(SSD.rank-1)  then return

  npts=SSD.npts
  detMax = SSD.detMax(SSD.rank-1)
  if detMax eq 0 then return
  im_array = make_array(npts(SSD.rank-1),detMax)
  sz = size(*SSD.da(SSD.rank-1))

if detMax gt sz(2) then begin
	id_def = SSD.id_def(4:88,SSD.rank-1)
	jj=0
	for ij=0,detMax-1 do begin
		if id_def(ij) then begin
		im_array(*,ij) = (*SSD.da(SSD.rank-1))[*,jj]
		jj = jj+1
		end
	end

	*SSD.da(SSD.rank-1) = im_array
	SSD.im_filled(SSD.rank-1) = 1

	if keyword_set(echo) then begin
	title = strtrim(SSD.rank,2)+"D SCAN #"+strtrim(SSD.scanno,2)
	x = *SSD.pa(SSD.rank-1)
	plot1d,x,im_array,title=title,/data
	end
end
END


PRO scanSee_fillImage,SSD,im_array,echo=echo
; map to real defined detector image array

  if SSD.rank lt 2 then return
  if SSD.im_filled(SSD.rank-2) then return

  npts=SSD.npts
  detMax = SSD.detMax(SSD.rank-2)
  if detMax eq 0 then return
  im_array = make_array(npts(SSD.rank-2),npts(SSD.rank-1),detMax) 
		;,value=!values.F_NAN)
  sz = size(*SSD.da(SSD.rank-2))

if detMax gt sz(3) then begin
	id_def = SSD.id_def(4:88,SSD.rank-2)
	jj=0
	for ij=0,detMax-1 do begin
		if id_def(ij) then begin
		im_array(*,*,ij) = (*SSD.da(SSD.rank-2))[*,*,jj]
		jj = jj+1
		end
	end

	*SSD.da(SSD.rank-2) = im_array
	SSD.im_filled(SSD.rank-2) = 1
	if keyword_set(echo) then begin
	title = strtrim(SSD.rank,2)+"D SCAN #"+strtrim(SSD.scanno,2)
	panimage,im_array,id_def,numd=10,title=title
	end
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


PRO sscan_read3DPick,SSD,da2D,idet=idet
; read 3D scan
   if SSD.rank lt 3 then return
	ptr3D = *SSD.sub_scan_ptr(0)
	if n_elements(ptr3D) eq 0 then return
	if keyword_set(idet) eq 0 then idet=0
	if idet ge SSD.nb_det(0) then return
widget_control,/hourglass
	da2D = *SSD.da(1)
	*SSD.da[0] = make_array(SSD.npts(0),SSD.npts(1),SSD.npts(2),1)
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
if j eq 0 then print,'Reading 3D ...',k,j,tp1,tp
		if n_elements(d2) gt 1 then $
		(*SSD.da[0])[0,j,k,0] = d
		if j eq 0 then pa3D = p2
	  end
	end

	end

	if SSD.im_filled(1) eq 0 then begin
		*SSD.da[1] = da2D
		scanSee_fillImage,SSD,da2D  ;,/echo
	end
END

PRO sscan_read3D,SSD,da2D,only2d=only2d
; read 3D scan
   if SSD.rank lt 3 then return

	ptr3D = *SSD.sub_scan_ptr(0)
	if n_elements(ptr3D) eq 0 then return
;t1=systime(1)
	da2D = *SSD.da(1)
	cpt = SSD.cpt
	for k=0,cpt(2)-1 do begin
	  pos = ptr3D(k)
	  if pos gt 0 then begin
	  sscan_read1D,SSD,k,level=0,pa=p1,da=d1
 	  point_lun,-SSD.lun,tp1
	  if n_elements(d1) gt 1 then da2D(*,k,*) = d1(*,*)
	  if k eq 0 then pa2D = p1
	  
	  if keyword_set(only2d) eq 0 or only2d eq 0 then begin
	    for j=0,cpt(1)-1 do begin
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
		(*SSD.da(0))[*,j,k,*] = d2(*,*)
		if j eq 0 then pa3D = p2
	  end
	end
	end
	end

	*SSD.da(1) = da2D

;print,'time used=',systime(1)-t1
END

PRO sscan_read1D,SSD,seqno,level=level,echo=echo,pa=pa,da=da 
; extract 1D data from the multi 1D scan 
; level=1   default from scanH 
; level=0   from scan1 
;
if n_elements(seqno) eq 0 then seqno=0

  file = SSD.file
  if n_elements(level) eq 0 then level=1
  fptr = *SSD.sub_scan_ptr(level)	
  pos_info = SSD.HD_P(0,0)
  det_info = SSD.HD_D(0,0)
  trg_info = SSD.HD_T(0,0)

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
  if SSD.rank lt 4 then (*SSD.ts2)[seqno] = time
  if keyword_set(echo) then print,name,',',time

  nb_pos=0
  nb_det=0
  nb_trg=0
  readu,lun,nb_pos,nb_det,nb_trg
  if keyword_set(echo) then print,nb_pos,nb_det,nb_trg


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

PRO sscan_read,SSD,file=file,path=path,echo=echo,pick3d=pick3d,header=header,data=data,error=error,zslice=zslice
;+
; NAME:
;	SSCAN_READ
;
; PURPOSE:
;	This routine returns the SSD data structure of an input MDA file.  
;	If no file specified on the command line a MDA file selection dialog 
;	pops up.
;
; CATEGORY:
;	Widget.
;	
; CALLING SEQUENCE:
;	SSCAN_READ, SSD [,FILE=file, ...]
;
; INPUTS:
; 	None.
;
; KEYWORD PARAMETERS:
;	FILE:	specify the known MDA input file
;		If file is given no file selection dialog pop up 
;	PATH:	specify the MDA directory for the file selection dialog
;		If not specified, current working directory assumed
;	ECHO:   if specified the appropriate plot window pops up
;	PICK3D: specify the 3D data seq # , default is 0
; 	HEADER: if specified only the complete set of scan headers are read
;	DATA:	extract data array from the file
;	ERROR:  read error indicator, 0 - success, -1 - fail
;	ZSLICE: specify the slice number from the data array, default 1
;
; RESTRICTION:
;	The MDA file must be XDR file created automatically by the sscan
;	record.
;
; OUTPUTS:
;	SSD:	returns the data structure extracted from a MDA file
;
; MODIFICATION HISTORY:
;	Written by: 	Ben-chin K. Cha, July 27,2004
;-
; mda file reader with dialog_pickfile
; if echo=0 read only
;    echo=1 plot window pops up

	error=-1

	if size(SSD,/type) eq 8 then p = SSD.path
	if keyword_set(path) then p = path

	if keyword_set(file) eq 0 then begin
	file = dialog_pickfile(get_path=path,filter='*.mda',/must_exist, $
		path=p,/read,title='Pick MDA file')
	if file(0) eq '' then begin
		error=-1
		return
	end
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
	sscan_readHeader,SSD,file=file,error=error,pick3d=pick3d
	if  error ne 0  then return
	if keyword_set(header) then return

dataonly:
	if SSD.rank eq 2 then sscan_read2D,SSD,da2D
	if SSD.rank eq 3 then begin
	if n_elements(pick3d) eq 0 or pick3d lt 0 then SSD.pick3d=0
	if SSD.pick3d then sscan_read3DPick,SSD,da2D,idet=pick3d else $
	sscan_read3D,SSD,da2D,/only2d
	end

; need implement sscan_read4D
	if SSD.rank eq 4 then begin
;	sscan_read4DPick,SSD,da4D    ; read whole 4D data array
	sscan_read4DPick,SSD,da4D,idet=0    ; read D01 data array
	end

  scanSee_fillImage,SSD,rim_array
  scanSee_fillVector,SSD,rim_array

	if keyword_set(echo) eq 0 then return

	if SSD.rank eq 1 then begin
		if keyword_set(echo) then scanSee_plot1d,SSD,1
	end


	if SSD.rank eq 2 then begin
		id_def = SSD.id_def(4:4+SSD.detMax(0)-1,0)
		panimage,*SSD.da(0),id_def,numd=10, $
			title='SSCAN: 2D scan #'+strtrim(SSD.scanno,2)
	end

if n_elements(zslice) eq 0 then zslice=1

	if SSD.rank eq 3 then begin 
		title='SSCAN: 3D Scan #'+strtrim(SSD.scanno,2)
		if SSD.nb_det(1) gt 0 then begin
			id_def = SSD.id_def(4:4+SSD.detMax(1)-1,1)
			 panimage,*SSD.da(1),id_def,numd=10,title=title 
		endif else begin
		; only 3D data detected, extract second slice from da3D
		if n_elements(*SSD.da(0)) gt 2 then begin
		sz = size(*SSD.da(0))
		if sz(0) eq 4 then begin
		da = make_array(sz(1),sz(2),SSD.detMax(0))
		id_def = SSD.id_def(4:4+SSD.detMax(0)-1,0)
		ip=0
		for i=0,SSD.detMax(0)-1 do begin
		  if id_def(i) gt 0 then begin
		  if zslice ge SSD.cpt(ssd.rank-1) then zslice = ssd.cpt(ssd.rank-1)-1
		  da(*,*,i) = (*SSD.da(0))(*,*,zslice,ip)       ; 2nd scan
		  ip=ip+1
		  end
		end
		title='SSCAN: 3D Scan #'+strtrim(SSD.scanno,2)+'  Zslicer:'+strtrim(zslice,2)
		panimage,da,id_def,numd=10,title=title
		end
		end
		end
	end

	if SSD.rank eq 4 then begin
		if SSD.nb_det(2) gt 0 then panimage,*SSD.da(2)
		if SSD.nb_det(1) gt 0 then begin
		id_def = ssd.id_def(4:4+ssd.nb_det(1)-1,1)
		da3D = *SSD.da(1)
		cpt = ssd.cpt
		if zslice ge cpt(ssd.rank-1) then zslice = cpt(ssd.rank-1)-1
		da = da3D(*,*,zslice,*)	
		da = reform(da,cpt(1),cpt(2),ssd.nb_det(1))
		panimage,da,id_def,title='4D Scan #'+strtrim(SSD.scanno,2)+',  Zslice:'+strtrim(zslice,2)
help,da4D
		end
	end
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

	for i=0,(SSD.DETMAX)[j]-1 do begin
		printf,1,i,'  ',SSD.HD_D(i,j)
	end
	printf,1,''
	printf,1,'NB_TRG=',(SSD.NB_TRG)[j]
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
  	WIDGET_CONTROL,Event.top,/HOURGLASS
	sscan_read,SSD,/echo,error=error,pick3d=-1
	if error eq 0 then begin
	 scanSee_writeConfig,SSD
	*scanSee_data.SSD = SSD
print,SSD.path
  	WIDGET_CONTROL, scanSee_data.file_wid, set_value=SSD.file 
  	WIDGET_CONTROL, scanSee_data.btns_wid, SENSITIVE=1
  	widget_control,scanSee_data.p3d_wid,set_value='Pick3D Seq # '+strtrim(indgen(SSD.nb_det(0))+1,2)
	if SSD.rank eq 3 then WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=1 $
	else WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=0

	scanSee_checkrank,scanSee_data
	
	end
        widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
	return
    END
  'File.Exit': BEGIN
	scanSee_writeConfig,SSD
	widget_control,Event.top,/destroy,bad=bad
	return
    END

  'Setup.Color...': BEGIN
  	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
	xloadct,group=Event.top
    END
  'ViewData.1D Array...': BEGIN
	xaxis = scanSee_data.pick1d
  	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
	if ssd.nb_det(ssd.rank-1) eq 0 then return
	scanSee_plot1d,SSD,xaxis
    END
  'ViewData.2D Array...': BEGIN
	axis1 = scanSee_data.pick1d
	axis2 = scanSee_data.pick2d
	vers = scanSee_data.vers
  	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
	if ssd.rank lt 2 then return
	if SSD.detMax(SSD.rank-2) eq 0 then return
	if ssd.nb_det(SSD.rank-2) eq 0 then return
	SSD.vers = vers
	scanSee_image2d,SSD,axis1,axis2,vers=vers
    END
  'ViewData.4D Array...': BEGIN
  	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
	if ssd.rank lt 4 then return
	if ssd.nb_det(ssd.rank-4) eq 0 then return

	sscan_dlistpick4d,SSD=SSD,group=Event.top

    END
  'ViewData.3D Array...': BEGIN
	if SSD.rank eq 4 then begin
  	widget_control,scanSee_data.p3d_wid,set_value='Pick3D Seq # '+strtrim(indgen(SSD.nb_det(0))+1,2)
	if SSD.rank eq 3 then WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=1 $
	else WIDGET_CONTROL,scanSee_data.p3d_wid,SENSITIVE=0
	end
  	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
	if ssd.rank lt 3 then return
	if ssd.rank eq 3 then begin
	r = dialog_message(title='3D Array...', $
		['Please use "Pick3D Seq # . " droplist', $
		' to select the desired 3D data array.'],/info)
	end
	if ssd.rank eq 4 then begin
	ip=1 
	if n_elements(*SSD.da(ip)) eq 0 then return
	detID = SSD.nb_det(ssd.rank-3)
	if detID le 0 then return
	sscan_read_pick3d,*SSD.da(ip),detID=detID,group=Event.top 
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
  'Report.View Report...': BEGIN
  	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
	cd,current=p
	p = p + !os.file_sep + 'ASCII'
        f = pickfile(title='Pick Report File',/read,path=p,filter='*')
        if f ne '' then begin
                xdisplayfile,f
        end
    END
  'Report.From 1D Array...': BEGIN
  	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
	if SSD.nb_det(SSD.rank-1) gt 0 then SB2RPT_1D,SSD
    END
  'Report.From 2D Array...': BEGIN
  	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
	if SSD.rank gt 1 and SSD.nb_det(SSD.rank-2) gt 0 then $
	SB2RPT,SSD,group=Event.top
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
     st = ['When there is a problem in getting a new scan by buttons at the startup time', $
	'try to press the CR key to re-initialate the data structure', $
	'if there is still problem try to use File->Open.']
	r = dialog_message(st,/info)
    END
  ENDCASE
END



PRO SSCAN_MAIN13_Event, Event


  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  ; Event for PDMENU2
  'PDMENU2': PDMENU2_Event, Event

  'FIELD3': BEGIN
  	WIDGET_CONTROL,Event.top,/HOURGLASS
	scanSee_field,Event
      END
  'SSCAN_PICKAX': BEGIN
	scanSee_pickXaxis,Event
      END
  'SSCAN_PICKAY': BEGIN
	scanSee_pickYaxis,Event
      END
  'SSCAN_PICKVS': BEGIN
 	scanSee_version,Event	
      END
  'SSCAN_PICK4D': BEGIN
  	WIDGET_CONTROL,Event.top,/HOURGLASS
	scanSee_pick4d_det,Event
      END
  'SSCAN_PICK3D': BEGIN
  	WIDGET_CONTROL,Event.top,/HOURGLASS
	scanSee_pick3d_det,Event
      END
  'SSCAN_FIRST': BEGIN
  	WIDGET_CONTROL,Event.top,/HOURGLASS
	scanSee_first,Event
      END
  'SSCAN_NEXT': BEGIN
  	WIDGET_CONTROL,Event.top,/HOURGLASS
	scanSee_next,Event
      END
  'SSCAN_PREV': BEGIN
  	WIDGET_CONTROL,Event.top,/HOURGLASS
	scanSee_prev,Event
      END
  'SSCAN_LAST': BEGIN
  	WIDGET_CONTROL,Event.top,/HOURGLASS
	scanSee_last,Event
      END
  'SSCAN_DONE': BEGIN
  	widget_control,Event.top,get_uvalue=scanSee_data,/no_copy
	SSD = *scanSee_data.SSD
  	widget_control,Event.top,set_uvalue=scanSee_data,/no_copy
	scanSee_writeConfig,SSD
	scanSee_free,SSD
	widget_control,Event.top,/destroy,bad_id=bad
      END

  ENDCASE

END



PRO sscan,file=file,GROUP=Group
;+
; NAME:
;	SSCAN
;
; PURPOSE:
;	This program allows the user to view any 1D/2D/3D scan MDA file
;	generated by the sscan record. It let the user easily to access any 
;	1D/2D/3D data and display the data graphically. It provide various
;	graphic output features and provide simple 1D/2D analysis tools.
;
; CATEGORY:
;	Widgets.
;
; CALLING SEQUENCE:
;	SSCAN [,FILE=file, GROUP=group]
;
; INPUTS:
;	None.
;
; KEYWORD PARAMETERS:
;	FILE:	specify the input MDA file name
;	GROUP:  specify the parent widget ID
;	
; OUTPUTS:
;	None.
;
; SIDE EFFECTS:
;	The appropriate subprograms image2d, view3d_2d, scan2d_roi,...
;	will be popped up for extracted data arrays.
;
; MODIFICATION HISTORY:
;        Written by:     Ben-chin K. Cha, July 27, 2004.
;	03-30-05 bkc	Add support for 85/70 detectors, default 85 Dis
;-

if XRegistered('SSCAN_MAIN13') then return

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  SSCAN_MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
	title='sscan Reader (R1.0)', $
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
        { CW_PDMENU_S,       0, '3D Array...' }, $ ;        6
        { CW_PDMENU_S,       2, '4D Array...' }, $ ;        6
      { CW_PDMENU_S,       1, 'Scan Info' }, $ ;        7
        { CW_PDMENU_S,       0, 'Axes Info...' }, $ ;       10
        { CW_PDMENU_S,       0, 'Env Vars...' }, $ ;       11
        { CW_PDMENU_S,       2, 'SSD,/struct...' }, $ ;       12
      { CW_PDMENU_S,       1, 'Report' }, $ ;        7
        { CW_PDMENU_S,       0, 'From 2D Array...' }, $ ;       12
        { CW_PDMENU_S,       0, 'From 1D Array...' }, $ ;       12
        { CW_PDMENU_S,       2, 'View Report...' }, $ ;       12
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
      VALUE='  ')

  pick4d = widget_droplist(BASE4,Value=' 4D Seq # '+ strtrim(indgen(10)+1,2), $
	/frame, UVALUE='SSCAN_PICK4D',Title='')
  widget_control,pick4d,sensitive=0

  pick3d = widget_droplist(BASE4,Value=' 3D Seq # '+ strtrim(indgen(10)+1,2), $
	/frame, UVALUE='SSCAN_PICK3D',Title='')
  widget_control,pick3d,sensitive=0

  pickAx = widget_droplist(BASE4,Value=['P1','P2','P3','P4','Step #'], $
	/frame, UVALUE='SSCAN_PICKAX',Title='Xaxis:')
  
  pickAy = widget_droplist(BASE4,Value=['P1','P2','P3','P4','Step #'], $
	/frame, UVALUE='SSCAN_PICKAY',Title='Yaxis:')
  
  pickvs = widget_droplist(BASE4,Value=['85','70'], $
	/frame, UVALUE='SSCAN_PICKVS',Title='DIs')

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
	vers_wid : pickvs, $
	p4d_wid : pick4d, $
	p3d_wid : pick3d, $
	type_wid : LABEL6, $
	SSD : ptr_new(/allocate_heap), $
	vers : 0, $
	pick1d : 0, $
	pick2d : 0, $
	pick3d : 0, $
	pick4d : 0 $
	}

  if keyword_set(file) then begin
  	WIDGET_CONTROL,SSCAN_MAIN13,/HOURGLASS
	sscan_read,SSD,file=file,/echo,error=error,pick3d=-1
	if error eq 0 then begin
		*scanSee_data.SSD = SSD
		scanSee_writeConfig,SSD
		scanSee_checkrank,scanSee_data
	endif else begin
		st = [ 'Invalid file in  scanSee.config file:', $
		'You have to either enter a valid file in the text field first,', $
		'or','You have to remove the configuration file first,', $
		'then restart the sscan again']
		r = dialog_message(st,/error)
	end
  end
  widget_control,SSCAN_MAIN13,set_uvalue=scanSee_data,/no_copy

  XMANAGER, 'SSCAN_MAIN13', SSCAN_MAIN13,/no_block
END
