FORWARD_FUNCTION READ_SCAN,READ_SCAN_FIRST,READ_SCAN_REST


PRO scanimage_print,gD,test=test
	gData = *gD
	print,'scanno  : ',*gData.scanno
	print,'dim     : ',*gData.dim
	print,'num_pts : ',*gData.num_pts
	print,'cpt     : ',*gData.cpt
	print,'id_def  : ',*gData.id_def
	print,'pvname  : ',*gData.pv
	print,'labels  : ',*gData.labels
	help,*gData.pa1D
	help,*gData.da1D
	if *gData.dim eq 2 then begin
	help,*gData.pa2D
	help,*gData.da2D
	end
	
	if keyword_set(test) then begin
	num_pts = *gData.num_pts
	width = num_pts(0)
	height = num_pts(1)
	help,width,height
	da2D = *gData.da2D
	im = da2d(*,*,1)
	help,im
	tvscl, congrid(im,400,400),/NAN  ; IDL 5.1
	end

END

PRO scanimage_free,gD
	gData = *gD
	if ptr_valid(gData.scanno) then	ptr_free,gData.scanno
	if ptr_valid(gData.dim) then	ptr_free,gData.dim
	if ptr_valid(gData.num_pts) then	ptr_free,gData.num_pts
	if ptr_valid(gData.cpt) then	ptr_free,gData.cpt
	if ptr_valid(gData.id_def) then	ptr_free,gData.id_def
	if ptr_valid(gData.pv) then	ptr_free,gData.pv
	if ptr_valid(gData.labels) then	ptr_free,gData.labels
	if ptr_valid(gData.pa1D) then	ptr_free,gData.pa1D
	if ptr_valid(gData.da1D) then	ptr_free,gData.da1D
	if ptr_valid(gData.pa2D) then	ptr_free,gData.pa2D
	if ptr_valid(gData.da2D) then	ptr_free,gData.da2D
	if ptr_valid(gD) then ptr_free,gD
END

PRO scanimage_cleanup
	help,/heap_variables
	heap_gc
END

PRO scanimage_alloc,filename,gD,scanno

gData = { $
	scanno	: ptr_new(/allocate_heap), $  ;0L, $
	dim	: ptr_new(/allocate_heap), $  ;0, $
	num_pts	: ptr_new(/allocate_heap), $  ;[0,0], $
	cpt	: ptr_new(/allocate_heap), $  ;[0,0], $
	id_def	: ptr_new(/allocate_heap), $  ;intarr(19,2), $
	pv	: ptr_new(/allocate_heap), $  ;['',''], $
	labels	: ptr_new(/allocate_heap), $  ;strarr(57,2), $
	pa1D	: ptr_new(/allocate_heap), $
	da1D	: ptr_new(/allocate_heap), $
	pa2D	: ptr_new(/allocate_heap), $
	da2D	: ptr_new(/allocate_heap) $
	}
	gD = ptr_new(/allocate_heap)
	*gD = gData

;	scanno = read_scan(filename,dim,num_pts,cpt,pv,labels,id_def,pa1D,da1D,pa2D,da2D)

; help,scanno,dim,num_pts,cpt,pv,labels,id_def,pa1d,pa2d,da1d,da2d

	scanno = read_scan(filename, Scan)
	*gData.scanno = scanno

	if scanno lt 0 then return
	rix2DC, Scan, gData
;	*gData.dim = dim
;	*gData.num_pts = num_pts
;	*gData.cpt = cpt
;	*gData.pv = pv
;	if n_elements(labels) then $
;	*gData.labels = labels
;	if n_elements(id_def) then $
;	*gData.id_def = id_def
;	*gData.pa1D = pa1D
;	*gData.da1D = da1D
;	if dim eq 2 then begin
;	*gData.pa2D = pa2D
;	*gData.da2D = da2D
;	end

;	scanimage_print,gD

END


PRO rix2DC,Scan,gData
ON_ERROR,1
 
        *gData.scanno  = *Scan.scanno
        *gData.dim     = *Scan.dim
        *gData.num_pts = *Scan.npts
        *gData.cpt     = *Scan.cpt
        *gData.id_def  = *Scan.id_def
        *gData.pv      = *Scan.pv
        *gData.labels  = *Scan.labels

	if *Scan.dim eq 1 then begin
          *gData.pa1D  = *(*Scan.pa)[0]
          *gData.da1D  = *(*Scan.da)[0]
	  *gData.pa2D  = ptr_new(/ALLOCATE_HEAP)
	  *gData.da2D  = ptr_new(/ALLOCATE_HEAP)
	end
	if *Scan.dim eq 2 then begin
          *gData.pa1D    = *(*Scan.pa)[1]
          *gData.da1D    = *(*Scan.da)[1]
          *gData.pa2D  = *(*Scan.pa)[0]
          *gData.da2D  = *(*Scan.da)[0]
        end
 
  ptr_free,Scan.scanno
  ptr_free,Scan.dim
  ptr_free,Scan.npts
  ptr_free,Scan.cpt
  ptr_free,Scan.id_def
  ptr_free,Scan.pv
  ptr_free,Scan.labels
  ptr_free,Scan.pa
  ptr_free,Scan.da


END
	
FUNCTION nbElem,dim,vector
  res=1L
  for i=0,dim-1 do begin
     res= res*vector[i]
  end
  return, res
END

FUNCTION read_scan_rest,lun,Scan,dim,offset
ON_IOERROR, BAD	

  rank=0
  npts=0
  cpt=0
  readu,lun,rank,npts,cpt

  nb_pts=cpt
  if(nb_pts EQ npts) then nb_pts=nb_pts-1

  if(rank GT 1) then begin $
    sub_scan_ptr=lonarr(npts)
    readu,lun,sub_scan_ptr
  endif

  (*Scan.cpt)[rank-1]=cpt;

  ; read the pvname
  name=''
  time=''
  readu,lun,name,time
  
  nb_pos=0
  nb_det=0
  nb_trg=0
  readu,lun,nb_pos,nb_det,nb_trg

  if(nb_pos NE 0) then begin $
    pos_num=intarr(nb_pos)
  endif
  pos_info= { $
     pxpv:'', $
     pxds:'', $
     pxsm:'', $
     pxeu:'', $
     rxpv:'', $
     rxds:'', $
     rxeu:'' }
  if(nb_det NE 0) then begin
    det_num=intarr(nb_det)
  endif

  det_info= { $
     dxpv:'', $
     dxds:'', $
     dxeu:'' }
  if(nb_trg NE 0) then trg_num=intarr(nb_trg)
  trg_info= { $
     txpv:'', $
     txcd:'' }

  num=0
  for i=0,nb_pos-1 do begin
     readu,lun, num
     pos_num[i]=num
     readu,lun,pos_info
  end

  for i=0,nb_det-1 do begin
     readu,lun,num
     det_num[i]=num
     readu,lun,det_info
  end

  for i=0,nb_trg-1 do begin
     readu,lun,num
     trg_num[i]=num
     readu,lun,trg_info
  end
  
  tmp=dblarr(npts)
  for i=0,nb_pos-1 do begin
     readu,lun,tmp
     if(cpt NE 0) then $
       (*(*Scan.pa)[rank-1])[offset:offset+cpt-1,pos_num[i]]=tmp[0:cpt-1]
  end

  tmp=fltarr(npts)
  for i=0,nb_det-1 do begin
     readu,lun,tmp
     if(cpt NE 0) then $
       (*(*Scan.da)[rank-1])[offset:offset+cpt-1,det_num[i]]=tmp[0:cpt-1]
  end

  if(rank GT 1) then begin
    sub_offset=offset
    nb_sub= cpt
    if(cpt NE npts) then nb_sub=nb_sub+1
    for i=0,nb_sub do begin
      res= read_scan_rest(lun,Scan,dim+1,sub_offset)
      if(res NE 1) then goto,BAD
    end
  end

  offset=offset+(*Scan.npts)[rank+dim-2]

  return, 1

BAD:
  return, 0
end  



FUNCTION read_scan_first,lun,Scan,dim
ON_IOERROR, BAD	

  rank=0
  npts=0
  cpt=0
  readu,lun,rank,npts,cpt

  if(rank GT 1) then begin $
    sub_scan_ptr=lonarr(npts)
    readu,lun,sub_scan_ptr
  endif

  (*Scan.cpt)[rank-1]=cpt;

  ; read the pvname
  name=''
  time=''
  readu,lun,name,time
  (*Scan.pv)[rank-1]=name
  
  nb_pos=0
  nb_det=0
  nb_trg=0
  readu,lun,nb_pos,nb_det,nb_trg

  dims=(*Scan.npts)[rank-1:rank+dim-1]
  size= nbElem(dim+1, dims)
  if(nb_pos NE 0) then pos_num= intarr(nb_pos)

  (*Scan.pa)[rank-1]= ptr_new(dblarr(size,4), /NO_COPY)
;  (*(*Scan.pa)[rank-1])[*]= !VALUES.D_NAN  

  pos_info= { $
     pxpv:'', $
     pxds:'', $
     pxsm:'', $
     pxeu:'', $
     rxpv:'', $
     rxds:'', $
     rxeu:'' }


  if(nb_det NE 0) then det_num=intarr(nb_det)

  (*Scan.da)[rank-1]= ptr_new(fltarr(size,15), /NO_COPY)
;  (*(*Scan.da)[rank-1])[*]= !VALUES.F_NAN

  det_info= { $
     dxpv:'', $
     dxds:'', $
     dxeu:'' }

  if(nb_trg NE 0) then trg_num=intarr(nb_trg)
  trg_info= { $
     txpv:'', $
     txcd:'' }

  num=0
  for i=0,nb_pos-1 do begin
     readu,lun, num
     pos_num[i]=num
     readu,lun,pos_info
     (*Scan.id_def)[num,rank-1]=1
     if(pos_info.rxpv NE '') then begin
	(*Scan.labels)[num,rank-1]= pos_info.rxpv
	(*Scan.labels)[19+num,rank-1]= pos_info.rxds
	(*Scan.labels)[38+num,rank-1]= pos_info.rxeu
     endif else begin
	(*Scan.labels)[num,rank-1]= pos_info.pxpv
	(*Scan.labels)[19+num,rank-1]= pos_info.pxds
	(*Scan.labels)[38+num,rank-1]= pos_info.pxeu
     endelse
  end

  for i=0,nb_det-1 do begin
     readu,lun,num
     det_num[i]=num
     readu,lun,det_info
     (*Scan.id_def)[4+num,rank-1]=1
     (*Scan.labels)[4+num,rank-1]= det_info.dxpv
     (*Scan.labels)[23+num,rank-1]= det_info.dxds
     (*Scan.labels)[42+num,rank-1]= det_info.dxeu
  end

  for i=0,nb_trg-1 do begin
     readu,lun,num
     trg_num[i]=num
     readu,lun,trg_info
  end
  
  tmp=dblarr(npts)
  for i=0,nb_pos-1 do begin
     readu,lun,tmp
     if(cpt NE 0) then (*(*Scan.pa)[rank-1])[0:cpt-1,pos_num[i]]=tmp[0:cpt-1]
  end

  tmp=fltarr(npts)
  for i=0,nb_det-1 do begin
     readu,lun,tmp
     if(cpt NE 0) then (*(*Scan.da)[rank-1])[0:cpt-1,det_num[i]]=tmp[0:cpt-1]
  end

  if(rank GT 1) then begin
    res=0
    res= read_scan_first(lun,Scan,dim+1)
    if(res NE 1) then goto,BAD
    offset= LONG((*Scan.npts)[rank+dim-2])
    nb_sub= cpt-1
    if(cpt NE npts) then nb_sub=nb_sub+1
    for i=1,nb_sub do begin
      res= read_scan_rest(lun,Scan,dim+1,offset)
      if(res NE 1) then goto,BAD
    end
  end

  return, 1

BAD:
  return, 0
end  


FUNCTION read_scan,filename, Scan

  ON_ERROR,1
  ON_IOERROR,BAD

  res=0

  Scan = { $
	scanno	: ptr_new(/allocate_heap), $  ;0L, $
	dim	: ptr_new(/allocate_heap), $  ;0, $
	npts	: ptr_new(/allocate_heap), $  ;[0,0], $
	cpt	: ptr_new(/allocate_heap), $  ;[0,0], $
	id_def	: ptr_new(/allocate_heap), $  ;intarr(19,2), $
	pv	: ptr_new(/allocate_heap), $  ;['',''], $
	labels	: ptr_new(/allocate_heap), $  ;strarr(57,2), $
	pa	: ptr_new(/allocate_heap), $
	da	: ptr_new(/allocate_heap) $
	}

  get_lun, lun
  openr, /XDR, lun, filename      ;Open the file for input.

  tmp= {$
     version: 0.0, $
     scanno: 0L, $
     rank: 0L }

  readu,lun, tmp

  npts= intarr(tmp.rank)
  readu,lun, npts
  readu,lun, isRegular
  readu,lun, env_fptr

  *Scan.scanno=tmp.scanno
  *Scan.dim= tmp.rank
  *Scan.npts= reverse(npts)
  *Scan.cpt = intarr(tmp.rank)
  *Scan.id_def= intarr(19,tmp.rank)
  *Scan.pv= strarr(tmp.rank)
  *Scan.labels= strarr(57,tmp.rank)
  *Scan.pa= ptrarr(tmp.rank)
  *Scan.da= ptrarr(tmp.rank)

  if(read_scan_first(lun, Scan, 0) NE 1) then goto,BAD

  for i=0,tmp.rank-1 do begin
    dims=(*Scan.npts)[i:tmp.rank-1]
    *(*Scan.pa)[i]= reform(*(*Scan.pa)[i], [dims,4])
    *(*Scan.da)[i]= reform(*(*Scan.da)[i], [dims,15])
  end

  res= *Scan.scanno

  goto,DONE
BAD:
  res= -1
  print, !ERR_STRING

DONE:
  free_lun, lun

  return, res
END

;@DC_alloc.pro
;@read_scan.pro


PRO readScanFile,filename,gD,scanno
;+
; NAME:
;       READSCANFILE
;
; PURPOSE:
;	This procedure reads the scan file which was automatically created 
;       by the IOC and returns a pointer structure to point to the scan 
;       data components.  
;
; CALLING SEQUENCE:
;       READSCANFILE, Filename, Gp, Scanno
;
; INPUTS:
;	Filename:    Specifies the IOC saved scan file name to be read.
;
; KEYWORD PARAMETERS:
;       None.
;
; OUTPUTS:
;       Gp:          Parameter used to return the pointer of scan
;                    structure which consists of the scan data pointers to 
;                    scan data components 
;
;       Scanno:      Optional output, it returns the scan number of the file.
;
; RESTRICTIONS:
;       The directory /usr/local/epics/extensions/bin/solaris must be
;       in your IDL search path.
;
;       The scan data is saved in XDR format by the IOC scan save data 
;       software.
;
; EXAMPLE:
;       In the following example it read a 2D scan file 'cha:_0000.scan' from
;       the directory /home/sricat/CHA/rix directory and the pointer gD is
;       used to store the scan data structure. 
;
;       Then the image of detector 2 is selected and plotted and the 2D
;       image is returned as Im varaible (15 detectors supported). 
;
;       Then for 1D scan # 5, a 1D plot is desired, the positioner 1 is
;       selected as X axis, the detecor 1,2,3 are selected for Y array.
;
;       file = '/home/sricat/CHA/rix/cha:_0000.scan'
;       ReadScanFile, file, gD
;       scan2Ddata, gD, 2, /view, xarr=xarr, yarr=yarr, im=im
;       scan1Ddata, gD, 5, /plot, xarr=x, yarr=y, xsel=0, ysel='0,1,2'
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Sept 4, 1998.
;	xx-xx-xxxx	comment
;-

	if n_params() lt 2 then begin
		print,'Usage: ReadScanFile, filename, gD [,scanno]'
	print,''
	print,'   filename   -  speicifes the scan file created by the IOC savedata software'
	print,'   gD         -  returns the structure pointer for scan data '
	print,'   scanno     -  returns scanno '
		return

	end
	if n_elements(gD) then begin

;	scanno = read_scan(filename,dim,num_pts,cpt,pv,labels,id_def,pa1D,da1d,pa2D,da2D)

;	help,dim,num_pts,pv,labels,id_def,pa1D,da1D,pa2D,da2D

	gData = *gD
	scanno = read_scan(filename,Scan)
	*gData.scanno = scanno

	if scanno lt 0 then return
	rix2DC, Scan, gData

;scanimage_print,gD,/test
;        *gData.scanno = scanno
;        if scanno lt 0 then return
;        *gData.dim = dim
;        *gData.num_pts = num_pts
;        *gData.cpt = cpt
;        *gData.pv = pv
;       if n_elements(labels) then $
;        *gData.labels = labels
;       if n_elements(id_def) then $
;        *gData.id_def = id_def
;        *gData.pa1D = pa1D
;        *gData.da1D = da1D
;        if dim eq 2 then begin
;        *gData.pa2D = pa2D
;        *gData.da2D = da2D
;	end

	endif else begin
		scanimage_alloc,filename,gD,scanno
	end

	if scanno lt 0 then begin
		print,'Error: readScanFile failed on ',filename
		return
	end
END


PRO scan2Ddata,gD,seq,view=view,xarr=xarr,yarr=yarr,im=im,width=width,height=height,scanno=scanno,xdesc=xdesc,ydesc=ydesc,xpv=xpv,ypv=ypv,plot=plot,group=group
;+
; NAME:
;       SCAN2DDATA
;
; PURPOSE:
;	This procedure extracts various 2D scan data components and returns
;       as IDL varibles from the given scan structure pointer. 
;
; CALLING SEQUENCE:
;       SCAN2DDATA, gD, dN, /VIEW, Im=Im, Xarr=Xarr, Yarr=Yarr, ... 
;
; INPUTS:
;       gD:          Parameter to specify the pointer of scan data structure
;                    returned by the procedure READSCANFILE
;       dN:          Speicifies the desired image number, i.e. detector number.
;
; KEYWORD PARAMETERS:
;       View:        If specified, the TVSCL of the 2D image is plotted.
;       Plot:        If specified, the 2D image plot program is called.
;       Xarr:        Returns the positioner 1 vector of X scan
;       Yarr:        Returns the positioner 1 vector of Y scan
;       Im:          Returns the 2D image of the selected detector
;       Width:       Returns the X width of the 2D image
;       Height:      Returns the Y height of the 2D image
;       Xdesc:       Returns the X positioner desc string
;       Ydesc:       Returns the Y positioner desc string
;       Xpv:         Returns the X scan record pvname
;       Ypv:         Returns the Y scan record pvname
;       Scanno:      Returns the 2D scan number
;
; RESTRICTIONS:
;       Same as READSCANFILE.
;
; EXAMPLE:
;
;       In the following example it read a 2D scan file 'cha:_0000.scan' from
;       the directory /home/sricat/CHA/rix directory and the pointer gD is
;       used to store the scan data structure.
;
;       Then the image of detector 2 is selected and plotted and the 2D
;       image is returned as Im varaible (15 detectors supported).
;       Then for 1D scan # 5, a 1D plot is desired, the positioner 1 is
;       selected as X axis, the detecor 1,2,3 are selected for Y array.
;
;       file = '/home/sricat/CHA/rix/cha:_0000.scan'
;       ReadScanFile, file, gD
;       scan2Ddata, gD, 2, /view, xarr=xarr, yarr=yarr, im=im
;       scan1Ddata, gD, 5, /plot, xarr=x, yarr=y, xsel=0, ysel='0,1,2'
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Sept 4, 1998.
;	xx-xx-xxxx	comment
;-
	if n_params() eq 0 then begin
		print,'Usage: scan2Ddata,gD,seq#,view=view,xarr=xarr,yarr=yarr,'
		print,'              im=im,width=width,height=height,scanno=scanno,'
		print,'              xdesc=xdesc,ydesc=ydesc,xpv=xpv,ypv=ypv'

	print,'INPUT'
	print,' gD      - specifies the scan data pointer created by scanReadFile'
	print,' seq#    - specifies the detector seq # [1-15]'
	print,'KEYWORDS'
	print,'  VIEW   - if specified, show the 2D image'
	print,'  xarr   - returns x positioner vector'
	print,'  yarr   - returns y positioner vector'
	print,'   im    - returns 2D image of the detector'
	print,'  width  - returns im  width'
	print,'  height - returns im  height'
	print,'  ydesc  - returns y description'
	print,'  xpv    - returns x pvname'
	print,'  ypv    - returns y pvname'
	print,'  scanno - returns 2D scan #'
		return
	end

	scanno = *(*gD).scanno
	if scanno lt 0 then begin
		print,'Error: scan2Ddata,gD,seq#'
		print,!err_string+!err
		return
	end

        dim = *(*gD).dim
        num_pts = *(*gD).num_pts
        cpt = *(*gD).cpt
        pv = *(*gD).pv
        labels = *(*gD).labels
        id_def = *(*gD).id_def
        pa1D = *(*gD).pa1D
        da1D = *(*gD).da1D

	if dim eq 2 then begin
        pa2D = *(*gD).pa2D
        da2D = *(*gD).da2D

	if (seq-1) lt 0 or seq gt 14 then begin
		print,'Error: invalid image number - ' ,seq
		return
	end

	xarr = pa2d(*,0,0)
	yarr = pa1d(*,0)
	im = da2d(*,*,seq-1)
	max_pidi = n_elements(id_def)/2
	xdesc = labels(max_pidi,0)
	if xdesc eq '' then xdesc = labels(0,0)
	ydesc = labels(max_pidi,1)
	if ydesc eq '' then ydesc = labels(0,1)
	xpv = pv(0)
	ypv = pv(1)
	w = cpt(0)
	h = cpt(1)
	width = w
	height = h

        header_note1 = '2D Scan # '+string(scanno) + $
                ',    Image seqno ' + string(seq)
        header_note = 'Image( '+strtrim(w,2)+' , '+  strtrim(h,2)+') '

	if keyword_set(view) then begin
	loadct,39
	window,0,xsize=500,ysize=500,title='scan2d Object'
 
        ncolors = !d.table_size

          xdis = 0.001 * !d.x_size
          ydis = !d.y_size - 1.2 * !d.y_ch_size
          xyouts,xdis,ydis,header_note1,/device,color=ncolors-1
 
          ydis = !d.y_size - 2.2 * !d.y_ch_size
          xyouts,xdis,ydis,header_note,/device,color=ncolors-1
 
        TVSCL,congrid(im,!d.x_size -100,!d.y_size-100),50,50
 
	xrange=[xarr(0),xarr(w-1)]
	yrange=[yarr(0),yarr(h-1)]
;	xrange=[min(xarr),max(xarr)]
;	yrange=[min(yarr),max(yarr)]
	xstyle = 1
	ystyle = 1
	if yrange(0) eq yrange(1) then ystyle = 4
	if xrange(0) eq xrange(1) then xstyle = 4
        plot,xrange=xrange,yrange=yrange,[-1+yrange(0),-1+yrange(0)],/noerase, $
                pos=[50./!d.x_size, 50./!d.y_size, $
                 (!d.x_size-50.)/!d.x_size, (!d.y_size-50.)/!d.y_size], $
                xstyle=xstyle, ystyle=ystyle, xtitle=xdesc, ytitle=ydesc, $
                title=xpv + 'D'+ strtrim(seq,2)
 
	end

	if keyword_set(plot) then $
        plot2d,im, xarr=xarr, yarr=yarr, $
                comment=[header_note1,header_note], $
                xtitle=xdesc, ytitle=ydesc, $
                title=xpv + 'D'+ strtrim(seq,2), group=group

	endif else begin
		res = dialog_message('Error: not a 2D scan!',/Error)
	end
END


PRO parse_num0,instring,ids,sep=sep
Keysepar = '-'
if keyword_set(sep) then Keysepar = sep
res = strpos(instring,keysepar)
if res ne -1 then begin
        str = str_sep(instring,keysepar,/trim)
        no = fix(str(1)) - fix(str(0)) + 1
        ids = indgen(no) + fix(str(0))
endif else begin
        com = strpos(instring,',')
        if com ne -1 then begin
                str = str_sep(instring,',')
                ids = fix(str)
        endif else begin
                str = str_sep(instring,' ')
                ids = fix(str)
        end
 
end
END
 
; parse by sep1 first then by sep2
; default sep1=',' sep2='-'
;       instring = '1,2:5,7'
;       instring = '1,2-5,7'
PRO parse_num,instring,res,sep1=sep1,sep2=sep2
        d_sep1 = ','
        d_sep2 = '-'
        if keyword_set(sep1) then d_sep1 = sep1
        if keyword_set(sep2) then d_sep2 = sep2
        str = str_sep(instring,d_sep1,/trim)
        res = fix(str(0))
        for i=0,n_elements(str)-1 do begin
        newstr =  strtrim(str(i),2)
        if strlen(newstr) gt 0 then begin
        parse_num0,newstr,ids,sep=d_sep2
        if i eq 0 then begin
                if n_elements(ids) gt 1 then res = ids
                end
        if i gt 0 then  res = [res,ids]
        end
        end
END

PRO scan1Ddata,gD,seq,plot=plot,pa=pa,da=da,npts=npts,$
xsel=xsel,ysel=ysel,xarr=xarr,yarr=yarr, $
xdesc=xdesc,ydesc=ydesc,xengu=xengu,yengu=yengu, $
id_def=id_def,scanno_2d=scanno_2d,title=title,group=group
;+
; NAME:
;       SCAN1DDATA
;
; PURPOSE:
;	This procedure extracts scan data and returns as IDL varibles 
;       from the given scan structure pointer. It is able to extracts
;       data from either 1D or 2D scan data. 
;
; CALLING SEQUENCE:
;       SCAN1DDATA, gD, sN, /PLOT, Pa=Pa, Da=Da, Xarr=Xarr, Yarr=Yarr, ... 
;
; INPUTS:
;       gD:          Parameter to specify the pointer of scan data structure
;                    returned by the procedure READSCANFILE
;       sN:          Speicifies the desired 1D scan number.
;
; KEYWORD PARAMETERS:
;       Plot:        If specified, the selected detector data will be plotted
;                    by PLOT1D program.
;       Xsel:        Specifies the desired positioner as X aixs, default 0
;       Ysel:        Specifies a string of desired detectors, default to all 
;                    defined detectors in the Da array
;       Xarr:        Returns the X vector of the selected  positioner
;       Yarr:        Returns the Y array of the selected detectors
;       Xdesc:       Returns the X positioner desc string
;       Ydesc:       Returns the Y positioner desc string
;       Xengu:       Returns the X positioner engu string
;       Yengu:       Returns the Y positioner engu string
;       Npts:        Returns the data points in the X vector
;       Title:       Returns the inner scan record pvname
;       Pa:          Returns the original positioner array of inner scan
;       Da:          Returns the original detecotr array of inner scan
;       id_def:      Returns the vector of monitored positioners and detectors
;                    of inner scan record 
;       Scanno_2d:   Returns the 2D scan number 
;
; RESTRICTIONS:
;       Same as READSCANFILE.
;
; EXAMPLE:
;       In the following example it read a 2D scan file 'cha:_0000.scan' from
;       the directory /home/sricat/CHA/rix directory and the pointer gD is
;       used to store the scan data structure.
;
;       Then the image of detector 2 is selected and plotted and the 2D
;       image is returned as Im varaible (15 detectors supported).
;
;       Then for 1D scan # 5, a 1D plot is desired, the positioner 1 is
;       selected as X axis, the detecor 1,2,3 are selected for Y array.
;
;       file = '/home/sricat/CHA/rix/cha:_0000.scan'
;       ReadScanFile, file, gD
;       scan2Ddata, gD, 2, /view, xarr=xarr, yarr=yarr, im=im
;       scan1Ddata, gD, 5, /plot, xarr=x, yarr=y, xsel=0, ysel='0,1,2'
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Sept 4, 1998.
;	xx-xx-xxxx	comment
;-
	if n_params() eq 0 then begin
		print,'Usage: scan1Ddata,gD,seq#,plot=plot,xarr=x,yarr=y,'
		print,'              npts=npts,xdesc=xdesc,ydesc=ydesc,...'
		print,'              pa=pa,da=da,id_def=id_def'

	print,'INPUT'
	print,' gD      - specifies the scan data pointer created by scanReadFile'
	print,' seq#    - specifies the scan seq # within 2D scan'
	print,'KEYWORDS'
	print,' plot    - 1D plot of yarr if set to 1'
	print,' xarr    - returns the X axis vector'
	print,' yarr    - returns the Y array'
	print,' xsel    - specifies positioner # as x-axis (default 0) '
	print,' ysel    - a string to specifies a list of detectors, default all monitored detectors'
	print,' xdesc   - returns x labels for xarr'
	print,' ydesc   - returns y labels for yarr'
	print,' npts    - returns number of scan data points'
	print,' pa      - returns original scan positioner array'
	print,' da      - returns original scan detector array'
	print,' id_def  - returns indicators for monitored positoner and detector '
		return
	end

	scanno = *(*gD).scanno
	if scanno lt 0 then begin
		print,'Error: scan1Ddata,gD,seq#'
		print,!err_string+ string(!err)
		return
	end

        dim = *(*gD).dim
        num_pts = *(*gD).num_pts
        cpt = *(*gD).cpt
        pv = *(*gD).pv
        labels = *(*gD).labels
        id_def = *(*gD).id_def
        pa1D = *(*gD).pa1D
        da1D = *(*gD).da1D

IF dim EQ 2 THEN BEGIN
	 print,'**2D scan**'
        pa2D = *(*gD).pa2D
        da2D = *(*gD).da2D

	if (seq-1) lt 0 or seq gt cpt(1) then begin
		str = ['Error: Invalid scan line number'+string(seq), $
		  '       Valid scan range: [1-' + strtrim(cpt(1),2) + ']']
		res = dialog_message(str,/error)
		return
	end

	title = pv(0)
	seqno = seq - 1
	scanno_2d = scanno
	yvalue = pa1D(seqno,0)
	npts = cpt(0)

	ndim = n_elements(id_def)/2
	pa = make_array(npts,4,/double)
	da = make_array(npts,ndim-4)
	pa[*,*] = pa2D[*,seqno,*]
	da[*,*] = da2D[*,seqno,*]

	max_pidi = n_elements(id_def)/2
	desc1 = labels(max_pidi:2*max_pidi-1,0)
	desc2 = labels(max_pidi:2*max_pidi-1,1)
	for i=0,max_pidi-1 do begin
	if id_def(i,0) gt 0 then $
	if desc1(i) eq '' then desc1(i) = labels(i,0)
	if id_def(i,1) gt 0 then $
	if desc2(i) eq '' then desc2(i) = labels(i,1)
	end

ENDIF ELSE begin
	 print,'**1D scan**'
	IF dim EQ 1 THEN BEGIN
		max_pidi = n_elements(id_def)
		npts = cpt
		desc1 = labels(max_pidi:2*max_pidi-1)
		for i=0,max_pidi-1 do begin
		if id_def(i) gt 0 then begin
		if desc1(i) eq '' then desc1(i) = labels(i)
		end
		end
		pa = pa1D
		da = da1D	
	END
end

	isel = 0
	xarr = pa(*,isel)
	yarr = da

	; set defualt ysel if not set by user
	if n_elements(ysel) eq 0 then begin
		st='0'
		for i=5,max_pidi-1 do begin
		if id_def(i) gt 0 then  st=st+','+strtrim(i-4,2)
		end
		ysel = st
	end
	IF keyword_set(xsel) EQ 0 AND keyword_set(ysel) EQ 0 THEN BEGIN
		xdesc = desc1(0:3)
		ydesc = desc1(4:max_pidi-1)
		xengu = labels(2*max_pidi:2*max_pidi+3,0)
		yengu = labels(2*max_pidi+4:3*max_pidi-1,0)
	END
	if keyword_set(xsel) then begin
		if xsel gt 0 and xsel lt 4 then isel = xsel
	end
	xdesc = desc1(isel)
	xengu = labels(isel,0)

	res=0
	if keyword_set(ysel) then begin
	 parse_num,string(ysel),res
	end
	no = n_elements(res)
	yarr = make_array(npts(0),no)
	ydesc = make_array(no,/string)
	yengu = make_array(no,/string)
	for i=0,no-1 do begin
	yarr(*,i) = da(*,res(i))
	ydesc(i) = desc1(4+res(i))
	yengu(i) = labels(max_pidi*2+4+res(i),0)
	end

	if keyword_set(plot) then begin
		if n_elements(res) eq 1 then $
		plot1d,xarr,yarr,title=title, $
		xtitle=xdesc(0), $
		ytitle=ydesc(0) else $
		plot1d,xarr,yarr,title=title, $
		xtitle=xdesc(0), group=group
	end
 
END

PRO get_1DLines,im,textfile,title,ydesc
; this function returns columns of 2D im array
; to get rows  just pass in transpose(im) to this routine
;
file='tmp.txt'
if n_elements(textfile) then file=textfile

s = size(im)
if n_elements(s) ne 5 then return

	f1="(" + strtrim(s(2),2) + "f14.5" + ")" 
	openw,1,file
	if n_elements(title) then printf,1,'; ** '+ title
	printf,1,'; ** This ASCII file contains a 2D Array'
	printf,1,'; ** # of fields per line represents total # of Y variables ' 
	printf,1,'; ** # of values per column represents dim of X.'

	st = ';  YDESC:    '

	ny = n_elements(ydesc)
	if ny gt 0 then begin
	for i=0,ny-1 do begin
		st = st + ydesc(i) + ' : '
	end
	end
	printf,1,st
 
	for i=0,s(1)-1 do begin
		vect = im(i,*)
	printf,1,string(vect),format=f1
	end
	close,1
END
