FORWARD_FUNCTION READ_SCAN,READ_SCAN_FIRST,READ_SCAN_REST


PRO rix2DC,Scan,gData
ON_ERROR,0 ;,1
 
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
	  *gData.pa3D  = ptr_new(/ALLOCATE_HEAP)
	  *gData.da3D  = ptr_new(/ALLOCATE_HEAP)
	end
	if *Scan.dim eq 2 then begin
          *gData.pa1D    = *(*Scan.pa)[1]
          *gData.da1D    = *(*Scan.da)[1]
          *gData.pa2D  = *(*Scan.pa)[0]
          *gData.da2D  = *(*Scan.da)[0]
	  *gData.pa3D  = ptr_new(/ALLOCATE_HEAP)
	  *gData.da3D  = ptr_new(/ALLOCATE_HEAP)
        end
	if *Scan.dim eq 3 then begin
          *gData.pa1D    = *(*Scan.pa)[2]
          *gData.da1D    = *(*Scan.da)[2]
          *gData.pa2D  = *(*Scan.pa)[1]
          *gData.da2D  = *(*Scan.da)[1]
          *gData.pa3D  = *(*Scan.pa)[0]
          *gData.da3D  = *(*Scan.da)[0]
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

  if rank gt *Scan.dim then goto,BAD
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
;print,'READ_SCAN_REST: rank=',rank,'   Nelem=',n_elements(tmp),'   Detector=',i+1
  end

  if(rank GT 1) then begin
    sub_offset=offset
    nb_sub= cpt
    if(cpt NE npts) then nb_sub=nb_sub+1
    for i=1,nb_sub do begin
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

  ndet = 85 ; 15
  ntot = ndet + 4

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

  (*Scan.da)[rank-1]= ptr_new(fltarr(size,ndet), /NO_COPY)
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
	(*Scan.labels)[ntot+num,rank-1]= pos_info.rxds
	(*Scan.labels)[ntot*2+num,rank-1]= pos_info.rxeu
     endif else begin
	(*Scan.labels)[num,rank-1]= pos_info.pxpv
	(*Scan.labels)[ntot+num,rank-1]= pos_info.pxds
	(*Scan.labels)[ntot*2+num,rank-1]= pos_info.pxeu
     endelse
  end

  for i=0,nb_det-1 do begin
     readu,lun,num
     det_num[i]=num
     readu,lun,det_info
     (*Scan.id_def)[4+num,rank-1]=1
     (*Scan.labels)[4+num,rank-1]= det_info.dxpv
     (*Scan.labels)[ntot+4+num,rank-1]= det_info.dxds
     (*Scan.labels)[ntot*2+4+num,rank-1]= det_info.dxeu
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

  ON_ERROR,0 ;,1
  ON_IOERROR,BAD

  res=0
  ndet = 85 ; 15
  ntot = ndet+4

  Scan = { $
	scanno	: ptr_new(/allocate_heap), $  ;0L, $
	dim	: ptr_new(/allocate_heap), $  ;0, $
	npts	: ptr_new(/allocate_heap), $  ;[0,0], $
	cpt	: ptr_new(/allocate_heap), $  ;[0,0], $
	id_def	: ptr_new(/allocate_heap), $  ;intarr(ntot,2), $
	pv	: ptr_new(/allocate_heap), $  ;['',''], $
	labels	: ptr_new(/allocate_heap), $  ;strarr(ntot*3,2), $
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
  *Scan.id_def= intarr(ntot,tmp.rank)
  *Scan.pv= strarr(tmp.rank)
  *Scan.labels= strarr(ntot*3,tmp.rank)
  *Scan.pa= ptrarr(tmp.rank)
  *Scan.da= ptrarr(tmp.rank)

  if(read_scan_first(lun, Scan, 0) NE 1) then goto,BAD

  for i=0,tmp.rank-1 do begin
    dims=(*Scan.npts)[i:tmp.rank-1]
    *(*Scan.pa)[i]= reform(*(*Scan.pa)[i], [dims,4])
    *(*Scan.da)[i]= reform(*(*Scan.da)[i], [dims,ndet])
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

