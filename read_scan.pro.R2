;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
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

  rank = *Scan.dim
	IF rank EQ 1 THEN BEGIN
    *gData.pa1D  = *(*Scan.pa)[0]
    *gData.da1D  = *(*Scan.da)[0]
	  *gData.pa2D  = ptr_new(/ALLOCATE_HEAP)
	  *gData.da2D  = ptr_new(/ALLOCATE_HEAP)
	  *gData.pa3D  = ptr_new(/ALLOCATE_HEAP)
	  *gData.da3D  = ptr_new(/ALLOCATE_HEAP)
	ENDIF
	IF rank EQ 2 THEN BEGIN
    *gData.pa1D  = *(*Scan.pa)[1]
    *gData.da1D  = *(*Scan.da)[1]
 if ptr_valid(gData.pa2D) eq 0 then *gData.pa2D  = ptr_new(/ALLOCATE_HEAP)
 if ptr_valid((*Scan.pa)[0]) then *gData.pa2D  = *(*Scan.pa)[0] 
 if ptr_valid(gData.da2D) eq 0 then *gData.da2D  = ptr_new(/ALLOCATE_HEAP)
 if ptr_valid((*Scan.da)[0]) then *gData.da2D  = *(*Scan.da)[0] 
	  *gData.pa3D  = ptr_new(/ALLOCATE_HEAP)
	  *gData.da3D  = ptr_new(/ALLOCATE_HEAP)
  ENDIF
	IF rank EQ 3 THEN BEGIN
    *gData.pa1D  = *(*Scan.pa)[2]
    *gData.da1D  = *(*Scan.da)[2]
    *gData.pa2D  = *(*Scan.pa)[1]
    *gData.da2D  = *(*Scan.da)[1]
    *gData.pa3D  = *(*Scan.pa)[0]
if n_elements(*gData.da3D) lt 1 then *gData.da3D  = ptr_new(/ALLOCATE_HEAP)
if ptr_valid((*Scan.da)[0]) then *gData.da3D  = *(*Scan.da)[0]
	ENDIF
 
	free_scanAlloc,Scan

END

PRO free_scanAlloc,Scan

  rank = *Scan.dim

  ptr_free,Scan.timestamp1
  ptr_free,Scan.timestamp2
  ptr_free,Scan.scanno
  ptr_free,Scan.dim
  ptr_free,Scan.npts
  ptr_free,Scan.cpt
  ptr_free,Scan.id_def
  ptr_free,Scan.pv
  ptr_free,Scan.labels
  for i=0,rank-1 do begin
  ptr_free,(*scan.pa)[i]
  ptr_free,(*scan.da)[i]
  end
  ptr_free,Scan.pa
  ptr_free,Scan.da

;  Scan = 0
;  heap_gc

END
	

FUNCTION nbElem,dim,vectOR
  res=1L
  FOR i=0,dim-1 DO BEGIN
     res= res*vectOR[i]
  ENDFOR
  RETURN, res
END


FUNCTION read_scan_rest,lun,Scan,dim,offset,DetMax,dump,pickDet=pickDet
ON_IOERROR, BAD	

  rank=0
  npts=0
  cpt=0
  readu,lun,rank,npts,cpt
  
;  print,'REST  *** rank,npts,offset', rank,npts,offset
  
  IF(rank GT 1) THEN BEGIN
    sub_scan_ptr=lonarr(npts)
    readu,lun,sub_scan_ptr
  ENDIF

  (*Scan.cpt)[rank-1]=cpt;

  ; read the pvname
  name=''
  time=''
  readu,lun,name,time
  *Scan.timestamp2 = time
  
  nb_pos=0
  nb_det=0
  nb_trg=0
  readu,lun,nb_pos,nb_det,nb_trg

  IF(nb_pos NE 0) THEN BEGIN
    pos_num=intarr(nb_pos)
  ENDIF
  pos_info= { $
    pxpv:'', $
    pxds:'', $
    pxsm:'', $
    pxeu:'', $
    rxpv:'', $
    rxds:'', $
    rxeu:'' }
  IF(nb_det NE 0) THEN BEGIN
    det_num=intarr(nb_det)
  ENDIF

  det_info= { $
    dxpv:'', $
    dxds:'', $
    dxeu:'' }
  IF(nb_trg NE 0) THEN trg_num=intarr(nb_trg)
  trg_info= { $
    txpv:'', $
    txcd: 1.0 } 

  num=0
  FOR i=0,nb_pos-1 DO BEGIN
    readu,lun, num
    pos_num[i]=num
    readu,lun,pos_info
  ENDFOR

  FOR i=0,nb_det-1 DO BEGIN
    readu,lun,num
    det_num[i]=num
    readu,lun,det_info
  ENDFOR

  FOR i=0,nb_trg-1 DO BEGIN
    readu,lun,num
    trg_num[i]=num
    readu,lun,trg_info
  ENDFOR
  
  IF nb_pos GT 0 THEN BEGIN
    tmp=dblarr(npts)
    FOR i=0,nb_pos-1 DO BEGIN
      readu,lun,tmp
; change to single vector only
      IF(cpt NE 0) THEN (*(*Scan.pa)[rank-1])[0:cpt-1,pos_num[i]]=tmp[0:cpt-1]
;	if i eq 0 then $
;      IF(cpt NE 0) THEN (*(*Scan.pa)[rank-1])[offset[rank-1]:offset[rank-1]+cpt-1,0]=tmp[0:cpt-1]
;      IF(cpt NE 0) THEN (*(*Scan.pa)[rank-1])[offset[rank-1]:offset[rank-1]+cpt-1,pos_num[i]]=tmp[0:cpt-1]
    ENDFOR
  ENDIF

  IF (nb_det GT 0) AND (cpt GT 0) THEN BEGIN
    tmp=fltarr(npts)
    point_lun,-lun,filepos
    IF rank eq 1 and keyword_set(pickDet) THEN BEGIN
	if pickDet gt 0 then begin
      FOR i=0,nb_det-1 DO BEGIN
        IF det_num[i]+1 EQ pickDet THEN BEGIN
          point_lun,lun, (filepos+npts*4L*i)
          readu,lun,tmp
          (*(*Scan.da)[rank-1])[offset[rank-1]:offset[rank-1]+cpt-1,0]=tmp[0:cpt-1]
          GOTO,doneDetectors
        ENDIF
      ENDFOR
	end
    ENDIF ELSE BEGIN
      FOR i=0,nb_det-1 DO BEGIN
        IF det_num[i] LT DetMax[rank-1] THEN BEGIN
          point_lun,lun, (filepos+npts*4L*i)
          readu,lun,tmp
          (*(*Scan.da)[rank-1])[offset[rank-1]:offset[rank-1]+cpt-1,det_num[i]]=tmp[0:cpt-1]
        ENDIF
      ENDFOR
    ENDELSE
  ENDIF
doneDetectors:

      
  IF(rank GT 1) THEN BEGIN
    FOR i=0,npts-1 DO BEGIN
      IF sub_scan_ptr[i] EQ 0 THEN GOTO,done
      point_lun,lun,sub_scan_ptr[i]
      res= read_scan_rest(lun,Scan,dim+1,offset,DetMax,dump,pickDet=pickDet)
      IF(res NE 1) THEN GOTO,BAD
    ENDFOR
done:
  END

  offset[rank-1]=offset[rank-1]+(*Scan.npts)[rank-1]

  RETURN, 1
BAD:
  RETURN, 0
END  



FUNCTION read_scan_first,lun,Scan,dim,offset,DetMax,dump,pickDet=pickDet,header=header
ON_IOERROR, BAD	

  ndet = 85 ; 15
  ntot = ndet + 4

  rank=0
  npts=0
  cpt=0
  readu,lun,rank,npts,cpt

;  print,'FIRST *** rank,npts,offset', rank,npts,offset

  IF(rank GT 1) THEN BEGIN
    sub_scan_ptr=lonarr(npts)
    readu,lun,sub_scan_ptr
  ENDIF

  (*Scan.cpt)[rank-1]=cpt;

  ; read the pvname
  name=''
  time=''
  readu,lun,name,time
  *Scan.timestamp1 = time
  (*Scan.pv)[rank-1]=name
  
  nb_pos=0
  nb_det=0
  nb_trg=0
  readu,lun,nb_pos,nb_det,nb_trg

IF dump then print,'nb_pos,nb_det,nb_trg:',nb_pos,nb_det,nb_trg

  dims=(*Scan.npts)[rank-1:rank+dim-1]
  size= nbElem(dim+1, dims)
  IF(nb_pos NE 0) THEN pos_num= intarr(nb_pos)

  (*Scan.pa)[rank-1]= ptr_new(dblarr(dims[0],4), /NO_COPY)    ; one vector Position
;  (*Scan.pa)[rank-1]= ptr_new(dblarr(size,1), /NO_COPY)    ; one Position
;  (*Scan.pa)[rank-1]= ptr_new(dblarr(size,4), /NO_COPY)
;  (*(*Scan.pa)[rank-1])[*]= !VALUES.D_NAN  

  pos_info= { $
    pxpv:'', $
    pxds:'', $
    pxsm:'', $
    pxeu:'', $
    rxpv:'', $
    rxds:'', $
    rxeu:'' }

  IF(nb_det NE 0) THEN det_num=intarr(nb_det)

  det_info= { $
    dxpv:'', $
    dxds:'', $
    dxeu:'' }

  IF(nb_trg NE 0) THEN trg_num=intarr(nb_trg)
  trg_info= { $
    txpv:'', $
    txcd: 1.0 }

  num=0
  FOR i=0,nb_pos-1 DO BEGIN
    readu,lun, num
    pos_num[i]=num
    readu,lun,pos_info
    IF dump THEN print,"====>position: ",i,num
    IF dump THEN help,pos_info,/st
    (*Scan.id_def)[num,rank-1]=1
    IF(pos_info.rxpv NE '') THEN BEGIN
	    (*Scan.labels)[num,rank-1]= pos_info.rxpv
	    (*Scan.labels)[ntot+num,rank-1]= pos_info.rxds
	    (*Scan.labels)[ntot*2+num,rank-1]= pos_info.rxeu
    ENDIF ELSE BEGIN
	    (*Scan.labels)[num,rank-1]= pos_info.pxpv
	    (*Scan.labels)[ntot+num,rank-1]= pos_info.pxds
	    (*Scan.labels)[ntot*2+num,rank-1]= pos_info.pxeu
     ENDELSE
  ENDFOR

  FOR i=0,nb_det-1 DO BEGIN
    readu,lun,num
    det_num[i]=num
    DetMax[rank-1] = num+1
;print,'rank,i,detno,detmax',rank,i,num,detMax
;if rank eq 1 and num gt DetMax then DetMax=num
    readu,lun,det_info
    IF dump THEN print,"====>detector: ",i,num
    IF dump THEN help,det_info,/st
    (*Scan.id_def)[4+num,rank-1]=1
    (*Scan.labels)[4+num,rank-1]= det_info.dxpv
    (*Scan.labels)[ntot+4+num,rank-1]= det_info.dxds
    (*Scan.labels)[ntot*2+4+num,rank-1]= det_info.dxeu
  ENDFOR

  IF keyword_set(pickDet) GT 0 and rank eq 1 THEN DetMax[rank-1] = 1

  if rank eq 1 and keyword_set(pickDet) then begin
	 if pickDet lt 0 then goto,bypass
   end
  (*Scan.da)[rank-1]= ptr_new(fltarr(size,DetMax[rank-1]), /NO_COPY)
;  (*(*Scan.da)[rank-1])[*]= !VALUES.F_NAN
bypass:

  FOR i=0,nb_trg-1 DO BEGIN
    readu,lun,num
    trg_num[i]=num
    readu,lun,trg_info
  ENDFOR

  IF keyword_set(header) THEN RETURN,1

  IF nb_pos GT 0 THEN BEGIN
    tmp=dblarr(npts)
    FOR i=0,nb_pos-1 DO BEGIN
      readu,lun,tmp
      IF(cpt NE 0) THEN (*(*Scan.pa)[rank-1])[0:cpt-1,pos_num[i]]=tmp[0:cpt-1]
;	if i eq 0 then $
;      IF(cpt NE 0) THEN (*(*Scan.pa)[rank-1])[offset[rank-1]:offset[rank-1]+cpt-1,0]=tmp[0:cpt-1]
;      IF(cpt NE 0) THEN (*(*Scan.pa)[rank-1])[offset[rank-1]:offset[rank-1]+cpt-1,pos_num[i]]=tmp[0:cpt-1]
    ENDFOR
  ENDIF

  IF (nb_det GT 0) AND (cpt GT 0) THEN BEGIN
    point_lun,-lun,filepos
    tmp=fltarr(npts)
    IF keyword_set(pickDet) and rank eq 1 THEN BEGIN
	if pickDet gt 0 then begin
      FOR i=0,nb_det-1 DO BEGIN
        IF det_num[i]+1 EQ pickDet THEN BEGIN
          point_lun,lun,filepos+npts*4L*i
          readu,lun,tmp
          (*(*Scan.da)[rank-1])[offset[rank-1]:offset[rank-1]+cpt-1,0]=tmp[0:cpt-1]
          GOTO,doneDetectors
        ENDIF
      ENDFOR
	end
    ENDIF ELSE BEGIN
      FOR i=0,nb_det-1 DO BEGIN
        IF det_num[i] LT DetMax[rank-1] THEN BEGIN
          point_lun,lun,filepos+npts*4L*i
          readu,lun,tmp
          (*(*Scan.da)[rank-1])[offset[rank-1]:offset[rank-1]+cpt-1,det_num[i]]=tmp[0:cpt-1]
        ENDIF
      ENDFOR
    ENDELSE
  ENDIF
doneDetectors:


  IF(rank GT 1) THEN BEGIN
    IF sub_scan_ptr[0] EQ 0 THEN GOTO,done
    point_lun,lun,sub_scan_ptr[0]
    if(read_scan_first(lun,Scan,dim+1,offset,DetMax,dump,pickDet=pickDet) NE 1) THEN GOTO,bad
    FOR i=1,npts-1 DO BEGIN
      IF sub_scan_ptr[i] EQ 0 THEN GOTO,done
      point_lun,lun,sub_scan_ptr[i]
      if(read_scan_rest(lun,Scan,dim+1,offset,DetMax,dump,pickDet=pickDet) NE 1) THEN GOTO,bad
    ENDFOR
done:
  ENDIF

  offset[rank-1]=offset[rank-1]+(*scan.npts)[rank-1]
  RETURN, 1

BAD:
  RETURN, 0
END  


FUNCTION read_scan,filename, Scan, dump=dump, lastDet=lastDet,pickDet=pickDet,header=header
; lastDet if specified only detectOR 1 to lastDet is extracted
; pickDet>=1  if specified only the specified detector is extracted 
;             it is target for big 3D scan 
; DetMax[3]     return actual detMax
;

  debug = 0
  IF keyword_set(dump) THEN debug = dump
  ON_ERROR,0 ;,1
  ON_IOERROR,BAD

  res=0
  ndet = 85 ; 15
  ntot = ndet+4

  if n_elements(Scan) eq 0 then $
  Scan = { $
        timestamp1: ptr_new(/allocate_heap), $
        timestamp2: ptr_new(/allocate_heap), $
  	scanno	: ptr_new(/allocate_heap), $  ;0L, $
  	dim	: ptr_new(/allocate_heap),     $  ;0, $
  	npts	: ptr_new(/allocate_heap),   $  ;[0,0], $
  	cpt	: ptr_new(/allocate_heap),     $  ;[0,0], $
  	id_def	: ptr_new(/allocate_heap), $  ;intarr(ntot,2), $
  	pv	: ptr_new(/allocate_heap),     $  ;['',''], $
  	labels	: ptr_new(/allocate_heap), $  ;strarr(ntot*3,2), $
  	pa	: ptr_new(/allocate_heap),     $
  	da	: ptr_new(/allocate_heap)      $
	}

  get_lun, lun
  openr, /XDR, lun, filename      ;Open the file for input.

  tmp= {$
    version: 0.0, $
    scanno : 0L , $
    rank   : 0L }

  readu,lun, tmp

  npts= intarr(tmp.rank)
  readu,lun, npts
  readu,lun, isRegular
  readu,lun, env_fptr

  *Scan.timestamp1=''
  *Scan.timestamp2=''
  *Scan.scanno=tmp.scanno
  *Scan.dim= tmp.rank
  *Scan.npts= reverse(npts)
  *Scan.cpt = intarr(tmp.rank)
  *Scan.id_def= intarr(ntot,tmp.rank)
  *Scan.pv= strarr(tmp.rank)
  *Scan.labels= strarr(ntot*3,tmp.rank)
  *Scan.pa= ptrarr(tmp.rank)
  *Scan.da= ptrarr(tmp.rank)

  if keyword_set(pickDet) then begin
	if tmp.rank lt 3 then pickdet=0
  end

; IF 3D rank exceed 500 set default pickDet=16 
  IF tmp.rank EQ 3  THEN BEGIN
  	IF n_elements(pickDet) EQ 0 THEN BEGIN 
    	IF npts(0) GE 1000 or npts(1) GE 500 OR npts(2) GE 500 THEN pickDet = 16
	  ENDIF
  dd =1L *npts(0)*npts(1)*npts(2)
	if dd gt 500000000L then begin
        msg = ['Warning! 3D scan array dimension kind of big', string(npts), $
                'Only one detector returned : ',string(pickDet)]
	print,msg
;	r = dialog_message(msg,/error)
;       goto,BAD
	end
  ENDIF

  ; extract the first DetMax detectORs only
  DetMax = intarr(tmp.rank)
  DetMax(*) = ndet
  IF keyword_set(lastDet) THEN DetMax = lastDet 

  offset= lonarr(tmp.rank)
  IF(read_scan_first(lun, Scan,0,offset,DetMax,debug,pickDet=pickDet,header=header) NE 1) THEN GOTO,BAD

  IF keyword_set(header) THEN GOTO,DONE

  fOR i=0,tmp.rank-1 DO BEGIN
    dims=(*Scan.npts)[i:tmp.rank-1]
	if dims[0] le 0 then goto,BAD
    *(*Scan.pa)[i]= reform(*(*Scan.pa)[i], [dims[0],4])       ; vector only
;    *(*Scan.pa)[i]= reform(*(*Scan.pa)[i], [dims,4])         ; only one PI
;    *(*Scan.pa)[i]= reform(*(*Scan.pa)[i], [dims,1])

;    IF i eq 0 and keyword_set(pickDet) THEN $
;      *(*Scan.da)[i]= reform(*(*Scan.da)[i], [dims]) ELSE $
	if ptr_valid((*Scan.da)[i]) eq 1 then $
	*(*Scan.da)[i]= reform(*(*Scan.da)[i], [dims,DetMax[i]])
    IF debug THEN BEGIN
      print,'dims: ',dims, tmp.rank
      help,*(*Scan.pa)[i]
      print,i,min(*(*Scan.pa)[i]), max(*(*Scan.pa)[i])
	if ptr_valid((*Scan.da)[i]) eq 1 then begin 
      help,*(*Scan.da)[i]
      print,i,min(*(*Scan.da)[i]), max(*(*Scan.da)[i])
	end
    ENDIF
  ENDFOR

  res= *Scan.scanno

  GOTO,DONE
BAD:
  res= -1
  print, !ERR_STRING
DONE:
  free_lun, lun

  RETURN, res
END


