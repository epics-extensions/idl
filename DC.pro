
PRO rix2BenChin, Scan
ON_ERROR,1
  if(*Scan.dim EQ 1) then begin
    BenChin= { $
	scanno	: Scan.scanno, $
	dim	: Scan.dim, $
	num_pts : Scan.npts, $
	cpt	: Scan.cpt, $
	id_def	: Scan.id_def, $
	pv	: Scan.pv, $
	labels	: Scan.labels, $
	pa1D	: (*Scan.pa)[0], $
	da1D	: (*Scan.da)[0], $
	pa2D	: ptr_new(/ALLOCATE_HEAP), $
	da2D	: ptr_new(/ALLOCATE_HEAP) $
	}
  endif else begin
    BenChin= { $
	scanno	: Scan.scanno, $
	dim	: Scan.dim, $
	num_pts : Scan.npts, $
	cpt	: Scan.cpt, $
	id_def	: Scan.id_def, $
	pv	: Scan.pv, $
	labels	: Scan.labels, $
	pa1D	: (*Scan.pa)[1], $
	da1D	: (*Scan.da)[1], $
	pa2D	: (*Scan.pa)[0], $
	da2D	: (*Scan.da)[0] $
	}
  endelse

  ptr_free,Scan.pa
  ptr_free,Scan.da
  Scan=BenChin
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

;
;  To save platform independent data use the /XDR option on file open
;
;  u_openw, unit [,/XDR]  [,'filename']
;  u_openr, unit [,/XDR]  [,'filename']
;  u_write 
;  u_read
;  u_dump
;  u_rewind, unit
;
; Examples: 
;  u_openw, unit, 'filename'
;  u_write, unit, x
;  u_close, unit
;
;
;  u_dump
;
;  u_openr, unit, 'filename'
;  u_read, unit, x  &  print,x
;  u_close, unit
;

;PRO DebugError
;help,/struct,!error_state
;END

FUNCTION u_writePermitted,filename,VT=VT
;
; check for filename write permission
;
; existed
	found = findfile(filename)
	if found(0) ne '' then begin
	ret=''
	if keyword_set(VT) then $ 
	read,ret,prompt='Overwrite the existed file - '+filename+' (Yes/No) ?' else $
	ret= widget_message(['Do you want to overwrite the existed file : ',$
		'','     '+filename], $
			/question)
	if strupcase(ret) eq 'NO' then return,-1
	end
; create new
        CATCH,error_status
;        if !error_state.name eq 'IDL_M_CNTOPNFIL' then begin 
	if error_status eq -215 or error_status eq -206 then begin
		if keyword_set(VT) then $
		read,ret,prompt=!err_string+string(!err) else $
                ret=WIDGET_MESSAGE(!err_string + string(!err))
                if n_elements(unit) then u_close,unit
                return,-1
        end
	openw,1,filename
	close,1
	return,0
END

PRO u_rewind,unit
;+
; NAME:
;       U_REWIND
;
; PURPOSE:
;       This routine locates the LUN file pointer at the beginning of the 
;       file.
;
; CALLING SEQUENCE:
;
;       U_REWIND, Unit
;
; INPUTS:
;       Unit:     The LUN number to be rewind.
;
; OUTPUTS:
;       None.
;
; EXAMPLE:
;
;        U_REWIND, unit
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       07-25-97      bkc  Rename routine rewind to u_rewind     
;-
point_lun,unit,0
END

PRO u_openw,unit,filename,append=append,help=help,XDR=XDR,ERRCODE
;+
; NAME:
;       U_OPENW
;
; PURPOSE:
;       This routine assigns a LUN to the opened file for unformatted
;       write only.
;
; CALLING SEQUENCE:
;
;       U_OPENW, Unit, 'filename' [,/Append] [,/XDR] [,/Help]
;
; INPUTS:
;       filename: Specifies the filename to be created or opened for
;                 data recording through using the U_WRITE command.
;
; OUTPUTS:
;       Unit:     The LUN number to be associated with the opened file.
;
; KEYWORD PARAMETERS:
;       APPEND:   This keyword specifies that the file is opened for 
;                 data appending. If not specified, write on the unit
;                 will replace the old file content by the new data.
;       XDR:      This keyword specifies that the file is opened for 
;                 writing data in platform-independent XDR binary form. 
;       HELP:     If this keyword is set, a simple on line help is given.
;
; RESTRICTIONS:
;       The data file should contain only consistant type of binary 
;       objects: either native binary data or platform-independent 
;       XDR data. No mixture type is allowed.
;
; EXAMPLE:
;
;        U_OPENW, unit, 'catch1d.trashcan'
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       xx-xx-xx      iii  comment     
;-
;
if keyword_set(help) then goto, help1
if n_elements(filename) eq 0 then filename='data.dat'
ERRCODE=0
        CATCH,error_status
;        if !error_state.name eq 'IDL_M_CNTOPNFIL' then begin 
	if !err eq -215 or !err eq -206 then begin
                ret=WIDGET_MESSAGE(!err_string + string(!err))
                if n_elements(unit) then u_close,unit
		ERRCODE=-99
;		exit
                return 
        end

if keyword_set(XDR) then begin
	if keyword_set(append) then $
	openw,/XDR,unit,filename,/GET_LUN,/APPEND else $
	openw,/XDR,unit,filename,/GET_LUN  
endif else begin
	if keyword_set(append) then $
	openw,unit,filename,/GET_LUN,/APPEND else $
	openw,unit,filename,/GET_LUN  
end

if n_params() eq 0 then print,'unit=',unit
return

help1:
	print,''
	print,'Usage:  u_openw, unit, filename'
	print,''
	print,'This routine assigns a LUN to the opened file for write only.'
	print,'        unit     - a LUN is returned by unit'
	print,"        filename - optional, default to 'data.dat'" 
	print,'e.g.'
	print,'       u_openw,unit'
	print,"       u_openw,unit,'data1.dat'"
	print,''
END

PRO u_openr,unit,filename,help=help,XDR=XDR
;+
; NAME:
;       U_OPENR
;
; PURPOSE:
;       This routine assigns a LUN to the opened file for unformatted
;       read only.
;
; CALLING SEQUENCE:
;
;       U_OPENR, Unit, 'filename' [,/XDR] [,/Help]
;
; INPUTS:
;       filename: Specifies the filename to be read by the U_READ 
;                 command.
;
; OUTPUTS:
;       Unit:     The LUN number to be associated with the opened file.
;
; KEYWORD PARAMETERS:
;       XDR:      This keyword specifies that the file is opened for 
;                 reading data in platform-independent XDR binary form. 
;       HELP:     If this keyword is set, a simple on line help is given.
;
; RESTRICTIONS:
;       The data file should contain only consistant type of binary 
;       objects: either native binary data or platform-independent 
;       XDR data. No mixture type is allowed.
;
; EXAMPLE:
;
;        U_OPENR, unit, 'catch1d.trashcan'
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       xx-xx-xx      iii  comment     
;-
if keyword_set(help) then goto, help1
if n_elements(filename) eq 0 then filename='data.dat'

if keyword_set(XDR) then  $
	openr,/XDR,unit,filename,/GET_LUN $
else $
	openr,unit,filename,/GET_LUN

if n_params() eq 0 then print,'unit=',unit
return

help1:
	print,''
	print,'Usage:  u_openr, unit, filename '
	print,''
	print,'This routine assigns a LUN to the opened file for read only.'
	print,'        unit     - a LUN is returned by unit'
	print,"        filename - optional, default to 'data.dat'" 
	print,'e.g.'
	print,'       u_openr,unit'
	print,"       u_openr,unit,'data1.dat'"
	print,''
END

PRO u_close,unit
;+
; NAME:
;       U_CLOSE
;
; PURPOSE:
;       This routine closes a file LUN opened for unformmated I/O.
;
; CALLING SEQUENCE:
;
;       U_CLOSE, Unit
;
; INPUTS:
;       Unit:     The LUN number to be closed.
;
; OUTPUTS:
;       None.
;
; EXAMPLE:
;
;        U_CLOSE, unit
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       xx-xx-xx      iii  comment     
;-
close,unit
free_lun,unit
END

;
;  u_write, unit, array
;
PRO u_write,unit,array,help=help
;+
; NAME:
;       U_WRITE
;
; PURPOSE:
;       This routine writes an IDL data array to a file unit which is
;       opened for unformatted write. It supports all IDL data type except the
;       complex number and up to 2D array.  
;
; CALLING SEQUENCE:
;
;       U_WRITE, Unit, Var [,/Help]
;
; INPUTS:
;       Unit:   The logic unit number returned by file open for unformatted
;               write.
;
;       Var:    This variable holds the data  array to be written to
;               the opened file, it can be scaler, vector, or 2D array.
;	
; KEYWORD PARAMETERS:
;       HELP:   If this keyword is set, a simple on line help is given.
;
;
; RESTRICTIONS:
;       The data array can not be complex number. In order to make the
;       data to be read by the U_READ routine, all the data saved must 
;       be using this routine. 
;
; EXAMPLE:
;
;       Create the 'test.dat'  and write the variable X to the file
;
;         u_openw,unit,'test.dat'
;         u_write, unit, X
;         u_close,unit
;
;       Create or append X to the 'test.dat'  
;
;         u_openw,unit,'test.dat',/append
;         u_write, unit, X
;         u_close,unit
;
;       Create XDR platform independent data to the 'test.dat'  
;
;         u_openw,unit,/XDR,'test.dat'
;         u_write, unit, X
;         u_close,unit
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       05-30-97	bkc	Support opened file as XDR type data.
;-
;
if keyword_set(help) then goto, help1
if n_params() ne 2 then begin
	print,'Usage: u_write, unit, array'
	return
end
s = size(array)
if (s(0) eq 0) and (s(1) eq 0) then return    	; undefined
no = n_elements(s)
if s(no-2) eq 7 then begin
	len = strlen(array)
	if no eq 4 then s = [s,len(0)] else $		; vector
	if no eq 3 then s = [s,0,len]			; scalor
endif else begin
	if no eq 4 then s = [s,0] else $		; vector
	if no eq 3 then s = [s,0,0]			; scalor
end
writeu,unit,s(0:4),array
return

help1:
	print,''
	print,'Usage:  u_write, unit, array'
	print,''
	print,'This routine writes an array variable to the referenced unit in'
	print,'unformatted form.'
	print,'       unit     - required, a LUN already opened'
	print,"       array    - the array contents to be recorded" 
	print,''
	print,'Note:  For each array, two variables are written:'
	print,'       the long(5) size info array, and the array itself.'
	print,'       For string array each element must be exactly same size'
	print,' e.g.'
	print,'       u_write, unit, findgen(3,5)'
	print,"       u_write, unit, ,['111','222','333']"
	print,''
END


;
; dump unfomatted data ( which was written by u_write)
;
PRO u_dump,norecord,filename,data=data,help=help,XDR=XDR
ON_IOERROR,BAD
if keyword_set(help) then goto, help1
if n_elements(filename) eq 0 then filename='data.dat'

if keyword_set(XDR) then $
	openr,/XDR,unit,filename,/GET_LUN $
else $
	openr,unit,filename,/GET_LUN
	
s = lonarr(5)
norecord = 0
print,''
print,'DUMP FILE : ',filename
WHILE NOT EOF(unit) DO BEGIN
norecord = norecord + 1
u_read_set,unit,s,x
print,''
if keyword_set(data) then print,'<<',norecord,'>>  RECORDED SIZE & DATA:' else print,'<<',norecord,'>>  RECORDED SIZE INFO :'
print,'SIZE=',s
if keyword_set(data) then print,x
ENDWHILE
	goto, done
BAD:
	print,'Error encounted in'
	print,!ERR_STRING
done:
free_lun,unit
return

help1:
print,''
print,'Usage: u_dump, /data, norecord, filename '
print,''
print,'This routine dump the unformatted data from a file which was recored by u_write'
print,"       norecord - optional, if given it returns the number of records to the caller"
print,"       filename - optional, default to 'data.dat'"
print,"       /data    - optional, both size, and data arrays will be printed'"
print,''
print,' e.g.'
print,''
print,'       u_dump'
print,"       u_dump,no,'data1.dat',/data"
print,''
END


PRO u_read_set,unit,s,x,ERRCODE,help=help
if n_params() lt 3 then begin
	print,''
	print,'Usage: u_read_set, unit, s, x'
	print,''
	print,'This routine reads a set of two varaibles: size and array '
	print,'from the LUN unit. Note that the data must be recorded by the'
	print,'u_write routine.'
	print,'     where   S    LONG = Array(5), must be defined before calling this routine'
	print,'             X    returned array'
	print,'       ERRCODE    returned code, 0 for success, -99 for failure'
	return
	end

CATCH,error_status
if error_status  eq -229 or error_status eq -219 or error_status eq -184 then begin 
	str = [ !err_string + string(!err),'', $
		'Error: unable to read data, wrong type of file opened!!' ]
	ret=WIDGET_MESSAGE(str)
	return
	end

IF EOF(unit) THEN begin
	print,'Error! Error! Error!'
	print,'Error: wrong type or bad data encountered'
	return 
END
readu,unit,s

if (s(0) gt 1L) then begin	; two dim
	type = s(3)
	int = s(4)
endif else if (s(0) eq 1) then begin    ; one dim
	type = s(2)
	int = s(3)
endif else if (s(0) eq 0) then begin     ; scalor
	type = s(1)
	int = s(2)
end
;print,s
;print,'type=',type, '  dim=',int
int2 = int/s(1)
case fix(type) of 
	1: if (s(0) eq 2) then begin		; byte
		x = make_array(s(1),s(2),/byte) 
	   endif else begin
		x = bytarr(fix(int))  	
	   end
	2: if (s(0) eq 2) then begin		; int
		x = make_array(s(1),s(2),/int) 
	   endif else begin
		x = intarr(fix(int))  	
	   end
	3: if (s(0) eq 2) then begin		; long 
		x = make_array(s(1),s(2),/long) 
	   endif else begin
		x = lonarr(fix(int))  	
	   end
	4: if (s(0) eq 2) then begin		; float
		x = make_array(s(1),s(2),/float) 
	   endif else begin
		x = fltarr(fix(int))  	
	   end
	5: if (s(0) eq 2) then begin		; double 
		x = make_array(s(1),s(2),/double) 
	   endif else begin
		x = make_array(fix(int),/double)  	
	   end
	6: if (s(0) eq 2) then begin		; complex
		x = make_array(s(1),s(2),/complex) 
	   endif else begin
		x = make_array(fix(int),/complex)  	
	   end
	7: if (s(0) eq 2) then begin		; string
		print,'Error u_write/u_read can only support single string array'
		print,'size=',s
		return
	   endif else begin
		x = make_array(fix(int),/string,value=string(replicate(32b,s(4))))  	
	   end
else: begin
	print,'type=',type
;		ret=WIDGET_MESSAGE('Error: wrong type of data read in!!')
;		retall
		return
	end
endcase

	readu,unit,x
ERRCODE = 0
END


PRO u_read,unit,x,ERRCODE,help=help
;+
; NAME:
;       U_READ
;
; PURPOSE:
;       This routine reads an unformatted data entity from a file unit which is
;       opened for unformatted read. It supports all IDL data type except the
;       complex number and up to 2D array.  
;
; CALLING SEQUENCE:
;
;       U_READ, Unit, Var [,ERRCODE ,/Help]
;
; INPUTS:
;       Unit:   The logic unit number returned by file open for unformatted
;               read.
;	
; KEYWORD PARAMETERS:
;       HELP:   If this keyword is set, a simple on line help is given.
;
; OUTPUTS:
;       Var:    This variable holds the right type of data obtained from 
;               the opened file, it can be either 1D vector, or 2D array.
;   ERRCODE:    This variable holds the error code for the u_read. It
;               returns 0 if succeeded, returns -99 if failed.
;
; RESTRICTIONS:
;       All the data must be created by the U_WRITE routine in order to be 
;       read by this routine.
;
; EXAMPLE:
;
;       Read the first data entity from the 'test.dat' which was previously
;       created by the U_WRITE routine.
;
;         u_openr,unit,'test.dat'
;         u_read, unit, X
;         u_close,unit
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       05-30-97	bkc	Support opened file as XDR type data.
;       10-13-97	bkc	Add the ERRCODE to indicate success or failure.
;-

ERRCODE = -99
if keyword_set(help) then goto, help1
if n_params() lt 2 then begin
	print,'Usage: u_read, unit, array'
	return
	end
s = lonarr(5)
IF NOT EOF(unit) THEN  u_read_set,unit,s,x,ERRCODE ;ELSE print,'EOF on unit ',unit
return

help1:
	print,''
	print,'Usage: u_read, unit, x'
	print,''
	print,'This routine reads an array from the LUN unit.'
	print,'Note that the data must be recorded by the u_write routine.'
	print,'       X    returned array'

END


PRO u_bi2xdr,filename,help=help,VT=VT
;+
; NAME:
;       U_BI2XDR
;
; PURPOSE:
;       This IDL routine converts native binary data into platform-independent
;       XDR binary data. 
;
;       The input file should contain only pure native binary data.
;       The output filename uses the input filename suffixed with '.xdr'.
;
; CALLING SEQUENCE:
;
;       U_BI2XDR, 'filename' [,/VT] [,/Help]
;
; INPUTS:
;       filename:   The data file should contain pure binary data objects.
;
; OUTPUTS:
;       filename.xdr:   Output file. 
;                   It contains the converted XDR binary data objects.
;
; KEYWORD PARAMETERS:
;       VT:     If a dumb terminal without X window server is used, 
;               this option must be set, e.g. a telnet session.
;       HELP:   If this keyword is set, a simple on line help is given.
;
; RESTRICTIONS:
;       The input data file should contain pure binary data objects.
;
; EXAMPLE:
;
;        U_BI2XDR,'catch1d.trashcan'
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 05-30-97.
;
;       xx-xx-xx      iii  comment     
;-
;

if n_elements(filename) eq 0 or keyword_set(help) then goto,help1

	found = findfile(filename)
	if found(0) eq '' then begin
		print,'Error: '+filename+' not found!'
		return
		end
	if keyword_set(VT) then $
	OK_WRITE = u_writePermitted(filename+'.xdr',/VT) else $
	OK_WRITE = u_writePermitted(filename+'.xdr')
	if OK_WRITE lt 0 then return

        id=0
        u_openr,unit,filename
;        u_openw,unit2,filename+'.xdr',/XDR
	openw,/XDR,unit2,filename+'.xdr',/GET_LUN  

        WHILE NOT  EOF(unit) DO BEGIN
        id = id + 1
        u_read,unit,x
	u_write,unit2,x
        END
        maxno = id
        u_close,unit
        u_close,unit2
	if keyword_set(VT) then $
	print,string(maxno)+' sets of binary objects saved in "'+ filename+'.xdr"' else $
        ret=WIDGET_MESSAGE(string(maxno)+' sets of binary objects saved in "'+ $
		filename+'.xdr"')

	return

help1:

	print,''
	print,'Usage: U_BI2XDR,"filename"'
	print,''
	print,'This program converts the pure binary data objects into XDR binary format.'
	print,'The file "filename.xdr" created will be IDL platform independent.'
	print,''
END

PRO u_xdr2bi,filename,help=help,VT=VT
;+
; NAME:
;       U_XDR2BI
;
; PURPOSE:
;       This IDL routine converts platform-independent XDR data into
;       native binary data. 
;
;       The output filename uses the input filename suffixed with '.2bi'.
;
; CALLING SEQUENCE:
;
;       U_XDR2BI, 'filename' [,/VT] [,/Help]
;
; INPUTS:
;       filename:   The data file should contain XDR binary data objects.
;
; OUTPUTS:
;       filename.2bi:   Output file. 
;                   It contains the converted native binary data objects.
;
; KEYWORD PARAMETERS:
;       VT:     If a dumb terminal without X window server is used, 
;               this option must be set, e.g. a telnet session.
;       HELP:   If this keyword is set, a simple on line help is given.
;
; RESTRICTIONS:
;       The XDR input file should be created by the u_write command.
;
; EXAMPLE:
;
;        U_XDR2BI,'catch1d.trashcan.xdr'
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 08-10-98.
;
;       xx-xx-xx      iii  comment     
;-
;

if n_elements(filename) eq 0 or keyword_set(help) then goto,help1

	found = findfile(filename)
	if found(0) eq '' then begin
		print,'Error: '+filename+' not found!'
		return
		end
	if keyword_set(VT) then $
	OK_WRITE = u_writePermitted(filename+'.2bi',/VT) else $
	OK_WRITE = u_writePermitted(filename+'.2bi')
	if OK_WRITE lt 0 then return

        id=0
        u_openr,unit,filename,/XDR
        u_openw,unit2,filename+'.2bi'

        WHILE NOT  EOF(unit) DO BEGIN
        id = id + 1
        u_read,unit,x
	u_write,unit2,x
        END
        maxno = id
        u_close,unit
        u_close,unit2
	if keyword_set(VT) then $
	print,string(maxno)+' sets of binary objects saved in "'+ filename+'.2bi"' else $
        ret=WIDGET_MESSAGE(string(maxno)+' sets of binary objects saved in "'+ $
		filename+'.2bi"')

	return

help1:

	print,''
	print,'Usage: U_XDR2BI,"filename"'
	print,''
	print,'This program converts the xdr data objects into native binary format.'
	print,'A new file "filename.2bi" will be created.'
	print,''
END
;
; this routine is requied for generate the runtime executable
;
PRO os_init
;+
; NAME:
;	OS_INIT
;
; PURPOSE:
;       Defines the structure of main operating system variables. All the 
;       operating system dependent varibles used in data catcher are 
;       assembled in this routine. 
;
; CATEGORY:
;       Global System Variables !os.
;
; CALLING SEQUENCE:
;       OS_INIT
;
; PARAMETER FIELDS:
;       ARCH:           IDL detected operating system architecture
;       OS:             IDL detected operating system 
;       OS_FAMILY:      IDL detected operating system family
;       RELEASE:        IDL release number
;       FONT:           Bold font used in highlight label in dialog
;       DEVICE:         Default output device
;       FILE_SEP:       Operating sytem file separator, '/' for unix '\' for W95
;       CHMOD:          Command change file permission mode, 'chmod'
;       MV:             Command rename file, 'mv' for unix, 'rename' for W95
;       CP:             Command copy file, 'cp' for unix, 'copy' for W95
;       RM:             Command remove file, 'rm' for unix, 'del' for W95
;       LPR:            Command print PS file, 'lpr' for unix, 'print' for W95
;       PRT:            Command print text file, 'enscript' for unix, 'print' for W95
;       PRINTER:        Default printer name, '' 
;       WC:             Command return line count, 'wc' for unix
;
; COMMON BLOCKS:
;       COMMON SYSTEM_BLOCK
;
; SIDE EFFECTS:
;       This routine defines the OS_SYSTEM structure and the global 
;       system variable !os. All the system dependent varialbes used in
;       data catcher and data viewer are kept in this routine.
;
; RESTRICTIONS:
;       Current version works for Unix and W95 operating system. 
;
; PROCEDURE:
;       Porting to other operating system, the corresponding
;       fields in 'os.init' may need to be modified accordingly.
;
; EXAMPLE:
;
;               OS_INIT
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 6-01-97.
;       xx-xx-xx iii  - comment
;-
@os.init
END

;
; PS_open,'name.ps'               name defalut to idl
;
PRO PS_init
COMMON PRINTER_BLOCK,printer_info
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

if n_elements (printer_info) eq 0 then $
  printer_info = { $
	name: '', $
	color: 1, $
	reverse: 0, $
	base: 0L, $
	ptr_field:0L }
; inherit from the parent process
;if n_elements(r_curr) eq 0 then begin
;	LOADCT,39  
;	end
END

PRO PS_open,psfile,TV=TV
;+
; NAME:
;       PS_OPEN
;
; PURPOSE:
;       This routine sets the current graphics device to PostScript.
;       and saves plot in a user specified PostScript file.
;
; CALLING SEQUENCE:
;       PS_OPEN, 'myfile.ps' [,/TV]
;
; INPUTS:
;       myfile.ps:  Specifies the PostScript filename to be saved.
;
; OUTPUTS:
;       The PostScript graphic output is saved which can be sent to
;       any PostScript printer or viewer. 
;
; KEYWORD PARAMETERS:
;       TV:       Specifies whether reverse color video to be used in PS. 
;
; COMMON BLOCKS:
;       COMMON PRINTER_BLOCK
;       COMMON COLORS 
;
; RESTRICTIONS:
;       The program 'PS_open.pro' must be loaded into IDL first before 
;       calling this routine.
;
; EXAMPLE:
;
;        PS_OPEN, 'myfile.ps'
;        tvscl,scan
;        PS_CLOSE
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       07-28-97   bkc  Add the support for color PostScript
;                       Add the support for reverse video
;                       Add handling capability for different operating system
;       05-15-98   bkc  Change the reverse video to reverse legend color for
;                       2D TV plot, to get reverse video use the xloadct's
;                       option, reverse feature  
;-

COMMON PRINTER_BLOCK,printer_info
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

	PS_init
	set_plot,'PS'
	!P.FONT=0
	if (n_elements(psfile) ne 0) then begin

	if keyword_set(TV) then begin 

	; use xloadct reverse video, reverse legend only  


	    if printer_info.color gt 0 then $
		device,filename=psfile,/color,bits=8, $
			/Courier,/Bold, $
			 yoffset=7, xsize=15, ysize=15  else  $
		device,filename=psfile,/Courier,/Bold
	endif else begin
	    if printer_info.color gt 0 then $
		device,filename=psfile,/color,bits=8, $
			/Courier,/Bold, $
			yoffset=7, xsize=17.78, ysize=12.7  else $
		device,filename=psfile,/Courier,/Bold
	end

	end
END

PRO PS_close
;+
; NAME:
;       PS_CLOSE
;
; PURPOSE:
;       This routine closes the PostScript output device and resets the
;       the original system graphic device as the output plot device.
;
; CALLING SEQUENCE:
;       PS_CLOSE
;
; INPUTS:
;       None.
;
; OUTPUTS:
;       None.
;
; KEYWORD PARAMETERS:
;       None.
;
; COMMON BLOCKS:
;       COMMON SYSTEM_BLOCK
;       COMMON COLORS 
;
; RESTRICTIONS:
;       The program 'PS_open.pro' must be loaded into IDL prior calling
;       this routine. 
;
; EXAMPLE:
;
;        PS_OPEN, 'myfile.ps'
;        tvscl,scan
;        PS_CLOSE
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       07-28-97   bkc  Add the support for reverse PostScript color scheme. 
;                       Add handling capability for different operating system
;-

COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

	if !d.name eq 'PS' then begin
	!P.FONT=-1
	device,/close

	r_curr = r_orig
	g_curr = g_orig
	b_curr = b_orig
	TVLCT,r_orig,g_orig,b_orig

	set_plot,OS_SYSTEM.device
	end
END

PRO PS_enscript,fileName
;+
; NAME:
;       PS_ENSCRIPT
;
; PURPOSE:
;       This routine uses the system printing command to print
;       an ASCII text file. On unix operating system the command
;       'enscript -r' is used. 
;
; CALLING SEQUENCE:
;       PS_ENSCRIPT, 'filename'
;
; INPUTS:
;       filename : Specifies the ASCII filename to be printed.
;
; OUTPUTS:
;       A copy of the specified file is sent to the user selected  
;       printer.
;
; KEYWORD PARAMETERS:
;
; COMMON BLOCKS:
;       COMMON SYSTEM_BLOCK
;
; RESTRICTIONS:
;       The program 'PS_open.pro' must be loaded into IDL prior calling
;       this routine. 
;
; EXAMPLE:
;
;        PS_ENSCRIPT, 'myfile'
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       07-28-97   bkc  Add handling capability for different operating system
;-
COMMON SYSTEM_BLOCK,OS_SYSTEM

	if n_elements(fileName) eq 0 then begin
		print,'Usage: PS_enscript, <fileName>'
		return
	end
	if strtrim(fileName,2) eq '' then begin
		print,'Usage: PS_enscript, <fileName>'
		return
	end
	if OS_SYSTEM.os_family eq 'unix' then $
	spawn,[OS_SYSTEM.prt, OS_SYSTEM.printer ,  '-r', fileName], /noshell else $
	spawn,[OS_SYSTEM.prt, fileName, OS_SYSTEM.printer]
END

PRO PS_print,psfile
;+
; NAME:
;       PS_PRINT
;
; PURPOSE:
;       This routine uses the system printing command to print
;       a PostScript or ASCII text file. On the unix operating 
;       system the command 'lpr' is used. 
;
; CALLING SEQUENCE:
;       PS_PRINT, 'myfile.ps'
;
; INPUTS:
;       myfile:    Specifies either the PostScript or ASCII text filename 
;                  to be printed.
;
; OUTPUTS:
;       A copy of the specified file is sent to the user selected  
;       printer.
;
; KEYWORD PARAMETERS:
;
; COMMON BLOCKS:
;       COMMON SYSTEM_BLOCK
;
; RESTRICTIONS:
;       The program 'PS_open.pro' must be loaded into IDL prior calling
;       this routine. 
;
; EXAMPLE:
;
;        PS_PRINT, 'myfile.ps'
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-23-95.
;
;       07-28-97   bkc  Add handling capability for different operating system
;       05-14-98   bkc  Add the checking for unreadable color on the PS plot
;                       On unix if the color is too light use the gv to preview 
;			pops up setup printer and info dialog
;-
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON PRINTER_BLOCK,printer_info
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

	if (n_elements(psfile) ne 0) then begin 
		if strtrim(psfile,2) eq '' then begin
			print,'Usage: PS_print, <filename>'
			return
		end
	end else psfile = 'idl.ps'

	if OS_SYSTEM.os_family eq 'unix' then begin
        	str =  OS_SYSTEM.lpr + ' ' + OS_SYSTEM.printer +  psfile 
		color = r_curr(0) + g_curr(0)*256L + b_curr(0)*256L ^2
		if color ge 16777200 then begin 
			temp = ['Warning:','',$
			 'There may be problem of unreadable title or legend on PS plot.',$
			'The ghostview is brought up for you to preview the PS plot.', $
			'If the PS plot looks fine you may use the ghostview to send the',$
			'print job and then close the ghostview and Printer Setup Dialog.',$
			'','If you can not see the title and legend, please close', $
			'the ghostview program first, try different color table or set the ',$
			'Reverse_Legend_Color to "Y" in Printer Setup Dialog first then', $
			'try Print again'] 
			res=dialog_message(temp,/info,title='PS legend problem')
			PS_printer
			spawn,'gv '+psfile + ' &'
		endif else spawn,str
	endif else begin
		str = OS_SYSTEM.lpr + ' ' + psfile + ' ' + OS_SYSTEM.printer
	        spawn,str
	end
	print,str
END


PRO PS_printer_Event, Event
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON PRINTER_BLOCK,printer_info

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'PS_REVERSE': BEGIN
	printer_info.reverse = Event.Index
      END

  'BGROUP3': BEGIN
      CASE Event.Value OF
      0: begin
		Print,'Button B/W Pressed'
		printer_info.color = 0
	 end
      1: begin
		Print,'Button Color Pressed'
		printer_info.color = 1
	 end
      ELSE: Message,'Unknown button pressed'
      ENDCASE
      END

  'FIELD5': BEGIN
      	WIDGET_CONTROL, printer_info.ptr_field, GET_VALUE=str
	printer_info.name = strtrim(str(0),2)
      END

  'BGROUP7': BEGIN
      CASE Event.Value OF
      0: begin
      		WIDGET_CONTROL, printer_info.ptr_field, GET_VALUE=str
		printer_info.name = strtrim(str(0),2)
      		WIDGET_CONTROL, printer_info.base, /DESTROY, BAD_ID=bad
	 end
      1: begin
      		WIDGET_CONTROL, printer_info.base, /DESTROY, BAD_ID=bad
	 end
      ELSE: Message,'Unknown button pressed'
      ENDCASE
      END
  ENDCASE

if printer_info.name ne '' then begin
	if OS_SYSTEM.os_family eq 'unix' then $
	OS_SYSTEM.printer = '-P'+printer_info.name + ' ' else $
	OS_SYSTEM.printer = printer_info.name 
end

END



PRO PS_printer, GROUP=Group
;+
; NAME:
;	PS_PRINTER
;
; PURPOSE:
;       This widget dialog allows the user to set up PostScript printer
;       and printer name to be used by the IDL session. Default setting
;       is color PS using default printer.
;
; CATEGORY:
;       Widgets.
;
; CALLING SEQUENCE:
;       PS_PRINTER [,GROUP=Group]
;
; INPUTS:
;       None.
;	
; KEYWORD PARAMETERS:
;       GROUP:  The widget ID of the group leader of the widget. If this 
;               keyword is specified, the death of the group leader results in
;               the death of PS_PRINTER.
;
; OUTPUTS:
;
; COMMON BLOCKS:
;       COMMON PRINTER_BLOCK
;
; SIDE EFFECTS:
;       Initially the system printer is set to the user's default 
;       printer. If a null printer name is specified, whatever the 
;       system printer was previously set will be used. 
;
; RESTRICTIONS:
;       The program 'PS_open.pro' must be loaded into IDL prior calling
;       this routine. 
;       
; PROCEDURE:
;      
; EXAMPLE:
;
;               PS_PRINTER
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 6-01-97.
;       10-15-97 bkc  - Add droplist Y/N for reverse color option
;                       Now it defaults to non reverse color option.
;       05-14-98 bkc  - Remove the B/W option, use the xloadct to select B/W
;                       Change reverse video to reverse legeng color if legend
;                       is in white color 
;-

COMMON PRINTER_BLOCK,printer_info

if XRegistered('PS_printer') then return

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  PS_init

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  PS_printer_base = WIDGET_BASE(GROUP_LEADER=Group, $
      TITLE='Setup Printer', $
      ROW=1, $
      MAP=1, $
      UVALUE='PS_PRINTER')
  printer_info.base = PS_printer_base 

  BASE2 = WIDGET_BASE(PS_printer_base, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  LABEL3 = WIDGET_LABEL( BASE2, $
;	FONT=!os.font, $
      UVALUE='LABEL3', $
      VALUE='Setup PS Printer')

;  Btns167 = [ $
;    'B/W', $
;    'Color' ]
;  BGROUP3 = CW_BGROUP( BASE2, Btns167, $
;      ROW=1, $
;      EXCLUSIVE=1, $
;      LABEL_LEFT='Output PS', $
;      UVALUE='BGROUP3')
;  WIDGET_CONTROL,BGROUP3,SET_VALUE= printer_info.color

  Btn168 = ['N','Y']
  ps_reverse = WIDGET_DROPLIST(BASE2, VALUE=Btn168, $
        UVALUE='PS_REVERSE',TITLE='Reverse Legend Color')
  WIDGET_CONTROL,ps_reverse,SET_DROPLIST_SELECT=printer_info.reverse

  FieldVal269 = [ $
    '' ]
  FIELD5 = CW_FIELD( BASE2,VALUE=FieldVal269, $
      ROW=1, RETURN_EVENTS=1, $
      STRING=1, $
      TITLE='Printer Name', $
      UVALUE='FIELD5', $
      XSIZE=10)
  printer_info.ptr_field = FIELD5  
  if strtrim(printer_info.name,2) ne '' then $
  WIDGET_CONTROL,FIELD5,SET_VALUE=printer_info.name

  Btns342 = [ 'Accept', 'Cancel' ] 
  BGROUP7 = CW_BGROUP( BASE2, Btns342, $
      ROW=1, $
      UVALUE='BGROUP7')

  WIDGET_CONTROL, PS_printer_base, /REALIZE

  XMANAGER, 'PS_printer', PS_printer_base
END
; $Id: DC.pro,v 1.1 1998/12/22 19:54:52 cha Exp $

pro my_box_cursor, x0, y0, nx, ny, INIT = init, FIXED_SIZE = fixed_size, $
	MESSAGE = message
;+
; NAME:
;	BOX_CURSOR
;
; PURPOSE:
;	Emulate the operation of a variable-sized box cursor (also known as
;	a "marquee" selector).
;
; CATEGORY:
;	Interactive graphics.
;
; CALLING SEQUENCE:
;	BOX_CURSOR, x0, y0, nx, ny [, INIT = init] [, FIXED_SIZE = fixed_size]
;
; INPUTS:
;	No required input parameters.
;
; OPTIONAL INPUT PARAMETERS:
;	x0, y0, nx, and ny give the initial location (x0, y0) and 
;	size (nx, ny) of the box if the keyword INIT is set.  Otherwise, the 
;	box is initially drawn in the center of the screen.
;
; KEYWORD PARAMETERS:
;	INIT:  If this keyword is set, x0, y0, nx, and ny contain the initial
;	parameters for the box.
;
;	FIXED_SIZE:  If this keyword is set, nx and ny contain the initial
;	size of the box.  This size may not be changed by the user.
;
;	MESSAGE:  If this keyword is set, print a short message describing
;	operation of the cursor.
;
; OUTPUTS:
;	x0:  X value of lower left corner of box.
;	y0:  Y value of lower left corner of box.
;	nx:  width of box in pixels.
;	ny:  height of box in pixels. 
;
;	The box is also constrained to lie entirely within the window.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	A box is drawn in the currently active window.  It is erased
;	on exit.
;
; RESTRICTIONS:
;	Works only with window system drivers.
;
; PROCEDURE:
;	The graphics function is set to 6 for eXclusive OR.  This
;	allows the box to be drawn and erased without disturbing the
;	contents of the window.
;
;	Operation is as follows:
;	Left mouse button:   Move the box by dragging.
;	Middle mouse button: Resize the box by dragging.  The corner
;		nearest the initial mouse position is moved.
;	Right mouse button:  Exit this procedure, returning the 
;			     current box parameters.
;
; MODIFICATION HISTORY:
;	DMS, April, 1990.
;	DMS, April, 1992.  Made dragging more intutitive.
;	June, 1993 - Bill Thompson
;			prevented the box from having a negative size.
;       04-18-96   bkc  Made the box color more visible.
;       05-28-98   bkc  Reset bounding box color 
;-

device, get_graphics = old, set_graphics = 6  ;Set xor
col = !d.n_colors - 2

if keyword_set(message) then begin
	st = [$,
	"Drag Left button to move box.",$
	"Drag Middle button near a corner to resize box.",$
	"Right button when done."]
	res=WIDGET_MESSAGE(st)
	endif

if keyword_set(init) eq 0 then begin  ;Supply default values for box:
	if keyword_set(fixed_size) eq 0 then begin
		nx = !d.x_size/8   ;no fixed size.
		ny = !d.x_size/8
		endif
	x0 = !d.x_size/2 - nx/2
	y0 = !d.y_size/2 - ny/2
	endif

button = 0
goto, middle

while 1 do begin
	old_button = button
	cursor, x, y, 2, /dev	;Wait for a button
	button = !err
	if (old_button eq 0) and (button ne 0) then begin
		mx0 = x		;For dragging, mouse locn...
		my0 = y		
		x00 = x0	;Orig start of ll corner
		y00 = y0
		endif
	if !err eq 1 then begin  ;Drag entire box?
		x0 = x00 + x - mx0
		y0 = y00 + y - my0
		endif
	if (!err eq 2) and (keyword_set(fixed_size) eq 0) then begin ;New size?
		if old_button eq 0 then begin	;Find closest corner
			mind = 1e6
			for i=0,3 do begin
				d = float(px(i)-x)^2 + float(py(i)-y)^2
				if d lt mind then begin
					mind = d
					corner = i
					endif
			   endfor
			nx0 = nx	;Save sizes.
		   	ny0 = ny
			endif
		dx = x - mx0 & dy = y - my0	;Distance dragged...
		case corner of
		0: begin x0 = x00 + dx & y0 = y00 + dy
			nx = nx0 -dx & ny = ny0 - dy & endcase
		1: begin y0 = y00 + dy
			nx = nx0 + dx & ny = ny0 - dy & endcase
		2: begin nx = nx0 + dx & ny = ny0 + dy & endcase
		3: begin x0 = x00 + dx
			nx = nx0 -  dx & ny = ny0 + dy & endcase
		endcase
		endif
	plots, px, py, col=col, /dev, thick=3, lines=0	;Erase previous box
	empty				;Decwindow bug

	if !err eq 4 then begin  ;Quitting?
		device,set_graphics = old
		return
		endif
middle:

	if nx lt 0 then begin
		x0 = x0 + nx
		nx = -nx
	endif
	if ny lt 0 then begin
		y0 = y0 + ny
		ny = -ny
	endif

	x0 = x0 > 0
	y0 = y0 > 0
	x0 = x0 < (!d.x_size-1 - nx)	;Never outside window
	y0 = y0 < (!d.y_size-1 - ny)

	px = [x0, x0 + nx, x0 + nx, x0, x0] ;X points
	py = [y0, y0, y0 + ny, y0 + ny, y0] ;Y values

	plots,px, py, col=col, /dev, thick=3, lines=0  ;Draw the box
	wait, .1		;Dont hog it all
	endwhile
end
;
; Auto Save File For xy_coord.pro
;
;  Wed Aug  2 15:51:05 CDT 1995
;


PRO xycoord, clean=clean
COMMON CATCH1D_COM, widget_ids, scanData
COMMON XY_COORD_BLOCK, xy_id, xy_wid
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

; wipe out the old value
if xy_id.plot eq 1 then begin
    if w_plotspec_id.log ne 1 then begin
	oplot,[xy_id.x0,xy_id.x0],!y.crange, line = 1, color=0
	oplot,!x.crange,[xy_id.y0,xy_id.y0], line = 1, color=0
    endif else begin    ; YLOG
	oplot,[xy_id.x0,xy_id.x0],10^!y.crange, line = 1, color=0
	oplot,!x.crange,[xy_id.y0,xy_id.y0], line = 1, color=0
    end
end
if keyword_set(clean) then return

cursor,x,y,0,/normal
x = (x - !x.window(0)) / (!x.window(1)-!x.window(0)) * $
	(!x.crange(1)-!x.crange(0)) + !x.crange(0)

y = (y - !y.window(0)) / (!y.window(1)-!y.window(0)) * $
	(!y.crange(1)-!y.crange(0)) + !y.crange(0)

if w_plotspec_id.log eq 1 then begin    ; YLOG
	y = 10^y
	oplot,[x,x],10^!y.crange, line = 1
	oplot,!x.crange,[y,y], line = 1
endif else begin
	oplot,[x,x],!y.crange, line = 1
	oplot,!x.crange,[y,y], line = 1
end

st = 'Cursor X Y value @ button 1 pressed: '+string(x) +string(y)
if scanData.debug gt 0 then print,st

xy_id.x0 = x
xy_id.y0 = y
xy_id.x1 = x
xy_id.y1 = y
xy_id.plot = 1
;xy_id.st = st

if xy_wid.base ne 0 then begin
WIDGET_CONTROL,	xy_wid.x, SET_VALUE = strtrim(xy_id.x0,2)
WIDGET_CONTROL,	xy_wid.y, SET_VALUE = strtrim(xy_id.y1,2) 
WIDGET_CONTROL,	xy_wid.motor, SET_VALUE =  'Ref Positioner # '+strtrim(w_plotspec_id.xcord+1,2)
end

END

PRO xycoord_setmotor_confirmed
COMMON GOTO_BLOCK,goto_n,goto_pv,goto_val
	pv = goto_pv(0:goto_n-1)
	val = goto_val(0,0:goto_n-1)
	id = caputArray(pv,val)
	if id ne 0 then w_warningtext,'Error: in Goto setting !',40,3 
END

PRO xycoord_setmotor,val
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus
COMMON w_warningtext_block,w_warningtext_ids
COMMON GOTO_BLOCK,goto_n,goto_pv,goto_val

   x_axis = w_plotspec_id.x_axis_u

	num_pts =  1 > (scanData.act_npts - 1)
	
		x1 = MAX(scanData.pa(0:num_pts,w_plotspec_id.xcord))
		x0 = MIN(scanData.pa(0:num_pts,w_plotspec_id.xcord))
	if x1 eq x0 then begin
		w_warningtext,'Error: Invalid request !'
		return
		end
	f1 = (val - x0) / (x1 - x0)
	if x_axis eq 1 then f1 = val / num_pts

	goto_val = make_array(1,4,/double)
	goto_pv = make_array(4,/string)
	k=0
        piname=['.P1PV','.P2PV','.P3PV','.P4PV']
	for i=0,3 do begin
		s1 = ''
		ln = cagetArray(scanData.pv+piname(i), s1, /string)
		if ln eq 0 then begin
		if strtrim(s1,2) ne '' then begin
		xmax = MAX(scanData.pa(0:num_pts,i))
		xmin = MIN(scanData.pa(0:num_pts,i))
		goto_val(0,i) = xmin + f1 * (xmax - xmin)	
		goto_pv(i) = s1
		k=k+1
		end
		end
	end
	if k lt 1 then return      ; none defined
	goto_n = k
	st = 'Set New Positions:'
	for i=0,goto_n-1 do begin
	st = [st,goto_pv(i)+ ' --> ' + string(goto_val(0,i))]	
	end
	w_warningtext,st,45,5,'Set Positioner Locations',title='GoTo ...',quest='GoTo'
	return
	
END

PRO XYCOORD_BASE_Event, Event
COMMON XY_COORD_BLOCK, xy_id, xy_wid

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'XY_COORD_FIELD4': BEGIN
	WIDGET_CONTROL,xy_wid.x,GET_VALUE=xvalue
      END
  'XY_COORD_FIELD7': BEGIN
;	WIDGET_CONTROL,xy_wid.y,GET_VALUE=yvalue
      END
  'BUTTON9': BEGIN
      Print, 'Event for GoTo'
	WIDGET_CONTROL,xy_wid.x,GET_VALUE=xvalue
	val = float(xvalue(0))
	xycoord_setmotor,val
	WIDGET_CONTROL, xy_wid.base,/DESTROY,BAD_ID=bad
      END
  'BUTTON10': BEGIN
      Print, 'Event for Close'
	WIDGET_CONTROL,Event.top,/DESTROY
	xy_wid.base = 0L
	xycoord,/CLEAN
;	UPDATE_PLOT,1
      END
  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END XYCOORD_BASE
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO xy_coord, GROUP=Group
COMMON XY_COORD_BLOCK, xy_id, xy_wid
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

if XRegistered('XYCOORD_BASE') NE 0 then return

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

if n_elements(xy_id) eq 0 then begin
xy_id = { $
        plot : 0, $
        x0 : 0., $
        x1 : 0., $
        y0 : 0., $
        y1 : 0., $
        st : '' $
        }
        end

  XYCOORD_BASE = WIDGET_BASE(GROUP_LEADER=Group, $
	/COLUMN, MAP=1, $
      TITLE='XY-COORD', $
      UVALUE='XYCOORD_BASE')

  XY_COORD_MOTOR = WIDGET_LABEL(XYCOORD_BASE, $
	VALUE='Ref Positioner # '+strtrim(w_plotspec_id.xcord+1,2))

; x value 
  BASE3 = WIDGET_BASE(XYCOORD_BASE, ROW=1, MAP=1, UVALUE='BASE3')

  label_p1 = WIDGET_LABEL(BASE3,VALUE='X: ')
  XY_COORD_FIELD4 = WIDGET_TEXT( BASE3,VALUE='', $
      EDITABLE=1, UVALUE='XY_COORD_FIELD4', XSIZE=20)

; y value 
  BASE4 = WIDGET_BASE(XYCOORD_BASE, ROW=1, MAP=1, UVALUE='BASE4')
  label_d1 = WIDGET_LABEL(BASE4,VALUE='Y: ')
  XY_COORD_FIELD7 = WIDGET_TEXT( BASE4,VALUE='', $
      EDITABLE=1, UVALUE='XY_COORD_FIELD7', XSIZE=20)

; close button
  BASE5 = WIDGET_BASE(XYCOORD_BASE, ROW=1, MAP=1, UVALUE='BASE5')
  BUTTON9 = WIDGET_BUTTON( BASE5, $
      UVALUE='BUTTON9', $
      VALUE='GoTo ...')
  BUTTON10 = WIDGET_BUTTON( BASE5, $
      UVALUE='BUTTON10', $
      VALUE='Close')


xy_wid = { $
	base : XYCOORD_BASE, $ 
	motor : XY_COORD_MOTOR, $
	x : XY_COORD_FIELD4, $
	y : XY_COORD_FIELD7 $
	}

DEVICE,GET_SCREEN_SIZE=ssize

  WIDGET_CONTROL, XYCOORD_BASE, /REALIZE, $
	TLB_SET_XOFFSET= ssize(0)-200, TLB_SET_YOFFSET= 100

  XMANAGER, 'XYCOORD_BASE', XYCOORD_BASE
END

; $Id: DC.pro,v 1.1 1998/12/22 19:54:52 cha Exp $

; Copyright (c) 1991-1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
PRO XDispFile_evt, event


WIDGET_CONTROL, event.top, GET_UVALUE = state
WIDGET_CONTROL, event.id, GET_UVALUE=Ev

CASE Ev OF 
'EXIT': WIDGET_CONTROL, event.top, /DESTROY
'FILE_PRINT': begin
	WIDGET_CONTROL,state.text_area,GET_VALUE=str
	openw,unit,'tmp',/GET_LUN
	for i=0,n_elements(str)-1 do printf,unit,str(i)
	FREE_LUN,unit
	PS_print,'tmp'
	end
ENDCASE
END


PRO XDisplayFile, FILENAME, TITLE = TITLE, GROUP = GROUP, WIDTH = WIDTH, $
		HEIGHT = HEIGHT, TEXT = TEXT, FONT = font
;+
; NAME: 
;	XDISPLAYFILE
;
; PURPOSE:
;	Display an ASCII text file using widgets and the widget manager.
;
; CATEGORY:
;	Widgets.
;
; CALLING SEQUENCE:
;	XDISPLAYFILE, Filename
;
; INPUTS:
;     Filename:	A scalar string that contains the filename of the file
;		to display.  The filename can include a path to that file.
;
; KEYWORD PARAMETERS:
;	FONT:   The name of the font to use.  If omitted use the default
;		font.
;	GROUP:	The widget ID of the group leader of the widget.  If this 
;		keyword is specified, the death of the group leader results in
;		the death of XDISPLAYFILE.
;
;	HEIGHT:	The number of text lines that the widget should display at one
;		time.  If this keyword is not specified, 24 lines is the 
;		default.
;
;	TEXT:	A string or string array to be displayed in the widget
;		instead of the contents of a file.  This keyword supercedes
;		the FILENAME input parameter.
;
;	TITLE:	A string to use as the widget title rather than the file name 
;		or "XDisplayFile".
;
;	WIDTH:	The number of characters wide the widget should be.  If this
;		keyword is not specified, 80 characters is the default.
;
; OUTPUTS:
;	No explicit outputs.  A file viewing widget is created.
;
; SIDE EFFECTS:
;	Triggers the XMANAGER if it is not already in use.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	Open a file and create a widget to display its contents.
;
; MODIFICATION HISTORY:
;	Written By Steve Richards, December 1990
;	Graceful error recovery, DMS, Feb, 1992.
;       12 Jan. 1994  - KDB
;               If file was empty, program would crash. Fixed.
;       4 Oct. 1994     MLR Fixed bug if /TEXT was present and /TITLE was not.
;      14 Jul. 1995     BKC Increased the max line to variable size.
;      16 Jun. 1997     BKC Max dispalyable line is 10000 for non-unix OS system.
;      28 Aug. 1997     BKC Add the printer button, file name label, it uses the
;                       PS_print,file to print.
;-
COMMON SYSTEM_BLOCK,OS_SYSTEM
                                                        ;use the defaults if
IF(NOT(KEYWORD_SET(HEIGHT))) THEN HEIGHT = 24		;the keywords were not
IF(NOT(KEYWORD_SET(WIDTH))) THEN WIDTH = 80		;passed in

IF(NOT(KEYWORD_SET(TEXT))) THEN BEGIN
  IF(NOT(KEYWORD_SET(TITLE))) THEN TITLE = FILENAME     
  OPENR, unit, FILENAME, /GET_LUN, ERROR=i		;open the file and then
  if i lt 0 then begin		;OK?
	a = [ !err_string, ' Can not display ' + filename]  ;No
  endif else begin

    y=10000
    if OS_SYSTEM.os_family eq 'unix' then begin
	spawn,[OS_SYSTEM.wc,'-l',FILENAME],y,/noshell

	lines=long(y(0))
	if lines eq 0 then begin
	res=WIDGET_MESSAGE('Unable to display '+FILENAME)
	return
	end
    end

	  a = strarr(y(0))				;Maximum # of lines
	  i = 0L
	  c = ''
	  while not eof(unit) do begin
		readf,unit,c
		a(i) = c
		i = i + 1
		if i ge y(0) then goto,stopread
		endwhile
	  stopread:
	  a = a(0:(i-1)>0)  ;Added empty file check -KDB
	  FREE_LUN, unit				;free the file unit.
  endelse
ENDIF ELSE BEGIN
    IF(NOT(KEYWORD_SET(TITLE))) THEN TITLE = 'XDisplayFile'
    a = TEXT
ENDELSE

filebase = WIDGET_BASE(TITLE = TITLE, $			;create the base
		/COLUMN ) 

label=WIDGET_LABEL(filebase,value=TITLE)
rowbtn = WIDGET_BASE(filebase,/ROW,TITLE='ROWBTN')
fileprint = WIDGET_BUTTON(rowbtn, $			;create a Print Button
		VALUE = "Print", $
		UVALUE = "FILE_PRINT")

filequit = WIDGET_BUTTON(rowbtn, $			;create a Done Button
		VALUE = "Done", $
		UVALUE = "EXIT")

IF n_elements(font) gt 0 then $
 filetext = WIDGET_TEXT(filebase, $			;create a text widget
		XSIZE = WIDTH, $			;to display the file's
		YSIZE = HEIGHT, $			;contents
		/SCROLL, FONT = font, $
		VALUE = a) $
ELSE filetext = WIDGET_TEXT(filebase, $			;create a text widget
		XSIZE = WIDTH, $			;to display the file's
		YSIZE = HEIGHT, $			;contents
		/SCROLL, $
		VALUE = a)

state = { $
	 base: filebase, $
	 text_area: filetext, $
	 file: filename $
	 }
WIDGET_CONTROL,filebase,SET_UVALUE=state

WIDGET_CONTROL, filebase, /REALIZE			;instantiate the widget

Xmanager, "XDisplayFile", $				;register it with the
		filebase, $				;widget manager
		GROUP_LEADER = GROUP, $
		EVENT_HANDLER = "XDispFile_evt" 

END  ;--------------------- procedure XDisplayFile ----------------------------

;+
; NAME:
;	cw_term
;
; PURPOSE:
;      writtable text window widget
;
; CATEGORY:
;	Compound widgets.
;
; CALLING SEQUENCE:
;	widget_id = CW_TERM(parent)
;
; INPUTS:
;       PARENT - The ID of the parent widget.
;
; KEYWORD PARAMETERS:
;	BG_NAMES:	An array of strings to be associated with
;			each button and returned in the event structure as VALUE.
;	BGEVENT_FUNCT:	The name of an user-supplied event function 
;			for the buttons. This function is called with the return
;			value structure whenever a button is pressed, and 
;			follows the conventions for user-written event
;			functions.
;	FONT:		The name of the font to be used for the text output 
;			If this keyword is not specified, the default
;			font is used.
;	FRAME:		Specifies the width of the frame to be drawn around
;			the base.
;       FILENAME:       Copy contents of file into widget
;       RESET:          Clear existing widget contents and write new value/file.
;                       The parent widget is the existing widget id. 
;	MAP:		If set, the base will be mapped when the widget
;			is realized (the default).
;	SCROLL:		If set, the base will include scroll bars to allow
;			viewing a large text area through a smaller viewport.
;	SET_VALUE:	The initial value of the text widget. This is equivalent
;			to the later statement:
;
;			WIDGET_CONTROL, widget, set_value=value
;
;       TITLE:          New Window title
;	UVALUE:         The user value for the compound widget
;
;	XSIZE:		The width of the text widget
;	YSIZE:		The height of the text widget
;
; OUTPUTS:
;       The ID of the created widget is returned.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;
; PROCEDURE:
;	WIDGET_CONTROL, id, SET_VALUE=value can be used to change the
;		current value displayed by the widget.
;
;	WIDGET_CONTROL, id, GET_VALUE=var can be used to obtain the current
;		value displayed by the widget.
;
; MODIFICATION HISTORY:
;  01  8-9-95  jps  	modified from idl's cw_tmpl.pro
;-



PRO cwterm_Save_Event, Event
COMMON SYSTEM_BLOCK,OS_SYSTEM

  WIDGET_CONTROL,Event.Top,GET_UVALUE=info
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'CWTERM_SAVEFILE': BEGIN
      END
  'CWTERM_SAVEACCEPT': BEGIN
	WIDGET_CONTROL,info.newname, GET_VALUE=filename
	if strtrim(filename(0),2) ne '' then begin
	found = findfile(filename(0))
	if found(0) ne '' then begin
		WIDGET_CONTROL,info.base,/DESTROY
		st = [ 'File: '+filename(0),' already existed!', $
			'ASCII data saved in ',info.oldname]
		res = widget_message(st,/info)
		return
	end
	spawn,[OS_SYSTEM.cp, info.oldname, filename(0)],/noshell
	WIDGET_CONTROL,info.base,/DESTROY
;	res=widget_message('File: "'+filename(0)+'" saved',/info)
	end
      END
  'CWTERM_SAVECANCEL': BEGIN
	WIDGET_CONTROL,info.base,/DESTROY
      END
  ENDCASE
END

;
; if filename specifies the default file name used by the cw_term, 
;     it will be override by the textfield entered by the user
;
PRO cwterm_save_dialog, GROUP=Group,oldname=oldname, rename=rename

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  cwterm_Save = WIDGET_BASE(GROUP_LEADER=Group, $
	TITLE='CW_TERM Save File', $
      ROW=1, $
      MAP=1, $
      UVALUE='cwterm_Save')

  BASE2 = WIDGET_BASE(cwterm_Save, $
      COLUMN=1, TITLE='CW_TERM SaveFile', $
      MAP=1, $
      UVALUE='BASE2')

  FieldVal288 = [ $
    '' ]
  if n_elements(rename) then FieldVal288 = strtrim(rename,2)
  FIELD3 = CW_FIELD( BASE2,VALUE=FieldVal288, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='File:', $
      UVALUE='CWTERM_SAVEFILE', $
      XSIZE=60)

  BASE4 = WIDGET_BASE(BASE2, $
      COLUMN=2, $
      MAP=1, $
      UVALUE='BASE4')

  CWTERM_SAVE_BUTTON5 = WIDGET_BUTTON( BASE4, $
      UVALUE='CWTERM_SAVEACCEPT', $
      VALUE='Accept')

  CWTERM_SAVE_BUTTON6 = WIDGET_BUTTON( BASE4, $
      UVALUE='CWTERM_SAVECANCEL', $
      VALUE='Cancel')

  info = {  $
	base : cwterm_Save, $
	oldname: oldname, $
	newname: FIELD3 $
	}

  WIDGET_CONTROL, cwterm_Save, SET_UVALUE=info
  WIDGET_CONTROL, cwterm_Save, /REALIZE

  XMANAGER, 'cwterm_Save', cwterm_Save
END

PRO term_set_value, id, value

	; This routine is used by WIDGET_CONTROL to set the value for
	; your compound widget.  It accepts one variable.  
	; You can organize the variable as you would like.  If you have
	; more than one setting, you may want to use a structure that
	; the user would need to build and then pass in using 
	; WIDGET_CONTROL, compoundid, SET_VALUE = structure.

	; Return to caller.
  ON_ERROR, 2

	; Retrieve the state.
   stash = WIDGET_INFO(id, /CHILD)
   WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY, BAD_ID=bad_id

    IF (N_ELEMENTS(value) NE 0) THEN BEGIN
	   WIDGET_CONTROL, state.text_id, $
				SET_VALUE=value, $
				/APPEND, $
				BAD_ID=bad_id, $
				/NO_COPY
    ENDIF
   
   WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY, BAD_ID=bad_id

END



FUNCTION term_get_value, id, value

	; This routine is by WIDGET_CONTROL to get the value from 
	; your compound widget.  As with the set_value equivalent,
	; you can only pass one value here so you may need to load
	; the value by using a structure or array.

	; Return to caller.
  ON_ERROR, 2

	; Retrieve the structure from the child that contains the sub ids.
  stash = WIDGET_INFO(id, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY, BAD_ID=bad_id

	; Get the value here
  WIDGET_CONTROL, state.text_id, GET_VALUE=ret, BAD_ID=bad_id

  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY, BAD_ID=bad_id
  
        ; Return the value here.
  RETURN,ret
END

;-----------------------------------------------------------------------------

FUNCTION term_event, event
COMMON SYSTEM_BLOCK,OS_SYSTEM

  parent=event.handler

  WIDGET_CONTROL, event.id, GET_UVALUE=Ev

		; Retrieve the structure from the child that contains the sub ids.
  stash = WIDGET_INFO(parent, /CHILD)
  WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY,  BAD_ID=bad_id
  fileName = state.win_file

  CASE Ev OF 

  'MAIN': BEGIN
      END
  'BGROUP':BEGIN
	CASE event.value OF
	  'Save...': BEGIN
		if XRegistered('cwterm_Save') eq 0 then $
		cwterm_save_dialog,GROUP=Event.id, $
			rename=state.rename,oldname=fileName
	      END
	  'Close': BEGIN
	      WIDGET_CONTROL, parent, DESTROY=1, BAD_ID=bad_id
	      END
	  'Clear': BEGIN
		  WIDGET_CONTROL, state.text_id, SET_VALUE='', BAD_ID=bad_id
	      END
	  'Print': BEGIN
		  ANS = WIDGET_MESSAGE('Are you sure ?',/QUESTION, $
			/DEFAULT_NO, DIALOG_PARENT=Event.top)
		  IF ANS EQ 'Yes' THEN BEGIN
		  WIDGET_CONTROL, state.text_id, GET_VALUE=value, BAD_ID=bad_id
			; open the scratch file for printing
		  fileName = state.win_file
		  OPENW, unit, fileName, /GET_LUN, ERROR=error	;
	    	  IF error LT 0 THEN BEGIN		;OK?
		     print, [ !err_string, ' Can not display ' + filename]  ;No
		  ENDIF ELSE BEGIN	
		     printf,unit, FORMAT='(A)',value
	     	     FREE_LUN, unit			;free the file unit.
		     if OS_SYSTEM.os_family eq 'unix' then begin
		     spawn,[OS_SYSTEM.prt, OS_SYSTEM.printer, '-r', fileName], /noshell
		     spawn,[OS_SYSTEM.rm, '-f', fileName], /noshell
		     endif else begin
		     spawn,[OS_SYSTEM.prt, fileName]
		     spawn,[OS_SYSTEM.rm, fileName]
		     end
		  ENDELSE
		  END
	      END
	   ELSE: 
	ENDCASE
      END
  'TEXT': BEGIN
      End
  ENDCASE

  WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY,  BAD_ID=bad_id

  RETURN, { ID:parent, TOP:event.top, HANDLER:0L }
END

;-----------------------------------------------------------------------------

FUNCTION cw_term, parent, SET_VALUE=value, $
	COLUMN=column, TITLE=title, $
	FILENAME=filename, $
	RENAME = rename, $
        RESET=reset, $
	BG_NAMES = bg_names, BGEVENT_FUNCT = bg_efun, $
	FONT=font, FRAME=frame, $
	MAP=map, SENSITIVE=sense, $
	ROW=row, SCROLL=scroll, SPACE=space, UVALUE=uvalue, $
	XSIZE=xsize, YSIZE=ysize

COMMON SYSTEM_BLOCK,OS_SYSTEM

  IF (N_PARAMS() LT 1) THEN MESSAGE, 'Must specify a parent for cw_term.'

  ON_ERROR, 2					;return to caller

	; Defaults for keywords
  version = WIDGET_INFO(/version)
  if (version.toolkit eq 'OLIT') then def_space_pad = 4 else def_space_pad = 3
  IF NOT (KEYWORD_SET(append))  THEN append = 0
  IF NOT (KEYWORD_SET(xsize)) THEN xsize = 80
  IF NOT (KEYWORD_SET(ysize)) THEN ysize = 24
  IF NOT (KEYWORD_SET(reset)) THEN reset = 0

;  IF (N_ELEMENTS(value) eq 0) 	then value = ''
  IF (N_ELEMENTS(Title) eq 0) 	 	then Title = ''
  IF (N_ELEMENTS(column) eq 0) 		then column = 0
  IF (N_ELEMENTS(frame) eq 0)		then frame = 0
  IF (N_ELEMENTS(map) eq 0)		then map=1
  IF (N_ELEMENTS(row) eq 0)		then row = 0
  IF (N_ELEMENTS(scroll) eq 0)		then scroll = 0
  IF (N_ELEMENTS(sense) eq 0)		then sense = 1
  IF (N_ELEMENTS(uvalue) eq 0)		then uvalue = 0



; File read section copied from XDISPLAYFILE utility
;	Written By Steve Richards, December 1990
;	Graceful error recovery, DMS, Feb, 1992.
;       12 Jan. 1994  - KDB
;               If file was empty, program would crash. Fixed.
;       4 Oct. 1994     MLR Fixed bug if /TEXT was present and /TITLE was not.
;      14 Jul. 1995     BKC Increased the max line to variable size.
;      16 Jun. 1997     BKC Max line set to 10000, os system check.
;      18 Dec. 1997     BKC add the save file event.

  IF(KEYWORD_SET(filename)) THEN BEGIN

    IF(NOT(KEYWORD_SET(TITLE))) THEN TITLE = filename     
    OPENR, unit, filename, /GET_LUN, ERROR=i		;open the file and then
    IF i LT 0 THEN BEGIN		;OK?
	text = [ !err_string, ' Can not display ' + filename]  ;No
    ENDIF ELSE BEGIN

    y=10000
    if OS_SYSTEM.os_family eq 'unix' then  spawn,[OS_SYSTEM.wc,'-l',FILENAME],y,/noshell

	text = strarr(y(0))				;Maximum # of lines
	i = 0L
	c = ''
	WHILE not eof(unit) do BEGIN
		READF,unit,c
		text(i) = c
		i = i + 1
		if i ge y(0) then goto,stopread
	ENDWHILE
    stopread:
	value = text(0:(i-1)>0)  ;Added empty file check -KDB
	FREE_LUN, unit			;free the file unit.
    ENDELSE
  ENDIF ELSE BEGIN
    IF(NOT(KEYWORD_SET(TITLE))) THEN TITLE = 'Term'
  ENDELSE

  winFile = ''
  if n_elements(filename) then winFile=filename
  winTitle = title
 
 IF reset EQ 0 THEN BEGIN

  if n_elements(rename) then $
	  state = { main_id:0L, group_leader:0L, $
			rename : rename, $
		    bgroup_id:0L, text_id:0L, win_file:winFile } else $
	  state = { main_id:0L, group_leader:0L, $
		    bgroup_id:0L, text_id:0L, win_file:winFile }

	  MAIN = WIDGET_BASE( $
			    GROUP_LEADER=parent, $
			    UVALUE = uvalue, $
			    TITLE=winTitle, $
			    MAP=map, $
			    EVENT_FUNC = "term_event", $
			    FUNC_GET_VALUE = "term_get_value", $
			    PRO_SET_VALUE = "term_set_value", $
			    /COLUMN)
		
	  state.main_id = MAIN
	  state.group_leader = parent

	  ; Create text widget
	  IF (N_ELEMENTS(font) EQ 0) THEN BEGIN
	      state.text_id = WIDGET_TEXT( MAIN, $
	      XSIZE=xsize, $
	      YSIZE=ysize, $
	      /NO_COPY, $
	      SCROLL=scroll)
	  ENDIF ELSE BEGIN
	      state.text_id = WIDGET_TEXT( MAIN, $
	      XSIZE=xsize, $
	      YSIZE=ysize, $
	      /NO_COPY, $
	      SCROLL=scroll, $
	      FONT=font)
	  ENDELSE


	  IF (N_ELEMENTS(value) NE 0) THEN $
		WIDGET_CONTROL, state.text_id, SET_VALUE=value

	  ; Standard control buttons
	  N_BUTTONS = 3
	  buttons = STRARR(N_BUTTONS+N_ELEMENTS(bg_names))
	  buttons(0:N_BUTTONS-1) = ['Print','Clear','Close']

	
	  ; User control buttons
	  IF N_ELEMENTS(bg_names) NE 0 THEN BEGIN
 	   buttons(N_BUTTONS:N_BUTTONS+N_ELEMENTS(bg_names)-1) = bg_names(*)
	  ENDIF

	  ; Create control buttons
	  state.bgroup_id = CW_BGROUP( MAIN, buttons, $
				      /ROW, $
				      /RETURN_NAME, $
				      EVENT_FUNCT=bg_efun, $
				      FRAME=frame, $
				      UVALUE='BGROUP')

	  ; Save out the initial state structure into the first childs UVALUE.
	  WIDGET_CONTROL, WIDGET_INFO(MAIN, /CHILD), SET_UVALUE=state, /NO_COPY

	  WIDGET_CONTROL, MAIN, /REALIZE 

  ENDIF ELSE BEGIN
		; Retrieve the structure from the child that contains the sub ids.
	  IF  WIDGET_INFO(parent, /VALID_ID) THEN BEGIN
	      stash = WIDGET_INFO(parent, /CHILD)
	      WIDGET_CONTROL, stash, GET_UVALUE=state, /NO_COPY, BAD_ID=bad_id
              state.win_file= winFile

	      IF (N_ELEMENTS(value) eq 0) 	then value = ''	  

	      WIDGET_CONTROL, state.text_id, SET_VALUE=value, BAD_ID=bad_id

	      WIDGET_CONTROL, stash, SET_UVALUE=state, /NO_COPY, BAD_ID=bad_id
	 ENDIF

         MAIN = parent
   ENDELSE

	; value is all the user will know about the internal structure
	; of your widget.
  RETURN, MAIN

END




PRO catch1d_get_pvtcolor,i,color
COMMON COLORS, R_ORIG, G_ORIG, B_ORIG, R_CURR, G_CURR, B_CURR
; 24 bits
	if n_elements(R_ORIG) eq 0 then $
	catch1d_get_pvtct
	color = R_ORIG(i) + G_ORIG(i)*256L + B_ORIG(i)*256L ^2
;	plot,indgen(10),color=color
END

PRO catch1d_load_pvtct,ctfile
	if n_params() eq 0 then restore,'catch1d.tbl' else $
	restore,ctfile
	tvlct,red,green,blue
	xpalette
END

PRO catch1d_save_pvtct
	tvlct,red,green,blue,/get
	save,red,green,blue,file='catch1d.tbl'
END

PRO catch1d_get_pvtct
COMMON COLORS, R_ORIG, G_ORIG, B_ORIG, R_CURR, G_CURR, B_CURR

; 8 bit visual

	if  !d.n_colors lt 16777216 then begin
		tvlct,red,green,blue,/get
	endif else begin

; 24 bit visual
	file = 'catch1d.tbl'
	found = findfile(file)
	if found(0) eq '' then begin
		file =getenv('EPICS_EXTENSIONS_PVT')+'/bin/'+getenv('HOST_ARCH')+'/catch1d.tbl'
		found1 = findfile(file)
		if found1(0) eq '' then $
		file =getenv('EPICS_EXTENSIONS')+'/bin/'+getenv('HOST_ARCH')+'/catch1d.tbl'
		end
	restore,file
	tvlct,red,green,blue
	end

; set ORIG color 

	R_ORIG = red
	G_ORIG = green
	B_ORIG = blue

	LOADCT,39
END

PRO zoom_to_box
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_warningtext_block,w_warningtext_ids

tx = ['Mouse buttons :', $
	'    Left :   drag box ', $
	'    Middle:  resize box ', $
	'    Right:   zoom to box']
w_warningtext,tx,40,5,'Zoom to box'

	win_state = WIDGET_INFO(widget_ids.base,/GEOMETRY)
	if win_state.xoffset gt 300 then $
	WIDGET_CONTROL,w_warningtext_ids.base, $
		TLB_SET_XOFFSET=10, $
		TLB_SET_YOFFSET=300 $
	else $
	WIDGET_CONTROL,w_warningtext_ids.base, $
		TLB_SET_XOFFSET=10+win_state.xoffset+win_state.xsize, $
		TLB_SET_YOFFSET=300 


IF scanData.lastPlot eq -1 then return 
;        WIDGET_CONTROL, widget_ids.plot_area, SENSITIVE = 0

save_limits = make_array(4,/float)

save_limits(0) = w_plotspec_limits(0)
save_limits(1) = w_plotspec_limits(1)
save_limits(2) = w_plotspec_limits(2)
save_limits(3) = w_plotspec_limits(3)

WSET, widget_ids.plot_area

        MY_BOX_CURSOR,x,y,xs,ys
        WIDGET_CONTROL,widget_ids.plot_wid,/CLEAR_EVENTS 
        WIDGET_CONTROL,w_warningtext_ids.base,/DESTROY
d=convert_coord([x,x+xs],[y,y+ys],/DEVICE,/TO_DATA)
w_plotspec_limits(0) = d(0,0)
w_plotspec_limits(1) = d(0,1)
w_plotspec_limits(2) = d(1,0)
w_plotspec_limits(3) = d(1,1)
        WAIT, .2
;        WIDGET_CONTROL,widget_ids.plot_area , SENSITIVE = 1
        UPDATE_PLOT, 0

w_plotspec_limits(0) = save_limits(0)
w_plotspec_limits(1) = save_limits(1)
w_plotspec_limits(2) = save_limits(2)
w_plotspec_limits(3) = save_limits(3)

END

PRO zoom_box,x1,y1,x2,y2
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

IF scanData.lastPlot eq -1 then return 

save_limits = make_array(4,/float)

save_limits(0) = w_plotspec_limits(0)
save_limits(1) = w_plotspec_limits(1)
save_limits(2) = w_plotspec_limits(2)
save_limits(3) = w_plotspec_limits(3)

WSET, widget_ids.plot_area

w_plotspec_limits(0) = x1
w_plotspec_limits(1) = x2
w_plotspec_limits(2) = y1
w_plotspec_limits(3) = y2
        WAIT, .2
;        WIDGET_CONTROL,widget_ids.plot_area , SENSITIVE = 1
        UPDATE_PLOT, 0

w_plotspec_limits(0) = save_limits(0)
w_plotspec_limits(1) = save_limits(1)
w_plotspec_limits(2) = save_limits(2)
w_plotspec_limits(3) = save_limits(3)
END


PRO zoom_in_out
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_warningtext_block,w_warningtext_ids
tx = ['Mouse buttons :', $
	'    Left :   zoom in ', $
	'    Middle:  zoom out ', $
	'    Right:   quit zoom in/out mode']
w_warningtext,tx,40,5,'Zoom In/Out'

	win_state = WIDGET_INFO(widget_ids.base,/GEOMETRY)
	if win_state.xoffset gt 300 then $
	WIDGET_CONTROL,w_warningtext_ids.base, $
		TLB_SET_XOFFSET=10, $
		TLB_SET_YOFFSET=300 $
	else $
	WIDGET_CONTROL,w_warningtext_ids.base, $
		TLB_SET_XOFFSET=10+win_state.xoffset+win_state.xsize, $
		TLB_SET_YOFFSET=300 

WSET, widget_ids.plot_area

WHILE 1 do begin
;cursor,x,y,1,/normal
cursor,x,y,0,/normal

if !err eq 2 then begin            ; zoom out
;	UPDATE_PLOT,1
x = (x - !x.window(0)) / (!x.window(1)-!x.window(0)) * $
        (!x.crange(1)-!x.crange(0)) + !x.crange(0)

y = (y - !y.window(0)) / (!y.window(1)-!y.window(0)) * $
        (!y.crange(1)-!y.crange(0)) + !y.crange(0)
;st = 'Cursor X Y value @ button 1 pressed: '+string(x) +string(y)
;print,st

dx = 1. * (!x.crange(1)-!x.crange(0))
dy = 1. * (!y.crange(1)-!y.crange(0))
x1 = x - dx
x2 = x + dx
y1 = y - dy
y2 = y + dy
zoom_box,x1,y1,x2,y2
end

if !err eq 1 then begin            ; zoom in 
x = (x - !x.window(0)) / (!x.window(1)-!x.window(0)) * $
        (!x.crange(1)-!x.crange(0)) + !x.crange(0)

y = (y - !y.window(0)) / (!y.window(1)-!y.window(0)) * $
        (!y.crange(1)-!y.crange(0)) + !y.crange(0)
;st = 'Cursor X Y value @ button 1 pressed: '+string(x) +string(y)
;print,st

dx = 0.25 * (!x.crange(1)-!x.crange(0))
dy = 0.25 * (!y.crange(1)-!y.crange(0))
x1 = x - dx 
x2 = x + dx 
y1 = y - dy 
y2 = y + dy 
;if x2 gt !x.crange(1) then x2 = !x.crange(1)
;if x1 lt !x.crange(0) then x1 = !x.crange(0)
;if y2 gt !y.crange(1) then y2 = !y.crange(1)
;if y1 lt !y.crange(0) then y1 = !y.crange(0)
zoom_box,x1,y1,x2,y2
end

if !err eq 4 then begin 		; stop zoom in/out
	WIDGET_CONTROL,w_warningtext_ids.base,BAD=bad,/DESTROY
	WIDGET_CONTROL,widget_ids.plot_wid,/CLEAR_EVENTS
	return
	end

end
END

PRO zoom_out
COMMON w_warningtext_block,w_warningtext_ids

tx = ['ZOOM_OUT MODE', '    LMB stays in zoom out mode', $
	'    MMB refresh the drawing area', $
	'    RMB stops zoom out mode']
w_warningtext,tx,40,5
WHILE 1 do begin
;cursor,x,y,1,/normal
cursor,x,y,0,/normal
if !err eq 2 then begin
	UPDATE_PLOT,1
end
if !err eq 1 then begin
x = (x - !x.window(0)) / (!x.window(1)-!x.window(0)) * $
        (!x.crange(1)-!x.crange(0)) + !x.crange(0)

y = (y - !y.window(0)) / (!y.window(1)-!y.window(0)) * $
        (!y.crange(1)-!y.crange(0)) + !y.crange(0)
st = 'Cursor X Y value @ button 1 pressed: '+string(x) +string(y)
;print,st

dx = 1. * (!x.crange(1)-!x.crange(0))
dy = 1. * (!y.crange(1)-!y.crange(0))
x1 = x - dx 
x2 = x + dx 
y1 = y - dy 
y2 = y + dy 
zoom_box,x1,y1,x2,y2
end
if !err eq 4 then begin
	WIDGET_CONTROL,w_warningtext_ids.base,BAD=bad,/DESTROY
	return
	end
end
END

PRO draw_dragLine,clean=clean,x,y,slope
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_warningtext_block,w_warningtext_ids
COMMON w_statistic_block,w_statistic_ids

tx = ['Mouse buttons :', $
	'    Left :   pick start point ', $
	'    Middle:  pick end point ', $
	'    Right:   quit slope calc mode']
w_warningtext,tx,40,5,'Pick Slope Line'

	win_state = WIDGET_INFO(widget_ids.base,/GEOMETRY)
	if win_state.xoffset gt 300 then $
	WIDGET_CONTROL,w_warningtext_ids.base, $
		TLB_SET_XOFFSET=10,TLB_SET_YOFFSET=300 $
	else $
	WIDGET_CONTROL,w_warningtext_ids.base, $
		TLB_SET_XOFFSET=10+win_state.xoffset+win_state.xsize, $
		TLB_SET_YOFFSET=300


!Err = 0
if keyword_set(clean) then begin
        if n_params() lt 2 then begin
                print,'Usage: draw_dragLine,x,y,[/clean,slope]
                return
                end
        if n_elements(x) eq n_elements(y) and n_elements(x) gt 1 then $
        oplot,x,y,color=0
        return
        end

; need to be drawing area

WSET, widget_ids.plot_area

LOOP0:
cursor,x1,y1,/down
x2=x1 & y2=y1
LOOP:
while (!err ne 2) do begin
        oplot,[x1,x2],[y1,y2], color=0
        cursor,x2,y2,/nowait
        oplot,[x1,x2],[y1,y2], color = !d.n_colors - 2
wait,0.001
endwhile
	
	slope=0
	x=[x1,x2]
	y=[y1,y2]
	if  x(1) ne x(0) then slope = (y(1)-y(0)) /(x(1) -x(0))
	st = ''
	st = [st,'X1 = '+string(x1)]
	st = [st,'Y1 = '+string(y1)]
	st = [st,'X2 = '+string(x2)]
	st = [st,'Y2 = '+string(y2)]
	st = [st,'','Slope = '+string(slope)]	

	if !err eq 2 then begin 		; whether stop the mode 
	WIDGET_CONTROL,widget_ids.plot_wid,/CLEAR_EVENTS

	w_statistic,st,25,10,'Slope Calc'
	win_state = WIDGET_INFO(widget_ids.base,/GEOMETRY)
	if win_state.xoffset gt 300 then $
	WIDGET_CONTROL,w_statistic_ids.base, $
		TLB_SET_XOFFSET=10, $
		TLB_SET_YOFFSET=500 $
	else $
	WIDGET_CONTROL,w_statistic_ids.base, $
		TLB_SET_XOFFSET=10+win_state.xoffset+win_state.xsize, $
		TLB_SET_YOFFSET=500


	cursor,x1,y1,/down

	if !err eq 4 then begin
		oplot,x,y,color=0
		oplot,x,y
		WIDGET_CONTROL,w_warningtext_ids.base,BAD=bad,/DESTROY
		WIDGET_CONTROL,w_statistic_ids.base,BAD=bad,/DESTROY
		UPDATE_PLOT,scanData.lastPlot
		return
		end

	if !err eq 1 then begin
		oplot,x,y,color=0
		goto,LOOP
		end
	end

	if !err eq 2 then begin
		oplot,x,y,color=0
		goto,LOOP0
	end
END



PRO scan_field_set,pv,print=print
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids
if n_params() eq 0 then begin
	w_warningtext,"usage:  scan_field_set,'scan_pvname',/print
	return
	end

scan_field_init,pv

s1 = n_elements(field_value)
;
;  Note the last .CPT field can not be set
; 
no = s1 - 1
if keyword_set(print) then begin
	for i=0,no-1 do print,field_name_array(i),'    ',field_value(i)
	end
ret = caputArray(field_name_array(0:no-1),field_value(0:no-1))
if ret ne 0 then w_warningtext,'scan_field_set failed in array put'
END

PRO scan_field_get,pv,print=print
 COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids

if n_params() eq 0 then begin
	w_warningtext,"usage:  scan_field_get,'scan_pvname',/print
	return
	end

scan_field_init,pv

s = size(field_name)

no = s(1)
field_value = make_array(no,/string,value=string(replicate(32b,40)))

	ln = cagetArray(field_name_array,y,/string)
	field_value = y
	y = 0

	if keyword_set(print) then begin
	for i=0, no-1  do print,field_name_array(i), '  ', field_value(i)
	end

END


PRO scan_field_init,pv,print=print
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids

if n_elements(pv) ne 0 then begin
	s = size(field_name)
	no = s(1)

field_name_array = make_array(no,/string,value=string(replicate(32b,30)))

	for i=0, no-1  do begin 
		 field_name_array(i)  = pv + field_name(i)
		end
	
	if keyword_set(print) then begin
	for i=0, no-1  do print,field_name_array(i)
	end
end
END


PRO w_scanfield_close
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids
	if XRegistered('w_scanfield') ne 0 then $
	WIDGET_CONTROL, w_scanfield_ids.base, /DESTROY
END


PRO w_scanfield_event,event
WIDGET_CONTROL, event.id, GET_UVALUE = eventval
CASE eventval OF
	"SCANFIELD_OK" : BEGIN
		WIDGET_CONTROL,event.top,/DESTROY
		END
	ENDCASE
END

PRO w_scanfield, GROUP = GROUP
COMMON CATCH1D_COM, widget_ids, scanData
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids

if XRegistered('w_scanfield') ne 0 then return 

w_scanfield_base=WIDGET_BASE(TITLE = 'Scan Fields Widget', /COLUMN)
w_scanfield_title = WIDGET_LABEL(w_scanfield_base,VALUE='SCAN Record Set(Formated as 03/27/95)')

if scanData.pv ne '' then scan_field_init,scanData.pv

s1 = size(field_name_array)
s2 = size(field_value)
no = s1(1) - 1
str = make_array(s1(1),/string,value=string(replicate(32b,80)))
for i=0,no do begin
str(i) = field_name_array(i) + '        ' + field_value(i)
end

list = WIDGET_TEXT(w_scanfield_base,VALUE=str,UVALUE='LIST', $
	XSIZE =60, $
	YSIZE=20,/SCROLL)

close = WIDGET_BUTTON(w_scanfield_base, $
                        VALUE = ' Close ', $
                        UVALUE = 'SCANFIELD_OK')

w_scanfield_ids = { base: w_scanfield_base }

WIDGET_CONTROL, w_scanfield_base,/REALIZE

XMANAGER, 'w_scanfield',w_scanfield_base, GROUP_LEADER = GROUP

END



PRO w_warningtext_quest
COMMON w_warningtext_block,w_warningtext_ids

	WIDGET_CONTROL,w_warningtext_ids.text,GET_VALUE=ans
	w_warningtext_ids.answer = strtrim(strupcase(ans(0)),2)
	WIDGET_CONTROL,w_warningtext_ids.base,BAD_ID=bad,/DESTROY
	if w_warningtext_ids.answer eq 'Y' then begin
		if w_warningtext_ids.quest eq 'GoTo' then $
			xycoord_setmotor_confirmed
		if w_warningtext_ids.quest eq 'Get Scan Data and Save' then begin
			catch1dReadScanRecordAppendFile 
			end
	endif else begin   ; 'N'
		if w_warningtext_ids.quest eq 'APPEND' then $
			catch1d_append
	end
END

PRO w_warningtext_event,event
COMMON w_warningtext_block,w_warningtext_ids

WIDGET_CONTROL, event.id, GET_UVALUE = eventval
CASE eventval OF
        "WARNINGTEXT_GET" : BEGIN
		WIDGET_CONTROL,event.id,GET_VALUE=ans
		w_warningtext_ids.answer = strtrim(strupcase(ans(0)),2)
		w_warningtext_quest
		END
        "WARNINGTEXT_Y" : BEGIN
                WIDGET_CONTROL,w_warningtext_ids.text,SET_VALUE='Y'
                END
        "WARNINGTEXT_N" : BEGIN
                WIDGET_CONTROL,w_warningtext_ids.text,SET_VALUE='N'
                END
        "WARNINGTEXT_OK" : BEGIN
		w_warningtext_quest
		END
        "WARNINGTEXT_CLOSE" : BEGIN
                WIDGET_CONTROL,event.top,BAD_ID=bad,/DESTROY
                END
ENDCASE
END


PRO w_warningtext, str,width,height,heading,title=title,quest=quest,xloc=xloc,yloc=yloc, GROUP = GROUP
COMMON w_warningtext_block,w_warningtext_ids

if XRegistered('w_warningtext') ne 0 then begin
	WIDGET_CONTROL,w_warningtext_ids.base,/DESTROY
	end
wtitle = 'scanSee Messages'
dtitle = ''
if n_elements(width) eq 0 then width = 80
if n_elements(height) eq 0 then height = 5 
if n_elements(heading) gt 0 then dtitle=string(heading)
if n_elements(title) gt 0 then wtitle=string(title)

w_warningtext_ids = { $
	base : 0L, $
	text : 0L, $
	quest : '', $
	answer : 'Y' $
	}

w_warningtext_base=WIDGET_BASE(TITLE = wtitle, $
	TLB_FRAME_ATTR = 2, $
	/COLUMN)
w_warningtext_ids.base = w_warningtext_base
w_warningtext_title = WIDGET_LABEL(w_warningtext_base,VALUE=dtitle)

list = WIDGET_TEXT(w_warningtext_base,VALUE=str,UVALUE='LIST', $
	XSIZE =width, $
	YSIZE=height,/SCROLL)

if n_elements(quest) ne 0 then begin
w_warningtext_ids.quest = string(quest)
w_warningtext_row =WIDGET_BASE(w_warningtext_base, /ROW, /FRAME)
w_warningtext_lab = WIDGET_LABEL(w_warningtext_row,VALUE=string(quest)+' (Y/N) ?')
w_warningtext_text = WIDGET_TEXT(w_warningtext_row,VALUE='Y', $
	EDITABLE=1, UVALUE='WARNINGTEXT_GET', XSIZE=2)
w_warningtext_ids.text = w_warningtext_text 

w_warningtext_y = WIDGET_BUTTON(w_warningtext_row,VALUE='Y', $
	UVALUE='WARNINGTEXT_Y')
w_warningtext_n = WIDGET_BUTTON(w_warningtext_row,VALUE='N', $
	UVALUE='WARNINGTEXT_N')

w_warningtext_actrow =WIDGET_BASE(w_warningtext_base, /ROW)
w_warningtext_ok = WIDGET_BUTTON(w_warningtext_actrow,VALUE=' Accept ', $
	UVALUE='WARNINGTEXT_OK')
close = WIDGET_BUTTON(w_warningtext_actrow, $
                        VALUE = ' Cancel ', $
                        UVALUE = 'WARNINGTEXT_CLOSE')

endif else begin
close = WIDGET_BUTTON(w_warningtext_base, $
                        VALUE = ' Close ', $
                        UVALUE = 'WARNINGTEXT_CLOSE')
end

if keyword_set(xloc) then begin
	if n_elements(yloc) eq 0 then yloc = 300
	WIDGET_CONTROL, w_warningtext_base,/REALIZE, $
	TLB_SET_XOFFSET= xloc, TLB_SET_YOFFSET= yloc 
endif else $
	WIDGET_CONTROL, w_warningtext_base,/REALIZE


XMANAGER,'w_warningtext',w_warningtext_base, GROUP_LEADER = GROUP


END


PRO  getStatisticDeviation_1d,id1,y,mean,sdev,mdev,st
	mean=0.
	sdev=0.
	mdev=0.
	no = n_elements(y)
	if no eq 0 then return 
	mean = total(y)/no
	if no eq 1 then return
	index = where(y gt mean, count)      ; check for constant function 
	mean = [mean,0.,0.,0.]
	if count gt 0 then mean = MOMENT(y,mdev=mdev,sdev=sdev)

st = [' Detector '+strtrim(id1+1,1)]
st= [st+' ']
        st = [st, '   Mean         = '+string(mean(0))]
        st = [st, '   Standard Dev = '+string(sdev)]
        st = [st, '   Mean Abs Dev = '+string(mdev)]
        st = [st, '   Variance     = '+string(mean(1))]
        st = [st, '   Skewness     = '+string(mean(2))]
        st = [st, '   Kurtosis     = '+string(mean(3))]
END

PRO  getStatistic_1d,id1,p1,d1,c_mass,xpeak,ypeak,y_hpeak,FWHM,st

; call statistic_1d

        statistic_1d,p1,d1,c_mass,x_peak,y_peak,y_hpeak,FWHM

st = [' Detector '+strtrim(id1+1,1)]
st= [st+' ']
        st = [st, '   Peak  X='+strtrim(x_peak,1)+'  Y='+strtrim(y_peak,1)]
;       st = [st, '   H-Peak  Y='+strtrim(y_hpeak)]
        st = [st, '   Centroid  '+ strtrim(c_mass,1)]
        st = [st, '   FWHM      '+strtrim(FWHM,1)]

if n_elements(x_peak) gt 0 then begin
	largest = max(y_peak)
	i_largest = 0
	for i=0,n_elements(x_peak)-1 do begin
		if y_peak(i) ge largest then begin 
		i_largest = i
		goto, write_peak
		end
		end
	write_peak:
	xpeak = x_peak(i_largest)
	ypeak = y_peak(i_largest)
	end

END





;
; find  fwh_max, c_mass, peak for a given x,y array
;
PRO statistic_1d,x,y,c_mass,x_peak,y_peak,y_hpeak,fwhm,fwhm_xl,fwhm_wd, $
	FIT=FIT,XINDEX=XINDEX,LIST=LIST

xindex = keyword_set(XINDEX)
list = keyword_set(LIST)

nx = n_elements(x)
a=make_array(nx,/float)
da=make_array(nx,/float)
ny=make_array(nx,/float)
slopey=make_array(nx,/float)

ymin = min(y)
ymax = max(y)
ny = y - ymin

peak = ymax
hpeak = 0.5 * max(ny)
y_hpeak= hpeak + ymin

; area = int_tabulated(x,ny)
; harea = 0.5 * area

d0=0
for i=1,nx-1 do begin
	dx = x(i) - x(i-1)
	if dx ne 0. then begin
	da(i) = 0.5 *(ny(i)+ny(i-1)) * dx
	d0 = d0 + da(i)
	a(i) = d0
	slopey(i)= (ny(i)-ny(i-1))/dx
	if list then print,strtrim(i,1),x(i),y(i),da(i),a(i),slopey(i),ny(i)
	end
end

area = d0
harea = 0.5 * area

; Find c_mass

newtons_method,x,a,harea,c_mass
if list then print,'===='
if list then print,'C_mass',harea,c_mass


; Find half peaks

if list then print,'===='
nohwdl=0
nohwdr=0
x_hwdl=0
x_hwdr=0
for i=1,nx-1 do begin
	yl = ny(i-1) - hpeak
	yr = ny(i) - hpeak
       if yl*yr lt 0. and yl lt 0. then begin
		nohwdl = [nohwdl, i-1]
;		print,i-1,y(i-1)
		newtons_method,[x(i-1),x(i)],[yl,yr],0.,x_sol,notfound
		x_hwdl= [x_hwdl,x_sol]
		end
       if yl*yr lt 0. and yl gt 0. then begin
		nohwdr = [nohwdr, i-1]
;		print,i-1,y(i-1)
		newtons_method,[x(i-1),x(i)],[yl,yr],0.,x_sol,notfound
		x_hwdr= [x_hwdr,x_sol]
		end
end
;print,'nohwdl',nohwdl, x_hwdl
;print,'nohwdr',nohwdr, x_hwdr
	lo=0
	fwhm = 0.
if n_elements(nohwdl) gt 1 then begin 
	x_hwd = x_hwdl(1:n_elements(nohwdl)-1)
	nohw = n_elements(x_hwd)
if n_elements(nohwdr) gt 1 then begin
	x_hwde = x_hwdr(1:n_elements(nohwdr)-1)
	nohwe = n_elements(x_hwde)
	fwhm = make_array(nohw,/float)
	for i=0,nohw-1 do begin
		x1 = x_hwd(i)
	for j=0,nohwe-1 do begin
		if x_hwde(j) ne x1 then begin
			fwhm(i) = abs(x_hwde(j) - x1)
			lo=lo+1
;			print,'FWHM',lo,fwhm(i)
			goto,outer
			end
		end
	outer:
	end
	end
	FWHM = max(fwhm)
end

;if n_elements(nohwdr) gt 1 then begin
;	if n_elements(x_hwd) gt 0 then $
;	x_hwd = [x_hwd, x_hwdr(1:n_elements(nohwdr)-1)] else $
;	x_hwd = [x_hwdr(1:n_elements(nohwdr)-1)]
;	end
;if n_elements(x_hwd) gt 0 then begin
;	x_HPeak = x_hwd(sort(x_hwd))
;	if list then print,'hpeak,y_hpeak',hpeak,y_hpeak
;	if list then print,'HPeak pts:',x_HPeak
;end

; Find peaks

if keyword_set(FIT) then begin
nopeaks=0
if list then print,'===='
for i=1,nx-1 do begin
       if slopey(i-1) gt 0 and slopey(i-1)*slopey(i) lt 0. then begin
;		print,i,slopey(i-1),slopey(i)
		nopeaks = [nopeaks, i]
		end
end
;print,'nopeaks',nopeaks
no = n_elements(nopeaks)-1
if no gt 0 then begin
x_peak = make_array(no,/float)
y_peak = make_array(no,/float)
for i=1,no do begin
	i2= nopeaks(i)
	i1= i2-1
	newtons_method,[x(i1),x(i2)],[slopey(i1),slopey(i2)],0.,x_sol,notfound
	if notfound eq 0 then begin
if list then 	print,'Peak #',i,x_sol,y(i1)
		x_peak(i-1)= x_sol
		y_peak(i-1) = y(i1)
		end
end
endif else begin
	y_peak = ymax
	if y(0) gt y(nx-1) then x_peak = x(0) else x_peak = x(nx-1)
if list then 	print,'Ymax at pt ',y_peak,x_peak
end
endif else begin

	for i=0,nx -1 do begin
		if y(i) eq peak then begin
		x_peak = x(i)
		y_peak = peak
		return
		end
	end
end

END

PRO find_hpeak,x,nx,xindex=xindex
print,'===='
fwh_max= make_array(4,/float)
ix = nx / 4
x_index = indgen(nx)
for m=0,3 do begin
i1 = ix *m 
i2 = i1+ix-1
newx = x(i1:i2)
newy = ny(i1:i2)

xindex = keyword_set(XINDEX)
	if xindex then begin
	newx = x_index(i1:i2)
	newtons_method_norm,newx,newy,hpeak,n1,x_sol,notfound
	fwh_max_x1 = x(n1) + x_sol * (x(n1+1) - x(n1))
	endif else begin
	newtons_method,newx,newy,hpeak,fwh_max_x1,notfound
	end

	if notfound then print,'HPeak RANGE #',m+1,'     ENCOUNTERED NOT FOUND PROBLEM' 
	fwh_max(m)=fwh_max_x1
	print,'HPeak RANGE #',m+1,fwh_max(m)
end
END


PRO newtons_method,x,y,y_sol,x_sol,notfound
notfound = 0
nx = n_elements(y)
n1 = 0 
n2 = nx-1 
RETEST:
;print,'N1,N2',n1,n2,y(n1),y(n2)
if (n2-n1) le 1 then begin
	if (y_sol - y(n2)) * (y_sol - y(n1)) gt 0 then begin
		x_sol= x(n1)
		notfound = 1
		return
		end
	if (x(n2)-x(n1)) eq 0. then begin
		x_sol = x(n1)
		return
	end
	x_sol = x(n1)+ (y_sol - y(n1)) /(y(n2)-y(n1)) *(x(n2)-x(n1))
	 return
	end
 
nm = (n2-n1)/ 2 + n1
fm = y (nm)
;print,nm,fm,y_sol
if abs(fm-y_sol) le 1.e-5 then begin
	x_sol = x(nm)
;	print,'Stop at NM,x_sol',nm,x_sol
	return
endif else begin
	if (fm-y_sol) *(y(n2) - y_sol) gt 0 then begin
		n2 = nm
	endif else  begin
		n1 = nm
	end
	goto,RETEST
	end
END

;
; using index and factor instead of real value for x array
;
PRO newtons_method_norm,x,y,y_sol,n1,x_sol,notfound
	rx = float(x)
	newtons_method,rx,y,y_sol,x_sol,notfound
	n1 = fix(x_sol)
	x_sol = x_sol-float(n1)
END



PRO user_scale_event,event
COMMON CATCH1D_COM, widget_ids, scanData
COMMON user_scale_block,user_scale_ids
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames

WIDGET_CONTROL, event.id, GET_UVALUE = eventval
CASE eventval OF
	"USER_SCALE_SLDR1" : BEGIN
		WIDGET_CONTROL,user_scale_ids.slider1,GET_VALUE=s1
		val = 10.D^s1
		WIDGET_CONTROL,user_scale_ids.xmin, $
			SET_VALUE=strtrim(string(val,format='(g20.10)'),2)
		w_plotspec_limits(0) = val
		END
	"USER_SCALE_SLDR2" : BEGIN
		WIDGET_CONTROL,user_scale_ids.slider2,GET_VALUE=s1
		val = 10.D^s1
		WIDGET_CONTROL,user_scale_ids.xmax, $
			SET_VALUE=strtrim(string(val,format='(g20.10)'),2)
		w_plotspec_limits(1) = val
		END
	"USER_SCALE_SLDR3" : BEGIN
		WIDGET_CONTROL,user_scale_ids.slider3,GET_VALUE=s1
		val = 10.D^s1
		WIDGET_CONTROL,user_scale_ids.ymin, $
			SET_VALUE=strtrim(string(val,format='(g20.10)'),2)
		w_plotspec_limits(2) = val
		END
	"USER_SCALE_SLDR4" : BEGIN
		WIDGET_CONTROL,user_scale_ids.slider4,GET_VALUE=s1
		val = 10.D^s1
		WIDGET_CONTROL,user_scale_ids.ymax, $
			SET_VALUE=strtrim(string(val,format='(g20.10)'),2)
		w_plotspec_limits(3) = val
		END
        "USER_SCALE_XMIN" : BEGIN
		WIDGET_CONTROL,user_scale_ids.xmin,GET_VALUE=s1
		val = float(s1)
		w_plotspec_limits(0) = val 
;       	 	UPDATE_PLOT,scanData.lastPlot
		END
        "USER_SCALE_XMAX" : BEGIN
		WIDGET_CONTROL,user_scale_ids.xmax,GET_VALUE=s1
		val = float(s1)
		w_plotspec_limits(1) = val 
;       	 	UPDATE_PLOT,scanData.lastPlot
		END
        "USER_SCALE_YMIN" : BEGIN
		WIDGET_CONTROL,user_scale_ids.ymin,GET_VALUE=s1
		val = float(s1)
		w_plotspec_limits(2) = val 
;       	 	UPDATE_PLOT,scanData.lastPlot
		END
        "USER_SCALE_YMAX" : BEGIN
		WIDGET_CONTROL,user_scale_ids.ymax,GET_VALUE=s1
		val = float(s1)
		w_plotspec_limits(3) = val 
;       	 	UPDATE_PLOT,scanData.lastPlot
		END
        "USER_SCALE_REFRESH" : BEGIN
		scanData.lastPlot = 1
		if realtime_id.ind eq 1 then begin
			realtime_id.ymin =0.
			realtime_id.ymax =0.
			realtime_id.axis = 1 
		endif else begin
       		 	UPDATE_PLOT,1
		end
		END
        "USER_SCALE_OK" : BEGIN
        	WIDGET_CONTROL,user_scale_ids.xmin,GET_VALUE=temp
	        w_plotspec_limits(0) = float(strcompress(temp(0),/remove_all))
       		WIDGET_CONTROL,user_scale_ids.xmax,GET_VALUE=temp
       		w_plotspec_limits(1) = float(strcompress(temp(0),/remove_all))
       	 	WIDGET_CONTROL,user_scale_ids.ymin,GET_VALUE=temp
       	 	w_plotspec_limits(2) = float(strcompress(temp(0),/remove_all))
       	 	WIDGET_CONTROL,user_scale_ids.ymax,GET_VALUE=temp
       	 	w_plotspec_limits(3) = float(strcompress(temp(0),/remove_all))
		scanData.lastPlot = 0
		if realtime_id.ind eq 1 then begin
			realtime_id.axis = 1
		endif else begin
       		 	UPDATE_PLOT,0
;			scanData.lastPlot = 1
		end
		END
        "USER_SCALE_CLOSE" : BEGIN
                WIDGET_CONTROL,event.top,/DESTROY
                END
ENDCASE
END


PRO user_scale, GROUP = GROUP
COMMON user_scale_block,user_scale_ids
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved

if XRegistered('user_scale') ne 0 then begin
	WIDGET_CONTROL,user_scale_ids.base,/DESTROY
	end

user_scale_base=WIDGET_BASE(TITLE = 'Plot Ranges ... ', /COLUMN)
label0 = WIDGET_LABEL(user_scale_base,value='User Scale Plot Ranges')

row1 = WIDGET_BASE(user_scale_base, /ROW)
label1 = WIDGET_LABEL(row1,value='XMIN')
user_scale_xmin = WIDGET_TEXT(row1,VALUE=strtrim(w_plotspec_limits(0),2), $
	EDITABLE=1, UVALUE='USER_SCALE_XMIN', XSIZE=20)
slider1 = WIDGET_SLIDER(row1,MIN=-17, MAX=17, SUPPRESS_VALUE=0, $
	UVALUE='USER_SCALE_SLDR1', VALUE=0)

row2 = WIDGET_BASE(user_scale_base, /ROW)
label2 = WIDGET_LABEL(row2,value='XMAX')
user_scale_xmax = WIDGET_TEXT(row2,VALUE=strtrim(w_plotspec_limits(1),2), $
	EDITABLE=1, UVALUE='USER_SCALE_XMAX', XSIZE=20)
slider2 = WIDGET_SLIDER(row2,MIN=-17, MAX=17, SUPPRESS_VALUE=0, $
	UVALUE='USER_SCALE_SLDR2', VALUE=0)

row3 = WIDGET_BASE(user_scale_base, /ROW)
label3 = WIDGET_LABEL(row3,value='YMIN')
user_scale_ymin = WIDGET_TEXT(row3,VALUE=strtrim(w_plotspec_limits(2),2), $
	EDITABLE=1, UVALUE='USER_SCALE_YMIN', XSIZE=20)
slider3 = WIDGET_SLIDER(row3,MIN=-17, MAX=17, SUPPRESS_VALUE=0, $
	UVALUE='USER_SCALE_SLDR3', VALUE=0)

row4 = WIDGET_BASE(user_scale_base, /ROW)
label4 = WIDGET_LABEL(row4,value='YMAX')
user_scale_ymax = WIDGET_TEXT(row4,VALUE=strtrim(w_plotspec_limits(3),2), $
	EDITABLE=1, UVALUE='USER_SCALE_YMAX', XSIZE=20)
slider4 = WIDGET_SLIDER(row4,MIN=-17, MAX=17, SUPPRESS_VALUE=0, $
	UVALUE='USER_SCALE_SLDR4', VALUE=0)

row5 = WIDGET_BASE(user_scale_base, /ROW)
ok = WIDGET_BUTTON(row5, $
                        VALUE = ' User Scale ', $
                        UVALUE = 'USER_SCALE_OK')

refresh = WIDGET_BUTTON(row5, $
                        VALUE = ' Auto Scale ', $
                        UVALUE = 'USER_SCALE_REFRESH')

close = WIDGET_BUTTON(row5, $
                        VALUE = ' Done ', $
                        UVALUE = 'USER_SCALE_CLOSE')


user_scale_ids = { $
	base : user_scale_base, $
	xmin : user_scale_xmin, $
	xmax : user_scale_xmax, $
	ymin : user_scale_ymin, $
	ymax : user_scale_ymax, $
	slider1 : slider1, $
	slider2 : slider2, $
	slider3 : slider3, $
	slider4 : slider4 $
	}
	

WIDGET_CONTROL, user_scale_base,/REALIZE

XMANAGER, 'user_scale',user_scale_base, GROUP_LEADER = GROUP


END

;
; catch1d_optionmenu.pro
;

PRO plotoptionsmenu_sensitive,i,on_off
COMMON PLOTMENU_OPTION_BLOCK,ids,r_names
        WIDGET_CONTROL,ids(i),SENSITIVE=on_off
END

PRO plotoptionsmenu_set_string,i,j,k,l,m
COMMON PLOTMENU_OPTION_BLOCK,ids,r_names
        WIDGET_CONTROL,ids(i),SET_VALUE=r_names(i)
        len = strlen(r_names(j))-1
        WIDGET_CONTROL,ids(j),SET_VALUE=' '+strmid(r_names(j),1,len)
if n_params() eq 2 then return
        len = strlen(r_names(k))-1
        WIDGET_CONTROL,ids(k),SET_VALUE=' '+strmid(r_names(k),1,len)
if n_params() eq 3 then return
        len = strlen(r_names(l))-1
        WIDGET_CONTROL,ids(l),SET_VALUE=' '+strmid(r_names(l),1,len)
if n_params() eq 4 then return
        len = strlen(r_names(m))-1
        WIDGET_CONTROL,ids(m),SET_VALUE=' '+strmid(r_names(m),1,len)
END


PRO plotoption_setcolor
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

  if w_plotspec_id.color eq 1 then begin
;        LOADCT, 39
	dcl = !d.table_size - 2
	ncv = 4
        colorlevel = dcl / ncv
        for i=0,18 do begin
        ii = i / ncv
        im = i mod ncv
        w_plotspec_id.colorI(i) = dcl - ii - im * colorlevel
        end
  end

END

PRO plotoptionsmenu_Event, Event
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON PLOTMENU_OPTION_BLOCK,ids,r_names

  WIDGET_CONTROL,EVENT.Id,GET_UVALUE=Ev

WSET,widget_ids.plot_area

if w_plotspec_id.scan eq 0 and realtime_id.ind eq -1 then $
   scanData.act_npts = scanData.readin_npts

  CASE Event.Value OF
; Color Curve 
    2: begin
	plotoptionsmenu_set_string,2,3
	w_plotspec_id.color = 1
	plotoption_setcolor
	end
    3: begin
	plotoptionsmenu_set_string,3,2
	w_plotspec_id.color = 0
	for i=0,18 do begin
       	w_plotspec_id.colorI(i) = !d.table_size - 1 
	end
	end
; solid / dotted/ dashed
      5: begin
	plotoptionsmenu_set_string,5,6
	w_plotspec_id.solid = 0
	end
      6: begin
	plotoptionsmenu_set_string,6,5
	w_plotspec_id.solid = 1
	end
; plot style line,point,both
      8: begin
	plotoptionsmenu_set_string,8,9,10
	w_plotspec_id.type = 0
	end
      9: begin
	plotoptionsmenu_set_string,9,10,8
	w_plotspec_id.type = 1
	end
      10: begin
	plotoptionsmenu_set_string,10,8,9
	w_plotspec_id.type = 2
	end
; Grid off/on
     12: begin
	plotoptionsmenu_set_string,12,13
	w_plotspec_id.xticklen = 0.04
	w_plotspec_id.yticklen = 0.02
	w_plotspec_id.gridstyle= 0
	w_plotspec_id.grid = 0
	end
     13: begin
	plotoptionsmenu_set_string,13,12
	w_plotspec_id.xticklen = 0.5
	w_plotspec_id.yticklen = 0.5
	w_plotspec_id.gridstyle= 1
	w_plotspec_id.grid = 1
	end
; Errbar off/on
     15: begin
	plotoptionsmenu_set_string,15,16
	w_plotspec_id.errbars = 0
	end
     16: begin
	plotoptionsmenu_set_string,16,15
	w_plotspec_id.errbars = 1
	end
; Y scale linear, Y > 0, log
      18: begin
	plotoptionsmenu_set_string,18,19,20
	w_plotspec_id.log = 0
	end
     19: begin
	plotoptionsmenu_set_string,19,18,20
	w_plotspec_id.log = 2
	end
     20: begin
	plotoptionsmenu_set_string,20,18,19
	w_plotspec_id.log = 1
	end
; Plot ranges 
     21: begin
        user_scale, GROUP= event.top
        return
	end
; Plot labels 
     22: begin
 	if realtime_id.ind eq 1 then return
        w_plotspec, GROUP= event.top
        return
	end
  ELSE:
  ENDCASE

if realtime_id.ind eq 1 then begin
	realtime_id.axis = 1
endif else $
   UPDATE_PLOT,scanData.lastPlot

END

FUNCTION plotOptions,parent,UVALUE=uvalue
COMMON PLOTMENU_OPTION_BLOCK,ids,r_names
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

  junk   = { CW_PDMENU_S, flags:0, name:'' }

; replot menu
  MenuOptions = [ $
      { CW_PDMENU_S,       3, 'Plot Options' }, $ ;        0
        { CW_PDMENU_S,       1, 'Colors' }, $ ;        1
          { CW_PDMENU_S,       0, '* Colors' }, $ ;        2
          { CW_PDMENU_S,       2, '  Black&White' }, $ ;        3
        { CW_PDMENU_S,       1, 'Lines' }, $ ;        4
          { CW_PDMENU_S,       0, '  Solid/Dotted/etc ' }, $ ;        5
          { CW_PDMENU_S,       2, '* Solid Only' }, $ ;        6
        { CW_PDMENU_S,       1, 'Symbols' }, $ ;        7
          { CW_PDMENU_S,       0, '* Line Only' }, $ ;        8
          { CW_PDMENU_S,       0, '  Symbol Only' }, $ ;        9
          { CW_PDMENU_S,       2, '  Both' }, $ ;        10
        { CW_PDMENU_S,       1, 'Grid' }, $ ;        11
          { CW_PDMENU_S,       0, '* Off' }, $ ;       12
          { CW_PDMENU_S,       2, '  On' }, $ ;        13
        { CW_PDMENU_S,       1, 'Err Bars' }, $ ;        14
          { CW_PDMENU_S,       0, '* Off' }, $ ;        15
          { CW_PDMENU_S,       2, '  On' }, $ ;       16 
        { CW_PDMENU_S,       1, 'Y Scale' }, $ ;        17
          { CW_PDMENU_S,       0, '* Linear' }, $ ;       18 
          { CW_PDMENU_S,       0, '  Linear (Y>0)' }, $ ;       19 
          { CW_PDMENU_S,       2, '  Log' }, $ ;       20 
      { CW_PDMENU_S,       0, 'Ranges ...' }, $ ;        21
      { CW_PDMENU_S,       0, 'Labels ...' } $ ;       22 
  ]

ids = make_array(23,value=0L)
r_names = [ '', $
	'', '* Colors', '* Black&White', $
	'', '* Solid/Dotted/etc', '* Solid Only', $
	'', '* Line Only','* Symbol Only','* Both', $
	'', '* Off', '* On', $
	'', '* Off', '* On', $
	'', '* Linear',  '* Linear (Y>0)', '* Log', $
	'', '']


  PLOTOPTIONSMENU = CW_PDMENU( parent, MenuOptions, $
	IDS=ids, $
 	RETURN_ID = r_id, $
	RETURN_NAME = r_name, $
      UVALUE=uvalue)

	return, PLOTOPTIONSMENU
END

PRO setupoptionsmenu_sensitive,i,on_off
COMMON SETUPMENU_OPTION_BLOCK,ids,r_names
        WIDGET_CONTROL,ids(i),SENSITIVE=on_off
END

PRO setupoptionsmenu_set_string,i,j
COMMON SETUPMENU_OPTION_BLOCK,ids,r_names
        WIDGET_CONTROL,ids(i),SET_VALUE=r_names(i)
        len = strlen(r_names(j))-1
        WIDGET_CONTROL,ids(j),SET_VALUE=' '+strmid(r_names(j),1,len)
END

PRO setupOptionsMenu_event,Event
COMMON SETUPMENU_OPTION_BLOCK,ids,r_names
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

  WIDGET_CONTROL,EVENT.Id,GET_UVALUE=Ev

  CASE Event.Value OF
      2: begin
	WIDGET_CONTROL,ids(2),SET_VALUE=r_names(2)
	len = strlen(r_names(3))-1
	WIDGET_CONTROL,ids(3),SET_VALUE=' '+strmid(r_names(3),1,len)
	scanData.option = 0
        return
	end
      3: begin
	WIDGET_CONTROL,ids(3),SET_VALUE=r_names(3)
	len = strlen(r_names(2))-1
	WIDGET_CONTROL,ids(2),SET_VALUE=' '+strmid(r_names(2),1,len)
	scanData.option = 1
        return
	end
      5: begin
	WIDGET_CONTROL,ids(5),SET_VALUE=r_names(5)
	len = strlen(r_names(6))-1
	WIDGET_CONTROL,ids(6),SET_VALUE=' '+strmid(r_names(6),1,len)
        w_plotspec_id.autosave = 1
        WIDGET_CONTROL,widget_ids.savelabel,SET_VALUE='  No Save'
        return
	end
      6: begin
	WIDGET_CONTROL,ids(6),SET_VALUE=r_names(6)
	len = strlen(r_names(5))-1
	WIDGET_CONTROL,ids(5),SET_VALUE=' '+strmid(r_names(5),1,len)
        w_plotspec_id.autosave = 0
        WIDGET_CONTROL,widget_ids.savelabel,SET_VALUE='          '
	catch1d_check_seqno, scanData.trashcan
        return
	end
      8: begin
	WIDGET_CONTROL,ids(8),SET_VALUE=r_names(8)
	len = strlen(r_names(9))-1
	WIDGET_CONTROL,ids(9),SET_VALUE=' '+strmid(r_names(9),1,len)
        w_plotspec_id.realtime = 0
        return
	end
      9: begin
	WIDGET_CONTROL,ids(9),SET_VALUE=r_names(9)
	len = strlen(r_names(8))-1
	WIDGET_CONTROL,ids(8),SET_VALUE=' '+strmid(r_names(8),1,len)
        w_plotspec_id.realtime = 1
        return
	end
      11: begin
	WIDGET_CONTROL,ids(11),SET_VALUE=r_names(11)
	len = strlen(r_names(12))-1
	WIDGET_CONTROL,ids(12),SET_VALUE=' '+strmid(r_names(12),1,len)
        scanData.showlist = 0
                if widget_ids.terminal ne 0 then begin
                        WIDGET_CONTROL,widget_ids.TERMINAL,BAD_ID=bad
                        if bad eq 0 then $
                        WIDGET_CONTROL,widget_ids.TERMINAL,/DESTROY
                        widget_ids.terminal = 0L
                        end
        return
	end
      12: begin
	WIDGET_CONTROL,ids(12),SET_VALUE=r_names(12)
	len = strlen(r_names(11))-1
	WIDGET_CONTROL,ids(11),SET_VALUE=' '+strmid(r_names(11),1,len)
        scanData.showlist = 1
                widget_ids.terminal = CW_TERM(Event.top, $
                                        TITLE=scanData.pv, $
;                                        BGROUP_NAMES=names, $
;                                        BGEVENT_FUNCT='CWTERM_event', $
                                        /FRAME, $
                                        XSIZE=100, YSIZE=20, /SCROLL)
        return
	end
      14: begin
	WIDGET_CONTROL,ids(14),SET_VALUE=r_names(14)
	len = strlen(r_names(15))-1
	WIDGET_CONTROL,ids(15),SET_VALUE=' '+strmid(r_names(15),1,len)
	scanData.debug = 0
        return
	end
      15: begin
	WIDGET_CONTROL,ids(15),SET_VALUE=r_names(15)
	len = strlen(r_names(14))-1
	WIDGET_CONTROL,ids(14),SET_VALUE=' '+strmid(r_names(14),1,len)
	scanData.debug = 1
        return
	end
      16: begin
	catcher_setup,GROUP=event.top
	end
      17: begin
	xloadct, GROUP= event.top
	end
   ELSE:
   ENDCASE
END


FUNCTION setupOptions,parent,UVALUE=uvalue
COMMON SETUPMENU_OPTION_BLOCK,ids,r_names
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  MenuSetup = [ $
      { CW_PDMENU_S,       3, 'Setup' }, $ ;        0
        { CW_PDMENU_S,       1, 'Acquisition' }, $ ;        1
          { CW_PDMENU_S,       0, '  Off' }, $ ;        2
          { CW_PDMENU_S,       2, '* On' }, $ ;        3
        { CW_PDMENU_S,       1, 'AutoSave' }, $ ;        1
          { CW_PDMENU_S,       0, '  Off' }, $ ;        2
          { CW_PDMENU_S,       2, '* On' }, $ ;        3
        { CW_PDMENU_S,       1, 'Realtime' }, $ ;        1
          { CW_PDMENU_S,       0, '  Off' }, $ ;        2
          { CW_PDMENU_S,       2, '* On' }, $ ;        3
        { CW_PDMENU_S,       1, 'TextWin' }, $ ;        1
          { CW_PDMENU_S,       0, '* Off' }, $ ;        2
          { CW_PDMENU_S,       2, '  On' }, $ ;        3
        { CW_PDMENU_S,       1, 'Debug' }, $ ;        1
          { CW_PDMENU_S,       0, '* Off' }, $ ;        2
          { CW_PDMENU_S,       2, '  On' }, $ ;        3
        { CW_PDMENU_S,       0, 'Scan ...' }, $ ;        2
        { CW_PDMENU_S,       0, 'Color ...' } $ ;        2
  ]

;  PDMENU_setup = CW_PDMENU( BASE68, MenuSetup, /RETURN_FULL_NAME, $
;      UVALUE='SETUPMENU')

ids = make_array(18,value=0L)
r_names = ['', '', '* Off','* On', $
	'', '* Off', '* On', $
	'', '* Off', '* On', $
	'', '* Off', '* On', $
	'', '* Off', '* On', $
	'', $
	'']

  SETUPSMENU = CW_PDMENU( parent, MenuSetup, $
	IDS=ids, $
 	RETURN_ID = r_id, $
;	RETURN_NAME = r_name, $
      UVALUE=uvalue)


	return, SETUPSMENU
END

;
; Auto Save File For catch1d_setup.pro
;
;  Wed Apr  3 11:04:10 CST 1996
;


PRO catcher_setup_Event, Event
COMMON CATCH1D_COM, widget_ids, scanData
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'SCAN1D_PVNAME': BEGIN
      WIDGET_CONTROL,catcher_setup_ids.pv,GET_VALUE=pv
	len = strpos(pv(0),'.')
	if len eq -1 then newpv = pv(0) else newpv = strmid(pv(0),0,len)
	if caSearch(newpv+'.EXSC') eq 0 then begin
      	WIDGET_CONTROL,catcher_setup_ids.pv,SET_VALUE=newpv
	scanData.pv = newpv
	scanData.pvconfig = newpv
	pventry_event
	endif else begin
		w_warningtext,'Error: invalid SCAN 1D Pvname',40,2
	end
      END
  'SCAN2D_PVNAME': BEGIN
      WIDGET_CONTROL,catcher_setup_ids.y_pv,GET_VALUE=pv
	len = strpos(pv(0),'.')
	if len eq -1 then newpv = pv(0) else newpv = strmid(pv(0),0,len)
if newpv eq '' then scanData.y_pv = newpv
	if caSearch(newpv+'.EXSC') eq 0 then begin
        WIDGET_CONTROL,catcher_setup_ids.y_pv,SET_VALUE=newpv
	pventry2_event
	endif else begin
		w_warningtext,'Error: invalid SCAN 2D Pvname',40,2
	end
      END
  'SCAN1D_START': BEGIN
	catch1d_Start_xScan
	WIDGET_CONTROL,catcher_setup_ids.stop,SENSITIVE=1
      END
  'SCAN1D_STOP': BEGIN
	catch1d_Stop_xScan
      END
  'SCAN2D_START': BEGIN
	catch1d_Start_yScan
;  	WIDGET_CONTROL,catcher_setup_ids.y_handshake_proc,SENSITIVE=1
      END
  'SCAN2D_STOP': BEGIN
	catch1d_Stop_yScan
;  	WIDGET_CONTROL,catcher_setup_ids.y_handshake_proc,SENSITIVE=0
      END
  'SCAN2D_HANDSHAKE': BEGIN
      WIDGET_CONTROL,catcher_setup_ids.y_handshake,GET_VALUE=pv
	if caSearch(pv(0)) eq 0 or pv(0) eq '' then scanData.y_handshake = pv(0)
      END
  'SCAN2D_HANDSHAKE_V': BEGIN
      WIDGET_CONTROL,catcher_setup_ids.y_handshake_v,GET_VALUE=v
	if strlen(v(0)) gt 0 then catcher_setup_scan.y_handshake_v = v(0)
      END
  'SCAN2D_HANDSHAKE_PROC': BEGIN
	ln = caputArray(scanData.y_handshake, catcher_setup_scan.y_handshake_v) 
      END
  'CATCHER_SETUP_CANCEL': BEGIN
      WIDGET_CONTROL,catcher_setup_ids.base,/DESTROY,BAD=bad
      END
  'CATCHER_SETUP_DONE': BEGIN
	write_config
      WIDGET_CONTROL,catcher_setup_ids.pv,GET_VALUE=pv
	if caSearch(pv(0)) eq 0 then begin
	pventry_event
	end
        WIDGET_CONTROL,catcher_setup_ids.y_pv,GET_VALUE=pv1
	if caSearch(pv1(0)) eq 0 then begin
	pventry2_event
	end
        WIDGET_CONTROL,catcher_setup_ids.y_handshake,GET_VALUE=pv2
	  if caSearch(pv2(0)) eq 0 then begin
	  scanData.y_handshake = pv2(0)
          WIDGET_CONTROL,catcher_setup_ids.y_handshake_v,GET_VALUE=v
	  if strlen(v(0)) gt 0 then catcher_setup_scan.y_handshake_v = v(0)
	  ln = caputArray(pv2(0),v(0))
	  end
;     WIDGET_CONTROL,catcher_setup_ids.base,/DESTROY,BAD=bad
      END
  ENDCASE
END



; DO NOT REMOVE THIS COMMENT: END catcher_setup
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.

PRO catcher_setup_init
COMMON CATCH1D_COM, widget_ids, scanData
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan

;if n_elements(catcher_setup_scan) eq 0 then begin
;	scanData = {$
;		pv : 'cha:scanRec3SC', $
;		y_pv : 'cha:scanRec4SC', $
;		y_handshake : 'cha:scanRec4SC.PROC' $
;		} 
;	end

if n_elements(catcher_setup_scan) eq 0 then begin
	catcher_setup_scan = { $
		pv : '', $
		y_pv : '', $
		y_handshake : '', $
		y_handshake_v : '' $
		}
	end

;if strlen(scanData.pv) gt 0   then $
	catcher_setup_scan.pv = scanData.pv
;if strlen(scanData.y_pv) gt 0 then $
	catcher_setup_scan.y_pv = scanData.y_pv
;if strlen(scanData.y_handshake) gt 0  then  $
	catcher_setup_scan.y_handshake = scanData.y_handshake
END


PRO catcher_setup, GROUP=Group
COMMON CATCH1D_COM, widget_ids, scanData
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan

IF XRegistered('catcher_setup') ne 0 then $
WIDGET_CONTROL,catcher_setup_ids.base,/DESTROY

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  catcher_setup_init

  catcher_setup_base = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
      MAP=1, $
      TITLE='Scan ... ( PV SETUP )', $
      UVALUE='CATCHER_SETUP')

  BASE2 = WIDGET_BASE(catcher_setup_base, $
      ROW=1, $
      MAP=1, $
      TITLE='scan_1d', $
      UVALUE='BASE2')

  SCAN1D_PVNAME = CW_FIELD( BASE2,VALUE=catcher_setup_scan.pv, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='SCAN 1D Pvname:', $
      UVALUE='SCAN1D_PVNAME', $
      XSIZE=30)

  SCAN1D_START = WIDGET_BUTTON( BASE2, $
      UVALUE='SCAN1D_START', $
      VALUE='Start')

  SCAN1D_STOP = WIDGET_BUTTON( BASE2, $
      UVALUE='SCAN1D_STOP', $
      VALUE='Stop')


  BASE3 = WIDGET_BASE(catcher_setup_base, $
      ROW=1, $
      MAP=1, $
      TITLE='scan_2d', $
      UVALUE='BASE3')

  SCAN2D_PVNAME = CW_FIELD( BASE3,VALUE=catcher_setup_scan.y_pv, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='SCAN 2D Pvname:', $
      UVALUE='SCAN2D_PVNAME', $
      XSIZE=30)

  SCAN2D_START = WIDGET_BUTTON( BASE3, $
      UVALUE='SCAN2D_START', $
      VALUE='Start')

  SCAN2D_STOP = WIDGET_BUTTON( BASE3, $
      UVALUE='SCAN2D_STOP', $
      VALUE='Stop')


  BASE4 = WIDGET_BASE(catcher_setup_base, $
      ROW=1, $
      MAP=1, $
      TITLE='2d_handshake', $
      UVALUE='BASE4')

  SCAN2D_HANDSHAKE = CW_FIELD( BASE4,VALUE=catcher_setup_scan.y_handshake, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='2D Handshake PV:', $
      UVALUE='SCAN2D_HANDSHAKE', $
      XSIZE=30)

  fld='1'
  if strlen(catcher_setup_scan.y_handshake) gt 1 then begin
  ln = cagetArray(catcher_setup_scan.y_handshake,pd,/string)
  if ln eq 0 then fld = pd(0)
  end
  SCAN2D_HANDSHAKE_V = CW_FIELD( BASE4,VALUE=fld, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Value:', $
      UVALUE='SCAN2D_HANDSHAKE_V', $
      XSIZE=10)

;  SCAN2D_HANDSHAKE_PROC = WIDGET_BUTTON( BASE4, $
;      UVALUE='SCAN2D_HANDSHAKE_PROC', $
;      VALUE='Proc')
;  WIDGET_CONTROL,SCAN2D_HANDSHAKE_PROC,SENSITIVE=0

  BASE5 = WIDGET_BASE(catcher_setup_base, $
      ROW=1, $
      MAP=1, $
      TITLE='row4', $
      UVALUE='BASE5')

  CATCHER_SETUP_DONE = WIDGET_BUTTON( BASE5, $
      UVALUE='CATCHER_SETUP_DONE', $
      VALUE='Ok')

  CATCHER_SETUP_CANCEL = WIDGET_BUTTON( BASE5, $
      UVALUE='CATCHER_SETUP_CANCEL', $
      VALUE='Close')

catcher_setup_ids = { base : catcher_setup_base, $
	pv : SCAN1D_PVNAME, $
	y_pv : SCAN2D_PVNAME, $
	y_handshake: SCAN2D_HANDSHAKE, $
	y_handshake_v: SCAN2D_HANDSHAKE_V, $
;	y_handshake_proc: SCAN2D_HANDSHAKE_PROC, $
	start : SCAN1D_START, $
	stop : SCAN1D_STOP, $
	start2 : SCAN2D_START, $
	stop2 : SCAN2D_STOP $
	}

  WIDGET_CONTROL, catcher_setup_base, /REALIZE

if XRegistered('w_viewscan') ne 0 then $ 
	WIDGET_CONTROL,catcher_setup_ids.base,SENSITIVE=0
if scanData.nosave and scanData.y_scan and strtrim(scanData.y_handshake,2) eq ''then WIDGET_CONTROL,catcher_setup_ids.y_handshake,SENSITIVE=0

  XMANAGER, 'catcher_setup', catcher_setup_base
;  XMANAGER, 'catcher_setup', catcher_setup_base,NO_BLOCK=0
END
PRO filenamepath,filename,F,P
COMMON CATCH1D_COM, widget_ids, scanData
if n_elements(filename) eq 0 then return
        len = strlen(filename)
        F=filename
        P=scanData.home
        if strpos(filename,'/') eq -1 then return
 
        x=byte(filename)
        P=''
        for i=0,len-1 do begin
        is = len-1 -i
        if string(x(is)) eq '/' then begin
                P = strmid(filename,0,is+1)
                F = strmid(filename,is+1,len-is)
                return
                end
        end
END

PRO write_config
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids

        CATCH,error_status

; demo mode error -128

        if error_status eq -128 then begin
	w_warningtext,['Error: Demo mode no write on config file allowed !', $
		'       You can exit the IDL now.']
	stop
	exit
	end

; write permission  error -171  

        if error_status lt 0 then begin
	res = dialog_message([!err_string +  string(error_status), 'Failed to update the configuration file'],/Error)
	return    ;  exit
        end

openw,unit,scanData.config,/get_lun

printf,unit,"; Generated by ",+scanData.version+scanData.release
printf,unit,"scanData.pv='",scanData.pv,"'"
if strlen(scanData.y_pv) gt 1 then $
	 printf,unit,"scanData.y_pv='",scanData.y_pv,"'"
if strlen(scanData.pvwait) gt 1 then $
	printf,unit,"scanData.pvwait='",scanData.pvwait,"'"
if strlen(scanData.pvbusy) gt 1 then $
	printf,unit,"scanData.pvbusy='",scanData.pvbusy,"'"
if strlen(scanData.y_handshake) gt 1 then $
	printf,unit,"scanData.y_handshake='",scanData.y_handshake,"'"

; add  path 
;	st = "scanData.home='"+scanData.home+"'"
;        printf,unit,st

	x = scanData.path
	first = strpos(x,'/home')
	if first gt 0 then begin
		y = strmid(x,first,strlen(x))
		scanData.path = y
	end

	st = "scanData.path='"+scanData.path+"'"
        printf,unit,st
	st = "scanData.trashcan='"+w_plotspec_array(3)+"'"
        printf,unit,st
	st = "scanData.config='"+ scanData.config+"'"
        printf,unit,st

printf,unit,''
free_lun,unit

if scanData.debug eq 1 then $
print,'***File ',scanData.config,' saved.'

END

PRO read_config,filename
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved

file = scanData.config
if n_elements(filename) gt 0 then file = string(filename)

found = findfile(file)
if found(0) eq '' then return
openr,unit,file,/get_lun 

st=''
while(not eof(unit)) do begin
readf,unit,st
;print,st
	key_st=''
	new_st=''
	; get key variable name defined

	ln1 = strpos(st,"=")
	if ln1 gt -1 then begin
		key_st = strmid(st,0,ln1)

	; get string specification

	ln1 = strpos(st,"'")
	if ln1 gt -1 then begin
		new_st = strmid(st,ln1,strlen(st) - ln1)
		ln2 = strpos(new_st,"'",1)
		if ln2 eq -1 then $ 
		new_st = strmid(new_st,1,strlen(new_st)-1) $
		else if ln2 eq 1 then new_st = '' else $
		new_st = strmid(new_st,1,ln2-1)
	end

	CASE key_st OF 
	'scanData.pv' : scanData.pv = new_st
	'scanData.y_pv' : scanData.y_pv = new_st
	'scanData.pvwait' : scanData.pvwait = new_st
	'scanData.pvbusy' : scanData.pvbusy = new_st
	'scanData.y_handshake' : scanData.y_handshake = new_st
	'scanData.path' : scanData.path = new_st
	'scanData.trashcan' : w_plotspec_array(3) = new_st
	'scanData.envfile' : scanData.envfile = new_st
	'scanData.config' : scanData.config = new_st
	'scanData.option' : scanData.option = new_st
	'scanData.nosave' : scanData.nosave = new_st
	'scanData.debug' : scanData.debug = new_st
	ELSE :
	ENDCASE
	end

end
free_lun,unit

END


;
; this routine does an auto-scaled plot of the selected waveforms
;
PRO UPDATE_PLOT, auto, st

   COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON font_block, text_font, graf_font, ps_font
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON w_statistic_block,w_statistic_ids

   WIDGET_CONTROL, widget_ids.wf_select, GET_VALUE = wf_sel
   if total(wf_sel) eq 0 then return
   x_axis = w_plotspec_id.x_axis_u

win_state = WIDGET_INFO(widget_ids.plot_wid, /GEOMETRY)

;   plotSubTitle = strtrim(w_plotspec_array(4))
   plotSubTitle = ''
   plotTitle=''
   plotYTitle=''
   plotXTitle =''

   num_pts = 1 > (scanData.act_npts-1)


   ;extract valid data from global arrays

   catch1d_check_xaxis,num_pts,p1,xmin,xmax

; check any data available 
   if scanData.lastPlot lt 0 then begin 
	if xmax eq xmin then return 
	auto = 1
	end
 
if w_plotspec_id.log eq 0 and auto ne 0 then auto=1   
if w_plotspec_id.log eq 2 and auto ne 0 then auto=2   ;  Y > 0

;   setPlotLabels                

   scanData.lastPlot = auto
   y_zero = 0
   if auto eq 2 then y_zero = 1.e-7    ; exclude zero for auto scale

   IF (auto gt 0) THEN BEGIN     ;  auto scale
!y.style = 1
!x.style = 1

     ;if autoscale, determine appropriate Y values
     ;depending on which waveforms will be plotted
     pos_ymin = 1.e20
     ymin = 1.e20
     ymax = -1.e20
     err_dy = 0


for i=0,14 do begin

     IF (wf_sel(i) EQ 1) THEN  BEGIN

	d1 = scanData.da(0:num_pts,i)

         IF (MIN(d1) LT ymin) THEN ymin = MIN(d1)
         IF (MAX(d1) GT ymax) THEN ymax = MAX(d1)
	 if w_plotspec_id.log eq 1 and ymin le 0. then begin
		for j=0,num_pts-1 do begin
		  if d1(j) gt 0. and d1(j) lt pos_ymin then pos_ymin=d1(j)
		end
	 endif else pos_ymin = ymin
	if sqrt(abs(ymax)) gt err_dy then err_dy = sqrt(abs(ymax))
	if sqrt(abs(ymin)) gt err_dy then err_dy = sqrt(abs(ymin))
     END

end

; add the support postioner as Y
for i=15,18 do begin
     IF (wf_sel(i) EQ 1) THEN  BEGIN
	d1 = scanData.pa(0:num_pts,i-15)
         IF (MIN(d1) LT ymin) THEN ymin = MIN(d1)
         IF (MAX(d1) GT ymax) THEN ymax = MAX(d1)
	 if w_plotspec_id.log eq 1 and ymin le 0. then begin
		for j=0,num_pts-1 do begin
		  if d1(j) gt 0. and d1(j) lt pos_ymin then pos_ymin=d1(j)
		end
	 endif else pos_ymin = ymin
	if sqrt(abs(ymax)) gt err_dy then err_dy = sqrt(abs(ymax))
	if sqrt(abs(ymin)) gt err_dy then err_dy = sqrt(abs(ymin))
     END
end

; if error bar is on ajust ymin,ymax accordingly

	if w_plotspec_id.errbars  eq 1 then begin
		ymax = ymax + err_dy
		ymin = ymin - err_dy
		end

;
;  increase the xmin,xmax by +5%
;

	if auto gt 0 then view1d_adjust_ranges,xmin,xmax

   ENDIF ELSE BEGIN
;
; user scale auto=0
;
   ; if not autoscale, get limits from entry widgets. 

!y.style = 1
!x.style = 1
xmin = w_plotspec_limits(0)
xmax = w_plotspec_limits(1)
ymin = w_plotspec_limits(2)
ymax = w_plotspec_limits(3)
pos_ymin = ymin
ENDELSE
     
     ;now determine xmin and xmax depending on x-axis selection

     IF (x_axis EQ 0) THEN BEGIN
       plotXTitle = strtrim(w_plotspec_array(1))
     ENDIF ELSE BEGIN
       xmin = 0
	xmax=num_pts
	if auto eq 1 then view1d_adjust_ranges,xmin,xmax
       plotXTitle = 'Step #'        
     ENDELSE

     if total(wf_sel) eq 0 then  plotXTitle = 'Nothing Selected' 

     if n_elements(w_plotspec_array) ne 0 then begin 
	if strlen(strtrim(w_plotspec_array(0))) gt 1 then $
	plotTitle = strtrim( w_plotspec_array(0))
	if strlen(w_plotspec_array(2)) gt 1 then $
	plotYTitle = strtrim(w_plotspec_array(2))
	end

   ;Now draw the axis and plot the selected waveforms


if !d.name ne 'PS' then WSET, widget_ids.plot_area
;   ERASE

   ;fake out PLOT to plot an empty axis
   junk = ['5','6']

   ; If plotting before p1 was read ...
;   IF ((STRLEN(scanData.pv) EQ 0) OR  $    gives problem when no config
    IF (  (ymax LE ymin)             OR  $
       ((MIN(p1) EQ 0) AND (MAX(p1) EQ 0))) THEN  BEGIN
	p1=indgen(num_pts+1)
	xmin=0
	xmax=num_pts
	if auto eq 1 then view1d_adjust_ranges,xmin,xmax
   ENDIF

   ;Plot the axis w/o any waveforms

	POS=[0.15,0.2,0.78,0.85] 
	xticklen = w_plotspec_id.xticklen
	yticklen = w_plotspec_id.yticklen
	gridstyle = w_plotspec_id.gridstyle

;if scanData.act_npts ge scanData.req_npts then begin
; 
; linear plot
;
if w_plotspec_id.log ne 1 then begin


; 10 % margin

	if auto gt 0 then begin
        dy = 0.1 *(ymax-ymin)
        if dy eq 0 then begin
                if ymax eq 0 then  dy = 10 else dy = 0.05 * ymax
                end
        ymax = ymax + dy
        ymin = ymin - dy
	end

; auto scale but only plot y> 0 case

	if auto gt 1 then ymin = 0.

   PLOT, XRANGE = [xmin,xmax],             $
         YRANGE = [ymin,ymax],             $
         XTITLE = plotXTitle,               $
         YTITLE = plotYTitle,               $
	YNOZERO = y_zero, $
	XTICKLEN = xticklen, $
	YTICKLEN = yticklen, $
	XGRIDSTYLE = gridstyle, YGRIDSTYLE= gridstyle, $
	XMINOR = 10, $
        YMINOR = 10, $
         TITLE = plotTitle,               $
         SUBTITLE = plotSubTitle,               $
	POS=pos, $
;	FONT = graf_font, $
         MAX_VALUE = 0, junk
end

;
; log plot
;
if w_plotspec_id.log eq 1 then begin
if ymax le 0. then begin
	w_warningtext,'Data not suitable for YLOG plot.'
;	plotoptionsmenu_set_string,18,19
;	w_plotspec_id.log = 0
	return
	end

	yrange = [ymin,ymax]
	if ymin le 0. then begin
		ymin = pos_ymin
		yrange = [ymin,ymax*10]
		end
   PLOT, XRANGE = [xmin,xmax],             $
         YRANGE =  yrange,            $
         XTITLE = plotXTitle,               $
         YTITLE = plotYTitle,               $
	XTICKLEN = xticklen, $
	YTICKLEN = yticklen, $
	XGRIDSTYLE = gridstyle, YGRIDSTYLE= gridstyle, $
         TITLE = plotTitle,               $
         SUBTITLE = plotSubTitle,               $
	 XMINOR = 10, $
;	YMINOR=9,$
	YTYPE=1,$
	POS=pos, $
;	FONT = graf_font, $
         MAX_VALUE = 0, junk
end

y_descs = strtrim(y_descs,2)

st='Scan #: ' + strtrim(scanData.scanno)

is = 0
for i=0,14 do begin
   IF (wf_sel(i) EQ 1 and realtime_id.def(4+i) gt 0) THEN begin
	d1 = scanData.da(0:num_pts,i)
if w_plotspec_id.statistic eq 3 then begin
	getStatisticDeviation_1d,i,d1,moments,sdev,mdev,st1
        st = [st, st1]
	statis_value = [sdev,mdev,moments(0),moments(1)]
endif else if w_plotspec_id.statistic gt 0 then begin
	getStatistic_1d,i,p1,d1,c_mass,xpeak,ypeak,y_hpeak,FWHM,st1
        st = [st, st1]
	statis_value = [xpeak,c_mass,FWHM,ypeak]
end
if n_elements(statis_value) gt 0 then $
       view1d_legends,pos,is,i,p1,d1,num_pts,x_axis,statis_value else $
       view1d_legends,pos,is,i,p1,d1,num_pts,x_axis

	is = is + 1
        end
end

for i=15,18 do begin
   IF (wf_sel(i) EQ 1 and realtime_id.def(i-15) gt 0) THEN begin
        d1 = scanData.pa(0:num_pts,i-15)
if w_plotspec_id.statistic eq 3 then begin
        getStatisticDeviation_1d,i,d1,moments,sdev,mdev,st1
        st = [st, st1]
        statis_value = [sdev,mdev,moments(0),moments(1)]
endif else if w_plotspec_id.statistic gt 0 then begin
        getStatistic_1d,i,p1,d1,c_mass,xpeak,ypeak,y_hpeak,FWHM,st1
        st = [st, st1]
        statis_value = [xpeak,c_mass,FWHM,ypeak]
end
       view1d_legends,pos,is,i,p1,d1,num_pts,x_axis,statis_value
        is = is + 1
        end
end

if auto eq 1 and n_elements(st) gt 0  and widget_ids.statistic gt 1 then begin
	WIDGET_CONTROL,widget_ids.statistic,SET_VALUE=st,BAD_ID=bad_id,/NO_COPY
	if bad_id ne 0 then widget_ids.statistic = 0L
        end


;
; plot scan number + filename
;
	filenamepath,scanData.trashcan,F,P

	header_note='data file: ' + F

	xdis = 0.01 * !d.x_size
	ydis = !d.y_size - 1.2 * !d.y_ch_size
	xyouts,xdis,ydis,header_note,/device

	if scanData.y_scan gt 0 then begin
	xdis = 0.45 * !d.x_size
	ydis = !d.y_size - 1.2 * !d.y_ch_size

	header_note = '2D SCAN # : '+string(scanData.scanno_2d)
	xyouts,xdis,ydis,header_note,/device
	end

	header_note =  '1D SCAN # : ' + string(scanData.scanno) 
	xdis = 0.75 * !d.x_size
	ydis = !d.y_size - 1.2 * !d.y_ch_size
	xyouts,xdis,ydis,header_note,/device


	footer_note = strmid(strtrim(w_plotspec_array(4)),0,29)
	xdis = 0.01 * !d.x_size
	ydis = 1.2*!d.y_ch_size
	xyouts,xdis,ydis,footer_note,/device


	len = strlen( strtrim(w_plotspec_array(4)))
	footer_note = strmid(strtrim(w_plotspec_array(4)),30,len-30)
	xdis = 0.7 * !d.x_size
	ydis = 1.2*!d.y_ch_size
	xyouts,xdis,ydis,footer_note,/device


footer_note= 'comment: ' + strtrim(w_plotspec_array(5))
	view1d_ydist,(.01-pos(1)),ydis	
	xdis = 0.01 * !d.x_size
	ydis = 0.1*!d.y_ch_size
	xyouts,xdis,ydis,footer_note,/device

END


PRO catch1d_check_xaxis,num_pts,p1,xmin,xmax
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

; select the x-axis for plot

	i = w_plotspec_id.xcord
	if w_plotspec_id.xcord lt 4 then $
	   p1 = scanData.pa(0:num_pts,i) else $
	   p1 = scanData.da(0:num_pts,i-4)
	   xmin = MIN(p1)
	   xmax = MAX(p1)
	   if xmin eq xmax then begin
		str='Warning: Maybe no data available for x-axis P'+ $
			strtrim(i+1,2)+ ' array'
		w_warningtext,str
	   end
END


PRO view1d_xticks,xmin,xmax,XVAL

  XVAL = make_array(!X.TICKS+1,/float)
  DXVAL = (xmax - xmin)/ !X.TICKS 
  for i=0,!X.TICKS do begin
  XVAL(i) = xmin + i*DXVAL
  end
END
 
PRO view1d_yticks,ymin,ymax,YVAL
  YVAL = make_array(!Y.TICKS+1,/float)
  DYVAL = (ymax - ymin)/ !Y.TICKS 
  for i=0,!Y.TICKS do begin
  YVAL(i) = ymin + i*DYVAL
  end
END

PRO view1d_xdist,fact,xval
	dx = !x.window(1) - !x.window(0)
	if fact gt (1.-!x.window(0)) then begin
		print,'Error: ',-!x.window(0),' < fact < ',1 -!x.window(0)
		return
		end
	xval = !x.crange(0) + fact/dx * (!x.crange(1) - !x.crange(0)) 
END

PRO view1d_ydist,fact,yval
	dy = !y.window(1) - !y.window(0)
	if fact gt (1.-!y.window(0)) then begin
		print,'Error: ',-!y.window(0),' < fact < ',1 -!y.window(0)
		return
		end
	yval = !y.crange(0) + fact/dy * (!y.crange(1) - !y.crange(0)) 
END

PRO view1d_set_range,xmin,xmax,no
print,xmin,xmax,no
dx = (xmax-xmin)/no
xmin = xmin - 0.5 * dx
xmax = xmax + 0.5 * dx
i1 = fix(xmin/dx) - 1
i2 = fix(xmax/dx) + 1
xmin = i1*dx
xmax = i2*dx
print,xmin,xmax,no
END

PRO view1d_adjust_ranges,xmin,xmax
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
 


;
;  increase the xmin,xmax by +5%
;
	if xmax eq xmin then dx = 1. else $
        dx = 0.05 *(xmax-xmin)
        xmax = xmax + dx
        xmin = xmin - dx

	view1d_round_xrange,xmin,xmax

	w_plotspec_limits(0:1) = [xmin,xmax]
END

;
; round the xrange to integer if total width > 5
;
PRO view1d_round_xrange,xmin,xmax
if (xmax - xmin) le 5. then return 
xmax = floor(xmax) + 1
xmin = floor(xmin)

END

PRO view1d_power10_max,x,newx,no
newx = x
if x lt 2. then return
v = fix(x)
;if (x-v) gt 0 then v = v+1

in1 = v / 10 + 1
ir1 = v - in1 *10
p = 1
if ir1 eq 0 then begin
	newx = v
	return
	end

if abs(in1) lt 10 then begin
	newx = in1 * 10^p + (1+ir1) *10^(p-1)
	no = p
	return
	end

REP:
        in2 = in1 /10
	ir2 = in1 - in2 *10
	p = p + 1
	if abs(in2) lt 10 then begin
		newx = in2 * 10^p + (1+ir2) *10^(p-1)
		no = p
		return
		end
	in1 = in2
	ir1 = ir2	
	goto, REP

END

PRO view1d_legends,pos,id1,id,p1,d1,num_pts,x_axis,statis_value

   COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus

	view1d_xdist,(0.01 + pos(2)-pos(0)),xdis	
	view1d_xdist,(0.075+pos(2)-pos(0)),xdis2	

	ino = 5*id1
	ch_ratio = float(!d.y_ch_size) / !d.y_size
	view1d_ydist,(pos(3)-pos(1)-5*id1*ch_ratio),lydis	

	color = w_plotspec_id.colorI(id1)
	; 24 bit visual case
	if !d.n_colors eq 16777216 then begin
		catch1d_get_pvtcolor,color,t_color
		color = t_color
		end
	if !d.name eq 'PS' then color = 0

	line = id1
	if w_plotspec_id.solid eq 1 and !d.name ne 'PS' then line = 0

if w_plotspec_id.type eq 0 then begin
      IF (x_axis EQ 0) THEN OPLOT, p1, d1, color=color, LINE = line , THICK=2 $
      ELSE OPLOT, d1, color=color, LINE = line, THICK=2
	; write legend 
	if w_plotspec_id.log ne 1 then $ 
	oplot,[xdis,xdis2],[lydis,lydis],color=color,LINE=line,/noclip else $
	oplot,[xdis,xdis2],[10^lydis,10^lydis],color=color,LINE=line,/noclip
	xdis = 0.8*!d.x_size
	xdis2 = 0.85*!d.x_size
	ydis = pos(3) * !d.y_size - 5 *id1*!d.y_ch_size
	if id lt 15 then begin
	   if strlen(y_descs(id)) gt 1 then $
		xyouts,xdis2,ydis,'  '+y_descs(id), /device else $
		xyouts,xdis2,ydis,'  Detector '+strtrim(id+1,1), /device
	endif else begin
	   idd = id-15
	   if strlen(x_descs(idd)) gt 1 then $
		xyouts,xdis2,ydis,'  '+x_descs(idd), /device else $
		xyouts,xdis2,ydis,'  Encoder P'+strtrim(idd+1,1), /device
	end
endif else begin
	sym = id1+1
	if w_plotspec_id.type eq 2 then sym = -(id1+1)
		IF (x_axis EQ 0) THEN OPLOT, p1, d1,color=color, PSYM = sym else $
		OPLOT, d1,color=color, PSYM = sym

	if w_plotspec_id.log ne 1 then $ 
		oplot,[xdis,xdis],[lydis,lydis],color=color,PSYM=sym,/noclip else $
		oplot,[xdis,xdis],[10^lydis,10^lydis],color=color,PSYM=sym,/noclip
	; write legend
	xdis = 0.8*!d.x_size
	xdis2 = 0.85*!d.x_size
	ydis = pos(3) * !d.y_size - 5 *id1*!d.y_ch_size
	if id lt 15 then begin
	   if strlen(y_descs(id)) gt 1 then  $
		xyouts,xdis2,ydis,'  '+y_descs(id),/device  else $
		xyouts,xdis2,ydis,'  Detector '+strtrim(id+1,1),/device
	endif else begin
	   idd = id-15
	   if strlen(x_descs(idd)) gt 1 then $
		xyouts,xdis2,ydis,'  '+x_descs(idd), /device else $
		xyouts,xdis2,ydis,'  Encoder P'+strtrim(idd+1,1), /device
	end
end

if w_plotspec_id.errbars eq 1 then begin 
	d_err = sqrt(abs(d1))
	for i=0, num_pts do begin
	x2 = p1(i)
	ny1 = d1(i) - d_err(i)
	ny2 = d1(i) + d_err(i)
      IF (x_axis EQ 0) THEN $
	OPLOT,color=color, [x2,x2],[ny1,ny2] else OPLOT,color=color,[i,i], [ny1,ny2] 
	end
end


if w_plotspec_id.statistic gt 0 then begin

	xpeak = statis_value(0)
	c_mass = statis_value(1)
	FWHM = statis_value(2)
	peak = statis_value(3)

desc_legend = make_array(4,/string)
if w_plotspec_id.statistic eq 3 then begin
	desc_legend(0) = 'Std Dev '
	desc_legend(1) = 'Ave Dev '
	desc_legend(2) = '  Mean  '
	desc_legend(3) = '  Vari  '
endif else begin
	desc_legend(0) = '  Peak @'
	desc_legend(1) = '  Cntr @'
	desc_legend(2) = '  FWHM '
	desc_legend(3) = '  Peak '
end

if n_elements(xpeak) gt 0 then begin
	ydis = pos(3) * !d.y_size - (ino+1)*!d.y_ch_size
	xyouts,xdis,ydis,desc_legend(0)+strtrim(xpeak,1),/device
	end

if n_elements(c_mass) gt 0 then begin
	ydis = pos(3) * !d.y_size - (ino+2)*!d.y_ch_size
	xyouts,xdis,ydis,desc_legend(1)+strtrim(c_mass,1) ,/device 
	end

if n_elements(FWHM) gt 0 then begin
	ydis = pos(3) * !d.y_size - (ino+3)*!d.y_ch_size
	xyouts,xdis,ydis,desc_legend(2)+strtrim(FWHM,1) ,/device
	end

if n_elements(peak) gt 0 then begin
	ydis = pos(3) * !d.y_size - (ino+4)*!d.y_ch_size
	xyouts,xdis,ydis,desc_legend(3)+strtrim(peak,1) ,/device
	end
end

END



PRO w_statistic_event,event
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_statistic_block,w_statistic_ids
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

WIDGET_CONTROL, event.id, GET_UVALUE = eventval
CASE eventval OF
        "STATISTIC_CLOSE" : BEGIN
                WIDGET_CONTROL,event.top,/DESTROY
		widget_ids.statistic = 0L
		w_plotspec_id.statistic = 0
                END
ENDCASE
END


PRO w_statistic, str,width,height,title,quest=quest, GROUP = GROUP
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_statistic_block,w_statistic_ids

if n_elements(str) eq 0 then return
if XRegistered('w_statistic') ne 0 then begin
	WIDGET_CONTROL,w_statistic_ids.base,/DESTROY
	end
if n_elements(width) eq 0 then width = 80
if n_elements(height) eq 0 then height = 5 
if n_elements(title) eq 0 then title=''

w_statistic_base=WIDGET_BASE(TITLE = 'scanSee '+ title, $
	TLB_FRAME_ATTR = 2, $
	 /COLUMN)
w_statistic_title = WIDGET_LABEL(w_statistic_base,VALUE=title)

list = WIDGET_TEXT(w_statistic_base,VALUE=str,UVALUE='LIST', $
	XSIZE =width, $
	YSIZE=height,/SCROLL)

close = WIDGET_BUTTON(w_statistic_base, $
                        VALUE = ' Close ', $
                        UVALUE = 'STATISTIC_CLOSE')

WIDGET_CONTROL, w_statistic_base,/REALIZE, $
	 TLB_SET_XOFFSET= 10, TLB_SET_YOFFSET= 400

widget_ids.statistic = list 
XMANAGER, 'w_statistic',w_statistic_base, GROUP_LEADER = GROUP

w_statistic_ids = { base : w_statistic_base }

END
;
; catch1d_realtime.pro
;
PRO  setScanPvnames,file=file,help=help
COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames

if keyword_set(help) then begin
	st = ["Usage: setScanPvnames,/file", $
	'       This function defines the pvnames monitored by the scan record',$
	'       If file keyword not used, then 4 positioners and 15 detectors assumed', $
	'       If file keyword used, the pvnames are read from the scan_pvnames file']
	w_warningtext,st
	return
	end

filename = scanData.pvfile
if keyword_set(file) then begin
	f = findfile(filename)
	if strlen(f(0)) gt 0 then begin
		readLabelsPvnames,filename,labels,pvnames
		scanData.nonames = n_elements(pvnames)
	       	 realtime_pvnames = make_array(scanData.nonames,/string,value=string(replicate(32b,30)))
		for i=0,scanData.nonames-1 do begin 
			n = strpos(pvnames(i),'.')
			len = strlen(pvnames(i)) - n
			if n ne -1 then realtime_pvnames(i)= scanData.pv+strmid(pvnames(i),n,len) $
				else realtime_pvnames(i)=pvnames(i)
			end
	endif else begin
	w_warningtext,'File '+filename+' not found!'
	return
	end
endif else begin
	scanData.nonames = 19 
	realtime_pvnames = make_array(19,/string,value=string(replicate(32b,5)))
	realtime_pvnames(0)=scanData.pv+'.R1CV'
	realtime_pvnames(1)=scanData.pv+'.R2CV'
	realtime_pvnames(2)=scanData.pv+'.R3CV'
	realtime_pvnames(3)=scanData.pv+'.R4CV'
	realtime_pvnames(4)=scanData.pv+'.D1CV'
	realtime_pvnames(5)=scanData.pv+'.D2CV'
	realtime_pvnames(6)=scanData.pv+'.D3CV'
	realtime_pvnames(7)=scanData.pv+'.D4CV'
	realtime_pvnames(8)=scanData.pv+'.D5CV'
	realtime_pvnames(9)=scanData.pv+'.D6CV'
	realtime_pvnames(10)=scanData.pv+'.D7CV'
	realtime_pvnames(11)=scanData.pv+'.D8CV'
	realtime_pvnames(12)=scanData.pv+'.D9CV'
	realtime_pvnames(13)=scanData.pv+'.DACV'
	realtime_pvnames(14)=scanData.pv+'.DBCV'
	realtime_pvnames(15)=scanData.pv+'.DCCV'
	realtime_pvnames(16)=scanData.pv+'.DDCV'
	realtime_pvnames(17)=scanData.pv+'.DECV'
	realtime_pvnames(18)=scanData.pv+'.DFCV'
	end

;print,'pvnames',realtime_pvnames
END


PRO terminal_dump_header
COMMON CATCH1D_COM, widget_ids, scanData
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
 
; showlist then dump to terminal window

if scanData.showlist eq 1 then begin
	WIDGET_CONTROL,widget_ids.terminal,BAD_ID=bad
	if bad ne 0 then $
		widget_ids.terminal = CW_TERM(widget_ids.base, $
                                        TITLE=scanData.pv, $
;                                        BGROUP_NAMES=names, $
;                                        BGEVENT_FUNCT='CWTERM_event', $
                                        /FRAME, $
                                        XSIZE=100, YSIZE=20, /SCROLL)
	st = "; VERSION: "+scanData.version+' '+scanData.release
	WIDGET_CONTROL,widget_ids.terminal,SET_VALUE=strtrim(st),BAD_ID=bad_id
;	st = "; SCAN #: "+ string(w_plotspec_id.seqno+1)
	st = "; SCAN #: "+ string(scanData.scanno)
	WIDGET_CONTROL,widget_ids.terminal,SET_VALUE=strtrim(st),BAD_ID=bad_id
	st = "; SCAN Record Name: "+scanData.pv
	WIDGET_CONTROL,widget_ids.terminal,SET_VALUE=strtrim(st),BAD_ID=bad_id


	st =';'
	WIDGET_CONTROL,widget_ids.terminal,SET_VALUE=strtrim(st),BAD_ID=bad_id

; find name, desc, engu for defined PiPV & DiPV 

	find_desc_engu,x_dn,descs,engus
	no = n_elements(x_dn)
        st = ';    I   '
        for i=0,no-1 do begin
        if realtime_id.def(i) ne 0 then begin
                st = st+ ' '+x_dn(i)
                end
        end

;	print,st

	WIDGET_CONTROL,widget_ids.terminal,SET_VALUE=strtrim(st),BAD_ID=bad_id

;	s0 = string(replicate(32b,340))
twd = strlen(st) > 18*total(realtime_id.def) + 10
s0 = string(replicate(32b,twd))
	st = s0
	strput,st,';  (Desc)',0  &  ij = 17
	for i=0,no-1 do begin 
	if realtime_id.def(i) ne 0 then begin
		strput,st,descs(i),ij
		ij = ij + 18
		end
	end
;	print,st
	WIDGET_CONTROL,widget_ids.terminal,SET_VALUE=strtrim(st),BAD_ID=bad_id

	st = s0
	strput,st,';  (Units)',0  &  ij = 17
	for i=0,no-1 do begin 
	if realtime_id.def(i) ne 0 then begin
		strput,st,engus(i),ij
		ij = ij + 18
		end
	end
;	print,st
	WIDGET_CONTROL,widget_ids.terminal,SET_VALUE=strtrim(st),BAD_ID=bad_id

end
END

; calling procedure
;	realtime_init
;	realtime_read, npts
;
;
PRO realtime_init
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON field_block, field_name, field_name_array, field_value, w_scanfield_ids

	realtime_id.ind = 0
	realtime_id.no = 0
	realtime_id.axis = 0
	realtime_id.xmin = 0.
	realtime_id.xmax = 0.
	realtime_id.ymin = 0.
	realtime_id.ymax = 0.

if scanData.readpv then setScanPvnames,/file else setScanPvnames

if caSearch(scanData.pv) ne 0 then begin
	w_warningtext,['Error: scan record  '+ scanData.pv + '  not found']
	return
	end

	ln = caget(scanData.pv+'.NPTS',pd)
	ln = caget(scanData.pv+'.MPTS',mpts)
;	ln = cagetArray([scanData.pv+'.NPTS', scanData.pv+'.MPTS'],pd,/short)
;	mpts = pd(1) 
	scanData.req_npts = pd
	realtime_retval = make_array(scanData.req_npts,scanData.nonames,/double)
	realtime_id.mpts = mpts

;if scanData.y_seqno eq 0 then begin
if scanData.realtime eq 0 then begin
	ln = caScan(scanData.pv+'.CPT',realtime_pvnames,/clear)
	ln = caScan(scanData.pv+'.CPT',realtime_pvnames,/add,max=mpts)
	ln = caScan(scanData.pv+'.CPT',realtime_pvnames,scanData.nonames,npts,pd,/get,max=mpts)
	realtime_retval = pd
	scanData.realtime = 1

if scanData.debug eq 1 then $
print,'REALTIME_INIT: add caScan at # ',w_plotspec_id.seqno

scanData.px = make_array(4000,/float)
scanData.pa = make_array(4000,4,/float)
scanData.da = make_array(4000,15,/float)
end
	ln = caScan(scanData.pv+'.CPT',realtime_pvnames,/zero,max=mpts)
	scanData.act_npts = 0
 
;  check for terminal dump

	terminal_dump_header


WSET, widget_ids.plot_area

;    ind = 0 plot the x axis and get monitor queue


if realtime_id.ind eq 0 then begin 
;	tempTitle=strtrim(w_plotspec_array(0))+' (1D SCAN # '+strtrim(w_plotspec_id.seqno+1,2) +')'
	tempTitle=strtrim(w_plotspec_array(0))+' (1D SCAN # '+strtrim(scanData.scanno,2) +')'

	xrange = [0,100]
	realtime_xrange,1,xmin,xmax
	xrange = [xmin,xmax]

	y_range=[w_plotspec_limits(2),w_plotspec_limits(3)]

; Y>0
	if w_plotspec_id.log eq 2 then y_range=[0.,w_plotspec_limits(3)] 
	if w_plotspec_id.log eq 1 then y_range=[0.1,w_plotspec_limits(3)] 

	plot,xrange=xrange, $
		yrange=y_range, $
		title=tempTitle, $
		xtitle= 'P1', $
		xticklen = w_plotspec_id.xticklen, $
		yticklen = w_plotspec_id.yticklen, $
		xgridstyle = w_plotspec_id.gridstyle, $
		ygridstyle = w_plotspec_id.gridstyle, $
		xminor= 10, $
		yminor= 10, $
		ytype = w_plotspec_id.log, $
		/nodata, /xstyle, /ystyle, $
		max_value=0,['1','1']

	realtime_id.ind = 1
	end

x_dv = 0
x_dn = 0
realtime_retval = 0
;	print,caclock()
END

;
;
PRO realtime_read,npts
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames

   WIDGET_CONTROL, widget_ids.wf_select, GET_VALUE = wf_sel

if n_params() eq 0 then begin
	st = ['usage:           realtime_init',$
	'                 realtime_read,  npts',$
	'        where',$
	'	                npts > 1']
	w_warningtext,st
	return
	end

;   ind = 2 scan finished already
if realtime_id.ind eq 2 then return 

symbol = w_plotspec_id.type 
if w_plotspec_id.type eq 2 then symbol = -1
ln_style = intarr(19)
for i=0,18 do begin
	ln_style(i) = i mod 6
	if w_plotspec_id.solid eq 1 then ln_style(i) = 0
end

retval = make_array(scanData.nonames,scanData.req_npts+1);
nonames= scanData.nonames
pts = scanData.req_npts+1
ln = caScan(scanData.pv+'.CPT',realtime_pvnames,nonames,pts,pd,/get,max=realtime_id.mpts)
cpts = pts
if cpts le 1 then return
retval = pd

if cpts le scanData.act_npts  then return

n1 = realtime_id.no
n2 = cpts - 1  
scanData.act_npts = cpts 
if scanData.act_npts eq 0 then return

realtime_retval = transpose(retval)
	for i=0,scanData.num_pos-1 do begin
	is = i*cpts
	scanData.pa(n1:n2,i)	= realtime_retval(n1:n2,i)
	if w_plotspec_id.xcord eq i then  $
		scanData.px(n1:n2) = realtime_retval(n1:n2,i)
	end

	for i=0,scanData.num_det-1 do begin
	is = i*cpts
	scanData.da(n1:n2,i) = realtime_retval(n1:n2,i+scanData.num_pos)
	end


;realtime_retval = 0 

; showlist then dump to terminal window

if scanData.showlist eq 1 then begin
	s0=string(replicate(32b,260))
	for i=n1+1,n2 do begin
	st = s0
	strput,st,i,0  &  ij = 10
	for j=0,scanData.num_pos - 1 do begin
	if realtime_id.def(j) ne 0 then begin 
		strput,st,scanData.pa(i,j),ij  & ij = ij + 13 &end

		end
	for j=0,scanData.num_det - 1 do begin
	if realtime_id.def(4+j) ne 0 then begin 
		strput,st,scanData.da(i,j),ij  & ij = ij + 13 &end
		end
	;print,st
	WIDGET_CONTROL,widget_ids.terminal,SET_VALUE=strtrim(st),BAD_ID=bad_id
	end
end

; detect any change of end point during the middle of scanning

xmin = realtime_id.xmin
xmax = realtime_id.xmax
x_dn = [scanData.pv+'.P1WD',scanData.pv+'.P2WD',scanData.pv+'.P3WD', $
	scanData.pv+'.P4WD', $
	scanData.pv+'.P1PP',scanData.pv+'.P2PP',scanData.pv+'.P3PP', $
	scanData.pv+'.P4PP']
ln = caMonitor(x_dn,ret,/check)
if total(ret) gt 0 then begin
	realtime_xrange,1,xmin,xmax
	realtime_id.axis = 1
	end

; if time axis plot is requested

if realtime_id.def(w_plotspec_id.xcord) gt 1 then begin
	if scanData.px(n2) gt realtime_id.xmax then begin
	dxx = 0.1*(scanData.px(n2)-scanData.px(0))
	xmin = scanData.px(0) - dxx
	xmax = scanData.px(n2) + dxx
	if xmax gt realtime_id.xmax then realtime_id.xmax = xmax
	if xmin lt realtime_id.xmin then realtime_id.xmin = xmin
	realtime_id.axis = 1
	end
end

; if data point as x axis is requested

xtitle = strtrim(w_plotspec_array(1),2)
if w_plotspec_id.x_axis_u eq 1 then begin
	xa = findgen(n2+1) 
	scanData.px(0:n2) = xa
	xmin = - 0.05 * scanData.req_npts 
	xmax = scanData.req_npts *1.05 
	xtitle = 'Step #'
end

realtime_yrange,scanData.lastPlot,ymin,ymax,plotXTitle,pos_ymin
;print,'axis',realtime_id.axis,xmin,xmax,ymin,ymax,plotXTitle

WSET, widget_ids.plot_area

; reset the realtime plot coordinates

;if ymin eq ymax then return
if ymin eq ymax then begin
	ymin = ymin - 5
	ymax = ymax + 5
end 

if realtime_id.axis eq 1 then begin 

;	tempTitle=strtrim(w_plotspec_array(0))+' (1D SCAN # '+strtrim(w_plotspec_id.seqno+1,2) +')'
	tempTitle=strtrim(w_plotspec_array(0))+' (1D SCAN # '+strtrim(scanData.scanno,2) +')'

	y_range = [ymin,ymax]
; Y > 0
	if w_plotspec_id.log eq 2 and ymin lt 0. then  y_range=[0.,ymax]
	if w_plotspec_id.log eq 1 then begin
		if ymin le 0. then ymin = pos_ymin
		y_range=[ymin,ymax*10]
		end

	if w_plotspec_id.log eq 0 or w_plotspec_id.log eq 2 then $
	plot,xrange=[xmin,xmax], $
		yrange=y_range, $
		title=tempTitle, $
		xtitle=xtitle, $
		xticklen = w_plotspec_id.xticklen, $
		yticklen = w_plotspec_id.yticklen, $
		xgridstyle = w_plotspec_id.gridstyle, $
		ygridstyle = w_plotspec_id.gridstyle, $
		xminor= 10, $
		yminor= 10, $
		/nodata, /xstyle, /ystyle, $
		max_value=0,['1','1'] $

	else $
	plot,xrange=[xmin,xmax], $
		yrange=y_range, $
		title=tempTitle, $
		xtitle=xtitle, $
		xticklen = w_plotspec_id.xticklen, $
		yticklen = w_plotspec_id.yticklen, $
		xgridstyle = w_plotspec_id.gridstyle, $
		ygridstyle = w_plotspec_id.gridstyle, $
		xminor= 10, $
	;	ytype = w_plotspec_id.log, $
		/ylog, /nodata, /xstyle, /ystyle, $
		max_value=0,['1','1']


	if n1 gt 0  then begin
	; plot Detector vs positioner 
	for i=0,scanData.num_det - 1 do begin
	if realtime_id.def(4+i) ne 0 and wf_sel(i) eq 1 then begin
	color = w_plotspec_id.colorI(i)
	; 24 bit visual case
	if !d.n_colors eq 16777216 then begin
		catch1d_get_pvtcolor,color,t_color
		color = t_color
		end
		oplot,scanData.px(0:n1), scanData.da(0:n1,i),LINE=ln_style(i), $
			PSYM = symbol * (i+1) mod 8, $
			COLOR=color
		end
	end
	; plot positoner vs positioner (encode cases)
	for i=0,scanData.num_pos - 1 do begin
	if realtime_id.def(i) ne 0 and wf_sel(15+i) eq 1 then begin
	color = w_plotspec_id.colorI(15+i)
	; 24 bit visual case
	if !d.n_colors eq 16777216 then begin
		catch1d_get_pvtcolor,color,t_color
		color = t_color
		end
	oplot,scanData.px(0:n1), scanData.pa(0:n1,i),LINE=ln_style(i+15), $
			PSYM = symbol * (i+1) mod 8, $
			COLOR=color
		end
	end
	realtime_id.axis = 0
	end
end

if n2 ge n1 then begin
for i=0,scanData.num_det-1 do begin
	if realtime_id.def(4+i) ne 0 then begin
		if n2 eq 0 then begin
		xtemp = [scanData.px(0),scanData.px(0)]
		ytemp = [scanData.da(0,i),scanData.da(0,i)]
		endif else begin
		xtemp = [scanData.px(n1:n2)]
		ytemp = [scanData.da(n1:n2,i)]
		end
		if wf_sel(i) eq 1 then begin
	color = w_plotspec_id.colorI(i)
	; 24 bit visual case
	if !d.n_colors eq 16777216 then begin
		catch1d_get_pvtcolor,color,t_color
		color = t_color
		end
			 oplot,xtemp, ytemp,LINE=ln_style(i), $
			PSYM = symbol * (i+1) mod 8, $
			 COLOR=color
		end
	end
end
for i=0,scanData.num_pos-1 do begin
	if realtime_id.def(i) ne 0 then begin
		if n2 eq 0 then begin
		xtemp = [scanData.px(0),scanData.px(0)]
		ytemp = [scanData.pa(0,i),scanData.pa(0,i)]
		endif else begin
		xtemp = [scanData.px(n1:n2)]
		ytemp = [scanData.pa(n1:n2,i)]
		end
		if wf_sel(i+15) eq 1 then begin
	color = w_plotspec_id.colorI(i+15)
	; 24 bit visual case
	if !d.n_colors eq 16777216 then begin
		catch1d_get_pvtcolor,color,t_color
		color = t_color
		end
			oplot,xtemp, ytemp,LINE=ln_style(i+15), $
			PSYM = symbol * (i+1) mod 8, $
			COLOR=color
		end
	end
end
end

xtemp=0
ytemp=0

realtime_id.no = n2 
if (n2+1) ge npts then begin
	realtime_id.ind = 2
;	print,'caclock',caclock()
	end

END


PRO realtime_yrange,auto,ymin,ymax,plotXTitle,pos_ymin

   COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames

   WIDGET_CONTROL, widget_ids.wf_select, GET_VALUE = wf_sel
   x_axis = w_plotspec_id.x_axis_u

   num_pts = 1 > (scanData.act_npts-1)

   ;extract valid data from global arrays

   ;remember the state of "auto" for next time
   scanData.lastPlot = auto

!y.style = 1
!x.style = 1

   IF (auto EQ 1) THEN BEGIN
;!y.style = 2
;!x.style = 2
     ;if autoscale, determine appropriate Y values
     ;depending on which waveforms will be plotted
     ymin = realtime_id.ymin
     ymax = realtime_id.ymax
     if total(wf_sel) eq 0 then plotXTitle = 'Nothing Selected'

pos_ymin = 1.e20
for i=0,scanData.num_det-1 do begin
     IF (wf_sel(i) EQ 1 and realtime_id.def(scanData.num_pos+i) NE 0) THEN  BEGIN
     d4 = scanData.da(0:num_pts,i)
         IF (MIN(d4) LT ymin) THEN begin 
		ymin = MIN(d4)
                realtime_id.axis = 1
		end
         IF (MAX(d4) GT ymax) THEN begin
		ymax = MAX(d4)
                realtime_id.axis = 1
		end
	if w_plotspec_id.log eq 1 and ymin le 0. then begin
	for j=0,num_pts-1 do begin
		if d4(j) gt 0. and d4(j) lt pos_ymin then pos_ymin = d4(j)
	end
	end
     END
end

; if Pi to be plotted as Y

for i=0,scanData.num_pos -1 do begin
	IF(wf_sel(scanData.num_det+i) eq 1 and realtime_id.def(i) NE 0) THEN BEGIN
	d4 = scanData.pa(0:num_pts,i)
	if min(d4) lt ymin then begin
		ymin = min(d4)
		realtime_id.axis = 1
		end
	if max(d4) gt ymax then begin
		ymax = max(d4)
		realtime_id.axis = 1
		end
	if w_plotspec_id.log eq 1 and ymin le 0. then begin
	for j=0,num_pts-1 do begin
		if d4(j) gt 0. and d4(j) lt pos_ymin then pos_ymin = d4(j)
	end
	end
	END
end

;
;  increase the ymin,ymax by +5%
;
if realtime_id.axis eq 1 then begin
	if ymax gt realtime_id.ymax or ymin lt realtime_id.ymin then begin
	dy = 0.1 *(ymax-ymin)
	ymax = ymax + dy
	ymin = ymin - dy
	end
	end

   ; if not autoscale, get limits from entry widgets.
   ENDIF ELSE BEGIN
ymin = w_plotspec_limits(2)
ymax = w_plotspec_limits(3)
   ENDELSE
     
realtime_id.ymin = ymin
realtime_id.ymax = ymax

     ;now determine xaxis label 
     IF (x_axis EQ 0) THEN BEGIN
       plotXTitle = 'P' + strtrim(w_plotspec_id.xcord+1)
     ENDIF ELSE BEGIN
       plotXTitle = 'Step #'
     ENDELSE


END

PRO realtime_xrange,auto,xmin,xmax

   COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames

   WIDGET_CONTROL, widget_ids.wf_select, GET_VALUE = wf_sel
   x_axis = w_plotspec_id.x_axis_u

   ;remember the state of "auto" for next time
   scanData.lastPlot = auto

; if the time axis is selected for plot for real time

if realtime_id.def(w_plotspec_id.xcord) eq 2 then begin

        x_rn = realtime_pvnames

        xmax = realtime_id.xmax
        ln = caget(x_rn(w_plotspec_id.xcord),pd)
        if ln eq 0 and pd gt xmax then begin
                xmin=0
                xmax=pd
                dx = 0.1 * pd
                realtime_id.xmax = xmax + dx
                realtime_id.xmin = xmin - dx
                realtime_id.axis = 1
        end
;ln = cagetTypeCount(x_rn(w_plotspec_id.xcord),ty,ct,wty)
;print,pd,ty(0),ct(0),wty(0)
        return

endif else begin

; not time axis case

CASE  w_plotspec_id.xcord OF
0: begin
	x_dn = [scanData.pv+'.P1SP', scanData.pv+'.P1EP', scanData.pv+'.P1CV', $
		scanData.pv+'.P1SM', scanData.pv+'.P1AR', scanData.pv+'.P1PP']
   end
1: begin
	x_dn = [scanData.pv+'.P2SP', scanData.pv+'.P2EP', scanData.pv+'.P2CV', $
		scanData.pv+'.P2SM', scanData.pv+'.P2AR', scanData.pv+'.P2PP']
   end
2: begin
	x_dn = [scanData.pv+'.P3SP', scanData.pv+'.P3EP', scanData.pv+'.P3CV', $
		scanData.pv+'.P3SM', scanData.pv+'.P3AR', scanData.pv+'.P3PP']
   end
3: begin
	x_dn = [scanData.pv+'.P4SP', scanData.pv+'.P4EP', scanData.pv+'.P4CV', $
		scanData.pv+'.P4SM', scanData.pv+'.P4AR', scanData.pv+'.P4PP']
   end
ELSE: w_warningtext,'w_plotspec_id.xcord is an illegal value.'
ENDCASE
	ln = cagetArray(x_dn,pd)
	x_dv = pd

; On the fly mode
	if x_dv(3) eq 2. then begin
	if x_dv(4) gt 0. then begin
	xmin = x_dv(0) + x_dv(5)
	xmax = x_dv(1) + x_dv(5)
	endif else begin
	xmin = x_dv(0)
	xmax = x_dv(1)
	end
	end


; linear mode
	if x_dv(3) eq 0. then begin
	if x_dv(4) gt 0. then begin
    ;   relative mode
;	xmin = x_dv(2) 
;	if x_dv(2) gt 0 and x_dv(5)  gt x_dv(2) then xmin = x_dv(5)
;	if x_dv(2) lt 0 and x_dv(5) lt x_dv(2) then xmin = x_dv(5) 
;	xmax = xmin + x_dv(1)-x_dv(0)

	xmin = x_dv(0)+x_dv(5)
	xmax = x_dv(1)+x_dv(5)
	if x_dv(0) gt x_dv(1) then begin
		xtemp = xmax
		xmax = xmin
		xmin = xtemp
		end
	endif else begin
    ;   absolute mode
	xmin = x_dv(0)
	xmax = x_dv(1)
	end
	end

; table mode
	if x_dv(3) eq 1. then begin
	CASE w_plotspec_id.xcord OF
	0: x_dn = scanData.pv+'.P1PA'
	1: x_dn = scanData.pv+'.P2PA'
	2: x_dn = scanData.pv+'.P3PA'
	3: x_dn = scanData.pv+'.P4PA'
	ENDCASE
;	ln = caget(x_dn, pd, max=scanData.req_npts)
	ln = cagetArray(x_dn, pd, max=scanData.req_npts)
	x = pd
	if x_dv(4) gt 0. then begin
	dx = MAX(x) - MIN(x)
	xmax = MAX(x) + x_dv(2)  
	xmin = xmax - dx
	endif else begin
	xmin = MIN(x)
	xmax = MAX(x)
	end
	end

end
;
;  increase the xmin,xmax by +5%
;
	dx = 0.1 *(xmax-xmin)
	if dx lt 0. then dx = 2.*dx
	xmax = xmax + dx
	xmin = xmin - dx

     
realtime_id.xmin = xmin
realtime_id.xmax = xmax


END

PRO close_plotspec
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved
if XRegistered('w_plotspec') ne 0 then $
	WIDGET_CONTROL,w_plotspec_ids.base,/DESTROY
END

PRO w_plotspec_saveTitle
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved 
		w_plotspec_saved(0) = w_plotspec_array(0)
		w_plotspec_saved(1) = w_plotspec_array(1)
		w_plotspec_saved(2) = w_plotspec_array(2)
END

PRO w_plotspec_restoreTitle
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved 
		w_plotspec_array(0) = w_plotspec_saved(0)
		w_plotspec_array(1) = w_plotspec_saved(1)
		w_plotspec_array(2) = w_plotspec_saved(2)
END


PRO setDefaultLabels
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus

if n_elements(x_names) eq 0 then begin
        x_names=make_array(4,/string,value=string(replicate(32b,30)))
        y_names=make_array(15,/string,value=string(replicate(32b,30)))
        x_descs=make_array(4,/string,value=string(replicate(32b,30)))
        y_descs=make_array(15,/string,value=string(replicate(32b,30)))
        x_engus=make_array(4,/string,value=string(replicate(32b,30)))
        y_engus=make_array(15,/string,value=string(replicate(32b,30)))
	end

if w_plotspec_id.mode eq 0 then begin
if casearch(scanData.pv) eq 0 then begin 
        find_desc_engu,names,descs,engus

        x_names = names(0:3)
        y_names = names(4:18)
        x_descs = descs(0:3)
        y_descs = descs(4:18)
        x_engus = engus(0:3)
        y_engus = engus(4:18)

end
end

END

PRO setPlotLabels
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus

; if w_plotspec is open

; if w_plotspec_id.mode eq 1 then return
if XRegistered('w_plotspec') ne 0 then return
	
        title = w_plotspec_array(0)
        ix = w_plotspec_id.xcord
	t_xlabel = 'P'+strtrim(ix+1,2)

; get title
	st = scanData.pv
	title = string(replicate(32b,60))
        title = st +' ('+ scanData.pv +')'
        if scanData.y_scan then begin
		y_seqno = scanData.y_seqno
		if y_seqno gt 0 and y_seqno eq scanData.y_req_npts then $
			 y_seqno = y_seqno-1
		title = st + ' @ y('+strtrim(y_seqno,2) + ')=' + $
			strtrim(scanData.y_value,2) 
	end

; get xlabel
	xlabel = string(replicate(32b,60))
     if ix lt 4 then begin
	len = strlen(x_descs(ix))
        if len gt 1 then strput,xlabel,x_descs(ix) else $
        	strput,xlabel,x_names(ix)
	if len lt 1 then len = 2
	l2 = strlen(x_engus(ix))
        if l2 gt 1 then begin
		len = len + 2
		strput,xlabel,'(',len
		len = len + 2
		strput,xlabel,strtrim(x_engus(ix)),len
		len = len + l2 + 1
		strput,xlabel,')',len
		end
     end
     if ix ge 4 then begin   ; if detector for x axis
	ixx = ix - 4
	len = strlen(y_descs(ixx))
        if len gt 1 then strput,xlabel,y_descs(ixx) else $
        	strput,xlabel,y_names(ixx+1,2)
	if len lt 1 then len = 2
	l2 = strlen(y_engus(ixx))
        if l2 gt 1 then begin
		len = len + 2
		strput,xlabel,'(',len
		len = len + 2
		strput,xlabel,strtrim(y_engus(ixx)),len
		len = len + l2 + 1
		strput,xlabel,')',len
		end
     end

	if strtrim(title,2) ne '' then w_plotspec_array(0) = title
	if strtrim(xlabel,2) ne '' then w_plotspec_array(1) = xlabel else $
		w_plotspec_array(1) = t_xlabel
;print,'TITLE:',title
;print,'XLABEL:',xlabel

END




PRO find_desc_engu,names,descs,engus
; RETURN:
;       names - scan record Pi,Di PV names (4 positioners and 15 detectors)
;       descs - corresponding descs from the  database
;       engus - corresponding Pi, Di engu from the database
;
COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON field_block, field_name, field_name_array, field_value, w_scanfield_ids
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus

names= make_array(19,/string,value=string(replicate(32b,30)))
descs = make_array(19,/string,value=string(replicate(32b,30)))
engus = make_array(19,/string,value=string(replicate(32b,30)))

if casearch(scanData.pv) eq 0 then begin 
scan_field_get,scanData.pv

; check for defined PiPV & DiPV
realtime_id.def = make_array(19,/int)
; 4 positioner + 15 detectors

x_dn = [ scanData.pv+'.R1PV', $
	scanData.pv+'.R2PV', $
	scanData.pv+'.R3PV', $
	scanData.pv+'.R4PV' ]
ln = cagetArray(x_dn,p1,/string)
x_dn = [ scanData.pv+'.P1PV', $
	scanData.pv+'.P2PV', $
	scanData.pv+'.P3PV', $
	scanData.pv+'.P4PV' ]
ln = cagetArray(x_dn,p2,/string)

p1 = strtrim(p1,2)
p2 = strtrim(p2,2)
for i=0,3 do begin
	if strlen(p1(i)) eq 0 then p1(i) = p2(i)
end
 
x_dn = [ scanData.pv+'.D1PV', $
	scanData.pv+'.D2PV', $
	scanData.pv+'.D3PV', $
	scanData.pv+'.D4PV', $
	scanData.pv+'.D5PV', $
	scanData.pv+'.D6PV', $
	scanData.pv+'.D7PV', $
	scanData.pv+'.D8PV', $
	scanData.pv+'.D9PV', $
	scanData.pv+'.DAPV', $
	scanData.pv+'.DBPV', $
	scanData.pv+'.DCPV', $
	scanData.pv+'.DDPV', $
	scanData.pv+'.DEPV', $
	scanData.pv+'.DFPV' $
	]
ln = cagetArray(x_dn,pd,/string)
x_dv = strtrim(pd,2)
names(0:3) = p1
names(4:18) = x_dv

; get desc & eng units
 
s0=string(replicate(32b,30))

for i=0,18 do begin
if strlen(names(i)) gt 1 then begin
 
        realtime_id.def(i) = 1
        id = strpos(names(i),'.',0)
 
	v=s0
        if id ne -1 then strput,v,strmid(names(i),0,id),0 else $
		strput,v,names(i),0
	vd = strcompress(v + '.DESC',/remove_all)
	pd=''
	ln = caget(vd,pd)
	descs(i) = pd
        if strtrim(descs(i),2) eq '-1' then descs(i)=''
        end
end

x_dn = [ scanData.pv+'.P1EU', $
	scanData.pv+'.P2EU', $
	scanData.pv+'.P3EU', $
	scanData.pv+'.P4EU', $
	scanData.pv+'.D1EU', $
	scanData.pv+'.D2EU', $
	scanData.pv+'.D3EU', $
	scanData.pv+'.D4EU', $
	scanData.pv+'.D5EU', $
	scanData.pv+'.D6EU', $
	scanData.pv+'.D7EU', $
	scanData.pv+'.D8EU', $
	scanData.pv+'.D9EU', $
	scanData.pv+'.DAEU', $
	scanData.pv+'.DBEU', $
	scanData.pv+'.DCEU', $
	scanData.pv+'.DDEU', $
	scanData.pv+'.DEEU', $
	scanData.pv+'.DFEU' $
	]
	ln = cagetArray(x_dn,pd,/string)
	x_dv = pd
	for i=0,18 do begin
        engus(i) = strtrim(x_dv(i),2)
        if strtrim(engus(i),2) eq '-1' then engus(i)=''
	end
 
; check whether time array to be used for one of the positioner

dd = [scanData.pv+'.R1PV',scanData.pv+'.R2PV',scanData.pv+'.R3PV', $
	scanData.pv+'.R4PV']
ln = cagetArray(dd,ptime,/string)
if ln eq 0 then begin
for i=0,3 do begin
	if strlen(ptime(i)) gt 1 and strpos("TIMEtimeTime",ptime(i)) ne -1 then begin	
		 names(i) = ptime(i)
		 descs(i) = 'Time'
		 engus(i) = 'sec'
		 realtime_id.def(i) = 2		; time second used 
		end
end
end

; need redefine the realtime_id.def for the case when MCA array is 
;      entered in the DiPV for scan record
;
end
; need check the case when readback PV name is non zero case======

END

PRO w_plotspec_event,event
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON PLOTMENU_OPTION_BLOCK,ids,r_names        ; update plotoption menu
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvname


WIDGET_CONTROL, event.id, GET_UVALUE = eventval

if w_plotspec_id.scan eq 0 and realtime_id.ind eq -1 then $
  scanData.act_npts = scanData.readin_npts 

CASE eventval OF
	"PLOT_TITLE" : BEGIN
                WIDGET_CONTROL,w_plotspec_ids.title,GET_VALUE=temp
                w_plotspec_array(0) = strcompress(temp(0))
		END
	"PLOT_XTITLE" : BEGIN
                WIDGET_CONTROL,w_plotspec_ids.xtitle,GET_VALUE=temp
                w_plotspec_array(1) = strcompress(temp(0))
		END
	"PLOT_YTITLE" : BEGIN
                WIDGET_CONTROL,w_plotspec_ids.ytitle,GET_VALUE=temp
                w_plotspec_array(2) = strcompress(temp(0))
		END
	"PLOT_SAVENAME" : BEGIN
                WIDGET_CONTROL,w_plotspec_ids.savename,GET_VALUE=temp
                w_plotspec_array(3) = strcompress(temp(0),/remove_all)
		END
	"PLOT_FOOTER" : BEGIN
                WIDGET_CONTROL,w_plotspec_ids.footer,GET_VALUE=temp
                w_plotspec_array(5) = strcompress(temp(0))
		END
;	"PLOT_RANGES" : BEGIN
;		user_scale,GROUP=event.top
;		END
        "PLOTSPEC_OK" : BEGIN
                WIDGET_CONTROL,w_plotspec_ids.title,GET_VALUE=temp
                w_plotspec_array(0) = strcompress(temp(0))
                WIDGET_CONTROL,w_plotspec_ids.xtitle,GET_VALUE=temp
                w_plotspec_array(1) = strcompress(temp(0))
                WIDGET_CONTROL,w_plotspec_ids.ytitle,GET_VALUE=temp
                w_plotspec_array(2) = strcompress(temp(0))
                WIDGET_CONTROL,w_plotspec_ids.savename,GET_VALUE=temp
                w_plotspec_array(3) = strcompress(temp(0),/remove_all)
                WIDGET_CONTROL,w_plotspec_ids.footer,GET_VALUE=temp
                w_plotspec_array(5) = strcompress(temp(0))
                END
	"PLOTSPEC_DONE" : BEGIN
		WIDGET_CONTROL,w_plotspec_ids.title,GET_VALUE=temp
		w_plotspec_array(0) = strcompress(temp(0))
		WIDGET_CONTROL,w_plotspec_ids.xtitle,GET_VALUE=temp
		w_plotspec_array(1) = strcompress(temp(0))
		WIDGET_CONTROL,w_plotspec_ids.ytitle,GET_VALUE=temp
		w_plotspec_array(2) = strcompress(temp(0))
		WIDGET_CONTROL,w_plotspec_ids.savename,GET_VALUE=temp
		w_plotspec_array(3) = strcompress(temp(0),/remove_all)
                WIDGET_CONTROL,w_plotspec_ids.footer,GET_VALUE=temp
                w_plotspec_array(5) = strcompress(temp(0))
		WIDGET_CONTROL, event.top, /DESTROY
		return
                END
	"PLOTSPEC_CANCEL" : BEGIN
		WIDGET_CONTROL, event.top, /DESTROY
		return
		END
ENDCASE

if realtime_id.ind eq 1 then begin
	realtime_id.axis = 1
endif else $
	UPDATE_PLOT,scanData.lastPlot

END

PRO w_plotspec, GROUP = GROUP, help=help
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved 
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

if XRegistered('w_plotspec') ne 0 then return

if n_elements(w_plotspec_array) eq 0 then $
w_plotspec_array = make_array(6,/string,value=string(replicate(32b,60)))
	
if n_elements(w_plotspec_limits) eq 0 then begin
	w_plotspec_limits = make_array(4,/float)
	w_plotspec_limits = [0., 100., 0., 100.]
	end


if strlen(strcompress(w_plotspec_array(3),/remove_all)) lt 1 then $
	w_plotspec_array(3) = 'catch1d.trashcan'

w_plotspec_base=WIDGET_BASE(TITLE = 'Plot Labels ... ', /COLUMN)     

row0 = WIDGET_BASE(w_plotspec_base, /ROW)

seqno_lb = WIDGET_LABEL(row0, VALUE='Scan #: ' + $
	 strcompress(w_plotspec_id.seqno + 1))

;limits_lb = WIDGET_BUTTON(row0, VALUE='Set User Scale ...', $
;		UVALUE= 'PLOT_RANGES')


row1 = WIDGET_BASE(w_plotspec_base, /ROW)
title_lb = WIDGET_LABEL(row1, VALUE='Title  :')
w_plotspec_title = WIDGET_TEXT(row1, /EDITABLE,  /NO_NEWLINE, $
	SCR_XSIZE = 300, $
        XSIZE=60, YSIZE=1, VALUE=strtrim(w_plotspec_array(0)), UVALUE='PLOT_TITLE')

row2 = WIDGET_BASE(w_plotspec_base, /ROW)
xtitle_lb = WIDGET_LABEL(row2, VALUE='X Label:')
w_plotspec_xtitle = WIDGET_TEXT(row2, /EDITABLE,  /NO_NEWLINE, $
	SCR_XSIZE = 300, $
        XSIZE=60, YSIZE=1, VALUE=strtrim(w_plotspec_array(1)), UVALUE='PLOT_XTITLE')

row3 = WIDGET_BASE(w_plotspec_base, /ROW)
ytitle_lb = WIDGET_LABEL(row3, VALUE='Y Label:')
w_plotspec_ytitle = WIDGET_TEXT(row3, /EDITABLE,  /NO_NEWLINE, $
	SCR_XSIZE = 300, $
        XSIZE=60, YSIZE=1, VALUE=strtrim(w_plotspec_array(2)), UVALUE='PLOT_YTITLE')

row4 = WIDGET_BASE(w_plotspec_base, /ROW)
savefile_lb = WIDGET_LABEL(row4, VALUE='Scan Data Saved in:   ')
w_plotspec_savename = WIDGET_LABEL(row4, VALUE=strtrim(w_plotspec_array(3)) )

row4_1 = WIDGET_BASE(w_plotspec_base, /ROW)
savefile_lb = WIDGET_LABEL(row4_1, VALUE='Comment:')
w_plotspec_footer = WIDGET_TEXT(row4_1, /EDITABLE,  /NO_NEWLINE, $
	SCR_XSIZE = 300, $
        XSIZE=60, YSIZE=1, VALUE=strtrim(w_plotspec_array(5)), UVALUE='PLOT_FOOTER')


lastrow = WIDGET_BASE(w_plotspec_base, /ROW)

w_plotspec_ok = WIDGET_BUTTON(lastrow, $
                        VALUE = ' Apply ', $
                        UVALUE = 'PLOTSPEC_OK')
w_plotspec_cancel = WIDGET_BUTTON(lastrow, $
                        VALUE = 'Cancel', $
                        UVALUE = 'PLOTSPEC_CANCEL')
w_plotspec_done = WIDGET_BUTTON(lastrow, $
                        VALUE = ' Done ', $
                        UVALUE = 'PLOTSPEC_DONE')

; set widget ids :
w_plotspec_ids = { $
	base:	w_plotspec_base, $
	title:  w_plotspec_title, $
	xtitle:  w_plotspec_xtitle, $
	ytitle:  w_plotspec_ytitle, $
	savename:  w_plotspec_savename, $
	footer:  w_plotspec_footer $
	 }

; Realize the widgets:
WIDGET_CONTROL, w_plotspec_base, /REALIZE
if w_plotspec_id.realtime eq 0 then $
WIDGET_CONTROL, w_plotspec_dtime,SENSITIVE=0 

; Hand off to the XMANAGER:
XMANAGER, 'w_plotspec', w_plotspec_base, GROUP_LEADER = GROUP

END


 


PRO read_desc_engu,labels
COMMON LABEL_BLOCK,x_names,y_names,x_descs,y_descs,x_engus,y_engus

if n_elements(x_names) lt 4 then begin
	x_names = make_array(4,/string,value=string(replicate(32b,30)))
	x_descs = x_names
	x_engus = x_names
	y_names = make_array(15,/string,value=string(replicate(32b,30)))
	y_descs = y_names
	y_engus = y_names
	end
labels = string(labels)
for i=0,3 do begin 
	x_names(i) = labels(i)
	x_descs(i) = labels(i+19)
	x_engus(i) = labels(i+38)
end
for i=0,14 do begin 
	y_names(i) = labels(i+4)
	y_descs(i) = labels(i+4+19)
	y_engus(i) = labels(i+4+38)
end

END


; 
; save ascii file of curr scan record 
; 
PRO save_scan_dump_curr,filename 
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits , w_plotspec_saved

filename = strcompress(w_plotspec_array(3),/remove_all)+'.tmp'
openw,unit,filename,/get_lun

shortreport_data_dump,unit
free_lun, unit

END

; 
; save ascii file of a scan record 
; 
PRO save_scan_dump,filename 
COMMON CATCH1D_COM, widget_ids, scanData 
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits , w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames

filename = strcompress(w_plotspec_array(3),/remove_all)+'.tmp'

CATCH,error_status
if error_status lt 0 then begin
	w_warningtext,'Error: '+ !err_string
	return
	end

openw,unit,filename,/get_lun
printf,unit,"; VERSION: ",scanData.version,' ',scanData.release

if scanData.y_seqno gt 0 or scanData.y_scan gt 0 then begin
printf,unit,"; 2D SCAN #: ",scanData.scanno_2d,",      Y INDEX #:",scanData.y_seqno
	end
printf,unit,"; 1D SCAN #: ",w_plotspec_id.seqno + 1 
printf,unit,"; SCAN Record Name: ",scanData.pv

printf,unit,'; '
printf,unit,"; PLOT SPECIFICATIONS"
printf,unit,'; '
printf,unit,"; Title:      ",w_plotspec_array(0)
printf,unit,"; X Label:    ",w_plotspec_array(1)
printf,unit,"; Y Label:    ",w_plotspec_array(2)
printf,unit,"; Saved in:   ",scanData.path+w_plotspec_array(3)
printf,unit,"; Time Stamp: ",w_plotspec_array(4)
printf,unit,"; Comment:    ",w_plotspec_array(5)

if w_plotspec_id.type eq 0 then printf,unit,"; Type:      Line"
if w_plotspec_id.type eq 1 then printf,unit,"; Type:      Point"
if w_plotspec_id.type eq 2 then printf,unit,"; Type:      Line/Point"
if w_plotspec_id.log  eq 0 then printf,unit,"; Y Scale:   Linear"
if w_plotspec_id.log  eq 1 then printf,unit,"; Y Scale:   Log"
if w_plotspec_id.errbars eq 0 then printf,unit,"; Errbars:   Off"
if w_plotspec_id.errbars eq 1 then printf,unit,"; Errbars:   On"
printf,unit,'; Realtime: itime=',w_plotspec_id.itime, ',  dtime=',w_plotspec_id.dtime
printf,unit,'; Plot Vs Position Array # ',w_plotspec_id.xcord + 1

printf,unit,'; '
printf,unit,'; SCAN Data:'
printf,unit,'; '

 save_data_subset_dump, unit
free_lun,unit
return
END


PRO save_data_subset_dump,unit 
COMMON CATCH1D_COM, widget_ids, scanData 
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON LABEL_BLOCK,x_names,y_names,x_descs,y_descs,x_engus,y_engus

; get desc & eng units

if w_plotspec_id.mode eq 0 and  casearch(scanData.pv) eq 0 then begin
	find_desc_engu,names,descs,engus
endif else begin    ; viewing mode
	names = [x_names,y_names]
	descs = [x_descs,y_descs]
	engus = [x_engus,y_engus]
end

	no = n_elements(names)
	st = ';    I   '
        for i=0,no-1 do begin
        if realtime_id.def(i) ne 0 then begin
                st = st+ ' '+names(i)
                end
        end
	printf,unit,st

;s0 = string(replicate(32b,340))
twd = strlen(st) > 18*total(realtime_id.def) + 10
s0 = string(replicate(32b,twd))
st = s0
strput,st,';  (Desc)',0  &  ij = 17 
        for i=0,no-1 do begin
        if realtime_id.def(i) ne 0 then begin
                strput,st,descs(i),ij
                ij = ij + 18
                end
        end
printf,unit,st

st = s0
strput,st,'; (Units)',0  &  ij = 17 
        for i=0,no-1 do begin
        if realtime_id.def(i) ne 0 then begin
                strput,st,engus(i),ij
                ij = ij + 18
                end
        end
printf,unit,st

num_npts = scanData.readin_npts

temp_format = '('+scanData.code+scanData.format+')'
temp_digit = fix(scanData.format)

for i=0,num_npts-1 do begin
st = s0
strput,st,i,0  &  ij = 10
	for j = 0,3 do begin
		if realtime_id.def(j) ne 0 then begin
		strput,st,string(scanData.pa(i,j),format=temp_format),ij  
		ij = ij + temp_digit
		end
	end
	for j = 0,14 do begin
		if realtime_id.def(4+j) ne 0 then begin
		strput,st,string(scanData.da(i,j),format=temp_format),ij  
		ij = ij + temp_digit
		end
	end
printf,unit,st
end

END


PRO w_viewscan_event,event
COMMON SYSTEM_BLOCK,OS_SYSTEM
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

WIDGET_CONTROL, event.id, GET_UVALUE = eventval

CASE eventval OF

	"VIEWSPEC_SEQNO" : BEGIN
		WIDGET_CONTROL,w_viewscan_ids.seqno,GET_VALUE=seqno
		i1 = fix(seqno(0))
	if i1 lt 1  or i1 gt w_viewscan_id.maxno then begin
	w_warningtext,'Input out of range !!'
	return
	end
		if i1 ge 1 then begin 
		i2 = i1 + 1
		scan_read,w_viewscan_id.unit, i1, i2
		WIDGET_CONTROL,w_viewscan_ids.plotspec,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.field,SENSITIVE=1
;		WIDGET_CONTROL,w_viewscan_ids.ascii,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.print,SENSITIVE=0
		end
                END
	"VIEWSPEC_FORMAT" : BEGIN
	        WIDGET_CONTROL,w_viewscan_ids.format, GET_VALUE=format
	       	format = strcompress(format(0),/remove_all)
	       	scanData.code = strmid(format,0,1)
	        scanData.format = strmid(format,1,10)
		ret = strpos('defgDEFG',scanData.code)
		if ret eq -1 then w_warningtext,'Error:   illegal format entered !!!'
		WIDGET_CONTROL,w_viewscan_ids.print,SENSITIVE=0
		END
	"VIEWSPEC_PSPRINT" : BEGIN
        	if scanData.lastPlot lt 0 then return
		scanData.act_npts = scanData.readin_npts
        	PS_open,'catch1d.ps'
        	UPDATE_PLOT, scanData.lastPlot
        	PS_close
        	PS_print,'catch1d.ps'
       		END
	"VIEWSPEC_NEW" : BEGIN
		w_plotspec, GROUP=event.top
		END
	"VIEWSPEC_FIELD" : BEGIN
		save_scan_dump,filename
		WIDGET_CONTROL,w_viewscan_ids.print,SENSITIVE=1
		xdisplayfile,filename,width=110,GROUP= event.top
		END
	"VIEWSPEC_PRINT" : BEGIN
		filename = strcompress(w_plotspec_array(3),/remove_all)+'.tmp'
		PS_enscript,filename
		END
	"VIEWSPEC_ASCII" : BEGIN
		save_scan_dump,file1
filename = w_plotspec_array(3)+'.'+ string(w_plotspec_id.seqno + 1)
filename = strcompress(filename,/remove_all)
if OS_SYSTEM.os_family eq 'unix' then spawn,[OS_SYSTEM.mv, file1, filename],/noshell $
	else spawn,[OS_SYSTEM.mv, file1, filename]

		print,'ASCII SCAN data saved in : ',filename
		END
	"VIEWSPEC_PREV" : BEGIN
		close_plotspec
		w_warningtext_close
		WIDGET_CONTROL,w_viewscan_ids.seqno,GET_VALUE=seqno
		i1 = fix(seqno(0)) - 1
	if i1 lt 1 then i1 = w_viewscan_id.maxno 

		i2 = i1 + 1
		scan_read,w_viewscan_id.unit, i1, i2
		seqno = strtrim(string(i1),2)
		WIDGET_CONTROL,w_viewscan_ids.seqno,SET_VALUE=seqno
		WIDGET_CONTROL,w_viewscan_ids.printplot,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.plotspec,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.field,SENSITIVE=1
;		WIDGET_CONTROL,w_viewscan_ids.ascii,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.print,SENSITIVE=0
                END
	"VIEWSPEC_OK" : BEGIN
		close_plotspec
		w_warningtext_close
		WIDGET_CONTROL,w_viewscan_ids.seqno,GET_VALUE=seqno
		i1 = fix(seqno(0))
	if i1 lt 1  or i1 gt w_viewscan_id.maxno then begin
	w_warningtext,'Input out of range !!'
	return
	end

		i2 = i1 + 1
		scan_read,w_viewscan_id.unit, i1, i2
		WIDGET_CONTROL,w_viewscan_ids.printplot,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.plotspec,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.field,SENSITIVE=1
;		WIDGET_CONTROL,w_viewscan_ids.ascii,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.print,SENSITIVE=0
                END
	"VIEWSPEC_NEXT" : BEGIN
		close_plotspec
		w_warningtext_close
		WIDGET_CONTROL,w_viewscan_ids.seqno,GET_VALUE=seqno
		i1 = fix(seqno(0)) + 1
		if i1 ge (w_viewscan_id.maxno+1) then i1 = 1 
		i2 = i1 + 1

		scan_read,w_viewscan_id.unit, i1, i2 

		seqno = strtrim(string(i1),2)
		WIDGET_CONTROL,w_viewscan_ids.seqno,SET_VALUE=seqno
		WIDGET_CONTROL,w_viewscan_ids.printplot,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.plotspec,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.field,SENSITIVE=1
;		WIDGET_CONTROL,w_viewscan_ids.ascii,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.print,SENSITIVE=0

                END
	"VIEWSPEC_LAST" : BEGIN
		close_plotspec
		w_warningtext_close
		if w_viewscan_id.maxno eq 0 then begin
			w_warningtext,'Error: no data available!'
			return
			end
		i1 = w_viewscan_id.maxno-1
		i2 = i1+1
		scan_read,w_viewscan_id.unit, i2, i2+1
		WIDGET_CONTROL,w_viewscan_ids.seqno,SET_VALUE=strtrim(i2,2)
		WIDGET_CONTROL,w_viewscan_ids.printplot,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.plotspec,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.field,SENSITIVE=1
;		WIDGET_CONTROL,w_viewscan_ids.ascii,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.print,SENSITIVE=0
                END
	"VIEWSPEC_FIRST" : BEGIN
		close_plotspec
		w_warningtext_close
		if w_viewscan_id.maxno eq 0 then begin
			w_warningtext,'Error: no data available!'
			return
			end
		scan_read,w_viewscan_id.unit, 1, 2
		WIDGET_CONTROL,w_viewscan_ids.seqno,SET_VALUE='1'
		WIDGET_CONTROL,w_viewscan_ids.printplot,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.plotspec,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.field,SENSITIVE=1
;		WIDGET_CONTROL,w_viewscan_ids.ascii,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.print,SENSITIVE=0
                END
	"VIEWSPEC_SLIDER" : BEGIN
		close_plotspec
		w_warningtext_close
		WIDGET_CONTROL,w_viewscan_ids.slider,GET_VALUE=seqno
		scan_read,w_viewscan_id.unit,seqno,seqno+1 
		s1 = strtrim(string(seqno),2)
		WIDGET_CONTROL,w_viewscan_ids.seqno,SET_VALUE=s1
		WIDGET_CONTROL,w_viewscan_ids.printplot,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.plotspec,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.field,SENSITIVE=1
;		WIDGET_CONTROL,w_viewscan_ids.ascii,SENSITIVE=1
		WIDGET_CONTROL,w_viewscan_ids.print,SENSITIVE=0
                END
	"VIEWSPEC_CANCEL" : BEGIN
		WIDGET_CONTROL, event.top, /DESTROY
		END
ENDCASE

END

PRO w_viewscan_close, wid
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan

WIDGET_CONTROL,/HOURGLASS

; get the last scan first

if w_viewscan_id.unit gt 0 then begin
	if (w_viewscan_id.seqno+1) lt w_viewscan_id.maxno then begin
		i1 = w_viewscan_id.maxno
		i2 = i1 + 1
		scan_read,w_viewscan_id.unit, i1, i2
	end
end

; close the view mode 
		w_viewscan_id.unit = 0
		w_plotspec_id.opened = 0
		set_sensitive_on

; reset the w_plotspec variable
		w_viewscan_id.maxno = scanData.scanno
		w_plotspec_id = scanData.plotspec 

		w_warningtext_close

		if XRegistered('catcher_setup') ne 0 then $
		WIDGET_CONTROL,catcher_setup_ids.base,SENSITIVE=1

; reset to config pv names
 
		scanData.pv = scanData.pvconfig

 		pventry_event ;======
;		before_sys_scan

		w_plotspec_restoreTitle

	catch1d_check_seqno

scanData.option = 1  ; acquisition on

END

PRO w_viewscan, unit, GROUP = GROUP, help=help

COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON view1d_summary_block, view1d_summary_ids, view1d_summary_id

if XRegistered('w_viewscan') ne 0 then return

scanData.option = 0  ; acquisition off
if XRegistered('view1d_summary_setup') ne 0 then $
	WIDGET_CONTROL,view1d_summary_ids.base,/DESTROY

if scanData.lastPlot lt 0 then scanData.lastPlot = 1  ; autoscale  

if n_params() lt 1 then begin
	w_warningtext,'usage:  w_viewscan, unit, GROUP=event.top'
	end

; reset the pv name in viewing mode and save the config pv for later on
;      scan mode
scanData.pvconfig = scanData.pv  

WIDGET_CONTROL, /HOURGLASS

w_viewscan_id.unit = unit
scan_read,unit,-1,-1,maxno ; scan_read_all,w_viewscan_id.unit,maxno

w_viewscan_base=WIDGET_BASE(GROUP_LEADER=Group, $
	TLB_FRAME_ATTR = 8, $
	TITLE = 'VIEW 1D ... ', /COLUMN)     

w_viewscan_label = WIDGET_LABEL(w_viewscan_base,VALUE='Scan Data from : ' + $
		strcompress(w_plotspec_array(3)))
row0 = WIDGET_BASE(w_viewscan_base, /ROW,/FRAME)

w_viewscan_printplot = WIDGET_BUTTON(row0, $
                        VALUE = 'Print Plot', $
                        UVALUE = 'VIEWSPEC_PSPRINT')
WIDGET_CONTROL,w_viewscan_printplot,SENSITIVE=0

w_viewscan_plotspec = WIDGET_BUTTON(row0, $
                        VALUE = 'Modify Plot', $
                        UVALUE = 'VIEWSPEC_NEW')
WIDGET_CONTROL,w_viewscan_plotspec,SENSITIVE=0
;w_viewscan_ascii = WIDGET_BUTTON(row0, $
;                        VALUE = 'ASCII Save', $
;                        UVALUE = 'VIEWSPEC_ASCII')
;WIDGET_CONTROL,w_viewscan_ascii,SENSITIVE=0
w_viewscan_field = WIDGET_BUTTON(row0, $
                        VALUE = 'ASCII View', $
                        UVALUE = 'VIEWSPEC_FIELD')
WIDGET_CONTROL,w_viewscan_field,SENSITIVE=0
w_viewscan_print = WIDGET_BUTTON(row0, $
                        VALUE = 'ASCII Print', $
                        UVALUE = 'VIEWSPEC_PRINT')
WIDGET_CONTROL,w_viewscan_print,SENSITIVE=0

	str = 'Enter Scan # [ 1 -'+ strcompress(string(maxno)) +' ] '
row1 = WIDGET_BASE(w_viewscan_base, /ROW)
w_viewscan_label = WIDGET_LABEL(row1,VALUE=str)
w_viewscan_seqno = WIDGET_TEXT(row1,VALUE='0', $
		EDITABLE=1, $
		UVALUE='VIEWSPEC_SEQNO', XSIZE = 8)

w_viewscan_format = CW_FIELD( row1,VALUE=scanData.code+scanData.format, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS= 1, $
      TITLE='Column Format: ', $
      XSIZE=8, $
      UVALUE='VIEWSPEC_FORMAT')

; add seqno slider here

if w_viewscan_id.maxno gt 1 then begin
	w_viewscan_slider = WIDGET_SLIDER(w_viewscan_base, $
		MAX=w_viewscan_id.maxno, $
		MIN=1,UVALUE='VIEWSPEC_SLIDER')
	end


lastrow = WIDGET_BASE(w_viewscan_base, /ROW)

;w_viewscan_ok = WIDGET_BUTTON(lastrow, $
;                        VALUE = ' Apply ', $
;                        UVALUE = 'VIEWSPEC_OK')
w_viewscan_first = WIDGET_BUTTON(lastrow, $
                        VALUE = ' First ', $
                        UVALUE = 'VIEWSPEC_FIRST')
w_viewscan_next = WIDGET_BUTTON(lastrow, $
                        VALUE = ' Next ', $
                        UVALUE = 'VIEWSPEC_NEXT')
w_viewscan_next = WIDGET_BUTTON(lastrow, $
                        VALUE = ' Prev ', $
                        UVALUE = 'VIEWSPEC_PREV')
w_viewscan_last = WIDGET_BUTTON(lastrow, $
                        VALUE = ' Last ', $
                        UVALUE = 'VIEWSPEC_LAST')
w_viewscan_cancel = WIDGET_BUTTON(lastrow, $
                        VALUE = ' Done ', $
                        UVALUE = 'VIEWSPEC_CANCEL')

; set widget ids :
w_viewscan_ids = { $
	base:	w_viewscan_base, $
	printplot: w_viewscan_printplot, $
	plotspec: w_viewscan_plotspec, $
	field: w_viewscan_field, $
;	ascii:	w_viewscan_ascii, $
	print:	w_viewscan_print, $
	slider:  0L, $
	seqno:  w_viewscan_seqno, $
	format:  w_viewscan_format $
	 }

if n_elements(w_viewscan_slider) gt 0 then w_viewscan_ids.slider = w_viewscan_slider

WIDGET_CONTROL, widget_ids.wf_select,SENSITIVE=1

; Realize the widgets:
WIDGET_CONTROL, w_viewscan_base, /REALIZE, $
	TLB_SET_XOFFSET= 10, TLB_SET_YOFFSET= 100

; Hand off to the XMANAGER:
XMANAGER, 'w_viewscan', w_viewscan_base, GROUP_LEADER = GROUP, CLEANUP = 'w_viewscan_close'

END



; 
; save data only for curr scan record 
; 
PRO mere_data_dump, unit
COMMON CATCH1D_COM, widget_ids, scanData 
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits , w_plotspec_saved
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids

if scanData.y_seqno gt 0 or scanData.y_scan gt 0 then begin
printf,unit,"; 2D SCAN #: ",scanData.scanno_2d,",      Y INDEX #:",scanData.y_seqno
        end

printf,unit,"; 1D SCAN #: ",scanData.scanno
save_data_subset_dump, unit 
printf,unit,' '

END
; 
; save ascii file of curr scan record 
; 
PRO shortreport_data_dump, unit
COMMON CATCH1D_COM, widget_ids, scanData 
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits , w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids

printf,unit,"; VERSION: ",scanData.version,' ',scanData.release
if scanData.y_seqno gt 0 or scanData.y_scan gt 0 then begin
printf,unit,"; 2D SCAN #: ",scanData.scanno_2d,",      Y INDEX #:",scanData.y_seqno
        end

printf,unit,"; 1D SCAN #: ",scanData.scanno
printf,unit,"; SCAN Record Name: ",scanData.pv



printf,unit,'; '
printf,unit,"; Saved in:   ",scanData.path+w_plotspec_array(3)
printf,unit,"; Time Stamp: ",w_plotspec_array(4)
printf,unit,"; Comment:    ",w_plotspec_array(5)
printf,unit,'; '
printf,unit,'; SCAN Data:'
printf,unit,'; '

save_data_subset_dump, unit 

printf,unit,' '

END

; 
; save ascii file of a scan record 
; 
PRO summary_report_dump,filename,outfile,start,stop,header 
COMMON CATCH1D_COM, widget_ids, scanData 
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits , w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

;tempname = strcompress(w_plotspec_array(3),/remove_all)+'.rep'
tempname=outfile

openw,unit2,tempname,/get_lun 
unit1 = 1
for i=start, stop do begin
	ip = i
	scan_read,unit1,ip,ip	
	if header eq 0 then report_data_dump,unit2
	if header eq 1 then shortreport_data_dump,unit2
	if header eq 2 then mere_data_dump,unit2
end

end_loop:
	u_close,unit2

END


PRO report_data_dump,unit
COMMON CATCH1D_COM, widget_ids, scanData 
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits , w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids


printf,unit,"; VERSION: ",scanData.version,' ',scanData.release
if scanData.y_seqno gt 0 or scanData.y_scan gt 0 then begin
printf,unit,"; 2D SCAN #: ",scanData.scanno_2d,",      Y INDEX #:",scanData.y_seqno
        end

printf,unit,"; 1D SCAN #: ",w_plotspec_id.seqno + 1 
printf,unit,"; SCAN Record Name: ",scanData.pv


printf,unit,'; '
printf,unit,"; PLOT SPECIFICATIONS"
printf,unit,'; '
printf,unit,"; Title:      ",w_plotspec_array(0)
printf,unit,"; X Label:    ",w_plotspec_array(1)
printf,unit,"; Y Label:    ",w_plotspec_array(2)
printf,unit,"; Saved in:   ",scanData.path+w_plotspec_array(3)
printf,unit,"; Time Stamp: ",w_plotspec_array(4)
printf,unit,"; Comment:    ",w_plotspec_array(5)

if w_plotspec_id.type eq 0 then printf,unit,"; Type:      Line"
if w_plotspec_id.type eq 1 then printf,unit,"; Type:      Point"
if w_plotspec_id.type eq 2 then printf,unit,"; Type:      Line/Point"
if w_plotspec_id.log  eq 0 then printf,unit,"; Y Scale:   Linear"
if w_plotspec_id.log  eq 1 then printf,unit,"; Y Scale:   Log"
if w_plotspec_id.errbars eq 0 then printf,unit,"; Errbars:   Off"
if w_plotspec_id.errbars eq 1 then printf,unit,"; Errbars:   On"
printf,unit,'; Realtime: itime=',w_plotspec_id.itime, ',  dtime=',w_plotspec_id.dtime
printf,unit,'; Plot Vs Position Array # ',w_plotspec_id.xcord + 1

printf,unit,'; '
printf,unit,'; SCAN Data:'
printf,unit,'; '

 save_data_subset_dump, unit

printf,unit,' '

END


PRO view1d_summary_setup_Event, Event
COMMON SYSTEM_BLOCK,OS_SYSTEM
COMMON CATCH1D_COM, widget_ids, scanData 
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits , w_plotspec_saved
COMMON view1d_summary_block, view1d_summary_ids, view1d_summary_id

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'summary_header': BEGIN
      CASE Event.Value OF
      0: view1d_summary_id.header = 0
      1: view1d_summary_id.header = 1
      2: view1d_summary_id.header = 2
      ELSE: Message,'Unknown button pressed'
      ENDCASE
      END
  'summary_format': BEGIN
      Print, 'Event for output format'
	WIDGET_CONTROL, view1d_summary_ids.format, GET_VALUE=format
	format = strcompress(format(0),/remove_all)
	scanData.code = strmid(format,0,1)
	scanData.format = strmid(format,1,10)
        ret = strpos('defgDEFG',scanData.code)
       if ret eq -1 then w_warningtext,'Error:   illegal format entered !!!'
	END
  'summary_file': BEGIN
      Print, 'Event for Filename'
	WIDGET_CONTROL, view1d_summary_ids.file, GET_VALUE=file 
	filename=strcompress(file(0),/remove_all)
	found = findfile(filename)
	if found(0) ne '' then begin	
	view1d_summary_id.file = filename

scan_read,unit,-1,-1,maxno ; scan_read_all,unit,maxno
view1d_summary_id.start = maxno
view1d_summary_id.stop = maxno

	WIDGET_CONTROL,view1d_summary_ids.start, $ 
		SET_VALUE=strtrim(view1d_summary_id.start,2)
	WIDGET_CONTROL,view1d_summary_ids.stop, $
		SET_VALUE=strtrim(view1d_summary_id.stop,2)
	report_setup
	outfile = view1d_summary_id.outfile
	WIDGET_CONTROL, view1d_summary_ids.outfile, SET_VALUE= outfile
	end
      END
  'summary_start': BEGIN
	WIDGET_CONTROL,view1d_summary_ids.start, GET_VALUE=start
	if start gt 0 and start le w_viewscan_id.maxno then begin
	view1d_summary_id.start = start
	report_setup
	WIDGET_CONTROL, view1d_summary_ids.outfile, SET_VALUE= view1d_summary_id.outfile
	endif else w_warningtext,['Error: can not exceed '+ string(w_viewscan_id.maxno) ]
      END
  'summary_end': BEGIN
	WIDGET_CONTROL,view1d_summary_ids.stop, GET_VALUE=stop
	if stop gt 0 and stop le w_viewscan_id.maxno then begin
	view1d_summary_id.stop = stop
	report_setup
	WIDGET_CONTROL, view1d_summary_ids.outfile, SET_VALUE= view1d_summary_id.outfile
	endif else begin
	  w_warningtext,['Error: can not exceed '+ string(w_viewscan_id.maxno),$
		'       Reset to '+string(w_viewscan_id.maxno) ]
          WIDGET_CONTROL,view1d_summary_ids.stop, SET_VALUE=w_viewscan_id.maxno
	end
      END
  'summary_separate': BEGIN
	view1d_summary_ids.separate = Event.Index
	END
  'summary_ok': BEGIN
	WIDGET_CONTROL, view1d_summary_ids.format, GET_VALUE=format
	format = strcompress(format(0),/remove_all)
	scanData.code = strmid(format,0,1)
	scanData.format = strmid(format,1,10)

	WIDGET_CONTROL, view1d_summary_ids.view, SENSITIVE = 0 
	WIDGET_CONTROL, view1d_summary_ids.print, SENSITIVE = 0 
	WIDGET_CONTROL,view1d_summary_ids.start, GET_VALUE=start
	WIDGET_CONTROL,view1d_summary_ids.stop, GET_VALUE=stop
	if stop lt start then stop=start
	view1d_summary_id.start = start
	view1d_summary_id.stop = stop

	WIDGET_CONTROL,view1d_summary_ids.file, GET_VALUE=file
	filename=strcompress(file(0),/remove_all)
	found = findfile(filename)
if found(0) ne '' then  begin	
	view1d_summary_id.file = filename
	WIDGET_CONTROL,view1d_summary_ids.outfile, GET_VALUE=file
	view1d_summary_id.outfile=strcompress(file(0),/remove_all)

	;  use the data directory first if failed then home directory

	report_path = scandata.path
	save_outfile = scandata.path+view1d_summary_id.outfile

; quard trashcan
	if save_outfile eq scandata.trashcan then begin
		res = widget_message('Error: illigal file name entered!!',/error)
		return
	end

; quard existing file 
	found = findfile(save_outfile)
	if found(0) ne '' then begin

		st = ['Warning!  Warning!  Warning!  ' , save_outfile, '     already existed.', $
		     ' Is it ok to rename as ', $
		     save_outfile+ '.bk','???']
		res = widget_message(st,/Question)
		if res eq 'No' then goto,view_print
		move_file = save_outfile + '.bk'
deepmove:
		found1 = findfile(move_file)
		if found1(0) ne '' then begin
			st = [' Warning!  Warning!', $
				move_file, $
				'also already existed !!']
			res = widget_message(st,/Question)
			if res eq 'No' then goto,view_print 
			move_file = move_file + '.bk'
			goto,deepmove
		end
		spawn,[OS_SYSTEM.mv, save_outfile, move_file],/noshell 
	end

	CATCH,error_status
	if error_status ne 0 then begin
		report_path = scandata.home + '/'
		save_outfile = report_path+view1d_summary_id.outfile
		goto, RESETSENSE
	end
	openw,unit,save_outfile,/get_lun
	u_close,unit
RESETSENSE:

; save as one big file
    if view1d_summary_ids.separate eq 0 then begin
		w_plotspec_id.mode = 1
		summary_report_dump,filename,save_outfile,start,stop,view1d_summary_id.header
		w_plotspec_id.mode = 0

	if scanData.debug eq 1 then $
		print, 'Report file: ', save_outfile,' created!'
; save as separate files
     endif else begin
	str = '0000'
	len0 = 4 
	w_plotspec_id.mode = 1
	for i=start,stop do begin
	sss = str
	st = strtrim(i,2)
	len = strlen(st)
	strput,sss,st,len0-len
		save_outfile=report_path+w_plotspec_array(3)+'.'+sss
		summary_report_dump,filename,save_outfile,i,i,view1d_summary_id.header
	if scanData.debug eq 1 then print,save_outfile
	w_plotspec_id.mode = 0
	end
     end

endif else w_warningtext,'Error:  Data file " '+filename+' " not found!'
;	WIDGET_CONTROL, view1d_summary_ids.base , /DESTROY
view_print:
	WIDGET_CONTROL, view1d_summary_ids.view, SENSITIVE = 1 
	WIDGET_CONTROL, view1d_summary_ids.print, SENSITIVE = 1 

      END
  'summary_view': BEGIN
	WIDGET_CONTROL,view1d_summary_ids.outfile, GET_VALUE=file
	filename=strcompress(file(0),/remove_all)

	; check the data directory first

	view1d_summary_id.outfile = scanData.path + filename
	found = findfile(view1d_summary_id.outfile)
	if found(0) ne '' then begin
	  xdisplayfile,view1d_summary_id.outfile,width=110,GROUP=event.top 
  		return
 	end

	; check startup directory

	found = findfile(filename)
	if found(0) ne '' then 	$
        xdisplayfile,filename,width=110,GROUP=event.top else $
	w_warningtext,['Error:','    '+filename+ '  not found!']
	END

  'summary_print': BEGIN

	WIDGET_CONTROL,view1d_summary_ids.outfile, GET_VALUE=file
	filename=strcompress(file(0),/remove_all)

	; check the data directory first

	view1d_summary_id.outfile = scanData.path + filename
	found = findfile(view1d_summary_id.outfile)
	if found(0) ne '' then begin
		if OS_SYSTEM.os_family eq 'unix' then $
		spawn,[OS_SYSTEM.prt, OS_SYSTEM.printer, '-r', view1d_summary_id.outfile], /noshell else $
		spawn,[OS_SYSTEM.prt, OS_SYSTEM.printer, view1d_summary_id.outfile]
		return
	end

	; check startup directory

	found = findfile(filename)
	if found(0) ne '' then 	begin 
		if OS_SYSTEM.os_family eq 'unix' then $
		spawn,[OS_SYSTEM.prt, OS_SYSTEM.printer, '-r', filename], /noshell else $
		spawn,[OS_SYSTEM.prt, OS_SYSTEM.printer, filename]
	endif else $
		w_warningtext,['Error:','    '+filename+ '  not found!']
	END
  'summary_cancel': BEGIN
	WIDGET_CONTROL, view1d_summary_ids.base , /DESTROY
      END
  ENDCASE
END

PRO report_setup
COMMON view1d_summary_block, view1d_summary_ids, view1d_summary_id

str = '0000'
len0 = strlen(str)
sss = str
st = strtrim(view1d_summary_id.start,2)
len = strlen(st)
strput,sss,st,len0-len

filenamepath, view1d_summary_id.file, file, path
view1d_summary_id.outfile = file+'.'+sss

if view1d_summary_id.stop gt view1d_summary_id.start then begin
eee = str
st = strtrim(view1d_summary_id.stop,2)
len = strlen(st)
strput,eee,st,len0-len
view1d_summary_id.outfile = view1d_summary_id.outfile+ '_'+eee
end

END

PRO view1d_summary_setup, GROUP=Group
COMMON CATCH1D_COM, widget_ids, scanData 
COMMON view1d_summary_block, view1d_summary_ids, view1d_summary_id
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id


if XRegistered('view1d_summary_setup') ne 0 then begin 
	WIDGET_CONTROL,view1d_summary_ids.base,/DESTROY
	end

if XRegistered('w_viewscan') ne 0 then begin
	WIDGET_CONTROL,w_viewscan_ids.base,/DESTROY
	end

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

view1d_summary_id = { $
	start: 1, $
	stop: 1, $
	header: 0, $
	outfile: w_plotspec_array(3)+'.rep',  $
	file: scanData.trashcan  $
	}

scan_read,unit,-1,-1,maxno ; scan_read_all,unit,maxno
view1d_summary_id.start = maxno
view1d_summary_id.stop = maxno

  report_setup

  view1d_summary_base = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, $
 TITLE = 'Report ...', $
      UVALUE='view1d_summary_base')

  BASE2 = WIDGET_BASE(view1d_summary_base, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  LABEL3 = WIDGET_LABEL( BASE2, $
      UVALUE='LABEL3', $
      VALUE='SUMMARY ASCII REPORT')

Btns220 = [ 'Full', 'Abbreviated', 'None' ]
  summary_header = CW_BGROUP( BASE2, Btns220, $
      ROW=1, $
      EXCLUSIVE=1, $
      LABEL_LEFT='Header options:', $
      UVALUE='summary_header')
WIDGET_CONTROL,summary_header,SET_VALUE=0

  summary_format = CW_FIELD( BASE2,VALUE=scanData.code+scanData.format, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS= 1, $
      TITLE='Ouput Data Format: ', $
      XSIZE=8, $
      UVALUE='summary_format')

  summary_file = CW_FIELD( BASE2,VALUE=view1d_summary_id.file, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS= 1, $
      TITLE='Data file name: ', $
      XSIZE=60, $
      UVALUE='summary_file')

  FieldVal388 = strtrim(view1d_summary_id.start,2)
  summary_start = CW_FIELD( BASE2,VALUE=FieldVal388, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS= 1, $
      TITLE='Start scan #    ', $
      UVALUE='summary_start')

  FieldVal465 = strtrim(view1d_summary_id.stop,2)
  summary_end = CW_FIELD( BASE2,VALUE=FieldVal465, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS= 1, $
      TITLE='End scan #      ', $
      UVALUE='summary_end')

  summary_separate = WIDGET_DROPLIST(BASE2, VALUE=['No', 'Yes'], $
        UVALUE='summary_separate',TITLE='Save Selected Scans as Separate ASCII Files')
  WIDGET_CONTROL,summary_separate,set_droplist_select = 0

  BASE112 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE112')

;  label = WIDGET_LABEL(BASE112,VALUE='Out Report File: ')
;  summary_outfile = WIDGET_LABEL(BASE112,VALUE=view1d_summary_id.outfile)

  summary_outfile = CW_FIELD( BASE2,VALUE=view1d_summary_id.outfile, $
      ROW=1, XSIZE=60, $
      TITLE='Output file name: ', $
      UVALUE='summary_outfile')

  BASE12 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE12')

  summary_ok = WIDGET_BUTTON( BASE12, $
      UVALUE='summary_ok', $
      VALUE='Generate Report')

  summary_view = WIDGET_BUTTON( BASE12, $
      UVALUE='summary_view', $
      VALUE='View Report')

  summary_print = WIDGET_BUTTON( BASE12, $
      UVALUE='summary_print', $
      VALUE='Print Report')

  summary_cancel = WIDGET_BUTTON( BASE12, $
      UVALUE='summary_cancel', $
      VALUE='Done')


; set widget ids:

	widget_ids.summary_base = view1d_summary_base

view1d_summary_ids = { $
	base: view1d_summary_base, $
	format: summary_format, $
	file: summary_file, $
	outfile: summary_outfile, $
	view: summary_view, $
	print: summary_print, $
	separate: 0, $
	start: summary_start, $
	stop: summary_end $
	}

  WIDGET_CONTROL, view1d_summary_base, /REALIZE

  XMANAGER, 'view1d_summary_setup', view1d_summary_base, GROUP_LEADER = GROUP 
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
	if *gDdata.dim eq 2 then begin
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


;
;    if seq_no < =0 then the last scan is plotted
;
PRO scan_read,unit,seq_no,id,maxno

COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array, w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON field_block, field_max, field_name, field_name_array, field_value, field_label_array, w_scanfield_ids
COMMON LABEL_BLOCK, x_names,y_names,x_descs,y_descs,x_engus,y_engus
COMMON CATCH1D_2D_COM,data_2d, gD

 new_pv = [scanData.pv, scanData.y_pv]
 old_yscan = scanData.y_scan
 old_id_def = realtime_id.def
; start_seqno = getscan_num(scanData.trashcan)

next_seq_no = seq_no + 1

;if id eq next_seq_no or id lt 0 then begin
if id lt 0 then begin

	if n_elements(gD) then begin

;	scanno = read_scan(scanData.trashcan,dim,num_pts,cpt,pv,labels,id_def,pa1D,da1D,pa2D,da2D) 

	scanno = read_scan(scanData.trashcan,Scan)

	if scanno lt 0 then begin   ; a new file is picked
	scanData.y_scan = 0
	scanData.y_seqno = 0
	scanData.scanno = 0
	catch1d_check_seqno
	return
	end

	rix2DC, Scan, *gD

	scanno = *(*gD).scanno
	dim = *(*gD).dim
	num_pts = *(*gD).num_pts
	cpt = *(*gD).cpt
	pv = *(*gD).pv
	labels = *(*gD).labels
	id_def = *(*gD).id_def
	pa1D = *(*gD).pa1D
	da1D = *(*gD).da1D
	pa2D = *(*gD).pa2D
	da2D = *(*gD).da2D

;print,'READ AGAIN'
	goto, populate
	endif else begin

	scanimage_alloc,scanData.trashcan, gD, scanno

;print,'ALLOC gD'
	if scanno lt 0 then return
	end
end ; seq_no > 0

	scanno = *(*gD).scanno
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
	end

populate:

; check the header for the file if the the seq_no is set to le 0
	if seq_no le 0 then begin

	realtime_id.def = id_def[*,0]
	label = labels[*,0]
	read_desc_engu,label 


        scanData.req_npts = num_pts[0]
        scanData.act_npts = num_pts[0]
	scanData.refno = 0 ;        scanData.refno = start_seqno
	if dim eq 2 then maxno = cpt[1] else maxno=1
	w_viewscan_id.maxno = maxno
	w_viewscan_id.file = scanData.trashcan 
	w_viewscan_id.seqno = maxno
	w_plotspec_id.seqno = maxno
	unit = 1
	w_viewscan_id.unit = unit
	w_plotspec_id.opened = unit

;  bring up the image window for 2D
	if dim eq 2 then begin
	scanData.y_req_npts = num_pts[1]
	update_2d_data,data_2d,num_pts[0],num_pts[1],da2D,id_def[*,0]
;	scan_mode_write_image,pa1D,da1D,pa2D,da2D,id_def[*,0]
	end
	end


        if dim eq 2 then begin
		t_cpt = cpt[1]
		if seq_no gt 0 then t_cpt = seq_no  ; view old data
		maxno = t_cpt
                seqno = t_cpt
		scanData.y_seqno = t_cpt - 1
		scanData.scanno_2d = scanno 
		scanData.scanno = seqno
		scanData.pv = pv[0]
		scanData.y_pv = pv[1]
		scanData.y_scan = 1
        endif else begin
                seqno = 1   
		maxno = 1
		scanData.pv = pv
		scanData.scanno = seqno
		scanData.y_scan = 0
        end


if dim eq 2 then begin
	if seq_no le 0 then seq_no = cpt[1]
	scanData.y_seqno = seq_no - 1
	yvalue = pa1D(seq_no-1,0)
end
if dim eq 1 and seq_no lt 0 then seq_no = 1
next_seq_no = seq_no + 1

scanData.pv = pv[0]
act_npts = num_pts[0]
scanData.act_npts = act_npts 

	scanData.pa = make_array(4000,4,/double)
	scanData.da = FLTARR(4000,15)

for i=0,3 do begin
        if id_def[i,0] gt 0 then begin
;	scanData.pa(0:act_npts-1,i) = pa2D[*,seq_no-1,i]  else $  
	if dim eq 2 then $
	scanData.pa(0:act_npts-1,i) = pa2D[*,0,i]  else $  
	scanData.pa(0:act_npts-1,i) = pa1D[*,i] 
        end
end

for i=0,14 do begin
        if id_def[4+i,0] gt 0 then begin
	if dim eq 2 then $
	scanData.da(0:act_npts-1,i) = da2D[*,seq_no-1,i] else $
	scanData.da(0:act_npts-1,i) = da1D[*,i]
        end
end


w_plotspec_id.seqno = seq_no
w_viewscan_id.seqno = seq_no


   ;populate read data into global arrays

	if scanData.debug eq 1 then $
	print,'Scan # ',seq_no, ' accessed.'
	scanData.scanno = seq_no 
;	setPlotLabels
	w_plotspec_array(0) = scanData.pv+' @ y('+strtrim(seq_no-1,2)+')'
	if dim eq 2 then w_plotspec_array(0) = w_plotspec_array(0) +'='+strtrim(yvalue,2)
	ix = w_plotspec_id.xcord
	w_plotspec_array(1) = x_descs(ix)
	if w_plotspec_array(1) eq '' then w_plotspec_array(1) = x_names(ix) 
	if x_engus(ix) ne '' then w_plotspec_array(1) = w_plotspec_array(1)+'('+x_engus(ix)+')'
	UPDATE_PLOT, 1
	id = next_seq_no

; reset for scan mode 

if id lt 0 then begin
	if strlen(new_pv[0]) gt 2 then scanData.pv = new_pv[0]
	if strlen(new_pv[1]) gt 2 then scanData.y_pv = new_pv[1]

	realtime_id.def = old_id_def
	scanData.y_scan = old_yscan
end

	scanData.readin_npts=scanData.act_npts
 
END


PRO catch1d_check_seqno,filename
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
;

if scanData.y_scan then begin
	w_plotspec_id.seqno = scanData.scanno ; scanno
	w_viewscan_id.maxno = scanData.scanno ; scanno
end
END



PRO rix2BenChin, Scan
ON_ERROR,1
  if(*Scan.dim EQ 1) then begin
    BenChin= { $
	scanno	: Scan.scanno, $
	dim	: Scan.dim, $
	num_pts : Scan.npts, $
	cpt	: Scan.cpt, $
	id_def	: Scan.id_def, $
	pv	: Scan.pv, $
	labels	: Scan.labels, $
	pa1D	: (*Scan.pa)[0], $
	da1D	: (*Scan.da)[0], $
	pa2D	: ptr_new(/ALLOCATE_HEAP), $
	da2D	: ptr_new(/ALLOCATE_HEAP) $
	}
  endif else begin
    BenChin= { $
	scanno	: Scan.scanno, $
	dim	: Scan.dim, $
	num_pts : Scan.npts, $
	cpt	: Scan.cpt, $
	id_def	: Scan.id_def, $
	pv	: Scan.pv, $
	labels	: Scan.labels, $
	pa1D	: (*Scan.pa)[1], $
	da1D	: (*Scan.da)[1], $
	pa2D	: (*Scan.pa)[0], $
	da2D	: (*Scan.da)[0] $
	}
  endelse

  ptr_free,Scan.pa
  ptr_free,Scan.da
  Scan=BenChin
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


PRO make_2d_data,data_2d,xdim,ydim
data_2d = { $
	image : make_array(xdim,ydim) $
	}
END

PRO update_2d_data,data_2d,xdim,ydim,da2D,id_def
COMMON CATCH1D_COM, widget_ids, scanData

	if n_elements(data_2d) then data_2d = 0

     	make_2d_data,data_2d,xdim,ydim

	sz = size(da2D)
	y_last = sz(2) - 1

	if scanData.image gt 2 then catch1d_win_2D_update2,y_last else $
	catch1d_win_2D_update1,y_last
END


PRO scan_mode_write_image,pa1D,da1D,pa2D,da2D,id_def
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON LABEL_BLOCK,x_names,y_names,x_descs,y_descs,x_engus,y_engus
  COMMON CATCH1D_2D_COM, data_2d, gD

filename = scanData.imgfile

CATCH,error_status
if error_status ne 0 then begin
	ret=dialog_message(!err_string+string(error_status),/Error)
	filename = '/tmp/DCV2D.image.xdr'
end

scanData.imgfile = filename

brandnew:
	u_openw,unit,filename,/XDR
	npts = scanData.req_npts-1
	y_seqno = scanData.y_req_npts -1 ; scanData.y_seqno - 1

if y_seqno ge 0 then begin

	pa1D = *(*gD).pa1D
;	da1D = *(*gD).da1D
	pa2D = *(*gD).pa2D
	da2D = *(*gD).da2D
	id_def = *(*gD).id_def(*,0)
	labels = *(*gD).labels

	pv2_desc = labels(19,1)
	if pv2_desc eq '' then pv2_desc = labels(0,1)

	for i=0,14 do begin
	if  id_def(4+i) gt 0 then begin

	; write the detector  number i, width, height

	pvs = make_array(60,6,/byte)
	pvs(0,0) = byte(scanData.pv)
	pvs(0,1) = byte(scanData.y_pv)
	temp = w_plotspec_array(3)
	if strlen(temp) gt 60 then temp = strmid(temp,0,60)
	pvs(0,2) = byte(temp)

;  3  for scan1  positioner desc
;  4  for scan2  positioner desc
;  5  for detector desc

        len = strlen(x_descs(0))
        if len gt 0 then pvs(0:len-1,3) = byte(x_descs(0)) else $
	pvs(0,3) = byte(x_names(0))
	len = strlen(pv2_desc)
	if len gt 0 then pvs(0:len-1,4) = byte(pv2_desc)
        len = strlen(y_descs(i))
        if len gt 0 then pvs(0:len-1,5) = byte(y_descs(i)) else $
	pvs(0,5) = byte(y_names(i))

	u_write,unit,pvs

if scanData.debug eq 1 then begin
print,'2D Scan # =',scanData.scanno_2d, ' Scan # =',scanData.scanno
print,'    width =',scanData.req_npts, ' height =', $
	y_seqno, ' Detector # =',i
	end

	x = [scanData.scanno, scanData.req_npts, y_seqno+1, i, $
		scanData.scanno_2d, scanData.y_req_npts]

	u_write,unit,x

	; write x and y positioner array
	
	x = pa2D(*,0,0)
	u_write,unit,x

	y = pa1D(*,0)
	u_write,unit,y

	; write image

	data_2d.image = da2D(*,0:y_seqno,i)
	u_write,unit,data_2d.image
	end
end
end
	free_lun,unit

END

PRO catch1d_process2Ddata
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

        if scanData.y_scan eq 1 then begin

; update the 2D-scan data


        ; assign 2D data array

;        catch1d_fill_2D_data

;	empty

; process the record at the end of scan

;       id = caPutArray(scanData.y_pv+'.PROC',1)
	if strlen(strtrim(scanData.y_handshake,2)) gt 0 then $
        id = caPutArray(scanData.y_handshake,1)

	ln = cagetArray([scanData.y_pv+'.P1CV',scanData.y_pv+'.CPT'],pd,/float)
	scanData.y_value = pd(0)
	scanData.y_seqno = pd(1)

; update the seq no

        w_plotspec_id.seqno = scanData.y_seqno 
        w_viewscan_id.maxno = w_plotspec_id.seqno
        scanData.scanno = scanData.y_seqno + 1 

if scanData.debug eq 1 then $
print,'2D SCAN #',scanData.scanno_2d, '  Y SCAN #',scanData.y_seqno
        end

	if scanData.debug eq 1 then $
	print,'1D SCAN #',scanData.scanno

END



PRO catch1d_win_2D_update1,y_seqno
COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
  COMMON CATCH1D_2D_COM, data_2d, gD

; update the image plot

;loadct, 39

npts = scanData.act_npts-1
if n_params() eq 0 then y_seqno = scanData.y_seqno
if y_seqno lt 0 then return
	width = scanData.image_width * scanData.image
	height = scanData.image_height * scanData.image

;	old_win = !D.window
	old_win = widget_ids.plot_area
	new_win = old_win - 1 
	if y_seqno eq 0 then begin
		window,new_win, xsize = 8*width, ysize=2*height, title='2D_Images'
		for i=0,14 do begin
		xi=(i mod 8)*width+width/2 - 5
		yi=height/2+(15-i)/8*height
		xyouts, xi,yi,'D'+strtrim(i+1,2),/device
		end
        plots,[0,8*width],[height,height],/device
        for i=1,7 do plots,[i*width,i*width],[0,2*height],/device

	end

; WSET: Window is closed and unavailable.        -324  R4.0.1
; WSET: Window is closed and unavailable.        -367  R5.0
; WSET: Window is closed and unavailable.        -386  R5.1
CATCH,error_status
if error_status eq -97 then begin
	str= [!err_string+string(!err), 'Run only with IDL 5.1 or higher']
	res = widget_message(str,/INFO)
	return
end
if error_status eq -367 or error_status eq -386  or error_status eq -324 then begin

;print,'name: ',!error_state.name
;print,'code: ',!error_state.code
;print,'msg:  ',!error_state.msg
;print,'sys_msg:  ',!error_state.sys_msg

	window,new_win, xsize = 8*width, ysize=2*height, title='2D_Images'
		for i=0,14 do begin
		xi=(i mod 8)*width+width/2 - 5
		yi=height/2+(15-i)/8*height
		xyouts, xi,yi,'D'+strtrim(i+1,2),/device
		end
        plots,[0,8*width],[height,height],/device
        for i=1,7 do plots,[i*width,i*width],[0,2*height],/device

end
	wset,new_win

;	erase

	da2D = *(*gD).da2D
	for sel=0,14 do begin
	if realtime_id.def(4+sel) gt 0 then begin
	im = da2D(*,0:y_seqno,sel)
	if strpos(!version.release,'5.0') eq 0 then $
	TVSCL,congrid(im,width,height), sel else $
	TVSCL,congrid(im,width,height), sel, /NAN 
	end
	end

	wset,old_win

END


;
; update only selected 2D detector
;
PRO catch1d_win_2D_update2,y_seqno
COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
  COMMON CATCH1D_2D_COM, data_2d, gD

; update the image plot

;loadct, 39
npts = scanData.act_npts-1
if n_params() eq 0 then y_seqno = scanData.y_seqno
if y_seqno lt 0 then return

;	old_win = !D.window
	old_win = widget_ids.plot_area
	new_win = old_win - 1 
	if y_seqno eq 0 then window,new_win, $
		xsize = 200, ysize=200, $
		title='2D_Image'

; -386 window not round in 5.1
CATCH,error_status
if error_status lt 0 then begin
;	print,!err_string,!err
	if error_status eq -367 or error_status eq -386 or error_status eq -324  then begin
	window,new_win, $
		xsize = 200, ysize=200, $
                title='2D_Image'
        end
end

	wset,new_win

	sel = scanData.image - 3
	
	da2D = *(*gD).da2D
	im = da2D(*,0:y_seqno,sel)

help,old_win,new_win,im,sel,y_seqno
        if strpos(!version.release,'5.0') eq 0 then $
	TVSCL,congrid(im, 200, 200) else $
	TVSCL,congrid(im, 200, 200) ,/NAN   ;  IDL 5.1

	wset,old_win
END

;
; Auto Save File For ./catch1d.pro
;
;  Tue Oct  4 11:20:04 CDT 1994
;

PRO catch1d_append
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

;
; destroy the old w_plotspec window
;
	close_plotspec

	WIDGET_CONTROL,widget_ids.summary_base,/DESTROY,BAD_ID=bad

	filenamepath,scanData.trashcan,old_file,old_path

        FNAME = ''


; check for bad directory -296

        CATCH, error_status

        if error_status ne 0 then begin     ; eq -296 then begin
        w_warningtext,[!err_string + string(!err) ,'       Bad data directory !!!', $
		'       Please try to fix the problem first.']
        retall
        end


        F = PICKFILE(TITLE='Open ...',/WRITE,FILE=filename,PATH=old_path,GET_PATH=P,FILTER=FNAME)

        IF F eq '' THEN return 

	if STRMID(F,0,1) eq '~' then filename_expand,F 
        found=findfile(F)

        IF (STRMID(F,0,1) EQ '/') THEN $
                FNAME = F $
        ELSE $
                FNAME = P+F

	filenamepath,FNAME,F,P
	scanData.path = P 

if strlen(P) gt 1 then begin

	CATCH,error_status
	if error_status lt 0 then begin
	w_warningtext,!err_string + string(!err)
	return
	end

	if scanData.debug eq 1 then $
	print,'CURRENT_DIR: ',P
	cl = strlen(P)
	if strmid(P,cl-1,1) eq '/' then D = strmid(P,0,cl-1)
	end

; check for bad D

	if n_elements(D) eq 0 then begin
	w_warningtext,'Error:  bad directory path for the data file '
	return
	end

; check file version

        w_plotspec_array(3) = F
	scanData.trashcan = FNAME


; set plot menu options
	w_plotspec_id.x_axis_u = 0
	w_plotspec_id.type = 0
	w_plotspec_id.log = 0
	w_plotspec_id.grid = 0
	w_plotspec_id.errbars = 0
	w_plotspec_id.xcord = 0


;	scan_read_all,unit,maxno
	scan_read,1,-1,-1    ; plot the last scan

	if string(D) ne string(old_path) then  pventry_event

END


PRO catch1d_viewmode, Event
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

;	cadebug,-1

	w_plotspec_saveTitle
	scanData.plotspec = w_plotspec_id

	FNAME = strcompress(scanData.trashcan,/remove_all) 
	w_plotspec_id.mode = 1
	w_plotspec_id.seqno = 0
	if scanData.debug eq 1 then $
	print,'Read Scan Data from: '+ FNAME

u = findfile(FNAME) 
if u(0) eq '' then begin
w_warningtext,'Error file not found: '+FNAME
w_plotspec_id.mode = 0
return
end

	set_sensitive_off
	WIDGET_CONTROL,widget_ids.rept_base,SENSITIVE=1
	WIDGET_CONTROL,widget_ids.viewdata,SENSITIVE=1
;
; destroy the old w_plotspec window
;
	close_plotspec
	unit = 1
	w_viewscan, unit, GROUP=Event.top

END

PRO PDMENU_VDATA_Event, Event
  COMMON CATCH1D_COM, widget_ids, scanData

  CASE Event.Value OF 
  'ViewData.1D ...': BEGIN
	catch1d_viewmode, Event
 	END
  'ViewData.2D ...': BEGIN
	vw2d, GROUP=event.top, file=scanData.trashcan
;	if scanData.trashcan eq '' then begin
;		w_warningtext,'You have to load in scan data first!'
;		return
;	end
;	scan_read,1,-1,-1,maxno
;	if maxno gt 1 then $
;	scan_mode_write_image
;	DCV2D, GROUP=event.top, file= scanData.imgfile
 	END
  'ViewData.1D Overlay ...': BEGIN
;        view1d_overlay, scanData.trashcan, GROUP=event.top 
 	END
  ENDCASE
END

PRO HELPMENU_Event, Event
  COMMON CATCH1D_COM, widget_ids, scanData

  CASE Event.Value OF 
  'Help.Version ...': BEGIN
	st = caVersion()
	st = [st,'','scanSee Version :  R1.2']
	w_warningtext,st
 	END
  'Help.Release Note ...': BEGIN
        private = getenv('EPICS_EXTENSIONS_PVT') + '/doc/scanSee.README'
        found = findfile(private)
        if found(0) ne '' then xdisplayfile,found(0),GROUP=Event.top else begin
        str = getenv('EPICS_EXTENSIONS')+'/doc/scanSee.README'
        xdisplayfile,str, GROUP=Event.top
        end
        END
  'Help.Help ...': BEGIN
	st = 'Not available yet !'
	w_warningtext,st
 	END

  ENDCASE
END



PRO PRINTMENU_Event, Event
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON font_block, text_font, graf_font, ps_font

  CASE Event.Value OF 

  'Print.Plot': BEGIN
	if scanData.lastPlot lt 0 then return
	scanData.act_npts = scanData.readin_npts
    	PS_open,'catch1d.ps'
;    graf_font = ps_font
    	UPDATE_PLOT, scanData.lastPlot
    	PS_close
    	PS_print,'catch1d.ps'
;    graf_font = text_font
 	END
  'Print.Report ...': BEGIN
	view1d_summary_setup,GROUP=Event.top  		; pick the range

 	END
  ENDCASE
END

PRO ZOOMMENU_Event, Event
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON user_scale_block,user_scale_ids

if scanData.lastPlot ge 0 then begin 

if XRegistered('user_scale') ne 0 then begin
        WIDGET_CONTROL,user_scale_ids.base,/DESTROY
        end

scanData.act_npts = scanData.readin_npts

  CASE Event.Value OF 

  'Zoom.Zoom To Box': BEGIN
	zoom_to_box
 	END
  'Zoom.Zoom In/Out': BEGIN
	zoom_in_out
 	END
  'Zoom.Calc Slopes': BEGIN
	draw_dragLine
 	END
  'Zoom.Auto Scale (Refresh)': BEGIN
	scanData.lastPlot = 1
	UPDATE_PLOT, 1
 	END
  'Zoom.User Scale ...': BEGIN
	scanData.lastPlot = 0
	user_scale,GROUP=event.top
;	UPDATE_PLOT, 0
 	END
  ENDCASE
end
END

PRO STATISTICMENU_Event, Event

  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_statistic_block,w_statistic_ids

  CASE Event.Value OF

  'Statistic.None': BEGIN
        w_plotspec_id.statistic = 0
        UPDATE_PLOT,1
	if XRegistered('w_statistic') ne 0 then begin
	WIDGET_CONTROL,w_statistic_ids.base,BAD=bad,/DESTROY
	widget_ids.statistic = 0
	end
    END

  'Statistic.Peak/Centroid/FWHM on plot': BEGIN
        w_plotspec_id.statistic = 1
        UPDATE_PLOT,1,st
	if XRegistered('w_statistic') ne 0 then begin
	WIDGET_CONTROL,w_statistic_ids.base,BAD=bad,/DESTROY
	widget_ids.statistic = 0
	end
    END

  'Statistic.Peak/Centroid/FWHM ...': BEGIN
        w_plotspec_id.statistic = 2
        UPDATE_PLOT,1,st
        if n_elements(st) gt 0 then begin
        if widget_ids.statistic eq 0 then $
                w_statistic,st,34,25,'Statistic',GROUP=Event.top $
          else WIDGET_CONTROL,widget_ids.statistic,SET_VALUE=st
        end
    END

  'Statistic.Average/Deviation ...': BEGIN
        w_plotspec_id.statistic = 3
        UPDATE_PLOT,1,st
        if n_elements(st) gt 0 then begin
        if widget_ids.statistic eq 0 then $
        	w_statistic,st,34,25,'Statistic',GROUP=Event.top $ 
	else WIDGET_CONTROL,widget_ids.statistic,SET_VALUE=st 
	end
    END

  ENDCASE

END

PRO FITTINGMENU_Event, Event
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

if scanData.act_npts lt 2 then begin
        w_warningtext,['No data available yet','', $
		 'You have to load 1D data in first.']
	return
end

x = scanData.pa(0:scanData.act_npts-1,w_plotspec_id.xcord)
y = make_array(scanData.act_npts,15)
y(*,*) = scanData.da(0:scanData.act_npts-1,0:14)

  CASE Event.Value OF

  'Fitting.Ez_Fit ...': BEGIN
        ez_fit,x=x,y=y,GROUP=Event.Top
        END
  'Fitting.1D Binary': BEGIN
	u_openw,unit,'fitting.bin1d',/XDR
	u_write,unit,x
	u_write,unit,y
	u_close,unit
        st = '1D binary data save in fitting.bin1d'
        w_warningtext,st
	END
  ENDCASE
END


PRO PDMENU127_Event, Event

  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON w_caset_block, w_caset_base, w_caset_ids, w_caset_narray, w_caset_varray

  CASE Event.Value OF 

  'File.Open ...': BEGIN
	catch1d_append
;	catcher_setup,GROUP=Event.top
    END

  'File.Printer ...': BEGIN
	PS_printer,GROUP=Event.Top
    END

  'File.Quit': BEGIN
;    catcher_close,event.top
    	WIDGET_CONTROL, event.top, /DESTROY
    END
  ENDCASE
END

PRO catcher_close,wid
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON w_caset_block, w_caset_base, w_caset_ids, w_caset_narray, w_caset_varray

    if XRegistered('w_caset') ne 0 then $
	WIDGET_CONTROL,w_caset_base,/DESTROY
	w_warningtext_close

;    IF (STRLEN(scanData.pv) NE 0) THEN begin 

	; change director error -296

	CATCH,error_status
        if error_status ne 0 then begin     ; eq -296 then begin
	w_warningtext,!err_string + string(!err)
        scanData.home = oldpath
        end
	write_config

;   end

	EXIT
END




PRO w_warningtext_close
COMMON w_warningtext_block,w_warningtext_ids

	if XRegistered('w_warningtext') ne 0 then $
	WIDGET_CONTROL, w_warningtext_ids.base, /DESTROY
END


PRO set_sensitive_off
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan

;	WIDGET_CONTROL,widget_ids.file,SENSITIVE=0
;	WIDGET_CONTROL,widget_ids.viewdata,SENSITIVE=0
	WIDGET_CONTROL,widget_ids.rept_base,SENSITIVE=0

	;  set plot option menu errbar, labels
	plotoptionsmenu_sensitive,14,0
	plotoptionsmenu_sensitive,21,0

	if XRegistered('catcher_setup') ne 0 then begin
	WIDGET_CONTROL,catcher_setup_ids.start,SENSITIVE=0
	WIDGET_CONTROL,catcher_setup_ids.start2,SENSITIVE=0
	WIDGET_CONTROL,catcher_setup_ids.stop,BAD=bad,SENSITIVE=0
	end

     if scanData.nosave and scanData.y_scan and strtrim(scanData.y_handshake,2) eq '' then WIDGET_CONTROL,widget_ids.file,SENSITIVE=1

END

PRO set_sensitive_on
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan

;	WIDGET_CONTROL,widget_ids.menubar_base,SENSITIVE=1
;	WIDGET_CONTROL,widget_ids.file,SENSITIVE=1
;	WIDGET_CONTROL,widget_ids.viewdata,SENSITIVE=1
	WIDGET_CONTROL,widget_ids.rept_base,SENSITIVE=1
	WIDGET_CONTROL,widget_ids.wf_select,SENSITIVE=1

	;  set plot option menu errbar, labels
	plotoptionsmenu_sensitive,14,1
	plotoptionsmenu_sensitive,21,1

	if XRegistered('catcher_setup') ne 0 then begin
	WIDGET_CONTROL,catcher_setup_ids.start,SENSITIVE=1
	WIDGET_CONTROL,catcher_setup_ids.stop,SENSITIVE=1
	WIDGET_CONTROL,catcher_setup_ids.start2,SENSITIVE=1
	end

     WIDGET_CONTROL,widget_ids.axis_base,SENSITIVE=1

END

PRO catch1d_Start_xScan
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan

	x = caput(scanData.pv+'.EXSC',1)
	if x eq -1 then w_warningtext,'Error encounted in START_1D_SCAN'

        if XRegistered('w_viewscan') ne 0 then begin
                WIDGET_CONTROL,w_viewscan_ids.base,/DESTROY
                end
        if XRegistered('w_plotspec') ne 0 then begin
                WIDGET_CONTROL,w_plotspec_ids.base,/DESTROY
                end
	WIDGET_CONTROL,catcher_setup_ids.pv,GET_VALUE=pv
	pv = strtrim(pv(0),2)
	if scanData.pv ne pv then begin
		scanData.pv = pv 
	pventry_event
		end

	if strlen(scanData.pv) lt 1 then begin
		w_warningtext,'Enter SCAN Record Name first !!'
		return
		end

; check for proper setup first

	if scanData.pvfound eq -1 then return
	
	catch1d_check_seqno

	w_plotspec_id.scan = 1
	set_sensitive_off
	setPlotLabels

	if w_plotspec_id.realtime eq 1 then begin
		realtime_init
		end

END

PRO catch1d_Stop_xScan
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

	if scanData.pvfound eq -1 then return
	if caSearch(scanData.pv) eq 0 then begin
;	ln = caget(scanData.pv+'.EXSC',pd) 
	ln = cagetArray(scanData.pv+'.EXSC',pd) 
	if ln eq 0 and pd(0) gt 0 then begin
	x = caput(scanData.pv+'.EXSC',0)
	if x eq -1 then w_warningtext,'Error encounted in STOP_1D_SCAN'
	end
	endif else w_warningtext,['Error: scan record  '+ scanData.pv +'  not found!']
	w_plotspec_id.scan = 1
        if scanData.y_scan eq 0 then set_sensitive_on
END

PRO catch1d_Stop_yScan
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved

if scanData.pvfound eq -1 then return

        if strlen(scanData.y_pv) lt 1 then return
;        ln = caget(scanData.y_pv+'.EXSC',pd)
        ln = cagetArray(scanData.y_pv+'.EXSC',pd)
        if ln eq 0 and pd(0) gt 0 then begin
        x = caput(scanData.y_pv+'.EXSC',0)
        if x eq -1 then w_warningtext,'Error encounted in STOP_2D_SCAN'
        end

	; save the 2D-image file
;	scan_mode_write_image

	; reset y-scan parameters

        w_plotspec_id.scan = 1
        scanData.y_scan = 0
        scanData.y_seqno = 0
        set_sensitive_on

END

PRO catch1d_Start_yScan
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

        ; if view window is opened close it while scan is going on
        if XRegistered('w_viewscan') ne 0 then begin
                WIDGET_CONTROL,w_viewscan_ids.base,/DESTROY
                end

;	catch1d_Start_xScan
	pventry_event
	if w_plotspec_id.realtime eq 1 then begin
		realtime_init
		end

       	pventry2_event

        if strlen(scanData.y_pv) lt 1 then begin
                w_warningtext,'Enter Y SCAN Record Name first !!'
                return
                end

; check for proper setup first

if scanData.pvfound eq -1 then return

;	ln = caget(scanData.y_pv+'.P1PV',s1)
	ln = cagetArray(scanData.y_pv+'.P1PV',s1)
	if ln eq 0 and s1(0) eq ''  then $ 
	begin
	st = ['Error: Y Direction Scan is not properly set', $
		'       for the scan record -  '+scanData.y_pv]
	w_warningtext,st
	return
	end
	

	scanData.y_scan = 1
	scanData.scanno_2d = scanData.scanno_2d + 1
	set_sensitive_off

	ln = cagetArray(scanData.y_pv+'.EXSC',pd)
	if pd(0) eq 0 then begin 
	x = caputArray(scanData.y_pv+'.EXSC', 1)
	if x eq -1 then w_warningtext,'Error encounted in START_2D_SCAN'
	end

       scanData.y_value=0.
       ln = cagetArray(scanData.y_pv+'.P1DV',pd,/float)
       if ln eq 0 then scanData.y_value = pd(0)

	setPlotLabels

;  find new filename based on prefix and scan #

	calc_newfilename
	WIDGET_CONTROL,widget_ids.trashcan, SET_VALUE = scanData.trashcan
	
END


;
;  getPositionDetectorData1    
;
PRO getPositionDetectorData1

COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames

	scanData.pa = make_array(4000,4,/double)
	scanData.da = FLTARR(4000,15)

		
	if scanData.y_scan then seq_no = scanData.y_seqno+1
	status = get_1Dscan(scanData.trashcan,seq_no,pa1D,da1D)
	
if status eq 0 then begin
	print,'Error: in getPositionDetectorData1'
	return
end

act_npts = scanData.act_npts
for i=0,3 do begin
        if realtime_id.def[i,0] gt 0 then begin
	scanData.pa(0:act_npts-1,i) = pa1D[0:act_npts-1,i] 
        end
end

for i=0,14 do begin
        if realtime_id.def[4+i,0] gt 0 then begin
	scanData.da(0:act_npts-1,i) = da1D[0:act_npts-1,i]
        end
end


END

PRO getPositionDetectorData
COMMON CATCH1D_COM, widget_ids, scanData
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames

if n_elements(realtime_pvnames) gt 0 then begin

	p_name = [ scanData.pv+'.P1RA', + scanData.pv+'.P2RA', $
		scanData.pv+'.P3RA', + scanData.pv+'.P4RA']

	d_name = [ scanData.pv+'.D1DA', + scanData.pv+'.D2DA', $
		scanData.pv+'.D3DA', + scanData.pv+'.D4DA', $
		scanData.pv+'.D5DA', + scanData.pv+'.D6DA', $
		scanData.pv+'.D7DA', + scanData.pv+'.D8DA', $
		scanData.pv+'.D9DA', + scanData.pv+'.DADA', $
		scanData.pv+'.DBDA', + scanData.pv+'.DCDA', $
		scanData.pv+'.DDDA', + scanData.pv+'.DEDA', $
		scanData.pv+'.DFDA'   $
		]

	scanData.pa = make_array(1000,4,/double)
	scanData.da = make_array(1000,15,/float)

	; get type and count for  positioner & detector

	ln = caGetTypeCount(realtime_pvnames,types,counts,wave_types)
	scanData.x_dpt(0:14) = counts(4:18)
	scanData.x_dtype(0:14) = wave_types(4:18)

	; fill position array

px_name = [ scanData.pv+'.P1PV', scanData.pv+'.P2PV', $
		scanData.pv+'.P3PV', scanData.pv+'.P4PV' ]
ln = cagetArray(px_name,pname,/string)

px_name = [ scanData.pv+'.R1PV', scanData.pv+'.R2PV', $
		scanData.pv+'.R3PV', scanData.pv+'.R4PV' ]
ln = cagetArray(px_name,tname,/string)

	for i=0,3 do begin
		if strlen(pname(i)) gt 1 or strlen(tname(i)) gt 1 then begin
		npts = scanData.act_npts + 1
		type = wave_types(i)
		ln = cagetArray(p_name(i), pd, max=npts,/double)
		if ln eq 0 then $
		scanData.pa(0:scanData.act_npts,i) = pd(0:scanData.act_npts)
		end
	end

	; fill detector array

px_name = [ scanData.pv+'.D1PV', scanData.pv+'.D2PV', $
		scanData.pv+'.D3PV', scanData.pv+'.D4PV', $
		scanData.pv+'.D5PV', scanData.pv+'.D6PV', $
		scanData.pv+'.D7PV', scanData.pv+'.D8PV', $
		scanData.pv+'.D9PV', scanData.pv+'.DAPV', $
		scanData.pv+'.DBPV', scanData.pv+'.DCPV', $
		scanData.pv+'.DDPV', scanData.pv+'.DEPV', $
		scanData.pv+'.DFPV' $
	]

ln = cagetArray(px_name,pname,/string)

	for i=0,14 do begin
	if strlen(pname(i)) gt 1 then begin
		npts = counts(i+4) * scanData.act_npts + 1
		type = wave_types(i+4)
		ln = cagetArray(d_name(i), pd, max=npts,type=type)
		if ln eq 0 then $
		scanData.da(0:scanData.act_npts,i) = pd(0:scanData.act_npts)
	   end
	end

end

END 



PRO catch1dReadScanRecordAppendFile
COMMON CATCH1D_COM,widget_ids,scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotpec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames

if scanData.pvfound eq -1 then return

	  ln = cagetArray(scanData.pv+'.CPT',pd)
          if ln eq 0 then scanData.act_npts = pd(0)
          WIDGET_CONTROL, widget_ids.pv_stat, $
            SET_VALUE = 'IDLE : Acquired' +STRING(scanData.act_npts)+' Pts'

	 if scanData.y_scan eq 0 then  set_sensitive_on

	if scanData.act_npts le 1 then begin 
;	w_warningtext,'No data detected in scan record: ' + scanData.pv
	return
	end

	scanData.readin_npts = scanData.act_npts

;
; automatic save scan data
;
	F = scanData.trashcan

; spawn,'date', x, /noshell
x = catimestamp(scanData.pv+'.EXSC')
y = strupcase(getenv('USER'))
w_plotspec_array(4) = x(0) + '. User Name: ' + y
	if scanData.debug eq 1 then begin
	print,''
	print,w_plotspec_array(4)
	end

; get position and data array from the scan record

;	if scanData.y_scan eq 1 then $
;	getPositionDetectorData1 else $
;	getPositionDetectorData
;	UPDATE_PLOT,scanData.lastPlot
;	if scanData.y_seqno lt scanData.y_req_npts then $
;	catch1d_fill_2D_data

	scan_read,1,-1,-1,maxno

	if scanData.y_scan then begin
		catch1d_process2Ddata
		setPlotLabels 
	end
	realtime_id.ind = -1

;#####  may need to read the scan # from the data base


END



; CODE MODIFICATIONS MADE ABOVE THIS COMMENT WILL BE LOST.
; DO NOT REMOVE THIS COMMENT: BEGIN MAIN13_1

PRO MAIN13_1_Event, Event

  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id

if scanData.option eq 1 then begin        ; if Acquisition.On is set by user

  ; this is and added CASE statement to see if the event.id
  ; is on that I assigned to a caWidgetAddmonitor ...

scanData.pv = scanData.pvconfig

  CASE event.id OF

  widget_ids.base: BEGIN
      IF (STRLEN(scanData.pv) EQ 0) THEN  RETURN
	chkid = caSearch(scanData.pv+'.EXSC')
IF chkid eq 0 then BEGIN

;
; check whether 2D scan started by outside CA clients
;

	  if strlen(scanData.y_pv) gt 0 and scanData.y_scan eq 0 then begin
	if caSearch(scanData.y_pv+'.EXSC') eq 0 then begin 
		pvs = [scanData.y_pv+'.EXSC',scanData.y_pv+'.CPT']
		id = cagetArray(pvs,pd) 
		if pd(0) eq 1 then begin
			catch1d_Start_yScan
			scanData.y_seqno = pd(1)
			scanData.scanno = pd(1)+1 
			end
	  end
	end

;   ret = caCheckMonitor(scanData.pv+'.EXSC')
    ret = caMonitor(scanData.pv+'.EXSC',valchange,/check) 
if ret eq -1 then begin                 ; ****may be error in caCheckMonitor	
	pventry_event
	end

; scan mode check the following

if w_plotspec_id.mode eq 0 then begin
setPiDiMonitor,ret,/check 
if total(ret) gt 0 then begin
	if scanData.debug eq 1 then $
	print,ret,'Warning: Reset PV name in scan record!!!'
	if scanData.y_scan eq 0 then $	
	pventry_event
	end
end

scanFlag=0
if caSearch(scanData.pv+'.EXSC') eq 0 then begin
	ln = cagetArray(scanData.pv+'.EXSC',pd)
	if ln eq 0 then scanFlag = pd(0) else scanFlag=0 ;======= 8/15/96
end
      IF (scanFlag EQ 1) THEN BEGIN
	;  find new filename based on prefix and scan #
	if scanData.y_scan eq 0 and valchange(0) then begin
	calc_newfilename
	WIDGET_CONTROL,widget_ids.trashcan, SET_VALUE = scanData.trashcan
	end


	ln = caMonitor(scanData.pv+'.NPTS',ret,/check)
	if ret(0) gt 0 then begin
;	ln = caget(scanData.pv+'.NPTS',pd)
	ln = cagetArray(scanData.pv+'.NPTS',pd)
	scanData.req_npts = pd(0) 

; if view window is opened close it while scan is going on

        if XRegistered('w_viewscan') ne 0 then begin
                WIDGET_CONTROL,w_viewscan_ids.base,/DESTROY
                end

;
; if realtime_init has not been called, must call it here
;
if w_plotspec_id.realtime eq 1 then begin
	if  n_elements(realtime_pvnames) lt 1 then realtime_init
	realtime_xrange,1
	realtime_id.axis = 1
	  end
end

stt = 'SCANNING: ' +strtrim(scanData.act_npts,2)+ ' of '+  strtrim(scanData.req_npts,2)+ ' Pts' 
if scanData.y_scan gt 0 then stt = stt+' At '+strtrim(scanData.y_seqno,2)+"'th Scan 2D#"+strtrim(scanData.scanno_2d,2)
WIDGET_CONTROL, widget_ids.pv_stat, SET_VALUE = stt

;
;  set scan for outside CA events, e.g. medm set EXSC
;
	w_plotspec_id.scan = 1

if w_plotspec_id.realtime eq 1 then begin
	if realtime_id.ind eq -1 then begin

; if view window is opened close it while scan is going on

        if XRegistered('w_viewscan') ne 0 then begin
                WIDGET_CONTROL,w_viewscan_ids.base,/DESTROY
                end

set_sensitive_off   ; when realtime is going on don't let user change 
                    ; the plot options or detecters 
                    ; this will guard the outside client invoked scan
		    ; only stop can terminate this
		realtime_init
		end
	if realtime_id.ind ne 2 then begin
		realtime_read,scanData.req_npts
		WIDGET_CONTROL,widget_ids.base, timer=w_plotspec_id.dtime
		endif else begin
;		WIDGET_CONTROL,widget_ids.base, /clear_events
		empty
		end
end


      ENDIF ELSE BEGIN
;
; scanFlag eq 0 case
;

	if w_plotspec_id.scan eq 1 then begin

if scanData.debug then $
print,'st1',scanData.y_seqno,scanData.scanno,w_plotspec_id.seqno,w_viewscan_id.maxno
	catch1dReadScanRecordAppendFile
if scanData.debug then $
print,'st2',scanData.y_seqno,scanData.scanno,w_plotspec_id.seqno,w_viewscan_id.maxno

;
; update the cw_term with the final scan result
;
	if scanData.showlist eq 1 then begin
	save_scan_dump_curr,filename
	id = cw_term(widget_ids.terminal,filename=filename,/reset)
	end
 	w_plotspec_id.scan = 0
;	after_scan
	end


;
; check whether 2D scan stopped by outside CA clients
;
	  if scanData.y_scan eq 1 then begin
;		scan_read,1,-1,-1,maxno
;		id = caGet(scanData.y_pv+'.EXSC',pd) 
		id = cagetArray(scanData.y_pv+'.EXSC',pd) 
		if pd(0) eq 0 then begin
		scan_read,1,-1,-1,maxno
		scanData.y_scan = 0
		set_sensitive_on
;print,'stop by CA client',scanData.y_seqno,scanData.scanno,pd(0)
		end
	  end

;
; check whether to terminate the  Y-scan 
;
	if scanData.y_scan eq 1 and $
		scanData.y_seqno ge scanData.y_req_npts then begin
		scan_read,1,-1,-1,maxno
		scanData.y_scan = 0
		set_sensitive_on
;print,'terminate by y_req_npts'
		end

	
      ENDELSE
ENDIF else begin
	WIDGET_CONTROL, widget_ids.pv_stat,SET_VALUE = '>> PV NOT VALID <<'
	return
	end

  END
  ELSE:
  ENDCASE
end ;     end of if scanData.option = 1

  ; The next CASE statement is from the Widget Builder.
  ; It uses the User_value of a widget to identify itself.

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  ; Event for FILE_MENU
  'PDMENU127': PDMENU127_Event, Event

  'PDMENU_VDATA': PDMENU_VDATA_Event, Event

 ; Event for SETUP_MENU
  'SETUPOPTIONSMENU': SETUPOPTIONSMENU_Event, Event
  'PLOTOPTIONSMENU': PLOTOPTIONSMENU_Event, Event
  'HELPMENU': HELPMENU_Event, Event
;  'IMAGEMENU': IMAGEMENU_Event, Event
  'PRINTMENU': PRINTMENU_Event, Event
  'ZOOMMENU': ZOOMMENU_Event, Event
  'STATISTICMENU': STATISTICMENU_Event, Event
  'FITTINGMENU': FITTINGMENU_Event, Event
  'EZFIT_FITTING': BEGIN
	x = scanData.pa(0:scanData.act_npts-1,w_plotspec_id.xcord)
	y = make_array(scanData.act_npts,15)
	y(*,*) = scanData.da(0:scanData.act_npts-1,0:14)
	ez_fit,x=x,y=y,GROUP=Event.Top
	END
  'PICK_IMAGE': BEGIN
	wdelete,!D.window - 1		; 2D image window
	scanData.image = Event.Index + 1
	END

  'PICK_XAXIS': BEGIN
	w_plotspec_id.x_axis_u = 0
	w_plotspec_id.xcord = 0
	if Event.Index eq 0 then w_plotspec_id.x_axis_u = 1 else $
	w_plotspec_id.xcord = Event.Index - 1
	if realtime_id.ind eq 1 then begin
		setPlotLabels
		realtime_id.no = 0
		realtime_xrange,1,xmin,xmax
		realtime_id.axis = 1
 	endif else $	
	UPDATE_PLOT, scanData.lastPlot
	END


  'DRAW61': BEGIN
;print,'Event.PRESS',event.press
   if w_plotspec_id.scan eq 0 then begin
	if (!x.window(1) - !x.window(0)) eq 0 then begin
		w_warningtext,'Error: Plot data not established yet.'
		return
		end
; cross-hairs
      IF (Event.PRESS EQ 1) THEN BEGIN
	WSET, widget_ids.plot_area
	if XRegistered('main13_2') ne 0 then UPDATE_PLOT, scanData.lastPlot
	xy_coord, GROUP=Event.top
	xycoord
	END
   end
      END

  'BUTTON165': BEGIN
      UPDATE_PLOT, 1
      END
  'USER_SCALE': BEGIN
      UPDATE_PLOT, 0
      END

; Take care case of w_viewscan been closed by WM
  'USER_SANE': BEGIN
	if Xregistered('w_viewscan') then  $
		WIDGET_CONTROL,w_viewscan_ids.base,/DESTROY
	END

  'BGROUP145': BEGIN

	if scanData.y_scan eq 0 and w_plotspec_id.scan eq 0 then begin
		UPDATE_PLOT, scanData.lastPlot 
		return
	end
	if realtime_id.ind eq 1 then begin
                        realtime_id.ymin =0.
                        realtime_id.ymax =0.
                        realtime_id.axis = 1
		end
      END

  ELSE:     ;don't stop of no matches
  ENDCASE
END


PRO pventry2_event 
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
  COMMON CATCH1D_2D_COM, data_2d, gD
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan


if XRegistered('catcher_setup') ne 0 then begin

        WIDGET_CONTROL,catcher_setup_ids.y_pv,GET_VALUE=input_string
        new_pv_name = input_string(0)

	; clear monitors on existing pv.EXSC

      IF new_pv_name ne scanData.y_pv THEN begin 
	if caSearch(scanData.y_pv+'.EXSC') eq 0 then begin
           u=caWidgetClearmonitor(scanData.y_pv+'.EXSC',widget_ids.base)
	   u = caMonitor(scanData.y_pv+'.EXSC',/clear)
	   u = caScan(scanData.y_pv+'.CPT','',/clear)
	   u = caMonitor(scanData.y_pv+'.NPTS',/clear)
		end
	  end

	scanData.y_pv = new_pv_name
end

; get the new PV

	new_pv_name = scanData.y_pv

      IF (STRLEN(new_pv_name) EQ 0) THEN res = -1  $
      ELSE res = caSearch(new_pv_name+'.EXSC')

      IF res EQ 0 THEN BEGIN
        WIDGET_CONTROL, widget_ids.pv_stat, SET_VALUE = '>> PV2 Valid <<'
	res=caWidgetSetMonitor(new_pv_name+'.EXSC',widget_ids.base)
	u = caMonitor(new_pv_name+'.EXSC',/add)
	u = caMonitor(new_pv_name+'.NPTS',/add)
	pd=0
	ln = cagetArray([scanData.y_pv+'.NPTS',scanData.y_pv+'.CPT'],pd)
	if ln eq 0 then scanData.y_req_npts = pd(0)
	scanData.y_seqno = pd(1)
	realtime_id.ind = -1


; create 2D data arrays

;	if n_elements(data_2d) eq 0 then $
	make_2d_data,data_2d,scanData.req_npts,scanData.y_req_npts

	if caSearch(scanData.pv+'.EXSC') eq 0 then begin
	if caMonitor(scanData.pv+'.EXSC',/check) ne 0 then begin
	scan_field_get,scanData.pv
	setDefaultLabels
	setPiDiMonitor,/add
	end
	end

      ENDIF ELSE BEGIN
        WIDGET_CONTROL, widget_ids.pv_stat,SET_VALUE = '>> PV2 NOT VALID <<'
        scanData.y_pv = ''
      ENDELSE

;	scanData.y_seqno = 0
	if scanData.debug eq 1 then $
        print,'scanData.y_seqno=',scanData.y_seqno
END


PRO pventry_event 
  COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan

      cadebug,0

if XRegistered('catcher_setup') ne 0 then begin
; get the new PV

	WIDGET_CONTROL,catcher_setup_ids.pv,GET_VALUE=input_string
	new_pv_name = input_string(0)

; clear monitors on existing pv.EXSC

      IF (STRLEN(scanData.pv) NE 0 and new_pv_name ne scanData.pv) THEN begin 
	if caSearch(scanData.pv+'.EXSC') eq 0 then begin
           u=caWidgetClearmonitor(scanData.pv+'.EXSC',widget_ids.base)
	   u = caMonitor(scanData.pv+'.EXSC',/clear)
	   u = caScan(scanData.pv+'.CPT','',/clear)
	   u = caMonitor(scanData.pv+'.NPTS',/clear)
	   setPiDiMonitor,/clear
		end
	  scanData.pv = new_pv_name
	  end

end

        new_pv_name = scanData.pv

w_plotspec_array(0) = new_pv_name
w_plotspec_id.scan = 0

      IF (STRLEN(new_pv_name) EQ 0) THEN res = -1  $
      ELSE res = caSearch(new_pv_name+'.EXSC')

scanData.pvfound = res

      IF res EQ 0 THEN BEGIN
        WIDGET_CONTROL, widget_ids.pv_stat, SET_VALUE = '>> PV Valid <<'
	res=caWidgetSetMonitor(new_pv_name+'.EXSC',widget_ids.base)
	u = caMonitor(new_pv_name+'.EXSC',/add)
	u = caMonitor(new_pv_name+'.NPTS',/add)
	pd=0

	ln = cagetArray(scanData.pv+'.NPTS',pd)
	if ln eq 0 then scanData.req_npts = pd(0)
	scanData.realtime = 0
	realtime_id.ind = -1

        ; get type and count for  positioner & detector

        pvnames = [ scanData.pv+'.D1CV', $
        	scanData.pv+'.D2CV', $
	        scanData.pv+'.D3CV', $
       	 	scanData.pv+'.D4CV', $
       	 	scanData.pv+'.D5CV', $
       	 	scanData.pv+'.D6CV', $
       	 	scanData.pv+'.D7CV', $
        	scanData.pv+'.D8CV', $
        	scanData.pv+'.D9CV', $
        	scanData.pv+'.DACV', $
        	scanData.pv+'.DBCV', $
        	scanData.pv+'.DCCV', $
        	scanData.pv+'.DDCV', $
        	scanData.pv+'.DECV', $
        	scanData.pv+'.DFCV' $
		]

        ln = caGetTypeCount(pvnames,types,counts,wave_types)
        scanData.x_dpt = counts
        scanData.x_dtype = wave_types

	scan_field_get,scanData.pv
	setDefaultLabels
	setPlotLabels
	setScanPvnames

	setPiDiMonitor,/add

;	before_sys_scan

      ENDIF ELSE BEGIN
;	w_warningtext,'Invalid SCAN Record Name !!'
        WIDGET_CONTROL, widget_ids.pv_stat,SET_VALUE = '>> PV NOT VALID <<'
        scanData.pv = ''
      ENDELSE

	WIDGET_CONTROL,widget_ids.trashcan, SET_VALUE = scanData.trashcan
	scanData.y_scan = 0

END


PRO calc_newfilename
COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
 
	prefix = str_sep(scanData.pv,':')
	nm = [prefix[0]+':saveData_scanNumber', $
		scanData.pv+'.EXSC', prefix[0]+'.scanPause.VAL']
	ln = cagetArray(nm,pd,/short)

	str = '0000'
	len0 = strlen(str)
	sss = str

	if pd[1] eq 0 then st = strtrim(pd(0),2) else $
		st = strtrim(pd(0)-1,2)

	if pd[2] eq 1 then st = strtrim(pd(0)-1,2); pause is true

	len = strlen(st)
	strput,sss,st,len0-len

	filename = prefix[0]+':_'+sss+'.scan'
	scanData.trashcan = scanData.path + filename
	w_plotspec_array(3) = filename
END


PRO setPiDiMonitor,ret,add=add,clear=clear,check=check
COMMON CATCH1D_COM, widget_ids, scanData

x_wd = [scanData.pv+'.P1WD', $
	scanData.pv+'.P2WD', $
	scanData.pv+'.P3WD', $
	scanData.pv+'.P4WD', $
	scanData.pv+'.P1PP', $
	scanData.pv+'.P2PP', $
	scanData.pv+'.P3PP', $
	scanData.pv+'.P4PP' $
	]

x_dn = [ scanData.pv+'.R1PV', $
        scanData.pv+'.R2PV', $
        scanData.pv+'.R3PV', $
        scanData.pv+'.R4PV', $
        scanData.pv+'.P1PV', $
        scanData.pv+'.P2PV', $
        scanData.pv+'.P3PV', $
        scanData.pv+'.P4PV', $
        scanData.pv+'.D1PV', $
        scanData.pv+'.D2PV', $
        scanData.pv+'.D3PV', $
        scanData.pv+'.D4PV', $
        scanData.pv+'.D5PV', $
        scanData.pv+'.D6PV', $
        scanData.pv+'.D7PV', $
        scanData.pv+'.D8PV', $
        scanData.pv+'.D9PV', $
        scanData.pv+'.DAPV', $
        scanData.pv+'.DBPV', $
        scanData.pv+'.DCPV', $
        scanData.pv+'.DDPV', $
        scanData.pv+'.DEPV', $
        scanData.pv+'.DFPV' $
        ]

if keyword_set(check) eq 1 then begin
	ln = caMonitor(x_dn,ret,/check)
	return
	end
if keyword_set(add) eq 1 then begin
        ln = caMonitor(x_wd,/add)
	ret = caMonitor(x_dn,/add)
	ln = caMonitor(x_dn,ret,/check)
	return
	end
if keyword_set(clear) eq 1 then begin
        ln = caMonitor(x_wd,/clear)
	ret = caMonitor(x_dn,/clear)
	return
	end
END

PRO catch1d_scanInitSetup
 COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON field_block, field_max, field_name, field_name_array, field_value,  field_label_array,w_scanfield_ids
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan

; check for demo mode

	if demo_mode() eq 1 then begin
	str = ['Sorry: Not able to obtain the license (Demo mode)', $
		'      Try:', $
		'           catcher -D       ( for developer license) ', $
		'', $
		'           catcher          ( for runtime license) ' $
	]
	w_warningtext,str,75,6
	exit
	end

; initalize env arrays

; read in configuration file  

  	read_config

; read in go_catcher2 for runtime version

;found = findfile('go_catcher2')
;if found(0) ne '' then read_config,'go_catcher2'

scanData.pvconfig = scanData.pv
if scanData.debug eq 1 then begin
print,scanData.home
print,scanData.path
end

; override the data file on command line by the setting in 
; the configuration file

	scanData.trashcan = scanData.path + w_plotspec_array(3)
	w_viewscan_id.file = scanData.trashcan

	if scanData.option ne 0 and  strlen(scanData.pv) lt 2 then begin
	st = [ $
	'Note:  ', $
	'',$
	'      You first have to set up the scan PV name by using the', $
	'           Setup->Scan ...    menu ' $
	]
	w_warningtext,st
	if scanData.nosave eq 0 then return
	end


	found = findfile(scanData.trashcan)
	if found(0) eq '' then begin
		st = ['Filename will be created','',scanData.trashcan, $
		'','Otherwise use the "File" menu to set up the catcher file.', $
		'Then use the "Setup" menu to set up the scan PV names.']
		mes = widget_message(st,/Error)
		return
		end
	

WIDGET_CONTROL,/HOURGLASS

;	scan_read_all ,unit, maxno
	scan_read,unit,-1,-1,maxno

	if scanData.y_scan  then scanData.y_seqno = maxno

if scanData.option gt 0 then begin
if strlen(scanData.y_pv) gt 1  and caSearch(scanData.y_pv+'.EXSC') eq 0 then begin
        pventry2_event
        end
if strlen(scanData.pv) gt 1  and caSearch(scanData.pv+'.EXSC') eq 0 then begin

        pventry_event
        end
end

; set plot menu options

	w_plotspec_id.x_axis_u = 0
	w_plotspec_id.type = 0
	w_plotspec_id.log = 0
	w_plotspec_id.grid = 0
	w_plotspec_id.errbars = 0
	w_plotspec_id.xcord = 0

;	w_plotspec_id.scan = 1

; sensitive off on  acquisition and autosave menu 
;	setupoptionsmenu_sensitive,1,0
	setupoptionsmenu_sensitive,4,0


END

; DO NOT REMOVE THIS COMMENT: END MAIN13_1
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO DC, config=config, data=data, nosave=nosave, viewonly=viewonly, GROUP=Group
;+
; NAME:
;       DC
;
; PURPOSE:
;	This procedure runs the scanSee 1D/2D data displayer
;
; CALLING SEQUENCE:
;       DC [,Group=wid] [,Config=config] [,Data=data]  
;
; INPUTS:
;       None.
; KEYWORD PARAMETERS:
;       Group:       Parent group id 
;       Config:      Specifies the configuration file, default to DC.config
;       Data:        Specifies the input file name for scan data
;
; EXAMPLE:
;
;       .run DC 
;       DC
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Sept 4, 1998.
;	xx-xx-xxxx	comment
;-
;
COMMON SYSTEM_BLOCK,OS_SYSTEM
 COMMON CATCH1D_COM, widget_ids, scanData
COMMON w_plotspec_block, w_plotspec_ids, w_plotspec_array , w_plotspec_id, w_plotspec_limits, w_plotspec_saved
COMMON w_viewscan_block, w_viewscan_ids, w_viewscan_id
COMMON realtime_block, realtime_id, realtime_retval, realtime_pvnames
COMMON field_block, field_max, field_name, field_name_array, field_value,  field_label_array,w_scanfield_ids
COMMON font_block, text_font, graf_font, ps_font
COMMON catcher_setup_block,catcher_setup_ids,catcher_setup_scan


  if XRegistered('MAIN13_1') NE 0 then return

; text_font = '6x13'
; graf_font = text_font
; ps_font ='9x15'
;Widget_Control, Default_Font= text_font

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  MAIN13_1 = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
      MAP=1, /TLB_SIZE_EVENTS, $
      TLB_FRAME_ATTR = 8, $
      TITLE='scanSee (R1.2)', $
      UVALUE='MAIN13_1')

  BASE68 = WIDGET_BASE(MAIN13_1, $
	/ROW, $
      MAP=1, $
;      FRAME=2, $
;      TITLE='Top Menu Line', $
      UVALUE='BASE68')

  MenuDesc1981 = [ $
      { CW_PDMENU_S,       3, 'File' }, $ ;        0
;        { CW_PDMENU_S,       0, 'New ...' }, $ ;        1
        { CW_PDMENU_S,       0, 'Open ...' }, $ ;        1
        { CW_PDMENU_S,       0, 'Printer ...' }, $ ;        2
        { CW_PDMENU_S,       2, 'Quit' } $  ;      4
  ]

  PDMENU127 = CW_PDMENU( BASE68, MenuDesc1981, /RETURN_FULL_NAME, $
      UVALUE='PDMENU127')


  SETUPOPTIONSMENU = setupOptions( BASE68, UVALUE='SETUPOPTIONSMENU')

  PLOTOPTIONSMENU = plotOptions( BASE68, UVALUE='PLOTOPTIONSMENU')


;  Btns_mode = [ $
;    'Scan', $
;    'View' ]
;  BGROUP_mode = CW_BGROUP( BASE68, Btns_mode, $
;      COLUMN=2, $
;      FRAME=1, $
;      LABEL_LEFT='Mode:', $
;      EXCLUSIVE=1, $
;      UVALUE='BGROUP_MODE')
;  WIDGET_CONTROL, BGROUP_mode, SET_VALUE=0

  MenuHelp = [ $
      { CW_PDMENU_S,       3, 'Help' }, $ ;        0
        { CW_PDMENU_S,       0, 'Version ...' }, $ ;        1
        { CW_PDMENU_S,       0, 'Release Note ...' }, $ ;        1
        { CW_PDMENU_S,       0, 'Help ...' } $ ;        1
	]

  PDMENU_help = CW_PDMENU( BASE68, MenuHelp, /RETURN_FULL_NAME, $
      UVALUE='HELPMENU')

; add the 1D/2D view memu

  MenuVData = [ $
      { CW_PDMENU_S,       3, 'ViewData' }, $ ;        0
        { CW_PDMENU_S,       0, '1D ...' }, $ ;        1
        { CW_PDMENU_S,       0, '2D ...' } $ ;        1
;        { CW_PDMENU_S,       0, '1D Overlay ...' } $ ;        1
	]

  PDMENU_VDATA = CW_PDMENU( BASE68, MenuVData, /RETURN_FULL_NAME, $
      UVALUE='PDMENU_VDATA')


  SAVE_LABEL = WIDGET_LABEL( BASE68, $
      FONT='-bitstream-charter-bold-i-normal--25-240-75-75-p-154-iso8859-1', $
	xsize=150, VALUE='          ')

  BASE69 = WIDGET_BASE(MAIN13_1, $
      ROW=1, $
      MAP=1, $
      TITLE='FILE', $
      UVALUE='BASE69')

  CATCHER_FILE = WIDGET_LABEL(BASE69, $
	/ALIGN_LEFT,/DYNAMIC_RESIZE, VALUE='')

  BASE140 = WIDGET_BASE(MAIN13_1, $
      ROW=1, $
;      FRAME=2, $
      MAP=1, $
      TITLE='STATUS', $
      UVALUE='BASE140')

  FieldVal1988 = [ $
    '>> PV NOT VALID <<' ]
  FIELD141 = CW_FIELD( BASE140,VALUE=FieldVal1988, $
      ROW=1, $
      STRING=1, $
      TITLE='Status', $
      UVALUE='FIELD141', $
      XSIZE=40)

  BASE140_1 = WIDGET_BASE(BASE140, $
      ROW=1, $
      MAP=1, YSIZE=30, $
      UVALUE='BASE140_1')
  MenuPrint = [ $
      { CW_PDMENU_S,       3, 'Print' }, $ ;        0
        { CW_PDMENU_S,       0, 'Plot' }, $ ;        1
        { CW_PDMENU_S,       2, 'Report ...' } $ ;        4
  ]
  PDMENU_print = CW_PDMENU( BASE140_1, MenuPrint, /RETURN_FULL_NAME, $
      UVALUE='PRINTMENU')

  MenuZoom = [ $
      { CW_PDMENU_S,       3, 'Zoom' }, $ ;        0
        { CW_PDMENU_S,       0, 'Zoom To Box' }, $ ;    1   
        { CW_PDMENU_S,       0, 'Zoom In/Out' }, $ ;        2
        { CW_PDMENU_S,       0, 'Calc Slopes' }, $ ;       3
        { CW_PDMENU_S,       0, 'Auto Scale (Refresh)' }, $ ;       4
        { CW_PDMENU_S,       2, 'User Scale ...' } $ ;       5
  ]
  PDMENU_zoom = CW_PDMENU( BASE140_1, MenuZoom, /RETURN_FULL_NAME, $
      UVALUE='ZOOMMENU')

; statistic menu

  MenuStatistic = [ $
      { CW_PDMENU_S,       3, 'Statistic' }, $ ;        0
        { CW_PDMENU_S,       0, 'None' }, $ ;        1
        { CW_PDMENU_S,       0, 'Peak/Centroid/FWHM on plot' }, $ ;        1
        { CW_PDMENU_S,       0, 'Peak/Centroid/FWHM ...' }, $ ;        1
        { CW_PDMENU_S,       2, 'Average/Deviation ...' } $ ;        1
  ]

  PDMENU_statistic = CW_PDMENU( BASE140_1, MenuStatistic, /RETURN_FULL_NAME, $
      UVALUE='STATISTICMENU')

;  fitting_1d = WIDGET_BUTTON(BASE140_1,VALUE='Fitting',UVALUE='EZFIT_FITTING')
; fitting menu
  MenuFitting = [ $
      { CW_PDMENU_S,       3, 'Fitting' }, $ ;        0
        { CW_PDMENU_S,       0, 'Ez_Fit ...' }, $ ;        1
        { CW_PDMENU_S,       2, '1D Binary'} $ ;        1
  ]

  PDMENU_fitting = CW_PDMENU( BASE140_1, MenuFitting, /RETURN_FULL_NAME, $
      UVALUE='FITTINGMENU')

  BASE61 = WIDGET_BASE(MAIN13_1, $
      COLUMN=1, $
      MAP=1, $
      TITLE='Plot Area', $
      UVALUE='BASE61')


  DRAW61 = WIDGET_DRAW( BASE61, $
      BUTTON_EVENTS=1, $
      RETAIN=2,  $
      UVALUE='DRAW61', $
      XSIZE=400, $
      YSIZE=300)


  BASE144 = WIDGET_BASE(MAIN13_1, $
;      COLUMN=1, $
      ROW=1, $
;      FRAME=2, $
      MAP=1, $
      TITLE='WF Selector', $
      UVALUE='BASE144')

  Btns1994 = [ $
    'D1', $
    'D2', $
    'D3', $
    'D4', $
    'D5', $
    'D6', $
    'D7', $
    'D8', $
    'D9', $
    'D10', $
    'D11', $
    'D12', $
    'D13', $
    'D14', $
    'D15', $
    'P1', $
    'P2', $
    'P3', $
    'P4' $
	 ]
  BGROUP145 = CW_BGROUP( BASE144, Btns1994, $
      ROW=2, $
      NONEXCLUSIVE=1, $
      LABEL_LEFT='Y', $
      UVALUE='BGROUP145')

  BASE144_1 = WIDGET_BASE(BASE144, $
      COLUMN=1, $
      FRAME=2, $
      MAP=1, $
      TITLE='Image', $
      UVALUE='BASE144_1')

;  Btns913 = ['#','P1','P2','P3','P4', $
;	     'D1','D2','D3','D4','D5','D6','D7','D8', $
;	     'D9','D10','D11','D12','D13','D14','D15']

; if detector for X axis is desired just comment out the following line
  Btns913 = ['#','P1','P2','P3','P4']

  pick_xaxis = WIDGET_DROPLIST(BASE144_1, VALUE=BTNS913, $
        UVALUE='PICK_XAXIS',TITLE='Xaxis')
  WIDGET_CONTROL,pick_xaxis,set_droplist_select = 1

   Btns912 = ['Small','Large', 'D1','D2','D3','D4','D5','D6','D7','D8', $
	     'D9','D10','D11','D12','D13','D14','D15']

  pick_image = WIDGET_DROPLIST(BASE144_1, VALUE=BTNS912, $
        UVALUE='PICK_IMAGE',TITLE='Images')


; set drawing area as wide as window width
win_state = WIDGET_INFO(MAIN13_1, /GEOMETRY)
WIDGET_CONTROL, DRAW61, DRAW_XSIZE=win_state.scr_xsize 

  WIDGET_CONTROL, MAIN13_1, /REALIZE


  ; Get drawable window index

  COMMON DRAW61_Comm, DRAW61_Id
  WIDGET_CONTROL, DRAW61, GET_VALUE=DRAW61_Id

@DC.init

; get start home work directory

  CD,'.',CURRENT=old_path
  scanData.home=old_path

;  default is acquisition mode now

	scanData.option = 1
	w_plotspec_id.mode = 0

if keyword_set(viewonly) then  scanData.option = 0
 
; check for input file names
;

if keyword_set(config) then scanData.config = config

if keyword_set(data) then w_plotspec_array(3) = data 

if keyword_set(nosave) then scanData.nosave = 1 

  catch1d_scanInitSetup

WIDGET_CONTROL,widget_ids.trashcan, SET_VALUE = scanData.trashcan

  XMANAGER, 'MAIN13_1', MAIN13_1, CLEANUP='catcher_close'
;  XMANAGER, 'MAIN13_1', MAIN13_1, CLEANUP='catcher_close',NO_BLOCK=0


END