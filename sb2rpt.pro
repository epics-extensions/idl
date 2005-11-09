@PS_open.pro

PRO SB2RPT_1D,SSD,option=option,format=format
	if n_elements(*SSD.da(SSD.rank-1)) eq 0 then return
	if keyword_set(option) eq 0 then option=0
	if keyword_set(format) eq 0 then format='G18.8'
	if keyword_set(xaxis) eq 0 then xaxis=0
	def = SSD.id_def(*,SSD.rank-1)
	nip = SSD.nb_pos(SSD.rank-1)
	nid = SSD.nb_det(SSD.rank-1)
	t_column = nip+nid
	ip = def(0:3)
	id_def = def(4:88)
	pa = *SSD.pa(SSD.rank-1)
	da1d = *SSD.da(SSD.rank-1)
        HD_D=SSD.HD_D(*,SSD.rank-1)
	HD_P=SSD.HD_P(*,SSD.rank-1)
	
	sz = size(da1d)

	id = where(id_def > 0)
	no = n_elements(id)

	format = format
; output ascii data array

	fmt = strmid(format,1,10)
	t_format = '('+format +')'
	t_digit = fix(fmt)

	twd = t_digit*(t_column+1)
	s0 = string(replicate(32b,twd))

  CD,current=p
  path = p + !os.file_sep + 'ASCII' + !os.file_sep
  report = path+SSD.class+'_1d.txt'
  openw,1,report
  if SSD.rank eq 3 then $
  printf,1,"; 1D Array from 3D Scan #:",SSD.scanno
  if SSD.rank eq 2 then $
  printf,1,"; 1D Array from 2D Scan #:",SSD.scanno
printf,1,'; SCAN Record Name: ',SSD.pv[SSD.rank-1]
printf,1,';'
printf,1,"; MDA file:   ",SSD.file
if SSD.rank gt 1 then ts = (*SSD.ts2)[0] else ts = SSD.ts1
printf,1,"; Time Stamp: ",ts
printf,1,"; Comment:    ", ''
printf,1,'; '
printf,1,'; SCAN Data:'
printf,1,'; '
	SB2RPT_header_dump,SSD,nip,ip,sz,id_def,HD_P,HD_D,t_digit,s0,option=option

	SB2RPT_short_dump,pa,ip,sz,da1d,id_def,t_format,t_digit,s0
  close,1
  xdisplayfile,report
END

PRO SB2RPT_header_dump,SSD,nip,ip,sz,id_def,HD_P,HD_D,t_digit,s0,option=option
if option eq 2 then  return 

	vers = SSD.vers
        DI = [ 'D1','D2','D3','D4','D5','D6','D7','D8','D9', $
        'DA','DB','DC','DD','DE','DF', $
    'D01', 'D02', 'D03', 'D04', 'D05', 'D06', 'D07', 'D08', 'D09', 'D10' ] 
	DI = [DI,'D'+strtrim(indgen(60)+11,2)]
	if SSD.vers then DI = D[15:84]

	ypvs = HD_D.DXPV
	ydescs = HD_D.DXDS
	yengus = HD_D.DXEU
	xpvs = HD_P.PXPV
	xdescs = HD_P.PXDS
	xengus = HD_P.PXEU
l1 = intarr(nip+sz(2))
st = s0
strput,st,'; (DIS)   ',0  & ij=t_digit
	if nip gt 0 then begin
	  for i=0,3 do begin
	  if ip(i) then begin
	    pn = 'P'+strtrim(i+1,2)
	    strput,st,pn,ij
;	    l = max(strlen([xpvs[i],xdescs[i],xengus[i]]))
;	    if l gt t_digit then l1[i] = l else l1[i]=t_digit
;	    ij = ij + l1[i] + 1
	    ij = ij+t_digit
	  end
	  end
	end
	for i=0,sz(2)-1 do begin
	if id_def(i) then begin
	  strput,st,DI(i),ij
;	  l = max(strlen([ypvs[i],ydescs[i],yengus[i]]))
;	  if l gt t_digit then l1[i+nip]=l else l1[i+nip]=t_digit
;	  ij = ij + l1[nip+i] + 1
	  ij = ij+t_digit
	end
	end
printf,1,st

st = s0
strput,st,'; (PVS)   ',0  & ij=t_digit
	if nip gt 0 then begin
	  for i=0,3 do begin
	  if ip(i) then begin
	    strput,st,strmid(xpvs(i),0,t_digit-1),ij
;	    ij = ij + l1[i] + 1
	    ij = ij+t_digit
	  end
	  end
	end
	for i=0,sz(2)-1 do begin
	if id_def(i) then begin
	  strput,st,strmid(ypvs(i),0,t_digit-1),ij
;	  ij = ij + l1[nip+i] + 1
	  ij = ij+t_digit
	end
	end
printf,1,st

st = s0
strput,st,'; (DESCS) ',0  & ij=t_digit
	if nip gt 0 then begin
	  for i=0,3 do begin
	  if ip(i) then begin
	    strput,st,strmid(xdescs(i),0,t_digit-1),ij
;	    ij = ij + l1[i] + 1
	    ij = ij+t_digit
	  end
	  end
	end
	for i=0,sz(2)-1 do begin
	if id_def(i) then begin
	  strput,st,strmid(ydescs(i),0,t_digit-1),ij
;	  ij = ij + l1[nip+i] + 1
	  ij = ij+t_digit
	end
	end
printf,1,st
st = s0
strput,st,'; (UNITS) ',0  & ij=t_digit
	if nip gt 0 then begin
	  for i=0,3 do begin
	  if ip(i) then begin
	    strput,st,strmid(xengus(i),0,t_digit-1),ij
;	    ij = ij + l1[i] + 1
	    ij = ij+t_digit
	  end
	  end
	end
	for i=0,sz(2)-1 do begin
	if id_def(i) then begin
	  strput,st,strmid(yengus(i),0,t_digit-1),ij
;	  ij = ij + l1[nip+i] + 1
	  ij = ij+t_digit
	end
	end
printf,1,st

END

PRO SB2RPT_data_dump,sb2rpt_data,y_seqno,scanH=scanH
;	scanH = sb2rpt_data.scanH

	SSD = sb2rpt_data.SSD
	if n_elements(scanH) eq 0 then scanH = 0

	if SSD.rank ge 2 then begin
	  if SSD.rank eq 2 and scanH gt 0 then begin
	  	def = SSD.id_def(*,1)
	  	nip = SSD.nb_pos(1)
	  	nid = SSD.nb_det(1)
	  endif else begin
		def = SSD.id_def(*,SSD.rank-2)
		nip = SSD.nb_pos(SSD.rank-2)
		nid = SSD.nb_det(SSD.rank-2)
	  end
	endif else begin
		def = SSD.id_def(*,0)
		nip = SSD.nb_pos(0)
		nid = SSD.nb_det(0)
	end
	t_column = nip+nid

	ip = def(0:3)
	id_def = def(4:88)

	if SSD.rank ge 2 then begin
	  if SSD.rank eq 2 and scanH gt 0 then begin
	  	pa = *SSD.pa(1)
	  	da1d = *SSD.da(1)
	  endif else begin 
		pa = *SSD.pa(SSD.rank-2)
		da1d = *SSD.da(SSD.rank-2)
		da1d = da1d(*,y_seqno-1,*)
		sz = size(da1d)
		da1d = reform(da1d,sz(1),sz(3))
	  end
	end	
	if SSD.rank eq 1 then begin
	  pa = *SSD.pa(0)
	  da1d = *SSD.da(0)
	end	

	sz = size(da1d)

	id = where(id_def > 0)
	no = n_elements(id)

	HD_D = SSD.HD_D(*,0)
	if SSD.rank ge 2 then begin
	  HD_D=SSD.HD_D(*,SSD.rank-2)
	  if SSD.rank eq 2 and scanH gt 0 then HD_D=SSD.HD_D(*,1)
	end

	HD_P = SSD.HD_P(*,0)
	if SSD.rank ge 2 then begin
	  HD_P=SSD.HD_P(*,SSD.rank-2)
	  if SSD.rank eq 2 and scanH gt 0 then HD_D=SSD.HD_P(*,1)
	end

	format = sb2rpt_data.format
; output ascii data array

	fmt = strmid(format,1,10)
	t_format = '('+format +')'
	t_digit = fix(fmt)

twd = t_digit*(t_column+1)
s0 = string(replicate(32b,twd))
st = s0

	SB2RPT_header_dump,SSD,nip,ip,sz,id_def,HD_P,HD_D,t_digit,s0,option=sb2rpt_data.option


	SB2RPT_short_dump,pa,ip,sz,da1d,id_def,t_format,t_digit,s0

END

PRO SB2RPT_short_dump,pa,ip,sz,da1d,id_def,t_format,t_digit,s0
	for i=0,sz(1)-1 do begin
	st = s0
	strput,st,i,0 & ij=10
	; may need nb_pos info
	  for j=0,3 do begin
	    if ip(j) then begin
	      strput,st,string(pa(i,j),format=t_format),ij
	      ij = ij+t_digit
	    end
	  end
	; detectors
	  for j=0,sz(2)-1 do begin
	    if id_def(j) then begin
	      strput,st,string(da1d(i,j),format=t_format),ij
	      ij = ij+t_digit
	    end
	  end
printf,1,st
	end

printf,1,''

END

PRO SB2RPT_full_dump,sb2rpt_data

SSD = sb2rpt_data.SSD
rank = SSD.rank

for y_seqno=sb2rpt_data.startid,sb2rpt_data.endid do begin

; get new file name
if sb2rpt_data.separate eq 0 then SB2RPT_outfile,sb2rpt_data else $
SB2RPT_outfile,sb2rpt_data , seqno=y_seqno

str = '; 2D SCAN #: '
if rank eq 3 then str = '; 3D SCAN #: '

if sb2rpt_data.separate eq 1 then openw,1,sb2rpt_data.outfile else begin
	if y_seqno eq sb2rpt_data.startid then openw,1,sb2rpt_data.outfile
end

; abrivated header
if sb2rpt_data.option lt 2 then begin
if rank gt 1 then begin
printf,1,str,SSD.scanno,",      Y INDEX #:",y_seqno
        end

pvs = SSD.pv
if rank gt 1 then pv = pvs[rank-2] else pv = pvs[0]
if rank gt 2 then printf,1,"; 2D SCAN #: ",y_seqno else $
printf,1,"; 1D SCAN #: ",y_seqno
printf,1,"; SCAN Record Name: ", pv
end

; full header
if sb2rpt_data.option eq 0 then begin
printf,1,'; '
printf,1,"; MDA file:   ",SSD.file
if rank gt 1 then ts = (*SSD.ts2)[y_seqno-1] else ts = SSD.ts1
printf,1,"; Time Stamp: ",ts
printf,1,"; Comment:    ", ''
printf,1,'; '
printf,1,'; SCAN Data:'
printf,1,'; '
end

	scanH = sb2rpt_data.scanH
	SB2RPT_data_dump,sb2rpt_data,y_seqno,scanH=scanH

if sb2rpt_data.separate then close,1 else begin
	if y_seqno eq sb2rpt_data.endid then close,1
end
end

END

PRO SB2RPT_outfile,sb2rpt_data , seqno=seqno
    if sb2rpt_data.scanH gt 0 then begin
	outfile = sb2rpt_data.path + sb2rpt_data.class + '.1d.txt'
    endif else begin
	str = '0000'
	sss = str
	if keyword_set(seqno) then begin
	  s1 = strtrim(seqno,2)
	  len = strlen(s1)
	  strput,sss,s1,4-len
	  outfile = sb2rpt_data.path + sb2rpt_data.class + '.' + sss
	  sb2rpt_data.outfile = outfile
	  return
	end

	s1 = strtrim(sb2rpt_data.startid,2)
	len = strlen(s1)
	strput,sss,s1,4-len
	outfile = sb2rpt_data.path + sb2rpt_data.class + '.' + sss
	
	if sb2rpt_data.startid ne sb2rpt_data.endid then begin
	if sb2rpt_data.separate eq 0 then begin
	SSD = sb2rpt_data.SSD
	if SSD.rank gt 1 then begin
	eee = str
	s1 = strtrim(sb2rpt_data.endid,2)
	len = strlen(s1)
	strput,eee,s1,4-len
	outfile = outfile + '_'+eee
	end
	end
	end
    end
    sb2rpt_data.outfile= outfile
    widget_control,sb2rpt_data.WIDoutfile,set_value=outfile

END

PRO SB2RPT_Event, Event

  widget_control,Event.top,get_uvalue= sb2rpt_data,/no_copy
	SSD = sb2rpt_data.SSD
	rank = SSD.rank

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'SB2_RPT_OPTION': BEGIN
	sb2rpt_data.option = Event.Value
      END
  'SB2_RPT_FORMAT': BEGIN
	widget_control,Event.Id,get_value=fmt
	sb2rpt_data.format = fmt
      END
  'SB2_RPT_INFILE': BEGIN 
;	widget_control,Event.Id,get_value=fmt
;	sb2rpt_data.infile = fmt
      END
  'SB2_RPT_START': BEGIN
	widget_control,Event.Id,get_value=fmt
	sb2rpt_data.startid = fmt
	if fmt le 0 then sb2rpt_data.startid = 1
	if rank eq 1 then sb2rpt_data.startid = 1
	if rank gt 1 then begin
		top = SSD.cpt(rank-1)
		if fmt ge top then sb2rpt_data.startid = top
		widget_control,Event.ID,set_value=sb2rpt_data.startid
	end
	SB2RPT_outfile,sb2rpt_data
      END
  'SB2_RPT_END': BEGIN
	widget_control,Event.Id,get_value=fmt
	sb2rpt_data.endid = fmt
	if fmt le 0 then sb2rpt_data.endid = 1
	if rank eq 1 then sb2rpt_data.endid = 1
	if rank gt 1 then begin
		top = SSD.cpt(rank-1)
		if sb2rpt_data.scanH gt 0 then top=1
		if fmt ge top then sb2rpt_data.endid = top
		widget_control,Event.ID,set_value=sb2rpt_data.endid
	end
	SB2RPT_outfile,sb2rpt_data
      END
  'SB2_RPT_SEPARATE': BEGIN
	sb2rpt_data.separate = Event.Value
      END
  'SB2_RPT_OUTFILE': BEGIN
	widget_control,Event.Id,get_value=fmt
	sb2rpt_data.outfile = fmt
      END
  'SB2_RPT_GENERATE': BEGIN
	widget_control,sb2rpt_data.WIDstart,get_value=st
	sb2rpt_data.startid = st
	if st le 0 then sb2rpt_data.startid = 1
	if rank eq 1 then sb2rpt_data.startid = 1
	if rank gt 1 then begin
		top = SSD.cpt(rank-1)
		if st ge top then sb2rpt_data.startid = top
		widget_control,sb2rpt_data.WIDstart,set_value=sb2rpt_data.startid
	end
	widget_control,sb2rpt_data.WIDend,get_value=st
	sb2rpt_data.endid = st
	if st le 0 then sb2rpt_data.endid = 1
	if rank eq 1 then sb2rpt_data.endid = 1
	if rank gt 1 then begin
		top = SSD.cpt(rank-1)
		if st ge top then sb2rpt_data.endid = top
		widget_control,sb2rpt_data.WIDend,set_value=sb2rpt_data.endid
	end
	SB2RPT_outfile,sb2rpt_data
        SB2RPT_full_dump,sb2rpt_data
      END
  'SB2_RPT_VIEW': BEGIN
	xdisplayfile,sb2rpt_data.outfile,title=sb2rpt_data.outfile,width=100
      END
  'SB2_RPT_CLOSE': BEGIN
	widget_control,Event.top,/destroy
	return
      END
  'SB2_RPT_PRINT': BEGIN
	PS_enscript,sb2rpt_data.outfile
      END
  ENDCASE
  widget_control,Event.top,set_uvalue= sb2rpt_data,/no_copy
END



PRO SB2RPT,SSD,GROUP=Group,scanH=scanH

  if n_elements(SSD) eq 0 then return
  if n_elements(scanH) eq 0 then scanH=0
  file = SSD.file
  class = SSD.class
  cl = strsplit(class,'.',/extract)
  class = cl[0]
  CD,current=p
  path = p + !os.file_sep + 'ASCII' + !os.file_sep
  fd = findfile(path,count=ct)
  if ct eq 0 then spawn,!os.mkdir + ' ' +path
  outfile = path+class+'.0001'
  if scanH gt 0 then outfile = path + class + '.1d.txt'

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  title = 'Report from 2D Array'
  if scanH gt 0 or SSD.rank eq 1 then title = 'Report from 1D Array'
  SB2RPT = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, title=title, $
      UVALUE='SB2RPT')

  BASE2 = WIDGET_BASE(SB2RPT, $
      COLUMN=1, $
      MAP=1, $
      TITLE='Report ...', $
      UVALUE='BASE2')

  BASE3 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE3')

  Btns958 = [ $
    'Full', $
    'Abbreviated', $
    'None' ]
  BGROUP14 = CW_BGROUP( BASE3, Btns958, $
      ROW=1, $
      EXCLUSIVE=1, $
      LABEL_LEFT='Header options:', $
      UVALUE='SB2_RPT_OPTION')
  widget_control,BGROUP14,set_value=0

  FieldVal1170 = [ $
    'G18.8' ]
  FIELD9 = CW_FIELD( BASE2,VALUE=FieldVal1170, $
      ROW=1, $
      STRING=1, /return_events, $
      TITLE='Output Data Format:', $
      UVALUE='SB2_RPT_FORMAT')

  FIELD11 = CW_FIELD( BASE2,VALUE=File, $
      ROW=1, xsize=70, $
      STRING=1, /NOEDIT, $ ;/return_events, $
      TITLE='Data file name:', $
      UVALUE='SB2_RPT_INFILE')

  FieldVal1315 = [ $
    '1' ]
  FIELD12 = CW_FIELD( BASE2,VALUE=FieldVal1315, $
      ROW=1, $
      INTEGER=1, /return_events, $
      TITLE='Start Scan #', $
      UVALUE='SB2_RPT_START')

  FieldVal1380 = [ $
    '1' ]
  FIELD13 = CW_FIELD( BASE2,VALUE=FieldVal1380, $
      ROW=1, $
      INTEGER=1, /return_events, $
      TITLE='End Scan # ', $
      UVALUE='SB2_RPT_END')

  Btns1708 = [ $
    'No', $
    'Yes' ]
  BGROUP17 = CW_BGROUP( BASE2, Btns1708, $
      ROW=1, $
      EXCLUSIVE=1, $
      LABEL_LEFT='Save Selected Scans as Separate ASCII files:', $
      UVALUE='SB2_RPT_SEPARATE')
  widget_control,BGROUP17,set_value=1

  FIELD19 = CW_FIELD( BASE2,VALUE=outfile, $
      ROW=1, xsize=70, $
      STRING=1, /return_events, $
      TITLE='Output file Name: ', $
      UVALUE='SB2_RPT_OUTFILE')

  BASE20 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE20')

  BUTTON21 = WIDGET_BUTTON( BASE20, $
      UVALUE='SB2_RPT_GENERATE', $
      VALUE='Generate Report')

  BUTTON22 = WIDGET_BUTTON( BASE20, $
      UVALUE='SB2_RPT_VIEW', $
      VALUE='View Report ...')

  BUTTON28 = WIDGET_BUTTON( BASE20, $
      UVALUE='SB2_RPT_PRINT', $
      VALUE='Print Report')

  BUTTON23 = WIDGET_BUTTON( BASE20, $
      UVALUE='SB2_RPT_CLOSE', $
      VALUE='Close')

  sb2rpt_data = { $
	WIDstart: FIELD12, $
	WIDend: FIELD13, $
	WIDoutfile: FIELD19, $
	option: 0, $
	scanH : scanH, $
	format: 'G18.8', $
	infile: file, $
	path: path, $
	class: class, $
	startid : 1, $	
	endid : 1, $	
	separate: 1, $
	outfile: outfile, $
	SSD : SSD $
	}

  widget_control,SB2RPT,set_uvalue= sb2rpt_data

  WIDGET_CONTROL, SB2RPT, /REALIZE

  XMANAGER, 'SB2RPT', SB2RPT
END
