
@wc.pro
@panimage.pro

PRO h5b_configWrite,file
        ; write config
        openw,1,'h5b.config'
        printf,1,file
        close,1
END

PRO h5b_configRead,file
        file=''
        openr,1,'h5b.config'
        readf,1,file
        close,1
END

PRO h5b_sdsDumpHeader,h5b_sdsState,st=st
	dname = h5b_sdsState.dlist
	file = h5b_sdsState.file
	fid = h5f_open(file)
	no = n_elements(dname)
	st = ''
	for i=0,no-1 do begin
	did = h5d_open(fid,dname(i))	
	data = h5d_read(did)
	h5d_close,did
	num = n_elements(data)
	if num gt 5 then num = 6
	st1 = [h5b_sdsState.list1(i),string(data(0:num-1),/print)]
	st = [st , st1]
	end
	h5f_close,fid
END

PRO h5b_sdshelp
st = ['DumpSDSHeader - dump header for all SDS found in file', $
  'First	- list the first SDS dataset found in file', $
  'Next		- list the next SDS dataset found in file', $
  'Ppev		- list the prev SDS dataset found in file', $
  'Last		- list the last SDS dataset found in file', $
  'SDS #	- enter the SDS dataset sequence number in file', $
  'Slider       - pick the SDS dataset number from file', $
  'Multiple SDSs... - popup dialog for entering  multiple seq #', $
  'Scroll List  - display complete list of SDS dataset names found in file', $
  'Close        - close the SDS dialog', $
  '']
r = dialog_message(st,/info)

END

PRO h5b_SDS_multi1d,h5b_sdsState,seq=seq,data=data,dname=dname,echo=echo
; seq  - specify selected seq of 1D data
; data - return image array
; dname - return dname
	dname = h5b_sdsState.dlist(seq)
	file = h5b_sdsState.file
	fid = h5f_open(file)
	no = n_elements(seq)
	did = h5d_open(fid,dname(0))	
	pda = h5d_read(did)
	h5d_close,did
	sz = size(pda)
	if sz(0) eq 1 then begin
	data = make_array(sz(1),no)
	data(*,0) = pda(*)
	for i=1,no-1 do begin
	did = h5d_open(fid,dname(i))	
	pda = h5d_read(did)
	h5d_close,did
	s = size(pda)
	if s(0) eq 1 and s(1) eq sz(1) then $
	data(*,i) = pda(*)
	end
	end
	h5f_close,fid

END


PRO h5b_SDS_multi2d,h5b_sdsState,seq=seq,data=data,dname=dname,echo=echo
; seq  - specify selected seq of 2D data
; data - return image array
; dname - return dname
	dname = h5b_sdsState.dlist(seq)
	file = h5b_sdsState.file
	fid = h5f_open(file)
	no = n_elements(seq)
	did = h5d_open(fid,dname(0))	
	pda = h5d_read(did)
	h5d_close,did
	sz = size(pda)
	if sz(0) eq 2 then begin
	data = make_array(sz(1),sz(2),no)
	data(*,*,0) = pda(*,*)
	for i=1,no-1 do begin
	did = h5d_open(fid,dname(i))	
	pda = h5d_read(did)
	h5d_close,did
	s = size(pda)
	if s(0) eq 2 and s(1) eq sz(1) and s(2) eq sz(2) then $
	data(*,*,i) = pda(*,*)
	end
	end
	h5f_close,fid

	if keyword_set(echo) then panimage,data
END

PRO H5B_SDSMULTI_Event, Event

  widget_control,Event.top,get_uvalue=h5b_multiState
  h5b_sdsState = h5b_multiState.sdsState

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'H5B_SDSMULTI1DSEQ': BEGIN
	widget_control,h5b_multiState.oneWID,get_value=st
	if strlen(st) gt 0 then begin
	parse_num,st,res
	id = where(res lt h5b_sdsState.num)
	if res(0) lt h5b_sdsState.num and res(0) ge 0 then begin
        h5b_SDS_multi1d,h5b_sdsState,seq=res(id),data=data,dname=dname,/echo
	if n_elements(data) gt 0 then begin
          sz = size(data)
          if h5b_sdsState.naxis gt 1 then x = *h5b_sdsState.yarr else $
          x = *h5b_sdsState.xarr
          if sz(1) ne n_elements(x) then x = *h5b_sdsState.yarr
          if sz(1) eq n_elements(x) then $
          plot1d,x,data,legend=dname,Group=Event.top else $
          plot1d,data,legend=dname,Group=Event.top,xtitle='Index #'
	endif else begin
          st = ['Error: Invalid SDS # entered!', '', $
                 'Valid SDS # : [0-'+strtrim(h5b_sdsState.num-1,2)+']']
          r = dialog_message(/error,st)
	end
        endif else begin
        st = ['Error: Invalid SDS # entered!', '', $
                 'Valid SDS # : [0-'+strtrim(h5b_sdsState.num-1,2)+']']
        r = dialog_message(/error,st)
        end
	end
      END
  'H5B_SDSMULTI2DSEQ': BEGIN
	widget_control,h5b_multiState.twoWID,get_value=st
	if strlen(st) gt 0 then begin
	parse_num,st,res
	id = where(res lt h5b_sdsState.num)
	if res(0) lt h5b_sdsState.num and res(0) ge 0 then begin
        h5b_SDS_multi2d,h5b_sdsState,seq=res(id),data=data,dname=dname,/echo
        endif else begin
        st = ['Error: Invalid SDS # entered!', '', $
                 'Valid SDS # : [0-'+strtrim(h5b_sdsState.num-1,2)+']']
        r = dialog_message(/error,st)
        end
	end
	
      END
  'H5B_SDSIMAGE2D': BEGIN
	widget_control,h5b_multiState.twoWID,get_value=st
	if strlen(st) gt 0 then begin
	parse_num,st,res
	id = where(res lt h5b_sdsState.num)
	if res(0) lt h5b_sdsState.num and res(0) ge 0 then begin
        h5b_SDS_multi2d,h5b_sdsState,seq=res(id),data=data,dname=dname,/echo
        sz = size(data)
        if sz(0) eq 3 then begin
	if h5b_sdsState.naxis gt 0 then begin
	if n_elements(*h5b_sdsState.xarr) gt 1 then $
        xarr = *h5b_sdsState.xarr
	if n_elements(*h5b_sdsState.yarr) gt 1 then $
        yarr = *h5b_sdsState.yarr
	end
        if n_elements(xarr) eq sz(1) then begin
          if n_elements(yarr) ne sz(2) then $
          image2d,data,xdescs='Index #',ydescs='Index #',zdescs=dname,/seqnm,Group=Event.top else $
          image2d,data,xarr,yarr,zdescs=dname,/seqnm,Group=Event.top
        endif else begin
          if n_elements(yarr) eq sz(1) then begin
            if n_elements(xarr) eq sz(2) then $
            image2d,data,yarr,xarr,zdescs=dname,/seqnm,Group=Event.top else $
            image2d,data,xdescs='Index #',ydescs='Index #',zdescs=dname,/seqnm,Group=Event.top
          endif else image2d,data,xdescs='Index #',ydescs='Index #',zdescs=dname,/seqnm,Group=Event.top
        end
	end
        endif else begin
        st = ['Error: Invalid SDS # entered!', '', $
                 'Valid SDS # : [0-'+strtrim(h5b_sdsState.num-1,2)+']']
        r = dialog_message(/error,st)
        end
	end
      END
  'H5B_SDSMULTIDONE': BEGIN
	widget_control,Event.top,/destroy,bad=bad
	return
      END
  ENDCASE
  widget_control,Event.top,set_uvalue=h5b_multiState
END


PRO h5b_sdsMultiple, h5b_sdsState, GROUP=Group


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  H5B_SDSMULTI = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, title='H5B_SDSMULTI', $
      UVALUE='H5B_SDSMULTI')

  BASE2 = WIDGET_BASE(H5B_SDSMULTI, $
      COLUMN=1, $
      MAP=1, $
      TITLE='SDS CONSTRUCT MULTIPLE ARRAYS', $
      UVALUE='BASE2')

  LABEL3 = WIDGET_LABEL( BASE2, $
      UVALUE='LABEL3', $
      VALUE='Please refer SDS scroll list for data array dimensions')

  LABEL4 = WIDGET_LABEL( BASE2, $
      UVALUE='LABEL4', $
      VALUE='    Only same dimension 1D/2D data can be entered togather')

  LABEL9 = WIDGET_LABEL( BASE2, $
      UVALUE='LABEL9', $
      VALUE='      e.g. range of seq # 47-100')

  LABEL10 = WIDGET_LABEL( BASE2, $
      UVALUE='LABEL10', $
      VALUE='      e.g. comma seperated seq # : 4,5,10-15')

  LABEL11 = WIDGET_LABEL( BASE2, $
      UVALUE='LABEL11', $
      VALUE='  Entered values are accepted with <CR>')

  FieldVal3510 = [ $
    '' ]
  FIELD12 = CW_FIELD( BASE2,VALUE=FieldVal3510, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='1D seq#:', $
      UVALUE='H5B_SDSMULTI1DSEQ', $
      XSIZE=40)

  BASE15 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE15')

  FieldVal3792 = [ $
    '' ]
  FIELD16 = CW_FIELD( BASE15,VALUE=FieldVal3792, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='2D seq#:', $
      UVALUE='H5B_SDSMULTI2DSEQ', $
      XSIZE=40)

  BUTTON17 = WIDGET_BUTTON( BASE15, $
      UVALUE='H5B_SDSIMAGE2D', $
      VALUE='IMAGE2D...')


  BUTTON20 = WIDGET_BUTTON( BASE2, $
      UVALUE='H5B_SDSMULTIDONE', $
      VALUE='Done')

  h5b_multiState = { oneWID: FIELD12, $
	twoWID: FIELD16, $
	sdsState: h5b_sdsState $
	}

  widget_control,H5B_SDSMULTI,set_uvalue=h5b_multiState
  WIDGET_CONTROL, H5B_SDSMULTI, /REALIZE

  XMANAGER, 'H5B_SDSMULTI', H5B_SDSMULTI
END
PRO h5b_sds_getdata,h5b_sdsState

	name = h5b_sdsState.dlist(h5b_sdsState.pick)
	if h5b_sdsState.xaxis ge 0 then xarr = *h5b_sdsState.xarr
	if h5b_sdsState.yaxis ge 0 then yarr = *h5b_sdsState.yarr
	h5b_readArray,name,file=h5b_sdsState.file,data=data,xarr=xarr,yarr=yarr

END

PRO H5B_SDS_Event, Event

  WIDGET_CONTROL, Event.top, get_uvalue=h5b_sdsState

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'H5B_SDSDUMP': BEGIN
	h5b_sdsDumpHeader,h5b_sdsState,st=st
	xdisplayfile,text=st,group=Event.top,title='SDS Dump Header'
      END
  'H5B_SDSFIRST': BEGIN
	h5b_sdsState.pick = 0
	widget_control,h5b_sdsState.listWID,set_list_select=h5b_sdsState.pick
	h5b_sds_getdata,h5b_sdsState
      END
  'H5B_SDSNEXT': BEGIN
	if (h5b_sdsState.pick+1) lt h5b_sdsState.num then begin
	h5b_sdsState.pick = h5b_sdsState.pick+1 
	widget_control,h5b_sdsState.listWID,set_list_select=h5b_sdsState.pick
	h5b_sds_getdata,h5b_sdsState
	end
      END
  'H5B_SDSPREV': BEGIN
	if h5b_sdsState.pick gt 0 then begin
	h5b_sdsState.pick = h5b_sdsState.pick-1 
	widget_control,h5b_sdsState.listWID,set_list_select=h5b_sdsState.pick
	h5b_sds_getdata,h5b_sdsState
	end
      END
  'H5B_SDSLAST': BEGIN
	h5b_sdsState.pick = h5b_sdsState.num-1
	widget_control,h5b_sdsState.listWID,set_list_select=h5b_sdsState.pick
	h5b_sds_getdata,h5b_sdsState
      END
  'H5B_SDSHELP': BEGIN
	h5b_sdshelp
      END
  'H5B_SDSLIST': BEGIN
	p = widget_info(Event.id,/list_select)
	h5b_sdsState.pick = p 
	h5b_sds_getdata,h5b_sdsState
      END
  'H5B_SDSSEQNO': BEGIN
	widget_control,Event.id,get_value=p
	h5b_sdsState.pick = p 
	widget_control,h5b_sdsState.listWID,set_list_select=h5b_sdsState.pick
	h5b_sds_getdata,h5b_sdsState
      END
  'H5B_SDSSLIDER': BEGIN
	widget_control,Event.id,get_value=p
	h5b_sdsState.pick = p 
	widget_control,h5b_sdsState.listWID,set_list_select=h5b_sdsState.pick
	h5b_sds_getdata,h5b_sdsState
      END
  'H5B_SDSMULTIPLE': BEGIN
      Print, 'Event for Multiple SDSs...'
	h5b_sdsMultiple,h5b_sdsState,group=Event.top
      END
  'H5B_SDSREALPLOT': BEGIN
	h5b_sds_getdata,h5b_sdsState
      END
  'H5B_SDSXAXIS': BEGIN
	p = widget_info(h5b_sdsState.listWID,/list_select)
	h5b_sdsState.xaxis = p
      Print, 'Event for set Xaxis',p
	  xname =  h5b_sdsState.dlist(p)
	  h5b_readArray,xname,file=h5b_sdsState.file,data=xarr,/readonly
	  sz = size(xarr)
	  if sz(0) ne 1 then h5b_sdsState.xaxis = -1 else $
	  *h5b_sdsState.xarr = xarr
      END
  'H5B_SDSYAXIS': BEGIN
	p = widget_info(h5b_sdsState.listWID,/list_select)
	h5b_sdsState.yaxis = p
      Print, 'Event for set Yaxis',p
	if h5b_sdsState.yaxis ge 0 then begin
	  yname =  h5b_sdsState.dlist(p)
	  h5b_readArray,yname,file=h5b_sdsState.file,data=yarr,/readonly
	  sz = size(yarr)
	  if sz(0) ne 1 then h5b_sdsState.yaxis = -1 else $
	  *h5b_sdsState.yarr = yarr
	end
      END
  'H5B_SDSCLOSE': BEGIN
	widget_control,Event.top,/destroy,bad=bad
	return
      END
  ENDCASE

  WIDGET_CONTROL, Event.top, set_uvalue=h5b_sdsState

END



PRO h5b_sds,dlist=dlist,title=title,file=file, GROUP=Group

  if keyword_set(file) then begin
	h5b_dNames,name='/',file=file,SDS=SDS
	dlist = SDS
  end

  if keyword_set(dlist) then list1=dlist

   naxis = where(strpos(dlist,'axis') ge 0)
	
  num = n_elements(list1)
if num eq 0 then begin
	r = dialog_message('No SDS dataset found!!',/error)
	return
end

types = ['UNDEFINED','BYTE','INT','LONG','FLOAT','DOUBLE','COMPLEX','STRING','STRUCT']
fid = h5f_open(file)
  for i=0,num-1 do begin
	did = h5d_open(fid,dlist(i))
	data = h5d_read(did)
	h5d_close,did
	sz = size(data)
	type = types(sz(sz(0)+1))
	st = ' : '+ type + ' ['
	for j=1,sz(0) do begin
		st = st + strtrim(sz(j),2)
		if j lt sz(0) then st=st+','
	end
	st = st+']'
  	list1(i) = strtrim(i,2)+': '+list1(i) + st
  end
h5f_close,fid


  if keyword_set(title) eq 0 then title='H5B_SDS'

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  H5B_SDS = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, MAP=1, title = title, $
      UVALUE='H5B_SDS')

  BASE2 = WIDGET_BASE(H5B_SDS, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  BUTTON3 = WIDGET_BUTTON( BASE2, $
      UVALUE='H5B_SDSDUMP', $
      VALUE='DumpSDSHeader')

  BASE4 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE4')

  BUTTON5 = WIDGET_BUTTON( BASE4, $
      UVALUE='H5B_SDSFIRST', $
      VALUE='First')

  BUTTON6 = WIDGET_BUTTON( BASE4, $
      UVALUE='H5B_SDSNEXT', $
      VALUE='Next')

  BUTTON7 = WIDGET_BUTTON( BASE4, $
      UVALUE='H5B_SDSPREV', $
      VALUE='Prev')

  BUTTON8 = WIDGET_BUTTON( BASE4, $
      UVALUE='H5B_SDSLAST', $
      VALUE='Last')

  BUTTON9 = WIDGET_BUTTON( BASE4, $
      UVALUE='H5B_SDSHELP', $
      VALUE='Help...')


  BASE45 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE45')

  FieldVal1213 = [ $
    '0' ]
  FIELD42 = CW_FIELD( BASE45,VALUE=FieldVal1213, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='SDS #', $
      UVALUE='H5B_SDSSEQNO', $
      XSIZE=4)

  SLIDER43 = WIDGET_SLIDER( BASE45, $
	MAX=num, UVALUE='H5B_SDSSLIDER', $
      VALUE=0)

  BUTTON44 = WIDGET_BUTTON( BASE45, $
      UVALUE='H5B_SDSMULTIPLE', $
      VALUE='Multiple SDSs...')

  LIST41 = WIDGET_LIST( BASE2,VALUE=list1, $
      UVALUE='H5B_SDSLIST', $
      YSIZE=7)


  BASE46 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE46')

if naxis ne -1 then begin
  BUTTON48 = WIDGET_BUTTON( BASE46, $
      UVALUE='H5B_SDSREALPLOT', $
      VALUE='RAxis Plot...')

  BUTTON49 = WIDGET_BUTTON( BASE46, $
      UVALUE='H5B_SDSXAXIS', $
      VALUE='Set Xaxis')

  BUTTON50 = WIDGET_BUTTON( BASE46, $
      UVALUE='H5B_SDSYAXIS', $
      VALUE='Set Yaxis')
end

  BUTTON47 = WIDGET_BUTTON( BASE2, $
      UVALUE='H5B_SDSCLOSE', $
      VALUE='Close')

  h5b_sdsState = { base:base2, $
	file : file, $
	dlist : dlist, $
	list1 : list1, $
	num : num, $
	xaxis : -1, $
	yaxis : -1, $
	naxis : naxis, $
	listWID : LIST41, $
	seqWID : FIELD42, $
	pick : -1, $
	xarr : ptr_new(/allocate_heap), $
	yarr : ptr_new(/allocate_heap) $
	}

  WIDGET_CONTROL, H5B_SDS, set_uvalue=h5b_sdsState
  WIDGET_CONTROL, H5B_SDS, /REALIZE

  XMANAGER, 'H5B_SDS', H5B_SDS
END
PRO h5b_getStruct,name,file=file,data=data
	if keyword_set(file) eq 0 then $
	file=filepath('hdf5_test.h5',subdirectory=['examples','data'])

	fid = h5f_open(file)
	catch,error_status
	if error_status ne 0 then begin
		h5f_close,fid
		data = 'Push "? Struct Data..." button to get more info'
		return
	end
	data = h5_parse(fid,name,file=file,/read_data)
	h5f_close,fid
END

PRO h5b_attrs,file,st
; get file attributes

	fid = h5f_open(file)
	gid = h5g_open(fid,'/')
	nga = h5a_get_num_attrs(gid)
	st ='Number of file attributes='+string(nga)
	for j=0,nga-1 do begin
	aid = h5a_open_idx(gid,j)
	an = h5a_get_name(aid)
	rd =h5a_read(aid)
	sti = string(j)+'  '+an+':    '+rd
	st = [st,sti]
	h5a_close,aid
	end
	h5g_close,gid
	h5f_close,fid
END


PRO h5b_dattrs,gname,st,file=file
; get group attributes for give gname

	if keyword_set(file) eq 0 then $
	file=filepath('hdf5_test.h5',subdirectory=['examples','data'])

	st=['']
	fid = h5f_open(file)
	catch,error_status
	if error_status ne 0 then begin
		h5f_close,fid
		return	
	end

	gid = h5d_open(fid,gname)
	nga = h5a_get_num_attrs(gid)
	if nga le 0 then goto,out1

	for i=0,nga-1 do begin 
	aid = h5a_open_idx(gid,i)
	an = h5a_get_name(aid)
	aty = h5a_get_type(aid)
	dim = h5a_get_space(aid)
	att = h5a_read(aid)
	ati = string(i)+'  '+an+':    '+ string(att)
	if i eq 0 then st = ati else st=[st,ati]
	h5t_close,aty
	h5s_close,dim
	h5a_close,aid
	end
out1:
	h5d_close,gid
	h5f_close,fid
END

PRO h5b_checkData,tgn,gid,dlist,parent,SDS=SDS
	if parent eq '/' then parent=''
	name = parent+'/'+tgn
	catch,error_status
	if error_status ne 0 then begin
;		print,!err_string
		dlist = [dlist,name]
		return
	end
	stat = h5g_get_objinfo(gid,name)  ;,/follow_link)
	if stat.type eq 'DATASET' then begin
		if n_elements(dlist) eq 0 then dlist=tgn else $
		dlist = [dlist,name]
		if SDS(0) eq '' then SDS = name else SDS=[SDS,name]
	endif else begin
; non dataset type
;help,stat,/st
		dlist = [dlist,parent+'/'+tgn]
	end
END

PRO h5b_dNames,dnames,name=name,gnames=gnames,SDS=SDS,file=file,debug=debug
; this function get the list of data set found in groups
;  dnames   - returns the list of string names found in hdf5 file
; KEYWORD  
;  file     - specify the hdf5 file name, default to 
;             '/usr/local/rsi/idl_6.0/examples/data/hdf5_test.h5'
;  name     - specify the inpu group name, default to '/' for whole file
;  gnames   - return the list of subgroups found for the specified name 
;  dnames   - return the list of all leaves found for the specified name 
;  SDS      - return the list of SDS datasets found for the specified name 
;
	gnames=''
	dnames=''
	SDS=''
	h5b_gNames,gnames,dnames,file=file,debug=debug,name=name,SDS=SDS
if keyword_set(debug) then begin
	print,'gnames=',gnames
	print,'dnames=',dnames
	print,'SDS datasets=',SDS
end
END

PRO h5b_gNames,gnames,dlist,file=file,debug=debug,name=name,parent=parent,SDS=SDS
; this routine finds the sub-groups for a given group name
; OUTPUT 
; gnames  - returns the sub-group names
; KEYWORDS
; name  - specify the group name, default is the top group '/'
; file  - specify the full path file name
; debug - if specified the subgroup found will be printed
; 
	if keyword_set(file) eq 0 then $
	file=filepath('hdf5_test.h5',subdirectory=['examples','data'])

	fid = h5f_open(file)
	tgn='/'
	gid = h5g_open(fid,tgn)

	if keyword_set(parent) eq 0 then parent=''
	if keyword_set(name) then tgn=name

;	stree = h5_group_parse(fid,tgn,file=file)
;	help,stree,/st
;	nms = tag_names(stree)

catch,error_status
if error_status ne 0 then begin
;print,!err_string
	h5b_checkData,tgn,gid,dlist,parent,SDS=SDS
goto,cont1
end
	n = h5g_get_nmembers(gid,tgn)
	if n gt 0 then begin
	gnames = strarr(n)
	for i=0,n-1 do begin
	nm = h5g_get_member_name(gid,tgn,i)
	gnames(i) = nm
	if keyword_set(debug) then print,'***group',i, '  ',nm
	h5b_gNames,file=file,gnames_1,dlist,/debug,name=nm,parent=parent+tgn,SDS=SDS
	end
	endif else begin
		h5b_checkData,tgn,gid,dlist,parent,SDS=SDS
	end
cont1:
  	h5g_close,gid
  	h5f_close,fid
END

PRO h5b_plot,varname,pn=pn,file=file
; varname - is the varname imported by the h5_browser
; pn      - is the palette varname imported by the h5_browser
; file is a dummy variable not used
;
	sz = size(varname)
	if sz(2) ne 8 then return
	if varname._type eq 'DATASET' then begin
	file = varname._file
	title = varname._file+' ('+varname._name+')'
	data = varname._data
	dim = varname._dimensions
	type = size(data,/type)

	if type lt 7 then begin 
	case varname._ndimensions of
	1:  begin
		plot1d,data,wtitle=title
	    end
	2:  begin
		if keyword_set(pn) then begin
		plot2d_image,data,palette=pn._data,wtitle=title
		endif else begin
		plot2d,reverse(data,2),wtitle=title ;,width=dim[0]*1.4,height=dim[1]*1.25
		end
	    end
	3: begin
		view3d_2d,data,title=title
	    end
	else:
	endcase
	endif else begin
	   xdisplayfile,text=data,title=title
	end 
	end
END

 

PRO h5b_readImage,name,name_p,file=file
; name   - specifies the name of the image data
; name_p - specifies the name of image palette, if no name_p defined, the
;          current color palette is used
; file   - specifies the hdf5 file name
;
; example
;  h5b_readimage,'images/Iceberg','images/iceberg_palette'
;  h5b_readimage,'images/Eskimo','images/Eskimo_palette'
;  h5b_readimage

	if keyword_set(file) eq 0 then $
	file=filepath('hdf5_test.h5',subdirectory=['examples','data'])

	fid = h5f_open(file)
	
	if n_elements(name) eq 0 then begin
		name='/images/Eskimo'
		name_p = name+'_palette'
	end

	catch,error_status
	if error_status ne 0 then begin
 	  h5f_close,fid
	  r = dialog_message(!err_string,/error)
	  return
	end
	d_id1 = h5d_open(fid,name)
	image = h5d_read(d_id1)
	h5d_close,d_id1

	if n_elements(name_p) then begin
	d_id2 = h5d_open(fid,name_p)
	palette = h5d_read(d_id2)
	h5d_close,d_id2
	end

	h5f_close,fid
	
	plot2d_image,image,palette=palette

END

PRO h5b_readArray,name,file=file,data=data,readonly=readonly,xarr=xarr,yarr=yarr
; if readonly is specified no plot will be done
;
	if keyword_set(file) eq 0 then $
	file=filepath('hdf5_test.h5',subdirectory=['examples','data'])

	if n_elements(name) eq 0 then name='/arrays/2D float array'
	fid = h5f_open(file)
	catch,error_status
	if error_status ne 0 then begin
 	    h5b_getStruct,name,file=file,data=data
	    catch,error_close
	    if error_close then return
 	    h5f_close,fid
	    return
	end
	d_id1 = h5d_open(fid,name)
	data = h5d_read(d_id1)
	h5d_close,d_id1
	h5f_close,fid
	
	if keyword_set(readonly) then return
	type = size(data,/type)
	if type eq 8 then return

	sz = size(data)
	case sz(0) of
	1: begin
		if sz(2) eq 7 then begin
		r=dialog_message([name,'-------',data],/info,title=name)
		endif else begin
			if keyword_set(xarr) then $
			plot1d,xarr,data,wtitle=name else $
		 	plot1d,data,wtitle=name
		end
	   end
	2: begin
		plot2d,data,wtitle=name,xarr=xarr,yarr=yarr
	   end
	3: begin
		if keyword_set(xarr) then $
		view3d_2d,data,0,xarr,yarr,title=name else $
		view3d_2d,data,title=name
	   end
	else:
	endcase

END

PRO h5b_pickhelp
   str=["Plot Image with Palette... - this button will accept the 2D 'DATASET'", $
	"                  and the 'PALETTE' selected from the two pick lists",$
	"                  and pass to he", $ "plot2d_image program.", $
	"? Struct Data... - info on selected group/data structure", $
	"               when thers is a problem of reading hdf5 dataset always", $
	"               use this button to query group/data structure", $
	"SDS...   -  dialog to access the SDS datasets found in file", $
	"Color... -  dialog to access the default IDL color tables", $
	"Help...  -  display this help message", $
	"Close    -  close this group dataset selection dialog", $
	"Pick DATASET - a scrollable list of names found in a group", $
	"Pick PALETTE - a sub-list from pick DATASET list which contains the", $
	"               palette sting", $
	"Attributes Info - list of attribute info found for the picked DATASET", $
	'']
	r = dialog_message(str,/info,title='H5B_PICKHELP') 
END

PRO H5B_DIALOGPICK_Event, Event

  WIDGET_CONTROL,Event.top,GET_UVALUE=h5b_statepick
  file = h5b_statepick.file

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'H5B_PLOTARRAy': BEGIN
	h5b_readArray,h5b_statepick.dpick 
      END
  'H5B_PLOTIMAGE': BEGIN
	if h5b_statepick.ppick eq '' then begin
	r = dialog_message('Please pick a palette dataset first!',/error)
	endif else $
	h5b_readImage,h5b_statepick.dpick,h5b_statepick.ppick 
      END
  'H5B_GETSTRUCT': BEGIN
	if h5b_statepick.dpick ne '' then begin
	h5b_getStruct,h5b_statepick.dpick,file=file,data=data
;*h5b_statepick.data = data
;print,ptr_valid()
	help,data,/st,output=out
	xdisplayfile,text=out,title=h5b_statepick.dpick,group=Event.top
	endif else begin
	r = dialog_message('First select an item from the Pick DATASET list!',/info)
	end
      END
  'H5B_SDSDATA': BEGIN
	h5b_dNames,name='/',SDS=SDS,file=file
	pos = strpos(file,!os.file_sep,/reverse_search)
	if pos ge 0 then title= strmid(file,pos+1) else title=file
	title ='SDS: '+title
	h5b_sds,dlist=SDS,file=file,title=title,group=Event.top
      END
  'H5B_TEXTDATA': BEGIN
      Print, 'Event for Text...'
      END
  'H5B_PICKHELP': BEGIN
	h5b_pickhelp
      END
  'H5B_CLOSE': BEGIN
;	if ptr_valid(h5b_statepick.data)  then ptr_free,h5b_statepick.data
	widget_control,Event.top,/destroy,bad=bad
	return
      END
  'H5B_DATALIST': BEGIN
	widget_control,h5b_statepick.attrWID,set_value=''
	r = widget_info(Event.id,/list_select)
	h5b_statepick.dpick = h5b_statepick.dlist(r)
	; check for group 
	catch,error_status
	if error_status ne 0 then begin
	  ; not group may be data
	  h5b_readArray,h5b_statepick.dpick,data=data 
	type = 8
	if n_elements(data) gt 0 then type = size(data,/type)   ; structure
	if n_elements(data) eq 0 or type eq 8 then begin
		h5b_getStruct,h5b_statepick.dpick,file=file,data=data
		help,data,/st,output=out
		xdisplayfile,text=out,title=h5b_statepick.dpick,group=Event.top
	end
	h5b_dattrs,h5b_statepick.dpick,st,file=file
	widget_control,h5b_statepick.attrWID,set_value=st
	  catch,err1
	  if err1 ne 0 then goto,cont1
	  h5g_close,gid
	  h5f_close,fid
	  goto,cont1
	end
	fid = h5f_open(file)
	gid = h5g_open(fid,h5b_statepick.dpick)
	stat = h5g_get_objinfo(gid,h5b_statepick.dpick,/follow_link)
	h5g_close,gid
	h5f_close,fid
	if stat.type eq 'GROUP' then begin
h5b_dattrs,h5b_statepick.dpick,st,file=file
widget_control,h5b_statepick.attrWID,set_value=st
	h5b_dNames,dnames,name=h5b_statepick.dpick,file=file,SDS=SDS
	dlist = dnames
	dlist = SDS
	h5b_findPlist,dnames,pnames,error=error
	if error eq 0 then $
	h5b_dialogPick,file=file,dlist=dlist,plist=pnames,Group=Event.top, $
		title=h5b_statepick.dpick
	end
cont1:
      END
  'H5B_PICKPALETTE': BEGIN
	r = widget_info(Event.id,/list_select)
	h5b_statepick.ppick = h5b_statepick.plist(r)
      END
  'H5B_GROUPATTR': BEGIN
      END
  'H5B_XLOADCT': BEGIN
	xloadct,group=Event.top
      END
  ENDCASE

  WIDGET_CONTROL,Event.top,SET_UVALUE=h5b_statepick

END

PRO h5b_findPlist,dlist,plist,error=error,file=file
; check whether palette name exist in dlist
;
	error=0
	ndata = n_elements(dlist) 
	if ndata eq 1 and dlist(0) eq '' then begin
	r=dialog_message(['No DATASET found in:','',file],/info)
	error=-1
	return
	end
	if ndata gt 1 and dlist(0) eq '' then dlist = dlist(1:ndata-1)

	plist=['']
	if ndata gt 1 then begin
	r = strpos(dlist,'palette')
	plist = where(r ge 0)
	if plist(0) lt 0 then plist=[''] else $
	plist = dlist(plist)
	end
END

PRO h5b_dialogPick,dlist=dlist,plist=plist,file=file,GROUP=Group,title=title,SDS=SDS
if keyword_set(file) eq 0 then $
	file=filepath('hdf5_test.h5',subdirectory=['examples','data'])
if keyword_set(dlist) eq 0 then begin
	h5b_dNames,dnames,file=file,SDS=SDS
	dlist = dnames
	h5b_findPlist,dlist,plist,error=error,file=file
	if error eq -1 then return
end
if keyword_set(SDS) eq 0 then SDS=dlist

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

 if keyword_set(title) eq 0 then title=file
  H5B_DIALOGPICK = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
	title=title, MAP=1, $
      UVALUE='H5B_DIALOGPICK')

  BASE2 = WIDGET_BASE(H5B_DIALOGPICK, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2')

  BUTTON4 = WIDGET_BUTTON( BASE2, $
      UVALUE='H5B_PLOTIMAGE', $
      VALUE='Plot Image with Palette...')

  BUTTON28 = WIDGET_BUTTON( BASE2, $
      UVALUE='H5B_GETSTRUCT', $
      VALUE='? Struct Data...')

  BUTTON29 = WIDGET_BUTTON( BASE2, $
      UVALUE='H5B_SDSDATA', $
      VALUE='  SDS...  ')

;  BUTTON5 = WIDGET_BUTTON( BASE2, $
;      UVALUE='H5B_TEXTDATA', $
;      VALUE='Text...')

  BUTTON27 = WIDGET_BUTTON( BASE2, $
      UVALUE='H5B_XLOADCT', $
      VALUE='Color...')

  BUTTON6 = WIDGET_BUTTON( BASE2, $
      UVALUE='H5B_PICKHELP', $
      VALUE='Help...')

  BUTTON7 = WIDGET_BUTTON( BASE2, $
      UVALUE='H5B_CLOSE', $
      VALUE='Close')


  BASE11 = WIDGET_BASE(H5B_DIALOGPICK, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE11')

  BASE12 = WIDGET_BASE(BASE11, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE12')

  LABEL14 = WIDGET_LABEL( BASE12, $
      UVALUE='H5B_PICKDATA', $
      VALUE='Pick DATASET')

  ListVal1768 = [ '' ]
  if keyword_set(dlist) then ListVal1768=dlist
  LIST32 = WIDGET_LIST( BASE12,VALUE=ListVal1768, $
      UVALUE='H5B_DATALIST', $
      YSIZE=10,xsize=40)


  BASE13 = WIDGET_BASE(BASE11, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE13')

  LABEL30 = WIDGET_LABEL( BASE13, $
      UVALUE='LABEL30', $
      VALUE='Pick PALETTE')

  ListVal1858 = [ '']
  if keyword_set(plist) then ListVal1858=plist
  LIST31 = WIDGET_LIST( BASE13,VALUE=ListVal1858, $
      UVALUE='H5B_PICKPALETTE', $
      YSIZE=4,xsize=40)

  LABEL31 = WIDGET_LABEL( BASE13, $
      UVALUE='LABEL31', $
      VALUE='Attributes Info')

  TEXT41 = WIDGET_TEXT( BASE13,VALUE=[''], $
      UVALUE='H5B_GROUPATTR',/scroll,  $
      YSIZE=4,xsize=40)

  h5b_statepick = { $
	file: file, $
	dpick: '', $
	ppick: '', $
	dlistWID : LIST32, $
	plistWID : LIST31, $
	attrWID : TEXT41, $
;	data: ptr_new(/allocate_heap), $
	SDS : SDS, $
	dlist : dlist, $
	plist : plist $
	}

  WIDGET_CONTROL, H5B_DIALOGPICK, /REALIZE
  widget_control,H5B_DIALOGPICK,set_uvalue=h5b_statepick

  XMANAGER, 'H5B_DIALOGPICK', H5B_DIALOGPICK
END


PRO H5B_Event, Event

  WIDGET_CONTROL, Event.top,get_uvalue=h5b_state
  file = h5b_state.file
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'H5B_RUNSDS': BEGIN
	h5b_dNames,name='/',SDS=SDS,file=file
	h5b_sds,dlist=SDS,file=file,group=Event.top
	END
  'H5B_DATASET': BEGIN
	h5b_dialogPick,file=file,Group=Event.top
	END
  'H5B_LOADCT': BEGIN
	xloadct
	END
  'H5B_HELP': BEGIN
	st=['Open... - run h5_browser with file dialog selection', $
	'Color...    - run xloadct to let user to load desired IDL default color tables', $
	'DATASET...  - use h5b_dialogPick to find groups and DATASETs list', $
	'SDS...      - get SDS dataset dialog for the opened file', $
	'Help...     - pop up this help dialog', $
	'Done        - done with H5B dialog', $
	'HDF5 File   - display/enter a hdf5 filename passing to h5_browser', $
	'              <CR> accept the filename and run the h5_browser', $
	'File_Attr   - button to list file attributes found in file', $
	'Scroll Text   - display valid command syntax or file attributes ', $
	'DATA EXTRACTION COMMANDS - button to list available data extraction commands', $
	'CMD Field   - <CR> accept the CMD text and run the corresponding command', $
	'Run...      - Run CMD field with the opened file, a user has to make sure', $
	'              the dataset name must be existed in the opened file', $

	'','Refering the tree struction display of the h5_browser to find out the ', $
	'correct valid group/data name, and file name', $
	'']
	r = dialog_message(st,/info)
	END
  'H5B_OPEN': BEGIN
	if h5b_state.H5BASE then $
	widget_control, h5b_state.H5BASE,/destroy,bad=bad
	file = dialog_pickfile(file=h5b_state.file,filter=['*.h5*','*.Nx*','*.nx*','*.nexus'], $
		get_path=path,dialog_parent=Event.top, $
		/MUST_EXIST,MULTIPLE_FILES=0, $
		path=h5b_state.path,/READ,TITLE='PICK HDF5...')
;print,'path=',path
;print,'file=',file
	if file ne "" then begin
	h5b_state.path = path
	h5b_state.file = file
	res = H5F_IS_HDF5(file)
	if res then begin
	h5b_configWrite,file
	widget_control,h5b_state.fileWID,set_value=file
	h5b_state.H5BASE = h5_browser(file)
	widget_control,h5b_state.textWID,set_value=''
	endif else begin
		if HDF_ISHDF(file) then HDFB,file,group=Event.top
	end
	end
      END
  'FIELD4': BEGIN
	widget_control,Event.Id,get_value=file
	h5b_state.file = file
	res = H5F_IS_HDF5(file)
	if res then begin
print,file
	h5b_configWrite,file
	if h5b_state.H5BASE then $
	widget_control, h5b_state.H5BASE,/destroy,bad=bad
	widget_control,h5b_state.textWID,set_value=''
	h5b_state.H5BASE = h5_browser(file(0))
	end
      END
  'H5B_LISTVAR': BEGIN
	out=[ $
	"h5b_readArray, 'dataset_name' [,file=file] ", $
	"e.g.   h5b_readArray, '/arrays/2D float array'",'', $
	"h5b_readImage, 'image_name', 'palette_name' [,file=file]", $
	"e.g.   h5b_readImage, '/images/Eskimo', '/images/Eskimo_palette'",'', $
	"where each dataset name must include the root parent group name.",'', $
	"Push the 'Run...' button will automatically append the opened file", $
	"name to CMD text", $
;	"*** Following command only works with IDL developer version ***", $
;	'     IDL> help', $
;	"     IDL> h5b_plot, Eskimo, PN=Eskimo_palette", $
;	"where Eskimo, Eskimo_palette are varibles 'Import to IDL' by h5_brower", $
	'' ]
	widget_control,h5b_state.textWID,set_value=out
      END
  'H5B_LISTATTR': BEGIN
	h5b_attrs,h5b_state.file,st
	widget_control,h5b_state.textWID,set_value=st
      END
  'H5B_TEXT': BEGIN
      Print, 'Event for H5B_TEXT'
      END
  'H5B_RUN': BEGIN
	widget_control,h5b_state.cmdWID,get_value=cmd
	x = cmd(0) +",file='"+h5b_state.file+"'"
print,x
	r = execute(x)
      END
  'H5B_COMMAND': BEGIN
	widget_control,Event.Id,get_value=cmd
	x = cmd(0) ;+",file='"+h5b_state.file+"'"
print,x
	r = execute(x)
      END
  'H5B_DONE': BEGIN
	widget_control,h5b_state.H5BASE,/destroy,bad=bad
	widget_control,Event.top,/destroy
	return
      END
  ENDCASE
  WIDGET_CONTROL, Event.top,set_uvalue=h5b_state,bad=bad
END


PRO H5B , GROUP=Group,file=file

cd,current=path
found = findfile('h5b.config',count=ct)
if ct gt 0 then begin
	h5b_configRead,file
	if h5f_is_hdf5(file) eq 0 then file='' else begin
	pos = strpos(file,!os.file_sep,/reverse_search)
	path = strmid(file,0,pos+1)
	end
end

if keyword_set(file) eq 0 then $
file=filepath('hdf5_test.h5',subdirectory=['examples','data'])
@os.init
loadct,39


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  H5B = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, TITLE='H5B', $
      MAP=1, $
      UVALUE='H5B')

  BASE2 = WIDGET_BASE(H5B, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2')

  BUTTON3 = WIDGET_BUTTON( BASE2, $
      UVALUE='H5B_OPEN', $
      VALUE='Open...')

  BUTTON18 = WIDGET_BUTTON( BASE2, $
      UVALUE='H5B_LOADCT', $
      VALUE='Color...')

  BUTTON38 = WIDGET_BUTTON( BASE2, $
      UVALUE='H5B_DATASET', $
      VALUE='DATASET...')

  BUTTON39 = WIDGET_BUTTON( BASE2, $
      UVALUE='H5B_RUNSDS', $
      VALUE='  SDS...  ')

  BUTTON20 = WIDGET_BUTTON( BASE2, $
      UVALUE='H5B_HELP', $
      VALUE='HELP...')

  BUTTON19 = WIDGET_BUTTON( BASE2, $
      UVALUE='H5B_DONE', $
      VALUE='Done')

  BASE17 = WIDGET_BASE(H5B, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE17')

  FIELD4 = CW_FIELD( BASE17,VALUE=file, $
      ROW=1, title='HDF5 File:', $
      STRING=1, xsize=60, $
      RETURN_EVENTS=1, $
      UVALUE='FIELD4')

  BASE5 = WIDGET_BASE(H5B, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE5')

  BASE6 = WIDGET_BASE(BASE5, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE6')

  BUTTON7 = WIDGET_BUTTON( BASE6, $
      UVALUE='H5B_LISTATTR', $
      VALUE='File_Attr')

  BUTTON6 = WIDGET_BUTTON( BASE6, $
      UVALUE='H5B_LISTVAR', $
      VALUE='DATA EXTRACTION COMMANDS')

  TextVal389 = [ $
    '' ]
  TEXT7 = WIDGET_TEXT( BASE5,VALUE=TextVal389, $
      scroll=1, $
      NO_NEWLINE=1, $
      UVALUE='H5B_TEXT', $
      XSIZE=70, $
      YSIZE=10)

  h5b_attrs,file,st
  widget_control,TEXT7,set_value=st

  BASE12 = WIDGET_BASE(H5B, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE12')

  FieldVal581 = [ $
    "h5b_readImage, '/images/Eskimo','/images/Eskimo_palette'" ]
  FIELD14 = CW_FIELD( BASE12,VALUE=FieldVal581, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='CMD:', $
      UVALUE='H5B_COMMAND', $
      XSIZE=60,ysize=1)

  BUTTON15 = WIDGET_BUTTON( BASE12, $
      UVALUE='H5B_RUN', $
      VALUE='Run...')

  h5b_state = { fileWID: FIELD4, $
	textWID: TEXT7, $
	cmdWID: FIELD14, $
	H5BASE: 0L, $
	base: H5B, $
	file: file, $
	path: path $
	}

  WIDGET_CONTROL, H5B, /REALIZE
  WIDGET_CONTROL, H5B,set_uvalue=h5b_state

  XMANAGER, 'H5B', H5B,no_block=1
END
