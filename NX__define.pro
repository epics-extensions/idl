;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
;
;  NX__define.pro
;

PRO NX::debug,on
; turn debug on/off with on equals 1/0
;+
; NAME:
;	NX::DEBUG
;
; PURPOSE:
;       This method turns on/off the display of the various NX methods. 
;
; CALLING SEQUENCE:
;       Obj->[NX::]Debug, On
;
; INPUT:
;     On:        Specifies display output on or off. On defaults to 1 implies
;                debug is on. If 0 no display of the output will be shown.
;
; KEYWORDS:
;     NOWIN:     If specified, only the ASCII files will be created but the
;                xdisplayfile window will not be shown. 
;
; EXAMPLE:
;     Turn the display of method output on for the NX HDF object v.
;
;         v->Debug,1
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jun 19, 1998.
;	xx-xx-xxxx      comment
;-

self.debug = on
END

PRO NX::User,user_id,user_name,user_email
;+
; NAME:
;	NX::USER 
;
; PURPOSE:
;       This method allows the NX user to get the user_id, user_name,
;       and user_email from the Unix operating system.
;
; CALLING SEQUENCE:
;       Obj->[NX::]User [,User_id, User_name, User_email]
;
; OUTPUT:
;     User_ID:      returns the user_id based on the system mailalias file
;     User_name:    returns the user_name corresponding to user_id
;     User_email:   returns the user_email corresponding to user_id 
;
; KEYWORDS:
;     None.
;
; RESTRICTION:
;     The method uses the Unix command malias and the /home/oxygen/etc/mailalias
;     file automaically figuring out the user_id, user_name, and user_email.
;     For non Unix system it may need modification
;
; EXAMPLE:
;     The NX object v only need to be defined once for a given HDF file in 
;     whole IDL session.
;
;         v = obj_new('NX',file='1.hdf')
;         v->User,user_id,user_name,user_email
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, May 4, 1998.
;	xx-xx-xxxx      comment
;-

;

	user_name = ''
	user_email = ''

	user_id =  getenv('USER') + '@'
	CATCH, error_status
	if error_status lt 0 then return 

	spawn,['malias',user_id],/NOSHELL, listing
	ml = listing(2)
	l2 = strpos(ml,user_id)
	len = strlen(ml)
	user_name = strtrim(strmid(ml,0,29),2)
	user_email = strmid(ml,l2(0),len(0)-l2(0))
	if strlen(user_name) gt 0 then self.user_name = user_name
	if strlen(user_email) gt 0 then self.user_email = user_email

	if self.debug then begin
		print,'user_name: ',user_name
		print,'user_email: ',user_email
	end
END

PRO NX::Putfid,fids,file=file
;+
; NAME:
;	NX::PUTFID
;
; PURPOSE:
;       This method allows the user to add string array of file annotations to a
;       HDF file.
;
; CALLING SEQUENCE:
;       Obj->[NX::]Putfid, Fids [,File=file]
;
; INPUt:
;     Fids:      File annotation string array to be added to an NX HDF file.
;
; KEYWORDS:
;     FILE:      Specifies the destination HDF file instead of the default 
;                NX object.
;
; EXAMPLE:
;     The NX object v needs to be defined for a HDF file if it is not
;     defined yet in the IDL session.
;
;	  fids = ['This is the file annotation array.', $
;                 'More lines of text....']
;         v->Putfid,fids
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jun 19, 1998.
;	xx-xx-xxxx      comment
;-
;
	filename = self.file	
	if keyword_set(file) then filename = file
	if HDF_ISHDF(filename) eq 0 then begin
		str = ['File: '+filename,'','Error: it is not a HDF file!']
		xdisplayfile,text=str,title='NX::Putfid'
		return
	end

	if n_elements(fids) eq 0 then begin
		str = ['Usage: Obj->[NX::]Putfid, Fids [,File=file]', '',$
		'     Add file annotations to an existing HDF file.','', $
		'INPUT:',$
		'   Fids  - string array for file annotation', $
		'KEYWORD:',$
		'   FILE  - overrides the default destination HDF file']
		xdisplayfile,text=str,title='NX::Putfid'
		return
	end
	fid = HDF_OPEN(filename,/RDWR)
	nfids = n_elements(fids)
	for i=0, nfids-1 do begin
	HDF_DFAN_ADDFID, filename, fids(i)
	end

	HDF_CLOSE,fid
END

PRO NX::Getfid,fids,file=file, nowin=nowin
;+
; NAME:
;	NX::GETFID
;
; PURPOSE:
;       This method allows the user to get string array of file annotations from
;       the given NX HDF object or from a specified HDF file.
;
; CALLING SEQUENCE:
;       Obj->[NX::]Getfid, Fids [,File=file]
;
; OUTPUT:
;     Fids:      Variable returns the file annotation string array.
;
; KEYWORDS:
;     File:     If specified, the given HDF file name override the default 
;               NX object file. 
;     Nowin:    If specified, the popup window is suppressed.
;
; EXAMPLE:
;         Obtains the file annotation for the NX object v. 
;
;         v->Getfid,fids
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jun 19, 1998.
;	xx-xx-xxxx      comment
;-

; get all file annotations
;
	filename = self.file	
	if keyword_set(file) then filename=file
	str0 = 'File: '+filename

	fid = HDF_OPEN(filename)
	if fid eq -1 then begin
		xdisplayfile,text=[str0, '', 'Failed to open HDF file.']
		return
	end
	HDF_DFAN_GETFID, filename, ids,/FIRST
	if ids eq '' then begin
		fids = [str0,'','NO File Annotation found']
	endif else begin
	fids=[str0,'','File Annotations:',ids]

	while (ids ne '') do begin
		HDF_DFAN_GETFID, filename, ids
		fids = [fids,ids]
	endwhile
	end
	HDF_CLOSE,fid

	if self.debug then $
	xdisplayfile,text=fids,title='NX::getfid,fids'
END

PRO NX::PutAttr,attr_name,attr_data,sds_name=sds_name,s_id=s_id,output=output,nowin=nowin
;+
; NAME:
;	NX::PUTATTR
;
; PURPOSE:
;       This method writes the global or SDS data attibute.
;
; CALLING SEQUENCE:
;       Obj->[NX::]PutAttr, Attr_name, Attr_data [,Sds_name=sds_name]
;                    [,Output=output]
;
; INPUT:
;     Attr_name:     Specifies the attribute name to be written 
;     Attr_data:     Specifies the attribute data to be written
;
; KEYWORDS:
;     S_ID:          Specifies the SD interface ID as returned by HDF_SD_START
;                    for global attribute, or HDF_SD_SELECT/HDF_SD_CREATE for
;                    SDS attribute. If S_ID is given, the SDS_name is ignored. 
;     SDS_name:      Specifies the SDS set to be tagged, if not specified then
;                    writing the global attribute is assumed 
;     Output:        If specified, it returns the output strings 
;     Nowin:         If specified, no info window pops up 
;
; EXAMPLE:
;     Find the SDS data set name 'temperature', then tag with the attr_name 
;     'primary' and attr_data of 1 for the HDF NX object v.
;     The NX object v needs to be defined for a HDF file if it is not
;     defined yet in the IDL session.
;
;         attr_name = 'primary'
;         attr_data = 1
;         v->PutAttr,attr_name,attr_data,sds_name='temperature'
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jun 19, 1998.
;	xx-xx-xxxx      comment
;-
;
	out1='NX::PutAttr'
if n_params() eq 0 then begin
	str = ['Usage:  Obj->[NX::]PutAttr, Attr_name, Attr_data [,S_id=s_id,sds_name=sds_name, Output=output,/Nowin] ','', $
	'This method writes the global or SDS data attribute','',$
	'INPUT', $
	'   Attr_name   -  attribute name ', $
	'   Attr_data   -  attribute data ', $
	'KEYWORD', $
	'     S_ID      -  Specifies the SD interface ID as returned by HDF_SD_START', $
	'                  for global, or HDF_SD_SELECT/HDF_SD_CREATE for SDS',$

	'   Sds_name    -  if specified, tags the specified SDS with attribute', $
	'   Nowin       -  if specified, no info window pops up', $
	'   Output      -  if specified, returns the info strings']
	xdisplayfile,text=str,title=out1
	return
end

	filename = self.file
	sd_id = hdf_sd_start(filename,/RDWR)

	if keyword_set(s_id) then begin
		; tag if sds_id is given
		sds_id = long(s_id)
		HDF_SD_ATTRSET,sds_id,attr_name,attr_data
		index = HDF_SD_ATTRFIND(sds_id,attr_name)
		if index ne -1 then begin
		HDF_SD_ATTRINFO,sds_id,index,name=name,count=len,type=ty,data=data
		end
		output='Add attribute with Name : '+attr_name
		goto, final_step
	end

	if keyword_set(sds_name) then begin
		self->FindSDS,sds_name,seq,ref
		sds_id = HDF_SD_SELECT(sd_id,seq)
		HDF_SD_ATTRSET,sds_id,attr_name,attr_data
		index = HDF_SD_ATTRFIND(sds_id,attr_name)
		if index ne -1 then begin
		HDF_SD_ATTRINFO,sds_id,index,name=name,count=len,type=ty,data=data
		end
		HDF_SD_ENDACCESS,sds_id
		output='Add SDS attribute with Name : '+attr_name
	endif else begin
		HDF_SD_ATTRSET,sd_id,attr_name,attr_data
		index = HDF_SD_ATTRFIND(sd_id,attr_name)
		if index ne -1 then begin
		HDF_SD_ATTRINFO,sd_id,index,name=name,count=len,type=ty,data=data
		end
		output='Add global attribute with Name : '+attr_name
	end

final_step:
	HDF_SD_END, sd_id

	help,name,ty,len,data,output=out
	output = [output,'' , out]
	out = [string(string(data),/print)]
	output = [output,'' , out]
	if self.debug eq 0 or keyword_set(nowin) then return
	xdisplayfile,text=output,title=out1
END


PRO NX::Getattr,name,data,S_id=s_id,output=output,nowin=nowin
;+
; NAME:
;	NX::GETATTR
;
; PURPOSE:
;       This method allows the user to get global or SDS attribute data for 
;       a specified attribute name from the NX HDF object. 
;
; CALLING SEQUENCE:
;       Obj->[NX::]Getattr, Name, Data [,S_id=s_id, Output=output]
;
; INPUT:
;     Name:      Attribute name to search for
;
; OUTPUT:
;     Data:      Returns the corresponding global attribute data 
;
; KEYWORDS:
;     S_ID:      Specifies the SD interface ID as returned by HDF_SD_START
;                for global, or HDF_SD_SELECT/HDF_SD_CREATE for SDS
;     Output:    Detailed info about the found attribute.
;     Nowin:     If specified, no info window pops up
;
; EXAMPLE:
;     Example 1 finds the global attribute 'owner_name' for the NX object v.
;     The NX object v needs to be defined for a HDF file if it is not
;     defined yet in the IDL session.
;
;         v->Getattr,'owner_name',data
;
;     Example 2 finds the global attribute 'owner_name' using the S_id keyword:
;
;	  sd_id = hdf_sd_start(filename)
;         v->Getattr,'owner_name',data,S_id=sd_id
;
;     Example 3 finds the attribute data of the third set of SDS with 
;     attribute name 'primary' using the S_id keyword:
;
;	  sd_id = hdf_sd_start(filename)
;         sds_id = hdf_sd_select(sd_id,2)
;         v->Getattr,'primary',data, S_id=sds_id
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jun 19, 1998.
;	xx-xx-xxxx      comment
;-
;
	out1='NX::getattr,name,data'
if n_params() eq 0 then begin
	str = ['Usage:  Obj->[NX::]GetAttr, Name, Data [,S_id=s_id, Output=output]','', $
	'This method gets the global attribute for a given attribute name','',$
	'INPUT', $
	'   Name    -  attribute name to search for', $
	'OUTPUT', $
	'   Attdata -  attribute data found for the given name', $
	'KEYWORD', $
	'   S_ID    -  Specifies the SD interface ID as returned by HDF_SD_START', $
	'              for global, or HDF_SD_SELECT/HDF_SD_CREATE for SDS',$
	'   Nowin   -  no info window pops up if specified', $
	'   Output  -  detailed info about the found attibute data']
	xdisplayfile,text=str,title=out1
	return
end
	data = ''
	filename = self.file

	if keyword_set(s_id) then begin
		sd_id = long(s_id)
		index = hdf_sd_attrfind(sd_id,name)
		if index ne -1 then begin
		hdf_sd_attrinfo,sd_id,index,name=nm,count=len,type=ty,data=data
		end
		output='Find attribute with Name : '+name
		goto,final_step
	end

	sd_id = hdf_sd_start(filename,/rdwr)
	index = hdf_sd_attrfind(sd_id,name)
	if index ne -1 then begin
	hdf_sd_attrinfo,sd_id,index,name=nm,count=len,type=ty,data=data
	end
	HDF_SD_END, sd_id
	output='Find global attribute with Name : '+name

final_step:
	help,nm,ty,len,data,output=out
	output = [output,'' , out]
	if self.debug eq 0 or keyword_set(nowin) then return
	xdisplayfile,text=output,title=out1
END

PRO NX::Open,filename, gname=gname, gclass=gclass, $
	create=create, append=append, $
	location = location, $
	user_name = user_name, $
	user_email = user_email, $
	user_phone = user_phone, $
	user_fax= user_fax, $
	program_name = program_name
;+
; NAME:
;	NX::OPEN
;
; PURPOSE:
;       This method opens any existing HDF or creates a new NX HDF file. 
;
; CALLING SEQUENCE:
;       Obj->[NX::]Open, File [, Gname=gname] [,Gclass=gclass] 
;            [,Program_name=program_name] [,Location=location] 
;            [,User_name=user_name] [,User_email=user_email]
;            [,User_fax=user_fax] [,User_phone=user_phone ]
;
; INPUT:
;     File:      Specifies the HDF file name to be opened.
;
; KEYWORDS:
;     Following keywords are used in NX HDF file creation.
;
;     Create:     It is required if the new HDF file to be created
;     Gname:      Specifies entry group name, default to 'entry_default'
;     Gclass:     Specifies entry group class name, default to 'NXdefault'
;     Program_name: HDF file associated with program name
;     Location:   Experimental location, default to 'APS'
;     User_name:  Owner name, from the unix system malias 
;     User_email: Owner email address, from the unix system malias
;     User_fax:   Owner fax number
;     User_phone: Owner phone number
;
; EXAMPLE:
;     Example 1 creates the HDF file '1.hdf' with the default settings if 
;     file does not exist yet.  
;
;         v->Open,'1.hdf'
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jun 19, 1998.
;	xx-xx-xxxx      comment
;-

if n_elements(filename) eq 0 then begin
	str = ['Usage:  Obj->[NX::]Open, File, Gname=gname, Gclass=gclass,...', $
	'','This method opens the HDF file specified by the filename. ',$
	'If the entered file does not exist yet it will be created, the global ', $
	'attribute and fileid description will be added based on the keyword', $
	'specification. On the Unix system, if the keywords are not specified, ' ,$
	'the appropriate default will be automatically figured out.', $
	'','INPUT', $
	'  File          - HDF file name', $
	'KEYWORD', $
	'  Append        - add new SDS or Group to the file', $
	'  Create        - required, if a new hdf file to be created', $
	'  Gname         - Default entry group name, e.g. entry_default', $
	'  Gclass        - Default entry group class name, e.g. NXdefault', $
	'  Program_name  - Associated program name', $
	'  location      - Experimental location, default to APS', $
	'  User_name     - Owner name, from the unix system malias ', $
	'  User_email    - Owner email address, from the unix system malias', $
	'  User_fax      - Owner fax number', $
	'  User_phone    - Owner phone number']
	xdisplayfile,text=str,title='NX::Open,file'
	return
end

time0 = systime(1)

found = findfile(filename)

if found(0) eq '' then begin
; if gname gclass not given the APS default is assumed
; this section only hold true for aps
;

	if keyword_set(gname) then default_entry = gname else $
		default_entry='entry_default'
	if keyword_set(gclass) then default_class = gclass else $
		default_class='NXdefault'

	if keyword_set(location) eq 0 then location = 'APS'

	if keyword_set(program_name) eq 0 then $
		program_name = self.version

	if keyword_set(user_phone) then self.user_phone = user_phone else $
		self.user_phone = ''
	if keyword_set(user_name) then self.user_name = user_name else $
		self.user_name = ''
	if keyword_set(user_email) then self.user_email = user_email else $
		self.user_email = ''
	if keyword_set(user_fax) then self.user_fax = user_fax else $
		self.user_fax = ''


	self.file = filename

	fid = HDF_OPEN(filename,/ALL)
	HDF_CLOSE,fid

	fid = HDF_OPEN(filename,/RDWR)
        self.fid = fid

; Write global attributes only once

	if self.user_name eq '' then begin
	self->user,u_id,u_nm,u_em
	self.user_name = u_nm
	self.user_email = u_em
	end

	self->global_attribute


; Write the default entry & class

	sd_id = HDF_SD_START(filename,/RDWR)
	vg_id = HDF_VG_ATTACH(fid,-1,/WRITE)

	HDF_VG_SETINFO,vg_id,name=default_entry,class=default_class

; write  program_name attribute to the top group 
;	vg_tag = 1965
	vg_tag = self.vg_tag
	if strlen(location) gt 0 then begin
	self->PutSDS,sid=sd_id,name='location',byte(location),index,ref
	HDF_VG_ADDTR,vg_id,vg_tag,ref
	end
	if strlen(program_name) gt 0 then begin
	self->PutSDS,sid=sd_id,name='program_name',byte(program_name),index,ref
	HDF_VG_ADDTR,vg_id,vg_tag,ref
	end
	HDF_VG_DETACH,vg_id
	HDF_SD_END,sd_id

print,'filename=',filename, ' created'

endif else begin

; print,'filename=',filename ,' already exists!' 

	if HDF_ISHDF(filename) eq 0 then begin
		str = ['File: '+filename,'','Error: it is not a HDF file !']
		xdisplayfile,text=str,title='NX::Open,filename'
		return
	end

	if self.fid gt 0 then self->close

	if keyword_set(append) then fid  = HDF_OPEN(filename,/RDWR) else $
	fid  = HDF_OPEN(filename)

	self.fid = fid

	if self.debug eq 0 then return 
	time1 = systime(1)
	print,'HDF open used time = ',time1-time0, ' seconds.'
end

END

PRO NX::global_attribute,user_name=user_name,user_mail=user_mail, $
 user_email=user_email,user_phone=user_phone,user_fax=user_fax,$
 user_history=user_history,user_desc=user_desc
; global attribute created by NX class including the following : 
;           owner_name, owner_mail,owner_email, owner_phone, owner_fax
;           file_name, version, file_history
;+
; NAME:
;	NX::GLOBAL_ATTRIBUTE
;
; PURPOSE:
;       This method writes a new set of global attributes: file_name,
;       version, owner_name, owner_mail, owner_email,owner_phone,
;       owner_fax, file_history, experiment_description for the HDF 
;       file. If the HDF file is created by the NX object initialization 
;       which automatically calls this method.
;
; CALLING SEQUENCE:
;       Obj->[NX::]Global_attribute
;
; ARGUMENTS:
;       None.
;
; KEYWORDS:
;       User_name        Updates the user name if specified.
;       User_mail        Updates the user mail address if specified.
;       User_email       Updates the user email address if specified.
;       User_phone       Updates the user phone number if specified.
;       User_fax         Updates the user fax number if specified.
;       User_history     Updates the file history if specified.
;       User_desc        Updates file description if specified.
;
; RESTRICTION
;     The total number of lines of description can not exceeds 100.
;     Every set of global attribute includes owner's name, mail,email,
;     phone, fax, history, and description.
;
; EXAMPLE:
;     The NX object v needs to be defined for a HDF file if it is not
;     defined yet in the IDL session.
;
;         descs = ['line1 ...','line2...', ...]
;         v->[NX::]Global_attribute,user_desc=descs,user_name='...' 
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jun 19, 1998.
;	xx-xx-xxxx      comment
;-

filename = self.file

if  HDF_ISHDF(filename) eq 0 then return

	if keyword_set(user_name) then self.user_name = user_name 
	if keyword_set(user_mail) then self.user_mail = user_mail 
	if keyword_set(user_email) then self.user_email = user_email 
	if keyword_set(user_phone) then self.user_phone = user_phone 
	if keyword_set(user_fax) then self.user_fax = user_fax 
	if keyword_set(user_history) then $
		 self.user_history = self.user_history +string(10b)+ user_history 


default:
sd_id = HDF_SD_START(filename,/RDWR)

	HDF_SD_ATTRSET,sd_id,'file_name',filename
	if self.version ne '' then $
	HDF_SD_ATTRSET,sd_id,'version',self.version

	if self.user_name ne '' then $
	HDF_SD_ATTRSET,sd_id,'owner_name',self.user_name
	if self.user_mail ne '' then $
	HDF_SD_ATTRSET,sd_id,'owner_mail',self.user_mail
	if self.user_email ne '' then $
	HDF_SD_ATTRSET,sd_id,'owner_email',self.user_email
	if self.user_phone ne '' then $
	HDF_SD_ATTRSET,sd_id,'owner_phone',self.user_phone
	if self.user_fax ne '' then $
	HDF_SD_ATTRSET,sd_id,'owner_fax',self.user_fax
	if self.user_history ne '' then $
	HDF_SD_ATTRSET,sd_id,'file_history',self.user_history

	HDF_SD_FILEINFO,sd_id,NumSDS,NumGAttr
	HDF_SD_END,sd_id

; write file desc

	if keyword_set(user_desc) then begin

	crt = 10b  ; linefeed
	str = 'Owner Name: '+ self.user_name
	descs = str
	str = 'Owner Mail: '+ self.user_mail
	descs = [descs,str]
	str = 'Owner_email: ' + self.user_email
	descs = [descs,str]
	str = 'Owner_phone: ' + self.user_phone
	descs = [descs,str]
	str = 'Owner_fax:   ' + self.user_fax
	descs = [descs,str]
	str = 'File_history :' + self.user_history
	descs = [descs,str]

	str = 'Experiment_description: ' 
	descs = [descs,str]
	descs = [descs , user_desc] 

	descs = descs + string(crt)

	self.desc_nline = n_elements(descs)
	self.user_desc = descs
	self->putfid,self.user_desc(0:self.desc_nline - 1)
	end
END


PRO NX::Close
;+
; NAME:
;	NX::CLOSE
;
; PURPOSE:
;       This method closes the currently opened HDF file by the NX object.
;
; CALLING SEQUENCE:
;       Obj->[NX::]Close
;
; ARGUMENTS:
;       None.
;
; KEYWORDS:
;       None.
;
; EXAMPLE:
;     Close the HDF file opened by the NX object v. 
;
;         v->Close
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jun 19, 1998.
;	xx-xx-xxxx      comment
;-


	time2 = systime(1)

	;HDF_SD_END,self.sd_id
	if self.fid gt 0 then hdf_close,self.fid
	self.fid = -1 

	if self.debug eq 0 then return 
	time3 = systime(1)
	print,'HDF close used time = ',time3-time2, ' seconds.'
END

PRO NX::PutSDS, file=file, data, name=name, index, ref, $
    sid=sid, range=range, unit=unit, format=format, $
    coordsys=coordsys, label=label, caldata=caldata, fill=fill
;+
; NAME:
;	NX::PUTSDS
;
; PURPOSE:
;       This method allows the user to add a SDS data with few possible
;       predefined data attributes. The destination file can be default NX 
;       object file or any valid HDF file specified by the keyword File.
;
; CALLING SEQUENCE:
;       Obj->[NX::]PutSDS, Data,index,ref, Name=name, File=file, Range=range,
;                   Unit=unit, Format=format, Coordsys=coordsys, Label=label,
;                   Caldata=caldata, Fill=fill
;
; INPUT:
;     Data:      Scientific data set can be any of byte, short, long, float, 
;                double array.
;
; OUTPUT:
;     Index:     Created SD dataset index number
;     Ref:       SD reference number
;
; KEYWORDS:
;     Sid:       Specifies the opened SD interface ID for writing 
;     Name:      Specifies the name attribute for SD data
;     File:      Specifies destination file, defaults to NX object file
;     Range:     Scientific data range [min,max]
;     Unit:      String
;     Format:    String
;     Coordsys:  String
;     Label:     String
;     Caldata:   Calibration array
;     Fill:      Fill data
;
; RESTRICTION:
;     If no sid interface is used, the proper SD interface startup and closing
;     is automatically taking cared by putSDS. 
;
;     If sid interface is used a user has to open the file with HDF_SD_START
;     with /RDWR option before calling putSDS. At the end of all putSDS, a 
;     user has to explicitly close the SD interface by calling the HDF_SD_END.
;
; EXAMPLE:
;     Example 1 write the image array to the HDF file opened by the NX object v.
;
;	  image=....
;         v->PutSDS,image,name='Image_1'
;
;     Example 2 write few image arrays to the HDF file opened by the NX object v
;     through using the SD interface ID.
;
;         sd_id = HDF_SD_START(self.file,/RDWR)
;         v->PutSDS,Sid=sd_id,image1,name='Image_1'
;         v->PutSDS,Sid=sd_id,image2,name='Image_2'
;         v->PutSDS,Sid=sd_id,image3,name='Image_3'
;         HDF_SD_END,sd_id
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jun 19, 1998.
;	xx-xx-xxxx      comment
;-

; write SD to any HDF file
;
	str=['Usage:  Obj->[NX::]PutSDS, Data, Name=name, File=file, ...','', $
	'This method write SD data with given name to a destination HDF file', $
	'Data can be byte, short, long, float, double array.' ,'' $
	]
	str=[str,' INPUT:']
	str=[str,'   Data          - SD data to be written']
	str=[str,' OUTPUT:']
	str=[str,'   Index         - created SD dataset index number ']
	str=[str,'   Ref           - SD reference number']
	str=[str,' KEYWORD:']
	str=[str,'   Sid           - Uses currently opened SD interface ID for writing']
	str=[str,'   Name          - specifies the name attribute for SD data']
	str=[str,'   File          - specifies destination file, defaults to NX object file']
	str=[str,'   Range         - [min,max]']
	str=[str,'   Unit          - string']
	str=[str,'   Format        - string']
	str=[str,'   Coordsys      - string']
	str=[str,'   Label         - string']
	str=[str,'   Caldata       - calibration array']
	str=[str,'   Fill          - fill data']

if n_params() lt 1 then begin
	xdisplayfile,text=str,title='NX::PutSDS,data,file=file,...'
	return
	end

if keyword_set(sid) eq 0 then begin

	filename = self.file
	if keyword_set(file) then filename = file

	if HDF_ISHDF(filename) eq 0 then begin
		str = ['File :'+filename, '', 'Error: it is not a HDF file!']
		xdisplayfile,text=str,title='NX::PutSDS,data,file=file,...'
		return
	end

	if n_elements(name) eq 0 then begin
		str = ['Error:', 'Name attribute is required for this routine.','',$
			str]
		xdisplayfile,text=str,title="NX::PutSDS,data,Name=name,..."
		return
	end

	sd_id = HDF_SD_START(filename,/RDWR)

endif else sd_id = sid

;  write data

	varName=string(name)

	s = size(data)
	num = n_elements(s)
	type = s(num-2)

	no = s(0) 
	if no gt 0 then dim = s(1:no) else dim = [s(num-1)]

CASE type OF
	0: begin
	   print,'Undefined should never happened'
		return
	   end
	1: begin
	   sds_id = HDF_SD_CREATE(sd_id,varName,dim,/BYTE)
	   end
	2: begin
	   sds_id = HDF_SD_CREATE(sd_id,varName,dim,/SHORT)
	   end
	3: begin
	   sds_id = HDF_SD_CREATE(sd_id,varName,dim,/LONG)
	   end
	4: begin
	   sds_id = HDF_SD_CREATE(sd_id,varName,dim,/FLOAT)
	   end
	5: begin
	   sds_id = HDF_SD_CREATE(sd_id,varName,dim,/DOUBLE)
	   end
else: begin
	print,'HDF_SD_WRITE Error: type not supported ',type
	return
	end
ENDCASE

	if keyword_set(caldata) ne 0 then $
		HDF_SD_SETINFO,sds_id,caldata=caldata

	if keyword_set(range) ne 0 then $
		HDF_SD_SETINFO,sds_id,range=range 
	
	if keyword_set(label) ne 0 then begin
		if strlen(label) gt 0 then $
		HDF_SD_SETINFO,sds_id,label=label
		end

	if keyword_set(format) ne 0 then begin
		if strlen(format) gt 0 then $
		HDF_SD_SETINFO,sds_id,format=format
		end

	if keyword_set(unit) ne 0 then begin
		if strlen(unit) gt 0 then $
		HDF_SD_SETINFO,sds_id,unit=unit
		end

	if keyword_set(coordsys) ne 0 then begin
		if strlen(coordsys) gt 0 then $
		HDF_SD_SETINFO,sds_id,coordsys=coordsys
		end

	if keyword_set(fill) ne 0 then begin
		if strlen(fill) gt 0 then $
		HDF_SD_SETINFO,sds_id,fill=fill
		end


	HDF_SD_ADDDATA,sds_id,data

	ref = HDF_SD_IDTOREF(sds_id)

	index = HDF_SD_REFTOINDEX(sd_id,ref)

	HDF_SD_ENDACCESS,sds_id

	if keyword_set(sid) then return
	HDF_SD_END,sd_id

END



PRO NX::FindVg,seq,gid,output=str,name=name,class=class,nowin=nowin,seq_start=seq_start,seq_end=seq_end
;+
; NAME:
;	NX::FINDVG
;
; PURPOSE:
;       This method searches and prints out all the VGroups found which 
;       satisfy the search condition. The search condition can either
;       by group name / group classname / group name and classname. 
;       This method can return the group sequence and groupid reference 
;       arrays. 
;
; CALLING SEQUENCE:
;       Obj->[NX::]FindVg, Seq, Gid, Name=name, Class=class, Output=output
;
; OUTPUT:
;     Seq:       Returns the array of matched group sequence id 
;     Gid:       Returns the array of matched group reference id 
;
; KEYWORDS:
;     Seq_start: Specifies the start search  group sequence # 
;     Seq_end:   Specifies the end search  group sequence # 
;     Name:      Specifies the group name to search for
;     Class:     Specifies the class name to search for
;     Output:    If specified, it returns the output strings. If debug option
;                is set the output display will be automatically popped up.
;     Nowin:     Suppresse the output window 
;                
; RESTRICTIOM
;     The NX HDF file will be closed and re-opened by this method. This will
;     gaurantee that the newly added groups will be found too.
;
; EXAMPLE:
;     Example 1 finds the group seq numbers where group name contains the search 
;     string 'reactor'. Variable V is a previously defined NX HDF object.
;
;         v->FindVg, name='reactor', seq, ref
;
;     Example 2 finds the NX group seq numbers where group name contains the search 
;     string 'reactor' and class name contains the search string 'NXreactor'
;
;         v->FindVg, name='reactor', class='NXreactor', seq, ref
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jun 19, 1998.
;	xx-xx-xxxx      comment
;-

; seq - sequence array for matched found
; gid - group ref array for the file 
; given name and/or class string find the matched groups
; return found group ids

search = 0

if keyword_set(name) and keyword_set(class) then search = 3
if keyword_set(name) then search = 1 
if keyword_set(class) then search = 2

if search eq 0 then begin
	str = ['Usage: Obj->[NX::]FindVg,Name=name,Class=class,seq,gid','',$
		'This method returns all the Vgroups which satisfy the search',$
		'conditions. The search can be done on name only, class only, or',$
		'both name and class.','',$
		'OUTPUT',$
		'   Seq        -  returns the zero based Vgroup sequence #',$
		'   Gid        -  returns the group reference number',$
		'KEYWORD',$
		'   Name       -  specifies the group name to search for',$ 
		'   Class      -  specifies the group classname to search for',$ 
		'   Output     -  returns output strings if specified'$ 
		]
	xdisplayfile,text=str,title='NX::FindVg'
	return
	end

fid  = HDF_OPEN(self.file)

id=-1
tmp=(num=0)
while tmp ne -1 do begin
tmp=HDF_VG_GETID(fid,id)
if tmp ne -1 then begin
	num=num+1
	if id eq -1 then vgroup_ids = tmp else vgroup_ids = [vgroup_ids,tmp]
	end
id=tmp
endwhile

self.numVG = num
self.vgids = vgroup_ids(0:num-1)

ng = n_elements(vgroup_ids)

str = 'Search for: '
if search eq 0 then str = 'List for: '
if keyword_set(name) then str = str + " name='"+name+"'"
if keyword_set(class) then str = str + ", class='"+class+"'"

seq1 = 0
seq2 = ng - 1
if keyword_set(seq_start) then seq1 = seq_start
if keyword_set(seq_end) then seq2 = seq_end
seq = -1
gid = -1
str = [str,'','VGroups found:'] 
for i= seq1,seq2 do begin
	vg_hdl = HDF_VG_ATTACH(fid,vgroup_ids(i))   ;gid(i)
	if vg_hdl gt 0 then begin
	HDF_VG_GETINFO,vg_hdl,class=cl,name=na,NENTRIES=nume
	found = 0
	CASE search OF 
	0: begin
		self->dumpVG,/data
		return
	   end
	1: if strpos(na,name) ge 0 then found=1
	2: if strpos(cl,class) ge 0 then found=1
	3: if strpos(cl,class) ge 0 and strpos(na,name) ge 0 then found=1
	ENDCASE
	if found then begin
		if seq(0) eq -1 then seq = i else seq = [seq, i]
		if gid(0) eq -1 then gid = vgroup_ids(i) else gid=[gid,vgroup_ids(i)]
		str = [str, 'VGroup #'+string(i)]
		help,na,cl,nume,output=out1
		str = [str,out1,'']	
	end 
	HDF_VG_DETACH,vg_hdl
	end
end

HDF_CLOSE,fid

	if self.debug eq 0 or keyword_set(nowin) then return
	xdisplayfile,text=str,title='NX::findVg,name=name,class=class'
END

PRO NX::PutVg,name=name,class=class,parent=parent,file=file
;+
; NAME:
;	NX::PUTVG
;
; PURPOSE:
;       This method allows the user to create a new Vgroup with user specified 
;       group name and group classname.
;
;       If the parent name is specified the newly created group will be added
;       to the parent group.
;
; CALLING SEQUENCE:
;       Obj->[NX::]PutVg, Name=name, Class=class, File=file
;
; ARGUMENTS:
;     None.
;
; KEYWORDS:
;     Name:      Specifies the Vgroup name.
;     Class:     Specifies the Vgroup classname.
;     Parent:    Specifies the parent Vgroup name.
;     File:      Optional, specifies the destination HDF file.
;
; RESTRICTION
;     The user is responsible for the HDF integrity for the group added.
;     PutVg method does not check for the uniqeness of group name and
;     group classname, the user is reqposible for it.
;
; EXAMPLE:
;     Example 1 creates a group with group name='reactor', group classname
;     'NXreactor NIST'. Variable v is an opened NX HDF object.
;
;         v->PutVg, Name='reactor', Class='NXreactor'
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jun 19, 1998.
;	xx-xx-xxxx      comment
;-

	if (keyword_set(name) or keyword_set(class)) eq 0 then begin
		str = ["Usage: Obj->[NX::]PutVg, Name='...', Class='...'", '', $
		'KEYWORD:',$
		'    Name     - required, specifies the group name',$
		'    Class    - required, specifies the group classname',$
		'    Parent    - optional, specifies the parent group name',$
		'    File     - optional, specifies HDF file name overrides the ',$
		'               object default']
		xdisplayfile,text=str,title='NX::PutVg'
		return
	end
	filename = self.file
	if keyword_set(file) then filename = file

	fid = HDF_OPEN(filename,/RDWR)
	vg_id = HDF_VG_ATTACH(fid,-1,/WRITE)
	if vg_id gt 0 then begin
	HDF_VG_SETINFO, vg_id, name=name, class=class

	; add to parent here

	seq1 = -1 & gid1 = -1
	if keyword_set(parent) then $
	self->FindVg,name=parent,seq1,gid1,/NOWIN
	if seq1(0) ge 0 and gid1(0) ge 0 then begin
	p_vg_id = HDF_VG_ATTACH(fid,gid1(0),/write)
	HDF_VG_INSERT,p_vg_id,vg_id
	HDF_VG_DETACH,p_vg_id
	end

	HDF_VG_DETACH,vg_id
	end
	HDF_close,fid
END


PRO NX::LinkVG,parent_name,child_name,gid1,gid2,p_index=p_index,c_index=c_index,nowin=nowin
;+
; NAME:
;	NX::LINKVG
;
; PURPOSE:
;       This method links a child group to a parent group for a NX HDF file.
;
; CALLING SEQUENCE:
;       Obj->[NX::]LinkVG, Parent_name, Child_name [,C_index=c_index]
;                  [,P_index=p_index] [,gid1 [,gid2]] [,/NOWIN]
;
; INPUT:
;     Parent_name: Parent group name 
;     Child_name:  Child group name to be linked with the parent group 
;
; OUTPUT:
;     gid1:        Returns parent group reference id
;     gid2:        Returns child group reference id
; KEYWORDS:
;     P_INDEX:     Specifies the desired parent index of matched parent groups 
;                  If not specified, it defaults to zero. 
;     C_INDEX:     Specifies the desired child index of matched sub-groups 
;                  If not specified, it defaults to zero. 
;     NOWIN:       If specified, no info window pops up 
;
; EXAMPLE:
;     Example 1 links the first matched 'sub-group1' to first 'default' 
;     parent group.
;
;         v->LinkVG,'default','sub-group1'
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jun 19, 1998.
;	xx-xx-xxxx      comment
;-

if n_params() eq 0 then begin
        str = ['Usage: Obj->[NX::LinkVG],Parent_name,Child_name,gid1,gid2','',$
        'This method links a child VGoup to the parent VGroup', '', $
        'INPUT', $
        '   Parent_name   - Parent VGroup name', $
        '   Child_name    - Child VGroup name ', $
        'OUTPUT', $
        '    gid1         - returns the HDF ref number of the parent VGroup',$
        '    gid2         - returns the HDF ref number of the child VGroup',$
	'KEYWORD',$
	'   Nowin         - if specified, no info window pops up', $
	'   P_index       - index of parent group from the list of matched groups', $
	'   C_index       - index of child group from the list of matched groups' $
        ]
        xdisplayfile,text=str,title='NX::LinkVG'
        return
end
	self->FindVg,name=parent_name,seq1,gid1,/NOWIN
	self->FindVg,name=child_name,seq2,gid2,/NOWIN

	i2 = 0
	i1 = 0
	if keyword_set(p_index) then begin
		if p_index lt n_elements(gid1) then i1 = p_index
	end
	if keyword_set(c_index) then begin
		if c_index lt n_elements(gid2) then i2 = c_index
	end
	if gid1(i1) gt 0  and gid2(i2) gt 0 then begin

	vg_tag = self.vg_tag   ;  1965     
	ref1 = gid1(i1)
	ref2 = gid2(i2)

	fid = self.fid
	vg_id = HDF_VG_ATTACH(fid,ref1,/WRITE)
	HDF_VG_GETINFO,vg_id,name=nm,class=cl

	vdata = HDF_VG_ATTACH(fid,ref2)
	HDF_VG_GETINFO,vdata,name=nm2,class=cl2
	HDF_VG_ADDTR,vg_id,vg_tag,ref2

	HDF_VG_DETACH,vdata

	HDF_VG_DETACH,vg_id

	help,nm,cl,nm2,cl2,output=out1
	out1 = [out1,'','PARENT VG Seq:'+ string(seq1(i1)) + ',    Ref:'+string(ref1) +',     Name: "'+nm+'"']
	out1 = [out1,'CHILD  VG Seq:'+ string(seq2(i2)) + ',    Ref:'+string(ref2) +',     Name: "'+nm2+'"']

	if self.debug eq 0 or keyword_set(nowin) then return
	xdisplayfile,text=out1,title='NX::LinkVG,parent_name,child_name'
	end

END



PRO NX::VG,seq,tags,refs,output=output,nowin=nowin,nument=nument
;+
; NAME:
;	NX::VG
;
; PURPOSE:
;       This method gives the VGroup info for a user specified zero based 
;       group seq number in the NX HDF object.
;
; CALLING SEQUENCE:
;       Obj->[NX::]VG, Seq [,Tags [,Refs]] [,Output=output]
;
; INPUT:
;     Seq:       Specifies the VGroup zero based seq number
;
; OUTPUT:
;     Tags:      Returns the HDF tags of the entries in the VGroup
;     Refs:      Returns the HDF refs of the entries in the VGroup
;
; KEYWORDS:
;     Nument:  If specified, it returns the number of entires found for the
;                VG
;     Output:    If specified, output info strings are returned through this
;                keyword.
;     Nowin:     If specified, the popup window is suppressed.
;
; EXAMPLE:
;     Example 1 read the first VGroup from the NX HDF object v. 
;
;         v->VG,0
;
;     Example 2 read the 7'th VGroup from the NX HDF object v and return the
;     output info as str. 
;
;         v->VG,0,output=str
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jun 19, 1998.
;	xx-xx-xxxx      comment
;-

; HDF file already open and vgids,vdids already populated
; seq - zero based group sequence

if n_params() eq 0 then begin
	str=['Usage:  Obj->[NX::]VG, Seq','', $
	'This method reads all the subgroup entries data for a specified', $
	'Vgroup sequence number.','',$
	'INPUT:', '  Seq  -  zero based seq number']
	xdisplayfile,text=str,title='NX::VG,seq'
	return
end

if seq ge self.numVG then begin
	str = 'Error: max seq can be entered is '+string(self.numVG-1)
	xdisplayfile,text=str,title='NX::VG,seq'
	return
end

fid = HDF_OPEN(self.file)
self->VgVd,gid,did 

str = 'VGroup # '+string(seq)
	vg_hdl = HDF_VG_ATTACH(fid,gid(seq))
	if vg_hdl gt 0 then begin
	HDF_VG_GETINFO,vg_hdl,class=class,name=name,NENTRIES=NENTRIES
nument=nentries
	str = [str,  'Group Name:           '+name, $
		'Group Class Name:     '+class, $
		'Number of Entries: '+ string(nentries)]
	self->subVg,vg_hdl,NENTRIES,tags,refs,ent_name,ent_type,subgroups
	str = [str,subgroups]
	end
	HDF_VG_DETACH,vg_hdl
HDF_CLOSE,fid
	if self.debug eq 0 or keyword_set(nowin) then return
	xdisplayfile,text=str,title='NX::VG,seq'
END

PRO NX::GetSDS,sds_name,data,type=ty,ndims=nd,dims=di,nowin=nowin
;+
; NAME:
;	NX::GETSDS
;
; PURPOSE:
;       This method returns SDS data, type, ndims, dims with known sds_name.   
;
; CALLING SEQUENCE:
;       Obj->[NX::]GetSDS, Sds_name, Data [,Type=type] [,Ndims=ndims]
;                            [,Dims=dims] [,/NOWIN]
;
; INPUT:
;     Sds_name:   Specifies the SDS data set name to search for 
;
; OUTPUT:
;     Data:       Returns the SDS data array
;
; KEYWORDS:
;     Type:       Returns the type of the SDS data 
;     Ndims:      Returns the ndims of the SDS data 
;     Dims:       Returns the dims of the SDS data 
;     Nowin:      If specified, no info window pops up. 
;
; EXAMPLE:
;     Example 1 finds the SDS data set with name as 'location' where v is the
;     opened NX HDF object.
;
;         v->GetSDS,'location',data,type=ty
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jun 19, 1998.
;	xx-xx-xxxx      comment
;-
if n_params() eq 0 then begin
	str = ['Usage: Obj->[NX::]GetSDS,Sds_name,Data [,TYPE=ty,NDIMS=nd,DIMS=di,OUTPUT=str,/NOWIN]','',$
	'This method seaches for the SDS data set with known sds_name.','',$
	'INPUT',$
	'   Sds_name   - SDS name to search for', $
	'OUTPUT', $
	'   Data       - returns the SDS data array ',$
	'KEYWORD', $
	'   TYPE       - if specified, returns the SDS data type',$
	'   NDIMS      - if specified, returns the SDS data NDIMS',$
	'   DIMS       - if specified, returns the SDS data DIMS',$
	'   OUTPUT     - if specified, returns the info strings',$
	'   NOWIN      - if specified, no info window pops up ']
	xdisplayfile,text=str,title='NX::GetSDS'
	return
end

	self->FindSDS,sds_name,seq,ref,data,type=ty,ndims=nd,dims=di,output=str,/nowin
	if self.debug eq 0 or keyword_set(nowin) then return
	xdisplayfile,text=str,title='NX::GetSDS'
END

PRO NX::FindAll,sds_name,seqs,nowin=nowin,seq_start=seq_start,seq_end=seq_end
;+
; NAME:
;	NX::FINDALL
;
; PURPOSE:
;       This method returns all seq number of the SDS which match the search
;       sds_name.   
;
; CALLING SEQUENCE:
;       Obj->[NX::]FindAll, Sds_name, Seqs, [,/NOWIN]
;
; INPUT:
;     Sds_name:   Specifies the SDS data set name to search for 
;
; OUTPUT:
;     Seqs:       Returns the matched SDS index array 
;
; KEYWORDS:
;     Nowin:      If specified, no info window pops up. 
;     Seq_start:  Specifies the start search SDS seq #, default 0
;     Seq_end:    Specifies the end search SDS seq #, default numSDS
;
; EXAMPLE:
;     Example 1 finds the SDS data set with name as 'scan1' where v is the
;     opened NX HDF object.
;
;         v->FindALL,'scan1',seqs
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jan 25, 2002.
;	xx-xx-xxxx      comment
;-

	sd_id = HDF_SD_START(self.file)
	numSDS = self.numSDS

	output = 'search for name = '+string(sds_name)

	seq1 = 0
	seq2 = numSDS-1
	if keyword_set(seq_start) then seq1 = seq_start
	if keyword_set(seq_end) then seq2 = seq_end
	for seq = seq1,seq2 do begin
	sd_ids = HDF_SD_SELECT(sd_id,seq)
        HDF_SD_GETINFO,sd_ids,dims=dims,format=format,label=label, $
		name=name,caldata=caldata,coordsys=coordsys,range=range, $
                natts=natts,ndims=ndims,type=type,unit=unit

	if strpos(name,sds_name) ge 0 then begin
		output = [ output,'SDS # '+string(seq)]
		if n_elements(ids) eq 0 then ids = seq else $
		ids = [ids,seq]
	end
	HDF_SD_ENDACCESS,sd_ids
	end
	HDF_SD_END,sd_id

	seqs = -1
	if n_elements(ids) gt 0 then seqs = ids else output=[output,'','None found!!!']

	if keyword_set(nowin) then return
	if self.debug then $
	xdisplayfile,text=output,title='NX::FindALL,sds_name'
print,'FindAll: seqs= ',seqs
END


PRO NX::FindSDS,sds_name,seq,ref,data,type=type,ndims=ndims,dims=dims,output=output,nowin=nowin,start=start
;+
; NAME:
;	NX::FINDSDS
;
; PURPOSE:
;       This method searches for the SDS data set with  the user specified
;       SDS name. It returns the corresponding seq and ref number found.
;
; CALLING SEQUENCE:
;       Obj->[NX::]FindSDS,sds_name [,Seq, Ref, Data] [,Type=type] 
;                   [,Ndims=ndims] [,Dims=dims] [,Output=output] 
;                   [,Nowin=nowin]
;
; INPUT:
;     sds_name:  specifies the SDS data name to be searched for
;
; OUTPUT:
;     Seq:       returns the zero based sequence index number of the SDS
;     Ref:       returns the corresponding HDF reference number of the SDS 
;     Data:      returns the SDS data array
; 
; KEYWORDS:
;     Type:      If specified, it returns the SDS data type
;     Ndims:     If specified, it returns the SDS data ndims
;     Dims:      If specified, it returns the SDS data dims
;     Output:    If specified, it returns the output string 
;     Nowin:     If specified, no output window pops up
;     START:     Specify the begin search SDS sequence number
;                default 0
;
; EXAMPLE:
;     Example 1 find the info about the SDS data set with 'location' as name
;     from the HDF file '1.hdf'
;
;         v = obj_new('NX',file='1.hdf')
;         v->FindSDS,'location'
;
;     Example 2 returns the found SDS seq and ref number without popping up
;     the info window
;
;         v->FindSDS,'location',seq,ref,/nowin
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jun 19, 1998.
;	xx-xx-xxxx      comment
;-

if n_params() eq 0 then begin
	str = ['Usage: Obj->[NX::]FindSDS,Sds_name [,Seq,Ref,Data,Type=type,NDIMS=ndims,DIMS=dims,/NOWIN]','',$
	'This method searches for the SDS name from the HDF file of NX object and', $
	'returns the corresponding matched SDS Seq and Ref.',$ 
	'','INPUT', $
	'   Sds_name    -  SDS name to search for', $
	'OUTPUT', $
	'   Seq         -  returns the seq number of matched SDS name', $
	'   Ref         -  returns the ref number of matched SDS name', $
	'   Data        -  returns the SDS data array ', $
	'KEYWORD', $
	'   TYPE        -  if specified, returns the SDS data type', $
	'   NDIMS       -  if specified, returns the SDS data NDIMS', $
	'   DIMS        -  if specified, returns the SDS data DIMS', $
	'   OUTPUT      -  if specified, output string returned', $
	'   NOWIN       -  if specified, no output window pops up' $
	]
	xdisplayfile,text=str,title='NX::FindSDS'
	return
end
	sd_id = HDF_SD_START(self.file)
	numSDS = self.numSDS

	output='search for name = '+ string(sds_name)
	begin_seq = 0
	if keyword_set(start) then begin_seq = start

	for seq=begin_seq,numSDS-1 do begin
        sd_ids = HDF_SD_SELECT(sd_id,seq)
        HDF_SD_GETINFO,sd_ids,dims=dims,format=format,label=label, $
		name=name,caldata=caldata,coordsys=coordsys,range=range, $
                natts=natts,ndims=ndims,type=type,unit=unit

	if strpos(name,sds_name) ge 0 then begin

		ref = HDF_SD_IDTOREF(sd_ids)
        	HDF_SD_GETDATA,sd_ids,data
		help,ref,natts,caldata,coordsys,format,label,unit,name,type,range,ndims,dims,data,output=out1
		output = [ output,'SDS # '+string(seq), out1,'data = ']
		if type eq 'BYTE' and ndims eq 1 then $
		output = [output, string(string(data),/print)] else $
		output = [output, string(data,/print)] 
		HDF_SD_ENDACCESS,sd_ids
		HDF_SD_END,sd_id
		if keyword_set(nowin) then return
		if self.debug then $
		xdisplayfile,text=output,title='NX::FindSDS,sds_name'
		return
	end
	HDF_SD_ENDACCESS,sd_ids
	end
	HDF_SD_END,sd_id

END

PRO NX::SDS,seq,data,name=name,dims=dims,format=format,label=label,natts=natts,attrnms=attrnms,attrdas=attrdas,ndims=ndims,type=type,unit=unit,range=range,caldata=caldata,coordsys=coordsys,output=output,nowin=nowin
;+
; NAME:
;	NX::SDS
;
; PURPOSE:
;       This method reads the SDS data for a specified SD sequence number for a 
;       HDF file.
;
; CALLING SEQUENCE:
;       Obj->[NX::]SDS, Seq, Data [,Name=name] [,Dims=dims] [,Format=format]
;                   [,Attrnms=attrnms] [,Attrdas=attrdas] 
;                   [,Label=label] [,Natts=natts] [,Ndims=ndims] [,Type=type]
;                   [,Range=range] [,Caldata=caldata] [,Coordsys=coordsys] 
;                   [,Unit=unit] [,Output=output] [,/NOWIN]
;
; INPUT:
;     Seq:      zero based seq number for SDS data set
;
; OUTPUT:
;     Data:     returns the SDS data obtained
;
; KEYWORDS:
;     NAME:     returns the name attribute
;     DIMS:     returns dims array
;     FORMAT:   returns format attribute
;     LABEL:    returns lagel attribute 
;     NATTS:    returns number of attributes
;     ATTRNMS:    returns name array  of attributes
;     ATTRDAS:    returns value array of attributes
;     NDIMS:    returns number of dimensions
;     TYPE:     returns data type
;     RANGE:    returns range attribute
;     CALDATA:  returns caldata attribute
;     COORDSYS: returns coordsysattribute
;     UNIT:     returns unit attribute
;     OUTPUT:   returns output string
;     NOWIN:    If specified, no info window pops up. 
;
; EXAMPLE:
;     Example 1 get the first set of SDS data from the opened NX HDF object v
;     without popping up info window.
;
;         v->SDS,0,data,/nowin
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jun 19, 1998.
;	xx-xx-xxxx      comment
;-

if n_params() eq 0 then begin
	str=['Usage:  Obj->[NX::]SDS, seq, data, Name=name, Dims=dims,...','', $
	'This method reads the SD data for a known SDS sequence number.','',$
	'INPUT:', '     seq -  zero based seq number', $
	'OUTPUT:','    data -  return the SDS data variable', $
	'KEYWORDS:', $
	'  NAME   -  returns name attribute', $
	'  DIMS   -  returns dims array', $
	'  UNIT   -  returns unit attribute', $
	'  FORMAT -  returns format attribute', $
	'  RANGE  -  returns range attribute', $
	'  CALDATA -  returns caldata attribute', $
	'  COORDSYS - returns coordsysattribute', $
	'  LABEL  -  returns lagel attribute ', $
	'  NATTS  -  returns number of attributes', $
	'  ATTRNMS  -  returns attribute names', $
	'  ATTRDAS  -  returns attribute values', $
	'  NDIMS  -  returns number of dimensions', $
	'  TYPE   -  returns data type', $
	'  OUTPUT -  returns output string', $
	'  NOWIN  -  if specifed, no info window pops up' $
	]
	xdisplayfile,text=str,title='NX::SDS,seq'
	return
end

        output=['FILE: '+self.file, '']
	if seq lt 0 or seq ge self.numSDS then begin
		str = [output,'Error: there are only '+strtrim(self.numSDS,2)+' sets of SD data.','       0 based seq should be entered.']
		xdisplayfile,text=str,title='NX::SDS,seq'
		return
	end
	sd_id = HDF_SD_START(self.file)
        sd_ids = HDF_SD_SELECT(sd_id,seq)
        HDF_SD_GETINFO,sd_ids,dims=dims,format=format,label=label, $
		name=name,caldata=caldata,coordsys=coordsys,range=range, $
                natts=natts,ndims=ndims,type=type,unit=unit
        HDF_SD_GETDATA,sd_ids,data
help,natts,caldata,coordsys,format,label,unit,name,type,range,ndims,dims,data,output=out1
	; get attributes

	attrnms = make_array(natts,/string)
	attrdas = make_array(natts,/string)

		out2=''
		if natts gt 0 then begin
		for j=0,natts-1 do begin
		out2=[out2,'     Attribute # '+string(j)]
		HDF_SD_ATTRINFO,sd_ids,j,count=act,data=adata,name=anm,type=aty
attrnms(j) = anm
attrdas(j) = string(adata)
		help,act,aty,output=out3
		out2 = [out2,$
			'     Attribute Type : '+ string(aty), $
			'     Attribute Count: '+ string(act), $
			'     Attribute Name : '+ anm, $
			'     Attribute Data : ',string(adata)	]
		end
		end

	output = [output,out2]
	HDF_SD_ENDACCESS,sd_ids
	HDF_SD_END,sd_id

	if keyword_set(nowin) then return
	output = [ output,'SDS # '+string(seq), out1,'data = ']
	if type eq 'BYTE' and ndims eq 1 then $
	output = [output, string(string(data),/print)] else $
	output = [output, string(data,/print)] 

	if self.debug then $
	xdisplayfile,text=output,title='NX::SDS,seq'
END

PRO NX::DumpSDS,SDS_ref,output,file=file,help=help
;+
; NAME:
;	NX::DUMPSDS
;
; PURPOSE:
;       This method dump all the SDS data sets found in a NX HDF file. 
;
; CALLING SEQUENCE:
;       Obj->[NX::]DumpSDS [,Sds_ref] [,Output] [,File=file] [,Help=help]
;
; INPUT:
;     None.
;
; OUTPUT:
;     Sds_ref    Returns the SDS reference ID array 
;     Output     Returns the output info strings
;
; KEYWORDS:
;     FILE:      Specifies another HDF file instead of the default file sepecified 
;                by the NX object 
;     HELP:      If specified, gives this on-line help
;     NOWIN:     If specified, no info window pops up 
;
; EXAMPLE:
;     Example 1 lists all the SDS data sets found in the opened NX HDF object v
;
;         v->dumpSDS
;
;     Example 2 lists all the SDS data sets in the opened NX HDF object v 
;
;         v->dumpSDS,file='old.hdf'
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jun 19, 1998.
;	xx-xx-xxxx      comment
;-

if keyword_set(help) then begin
	str = ['Usage: Obj->[NX::]DumpSDS [,Sds_ref,Output,FILE=file,/NOWIN,/HELP]','',$
	'This method lists all the SDS data sets defined in a HDF file.','',$
	'INPUT',$
	'   None.',$
	'OUTPUT',$
	'   Sds_ref    - returns the SDS reference array',$
	'   Output     - returns the output info strings',$
	'KEYWORD',$
	'   FILE       - overrides the filename of the NX object',$
	'   HELP       - provides this on line help',$
	'   NOWIN      - if specified, no info window pops up']
	xdisplayfile,text=str,title='NX::DumpSDS'
	return
end

; dump all SDS data as output

	filename = self.file
	if keyword_set(file) then filename = file
	output=['File: '+filename, '']

	if HDF_ISHDF(filename) eq 0 then begin
		output = [output, 'Error: is not a HDF file!']
		xdisplayfile,text=output,title='NX::dumpSDS,File=file'
		return
	end	

	sd_id = HDF_SD_START(self.file)
	HDF_SD_FILEINFO, sd_id, no_datasets, no_gattributes
	self.numSDS = no_datasets

	sds_ref =  make_array(no_datasets,/long)
	for i=0,no_datasets-1 do begin
	sd_ids = HDF_SD_SELECT(sd_id,i)
	sds_ref[i] = HDF_SD_IDTOREF(sd_ids)
        HDF_SD_GETINFO,sd_ids,dims=dims,format=format,label=label, $
		name=name,caldata=caldata,coordsys=coordsys,range=range, $
                natts=natts,ndims=ndims,type=type,unit=unit
        HDF_SD_GETDATA,sd_ids,data
help,natts,caldata,coordsys,format,label,unit,name,type,range,ndims,dims,data,output=out1

	out1 = [string(replicate(45b,85)), '','SDS # '+string(i), $
		'REF # '+string(sds_ref(i)), out1,'data = ']
	if type eq 'BYTE' and ndims eq 1 then $
		out1 = [out1,string(data)] else $
		out1 = [out1,string(string(data),/print)]

		out2=''
		if natts gt 0 then begin
		for j=0,natts-1 do begin
		out2=[out2,'     Attribute # '+string(j)]
		HDF_SD_ATTRINFO,sd_ids,j,count=act,data=adata,name=anm,type=aty
		help,act,aty,output=out3
		out2 = [out2,$
			'     Attribute Type : '+ string(aty), $
			'     Attribute Count: '+ string(act), $
			'     Attribute Name : '+ anm, $
			'     Attribute Data : ',string(adata)	]
		end
		end
	HDF_SD_ENDACCESS,sd_ids
	output=[output,out1,out2]
	end

	HDF_SD_END,sd_id

	if keyword_set(nowin) then return
	xdisplayfile,text=output,title='NX::dumpSDS'
END


PRO NX::SDSData,ref,name,data,type,ndims,dims,seq=seq,nowin=nowin
;+
; NAME:
;	NX::SDSDATA
;
; PURPOSE:
;	This method returns SD data set for a known SDS reference number.
;
; CALLING SEQUENCE:
;	Obj->[NX::]SDSData,ref,name,data,type,ndims,dims,seq=seq,/nowin
;
; INPUT:
;	Ref:	Specify the reference number of the SDS data set
;
; OUTPUT:
; 	Name:   Return the name attribute of the SDS
;	Data:   Return the data array of the SDS
;	Type:   Return the data type
;	Ndims:  Return the dimension of the SDS
;	Dims:   Return the dimensions of the SDS
;
; KEYWORD:
;	Seq:    Return the corresponding SDS sequence number 
;	Nowin:  Suppress the terminal window information
; EXAMPLE:
;
;	V->findVG,name='scan1_axis',g_seq,gid
;	v->Vg,g_seq,tags,refs
;	v->SDSData,refs,name,data,type,ndims,dims,seq=seq
;-
	if n_params() eq 0 then begin
	str = ['Usage:  Obj->[NX::]SDSData, ref [,Name,Data,Type,Ndims,Dims]','', $
	'This method returns SD data set for a known SDS reference number.','',$
	'INPUT:','    ref  - SDS reference number', $,
	'OUTPUT:', $
	'    Name  - variable returns the name of the SDS data', $
	'    Data  - variable returns the SDS data', $
	'    Type  - variable returns the type of SDS data', $
	'    Ndims - variable returns the ndims of SDS data', $
	'    Dims  - variable returns the dims of SDS data', $
	'KEYWORD',$
	'    Seq   - if specified, it returns the corresponding SDS seq #', $
	'    Nowin - if specified, no info window pops up' $
	]
	xdisplayfile,text=str,title='NX::SDSData'
	return
	end

	sd_id = HDF_SD_START(self.file)
	seq = HDF_SD_REFTOINDEX(sd_id,ref)
	if seq ge 0 then begin
		sds_id = HDF_SD_SELECT(sd_id,seq)
		HDF_SD_GETINFO,sds_id,name=name,type=type,Ndims=ndims,dims=dims
		HDF_SD_GETDATA,sds_id,data
		HDF_SD_ENDACCESS,sds_id
	end
	HDF_SD_END,sd_id

	if self.debug eq 0 or keyword_set(nowin) then return 
	help,ref,seq,name,type,ndims,dims,output=str
	if type eq 'BYTE' and ndims eq 1 then $
		str = [str,'','DATA=',string(data)] else $
		str = [str,'','DATA=',string(string(data),/print)]

	xdisplayfile,text=str,title='NX::SDSData'
END

PRO NX::DumpVD,vdata_ids,file=file,output=str,nowin=nowin,help=help
;+
; NAME:
;	NX::DumpVD
;
; PURPOSE:
;       This method dumps all the Vdata found in the HDF file. 
;
; CALLING SEQUENCE:
;       Obj->[NX::]DumpVD [,Vdata_ids, File=file, Output=str, Help=help]
;
; OUTPUT:
;     Vdata_ids:     Returns the Vdata reference array found for the HDF file
;
; KEYWORDS:
;     File:      If specified, a new HDF file will be used.
;     Output:    If specified, the output strings are returned.
;     Nowin:     If specified, the xdisplayfile window will not be shown. 
;     Help:      If specified, it provides this on-line help message.
;
; EXAMPLE:
;     Example 1 dumps all the vdata found in the opened NX HDF object v and 
;     returns the corresponding Vdata reference id array.
;
;         v->DumpVd,vdata_ids
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jun 19, 1998.
;	xx-xx-xxxx      comment
;-

; dump VD for any HDF file

if keyword_set(help) then begin
	str= ['Usage:  Obj->[NX::]DumpVD [,Vdata_ids] [,Output=output] [,Nowin=nowin]',$
	'                [,File=file] [,Help=help]', $
	'','INPUT:','    None.',$
	'OUTPUT:',$
	'  Vdata_ids  -  variable returns the Vdata reference array found for the file', $
	'KEYWORD:',$
	'    File      -  specifies the input HDF file name', $
        '                 if not given the current opened NX file is assumed', $
	'    Output    -  returns the info output strings', $
	'    Nowin     -  if specified, no DumpVD info window pops up', $
	'    Help      -  pops up this command syntax window' $
	]
	xdisplayfile,text=str,title='NX::DumpVD'
	return
end

filename = self.file 
if keyword_set(file) then filename = file

fid = HDF_OPEN(filename,/READ)
if fid ne -1 then begin
	id = -1
	tmp=(num=0)
	while tmp ne -1 do begin
		tmp = HDF_VD_GETID(fid,id)
		if tmp ne -1 then begin
			num=num+1
			if id eq -1 then vdata_ids = tmp else $
				vdata_ids = [vdata_ids,tmp]
		end
	id = tmp
	endwhile
        print,'numVD=',num

str=['FILE: ' + filename, '']
if num gt 0 then begin
	for i=0,num-1 do begin
	st1 = [string(replicate(45b,85)),'','VDATA # '+ strtrim(i,2)]
		vd = HDF_VD_ATTACH(fid,vdata_ids(i))
		HDF_VD_GET,vd, class=class, count=count, fields=fd, interlace=il,$
                        name=name, nfields=nf, ref=ref, size=sz, tag=tag
                nread = HDF_VD_READ(vd,data,fields=fd)
	help,name,class,count,fd,il,nf,ref,tag,sz,data,output=st2
	str = [str,st1,st2,string(string(data),/print),'']
		HDF_VD_DETACH,vd
	end
endif else begin
	str = [str, 'Error: No VDATA found in this file !!']
end
HDF_CLOSE,fid	
end
xdisplayfile,text=str,title='NX::DumpVD'
END

;
; dumpVG [,file=filename]          - dump whole VG
; dumpVG [,file=filename] , n1, n2  
;			    - dump groups n1 to n2
;
;   /ENTRY                  - dump entry names if specified
;   /DATA		    - both entry and vdata are returned if specified
;
PRO NX::DumpVG,file=file,n1,n2,vgroup_ids,data=data,entry=entry,help=help
;+
; NAME:
;	NX::DumpVG
;
; PURPOSE:
;       This method dumps all the VG found in a HDF file.
;
; CALLI:  SEQUENCE:
;      : bj->[NX::]DumpVG [,N1,N2] [,Vgroup_ids] [,File=file] [,/DATA]
;                    [,/ENTRY] [,/HELP]
;
; INPUT:
;  Vgroup_ids: Variable returns the group reference array for the HDF file.
;
; KEYWORDS:
;   FILE:    Specifies a new HDF file instead of using the current opened NX file
;  /ENTRY:   Dumps the subgroup entries in the Vgroup
;  /DATA:    Dumps the subgroup entry and data as well
;  /HELP:    Echo this on-line command syntax
;
; 
; EXAMPLE:
;     Example 1 dump the VGgoup name and class defined in the NX HDF object v.
;     The object variable v only need to be defined once in whole IDL session.
;
;         v->DumpVG
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jun 19, 1998.
;	xx-xx-xxxx      comment
;-

if keyword_set(help) then begin
	str = ['Usage:  Obj->[NX::]DumpVG, N1, N2, Vgroup_ids, File=file, /Data, /Entry, /Help', $
'','This method dumps various level of VGROUP info for a given HDF file.', $
	'','INPUT:','    N1 -  specifies the starting group seq number, defaults to 0', $
	'    N2 -  specifies the ending group seq number, defaults to the maxno found in file', $
	'OUTPUT:',' Vgroup_ids  -  variable returns the group reference array', $
	'KEYWORDS:', $
	'   FILE  -  specifies a new HDF file instead of using the current opened NX file', $
	'  /ENTRY -  dumps the subgroup entries in the Vgroup', $
	'  /DATA  -  dumps the subgroup entry and data as well', $
	'  /HELP  -  echo this on-line command syntax' $ 
	]
	xdisplayfile,text=str,title='NX::DumpVG,...'
	return
end

filename = self.file

if keyword_set(file) then filename = file 
fid = HDF_OPEN(filename, /READ)

if fid lt 0  then begin
	xdisplayfile,text=['File: '+filename,'', $
		 'Failed to open the HDF file' ]
	return
	end

	id = -1
	tmp=(num=0)
	while tmp ne -1 do begin
		tmp = HDF_VG_GETID(fid,id)
		if tmp ne -1 then begin
			num=num+1
			if id eq -1 then vgroup_ids = tmp else $
				vgroup_ids = [vgroup_ids,tmp]
		end
	id = tmp
	endwhile
	numVG = num
        print,'numVG=',num

if numVG lt 1  then begin
	xdisplayfile,text=['File: '+filename, 'Error: no VGroup available !']
	HDF_CLOSE,fid
	return
	end
	
start = 0
VGROUP=-1

if n_elements(n2) gt 0 and n_elements(n1) gt 0 then begin
	if n2 lt n1 then begin
	xdisplayfile,text='Error: n2 less tnan n1 '
	return
	end
	if n1 ge 1 then begin
	for i=0,n1-1 do begin
		VGROUP = HDF_VG_GETID(fid,VGROUP)
		if VGROUP eq -1 then begin
			HDF_scrolltext,'Error: only '+ string(i) +' groups found! ',60,3
			return
			end
		end
	end
	start= n1
	num = n2+1
	if num gt numVG then num= numVG
	end


fw = 'File: '+filename
fw = [fw,'','numVG = '+string(numVG)]

str = string(replicate(32b,85))
st = str
strput,st,'VGROUP #',0
strput,st,'NAME',15
strput,st,'CLASS',45
strput,st,'NUM_ENTRIES',55
fw = [fw,st]

for vg=start,num-1  do begin
 VGROUP=HDF_VG_GETID(fid,VGROUP)
 VGROUP_ID=HDF_VG_ATTACH(fid,VGROUP)
 HDF_VG_GETINFO,VGROUP_ID,CLASS=CLASS,NAME=NAME,NENTRIES=NUM_ENTRIES

fw = [fw,'',string(replicate(45b,85))]
st = str
strput,st,'VGROUP # = ',0
strput,st,strtrim(vg,2),11
strput,st,NAME,15
strput,st,CLASS,45
strput,st,string(NUM_ENTRIES,format='(i5)'),55
fw = [fw,st]

; if /ENTRY set then get and print the Vgroup entries

 if NUM_ENTRIES ge 1  and (keyword_set(data) or keyword_set(entry)) then begin

HDF_VG_GETTRS,VGROUP_ID,tags,refs
        id=-1
        for i=0,NUM_ENTRIES-1 do begin
                id=HDF_VG_GETNEXT(VGROUP_ID,id)
                v_string=STRING(i,FORMAT='(40x,"ENTRY # ",i3," is ")')
	if tags(i) ge 700 and tags(i) lt 800 then begin
		CASE tags(i) OF
                	700 : fw = [fw,v_string+'SD Group']
                	701 : fw = [fw,v_string+'SD DimRec']
                	702 : fw = [fw,v_string+'SD ']
                	703 : fw = [fw,v_string+'SD Scales']
                	704 : fw = [fw,v_string+'SD Labels']
                	705 : fw = [fw,v_string+'SD Units']
                	706 : fw = [fw,v_string+'SD Formats']
                	707 : fw = [fw,v_string+'SD Max/Min']
                	708 : fw = [fw,v_string+'SD Coord sys']
                	709 : fw = [fw,v_string+'SD Transpose']
                	720 : fw = [fw,v_string+'SD Numeric Data Group']
                	731 : fw = [fw,v_string+'SD Calibration info']
                	732 : fw = [fw,v_string+'SD Fill Value info']
		ENDCASE
	endif else begin
                if HDF_VG_ISVD(VGROUP_ID,id) then begin
                  vd_id=HDF_VD_ATTACH(fid,id)
                  HDF_VD_GET,vd_id,NAME=NAME
                  fw = [fw,v_string+' VDATA -> ' +NAME]

; if /DATA then get and print the Vdata

if keyword_set(data) then begin
HDF_VD_GET,vd_id,NAME=NAME,fields=fields,nfields=nfields,ref=ref,tag=tag,size=sz
ct= HDF_VD_READ(vd_id,vdata)
help,name,fields,nfields,ref,tag,sz,ct,vdata,output=st2
fw = [fw,st2,string(string(vdata),/print)]
end

                  HDF_VD_DETACH,vd_id
                endif else begin

                  if HDF_VG_ISVG(VGROUP_ID,id) then begin
                        vg_id=HDF_VG_ATTACH(fid,id)
                        HDF_VG_GETINFO,vg_id,NAME=NAME
                        fw = [fw,v_string+'VGROUP -> '+NAME]
                        HDF_VG_DETACH,vg_id
                   endif else $ 
                        fw = [fw,v_string+'NOT A VDATA or VGROUP ']
                endelse
	end   ; not SD
        endfor
 endif
 HDF_VG_DETACH,VGROUP_ID
endfor

	xdisplayfile,text=fw,title='NX::DumpVG'

NO_VGROUPS:

 HDF_CLOSE,fid


END


PRO NX::subVg,vgroup_id,num_en,tags,refs,att_name,att_type,str
; att_type = 1 VGroup subgroup
;            2 VData 
;            3 SDS data
	str=''
	if num_en lt 1 then return
	att_name = strarr(num_en)
	att_type = intarr(num_en)
	HDF_VG_GETTRS,vgroup_id,tags,refs

str = [str,string('tags:',tags,/print),string('refs:',refs,/print)]
	id = -1
	nm=''
	for i=0,num_en - 1 do begin
	id = HDF_VG_GETNEXT(vgroup_id,id)
	vd = HDF_VG_ISVD(vgroup_id,id)
	if vd then begin
		st = '     VDATA Attribute Entry # '+strtrim(i,2)
		att_type(i) = 2
		vd_id = HDF_VD_ATTACH(self.fid,id)
		if vd_id gt 0 then begin

		HDF_VD_GET,vd_id,NAME=NAME,fields=fields,nfields=nfields,ref=ref,tag=tag,size=sz
		count= HDF_VD_READ(vd_id,vdata)
		help,name,fields,nfields,ref,tag,sz,count,vdata,output=st0
		st=[st,st0, string(string(vdata),/print)]
 
;                types=make_array(nfields,/string)
;                for index=0,nf-1 do begin
;                  HDF_VD_GETINFO, vd_id, index, NAME=n, TYPE=ty
;                  types(index) = ty
;                end

		end 
		str = [str,st]
		HDF_VD_DETACH,vd_id
	end
	vg = HDF_VG_ISVG(vgroup_id,id)
	if vg then begin
		vg_id = HDF_VG_ATTACH(self.fid,id)
		; subgroup entry
		if vg_id ne -1 then begin
			att_type(i) = 1
			HDF_VG_GETINFO,vg_id,NAME=nm
			if n_elements(str) eq 0 then $
			str = '     Attribute Entry # '+strtrim(i,2) + '     SUBGROUP name:'+ nm  else $
			str = [str,'     Attribute Entry # '+strtrim(i,2) + '     SUBGROUP name:'+ nm ]
		; group attribute entry
		endif else begin ; vg_id = -1
			att_type(i) = 3
			; check for group attribute
			self->SDSData,refs(i),nm,data,type,/nowin
			st = '     SUBGROUP Attribute Entry # '+strtrim(i,2)
			if n_elements(nm) then st = st + '     name='+nm
			if n_elements(data) then st = st + '     data='+string(data)
			str = [str,st]
		end
		HDF_VG_DETACH,vg_id
	end
	att_name(i) = nm
	end
	str = [str,'']
END

PRO NX::VD,i,nm,data,Ref=Ref,Output=output,Nowin=nowin,Title=title
;+
; NAME:
;	NX::VD
;
; PURPOSE:
;       For a given Vdata seqence number this method gives the corresponding
;       VData information.
;
; CALLING SEQUENCE:
;       Obj->[NX::]VD, Seqno [,Nm] [,Data] [,Ref=ref] [,Title=title]
;                 [,Nowin=nowin] [,Output=output]
;
; INPUT:
;     Seqno:      Vdata sequence number in a HDF file.
;
; OUTPUT:
;     Nm:        If specified, it returns the Vdata name 
;     Data:      If specified, it returns the Vdata data array
;
; KEYWORDS:
;     Ref:        If specified, it returns the Vdata reference id 
;     Output:     If specified, returns the info output strings. 
;     Title:      If specified, resets the info window title. 
;     Nowin:      If specified, no VD info window pops up. 
;
; EXAMPLE:
;     Example 1 finds the info of the first VData of the NX HDF object v 
;
;         v->VD,0
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jun 19, 1998.
;	xx-xx-xxxx      comment
;-

wintitle = 'NX::VD'
if n_params() eq 0 then begin
 	output = ['Usage: Obj->[::NX]VD, Seqno [,Nm[,Data]] [,Ref=ref] ', $
		'               [,Output=output [,Title=title] [,Nowin=nowin]', $
	'', 'This method gives the Vdata info for a given vdata seqno.', $
	'', 'INPUT:', $
	'    Seqno    - specifies the vdata seqno in an HDF file', $
	'OUTPUT:', $
	'    Nm       - returns the string name of the Vdata', $
	'    Data     - returns the data array of the Vdata', $
	'KEYWORD:',$
	'    Ref      - returns the corresponding reference number of the Vdata', $
	'    Output   - returns the output info string of the Vdata', $
	'    Title    - specifies the output info window title ', $ 
	'    /NOWIN   - no info window pops up' $
	]
	xdisplayfile,text=output,title=wintitle
	return
end
	id = self.vdids(i)
	if keyword_set(title) then $
	self->getVD,id,nm,data,title=title,nowin=nowin else $
	self->getVD,id,nm,data,title='NX::VD',nowin=nowin
END

PRO NX::FindVD,name,select
;+
; NAME:
;	NX::FindVD
;
; PURPOSE:
;       This method finds the matched Vdata fro a given vdata name string.
;
; CALLING SEQUENCE:
;       Obj->[NX::]FindVD, Name [,Select]
;
; INPUT:
;     Name:      Specifies the name string of the desired vdata name.
;
;     Select:    Specifies the selected seqno of the matched Vdata name 
;
; EXAMPLE:
;     Example 1 lists all the vdata name defined in a NX HDF object v
;
;         v->FindVD,'*'
;
;     Example 2 find the vdata set with name of 'owner_name' from the HDF file
;
;         v->FindVD,'owner_name'
;
;     Example 2 find the vdata set with name of 'owner_name' from the HDF file
;     and select the 2nd set from the matched list.
;
;         v->FindVD,'owner_name',2
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jun 19, 1998.
;	xx-xx-xxxx      comment
;-

if n_params() eq 0 then begin
	str = ['Usage: Obj->[NX::]FindVD,Name [,Select]', $
	'','This method finds the matched Vdata for a given vdata name string.', $
	'','INPUT:', $
	'    Name   -  specifies the name string of the desired vdata',$
	'              Wild key * lists all the vdata found in the HDF file', $
	'    Select -  specifies the sequence number to be selected from the', $
	'              multiple matched vdata names list'$
	]
	xdisplayfile,text=str,title='NX::FindVD'	
	return
end
	filename = self.file
	fid = HDF_OPEN(filename,/READ)
	self->VgVd,gid,did
	match = -1
	match_name = ''
	for i=0,n_elements(did)-1 do begin
		vd = HDF_VD_ATTACH(fid,did(i))
		HDF_VD_GET,vd, name=nm, class=cl
		; include all vdata if name='*'
		if name eq '*' then begin
			if match(0) eq -1 then begin
				match = i 
				match_name = nm
			endif else begin
				match = [match,i]
				match_name = [match_name,nm]
			end
		endif else begin
		; search for name
		if strpos(nm,name) ge 0 then begin 
			if match(0) eq -1 then begin
				match = i 
				match_name = nm
			endif else begin
				match = [match,i]
				match_name = [match_name,nm]
			end
		end
		end
		HDF_VD_DETACH,vd
	end
	HDF_CLOSE,fid

	if n_elements(match) eq 1 then begin
		id = match(0)
		self->VD,id,name,data,title='NX::FindVD'
	endif else begin
	str = ['Search VData Name :' + name, '','    Seqno    Name']
	if n_elements(match) gt 0 then begin
		for i=0, n_elements(match)-1 do begin
		str = [str, string(match(i))+ '    ' + match_name(i)]
		end
	end

	if n_params() eq 2 then begin
		if select ge 0 and select le (n_elements(match)-1) then begin
		self->VD,match(select),name,data,ref=ref,title='NX::FindVD'
		str = [str,'', 'Select No: '+string(select)]
		return
		end
	end

	xdisplayfile,text=str,title='NX::FindVD'
	end

END

PRO NX::GetVD,id,nm,data,fields=fields,nrecrods=nrecords,output=output,title=title,nowin=nowin
;+
; NAME:
;	NX::GETVD
;
; PURPOSE:
;       For a given Vdata reference id this method gives the corresponding
;       VData information.
;
; CALLING SEQUENCE:
;       Obj->[NX::]GetVD, Id [,nm] [,data] [Fields=fields, Nrecords=nrecords,
;                     Output=output]
;
; INPUT:
;     Id:        Vdata reference id in a HDF file.
;
; OUTPUT:
;     Nm:        If specified, it returns the Vdata name 
;     Data:      If specified, it returns the Vdata data array
;
; KEYWORDS:
;     Fields:     If specified, it returns the field names for the Vdata
;     Nrecords:   It specifies the number of records to be read 
;     Output:     If specified, it returns the info output strings. 
;     Title:      If specified, it sets the GetVD info window name. 
;     Nowin:      If specified, no GetVD info window pops up. 
;
; EXAMPLE:
;     Example 1 finds the info of the first VData of the NX HDF object v
;
;         v->VgVd,gid,did
;         v->GetVD,did(0)
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jun 19, 1998.
;	xx-xx-xxxx      comment
;-

; id is from self.vdids(i)
;
if n_params() eq 0 then begin
	output=['Usage: Obj->[NX::]getVD,id,nm,data','', $
	'INPUT', $
	'     id   - Requested Vdata reference number from the HDF file', $
	'OUTPUT', $
	'     nm   - Return name of the requested Vdata', $
	'    data  - Return data array of the requested Vdata']
	xdisplayfile,text=output,title='NX::getVD,...'
	return
	end

	field = keyword_set(fields)
	nrecord = keyword_set(nrecords)
	ind = field + nrecord

	vd_id = HDF_VD_ATTACH(self.fid,id)
	if vd_id eq -1 then return

	HDF_VD_GET,vd_id,NAME=nm,NFIELDS=nf
	for index = 0,nf-1 do begin
	HDF_VD_GETINFO,vd_id,index,NAME=fdnm,TYPE=ty,ORDER=od,size=sz
        CASE ind OF
	0: nrec = HDF_VD_READ(vd_id,data)
	1: BEGIN
	   if nrecord then nrec = HDF_VD_READ(vd_id,data,nrecords=nrecords) else $
		begin 
	        if strpos(fdnm,fields) ge 0 then $
			nrec = HDF_VD_READ(vd_id,data,fields=fields) else $
			nrec = HDF_VD_READ(vd_id,data)
		end
	   END
	2: nrec = HDF_VD_READ(vd_id,data,fields=fields,nrecords=nrecords)
	ENDCASE

	help,nm,nf,ty,sz,nrec,data,output=out
	out= [out,string(string(data),/print)]
	end
	HDF_VD_DETACH,vd_id

	output='VD Ref ID (Long): '+ string(id)
	output = [output,out]
	if self.debug eq 0 or keyword_set(nowin) then return

	wintitle = 'NX::getVd'
	if keyword_set(title) then wintitle=title
	xdisplayfile,text=output,title=wintitle
END

PRO NX::getVg,id,gid=gid,tags,refs,ent_name,ent_type,output=str,name=name,class=class,nowin=nowin
; id - group ref id ,eg, gid(i)
;+
; NAME:
;	NX::GETVG
;
; PURPOSE:
;       This method searches for the VGroup by either matching the user 
;       specified HDF group reference id or matching the user specified group
;       name and/or classname.
;
; CALLING SEQUENCE:
;       Obj->[NX::]GetVG, {Id | Name=name, Class=class} [,/Nowin,Output=output,
;                 Gid=gid,Tags,Refs,Ent_name,Ent_type ]
;
; INPUT:
;     Id:        Specifies the group reference id of the interested group
;
; OUTPUT:
;     Tags:      Returns sub-group entries tags array 
;     Refs:      Returns sub-group entries refs array 
;     Ent_name:  Returns sub-group entries name array 
;     Ent_type:  Returns sub-group entries type array 
;
; KEYWORDS:
;     Name:      Specifies the group name to search for 
;     Class:     Specifies the group classname to search for 
;     Gid:       Returns the group id array of the HDF file if matching by
;                group name is chosen
;     Output:    Returns the info strings of matched groups
;     NOWIN:     If specified, no info window pops up 
;
; EXAMPLE:
;     Example 1 shows the first group of the HDF file '1.hdf' 
;
;         v = obj_new('NX',file='1.hdf')
;         v->VgVd,gid,did
;         v->GetVg,gid(0)
;
;     Example 2 shows all the groups which has group name starts with 'reactor'
;
;         v->GetVg, name='reactor'
;
;     Example 3 shows the group with known group name 'reactor1' and returns 
;     the group reference id array for the HDF file.
;
;         v->GetVg, name='reactor1', class='reactor1',gid=gid
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jun 19, 1998.
;	xx-xx-xxxx      comment
;-

; gid - group ref array for the file 
; list the groups if no search keyword found
; given name and/or class string find the matched groups and list all the subgroups
; return found group ids

search = 0

if keyword_set(name) and keyword_set(class) then search = 3
if keyword_set(name) then search = 1 
if keyword_set(class) then search = 2

fid = self.fid

if n_elements(id) eq 0 then begin
gid = self.vgids(0:self.numVG-1)
if gid(0) le 0 then self->VgVd,gid,did
endif else gid=id

if n_elements(id) eq 0 and search eq 0 then begin
	str = [ 'Usage: Obj->[NX::]GetVG,{Id | Name=name,Class=class}[,/Nowin,Output=output, ',$
	'          Gid=gid,Tags,Refs,Ent_name,Ent_type ]','',$
	'This method searches for the VGroup by either matching the user specified HDF ',$
	'group reference id or matching the user specified group name and/or classname.', $
	'','INPUT',$
	'   Id       - specifies group reference id of the group',$
	'OUTPUT',$
	'   Tags     - returns sub-group entries tags array ',$
	'   Refs     - returns sub-group entries refs array ',$
	'   Ent_name - returns sub-group entries name array', $
	'   Ent_type - returns sub-group entries type array', $
	'KEYWORD',$    
	'   Name     - specifies group name to search for', $
	'   Class    - specifies group classname to search for', $
	'   Gid      - returns group id array of the HDF file, if matching', $ 
        '              by name is chosen', $
	'   Output   - returns the info strings', $
	'   Nowin    - if specified, no info window pops up']
	xdisplayfile,text=str,title='NX::GetVg'
	return
end

ng = n_elements(gid)

str = 'Search for: '
if search eq 0 then str = 'List for: '
if n_elements(id) eq 1 then str = str + '  Gid Ref = '+string(id)
if keyword_set(name) then str = str + " name='"+name+"'"
if keyword_set(class) then str = str + ", class='"+class+"'"

str = [str,'','VGroups found:'] 
for i= 0, ng -1 do begin
	vg_hdl = HDF_VG_ATTACH(fid,gid(i))
	if vg_hdl gt 0 then begin
	HDF_VG_GETINFO,vg_hdl,class=cl,name=na,NENTRIES=nume

	found = 0
	CASE search OF 
	0: found=1 
	1: if strpos(na,name) ge 0 then found=1
	2: if strpos(cl,class) ge 0 then found=1
	3: if strpos(cl,class) ge 0 and strpos(na,name) ge 0 then found=1
	ENDCASE
	if found then begin 
	self->subVg,vg_hdl,nume,tags,refs,ent_name,ent_type,subgroups

		str = [str,$
		'Group Ref ID (Long):  '+strtrim(gid(i),2),  $
		'Group Name:           '+na, $
		'Group Class Name:     '+cl, $
		'Number of Entries:    '+ strtrim(nume,2)]
		if nume gt 0 then str = [str,subgroups]
		end
	HDF_VG_DETACH,vg_hdl
	end
end

	if keyword_set(nowin) then return
	if self.debug eq 0 or keyword_set(nowin) then return
	xdisplayfile,text=str,title="NX::getVg"
END

PRO NX::VgVd,vgroup_ids,vdata_ids,help=help
;+
; NAME:
;	NX::VGVD
;
; PURPOSE:
;       This method finds all the VGroup ids and VData ids for the specified
;       NX HDF objects. 
;
; CALLING SEQUENCE:
;       Obj->[NX::]VgVd, Vgroup_ids, Vdata_ids, /help
;
; OUTPUT:
;     Vgroup_ids:   Returns the Vgroup reference ids array   
;     Vdata_ids:    Returns the Vdata reference ids array   
;
; KEYWORDS:
;     Help:         Gives this on-line help
;
; EXAMPLE:
;     Example 1 gets the Vgroup (gid) and Vdata (did) arrays for the HDF file '1.hdf'
;
;         v = obj_new('NX',file='1.hdf')
;         v->VgVd, gid, did
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin Cha, Jun 19, 1998.
;	xx-xx-xxxx      comment
;-

if keyword_set(help) then begin
	str = ['Usage: Obj->[NX::]VgVd, Vgroup_ids, Vdata_ids, /Help','',$
	'This method finds all Vgroup ids and Vdata ids for the specified NX ',$
	'HDF objects.', '',$
	'INPUT', $
	'   None.', $
	'OUTPUT', $
	'   Vgroup_ids  - returns the Vgroup reference ids array', $ 
	'   Vdata_ids   - returns the Vdata reference ids array', $ 
	'KEYWORD',$
	'   Help        - gives this on-line help']
	xdisplayfile,text=str,title='NX::VgVd'
	return
end

	filename = self.file
	if self.fid eq 0 then begin
		self->open,filename
		end

	sd_id = HDF_SD_START(filename)
	HDF_SD_FILEINFO,sd_id,nmfsds,nglobatts
	self.numSDS = nmfsds
	self.numGAttr = nglobatts
	HDF_SD_END,sd_id

fid = self.fid
id=-1
tmp=(num=0)
while tmp ne -1 do begin
 tmp=HDF_VG_GETID(fid,id)
 if tmp ne -1 then begin
         num=num+1
        if id eq -1 then vgroup_ids = tmp  else  vgroup_ids = [vgroup_ids,tmp]
        end
 id=tmp
endwhile
self.numVG = num
 
id=-1
tmp=(num=0)
while tmp ne -1 do begin
 tmp=HDF_VD_GETID(fid,id)
 if tmp ne -1 then begin
         num=num+1
        if id eq -1 then vdata_ids = tmp  else  vdata_ids = [vdata_ids,tmp]
        end
 id=tmp
endwhile

self.numVD = num
;print,'numVG=', self.numVG
;print,'numVD=', self.numVD
	if self.numVG gt 0 then begin
	self.vgids = make_array(self.numVG,/LONG)
	if n_elements(vgroup_ids) then self.vgids = vgroup_ids 
	end
	if self.numVD gt 0 then begin
	self.vdids = make_array(self.numVD,/LONG)
	if n_elements(vdata_ids) then self.vdids = vdata_ids 
	end
return
 
END

PRO NX::Print,nowin=nowin
;+
; NAME:
;	NX::PRINT
;
; PURPOSE:
;     This method print a summary info about the hdf file opened
;
; CALLING SEQUENCE:
;	Obj->[NX::]Print [,NOWIN] [,NumSDS=numSDS] [,numGAttr=numgAttr]
;
; KEYWORD:
;    NOWIN:      Suppress the window display of the summary
;-
; close the file first to get the current HDF status
;
	filename = self.file
	str = ['File: '+filename,'']
	if HDF_ISHDF(filename) then begin

	self->VgVd

	str= [str,'Path :          '+self.path]
	str= [str,'File :          '+self.file]
	str= [str,'File Id:        '+string(self.fid)]
	str= [str,'numGAttr :      '+string(self.numGAttr)]
	str= [str,'numSDS :        '+string(self.numSDS)]
	str= [str,'numVG :         '+string(self.numVG)]
	str= [str,'numVD :         '+string(self.numVD)]
	str= [str,'sds_id :        '+string(self.sds_id)]
	str= [str,'seqno :         '+string(self.seqno)]
	str= [str,'endno :         '+string(self.endno)]
	str= [str,'maxno :         '+string(self.maxno)]
	str= [str,'vg_tag :        '+string(self.vg_tag)]
	str= [str,'sd_tag :        '+string(self.sd_tag)]
	str= [str,'user_name :     '+self.user_name]
	str= [str,'user_email :    '+self.user_email]
	str= [str,'user_phone :    '+self.user_phone]
	str= [str,'user_fax :      '+self.user_fax]
	str= [str,'user_history:   '+self.user_history]
	if self.desc_nline gt 0 then $
	str= [str,'','File_Descriptions:      ',self.user_desc(0:self.desc_nline - 1)]
	endif else begin
		str = [str,'Error: file is not a hdf file']
	end
	if keyword_set(nowin) then return
	xdisplayfile,text=str,title='NX::Print'
END

FUNCTION NX::Init,file=file,debug=debug
device,decompose=0   ; required for 24 bits
loadct,39

 	self.sd_tag = 720  ; SD
 	self.vg_tag = 1965 ; VG
	if keyword_set(debug) then self.debug=debug
	cd,current=h
  	self.path = h
	self.version = 'NX V1.0'
	self.key_id = 'new.dat'
	self.file = 'catch1d.trashcan.hdf'
	if keyword_set(file) then self.file = file
	return,1
END

PRO NX__define
struct = { NX, $
	version : 'NX', $
	path : '', $	
      file    : 'catch1d.trashcan.hdf', $
      fid     : 0L, $
      sd_id   : 0L, $
      key_id  : 'new.dat', $
      debug   : 0, $
      numSDS  : 0L, $
      numVG   : 0L, $
      numVD   : 0L, $
     numGAttr : 0L, $
      sds_id  : 0L, $
      seqno   : 0, $
	endno : 0, $
      maxno   : 0, $
      vgids: make_array(10000,/long), $
      vdids: make_array(10000,/long), $
	vg_tag : 1965, $
	sd_tag : 720, $
	user_name : '',$
	user_mail : '',$
	user_email : '',$
	user_phone : '',$
	user_fax : '',$
	user_history : '',$
	desc_nline : 1, $
	user_desc : strarr(100) $   ; 100 lines of desciptions per desc entry
        }
END

