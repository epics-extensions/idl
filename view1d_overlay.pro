;
; view1d_overlay.pro
;

@u_read.pro

PRO open_binary_type,unit,filename,type,wid
; check the binary type and return lun unit and XDR type
 
	type = 1
	U_OPENR,unit,filename,/XDR
        u_read,unit,version,errcode
	u_rewind,unit

        if errcode lt 0 then begin
	u_close,unit
	type = 0
	U_OPENR,unit,filename
        end

        if n_params() eq 4 then WIDGET_CONTROL,wid,set_droplist_select=type
END

PRO multiscan_readFileIndex,filename
COMMON  MULTI_BLOCK, multi_ids

;       print,'***Read Index File: ',indexfile
; check whether filename exists

fd = findfile(filename)
IF fd(0) NE '' THEN BEGIN

        openr,1,filename
        status = FSTAT(1)
        close,1
        indexfile = filename+'.index'

found = findfile(indexfile)
if found(0) ne '' then begin
        u_openr,unit,indexfile
        u_read,unit,name
        u_read,unit,fsize
        u_read,unit,maxno
        u_read,unit,array
        u_close,unit

        if status.size eq fsize(0) then begin
        multi_ids.idx_size = fsize(0)
        multi_ids.idx_maxno = maxno(0)
        multi_ids.idx_fptr = array
        end

	open_binary_type,unit,filename,type,multi_ids.pick
	multi_ids.XDR = type
	u_close, unit
	return
endif else begin
	; if index file not found then get all the record pointer
	open_binary_type,unit,filename,type,multi_ids.pick
	multi_ids.XDR = type
	id=0
	multi_ids.idx_fptr = make_array(10000,/long)
	WHILE NOT EOF(unit) DO BEGIN
	id = id + 1
		multiscan_read_record,unit
		point_lun,-unit,pos
		multi_ids.idx_fptr(id) = pos
	END	
        multi_ids.idx_size = status.size
	multi_ids.idx_maxno = id
end

ENDIF ELSE BEGIN
        ret= widget_message('Warning: file "' + filename + '" not found')
END 
END

PRO multiscan_read_record,unit,version,pv,num_pts,FA,x,y,n,ze,   id_def,x_dpt,labels

        u_read,unit,version
        u_read,unit,pv
        u_read,unit,num_pts
        u_read,unit,id_def
        u_read,unit,x_dpt

num_po = 0
for i=0,18 do begin
       if id_def(i) gt 0 then num_po = num_po + 1
end
FA = make_array(num_pts(0)+1,num_po)
for i=0,num_po-1 do begin
        u_read,unit,px
        FA(*,i) = px
end

        u_read,unit,labels
        u_read,unit,x
        u_read,unit,y
        u_read,unit,n
        if n(0) gt 0 then begin
        u_read,unit,ze
        end
END

PRO multi_read,filename, ids, lists,factors
COMMON  MULTI_BLOCK, multi_ids

; read index file

;	multiscan_readFileIndex,filename
; check for ids exceeds maxno

	for i=0,n_elements(ids)-1 do begin
	if ids(i) gt multi_ids.idx_maxno then begin
		ret=widget_message("Scan number exceeds maxno!", $
			DIALOG_PARENT=multi_ids.base)
		return
		end
	end

; read n1 n2 data
	if multi_ids.XDR eq 1 then u_openr,unit,filename,/XDR else $
	u_openr, unit, filename

	no = n_elements(ids)

	y=make_array(1000,no)
	ix = multi_ids.xaxis
	iy = multi_ids.yaxis

	legend=ids(0)
	for i=0, n_elements(ids)-1 do begin

	if i gt 0 then legend = [legend, ids(i)]
	point_lun, unit, multi_ids.idx_fptr(ids(i)-1)
	multiscan_read_record, unit, version,pv,num_pts,FA1,x1,y1,n1,ze1,id_def,x_dpt,labels

; build the array to be plotted
	num_pts=num_pts(0)+1
	x = findgen(num_pts)
	ij=0
	for j=0,3 do begin
		if ix eq (j+1) then x=FA1(0:num_pts-1,ij)
		if id_def(j) gt 0 then ij=ij+1
	end
	for j=4,18 do begin
		if id_def(j) gt 0 then begin
		if iy eq (j-4) then y(0,i) = FA1(0:num_pts-1,ij)
		ij=ij+1 
		end
	end
	end

	u_close, unit

; plot the data

	newy = y(0:num_pts -1,0:no-1)

	parse_desc_engu,labels,x_names,y_names,x_descs,y_descs,x_engus,y_engus
	title = pv
	comment = "Scans # " + lists
	
	temp= 'Detector '+strtrim(iy+1,2)  
	comment = [comment, temp, 'File: '+ filename]
	xtitle = x_descs(ix-1) 
	if strlen(x_engus(ix-1)) gt 0 then xtitle=xtitle+' (' +x_engus(ix-1)+')'
	ytitle = y_descs(iy)
	if strlen(y_engus(iy)) gt 0 then ytitle=ytitle+' ('+y_engus(iy)+')'
	symbol = multi_ids.symbol
	if multi_ids.symbol gt 1 then symbol = -1
	ylog = multi_ids.ylog
	linestyle = multi_ids.linestyle

if strtrim(multi_ids.title,2) ne '' then title = multi_ids.title
if strtrim(multi_ids.xtitle,2) ne '' then xtitle = multi_ids.xtitle
if strtrim(multi_ids.ytitle,2) ne '' then ytitle = multi_ids.ytitle

	plot1d,x,y,title=title,xtitle=xtitle,ytitle=ytitle, $
		legend=legend,$
		xylegend=[0.05, 0.95],comment=comment, /stamp, $
		symbol=symbol, $
		ylog=ylog, $
		linestyle=linestyle

END

; parse by sep1 first then by sep2
; default sep1=',' sep2='-'
;	newvalue = '1,2:5,7'
;	newvalue = '1,2-5,7'
PRO parse_num,newvalue,res,sep1=sep1,sep2=sep2
	d_sep1 = ','
	d_sep2 = '-'
	if keyword_set(sep1) then d_sep1 = sep1
	if keyword_set(sep2) then d_sep2 = sep2
	str = str_sep(newvalue,d_sep1,/trim)
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

PRO parse_num0,newvalue,ids,sep=sep
Keysepar = '-'
if keyword_set(sep) then Keysepar = sep
res = strpos(newvalue,keysepar)
if res ne -1 then begin
	str = str_sep(newvalue,keysepar,/trim)
	no = fix(str(1)) - fix(str(0)) + 1
	ids = indgen(no) + fix(str(0))
endif else begin
	com = strpos(newvalue,',')
	if com ne -1 then begin
        	str = str_sep(newvalue,',')
        	ids = fix(str)
	endif else begin
        	str = str_sep(newvalue,' ')
        	ids = fix(str)
	end
end
END


PRO parse_desc_engu,labels,x_names,y_names,x_descs,y_descs,x_engus,y_engus

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



PRO setLabels_Event, Event
COMMON  MULTI_BLOCK, multi_ids

  WIDGET_CONTROL,Event.top,GET_UVALUE=labels
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'FIELD4': BEGIN
	WIDGET_CONTROL,Event.id,GET_VALUE=title
	if strlen(title(0)) gt 0 then multi_ids.title=title(0)
      END
  'FIELD5': BEGIN
	WIDGET_CONTROL,Event.id,GET_VALUE=xtitle
	if strlen(xtitle(0)) gt 0 then multi_ids.xtitle=xtitle(0)
      END
  'FIELD6': BEGIN
	WIDGET_CONTROL,Event.id,GET_VALUE=ytitle
	if strlen(ytitle(0)) gt 0 then multi_ids.ytitle=ytitle(0)
      END
  'BUTTON9': BEGIN
	WIDGET_CONTROL,labels.title,GET_VALUE=title
	multi_ids.title = title(0)
	WIDGET_CONTROL,labels.xtitle,GET_VALUE=xtitle
	multi_ids.xtitle = xtitle(0)
	WIDGET_CONTROL,labels.ytitle,GET_VALUE=ytitle
	multi_ids.ytitle = ytitle(0)
;print,multi_ids.title
;print,multi_ids.xtitle
;print,multi_ids.ytitle
	WIDGET_CONTROL,Event.top,/DESTROY
      END
  'BUTTON10': BEGIN
	WIDGET_CONTROL,Event.top,/DESTROY
      END
  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END LABELS_BASE
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO setLabels, GROUP=Group


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  LABELS_BASE = WIDGET_BASE(GROUP_LEADER=Group, $
	TITLE='VIEW1D_OVERLAY (Set Labels)', $
      ROW=1, $
      MAP=1, $
      UVALUE='LABELS_BASE')

  BASE2 = WIDGET_BASE(LABELS_BASE, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  LABEL3 = WIDGET_LABEL( BASE2, $
;      FONT='-adobe-helvetica-bold-r-normal--20-140-100-100-p-105-iso8859-1', $
      UVALUE='LABEL3', $
      VALUE='Set  Plot  Labels')

  FieldVal1003 = [ $
    '' ]
  FIELD4 = CW_FIELD( BASE2,VALUE=FieldVal1003, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE=' Title', $
      UVALUE='FIELD4', $
      XSIZE=60)

  FieldVal1068 = [ $
    '' ]
  FIELD5 = CW_FIELD( BASE2,VALUE=FieldVal1068, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='XTitle', $
      UVALUE='FIELD5', $
      XSIZE=60)

  FieldVal1133 = [ $
    '' ]
  FIELD6 = CW_FIELD( BASE2,VALUE=FieldVal1133, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='YTitle', $
      UVALUE='FIELD6', $
      XSIZE=60)

  BASE7 = WIDGET_BASE(BASE2, $
      COLUMN=2, $
      MAP=1, $
      UVALUE='BASE7')

  BUTTON9 = WIDGET_BUTTON( BASE7, $
      UVALUE='BUTTON9', $
      VALUE='Done')

  BUTTON10 = WIDGET_BUTTON( BASE7, $
      UVALUE='BUTTON10', $
      VALUE='Cancel')

labels = { title: 0L, xtitle:0L, ytitle:0L }

  labels.title = FIELD4
  labels.xtitle = FIELD5
  labels.ytitle = FIELD6

  WIDGET_CONTROL, LABELS_BASE, /REALIZE
  WIDGET_CONTROL, LABELS_BASE, SET_UVALUE=labels,/no_copy
  XMANAGER, 'LABELS_BASE', LABELS_BASE, EVENT_HANDLER='setLabels_Event'
END

PRO VIEW1D_OVERLAY_PDMENU3_Event, Event
COMMON  MULTI_BLOCK, multi_ids

  CASE Event.Value OF


  'File.Open ...': BEGIN
    F=PICKFILE(TITLE='Open ... for Multiple Scans Overlay Plot', $
	/READ,PATH=multi_ids.path,GET_PATH=P)
	multi_ids.path = P
	multi_ids.filename = F
	WIDGET_CONTROL,multi_ids.filefld,SET_VALUE=F
	multiscan_readFileIndex,F
	WIDGET_CONTROL,multi_ids.ranges,SET_VALUE='Scan # [1-'+strtrim(multi_ids.idx_maxno,2)+']'

    END
  'File.Printer ...': BEGIN
    PS_printer, GROUP=Event.top
    END
  'File.Quit ': BEGIN
    WIDGET_CONTROL,Event.top,/DESTROY
    END
  ENDCASE
END


PRO MULTI_SCANS_Event, Event
COMMON  MULTI_BLOCK, multi_ids

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev


  CASE Ev OF 
  'PICK_XAXIS': begin
	multi_ids.xaxis = Event.Index
	end
  'PICK_YAXIS': begin
	multi_ids.yaxis = Event.Index
	end

  'PICK_XDR': BEGIN
        multi_ids.XDR = Event.Index
        END

  'YLOG_ONOFF': begin
	multi_ids.ylog = Event.Index
	end

  'SYMBOL_ONOFF': begin
	multi_ids.symbol = Event.Index
	end

  'LINESTYLE_ONOFF': begin
	multi_ids.linestyle = Event.Index
	end
  'FILE_MULTI': begin
	WIDGET_CONTROL,multi_ids.filefld,GET_VALUE=F
	multi_ids.filename = F(0)
	multiscan_readFileIndex,F(0)
	WIDGET_CONTROL,multi_ids.ranges,SET_VALUE='Scan # [1-'+strtrim(multi_ids.idx_maxno,2)+']'
	end


  'SCANS_MULTI': begin
	WIDGET_CONTROL,Event.id, GET_VALUE=newvalue
	sep1 = ','
	sep2 = '-'
	r1 = strpos(newvalue(0),sep1)
	if r1 eq -1  and strpos(newvalue(0), ' ') gt 0 then sep1 = ' '
	r2 = strpos(newvalue(0),sep2)
	if r2 eq -1  and strpos(newvalue(0), ':') gt 0 then sep2 = ':'
	parse_num,newvalue(0),ids,sep1=sep1,sep2=sep2
	if ids(0) eq 0 then begin 
        	ret= widget_message('Scan # not specified!',/ERROR, DIALOG_PARENT=Event.id)
		return
		end
	multi_read,multi_ids.filename,ids,newvalue(0) 
	end
  'VIEW_MULTI': begin
	WIDGET_CONTROL,multi_ids.field, GET_VALUE=newvalue
	sep1 = ','
	sep2 = '-'
	r1 = strpos(newvalue(0),sep1)
	if r1 eq -1  and strpos(newvalue(0), ' ') gt 0 then sep1 = ' '
	r2 = strpos(newvalue(0),sep2)
	if r2 eq -1  and strpos(newvalue(0), ':') gt 0 then sep2 = ':'
	parse_num,newvalue(0),ids,sep1=sep1,sep2=sep2
;	parse_num,newvalue(0),ids
	if ids(0) eq 0 then begin
        	ret= widget_message('Scan # not specified!',/ERROR, DIALOG_PARENT=Event.id)
		return
		end
	multi_read,multi_ids.filename,ids,newvalue(0) 
	end
  'HELP_MULTI': begin
 	str=["Enter scan numbers to be plotted togather in the text field.", $
	     "The valid scan range is shown right in front of the text field.", $
	     "Scan numbers entered must be separated by blank space or comma.", $
	     "For example three scans 1 100 and 200  to be plotted:", $
		"  1,100,200", $
		"",$
    	     "It is assumed that the X min and max ranges are same for each scan,",$
	     "i.e., they come from the same batch, same file.", $
		"", $
	     "Plot detector 1 vs positioner 1 with colored solid line are default", $
	     "settings. Options are provided to override the default setting.",$
		"", $
 	     "The PS paper plot is in black and white with different line style."]
	res = widget_message(str,/information,dialog_parent=Event.id, $
		title='Help on Entering Multiple Scans')
	end
  'DONE_MULTI': begin
	WIDGET_CONTROL,Event.top,/DESTROY
	end

  'PLOT_LABELS': setLabels,GROUP=Event.top

  ; Event for VIEW1D_OVERLAY_PDMENU3
  'VIEW1D_OVERLAY_PDMENU3': VIEW1D_OVERLAY_PDMENU3_Event, Event


  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END MULTI_SCANS
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO view1d_overlay,infile, XDR=XDR, GROUP=Group
;+
; NAME:
;       VIEW1D_OVERLAY	
;
; PURPOSE:
;
;       This program is specially written for the data catcher. It
;       allows the data catcher user overlay few 1D scans on the same 
;       plot for a selected detector.
;
; CATEGORY:
;	Widgets. 
;
; CALLING SEQUENCE:
;	VIEW1D_OVERLAY [,'filename'] [,/XDR] [,GROUP=Group] 
;
; INPUTS:
;	None
;
; KEYWORD PARAMETERS:
;     filename: Specifies the 1D file name to be used for displaying 
;               captured 1D scan data. If not specified, the default 
;               data file name 'catch1d.trashcan' is assumed.
;     XDR:      Required it the input data file is in XDR platform-
;               independent binary form.
;     GROUP:    The widget ID of the group leader of the widget.  If this 
;               keyword is specified, the death of the group leader results in
;               the death of VIEW1D.
;
; OUTPUTS:
;       It uses the general purpose 1D plot plackage to overlay plot the 
;       selected scans. The window is resizable by the X window manager.
;
; COMMON BLOCKS:
;       COMMON  MULTI_BLOCK, multi_ids
;
; SIDE EFFECTS:
;       It can be invoked by the script catcher_V1D as a stand along 
;       program.
;
; RESTRICTIONS:
;       Only native binary data is allowed.
;
; PROCEDURE:
;       The path to this program must be included in the IDL search
;       path.
;
; EXAMPLE:
;       Use default setting for view1d_overlay 
;
;       	VIEW1D_OVERLAY
;
;       Override the default setting by specifying the data file
;       on the command line
;
;       	VIEW1D_OVERLAY, 'test.dat.xdr', /XDR
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 05-27-97.
;
;       07-25-97  bkc   Rename the catcher_view1d to view1d_overlay.
;                       Add the support for XDR data format.
;       12-19-97  bkc   Automatic figure out input data format
;       01-15-98  bkc   Add the support of range of scans eg n1:n2 or n1-n2
;       03-24-98  bkc   Allows the startup of view1d_overlay even if the index
;                       file is not found
;-
COMMON  MULTI_BLOCK, multi_ids

  multi_ids = { $
	XDR: 0, $
	idx_size: 0L, $
	idx_maxno : 0L, $
	idx_fptr : make_array(10000,/long), $
	base : 0L, $
	pick : 0L, $
	ranges : 0L, $
	filefld : 0L, $
	field : 0L, $
	xaxis : 1, $
	yaxis : 0, $
ylog: 0, $
symbol: 0, $
linestyle: 0, $
title : '',$
xtitle : '',$
ytitle : '',$
	filename : 'catch1d.trashcan', $
	path : '.', $
	psfile : 'idl.ps' $
	}

  IF keyword_set(XDR) then multi_ids.XDR = 1

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  MULTI_SCANS = WIDGET_BASE(GROUP_LEADER=Group, $
      TITLE='VIEW1D_OVERLAY (Multiple Scans Plot)', $
      ROW=1, $
      MAP=1, $
      UVALUE='MULTI_SCANS')

  BASE2 = WIDGET_BASE(MULTI_SCANS, $
      /COLUMN, $
      MAP=1, $
      UVALUE='BASE2')

  BASE4 = WIDGET_BASE(BASE2, $
      /ROW, $
      MAP=1, $
      UVALUE='BASE4')

  Btns915 = ['BIN','XDR']
  pick_xdr = WIDGET_DROPLIST(BASE4, VALUE=BTNS915, $
        UVALUE='PICK_XDR',TITLE='Type')
  multi_ids.pick = pick_xdr
if multi_ids.XDR eq 1 then begin
  WIDGET_CONTROL,pick_xdr,set_droplist_select = 1
end

  MenuDesc167 = [ $
      { CW_PDMENU_S,       3, 'File' }, $ ;        0
        { CW_PDMENU_S,       0, 'Open ...' }, $ ;        1
        { CW_PDMENU_S,       0, 'Printer ...' }, $ ;        2
        { CW_PDMENU_S,       2, 'Quit ' } $  ;      3

  ]

  VIEW1D_OVERLAY_PDMENU3 = CW_PDMENU( BASE4, MenuDesc167, /RETURN_FULL_NAME, $
      UVALUE='VIEW1D_OVERLAY_PDMENU3')

filename='catch1d.trashcan'
if n_elements(infile) ne 0 then begin
	filename=infile 
	multi_ids.filename=infile
end

  FILE_MULTI = CW_FIELD( BASE4,VALUE=filename, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='File:', $
      UVALUE='FILE_MULTI', $
      XSIZE=60) 

 BASE3 = WIDGET_BASE(BASE2, $
	/ROW, $
      MAP=1, $
      UVALUE='BASE3')

ranges='Scan # [0-0]'
fd = findfile(filename)
IF fd(0) NE '' THEN BEGIN
  multiscan_readFileIndex,filename 
  ranges='Scan # [1-'+ strtrim(multi_ids.idx_maxno,2) + ']'
END

  RANGE_MULTI = WIDGET_LABEL( BASE3, VALUE=ranges, UVALUE='RANGE_MULTI', $
	/DYNAMIC_RESIZE) 

  SCANS_MULTI = CW_FIELD( BASE3,VALUE='', $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE=':', $
      UVALUE='SCANS_MULTI', $
      XSIZE=20)

  Btns913 = ['#','P1','P2','P3','P4']

  pick_xaxis = WIDGET_DROPLIST(BASE3, VALUE=BTNS913, $
        UVALUE='PICK_XAXIS',TITLE='Xaxis')
  WIDGET_CONTROL,pick_xaxis,set_droplist_select = 1

  Btns912 = ['D1','D2','D3','D4','D5','D6','D7','D8', $
             'D9','D10','D11','D12','D13','D14','D15']

  pick_yaxis = WIDGET_DROPLIST(BASE3, VALUE=BTNS912, $
        UVALUE='PICK_YAXIS',TITLE='Yaxis')
  WIDGET_CONTROL,pick_yaxis,set_droplist_select = 0

 BASE_OP1 = WIDGET_BASE(BASE2, $
	/ROW, $
      MAP=1, $
      UVALUE='BASE_OP1')

  plot_labels= WIDGET_BUTTON(BASE_OP1, VALUE='Set Labels', $
        UVALUE='PLOT_LABELS')

  symbol_onoff = WIDGET_DROPLIST(BASE_OP1, VALUE=['Off', 'On', 'Both'], $
        UVALUE='SYMBOL_ONOFF',TITLE='Symbol')
  WIDGET_CONTROL,symbol_onoff,set_droplist_select = 0

  linestyle_onoff = WIDGET_DROPLIST(BASE_OP1, VALUE=['Off', 'On'], $
        UVALUE='LINESTYLE_ONOFF',TITLE='LStyle')
  WIDGET_CONTROL,linestyle_onoff,set_droplist_select = 0

  ylog_onoff = WIDGET_DROPLIST(BASE_OP1, VALUE=['Off', 'On'], $
        UVALUE='YLOG_ONOFF',TITLE='Ylog')
  WIDGET_CONTROL,ylog_onoff,set_droplist_select = 0

  BASE5 = WIDGET_BASE(BASE2, $
      /ROW, $
      MAP=1, $
      UVALUE='BASE5')

  VIEW_MULTI = WIDGET_BUTTON( BASE5, $
      UVALUE='VIEW_MULTI', $
      VALUE='View')

  HELP_MULTI = WIDGET_BUTTON( BASE5, $
      UVALUE='HELP_MULTI', $
      VALUE='Help')

  DONE_MULTI = WIDGET_BUTTON( BASE5, $
      UVALUE='DONE_MULTI', $
      VALUE='Done')

multi_ids.base = BASE2
multi_ids.filefld = FILE_MULTI 
multi_ids.ranges = RANGE_MULTI
multi_ids.field = SCANS_MULTI

  WIDGET_CONTROL, MULTI_SCANS, /REALIZE

  XMANAGER, 'MULTI_SCANS', MULTI_SCANS
END
