;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
;
;  convert byte array to strings
;
PRO BytesToStrings,inbyte,outstring,lrecl=lrecl,print=print
if n_elements(inbyte) eq 0 then begin
        print,''
        print,"BytesToStrings  routine converts a byte array to a string array"
        print,"               with the user specifyable lrecl."
        print,''
        print,"USAGE: BytesToStrings, inbyte, outstring [,lrecl=#,/print]
        print,"INPUT:"
        print,'        inbyte   - input byte array, required'
        print,'OUTPUT:'
        print,'       outstring - output string array'
        print,'KEYWORD:
        print,'       LRECL=#   - specifies the output string length,'
        print,'                   # default to 80 if not specified.'
       print,'       /PRINT    - print the string array generated'
        print,''
        return
        end
len = 80
if n_elements(lrecl) gt 0 then len = lrecl
s = size(inbyte)
no = s(1)/len
if s(1) gt (no*len) then no = no +1
outstring = make_array(no,/string,value=string(replicate(32b,len)))
for i=0,no-1 do begin
        i1 = i*len & i2 = i1 + len - 1
        if i2 gt (s(1)-1) then i2 = s(1)-1
        outstring(i) = string(inbyte(i1:i2))
        if keyword_set(print) then print,outstring(i)
        end
END

PRO WC,filename,nline,ncol
; simulate unix command WC to read an ASCII file
; nline - return the total # of lines in file
; ncol  - returns the totol # of data elements in a line
	openr,1,filename
	line=''
	nline=0
	while NOT EOF(1) do begin
	on_ioerror,close1
	readf,1,line
	nline=nline+1
	if nline eq 1 then begin
		y = strsplit(line,' ',/extract)
		ncol = n_elements(y)
	end
	end
close1:
	close,1
END


PRO readascii_0,filename,rarray,x,y,double=double,skip=skip,lines=lines,l_column=l_column,columns=columns,xcol=xcol,yrow=yrow ,print=print
COMMON W_READASCII_BLOCK,readascii_info
;+
; NAME: 
;      READASCII_0
;
; PURPOSE:
;      Read data from an ASCII file into an array. It is assumed that each line
;      with same number of entries. It can extract a sub-rectangle of region
;      from the input  ascii file.
;
; CATEGORY:
;      Input/Output
;
; CALLING SEQUENCE:
;      READASCII_0, Filename, Rarray, X, Y [ /DOUBLE, SKIP=skip, LINES=lines,
;                        L_COLUMN=l_column, COLUMNS=columns , XCOL=xcol ]
;
; COMMON BLOCK: W_READASCII_BLOCK,readascii_info
;
; INPUT:
;      Filename -  Input file name
;
; KEYWORD PARAMETERS:
;      DOUBLE    -  specifies output variable in double position
;      SKIP      -  skip number of lines at beginning of file
;      LINES     -  total number of lines to be read
;      L_COLUMN  -  skip number of columns from the input line
;      COLUMNS   -  total number of columns to be read from the line
;      XCOL      -  specifies the X axis column number, defualt 0
;      YROW      -  specifies the Y axis column number, defualt 3 for 2D array
;      
; OUTPUTS:
;      Rarray   -  Return the dependent variable columns read in
;      X        -  Return the column as X independent variable
;      Y        -  Return dependent variable(s) for 1D data 
;                  Return the Y independent variable for 2D data
;
; RESTRICTIONS:
;     The input file must be in ASCII form. The header lines must be placed
;     at the beginning of the file. They can be skipped by setting the SKIP 
;     equal to the number of header lines at beginning. Each data line must 
;     contains the exactly same number of columns.
;
; EXAMPLE:
;   
;       READASCII_0, 'Filename', RARRAY, X, Y 
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha
;
;       09-16-98      bkc  Allows blank lines at end of ascii file, no blank
;                          lines allowed between data
;       11-23-99      bkc  Use keyword XCOL to specify the column to be used
;                          as X-axis
;       01-23-02      bkc  Use keyword YROW to specify the line to be used 
;                          as Y-axis ( only used by 2D data array)
;-

if n_params() eq 0 then begin
	print,'Usage: READASCII, Filename, Rarray, X, Y [ /DOUBLE, SKIP=skip, LINES=lines, $' 
	print,'      L_COLUMN=l_column, COLUMNS=columns ]'
	return
end

WC,filename,no,yel

start_line=0
last_line=no
start_col = 0
if keyword_set(skip) then start_line=skip
if keyword_set(lines) then last_line=skip+lines
if last_line gt no then last_line = no

if keyword_set(xcol) eq 0 then xcol = 0
if keyword_set(yrow) eq 0 then yrow = 3      ; true for view2d_data.txt 

line=''
openr,unit,filename,/get_lun
i=0
nline=0
WHILE NOT eof(unit) and i lt last_line DO begin
	readf,unit,line,prompt=''
	if strmid(line,0,1) ne readascii_info.comment then begin 
	if i eq yrow and readascii_info.dim eq 2 then begin
	yel = strsplit(strcompress(line),' ',/extract)
	end

	if i ge start_line then begin
	line=strcompress(strtrim(line,2))
	
	res = strsplit(line,' ',/extract)
	sz=size(res)

	; exclude the comment line

	if i eq start_line then begin
	lines = last_line - start_line
	end_col = sz(1)
		if keyword_set(l_column) then begin
		 if l_column lt sz(1) and l_column ge 0 then start_col=l_column
		end
		if keyword_set(columns) then end_col = start_col + columns
		if end_col gt sz(1) then end_col = sz(1)
		if keyword_set(double) then $
		rarray = make_array(end_col-start_col,lines,/double) else $
		rarray = make_array(end_col-start_col,lines,/float)
	end
catch,error_status
if error_status ne 0 then begin
	r = dialog_message([!error_state.msg,'','May be caused by Start Line error!!'],/error)
	return
end
	if strlen(line) gt 0 then begin
	if xcol lt 0 then begin
	if nline eq 0 then x = indgen(lines)
	endif else begin
	if nline eq 0 then x = float(res(xcol)) else x = [x,res(xcol)]
	end
	rarray(*,nline) = float(res(start_col:end_col-1))
	nline = nline+1
	end
	endif else  begin
		rstart_line = i+1
		if rstart_line gt start_line then start_line = rstart_line
		end
	end
	i = i+1
end
free_lun,unit
	if nline lt lines then rarray = rarray(*,0:nline-1) 

	if readascii_info.dim eq 1 then Y = transpose(rarray)
	if readascii_info.dim eq 2 then begin
	if n_elements(yel)  eq 0 then Y=indgen(end_col-start_col) else $ 
		 Y = yel(start_col:end_col-1)
;		 rarray = transpose(rarray)
	end


	if keyword_set(print) then begin
	help,rarray
	print,rarray
	help,x
	print,x
	help,y
	print,transpose(y)
	end
END

PRO wd_readascii_reset,file,readascii_info

n = strpos(file,!os.file_sep,/reverse_search)
p = strmid(file,0,n)

	  if strpos(file,'im') gt 0 then begin
		WIDGET_CONTROL,readascii_info.dimWID,set_droplist_select=1
		readascii_info.dim = 2
		WIDGET_CONTROL,readascii_info.skip,set_value=7
		WIDGET_CONTROL,readascii_info.xcol,set_value=0
		WIDGET_CONTROL,readascii_info.yrow,set_value=3 
		WIDGET_CONTROL,readascii_info.left_col,set_value= 2
	  endif else begin
		WIDGET_CONTROL,readascii_info.dimWID,set_droplist_select=0
		readascii_info.dim = 1
		WIDGET_CONTROL,readascii_info.skip,set_value=0
		WIDGET_CONTROL,readascii_info.xcol,set_value=0
		WIDGET_CONTROL,readascii_info.yrow,set_value= -1
		WIDGET_CONTROL,readascii_info.left_col,set_value= 1
	  end
	  readascii_info.inpath=p
	  openw,1,'readascii.config'
	  printf,1,p
	  close,1

END


PRO wd_readascii_Event, Event
COMMON W_READASCII_BLOCK,readascii_info

  WIDGET_CONTROL,Event.top,GET_UVALUE=readascii_info

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'READASCII_SELECT': BEGIN
	widget_control,readascii_info.vectWID,sensitive=0
	widget_control,readascii_info.transWID,sensitive=0
	widget_control,readascii_info.imWID,sensitive=0
        F=DIALOG_PICKFILE(/READ,FILTER=['*.txt*','*im*'],GET_PATH=P,PATH=readascii_info.inpath)
        if F eq '' then return
        found = findfile(F)
        if found(0) ne '' then begin
	  WIDGET_CONTROL,readascii_info.file,SET_VALUE=strtrim(F(0),2)
	  wd_readascii_reset,F(0),readascii_info
        endif else begin
                res=widget_message(F+ 'not found',/info,title='WD_READASCII Info', $
                        DIALOG_PARENT=Event.id)
                return
        end
      END
  'READASCII_FILENAME': BEGIN
	widget_control,readascii_info.vectWID,sensitive=0
	widget_control,readascii_info.transWID,sensitive=0
	widget_control,readascii_info.imWID,sensitive=0
	WIDGET_CONTROL,readascii_info.file,GET_VALUE=F
        found = findfile(F(0))
        if found(0) ne '' then begin
	 wd_readascii_reset,F(0),readascii_info
        endif else begin
         res=widget_message(F+ 'not found',/info,title='WD_READASCII Info', $
                        DIALOG_PARENT=Event.id)
         return
        end
      END
  'READASCII_RAWDATA': BEGIN
	WIDGET_CONTROL,readascii_info.file,GET_VALUE=filename
	found = findfile(filename(0))
	if found(0) ne '' then xdisplayfile,filename(0),group=Event.top
      END
  'READASCII_DIM': BEGIN
	readascii_info.dim = Event.Index + 1
	if readascii_info.dim eq 2 then begin
	WIDGET_CONTROL,readascii_info.yrow,set_value=3 
	WIDGET_CONTROL,readascii_info.left_col,set_value= 2
	endif else begin
	WIDGET_CONTROL,readascii_info.yrow,set_value=-1
	WIDGET_CONTROL,readascii_info.left_col,set_value= 1
	end
      END
  'FIELD4': BEGIN
      Print, 'Event for Start line'
      END
  'FIELD5': BEGIN
 ;     Print, 'Event for lines'
      END
  'FIELD6': BEGIN
	widget_control,Event.id,get_value=lc
	if lc le 0 then $
	widget_control,readascii_info.xcol,set_value=lc-1
      END
  'FIELD7': BEGIN
 ;     Print, 'Event for columns'
      END
  'FIELD81': BEGIN
 ;     Print, 'Event for Y axis'
      END
  'FIELD8': BEGIN
 ;     Print, 'Event for X axis'
      END
  'READASCII_PICKVECTOR': BEGIN
	r = widget_info(readascii_info.table,/table_select)
	rarray = *readascii_info.im
	array = rarray(r(1):r(3),r(0):r(2))
	if r(1) eq r(3) then array=transpose(array)
	if readascii_info.axis eq 0 then begin
	  if n_elements(array) gt 1 then plot1d,array else $
	  plot1d,rarray
	endif else begin
	  x = *readascii_info.x
	  if n_elements(array) gt 1 then plot1d,x,array else $
	  plot1d,x,rarray
	end
      END
  'READASCII_PLOT2D': BEGIN
	rarray = *readascii_info.im
	if readascii_info.axis eq 0 then begin
	  plot2d,rarray,/itools,group=Event.top
	endif else begin
	  x = *readascii_info.x
	  y = *readascii_info.y
	  plot2d,rarray,xarr=x,yarr=y,/itools,group=Event.top
	end
      END
  'READASCII_AXIS': BEGIN
	readascii_info.axis = Event.Index 
      END
  'READASCII_TRANSPOSE': BEGIN
        if readascii_info.table ne 0 then begin
	  rarray = *readascii_info.im
	  *readascii_info.im = transpose(rarray)
	  y = *readascii_info.x
	  x = *readascii_info.y
	  *readascii_info.x = x
	  *readascii_info.y = y
	  sz = size(rarray)
          readascii_info.cols = sz(1)
          readascii_info.rows = sz(2)
          if sz(1) gt readascii_info.cols then begin
            widget_control,readascii_info.table, $
                insert_columns=sz(1) - readascii_info.cols
            readascii_info.cols = sz(1)
          end
          if sz(2) gt readascii_info.rows then begin
            widget_control,readascii_info.table, $
                insert_rows=sz(2) - readascii_info.rows
            readascii_info.rows = sz(2)
          end
          widget_control,readascii_info.table,set_value=rarray
        end
      END
  'READASCII_ACCEPT': BEGIN
	WIDGET_CONTROL,readascii_info.skip,GET_VALUE=skip
	WIDGET_CONTROL,readascii_info.lines,GET_VALUE=lines
	WIDGET_CONTROL,readascii_info.left_col,GET_VALUE=l_col
	WIDGET_CONTROL,readascii_info.columns,GET_VALUE=columns
	WIDGET_CONTROL,readascii_info.xcol,GET_VALUE=xcol
	WIDGET_CONTROL,readascii_info.yrow,GET_VALUE=yrow
	WIDGET_CONTROL,readascii_info.string,GET_VALUE=str
	readascii_info.comment = str(0)
	WIDGET_CONTROL,readascii_info.file,GET_VALUE=filename
	readascii_0,filename(0),rarray,x,y,xcol=xcol,yrow=yrow, $
		skip=skip,lines=lines,l_column=l_col,columns=columns
	if n_elements(x) eq 0 or n_elements(rarray) eq 0 then return

	if n_elements(x) gt 1 then *readascii_info.x = x else $
		*readascii_info.x = indgen(1)
	if n_elements(y) gt 1 then *readascii_info.y = y else $
		*readascii_info.y = indgen(1)
	*readascii_info.im = transpose(rarray)
	sz = size(rarray)
	if readascii_info.dim eq 1 then begin
		if sz(1) ne n_elements(x) then  begin
		r = dialog_message('Please verify the col or row ranges',/info)
		end
	end

;        if readascii_info.dim eq 2 then rarray = transpose(rarray)

        if readascii_info.table eq 0 then begin
          readascii_info.cols = sz(1)
          readascii_info.rows = sz(2)
	  HT = sz(1)
	  if sz(2) gt HT then HT = sz(2)
          readascii_info.table = widget_table(readascii_info.base,/editable, $
		/scroll,value=make_array(HT,HT))
	  widget_control,readascii_info.table,set_value=rarray
        endif else begin
          if sz(1) gt readascii_info.cols then begin
            widget_control,readascii_info.table, $
                insert_columns=sz(1) - readascii_info.cols
            readascii_info.cols = sz(1)
          end

          if sz(2) gt readascii_info.rows then begin
            widget_control,readascii_info.table, $
                insert_rows=sz(2) - readascii_info.rows
            readascii_info.rows = sz(2)
          end
          widget_control,readascii_info.table,set_value=rarray
        end
	widget_control,readascii_info.vectWID,sensitive=1
	widget_control,readascii_info.transWID,sensitive=1
	widget_control,readascii_info.imWID,sensitive=1
      END
  'READASCII_HELP': BEGIN
	str=['This program allows the user flexiblely to set up the row and', $
	'column criteria in order to properly read the input ascii data file.', $
	'The input file should contain multi-columns of 1D or 2D text data.', '', $
	'ASCII... should be examined in determing the correct row/column #.', $
	'Zero based index # is used in column or line number specification.', '', $
	'File          - Use the file selection box to pick the ascii file which', $
	'                contains column of X and mumtiple columns of Y and',$
	'                parameter values', $
	'Filename      - Field for entering the ascii file name directly', $
	'ASCII...      - Use Xdisplayfile to display the content of ascii file', $
	'1D/2D         - Droplist to treat the data read in as 1D/2D data array', $
	'Close         - Close this program and return the reascii_info struct', $
	'Y Axis Line # - Specify the line # containing Y coordinates for 2D data ', $
	'                default to 3 for 2D data,set to -1 if no Y value present ', $
	'Data Start Line - Specify the start line # to begin with, default 0', $
	'Total Data Lines - Specify the total lines be be read, default all', $
	'Comment Char  - Specify the char the comment line to begin with, default ";"', $
	'X Axis Column # - Specify the column # containing X coordinates', $
	'                  default 0, set to -1 if no X value present', $
	'Data Start Column - Specify the start column # to begin with', $
	'                default 1 for 1D, default 2 tor 2D', $
	'Total Data Columns - Specify the total columns be be read, default all', $
	'Accept        - Accept all fields and tabulate the read in data', $
	'Transpose     - Transpose table array', $
	'PLOT1D...     - Pass the selected vector to plot1d subprogram', $
	'PLOT2D...     - Pass the selected vector to plot1d subprogram', $
	'Help...       - Show this help page', $
	'Step#/Values  - Droplist to set plot axis in step #/real values', $
	'Table         - Tabulate the columns of line data read in' $
	]
   res = widget_message(str,/info,title='WD_READASCII Help')
      END
  'READASCII_CANCEL': BEGIN
        WIDGET_CONTROL,Event.top,/DESTROY
	return
      END
  ENDCASE

  WIDGET_CONTROL,Event.top,SET_UVALUE=readascii_info

END


PRO wd_readascii_init,readascii_info

  readascii_info = { skip:0L, $
	lines:0L, $
	left_col:0L, $
	columns:0L,  $
	xcol:0L,  $
	string: 0L, $
	dimWID:0L,  $
	transWID:0L,  $
	vectWID:0L,  $
	imWID:0L, $
	yrow:0L,  $
	file:0L, $
	base: 0L,  $
	table: 0L,  $
	inpath: '', $
	comment: '',  $
	cols:0,  $
	rows:0, $
	dim : 2, $
	axis : 0, $
	x:  ptr_new(/allocate_heap), $
	y:  ptr_new(/allocate_heap), $
	im: ptr_new(/allocate_heap) }

	inpath=''
        found = findfile('readascii.config')
        if found(0) ne '' then begin
	  openr,1,'readascii.config'
	  readf,1,inpath
	  close,1
	endif else cd,current=inpath

	readascii_info.inpath = inpath
END

PRO wd_readascii, filename, readascii_info,inpath=inpath, GROUP=Group
;+
; NAME: 
;      WD_READASCII
;
; PURPOSE:
; 
; 	A widget program allows the user to freely read fix formatted ascii 
;	data array from any text file. It provides text window of showing 
;	raw ascii text file then a user can use the column and row widgets 
;	controls to extract data array from the input file, the read in 
;	array is displayed in the scroll table widget. Then it provides 
;	options of calling PLOT1D and PLOT2D to diaplay any 1D or 2D data 
;	arrays. 
;
;	It also provides the IDL 6.1 interactive ITOOLS from PLOT1D and
;	PLOT2D program. 
;
; CATEGORY:
;       Widget program
;
; INPUT:
;	FILENAME - Specify the input text file, default use the file
;		   selection dialog 
; OUTPUT:
;	READASCII_INFO - return the readascii_info data structure
;
; KEYWORD PARAMETERS:
;	INPATH    - return the input file path directory
;	GROUP	 - Specify the parent widget ID, close of parent widget
;		   resulting close of this program
; EXAMPLE:
;
;	wd_readascii
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha
;-

	wd_readascii_init,readascii_info

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  READASCII_MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      TITLE='WD_READASCII', $
      MAP=1, $
      UVALUE='READASCII_MAIN13')

  BASE2 = WIDGET_BASE(READASCII_MAIN13, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE2_0 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2')

  file_button = WIDGET_BUTTON(BASE2_0, $
	VALUE='File', $
	UVALUE='READASCII_SELECT')

  READASCII_FILENAME = CW_FIELD( BASE2_0,VALUE=filename, $
      ROW=1, XSIZE=50, $
      RETURN_EVENTS=1, $
	TITLE=' ', $
      UVALUE='READASCII_FILENAME')

  rawdata = WIDGET_BUTTON( BASE2_0, $
      UVALUE='READASCII_RAWDATA', $
      VALUE='ASCII...')

  dim_droplist = WIDGET_DROPLIST(BASE2_0,value=['1D','2D'], $
	UVALUE='READASCII_DIM')
  widget_control,dim_droplist,set_droplist_select=readascii_info.dim - 1

  BASE3 = WIDGET_BASE(BASE2, $
      MAP=1, /ROW, $
      UVALUE='BASE3')

  FIELD81 = CW_FIELD( BASE3,VALUE=-1, $
      ROW=1, XSIZE=5,$
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Y Axis Line #:', $
      UVALUE='FIELD81')

  FieldVal429 = [ $
    '0' ]
  FIELD4 = CW_FIELD( BASE3,VALUE=FieldVal429, $
      ROW=1, XSIZE=5, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='  Data Start Line:', $
      UVALUE='FIELD4')

  FieldVal494 = [ $
    '' ]
  FIELD5 = CW_FIELD( BASE3,VALUE=FieldVal494, $
      ROW=1, XSIZE=5, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='  Total Data Lines:', $
      UVALUE='FIELD5')

  comment_str = ';'
  FIELD51= CW_FIELD( BASE3,VALUE=comment_str, $
      ROW=1, XSIZE=2, $
      RETURN_EVENTS=1, $
      TITLE='   Comment Char:', $
      UVALUE='READASCII_STRING')

  BASE4 = WIDGET_BASE(BASE2, $
      MAP=1, /ROW, $
      UVALUE='BASE3')

  FIELD8 = CW_FIELD( BASE4,VALUE=0, $
      ROW=1, XSIZE=5,$
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='X Axis Column #:', $
      UVALUE='FIELD8')

  FIELD6 = CW_FIELD( BASE4,VALUE=2, $
      ROW=1, XSIZE=5,$
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Data Start Column:', $
      UVALUE='FIELD6')

  FieldVal624 = [ $
    '' ]
  FIELD7 = CW_FIELD( BASE4,VALUE=FieldVal624, $
      ROW=1, XSIZE=5,$
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Total Data Columns:', $
      UVALUE='FIELD7')

  BASE8 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE8')

  accept = WIDGET_BUTTON( BASE8, $
      UVALUE='READASCII_ACCEPT', $
      VALUE='Accept')

  transp = WIDGET_BUTTON( BASE8, $
      UVALUE='READASCII_TRANSPOSE', $
      VALUE='Transpose')
  pickvect = WIDGET_BUTTON( BASE8, $
      UVALUE='READASCII_PICKVECTOR', $
      VALUE='PLOT1D...')
  plot2d_b = WIDGET_BUTTON( BASE8, $
      UVALUE='READASCII_PLOT2D', $
      VALUE='PLOT2D...')

  widget_control,transp,sensitive=0
  widget_control,pickvect,sensitive=0
  widget_control,plot2d_b,sensitive=0

  help = WIDGET_BUTTON( BASE8, $
      UVALUE='READASCII_HELP', $
      VALUE='Help...')

  axis_droplist = WIDGET_DROPLIST(BASE8,value=['Step #','Values'], $
	UVALUE='READASCII_AXIS')
  widget_control,dim_droplist,set_droplist_select=readascii_info.axis

  cancel = WIDGET_BUTTON( BASE2_0, $
      UVALUE='READASCII_CANCEL', $
      VALUE='Close')


	readascii_info.skip = FIELD4
	readascii_info.lines = FIELD5
	readascii_info.left_col = FIELD6
	readascii_info.columns = FIELD7
	readascii_info.xcol = FIELD8
	readascii_info.yrow = FIELD81
	readascii_info.string = FIELD51
	readascii_info.dimWID = dim_droplist
	readascii_info.vectWID = pickvect
	readascii_info.transWID = transp
	readascii_info.imWID = plot2d_b
	readascii_info.comment = comment_str
	readascii_info.base = base2
	readascii_info.cols = 0
	readascii_info.rows = 0
	readascii_info.file = READASCII_FILENAME

  WIDGET_CONTROL, READASCII_MAIN13, /REALIZE
  widget_control,READASCII_MAIN13,set_uvalue=readascii_info

  XMANAGER, 'WD_READASCII', READASCII_MAIN13
END
