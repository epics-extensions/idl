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


PRO readascii,filename,rarray,x,y,double=double,skip=skip,lines=lines,l_column=l_column,columns=columns,xcol=xcol,yrow=yrow ,print=print
COMMON EZ_FIT_BLOCK,ezfitData,image
COMMON W_READASCII_BLOCK,readascii_info
;+
; NAME: 
;      READASCII
;
; PURPOSE:
;      Read data from an ASCII file into an array. It is assumed that each line
;      with same number of entries. It can extract a sub-rectangle of region
;      from the ascii file.
;
; CATEGORY:
;      Input/Output
;
; CALLING SEQUENCE:
;      READASCII, Filename, Rarray, X, Y [ /DOUBLE, SKIP=skip, LINES=lines,
;                        L_COLUMN=l_column, COLUMNS=columns , XCOL=xcol ]
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
;       READASCII, 'Filename', RARRAY, X, Y 
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

if !os.os_family eq 'unix' then begin
spawn,['wc',filename],y, /noshell
if y(0) eq '' then begin
        res = dialog_message('Error: bad filename for readascii',/error)
        return
        end
wcy = strsplit(y(0),' ',/extract)
no = fix(wcy(0))
endif else WC,filename,no,yel

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
	if i eq yrow and ezfitData.dim eq 2 then begin
	yel = strsplit(strcompress(line),' ',/extract)
	end

	if i ge start_line then begin
	line=strcompress(strtrim(line,2))
	
	res = strsplit(line,' ',/extract)
	sz=size(res)

	; exclude the comment line

	if strmid(line,0,1) ne readascii_info.comment then begin 
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

	if ezfitData.dim eq 1 then Y = transpose(rarray)
	if ezfitData.dim eq 2 then begin
	if n_elements(yel)  eq 0 then Y=indgen(end_col-start_col) else $ 
		 Y = yel(start_col:end_col-1)
		 rarray = transpose(rarray)
	end
;help,yel,x,y,rarray


	if keyword_set(print) then begin
	help,rarray
	print,rarray
	help,x
	print,x
	help,y
	print,transpose(y)
	end
END


PRO w_readascii_Event, Event
COMMON EZ_FIT_BLOCK,ezfitData,image
COMMON W_READASCII_BLOCK,readascii_info

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'READASCII_SELECT': BEGIN
	WIDGET_CONTROL,readascii_info.pick,SENSITIVE=0
        F=DIALOG_PICKFILE(/READ,FILTER='*.txt*',GET_PATH=P,PATH=readascii_info.inpath)
        if F eq '' then return
        found = findfile(F)
        if found(0) ne '' then begin
	  WIDGET_CONTROL,readascii_info.file,SET_VALUE=strtrim(F(0),2)

	  if strpos(found(0),'image') gt 0 then begin
		WIDGET_CONTROL,readascii_info.dim,set_droplist_select=1
		ezfitData.dim = 2
		WIDGET_CONTROL,readascii_info.xcol,set_value=0
		WIDGET_CONTROL,readascii_info.yrow,set_value=3 
		WIDGET_CONTROL,readascii_info.left_col,set_value= 2
	  endif else begin
		WIDGET_CONTROL,readascii_info.dim,set_droplist_select=0
		ezfitData.dim = 1
		WIDGET_CONTROL,readascii_info.xcol,set_value=0
		WIDGET_CONTROL,readascii_info.yrow,set_value= -1
		WIDGET_CONTROL,readascii_info.left_col,set_value= 1
	  end
        endif else begin
                res=widget_message(F+ 'not found',/info,title='W_READASCII Info', $
                        DIALOG_PARENT=Event.id)
                return
        end
      END
  'READASCII_FILENAME': BEGIN
 ;     Print, 'Event for filename'
      END
  'READASCII_RAWDATA': BEGIN
	WIDGET_CONTROL,readascii_info.file,GET_VALUE=filename
	found = findfile(filename(0))
	if found(0) ne '' then xdisplayfile,filename(0)
      END
  'READASCII_DIM': BEGIN
	ezfitData.dim = Event.Index + 1
	if ezfitData.dim eq 2 then begin
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
 ;     Print, 'Event for l_column'
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
	ezfit_getvector,image,GROUP=Event.Top
      END
  'READASCII_ACCEPT': BEGIN
	WIDGET_CONTROL,readascii_info.pick,SENSITIVE=1
	WIDGET_CONTROL,readascii_info.skip,GET_VALUE=skip
	WIDGET_CONTROL,readascii_info.lines,GET_VALUE=lines
	WIDGET_CONTROL,readascii_info.left_col,GET_VALUE=l_col
	WIDGET_CONTROL,readascii_info.columns,GET_VALUE=columns
	WIDGET_CONTROL,readascii_info.xcol,GET_VALUE=xcol
	WIDGET_CONTROL,readascii_info.yrow,GET_VALUE=yrow
;		if columns eq 1 or columns lt 0 then begin
;			res=dialog_message('Number of Columns is invalid.',/Error)
;			return
;		end
	WIDGET_CONTROL,readascii_info.string,GET_VALUE=str
	readascii_info.comment = str(0)
	WIDGET_CONTROL,readascii_info.file,GET_VALUE=filename
	readascii,filename(0),rarray,x,y,xcol=xcol,yrow=yrow, $
		skip=skip,lines=lines,l_column=l_col,columns=columns
	if n_elements(x) eq 0 then return

	  sz = size(rarray)
		ezfitData.width = sz(1)
		ezfitData.height = sz(2)
		ezfitData.J_index=0
		ezfitDaTA.I_index=0
	if ezfitData.dim eq 1 then begin
		*ezfitData.im = y
                ezfit_init1d,x,y
	endif else begin
		image = rarray
		*ezfitData.im = rarray
                ezfit_init2D,x,y,rarray
	end

	if ezfitData.dim eq 2 then rarray = transpose(rarray)

	if readascii_info.table eq 0 then begin
	  readascii_info.cols = sz(1)
	  readascii_info.rows = sz(2)
	  readascii_info.table = widget_table(readascii_info.base,value=rarray,/editable,/scroll) 
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

;r = widget_info(readascii_info.table,/table_select)
;if r(0) eq r(2) then ezfitData.J_index=r(0)

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
	'1D/2D         - Droplist to treat the data read in as 1D/2D data array', $
	'ASCII...      - Use Xdisplayfile to display the content of ascii file', $
	'Y Axis Line # - Specify the line # containing Y coordinates for 2D data ', $
	'                default to 3 for 2D data,set to -1 if no Y value present ', $
	'Start Line    - Specify the start line # to begin with, default 0', $
	'Total Lines   - Specify the total lines be be read, default all', $
	'Comment Char  - Specify the char the comment line to begin with, default ";"', $
	'X Axis Column # - Specify the column # containing X coordinates', $
	'                  default 0, set to -1 if no X value present', $
	'Start Column  - Specify the start column # to begin with', $
	'                default 1 for 1D, default 2 tor 2D', $
	'Total Columns - Specify the total columns be be read, default all', $
	'Accept        - Accept all fields and tabulate the read in data', $
	'Pick Curve... - Pop up the curve selection dialog', $
	'Help...       - Show this help page', $
	'Close         - Close this program', $
	'Table         - Tabulate the columns of line data read in' $
	]
   res = widget_message(str,/info,title='W_READASCII Help')
      END
  'READASCII_CANCEL': BEGIN
        WIDGET_CONTROL,Event.top,/DESTROY
	return
      END
  ENDCASE


END


; DO NOT REMOVE THIS COMMENT: END MAIN13
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO w_readascii, filename, inpath=inpath, GROUP=Group
COMMON W_READASCII_BLOCK,readascii_info

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  if n_elements(inpath) eq 0 then cd,current=inpath

  READASCII_MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      TITLE='W_READASCII', $
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

  dim_droplist = WIDGET_DROPLIST(BASE2_0,value=['1D','2D'], $
	UVALUE='READASCII_DIM')

  rawdata = WIDGET_BUTTON( BASE2_0, $
      UVALUE='READASCII_RAWDATA', $
      VALUE='ASCII...')

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
      TITLE='       Start Line:', $
      UVALUE='FIELD4')

  FieldVal494 = [ $
    '' ]
  FIELD5 = CW_FIELD( BASE3,VALUE=FieldVal494, $
      ROW=1, XSIZE=5, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='   Total Lines:', $
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
      TITLE='Start Column:', $
      UVALUE='FIELD6')

  FieldVal624 = [ $
    '' ]
  FIELD7 = CW_FIELD( BASE4,VALUE=FieldVal624, $
      ROW=1, XSIZE=5,$
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Total Columns:', $
      UVALUE='FIELD7')

  BASE8 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE8')

  accept = WIDGET_BUTTON( BASE8, $
      UVALUE='READASCII_ACCEPT', $
      VALUE='Accept')

  pickvector = WIDGET_BUTTON( BASE8, $
      UVALUE='READASCII_PICKVECTOR', $
      VALUE='PickCurve...')
  WIDGET_CONTROL,pickvector,SENSITIVE=0

  help = WIDGET_BUTTON( BASE8, $
      UVALUE='READASCII_HELP', $
      VALUE='Help...')

  cancel = WIDGET_BUTTON( BASE8, $
      UVALUE='READASCII_CANCEL', $
      VALUE='Close')


  readascii_info = { skip:FIELD4,  lines:FIELD5, left_col:FIELD6, $
	columns:FIELD7, xcol:FIELD8, string: FIELD51, dim:dim_droplist, $
	yrow:FIELD81, pick:pickvector, inpath:inpath, $
	comment:comment_str, base:BASE2, table:0L, cols:0, rows:0, $
	file:READASCII_FILENAME }

  WIDGET_CONTROL, READASCII_MAIN13, /REALIZE
  XMANAGER, 'W_READASCII', READASCII_MAIN13
END
