;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
; regressfit.pro


PRO regressgraf,x,y,w,yfit,A0,sigma,ftest,r,rmul,chisq,status,relative_weight=relative_weight,print=print,GROUP=group
;+
; NAME:
; 	REGRESSGRAF
;
; PURPOSE:
;       This routine uses the IDL multiple linear regression fit function
;       REGRESS with optional weighting vector specification estimates. 
;       Then calls PLOT1D to graph the calculated results with the raw 
;       data in a pop-up window.
;
; CATEGORY:
; 	Curve fitting with plot.
;
; CALLING SEQUENCE:
;
;       REGRESSGRAF,X,Y,W [,YFIT] [,A0] [,SIGMA] [,FTEST] [,R] [,RMUL]
;               [,Chisq] [,Status] [,/Relative_weight] [,/PRINT] [Group=group] 
;         
;
; INPUTS:
;       X:        Position X vector 
;       Y:        Data value Y vector
;       W:        The vector of weights.  This vector should be same length as 
;                 Y.
;	
; KEYWORD PARAMETERS:
;   PRINT:       Specifies whether the output window will be poped up.
;   RELATIVE_WEIGHT: If specified no weighting is desired
;
; OPTIONAL OUTPUTS:
;         YFIT:  The vector of calculated Y's.  Has an error of + or - Yband.
;        A0:     Returns the constant item.
;        SIGMA:  The standard deviation in Y units.
;        FTEST:  Returns the value of F for test of fit.
;        R:      Returns the vector of linear correlation coefficents.
;        RMUL:   Returns the multiple linear correlation coefficient.
;        CHISQ:  Returns the reduced, weighted chi-squared
;        STATUS: Returns the status of internal array inversion computation.
;                0-successful, 1-singular, 2-small pivot point 
;
; PROCEDURE:
;      Before accessing this routine, the 'ez_fit.pro' must be loaded into
;      IDL and the path to 'ez_fit.pro' must be in the IDL search path.  
;
; EXAMPLE:
;
;      X = [ ...]
;      Y = [ ...]
;      REGRESSGRAF,X,Y,W,YFIT,A0,/PRINT
;
; MODIFICATION HISTORY:
;      Written by:  Ben-chin K. Cha, 03-01-02.
;      xx-xx-xxbkc  comment
;-

if n_params() lt 5 then begin
	str='Usage: regressGraf,X,Y,W,YFIT,A0 [,SIGMA,FTEST,R,RMUL,CHISQ,STATUS] [,/PRINT]'
	str=[str,'',$
	'Y = A0 + A1 * X1 + A2 * X2 + A3 * X3 + A4 * X4 + ...', $
	'','REGRESS - multiple linear regress fit with Weights']
	res=widget_message(str,/info,title='FITTING Info')
		return
	end

	sz = size(x)
	if sz(0) ne 2 then return

	ndegree = n_elements(y)
	if sz(2) ne ndegree then return

	w = replicate(1.0,ndegree)

	result = regress(x,y,w,yfit,A0,sigma,ftest,r,rmul,chisq,status)

	title = 'Regress Fit with Weights' 
	comment = [title,'']
; 	comment = [comment,'  A0 = '+strtrim(A0,2)]
	temp = '  A0 = '+strtrim(A0,2) 
	temp = temp + '  A'+strtrim(1,2)+'='+strtrim(Result(0),2) + $
		'  A'+strtrim(2,2)+'='+strtrim(Result(1),2)
	comment = [comment,temp]
	temp = '  SIGMA1='+strtrim(sigma(0),2) + '    SIGMA2='+strtrim(sigma(1),2)
	comment = [comment,temp]

	for i=1,ndegree do begin
		comment = [comment,' X'+strtrim(i,2)+'1='+string(x(0,i-1))+ $
				   ' X'+strtrim(i,2)+'2='+string(x(1,i-1))+ $
		',  Y('+strtrim(i,2)+') = A0 + X'+strtrim(i,2) + $
		'1*A1 + X'+strtrim(i,2)+'2*A2']
	end

	comment=[comment,'','TEST OF FIT = '+string(ftest)]
	comment=[comment,'Reduce,weighted chi-squared='+string(chisq)]
	comment=[comment,'Linear correlation coefficients = '+string(r(0))+','+ string(r(1))]
	comment=[comment,'Multiple linear correlation coefficient = '+string(rmul)]

	
	t_x = x(0,*)
	t_x = transpose(t_x)
	for i=0,ndegree-1 do begin
	ri = a0 +x(0,i)*result(0) +x(1,i)*result(1)
	if i eq 0 then t_y = ri else t_y=[t_y,ri]
	end
		
	curv=make_array(ndegree,2)
	curv(*,0) = y
	curv(*,1) = t_y

	lastl = n_elements(comment) - 1
	if lastl gt 10 then lastl=10
	plot1d,t_x,curv,/curvfit,title=title,comment=comment(2:lastl),width=500,/stamp, $
		Group=group,/symbol,wtitle='REGRESS',report='fitting.rpt'

	if keyword_set(print) then begin

        OPENW,unit,'fitting.rpt',/GET_LUN,ERROR=err
        if err ne 0 then begin
        res = widget_message(!err_string,/info,title='FITTING Info')
        return
	end

	printf,unit,'REGRESS - ',title

 	vec = moment(y, mdev=md, sdev=sd)
	mean = vec(0)
	variance = vec(1)
	skew = vec(2)
	kurtosis = vec(3)
	sdev = replicate(sd, n_elements(x))

;	statistic_1d,x,y,c_mass,x_peak,y_peak,y_hpeak,fwhm,fwhm_xl,fwhm_wd

;	st = ''
;	st = [st, '   Peak Y   = '+strtrim(y_peak,1)]
;	st = [st, '   1st Peak @ '+strtrim(x_peak,1)]
;       st = [st, '   H-Peak Y = '+strtrim(y_hpeak)]
;        st = [st, '   Centroid @ '+ strtrim(c_mass,1)]
;        st = [st, '   FWHM     = '+strtrim(FWHM,1)]
;	for i=0,n_elements(st)-1 do printf,unit,st(i)

	printf,unit,''
	printf,unit,'        MEAN=',mean
	printf,unit,'        SDEV=',sqrt(variance)
	printf,unit,'    VARIANCE=',variance
	printf,unit,''

 	for i=0,n_elements(comment) - 1 do printf,unit,comment(i)	
	printf,unit,''

	printf,unit,'            Xi1            Xi2            Y           YFIT        WEIGHT          '
	for i=0,n_elements(y)-1 do printf,unit,X(0,i),x(1,i),Y(i),YFIT(i),W(i),format='(5G15.8)'
	FREE_LUN,unit
; xdisplayfile,'fitting.rpt',title=title
	end

END






PRO regress_setup_Event, Event


  WIDGET_CONTROL, Event.top, GET_UVALUE=regress_state,/no_copy 
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'REGRESS_XI2INIT': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=xi2
	regress_state.value(1,*) = xi2
	WIDGET_CONTROL,regress_state.table,set_value=regress_state.value
        END
  'REGRESS_NROW': BEGIN
	WIDGET_CONTROL,Event.ID,GET_VALUE=n
	if n ge 3 then begin
	regress_state.NPT = n
	WIDGET_CONTROL,regress_state.table,get_value=v
	sz = size(v)
help,v
	if n gt sz(2) then $
		WIDGET_CONTROL,regress_state.table,insert_rows=n-sz(2)
	if n lt sz(2) and N ge 3 then $
		WIDGET_CONTROL,regress_state.table,/delete_rows,/use_table_select
		WIDGET_CONTROL,regress_state.table,get_value=vp
		s = size(vp)
		regress_state.NPT = s(2)
		WIDGET_CONTROL,Event.ID,SET_VALUE=regress_state.NPT
	endif else begin
		r = dialog_message('At least 3 row of data PNT is required',/error)
		WIDGET_CONTROL,Event.ID,SET_VALUE=regress_state.NPT
	end
      END
  'REGRESS_INIT': BEGIN
      END
  'REGRESS_ACCEPT': BEGIN
	WIDGET_CONTROL,regress_state.table,GET_VALUE=v
	s = size(v)
	if s(2) ge 3 then begin
	x = [v[0,*],v[1,*]]
	y = transpose(v[2,*])
	regressgraf,x,y,w,yfit,a0,sigma,ftest,r,rmul,chisq,status,/relative_weight,/print
;	WIDGET_CONTROL,Event.top,/destroy
;	return
	endif else begin
	r = dialog_message('At least 3 row of data PNT is required',/error)
	end
      END
  'REGRESS_CLOSE': BEGIN
	WIDGET_CONTROL,Event.top,/destroy
	return
      END
  'REGRESS_TYPE': BEGIN
	regress_state.type = Event.value 
      END
  'REGRESS_READTABLE': BEGIN
	filter = '*.xdr'
	if regress_state.type then filter = '*.txt'
	file = dialog_pickfile(filter=filter,get_path=p,/read,title='Read Table')
	if file ne '' then begin
	if regress_state.type eq 0 then begin
	xdr_open,unit,file
	xdr_read,unit,v
	xdr_close,unit
	endif else begin
	data = read_ascii(file)
	v = data.field1
; help,v
	sz = size(v)
	if sz(2) gt regress_state.NPT then begin
	WIDGET_CONTROL,regress_state.rowWID,SET_VALUE=sz(2)
	WIDGET_CONTROL,regress_state.table,insert_rows=sz(2)-regress_state.NPT
	regress_state.NPT = sz(2)
	end
	end
	WIDGET_CONTROL,regress_state.table,SET_VALUE=v
	WIDGET_CONTROL,regress_state.fileWID,SET_VALUE=file
	end
      END
  'REGRESS_WRITETABLE': BEGIN
	filter = '*.xdr'
	if regress_state.type then filter = '*.txt'
	file = dialog_pickfile(filter=filter,get_path=p,/write,title='Save Table')
	if file ne '' then begin
	r = dialog_message('Are you sure',/question)
	if r eq 'Yes' then begin
	WIDGET_CONTROL,regress_state.table,GET_VALUE=v

	if regress_state.type eq 0 then begin
	xdr_open,unit,file,/write
	xdr_write,unit,v
	xdr_close,unit
	endif else begin
	openw,1,file
	s = size(v)
		for i=0,s(2)-1 do begin
		printf,1,v(*,i)
		end
	close,1
	end
	WIDGET_CONTROL,regress_state.fileWID,SET_VALUE=file
	end
	end
      END
  'REGRESS_HELP': BEGIN
      str = [ 'Linear regression fitting to find A0, A1, and A2 ','',$
	'	Yi = A0 + A1 * Xi1 + A2 * Xi2   for all i ','', $
	'                    Input ASCII Format:','', $
	'The initial regression example is extracted from the IDL book.', $
	'A user can load sample data into this program by an ascii file', $
	'which should contain 3 values for each data point: Xi1, Xi2, and Yi,', $
	'i.e. a input line for each data point.  The ascii file can be',$
	'loaded into table through the "Read Table" button.', $
	'','                    User Interface:','', $
	'Read Table  - Read data from the selected file and load into table', $
	'XDR         - Read/Save table data as XDR', $
	'ASCII       - Read/Save table data as ASCII', $
	'Save Table  - Save tablulated data to a file', $
	'File Field  - Show filename where the table data comes from', $ 
	'Table       - Show the input data points', $
	'           Each row represent a data point', $ 
	'           Table element is modifiable by the user', $
	'NROW        - Reflects current number of rows in table widget', $
	'           It can also dynamically control the table sizes,i.e.', $
	'           insert/delete rows of data points from the table', $
	'           Larger value entered additional rows will be inserted', $
	'           Smaller value entered all the heighted rows will be deleted', $
	'Trial Xi2   - <CR> update Xi2 column with this new trial value', $
	'Help...     - Show this help page', $
	'Accept      - Accept the table and perform the regressing fit', $
	'Close       - Close this dialog', $
	'']	
	xdisplayfile,text=str,group=Event.top,title='Regression_Help_Page'
      END
  ENDCASE
  WIDGET_CONTROL, Event.top, SET_UVALUE=regress_state,/no_copy 
END



PRO regressfit,x,y, GROUP=Group
;+
; NAME:
;     REGRESSFIT
;
; PURPOSE:
;     Linear regression fitting program allows the user to calibrate the
;     data and call the IDL REGRESS function to get the linear regression
;     fitting to find A0, A1, and A2 
;
;	Yi = A0 + A1 * Xi1 + A2 * Xi2   for all i 
;
;     The sample data can be loaded into REGRESSFIT from an ASCII file.
;     The fitting plot and fitting report can be generated from 
;     this program.
;
; CATEGORY:
;     Widgets.
;
; CALLING SEQUENCE:
;
;     REGRESSFIT [,X,Y]  [,GROUP=Group]
;
; INPUTS:
;     X[2,NPT]:      The independent variable [Xi1,Xi2] corresponding to Yi
;     Y[NPT]:        The dependent variable Yi
;                    where NPT is the total number of data point.
; 
; KEYWORD PARAMETERS:
;     GROUP:         If specified, the close of parent window will close this
;                    regressfit window.
;
; SIDE EFFECTS:
;     If no X, Y data arrays are specified on the command line, then the 
;     default sample data from the IDL reference book is initially assumed.
;
;RESTRICTIONS:
;     The fitting.rpt... is shared by all fitting methods. If the fitting 
;     result is desired, it must be pressed before committing the next
;     fitting method.
;
; EXAMPLES:
;
;	regressfit
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, Mar. 7, 2002.
;
;-


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  regress_setup = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, title='Multi-Linear Regression Fit', $$
      UVALUE='regress_setup')

  BASE2 = WIDGET_BASE(regress_setup, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE2_0 = WIDGET_BASE(BASE2, $
      ROW=1, /frame, $
      MAP=1, $
      UVALUE='BASE2_0')

  BUTTON1 = WIDGET_BUTTON( BASE2_0, $
      UVALUE='REGRESS_READTABLE', $
      VALUE='Read Table')

  Btns1923 = [ $
    'XDR', $
    'ASCII' ]
  BGROUP3 = CW_BGROUP( BASE2_0, Btns1923, $
      ROW=1, $
      EXCLUSIVE=1, /no_release, $
      LABEL_LEFT=' ', $
      UVALUE='REGRESS_TYPE')
  WIDGET_CONTROL,BGROUP3,SET_VALUE=1

  BUTTON2 = WIDGET_BUTTON( BASE2_0, $
      UVALUE='REGRESS_WRITETABLE', $
      VALUE='Save Table')

 file_label = WIDGET_TEXT( BASE2,VALUE='',xsize=65, ysize=1,$
      UVALUE='REGRESS_FILENAME')

  NTERM = 3
if n_elements(x) eq 0 then begin
  NPT = 6
  x1 = [0.0,2.0,2.5,1.0,4.0,7.0]
  x2 = [0.0,1.0,2.0,3.0,6.0,2.0]
  Y = [5.0,10.0,9.0,0.0,3.0,27.0]
  endif else begin
	NPT = n_elements(Y)
	x1 = transpose(x(0,*))
	x2 = transpose(x(1,*))
	if n_elements(x1) ne NPT then begin
		r=dialog_message('Input error in X,y for regression fit',/error)
		return
	end
  end
  v = reform([x1,x2,y],NPT,NTERM)
  v = transpose(v)

  regress_table = WIDGET_TABLE(BASE2,/editable,column_widths=150, $
	column_labels=['Xi1','Xi2','Yi'], $
	row_labels='Row '+ string(indgen(NPT)+1), $
	/scroll, UVALUE='REGRESS_INIT', value=v)

  BASE5 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE5')
 FIELD3 = CW_FIELD( BASE5,VALUE=NPT, $
      ROW=1, xsize=4, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='NROW in Table:', $
      UVALUE='REGRESS_NROW')
 FIELD5 = CW_FIELD( BASE5,VALUE=5., $
      ROW=1, xsize=14, /FLOAT, $
      RETURN_EVENTS=1, $
      TITLE='Trial Xi2:', $
      UVALUE='REGRESS_XI2INIT')

  BASE3 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE3')

  BUTTON3 = WIDGET_BUTTON( BASE3, $
      UVALUE='REGRESS_HELP', $
      VALUE='Help...')

  BUTTON4 = WIDGET_BUTTON( BASE3, $
      UVALUE='REGRESS_ACCEPT', $
      VALUE='Accept')

  BUTTON5 = WIDGET_BUTTON( BASE3, $
      UVALUE='REGRESS_CLOSE', $
      VALUE='Close')

  regress_state = { base: regress_setup, $
	table:regress_table, $
	fileWID: file_label, $
	rowWID: FIELD3, $
	xi2WID: FIELD5, $
	type: 1, $    ; 0 -xdr, 1-ascii
	NPT: NPT, $
	NTERM: NTERM, $
	value: v $
	}

  WIDGET_CONTROL, regress_setup, /REALIZE
  WIDGET_CONTROL, regress_setup, SET_UVALUE=regress_state,/no_copy 

  XMANAGER, 'regress_setup', regress_setup
END
