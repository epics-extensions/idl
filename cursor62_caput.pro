;@ezcaIDL.pro


PRO CURSOR62_CAPUT, id
COMMON CATCH2D_FILE_BLOCK,catch2d_file
COMMON CATCH2D_IMAGE, widget_ids, view_option, image, image_ref

  IF id EQ 1 THEN BEGIN         ;  'CURSOR62_CAPUT0': BEGIN
        if view_option.versus eq 1 then begin
        y = [catch2d_file.x_pv+'.P1PV',catch2d_file.y_pv+'.P1PV']
        r = cagetArray(y,nm,/string)
        WIDGET_CONTROL,widget_ids.x_cursor,GET_VALUE=x
        WIDGET_CONTROL,widget_ids.y_cursor,GET_VALUE=y
        vl = make_array(1,2,/double)
        vl(0) = double(x)
        vl(0,1)= double(y)
        str = ['To Set New Positions:','', $
		nm(0)+'  (x)'+string(vl(0)),nm(1)+'  (y)'+string(vl(1))]
        res = dialog_message(str,/question,title='Set New P1PV...')
        if res eq 'No' then return
        r = caputArray(nm,vl)
        endif else begin
                str = ['Only available for Plot vs Values Option!', $
                        'But Plot vs Step # is set.']
                res = dialog_message(str,/error)
        end
	END

; put new center position
  IF id EQ 2 THEN BEGIN         ;  'CURSOR62_CAPUT': BEGIN
	if view_option.versus eq 1 then begin
	nm = [catch2d_file.x_pv+'.P1CP',catch2d_file.y_pv+'.P1CP']
	WIDGET_CONTROL,widget_ids.x_cursor,GET_VALUE=x
	WIDGET_CONTROL,widget_ids.y_cursor,GET_VALUE=y
	vl = make_array(1,2,/double)
	vl(0) = double(x)
	vl(0,1)= double(y)
	str = ['To Set New Center Positions:','' $
		,nm(0)+'  (x)'+string(vl(0)),nm(1)+'  (y)'+string(vl(1))]
	res = dialog_message(str,/question,title='Set New P1CP...')
	if res eq 'No' then return
	r = caputArray(nm,vl)
	endif else begin
		str = ['Only available for Plot vs Values Option!', $
			'But Plot vs Step # is set.']
		res = dialog_message(str,/error)
	end
	END

; put new start P1SP and end P1EP positions
  IF id EQ 3 THEN BEGIN         ;  'DRAW62 RMB box.txt event'
	if view_option.versus eq 1 then begin
	nm = [catch2d_file.x_pv+'.P1SP', $
	        catch2d_file.x_pv+'.P1EP', $
		catch2d_file.y_pv+'.P1SP', $
		catch2d_file.y_pv+'.P1EP' $
		]

	WIDGET_CONTROL,widget_ids.x1WID,GET_VALUE=x1
	WIDGET_CONTROL,widget_ids.x2WID,GET_VALUE=x2
	WIDGET_CONTROL,widget_ids.y1WID,GET_VALUE=y1
	WIDGET_CONTROL,widget_ids.y2WID,GET_VALUE=y2

	vl = make_array(1,4,/double)
	vl(0) = double(x1)
	vl(0,1) = double(x2)
	vl(0,2)= double(y1)
	vl(0,3)= double(y2)
	str = ['To Set New Start and End Positions:','', $
		nm(0)+'  (XL)'+string(vl(0)), $
		nm(1)+'  (XR)'+string(vl(1)), $
		nm(2)+'  (YL)'+string(vl(2)), $
		nm(3)+'  (YR)'+string(vl(3))]
	res = dialog_message(str,/question,title='Set New 2D Scan Ranges...')
	if res eq 'No' then return
	r = caputArray(nm,vl)
	endif else begin
		str = ['Only available for Plot vs Values Option!', $
			'But Plot vs Step # is set.']
		res = dialog_message(str,/error)
	end
	END
END
