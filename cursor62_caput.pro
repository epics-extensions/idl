;.run eacaIDL


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
        res = dialog_message(str,/question)
        if res eq 'No' then return
        r = caputArray(nm,vl)
        endif else begin
                str = ['Only available for Plot vs Values Option!', $
                        'But Plot vs Step # is set.']
                res = dialog_message(str,/error)
        end
	END
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
	res = dialog_message(str,/question)
	if res eq 'No' then return
	r = caputArray(nm,vl)
	endif else begin
		str = ['Only available for Plot vs Values Option!', $
			'But Plot vs Step # is set.']
		res = dialog_message(str,/error)
	end
	END
END
