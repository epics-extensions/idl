; save tiff/pict/png image 


PRO save_tiff,win=win,file=file,path=path
;       file = plot2d_state.class+'.tiff' set init location of tiff file
;
	if keyword_set(win) eq 0 then win=!d.window
        old_win = !d.window
        tvlct,R,G,B,/get
	if keyword_set(path) eq 0 then begin
        cd,current=p
        p = p + !os.file_sep +'TIFF'+!os.file_sep
	endif else p=path
        found = findfile(p,count=ct)
        if ct eq 0 then spawn,!os.mkdir + ' ' +p
        fn = dialog_pickfile(filter='*tiff',path=p,file=file,/WRITE, $
                title='Save R-TIFF Image')
        if fn ne '' then begin
        WSET,win
        if !d.n_colors gt !d.table_size then $
        WRITE_TIFF,fn,reverse(TVRD(/true),3) else $
        WRITE_TIFF,fn,reverse(TVRD(),2),1,red=R,green=G,blue=B
        WSET,old_win
        end
END

PRO save_pict,win=win,file=file,path=path
;	file = plot2d_state.class+'.pict'  set the init pict out file
	if keyword_set(win) eq 0 then win=!d.window
	old_win = !d.window
       tvlct,R,G,B,/get
	if keyword_set(path) eq 0 then begin
	cd,current=p
	p = p + !os.file_sep +'PICT' +!os.file_sep
	endif else p=path
	found = findfile(p,count=ct)
	if ct eq 0 then spawn,!os.mkdir + ' ' +p
	fn = dialog_pickfile(filter='*pict',path=p,file=file,/WRITE, $
		title='Save PICT Image')
	if fn ne '' then begin
	if !d.n_colors gt !d.table_size then begin
		WSET,win
		t_arr = TVRD(/true)
		arr = color_quan(t_arr,1,red,green,blue)
		tvlct,red,green,blue
		WRITE_PICT,fn,arr,Red,Green,Blue
		WSET,old_win
		tvlct,R,G,B	
	endif else begin
	WSET,win
	WRITE_PICT,fn,TVRD(),R,G,B
	WSET,old_win
	end
	end
END


PRO save_png,win=win,file=file,path=path
;	file = plot2d_state.class+'.png'  set the init png out file
;
	if keyword_set(win) eq 0 then win=!d.window
        old_win = !d.window
       tvlct,R,G,B,/get
	if keyword_set(path) eq 0 then begin
	cd,current=p
	p = p + !os.file_sep +'PNG'+!os.file_sep
	endif else p=path
	found = findfile(p,count=ct)
	if ct eq 0 then spawn,!os.mkdir + ' ' +p
	fn = dialog_pickfile(filter='*png',path=p,file=file,/WRITE, $
		title='Save PNG Image')
	if fn ne '' then begin
	WSET,win
	if !d.n_colors gt !d.table_size then $
        WRITE_PNG,fn,TVRD(/true) else $
        WRITE_PNG,fn,TVRD(),R,G,B
	WSET,old_win
	end
END

PRO save_jpg,win=win,file=file,path=path
;	file = plot2d_state.class+'.jpg'  set the init jpg out file
;
	if keyword_set(win) eq 0 then win=!d.window
        old_win = !d.window
       tvlct,R,G,B,/get
	if keyword_set(path) eq 0 then begin
	cd,current=p
	p = p + !os.file_sep +'JPG'+!os.file_sep
	endif else p=path
	found = findfile(p,count=ct)
	if ct eq 0 then spawn,!os.mkdir + ' ' +p
	fn = dialog_pickfile(filter='*jpg',path=p,file=file,/WRITE, $
		title='Save JPG Image')
	if fn ne '' then begin
	WSET,win
	if !d.n_colors gt !d.table_size then $
        WRITE_JPEG,fn,TVRD(/true),/true else $
        WRITE_JPEG,fn,TVRD(),red=R,green=G,blue=B
	WSET,old_win
	end
END
