PRO colorbar,yrange,width,height,horizontal=horizontal,x=x,y=y,wid=wid
;    width - colorbar width
;    height - colorbar height
;+
; NAME:
;       COLORBAR
;
; PURPOSE:
;       This routine provides the user with the flexibility of specifying 
;       the size and the place of the colorbar to be drawn on the plot. 
;
; CALLING SEQUENCE:
;
;       COLORBAR, Zrange [,Width] [,Height] [,X=x] [,Y=y] [,Wid=wid]
;
; INPUTS:
;       Zrange: [Zmin,Zmax] specifies the minimum and maximum value of
;               data corresponding to the first and last entry of color 
;               table.
;
; OPTIONAL INPUTS:
;       Width:  specify the width of the color bar, default to 20 pixels
;       Height: specify the height of the color bar, default to 320 pixels
;	
; KEYWORD PARAMETERS:
;       X:      specify X location of the lower left corner of the color bar,
;               default to 70 pixels from right boundary for X device
;       Y:      specify Y location of the lower left corner of the color bar,
;               default to 100 pixels from bottom boundary for X device
;       Wid:    specify the destination window the color bar to be drawn,
;               default to current plot device 
;
; OUTPUTS:
;       None.
;
; RESTRICTION:
;       For PS device, an expansion factor of 20 is used for Width, Height, X,
;       and Y pixels 
;
; EXAMPLE:
;       Example 1 - Place a colorbar at the default location 70,100 pixels from 
;       the lower right corner of the plot area
;
;		colorbar,[min(z),max(z)]	
;
;       Example 2 - Place a colorbar at the location 10,100 pixels from 
;       the lower left corner of the plot area
;
;		colorbar,[min(z),max(z)],X=10,Y=100	
;
; MODIFICATION HISTORY:
;       Written by:     Ben-chin Cha, April 9, 1999
;       xx-xx-xxxx  xxx	Comment
;-

	if n_elements(width) eq 0 then width=20
	if n_elements(height) eq 0 then height=320
	if keyword_set(wid) then wset,wid
	xsize = !d.x_vsize
	ysize = !d.y_vsize
	nc = !d.table_size
	setnc = fix(getenv('IDL_NCOLORS'))
	if setnc gt 0 then nc=setnc	
	ns = 16
	
	fact = 1
	if !d.NAME eq 'PS' then fact=20

	; horizontal colorbar
	if keyword_set(horizontal) then begin
	if n_elements(x) eq 0 then x = 60*fact 
	if n_elements(y) eq 0 then y = 25*fact 
	beh = 20
	bew = (xsize-x*2)/ns
	dval = (yrange(1)-yrange(0))/ns
	for j=0,ns do begin
	color = (nc/ns)*j 
	if color eq nc then color = nc - 1
	tv,replicate(color,bew,beh), x+j*bew, y
	endfor
	for j=0,ns,8 do begin
	x1=x+j*bew
	str = string(format='(G10.5)',j*dval+yrange(0))
		xcor=[x1, x1, x1+bew, x1+bew, x1]
		ycor=[y, y+beh*fact, y+beh*fact, y, y]
		plots,xcor,ycor ,/device
		xyouts,x+j*bew, y-15*fact, strtrim(str,2),/device
	endfor
	return
	end

	; vertical colorbar
	if n_elements(x) eq 0 then x = xsize - 70*fact else x=x*fact
	if n_elements(y) eq 0 then y = 100*fact else y=y*fact
	bew = width;   20
	beh = height/ns  ;(ysize-y*2)/ns
	dval = (yrange(1)-yrange(0))/ns
	for j=0,ns do begin
	color = (nc/ns)*j 
	if color eq nc then color = nc - 1
	tv,replicate(color,bew,beh), x,y+j*beh*fact ,xsize=bew*fact,ysize=beh*fact
	jj = j mod 2
	if jj eq 0 then begin
;	str = string(format='(E10.2)',j*dval)
	str = string(format='(G10.5)',j*dval+yrange(0))
		 xyouts,x+1.25*bew*fact,y+j*beh*fact, strtrim(str,2),/device
		end
	endfor
END	
