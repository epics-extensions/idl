

PRO findFactor,inString,outString,factor,print=print,operator=operator
; separate the array and the factor 

x = '1,2,3,10*4,5,6'
sep = ','
if n_elements(inString) then x = inString
if keyword_set(operator) then sep = operator

	y = str_sep(x,sep)

	nl = n_elements(y) 
	factor = make_array(nl,value=1.)
	z = strpos(y,"*")

	for i=0,n_elements(z)-1 do begin
	if z(i) gt 0 then begin
		p = str_sep(y(i),"*")
		factor(i) = fix(p[0])
		y(i) = p[1]
	end
	end

if keyword_set(print) then print,factor,y
	outString = y

END

PRO findWFactor,inString,WFactor,separator=separator

x = '0.1, 3*.5, 5*1., 3*0.5,.1'
if n_elements(inString) then x = inString
sep = ','
if keyword_set(separator) then sep = separator

	y = str_sep(x,sep)
	z = strpos(y,'*')

	no = n_elements(z)

	for i=0,no-1 do begin
	if z(i) eq -1 then begin
		ff = y(i)
		if n_elements(factor) eq 0 then factor = float(y(i)) else $
			factor = [factor,float(y(i))]
	end
	if z(i) gt 0 then begin
		p = str_sep(y(i),'*')
		dnl = fix(p(0))
		for j =0,dnl-1 do begin
		if n_elements(factor) eq 0 then factor = float(p[1]) else $
			factor = [factor,float(p(1))]
		end
	end
	end
	Wfactor = factor
END

FUNCTION slope,X,Y
	nx = n_elements(x)
	if n_params() eq 1 then begin
		y = x
		X = indgen(nx)
	end
	slopey = y*0
	for i=1,nx-1 do begin
		dx = x(i) - x(i-1)
		if dx ne 0. then begin
		slopey(i)= (y(i)-y(i-1))/dx
;		print,i,x(i),y(i),slopey(i)
		end
	end
	return,slopey
END

;
; find  fwh_max, c_mass, peak for a given x,y array
;
PRO statistic_1d,x,y,c_mass,x_peak,y_peak,y_hpeak, fwhm,x_hwdl,x_hwdr, $
	FIT=FIT,XINDEX=XINDEX,LIST=LIST,PLOT=PLOT,TITLE=TITLE
;
; FIT  -  based on data points or use fit data
;
xindex = keyword_set(XINDEX)
list = keyword_set(LIST)

nx = n_elements(x)
a=make_array(nx,/float)
da=make_array(nx,/float)
ny=make_array(nx,/float)

slopey = slope(x,y)

ymin = min(y)
ymax = max(y)
ny = y - ymin

peak = ymax
hpeak = 0.5 * max(ny)
y_hpeak= hpeak + ymin

; area = int_tabulated(x,ny)
; harea = 0.5 * area

if list then print,'I             X           Y          deltaA          A         Slope     Y-Ymin'
d0=0
for i=1,nx-1 do begin
	dx = x(i) - x(i-1)
	if dx ne 0. then begin
	da(i) = 0.5 *(ny(i)+ny(i-1)) * dx
	d0 = d0 + da(i)
	a(i) = d0
	if list then print,strtrim(i,1),x(i),y(i),da(i),a(i),slopey(i),ny(i)
	end
end

area = d0
harea = 0.5 * area

; Find c_mass

newtons_method,x,a,harea,c_mass
if list then print,'===='
if list then print,'h_area,C_mass',harea,c_mass
if list then print,'peak,hpeak,y_hpeak',peak,hpeak,y_hpeak


; Find half peaks

if list then print,'===='
nohwdl=0
nohwdr=0
x_hwdl=0
x_hwdr=0

for i=1,nx-1 do begin
	yl = ny(i-1) - hpeak
	yr = ny(i) - hpeak
;	print,i-1,y(i-1),yl,yr
	if yl*yr lt 0. then begin
           if yl lt 0. then begin
		nohwdl = [nohwdl, i-1]
		newtons_method,[x(i-1),x(i)],[yl,yr],0.,x_sol,notfound
		x_hwdl= [x_hwdl,x_sol]
		end
           if yl gt 0. then begin
		nohwdr = [nohwdr, i-1]
		newtons_method,[x(i-1),x(i)],[yl,yr],0.,x_sol,notfound
		x_hwdr= [x_hwdr,x_sol]
		end
	endif else if yl*yr eq 0. then begin
		if yl eq 0. and yl ne yr then begin
		 nohwdl = [nohwdl, i-1]
		 x_hwdl = [x_hwdl,x(i-1)]
		end
		if yr eq 0. and yl ne yr then begin
		 nohwdr = [nohwdr, i]
		 x_hwdr = [x_hwdr,x(i)]
		end
	end
end
if list then $
 print,'nohwdl',nohwdl, x_hwdl
if list then  $
print,'nohwdr',nohwdr, x_hwdr
	lo=0
	FWHM = 0.
	xfwhm = 0.
if n_elements(nohwdl) gt 1 then begin 
	x_hwd = x_hwdl(1:n_elements(nohwdl)-1)
	nohw = n_elements(x_hwd)
if n_elements(nohwdr) gt 1 then begin
	x_hwde = x_hwdr(1:n_elements(nohwdr)-1)
	nohwe = n_elements(x_hwde)
;	xfwhm = make_array(nohw,/float)
	for i=0,nohw-1 do begin
		x1 = x_hwd(i)
	for j=0,nohwe-1 do begin
		if x_hwde(j) gt x1 then begin
		dxfwhm = x_hwde(j) - x_hwd(i)
		lo=lo+1
		if lo eq 1 then xfwhm = dxfwhm else $
		xfwhm = [xfwhm,dxfwhm]
		if list then print,'FWHM',lo,xfwhm(i-1)
		goto,outer
		end
	end
	outer:
	end
	end
	FWHM = max(xfwhm,imax)
end

;if n_elements(nohwdr) gt 1 then begin
;	if n_elements(x_hwd) gt 0 then $
;	x_hwd = [x_hwd, x_hwdr(1:n_elements(nohwdr)-1)] else $
;	x_hwd = [x_hwdr(1:n_elements(nohwdr)-1)]
;	end
if n_elements(x_hwd) gt 0 then begin
	x_HPeak = x_hwd(imax) 
	x_hwdl = x_hwd
	if n_elements(x_hwde) then x_hwdr = x_hwde
	if list then print,'0.5*(ymax-ymin)',hpeak
	if list then print,'x_hpeak:',x_hpeak
	if list then print,'y_hpeak:',y_hpeak

	; plot if view specified

	if keyword_set(plot) then begin
	ya= make_array(nx,2)
	ya(0,0)=y(*)
	ya(0,1) = y(*)*0 + y_hpeak
	comment=[ 'FWHM='+strtrim(FWHM,2) +',  Cntro='+strtrim(c_mass,2), $
		'yhpeak='+strtrim(y_hpeak,2) + '  xhpeak='+strtrim(x_hpeak,2)]
	nef = n_elements(xfwhm)
	nel = n_elements(x_hwde)
	for i=0,n_elements(x_hwd)-1 do begin
		st =' xl='+strtrim(x_hwd(i),2) 
		if nel gt 0 and i lt nel then $
		  st = st +'   xr='+strtrim(x_hwde(i),2)
		if nef gt 0 and i lt nef then $
		  st = st +'   fwhm='+strtrim(xfwhm(i),2)
		comment=[comment,st] 
	end

	if n_elements(comment) gt 10 then comment=[comment(0:8),'. . .']
	if keyword_set(title) then $
	plot1d,x,ya,comment=comment,title=title else $
	plot1d,x,ya,comment=comment
	end
end

; Find peaks

if keyword_set(FIT) then begin
nopeaks=0
if list then print,'===='
for i=1,nx-1 do begin
       if slopey(i-1) gt 0 and slopey(i-1)*slopey(i) lt 0. then begin
;		print,i,slopey(i-1),slopey(i)
		nopeaks = [nopeaks, i]
		end
end
;print,'nopeaks',nopeaks
no = n_elements(nopeaks)-1
if no gt 0 then begin
x_peak = make_array(no,/float)
y_peak = make_array(no,/float)
for i=1,no do begin
	i2= nopeaks(i)
	i1= i2-1
	newtons_method,[x(i1),x(i2)],[slopey(i1),slopey(i2)],0.,x_sol,notfound
	if notfound eq 0 then begin
if list then 	print,'Peak #',i,x_sol,y(i1)
		x_peak(i-1)= x_sol
		y_peak(i-1) = y(i1)
		end
end
endif else begin
	y_peak = ymax
	if y(0) gt y(nx-1) then x_peak = x(0) else x_peak = x(nx-1)
if list then 	print,'Ymax at pt ',y_peak,x_peak
end
endif else begin

	for i=0,nx -1 do begin
		if y(i) eq peak then begin
		x_peak = x(i)
		y_peak = peak
		return
		end
	end
end


END


PRO newtons_method,x,y,y_sol,x_sol,notfound
notfound = 0
nx = n_elements(y)
n1 = 0 
n2 = nx-1 
RETEST:
;print,'N1,N2',n1,n2,y(n1),y(n2)
if (n2-n1) le 1 then begin
	if (y_sol - y(n2)) * (y_sol - y(n1)) gt 0 then begin
		x_sol= x(n1)
		notfound = 1
		return
		end
	if (x(n2)-x(n1)) eq 0. then begin
		x_sol = x(n1)
		return
	end
	x_sol = x(n1)+ (y_sol - y(n1)) /(y(n2)-y(n1)) *(x(n2)-x(n1))
	 return
	end
 
nm = (n2-n1)/ 2 + n1
fm = y (nm)
;print,nm,fm,y_sol
if abs(fm-y_sol) le 1.e-5 then begin
	x_sol = x(nm)
;	print,'Stop at NM,x_sol',nm,x_sol
	return
endif else begin
	if (fm-y_sol) *(y(n2) - y_sol) gt 0 then begin
		n2 = nm
	endif else  begin
		n1 = nm
	end
	goto,RETEST
	end
END

;
; using index and factor instead of real value for x array
;
PRO newtons_method_norm,x,y,y_sol,n1,x_sol,notfound
	rx = float(x)
	newtons_method,rx,y,y_sol,x_sol,notfound
	n1 = fix(x_sol)
	x_sol = x_sol-float(n1)
END


