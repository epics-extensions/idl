PRO lorentzian_curve,a,x,y,plot=plot
;+
; NAME:
; 	LORENTZIAN_CURVE
;
; PURPOSE:
;       For a given set of lorentzian parameters [a0,a1,a2], this routine
;       calculate the corresponding {y(i)} for a given set of {x(i)}.
;
; CATEGORY:
; 	Fitted data with plot.
;
; CALLING SEQUENCE:
;       LORENTZIAN_CURVE, A, X, Y [,/PLOT]
;
; INPUTS:
;       A:        Fitted lorentzian coefficients, [A0,A1,A2].
;       X:        Position X vector of type float or double. 
;       Y:        Data value Y vector of type float or double. 
;	
; KEYWORD PARAMETERS:
;   PLOT:       Specifies whether to plot the Y vector.
;
; PROCEDURE:
;      Before accessing this routine, the 'ez_fit.pro' must be loaded into
;      IDL and the path to 'ez_fit.pro' must be in the IDL search path.  
;
; EXAMPLE:
;      Run the geometric fitting, and pops up the fitting result window
;
;      X = [ ...]
;      A = [Peak, Mean, FWHM/2] 
;      LORENTZIAN_CURVE,A,X,Y,/PLOT
;
; MODIFICATION HISTORY:
;      Written by:  Ben-chin K. Cha, 10-15-97.
;      xx-xx-xxbkc  comment
;-
N = n_elements(x)
y=make_array(N)
for i=0,N-1 do begin
        lorentzian,x(i),a,f
        y(i)=f
end
if keyword_set(plot) then plot,x,y
END

PRO lorentzfitgraf,x,y,a,print=print,yfit=yfit,GROUP=group
;+
; NAME:
; 	LORENTZFITGRAF
;
; PURPOSE:
;       This routine uses the CURVEFIT function with the non-linear fitting
;       function_name='lorentzian' specified. y=f(x) where:
;         F(X) = A0 * A2^2 / ((X-A1)^2 + A2^2) and
;          A0 = height of exp, A1 = center of exp, A2 = FWHM/2
;
;       Then calls PLOT1D to graph the calculated results with the raw 
;       data in a pop-up window.
;
; CATEGORY:
; 	Curve fitting with plot.
;
; CALLING SEQUENCE:
;
;       LORENTZFITGRAF, X, Y [,A] [,/PRINT] [,YFIT=yfit]
;         
;
; INPUTS:
;
;       X:        Position X vector of type float or double. 
;       Y:        Data value Y vector of type float or double. 
;       A:        Optional input [A0,A1,A2], initial estimates of 
;                 fitting coefficients.
;	
; KEYWORD PARAMETERS:
;   PRINT:       Specifies whether the output window will be poped up.
;   YFIT:        Y vector calculated from the fitted equation.
;
; SIDE EFFECTS:
;      The computed parameters and the convergence may depend on the data and
;      the initial parameters of A vector entered.
;
; RESTRICTIONS:
;      The number of parameters must be three. The initial value should be
;      close to the real data value with:
;          A0 = height of exp, A1 = center of exp, A2 = FWHM/2
;      Especially the center of expectation must corresponds to the peak 
;      of the lorentzian.
;
; PROCEDURE:
;      Before accessing this routine, the 'ez_fit.pro' must be loaded into
;      IDL and the path to 'ez_fit.pro' must be in the IDL search path.  
;
; EXAMPLE:
;      Run the geometric fitting, and pops up the fitting result window
;
;      X = [ ...]
;      Y = [ ...]
;      A = [ Peak, Mean, FWHM/2 ]
;      LORENTZFITGRAF,X,Y,A,/GEOMETRIC,/PRINT
;
; MODIFICATION HISTORY:
;      Written by:  Ben-chin K. Cha, 10-15-97.
;      xx-xx-xxbkc  comment
;-
if n_params() lt 2 then begin
	str = ['Usage: lorentzfitGraf, X, Y [,A] [,/PRINT] [,YFIT=yfit]', '',$
	  	'      Y = A0 * A2^2 / ( (X-A1)^2 + A2^2 )']
	res=widget_message(str,/info,title='FITTING Info')
	return
end

statistic_1d,x,y,c_mass,x_peak,y_peak,y_hpeak,fwhm,fwhm_xl,fwhm_wd
	if n_elements(fwhm_wd) eq 0 then begin
	res = WIDGET_MESSAGE('Data not suitable for lorentzian fit !',/info)
	return
	end
a=[y_peak, x_peak, 0.5*fwhm]

Weights= replicate(1.0,n_elements(x))

if n_elements(a) lt 3 then a=[1.,10.,20.] 

yfit = curvefit(x,y,Weights,a,sigma,function_name='lorentzian',/noderiv)
;yfit = curvefit(x,y,Weights,a,sigma,function_name='lorentzian')

        yres = yfit - y
        goodness = goodness_fit(yres,1)

        curv=make_array(n_elements(y),2)
        curv(0,1) = float(yfit)
        curv(0,0) = float(y)

	title = 'Lorentzian fit'
	comment = 'Y = A0 * A2^2 / ( (X-A1)^2 + A2^2 ) '
	for i=0,n_elements(A)-1 do comment= [comment,'A'+strtrim(i,2)+'='+ $
		strtrim(A(i),2) + '     SIGMA='+strtrim(sigma(i),2)]

        comment=[comment,'','GOODNESS OF FIT = '+string(goodness)]

	plot1d, x, curv,/curvfit,title=title,comment=comment,width=500,/stamp, $
		/symbol,GROUP=group,wtitle='Lorentzian Fit',report='lorentzian.rpt'


	if keyword_set(print) then begin
        OPENW,unit,'lorentzian.rpt',/GET_LUN,ERROR=err
        if err ne 0 then begin
        res = widget_message(!err_string,/info,title='FITTING Info')
        return
        end

	printf,unit,'LORENTZIAN FIT - ',title
	printf,unit,''

        vec = moment(y, mdev=md, sdev=sd)
        mean = vec(0)
        variance = vec(1)
        printf,unit,'        MEAN=',mean
        printf,unit,'        SDEV=',sqrt(variance)
        printf,unit,'    VARIANCE=',variance

        printf,unit,''
        printf,unit,'    Centroid @ X=',c_mass
        printf,unit,'        Y_Peak  =',y_peak
        printf,unit,'          @  X  =',x_peak
        printf,unit,'        Y_Hpeak =',y_hpeak
        printf,unit,'        FWHM    =',fwhm
        printf,unit,'        FWHM_xl =',fwhm_xl
        printf,unit,'        FWHM_wd =',fwhm_wd
        printf,unit,''
	for i=0,n_elements(comment)-1 do printf,unit,comment(i)
        printf,unit,''


	printf,unit,'         X           Y           YFIT        YFIT - Y'
	for i=0,n_elements(x)-1 do printf,unit,x(i),y(i),yfit(i),yres(i)
	FREE_LUN,unit
;	xdisplayfile,'lorentzian.rpt',title=title
	end

END


PRO lorentzian,x,a,f,pder
; function + partials

if n_elements(a) eq 3 then begin
a1 = a(0)
a2= a(1)
a3= a(2)
deno =  (x-a2)^2 + a3^2 
f = a1 * a3^2 /deno
f1 = a3^2 / deno
f2 = 2*a1*a3^2*(x-a2) / deno^2
f3 = 2*a1*a3*(x-a2)^2 / deno^2
if n_params() ge 4 then $
	pdef = [ [f1],[f2],[f3] ]
end

END


PRO multi_lorentzian,x,a,f,pder
; function + partials

N = n_elements(a)/3
if n_elements(a) eq 3*N then begin
	f=0. 
	f1=0.
	f2=0.
	f3=0.
	For i=0,N-1 do begin
	a1 = a(i*3 + 0)
	a2= a(i*3 + 1)
	a3= a(i*3 + 2)
	deno =  (x-a2)^2 + a3^2 
	f = f + a1 * a3^2 /deno
	f1 = f1 + a3^2 / deno
	f2 = f2 + 2*a1*a3^2*(x-a2) / deno^2
	f3 = f3 + 2*a1*a3*(x-a2)^2 / deno^2
	end
if n_params() ge 5 then $
	pdef = [ [f1],[f2],[f3] ]
endif else begin
	res=widget_message('Error: inconsistant parameter numbers',/error)
end

END


PRO multi_lorentzfitgraf,x,y,a,print=print,yfit=yfit,subplot=subplot,GROUP=group
; A0:   Ymax at x=mu
; A1:   MU 
; A2:   FWHM / 2.
; A=[[A0,A1,A2],[A0(1),A1(1),A2(1)], ...]
;+
; NAME:
; 	MULTI_LORENTZFITGRAF
;
; PURPOSE:
;       This routine uses the CURVEFIT function to fit a set of multiple 
;       LORENTZIAN functions y=f(x) where:
;         F(X) = A0 * A2^2 / ((X-A1)^2 + A2^2) + ... and
;          A0 = height of exp, A1 = center of exp, A2 = FWHM/2
;
;       Then calls PLOT1D to graph the calculated results with the raw 
;       data in a pop-up window.
;
; CATEGORY:
; 	Curve fitting with plot.
;
; CALLING SEQUENCE:
;
;       MULTI_LORENTZFITGRAF, X, Y, A [,YFIT=yfit] [,/PRINT] [,/SUBPLOT] 
;         
;
; INPUTS:
;
;       X:        Position X vector of type float or double. 
;       Y:        Data value Y vector of type float or double. 
;       A:        Lorentzian coefficients vector. It consists of a set of 
;                 multiple of 3 parameters for each lorentzian.  
;                 [ [A0,A1,A2], [A0,A1,A2], ... ] 
;	
; KEYWORD PARAMETERS:
;   YFIT:        Y vector calculated from the fitted lorentzian equations.
;   PRINT:       Specifies whether the output window will be poped up.
;   SUBPLOT:     If specified, a plot window shows the composition of all
;                the multiple lorentzian curves.
;
; SIDE EFFECTS:
;      The computed parameters and the convergence may depend on the data and
;      the initial parameters of A vector entered.
;
; RESTRICTIONS:
;      The number of parameters must be multiple of 3. The initial value 
;      should be close to the real data value with:
;          A0 = height of exp, A1 = center of exp, A2 = FWHM/2
;      Especially the center of expectation must corresponds to each local
;      peak of the lorentzian.
;
; PROCEDURE:
;      Before accessing this routine, the 'ez_fit.pro' must be loaded into
;      IDL and the path to 'ez_fit.pro' must be in the IDL search path.  
;
; EXAMPLE:
;      Run the geometric fitting, and pops up the fitting result window
;
;      X = [ ...]
;      Y = [ ...]
;      A = [ Peak1, Mean1, FWHM1/2, Peak2, Mean2, FWHM2/2, ... ]
;      MULTI_LORENTZFITGRAF,X,Y,A,/GEOMETRIC,/PRINT
;
; MODIFICATION HISTORY:
;      Written by:  Ben-chin K. Cha, 10-15-97.
;      xx-xx-xxbkc  comment
;-
if n_params() lt 3 then begin
	str = ['Usage: multi_lorentzfitGraf, X, Y ,A, /PRINT', '',$
	  	'      Y = A0 * A2^2 / ( (X-A1)^2 + A2^2 ) + ...']
	res=widget_message(str,/info,title='FITTING Info')
	return
end

N = n_elements(A) / 3
NEWA = make_array(3,N)
if n_elements(A) ne 3*N then begin
	res=widget_message('Error: number of coefficients in A is not right.',/error)
	return
end

Weights= replicate(1.0,n_elements(x))

yfit = curvefit(x,y,Weights,a,sigma,function_name='multi_lorentzian',/noderiv)

	yres = yfit - y
	goodness = goodness_fit(yres,N)

for i=0,N-1 do NEWA(*,i) = a(i*3:i*3+2)

        curv=make_array(n_elements(y),2)
        curv(0,1) = float(yfit)
        curv(0,0) = float(y)

	title = 'Multiple Lorentzian fit'
	comment = 'Y = A0 * A2^2 / ( (X-A1)^2 + A2^2 ) + ... '
	for i=0,N-1 do begin
	comment=[comment,string(i)+string(NEWA(0,i)) $
		+ string(NEWA(1,i)) + string(NEWA(2,i))]
	end

	comment=[comment,'','GOODNESS OF FIT = '+string(goodness)]

; plot seperate lines
if keyword_set(subplot) then begin
ytemp = make_array(n_elements(x),N+2)
ytemp(*,1) = yfit
ytemp(*,0) = y
for i=0,N-1 do begin
	a = NEWA(*,i)	
	lorentzian_curve,a,x,temp
	ytemp(*,i+2) = temp
end
	plot1d,x,ytemp,/symbol,title=title,GROUP=group,wtitle='Decomposed Lorentzians'
end


	plot1d, x, curv,/curvfit,title=title,comment=comment,width=500,/stamp, $
		/symbol,GROUP=group,wtitle='Multiple Lorentzian Fit',report='lorentzian.rpt'


	if keyword_set(print) then begin
        OPENW,unit,'lorentzian.rpt',/GET_LUN,ERROR=err
        if err ne 0 then begin
        res = widget_message(!err_string,/info,title='FITTING Info')
        return
        end

	printf,unit,'MULTIPLE LORENTZIAN FIT - ',title
	printf,unit,''

        vec = moment(y, mdev=md, sdev=sd)
        mean = vec(0)
        variance = vec(1)
        printf,unit,'        MEAN=',mean
        printf,unit,'        SDEV=',sqrt(variance)
        printf,unit,'    VARIANCE=',variance
        printf,unit,''

	for i=0,n_elements(comment)-1 do printf,unit,comment(i)
        printf,unit,''

	for i=0,N-1 do begin
        printf,unit,'    	i    =',i 
        printf,unit,'        Ymax(i) =',newa(0,i)
        printf,unit,'         MU(i)  =',newa(1,i)
        printf,unit,'        FWHM(i) =',newa(2,i)*2.
        printf,unit,''
	end

	format='('+strtrim(N+4,2)+'G15.7)'
	format='(4G15.7,'+strtrim(N,2)+'G15.7)'
	printf,unit,'         X            Y              YFIT          YFIT - Y  ||    MULTIPLE COMPONENTS'
	for i=0,n_elements(x)-1 do begin
		 printf,unit,format=format,x(i),y(i),yfit(i),yres(i),ytemp(i,2:2+N-1)
	end
	FREE_LUN,unit
;	xdisplayfile,'lorentzian.rpt',title=title
	end

END

FUNCTION goodness_fit,yres,N,Weight=Weight
;  sqrt(( W * YRES) ^2 / (M-N))

	M = n_elements(yres)
	if M gt N then begin
	goodness = 0.
	if keyword_set(Weight) then $
	for i=0,M-1 do goodness = goodness + ( Weight(i)*yres(i))^2 else $
	for i=0,M-1 do goodness = goodness + yres(i) ^ 2 
	goodness = sqrt(goodness /( M - N))
	return,goodness
	end
END
