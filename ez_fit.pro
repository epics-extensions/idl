;@plot1d.pro

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


PRO newSegment,x,y,begin_no,end_no,newX,newY,newW,gaussian=gaussian,poisson=poisson, whole=whole

	if n_params() lt 5 then begin
	print,'Usage: newSegment,X,Y,begin_no,end_no,newX,newY,newW'
	print,''
	print,'This routine extracts a continuous segment from the data array'
	print,''
	return 
	end

	width = n_elements(x)
	if begin_no ge end_no then begin
		print,'Error: start_no >= end_no'
		return
	end

	if n_elements(y) eq width and width ge end_no then begin
		newX = x(begin_no:end_no)
		newY = y(begin_no:end_no)

	newW = replicate(1., end_no-begin_no+1)
	if keyword_set(gaussian) then begin
		sdev = 0.05*newY 
		newW=1.0 / sdev ; Gaussian 
		end
	if keyword_set(poisson) then newW=1.0 / newY       ; Poisson 
	end

	if keyword_set(whole) then begin 
	tempY=make_array(width, value=MIN(newY))
	tempY(begin_no:end_no) = newY
	newY = tempY
	tempW=make_array(width)
	tempW(begin_no:end_no) = newW
	newW = tempW
	newX = X
	end
END

PRO ComfitGraf,x,y,a,yfit,sigma,print=print,test=test,geometric=geometric, $
	exponential=exponential, gompertz=gompertz, hyperbolic=hyperbolic, $
	logistic=logistic, logsquare=logsquare, GROUP=group
;+
; NAME:
; 	COMFITGRAF
;
; PURPOSE:
;       This routine packages the IDL gradient-expansion least
;       square fit COMFIT function to fit the paired data {x(i), y(i)}.
;       Then calls PLOT1D to graph the calculated results with the raw 
;       data in a pop-up window.
;
; CATEGORY:
; 	Curve fitting with plot.
;
; CALLING SEQUENCE:
;
;       COMFITGRAF, X, Y [,A] [,YFIT] [,SIGMA] ,/FIT_TYPE [,/PRINT] [,/TEST]
;         
;
; INPUTS:
;
;       X:        Position X vector of type integer, float or double. 
;
;       Y:        Data value Y vector of type integer, float or double. 
;
;       A:        Optional input, initial estimates of fitting coefficients.
;                 Number of elements in A depends on the FIT_TYPE specified.
;                 If A given, the number of elements in A must be consistant 
;                 with the FIT_TYPE entered.
;	
; KEYWORD PARAMETERS:
;
;   FIT_TYPE:    Six type of COMFIT, it can be any of following 
;
;                EXPONENTIAL Y = a0  * a1^x + a2 
;
;                GEOMETRIC Y = a0 * x^a1 + a2 
;
;                GOMPERTZ Y = a0 * a1^(a2*x) + a3 
;
;                HYPERBOLIC  Y = 1./(a0 + a1*x) 
;
;                LOGISTIC Y = 1./(a0 * a1^x + a2) 
;
;                LOGSQUARE Y = a0 + a1*alog10(x) + a2 * alog10(x)^2 
;
;   PRINT:       Specifies whether the output window will be poped up.
;
;   TEST:        Specifies whether the default test data will be used.
;
; OPTIONAL OUTPUTS:
;
;   YFIT:        Y vector calculated from the fitted equation.
;
;   SIGMA:       Standard deviation for the parameters in A.
;
; SIDE EFFECTS:
;
;      The computed parameters and the convergence may depend on the data and
;      the initial parameters of A vector entered.
;
; RESTRICTIONS:
;      The number of parameters must match exactly to the FIT_TYPE specified.
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
;      A = [ 0.5, 0.5, 0.5]
;      COMFITGRAF,X,Y,A,/GEOMETRIC,/PRINT
;
; MODIFICATION HISTORY:
;      Written by:  Ben-chin K. Cha, 08-13-97.
;      xx-xx-xxbkc  comment
;-

if keyword_set(test) then begin
x = [2.27, 15.01, 34.74, 36.01, 43.65, 50.02, 53.84, 58.30, 62.12, $
 64.66, 71.66, 79.94, 85.67, 114.95]

y = [5.16, 22.63, 34.36, 34.92, 37.98, 40.22, 41.46, 42.81, 43.91, $
 44.62, 46.44, 48.43, 49.70, 55.31]
	 yfit = make_array(n_elements(x)) 
end
;  newA = comfit(x, y, a, sigma = sigma, yfit = yfit, _Extra=extra)

if n_elements(a) eq 0 then begin
 if keyword_set(hyperbolic) then a=[0.5,0.5] else a=[0.5,0.5,0.5]
 if keyword_set(gompertz) then a =[a,0.5]
	N = n_elements(A)
end

title = 'GRADIENT-EXPANSION LEAST-SQUARE FIT'
if keyword_set(exponential) then begin
	title = title + ' ( EXPONENTIAL ) ' 
	comment = 'Y = a0 * a1^x + a2 '
	newA = comfit(x, y, a, sigma = sigma, yfit = yfit, /exponential)
end

if keyword_set(gompertz) then begin
	title = title + ' ( GOMPERTZ ) '
	comment = 'Y = a0 * a1^( a2 * x)  + a3 '
	newA = comfit(x, y, a, sigma = sigma, yfit = yfit, /gompertz)
end

if keyword_set(geometric) then begin
	title = title + ' ( GEOMETRIC )'
	comment = 'Y = a0 * x^a1 + a2 '
	newA = comfit(x, y, a, sigma = sigma, yfit = yfit, /geometric)
end

if keyword_set(hyperbolic) then begin
	title = title + ' ( HYPERBOLIC ) '
	comment = 'Y = 1. / ( a0 + a1 * x )'
	newA = comfit(x, y, a, sigma = sigma, yfit = yfit, /hyperbolic)
end

if keyword_set(logistic) then begin
	title = title + ' ( LOGISTIC ) '
	comment = 'Y = 1. / ( a0 * a1^x + a2 )'
	newA = comfit(x, y, a, sigma = sigma, yfit = yfit, /logistic)
end

if keyword_set(logsquare) then begin
	title = title + ' ( LOGSQUARE ) '
	comment = 'Y =  a0 + a1 * alog10(x) + a2 * alog10(x)^2 '
	newA = comfit(x, y, a, sigma = sigma, yfit = yfit, /logsquare)
end

if n_elements(yfit) eq 0 then begin
	str=['Usage:  comfitGraf, X, Y [,A] [,YFIT] [,SIGMA] ,/FIT_TYPE [,/PRINT] [,/TEST]', $
	'', $
	'   FIT_TYPE  -  EXPONENTIAL       Y = a0  * a1^x + a2',$
	'', $
	'                GEOMETRIC         Y = a0 * x^a1 + a2',$
	'', $
	'                GOMPERTZ          Y = a0 * a1^(a2*x) + a3',$
	'', $
	'                HYPERBOLIC        Y = 1./(a0 + a1*x)',$
	'', $
	'                LOGISTIC          Y = 1./(a0 * a1^x + a2)',$
	'', $
	'                LOGSQUARE         Y = a0 + a1*alog10(x) + a2 * alog10(x)^2',$
	'','If A is entered, then the number of elements in A must be ', $
	'consistant with the FIT_TYPE entered.' $
	]
	res=widget_message(str,/info,title='FITTING Info')
	return
end

N=n_elements(newA)
if n_elements(newA) eq 2 then begin
	comment = [comment, $ 
		'a0 = '+ string(newA(0)) + ',  sigma = '+strtrim(sigma(0),2),$
		'a1 = ' + string(newA(1)) + ',  sigma = '+strtrim(sigma(1),2) ]
end
if n_elements(newA) eq 3 then begin
	comment = [comment, $ 
		'a0 = '+ string(newA(0)) + ',  sigma = '+strtrim(sigma(0),2),$
		'a1 = ' + string(newA(1)) + ',  sigma = '+strtrim(sigma(1),2),$
		'a2 = ' + string(newA(2)) + ',  sigma = '+strtrim(sigma(2),2) ]
end
if n_elements(newA) eq 4 then begin
	comment = [comment, $ 
		'a0 = '+ string(newA(0)) + ',  sigma = '+strtrim(sigma(0),2),$
		'a1 = ' + string(newA(1)) + ',  sigma = '+strtrim(sigma(1),2),$
		'a2 = ' + string(newA(2)) + ',  sigma = '+strtrim(sigma(2),2),$
		'a3 = ' + string(newA(3)) + ',  sigma = '+strtrim(sigma(3),2) ]
end

if n_elements(y) le 1 then begin
	str=['','Usage: comfitGraf,X,Y,A,/geometric','', $
		'e.g. use default test data','', $
		' comfitGraf,/geometric,/test']
	res = widget_message(str,/info,title='FITTING Info')
	return
end
        yres = yfit - y
        goodness = goodness_fit(yres,N)
	comment = [comment,'','GOODNESS OF FIT = '+string(goodness)]

	curv=make_array(n_elements(y),2)
	curv(0,1) = float(yfit)
	curv(0,0) = float(y)

;	plot,x,yfit
;	oplot,x,y,PSYM=7
	plot1d,x,curv,/curvfit,title=title,comment=comment,width=500,/stamp, $
		/symbol,Group=group,wtitle='COMFIT',report='fitting.rpt'

	if keyword_set(print) then begin

        OPENW,unit,'fitting.rpt',/GET_LUN,ERROR=err
        if err ne 0 then begin
        res = widget_message(!err_string,/info,title='FITTING Info')
        return
        end

	printf,unit,''
	printf,unit,title
	printf,unit,''
	for i=0,n_elements(comment)-1 do printf,unit,comment(i)
	printf,unit,''

	printf,unit,'       X         Y          YFIT       YFIT-Y '
	for i=0,n_elements(x) - 1 do  printf,unit,x(i),y(i),yfit(i),yres(i)
	FREE_LUN,unit
; 	xdisplayfile,'fitting.rpt',title=title
	end

END


PRO ladfitgraf,x,y,yfit,absdev=absdev,test=test,double=double,print=print,GROUP=group
;+
; NAME:
; 	LADFITGRAF
;
; PURPOSE:
;       This routine uses the IDL LADFIT function to fit the paired data 
;       {x(i), y(i)} with the linear model Y = A + Bx. 
;       Then calls PLOT1D to graph the calculated results with the raw 
;       data in a pop-up window.
;
; CATEGORY:
; 	Curve fitting with plot.
;
; CALLING SEQUENCE:
;
;       LADFITGRAF, X, Y [,YFIT] [,ABSDEV=absdev] [,/DOUBLE] [,/PRINT] [,/TEST]
;         
;
; INPUTS:
;       X:        Position X vector 
;       Y:        Data value Y vector
;	
; KEYWORD PARAMETERS:
;  ABSDEV:       Specifies whether the mean absolute deviation to be returned.
;   PRINT:       Specifies whether the output window will be poped up.
;   TEST:        Specifies whether the default test data will be used.
;  DOUBLE:       If set to a non-zero value, computations are done in double 
;                precision arithmetic.
;
; OPTIONAL OUTPUTS:
;   YFIT:        Y vector calculated from the fitted equation.
;
; PROCEDURE:
;      Before accessing this routine, the 'ez_fit.pro' must be loaded into
;      IDL and the path to 'ez_fit.pro' must be in the IDL search path.  
;
; EXAMPLE:
;
;      X = [ ...]
;      Y = [ ...]
;      LADFITGRAF,X,Y,/PRINT
;
; MODIFICATION HISTORY:
;      Written by:  Ben-chin K. Cha, 08-13-97.
;      xx-xx-xxbkc  comment
;-

if keyword_set(test) then begin

         x = [-3.20, 4.49, -1.66, 0.64, -2.43, -0.89, -0.12, 1.41, $
               2.95, 2.18,  3.72, 5.26]
         y = [-7.14, -1.30, -4.26, -1.90, -6.19, -3.98, -2.87, -1.66, $
              -0.78, -2.61,  0.31,  1.74]
endif else begin
	if n_params() eq 0 then begin
	str='Usage: ladfitGraf,X,Y [,YFIT] [,ABSDEV=absdev] [,/DOUBLE] [,/PRINT] [,/TEST]'
	str=[str,'', '        Y = A0 + A1 * X   ','', $
		'Linear fit - Least Absolute Deviation Method']
	res=widget_message(str,/info,title='FITTING Info')
	return
	end
end
if keyword_set(double) then begin
	x=double(x)
	y=double(y)
end
       A = ladfit(x, y, absdev = absdev)
	yfit = A(0) + A(1) * x

	curv=make_array(n_elements(y),2)
	curv(0,1) = float(yfit)
	curv(0,0) = float(y)

	title = 'Linear Fit -  Least Absolute Deviation Method' 
	comment = 'Y = A0 + A1 * X'
	for i=0,1 do comment=[comment,'A'+strtrim(i,2)+'='+ $
		string(A(i)) ]
	comment=[comment,'','ABS_DEVIATION=' + string(absdev)]

        yres = yfit - y
        goodness = goodness_fit(yres,n_elements(A))
	comment=[comment,'','GOODNESS OF FIT = '+string(goodness)]

	plot1d,x,curv,/curvfit,title=title,comment=comment,width=500,/stamp, $
		/symbol,Group=group,wtitle='LADFIT',report='fitting.rpt'

	if keyword_set(print) then begin
        OPENW,unit,'fitting.rpt',/GET_LUN,ERROR=err
        if err ne 0 then begin
        res = widget_message(!err_string,/info,title='FITTING Info')
        return
        end

	printf,unit,'LADFIT - ',title
	printf,unit,''
	for i=0,n_elements(comment)-1 do printf,unit,comment(i)
	printf,unit,''

	printf,unit,'       X             Y          YFIT       YFIT-Y'
	for i=0,n_elements(x)-1 do printf,unit,x(i),y(i),yfit(i),yres(i)
	FREE_LUN,unit
;	xdisplayfile,'fitting.rpt',title=title
	end
END

PRO linfitgraf,x,y,sdev=sdev,chisq=chisq,prob=prob,sigma=sigma,double=double,test=test,print=print,GROUP=group
;+
; NAME:
; 	LINFITGRAF
;
; PURPOSE:
;       This routine uses the IDL LINFIT function to fit the paired data 
;       {x(i), y(i)} with the linear model Y = A + Bx.  It minimize the
;       chi-square error statistic.
;       Then calls PLOT1D to graph the calculated results with the raw 
;       data in a pop-up window.
;
; CATEGORY:
; 	Curve fitting with plot.
;
; CALLING SEQUENCE:
;
;       LINFITGRAF, X, Y [,YFIT] [,CHISQ=chisq] [,PROG=prob] [,SDEV=sdev] 
;                  [,SIGMA=sigma] [,/DOUBLE] [,/PRINT] [,/TEST]
;         
;
; INPUTS:
;       X:        Position X vector 
;       Y:        Data value Y vector
;	
; KEYWORD PARAMETERS:
;   CHISQ:    Use this keyword to specify a named variable which returns the
;             chi-square error statistic as the sum of squared errors between
;             Y(i) and A + BX(i). If individual standard deviations are 
;             supplied, then the chi-square error statistic is computed as
;             the sum of squared errors divided by the standard deviations.
;    PROB:    Use this keyword to specify a named variable which returns the
;             probability that the computed fit would have a value of CHISQR 
;             or greater. If PROB is greater than 0.1, the model parameters 
;             are "believable". If PROB is less than 0.1, the accuracy of the
;             model parameters is questionable.
;    SDEV:    An n-element vector of type integer, float or double that 
;             specifies the individual standard deviations for {X(i), Y(i)}.
;   SIGMA:    Use this keyword to specify a named variable which returns a 
;             two-element vector of probable uncertainties for the model par-
;             ameters, [SIG_A,SIG_B].
;   PRINT:    Specifies whether the output window will be poped up.
;   TEST:     Specifies whether the default test data will be used.
;  DOUBLE:    If set to a non-zero value, computations are done in double 
;             precision arithmetic.
;
; OPTIONAL OUTPUTS:
;   YFIT:        Y vector calculated from the fitted equation.
;
; PROCEDURE:
;      Before accessing this routine, the 'ez_fit.pro' must be loaded into
;      IDL and the path to 'ez_fit.pro' must be in the IDL search path.  
;
; EXAMPLE:
;
;      X = [ ...]
;      Y = [ ...]
;      LINFITGRAF,X,Y,sigma=sigma,chisq=chisq,prob=prob,/PRINT
;
; MODIFICATION HISTORY:
;      Written by:  Ben-chin K. Cha, 08-13-97.
;      xx-xx-xxbkc  comment
;-

if keyword_set(test) then begin

         x = [-3.20, 4.49, -1.66, 0.64, -2.43, -0.89, -0.12, 1.41, $
               2.95, 2.18,  3.72, 5.26]
         y = [-7.14, -1.30, -4.26, -1.90, -6.19, -3.98, -2.87, -1.66, $
              -0.78, -2.61,  0.31,  1.74]
	 sdev = replicate(0.85, n_elements(x))
endif else begin
	if n_params() eq 0 then begin
		str='Usage: linfitGraf,X,Y [,YFIT] [,CHISQ=chisq] [,PROB=prob] [,SDEV=sdev] '
		str=[str, '           [,SIGMA=sigma] [,/DOUBLE] [,/PRINT] [,/TEST]']
		str=[str,'','    Y = A0 + A1 * X   ',  '', $
			'Linear fit by Minimize the Chi-Square Error']
		res=widget_message(str,/info,title='FITTING Info')
		return
	end
	if keyword_set(sdev) then begin
		sdev = float(sdev)*y
	endif else begin
 	vec = moment(y, mdev=md, sdev=sd)
	mean = vec(0)
	variance = vec(1)
	skew = vec(2)
	kurtosis = vec(3)
	sdev = replicate(sd, n_elements(x))
	end
end

	if strpos(!version.release,'4.') lt 0 then $
        A = linfit(x, y, sdev=sdev, chisq=chisq, prob=prob, sigma=sigma) else $
        A = linfit(x, y, sdev=sdev, chisq=chisq, prob=prob)
	yfit = A(0) + A(1) * x

	curv=make_array(n_elements(y),2)
	curv(0,1) = float(yfit)
	curv(0,0) = float(y)

	title = 'Linear Fit by Minimize the Chi-Square Error' 
	comment = 'Y = A0 + A1 * X'
	if strpos(!version.release,'4.') lt 0  then begin
	for i=0,1 do comment=[comment,'A'+strtrim(i,2)+'='+ $
		string(A(i)) + '     SIGMA='+string(sigma(i))]
	endif else begin
	for i=0,1 do comment=[comment,'A'+strtrim(i,2)+'='+ $
		string(A(i))]
	end

        yres = yfit - y
        goodness = goodness_fit(yres,n_elements(A))
	comment=[comment,'','GOODNESS OF FIT = '+string(goodness)]

;	plot,x,yfit
;	oplot,x,y,PSYM=7
	plot1d,x,curv,/curvfit,title=title,comment=comment,width=500,/stamp, $
		/symbol,Group=group,wtitle='LINFIT - minimize chi-square',report='fitting.rpt'


	if keyword_set(print) then begin
        OPENW,unit,'fitting.rpt',/GET_LUN,ERROR=err
        if err ne 0 then begin
        res = widget_message(!err_string,/info,title='FITTING Info')
        return
        end

	printf,unit,'LINFIT - ',title
	printf,unit,''
	printf,unit,'        MEAN=',mean
	printf,unit,'        SDEV=',sqrt(variance)
	printf,unit,'    VARIANCE=',variance
	printf,unit,''
	for i=0,n_elements(comment)-1 do printf,unit,comment(i)
	printf,unit,''
	printf,unit,'       CHISQ=',chisq
	printf,unit,'        PROB=',prob
	printf,unit,''

	printf,unit,'        X            Y            YFIT     YFIT-Y     SDEV'
	for i=0,n_elements(x)-1 do printf,unit,x(i),y(i),yfit(i),yres(i),sdev(i)
	printf,unit,''
	printf,unit,'  PROB =',prob
	printf,unit,' CHISQ =',chisq, '
	FREE_LUN,unit
;	xdisplayfile,'fitting.rpt',title=title
	end
END

PRO polyfitwgraf,x,y,w,ndegree,A,yfit,yband,sigma,print=print,GROUP=group
;+
; NAME:
; 	POLYFITWGRAF
;
; PURPOSE:
;       This routine uses the IDL least square polynomial fit function
;       PLOYFITW with optional error estimates. 
;       Then calls PLOT1D to graph the calculated results with the raw 
;       data in a pop-up window.
;
; CATEGORY:
; 	Curve fitting with plot.
;
; CALLING SEQUENCE:
;
;       POLYFITWGRAF,X,Y,W,NDEGREE [,A ] [,YFIT] [,YBAND] [,SIGMA] [,/PRINT] 
;         
;
; INPUTS:
;       X:        Position X vector 
;       Y:        Data value Y vector
;       W:        The vector of weights.  This vector should be same length as 
;                 X and Y.
;     NDEGREE:    The degree of polynomial to fit.
;	
; KEYWORD PARAMETERS:
;   PRINT:       Specifies whether the output window will be poped up.
;
; OPTIONAL OUTPUTS:
;            A:  Correlation matrix of the coefficients.
;         YFIT:  The vector of calculated Y's.  Has an error of + or - Yband.
;        YBAND:  Error estimate for each point = 1 sigma.
;        SIGMA:  The standard deviation in Y units.
;
; PROCEDURE:
;      Before accessing this routine, the 'ez_fit.pro' must be loaded into
;      IDL and the path to 'ez_fit.pro' must be in the IDL search path.  
;
; EXAMPLE:
;
;      X = [ ...]
;      Y = [ ...]
;      POLYFITWGRAF,X,Y,W,4,/PRINT
;
; MODIFICATION HISTORY:
;      Written by:  Ben-chin K. Cha, 10-03-97.
;      xx-xx-xxbkc  comment
;-

if n_params() lt 4 then begin
	str='Usage: polyfitwGraf,X,Y,W,NDEGREE [,A] [,YFIT] [,YBAND] [,SIGMA] [,/PRINT]'
	str=[str,'',$
	'Y = A0 + A1 * X^1 + A2 * X^2 + A3 * X^3 + A4 * X^4 + ...', $
	'','POLYFITW - Least-Square Polynomial Fit with Weights']
	res=widget_message(str,/info,title='FITTING Info')
		return
	end

	result = polyfitw(x,y,w,ndegree,yfit,yband,sigma,corrm)

	curv=make_array(n_elements(y),2)
	curv(0,1) = float(yfit)
	curv(0,0) = float(y)

	title = 'Least-Square Polynomial Fit with Weights' 
	comment = 'Y = A0'
	for i=1,ndegree do comment=comment+' + A'+strtrim(i,2) +' * X^'+strtrim(i,2)
	for i=0,ndegree do comment=[comment,'A'+strtrim(i,2)+'='+strtrim(result(i),2)]
	comment=[comment,'SIGMA='+strtrim(sigma,2)]

        yres = yfit - y
        goodness = goodness_fit(yres,n_elements(A))
	comment=[comment,'','GOODNESS OF FIT = '+string(goodness)]

	plot1d,x,curv,/curvfit,title=title,comment=comment,width=500,/stamp, $
		/symbol,Group=group,wtitle='POLYFITW',report='fitting.rpt'

	if keyword_set(print) then begin
        OPENW,unit,'fitting.rpt',/GET_LUN,ERROR=err
        if err ne 0 then begin
        res = widget_message(!err_string,/info,title='FITTING Info')
        return
        end


	printf,unit,'POLYFITW - ',title

 	vec = moment(y, mdev=md, sdev=sd)
	mean = vec(0)
	variance = vec(1)
	skew = vec(2)
	kurtosis = vec(3)
	sdev = replicate(sd, n_elements(x))

	statistic_1d,x,y,c_mass,x_peak,y_peak,y_hpeak,fwhm,fwhm_xl,fwhm_wd

	st = ''
	st = [st, '   Peak Y   = '+strtrim(y_peak,1)]
	st = [st, '   1st Peak @ '+strtrim(x_peak,1)]
;       st = [st, '   H-Peak Y = '+strtrim(y_hpeak)]
        st = [st, '   Centroid @ '+ strtrim(c_mass,1)]
        st = [st, '   FWHM     = '+strtrim(FWHM,1)]
	for i=0,n_elements(st)-1 do printf,unit,st(i)

	printf,unit,''
	printf,unit,'        MEAN=',mean
	printf,unit,'        SDEV=',sqrt(variance)
	printf,unit,'    VARIANCE=',variance
	printf,unit,''

 	for i=0,n_elements(comment) - 1 do printf,unit,comment(i)	
	printf,unit,''

	printf,unit,'            X             Y            YFIT           YFIT-Y        WEIGHT          YBAND '
	for i=0,n_elements(x)-1 do printf,unit,X(i),Y(i),YFIT(i),YRES(i),W(i),yband(i),format='(6G15.8)'
;	print,'A',A
	FREE_LUN,unit
;	xdisplayfile,'fitting.rpt',title=title,width=100
	end
END

PRO polyfitgraf,x,y,ndegree,A,YFIT,YBAND,SIGMA,CORRM,print=print,GROUP=group
;+
; NAME:
; 	POLYFITGRAF
;
; PURPOSE:
;       This routine uses the IDL least square polynomial fit function
;       PLOYFIT with optional error estimates. Double precision computation
;       is assumed. 
;       Then calls PLOT1D to graph the calculated results with the raw 
;       data in a pop-up window.
;
; CATEGORY:
; 	Curve fitting with plot.
;
; CALLING SEQUENCE:
;
;       POLYFITGRAF,X,Y,NDEGREE [,A ] [,YFIT] [,YBAND] [,SIGMA] [,CORRM] [,/PRINT] 
;
; INPUTS:
;       X:        Position X vector 
;       Y:        Data value Y vector
;     NDEGREE:    The degree of polynomial to fit.
;	
; KEYWORD PARAMETERS:
;   PRINT:       Specifies whether the output window will be poped up.
;
; OPTIONAL OUTPUTS:
;            A:  Correlation matrix of the coefficients.
;         YFIT:  The vector of calculated Y's.  Has an error of + or - Yband.
;        YBAND:  Error estimate for each point = 1 sigma.
;        SIGMA:  The standard deviation in Y units.
;        CORRM:  The correlation matrix of the coefficients.
;
; PROCEDURE:
;      Before accessing this routine, the 'ez_fit.pro' must be loaded into
;      IDL and the path to 'ez_fit.pro' must be in the IDL search path.  
;
; EXAMPLE:
;
;      X = [ ...]
;      Y = [ ...]
;      POLYFITGRAF,X,Y,4,A,/PRINT
;
; MODIFICATION HISTORY:
;      Written by:  Ben-chin K. Cha, 10-03-97.
;      xx-xx-xxbkc  comment
;-

; if ndegree >=3 singular matrix detected
; always use double precision in this routine
if n_params() lt 3 then begin
	str='Usage: polyfitGraf,X,Y,NDEGREE [,A] [,YFIT] [,YBAND] [,SIGMA] [,CORRM] [,/PRINT]'
	str=[str,'',$
	'Y = A0 + A1 * X^1 + A2 * X^2 + A3 * X^3 + A4 * X^4 + ...', $
	'','POLY_FIT - Least-Square Polynomial Fit']
	res=widget_message(str,/info,title='FITTING Info')
	return
end
 
;	CATCH,Error_status
;	if Error_status ne 0 then begin
;		res=widget_message([!err_string ,'','NDEGREE='+string(ndegree)])
;		retall
;		return
;	end

dx=double(x)
dy=double(y)
	result = poly_fit(dx,dy,ndegree,yfit,yband,sigma,corrm) 

	curv=make_array(n_elements(y),2)
	curv(0,1) = float(yfit)
	curv(0,0) = float(y)

	title = 'Least-Square POLY_FIT ' 
	comment = 'Y = A0'
	for i=1,ndegree do comment=comment+' + A'+strtrim(i,2) +' * X^'+strtrim(i,2)
	for i=0,ndegree do comment=[comment,'A'+strtrim(i,2)+'='+strtrim(result(i),2)]
	comment=[comment,'SIGMA='+strtrim(sigma,2)]

        yres = yfit - y
        goodness = goodness_fit(yres,n_elements(A))
	comment=[comment,'','GOODNESS OF FIT = '+string(goodness)]

	plot1d,x,curv,/curvfit,title=title,comment=comment,width=500,/stamp, $
		/symbol,Group=group,wtitle='POLY_FIT',report='fitting.rpt'

	if keyword_set(print) then begin
        OPENW,unit,'fitting.rpt',/GET_LUN,ERROR=err
        if err ne 0 then begin
        res = widget_message(!err_string,/info,title='FITTING Info')
        return
        end

	printf,unit,'POLY_FIT - ',title
	printf,unit,''
 	for i=0,n_elements(comment) - 1 do printf,unit,comment(i)	
	printf,unit,''

	printf,unit,'          X              Y             YFIT         YFIT-Y       YBAND'
	for i=0,n_elements(x)-1 do printf,unit,X(i),Y(i),YFIT(i),yres(i),yband(i)
;	print,'A',A
	FREE_LUN,unit
;	xdisplayfile,'fitting.rpt',title=title,width=90
	end

END

PRO gaussfitgraf,x,y,A,estimates=estimages,nterms=nterms,print=print,GROUP=group
;+
; NAME:
; 	GAUSSFITGRAF
;
; PURPOSE:
;       This routine uses the IDL GAUSSIAN fit function y=f(x) where:
;               F(x) = A0*EXP(-z^2/2) + A3 + A4*x + A5*x^2
;                        and
;                z=(x-A1)/A2
;
;        A0 = height of exp, A1 = center of exp, A2 = sigma (the width).
;        A3 = constant term, A4 = linear term, A5 = quadratic term.
;
;       Then calls PLOT1D to graph the calculated results with the raw 
;       data in a pop-up window.
;
; CATEGORY:
; 	Curve fitting with plot.
;
; CALLING SEQUENCE:
;
;       GAUSSFITGRAF,X,Y [,A ] [,ESTIMATES=extimates] [,NTERMS=nterms] [,/PRINT] 
;         
;
; INPUTS:
;       X:        Position X vector 
;       Y:        Data value Y vector
;	
; KEYWORD PARAMETERS:
;  ESTIMATES:    Optional starting estimates for the parameters of the 
;                equation.  Should contain NTERMS (6 if NTERMS is not
;                provided) elements.
;     NTERMS:    Set NTERMS of parameters used in Gaussian fit.
;      PRINT:    Specifies whether the output window will be poped up.
;
; OPTIONAL OUTPUTS:
;       A:       The coefficients of the fit.  A is a three to six
;                element vector as described under PURPOSE.
;
; RESTRICTIONS:
;        The peak or minimum of the Gaussian must be the largest
;        or smallest point in the Y vector.
;
; PROCEDURE:
;      Before accessing this routine, the 'ez_fit.pro' must be loaded into
;      IDL and the path to 'ez_fit.pro' must be in the IDL search path.  
;
; EXAMPLE:
;
;      X = [ ...]
;      Y = [ ...]
;      GAUSSFITGRAF,X,Y,NTERMS=4,/PRINT
;
; MODIFICATION HISTORY:
;      Written by:  Ben-chin K. Cha, 10-03-97.
;      xx-xx-xxbkc  comment
;-

	if n_params() lt 2 then begin
	str="Usage: gaussfitGraf, X, Y [,A] [,ESTIMATES=estimates] [,NTERMS=nterms] [,/PRINT]"
	str=[str,'', $
	 'F(X) = A0 * exp( -Z^2 / 2 ) [ + A3 + A4 * X + A5 * X^2 ]',$
	 '      where   Z = (X - A1) / A2']
	res=widget_message(str,/info,title='FITTING Info')
	return
	end

	if keyword_set(nterms) then begin
	if nterms lt 3 then nterms = 3
	if nterms gt 6 then nterms = 6
	yfit = gaussfit(x,y,A,nterms=nterms)
	endif else yfit = gaussfit(x,y,A) 

	curv=make_array(n_elements(y),2)
	curv(0,1) = float(yfit)
	curv(0,0) = float(y)

	fname = 'gaussfit'
	title = 'Non-linear Least-square Fit of  Gaussian' 
	get_curvefit_function,fname,comment
	for i=0,n_elements(A)-1 do comment=[comment,'A'+strtrim(i,2)+'='+ $
		string(A(i))]
 
        yres = yfit - y
        goodness = goodness_fit(yres,n_elements(A))
	comment=[comment,'','GOODNESS OF FIT = '+string(goodness)]

;	plot,x,yfit
;	oplot,x,y,PSYM=7
	plot1d,x,curv,/curvfit,title=title,comment=comment,width=500,/stamp, $
		/symbol,Group=group,wtitle='GAUSSFIT',report='fitting.rpt'


	if keyword_set(print) then begin
	OPENW,unit,'fitting.rpt',/GET_LUN,ERROR=err
	if err ne 0 then begin
	res = widget_message(!err_string,/info,title='FITTING Info')
	return
	end


 	vec = moment(y, mdev=md, sdev=sd)
	mean = vec(0)
	variance = vec(1)
	skew = vec(2)
	kurtosis = vec(3)
	sdev = replicate(sd, n_elements(x))

	printf,unit,''
	printf,unit,'GAUSSFIT - ',title
	printf,unit,''
	printf,unit,'        MEAN=',mean
	printf,unit,'        SDEV=',sqrt(variance)
	printf,unit,'    VARIANCE=',variance
	printf,unit,''

	for i=0,n_elements(comment)-1 do printf,unit,comment(i)
	printf,unit,''

	printf,unit,'      X            Y           YFIT      YFIT-Y'
	for i=0,n_elements(x)-1 do printf,unit,x(i),y(i),yfit(i),yres(i)
	FREE_LUN,unit
;	xdisplayfile,'fitting.rpt',title='GAUSSFIT'
	end
END

PRO curvefitgraf,x,y,Weights,A,sigma,test=test,print=print,function_name=function_name,noderivative=noderivative,itmax=itmax,tol=tol,GROUP=group
;+
; NAME:
; 	CURVEFITGRAF
;
; PURPOSE:
;
;       Then calls PLOT1D to graph the calculated results with the raw 
;       data in a pop-up window.
;
; CATEGORY:
; 	Curve fitting with plot.
;
; CALLING SEQUENCE:
;
;       CURVEFITGRAF,X,Y,Weights [,A ] [,Sigma] [,/PRINT]  $
;            [,FUNCTION_NAME='funct']  $
;            [,/NODERIVATIVE] [,ITMAX=20] [,TOL=1.e-3] 
;         
;
; INPUTS:
;       X:       Position X vector 
;       Y:       Data value Y vector
;  Weights:      A row vector of weights, the same length as Y. Defaults to 1.
;                Instrumental weighting-Gaussian : Weights(i) = 1./sigma(i) ^2
;                Statistical weighting-Poisson :  Weights(i) = 1./y(i)
;       A:       The coefficients of the fit.  The number of elements in A 
;                must be exactly the same as that defined in the function_name.
;                If not specified, the fitting function should provide the
;                initial default.
;	
; KEYWORD PARAMETERS:
;      PRINT:    Specifies whether the output window will be poped up.
; FUNCTION_NAME: The name of the procedure function to fit. If omitted,
; NODERIVATIVE:  If this keyword is set then the partial derivatives will be
;                calculated by CURVEFIT by forward differences. Otherwise
;                the procedure function should provide the partial 
;                derivatives calculation.  Defaults nodevivative is not set.
;                The procedure function must be written as in 'FUNCT' as
;                described in IDL 'CURVEFIT' restrictions.
;      ITMAX:    Maximum number of iterations. Default = 20.
;      TOL:      The convergence tolerance. Default = 1.e-3. The routine 
;                returns when the relative decrease in chi-squared is less
;                than TOL.
;
; OPTIONAL OUTPUTS:
;       A:       Returns the coefficients of the fit.  
;      Sigma:    A vector of standard deviations for the parameters in A.
;
; RESTRICTIONS:
;       The function to be fit must be defined and called FUNCT,
;       unless the FUNCTION_NAME keyword is supplied.  This function,
;       (actually written as a procedure) must accept values of
;       X (the independent variable), and A (the fitted function's
;       parameter values), and return F (the function's value at
;       X), and PDER (a 2D array of partial derivatives).
;       For an example, see FUNCT in the IDL User's Libaray.
;       A call to FUNCT is entered as:
;       FUNCT, X, A, F, PDER
; where:
;       X = Variable passed into CURVEFIT.  It is the job of the user-written
;                function to interpret this variable.
;       A = Vector of NTERMS function parameters, input.
;       F = Vector of NPOINT values of function, y(i) = funct(x), output.
;       PDER = Array, (NPOINT, NTERMS), of partial derivatives of funct.
;               PDER(I,J) = DErivative of function at ith point with
;               respect to jth parameter.  Optional output parameter.
;               PDER should not be calculated if the parameter is not
;               supplied in call. If the /NODERIVATIVE keyword is set in the
;               call to CURVEFIT then the user routine will never need to
;               calculate PDER.
;
; PROCEDURE:
;      Before accessing this routine, the 'ez_fit.pro' must be loaded into
;      IDL and the path to 'ez_fit.pro' must be in the IDL search path.  
;
;      For more information please refer the PROCEDURE section of the
;      CURVEFIT in IDL online help.
;
; EXAMPLE:
;
;      X = [ ...]
;      Y = [ ...]
;      CURVEFITGRAF,X,Y,Weights,A,Sigma,/PRINT
;
;      For more information please refer the EXAMPLE section of the CURVEFIT
;      in IDL online help.
;
; MODIFICATION HISTORY:
;      Written by:  Ben-chin K. Cha, 10-15-97.
;      xx-xx-xxbkc  comment
;-

fname = 'funct'	
if keyword_set(function_name) then fname=string(function_name)
if keyword_set(test) then begin
	fname = 'gfunct'	
	x=findgen(10)
	y=[12.,11.,10.2,9.4,8.7,8.1,7,5,6.9,6.5,6.1]

	A =[ 10., -0.1, 2.0]
endif else begin
	if n_params() lt 2 then begin
	str = "Usage: curvefitGraf, X, Y, Weights [,A] [,Sigma] [./PRINT] [,FUNCTION_NAME='funct']"
	str = [str,'        [,/NODERIVATIVE] [,ITMAX=20] [,TOL=1.e-3] ']
	str = [str,'', 'with default fit function', $
	'', 'F(X) = A0 * exp( -Z^2 / 2 ) + A3 + A4 * X + A5 * X^2 ', $
	'         where  Z = (X - A1) / A2','', $
	'Non-linear Least Square Fit with Weights' ]
	res = widget_message(str,/info,title='FITTING Info')
	return
	end
end
	
if keyword_set(noderivative) then $
yfit = curvefit(x,y,Weights,A,sigma,iter=iter,function_name=fname,itmax=itmax,tol=tol,/noderivative) else $
yfit = curvefit(x,y,Weights,A,sigma,iter=iter,function_name=fname,itmax=itmax,tol=tol)
help,iter

	curv=make_array(n_elements(y),2)
	curv(0,1) = float(yfit)
	curv(0,0) = float(y)

	title = 'Non-linear Least Square Fit with '+ strupcase(fname) 
	get_curvefit_function,fname,comment
	if fname eq 'funct_erf' then begin
		noe = n_elements(comment)
		comment(noe-1) = comment(noe-1)+ ' = ' + strtrim(2.355*A(3),2)
		end
	for i=0,n_elements(A)-1 do comment=[comment,'A'+strtrim(i,2)+'='+ $
		string(A(i)) + '     SIGMA='+string(sigma(i))]
 
        yres = yfit - y
        goodness = goodness_fit(yres,n_elements(A))
	comment=[comment,'','GOODNESS OF FIT = '+string(goodness)]

;	plot,x,yfit
;	oplot,x,y,PSYM=7
	plot1d,x,curv,/curvfit,title=title,comment=comment,width=500,/stamp, $
		/symbol,Group=group,wtitle='CURVEFIT',report='fitting.rpt'


	if keyword_set(print) then begin
        OPENW,unit,'fitting.rpt',/GET_LUN,ERROR=err
        if err ne 0 then begin
        res = widget_message(!err_string,/info,title='FITTING Info')
        return
        end

 	vec = moment(y, mdev=md, sdev=sd)
	mean = vec(0)
	variance = vec(1)
	skew = vec(2)
	kurtosis = vec(3)
	sdev = replicate(sd, n_elements(x))

	printf,unit,'CURVEFIT - ',title
	printf,unit,''
	printf,unit,'        MEAN=',mean
	printf,unit,'        SDEV=',sqrt(variance)
	printf,unit,'    VARIANCE=',variance
	printf,unit,''

	for i=0,n_elements(comment)-1 do printf,unit,comment(i)
	printf,unit,''

	printf,unit,'      X            Y           YFIT       YFIT-Y   WEIGHTS'
	for i=0,n_elements(x)-1 do printf,unit,x(i),y(i),yfit(i),yres(i),Weights(i)

	FREE_LUN,unit
;	xdisplayfile,'fitting.rpt',title='CURVEFIT'
	end
END

PRO gfunct,x,a,f,pder
	bx=exp(a(1)*x)
	f = a(0)*bx +a(2)
	if n_params() ge 4 then $
	 pder=[[bx], [a(0)*x*bx], [replicate(1.,n_elements(f))] ]
END

PRO get_curvefit_funct_pvt,fname,expres
	expres = 'User defined private function used in curvefit.'
END

PRO get_curvefit_function,fname,expres
lfname = strlowcase(fname)
if  lfname eq 'gfunct' then begin
	expres='Y(X) = A0 * exp( A1 * X ) + A2'
	return
	end
if  lfname eq 'funct_erf' then begin 
	expres='Y(X) = A[0] + A[1] * ERRORF(Z) + A[4] * X '
	expres=[expres,' Z = (X - A[2]) / A[3] / SQRT(2.)',' FWHM = 2.355 * A[3]' ]
	return
	end
if  lfname eq 'funct' then begin 
	expres='Y(X) = A0 * exp( -Z^2 / 2 ) + A3 + A4 * X + A5 * X^2 ,'
	expres=[expres,' Z = (X - A1) / A2' ]
	return
	end
if  lfname eq 'gaussfit' then begin 
	expres='Y(X) = A0 * exp( -Z^2 / 2 ) [ + A3 + A4 * X + A5 * X^2 ] ,'
	expres=[expres,' Z = (X - A1) / A2' ]
	return
	end
if  lfname eq 'lorentzian' then begin 
	expres = 'Y(X) = A0 * A2^2 / ( (X-A1)^2 + A2^2 ) '
	return
	end
get_curvefit_funct_pvt,fname,expres
END


PRO regressfitgraf,x,y,weights,yfit,const,sigma,ftest,r,rmul,chisq,status, $
	relative_weight=relative_weight, $
	print=print,test=test,GROUP=group

if n_params() eq 0 and keyword_set(test) eq 0 then begin
	str='Usage: regressfitGraf,X,Y,Weights,yfit,const[,sigma,ftest,r,rmul,chisq,status,/RELATIVE_WEIGHT,/PRINT]'
	str=[str,'',$
	'Y = A0 + A1 * X1 + A2 * X2 + A3 * X3 + A4 * X4 + ...', $
	'','MULTIPLE Linear Regression Fit with Weights']
	res=widget_message(str,/info,title='FITTING Info')
		return
end

if keyword_set(test) then begin
;Create a two by six array of independent variable data.

X = [[0.0, 0.0], $     
     [2.0, 1.0], $
     [2.5, 2.0], $
     [1.0, 3.0], $
     [4.0, 6.0], $    
     [7.0, 2.0]]

;Create an Npoints-element vector of dependent variable data.

Y = [5.0, 10.0, 9.0, 0.0, 3.0, 27.0]

end

	sz1 = size(x)
	nterms = sz1(1)
	npoints = sz1(2)
	sz2 = size(y)

	if sz2(0) ne 1 and sz2(1) ne npoints then begin
	str =[ 'Inconsistant dimension in X, Y input','NTERMS=',string(nterms), $
		'NPOINTS=',string(npoints), $
		' Y = A0 + A1*X1 + A2*X2 + ... ']
	res = widget_message(str,/info,title='Fitting - multiple regress')
	return
	end

	weights = replicate(1.0,n_elements(y))
	result = regress(x,y,weights,yfit,const,sigma,ftest,r,rmul,chisq,status,/RELATIVE_WEIGHT)
;if keyword_set(print) then begin
;print,'sigma',sigma
;print,'ftest',ftest
;print,'r',r
;print,'rmul',rmul
;print,'chisq',chisq
;print,'status',status
;end

	curv=make_array(n_elements(y),2)
	curv(0,1) = float(yfit(*))
	curv(0,0) = float(y)

	title = 'Multiple Linear Regression Fit with Weights' 
	comment = 'Y(i) = A0'
	for i=1,nterms do comment=comment+' + A'+strtrim(i,2)+' * X' + $
			strtrim(i,2) + '(i)'

	comment = ['',comment,'A0 = '+strtrim(const,2)]
	for i=0,nterms-1 do begin
		str = 'A'+strtrim(i+1,2)+' = '+strtrim(result(0,i),2)
		comment=[comment,str]
		end
	
        yres = yfit - y
        goodness = goodness_fit(yres,n_elements(A))
	comment=[comment,'','GOODNESS OF FIT = '+string(goodness)]

	plot1d,curv,/curvfit,title=title,comment=comment,width=500,/stamp, $
		/symbol,Group=group,wtitle='REGRESS',report='fitting.rpt'
;	plot1d,x,curv,/curvfit,title=title,comment=comment,width=500,/stamp, $
;		wtitle='REGRESS'

	if keyword_set(print) then begin
        OPENW,unit,'fitting.rpt',/GET_LUN,ERROR=err
        if err ne 0 then begin
        res = widget_message(!err_string,/info,title='FITTING Info')
        return
        end

	printf,unit,'REGRESS - ',title
	printf,unit,''
	printf,unit,' X [',strtrim(sz1(1),2),' x ',strtrim(sz1(2),2),'] :'
	printf,unit,X
	printf,unit,' Y [',strtrim(sz2(1),2),'] :'
	printf,unit,Y
	printf,unit,''

 	for i=0,n_elements(comment) - 1 do printf,unit,comment(i)	
	printf,unit,''

	printf,unit,'          Weights      Y             YFIT       YFIT-Y '
	for i=0,n_elements(y)-1 do printf,unit,weights(i),Y(i),YFIT(i),yres(i)
	FREE_LUN,unit
;	xdisplayfile,'fitting.rpt',title='REGRESS'
	end
END
PRO pickYX_getvector
COMMON EZ_FIT_BLOCK,ezfitData,image
COMMON W_READASCII_BLOCK,readascii_info

;  'YX_GETVECTOR'
	if ezfitData.dim eq 1 then begin
	WIDGET_CONTROL, ezfitData.J_field, GET_VALUE=j
	if j ge 0 and j lt ezfitData.height then ezfitData.J_index = j else begin
	WIDGET_CONTROL,ezfitData.J_field, SET_VALUE=ezfitData.height-1
	ezfitData.J_index = ezfitData.height-1
	end
	if j ge ezfitData.height then ezfitData.J_index = ezfitData.height-1 
        ezfitData.y = image(*,ezfitData.J_index)
        title='Y vs X (1D Data)'

	WSET,ezfitData.image_area
        plot,ezfitData.x(0:ezfitData.width-1), ezfitData.y,PSYM=0,thick=2,$
		color=ezfitData.table_size-1, $
                xtitle='X', ytitle='Y', title=title
	if XRegistered('POLYFITW_SETUP') then $
	WIDGET_CONTROL,ezfitData.polyfit_label, $
		SET_VALUE='Number of Elements: '+strtrim(ezfitData.width,2)
        ezfitData.pick=0
	str = '       I    X Values         Y Values'
	for i=0,ezfitData.width-1 do $
	str = [str,string(i, ezfitData.x(i), ezfitData.y(i),/print)]
	WIDGET_CONTROL, ezfitData.text_area, SET_VALUE=str	
        end

	if n_elements(readascii_info) then $
	WIDGET_CONTROL,readascii_info.table, $
	 set_table_select=[ ezfitData.J_index, 0, $
		ezfitData.J_index,ezfitData.width-1 ], BAD_ID=b
END

PRO pickZX_getvector
COMMON EZ_FIT_BLOCK,ezfitData,image
COMMON W_READASCII_BLOCK,readascii_info
;  'ZX_GETVECTOR'
        if ezfitData.dim eq 2 then begin
;                ezfit_get2DData,ezfitData.file,image
                ezfitData.zy = image(ezfitData.I_index,*)
        title='Z vs Y @ X='+strtrim(ezfitData.x(ezfitData.I_index),2)

	WSET,ezfitData.image_area
        plot,ezfitData.y(0:ezfitData.height-1), ezfitData.zy,PSYM=0,thick=2, $
		color=ezfitData.table_size-1, $
                xtitle='Y', ytitle='Z', title=title
	if XRegistered('POLYFITW_SETUP') then $
	WIDGET_CONTROL,ezfitData.polyfit_label, $
		SET_VALUE='Number of Elements: '+strtrim(ezfitData.height,2)
        ezfitData.pick=2
	str = '       I    Y Values         Zx Values'
	for i=0,ezfitData.height-1 do $
	str = [str,string(i, ezfitData.y(i), ezfitData.zy(i),/print)]
	WIDGET_CONTROL, ezfitData.text_area, SET_VALUE=str	
        end

	; row
	if n_elements(readascii_info) then $
	WIDGET_CONTROL,readascii_info.table, $
	 set_table_select=[0,ezfitData.I_index, $
		ezfitData.width-1, ezfitData.I_index], BAD_ID=b
END

PRO pickZY_getvector
COMMON EZ_FIT_BLOCK,ezfitData,image
COMMON W_READASCII_BLOCK,readascii_info
; 'ZY_GETVECTOR'
        if ezfitData.dim eq 2 then begin
;               ezfit_get2DData,ezfitData.file,image
                ezfitData.zx = image(*,ezfitData.J_index)
        title='Z vs X @ Y='+strtrim(ezfitData.y(ezfitData.J_index),2)

	WSET,ezfitData.image_area
        plot,ezfitData.x(0:ezfitData.width-1), ezfitData.zx,PSYM=0,thick=2, $
		color=ezfitData.table_size-1, $
                xtitle='X', ytitle='Z', title=title
	if XRegistered('POLYFITW_SETUP') then $
	WIDGET_CONTROL,ezfitData.polyfit_label, $
		SET_VALUE='Number of Elements: '+strtrim(ezfitData.width,2)
        ezfitData.pick=1
	str = '       I    X Values         Zy Values'
	for i=0,ezfitData.width-1 do $
	str = [str,string(i, ezfitData.x(i), ezfitData.zx(i),/print)]
	WIDGET_CONTROL, ezfitData.text_area, SET_VALUE=str	
        end

	; column
	if n_elements(readascii_info) then $
	WIDGET_CONTROL,readascii_info.table, $
	 set_table_select=[ezfitData.J_index,0, $
		ezfitData.J_index,ezfitData.height-1], BAD_ID=b
END

PRO GETVECTOR_MAIN13_Event, Event
COMMON EZ_FIT_BLOCK,ezfitData,image

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'FIELD4': BEGIN
	WIDGET_CONTROL,Event.id,GET_VALUE=i
	if i ge 0 and i lt ezfitData.width then ezfitData.I_index = i else $
		WIDGET_CONTROL,Event.id,SET_VALUE=ezfitData.width-1
      END
  'FIELD5': BEGIN
	WIDGET_CONTROL,Event.id,GET_VALUE=j
	if j ge 0 and j lt ezfitData.height then ezfitData.J_index = j else $
		WIDGET_CONTROL,Event.id,SET_VALUE=ezfitData.height-1
      END
  'GETVECTOR_SLIDER': BEGIN
	WIDGET_CONTROL,Event.id,GET_VALUE=j
	ezfitData.J_index = j
	WIDGET_CONTROL,ezfitData.J_field,SET_VALUE=j
	if ezfitData.dim eq 1 then pickYX_getvector else $
	pickZY_getvector
      END
  'GETVECTOR_XSLIDER': BEGIN
	WIDGET_CONTROL,Event.id,GET_VALUE=j
	ezfitData.I_index = j
	WIDGET_CONTROL,ezfitData.I_field,SET_VALUE=j
	pickZX_getvector
      END
  'HELP_GETVECTOR': BEGIN
	str=["Click the  'Image' button to show the image", $
		"Click the left mouse button in the image area to select the interest point",$
		"Click the 'Z vs X' to plot the ZX curve", $
		"Click the 'Z vs Y' to plot the ZY curve", $
		"Click the 'Help' button to get this info", $
		"Click the 'Close' button to close the dialog" ]
	res=widget_message(str,/info,title='GetVector - Help')	
	END
  'CLOSE_GETVECTOR': BEGIN
	WIDGET_CONTROL,Event.Top,/DESTROY
	END
  'FRESH_GETVECTOR': BEGIN
	WSET,ezfitData.image_area
	TVSCL,congrid(image,ezfitData.TV_width,ezfitData.TV_height)
	WIDGET_CONTROL,ezfitData.J_field,set_value=ezfitData.J_index
	WIDGET_CONTROL,ezfitData.slider,set_slider_max=ezfitData.height-1
	if ezfitData.dim eq 2 then $
	WIDGET_CONTROL,ezfitData.xslider,set_slider_max=ezfitData.width-1
	END
  'YX_GETVECTOR': BEGIN
	if ezfitData.dim eq 1 then begin
	WIDGET_CONTROL, ezfitData.J_field, GET_VALUE=j
	if j ge 0 and j lt ezfitData.height then ezfitData.J_index = j else begin
	WIDGET_CONTROL,ezfitData.J_field, SET_VALUE=ezfitData.height-1
	ezfitData.J_index = ezfitData.height-1
	end
	if j ge ezfitData.height then ezfitData.J_index = ezfitData.height-1 
        ezfitData.y = image(*,ezfitData.J_index)
        title='Y vs X (1D Data)'

	WSET,ezfitData.image_area
        plot,ezfitData.x(0:ezfitData.width-1), ezfitData.y,PSYM=0,thick=2,$
		color=ezfitData.table_size-1, $
                xtitle='X', ytitle='Y', title=title
	if XRegistered('POLYFITW_SETUP') then $
	WIDGET_CONTROL,ezfitData.polyfit_label, $
		SET_VALUE='Number of Elements: '+strtrim(ezfitData.width,2)
        ezfitData.pick=0
	str = '       I    X Values         Y Values'
	for i=0,ezfitData.width-1 do $
	str = [str,string(i, ezfitData.x(i), ezfitData.y(i),/print)]
	WIDGET_CONTROL, ezfitData.text_area, SET_VALUE=str	
        end
	END
  'ZY_GETVECTOR': BEGIN
	pickZY_getvector
	END
  'ZX_GETVECTOR': BEGIN
	pickZX_getvector
	END
  'DRAW6': BEGIN
	;cursor,x,y,0,/device
	x=Event.x
	y=Event.y
	x = fix(x / ezfitData.x_mag)
	y = fix(y / ezfitData.y_mag)

	if x ge 0 and x lt ezfitData.width then begin
		ezfitData.I_index = x
		WIDGET_CONTROL,ezfitData.I_field,SET_VALUE=x
	end
	if y ge 0 and y lt ezfitData.height then begin
		ezfitData.J_index = y
		WIDGET_CONTROL,ezfitData.J_field,SET_VALUE=y
	end
      END
  ENDCASE
END



PRO ezfit_getvector,image, GROUP=Group
COMMON EZ_FIT_BLOCK,ezfitData

if XRegistered('GETVECTOR_MAIN13') then $
	WIDGET_CONTROL,ezfitData.base_getvector,/DESTROY,/bad_id

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

if !d.n_colors gt 256 then device,decomposed=0
device,retain=2

  GETVECTOR_MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
      MAP=1, $ ; TLB_FRAME_ATTR=8,$
      TITLE='EZ_FIT-2D Image', $
      UVALUE='GETVECTOR_MAIN13')

  BASE2 = WIDGET_BASE(GETVECTOR_MAIN13, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  sz = size(image)
  dim='('+strtrim(sz(1),2)+'x'+strtrim(sz(2),2)+')'
  LABEL3 = WIDGET_LABEL( BASE2, $
;     FONT='-misc-fixed-medium-r-normal--20-200-75-75-c-100-iso8859-1', $
      UVALUE='LABEL3', $
      VALUE='GetVector '+dim)

  BASE3 = WIDGET_BASE(GETVECTOR_MAIN13, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE3')

  FieldVal1722 = [ $
    strtrim(ezfitData.I_index,2) ]
  FIELD4 = CW_FIELD( BASE3,VALUE=FieldVal1722, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Row ', $
      UVALUE='FIELD4', $
      XSIZE=4)
  if ezfitdata.dim eq 2 then $
  xslider = widget_slider(BASE3,value=1,max=ezfitdata.width-1, $
		xsize=50, title='Row', $ 
		UVALUE='GETVECTOR_XSLIDER')


  FieldVal1787 = [ $
    strtrim(ezfitData.J_index,2) ]
  FIELD5 = CW_FIELD( BASE3,VALUE=FieldVal1787, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Column ', $
      UVALUE='FIELD5', $
      XSIZE=4)

  if ezfitdata.height gt 0 then $
  slider = widget_slider(BASE3,value=1,max=ezfitdata.height-1, $
		xsize=50, title='Column',$
		UVALUE='GETVECTOR_SLIDER')

  DRAW6 = WIDGET_DRAW( BASE2, $
      BUTTON_EVENTS=1, $
      RETAIN=2, $
      UVALUE='DRAW6', $
      XSIZE=ezfitData.TV_width, $
      YSIZE=ezfitData.TV_height, $
      X_SCROLL_SIZE=300, $
      Y_SCROLL_SIZE=300)

  TEXT_AREA = WIDGET_TEXT(BASE2,YSIZE=10,VALUE='',/SCROLL)
  ezfitData.text_area = TEXT_AREA

  BASE4 = WIDGET_BASE(GETVECTOR_MAIN13, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE4')

  fresh_getvector=WIDGET_BUTTON(BASE4,value='Image',UVALUE='FRESH_GETVECTOR')
if ezfitData.dim eq 1 then $
  YX_getvector=WIDGET_BUTTON(BASE4,value='Y vs X',UVALUE='YX_GETVECTOR') $
else begin
  ZX_getvector=WIDGET_BUTTON(BASE4,value='Zx vs Y',UVALUE='ZX_GETVECTOR')
  ZY_getvector=WIDGET_BUTTON(BASE4,value='Zy vs X',UVALUE='ZY_GETVECTOR')
end
  help_getvector=WIDGET_BUTTON(BASE4,value='Help...',UVALUE='HELP_GETVECTOR')
  close_getvector=WIDGET_BUTTON(BASE4,value='Close',UVALUE='CLOSE_GETVECTOR')


  WIDGET_CONTROL, GETVECTOR_MAIN13, /REALIZE

  ; Get drawable window index

  COMMON DRAW6_Comm, DRAW6_Id
  WIDGET_CONTROL, DRAW6, GET_VALUE=DRAW6_Id

  ezfitData.base_getvector = GETVECTOR_MAIN13
  ezfitData.I_field = FIELD4
  ezfitData.J_field = FIELD5
  ezfitData.slider = slider
  if ezfitdata.dim eq 2 then $
  ezfitData.xslider = xslider
  ezfitData.image_area = DRAW6_Id
  WSET,ezfitData.image_area

;  TVSCL,image
   TVSCL,congrid(image,ezfitData.TV_width,ezfitData.TV_height)
   ezfitData.x_mag= float(ezfitData.TV_width)/ezfitData.width
   ezfitData.y_mag= float(ezfitData.TV_height)/ezfitData.height

  XMANAGER, 'GETVECTOR_MAIN13', GETVECTOR_MAIN13
END




PRO POLYFITW_SETUP_Event, Event
COMMON EZ_FIT_BLOCK,ezfitData,image

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'FIELD4': BEGIN
      Print, 'Event for Ndegree'
	WIDGET_CONTROL,ezfitData.polyfit_ndfield, $
		GET_VALUE=i
	ezfitData.polyfit_ndegree=i
      END
  'FIELD5': BEGIN
      Print, 'Event for Weight Factor'
	WIDGET_CONTROL,ezfitData.polyfit_wffield, $
		GET_VALUE=f
	ezfitData.polyfit_factor=f
      END
  'OK_POLYFITW': BEGIN
	WIDGET_CONTROL,ezfitData.polyfit_ndfield, $
		GET_VALUE=i
	ezfitData.polyfit_ndegree=i
	WIDGET_CONTROL,ezfitData.polyfit_wffield, $
		GET_VALUE=f
	ezfitData.polyfit_factor=f
        ezfit_picktype,x,y
;        w=replicate(ezfitData.polyfit_factor,n_elements(x))
	w = make_array(n_elements(x),value=1.)
	findWFactor,f(0),WFactor
	if n_elements(WFactor) eq n_elements(x) then w = WFactor
	ndegree = ezfitData.polyfit_ndegree
	if ezfitData.polyfit eq 1 then polyfitwGraf,x,y,w,ndegree,/print,GROUP=Event.top $
	else polyfitGraf,x,y,ndegree,/print,GROUP=Event.top
	END
  'HELP_POLYFITW': BEGIN
	str = ['The number of wight factors entered should be the same as', $
		'the elements in the data array. Otherwise, 1. is used.']
	ret = dialog_message(str,/info)
	END
  'CANCEL_POLYFITW': BEGIN
	WIDGET_CONTROL,Event.Top,/DESTROY
	END
  ENDCASE
END



PRO polyfitwSetup, GROUP=Group
COMMON EZ_FIT_BLOCK,ezfitData,image

  IF XRegistered('POLYFITW_SETUP') then return
  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0


  junk   = { CW_PDMENU_S, flags:0, name:'' }


  POLYFITW_SETUP = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
      MAP=1, TITLE='POLYFIT Setup', $
      UVALUE='POLYFITW_SETUP')

  BASE2 = WIDGET_BASE(POLYFITW_SETUP, $
      COLUMN=1, $
      MAP=1, $
      TITLE='POLYFITW - Setup', $
      UVALUE='BASE2')

nelem=strtrim(ezfitData.width,2)
if ezfitData.pick eq 2 then nelem=strtrim(ezfitData.height,2)
  LABEL3 = WIDGET_LABEL( BASE2, $
      UVALUE='LABEL3', /ALIGN_LEFT, $
      VALUE='Number of Elements: '+nelem)
  ezfitData.polyfit_label = LABEL3 

  FieldVal395 = [ $
    '4' ]
  FIELD4 = CW_FIELD( BASE2,VALUE=FieldVal395, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Ndegree', $
      UVALUE='FIELD4')
  ezfitData.polyfit_ndfield = FIELD4

  FieldVal460 = [ $
    '1.' ]
  FIELD5 = CW_FIELD( BASE2,VALUE=FieldVal460, $
      ROW=1, $
;      FLOAT=1, $
      RETURN_EVENTS=1, $
      TITLE='Weight Factor', $
      UVALUE='FIELD5')
  ezfitData.polyfit_wffield = FIELD5

  BASE3 = WIDGET_BASE(POLYFITW_SETUP, $
      ROW=1, $
      MAP=1 )
  OK_polyfitw=WIDGET_BUTTON(BASE3,VALUE='Accept',UVALUE='OK_POLYFITW')
  help_polyfitw=WIDGET_BUTTON(BASE3,VALUE='Help...',UVALUE='HELP_POLYFITW')
  Cancel_polyfitw=WIDGET_BUTTON(BASE3,VALUE='Cancel',UVALUE='CANCEL_POLYFITW')

  WIDGET_CONTROL, POLYFITW_SETUP, /REALIZE

  XMANAGER, 'POLYFITW_SETUP', POLYFITW_SETUP

END





PRO SVDFIT_MAIN13_Event, Event
COMMON EZ_FIT_BLOCK,ezfitData,image

  WIDGET_CONTROL,Event.top,GET_UVALUE=info
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'SVDFIT_FNAME': BEGIN
	WIDGET_CONTROL,info.fname_fld,GET_VALUE=fname
	info.fname = fname(0)
	ezfitData.svdfit_fname = info.fname
      END
  'SVDFIT_NTERM': BEGIN
	WIDGET_CONTROL,info.nterm_fld,GET_VALUE=n
	ezfitData.svdfit_nterm = info.nterm
	info.nterm = n
      END
  'SVDFIT_WEIGHT': BEGIN
	WIDGET_CONTROL,info.weight_fld,GET_VALUE=n
	info.weight = n
	ezfitData.svdfit_factor = info.weight
      END
  'LEGENDRE_YES': BEGIN
	info.legendre = Event.Index
	ezfitData.svdfit_legendre = info.legendre
      END
  'SVDFIT_ACCEPT': BEGIN
	WIDGET_CONTROL,info.fname_fld,GET_VALUE=fname
	info.fname = fname(0)
	WIDGET_CONTROL,info.nterm_fld,GET_VALUE=n
	info.nterm = n
	WIDGET_CONTROL,info.weight_fld,GET_VALUE=n
	info.weight = n

	ezfitData.svdfit_fname = info.fname
	ezfitData.svdfit_nterm = info.nterm
	ezfitData.svdfit_factor = info.weight
	ezfitData.svdfit_legendre = info.legendre
       ezfit_picktype,x,y
       ws = replicate(info.weight,n_elements(y))
	if info.legendre eq 0 then begin 
      	 svdfitGraf,x,y,info.nterm, $
		function_name=info.fname, $
		weights=ws, $
		/print,GROUP=Event.top
	endif else begin
      	 svdfitGraf,x,y,info.nterm, $
		weights=ws, $
		/legendre, $
		/print,GROUP=Event.top
	end
      END
  'BUTTON7': BEGIN
	WIDGET_CONTROL,Event.Top,/DESTROY
      END
  ENDCASE

 WIDGET_CONTROL,Event.top,SET_UVALUE=info,bad_id=bad
;if bad eq 0 then begin
;ezfitData.svdfit_base = info.base
;ezfitData.svdfit_fname = info.fname
;ezfitData.svdfit_nterm = info.nterm
;ezfitData.svdfit_factor= info.weight
;ezfitData.svdfit_legendre = info.legendre
;end
END




PRO svdfitSetup, GROUP=Group


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }


  SVDFIT_MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, title='SVDFIT Setup', $
      UVALUE='SVDFIT_MAIN13')

  BASE2 = WIDGET_BASE(SVDFIT_MAIN13, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  FieldVal2739 = [ $
    'svdfunct' ]
  SVDFIT_FNAME = CW_FIELD( BASE2,VALUE=FieldVal2739, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Function_Name:', $
      UVALUE='SVDFIT_FNAME', $
      XSIZE=20)

  FieldVal2804 = [ $
    '4' ]
  SVDFIT_NTERM = CW_FIELD( BASE2,VALUE=FieldVal2804, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='NTERMS:', $
      UVALUE='SVDFIT_NTERM')

  FieldVal2804 = [ $
    '1.' ]
 SVDFIT_WEIGHT = CW_FIELD( BASE2,VALUE=FieldVal2804, $
      ROW=1, $
      Float=1, $
      RETURN_EVENTS=1, $
      TITLE='WEIGHTS:', $
      UVALUE='SVDFIT_WEIGHT')
  
  legendre_yes = WIDGET_DROPLIST(BASE2,VALUE=['No','Yes'], $
	UVALUE='LEGENDRE_YES', TITLE='LEGENDRE')
  WIDGET_CONTROL,legendre_yes,SET_DROPLIST_SELECT=0

  BASE5 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE5')

  svdfit_accept = WIDGET_BUTTON( BASE5, $
      UVALUE='SVDFIT_ACCEPT', $
      VALUE='Accept')

  BUTTON7 = WIDGET_BUTTON( BASE5, $
      UVALUE='BUTTON7', $
      VALUE='Cancel')

info = { base: SVDFIT_MAIN13, $
	fname_fld: SVDFIT_FNAME, $
	weight_fld: SVDFIT_WEIGHT, $
	nterm_fld: SVDFIT_NTERM, $
	fname: 'svdfunct', $
	nterm: 4, $
	weight :1., $
	legendre : 0 $
	}

WIDGET_CONTROL,SVDFIT_MAIN13,SET_UVALUE=info

  WIDGET_CONTROL, SVDFIT_MAIN13, /REALIZE

  XMANAGER, 'SVDFIT_MAIN13', SVDFIT_MAIN13
END


PRO findFactor,inString,outString,factor,print=print,operator=operator
; separate the array and the factor 

x = '1,2,3,10*4,5,6'
sep = ','
if n_elements(inString) then x = inString
if keyword_set(operator) then sep = operator

	y = strsplit(x,sep,/extract)

	nl = n_elements(y) 
	factor = make_array(nl,value=1.)
	z = strpos(y,"*")

	for i=0,n_elements(z)-1 do begin
	if z(i) gt 0 then begin
		p = strsplit(y(i),"*",/extract)
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

	y = strsplit(x,sep,/extract)
	z = strpos(y,'*')

	no = n_elements(z)

	for i=0,no-1 do begin
	if z(i) eq -1 then begin
		ff = y(i)
		if n_elements(factor) eq 0 then factor = float(y(i)) else $
			factor = [factor,float(y(i))]
	end
	if z(i) gt 0 then begin
		p = strsplit(y(i),'*',/extract)
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
	m = y*0
	for i=1,nx-1 do begin
		dx = x(i) - x(i-1)
		if dx ne 0. then begin
		m(i)= (y(i)-y(i-1))/dx
;		print,i,x(i),y(i),m(i)
		end
	end
	return,m
END

PRO  getStatisticDeviation_1d,id1,y,mean,sdev,mdev,st
        mean=0.
        sdev=0.
        mdev=0.
        no = n_elements(y)
        if no eq 0 then return
        mean = total(y)/no
        if no eq 1 then return
        index = where(y gt mean, count)      ; check for constant function
        mean = [mean,0.,0.,0.]
        if count gt 0 then mean = MOMENT(y,mdev=mdev,sdev=sdev)

st = id1
st= [st+' ']
        st = [st, '   Mean         = '+string(mean(0))]
        st = [st, '   Standard Dev = '+string(sdev)]
        st = [st, '   Mean Abs Dev = '+string(mdev)]
        st = [st, '   Variance     = '+string(mean(1))]
        st = [st, '   Skewness     = '+string(mean(2))]
        st = [st, '   Kurtosis     = '+string(mean(3))]
END

PRO  get_statistic_1d,namelabel,p1,d1,c_mass,xpeak,ypeak,y_hpeak,FWHM,st
; p1 - x vector
; d1 - y vector
; call statistic_1d

        statistic_1d,p1,d1,c_mass,x_peak,y_peak,y_hpeak,FWHM

st = namelabel
st= [st+' ']
        st = [st, '   Peak  X='+strtrim(x_peak,1)+'  Y='+strtrim(y_peak,1)]
        st = [st, '   H-Peak  Y='+strtrim(y_hpeak)]
        st = [st, '   Centroid  '+ strtrim(c_mass,1)]
        st = [st, '   FWHM      '+strtrim(FWHM,1)]

if n_elements(x_peak) gt 0 then begin
        largest = max(y_peak)
        i_largest = 0
        for i=0,n_elements(x_peak)-1 do begin
                if y_peak(i) ge largest then begin
                i_largest = i
                goto, write_peak
                end
                end
        write_peak:
        xpeak = x_peak(i_largest)
        ypeak = y_peak(i_largest)
        end

END

;
; find  fwh_max, c_mass, peak for a given x,y array
;
PRO statistic_1d,x,y,c_mass,x_peak,y_peak,y_hpeak, fwhm,x_hwdl,x_hwdr, $
	REPORT=REPORT, FIT=FIT, $
	XINDEX=XINDEX,LIST=LIST,PLOT=PLOT,TITLE=TITLE,GROUP=group

;
; FIT  -  based on data points or use fit data
;
xindex = keyword_set(XINDEX)

nx = n_elements(x)
a=make_array(nx,/float)
da=make_array(nx,/float)
ny=make_array(nx,/float)

slopey = slope(x,y)

ymin = min(y)
ymax = max(y)
ny = y - ymin
;if total(abs(ny)) eq 0. then $
;	r = dialog_message(['Invalid, constant vector data found!!'],/Error)

peak = ymax
hpeak = 0.5 * max(ny)
y_hpeak= hpeak + ymin

list = 0
if keyword_set(report) then list=1
if list then begin
	openw,unit,report,/get_lun
	if keyword_set(title) then printf,unit,title
	printf,unit,'========'
end
if list then printf,unit,'I             X           Y        Y-Ymin          A         DY/DX'
d0=0
for i=0,nx-1 do begin
	if i gt 0 then begin
	dx = x(i) - x(i-1)
	if dx ne 0. then begin
	da(i) = 0.5 *(ny(i)+ny(i-1)) * dx
	d0 = d0 + da(i)
	a(i) = d0
	end
	end
	if list then printf,unit,strtrim(i,2),x(i),y(i),ny(i),a(i),slopey(i)
end

area = d0
harea = 0.5 * area

; Find c_mass

newtons_method,x,a,harea,c_mass
if list then printf,unit,'========'
if list then printf,unit,'h_area,C_mass:',harea,c_mass
if list then printf,unit,'Ymax,(Ymax-Ymin)/2,Y_hpeak:',ymax,hpeak,y_hpeak


; Find half peaks

if list then printf,unit,'========'
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
if list then begin
	nl = n_elements(nohwdl)
	if nl gt 1 then begin
 	printf,unit,'hwdl X Index:',nohwdl(1:nl-1)
	printf,unit,'x_hwdl:',x_hwdl(1:nl-1)
	end
	nr = n_elements(nohwdr)
	if nr gt 1 then begin
	printf,unit,'hwdr X Index:',nohwdr(1:nr-1)
	printf,unit,'x_hwdr:',x_hwdr(1:nr-1)
	end
	printf,unit,'========'
end
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
	for j=i,nohwe-1 do begin
;		if x_hwde(j) gt x1 then begin
;		dxfwhm = x_hwde(j) - x_hwd(i)
		if x_hwde(j) ne x1 then begin
		dxfwhm = abs(x_hwde(j) - x_hwd(i))
		lo=lo+1
		if lo eq 1 then xfwhm = dxfwhm else $
		xfwhm = [xfwhm,dxfwhm]
		if list then printf,unit,'FWHM:',lo,xfwhm(lo-1)
		goto,outer
		end
	end
	outer:
	end
	end
	FWHM = max(xfwhm,imax)
end

	nef = n_elements(xfwhm)
	nel = n_elements(x_hwde)

if n_elements(nohwdr) gt 1 then begin
	if n_elements(x_hwd) gt 0 then $
	x_hwd = [x_hwd, x_hwdr(1:n_elements(nohwdr)-1)] else $
	x_hwd = [x_hwdr(1:n_elements(nohwdr)-1)]
	end

if n_elements(x_hwd) gt 0 then begin
	if n_elements(imax) then x_HPeak = x_hwd(imax) else x_HPeak = x_hwd
;	x_hwdl = x_hwd
	if n_elements(x_hwde) then x_hwdr = x_hwde
	if list then printf,unit,'0.5*(ymax-ymin):',hpeak
	if list then printf,unit,'x_hpeak:',x_hpeak
	if list then printf,unit,'y_hpeak:',y_hpeak


	; plot if view specified

	if keyword_set(plot) then begin
	ya= make_array(nx,2)
	ya(0,0)=y(*)
	ya(0,1) = y(*)*0 + y_hpeak
	comment=[ 'FWHM='+strtrim(FWHM,2) +',  Cntro='+strtrim(c_mass,2), $
		'yhpeak='+strtrim(y_hpeak,2) + '  xhpeak='+strtrim(x_hpeak,2)]

	if n_elements(x_hwdl) gt 1 then begin
	x_hwd = x_hwdl(1:n_elements(x_hwdl)-1)
	for i=0,n_elements(x_hwd)-1 do begin
		st =' xl='+strtrim(x_hwd(i),2) 
		if nel gt 0 and i lt nel then $
		  st = st +'   xr='+strtrim(x_hwde(i),2)
		if nef gt 0 and i lt nef then $
		  st = st +'   fwhm='+strtrim(xfwhm(i),2)
		comment=[comment,st] 
	end
	end

	if n_elements(comment) gt 10 then comment=[comment(0:8),'. . .']
	if keyword_set(title) then $
	plot1d,x,ya,comment=comment,title=title,group=group,report=report else $
	plot1d,x,ya,comment=comment,group=group,report=report
	end
end

; Find peaks


if keyword_set(FIT) eq 0 then begin

	x_peak = x(0)
	y_peak = peak
	for i=0,nx -1 do begin
		if y(i) eq peak then begin
		x_peak = x(i)
		y_peak = peak
		if list then 	printf,unit,'Peak @x,y:',x_peak, y_peak
		goto,append_close
		end
	end

endif else begin
nopeaks=0
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
	if list then 	printf,unit,'Peak #',i,x_sol,y(i1)
		x_peak(i-1)= x_sol
		y_peak(i-1) = y(i1)
		end
end
endif else begin
	y_peak = ymax
	if y(0) gt y(nx-1) then x_peak = x(0) else x_peak = x(nx-1)
	if list then 	printf,unit,'Xmax,Ymax: ',x_peak, y_peak
end

end

append_close:

	if list then begin
	free_lun,unit
	close,unit
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
;       07-15-02      bkc  Move drawing area  cursor event to top
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
PRO get_svdfit_function,fname,expres
ffname= strlowcase(fname)
if ffname eq 'svdfunct' then begin
	express='Function Name Used:  SVDFUNCT'
        expres=[ express,'F = A0 + A1*X + A2*X^2 + A3*X^3 + ...']
	return
	end
if ffname eq 'legendre' then begin
	express='Function Name Used:  SVDLEG'
        expres=[ express,'P0(X) = 1']
        expres=[expres,'P1(X) = X ']
        expres= [expres,'P2(X) = (3 * X^2 - 1)/2']
        expres=[expres,'P3(X) = (5 * X^3 - 3 * X) / 2 ', ' ...']
        expres=[expres,'Pn(X) = ((2n - 1)* X * Pn-1(X) - (n-1) * Pn-2(X)) / n ']
	return
        end
get_svdfit_myfunct,fname,expres  ; private myfunct
if n_elements(expres) eq 0 then expres='Function Name Used in SVDFIT:'+ffname
END

PRO get_svdfit_myfunct,fname,expres
if fname eq 'myfunct' then begin
	express='Function Name Used: MYFUNCT'
        expres=[express,'Y = A0 + A1 * sin(2*X)/X + A2 * cos(4*X)^2 ']
        end
if n_elements(expres) eq 0 then expres='Function Name Used: SVDFUNCT '
END

FUNCTION myfunct, X ,M
if M ne 3 then $ 
    res=widget_message('For myfunct the NTERMS must be 3', $
		/info,title='FITTING Info')
    return,[ [1.0], [SIN(2*X)/X], [COS(4.*X)^2.] ]
END

PRO svdfitgraf,x,y,NTERMS,weights=weights,print=print,test=test, $
	Group=group, $
	function_name=function_name,legendre=legendre,_Extra=extra

if n_params() eq 0 and keyword_set(test) eq 0 then begin
	str='Usage: svdfitGraf,X,Y,NTERMS [,/PRINT]'
	str=[str,'',$
	'','General Least Squares Fit with optional error estimates', $
	'', '         User-supplied function', $
	'  or       Legendre polynomial']
	res=widget_message(str,/info,title='FITTING Info')
		return
end

if keyword_set(test) then begin

fname = 'myfunct'
C = [7.77, 8.88, -9.99]          ;Provide an array of coefficients.
X = FINDGEN(100)/15.0 + 0.1
Y = C(0) + C(1) * SIN(2*X)/X + C(2) * COS(4.*X)^2.

sig = 0.05 * Y                        ; Set uncertainties to 5%
A=[1,1,1]                               ;Provide an initial guess
ws = 1/sig^2

	result = SVDFIT(X, Y, A=A, WEIGHTS=ws, $
	FUNCTION_NAME=fname, SIGMA=SIGMA, YFIT=YFIT)
	FOR I = 0, N_ELEMENTS(A)-1 DO $
    	PRINT, I, result(I), SIGMA(I), C(I),$
    	FORMAT = '(" A( ",I1," ) = ",F7.4," +- ",F7.4," VS. ",F7.4)'

endif else begin
	if n_elements(NTERMS) eq 0 then NTERMS=3
	A = replicate(1.,NTERMS)
	ws = replicate(1.,n_elements(y))
	if keyword_set(weights) then ws=weights
	if keyword_set(function_name) then fname=function_name else $
	fname = 'svdfunct'
	if keyword_set(legendre) then begin
	 fname='legendre'
	result = svdfit(x,y,A=A,yfit=yfit,weights=ws, $
		legendre=legendre,sigma=sigma, _Extra=extra)
	endif else begin
	result = svdfit(x,y,A=A,yfit=yfit,weights=ws, $
		function_name=fname,sigma=sigma, _Extra=extra)
	end
end


	curv=make_array(n_elements(y),2)
	curv(0,1) = float(yfit(*))
	curv(0,0) = float(y)

	title = 'SVDFIT - General Least Squares Fit with Weights' 
	get_svdfit_function,fname,comment
	
	for i=0,n_elements(A)-1 do begin
		str = 'A'+strtrim(i,2)+' = '+strtrim(result(i),2)
		comment=[comment,str]
		end
	
	yres = yfit - y
	goodness = goodness_fit(yres,n_elements(A))
        comment = [comment,'','GOODNESS OF FIT = '+string(goodness)]

	plot1d,x,curv,/curvfit,title=title,comment=comment,width=500,/stamp, $
		/symbol,Group=group,wtitle='SVDFIT',report='fitting.rpt'

	if keyword_set(print) then begin
        OPENW,unit,'fitting.rpt',/GET_LUN,ERROR=err
        if err ne 0 then begin
        res = widget_message(!err_string,/info,title='FITTING Info')
        return
        end


	printf,unit,title
	printf,unit,''

 	for i=0,n_elements(comment) - 1 do printf,unit,comment(i)	
	printf,unit,''
	printf,unit,'      X             Y             YFIT        YFIT-Y      WEIGHT'
	for i=0,n_elements(y)-1 do printf,unit,x(i),Y(i),YFIT(i),yres(i),ws(i)
	FREE_LUN,unit
;	xdisplayfile,'fitting.tmp',title=title
	end
END
PRO curvefit_setup_help
str = [ 'Itmax    - Maximun number of iterations for fitting.', $
	'TOL      - The convergence tolerance. The routine returns when the',$
	'           relative decrease in chi-squared is less than TOL in an iteration.', $
	'NoDerivative - Yes/No. If analytical derivatives are available then',$
	'               they should be defined in the function defenition.', $
	'               If No is set the analytical derivatives will be used.', $
	'               If Yes is set the forward differences will be used in',$
	'               estimate of the partial derivatives.', $ 
	'Function_Name   - Specifies the fit function to be used by the CURVEFIT. ',$
	'                  It defaults to FUNCT.', $
	'Initial Fitting Coefficients - Specifies the starting fit coefficients.', $
	'                  Values entered must be separated by the comma.',$
	'                  The number of values entered must consist with the',$
	'                  fit function used. If not given the default values ',$
	'                  will be used.',$
	'                  The fit function should provide the default values for', $
	'                  the fit coefficients.',$
	'Accept     - Accepts the setting and excutes the CURVEFIT',$
	'Clear      - Clear the initial fitting coefficients',$
	'Help...    - Provides this help info.',$
	'Close      - Closes the curvefit_setup dialog.' $
	]
	res=widget_message(str,title='CURVEFIT_SETUP_HELP',/info)
END

PRO CURVEFIT_SETUP_Event, Event
COMMON EZ_FIT_BLOCK,ezfitData,image

  WIDGET_CONTROL,Event.Top,GET_UVALUE=info
  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'CURVEFIT_ITMAX': BEGIN
      Print, 'Event for Itmax'
      END
  'CURVEFIT_TOL': BEGIN
      Print, 'Event for TOL'
      END
  'CURVEFIT_NODERIV': BEGIN
	ezfitData.curvefit_noderiv = Event.index
      END
  'CURVEFIT_FNAME': BEGIN
      Print, 'Event for Function_Name'
      END
  'CURVEFIT_PARAMS': BEGIN
      Print, 'Event for parameters in Vector A'
      END
  'BUTTON8': BEGIN
	curvefit_setup_help
      END
  'BUTTON11': BEGIN
      WIDGET_CONTROL,info.A_fld,SET_VALUE=''
      END
  'BUTTON9': BEGIN
      WIDGET_CONTROL,info.tol_fld,GET_VALUE=tol 
      WIDGET_CONTROL,info.itmax_fld,GET_VALUE=itmax
      WIDGET_CONTROL,info.fname_fld,GET_VALUE=fname
      WIDGET_CONTROL,info.A_fld,GET_VALUE=A
	newa = strcompress(strtrim(a(0),2))
	params=strsplit(newa,',',/extract)

     ezfit_picktype,x,y
	a = float(params)
	name = strlowcase(fname(0))
        statistic_1d,x,y,c_mass,x_peak,y_peak,y_hpeak,fwhm,fwhm_xl,fwhm_wd

;
; if no coefficient given use the default
;
if n_elements(a) le 1 then begin
; if lorentzian type
	if name eq 'lorentzian' then $ 
	a=[y_peak, x_peak, 0.5*fwhm_wd]

; if errorf type
	if name eq 'funct_erf' then begin
	y0 = min(y)
	a =[y0,y0,fwhm_xl(0),x(1)-x(0),y0]
	a0 = min(y)
	a1 = 1.
	a2 = x_peak
	a3 = (x(n_elements(x)-1)-x(0)) * .25
	a4 = (y(n_elements(x)-1)-y(0))/a3   ;x(1)-x(0)
	a = [a0,a1,a2,a3,a4]
	end

	if name eq 'funct_erf1' then begin
	a0 = min(y)
	a1 = 1.
	a2 = x_peak
	a3 = (x(n_elements(x)-1)-x(0)) * .25
	a4 = (y(n_elements(x)-1)-y(0))/sqrt(a3)
	a = [a0,a1,a2,a3,a4]
	end

; six parameters required in funct
	if name eq 'funct' then begin
        sd= 0.05 * y
        a0 = y_peak
        a1 = x_peak
        a2 = 1.
        a3 = -1.
        a4 = 1.
        a5 = -1.
        A = [a0,a1,a2,a3,a4,a5]
        end
end

Weights=replicate(1.0,n_elements(x))
if keyword_set(gaussian) and n_elements(sd) then Weights=1.0 / sd ; Gaussian
if keyword_set(poisson) and min(y) gt 0. then Weights=1.0 / y       ; Poisson

sigma = replicate(1.,n_elements(A))

	if ezfitData.curvefit_noderiv then $
     	curvefitGraf,x,y, Weights, A, sigma, itmax=itmax, tol=tol, $
		GROUP=Event.top, $
		function_name=name,/print,/noderivative else $ 
     	curvefitGraf,x,y, Weights, A, sigma, itmax=itmax, tol=tol, $
		GROUP=Event.top, $
		function_name=name,/print 
	str=strtrim(A(0),2)
	for i=1,n_elements(A)-1 do str=str+','+strtrim(A(i),2)
	widget_control,info.A_fld,set_value=str	
      END
  'BUTTON10': BEGIN
      WIDGET_CONTROL,Event.Top,/DESTROY
      END
  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END CURVEFIT_SETUP
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.



PRO curvefit_setup,function_name=function_name, GROUP=Group


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  title = 'CURVEFIT_SETUP'
  if keyword_set(function_name) then title = title+'_'+ function_name
  CURVEFIT_SETUP = WIDGET_BASE(GROUP_LEADER=Group, $
      COLUMN=1, $
      MAP=1, TITLE=title, $
      UVALUE='CURVEFIT_SETUP')

  BASE2 = WIDGET_BASE(CURVEFIT_SETUP, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE3 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE3')

  FieldVal234 = [ $
    '20' ]
  CURVEFIT_ITMAX = CW_FIELD( BASE3,VALUE=FieldVal234, $
      ROW=1, $
      INTEGER=1, $
      RETURN_EVENTS=1, $
      TITLE='Itmax', $
      UVALUE='CURVEFIT_ITMAX', $
      XSIZE=4)

  FieldVal299 = [ $
    '1.e-3' ]
  CURVEFIT_TOL = CW_FIELD( BASE3,VALUE=FieldVal299, $
      ROW=1, $
      FLOAT=1, $
      RETURN_EVENTS=1, $
      TITLE='TOL', $
      UVALUE='CURVEFIT_TOL', $
      XSIZE=8)

  CURVEFIT_NODERIV = WIDGET_DROPLIST(BASE3,title='NoDerivative', $
	value=['No','Yes'], $
	UVALUE='CURVEFIT_NODERIV')
  WIDGET_CONTROL,CURVEFIT_NODERIV,SET_DROPLIST_SELECT=0


  FieldVal429 = [ $
    'FUNCT' ]
  if keyword_set(function_name) then FieldVal429=function_name
  CURVEFIT_FNAME = CW_FIELD( BASE2,VALUE=FieldVal429, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Procedure_Name', $
      UVALUE='CURVEFIT_FNAME', $
      XSIZE=20)


  FieldVal429 = [ $
    '' ]
  CURVEFIT_PARAMS = CW_FIELD( BASE2,VALUE=FieldVal429, $
      COLUMN=1, FRAME=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Initial Fitting Coefficients (comma separated)', $
      UVALUE='CURVEFIT_PARAMS', $
      XSIZE=50)

  BASE8 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE8')

  BUTTON9 = WIDGET_BUTTON( BASE8, $
      UVALUE='BUTTON9', $
      VALUE='Accept')

  BUTTON11 = WIDGET_BUTTON( BASE8, $
      UVALUE='BUTTON11', $
      VALUE='Clear')

  BUTTON8 = WIDGET_BUTTON( BASE8, $
      UVALUE='BUTTON8', $
      VALUE='Help...')

  BUTTON10 = WIDGET_BUTTON( BASE8, $
      UVALUE='BUTTON10', $
      VALUE='Close')


  info = { itmax_fld: CURVEFIT_ITMAX, $
	tol_fld: CURVEFIT_TOL, $
	fname_fld: CURVEFIT_FNAME, $
	A_fld: CURVEFIT_PARAMS, $
	itmax: 20, $
	tol: 1.e-3, $
	noderiv:0, $
	fname: 'funct', $
	A: make_array(50) $
	}

  WIDGET_CONTROL, CURVEFIT_SETUP, SET_UVALUE=info
  WIDGET_CONTROL, CURVEFIT_SETUP, /REALIZE

  XMANAGER, 'CURVEFIT_SETUP', CURVEFIT_SETUP
END
;
; this fitting function provided by Peter Ilinski
;
PRO errorffit_help
str = ['Usage: curvefitGraf,FUNCTION_NAME="FUNCT_ERF",X,Y,Weights,[,A]', $
        '[,Sigma][,/NODERIVATIVE][,ITMAX=20],[,TOL=1.e-3]', $
        '', 'with FUNCT_ERF defined as ','',$
        'F(X) = A0 + A1 * ERRORF(Z) + A4*X', $
        '       where Z = (X -A2) / A3', $
	'             FWHM = 2.355 * A3', '', $
        'Non-linear Least Square Fit with Weights']
res = dialog_message(str,/info)
END

PRO     FUNCT_ERF,X,A,F,PDER
;       F=A[0]+A[1]*ERRORF(z)+A[4]*x
;       z=(x-A[2])/A[3]/sqrt(2.)
;
        ON_ERROR,2                        ;Return to caller if an error occurs
        NTERMS = 5
        if n_elements(A) ne NTERMS then begin
                res = dialog_message('Number of fitting terms must be'+ $
                string(NTERMS),/Error)
                return
        end
        if A[3] ne 0.0 then z=(x-A[2])/A[3]/sqrt(2.) $ ;GET Z
        else z= 10.
        F=A[0]+A[1]*ERRORF(z)+A[4]*x              ;FUNCTIONS.
;        Print, x,z,f
        IF N_PARAMS(0) LT 3 THEN RETURN ;NEED PARTIAL?
;
        PDER = FLTARR(N_ELEMENTS(X),5)                  ;init ARRAY.
        PDER[*,0] = 1.                                  
	;COMPUTE PARTIALS
        if A[3] ne 0. then PDER[0,1] = ERRORF(z)
        if A[3] ne 0. then PDER[0,2] = -A[1] * exp(-z^2) * sqrt(2./!pi)/A[3]
        PDER[0,3] = -A[1] * exp(-z^2) * Z*2/A[3]/sqrt(!pi)
        PDER[0,4] = x
        RETURN
END

PRO get_curvefit_funct_pvt,fname,expres
ffname = strlowcase(fname)
if  ffname eq 'funct_erf' then begin
       expres='Y(X) = A[0] + A[1] * ERRORF(z) + A[4] * x '
       expres=[expres,' Z = (x - A[2]) / A[3] / sqrt(2.)' ]
	expres=[expres,' FWHM = A[3] * 2.355']
        return
        end

END



;
; this fitting function provided by Peter Ilinski
;
PRO errorf1fit_help
str = ['Usage: curvefitGraf,FUNCTION_NAME="FUNCT_ERF1",X,Y,Weights,[,A]', $
        '[,Sigma][,/NODERIVATIVE][,ITMAX=20],[,TOL=1.e-3]', $
        '', 'with FUNCT_ERF1 defined as ','',$
        'F(X) = A0 + A1 * ERRORF(Z) + A4*X*X', $
        '       where Z = (X -A2) / A3', $
	'             FWHM = 2.355 * A3', '', $
        'Non-linear Least Square Fit with Weights']
res = dialog_message(str,/info)
END

PRO     FUNCT_ERF1,X,A,F,PDER
;       F=A[0]+A[1]*ERRORF(z)+A[4]*x*x
;       z=(x-A[2])/A[3]/sqrt(2.)
;
        ON_ERROR,2                        ;Return to caller if an error occurs
        NTERMS = 5
        if n_elements(A) ne NTERMS then begin
                res = dialog_message('Number of fitting terms must be'+ $
                string(NTERMS),/Error)
                return
        end
        if A[3] ne 0.0 then z=(x-A[2])/A[3]/sqrt(2.) $ ;GET Z
        else z= 10.
        F=A[0]+A[1]*ERRORF(z)+A[4]*x*x              ;FUNCTIONS.
;        Print, x,z,f
        IF N_PARAMS(0) LT 3 THEN RETURN ;NEED PARTIAL?
;
        PDER = FLTARR(N_ELEMENTS(X),5)                  ;init ARRAY.
        PDER[*,0] = 1.                                  
	;COMPUTE PARTIALS
        if A[3] ne 0. then PDER[0,1] = ERRORF(z)
        if A[3] ne 0. then PDER[0,2] = -A[1] * exp(-z^2) * sqrt(2./!pi)/A[3]
        PDER[0,3] = -A[1] * exp(-z^2) * Z*2/A[3]/sqrt(!pi)
        PDER[0,4] = x*x
        RETURN
END

PRO get_curvefit_funct_pvt,fname,expres
ffname = strlowcase(fname)
if  ffname eq 'funct_erf1' then begin
       expres='Y(X) = A[0] + A[1] * ERRORF(Z) + A[4] * X * X '
       expres=[expres,' Z = (X - A[2]) / A[3] / sqrt(2.)' ]
	expres=[expres,' FWHM = A[3] * 2.355']
        return
        end

END




PRO ROIFIT2_Event, Event

COMMON ROIFIT2_BLOCK,widget_ids

  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev


IF TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN begin
	print,'kill the application ?'
        end

  CASE Ev OF 
  'ROI_DRAW_FITTING': BEGIN
	if Event.Press eq 1 then begin
	wset,widget_ids.drawId
	cursor,x,y,/data
	if x ge widget_ids.xrange(0) and x le widget_ids.xrange(1) then begin
	WIDGET_CONTROL, widget_ids.centroid, SET_VALUE= strtrim(x,2)
        widget_ids.A1(widget_ids.roi) = float(x)
	end
	if y ge widget_ids.yrange(0) and y le widget_ids.yrange(1) then begin
	WIDGET_CONTROL, widget_ids.intensity, SET_VALUE= strtrim(y,2) 
        widget_ids.A0(widget_ids.roi) = float(y)
	end
	end
	END
  'ROI_SELECT': BEGIN
	widget_ids.roi = Event.index
	xl = strtrim(widget_ids.xl_val(Event.index),2)
	WIDGET_CONTROL,widget_ids.left_fld,SET_VALUE=xl
	xl = strtrim(widget_ids.xr_val(Event.index),2)
	WIDGET_CONTROL,widget_ids.right_fld,SET_VALUE=xl
	xl = strtrim(widget_ids.A0(Event.index),2)
	WIDGET_CONTROL,widget_ids.intensity,SET_VALUE=xl
	xl = strtrim(widget_ids.A1(Event.index),2)
	WIDGET_CONTROL,widget_ids.centroid,SET_VALUE=xl
	xl = strtrim(2.*widget_ids.A2(Event.index),2)
	WIDGET_CONTROL,widget_ids.fwhm,SET_VALUE=xl
	END
  'ROI_LEFT': BEGIN
	END
  'ROI_RIGHT': BEGIN
	END
  'ROI_CENTROID': BEGIN
	WIDGET_CONTROL, widget_ids.centroid, GET_VALUE= x_peak
        widget_ids.A1(widget_ids.roi) = float(x_peak)
	END
  'ROI_FWHM': BEGIN
	WIDGET_CONTROL, widget_ids.fwhm, GET_VALUE= fwhm
        widget_ids.A2(widget_ids.roi) = float(fwhm) * .5
	END
  'ROI_INTENSITY': BEGIN
	WIDGET_CONTROL, widget_ids.intensity, GET_VALUE= y_peak
        widget_ids.A0(widget_ids.roi) = float(y_peak)
	END
  'ROI_CENTROID_Y': BEGIN
	print,'ROI_CENTROID_Y',Event.Value
	END
  'ROI_FWHM_Y': BEGIN
	print,'ROI_FWHM_Y',Event.Value
	END
  'ROI_INTENSITY_Y': BEGIN
	print,'ROI_INTENSITY_Y',Event.Value
	END
  'SLIDER_CENTROID': BEGIN
	xl = widget_ids.x(Event.Value)
	WIDGET_CONTROL,widget_ids.centroid,SET_VALUE=strtrim(xl,2)
	widget_ids.A1(widget_ids.roi) = xl
      END
  'SLIDER2': BEGIN
	widget_ids.xl= Event.Value
	xl = widget_ids.x(Event.Value)
	WIDGET_CONTROL,widget_ids.left_fld,SET_VALUE=strtrim(xl,2)
      END
  'SLIDER3': BEGIN
	widget_ids.xr= Event.Value
	xr = widget_ids.x(Event.Value)
	WIDGET_CONTROL,widget_ids.right_fld,SET_VALUE=strtrim(xr,2)
      END
  'ROI_CLEAR': BEGIN
	widget_ids.code(widget_ids.roi) = 0
	widget_ids.A0(widget_ids.roi) = 0. 
	widget_ids.A1(widget_ids.roi) = 0. 
	widget_ids.A2(widget_ids.roi) = 0.
	WIDGET_CONTROL, widget_ids.centroid, SET_VALUE='0.'
	WIDGET_CONTROL, widget_ids.fwhm, SET_VALUE= '0.'
	WIDGET_CONTROL, widget_ids.intensity, SET_VALUE= '0.'
      END
  'ROI_ADD': BEGIN
if widget_ids.code(widget_ids.roi) eq 1 and widget_ids.roi lt 19 then begin
	widget_ids.roi = widget_ids.roi + 1
	WIDGET_CONTROL,widget_ids.roi_select,SET_DROPLIST_SELECT=widget_ids.roi
end
	widget_ids.code(widget_ids.roi) = 1
	newSegment,widget_ids.x,widget_ids.y,widget_ids.xl, widget_ids.xr,$
		newX,newY,newW 
	statistic_1d,newX,newY,c_mass,x_peak,y_peak,y_hpeak,fwhm,fwhm_xl,fwhm_wd
	widget_ids.A0(widget_ids.roi) = y_peak
	widget_ids.A1(widget_ids.roi) = x_peak
	widget_ids.A2(widget_ids.roi) = fwhm * .5
	WIDGET_CONTROL, widget_ids.centroid, SET_VALUE= strtrim(x_peak,2)
	WIDGET_CONTROL, widget_ids.fwhm, SET_VALUE= strtrim(fwhm,2)
	WIDGET_CONTROL, widget_ids.intensity, SET_VALUE= strtrim(y_peak,2)
	widget_ids.xl_val(widget_ids.roi) = newX(0)
	widget_ids.xr_val(widget_ids.roi) = max(newX)
        END
  'ROI_PANEL': BEGIN
	widget_ids.code(widget_ids.roi) = 1
	WIDGET_CONTROL, widget_ids.left_fld, GET_VALUE= xl 
	WIDGET_CONTROL, widget_ids.right_fld, GET_VALUE= xr 
	WIDGET_CONTROL, widget_ids.intensity, GET_VALUE= y_peak
	WIDGET_CONTROL, widget_ids.centroid, GET_VALUE= x_peak
	WIDGET_CONTROL, widget_ids.fwhm, GET_VALUE= fwhm
	widget_ids.xl_val(widget_ids.roi) = float(xl)
	widget_ids.xr_val(widget_ids.roi) = float(xr)
	widget_ids.A0(widget_ids.roi) = float(y_peak)
	widget_ids.A1(widget_ids.roi) = float(x_peak)
	widget_ids.A2(widget_ids.roi) = float(fwhm) * .5
        END
  'ROI_LIST': BEGIN
	for i=0,widget_ids.n_roi -1 do begin 
	if widget_ids.code(i) then begin
	str = string(i)+string(widget_ids.xl_val(i))+ $
		string(widget_ids.xr_val(i)) + $
		string(widget_ids.A0(i)) + $
		string(widget_ids.A1(i)) + string(widget_ids.A2(i))
	if n_elements(A) eq 0 then $
	A = ['      ROI         XL         XR        Intensity   Centroid       FWHM/2','-------------------------------------------------------------------------',str] else $
	A =[A,str]
	end
	end
	WIDGET_CONTROL,widget_ids.text,SET_VALUE=A
	END
  'ROI_CALC': BEGIN
	for i=0,widget_ids.n_roi -1 do begin 
	if widget_ids.code(i) then begin
	if n_elements(A) eq 0 then A = [widget_ids.A0(i),widget_ids.A1(i), widget_ids.A2(i)] else $
	A =[A,widget_ids.A0(i),widget_ids.A1(i), widget_ids.A2(i)]
	end
	end
	x = widget_ids.x
	y = widget_ids.y
	if widget_ids.subplot then $
	multi_lorentzfitgraf,x, y, A, /print, /subplot, GROUP=Event.top else $
	multi_lorentzfitgraf,x, y, A, /print, GROUP=Event.top
	END
  'ROI_SUBPLOT': BEGIN
	widget_ids.subplot = Event.index
	return
	END
  'ROI_HELP': BEGIN
	str = ['Y = A0 * A2^2 / ( (X-A1)^2 + A2^2 ) + ... ', $
	'',$
	'Drawing Area -  Click local peak to update the current ROI centroid ',$
	'                and intensity fields', $
	'Total # of ROIs can be set for Lorentzian Fit defaults to 20', $
	'','ROI #        - Droplist to select the current ROI', $
	'Left         -  Starting X value', $
	'  left       -  Slider setting the starting X value', $
	'Right        - Ending X value', $
	'  right      -  Slider setting the ending X value', $
	'Intensity    - Peak intensity factor, A0', $
	'Centroid     -  X value where peak intensity located, A1', $
	'  slider     -  Slider setting the X centroid of peak Intensity', $
	'FWHM         -  Full width half maximum, (=2*A2)', $
	'Subplot      -  Option of plotting each Lorentzian sub-curves', $
	'Add          -  Add ROI with default intensity, centroid, FWHM values', $
	'Edit         -  Modifiy selected ROI intensity, centroid, and FWHM values', $
	'Del          -  Delete selected ROI # from the list', $
	'Show ROIs    -  Display ROIs info in the text scroll window', $
	'Calc Fits... -  Accept the ROIs and do curve fitting', $
	'Help...      -  Pops up this help info window', $
	'Close        -  Close the multi Lorentzian fit dialog', $
'','NOTE:','', $
	'***Add a new ROI***', $
	'    Press the "Add Button" to accept the default values ', $
	'    Click the local peak to automatically filling in the intensity', $
	'    and centroid fields from the graph for the ROI.', $
	'***Delete the selected ROI***', $
	'    Press the "Del Button" to remove the selected ROI', $
	'    Press the "Show ROIs Button" to show total ROIs into', $
	'***Edit the selected ROI***', $
	'    Modifiy the appropriate Intensity, Centroid, and FWHM fields', $
	'    Press the "Edit Button" to accept the changes', $
	'    Press the "Show ROIs Button" to show the ROIs info', '',$
	'' $
	]

	xdisplayfile,text=str,title='Lorenzian Multi ROIs Help',group=Event.top
; 	res = widget_message(str,/info,title='ROI Help Info')	
        END
  'ROI_EXIT': BEGIN
	WIDGET_CONTROL,Event.Top,/DESTROY
        END
  ENDCASE
END




PRO ez_fit2, N_ROI,X,Y, GROUP=Group

COMMON ROIFIT2_BLOCK,widget_ids

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  if n_elements(x) eq n_elements(y) then begin
	dim = n_elements(x)
	statistic_1d,X,Y,c_mass,x_peak,y_peak,y_hpeak,fwhm,fwhm_xl,fwhm_wd
	end

  ROIFIT2 = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=1, $
      MAP=1, TITLE='LORENTZIAN MULTI-ROI Fitting', $
      UVALUE='ROIFIT2')

  BASE2 = WIDGET_BASE(ROIFIT2, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE2')

  LABEL4 = WIDGET_LABEL( BASE2, $
      UVALUE='LABEL4', $
      VALUE='Define Multi Lorentzian ROIs / Curve Fitting')

  DRAW7 = WIDGET_DRAW(BASE2, UVALUE='ROI_DRAW_FITTING',/BUTTON_EVENTS, $
	/EXPOSE_EVENTS,xsize=700,ysize=150)

  BASE7 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE7')

  BASE7_1 = WIDGET_BASE(BASE7, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE7_1')
  LABEL7_1 = WIDGET_LABEL( BASE7_1, VALUE='  ROI # ')

value='1'
if n_elements(N_ROI) then value=strtrim(indgen(N_ROI),2)
  roi_select = WIDGET_DROPLIST( BASE7_1, $
      UVALUE='ROI_SELECT', $
      VALUE=value)

  BASE7_2 = WIDGET_BASE(BASE7, $
      COLUMN=1, $
      MAP=1, FRAME=1, $
      UVALUE='BASE7_2')
  label7_2 = WIDGET_LABEL(BASE7_2,VALUE='Left')
  TextVal541 = [ $
    strtrim(x(0)) ]
  TEXT10 = WIDGET_TEXT( BASE7_2,VALUE=TextVal541, $
      XSIZE=10, $
      EDITABLE=1, $
      UVALUE='ROI_LEFT', $
      YSIZE=1)
  SLIDER2 = WIDGET_SLIDER( BASE7_2, $
      MAXIMUM=dim-1, $
      MINIMUM=0, $
      UVALUE='SLIDER2', $
      VALUE=0, $
      XSIZE=80)


  BASE7_3 = WIDGET_BASE(BASE7, $
      COLUMN=1, $
      MAP=1, FRAME=1, $
      UVALUE='BASE7_3')
  label7_3 = WIDGET_LABEL(BASE7_3,VALUE='Right')
  TextVal595 = [ $
    strtrim(x(dim-1)) ]
  TEXT11 = WIDGET_TEXT( BASE7_3,VALUE=TextVal595, $
      EDITABLE=1, $
      UVALUE='ROI_RIGHT', $
      XSIZE=10, $
      YSIZE=1)
  SLIDER3 = WIDGET_SLIDER( BASE7_3, $
      MAXIMUM=dim-1, $
      MINIMUM=0, $
      UVALUE='SLIDER3', $
      VALUE=dim-1, $
      XSIZE=80)


  BASE7_4 = WIDGET_BASE(BASE7, $
      MAP=1, FRAME=1, /column, $
      UVALUE='BASE7_4')
  label7_4 = WIDGET_LABEL(BASE7_4,VALUE='Peak Intensity')
  TextVal757 = [ $
    strtrim(y_peak,2) ]
  TEXT14 = WIDGET_TEXT( BASE7_4,VALUE=TextVal757, $
      EDITABLE=1, $
      UVALUE='ROI_INTENSITY', $
      XSIZE=15, $
;      XSIZE=6, $
      YSIZE=1)

;  btn1=['','']
;  roi_intensity_btn = CW_BGROUP(BASE7_4, btn1, /EXCLUSIVE, $
;		/ROW,UVALUE='ROI_INTENSITY_Y')
;  WIDGET_CONTROL,roi_intensity_btn,SET_VALUE=1


  BASE7_5 = WIDGET_BASE(BASE7, $
;      ROW=1, $
      COLUMN=1, $
      MAP=1, FRAME=1, $
      UVALUE='BASE7_5')
  label7_5 = WIDGET_LABEL(BASE7_5,VALUE='Centroid @ X')
  TextVal649 = [ $
    strtrim(x_peak,2) ]
  TEXT12 = WIDGET_TEXT( BASE7_5,VALUE=TextVal649, $
      EDITABLE=1, $
      UVALUE='ROI_CENTROID', $
      XSIZE=15, $
      YSIZE=1)
  SLIDER5 = WIDGET_SLIDER( BASE7_5, $
      MAXIMUM=dim-1, $
      MINIMUM=0, $
      UVALUE='SLIDER_CENTROID', $
;      VALUE=dim-1, $
      XSIZE=80)


;  btn1=['','']
;  roi_centroid_btn = CW_BGROUP(BASE7_5, btn1, /EXCLUSIVE, $
;		/ROW,UVALUE='ROI_CENTROID_Y')
;  WIDGET_CONTROL,roi_centroid_btn,SET_VALUE=1


  BASE7_6 = WIDGET_BASE(BASE7, $
      MAP=1, FRAME=1, /column, $
      UVALUE='BASE7_6')
  label7_6 = WIDGET_LABEL(BASE7_6,VALUE=' FWHM ')
  TextVal703 = [ $
    strtrim(fwhm,2) ]
  TEXT13 = WIDGET_TEXT( BASE7_6,VALUE=TextVal703, $
      EDITABLE=1, $
      UVALUE='ROI_FWHM', $
      XSIZE=15, $
      YSIZE=1)

;  btn1=['','']
;  roi_fwhm_btn = CW_BGROUP(BASE7_6, btn1, /EXCLUSIVE, $
;		/ROW,UVALUE='ROI_FWHM_Y')
;  WIDGET_CONTROL,roi_fwhm_btn,SET_VALUE=1

  BASE9 = WIDGET_BASE(BASE2, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE9')

  value=['N','Y']
  roi_subplot = WIDGET_DROPLIST( BASE9, $
      UVALUE='ROI_SUBPLOT', TITLE='Subplot', $
      VALUE=value)
  WIDGET_CONTROL,roi_subplot,SET_DROPLIST_SELECT=1


  roi_add = WIDGET_BUTTON(BASE9,VALUE='Add', $
                UVALUE='ROI_ADD')

  roi_panel = WIDGET_BUTTON(BASE9,VALUE='Edit', $
                UVALUE='ROI_PANEL')

  roi_clear = WIDGET_BUTTON(BASE9,VALUE='Del', $
                UVALUE='ROI_CLEAR')

  roi_list = WIDGET_BUTTON(BASE9,VALUE='Show ROIs', $
                UVALUE='ROI_LIST')

  roi_calc = WIDGET_BUTTON(BASE9,VALUE='Calc Fits...', $
                UVALUE='ROI_CALC')

  roi_help = WIDGET_BUTTON(BASE9,VALUE='Help...', $
                UVALUE='ROI_HELP')

  roi_exit = WIDGET_BUTTON(BASE9,VALUE='Close', $
                UVALUE='ROI_EXIT')

  roi_text = WIDGET_TEXT(BASE2, $
	VALUE='', $
	XSIZE=80, YSIZE=8, /SCROLL,UVALUE='ROI_TEXT')


  widget_ids = { $
	drawId: 0L, $
        roi_select : roi_select, $
	left_fld: TEXT10, $
	right_fld: TEXT11, $
	centroid:  TEXT12, $
	fwhm:     TEXT13, $
	intensity: TEXT14, $
	text: roi_text, $
	dim : dim, $
	n_roi : n_roi, $
	roi : 0, $
	code : make_array(n_roi,/int), $   ; 1 - parameters defined for the ROI 
	xl_val : make_array(n_roi,value=x(0)), $	; roi xl value
	xr_val : make_array(n_roi,value=x(dim-1)), $	; roi xr value
	A0: make_array(n_roi), $  	; y-Peak
	A1: make_array(n_roi), $  	; x at y-Peak
	A2: make_array(n_roi), $  	; fwhm/2.
	xrange: [min(x),max(x)], $
	yrange: [min(y),max(y)], $
	subplot : 1, $      ; 0 - no subplot, 1 - subplot on
	xl : 0, $
	xr : dim-1, $
	X: x, $
	Y: y $
	}

  WIDGET_CONTROL, ROIFIT2, /REALIZE
  COMMON DRAW7_Comm, DRAW7_Id
  WIDGET_CONTROL,DRAW7,GET_VALUE=DRAW7_Id
  widget_ids.drawId = DRAW7_Id

; default starts with 1 lorentzian fit


  wset,widget_ids.drawId 
  plot,x,y,yrange=widget_ids.yrange

  widget_ids.code(0) = 1
  widget_ids.A0(0) = y_peak
  widget_ids.A1(0) = x_peak
  widget_ids.A2(0) = fwhm/2.

  XMANAGER, 'ROIFIT2', ROIFIT2
END
;
; Auto Save File For ez_fit.pro
;
;  Mon Aug 25 13:23:31 CDT 1997
;

@PS_open.pro
@u_read.pro
@plot1d.pro
@fit_user.pro
;@regressfit.pro
;@readascii.pro
;@lorentzian.pro
;@xdisplayfile.pro
;@fitting.pro
;@svdfitgraf.pro
;@ezfit_getvector.pro

PRO ezfit_get1DData,filename,y
COMMON EZ_FIT_BLOCK,ezfitData,image
if n_params() eq 0 then begin
	res=widget_message("Usage: ezfit_get1DData,filename,image",/info, $
		title='EZ_FIT Info')
	return
end
u_openr,unit,filename,/XDR 
u_read,unit,x
u_read,unit,y
u_close,unit
	ezfit_init1d,x,y
        return
END

PRO ezfit_init1d,x,y
COMMON EZ_FIT_BLOCK,ezfitData,image
        ezfitData.x = x
        *ezfitData.im = y
        ezfitData.y = y(*,ezfitData.J_index)
        ezfitData.dim = 1
        ezfitData.pick = 0
        ezfitData.width = n_elements(x)
        sz=size(y)
        if sz(0) eq 1 then ezfitData.height=1 else ezfitData.height=sz(2)
        ezfitData.I_index=0
;        ezfitData.J_index=0
        image=y
END

PRO ezfit_readImageData,x,y,im,filename=filename

file='fitting.bin'
if keyword_set(filename) then file=strtrim(filename,2)
r = findfile(file,count=ct)
if ct eq 0 then return
u_openr,unit,file,/XDR
u_read,unit,x
u_read,unit,y
u_read,unit,im
u_close,unit

END 

PRO ezfit_get2DData,F,image2
COMMON EZ_FIT_BLOCK,ezfitData,image
		ezfit_readImageData,xarray,yarray,image,filename=F 
	if n_elements(xarray) eq 0 then return
		*ezfitData.im = image
		ezfit_init2D,xarray,yarray,image2
		ezfitData.file = F
END

PRO ezfit_init2D,xarray,yarray,image2
COMMON EZ_FIT_BLOCK,ezfitData,image
		ezfitData.x = xarray
		ezfitData.y = yarray
		ezfitData.width = n_elements(xarray) 
		ezfitData.height = n_elements(yarray) 
		ezfitData.dim = 2

image2=*ezfitData.im ;(0:ezfitData.width-1,0:ezfitData.height - 1)
END

PRO ezfit_picktype,x,y
COMMON EZ_FIT_BLOCK,ezfitData,image
	CASE ezfitData.pick OF 
	0: BEGIN
	x=ezfitData.x(0:ezfitData.width-1)
	y=ezfitData.y(0:ezfitData.width-1)
	END
	1: BEGIN
	x=ezfitData.x(0:ezfitData.width-1)
	y=ezfitData.zx(0:ezfitData.width-1)
	END
	2: BEGIN
	x=ezfitData.y(0:ezfitData.height-1)
	y=ezfitData.zy(0:ezfitData.height-1)
	END
	ENDCASE
END

PRO EZFIT_PDMENU3_Event, Event
COMMON EZ_FIT_BLOCK,ezfitData,image

r = strpos(Event.Value,'File')
if ezfitData.dim eq 0 and r eq -1 then begin
	str=['You have first to use the File->Open 1D or 2D ...', $
		'to load in the data array.']
	res=widget_message(str,/info,title='EZ_FIT Info')
	return
end

  CASE Event.Value OF 

  'File.Open 1D ...': BEGIN
	F=DIALOG_PICKFILE(/READ,FILTER='*.bin1d',GET_PATH=P,PATH=ezfitData.inpath)
	if F eq '' then return
	ezfit_open1d,F,Event
	ezfitData.inpath = P
    END
  'File.Open ASCII ...': BEGIN
	w_readascii,inpath=ezfitData.inpath,GROUP = Event.top
    END
  'File.Open 2D ...': BEGIN
	F=DIALOG_PICKFILE(/READ,FILTER='*.bin',GET_PATH=P,PATH=ezfitData.inpath)
	if F eq '' then return
	ezfit_open2d,F,Event
	ezfitData.inpath = P
    END
  'File.2D XDR ...': BEGIN
	F=DIALOG_PICKFILE(/READ,FILTER='*.bin.xdr',GET_PATH=P,PATH=ezfitData.inpath)
	found = findfile(F)
	if found(0) ne '' then begin
		ezfit_get2DData,F,image2
;		if XRegistered('GETVECTOR_MAIN13') then $
;		WIDGET_CONTROL,ezfitData.base_getvector,/DESTROY
		ezfit_getvector,image2,GROUP=Event.Top
	endif else begin
		res=widget_message(F+ 'not found',/info,title='EZ_FIT Info', $
			DIALOG_PARENT=Event.id)
		return
	end
    END
  'File.Printer ...': BEGIN
	PS_printer,GROUP=Event.Top
    END
  'File.Quit': BEGIN
    WIDGET_CONTROL,Event.Top,/DESTROY
    END
  'GetData.VectorX': BEGIN
	if XRegistered('GETVECTOR_MAIN13') then begin
		o_win = !D.window
		WSET,ezfitData.image_area
		plot,ezfitData.x(0:ezfitData.width-1), $
		  thick=2,color=ezfitData.table_size-1, $
		  xtitle='Index', ytitle='X', title='X vs Index'
		WSET,o_win
	end
	ezfitData.pick=0
    END
  'GetData.VectorY': BEGIN
	if XRegistered('GETVECTOR_MAIN13') then begin
		o_win = !D.window
		WSET,ezfitData.image_area
		if ezfitData.dim eq 1 then begin
			ezfitData.y = image(0:ezfitData.width-1, $ 
				ezfitData.J_index)
		end
		plot,ezfitData.x(0:ezfitData.width-1), ezfitData.y, $
		  thick=2,color=ezfitData.table_size-1, $
		  xtitle='X', ytitle='Y', title='Y vs X'
		WSET,o_win
	end
	ezfitData.pick=0
    END
  'GetData.VectorZy': BEGIN
	if ezfitData.dim eq 2 then begin
		ezfit_get2DData,ezfitData.file,image
		ezfitData.zx = image(*,ezfitData.J_index)
	title='Z vs X @ Y='+strtrim(ezfitData.y(ezfitData.J_index),2)
	ezfitData.pick=1
        if XRegistered('POLYFITW_SETUP') then $
        WIDGET_CONTROL,ezfitData.polyfit_label, $
                SET_VALUE='Number of Elements: '+strtrim(ezfitData.width,2)
        if XRegistered('GETVECTOR_MAIN13') then begin
	o_win = !D.WINDOW
	WSET,ezfitData.image_area
	plot,ezfitData.x(0:ezfitData.width-1), ezfitData.zx, $
		  thick=2,color=ezfitData.table_size-1, $
		xtitle='X', ytitle='Z', title=title 
	WSET,o_win
        str = '       I    X Values         Zy Values'
        for i=0,ezfitData.width-1 do $
        str = [str,string(i, ezfitData.x(i), ezfitData.zx(i),/print)]
        WIDGET_CONTROL, ezfitData.text_area, SET_VALUE=str
	end
	end
    END
  'GetData.VectorZx': BEGIN
	if ezfitData.dim eq 2 then begin
		ezfit_get2DData,ezfitData.file,image
		ezfitData.zy = image(ezfitData.I_index,*)
	title='Z vs Y @ X='+strtrim(ezfitData.x(ezfitData.I_index),2)
        if XRegistered('POLYFITW_SETUP') then $
        WIDGET_CONTROL,ezfitData.polyfit_label, $
                SET_VALUE='Number of Elements: '+strtrim(ezfitData.height,2)
	ezfitData.pick=2
        if XRegistered('GETVECTOR_MAIN13') then begin
	o_win = !D.WINDOW
	WSET,ezfitData.image_area
	plot,ezfitData.y(0:ezfitData.height-1), ezfitData.zy, $
		  thick=2,color=ezfitData.table_size-1, $
		xtitle='Y', ytitle='Z', title=title 
	WSET,o_win
        str = '       I    Y Values         Zx Values'
        for i=0,ezfitData.height-1 do $
        str = [str,string(i, ezfitData.y(i), ezfitData.zy(i),/print)]
        WIDGET_CONTROL, ezfitData.text_area, SET_VALUE=str
        end
	end
    END
  'GetData.2DImage': BEGIN
	if XRegistered('GETVECTOR_MAIN13') then begin
		o_win = !D.WINDOW
		WSET,ezfitData.image_area
		TVSCL,congrid(image,ezfitData.TV_width,ezfitData.TV_height)
		WSET,o_win
	endif else begin
		sz = size(image)
		if sz(0) eq 1 then return
		ezfit_getvector,image,GROUP=Event.Top
	end
    END
  'Curve Fit.COMFIT.EXPONENTIAL': BEGIN
 	ezfit_picktype,x,y	
	comfitGraf,x,y,/exponential,/print,Group=Event.top
    END
  'Curve Fit.COMFIT.GEOMETRIC': BEGIN
 	ezfit_picktype,x,y	
	comfitGraf,x,y,/GEOMETRIC,/print,Group=Event.top
    END
  'Curve Fit.COMFIT.GOMPERTZ': BEGIN
 	ezfit_picktype,x,y	
	comfitGraf,x,y,/GOMPERTZ,/print,Group=Event.top
    END
  'Curve Fit.COMFIT.HYPERBOLIC': BEGIN
 	ezfit_picktype,x,y	
	comfitGraf,x,y,/HYPERBOLIC,/print,Group=Event.top
    END
  'Curve Fit.COMFIT.LOGISTIC': BEGIN
 	ezfit_picktype,x,y	
	comfitGraf,x,y,/LOGISTIC,/print,Group=Event.top
    END
  'Curve Fit.COMFIT.LOGSQUARE': BEGIN
 	ezfit_picktype,x,y	
	comfitGraf,x,y,/LOGSQUARE,/print,Group=Event.top
    END
  'Curve Fit.ERRORFFIT': BEGIN
	curvefit_setup,function_name='FUNCT_ERF',GROUP=Event.Top
    END
  'Curve Fit.CURVEFIT': BEGIN
	curvefit_setup,GROUP=Event.Top
; 	ezfit_picktype,x,y	
;	curvefitGraf,x,y,/print
    END
  'Curve Fit.GAUSSFIT': BEGIN
 	ezfit_picktype,x,y	
	gaussfitGraf,x,y,/print,GROUP=Event.top
    END
  'Curve Fit.LADFIT': BEGIN
 	ezfit_picktype,x,y	
	ladfitGraf,x,y,/print,Group=Event.top
    END
  'Curve Fit.LINFIT': BEGIN
 	ezfit_picktype,x,y	
	linfitGraf,x,y,/print,Group=Event.top
    END
  'Curve Fit.LORENTZIAN': BEGIN
 	ezfit_picktype,x,y	
	lorentzfitgraf,x,y,/print,Group=Event.top
    END
  'Curve Fit.POLYFITW': BEGIN
	ezfitData.polyfit=1
	polyfitwsetup,GROUP=Event.Top
    END
  'Curve Fit.POLY_FIT': BEGIN
	ezfitData.polyfit=0
	polyfitwsetup,GROUP=Event.Top
    END
;  'Curve Fit.REGRESS': BEGIN
;	regressfitGraf,x,y,/print
;    END
  'Curve Fit.SVDFIT': BEGIN
 	ezfit_picktype,x,y	
	svdfitsetup,GROUP=Event.Top
    END
  'Multi Fit.REGRESS': BEGIN
	regressfit,Group=Event.top
    END
  'Multi Fit.LORENTZIAN': BEGIN
 	ezfit_picktype,x,y	
	ez_fit2,20,x,y,GROUP=Event.Top ; ROI 20
    END
  'Help.COMFIT': BEGIN
    comfitGraf
    END
  'Help.CURVEFIT': BEGIN
    curvefitGraf 
    END
  'Help.ERRORF1FIT': BEGIN
    errorf1fit_help 
    END
  'Help.ERRORFFIT': BEGIN
    errorffit_help 
    END
  'Help.GAUSSFIT': BEGIN
    gaussfitGraf 
    END
  'Help.LADFIT': BEGIN
    ladfitGraf
    END
  'Help.LINFIT': BEGIN
    linfitGraf
    END
  'Help.LMFIT': BEGIN
    res=widget_message(' lmfitGraf not available yet.',title='FITTING Info',/info)
    END
  'Help.LORENTZIAN': BEGIN
    lorentzfitGraf
    END
  'Help.POLYFITW': BEGIN
    polyfitwGraf
    END
  'Help.POLY_FIT': BEGIN
    polyfitGraf
    END
  'Help.REGRESS': BEGIN
	regressfitGraf
    END
  'Help.SVDFIT': BEGIN
	svdfitGraf
    END
  'Help.COMMAND': BEGIN
	str=['Usage: ez_fit [[[,XARRAY=X ,YARRAY=Y] ,IM=image] , GROUP=group]', $
	'', $
	'KEYWORD:', $
	'   XARRAY, YARRAY  - only required if 1D data is entered directly form the', $
	'                     command line.', $
	'  IM               - in addition of XARRAY, YARRAY, IM is required if', $
	'                     2D image data is entered directly form the', $
	'                     command line.', $
	'  GROUP            - specifies the calling parent widget ID' $
	]
	res=widget_message(str,/info,title='EZ_FIT Help Info')
    END
  ENDCASE
END

PRO ezfit_open1d,F,Event
COMMON EZ_FIT_BLOCK,ezfitData,image

        if F eq '' then return
        found = findfile(F)
        if found(0) ne '' then begin
        ezfitData.dim = 1
        ezfitData.file = F
                ezfit_get1DData,F,image2
;                if XRegistered('GETVECTOR_MAIN13') then $
;                WIDGET_CONTROL,ezfitData.base_getvector,/DESTROY
                ezfit_getvector,image2,GROUP=Event.Top
        endif else begin
                res=widget_message(F+ 'not found',/info,title='EZ_FIT Info', $
                        DIALOG_PARENT=Event.id)
                return
        end

END

PRO ezfit_open2d,F,Event
COMMON EZ_FIT_BLOCK,ezfitData,image
        found = findfile(F)
        if found(0) ne '' then begin
                ezfit_get2DData,F,image2
                if XRegistered('GETVECTOR_MAIN13') then $
                WIDGET_CONTROL,ezfitData.base_getvector,/DESTROY
                ezfit_getvector,image2,GROUP=Event.Top
        endif else begin
                res=widget_message(F+ 'not found',/info,title='EZ_FIT Info', $
                        DIALOG_PARENT=Event.id)
                return
        end
END

PRO EZFIT_MAIN13_Event, Event


  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  ; Event for EZFIT_PDMENU3
  'EZFIT_PDMENU3': EZFIT_PDMENU3_Event, Event

  'EZFIT_FIELD3': BEGIN
      WIDGET_CONTROL,Event.Id,GET_VALUE=file
	ezfit_open1d,file(0),Event
      END
  'EZFIT_FIELD4': BEGIN
      WIDGET_CONTROL,Event.Id,GET_VALUE=file
	ezfit_open2d,file(0),Event
      END
  'EZFIT_FIELD5': BEGIN
      WIDGET_CONTROL,Event.Id,GET_VALUE=file
	w_readascii,file(0),GROUP = Event.top
      END
  'EZFIT_XREPORT': BEGIN
	 xdisplayfile,'fitting.rpt',TITLE='Fitting Results'
      END
  'EZFIT_CLOSE': BEGIN
    WIDGET_CONTROL,Event.Top,/DESTROY
    END

  ENDCASE
END



PRO ez_fit,xarray=xarray,yarray=yarray,im=im, GROUP=Group,jpick=jpick,ipick=ipick,inpath=inpath
;+
; NAME:
;       EZ_FIT
;
; PURPOSE:
;       This routine integrates all the IDL line fitting routines into one 
;       single package. It provides the IDL user with a very easy to use
;       line fitting tool. It is a standalone widget application. It can
;       be easily plug into any other IDL program.
;
;       It accepts 1D or 2D arrays either from the input binary file or 
;       directly from the command line. It also accepts the spreadsheet
;       column type ascii input file. 
;
;       It allows the user to select any vector from the input data and
;       perform any fitting he/she desires. It lets the user easily 
;       get the hardcopy of fitting graph and tabulated data.
;
; CATEGORY:
;	Curve fitting Widgets application.
;
; CALLING SEQUENCE:
;
;       EZ_FIT
;	
; KEYWORD PARAMETERS:
;       XARRAY:	Specifies the independent varialbe  X vector if input to be
;               entered from the command line.
;
;       YARRAY:	Specifies the dependent varialbe Y array if input to be 
;               entered from the command line. It is used by 1D or 2D data.
;
;               For 2D image data, the YARRAY represents the Y vector of the
;               second dimension.
;
;               For 1D data, multiple data vectors can be packed into Y array. 
;               The Y(N,M), the N must have the same dimension as in X vector.
;               the M represents the number of dependent variables in Y array.
;               A user can use the cursor in the image area to select the 
;               dependent variable to be fitted, or use the J text field
;               to specify the dependent variable to be fitted..
;               
;       IM:     Specifies the 2D image array corresponding to XARRAY and 
;               YARRAY. IMAGE(N,M), where dimenstion N is the same as the 
;               number of elements in XARRAY, M is the same as the number 
;               of elements in YARRAY.
;
;       IPICK:  Specifies the row # from the Y array or 2D image array to be
;               fitted initially.  
;
;       JPICK:  Specifies the column # from the Y array or 2D image array to be
;               fitted initially. JPICK supercedes IPICK. 
;
;       INPATH: Specifies the initial search path
;
;       GROUP:  The widget ID of the group leader of the widget. If this
;               keyword is specified, the death of the group leader results 
;               in the death of EZ_FIT.
;
; OUTPUTS:
;       Pops up plot and list window to show fitting results with respect 
;       to the input data.
;
; COMMON BLOCKS:
;       COMMON EZ_FIT_BLOCK,ezfitData,image
;
; SIDE EFFECTS:
;       It uses the PLOT1D and XDISPLAYFILE program to show the fitting results.
;
; RESTRICTIONS:
;       Input binary file must be created by the U_OPERW and U_WRITE rountines.
;       The input data is in platform independent XDR binary format. 
;
;       Default input file filter for 1D data is '*.bin1d', the default 
;       input file filter for 2D data is '*.bin'. User may override this
;       default setting in the file selection dialog.
;
;       Input 1D file should contains only two binary objects XARRAY and 
;       YARRAY.  Input 2D file should contains only 3 binary objects XARRAY 
;       YARRAY and IMAGE.
;    
;       Input ASCII type file should contain columns of input data. It
;       uses the W_READASCII to read in columns of data. The first column
;       will be independent variable, the remaining columns will be dependent
;       variables.
;        
; EXAMPLE:
;       
;       Example 1 - Use the File->Open to load input data
;
;	       EZ_FIT
;
;       Example 2 - Use the X,Y keywords to load 1D data 
;
;              EZ_FIT,XARRAY=X, YARRAY=Y
;
;       Example 3 - Use the keywords to load 2D data 
;
;              EZ_FIT,XARRAY=X, YARRAY=Y, IM=image
;
; MODIFICATION HISTORY:
; 	Written by:	Ben-chin K. Cha, 09-12-97.
;	02-27-98	Inherit color tables from the calling program
;	10-04-99	Incooporate the FUNCT_ERF fit to curvefitgraf
;	10-18-99	Add the support of weight factor   
;       01-06-00        Add the support of jpick, and ipick for image array
;       01-23-02        Upgrade the readascii user interface for 1D/2D data
;       03-01-02        R1.6
;                       Modify multiple Lorenzian ROI user interface
;       04-30-02        Fix GetData menu features with fittin.bin data
;                       Handle the case where the table_size is less than 256
;-

COMMON EZ_FIT_BLOCK,ezfitData,image

;if XRegistered('EZFIT_MAIN13') then $
;	WIDGET_CONTROL,ezfitData.base,/DESTROY

  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

if XRegistered('EZFIT_MAIN13') eq 0 then begin
 
;  get actual table size 
;        tvlct,r,g,b,/get
;        t_size = where(r eq 0 and g eq 0 and b eq 0)
;        table_size = 256 - n_elements(t_size)+1
	table_size = !d.table_size

ezfitData = { $
	file : '', $ ;	1D name: *.bin1d or   2D name: *.bin 
	inpath : '', $ 
	base : 0L, $
	table_size : table_size, $
	dim : 0, $		; 1 - 1d, 2 - 2d data
	base_getvector:0L, $
	image_area : 0, $       ; drawing are for getvector
	text_area : 0L, $
	I_field: 0L, $
	J_field: 0L, $
	xslider: 0L, $
	slider: 0L, $
	I_index:0, $
	J_index:0, $
	pick:0, $ ; 1D: 0 - Y vs X , 2D: 1 - Zy vs X  , 2 - Zx vs Y 
	polyfit:0, $	; 0 for polyfit, 1 for polyfitw
	polyfit_label:0L, $
	polyfit_ndfield: 0L,$
	polyfit_wffield: 0L,$
	polyfit_ndegree: 4, $
;	polyfit_factor: 1., $	; weighting factor
	polyfit_factor: '1.', $
	svdfit_base: 0L, $	; svdfit base setup
	svdfit_fname: 'svdfunct', $	; user supplied fname
	svdfit_nterm: 4, $	; no of terms in curve fitting
	svdfit_factor: 1., $	; weighting factor
	svdfit_legendre: 0, $	; if 1 legendre fit is used
	curvefit_noderiv: 0, $
;	curvefit_itmax: 20, $
;	curvefit_tol:1.e-3, $
;	curvefit_fname: 'funct', $
;	curvefit_A: make_array(20), $
	x : make_array(4000), $
	y : make_array(4000), $
	zx: make_array(4000), $
	zy : make_array(4000), $
	im : ptr_new(/allocate_heap), $  ;make_array(1000,1000), $
	TV_width: 300, $
	TV_height: 300, $
	x_mag: 1., $
	y_mag: 1., $
	width : 0, $
	height : 0 $
	}
end

if keyword_set(ipick) then begin
        ezfitData.I_index = ipick(0)
        ezfitData.pick = 2
end
if keyword_set(jpick) then begin
	ezfitData.J_index = jpick(0)
	ezfitData.pick = 1
end
cd,current=p
if keyword_set(inpath) then ezfitData.inpath = inpath else ezfitData.inpath=p

if keyword_set(im) then begin
	image = im
	*ezfitData.im = im
	sz=size(im)
	if keyword_set(xarray) eq 0 then xarray = indgen(sz(1))
	if keyword_set(yarray) eq 0 then yarray = indgen(sz(2))
	ezfit_init2D,xarray,yarray,image2
		ezfitData.zx = image(*,ezfitData.J_index)
		ezfitData.zy = image(ezfitData.I_index,*)
	if ezfitData.pick eq 0 then ezfitData.pick=1 ; pick column 0
endif else begin
	if keyword_set(yarray) then begin
	sz=size(yarray)
	if keyword_set(xarray) eq 0 then xarray = indgen(sz(1))
	ezfit_init1d,xarray,yarray
	end
end

if XRegistered('EZFIT_MAIN13') then return

  junk   = { CW_PDMENU_S, flags:0, name:'' }

os_init

  EZFIT_MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
      /COLUMN, $
      TITLE='EZ_FIT (R1.6)', $
      MAP=1, $
      UVALUE='EZFIT_MAIN13')
  ezfitData.base = EZFIT_MAIN13

  BASE2 = WIDGET_BASE(EZFIT_MAIN13, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE2')

  BASE3 = WIDGET_BASE(EZFIT_MAIN13, $
      COLUMN=1, $
      MAP=1, $
      UVALUE='BASE3')

  MenuDesc831 = [ $
      { CW_PDMENU_S,       1, 'File' }, $ ;        0
        { CW_PDMENU_S,       0, 'Open 1D ...' }, $ ;        1
        { CW_PDMENU_S,       0, 'Open 2D ...' }, $ ;        2
        { CW_PDMENU_S,       0, 'Open ASCII ...' }, $ ;        1
        { CW_PDMENU_S,       0, 'Printer ...' }, $ ;        3
        { CW_PDMENU_S,       2, 'Quit' }, $ ;        4
      { CW_PDMENU_S,       1, 'GetData' }, $ ;        5
        { CW_PDMENU_S,       0, 'VectorX' }, $ ;        6
        { CW_PDMENU_S,       0, 'VectorY' }, $ ;        6
        { CW_PDMENU_S,       0, 'VectorZy' }, $ ;        7
        { CW_PDMENU_S,       0, 'VectorZx' }, $ ;        7
        { CW_PDMENU_S,       2, '2DImage' }, $ ;        8
      { CW_PDMENU_S,       1, 'Curve Fit' }, $ ;        9
        { CW_PDMENU_S,       1, 'COMFIT' }, $ ;       10
          { CW_PDMENU_S,       0, 'EXPONENTIAL' }, $ ;        2
          { CW_PDMENU_S,       0, 'GEOMETRIC' }, $ ;        3
          { CW_PDMENU_S,       0, 'GOMPERTZ' }, $ ;        4
          { CW_PDMENU_S,       0, 'HYPERBOLIC' }, $ ;        5
          { CW_PDMENU_S,       0, 'LOGISTIC' }, $ ;        6
          { CW_PDMENU_S,       2, 'LOGSQUARE'}, $  ;      7
        { CW_PDMENU_S,       0, 'CURVEFIT' }, $ ;       11
        { CW_PDMENU_S,       0, 'ERRORFFIT' }, $ ;      11-1
        { CW_PDMENU_S,       0, 'GAUSSFIT' }, $ ;       12
        { CW_PDMENU_S,       0, 'LADFIT' }, $ ;       13
        { CW_PDMENU_S,       0, 'LINFIT' }, $ ;       14
        { CW_PDMENU_S,       0, 'POLYFITW' }, $ ;       15
        { CW_PDMENU_S,       0, 'POLY_FIT' }, $ ;       16
          { CW_PDMENU_S,       0, 'LORENTZIAN' }, $ ;        5
;        { CW_PDMENU_S,       0, 'REGRESS' }, $ ;       17
        { CW_PDMENU_S,       2, 'SVDFIT' }, $ ;       18
      { CW_PDMENU_S,       1, 'Multi Fit' }, $ ;        9
        { CW_PDMENU_S,       0, 'REGRESS' }, $ ;       17
          { CW_PDMENU_S,       2, 'LORENTZIAN' }, $ ;        5
      { CW_PDMENU_S,       3, 'Help' }, $ ;       19
        { CW_PDMENU_S,       0, 'COMMAND' }, $ ;       20
        { CW_PDMENU_S,       0, 'COMFIT' }, $ ;       20
        { CW_PDMENU_S,       0, 'CURVEFIT' }, $ ;       21
        { CW_PDMENU_S,       0, 'ERRORFFIT' }, $ ;       21
        { CW_PDMENU_S,       0, 'ERRORF1FIT' }, $ ;       21
        { CW_PDMENU_S,       0, 'GAUSSFIT' }, $ ;       22
        { CW_PDMENU_S,       0, 'LADFIT' }, $ ;       23
        { CW_PDMENU_S,       0, 'LINFIT' }, $ ;       24
        { CW_PDMENU_S,       0, 'LMFIT' }, $ ;       25
        { CW_PDMENU_S,       0, 'POLYFITW' }, $ ;       26
        { CW_PDMENU_S,       0, 'POLY_FIT' }, $ ;       27
          { CW_PDMENU_S,       0, 'LORENTZIAN' }, $ ;        5
        { CW_PDMENU_S,       0, 'REGRESS' }, $ ;       28
        { CW_PDMENU_S,       2, 'SVDFIT' } $  ;     29

  ]


  EZFIT_PDMENU3 = CW_PDMENU( BASE2, MenuDesc831, /RETURN_FULL_NAME, $
      UVALUE='EZFIT_PDMENU3')

  ezfit_close = WIDGET_BUTTON(BASE2,value='Close',UVALUE='EZFIT_CLOSE')
;  ezfit_report = WIDGET_BUTTON(BASE2,value='Fitting Results',UVALUE='EZFIT_XREPORT')

; 1D data : xarray, yarray
  FieldVal1367 = [ $
    'fitting.bin1d' ]
  EZFIT_FIELD3 = CW_FIELD( BASE3,VALUE=FieldVal1367, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='1D XDR File:', $
      XSIZE=60, $
      UVALUE='EZFIT_FIELD3')

; image data : xarray,yarray,image
  FieldVal1432 = [ $
    'fitting.bin' ]
  EZFIT_FIELD4 = CW_FIELD( BASE3,VALUE=FieldVal1432, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='2D XDR File:', $
      XSIZE=60, $
      UVALUE='EZFIT_FIELD4')

  FieldVal1433 = [ $
    '' ]
  EZFIT_FIELD5 = CW_FIELD( BASE3,VALUE=FieldVal1433, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='ASCII File:', $
      XSIZE=60, $
      UVALUE='EZFIT_FIELD5')

  WIDGET_CONTROL, EZFIT_MAIN13, /REALIZE

  XMANAGER, 'EZFIT_MAIN13', EZFIT_MAIN13
END

