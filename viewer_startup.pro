;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
;device,pseudo=8
;device,decomposed=1		; 24 bit

;  assign color

private_table = fix(getenv('IDL_NCOLORS'))
if private_table gt 0 and private_table lt 256 then window,colors = private_table,/pixmap
if !d.window ne -1 then wdelete

!path= getenv('EPICS_EXTENSIONS')+'/bin/solaris:'+!path
!path= getenv('EPICS_EXTENSIONS')+'/idllib:'+!path
private = getenv('EPICS_EXTENSIONS_PVT')
print,'private ',private

if private ne '' then $
!path= private + '/idllib:' + !path

private = getenv('EPICS_EXT_PVT')
if private ne '' then $
!path= private + '/idllib:' + !path

; print,!path
;
!QUIET=1

@os.init
