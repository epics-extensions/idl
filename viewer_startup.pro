;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
  device,pseudo=8
; device,decompose=0

;  assign color

if !d.n_colors gt 256 then device,decomposed=0
private_table = fix(getenv('IDL_NCOLORS'))
if private_table gt 0 and private_table lt 256 then window,colors = private_table,/pixmap
if !d.window ne -1 then wdelete

arch = getenv('HOST_ARCH')
private = getenv('EPICS_EXTENSIONS_PVT')
print,'arch ',arch
print,'private ',private

!path= '/usr/local/epics/extensions/bin/'+arch+':'+!path
if private ne '' then $
!path= private + '/bin/'+arch + ':' + !path

private = getenv('EPICS_EXT_PVT')
if private ne '' then $
!path= private + '/bin/'+arch + '/idl:' + !path

print,!path
;
!QUIET=1

@os.init
device,bypass_translation=0         ;  required for 24 bit device
