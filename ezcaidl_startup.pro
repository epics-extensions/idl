;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
device,pseudo=8
;device,decompose=0
;  assign color

if !d.n_colors gt 256 then device,decomposed=0
private_table = fix(getenv('IDL_NCOLORS'))
if private_table gt 0 and private_table lt 256 then window,colors = private_table,/pixmap
if !d.window ne -1 then wdelete

device,retain=2

private = getenv('EPICS_EXTENSIONS_PVT')
print,'private ',private

arch = getenv('HOST_ARCH')
if arch eq '' then arch = 'solaris-sparc'
!path= '/usr/local/epics/extensions/bin/'+arch+':'+!path
!path= '/usr/local/epics/extensions/idllib:'+!path
if private ne '' then $
!path= private + '/idllib:' +private+'/bin/'+arch+':'+ !path

private = getenv('EPICS_EXT_PVT')
if private ne '' then $
!path= private + '/idllib:' + !path

print,!path
;
!QUIET=1

@os.init

print,'Run ezcaIDL ...'
.run ezcaIDL
print,'Run ezcaIDLWidgets ...'
.run ezcaIDLWidgets
caInit
caSetTimeout,0.001
caPendIO,time=0.01,list_time=3.
print,caVersion()
