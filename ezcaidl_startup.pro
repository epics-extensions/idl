device,pseudo=8
;device,decompose=0
;  assign color

if !d.n_colors gt 256 then device,decomposed=0
private_table = fix(getenv('IDL_NCOLORS'))
if private_table gt 0 and private_table lt 256 then window,colors = private_table,/pixmap
if !d.window ne -1 then wdelete

device,retain=2

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

print,'Run ezcaIDL ...'
.run ezcaIDL
print,'Run ezcaIDLWidgets ...'
.run ezcaIDLWidgets
caInit
caSetTimeout,0.001
print,caVersion()
