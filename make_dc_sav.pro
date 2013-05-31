; This IDL script makes DC.sav, to run under IDL Virtual Machine
; This script must be invoked from the IDL command line with:
; @make_dc_save 
@os.init
.compile ezcaIDL
.compile ezcaIDLWidgets
.compile DC
filenames = ['DC', 'xdr_open']
resolve_routine, filenames, /either, /compile_full_file, /no_recompile
resolve_all
;itresolve
save, /routine, file='DC.sav'
