;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
pro memtest
  i=2047L
  catch,error
  if (error ne 0) then begin
    i=i-1
  endif
  a=ptr_new(bytarr(i*2L^20L,/nozero),/no_copy)
  print, 'Maximum size array that IDL can allocate: ',i,' Mb'
  ptr_free, a
end
