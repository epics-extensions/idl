;*************************************************************************
; Copyright (c) 2002 The University of Chicago, as Operator of Argonne
; National Laboratory.
; Copyright (c) 2002 The Regents of the University of California, as
; Operator of Los Alamos National Laboratory.
; This file is distributed subject to a Software License Agreement found
; in the file LICENSE that is included with this distribution. 
;*************************************************************************
PRO obj_clean,var,cType,all=all,id=id,find=find,class=class
;+
; NAME:
;	OBJ_CLEAN
; 
; PURPOSE:
;        This routine allows the user flexibly clean up the objects heap 
;        in the IDL memory.
;
; CALLING SEQUENCE:
;       OBJ_CLEAN[,Var[,cType]][,/FIND][,Class=class][,ID=id][,/ALL]
;
; INPUTS:
;  Var   - if specified, the object name found will be destroyed
;  cType - string specifies the Var class name, if specified,
;          the Var class is also checked before destroy
;
;KEYWORD PARAMETERS:
;  /FIND  - if specified, find all the objects in the heap
;  CLASS  - to specify the classType string, if specified with /FIND, 
;           find all the objects matched the classType;
;           if specified alone, all the matched objects will be destroyed.
;  /ALL   - if specified, all the objects in the heap will be destroyed
;  ID=#   - if specified, the #'th objects in the heap will be destroyed
;
; EXAMPLES:
;
;     Find all the objects defined in the heap:       OBJ_CLEAN,/FIND
;
;     Destroy all the objects defined in the heap:    OBJ_CLEAN,/ALL 
;
;     Destroy all the objects with the 'scan2d' type: OBJ_CLEAN, class='scan2d'
;
;     Destroy the third object from the heap:         OBJ_CLEAN,ID=3
;
;     Destroy the object variable V2 from the heap:   OBJ_CLEAN,V2
;        
;        
; MODIFICATION HISTORY:
;       Written by:     Ben-chin K. Cha, 03-12-99.
;-
	u = obj_valid(count=no)
	np = n_params()
	case np of 
	0: begin
		if keyword_set(find) then begin
			if no lt 1 then begin
				print,'No object found!'
				return
				end
			print,'Objects found:'
			if keyword_set(class) then begin
				for i=0,no-1 do begin
				if obj_isa(u(i),class) then print,u(i),i+1 
				end
			endif else for i=0,no-1 do print,u(i),i+1
			return
		endif else begin
			if keyword_set(class) then begin
				for i=0,no-1 do begin
				if obj_isa(u(i),class) then begin
					obj_destroy,u(i) 
					print,u(i)," has been destroyed!"
					end
				end
			return
			end 
		end
		if keyword_set(id) then begin
			if id gt 0 and id le no then begin
			obj_destroy,u(id-1)
			print,"The",id,"'th object",u(id-1)," has been destroyed!"
			return
			end
		end
		if keyword_set(all) then begin
			print,'Destroying all objects:'
			for i=0,no-1 do begin
				obj_destroy,u(i)
				print,u(i), ' been destroyed!'
			end
		endif else begin
			print,''
			print,'Usage: obj_clean[,Var[,cType]][,Class="..."][,/FIND][,ID=id][,/ALL]'
			print,''
			print,'        This routine allows the user flexibly clean up the objects heap '
			print,'        in the IDL memory.'
			print,''
			print,'INPUT'
			print,'  Var   - if specified, the object name found will be destroyed'
			print,'  cType - string specifies the Var class name, if specified,'
			print,'          the Var class is also checked before destroy'
			print,''
			print,'KEYWORD'
			print,'  /FIND - if specified, find all the objects in the heap'
			print,'  /ALL  - if specified, all the objects in the heap will be destroyed'
			print,"  ID=#  - if specified, the #'th objects in the heap will be destroyed"
			print,'  Class - specifies class type string to be destroyed' 
		return
		end
	   end
	1: begin
		if obj_valid(var) then begin
			obj_destroy,var
			print,'Object ',var,' destroyed!'
		end
	   end
	2: begin
		if obj_isa(var,cType) then begin
			obj_destroy,var
			print,'Object ',var,' destroyed!'
		endif else begin
			print,'Wrong class specified, not destroyed!'
		end
	   end
	else:
	endcase
END
