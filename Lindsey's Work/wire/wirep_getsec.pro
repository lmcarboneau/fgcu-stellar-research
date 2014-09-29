PRO wirep_getsec,maintarget,sec

 restore,'~/wire/wire_process/wire_secondary_info.idl'

 w = where(strmatch(datsec.wirename,'*'+maintarget+'*') eq 1,c)

 if c ge 1 then begin
  sec = datsec(w)

  print,'SimbadName','SpecType','Slot','V','B-V', $
    format='(A20,A10,A5,A5,A5)'
 
  for i=0,c-1 do $
   print,datsec(w(i)).simbadname,datsec(w(i)).spec,$
     datsec(w(i)).slot, datsec(w(i)).v, datsec(w(i)).bv, $
     format = '(A20,A10,I5, F5.1, F5.2)'


 endif else begin
  print," %%% Main target not found ... try eg. wirep_getsec,'Cen',sec"  
  sec = -1
  RETURN
 endelse


END
