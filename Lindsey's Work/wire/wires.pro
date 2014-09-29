PRO wires,file

a = findfile(file,Count=cnt)
if cnt ne 1 then begin
 print,' %%% File not found: ' + file
 RETURN
endif

; =================================================================
; Restore unique object information
; =================================================================
 wire_red_setup,position_file,target_file,secinfo=secinfo
 restore,position_file
 wire_target_info, target_file, info
 restore,secinfo ; secondary star info, eg. star names!

 g  = strsplit(file,'/',/extract) & ng = n_elements(g)
 nam = g(ng-1)
 s = strsplit(nam,'_',/extract)
 field1 = s(0)
 field2 = s(1)

 if field2 ne 'merged' then begin
  wa = $
   where(strmatch(wireinfo.object,'*' + field1 + '*' + field2 + '*') eq 1,ca)
 endif else stop

 if ca ne 1 then begin
  print,' %%% Unique object name not found using fields: ' + $
   field1 + ' & ' + field2
  if ca ge 2 then begin
    print,' %%% Matches found: '
    for jj=0,ca-1 do print,strcompress(jj)+': ' + wireinfo(wa(jj)).object
     print,' %%% Choose one of these objects ... '
    s8 = get_kbrd(1) &    wa = wa(s8) &    ca = 1
  endif else print,' %%% No matches found ... '
 endif

 if ca ne 1 then begin
  print, '' 
  print, ' %%% No unique object found ... Aborting!' & print, ''
  RETURN
 endif

; =================================================================
; Look for reduction files
; =================================================================
 basedir = '~/wire/wire_process/'
 redfile = findfile(basedir + '*' + $
                     field1 + '*' + field2 + '*',Count=cntfile)

 if cntfile ne 1 then begin
  if cntfile ge 2 then begin
    print,' %%% More than one reduction file found: ' 
    for jk=0,cntfile-1 do $
     print,strcompress(jk)+': ' + redfile(jk)
    print,' %%% Choose one of them ... '
    s9 = get_kbrd(1) &    redfile = redfile(s9) &    cntfile = 1
  endif else print,' %%% No reduction file found ... '  
 endif


; =================================================================
; Find the HD numbers of the stars!
; =================================================================
 restore,file  &  target = wireinfo(wa).object
 cok = 0
 nstar = n_elements(wireult(0).mag)
 objects = strarr(nstar)

 wg = where(strmatch(info.object,'*'+field1+'*') eq 1,cg)
 if cg ge 2 then begin
  print,' %%% Several object names matches: '
  for l=0,cg-1 do print,strcompress(l)+': ' + $
                     info(wg(l)).object
  s7 = get_kbrd(1) & wg = wg(s7) & cg = 1
 endif
 if cg ne 1 then begin
  print,' %%% Identifying secondary objects failed ... '
 endif else begin
  object = info(wg).object
  w2 = where(strmatch(wireinfo.object,'*'+object+'*') eq 1,c2)

  if c2 ge 2 then begin
   print,' %%% Several object names matches: '
   for m=0,c2-1 do print,strcompress(m)+': ' + $
                      wireinfo(w2(m)).object
   s6 = get_kbrd(1) & w2 = w2(s6) & c2 = 1
  endif
  
  if c2 ne 1 then begin
   print,' %%% Did not find object ('+object+') in wireinfo structure!'
   stop
  endif
 
  print,''

   all_obj = xyinfo(w2).object
   wok = where(all_obj ne '' and all_obj ne 'Unknown',cok)

   if cok ge 1 then begin
     for o=0,cok-1 do begin
          gp = strsplit(all_obj(o),'&',/extract) & c_gp = n_elements(gp)
          obj_search = gp(0)
          wt = where(wireobj.nam eq obj_search,ct)
          if ct ne 1 then begin
           print,' %%% Not found (update with wire_simbad?): ' + obj_search,ct
           objects(o) = 'Unknown'
           if o eq 0 then objects(o) = strcompress(target,/remove_all)
          endif else $
           objects(o) = strcompress(wireobj(wt).hd,/remove_all)
     endfor    
   endif else begin
    print,' %%% Objects not identified (use wire_auto_field.pro): '
    print,' %%% ',all_obj
   endelse

 endelse


; =================================================================
; Store ALL information about the targets!!
; =================================================================
wiredat = -1
if nstar ne cok then begin
 print, ' %%% Targets not identified ... ' 
 objects(0) = strcompress(target,/remove_all)
 if nstar ge 2 then  objects(1:nstar-1) = 'Unknown' 
endif else begin 
 wiredat = wireobj(0:nstar-1) & wiredat.nam = 'Unknown' & wiredat.hd = -1
 match = 0B

 for i=0,nstar-1 do begin
  wp = where(wireobj.hd eq objects(i),c_wp)
  if c_wp eq 1 then begin
    wiredat(i) = wireobj(wp)  
    match = match + 1
  endif
 endfor

  if match ne nstar then begin
   print,' %%% 1:1 match in wireg failed ... '
   wiredat = -1
  endif

endelse
; =================================================================


; =================================================================
print,'' & print,' %%% Input objects: ' & print, ''
for i=0,nstar-1 do print,strcompress(i)+': ' + objects(i)
help,wiredat
print,''

print,' %%% Waiting three seconds ... ' & wait,3
; =================================================================

; =================================================================
; Now we're ready to launch wirep.pro widget!
; =================================================================
 reduction_file = redfile(0)
 if reduction_file ne '' then begin
  print,' %%% Using reduction file: ' + reduction_file
  wirep,wireult,file,target,0,f=reduction_file,objects=objects,wiredat=wiredat
 endif else begin
   print,' %%% Not using a reduction file!'
   wirep,wireult,file,target,0,objects=objects,wiredat=wiredat
 endelse

END
