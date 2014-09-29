PRO wireg,file,every=every,debug=debug,$
          eclipsing=eclipsing,freq_unit=freq_unit,$
          import_red=import_red

default9, import_red, 1B

a = findfile(file,Count=cnt)
if cnt ne 1 then begin
 print,' %%% File not found: ' + file
 RETURN
endif

if n_elements(every) eq 0 then begin
 every = 5
 spawnrob,'hostname',host
 wmm = where(strmatch(host,'*thales*') eq 1,cmm) ; Heather's machine == slow!
 if cmm eq 1 then every = 15
endif
if n_elements(debug) eq 0 then debug = 1

default9, eclipsing, 0B
default9, freq_unit, 'c/day'

if freq_unit ne 'c/day' and freq_unit ne 'milliHz' and freq_unit ne 'microHz' then begin
 print,' %%% Frequency unit not available ...'
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
 s2 = strsplit(s(1),'.',/extract)
 field2 = s2(0)

 if field2 ne 'merged' then begin
  wa = where(strmatch(wireinfo.object,'*' + field1 + '*' + field2 + '*') eq 1,ca)
 endif else stop

 if ca ne 1 then begin
  print,' %%% Unique object name not found using fields: ' + $
   field1 + ' & ' + field2

  ; wa = where(strmatch(wireinfo.object,'*' + field1 + '*') eq 1,ca)
  stop

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
 basedir = '~/wire/wire_process/' ; default location

 spawnrob,'hostname',host ; get the hostname of the computer
 whost = where(strmatch(host,'*brunttlt*') eq 1,chost_laptop)
 if chost_laptop then basedir = '/mnt/WinC/linux/wire_process/' 

 whost2 = where(strmatch(host,'*manowar*') eq 1,chost_laptop9200)
 if chost_laptop9200 then basedir = '/home/bruntt/wire_process/' 

 whost99 = where(strmatch(host,'*taurus*') eq 1,chost_taurus)
 if chost_taurus then basedir = '/home/bruntt/wire_process/' 

 whost98 = where(strmatch(host,'*leo*') eq 1,chost_leo)
 if chost_leo then basedir = '/home/bruntt/wire_process/' 

 whost98 = where(strmatch(host,'brixx.physics.usyd.edu.au') eq 1,chost_brixx)
 if chost_brixx then basedir = '/export/brixx1/bruntt/wire_process/' 

 if import_red then $
 redfile = findfile(basedir + '*' + $
                     field1 + '*' + field2 + '*',Count=cntfile) else begin
 redfile = 'do not import'
   cntfile = 0B
 endelse


 if cntfile ne 1 then begin
  if cntfile ge 2 then begin
    print,' %%% More than one reduction file found: ' 
    for jk=0,cntfile-1 do $
     print,strcompress(jk)+': ' + redfile(jk)
    print,' %%% Choose one of them ... '
    s9 = get_kbrd(1) &    redfile = redfile(s9) &    cntfile = 1
  endif else begin

   hitme,mess=' %%% No reduction file found ... hit any key',xx
   if xx eq 'x' then stop
  endelse

  endif




; =================================================================
; Find the HD numbers of the stars!
; =================================================================

; stop

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
  w2 = where(strmatch(wireinfo.object,'*'+field1+'*'+field2+'*') eq 1,c2)

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

; At least one object was identified?
   if cok ge 1 then begin
     for o=0,cok-1 do begin
          gp = strsplit(all_obj(o),'&',/extract) & c_gp = n_elements(gp)
          obj_search = gp(0)

          wt = where(wireobj.nam eq obj_search,ct) ; < 16 Feb '05
          wt = where(strmatch(wireobj.nam,'*'+obj_search+'*') eq 1,ct) ; 17 Feb 2004

          if ct ne 1 then begin
           print,' %%% Not found (update with wire_simbad.pro ?): ' + obj_search,ct
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
;; if nstar ne cok then begin ; < 1. juni 2005
if cok eq 0 then begin        ; > 1. juni 2005
 print, ' %%% All targets were not identified ... ' 
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
   hitme,mess=' %%% 1:1 match in wireg failed ... hit x to stop: ',s1
   if s1 eq 'x' then stop
;   wiredat = -1
   
  endif

endelse
; =================================================================


; =================================================================
print,'' & print,' %%% Input objects: ' & print, ''
for i=0,nstar-1 do print,strcompress(i)+': ' + objects(i)
help,wiredat
print,''

; print,' %%% Waiting three seconds ... ' & wait,3
; =================================================================

; =================================================================
; Now we're ready to launch wirep.pro widget!
; =================================================================
 reduction_file = redfile(0)
 if reduction_file ne '' then begin
  print,' %%% Using reduction file: ' + reduction_file
  wirep,wireult,file,target,debug,f=reduction_file,objects=objects,wiredat=wiredat,$
   every=every,eclipsing=eclipsing,thefreq_unit=freq_unit
 endif else begin
   print,' %%% Not using a reduction file!'
   wirep,wireult,file,target,debug,objects=objects,wiredat=wiredat,$
    every=every,eclipsing=eclipsing,thefreq_unit=freq_unit
 endelse

END


