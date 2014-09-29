; 15. MAR 2004 (c) Hans Bruntt

; Reduce stars of all data ... this prg. will
; then PRINT the lines you need to excute in IDL:

; REQUIRED: You must have a few .idl1x restore file for each star
;           You need this to find reference x,y pos for each star
;           .. and to select the best aperture size for each star
;           Set get_xy_pos = 1 to get lines you need to do this

; To do the final reduction (but before merging flux data points)
; set get_fin_red = 1

get_lun,u


; Choose mode
get_xy_pos = 0 ; launch wire_getpos ?
do_fin_red = 1 ; reduce with wire_flat (after wire_getpos !!)
ttt = 'aarhus'
 ttt = 'usafa'

posfile = '~/wire/targets_wire.txt'
wire_target_info, posfile, info
a = reverse(sort(info.v))
info = info(a)

outfile = '~bruntt/wire/process_' + $
 string(get_xy_pos,format='(I1)') + '_' + $
 string(do_fin_red,format='(I1)') + '_' + $
 ttt + '.txt'
openw,u,outfile

ni = n_elements(info)

; istart = 10L
istart = 0.

print, $
 ' %%% You must launch these commands:'

comm = '======================================================================='


for i=istart,ni-1 do begin
 newformat = 0B
 target = info(i).object 
  basedir = '/data2/bruntt/wire/dat/'+target+'/'
  spawn,'ls -1 '+basedir + 'data/*.idl1x',wfl

 if wfl(0) eq '' then begin ; check for new format 
   basedir = '/data1/bruntt/wire/'+target+'/'
   spawn,'ls -1 '+basedir + '*.idl1x',wfl
   newformat = 1B
   if wfl(0) eq '' then begin
      print, ' %%% No *.idl1x files for: '+target
      goto,skip_star
   endif
 endif


 printf,u,comm

; Basic information
  if ttt eq 'usafa' then begin
   basedir2 = '/data2/bruntt/wire/dat/'+target+'/'
   if newformat eq 1 then basedir2 = '/data1/bruntt/wire/'+target + '/'
  endif
  if ttt eq 'aarhus' then $
   basedir2 = '/ai40/bruntt/wire/'+target+'/'

 printf,u," basedir = '" + basedir2 + "'  &   target  = '" + target  + $
   "' ; V = " + string(info(i).v,format='(F4.1)')

if get_xy_pos EQ 1 then begin ; launch wire_getpos ?

; Now choose the wire*.idl file in the middle (t0)
 printf,u,"spawn,'ls -1 '+basedir + 'data/data_wire_*.idl1x' ,choose_ref else 
 

; Reduce a single WIRE restore file (choose_ref):
; printf,u,"wire_pos,choose_ref(1),1,'','' ; do all apertures for one wire file..."

; Use this reduction to get x,y pos of each star, FWHM, t0 etc. etc. (~10-15 min)
 printf,u,"wire_getpos,choose_ref(1),target, $"
 ; if ttt eq 'usafa' then $
  printf,u,"  '/data1/bruntt/wire/xy_positions2.idl', '~/wire/targets_wire.txt'"

; The new information (x,y,FWHM...) is stored in the xy_positions.idl restore file! 
; Now reduce all wire restore files -- put only with the best aperture!

endif

if do_fin_red EQ 1 then begin ; reduce with wire_flat (after wire_getpos !!)

 if ttt eq 'usafa' then begin
  printf,u,"spawn,'ls -1 '+basedir + 'data/data_wire_*.idl', fil"
  printf,u,"wire_flat,fil,1,'/data1/bruntt/wire/xy_positions2.idl',target,'',0"
 endif

 if ttt eq 'aarhus' then begin
  printf,u,"spawn,'ls -1 '+basedir + 'data_wire_*.idl', fil"
  printf,u,"wire_flat,fil,1,'/ai39/bruntt/wire/xy_positions2.idl',target,'',0"
 endif

; aarhus:
; basedir = '/ai40/bruntt/wire/HD113226/'  &   target  = 'HD113226' ; V =  2.8
;spawn,'ls -1 '+basedir + 'data_wire_*.idl', fil
;wire_flat,fil,1,'/ai39/bruntt/wire/xy_positions2.idl',target


; printf,u,"wire_merge3,basedir + '/data_wire_*.idl1x', target, $
;  '/data1/bruntt/wire/xy_positions2.idl'"

endif

skip_star:

endfor

close,u
free_lun,u

print,'Output file: '+outfile

; =======================================================================


END
