; Merge all wire data files...

wire_target_info, '~/wire/targets_wire.txt', info

ni = n_elements(info)

; istart = 10L
istart = 23L

print, $
 ' %%% I will process these stars (next prg to use is : wire_getpos.pro)
for i=istart,ni-1 do $
 print,info(i).object 


; =======================================================================

for i=istart,ni-1 do begin

target = info(i).object

; if strmatch(target,'*Altair*') eq 1 or $
;    strmatch(target,'*NSV4058*') eq 1 then begin
; print,' *** Already reduced: ',target
; goto,already_done
; endif

basedir = '/data2/bruntt/wire/dat/'+target+'/data/'
; wire_all,basedir ; read all wire data ... may take several hours !!
spawn,'ls -1 '+basedir + '*.idl',wfl
nwfl = n_elements(wfl)
nwfl2 = round(nwfl * 0.5) - 1

add2 = 0
if nwfl gt 6 then add2 = 1
if nwfl gt 10 then add2 = 2
if nwfl gt 20 then add2 = 3



if wfl(0) eq '' then begin
 print,' %% Apparently star '+target+' was not run thourgh wire_all.pro'
 goto,already_done
endif

; Now choose the wire*.idl file in the middle (t0)
 choose_ref = wfl(nwfl2)

if nwfl ge 5 then $
 choose_ref = [wfl(1),wfl(nwfl-1-1)]

if nwfl ge 7 then $
 choose_ref = [wfl(add2), wfl(nwfl2), wfl(nwfl-1-add2)]


print,''
print,' %%% Reference wire files that will be processed: ' 
for jjj=0,n_elements(choose_ref)-1 do print, choose_ref(jjj)
print,''

; Reduce a single WIRE restore file (choose_ref):
wire_pos,choose_ref,1,'','' ; do all apertures for one file!

already_done:

endfor


end

; Use this reduction to get x,y pos of each star, FWHM, t0 etc. etc. (~10-15 min)
;wire_getpos,$
; choose_ref+'1x',target,$
; '/data1/bruntt/wire/xy_positions.idl', '~/wire/targets_wire.txt'
;; The new information (x,y,FWHM...) is stored in the 
;; xy_positions.idl restore file! Measured error on magnitude & FWHM
;; is used to remove bad data points (currently, there is 10-sigma tolerance)

;; Now reduce all wire restore files -- put only with the best aperture!
;spawn,'ls -1 '+basedir + '/data_wire_*.idl',fil
;wire_pos,fil,1,'/data1/bruntt/wire/xy_positions.idl',target
;wire_merge3,basedir + '/data_wire_*.idl1x', target, $
;            '/data1/bruntt/wire/xy_positions.idl'

