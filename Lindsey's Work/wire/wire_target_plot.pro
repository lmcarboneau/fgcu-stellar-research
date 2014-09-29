; Make MV vs B-V plot of all wire targets

 dops = 1
 nameson = 2

 sectarg = 1 ; plot the identified secondary targets ?
 ; modesec = 'prep_for_wire_read_sec' ; make output ready for a SIMBAD list ... 
                                    ;  ---> program wire_read_sec.pro
 modesec = '' ; regular plot

 v_magn = ''
; v_magn = 'raw' ; plot raw V - magnitudes rather than M_V (using parallax)
 
 mark_procyon = 1B ; mark procyon with an arrow?

 if dops eq 1 then begin
   outname_ps = '/ai40/bruntt/wire/wire_eps/targets_'+string(nameson,format='(I1)') + $
     string(sectarg,format='(I1)')   + '.ps'

   set_plot,'ps'
   x = 20 & y = 25
   x = 18 & y = 18
   device, xsize=x, ysize=y-0.4, yoffset=26.9-y, filename=outname_ps

 endif

 wire_target_info, '/ai40/bruntt/wire/wire_process/targets_wire.txt', info

 restore,'~/wire/wire_essential/wire_sec_info.idl' ; 09 NOV 2004

 mv = info.v + 5. - 5 * alog10(1e3/info.pax) ; abs. magn, ignoring B.C. & A_V
 if v_magn eq 'raw' then mv = info.v

; Specify y-range of plot:
 y1 = 5.5 & y2 = -5.5 & ytt = 'M!IV!N'
 if v_magn eq 'raw' then begin
   y1 = 7.5 & y2 = -0.5 & ytt = 'V'
 endif

 plot,info.b - info.v,mv,psym=2,ysty=1,xsty=1,xr=[-0.3,1.9],yr=[y1,y2],/nodata,$
  xthick=2,ythick=2,charsi=1.5,charthick=2,$
  xtit='B - V', ytit=ytt
 plotsym,0,/fill
 oplot,info.b - info.v,mv, psym=8

 ns = n_elements(info)

 ind = ['Theta','Kappa','kappa','Beta','beta','Alpha','alpha','alf','Lambda','gamma','Gamma','epsilon','Epsilon','delta','Ksi','Zeta','Nu','Sigma']
 sub = ['!4t!3','!4j!3','!4j!3','!4b!3','!4b!3','!4a!3','!4a!3','!4a!3','!4k!3','!4c!3','!4c!3','!4e!3','!4e!3','4d!3','!4n!3','!4f!3','!4m!3','!4r!3']
 ni = n_elements(ind)

if nameson ge 1 then $
 for i=0,ns-1 do begin

   obj = info(i).object
   out = obj
  
   for j=0,ni-1 do begin
     if strmatch(obj,'*'+ind(j)+'*') eq 1 then begin

      len = strlen(ind(j))
      len2 = strlen(obj)
      out = sub(j) + ' ' + strmid(obj,len,len2-len+1)

     endif
   endfor


   yoff = 0.
   xoff = 0.

   if obj eq 'HR1910' then yoff    = -0.15
   if obj eq 'AltairA7V' then yoff = 0.3
   if obj eq 'AC' then yoff        = 0.15
   if obj eq 'betaLeo' then xoff   = -0.25
   if obj eq 'alphaCen' then yoff  = 0.15
   if obj eq 'NSV9189' then xoff   = -0.3
   if obj eq 'AltairA7V' then xoff = -0.15
   if obj eq 'AltairA7V' then yoff = 0.2

   if obj eq 'Theta2Tau' then out = '!4h!3!E2!N!4s!3'
   if obj eq 'AltairA7V' then out = '!3Altair'
   if obj eq 'HR5698F8V' then out = '!3HR5698'
   if obj eq 'HR6596F5V' then out = '!3HR6596'
   if obj eq 'ProcyonF5IV-V' then out = 'Procyon'
   if obj eq 'AC' then out = '!4a!3 Cir'

   if v_magn eq 'raw' then begin
     xoff = 0. & yoff = 0.
   endif

  xout = info(i).b - info(i).v + 0.04 + xoff
  yout = mv(i) + 0.04+yoff
  
  if yout lt y1 and yout gt y2 then $
  xyouts, xout, yout, out, $
   charsi=0.8,alignment = 0,charthick=2

  if mark_procyon and out eq 'Procyon' then $
    arrow,.2,3.5,info(i).b - info(i).v -0.03 + xoff,mv(i) + 0.1+yoff,/data,thick=3

 endfor

; xyouts,-.4,6,'!4abcdefghijklmnopqrstuvwxyz!3'

msun = 4.77
bvsun = 0.4 ; my guess!

plotsym,0
plots,bvsun,msun,psym=8,symsize=3
xyouts,bvsun-.1,msun,alignment=1.0,'Sun',charsi=2




if sectarg eq 1 then begin

; plot,info.ra2,info.dec2,psym=2
; oplot,xyinfo.extra(4,0),xyinfo.extra(5,0),psym=4,symsi=2

secfile = '~bruntt/wire/wire_process/wire_secondary_info.idl'
a = findfile(secfile,Count=ccx)
if ccx eq 1 then begin
 restore,secfile ; ---> datsec structure
 mv_sec = datsec.v + 5. - 5 * alog10(1e3/datsec.pax) ; abs. magn, ignoring B.C. & A_V
 if v_magn eq 'raw' then mv_sec = datsec.v 


 plotsym,8 ; open symbols
 oplot,datsec.bv, mv_sec, psym=8

 nsec = n_elements(datsec)
 for jj=0,nsec-1 do begin
  outname = strcompress(datsec(jj).simbadname,/remove_all)
  if nameson ge 2 then $
   xyouts,datsec(jj).b - datsec(jj).v + 0.04 ,mv_sec(jj) + 0.04, outname, /data, $
    charsi= 0.7
 endfor

endif

wire_red_setup, position_file, target_file

if modesec eq 'prep_for_wire_read_sec' then begin

   restore,position_file ; get wireinfo + xyinfo
   w = where(wireinfo.object ne '',ns)
   for i=0,ns-1 do begin
   
   obj_temp = xyinfo(w(i)).object
   
   ; Detect main target in info struture
   dist = (info.ra2  - xyinfo(w(i)).extra(4,0))^2.0 + $
          (info.dec2 - xyinfo(w(i)).extra(5,0))^2.0
   distlim = 0.5 & distlim = distlim^2.0
   wd = where(dist lt distlim,cdist)
   if cdist ne 1 then goto, nomatch
   wd = wd(0)
   object_name_base = info(wd).object
   
   
   wsec = where(strmatch(obj_temp,'* & *') eq 1,csec)
   for sec=0,csec-1 do begin

     object = xyinfo(w(i)).object(wsec(sec))
     object2 = strsplit(object,'&',/extract)
     object2 = object2(0)
    
     if strmid(object2,0,4) eq 'NAME' then $
      object2 = strmid(object2,4,200)
    
     if strmid(object2,0,2) eq 'V*' then $
      object2 = strmid(object2,2,200)
    
     if strmid(object2,0,1) eq '*' then $
      object2 = strmid(object2,1,200)
    
     if strmid(object2,0,3) eq 'SV*' then $
      object2 = strmid(object2,3,200)
    
     print,object2,object_name_base,wsec(sec),format='(A15,A15,I3)' 

   endfor


   nomatch:

   endfor


endif ; prepare mode?



endif ; plot / handle secondary stars ?




 if dops eq 1 then begin
  device,/close
  set_plot,'x'
  print,' $ ggv ' + outname_ps + ' & '
 endif


end
