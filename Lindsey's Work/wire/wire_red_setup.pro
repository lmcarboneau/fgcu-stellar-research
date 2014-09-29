PRO wire_red_setup, position_file, target_file, secinfo=secinfo

spawnrob,'hostname',host
nhost = n_elements(host)
if nhost ge 2 then begin
 print,' %%% Warning: computer lost directory: '
 for i=0,nhost-1 do print, host(i)
 hitme,s
 host = host(nhost-1)
endif

if strmatch(host,'*usafa*') eq 1 then begin
 position_file = '~/wire/wire_essential/xy_positions4.idl'
 ;; position_file = '~/wire/wire_essential/xy_positions4_betacma.idl' ; debug CMa
 target_file   = '~/wire/wire_essential/targets_wire.txt'
 secinfo       = '~/wire/wire_essential/wire_sec_info.idl' ; VIZIER/SIMBAD
endif

if strmatch(host,'*brunttlt*') eq 1 then begin
 ad = '/mnt/WinC/linux/'
 position_file = ad+'/wire/wire_essential/xy_positions4.idl'
 target_file   = ad+'/wire/wire_essential/targets_wire.txt'
 secinfo       = ad+'/wire/wire_essential/wire_sec_info.idl' ; VIZIER/SIMBAD
endif

if strmatch(host,'*manowar*') eq 1 then begin
 ad = '/home/bruntt/'
 position_file = ad+'/wire/wire_essential/xy_positions4.idl'
 target_file   = ad+'/wire/wire_essential/targets_wire.txt'
 secinfo       = ad+'/wire/wire_essential/wire_sec_info.idl' ; VIZIER/SIMBAD
endif

if strmatch(host,'brixx.physics.usyd.edu.au') eq 1 then begin
 ad = '/export/brixx1/bruntt'
 position_file = ad+'/wire/wire_essential/xy_positions4.idl'
 target_file   = ad+'/wire/wire_essential/targets_wire.txt'
 secinfo       = ad+'/wire/wire_essential/wire_sec_info.idl' ; VIZIER/SIMBAD
endif

spawnrob,'whoami',whoami
if strmatch(whoami,'rahmi*') eq 1 then begin
 ad = '/import/suphys2/bruntt'
 position_file = ad+'/wire/wire_essential/xy_positions4.idl'
 target_file   = ad+'/wire/wire_essential/targets_wire.txt'
 secinfo       = ad+'/wire/wire_essential/wire_sec_info.idl' ; VIZIER/SIMBAD
endif

if strmatch(host,'*taurus*') eq 1 then begin
 ad = '/home/bruntt/'
 position_file = ad+'/wire/wire_essential/xy_positions4.idl'
 target_file   = ad+'/wire/wire_essential/targets_wire.txt'
 secinfo       = ad+'/wire/wire_essential/wire_sec_info.idl' ; VIZIER/SIMBAD
endif

if strmatch(host,'*leo*') eq 1 then begin
 ad = '/home/bruntt/'
 position_file = ad+'/wire/wire_essential/xy_positions4.idl'
 target_file   = ad+'/wire/wire_essential/targets_wire.txt'
 secinfo       = ad+'/wire/wire_essential/wire_sec_info.idl' ; VIZIER/SIMBAD
endif

if strmatch(host,'*lynx*') eq 1 then begin
 ad = '/home/bruntt/'
 position_file = ad+'/wire/wire_essential/xy_positions4.idl'
 target_file   = ad+'/wire/wire_essential/targets_wire.txt'
 secinfo       = ad+'/wire/wire_essential/wire_sec_info.idl' ; VIZIER/SIMBAD
endif

if strmatch(host,'*phys.au*') eq 1 then begin ; updated in 8000 on 10 MAR 2005
 position_file = '/ai40/bruntt/wire/wire_essential/xy_positions4.idl'
 target_file   = '/ai40/bruntt/wire/wire_essential/targets_wire.txt'
 secinfo       = '/ai40/bruntt/wire/wire_essential/wire_sec_info.idl' ;VIZIER/SIMBAD
endif

if n_elements(position_file) eq 0 then begin
 print,' *** Host name not found in wire_red_setup.pro !! '
 stop
endif

END
