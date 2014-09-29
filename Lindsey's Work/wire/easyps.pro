PRO easyps, keywords, easystore, nops=nops, dops=dops, dir=dir, fil=fil, dim=dim, $
 prepare=prepare, close=close, help=help, silent=silent, $
 landscape=landscape

default9, landscape, 0B
default9, nops, 0B
default9, dops, 1B
if nops eq 1 then dops = 0B

default9, prepare, 1B  ; open a new ps file
default9, close, 0B    ; close the ps file (keywords required)
if close then prepare = 0B

spawnrob,'hostname',host
if strmatch(host,'*manowar*') then gvprogram = 'gv'

default9, gvprogram, 'ggv'

default9, help, 1B       ; give instructions on how to show ps file
default9, silent, 0B     ; give no output
if silent then help = 0B

if prepare then keywords = 'perparing easyps.pro

if n_elements(keywords) eq 0 then begin
 print,' *** keywords not supplied ... aborting ... '
 RETURN
endif

if dops then begin

if prepare then begin
   default9, dir, '/home/bruntt/papers/psplot/'
   default9, fil, 'dummy.ps'
   
   default9, dim, [20, 10, -1, -1] ; x,y size
   if dim(2) lt 0 then dim(2) = (21.5 - float(dim(0))) * 0.5
   if dim(3) lt 0 then dim(3) = (30.5 - float(dim(1))) * 0.5

;; print, ' %%% dim = ',dim

   fil = strcompress(fil,/remove_all)

   keywords = PSConfig(Cancel=cancelled, Filename=fil, $
    /European, directory=dir, landscape=landscape, $
    xsize=dim(0), ysize=dim(1), xoffset=dim(2), yoffset=dim(3))

    IF cancelled THEN begin
      print,' *** No ps file name given (setting dops = 0)'
      dops = 0B
      RETURN
    ENDIF

    easystore = {thisDevice:'',thisFont:0L}
    easystore.thisDevice = !D.Name
    easystore.thisFont   = !P.Font
 
;    thisDevice = !D.Name ; original values
;    thisFont   = !P.Font ; original values

    !P.Font = -1
    Set_Plot, 'PS'
    if keywords.color then col=getcolor(/load)
    Device, _Extra=keywords
    if keywords.color then col=getcolor(/load)

    col=getcolor(/load)

endif



if close then begin
      Device, /Close_File
      Set_Plot, easystore.thisDevice
      !P.Font = easystore.thisFont   
      set_plot,'x'
   if help then print,' $  ' + gvprogram + ' ' + keywords.filename + '  & '
endif



endif

END
