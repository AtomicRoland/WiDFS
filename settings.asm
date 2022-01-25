uart=&FC30              ; Base Address of UART
pagereg=&FCFF           ; Page register
pageram=&FD00           ; Paged RAM base address
pr_buffer=&18           ; Startpage in paged RAM for buffering
mounttab=pageram+&08    ; four bytes with drive mount status
cachetab=mounttab+&04   ; cache information 2 or 8 bytes per drive (t.b.d.)

_URL_DRIVE0_=&01        ; paged ram page with path to drive 0 image
_URL_DRIVE1_=&02        ; paged ram page with path to drive 1 image
_URL_DRIVE2_=&03        ; paged ram page with path to drive 2 image
_URL_DRIVE3_=&04        ; paged ram page with path to drive 3 image

; pages &05-&0E are used for DFS workspace

_CAT_DRIVE0_=&0F        ; cached catalog drive 0
_CAT_DRIVE1_=&11        ; cached catalog drive 1
_CAT_DRIVE2_=&13        ; cached catalog drive 2
_CAT_DRIVE3_=&14        ; cached catalog drive 3
_CONTEXT_=&17           ; page &A save space
_BLOCKSIZE_=2048        ; HTTP POST block size (max = 2048)

line=&F2
zp=&B0                  ; Work space
bank_save=zp            ; 1 byte
pr24pad=zp+1            ; 1 byte workspace for print24
timer=&FDFD             ; 3 bytes timer counter (not in main memory for speed)
time_out=zp+4           ; 1 byte timer duration
datalen=zp+5            ; 2 bytes data length
url_path_pointer=zp+7   ; 1 byte pointer to path in get function
paramblock_ptr=zp+8     ; 2 byte parameter block pointer to zero page
paramblock = zp+10      ; 4 byte parameter block

errorspace=&100         ; temp error routine
savespace=&100          ; some space to temporary save data
workspace=&110          ; temp work space
strbuf=&120             ; string buffer for WiFi commands
size=workspace+3        ; size of search string, 1 byte
httpstatus=workspace+4  ; pointer to http status code, 1 byte
writestatus=workspace+5 ; write status from server, 1 byte
get_or_post=workspace+9 ; get or post command
save_x=workspace-1      ; short time save space X-reg
save_y=workspace-2      ; short time save space Y-reg
blocksize=workspace+6   ; named workspace, 3 bytes
heap=workspace+9        ; more workspace

load_addr = &BE         ; load address set by DFS

; mounttab holds status of mounted image:
; &00 -> not mounted
; &01 -> mounted read/write
; &81 -> mounted read only