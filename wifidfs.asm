include "settings.asm"
include "serial.asm"
include "errors.asm"

.cmd_mount_list
    jsr set_bank_1
    ldx #0                  ; load pointer
    ldy #1                  ; page pointer
    stx pagereg             ; select page
.cmd_mount_list_l1
    txa                     ; print drive number
    jsr prtHexA4bit
    lda #':'
    jsr osasci
    lda mounttab,x          ; load mount status
    beq cmd_mount_list_l4   ; jump if not mounted
    sty pagereg             ; select page
    ldy #0                  ; reset pointer
.cmd_mount_list_l2
    lda pageram,y           ; load character
    cmp #&0D                ; test end of string
    beq cmd_mount_list_l3   ; jump if end of string
    jsr osasci              ; print it
    iny                     ; increment pointer
    bne cmd_mount_list_l2   ; process next character
.cmd_mount_list_l3
    ldy pagereg             ; restore page pointer
    lda #0                  ; restore page register
    sta pagereg
    jsr prtStr              ; print string
    equs " (R",&EA
    lda mounttab,x
    bpl cmd_mount_list_l3a
    lda #'W'
    jsr osasci
.cmd_mount_list_l3a
    jsr prtStr
    equs ")",&0D,&EA
    bne cmd_mount_list_l5   ; continue loop
.cmd_mount_list_l4
    jsr prtStr              ; print string
    equs " (not mounted)",&0D,&EA
.cmd_mount_list_l5
    iny                     ; increment page pointer
    inx                     ; increment drive number
    cpx #4                  ; all drives printed?
    bne cmd_mount_list_l1   ; no, then do the next one
    rts

.CMD_MOUNT
    cld
    jsr skipspace1          ; initialize string with space separator
    cmp #&0D
    beq cmd_mount_list      ; jump for mount list if no parameter
    sec                     ; substract 48
    sbc #'0'
    cmp #4                  ; test valid number
    bmi cmd_mount_l1        ; jump if < 4 (valid)
    jmp errBadDrv           ; throw an error
.cmd_mount_l1
    pha                     ; save drive number
    jsr set_bank_1
;    jsr save_context        ; save page &A00
    tax                     ; update mount table, x holds the drive number
    lda #0                  ; unmount drive...
    sta pagereg             ; select page 0 in paged ram
    sta mounttab,x          ; ... because the new url might be invalid
    jsr copyURL             ; copy the URL to paged ram
.cmd_mount_l2
    jsr testURL             ; check if the URL is valid
    bcs errBADURL           ; throw an error if it is not valid
    pla                     ; url is valid and reachable, update mount table
    tax                     ; copy to index
    lda writestatus         ; load write status
    cmp #1                  ; check for status
    beq cmd_mount_l3        ; if then the image is writable
    lda #&01                ; load with &01 for read only access
    bne cmd_mount_tab
.cmd_mount_l3
    lda #&81                ; load with &81 for read/write access
.cmd_mount_tab
    sta mounttab,x          ; set drive status

.cmd_mount_end
 ;   jmp restore_context     ; restore page &A00
    rts

.CMD_UMOUNT
    cld
;    jsr save_context
    jsr set_bank_1
    jsr skipspace1          ; initialize string with space separater
    cmp #&0D
    bne cmd_umount_l2
.cmd_umount_l1
    jmp errBadDrv
.cmd_umount_l2
    sec                     ; substract 48
    sbc #'0'
    cmp #4                  ; test valid number
    bpl cmd_umount_l1       ; jump if > 3 (invalid)
    tax                     ; transfer drive number to index
    lda #0                  ; load unmount status
    sta pagereg             ; select page 0
    beq cmd_mount_tab       ; jumps always

.errBADURL
\    jsr set_bank_0          ; restore bank setting
	ldx #(error_bad_url-error_table)
	jmp error

; Initialize the WiDFS workspace if it's not already initialized.
.initWFS
\    jsr set_bank_1          \ select paged ram bank 1
    jsr testWFS             \ test if WiDFS is already initialized
    beq initWFS_l4          \ jump if initialized

    ldy #4                  \ clear the first five pages
.initWFS_l1  
    sty pagereg             \ select page
    ldx #0                  \ reset pointer
    txa                     \ clear accu
.initWFS_l2
    sta pageram,x           \ write to memory
    inx                     \ increment pointer
    bne initWFS_l2          \ jump if not all cleared
    dey                     \ decrement page register
    bpl initWFS_l1

    ldx #0                  \ write signature to the first bytes
.initWFS_l3
    lda WFSignature,x
    sta pageram,x
    inx                     \ increment pointer
    cmp #&0D          
    bne initWFS_l3
.initWFS_l4
    rts
\    jmp set_bank_0          \ restore paged ram bank 0 and return

; Test if WiDFS is active or initialized. On return:
;   A and X are destroyed
;   Y is preserved
;   Z = 0: WiDFS active
;   Z = 1: WiDFS is not active
.testWFS                    \ test if WiDFS is active
    ldx #0                  \ reset pointer
    stx pagereg             \ select first page
.testWFS_l1
    lda WFSignature,x       \ load character
    cmp pageram,x           \ compare with check bytes
    bne testWFS_l2          \ jump if not equal
    inx                     \ increment pointer
    cmp #&0D                \ check end of signature
    bne testWFS_l1          \ jmp if not end
.testWFS_l2
    rts                     \ return

.protocol
    equs "http://"
.WFSignature
    equs "WiDFS",&0D

; on entry X is drive number
; A, X and Y are preserved
.copyURL
    pha                     \ save registers
    txa
    pha
    inx                     \ increase drive number (i.e. calculate page number)
    stx pagereg             \ switch to page
    ldx #0                  \ initialize pointer
.copy_url_l1
    jsr skipspace           \ read character
    sta pageram,x           \ store in memory
    inx
    beq copy_url_end        \ maximum reached
    cmp #&0D                \ test end of url
    bne copy_url_l1         \ no, get next character
.copy_url_end
    ldx #0                  \ switch back to page 0
    stx pagereg
    pla                     \ restore registers
    tax
    pla
    rts

; Test if the url is valid and the images exists on the server
; on entry: x is drive number
; on exit:  X is preserved, A and Y are undefined
;           C = 0  -> ok
;           C = 1  -> error
.testURL
    lda #'P'                \ set parameter for POST method (only for mounting the image)
    bne test_url_l0
.wifi_read_catalog
    lda #'G'                \ set parameter for GET method
.test_url_l0
    sta get_or_post
    stx save_x              \ save drive number
    inx                     \ select page with URL to image file
    stx pagereg
    ldy #6                  \ init pointer
.test_url_l1
    lda pageram,y           \ load byte from url
    ora #&20                \ convert to lower case, note: / and : have bit 6 set
    cmp protocol,y          \ compare to protocol string
    bne testURL_error       \ if not equal then jump to error
    dey                     \ decrement pointer
    bpl test_url_l1         \ jump if there are more characters
    lda pagereg             \ load current page register
    pha                     \ save it
    jsr wifi_openconn_start \ read a block of data, page register is already set up
    pla                     \ restore page register
    sta pagereg
    lda #0                  \ set range start (0)
    sta workspace+2
    sta workspace+1
    sta workspace+0
    sta workspace+5         \ set range end (511)
    lda #&01
    sta workspace+4
    lda #&FF
    sta workspace+3
\    jsr wifi_clear_buffer   \ reset pointer to start of buffer and clear buffer
    ldx #LO(workspace)
    ldy #HI(workspace)
    jsr wifi_get_cmd        \ execute the HTTP POST for downloading catalog
    jsr wifi_get_write_status
    jsr wifi_closeconn      \ close the connection to the server
    jsr wifi_reset_buffer   \ reset pointer to start of buffer
    jsr wifi_search_ipd     \ search the first +IPD string
    bcc testURL_error       \ jump if not found
    jsr wifi_http_status    \ check the response status
    jsr wifi_search_crlf    \ search the end of the headers (double CR+LF)
    bcc testURL_error       \ jump if not found
    \ Now we have received and checked the response. Any errors should be trapped
    \ in the previous routines. So now it's time to copy the received catalog to
    \ the paged ram. This is always a 512 byte block.
    \ Copying this data is a bit complicated because the data has to be moved
    \ in the paged ram. This involves quite a lot of page switching.
    lda save_x              \ load drive number
    clc                     \ clear carry
    asl a                   \ multiply by 2 (for 2 pages per catalog)
    adc #_CAT_DRIVE0_       \ add the start page of catalogs
    sta zpm0                \ set the page register to the catalog
    ldy #0                  \ reset pointer
    jsr readcat
    inc zpm0
    jsr readcat

    clc                     \ clear error flag
    bcc testURL_end         \ jump always
.testURL_error
    sec                     \ set error flag
.testURL_end
    php
    ldx #0                  \ restore page register to first page
    stx pagereg
    ldx save_x
    plp
    rts

.readcat
    jsr wifi_read_buffer    \ read byte from buffer
    sta zpm1                \ save in zero page
    lda pagereg             \ save page register of received data
    pha
    lda zpm0                \ set page register to catalog
    sta pagereg
    lda zpm1                \ load received data from zeropage
    sta pageram,y           \ store in catalog
    pla                     \ restore page register to receive buffer
    sta pagereg
    iny                     \ increment pointer
    bne readcat             \ jmp if first block not complete
    rts

\ This routine reads characters from the command line and returns after the first
\ non-space character. The Y register points to this character. The accu holds the
\ first non-space character.
.skipspace          iny                     \ increment pointer
.skipspace1         lda (line),y            \ load character
                    cmp #&20                \ is it a space
                    beq skipspace           \ yes, read next character
                    rts                     \ it's not a space, return


\ Read a block of data for the image in drive X
.wificmd_cipstart
    equs "AT+CIPSTART="
    equb '"'
    equs "TCP"
    equb '"',','

.wificmd_cipstart_close
    equb '"'
    equs ",80",&0D,&0A    \ for now only port 80 is supported

.wificmd_cipclose
    equs "AT+CIPCLOSE", &0D, &0A

.wifi_is_disabled
  ldx #(error_disabled-error_table)
  jmp error

\ Open connection to the server. On entry X holds the drive number. All
\ registers will be destroyed on exit.
.wifi_openconn
    inx                         \ increment drive number for page
    stx pagereg                 \ set in page register
.wifi_openconn_start
    jsr test_wifi_ena           \ test if WiFi is enabled
    bne wifi_is_disabled        \ jump if not enabled
    jsr init_uart               \ set up the uart
    ldy pagereg                 \ save page register
    jsr wifi_ate0               \ set echo off
    sty pagereg                 \ restore page register
    ldy #0                      \ reset pointer

.wifi_openconn_l1
    lda wificmd_cipstart,y
    sta strbuf,y                \ store in buffer space
    iny                         \ increment pointer
    cmp #','                    \ test for end of string
    bne wifi_openconn_l1        \ jump if more characters
    lda #'"'                    \ open quote
    sta strbuf,y                \ store in buffer
    iny                         \ increment buffer
    jsr wifi_hostname_strbuf    \ copy the hostname to string buffer
    inx                         \ increment pointer to next character
    stx url_path_pointer        \ store start position of path
.wifi_openconn_l3
    ldx #0                      \ reset pointer for terminating the string
.wifi_openconn_l4
    lda wificmd_cipstart_close,x
    sta strbuf,y
    inx                         \ increment pointers
    iny
    cmp #&0A                    \ test for end of string
    bne wifi_openconn_l4        \ jump if more characters

    jsr wifi_clear_buffer       \ reset pointer to start of buffer and clear buffer
    lda #16                     \ load (long) time out to catch DNS errors
    sta time_out
    jsr wifi_send_strbuf        \ send command to ESP device

.wifi_openconn_response
    jsr uart_read_response      \ read the response from the UART
    jsr wifi_reset_buffer       \ reset buffer pointer
    lda pageram+&B              \ check for OK response (Normal response is: CONNECT crlf crlf OK crlf crlf)
    cmp #'O'
    bne wifi_openconn_err       \ It's not OK, so throw an error
    lda pageram+&C              \ check second character, just to be sure
    cmp #'K'
    beq wifi_openconn_ok        \ It's OK, continue
.wifi_openconn_err
    jsr wifi_reset_buffer       \ reset buffer pointer to print error message from device
    jsr wifi_prtstr             \ print error string
    ldx #(error_opencon-error_table)
    jmp error

.wifi_openconn_ok
    rts                         \ that's it for now....  Y points to the first slash

.wifi_closeconn
    ldy #0                      \ reset pointer
.wifi_closeconn_l1
    lda wificmd_cipclose,y
    sta strbuf,y                \ store in buffer space
    iny                         \ increment pointer
    cmp #&0A                    \ test for end of string
    bne wifi_closeconn_l1       \ jump if more characters
    jsr wifi_send_strbuf        \ send the close command
    rts

\ Copy the host name to the string buffer at position Y
\ On exit, A will be preserved, X points to the first / after the hostname,
\ Y points to next position in strbuf
.wifi_hostname_strbuf
    pha                         \ save A
    ldx #7                      \ load pointer to hostname in paged RAM
.wifi_hostname_strbuf_1
    lda pageram,x               \ load character of hostname
    cmp #'/'                    \ check for end of hostname
    beq wifi_hostname_strbuf_2  \ jump if end of hostname
    sta strbuf,y                \ store in string buffer
    iny                         \ increment pointers
    inx
    bne wifi_hostname_strbuf_1  \ jump if not zero (prevents endless loop)
.wifi_hostname_strbuf_2
    pla                         \ save A
    rts                         \ return

\ Build the get HTTP get command. The request header will look like this:
\ GET /<url> HTTP/1.1
\ Host: <the host name>
\ Range: bytes=<begin>-<end>
\ crlf
\ On entry: X = LSB parameter block, Y = MSB parameter block
\   p0,p1,p2 -> start position in file
\   p3,p4,p5 -> end position in file
\   get_or_post is 'G' (for GET) or 'P' (for POST) method

.http_header_start
.http_header_get    equs "GET /"
.http_header_1_1    equs " HTTP/1.1"
.http_header_host   equb &0D,&0A : equs "Host: "
.http_header_ua     equb &0D,&0A : equs "User-agent: WiDFS 1.0"
.http_header_range  equb &0D,&0A : equs "Range: bytes="
.http_header_end

.wifi_get_cmd
    stx paramblock              \ save address of parameter block
    sty paramblock+1
    ldy #0                      \ reset pointer to start of strbuf
    ldx #0                      \ load index
    jsr wifi_set_get_or_post
    ldx url_path_pointer        \ load pointer to path
    jsr wifi_url_strbuf         \ copy url to string buffer
    ldx #0                      \ reset index
.wifi_get_l2                    \ copy HTTP/1.1 and HOST to string buffer
    lda http_header_1_1,x       \ read data
    sta strbuf,y                \ write to string buffer
    iny                         \ increment pointers
    inx
    cpx #(http_header_ua-http_header_1_1)
    bne wifi_get_l2
    jsr wifi_hostname_strbuf    \ copy hostname to string buffer
    ldx #0                      \ reset index
.wifi_get_l3
    lda http_header_ua,x        \ read data
    sta strbuf,y                \ write to string buffer
    iny                         \ increment pointers
    inx
    cpx #(http_header_end-http_header_ua)
    bne wifi_get_l3
    jsr wifi_range_strbuf       \ copy range to string buffer
    lda #&0D                    \ terminate string buffer (i.e. request header)
    sta strbuf,y                \ write twice to string buffer
    sta strbuf+2,y
    lda #&0A
    sta strbuf+1,y
    sta strbuf+3,y
    jsr mINY4                   \ increment Y -> Y is length of data in strbuf
    tya                         \ save Y
    pha
    jsr wifi_ipsend             \ submit IPSEND command
    pla                         \ restore Y
    tay                         \ I really miss the PHY and PHX instructions :-(
    lda #4
    sta time_out
    jsr wifi_send_strbuf        \ send the request command to the ESP device
    jsr uart_get_response
    jmp wifi_readresponse_l1

.wifi_readresponse
    lda #4                      \ load time out value
.wifi_readresponse_to
    sta time_out                \ set time out
    jsr uart_read_response      \ read the response from the UART
.wifi_readresponse_l1
    stx datalen                 \ save LSB end of data
    lda pagereg                 \   it's not the length in bytes
    sta datalen+1               \   but the end address of the buffer
    rts                         \ return to calling routine

\ Copy the url to the string buffer.
\ On entry: x points to start of URL in paged RAM
\           y points to position in strbuf
\ On exit:  x points to end of URL in paged RAM
\           y points to next position in strbuf
.wifi_url_strbuf
    lda pageram,x               \ read character
    sta strbuf,y                \ store in string buffer
    cmp #&0D                    \ test for end of string
    beq wifi_url_end            \ if not end, then continue
    inx                         \ increment pointers
    iny
    bne wifi_url_strbuf         \ prevent endless loop
.wifi_url_end                   \ note: Y points to &0D character. This is correct!
    rts                         \ end of routine

.wifi_reset_buffer
    ldx #pr_buffer              \ load startpage of buffer
    stx pagereg                 \ write to page register
    ldx #0                      \ reset pointer to start of page
    rts                         \ return to calling routine

.wifi_clear_buffer
    pha
    jsr wifi_reset_buffer
    txa
.wifi_clear_buffer_l1
    sta pageram,x
    inx
    bne wifi_clear_buffer_l1
    pla
    rts

\ This routine sends the WiFi AT+IPSEND=<len> command to the ESP device. This string
\ will be placed at the position indicated by the Y register. This may be after a block
\ of data.
\ On entry: Y = pointer to strbuf (might be end of existing data)
\ On exit: A, X and Y are not modified
.wifi_ipsend
    pha                         \ save accu
    tya                         \ save Y register (start of IPSEND command)
    pha
    txa                         \ save X register
    pha
    ldx #0                      \ reset pointer to command string
    sty workspace               \ write length of strbuf to workspace (will be destroyed by prtdec24)
    sty save_y                  \ save pointer for later use
    stx workspace+1             \ we need this later for pr24dec
    stx workspace+2
.wifi_ipsend_l1
    lda wifi_ipsend_cmd,x       \ read character from command string
    sta strbuf,y                \ write to strbuf
    inx                         \ increment pointers
    iny
    cmp #'='                    \ test for last character
    bne wifi_ipsend_l1          \ jump if not last character
    lda #LO(workspace)          \ set pointer to 24 bit hex value (content length)
    sta paramblock
    lda #HI(workspace)
    sta paramblock+1
    ldx #0                      \ reset pointer
    jsr prdec24                 \ convert content length to ascii string
    lda #&0D                    \ close the command string
    sta strbuf,y
    iny
    lda #&0A
    sta strbuf,y                \ Y points now to the end of the IPSEND string
    \ Now start transmitting the IPSEND command to the ESP8266
    ldy save_y                  \ reset pointer to beginning of IPSEND string
.wifi_ipsend_l2
    lda strbuf,y                \ read character
    jsr send_byte               \ send it
    iny                         \ increment pointer
    cmp #&0A                    \ test for end of string
    bne wifi_ipsend_l2          \ jump if more characters to send
    lda #4                      \ load time out value
    sta time_out                \ set time out
    jsr uart_read_response      \ read the response from the UART and return
    pla                         \ restore registers
    tax
    pla
    tay
    pla
    rts

.wifi_ipsend_cmd
    equs "AT+CIPSEND=256",&0D,&0A

\ This routine prints a text string until the &0D character is encountered. It returns
\ after the calling jsr instruction. This routine will usually be called to print text
\ strings from the ESP8266 response.
\.print_string
.wifi_prtstr        jsr wifi_read_buffer     \ read character
                    beq wifi_prtstr_end      \ on end of buffer also end routine
                    jsr osasci               \ print it
                    cmp #&0D                 \ test end of string
                    bne wifi_prtstr          \ continue for next character
.wifi_prtstr_end    rts                      \ end of routine

\ Reads a character from the paged ram buffer at position X
\ returns the character in A and the X register points
\ to the next data byte.
.wifi_read_buffer
     lda pageram,x
     php
.wifi_read_buffer_inc
     inx
     bne wifi_read_buffer_end
     jsr wifi_inc_page_reg
.wifi_read_buffer_end
     plp
     rts

\ Writes a character to the paged ram buffer at position X
\ returns with X pointing to the next byte
.wifi_write_buffer
     php
     sta pageram,x
     jmp wifi_read_buffer_inc

\ Sends a CRLF to the ESP8266
.wifi_send_crlf
     lda #&0D
     jsr send_byte
     lda #&0A
     jmp send_byte

\ Increments the paged ram register and sets the (X) pointer to the beginning of the page.
\ If the end of paged ram has been reached then the page register will roll over from &FF
\ to &00 and the Z flag is set. The pageregister and X will not be updated and the routine
\ returns with Z=1. The calling routine can test this flag for the end of buffer.
.wifi_inc_page_reg
     ldx pagereg            \ load page register
     inx                    \ increment the value
     beq wifi_buffer_end    \ if it becomes zero then the end of the buffer (paged ram) is reached
     stx pagereg            \ write back to page register (i.e. select next page)
     ldx #0                 \ reset y register
     cpx #1                 \ clears Z-flag
.wifi_buffer_end
     rts                    \ return to store routine

\ Sends the data in the string buffer to the ESP device. The length
\ of the data is in Y.
\ No registers are changed
.wifi_send_strbuf
     pha                    \ save A
     txa                    \ save X
     pha
     tya                    \ save Y
     pha
     ldx #0                 \ reset pointer
.wifi_send_cmd_1
     lda strbuf,x           \ load character from string buffer
     jsr send_byte          \ send to ESP device
     inx                    \ increment pointer
     dey                    \ decrement counter
     bne wifi_send_cmd_1    \ jump if more characters
.wifi_send_end
     pla                    \ restore Y
     tay
     pla                    \ restore X
     tax
     pla                    \ restore A
     rts                    \ return to calling routine

\ Writes the 2 x 24 bit hex values in (paramblock) to strbuf. It also writes a leading
\ '0' to the strbuf because prdec24 gives no output if the value is 0. A leading zero
\ is ignored by the webserver so it won't harm us.
\ On entry, Y points to position in strbuf
\           X points to start relative to workspace
\ On exit, Y points to end of strbuf
.wifi_range_strbuf
    jsr wifi_leading_zero   \ add the leading zero
    ldx #0                  \ load offset
    jsr prdec24             \ write start position of range to strbuf
    lda #'-'                \ load dash
    jsr wifi_leading_zero+2 \ add to strbuf
    jsr wifi_leading_zero   \ add the next leading zero
    ldx #3                  \ load next offset
    jsr prdec24             \ write end position of range to strbuf
    rts                     \ return

\ Write the content length to strbuf
\ On entry, Y points to position in strbuf
\           X points to start relative to workspace
\ On exit, Y points to end of strbuf

.wifi_set_conlenth
    jsr wifi_leading_zero   \ add a leading zero
    ldx #6                  \ load offset to length
    jmp prdec24             \ write content length to strbuf and return

\ Add leading zero
.wifi_leading_zero
    lda #'0'
    sta strbuf,y
    iny
    rts

\ print hex value in ascii digits
\ code from mdfs.net - j.g.harston
\ On entry: X is offset to 24 bit value from (paramblock)
\           Y is pointer to strbuf
\ On exit:  Y is pointer to next free position in strbuf
\           A and X are destroyed
.prdec24
 sty workspace+9                    \ save Y
 txa                                \ move X to Y
 tay
 lda (paramblock),Y
 sta workspace+0
 iny
 lda (paramblock),Y
 sta workspace+1
 iny
 lda (paramblock),Y
 sta workspace+2
 ldy #21
 lda #0
 sta pr24pad
.prdec24lp1
 ldx #&ff
 sec
.prdec24lp2
 lda workspace+0
 sbc prdec24tens+0,y
 sta workspace+0
 lda workspace+1
 sbc prdec24tens+1,y
 sta workspace+1
 lda workspace+2
 sbc prdec24tens+2,y
 sta workspace+2
 inx
 bcs prdec24lp2
 lda workspace+0
 adc prdec24tens+0,y
 sta workspace+0
 lda workspace+1
 adc prdec24tens+1,y
 sta workspace+1
 lda workspace+2
 adc prdec24tens+2,y
 sta workspace+2
 txa
 bne prdec24digit
 lda pr24pad
 bne prdec24print
 beq prdec24next
.prdec24digit
 ldx #'0'
 stx pr24pad
 ora #'0'
 .prdec24print
 ldx workspace+9
 sta strbuf,x
 inc workspace+9
 .prdec24next
 dey
 dey
 dey
 bpl prdec24lp1
 ldy workspace+9
 rts
.prdec24tens
   EQUW 1       :EQUB 1 DIV 65536
   EQUW 10      :EQUB 10 DIV 65536
   EQUW 100     :EQUB 100 DIV 65536
   EQUW 1000    :EQUB 1000 DIV 65536
   EQUW 10000   :EQUB 10000 DIV 65536
   EQUW 100000 MOD 65535    :EQUB 100000 DIV 65536
   EQUW 1000000 MOD 65535   :EQUB 1000000 DIV 65536
   EQUW 10000000 MOD 65535  :EQUB 10000000 DIV 65536

.printRegs
    php
    pha
    jsr prtHexA8bit
    txa
    jsr prtHexA8bit
    tya
    jsr prtHexA8bit
    jsr osnewl
    pla
    plp
    rts

\ Just wait a short moment by calling OSBYTE 19 two times
.wait
                    pha
                    txa
                    pha
                    tya
                    pha
                    lda #19 : jsr osbyte
                    lda #19 : jsr osbyte
                    lda #19 : jsr osbyte
                    pla
                    tay
                    pla
                    tax
                    pla
                    rts

\ Send ATEO command
.ate0   equs "ATE0",&0D,&0A
.wifi_ate0
    pha
    txa
    pha
    tya
    pha
    ldx #0
.wifi_ate0_1
    lda ate0,x
    jsr send_byte
    inx
    cmp #&0A
    bne wifi_ate0_1
    lda #4
    sta time_out
    jsr uart_read_response
    pla
    tay
    pla
    tax
    pla
    rts

.prtWorkSpace
    lda workspace+0               \ ***DEBUG***
    jsr prtHexA8bit
    lda workspace+1               \ ***DEBUG***
    jsr prtHexA8bit
    lda workspace+2               \ ***DEBUG***
    jsr prtHexA8bit
    lda workspace+3               \ ***DEBUG***
    jsr prtHexA8bit
    lda workspace+4               \ ***DEBUG***
    jsr prtHexA8bit
    lda workspace+5               \ ***DEBUG***
    jsr prtHexA8bit
    jmp osnewl

.ipd_needle equb "+IPD,"
.wifi_search_ipd
 ldy #4                     \ load pointer
.sipd0
 lda ipd_needle,y           \ load character from search string (= needle)
 sta heap,y                 \ store in workspace
 dey                        \ decrement pointer
 bpl sipd0                  \ jump if characters follow
 lda #5                     \ load needle length
.wifi_search
 sta size                   \ store in workspace
 ldy #0                     \ reset pointer
.sipd1
 jsr wifi_test_end_of_data  \ check for end of data
 bcc sipd5                  \ jump if no more data
 jsr wifi_read_buffer       \ read character from input buffer
 pha                        \ save it on stack
 jsr dec_blocksize          \ decrement block size
 pla                        \ restore character
 cmp heap,y                 \ compare with character in needle
 bne sipd3                  \ jump if not equal
\ CHARACTER MATCH
 iny                        \ character matches, increment pointer 
 cpy size                   \ test for end of needle
 bne sipd4                  \ not the end, continue for next character
 sec                        \ set carry for needle found
 rts                        \ return to calling routine
.sipd3
; CHARACTER DOES NOT MATCH, RESET POINTER
 ldy #0                     \ character does not match, reset pointer
.sipd4
 jsr wifi_test_end_of_data  \ test if any data follows
 bcs sipd1                  \ jump if there is more data
.sipd5
 rts                        \ else return with carry cleared, i.e. needle not found

.crlf equb &0D,&0A,&0D,&0A
.wifi_search_crlf
 ldy #3                     \ initialize pointer
.scrlf1
 lda crlf,y                 \ load character from search string (= needle)
 sta heap,y                 \ write to workspace
 dey                        \ decrement pointer
 bpl scrlf1                 \ jump if more characters to copy
 lda #4                     \ load needle length
 bne wifi_search            \ jumps always

.wifi_read_ipd
 lda #0                     \ reset block size
 sta blocksize
 sta blocksize+1
 sta blocksize+2
.read_ipd_loop
 jsr wifi_read_buffer       \ read character from input buffer
 cmp #':'                   \ test for end of IPD string
 beq read_ipd_end           \ jump if end of IPD string
 sec                        \ set carry for substraction
 sbc #'0'                   \ convert to hex value
 jsr mul10                  \ multiply the IPD value by 10 and add the last value read
 jmp read_ipd_loop          \ repeat for next character
.read_ipd_end
 lda blocksize+1            \ load blocksize+1
 ora blocksize              \ ora with blocksize
 rts                        \ return with Z flag indicating the IPD value (zero or non-zero)


.mul10 \ MULTIPLY VALUE OF blocksize BY 10 AND ADD A
 pha
 asl blocksize
 lda blocksize
 rol blocksize+1
 rol blocksize+2
 ldy blocksize+1
 asl blocksize
 rol blocksize+1
 rol blocksize+2
 asl blocksize
 rol blocksize+1
 rol blocksize+2 
 clc
 adc blocksize
 sta blocksize
 tya
 adc blocksize+1
 sta blocksize+1
 lda blocksize+2
 adc #0
 sta blocksize+2
 clc
 pla
 adc blocksize
 sta blocksize
 lda blocksize+1
 adc #0
 sta blocksize+1
 lda blocksize+2
 adc #0
 sta blocksize+2
 rts

.wifi_test_end_of_data
 cpx datalen                \ compare pam index with data length LSB
 bne not_end_of_data        \ jump if not equal
 lda datalen+1              \ load MSB data length
 cmp pagereg                \ compare with pam register
 bne not_end_of_data        \ jump if not equal
 clc                        \ end of data, clear carry
 rts                        \ return with c=0 (no more data)
.not_end_of_data
 sec                        \ there is still data, set carry
 rts                        \ return with c=1 (data available)

\ Decrement blocksize. Blocksize is a 24 bit number but here
\ we only use 16 bit values so we skip the third byte.
.dec_blocksize
 sec
 lda blocksize
 sbc #1
 sta blocksize
 cmp #&FF
 bne dbs1
 lda blocksize+1
 sbc #1
 sta blocksize+1
.dbs1 \ Check if blocksize is 0
 lda blocksize
 ora blocksize+1
 rts

\ This routine checks the HTTP status code. Normally this is 206 because
\ we read partial content.
.wifi_http_status
 jsr wifi_test_end_of_data  \ Read next character...
 bcc wifi_http_status_end   \ ... as long as there is data
 jsr dec_blocksize
 jsr wifi_read_buffer       \ ... read the next byte
 cmp #' '                   \ check for space
 bne wifi_http_status
 stx httpstatus             \ save the buffer pointer to http status code
 jsr dec_blocksize
 jsr wifi_read_buffer       \ read the http status code (3 digits)
 sta heap                   \ store in heap
 jsr dec_blocksize
 jsr wifi_read_buffer
 sta heap+1
 jsr dec_blocksize
 jsr wifi_read_buffer
 sta heap+2
 lda heap                   \ now check for code 200 or 206
 cmp #'2'
 bne wifi_http_status_err   \ if not 2 then jump to error
 lda #'0'                   \ load '0'
 cmp heap+1                 \ check second digit
 bne wifi_http_status_err   \ jump if not '0'
 lda heap+2                 \ load last digit of status
 cmp #'6'                   \ check third digit
 beq wifi_http_status_end   \ jump if '6'
 cmp #'0'
 bne wifi_http_status_err   \ jump if not '0'
.wifi_http_status_end
 rts                        \ end subroutine

.wifi_http_status_err
 ldx httpstatus             \ restore pointer to http status code
 jsr wifi_prtstr            \ print the status code + message
 ldx #(error_http_status-error_table)
 jmp error                  \ throw an error

\ Check if the drive is mounted, return with status flag
\ On entry: X = drive number
\ On exit:  A > &00 and Z = 0 -> mounted
\           A = &00 and Z = 1 -> not mounted
\           pagereg set to page &00
.check_mounted
 lda #0                     \ set page
 sta pagereg
 lda mounttab,x             \ load mount status
.check_mounted_end
 rts

\ Check if the drive is mounted, throw error if not
.check_mounted_with_error
 jsr check_mounted
 bne check_mounted_end

.errorNotMounted
 ldx #(error_not_mounted - error_table)
 jmp error

.check_rw_mount_with_error
 jsr check_mounted
 beq errorNotMounted
 bmi check_mounted_end
 jmp diskERR1

\
\************************************************
\** Core WiDFS transfer routines
\
\(&BE)=Destination ram
\(&C2)=Length
\(&C4)=Sector  with C4 = high byte and C5 = low byte
\
.wifi_readRAMblock
\ lda &C1 : jsr prtHexA8bit
\ lda &C0 : jsr prtHexA8bit
\ lda &BF : jsr prtHexA8bit
\ lda &BE : jsr prtHexA8bit
\ lda #' ': jsr osasci
\ lda &C3 : jsr prtHexA8bit
\ lda &C2 : jsr prtHexA8bit
\ jsr osnewl
 jsr wifi_read_write_params
 lda #'G'                       \ set parameter for GET method
 sta get_or_post
 jsr wifi_get_cmd               \ execute the HTTP GET for downloading catalog
 jsr wifi_copy_received_data    \ copy received data into main memory
 jmp wifi_closeconn             \ close the connection to the server and return
 rts

.wifi_writeRAMblock
\lda &BE : jsr prtHexA8bit
\lda &BF : jsr prtHexA8bit
\lda &C0 : jsr prtHexA8bit
\lda &C1 : jsr prtHexA8bit
\lda &C2 : jsr prtHexA8bit
\lda &C3 : jsr prtHexA8bit
\lda &C4 : jsr prtHexA8bit
\lda &C5 : jsr prtHexA8bit
\jsr osnewl

 ldx &CF                        \ read drive number
 jsr wifi_openconn              \ open connection to webserver
 lda &C2                        \ copy length to workspace
 sta blocksize
 lda &C3
 sta blocksize+1
 jsr wifi_read_write_params_l1  \ prepare parameters
 jsr wifi_post_cmd              \ send the HTTP POST headers
 lda &BF                        \ check if block is at paged RAM
 cmp #>pageram
 beq wifi_write_gbpb            \ jump if in paged RAM
 jsr wifi_write_block           \ write data block(s)
 jmp wifi_closeconn             \ close the connection to the server and return


\ This routine copies a sector to the file buffer. This is used when
\ a file is opened for random access or *EXEC or whatever....
.wifi_read_gbpb
 jsr wifi_gbpb_page         \ calculate ram page number into zpm0
 jmp readcat                \ perform the read operation

.wifi_write_gbpb
 jsr wifi_gbpb_page         \ calculate ram page number into zpm0
 jsr write_page             \ perform the write operation
 jmp wifi_closeconn         \ close the connection and return

.wifi_gbpb_page
 lda zpm0                   \ load ram page (original DFS address)
 sec                        \ set carry for substraction
 sbc #9                     \ calculate ram page number
 sta zpm0                   \ store it in workspace for readcat routine
 ldy #0
 rts

 \ Process received data
.wifi_copy_received_data
  jsr wifi_reset_buffer      \ reset pointer to recieve buffer (PAM)
  jsr wifi_search_ipd        \ search IPD string
  bcs wifi_crd_l0            \ jump if string found
  jmp wifi_crd_end           \ end if no IPD string found 
.wifi_crd_l0
  jsr wifi_read_ipd          \ read IPD (= number of bytes in datablok)
  jsr wifi_http_status       \ check for HTTP statuscode 206
  jsr wifi_search_crlf       \ search for newline
 
  \ Some webservers, like the Python SimpleHTTP send the HTTP header in a separate
  \ ethernet frame. So we must check here if the block length of the first +IPD has
  \ become zero or negative. The maximum block length is about 1470 bytes.
  lda blocksize+1            \ check if block size is negative
 ; php:jsr printhex:jsr osnewl:plp
  bmi wifi_reread_ipd        \ jump if negative to read the new IPD
  jsr read_ipd_end           \ check if block size is zero
  bne wifi_check_ld          \ if not zero then continue
.wifi_reread_ipd
  jsr wifi_search_ipd        \ else search next IPD string
  bcs wifi_reread_ipd2
  jmp wifi_crd_end           \ end if no IPD string found 
.wifi_reread_ipd2
  jsr wifi_read_ipd          \ read IPD (= number of bytes in datablok)
  jsr wifi_search_crlf       \ search end of response headers

.wifi_check_ld
  lda &BF                    \ load high byte load address
  cmp #HI(pageram)           \ is it in paged ram?
  beq wifi_read_gbpb         \ yes, then jump for reading a file buffer

.wifi_crd_loop               \ copy received data
  jsr wifi_read_http_data    \ read received data block
  jsr wifi_test_end_of_data  \ test if this was the last block
  bcc wifi_crd_end
  jsr wifi_search_ipd        \ search for next IPD
  bcc wifi_crd_end           \ jump if no more blocks found
  jsr wifi_read_ipd          \ read the block length
  jmp wifi_crd_loop          \ read this block
.wifi_crd_end
  rts
;  lda fflag
;  beq wifi_crd_end2          \ if fflag is set need to release tube
;  ldx #LO(laddr)
;  ldy #HI(laddr)
;  lda #tubeID
;  and #&BF                   \ clear bit 6 of ID to release tube
;  bit laddr+1
;  bpl wifi_tube_release
;  lda #4                     \ if address is >= &8000 this is a language so send run command (and release tube)
; .wifi_tube_release
;  jsr &406                   \ release tube
    
\ READ HTTP DATA UNTIL blocksize IS 0
\ DATA IS WRITTEN TO LOAD ADDRESS
.wifi_read_http_data
 ldy #0                     \ clear index
 jsr wifi_test_end_of_data  \ test for end of data
 bcc read_http_end          \ jump if end of data is reached
 jsr wifi_read_buffer       \ read byte from input buffer
; bit fflag
; bmi wifi_read_tube_data    \ if flag set this is tube transfer
 sta (load_addr),y          \ store in memory
 inc load_addr              \ increment load address
 bne rhd1
 inc load_addr+1
 jmp rhd1
;.wifi_read_tube_data
; sta tubereg                \ write data to tube
; jsr read_http_end
; jsr read_http_end          \ delay to allow tube transfer
 
.rhd1
 jsr dec_blocksize          \ decrement block size
 bne wifi_read_http_data    \ jump if more bytes to read
.read_http_end  
 rts                        \ return from subroutine


\ Open the connection to the web server and calculate start and end of data on web server
.wifi_read_write_params
 ldx &CF                        \ read drive number
 jsr wifi_openconn              \ open connection to webserver
.wifi_read_write_params_l1
 ldx &CF                        \ read drive number
 inx                            \ set page for url
 stx pagereg
 lda #&00                       \ set range start (sector * 256)
 sta workspace+0
 lda &C5
 sta workspace+1
 lda &C4
 and #&03
 sta workspace+2
 clc                            \ clear carry for addition
 lda &C2                        \ set range end (sector * 256 + length)
 adc workspace+0
 sta workspace+3
 lda &C3
 adc workspace+1
 sta workspace+4
 lda #&00
 adc workspace+2
 sta workspace+5
 \ Calculate content length (only used in POST requests)
\ clc                            \ clear carry for addition
 lda &C2
\ adc #5                         \ length of parameter (data=)
 sta workspace+6
 lda &C3
\ adc #0
 sta workspace+7
 lda #0
 adc #0
 sta workspace+8
 ldx #LO(workspace)             \ load pointers to workspace
 ldy #HI(workspace)
 rts


\ Wifi POST command -> write block of data to server
.http_header_post   equs "POST /"
.http_header_xrange equb &0D,&0A : equs "Content-type: application/octet-stream"
                    equb &0D,&0A : equs "X-Write-Range: bytes="
.http_header_conlen equb &0D,&0A : equs "Content-length: "
.http_header_data   equb &0D,&0A,&0D,&0A

.wifi_post_cmd
    stx paramblock              \ save address of parameter block
    sty paramblock+1
    ldy #0                      \ reset pointer to start of strbuf
    ldx #0                      \ load index
.wifi_post_l1                   \ copy "POST " to string buffer
    lda http_header_post,x      \ read data
    sta strbuf,y                \ write to string buffer
    iny                         \ increment pointers
    inx
    cpx #(http_header_xrange-http_header_post)
    bne wifi_post_l1
    ldx url_path_pointer        \ load pointer to path
    jsr wifi_url_strbuf         \ copy url to string buffer
    ldx #0                      \ reset index
.wifi_post_l2                   \ copy HTTP/1.1 and HOST to string buffer
    lda http_header_1_1,x       \ read data
    sta strbuf,y                \ write to string buffer
    iny                         \ increment pointers
    inx
    cpx #(http_header_ua-http_header_1_1)
    bne wifi_post_l2
    jsr wifi_hostname_strbuf    \ copy hostname to string buffer
    ldx #0                      \ reset index
.wifi_post_l3
    lda http_header_xrange,x    \ read data
    sta strbuf,y                \ write to string buffer
    iny                         \ increment pointers
    inx
    cmp #'='                    \ test end of string
    bne wifi_post_l3
    jsr wifi_range_strbuf       \ copy range to string buffer
    ldx #0
.wifi_post_l4
    lda http_header_conlen,x    \ read data
    sta strbuf,y                \ write to string buffer
    iny                         \ increment pointers
    inx
    cmp #' '                    \ test end of string
    bne wifi_post_l4
    jsr wifi_set_conlenth       \ write content length to string buffer
    ldx #0
.wifi_post_l5
    lda http_header_data,x      \ read data
    sta strbuf,y                \ write to string buffer
    iny                         \ increment pointers
    inx
    cpx #4                      \ test end of string
    bne wifi_post_l5
    \ At this point the HTTP header is are set in
    \ the strbuf. Send this block of data to the server

.wifi_post_l6
    tya                         \ save Y
    pha
    jsr wifi_ipsend             \ submit IPSEND command
    pla                         \ restore Y
    tay                         \ I really miss the PHY and PHX instructions :-(
    lda #4
    sta time_out
    jsr wifi_send_strbuf        \ send the request command to the ESP device
    jmp uart_get_response       \ read response and return


\ Write a paged ram to the server, the connection should already
\ be opened and the headers must be send. The page number is passed
\ in address zpm0.
.write_page
    ldx #0                      \ reset pointer
.write_page_l1
    lda wifi_ipsend_cmd,x       \ read character from command string
    jsr send_byte               \ send it to ESP device
    inx                         \ increment pointers
    cmp #&0A                    \ test for last character
    bne write_page_l1           \ jump if not last character
    lda #2                      \ use a short time out
    sta time_out
    jsr uart_get_response       \ read response
    lda zpm0                    \ load page number
    sta pagereg                 \ set page
    ldx #0                      \ reset pointer
.write_page_l2
    lda pageram,x               \ load data
    jsr send_byte               \ send to server
    inx                         \ increment pointer
    bne write_page_l2           \ jump if not all bytes send
    lda #2                      \ use a short time out
    sta time_out
    jsr wifi_reset_buffer
    jmp uart_get_response       \ read response and return


\ Write a block of memory to the server, the connection should already
\ be opened and the headers must be send. The page number is passed
\ in address zpm0. The memory block is chopped into blocks of _BLOCKSIZE_
\ (maximum 2048) bytes.
.wifi_write_block
    lda &C2                     \ copy data length to work area
    sta blocksize
    lda &C3
    sta blocksize+1
.wifi_write_block_loop
    lda blocksize+1             \ compare high bytes
    cmp #>_BLOCKSIZE_
    bcc wifi_write_block_2      \ jump if blocksize < 2048
    bne wifi_write_block_1      \ jump if blocksize > 2048
    lda blocksize               \ the high bytes are equal, so compare low bytes
    cmp #<_BLOCKSIZE_
    bcc wifi_write_block_2      \ jump if blocksize < 2048
.wifi_write_block_1             \ blocksize < 2048, so send the remaining bytes
    lda #<_BLOCKSIZE_           \ write block length to workspace
    sta workspace
    sta workspace+3             \ write to workspace (used as counter)
    lda #>_BLOCKSIZE_
    sta workspace+1
    sta workspace+4             \ write to workspace (used as counter)
    jmp wifi_write_block_2_l1
.wifi_write_block_2             \ blocksize < 2048, so send a block of 2048 bytes
    lda blocksize               \ write blocksize to workspace
    sta workspace
    sta workspace+3             \ write to workspace (used as counter)
    lda blocksize+1
    sta workspace+1
    sta workspace+4             \ write to workspace (used as counter)
.wifi_write_block_2_l1
    jsr wifi_write_block_3      \ do IPSEND command

    \ Send the data
    ldy #0                      \ reset index
.wifi_write_block_5
    lda (load_addr),y           \ read data from memory
    jsr send_byte               \ send to ESP device
    jsr dec_blocksize
    beq wifi_write_status       \ jump if all data send
    inc load_addr               \ increment load address
    bne wifi_write_block_5_l1
    inc load_addr+1
.wifi_write_block_5_l1
    dec workspace+3             \ decrement block length
    lda workspace+3             \ check if block length = 0
    ora workspace+4
    beq wifi_write_block_6      \ jump if block is transferred
    lda workspace+3             \ else check for roll over
    cmp #&FF
    bne wifi_write_block_5
    dec workspace+4
    jmp wifi_write_block_5      \ jump for next byte if not 0

.wifi_write_block_6
    jsr wait                    \ wait a little while to get the data transferred
    jmp wifi_write_block_loop

.wifi_write_status
    \ Read the result
    lda #12                     \ use a long time out
    sta time_out
    jsr uart_get_response       \ read response
.wifi_check_status
    jsr wifi_get_write_status   \ check for write status
    lda writestatus             \ load status code
    bne wifi_write_status_err   \ jump on error
    rts                         \ return

.wifi_write_status_err
    jsr prtStr                  \ print error message
    equs "Write error &",&EA
    lda writestatus             \ reload status code
    jsr prtHexA8bit             \ print it
    cmp #&FF                    \ no X-Write-Status received?
    beq wifi_write_err_l2       \ then do not print the error message (because there is none)
.wifi_write_err_l1
    jsr wifi_read_buffer        \ read the status error message
    jsr osasci                  \ print it
    inx                         \ increment pointer
    beq wifi_write_err_l2
    cmp #&0A                    \ test for end of string
    bne wifi_write_err_l1
.wifi_write_err_l2
    jmp diskERR1                \ throw an error  (disc read only)
    rts

\ This part is a subroutine to avoid out of range branches
.wifi_write_block_3
    lda #0                      \ the third byte is always zero
    sta workspace+2
    ldy #0                      \ reset pointer
.wifi_write_block_3_l1
    lda wifi_ipsend_cmd,y       \ load character
    sta strbuf,y                \ write to strbuf
    iny                         \ increment pointer
    cmp #'='                    \ test for end of string
    bne wifi_write_block_3_l1   \ jump if more characters
    ldx #0                      \ set pointer to workspace for prdec24
    jsr prdec24                 \ write the decimal value of length to strbuf
    lda #&0D                    \ write terminating &0D &0A to strbug
    sta strbuf,y
    iny
    lda #&0A
    sta strbuf,y

    \ Send the IPSEND command to the ESP device
    ldy #0                      \ reset pointer
.wifi_write_block_4
    lda strbuf,y                \ read character
    jsr send_byte               \ send to device
    iny                         \ increment pointer
    cmp #&0A                    \ check for end of string
    bne wifi_write_block_4      \ jump if not all send

    \ Read the result
    lda #2
    sta time_out
    jmp uart_get_response

.save_workspace
    pha                         \ save used registers
    tya
    pha
    ldy #8                      \ load index
.save_workspace_l1
    lda &BE,y                   \ load data from zero page
    sta savespace,y             \ store in save space
    dey                         \ decrement index
    bpl save_workspace_l1       \ jump if not all done
    pla                         \ restore registers
    tay
    pla
    rts

.restore_workspace
    pha                         \ save used registers
    tya
    pha
    ldy #8                      \ load index
.restore_workspace_l1
    lda savespace,y             \ load data from save space
    sta &BE,y                   \ restore in zero page
    dey                         \ decrement index
    bpl restore_workspace_l1    \ jump if not ready
    pla                         \ restore used registers
    tay
    pla
    rts

.wifi_wrstat_needle equs "X-Write-Status: "
.wifi_get_write_status
    lda datalen                 \ save data length
    pha
    lda datalen+1
    pha
    jsr wifi_reset_buffer       \ reset buffer and set X = 0
.wifi_get_write_status_l1
    lda wifi_wrstat_needle,x    \ copy needle to heap
    sta heap,x
    inx                         \ increment index
    cmp #' '                    \ test for end of string
    bne wifi_get_write_status_l1
    txa                         \ copy needle length to accu
    ldx #0                      \ reset pointer to paged ram
    jsr wifi_search             \ search the needle
    bcc wifi_status_not_found   \ no write status received
    jsr wifi_read_buffer        \ read the status code
    sec                         \ load carry for substraction
    sbc #'0'
    bpl wifi_get_write_status_l2
.wifi_status_not_found
    lda #&FF                    \ load status for 'not found'
.wifi_get_write_status_l2
    sta writestatus             \ store the status code
    pla                         \ restore data length
    sta datalen+1
    pla
    sta datalen
    rts                         \ end of routine

.wifi_set_get_or_post
    lda get_or_post             \ load HTTP method
    cmp #'P'                    \ check for POST
    beq wifi_set_l2
.wifi_set_l1                    \ copy "GET " to string buffer
    lda http_header_get,x       \ read data
    sta strbuf,y                \ write to string buffer
    iny                         \ increment pointers
    inx
    cmp #'/'                    \ check for end of string
    bne wifi_set_l1             \ jump if not done
    rts                         \ return
.wifi_set_l2
    lda http_header_post,x      \ copy "POST " to string buffer
    sta strbuf,y                \ write to string buffer
    iny                         \ increment pointers
    inx
    cmp #'/'                    \ check for end of string
    bne wifi_set_l2             \ jump if not done
    rts                         \ return


.save_context
    pha
    txa
    pha
    tya
    pha
    jsr prtStr
    EQUS "Save context",&0D,&0A,&EA
    jsr save_bank_nr
    lda #_CONTEXT_
    sta pagereg
    ldx #0
.save_context_l1
    lda savespace,x
    sta pageram,x
    inx
    bne save_context_l1
    jsr restore_bank_nr
    pla
    tay
    pla
    tax
    pla
    rts

.restore_context
    pha
    txa
    pha
    tya
    pha
    jsr prtStr
    EQUS "Restore context",&0D,&0A,&EA
    jsr save_bank_nr
    lda #_CONTEXT_
    sta pagereg
    ldx #0
.restore_context_l1
    lda pageram,x
    sta savespace,x
    inx
    bne restore_context_l1
    jsr restore_bank_nr
    pla
    tay
    pla
    tax
    pla
    rts

.dmul10 \ MULTIPLY VALUE OF datalen BY 10 AND ADD A
 pha
 asl datalen
 lda datalen
 rol datalen+1
 ldy datalen+1
 asl datalen
 rol datalen+1
 asl datalen
 rol datalen+1
 clc
 adc datalen
 sta datalen
 tya
 adc datalen+1
 sta datalen+1
 clc
 pla
 adc datalen
 sta datalen
 lda datalen+1
 adc #0
 sta datalen+1
 rts
