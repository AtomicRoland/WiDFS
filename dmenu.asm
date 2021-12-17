\ WiDFS Menu for bbc micro and acorn electron wifi board
\ (c) roland leurs, november 2021

.dmenu_host      EQUB "acornelectron.nl"
.dmenu_path      EQUB "discmenu/search.php?page="

.CMD_DMENU                      \ start of *DMENU command
    cld                         \ Why????
    jsr dmenu_init              \ build the screen
.cmd_dmenu_start
    jsr dmenu_open              \ open the connection to the server
    jsr dmenu_get_cmd           \ send the get command to the server
    stx datalen
    lda pagereg
    sta datalen+1
    jsr dmenu_print_status      \ print the status line
    php                         \ save processor status
    jsr wifi_closeconn          \ close the connection to the server
    plp                         \ restore processor status
    bcc cmd_menu_end            \ jump if the status line was not found
    lda httpstatus              \ load the status code
    cmp #'0'                    \ check if there are titles
    beq cmd_menu_l1             \ jump if no titles received (e.g. empty query result)
    jsr dmenu_copy_data         \ copy data to main memory
.cmd_menu_l1
    jsr dmenu_clear_titles      \ remove all titles for the new results
    jsr dmenu_print_titles      \ print the titles
    jmp dmenu_input             \ goto input loop
.cmd_menu_end
    rts                         \ that's it, for today....

\ Initialize the screen
.dmenu_init
    lda #'0'                    \ write page number as text string to workspace
    sta errorspace
    sta errorspace+1
    lda #'1'
    sta errorspace+2
    lda #&0D                    \ clear search string
    sta errorspace+4
    jsr prtStr
    EQUB 22,3                   \ mode 3
    EQUB 19,0,4,0,0,0           \ coloured lines
    EQUS "WiDFS Menu",&EA
    rts                         \ end of routine

.dmenu_print_cmdline
    jsr prtStr                  \ print text
    EQUB 31,0,24
    EQUS "  A-T = Run Title    Up/Down = Prev/Next screen    / = Search    ESCAPE = END"
    EQUB &EA
    jsr dmenu_cursor_off        \ hide the cursor
    rts

\ Open the connection for the disk menu
\ All registers will be destroyed on exit.
.dmenu_open
    jsr test_wifi_ena           \ test if WiFi is enabled
    beq dmenu_openconn
    jmp wifi_is_disabled        \ jump if not enabled
.dmenu_openconn
    jsr init_uart               \ set up the uart
    ldy pagereg                 \ save page register
    jsr wifi_ate0               \ set echo off
    sty pagereg                 \ restore page register
    ldy #0                      \ reset pointer

.dmenu_openconn_l1
    lda wificmd_cipstart,y      \ load character from initialization string
    sta strbuf,y                \ store in buffer space
    iny                         \ increment pointer
    cmp #','                    \ test for end of string
    bne dmenu_openconn_l1       \ jump if more characters
    lda #'"'                    \ open quote
    sta strbuf,y                \ store in buffer
    iny                         \ increment buffer
    ldx #0                      \ reset pointer to hostname
.dmenu_openconn_l2
    lda dmenu_host,x            \ load character from hostname
    sta strbuf,y                \ place into string buffer
    iny
    inx                         \ increment pointer to next character
    cpx #(dmenu_path-dmenu_host)\ test for last character
    bne dmenu_openconn_l2       \ jump if there are more characters
    jmp wifi_openconn_l3        \ terminate the string and open the connection

\ Construct and send the HTTP request
\ All registers will be destroyed on exit.
.dmenu_get_cmd
    ldy #0                      \ reset pointer to string buffer
    ldx #0                      \ reset pointer to string
    \ copy the GET string
.dmenu_get_cmd_l1
    lda http_header_get,x       \ load character
    sta strbuf,y                \ store in string buffer
    iny                         \ increment pointer to string buffer
    inx                         \ increment pointer to string
    cpx #(http_header_1_1-http_header_get)
    bne dmenu_get_cmd_l1        \ jump if there are more characters

    \ copy the path to string buffer
    ldx #0                      \ reset pointer
.dmenu_get_cmd_l2
    lda dmenu_path,x            \ load character
    sta strbuf,y                \ write to string buffer
    iny                         \ increment pointers
    inx
    cpx #(CMD_DMENU-dmenu_path)
    bne dmenu_get_cmd_l2        \ jump if there are more characters

    \ copy search string to stringbuffer
    jsr dmenu_search_string

    \ copy http protocol version to string buffer
    ldx #0                      \ reset pointer
.dmenu_get_cmd_l3
    lda http_header_1_1,x       \ load character from string
    sta strbuf,y                \ write to string buffer
    iny                         \ increment pointers
    inx
    cpx #(http_header_ua-http_header_1_1)
    bne dmenu_get_cmd_l3        \ jump if there are more characters

    \ copy hostname to string buffer
    ldx #0                      \ reset pointer
.dmenu_get_cmd_l4
    lda dmenu_host,x            \ load character
    sta strbuf,y                \ write to buffer
    iny                         \ increment pointers
    inx
    cpx #(dmenu_path-dmenu_host)
    bne dmenu_get_cmd_l4        \ jump if there are more characters

    \ copy the rest of the header to string buffer
    ldx #0                      \ again ...
.dmenu_get_cmd_l5
    lda http_header_ua,x        \ load character
    sta strbuf,y                \ write to string buffer
    iny                         \ increment pointers
    inx
    cpx #(http_header_range-http_header_ua)
    bne dmenu_get_cmd_l5        \ jump if there are more characters

    \ copy terminating CRLF's to string buffer
    ldx #0                      \ for the last time here
.dmenu_get_cmd_l6
    lda crlf,x                  \ load character
    sta strbuf,y                \ write to string buffer
    iny                         \ increment pointers
    inx
    cpx #4
    bne dmenu_get_cmd_l6        \ jump if there are more characters

    \ do IPSEND command and send the header
    jmp wifi_post_l6

.dmenu_xstatus
    EQUS "X-Status: "

.dmenu_print_status
    jsr wifi_reset_buffer       \ reset pointer to recieve buffer (PAM)
    jsr wifi_search_ipd         \ search IPD string
    bcc dmenu_print_l4          \ jump if string not found
    jsr wifi_read_ipd           \ read IPD (= number of bytes in datablok, stored in 'workspace')
    ldy #09                     \ load pointer (= length of search string xstatus minus 1)
.dmenu_print_l1
    lda dmenu_xstatus,y         \ load character from search string
    sta heap,y                  \ store in workspace
    dey                         \ decrement pointer
    bpl dmenu_print_l1          \ jump if characters follow
    lda #10                     \ load needle length
    jsr wifi_search             \ search the string
    bcc dmenu_print_l4          \ jump to end if the string is not found

.dmenu_print_l2
    jsr wifi_read_buffer        \ load the status byte (information about page)
    sta httpstatus              \ store it somewhere
    jsr dec_blocksize           \ decrement the block size
    lda #30                     \ cursor home
    jsr osasci
.dmenu_print_l3
    jsr dec_blocksize           \ decrement the block size
    jsr wifi_read_buffer        \ load character of status line (provided by server)
    jsr osasci                  \ print it
    cmp #&0A                    \ Is it the last character
    bne dmenu_print_l3          \ No, then print the next character
    sec                         \ Set carry to indicate that the status line was printed
.dmenu_print_l4
    rts

.dmenu_copy_data
    jsr wifi_search_crlf        \ search the end of the header
    lda #&00                    \ set load address
    sta load_addr
    lda #&2C
    sta load_addr+1
    jsr wifi_crd_loop
    lda load_addr+1             \ calculate the number of records
    sec                         \ based on the number of 256 byte
    sbc #&2C                    \ pages written.
    sta datalen+2               \ write to workspace
    rts

.dmenu_print_titles
    lda httpstatus              \ read the status
    cmp #'0'                    \ check if any results received
    beq dmenu_no_results        \ jump if no results received (e.g. empty query results)
    lda #&00                    \ set pointer to memory in zero page
    sta datalen
    lda #&2C                    \ we store our data from &2C00 upwards (need 5 kB)
    sta datalen+1
.dmenu_print_title_l1
    ldy #4                      \ preset pointer
.dmenu_print_title_l2
    lda (datalen),y             \ load character from title
    jsr osasci                  \ print it
    iny                         \ increment pointer
    cpy #&53                    \ test for end of title string
    bne dmenu_print_title_l2    \ jump if there's more to print
    jsr osnewl                  \ cursor to next line
    inc datalen+1               \ next record
    dec datalen+2               \ decrement record counter
    bne dmenu_print_title_l1    \ jump if there are more records to print
    jsr dmenu_print_cmdline     \ print the command line
    rts                         \ return after printing the last record

.dmenu_no_results
    jsr prtStr                  \ print the next string
    EQUS "No results found",&EA
    jmp dmenu_print_cmdline     \ restore command line and end routine

.dmenu_input_esc
    cmp #&1B                    \ is escape pressed?
    bne dmenu_input_loop        \ if not, ignore the error and read again
    lda #&7E                    \ acknowledge the escape
    jsr osbyte
.dmenu_input_end
    lda #22                     \ reset the screen
    jsr osasci
    lda #6                      \ back to mode 6
    jsr osasci
    lda #4                      \ restore normal function for editing keys
    ldx #0
    jsr osbyte                  \ enable cursor editing
    rts                         \ end command

.dmenu_page_nav
    sta zp                      \ save key value
    lda httpstatus              \ load search status
    and #1                      \ is there a next page
    bne dmenu_page_test_down    \ yes, so go check if cursor down is pressed
.dmenu_page_nav_l1
    lda httpstatus              \ reload page status
    and #2                      \ is there a previous page
    beq dmenu_input_loop        \ no, ignore the key and continue the input loop

.dmenu_page_test_up
    lda zp                      \ load key value
    cmp #&8B                    \ is it cursor up
    bne dmenu_input_loop        \ no, ignore the key and continue the input loop
    \ it is cursor up, so decrement the page number in workspace
    jsr dmenu_dec_page
    jmp cmd_dmenu_start         \ load new page

.dmenu_page_test_down
     lda zp                     \ load key value
     cmp #&8A                   \ is it cursor down
     bne dmenu_page_nav_l1      \ no, go back and check for cursor up
     \ it is cursor down, increment the page number in workspace
     jsr dmenu_inc_page
     jmp cmd_dmenu_start        \ load new page

.dmenu_input
    lda #4                      \ load A for OSBYTE call
    ldx #1                      \ load X to setup editing keys as normal keys
    jsr osbyte                  \ setup editing keys
.dmenu_input_loop
    jsr osrdch                  \ read a character from keyboard
    bcs dmenu_input_esc         \ carry is set, go check for escape
    bmi dmenu_page_nav          \ a negative value indicates a possible cursor key
    cmp #'/'                    \ test for search command
    beq dmenu_search            \ jump if it is a search command
    sec                         \ clear carry for subtraction
    sbc #'A'
    bmi dmenu_input_loop        \ jump if negative ( less than A pressed )
    cmp #&14                    \ compare for maximum allowed letter
    bpl dmenu_input_loop        \ jump if larger ( invalid keys will be ignored )

.dmenu_mount_image
    \ A contains the index to the page where the image URL is stored
    clc                         \ clear carry for addition
    adc #&2C                    \ add base memory address
    sta zp+1                    \ write it also to workspace
    lda #&53                    \ load offset to url in paged ram
    sta zp                      \ write to workspace
    ldy #0                      \ reset index
    lda #_URL_DRIVE0_           \ select page with url for drive 0
    sta pagereg                 \ select the page
.dmenu_mount_image_l1
    lda (zp),y                  \ load character from image url
    sta pageram,y               \ store in paged RAM
    iny                         \ increment pointer
    cmp #&0D                    \ test for end of string
    bne dmenu_mount_image_l1    \ jump if there are more characters
    ldx #0                      \ load drive number
    stx pagereg                 \ select page 0
    lda #&01                    \ load mount status byte (read only)
    sta mounttab,x              \ update mount tab
    jsr wifi_read_catalog       \ read the catalog

    \ Check the option byte
    lda #(_CAT_DRIVE0_+1)       \ load page number with second part of catalog 0
    sta pagereg                 \ store at page register
    lda pageram+6   			\ load boot option
    and #&0F			        \ Mask off MSB of disk sectors
    beq dmenu_end               \ if no action required then jump to the end of dmenu
    sec                         \ set carry
    sbc #1                      \ decrement A with 1
    asl a                       \ shift left (multiply by 2)
    tay                         \ move to index register
    lda dmenu_boot_tab,y        \ load low byte of boot command
    tax                         \ place it into the X register
    lda dmenu_boot_tab+1,y      \ load high byte of boot command
    tay                         \ place it into the Y regester
    jsr oscli                   \ start the !boot file
.dmenu_end
    jmp dmenu_input_end         \ terminate the command

.dmenu_search_esc
    cmp #&1B                    \ is escape pressed?
    bne dmenu_search            \ if not, ignore the error and read again
    lda #&7E                    \ acknowledge the escape
    jsr osbyte
    jsr dmenu_cursor_off        \ switch cursor off
    jsr dmenu_search_clear      \ clear the search line
    jsr dmenu_print_cmdline     \ restore command line
    jmp dmenu_input_loop        \ continue scanning keys

.dmenu_search
    jsr dmenu_search_clear      \ clear the search line
    jsr prtStr
    EQUB 31,0,24
    EQUS "Enter search string: ",&EA
    jsr dmenu_cursor_on         \ show cursor
    ldx #0                      \ load index
.dmenu_search_l1
    jsr osrdch
    bcs dmenu_search_esc        \ jump on error condition or escape press
    cmp #13                     \ compare with end of input
    beq dmenu_search_end        \ do something with the input
    cmp #33                     \ check for valid character
    bmi dmenu_search_l1         \ silently ignore invalid characters
    cmp #&7F                    \ check for delete
    beq dmenu_search_delete     \ jump on delete key
    sta errorspace+4,x          \ store search string in memory
    jsr osasci                  \ print the character
    inx                         \ increment pointer
    cpx #30                     \ accept a maximum of 30 characters to search for
    bne dmenu_search_l1         \ jump if maximum not reached
.dmenu_search_end
    lda #13                     \ terminate the search string
    sta errorspace+4,x
    jmp cmd_dmenu_start         \ read the data

.dmenu_search_delete
    cpx #0                      \ we can't go beyond the start of the string
    beq dmenu_search_l1         \ at begin of string, ignore the delete key
    lda #&7F                    \ erase the previous character
    jsr osasci
    dex
    bpl dmenu_search_l1         \ jumps always

.dmenu_search_clear
    jsr prtStr                  \ set cursor to beginning of line 23
    EQUB 31,0,24,&EA
    ldy #78                     \ load counter
    lda #' '                    \ load space character
.dmenu_search_clear_l1
    jsr osasci                  \ print a space
    dey                         \ decrement counter
    bpl dmenu_search_clear_l1   \ jump if not done
    rts                         \ end of routine

.dmenu_cursor_off
    jsr prtStr                  \ vdu sequence to hide cursor
    EQUB 23,1,0,0,0,0,0,0,0,0,&EA
    rts

.dmenu_cursor_on
    jsr prtStr                  \ vdu sequence to hide cursor
    EQUB 23,1,1,0,0,0,0,0,0,0,&EA
    rts

.dmenu_clear_titles
    jsr prtStr                  \ vdu sequence to define a view port, clear it and reset it
    EQUB 28,0,23,79,1           \ the view port
    EQUB 12                     \ clear it
    EQUB 26,10,10               \ destroy it
    nop
    rts                         \ that's all

\ This routine adds the search string to the url to download
\ Y points to the end of the url in the string buffer
.dmenu_search_search
    EQUS "=eltit&"
.dmenu_search_string
    lda errorspace              \ first copy the page number
    sta strbuf,y                \ write to string buffer
    iny                         \ increment pointer
    lda errorspace+1            \ first copy the page number
    sta strbuf,y                \ write to string buffer
    iny                         \ increment pointer
    lda errorspace+2            \ first copy the page number
    sta strbuf,y                \ write to string buffer
    iny                         \ increment pointer
    jsr dmenu_add_elk           \ add search parameter for Electron
    ldx #6                      \ set index
.dmenu_search_string_l1
    lda dmenu_search_search,x   \ load character
    sta strbuf,y                \ write to string buffer
    iny                         \ increment pointer to buffer
    dex                         \ decrement index to string
    bpl dmenu_search_string_l1  \ jump if there are more characters to copy
    \ now copy the search string
    ldx #0                      \ reset index
.dmenu_search_string_l2
    lda errorspace+4,x          \ load character
    sta strbuf,y                \ store into string buffer
    iny                         \ increment pointer to buffer
    inx                         \ increment pointer to search string
    cmp #&0D                    \ test for end of search string
    bne dmenu_search_string_l2  \ jump if there are more characters to copy
    dey                         \ this removes the CR from the string
    rts                         \ done, return

.dmenu_boot_tab
    EQUB <dmenu_load_boot, >dmenu_load_boot
    EQUB <dmenu_run_boot,  >dmenu_run_boot
    EQUB <dmenu_exec_boot, >dmenu_exec_boot

.dmenu_load_boot    EQUS "*L.$.!BOOT",&0D
.dmenu_run_boot     EQUS "*R.$.!BOOT",&0D
.dmenu_exec_boot    EQUS "*E.$.!BOOT",&0D

.dmenu_dec_page
    dec errorspace+2            \ decrement lowest byte (remember, it's ascii)
    lda errorspace+2            \ load the result
    cmp #&2F                    \ check for underrun (lower than ascii 0)
    bne dmenu_dec_page_end      \ no underrun, jump to end of routine
    lda #&39                    \ set lowest byte to 9
    sta errorspace+2
    dec errorspace+1            \ decrement middle byte (remember, it's ascii)
    lda errorspace+1            \ load the result
    cmp #&2F                    \ check for underrun (lower than ascii 0)
    bne dmenu_dec_page_end      \ no underrun, jump to end of routine
    lda #&39                    \ set lowest byte to 9
    sta errorspace+1
    dec errorspace              \ decrement highest byte (remember, it's ascii)
.dmenu_dec_page_end
    rts                         \ officially we can't go lower, so return

.dmenu_inc_page
    inc errorspace+2            \ increment lowest byte (remember, it's ascii)
    lda errorspace+2            \ load the result
    cmp #&3A                    \ check for overflow (higher than ascii 9)
    bne dmenu_inc_page_end      \ no overflow, jump to end of routine
    lda #&30                    \ set lowest byte to 0
    sta errorspace+2
    inc errorspace+1            \ increment middle byte (remember, it's ascii)
    lda errorspace+1            \ load the result
    cmp #&3A                    \ check for overflow (lower than ascii 9)
    bne dmenu_inc_page_end      \ no overflow, jump to end of routine
    lda #&30                    \ set lowest byte to 0
    sta errorspace+1
    dec errorspace              \ increment highest byte (remember, it's ascii)
.dmenu_inc_page_end
    rts                         \ officially we can't go higher, so return

.dmenu_add_elk_param
    EQUS "Y=kle&"

.dmenu_add_elk
    sty save_y                  \ save pointer to string buffer
    lda #129                    \ determine machine type
    ldx #0
    ldy #&FF
    jsr osbyte
    ldy save_y                  \ restore pointer to string buffer
    cpx #1                      \ compare with Electron (OS 1.00) machine type
    bne dmenu_inc_page_end      \ jump if not Electron
    ldx #5                      \ load number of bytes to add
.dmenu_add_elk_l1
    lda dmenu_add_elk_param,x   \ load character
    sta strbuf,y                \ write it to string buffer
    iny                         \ increment pointer
    dex                         \ decrement counter
    bpl dmenu_add_elk_l1        \ jump if more characters to follow
    rts                         \ end of routine