# Gauche-terminal
Terminal utility library for Gauche.

## Requirements
* Gauche 0.9 or later
* Ncurses (if you want to use tparm)

## Building from tarball
```shell
% ./DIST gen
% ./configure
% make
% sudo make install
```
    
If you don't have Ncurses or don't want to build tparm,
run configure as this:
```shell
% ./configure --disable-tparm
```

## Start
```scheme
(use terminal)
```

## Sample
view [sample/](sample/)

# Reference
## terminal
This module contains terminal.* modules.

### load-terminal-capability :key term fallback
* common interface of load-terminfo-entry and load-termcap-source
* `term` means a terminal type
* the default of `term` is (sys-getenv "TERM")
* type of return value: \<capability\> (capability-symbols receiver object)

```scm
(define TERM (load-terminal-capability))

(TERM 'true-booleans)     ; => get all symbols of true-boolean
(TERM 'false-booleans)    ; => get all symbols of false-boolean
(TERM 'available-numbers) ; => get all symbols of number
(TERM 'available-strings) ; => get all symbols of string
(TERM 'clear-screen)  ; => get clear_screen's escape sequence

(load-terminfo-entry :term 'ansi); => load the terminfo entry file of ansi
(load-termcap-source :term 'cygwin); => load terminal capabilities of cygwin, from termcap source
```

## terminal.terminfo
### load-terminfo-entry :key term path fallback
* Terminfo interface for load-terminal-capability

```scm
(load-terminal-capability :path "~/.terminfo"); => load "~/.terminfo"
```

## terminal.termcap
### load-termcap-source :key term path fallback
* Termcap interface for load-terminal-capability


## terminal.tparm
### tparm capability-string :optional param1 param2 ... param10
This function is a wrapper of GNU Ncurses tparm


## terminal.winsize
### \<winsize\>
This class is wrapping 'struct winsize'
    
**Instance Variable of <winsize>:** size.row => ws_row of 'struct winsize'    
**Instance Variable of <winsize>:** size.col => ws_col of 'struct winsize'    
**Instance Variable of <winsize>:** pixel.x  => ws_xpixel of 'struct winsize'    
**Instance Variable of <winsize>:** pixel.y  => ws_ypixel of 'struct winsize'    

### load-winsize :optional port-or-fd
* call ioctl with TIOCGWINSZ
* the default of `port-or-fd` is (current-input-port)
* type of return value: \<winsize\>

### read-winsize :optional port-or-fd
* call ioctl with TIOCGWINSZ
* type of return-value: \<pair\>
* format of return value: ```(size.row . size.col)```

### load-winsize! ws :optional port-or-fd
* call ioctl with TIOCGWINSZ and `ws`
* this function modifies `ws`, destructively
* type of return-value: \<boolean\> (\#t means that the executing is success)

### set-winsize ws :optional port-or-fd
* call ioctl with TIOCSWINSZ and `ws`
* type of return value: \<boolean\>

### make-winsize :key size.row size.col pixel.x pixel.y
* type of return value: \<winsize\>
