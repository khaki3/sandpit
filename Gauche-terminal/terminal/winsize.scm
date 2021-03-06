;;;
;;; winsize.scm
;;;
;;; THE ORANGINA LICENSE
;;; Copyright 2013 wasao
;;; All rights reserved.
;;;
;;; You can do whatever you want with this stuff.
;;; If we meet some day, and you think this stuff is worth it,
;;; you can buy me a bottle of orangina in return.
;;;
;;; wasao
;;; walter@wasao.org
;;;

(define-module terminal.winsize
  (export-all)
  )
(select-module terminal.winsize)

(inline-stub
 (when "!defined(GAUCHE_WINDOWS)"
 "#ifndef GAUCHE_WINSIZE_H"
 "#define GAUCHE_WINSIZE_H"
 "#include <gauche.h>"
 "#include <gauche/port.h>"
 "#include <gauche/class.h>"
 "#include <termios.h>"
 "#include <sys/ioctl.h>"
 "#endif"

 "typedef struct ScmWinSizeRec {"
 "	SCM_HEADER;"
 "	struct winsize ws;"
 "} ScmWinSize;"
 "SCM_CLASS_DECL(Scm_WinSizeClass);"
 "#define SCM_WINSIZE_P(obj) \
  	SCM_XTYPEP(obj, &Scm_WinSizeClass)"
 "#define SCM_WINSIZE_DATA(obj) \
  	((ScmWinSize*)obj)"
 "#define SCM_MAKE_WINSIZE(data) \
	(Scm_MakeWinSize(data))"

 (define-cfn make-winsize () :static
   (let* ([ws::ScmWinSize* (SCM_NEW ScmWinSize)])
     (SCM_SET_CLASS ws (& Scm_WinSizeClass))
     (return (SCM_OBJ ws))))

 "ScmObj Scm_MakeWinSize(ScmWinSize* data) {"
 "	SCM_SET_CLASS(data, &Scm_WinSizeClass);"
 "  SCM_RETURN(SCM_OBJ(data));"
 "}"
 
 (define-type <winsize>
   "ScmWinSize*" "ScmWinSize*"
   "SCM_WINSIZE_P" "SCM_WINSIZE_DATA" "SCM_MAKE_WINSIZE")
 
 (define-cclass <winsize>
   "ScmWinSize*" "Scm_WinSizeClass" ()
   ([size.row :c-name "ws.ws_row"    :type <ushort>]
    [size.col :c-name "ws.ws_col"    :type <ushort>]
    [pixel.x  :c-name "ws.ws_xpixel" :type <ushort>]
    [pixel.y  :c-name "ws.ws_ypixel" :type <ushort>])
   (allocator (c "make_winsize"))
   (printer (Scm_Printf port "#<<winsize> @%p %d:%d>" obj
                        (ref (-> (SCM_WINSIZE_DATA obj) ws) ws_row)
                        (ref (-> (SCM_WINSIZE_DATA obj) ws) ws_col))))

 (define-cproc load-winsize (:optional (port-or-fd::<port> (current-input-port))) :: <winsize>
   (let* ([fd::int (Scm_GetPortFd (SCM_OBJ port-or-fd) FALSE)]
          [t::ScmWinSize* (SCM_NEW ScmWinSize)])
     (ioctl fd TIOCGWINSZ (& (-> t ws)))
     (result t)))

 (define-cproc read-winsize (:optional (port-or-fd::<port> (current-input-port))) :: <pair>
   (let* ([fd::int (Scm_GetPortFd (SCM_OBJ port-or-fd) FALSE)]
          [t::ScmWinSize* (SCM_NEW ScmWinSize)])
     (ioctl fd TIOCGWINSZ (& (-> t ws)))
     (return (Scm_Cons (Scm_MakeIntegerFromUI (ref (-> t ws) ws_row))
                       (Scm_MakeIntegerFromUI (ref (-> t ws) ws_col))))))
 
 (define-cproc load-winsize! (t::<winsize> :optional (port-or-fd::<port> (current-input-port))) :: <boolean>
   (let* ([fd::int (Scm_GetPortFd (SCM_OBJ port-or-fd) FALSE)]
          [rv::int (ioctl fd TIOCGWINSZ (& (-> (SCM_WINSIZE_DATA t) ws)))])
     (if (< rv 0) (result 0) (result 1))))

 (define-cproc set-winsize (t::<winsize> :optional (port-or-fd::<port> (current-input-port))) :: <boolean>
   (let* ([fd::int (Scm_GetPortFd (SCM_OBJ port-or-fd) FALSE)]
          [rv::int (ioctl fd TIOCSWINSZ (& (-> (SCM_WINSIZE_DATA t) ws)))])
     (if (< rv 0) (result 0) (result 1))))
 ))

(cond-expand
  (gauche.os.windows)
  (else
   (define-syntax make-winsize
     (syntax-rules ()
       [(_) (make <winsize>)]
       [(_ initargs ...) (make <winsize> initargs ...)]))
   ))
