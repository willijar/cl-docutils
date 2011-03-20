;;;; Regular expression parse trees for Common Lisp restructured text
;;;;;implementation
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; See
;;;; http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html

(in-package :docutils.parser.rst)

;;;Fragments of patterns used by transitions.

(define-parse-tree-synonym wsp
    (:char-class #\space #\newline #\Return #\tab #\Page #\rubout
                 #+sbcl #\Vt
                 #+ccl #\PageUp))

(define-parse-tree-synonym alpha
    (:char-class (:range #\a #\z) (:range #\A #\Z)))

(define-parse-tree-synonym alphanum
    (:char-class (:range #\a #\z) (:range #\A #\Z) (:range #\0 #\9)))

(define-parse-tree-synonym digit (:char-class (:range #\0 #\9)))

(define-parse-tree-synonym alphanum+
    (:char-class (:range #\a #\z) (:range #\A #\Z) (:range #\0 #\9) #\_ #\-))

(define-parse-tree-synonym nonalphanum7bit
    (:char-class (:range #\! #\/) (:range #\: #\@)
		 (:range #\[ #\`) (:range #\{ #\~)))

(define-parse-tree-synonym optname
    (:sequence alphanum (:greedy-repetition 0 NIL alphanum+)))

(define-parse-tree-synonym optarg
    (:alternation
     optname
     (:sequence #\< alphanum
		(:greedy-repetition 1 nil (:inverted-char-class #\< #\>))
		#\>)))

;;; options use 3 registers
(define-parse-tree-synonym shortopt
    (:sequence
     (:NEGATIVE-LOOKBEHIND (:alternation #\+ #\-))
     (:register (:sequence (:alternation #\+ #\-) alpha))
     (:greedy-repetition 0 1
			 (:sequence
			  (:register #\space)
			  (:register optarg)))))

(define-parse-tree-synonym longopt
    (:sequence
     (:register (:sequence (:alternation "--" #\/) optname))
     (:greedy-repetition 0 1
			 (:sequence
			 (:register (:char-class #\space #\=))
			 (:register optarg)))))

(define-parse-tree-synonym option
    (:alternation longopt shortopt))

(define-parse-tree-synonym option-marker
    (:sequence
     :start-anchor
     option
     (:greedy-repetition 0 nil (:sequence ", " option))
     (:alternation (:greedy-repetition 2 2 #\space) :end-anchor)))

(define-parse-tree-synonym line
    (:sequence
     :start-anchor
     (:register nonalphanum7bit)
     (:greedy-repetition 0 nil (:back-reference 1))
     (:greedy-repetition 0 nil #\space)
     :end-anchor))

(define-parse-tree-synonym non-whitespace-before
    (:NEGATIVE-LOOKBEHIND wsp))

(define-parse-tree-synonym non-whitespace-escape-before
    (:NEGATIVE-LOOKBEHIND (:alternation wsp #\null)))

(define-parse-tree-synonym whitespace-after
    (:POSITIVE-LOOKAHEAD wsp))

(define-parse-tree-synonym non-whitespace-after
    (:NEGATIVE-LOOKAHEAD wsp))


;; Alphanumerics with isolated internal [-._] chars (i.e. not 2 together):
(define-parse-tree-synonym simplename
    (:SEQUENCE
     (:GREEDY-REPETITION
      1 NIL
      (:GROUP (:SEQUENCE (:NEGATIVE-LOOKAHEAD #\_) alphanum)))
     (:GREEDY-REPETITION
      0 NIL
      (:GROUP
       (:SEQUENCE (:CHAR-CLASS #\- #\. #\_)
		  (:GREEDY-REPETITION
		   1 NIL
		   (:GROUP (:SEQUENCE (:NEGATIVE-LOOKAHEAD #\_)
				      alphanum))))))))

(define-parse-tree-synonym whitespace-after
    (:POSITIVE-LOOKAHEAD (:CHAR-CLASS #\space #\newline)))

(define-parse-tree-synonym whitespace-before
    (:POSITIVE-LOOKBEHIND (:CHAR-CLASS #\space #\newline)))

(define-parse-tree-synonym not-escaped (:NEGATIVE-LOOKBEHIND #\Null))

(define-parse-tree-synonym start-phrase-prefix
    (:ALTERNATION
     (:POSITIVE-LOOKBEHIND :START-ANCHOR)
     (:POSITIVE-LOOKBEHIND
      (:CHAR-CLASS #\- #\/ #\: #\space  #\Newline
		   #\' #\" #\( #\[ #\{ #\<))))

(define-parse-tree-synonym end-phrase-suffix
    (:ALTERNATION
     (:POSITIVE-LOOKAHEAD :END-ANCHOR)
     (:POSITIVE-LOOKAHEAD
      (:CHAR-CLASS #\- #\/ #\: #\. #\, #\; #\! #\? #\Space #\Newline #\Null
                   #\' #\" #\) #\] #\} #\>))))

(defmacro define-recursive-element-parse-tree(name start &optional (end start))
  "Matches an element that is recursive i.e. uses a greedy match"
  `(define-parse-tree-synonym ,name
    (:sequence
     start-phrase-prefix
     ,start
     non-whitespace-before
     (:register (:GREEDY-REPETITION 0 NIL :everything))
     non-whitespace-before
     ,end
     end-phrase-suffix)))

(define-parse-tree-synonym uric
    ;; Valid URI characters (see RFC 2396 & RFC 2732);
    ;; final \x00 allows backslash escapes in URIs:
    (:CHAR-CLASS #\- #\_ #\. #\! #\~ #\* #\' #\( #\) #\[ #\] #\;
		 #\/ #\: #\@ #\& #\= #\+ #\$ #\, #\%
		 (:RANGE #\a #\z) (:RANGE #\A #\Z) (:RANGE #\0 #\9) #\Null))

(define-parse-tree-synonym uri-end-delim
    ;; Delimiter indicating the end of a URI (not part of the URI):
    (:CHAR-CLASS #\>))

(define-parse-tree-synonym urilast
    ;; Last URI character; same as uric but no punctuation:
    (:CHAR-CLASS #\_ #\~ #\* #\/ #\= #\+ (:RANGE #\a #\z) (:RANGE #\A #\Z)
		 (:RANGE #\0 #\9)))

(define-parse-tree-synonym uri-end
    ;; End of a URI (either 'urilast' or 'uric followed by a
    ;; uri_end_delim')
    (:ALTERNATION urilast (:sequence uric uri-end-delim)))

(define-parse-tree-synonym emailc
    (:CHAR-CLASS #\- #\_ #\! #\~ #\* #\' #\{ #\| #\} #\/ #\# #\? #\^ #\` #\&
		 #\= #\+ #\$ #\%
		 (:RANGE #\a #\z) (:RANGE #\A #\Z) (:RANGE #\0 #\9) #\Null))

(define-parse-tree-synonym email-pattern
    (:SEQUENCE
     (:GREEDY-REPETITION 1 NIL emailc) ; name
     (:GREEDY-REPETITION 0 NIL
                         (:SEQUENCE #\. (:GREEDY-REPETITION 1 NIL emailc)))
     not-escaped #\@ ; unescaped @
     (:GREEDY-REPETITION 1 NIL emailc) ; host
     (:GREEDY-REPETITION 0 NIL
                         (:SEQUENCE #\. (:GREEDY-REPETITION 1 NIL emailc)))
     uri-end)) ; final URI char

(define-parse-tree-synonym uri
    (:SEQUENCE
     start-phrase-prefix
     (:alternation
      (:register
       (:sequence
        (:register
         (:sequence
          alpha
          (:GREEDY-REPETITION
           0 NIL
           (:alternation alphanum (:char-class #\. #\+ #\-))))) ; scheme
        #\:
        (:sequence #\/  (:greedy-repetition 0 1 #\/))
        (:greedy-repetition 0 nil uric)
        uri-end
        (:GREEDY-REPETITION
         0 1         ; query
         (:sequence #\? (:greedy-repetition 0 nil uric) uri-end))
        (:GREEDY-REPETITION
         0 1         ; fragment
         (:sequence #\# (:greedy-repetition 0 nil uric) uri-end))))
      (:register email-pattern))
     end-phrase-suffix))

(define-parse-tree-synonym embedded-uri
    (:sequence
     (:alternation
      (:positive-lookbehind (:alternation #\space #\newline))
      (:positive-lookbehind :start-anchor))
     #\<
     non-whitespace-after
     (:register (:greedy-repetition 1 nil (:inverted-char-class #\< #\> #\null)))
     non-whitespace-before
     #\> :end-anchor))



;; from RFC 3986 (([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))"))
;; registers are
;;   scheme    = $2
;;   authority = $4
;;   path      = $5
;;   query     = $7
;;   fragment  = $9
