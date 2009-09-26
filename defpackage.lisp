;;;; rstructuredtext implementation for Common Lisp
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details

(in-package :cl-user)

;; we put the node names in their own package so they can be imported
;; (or not) separately if need be however this is not recommended
;; note some symbols clash with common-lisp names and need to be
;; shadowed if this package is used.

(defpackage :docutils.nodes
  (:documentation "Package containing standard docutils Node names")
  (:use) ;; do not import common lisp
  (:export
   #:node #:element #:text #:text-element #:document
   #:resolvable #:backlinkable #:root #:titular #:predecorative
   #:prebibliographic #:bibliographic #:decorative #:body #:general
   #:sequential #:abmonition #:special #:invisible #:part
   #:referential #:targetable #: #:title #:subtitle #:rubric
   #:docinfo #:author #:authors #:organization #:address #:contact
   #:version #:revision #:status #:date #:copyright #:decoration
   #:header #:footer #:structural #:section #:topic #:sidebar
   #:transition #:paragraph #:compound #:container
   #:bullet-list #:enumerated-list
   #:list-item #:definition-list #:definition-list-item #:term
   #:classifier #:definition #:field-list #:field #:field-name
   #:field-body #:option #:option-argument #:option-group #:option-list
   #:option-list-item #:option-string #:description #:literal-block
   #:doctest-block #:line #:line-block #:attribution #:attention #:caution
   #:error #:block-quote #:system-message
   #:danger #:admonition #:important #:note #:tip #:hint #:warning
   #:comment #:substitution-definition #:target #:footnote
   #:citation #:label #:figure #:caption #:legend #:table #:tgroup
   #:colspec #:thead #:tbody #:row #:entry #:system #:pending #:inline
   #:raw #:emphasis #:strong #:literal #:reference #:footnote-reference
   #:citation-reference #:substitution-reference #:title-reference
   #:abbreviation #:acronym #:superscript #:subscript #:image
   #:problematic #:generated #:meta #:equation #:math
   #:evaluateable #:inline-evaluation #:block-evaluation))

(defpackage docutils.utilities
    (:documentation "Common utilities used by several docutils components")
    (:use :cl :trivial-gray-streams)
    (:import-from :docutils.nodes #:document #:line)
    (:import-from :data-format-validation #:split-string)
    (:export
     #:+wsp+ #:line-blank-p #:escape2null #:unescape #:split-lines
     #:make-id #:normalise-name #:whitespace-normalise-name #:indented-block
     #:indent-level #:line-length #:rstrip #:strip #:lstrip #:wsp-char-p
     #:canonical-text #:translated-text #:*language* #:author-separators
     #:read-lines #:translate-text #:canonical-text #:babel #:latex-quotes
     #:latex-double-quote-replacement #:namespace #:make-name #:*namespace*
     #:length-unit #:convert-length-unit #:when-bind #:while #:for
     #:is-suffix-p #:is-prefix-p #:copy-stream
     #:find-file #:search-path* #:line-wrap-stream #:with-block-indentation
     #:last-char #:unwrite-char))

(defpackage :docutils
  (:documentation "Document handling and structured text parsing")
  (:use :cl :docutils.utilities :docutils.nodes :data-format-validation)
  (:shadowing-import-from :cl #:warning #:error #:inline #:special)
  (:shadowing-import-from :docutils.nodes #:date)
  (:export
   ;; source interface
   #:read-lines #:settings #:new-document
   ;; reader interface
   #:transforms #:reader
   #:*document* #:read-document #:*unknown-reference-resolvers*
   ;; transforming interface
   #:transform #:node #:field-value #:*field-types*
   ;; writer interface
   #:writer #:supports-format #:visit-node #:parts #:part-append
   #:part-prepend #:write-document #:with-part #:write-part
   ;; error reporting
   #:report #:record-system-message
   #:error-level #:error-message #:error-text-block #:error-line
   #:error-severity #:error-node #:markup-condition
   #:with-reports-to-node #:error-data
   ;; node interface
   #:document #:parent #:as-text #:line #:as-sexp #:add-class
   #:make-node #:attribute #:rem-attribute #:child #:rem-child #:index
   #:add-child #:allowed-child-p #:with-nodes #:with-children
   #:number-children
   #:with-attributes #:next-sibling #:prev-sibling
   #:set-id #:add-class #:copy-of-node #:language #:*current-line-number*
   #:collate-nodes #:targets #:refnames #:refids
   #:source #:nameids #:ids #:named-node
   #:backrefs #:referenced #:remove-node #:substitute-node #:resolved
   #:add-backref
   ;; settings interface
   #:register-settings-spec #:setting
   ;; evaluateable nodes interface
    #:evaluate  #:*evaluation-error-hook* #:*evaluator*
    #:*default-reader-package* #:output-format #:evaluation-language
   ;;simple example usage
   #:read-rst #:write-html #:write-latex))

(defpackage :docutils.transform
  (:documentation "Package containing standard docutils Node names")
  (:use :cl :docutils :docutils.utilities)
  (:import-from :data-format-validation #:format-output #:date #:split-string)
  (:shadow #:docinfo)
  (:export
   #:decorations #:final-checks #:filter-messages
   #:filter #:contents #:sectnum #:class-attribute #:target-notes
   #:doctitle #:docinfo #:substitutions #:chained-targets #:simple-transform
   #:anonymous-hyperlinks #:indirect-hyperlinks #:footnotes
   #:external-targets #:internal-targets #:fignum
   #:evaluate #:evaluate-transform #:*evaluator*
   #:resolve-media))