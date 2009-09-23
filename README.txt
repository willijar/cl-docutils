.. -*-RST-*-

=====================================
CL-DOCUTILS: Docutils for Common Lisp
=====================================

:Author:       Dr John A.R. Williams
:Contact:      J.A.R.Williams@jarw.org.uk
:created:      22 September 2009
:status:       Draft
:version:      0.1
:copyright:    © 2009 J.A.R. Williams
:Abstract:   |CDU| is a Common Lisp implementation of the Docutils text
  processing system for processing plaintext into presentational
  formats such as HTML and LaTeX. It is based upon the Python Docutils 
  reference implementation but uses Common Lisp idioms making it
  easier to extend and more flexible. As with the reference
  implementation it includes a parser for the `reStructured text`_
  plaintext markup syntax which is suitable for marking up
  documentation and for use as user markup for collaborative web
  sites. It is successfully used to support a
  higher education peer-review assessment and online tutorial system.

.. meta::
   :keywords: reStructuredText, Docutils, text, Common Lisp

.. contents:: Table of Contents

.. |CDU| replace:: CL-DOCUTILS
.. _restructured text: http://docutils.sourceforge.net/docs/ref/rst/
.. _Python: http://www.python.org/
.. _FlexiStreams: http://weitz.de/flexi-streams/

Download and Installation
=========================

|CDU| together with this documentation can be downloaded from
<http://www.jarw.org.uk/lisp/cl-docutils.tar.gz>. The
current version is 0.1.

|CDU| comes with a system definition for `ASDF
<http://www.cliki.net/asdf>`_ and is compiled and loaded in the usual
way. It depends upon `CL-DATA-FORMAT-VALIDATION
<http://www.jarw.org.uk/lisp/cl-data-format-validation.html>`_ and
`<CL-PPCRE <http://weitz.de/cl-ppcre/>`_. A git repository is
available at <http://www.jarw.org.uk/lisp/cl-docutils.git>. It has
been developed and tested using SBCL but should be portable to any
ANSI-compliant implementation of Common Lisp.

|CDU| is made available under the terms of the GPL v3 license - see
 the file ``LICENSE.txt`` for details.

Support
=======

For questions, bug reports, feature requests, improvements, or patches
relating to this implementation please email <J.A.R.Williams@jarw.org.uk>.

See `reStructured text`_ for the wide range of documentation on the
restructured text syntax and `Docutils
<http://docutils.sourceforge.net/index.html>`_ for information on the
reference implementation on which this is based.

Convenience Functions For Users
===============================

Although this is intended as a library for embedding in other
applications the following functions in the DOCUTILS package are
provided as examples suitable for direct use:

- function **read-rst** `input => document`

  Read and parse restructured text from an input (a stream, string or
  pathname) and generate a document object.

- function **write-html** `output document`

  Write the parsed document to output (a stream or pathname) in html
  format.

- function **write-latex** `output document`

  Write the parsed document to output (a stream or pathname) in LaTeX
  format.

API For Developers
==================

This documentation is in early stages and those wishing to use the
library are encouraged to read the documentation for the Python Docutils
implementation to get a flavour and examine the code here to determine
differences. The generic interface is largely defined and documented
in the file ``publisher.lisp`` which is a good place to start. Queries
to the author as mentioned under support are also welcome.

---------
Data Flow
---------

The following diagram shows the components and data flow::

               -----READER----
              /               \
             /                 \
 SOURCE ---> PARSE ---> TRANSFORM ---> WRITER ---> OUTPUT

------
Source
------

Specialised methods are provided for `stream`, `pathname` and `string`
source types. It is the responsibility of the source and output
streams to handle external character encodings.

- generic function **new-document** `source => document`

  Implementations of this should create a new document instance with
  the appropriate non-default document settings from source. 

  The default method
  loads no settings and the method specialised on pathname source will load
  settings from `*standard-config-files*` and `cl-docutils.conf` in
  the pathname directory.

- generic function **read-lines** `source => vector`

  Given a source, return a vector of text lines for subsequent
  parsing.

- generic function **settings** `source => hash-table`

  Return a hash table of processing settings to be associated with
  documents with this source.

-----
Parse
-----

- generic function **read-document** `source reader => document`

  Read a document from `source` using `reader` and using the
  transforms associated with this reader return a fully parsed
  and transformed document.

The following restructured text specific API is exported from package
``docutils.parser.rst``.

- class **rst-reader**

  Class of the standard restructured text parser.

- macro **def-role** `((name textvar &rest argument-list) &body body)`

  Define a new interpreted 
  role element handler for role with canonical name `name`.
  `textvar` is the name of the variable holding the textual argument
  to be referenced in `body` and `argument-list` is as follows::

   lambda-list::= ({var | (var [[specializer] [default]])}*
                  [{{&content {var [[specializer] [default]]}}] )

  `body` is the list of forms which should return a `docutils:text`
  node instance to be substituted into the document 

- macro **def-directive** `((name parent &rest argument-list) &body body)`

  Define a new directive element handler with name `name`. Similar to
  ``def-role`` except the `body` is responsible for adding nodes to
  the `parent` node.


---------
Transform
---------

- generic function **transforms** `reader => list`

  Return a list of transform specifications (class names) to be
  applied by this reader after parsing.

- generic function **transform** `transform`

  Evaluate the given transform instance on its node.

- generic function **do-transforms** `transforms document => document`

  Apply a list of transformation to the document in priority order.


------
Writer
------

- generic function **visit-node** `writer node`

  Process the node updating `writer` parts accordingly. Visiting a
  document will clear all document parts first.
 
- generic function **write-document** `writer document destination`

  Use `writer` to visit the `document` and write all parts in order
  to `destination`

- generic function **write-part** `writer part destination`

  Write a given named document part from a `writer` to `destination`. 

---------
Variables
---------

- variable **`*unknown-reference-resolvers*`**

  List of functions to try to resolve unknown references during
  transformation


- variable **`*standard-config-files*`**

  List of pathnames to files to be parsed in order for document
  configuration settings.

- variable **`*default-reader-package*`**

  Value to bind `*package*` to when reading lisp forms from the
  document.

- variable **`*evaluator*`**

  Name of function to use to evaluate lisp forms in evaluateable
  nodes/

- variable **`*evaluation-error-hook*`**

  Function to be called if there are errors when calling `*evaluator*`

Differences from Reference Implementation
=========================================

Character Sets
  This Common Lisp implementation uses standard Lisp string handling and
  the CL-PPCRE regular expression library. Character set translation on
  input and output is orthogonal to this processing and can be formed
  using the appropriate specialised stream libraries such as
  FlexiStreams_.

Nested Inline Elements
  This implementation supports nested inline elements (where they make
  sense) whereas the reference implementation did not.

Error Messages
  Error messages is handled differently in this implementation with the
  result that they may not be located in exactly the same position
  relative the cause as in the Python implementation.

Python Enhancement Proposals
  Parsing of Python Enhancement Proposals (PEPs) is not supported as
  these have significance only for the Python community, however PEP
  references are supported as to enable the use of the standalone
  test documents provided with the reference Python implementation.

Customisation
=============

A fundamental requirement and important feature of restructured text
is that it can easily be customised and extended for use in embedded
applications (particularly web applications) without changing the core
language. Restructured text was explicitly designed with this in
mind, however the Common Lisp implementation is quite different,
and more powerful in this regard than the reference Python
implementation.

Directives
 The major feature for language extension at the block level is by the
 use of the named directive explicit markup. New directives can be
 defined using the ``def-directive`` macro.

Roles
 Roles provide a means of implementing new inline elements. They are
 defined using the ``def-role`` macro.

Transformations
 Transformations are associated with the parser element. If you wish
 new transformations to be run, use a subclass of the parser which
 return the appropriate list of transformations.

-------------------
Additional Elements
-------------------

- role **math**

  This role allows the embedding of mathematical expressions using
  LaTeX syntax. An implict syntax, delimiting the argument with '$$' is
  also provided.

- directive **equation**

  This directive allows the embedding of equations in LaTeX format.

- role **eval**

  A role allowing the embedding of inline evaluateable expressions in
  Lisp syntax

- directive **evaluation**

  A directive allowing embedding of expressions to be evaluated -
  Currently only Lisp expressions are supported.

Acknowledgements
================

The many contributers to the original Docutils project on which this based
and the many examples of code made available by Common Lisp community
which have inspired and taught me.