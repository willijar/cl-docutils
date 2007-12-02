.. -*-RST-*-

========================
Docutils for Common Lisp
========================

:Author:       Dr John A.R. Williams
:organization: Aston University
:Address:      Aston Triangle, Birmingham B4 7ET
:Contact:      J.A.R.Williams@aston.ac.uk
:date:         2nd July 2006
:status:       Work in Progress
:version:      $version$
:copyright: This document has been placed in the public domain. You
            may do with it as you wish. You may copy, modify,
            redistribute, reattribute, sell, buy, rent, lease,
            destroy, or improve it, quote it at length, excerpt,
            incorporate, collate, fold, staple, or mutilate it, or do
            anything else to it that your or anyone else's heart
            desires.
:abstract: A description of the Common Lisp implementation of the
           Docutils suit for parsing `restructured text`_ 

.. meta::
   :keywords: reStructuredText, Docutils, parser, Common Lisp

.. contents:: Table of Contents

Introduction
============

Docutils is a very useful document processing system originally
developed in Python. Although Docutils provides a generic framework,
it is really only widely used in the Python community to format
document and programme strings formatted in the ReStructured text
format. 

The aim of this system is to reproduce the essential functionality of
the Python Docutils system in Lisp in such a way that it can easily be
used from within Lisp programmes to take user input (in the
restructured text format) and present it in a variety of presentation
formats. This implementation, although strongly influenced in design
on the original Python system tries to use appropriate Lisp idioms
where they would be more flexible or easier to read. It is not
guaranteed that this implementation will for the same input produce
the same output as the original system. 

Known Differences from Reference Python Implementation
======================================================

--------------
Character Sets
--------------

The main feature not explicitly implemented in the Common Lisp
implementation is support for character set translation on input and
output. It is assumed that Gray or simple streams could be used to
implement this functionality orthogonally to this code. THe Common
Lisp implementation uses standard Lisp string handling and the
CL-PPCRE regular expression library. It is believe (but untested) that
if the Lisp implementation supports unicode strings then this package
will support them.

----------------------
Nested Inline Elements
----------------------

This implementation supports nested inline elements (where they make
sense) whereas the reference implementation did not.

--------------
Error Messages
--------------

Error messages is handled differently in this implementation with the
result that they may not be located in exactly the same position
relative the cause as in the Python_ implementation.

----------------------------
Python Enhancement Proposals
----------------------------

Parsing of Python Enhancement Proposals (PEPs) is not supported as
these have significance only for the Python_ community, however PEP
references are supported as to enable the use of the standalone
test documents provided with the reference Python implementation.

-------------
Customisation
-------------

A fundamental requirement and important feature of restructured text
is that it can easily be customised and extended for use in embedded
applications (particularly web applications) without changing the core
language. Restructured text was explicitly designed with this in
mind, however the `Common Lisp`_ implementation is quite different,
and more powerful in this regard than the reference Python_
implementation.

---------------------
Additional Extensions
---------------------

Maths
-----

New Math roles and directives have been implemented allowing the
embedding of LaTeX Mathematical elements either inline or as
blocks. Processing of these requires that LaTeX be installed together
with the PNM image processing tools.

Customisation
=============

----------
Directives
----------

The major feature for language extension at the block level is by the
use of the named directive explicit markup. New directives can be
defined using the ``def-directive`` macro.

Roles
-----

Roles provide a means of implementing new inline elements. They are
defined using the ``def-role`` macro.

Transformations
---------------

Transformations are associated with the parser element. If you wish
new transformations to be run, use a subclass of the parser which
return the appropriate list of transformations.


.. _restructured text: http://docutils.sourceforge.net/docs/ref/rst/
.. _Python: http://www.python.org/
.. _Common Lisp: http://www.cliki.net/
.. _CMUCL: http://www.cons.org/cmucl/