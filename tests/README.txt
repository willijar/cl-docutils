========================
Docutils for Common Lisp
========================

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
the same output as the original system. The main feature not
explicitly implemented in the Common Lisp implementation is support
for character set translation on input and output. It is assumed that
Gray or simple streams could be used to implement this functionality
orthogonally to this code. THe Common Lisp implementation uses
standard Lisp string handling and the CL-PPCRE regular expression
library. It is believe (but untested) that if the Lisp implementation
supports unicode strings then this package will support them.


