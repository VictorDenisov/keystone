Keystone
--------

Tested with mondodb version 3.0.3 mmapv2

Documentation
-------------

Json error reporting
====================

Currently this keystone is not good at reporting errors in json format.
So, if you have an invalig policy.json file you can use json_verify tool
in order to find out what exactly is wrong with your file.

Meaning of suffixes
===================

F means it's a field name
C means it's a mongodb command
