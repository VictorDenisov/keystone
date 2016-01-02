Operations Guide
================

Configuring LDAP
----------------

Keystone assumes that you are trying to configure ldap for identity service
if you have ldap section in the config file.

Dangling Assignments
--------------------

Mongo database can't really have transactions across the whole database.  It
most cases it's fine, but assignments are particularly vulnerable.  In order
for them to be consistent they need to have all three entities present in the
database - (user, project, role).  Since mongo doesn't have transactions any of
these three components can be missing, but the assignment can still exist.
Every time we need to list assignments we find them and then we verify that all
components of every assignment exist.  It allows us to present to the user a
consistent picture, but we can have extra assignment entities in the database
that should be ignored. Since we assume that users, projects or roles are not
deleted very often it should be fine for now.
