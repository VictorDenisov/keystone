Keystone
========

[![Build Status](https://travis-ci.org/VictorDenisov/keystone.svg)](https://travis-ci.org/VictorDenisov/keystone)

Tested with mongodb version 3.0.3 mmapv2

Documentation
=============

API Version
-----------

This implementation of keystone supports only keystone V3 version. You will
need to configure openstack services to use only V3 endpoints. Sometimes it's
not a trivial endeavour. The details of how you can configure every service to
work with keystone v3 only are below.

Keystone V3 induces some limitations on openstack: horizon can't work with
domains and identity panel becomes unavailable. It means you need to work with
identity services using command line.

Port
----

This implementation of keystone is listening on port 35357 by default and can
be changed using configuration file. Several ports at the same time are not
allowed. So, you don't have port 5000 available by default in addition to 35357.

Domain Support
--------------

Currently domain support is very limited. You have only one default domain
with the name Default. You can determine default domain's id if you list all
domains in your keystone. You can specify domains in your requests, but they
have no effect on replies and this implementation doesn't take them into
account.

Only two API handlers for domains are available: list domains and show domain
details.

Groups Support
--------------

Support for groups is unavailable at the moment.

Region Support
--------------

The current version of keystone doesn't support regions.

Json Error Reporting
--------------------

Currently this keystone is not good at reporting errors in json format.
So, if you have an invalid policy.json file you can use json_verify tool
in order to find out what exactly is wrong with your file.

How to Configure Other Services to Use V3 Endpoints Only
--------------------------------------------------------

### Openstack Client ###

In order to interact with openstack you need to use openstack unified client.
This is the only CLI tool that is capable of interacting with keystone V3
endpoints. Conventional keystone client can talk only to keystone V2.

#### Adminrc File for Unified Openstack Client ####

> export OS_TOKEN="ADMIN"
> export OS_URL="http://localhost:35357/v3"
> export OS_IDENTITY_API_VERSION=3

Pay attention to the names of the variables. They are different from the names
of the variables for keystone V2.

#### Normal Openrc File ####

> export OS_USERNAME=cloud_admin
> export OS_PASSWORD=admin
> export OS_USER_DOMAIN_NAME=Default
> export OS_PROJECT_NAME=admin
> export OS_PROJECT_DOMAIN_NAME=Default
> export OS_IDENTITY_API_VERSION=3
> export OS_AUTH_URL=http://localhost:35357/v3

This is a sample openrc file for keystone V3 and openstack unified client. This
file works for conventional keystone v3 as well as for the implementation
described in this document. However even though you may specify project's domain
name and user's domain name they have no effect and are just ignored by this
implementation.

### Glance ###

In order to configure glance to use keystone v3 endpoints only, you need to add
these lines to glance.conf file:

> [keystone_authtoken]

> auth_section = generic_password

> [generic_password]
> auth_plugin = password
> username = glance
> user_domain_id = /default domain id/
> project_name = admin
> project_domain_id = /default domain id/
> password = glance
> auth_url = http://localhost:35357


Code and Architecture
=====================

Policy Configuration
--------------------

Policies are implemented as a separate json file.
Section identity of the json document describes rules for all actions available
in the current implementation of keystone. When you start the keystone server it
verifies that the policy.json file contains rules for all currently implemented
actions.

Rules in identity section may directly specify how to verify if the user is
authorized to perform the action or it can reference another rule from the main
section of the policy file.

Currently there is one built-in rule. It's rule owner. Authorizing code has
a concept called resource. What exactly the resource is varies from handler to
handler. All resources implement method is owner. This method verifies if the
token provided during the authentication is owner of this resource.

At the moment we suggest to use as a resource something that doesn't require an
extra round trip to DB. For example when we are manipulating user then
the resource is user id not the whole user because user id is available from the
request and full user record will require extra round trip to the database. This
behavior may be changed in future if it turns out to be absolutely inevitable
to retrieve the full resource for the authorization.

Also not every handler that works with resources provides authorization function
with any resource. Currently only several handlers do that. Most of the handlers
provide authorization function with EmptyResource. Its isOwner function always
returns true. It was done because we didn't want any extra work for no reason.
It was implemented this way after Looking at the default policy file for
keystone.

Field Names in Database Layer
-----------------------------

When you need to write requests for mongodb you may need to use record names.
The field names shouldn't be specified explicitly. They should be derived from
the entities' data structures.
Currently the approach is to create a variable called fieldNameF that contains
string representation of the field name. This representation should be
calculated. Here is an example below.

> endpointsF = T.pack $ nameBase 'endpoints

There is a convention to call these variables with ending F.

If there is a modifier applied to the field's name then this modifier should be
declared separately and used both in field name declaration and in the
derivement statement.

> endpointFieldMod = drop 1
>
> eidF = T.pack $ endpointFieldMod $ nameBase 'eid
>
> $(deriveBson endpointFieldMod ''Endpoint)

Maybe it's not the best mechanism, but at least it will provide some protection
from duplication errors for the time being.

Directory structure and imports
-------------------------------
Normally imports are explicit however sometimes haskell compiler requires us to
have separate files because of compilation stages and template haskell features
we are using. In this case we create another satellite file called Types (look
at Service and Service.Types modules).  Module Service.Types is imported into
Service module unqualified and without explicit declaration of symbols. So, if
you can't find where a certain symbol comes from then look at satellite Types
module.

Meaning of Suffixes
-------------------

F means it's a field name
C means it's a mongodb command
