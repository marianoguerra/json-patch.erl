json-patch.erl: JSON Patch (RFC 6902) implementation for erlang data structures
===============================================================================

this is a thing wrapper around `dotto <https://github.com/marianoguerra/dotto>`_
that translates json patch objects into dotto operations and then applies
them to a given object.

it's useful if you have a rest API and want to implement PATCH method with
"application/json-patch+json".

if you want to manipulate erlang data structures from code it's better to use
dotto directly.

build
-----

::

    make

run tests
---------

::

    make tests

see how it works
----------------

just read the RFC or look the test/tests.json file, the usage is::

    {ok, ParsedPatch} = jsonpatch:parse(JsonPatch),
    {ok, Result} = jsonpatch:patch(ParsedPatch, Obj).

JsonPatch can be a binary containing a json patch object or an erlang data
structure, with the result of parsing a json patch object from json.

LICENSE
-------

MPL 2.0
