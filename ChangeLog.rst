===========
 ChangeLog
===========

1.0.1 (2021-02-03)
==================

* Now a warning logged when a you pass plist or alist ``:PARAMS`` into the ``GITHUB:GET`` function.
  Because this is not how ``:PARAMS`` should be used.

1.0.0 (2021-02-01)
==================

* Incompatible change! Now for URLs which returns lists of items, like ``{"items": [...], "total": 42}``
  ``get`` function returns a plist in same format as original github handle.
  Previously, a plain list of items was returned.

  This way it is possible to use ``:limit`` option and still know how many items in the collection.

  This change also fixes problems whith GitHub's URLs returned items in keys other than ``"items"``.
  Now any key is supported.

0.3.0 (2021-01-22)
==================

* Variable ``github:*token*`` now is bound to ``nil`` by default.

0.2.1 (2021-01-21)
==================

* Error on rate limit hit was fixed.

0.2.0 (2021-01-21)
==================

* Now ``github:*token*`` is optional and library logs a warning
  if token is unbound. In this case requiests will be unauthenticated
  and rate limit will be about 60 requests per hour.
* Added support for timeouts, default is 10 seconds and stored in
  ``github:*default-timeout*``. Also you can pass it as a keyword
  argument to ``get`` and ``post`` methods.
* Fixed work with latest Dexador.

0.1.1 (2019-06-26)
==================

* Fixed a way how empty search results are processed.

0.1.0
=====

* Initial release.
