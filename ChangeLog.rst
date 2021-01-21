===========
 ChangeLog
===========

0.2.0 (2021-01-21)
==================

* Now ``github:*token*`` is optional and library logs a warning
  if token is unbound. In this case requiests will be unauthenticated
  and rate limit will be about 60 requests per hour.
* Fixed work with latest Dexador.

0.1.1 (2019-06-26)
==================

* Fixed a way how empty search results are processed.

0.1.0
=====

* Initial release.
