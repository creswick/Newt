# Changelog

## 0.0.3.0 - Feature Release (May 12, 2011)

 * Made the command line interface more intuitive, per the following algorithm:
  * If --source is not specified, the first undefined non-key=value argument is used as the source.
    * if --dest is *also* not specified, the second undefined non-key=value argument is used as the dest.
  * If --source is specified but dest is not,  the first undefined non-key=value argument is used as the dest.
  * If neither --source or --dest are specified, and there is only one  undefined non-key=value argument, then that is used as the source, and the dest is stdout.
  * If the above is true, and there are *no* undefined non-key=value arguments, then source is stdin and dest is stdout.
 * BugFix: Newt no longer tries to process the *content* of binary files when creating a template from a directory. (Binary files are copied over, and the filenames are updated per the templating algorithm.)
 * BugFix: Thanks to J3H, the template tags are no longer detected via regex.
  * this should improve performance (not that that was a problem)
  * it makes the code a bit cleaner
  * removes dependencies on `regex-base` and `regex-pcre` (but `unixutils` depends on them anyway, via `regex-tdfa`)
  * and, above all, should make it *much* easier to specify custom `--prefix=` and `--suffix=` values on the command line (but that's untested).
 * Added a healthy, but not overwhelming, number of tests.

## 0.0.1.0 - Initial Release (May 8, 2011)

 * Initial release with basic functionality.
