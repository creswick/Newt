# Newt!

Newt is a trivially simple template instantianion tool.  WTF is a
template instatiotion tool? It turns files with named fields into
files without named fields!

Say you just spent twelve hours
[Yak-shaving](http://projects.csail.mit.edu/gsb/old-archive/gsb-archive/gsb2000-02-11.html)
to get a configuration file tweaked *just* right, and your friends
want to use it.  However, the configuration file has your user name
and password in it, so you can't just share the file with them.  At
the very least, you have to replace those values, then tell your
friends where to put *their* values, and *hopefully* those only occurr
once in the file you need to share.

Newt can make this easier.

Here's what you do:

 - make a copy of your config file somewhere so you don't break the one you actually use.
 - replace your user name with the string `<<<username>>>`
 - replace your password with the string `<<<password>>>`

Now, send your friends the new file with these instructions:

 1. Go install newt!
 2. Save this template to /tmp/myTemplate
 3. run `newt --source=/tmp/myTemplate --dest=<destination> username=yourUsername password=yourPassword`
   * Alternatively, you can leave off the `--source=` and `--dest=` bits:
     * `newt /tmp/myTemplate <destination> username=yourUsername password=yourPassword`
 4. There you go.  `<destination>` now has a populated version of the config file.

I *did* say Newt was trivially simple.

Aside from usernames and passwords, you can use newt to replace *any*
string key with *any* string value, as long as none of the keys
include ">>>" or "=".  It might even handle unicode someday.

I hope this is handy for writing scripts that need to set
configuration files, or if you write a lot of LaTeX documents, and you
want to fill in boiler plate from a command line.  I also have grand
design for pointing newt at a tarball of templates and having it
expand the tarball while fleshing out the details.  For now, you get
to work with one file at a time.

## Examples

### Create a new cabal project!

Input File (`in.cabal`):

    name:                <<<name>>>
    version:             0.0.0.1
    synopsis:            <<<synopsis>>>
    description:         <<<description>>>
    category:            Tools
    license:             BSD3
    License-file:        LICENSE
    author:              <<<author>>>
    maintainer:          <<<authoremail>>>
    Cabal-Version:       >=1.8.0.6
    build-type:          Simple
    
    Executable <<<name>>>
       Main-Is:          Main.hs
       hs-source-dirs:   src

Newt command:

    $ newt --source=in.cabal --dest=FooApp.cabal name=FooApp author="Rogan Creswick" authoremail=creswick@someemail.com

Result (in FooApp.cabal):

    name:                FooApp
    version:             0.0.0.1
    synopsis:            <<<synopsis>>>
    description:         <<<description>>>
    category:            Tools
    license:             BSD3
    License-file:        LICENSE
    author:              Rogan Creswick
    maintainer:          creswick@someemail.com
    Cabal-Version:       >=1.8.0.6
    build-type:          Simple
    
    Executable FooApp
       Main-Is:          Main.hs
       hs-source-dirs:   src

Note that the author needed to be put in quotes to prevent the shell from splitting the `author="Rogan Creswick"` argument to Newt into two arguments.

Also note that we didn't need to define `synopsis` or `description`.  They remain in the file, so you can partially fill a template if you want.

### List the keys

Newt can also tell you which keys you can specify for a given
template.  If you don't know what the valid keys are, then just run
newt with `--list`:

    $ newt --source=in.cabal --list
    author
    authoremail
    description
    name
    synopsis

