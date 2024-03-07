# My initialization files for Emacs 29.1 (or later)

## Introduction

There's hubris in publishing your editor customization files. I don't think mine are better
than most, but it does make showing others a certain setting much easier than having to copy
& paste it into a discussion.

## Requirements

I use Emacs 29.1 on macOS, and do not bother to test in environments I don't use.  But as
far as I can tell, there is nothing in this files that should not work in Emacs 29.1 or
later, that is running in a Unix-like environment (and most should work in Windows as
well). YMMV, of course.

Aside from Emacs itself, the only other requirments for loading this file without errors
this is having [use-package](https://github.com/jwiegley/use-package), which I use to
organize the configuration. Some settings won't work without extra software, but Emacs could
be started.

## Errors

I can not claim full credit to most of the code herein: a lot of it was found online when
searching for solutions to issues I had with Emacs at the time, and I did not bother to keep
a link to the source once I copied the solution into my init.el.

As such, any errors should not be attributed to me, but to the authors I copied from. I
would appreciate you notifying me on any such errors though, so I could fix them.
