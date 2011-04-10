# $Id: Makefile,v 1.144.6.4 2001/10/22 16:26:58 layer Exp $
# This makefile requires GNU make.

include version.mak

SHELL = sh

###############################################################################
# $(OS) is from the environment on Windows NT
ifeq ($(OS),Windows_NT)
ifdef xemacs
#### non-Cygwin can't do -nw
#xemacsdir = $(shell /cygdrive/c/perl/bin/perl xemacsdir.pl)
#emacs = $(xemacsdir)/XEmacs-21.1.9/i386-pc-win32/xemacs.exe
#### Cygwin version is just right
emacs = /usr/bin/i686-pc-cygwin32/xemacs
pwd = $(shell pwd)
else
emacsdir = $(shell /cygdrive/c/perl/bin/perl emacsdir.pl)
emacs = $(emacsdir)/bin/emacs.exe
# ../bin/pwd prints like c:/... instead of /c/... like the cygnus version.
pwd = $(shell ../bin/pwd.exe)
endif
###############################################################################
else ### unix
ifdef xemacs
emacs = xemacs
else 
emacs = emacs
endif
pwd = $(shell pwd)
endif
###############################################################################

all default:	fi-vers.el compile

compile:	fi-vers.el
	$(emacs) -nw -batch -q -l $(pwd)/fi-compile.el -kill

fi-vers.el: Makefile version.mak
#	rm -f fi-vers.el
	echo ';; automatically generate file--do not edit.' > fi-vers.el
	echo '(defvar fi:emacs-lisp-interface-version)' >> fi-vers.el
	echo '(setq fi:emacs-lisp-interface-version "$(VERSION)")' >> fi-vers.el
	echo '(defvar fi:compiled-with-version)' >> fi-vers.el
	echo '(setq fi:compiled-with-version (eval-when-compile (cons emacs-major-version emacs-minor-version)))' >> fi-vers.el

#readme.htm: readme0.htm
#	sed -e 's/__VERSION__/$(VERSION)/g' < readme0.htm > readme.htm

test.out:	$(elcs) fi-test.el
	$(emacs) -nw -batch -q -l fi-test.el
	@date > test.out

clean:	FORCE
	rm -f *.elc *.doc readme*.htm test.out

tags:	FORCE
	etags *.el

FORCE:

include local.mak
