# Conveniences for Unix hackers.


SHELL = /bin/sh
RM    = /bin/rm -f
RM-R    = /bin/rm -rf
LISP  = $(ACLHOME)/alisp

SRC_DIRS = system/asa system/sherpa system/sherpa/pert system/utility \
	   system/foundation system/simulation system/simulation/test \
	   extensions \
	   system/loading apexlib apexlib/human regression-tests

EXAMPLE_DIRS = examples/apexlib examples/goms/atm examples/goms examples \
	       examples/goms/dual-task examples/goms/fox-cpm examples/xplane

LISP_DIRS = $(SRC_DIRS) $(EXAMPLE_DIRS) ./

COMPILATION = *.fas* *.lib

# Typing 'make' builds the CLISP memory image, which needs to be done
# for each machine.

default:
	$(LISP) -L load

# Create tag table for Emacs.  Tag tables enable searching/replacing
# throughout directory tree.

tags: newtag
	@ find . -name "*.lisp" | \
          etags --regex='{lisp}/[ \t]*[(]primitive[ \t\n]*[(].+[)]/\1/ig/' \
                --regex='{lisp}/[ \t]*[(]procedure[ \t\n]*[(].+[)]/\1/ig/' \
                --regex='{lisp}/[ \t]*[(]index[ \t\n]*[(].+[)]/\1/ig/' \
                -

# Hack to force rebuilding of tags table in OS/X
newtag:
	@ rm -f TAGS

# Remove all compilation products.

# Refactored below
#clean:
#	@ $(RM) apexapp* TAGS lib*.dylib autoloads.out
# 	@ $(RM) sherpa.ini
# 	@ $(RM-R) bin
# 	@ for dir in $(LISP_DIRS) ; do \
# 	    pushd $$dir >/dev/null ; \
# 	    $(RM) $(COMPILATION) ; \
# 	    popd >/dev/null ; \
# 	  done


clean: basic-clean
	@ $(RM) apexapp* TAGS lib*.dylib autoloads.out
	@ $(RM) sherpa.ini

basic-clean:
	@ $(RM-R) bin

app:
	@ ./buildapp
