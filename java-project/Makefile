#______________________________________________________________________________
#
#          Project Convenience Makefile Wrapper for Maven2
#______________________________________________________________________________
#

# This Makefile is just a convenience wrapper for the Maven2
# program. The actual building rules for this project may
# be found in the Maven2 "pom.xml" file located in this folder.

#______________________________________________________________________________
#                                                                  DEFINITIONS

# Define the commandline invocation of Maven2:
MAVEN  := mvn

# Define the commandline invocation for removing files and folders:
RM     := rm -rf

# Define the commandline invocation for echoing to the console
ECHO   := echo

#______________________________________________________________________________
#                                                                       COLORS

NO_COLOR    = \x1b[0m
OK_COLOR    = \x1b[32;01m
ERROR_COLOR = \x1b[31;01m
WARN_COLOR  = \x1b[33;01m

OK_STRING    = $(OK_COLOR)[OK]$(NO_COLOR)
ERROR_STRING = $(ERROR_COLOR)[ERRORS]$(NO_COLOR)
WARN_STRING  = $(WARN_COLOR)[WARNINGS]$(NO_COLOR)

#_______________________________________________________________________________
#                                                              HELPER FUNCTIONS

# Usage: $(call chdir, <target directory>)
CHDIR_SHELL := $(SHELL)
define chdir

    $(eval _D=$(firstword $(1) $(@D)))
    $(info $(MAKE): cd $(_D)) $(eval SHELL = cd $(_D); $(CHDIR_SHELL))

endef

define find_class_in_jar

    @ for I in $(shell find . -name "*.jar" |grep target);                 \
        do                                                                 \
            $(ECHO) "Looking in $$I ...";                                  \
            if `jar tvf $$I | grep $(1) &> /dev/null`; then                \
              $(ECHO) -e "$(OK_COLOR)==> Found '$(1)' in $$I$(NO_COLOR)";  \
           fi;                                                             \
        done

endef

#______________________________________________________________________________
#                                                                BUILD TARGETS

all:
	@ $(MAVEN) compile

package:
	@ $(MAVEN) package

compile:
	@ $(MAVEN) compile

check:
	@ $(MAVEN) test

check-style:
	@ $(MAVEN) clean install checkstyle:check

findbugs:
	@ $(MAVEN) findbugs:check

test: compile
	@ $(MAVEN) test

where.is:
	@ $(ECHO) -e "$(OK_COLOR)===> Start looking in JAR files for $(class)...$(NO_COLOR)"
	$(call find_class_in_jar, $(class))

doc:
	@ $(MAVEN) javadoc:javadoc

distclean: clean ;

docs: doc ;

javadoc: doc ;

documentation: doc ;

# Helper rule
_mvn_clean:
	@ $(MAVEN) clean

clean:_mvn_clean
	@- $(RM) ./bin/*
	@- $(RM) ./docs/*

#______________________________________________________________________________
#                                                                       GERRIT

publish:
	@ $(ECHO) -e "$(OK_COLOR)===> Checking Gerrit commit hook$(NO_COLOR)"
	@if [ -e .git/hooks/commit-msg ]; then $(ECHO) -e "\ngerrit-hook....... $(OK_STRING)"; else exit 1; fi;
	@ $(ECHO) -e "$(OK_COLOR)===> Get latest versions from Git$(NO_COLOR)"
	@ git pull --rebase;
	@ $(ECHO) -e "$(OK_COLOR)===> Publish your changes in Gerrit for review$(NO_COLOR)"
	@if [ $(sha) ]; then git push -u $${1:-origin} $(sha):`git config branch.$$(git name-rev --name-only HEAD).merge | sed -e 's@refs/heads/@refs/for/@'`; \
 else git push -u $${1:-origin} HEAD:`git config branch.$$(git name-rev --name-only HEAD).merge | sed -e 's@refs/heads/@refs/for/@'`; fi;

unpublish-head:
	@ $(ECHO) -e "$(OK_COLOR)===> Reset latest commit(HEAD) as place it in GIT stage$(NO_COLOR)"
	@ git reset --soft HEAD^;
	@ git diff --staged --stat;
	@ $(ECHO) -e "\nHint:\n Make your changes in file system, add it to stage and use 'republish-head' target\n"

republish-head:
	@ $(ECHO) -e "$(OK_COLOR)===> Get all staged files and publish them for review as using previous commit message$(NO_COLOR)"
	@ git commit -c ORIG_HEAD
	@ git push -u $${1:-origin} HEAD:`git config branch.$$(git name-rev --name-only HEAD).merge | sed -e 's@refs/heads/@refs/for/@'`

help:
	@ $(ECHO) "Usage   :  make <target>"
	@ $(ECHO) "Targets :"
	@ $(ECHO) "   all ........... Compiles the project."
	@ $(ECHO) "   package ....... Builds project and archives all *.class files."
	@ $(ECHO) "   compile ....... Compiles all Java files."
	@ $(ECHO) "   check ......... Builds and runs all unit tests."
	@ $(ECHO) "   check-style.... Builds and run Checkstyle."
	@ $(ECHO) "   findbugs ...... Runs FindBugs."
	@ $(ECHO) "   test .......... Builds and runs all unit tests."
	@ $(ECHO) "   docs .......... Generates project documentation."
	@ $(ECHO) "   clean ......... Removes build products."
	@ $(ECHO) "   publish ....... Publish all your commits in Gerrit for review"
	@ $(ECHO) "                     If you want to publish previous commit not HEAD or not all, you have to specify commit SHA1"
	@ $(ECHO) "                     Example:"
	@ $(ECHO) "                             make publish sha=415a6ac"
	@ $(ECHO)
	@ $(ECHO) "   unpublish-head ... If you have negative review use this target to make files from last commit available for change"
	@ $(ECHO) "   republish-head ... Staged files will be send for reveiw using previous commit message ('Change-id' is keeped)"
	@ $(ECHO) "                      NOTE: Use this target only if you use 'unpublish-head' first to reset HEAD"
	@ $(ECHO)
	@ $(ECHO) "   where.is ......... Looking for given class name in Quad project JAR files. It will not stop when class is found."
	@ $(ECHO) "                      Example:"
	@ $(ECHO) "                             make where.is class=com.foo.util.PostSocialActivityCache"
	@ $(ECHO)
	@ $(ECHO) "   help .......... Prints this help message."
