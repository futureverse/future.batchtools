include .make/Makefile

spelling:
	$(R_SCRIPT) -e "spelling::spell_check_files('NEWS', ignore=readLines('inst/WORDLIST', warn=FALSE))"
 
future.tests/%:
	$(R_SCRIPT) -e "future.tests::check" --args --test-plan=$*

future.tests: future.tests/future.batchtools\:\:batchtools_local

spelling:
	$(R_SCRIPT) -e "spelling::spell_check_package()"
	$(R_SCRIPT) -e "spelling::spell_check_files('NEWS.md', ignore=readLines('inst/WORDLIST', warn=FALSE))"
