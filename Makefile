# Makefile

# This file is part of Sfera, a library for SuperForth
# http://programandala.net/en.program.sfera.html

# Author: Marcos Cruz (programandala.net), 2016.

# ==============================================================
# License

# You may do whatever you want with this work, so long as you
# retain all the copyright/authorship/acknowledgment/credit
# notice(s) and this license in all redistributed copies and
# derived works.  There is no warranty.

# ==============================================================
# Description

# This Makefile compiles the assembly sources of Sfera and
# creates the _bin files ready to be loaded by the word
# `load-code` into the SuperForth dictionary.

# ==============================================================
# Requirements

# asmx assembler (http://xi6.com/projects/asmx/)

# ==============================================================
# Usage

# Build all assembly sources:
#
#     make all

# Build only the assembly sources *already assembled* that have
# been updated:
#
#     make

# ==============================================================
# History

# 2016-01-09: Start.
#
# 2016-01-11: Added the backup recipe. Fixed the main recipe.
#
# 2016-01-14: Improved the file headers.
#
# 2016-01-15: Added the zip recipe. Updated the backup recipe.
# Removed old code.

# ==============================================================
# Config

VPATH = ./:asm:bin
MAKEFLAGS = --no-print-directory

.ONESHELL:

# ==============================================================
# Recipes

asm_files = $(wildcard asm/*_asm)
bin_files = $(wildcard bin/*_bin)

# ----------------------------------------------
# Update

.PHONY : bin
bin: $(bin_files)

bin/%_bin: asm/%_asm
	cd asm ; \
	asmx -C 68K -e -w -b -o ../$@ -l ../$<_lst ../$< ; \
	cd -

# ----------------------------------------------
# Assemble all sources

all: $(asm_files)
	cd asm ; \
	for source in $$(ls -1 *_asm) ; do \
  		base_name=$${source%%_asm} ; \
		echo "Compiling $$source" ; \
		asmx -C 68K -e -w -b -o ../bin/$${base_name}_bin -l $${base_name}_asm_lst $$source ; \
	done ; \
	cd -

# ----------------------------------------------
# ZIP archive for distribution

# The -FS and -u options of zip do not work fine in some cases,
# so the archive is deleted first:

.PHONY: zip
zip:
	rm -f sfera.zip && \
	cd .. && \
	zip -9 sfera/sfera.zip \
		sfera/*_fs \
		sfera/LICENSE.txt \
		sfera/Makefile \
		sfera/README.adoc \
		sfera/TO-DO.adoc \
		sfera/asm/*_asm && \
	cd -

# ----------------------------------------------
# Development backup

.PHONY: backup
backup:
	cd .. ; \
	tar -cJf sfera/backups/$$(date +%Y%m%d%H%M)_sfera.tar.xz \
		sfera/*_fs \
		sfera/.gitattributes \
		sfera/.gitignore \
		sfera/LICENSE.txt \
		sfera/Makefile \
		sfera/README.adoc \
		sfera/TO-DO.adoc \
		sfera/_draft/ \
		sfera/asm/*_asm ; \
	cd -
