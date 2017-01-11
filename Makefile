# Used for running "unsafe" tasks
THIS_FILE := $(lastword $(MAKEFILE_LIST))

# Convert back and forth between source and destination filenames

# rendered/a/b.html -> rendered/a/b -> a/b -> a/b.* -> a/b.md
source   = $(wildcard $(addsuffix .*, $(subst rendered/,,$(basename $1))))

# a/b.md -> a/b -> a/b.html -> rendered/a/b.html
rendered = $(addprefix rendered/, $(addsuffix .html, $(basename $1)))

# Some pages have sub-entries, eg. blog.md has blog/

get_entries = $(call rendered,\
                     $(shell for ENTRY in $1; \
                             do [[ ! -e $$ENTRY ]] || find $$ENTRY -type f -name "*.md";\
                             done))
    entries = $(call get_entries,$(basename $(call source,$1)))

top_level  = $(call rendered,$(shell ls *.md))
all_pages := $(top_level) $(call entries,$(top_level))

indices = $(addprefix rendered/,$(shell ls */index.html | grep -v rendered))

# Resources

out_css   := $(addprefix rendered/, $(shell find css -type f))
out_js    := $(addprefix rendered/, $(shell find js -type f))

resources := $(out_css) $(out_js)

tmp := templates/default.html

# Extra bits of infrastructure

redirect := rendered/index.php rendered/archive.html

# Entry point

all : quick_test pages

pages : $(all_pages) $(resources) $(indices) redirects

render_to = SOURCE="$1" DEST="$2" ./static/render_page
render    = $(call render_to,$(call source,$1),$1)

# Dependencies of a page: the source, template and any "dependencies: ..." line
extradeps = $(wildcard $(shell ./static/getDeps $1))
     deps = $(call source,$1) $(call entries,$1) \
            $(call extradeps,$(call source,$1)) $(tmp)

# Use PAGE as a template for rendering every page

# $1 - Target HTML file
# $2 - Dependencies
define PAGE
$1 : $2 static/render_page
	mkdir -p $$(dir $$@)
	$$(call render,$$@)
endef

$(foreach p,$(all_pages),$(eval $(call PAGE,$p,$(call deps,$p))))

$(indices) : */index.html
	cp $(call js_source,$@) $@

# Resources

$(out_css) : css/$(notdir $@)
	mkdir -p rendered/css
	cp css/$(notdir $@) rendered/css/

js_source = $(subst rendered/,,$1)

$(out_js) : $(call js_source,$@)
	mkdir -p $(dir $@)
	cp $(call js_source,$@) $@

$(redirect) : redirect.md static/render_page
	mkdir -p $(dir $@)
	$(call render_to,redirect.md,$@)

redirects : $(redirect) rendered/essays
	pushd rendered > /dev/null; [[ -h posts ]] || ln -s blog posts; popd > /dev/null
	pushd rendered > /dev/null; [[ -h git ]] || ln -s /opt/git git; popd > /dev/null

rendered/essays: $(all_pages) static/mkEssayLinks static/mkRedirectTo \
                 static/redirectTemplate.html
	./static/mkEssayLinks

# Extra functionality

clean :
	rm -rf rendered

unsafe_push :
	ssh chriswarbo.net /home/chris/update.sh

unsafe_copy : pages
	ssh chriswarbo.net 'rm -rf /home/chris/rendered'
	ssh chriswarbo.net 'cp -a /var/www/html /home/chris/rendered'
	rsync -e ssh --recursive --times --perms --links --compress --checksum --info=progress2 rendered chriswarbo.net:~/

# The "unsafe" targets above perform the actual work for 'push' and 'copy', and
# are made available for exceptional circumstances. Most of the time, the
# following "safe" versions should be used instead, which run the test suite
# first.

# Note that these "safe" versions call 'make' recursively, to guarantee that
# the "unsafe" tasks will not begin until 'test' has succeeded. We couldn't do
# this with regular dependencies, without sacrificing the ability to run the
# "unsafe" tasks on their own.

push : copy
	$(MAKE) -f $(THIS_FILE) unsafe_push

copy : quick_test
	$(MAKE) -f $(THIS_FILE) unsafe_copy

# Tests

tests := $(addsuffix .pass, $(wildcard tests/*))

# All tests, which are useful when content is changed
test : $(tests)

$(tests) : pages
	$(basename $@)

# Quick integrity checks, which are worth running on every push
quick_test :
	tests/dirs_have_indices
	tests/essays_redirects_to_projects
	tests/everything_suffixed
	tests/have_all_repos
	tests/have_readmes
	tests/no_empty_files

.PHONY : all pages redirects clean test quick_test copy push unsafe_copy unsafe_push
