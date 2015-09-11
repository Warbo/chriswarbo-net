# Convert back and forth between source and destination filenames

# rendered/a/b.html -> rendered/a/b -> a/b -> a/b.* -> a/b.md
source   = $(wildcard $(addsuffix .*, $(subst rendered/,,$(basename $1))))

# a/b.md -> a/b -> a/b.html -> rendered/a/b.html
rendered = $(addprefix rendered/, $(addsuffix .html, $(basename $1)))

# Some pages have sub-entries, eg. blog.md has blog/

get_entries = $(call rendered,\
                     $(shell for ENTRY in $1; \
                             do [[ ! -x $$ENTRY ]] || find $$ENTRY -type f;\
                             done))
    entries = $(call get_entries,$(basename $(call source,$1)))

top_level  = $(call rendered,$(shell ls *.md))
all_pages := $(top_level) $(call entries,$(top_level)) \
             $(call rendered,$(shell find archived -type f))

# Resources

out_css   := $(addprefix rendered/, $(shell find css -type f))
out_js    := $(addprefix rendered/, $(shell find js -type f))
out_feeds := rendered/rss.xml rendered/atom.xml

resources := $(out_css) $(out_js) $(out_feeds)

tmp := templates/default.html

# Extra bits of infrastructure

redirect := rendered/index.php rendered/archive.html

# Entry point

all : $(all_pages) $(resources) $(redirect)

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
$1 : $2
	mkdir -p $$(dir $$@)
	$$(call render,$$@)
endef

$(foreach p,$(all_pages),$(eval $(call PAGE,$p,$(call deps,$p))))

# Resources

$(out_css) : css/$(notdir $@)
	mkdir -p rendered/css
	cp css/$(notdir $@) rendered/css/

js_source = $(subst rendered/,,$1)

$(out_js) : $(call js_source,$@)
	mkdir -p $(dir $@)
	cp $(call js_source,$@) $@

$(redirect) : redirect.html
	mkdir -p $(dir $@)
	$(call render_to,redirect.html,$@)

# RSS & ATOM

$(out_feeds) : $(call source,$(out_feeds)) rendered/blog.html
	mkdir -p $(dir $@)
	echo pandoc -o $@ $(call source,$@)

# Extra functionality

clean :
	rm -rf rendered

push : copy
	ssh -t chriswarbo.net /home/chris/update.sh

copy : test
	rsync -e ssh -r -p -z --info=progress2 --append rendered chriswarbo.net:~/

# Tests

tests := $(addsuffix .pass, $(wildcard tests/*))

test : $(tests)

$(tests) : all
	$(basename $@)

.PHONY : all clean test copy push