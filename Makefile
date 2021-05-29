pandoc_outputs := \
		$(patsubst posts/lhs/%.lhs, posts/lhs/.build/%.html, $(wildcard posts/lhs/*.lhs)) \
		$(patsubst posts/lhs/Data/Functor/%.lhs, posts/lhs/.build/Data/Functor/%.html, $(wildcard posts/lhs/Data/Functor/*.lhs))


lhs_posts := $(patsubst posts/lhs/%, dist/posts/%, $(wildcard posts/lhs/*.html))

html_posts := $(patsubst posts/html/%, dist/posts/%, $(wildcard posts/html/*.html))

htm := $(wildcard posts/html/*.htm)

css_assets := $(patsubst %, dist/%, $(wildcard assets/*.css))

font_assets := $(patsubst %, dist/%, $(wildcard assets/*.ttf))

templates := $(wildcard templates/*.html)

html := $(patsubst html/%, dist/%, $(wildcard html/*.html))

js_assets = \
	     dist/assets/index.js \
	     dist/assets/script.js

js_workbox = \
             dist/assets/workbox-sw.js \
             dist/assets/workbox-sw.js.map \

js_workbox_routing = \
             dist/assets/workbox-routing.prod.js \
             dist/assets/workbox-routing.prod.js.map \

js_workbox_precaching = \
             dist/assets/workbox-precaching.prod.js \
             dist/assets/workbox-precaching.prod.js.map \

js_html5shiv = \
             dist/assets/html5shiv.min.js \
             dist/assets/html5shiv-printshiv.min.js

images := $(patsubst %, dist/%, $(wildcard images/*.svg)) \
          $(patsubst %, dist/%, $(wildcard images/*.png))

png_images := $(patsubst latex/png/%.tex, images/%.png, $(wildcard latex/png/*.tex))

svg_images := $(patsubst latex/svg/%.tex, images/%.svg, $(wildcard latex/svg/*.tex))

$(png_images): images/%.png: latex/png/%.tex
	TEXINPUTS="latex/png:${TEXINPUTS}" pdflatex -shell-escape -halt-on-error -output-directory=latex/png $<
$(svg_images): images/%.svg: latex/svg/%.tex
	TEXINPUTS="latex/svg:${TEXINPUTS}" pdflatex -shell-escape -halt-on-error -output-directory=latex/svg $<

latex: $(png_images) $(svg_images)
.PHONY: latex

latex_clean:
	rm *.log 2>/dev/null || true
	rm *.aux 2>/dev/null || true
	rm *.pdf 2>/dev/null || true
.PHONY: clean_latex

$(templates):

post/base.html: $(templates)

cabal:
	cabal build homepage
.PHONY: cabal

$(pandoc_outputs): posts/lhs/.build/%.html: posts/lhs/%.lhs posts/base.html
	pandoc $< -o $@

pandoc_dir:
	mkdir -p posts/lhs/.build
	mkdir -p posts/lhs/.build/Data/Functor
.PHONY: pandoc_dir

pandoc: pandoc_dir $(pandoc_outputs)
.PHONY: pandoc

posts_dir:
	mkdir -p dist/posts
.PHONY: posts_dir

$(lhs_posts): dist/posts/%: posts/lhs/% $(pandoc_outputs) $(templates)
	echo '{"wrapperClass": "post"}' | j2 -f json $< -o $@

jinja: $(lhs_posts)
.PHONY: jinja

$(htm):

$(html_posts): dist/posts/%: posts/html/% $(htm) $(templates)
	echo '{"wrapperClass": "post"}' | j2 -f json $< -o $@

html_posts: posts_dir $(html_posts)

lhs_posts: posts_dir cabal pandoc jinja
.PHONY: lhs_posts

posts: posts_dir lhs_posts html_posts
.PHONY: posts

$(html): dist/%: html/% $(templates)
	echo '{"wrapperClass": ""}' | j2 -f json $< -o $@

templates: $(templates)
.PHONY: templates

$(css_assets): dist/assets/%: assets/%
	cleancss -o $@ $<

$(font_assets): dist/assets/%: assets/%
	cp $< $@

$(js_assets): dist/assets/%: assets/%
	cp $< $@

$(js_workbox): dist/assets/%: node_modules/workbox-sw/build/%
	cp $< $@

$(js_workbox_routing): dist/assets/%: node_modules/workbox-routing/build/%
	cp $< $@

$(js_workbox_precaching): dist/assets/%: node_modules/workbox-precaching/build/%
	cp $< $@

$(js_html5shiv): dist/assets/%: node_modules/html5shiv/dist/%
	cp $< $@

assets_dir:
	mkdir -p dist/assets
.PHONY: assets_dir

assets: assets_dir \
       	$(css_assets) \
	$(font_assets) \
	$(js_assets) \
	$(js_workbox) \
	$(js_workbox_routing) \
	$(js_workbox_precaching) \
	$(js_html5shiv)
.PHONY: assets

dist/manifest.json: manifest.json
	cp $< $@

dist/sw.js: sw.js $(css_assets) $(font_assets)
	node ./scripts/build.js

$(images): dist/images/%: images/%
	cp $< $@

images_dir:
	mkdir -p dist/images
.PHONY: images_dir

images: images_dir $(images)
.PHONY: images

dist/feed.rss: posts/feed.json $(pandoc_outputs)
	cabal run -v0 exe:rssbuilder -- $< - | j2 -f json templates/feed.rss -o $@

all: posts $(html) assets latex latex_clean images templates dist/manifest.json dist/sw.js dist/feed.rss
.PHONY: all

clean:
	rm     latex/png/*.{log,aux,pdf} || true
	rm     latex/svg/*.{log,aux,pdf} || true
	rm -rf posts/lhs/.build || true
	rm -rf dist || true

deploy: all
	rm -f dist.tar.gz
	tar -czf dist.tar.gz dist
	scp dist.tar.gz coot.me:/var/www/homepage
	ssh coot.me "cd /var/www/homepage && rm -rf dist && tar -xaf dist.tar.gz && rm dist.tar.gz"
	rm dist.tar.gz
.PHONY: deploy

.DEFAULT_GOAL := all
